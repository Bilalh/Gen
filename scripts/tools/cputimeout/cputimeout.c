


// cputimeout Execute a command with a cpu timeout,  by Bilal Syed Hussain
// Based off gnu timeout

#include "cputimeout.h"
#include <errno.h>
#include <getopt.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <math.h>
#include <sys/time.h>

bool update_process_stats(pid_t pid, struct ProcessStats *p){

	static char buff[1024];
	static char statfp[32];

	sprintf(statfp, "/proc/%d/stat", pid);

	FILE *fd = fopen(statfp, "r");
	if (fd==NULL) return false;

	if (fgets(buff, sizeof(buff), fd)==NULL) {
		fclose(fd);
		return false;
	}
	fclose(fd);
	// printf("%s\n",buff);

	char *field = strtok(buff, " ");
	// skips the first 13 fields of /proc/{id}/stat
	for (int i=0; i<13; i++){
		field = strtok(NULL, " ");
	}

	// convert utime and stime in `jiffies` to seconds * 1000
	// since HZ is a integer which has a max value of 1000
	// so * 1000 to prevent (some) rounding errors
	p->utime = atoi(field) * 1000 / HZ;
	field    = strtok(NULL, " ");
	p->stime = atoi(field) * 1000 / HZ;

	return true;
}

void usage (int status){
char help[] =
	"cputimeout [-o filepath] [-k seconds] [-f] [-h] timeout command\n"
	"  Execute a command with a cpu timeout.\n"
	"\n"
	"  Upon time-out expiration SIGTERM(15) is sent to the process. If SIGTERM signal\n"
	"  is blocked and -k is specified, then the subsequent SIGKILL(9) terminates it.\n"
	"\n"
	"    timeout\n"
	"       Number of seconds in cpu time to wait for command completion.\n"
	"    -i,   --interval interval\n"
	"       Interval between checks if the process is still alive.\n"
	"       Positive integer, default value: 3 seconds.\n"
	"       Specifying -i means that the command will NOT be killed until \n"
	"       interval seconds have passed, even if it is changed by the timeout file\n"
	"    -k,   --kill-after delay\n"
	"       Delay between sending SIGTERM and destroying the process by SIGKILL.\n"
	"    -o,  --timeout-file timeout-file\n"
	"       The timeout can be changed ONCE by placing a integer into timeout-file\n"
	"    -f,  --use-sigkill\n"
	"       Upon time-out expiration send SIGKILL(9) instead of SIGTERM(15)\n"
	"    -p,  --previous-used\n"
	"       Time to subtract from new time n in -o e.g running a series of processes\n"
	"        using one timeout file \n"
    " by Bilal Syed Hussain based on GNU timeout"
	;
	puts(help);

	exit (status);
}

static bool disable_core_dumps (void);
static double parse_duration (const char* str);
static int send_sig (int where, int sig);
static void cleanup (int sig);
static void close_stdout (void);
static void settimeout (double duration);
static void unblock_signal (int sig);


static pid_t monitored_pid;

static double timeout_increment = 3;
static double timeout_left;
static double timeout_total;

// for use with the timeoutfile
// e.g if 3 have a timeout of 5 seconds and timeout changes to 10
// the timeleft is (timeout_new - timeout_total) - timeout_previous_used
// This allows one timeoutfile for running a series of processess
static double timeout_previous_used = 0;

static struct timeval start_time = {};

static double kill_after;

static char *timeout_file;
static bool timeout_changed;

static int timed_out;
static bool preserve_status;
static int term_signal = SIGTERM;
static bool foreground = false;

static struct ProcessStats starting_stats = {};
static struct ProcessStats cur_stats = {};

// Exit statuses for programs like 'env' that exec other programs.
enum {
	EXIT_TIMEDOUT      = 124, // Time expired before child completed.
	EXIT_CANCELED      = 125, // Internal error prior to exec attempt.
	EXIT_CANNOT_INVOKE = 126, // Program located, but not usable.
	EXIT_ENOENT        = 127  // Could not find program to exec.
};

enum{
      PRESERVE_STATUS_OPTION = CHAR_MAX +2
};

// Help options
static struct option const long_options[] = {
  {"kill-after"      , required_argument , NULL , 'k'                    } ,
  {"timeout-file"    , required_argument , NULL , 'o'                    } ,
  {"interval"        , required_argument , NULL , 'i'                    } ,
  {"preserve-status" , no_argument       , NULL , PRESERVE_STATUS_OPTION } ,
  {"help"            , no_argument       , NULL , 'h'                    } ,
  {"use-sigkill"     , no_argument       , NULL , 'f'                    } ,
  {"previous-used"   , required_argument , NULL , 'p'                    } ,
  {NULL              , 0                 , NULL , 0                      }
};



static void install_signal_handlers (int sigterm){
  struct sigaction sa;
  sigemptyset (&sa.sa_mask);  // Allow concurrent calls to handler
  sa.sa_handler = cleanup;
  sa.sa_flags = SA_RESTART;   /* Restart syscalls if possible, as that's
                                 more likely to work cleanly.  */

  sigaction (SIGALRM, &sa, NULL); // our timeout.
  sigaction (SIGINT, &sa, NULL);  // Ctrl-C at terminal for example.
  sigaction (SIGQUIT, &sa, NULL); // Ctrl-\ at terminal for example.
  sigaction (SIGHUP, &sa, NULL);  // terminal closed for example.
  sigaction (SIGTERM, &sa, NULL); // if we're killed, stop monitored proc.
  sigaction (sigterm, &sa, NULL); // user specified termination signal.
}

static void cleanup (int sig){
	if (sig == SIGALRM){
		struct ProcessStats prev_stats = cur_stats;
		update_process_stats(monitored_pid, &cur_stats);
		// printf("cputimeout: u %ld\n", (cur_stats.utime - prev_stats.utime) );
		// printf("cputimeout: s %ld\n", (cur_stats.stime - prev_stats.stime) );

		if (timeout_left > 0){
			FILE *fp = NULL;
			if (timeout_file && !timeout_changed && (fp = fopen(timeout_file,"r")) ){
				int timeout_new;
				int res = fscanf(fp, "%d", &timeout_new);
				if (res >= 1){
					timeout_changed = true;
					printf("cputimeout: timeout changed to %ds after %0.0lf seconds\n", timeout_new, timeout_total - timeout_left );
					timeout_left += timeout_new - timeout_total - timeout_previous_used;
					printf("cputimeout: timeout_left %0.0lf timeout_new %d timeout_total(Original) %0.0lf  timeout_previous_used %0.0lf\n", \
							timeout_left, timeout_new, timeout_total, timeout_previous_used);

				}
				fclose(fp);
			}
			if (timeout_left > 0){
				double next_alarm  = timeout_left < timeout_increment
				                   ? timeout_left : timeout_increment;
				float cputime = (cur_stats.utime - prev_stats.utime) + (cur_stats.stime - prev_stats.stime);

				//TODO should we use ceil? floor will over estimate the timeleft which
				//     might be a good thing
				// printf("cputimeout:cputime:%lf\n", cputime);
				// printf("cputimeout:timeout before:%lf\n", timeout_left);
				timeout_left      -= cputime/1000;
				// printf("cputimeout:timeout after:%lf\n", timeout_left);
				// printf("cputimeout:next_alarm:%lf\n", next_alarm);

				if (timeout_left + next_alarm <= 0){
                    // printf("cputimeout: timed_out inside timeout_left > 0 ~L190  \n");
					timed_out = 1;
					sig = term_signal;
				}else{
					unsigned alarm_ret = alarm(next_alarm);
					return;
				}
			}else{
				timed_out = 1;
				sig = term_signal;
			}
		}else{
			timed_out = 1;
			sig = term_signal;
		}
	}
	if (monitored_pid) {
		if (kill_after) {
			// Start a new timeout after which we'll send SIGKILL.
			term_signal = SIGKILL;
			settimeout(kill_after);
			kill_after = 0; // Don't let later signals reset kill alarm.
		}

		/* Send the signal directly to the monitored child,
		   in case it has itself become group leader,
		   or is not running in a separate group.  */
		send_sig (monitored_pid, sig);
		/* The normal case is the job has remained in our
		   newly created process group, so send to all processes in that.  */
		if (!foreground)
			send_sig (0, sig);
		if ( sig != SIGKILL && sig != SIGCONT)
		{
			send_sig (monitored_pid, SIGCONT);
			if (!foreground)
				send_sig (0, SIGCONT);
		}
	}
	// we're the child or the child is not exec'd yet.
	else _exit (128 + sig);
}

// Start the timeout after which we'll receive a SIGALRM.
static void settimeout (double duration){

	/* We configure timers below so that SIGALRM is sent on expiry.
	   Therefore ensure we don't inherit a mask blocking SIGALRM.  */
	unblock_signal (SIGALRM);

	unsigned int timeint;
	if (UINT_MAX <= duration){
		timeint = UINT_MAX;
	} else {
		unsigned int duration_floor = duration;
		timeint = duration_floor + (duration_floor < duration);
	}
	alarm (timeint);
}


int main (int argc, char **argv){
	setpgid(0,0);
	int c;

	atexit (close_stdout);
	while ((c = getopt_long (argc, argv, "+k:hfo:ip:", long_options, NULL)) != -1) {
		switch (c) {
			case 'i':
				timeout_increment = parse_duration(optarg);
				break;
			case 'f':
				term_signal = SIGKILL;
				break;
			case 'k':
				kill_after = parse_duration(optarg);
				break;
			case 'o':
				timeout_file = strdup(optarg);
				break;
			case 'p':
				timeout_previous_used = parse_duration (optarg);
				break;
			case PRESERVE_STATUS_OPTION:
				preserve_status = true;
				break;
			default:
				usage (EXIT_CANCELED);
				break;
		}
	}

	if (argc - optind < 2)
		usage (EXIT_CANCELED);

	timeout_left = timeout_total = parse_duration (argv[optind++]);
	argv += optind;

	install_signal_handlers (term_signal);
	signal (SIGTTIN, SIG_IGN);   // Don't stop if background child needs tty.
	signal (SIGTTOU, SIG_IGN);   // Don't stop if background child needs tty.
	signal (SIGCHLD, SIG_DFL);   // Don't inherit CHLD handling from parent.

	monitored_pid = fork();
	if (monitored_pid == -1) {
		perror("fork system call failed");
		return EXIT_CANCELED;
	}
	else if (monitored_pid == 0) { // child
		int exit_status;

		// exec doesn't reset SIG_IGN -> SIG_DFL.
		signal (SIGTTIN, SIG_DFL);
		signal (SIGTTOU, SIG_DFL);

		execvp (argv[0], argv);   // FIXME: should we use "sh -c" ... here? probably not

		// exit like sh, env, nohup, ...
		exit_status = (errno == ENOENT ? EXIT_ENOENT : EXIT_CANNOT_INVOKE);
		fprintf(stderr,"failed to run command %s",argv[0]);
		return exit_status;
	}else{
		gettimeofday(&start_time, NULL);
		printf("cputimeout: monitored_pid:%ld\n", (long) monitored_pid  );
		bool res = update_process_stats(monitored_pid, &starting_stats);
		if (!res){
			fprintf(stderr, "Failed to get stats of pid:%d\n",monitored_pid );
			exit(EXIT_FAILURE);
		}

		pid_t wait_result;
		int status;
		settimeout(timeout_increment);

		while ((wait_result = waitpid (monitored_pid, &status, 0)) < 0 && errno == EINTR){
			continue;
		}

		struct timeval  end_time;
		gettimeofday(&end_time, NULL);

		double wall_time = (end_time.tv_sec * 1e6  +  end_time.tv_usec) - (start_time.tv_sec * 1e6  +  start_time.tv_usec);
		double wall_time_seconds = wall_time / 1e6;

		printf("cputimeout: recorded utime   :%0.4lf\n", ((float)(cur_stats.utime - starting_stats.utime))/1000 );
		printf("cputimeout: recorded stime   :%0.4lf\n", ((float)(cur_stats.stime - starting_stats.stime))/1000 );
		printf("cputimeout: recorded walltime:%0.4lf\n", wall_time_seconds );

		if (wait_result < 0) {
			// shouldn't happen.
			fprintf(stderr,"error waiting for command");
			status = EXIT_CANCELED;
		}else{
			if (WIFEXITED (status))
				status = WEXITSTATUS (status);
			else if (WIFSIGNALED (status))
			{
				int sig = WTERMSIG (status);
				if (WCOREDUMP (status))
					fprintf(stderr,"the monitored command dumped core");
				if (!timed_out && disable_core_dumps())
				{
					// exit with the signal flag set.
					signal (sig, SIG_DFL);
					raise (sig);
				}
				status = sig + 128; // what sh returns for signaled processes.
			}
			else
			{
				// shouldn't happen.
				fprintf(stderr,"unknown status from command (0x%X)", status);
				status = EXIT_FAILURE;
			}
		}

		if (timed_out && !preserve_status)
			return EXIT_TIMEDOUT;
		else
			return status;
	}

}

/* Try to disable core dumps for this process.
   Return TRUE if successful, FALSE otherwise.  */
static bool disable_core_dumps (void) {
#if HAVE_PRCTL && defined PR_SET_DUMPABLE
	if (prctl (PR_SET_DUMPABLE, 0) == 0)
		return true;

#elif HAVE_SETRLIMIT && defined RLIMIT_CORE
	/* Note this doesn't disable processing by a filter in
	   /proc/sys/kernel/core_pattern on Linux.  */
	if (setrlimit (RLIMIT_CORE, &(struct rlimit) {0,0}) == 0)
		return true;

#else
	return false;
#endif

	fprintf(stderr,"warning: disabling core dumps failed");
	return false;
}

static void unblock_signal (int sig) {
	sigset_t unblock_set;
	sigemptyset (&unblock_set);
	sigaddset (&unblock_set, sig);
	if (sigprocmask (SIG_UNBLOCK, &unblock_set, NULL) != 0){
		fprintf(stderr,"warning: sigprocmask");
	}
}

static int send_sig (int where, int sig) {
	/* If sending to the group, then ignore the signal,
	   so we don't go into a signal loop.  Note that this will ignore any of the
	   signals registered in install_signal_handlers(), that are sent after we
	   propagate the first one, which hopefully won't be an issue.  Note this
	   process can be implicitly multithreaded due to some timer_settime()
	   implementations, therefore a signal sent to the group, can be sent
	   multiple times to this process.  */
	if (where == 0){
		printf("cputimeout: ignoring signal %d to %d\n",sig,where);
		signal (sig, SIG_IGN);
	}
	printf("cputimeout: sending signal %d to %d\n",sig,where);
	return kill (where, sig);
}

// we keep track of the time in seconds
static double parse_duration (const char* str) {
	double duration;

	duration = strtol(str, NULL, 10);
	return duration;
}

int close_stream (FILE *stream)
{
  const bool prev_fail = (ferror (stream) != 0);
  const bool fclose_fail = (fclose (stream) != 0);

  /* Return an error indication if there was a previous failure or if
     fclose failed, with one exception: ignore an fclose failure if
     there was no previous error, no data remains to be flushed, and
     fclose failed with EBADF.  That can happen when a program like cp
     is invoked like this 'cp a b >&-' (i.e., with standard output
     closed) and doesn't generate any output (hence no previous error
     and nothing to be flushed).  */

  if (prev_fail || (fclose_fail && ( errno != EBADF))) {
      if (! fclose_fail) errno = 0;
      return EOF;
    }

  return 0;
}

static void close_stdout (void) {
	if (close_stream (stdout) != 0) {
		char const *write_error = "write error";
		perror(write_error);
		_exit (EXIT_FAILURE);
	}

	if (close_stream (stderr) != 0)
		_exit (EXIT_FAILURE);
}

