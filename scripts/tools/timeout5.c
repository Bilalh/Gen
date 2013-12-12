// timeout5 Execute a command with a time-out,  by Bilal Syed Hussain
// Based off gnu timeout

#include <errno.h>
#include <getopt.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

void usage (int status){
char help[] =
	"timeout5 [-o filepath] [-k seconds] [-f] [-h] timeout command\n"
	"  Execute a command with a time-out.\n"
	"\n"
	"  Upon time-out expiration SIGTERM(15) is sent to the process. If SIGTERM signal\n"
	"  is blocked and -k is specified, then the subsequent SIGKILL(9) terminates it.\n"
	"\n"
	"    timeout\n"
	"       Number of seconds to wait for command completion.\n"
	"\n"
	"    -i,   --interval interval\n"
	"       Interval between checks if the process is still alive.\n"
	"       Positive integer, default value: 3 seconds.\n"
	"       Specifying -i means that the command will NOT be killed until \n"
	"       interval seconds have passed, even if it is changed by the timeout file\n"
	"\n"
	"    -k,   --kill-after delay\n"
	"       Delay between sending SIGTERM and destroying the process by SIGKILL.\n"
	"\n"
	"    -o,  --timeout-file timeout-file\n"
	"       The timeout can be changed ONCE by placing a integer into timeout-file\n"
	"    -f,  --use-sigkill\n"
	"        Upon time-out expiration send SIGKILL(9) instead of SIGTERM(15)\n"
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


static int monitored_pid;

static double timeout_increment = 3;
static double timeout_left;
static double timeout_total;
static double kill_after;

static char *timeout_file;
static bool timeout_changed;

static int timed_out;
static bool preserve_status;
static int term_signal = SIGTERM;
static bool foreground = false;

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
	if (sig == SIGALRM) {
		if (timeout_left > 0){
			FILE *fp = NULL;
			if (timeout_file && !timeout_changed && (fp = fopen(timeout_file,"r")) ){
				int timeout_new;
				int res = fscanf(fp, "%d", &timeout_new);
				if (res >= 1){
					timeout_changed = true;
					printf("timeout5: timeout changed to %ds after %0.0lf seconds\n", timeout_new, timeout_total - timeout_left );
					timeout_left += timeout_new - timeout_total;
					printf("timeout5: timeout_left %0.0lf timeout_new %d timeout_total(Original) %0.0lf  \n", \
							timeout_left, timeout_new, timeout_total);

				}
				fclose(fp);
			}
			if (timeout_left > 0){
				double next_alarm  = timeout_left < timeout_increment
				                   ? timeout_left : timeout_increment;

                // printf("timeout5:timeout before:%lf\n", timeout_left);
				timeout_left      -= next_alarm;
                // printf("timeout5:timeout after:%lf\n", timeout_left);
                // printf("timeout5:next_alarm:%lf\n", next_alarm);

				if (timeout_left + next_alarm <= 0){
                    printf("timeout5: timed_out inside timeout_left > 0 ~L137  \n");
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
	while ((c = getopt_long (argc, argv, "+k:hfo:i:", long_options, NULL)) != -1) {
		switch (c) {
			case 'i':
				timeout_increment = parse_duration (optarg);
				break;
			case 'f':
				term_signal = SIGKILL;
				break;
			case 'k':
				kill_after = parse_duration (optarg);
				break;
			case 'o':
				timeout_file = strdup(optarg);
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

		execvp (argv[0], argv);   // FIXME: should we use "sh -c" ... here?

		// exit like sh, env, nohup, ...
		exit_status = (errno == ENOENT ? EXIT_ENOENT : EXIT_CANNOT_INVOKE);
		fprintf(stderr,"failed to run command %s",argv[0]);
		return exit_status;
	}else{
		pid_t wait_result;
		int status;

		timeout_left -= timeout_increment;
		settimeout(timeout_increment);

		while ((wait_result = waitpid (monitored_pid, &status, 0)) < 0 && errno == EINTR){
			continue;
		}

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
		printf("timeout5: ignoring signal %d to %d\n",sig,where);
		signal (sig, SIG_IGN);
	}
	printf("timeout5: sending signal %d to %d\n",sig,where);
	return kill (where, sig);
}

static double parse_duration (const char* str) {
	double duration;
	const char *ep;

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

