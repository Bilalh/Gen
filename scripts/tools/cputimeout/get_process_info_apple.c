#include "get_process_info.h"

#include <errno.h>
#include <libproc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>

static bool get_process_pti(pid_t pid, struct proc_taskallinfo *ti) {
	int bytes;
	bytes = proc_pidinfo(pid, PROC_PIDTASKALLINFO, 0, ti, sizeof(*ti));
	if (bytes <= 0) {
		if (!(errno & (EPERM | ESRCH))) {
			fprintf(stderr, "proc_pidinfo: %s\n", strerror(errno));
		}
		return false;
	} else if (bytes < sizeof(ti)) {
		fprintf(stderr, "proc_pidinfo: too few bytes; expected %ld, got %d\n", sizeof(ti), bytes);
		return false;
	}
	return true;
}

bool update_our_processes(Processes *our_starting, Processes *our_current, pid_t monitored_pid){
	assert(our_starting);
	assert(our_current);

	llprintf("%s for %d\n", "updating our processes", monitored_pid);

	char *cmd;
	asprintf(&cmd, "ps -o pid,pgid -ax | grep %ld | cut -d ' ' -f 1 ", (long) monitored_pid );

    FILE *ps_out;
	if (!(ps_out = popen(cmd, "r" ))){
		free(cmd);
		perror("Failed to run ps");
		exit(32);
	}
	free(cmd);

	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;

   	while ((linelen = getline(&line, &linecap, ps_out)) > 0){
		int pid = atoi(line);
		store_process(our_starting, pid, false);
		store_process(our_current, pid, true);
   	}

   	llprintf("%s for %d\n", "Finished updating our processes", monitored_pid);
	return true;
}

bool update_process_stats(struct ProcessStats *p){

	static struct proc_taskallinfo ti;

	if (! get_process_pti(p->pid, &ti) ) {
		perror("get_process_pti failed");
		return false;
	}

	p->utime = ti.ptinfo.pti_total_user   / 1000000;
	p->stime = ti.ptinfo.pti_total_system / 1000000;

	return true;
}

