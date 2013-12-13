#include "get_process_info.h"
#include <libproc.h>
#include <errno.h>
#include <stdio.h>

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

bool update_process_stats(pid_t pid, struct ProcessStats *p){

	static struct proc_taskallinfo ti;

	if (! get_process_pti(pid, &ti) ) {
		return false;
	}

	p->utime = ti.ptinfo.pti_total_user   / 1000000;
	p->stime = ti.ptinfo.pti_total_system / 1000000;

	return true;
}