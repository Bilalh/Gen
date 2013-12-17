#include "get_process_info.h"
#include "list.h"
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

#include <sys/types.h>
#include <unistd.h>

int main (int argc, char **argv){

	pid_t monitored_pid = getpid();

	Processes *our_starting = malloc(sizeof(struct Processes));

	for (int i = 0; i < PIDHASH_SZ; ++i){
		our_starting->list[i] = NULL;
	}

	// print_proclist(our_starting);
	bool res = update_our_processes(our_starting, monitored_pid);
	if (!res){
		fprintf(stderr, "Failed to get stats of pid:%d and it's children\n",monitored_pid );
		exit(EXIT_FAILURE);
	}

	return 0;
}