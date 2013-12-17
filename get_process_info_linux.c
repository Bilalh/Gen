
#include "get_process_info.h"
#include "list.h"

#include <dirent.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/procfs.h>
#include <time.h>
#include <unistd.h>
#include <assert.h>

static pid_t getppid_of(pid_t pid)
{
	char statfile[20];
	char buffer[1024];
	sprintf(statfile, "/proc/%d/stat", pid);
	FILE *fd = fopen(statfile, "r");
	if (fd==NULL) return -1;
	if (fgets(buffer, sizeof(buffer), fd)==NULL) {
		fclose(fd);
		return -1;
	}
	fclose(fd);
	char *token = strtok(buffer, " ");
	int i;
	for (i=0; i<3; i++) token = strtok(NULL, " ");
	return atoi(token);
}

static bool is_child_of(pid_t child_pid, pid_t parent_pid)
{
	int ppid = child_pid;
	while(ppid > 1 && ppid != parent_pid) {
		ppid = getppid_of(ppid);
	}
	return ppid == parent_pid;
}

static void store_process(Processes *our_processes, pid_t pid, bool replace){
	int hashed = pid_hashfn(pid);
	llprintf("pid:%d hashed:%d replace:%d \n", pid, hashed, replace);


	if (our_processes->list[hashed] == NULL){
		struct list *chain = malloc(sizeof(struct list));
		init_list(chain, sizeof(struct ProcessStats));
		our_processes->list[hashed] = chain;
	}

	if (our_processes->list[hashed] != NULL){
		llprintf("check list[%d] count:%d\n", hashed, our_processes->list[hashed]->count);
	}


	struct ProcessStats *stats = calloc(1, sizeof(struct ProcessStats));
	llprintf("stats addr:%p\n", stats);
	stats->pid =pid;
	stats->utime = 0;
	stats->stime = 0;


	// Could just use xlocate_node with the pid then make the stats inside?
	struct list_node *node_inside= NULL;
	if ( (node_inside = xlocate_node(our_processes->list[hashed], stats, 0, sizeof(pid_t)) ) ) {
		llprintf("inside xlocate if  node_inside:%p\n", node_inside);
		if (replace){
			update_process_stats(node_inside->data);
		}
		free(stats);

	}else{
		printf("new pid added:%ld to %p\n", (long) pid, our_processes);

		int count_before = get_list_count(our_processes->list[hashed]);
		llprintf("inside xlocate if  list[%d] count:%d\n", hashed, our_processes->list[hashed]->count);

		bool res = update_process_stats(stats);
		llprintf("update_process_stats Sucess? %d\n", res);
		llprintf("bucket:%d pid:%ld stime:%ld utime:%ld\n",hashed, (long)pid, stats->stime, stats->utime );
		add_elem(our_processes->list[hashed], stats);


		int count_after = get_list_count(our_processes->list[hashed]);
		llprintf("added %ld,  count:%d \n", (long)pid, count_after);
		assert(count_after = count_before + 1);

		// print_proclist(our_processes);
	}

	llprintf("end\n");
}

bool update_our_processes(Processes *our_starting, Processes *our_current, pid_t monitored_pid){
	assert(our_starting);
	assert(our_current);

	llprintf("%s for %d\n", "updating our processes", monitored_pid);

	DIR *dir_proc =  NULL;
	if ((dir_proc = opendir("/proc")) == NULL){
		perror("opening /proc");
		return false;
	}

	struct dirent *dit = NULL;

	while ((dit = readdir(dir_proc)) != NULL) {
		if(strtok(dit->d_name, "0123456789") != NULL)
			continue;
		pid_t pid = atoi(dit->d_name);

		if (is_child_of(pid,monitored_pid)){
			store_process(our_starting, pid, false);
			store_process(our_current, pid, true);
		}
	}

	if ( closedir(dir_proc) != 0 ){
		perror("closing /proc");
		return false;
	}
	llprintf("%s for %d\n", "Finished updating our processes", monitored_pid);
	return true;
}


bool update_process_stats(struct ProcessStats *p){
	char buff[1024];
	char statfp[32];

	sprintf(statfp, "/proc/%d/stat", p->pid);

	FILE *fd = fopen(statfp, "r");
	if (fd==NULL) return false;

	if (fgets(buff, sizeof(buff), fd)==NULL) {
		fclose(fd);
		return false;
	}
	fclose(fd);

	// printf("proc line for %ld\n%s\n", (long)p->pid, buff);

	char *field = strtok(buff, " "); // pid
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
