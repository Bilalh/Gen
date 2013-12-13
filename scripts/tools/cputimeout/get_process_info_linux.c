#include "get_process_info.h"

#include <unistd.h>
#include <limits.h>
#include <dirent.h>
#include "list.h"

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

static void store_process(pid_t pid){
	int hashed = pid_hashfn(pid);
	printf("pid:%d hashed:%d\n", pid, hashed);

	if (our_processes[hashed] == NULL){
		struct list *chain = malloc(sizeof(struct list));
		init_list(chain, sizeof(struct ProcessStats));
		our_processes[hashed] = chain;
	}

	struct ProcessStats *stats = malloc(sizeof(struct ProcessStats));
	stats->pid =pid;
	update_process_stats(stats);

	add_elem(our_processes[hashed], stats);
}

bool update_our_processes(pid_t monitored_pid){
	//FIXME error handing
	printf("%s\n", "updating our processes");

	sleep(1);
	DIR *dir_proc =  opendir("/proc");
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
			store_process(pid);
			// printf("child %d\n", pid);
		// 	struct ProcessStats proc_stats;
		// 	int res = update_process_stats(&proc_stats);
		// 	printf("%d utime:%ld  stime:%ld\n", res, proc_stats.utime, proc_stats.stime);
		}
	}

	return true;
}




bool update_process_stats(struct ProcessStats *p){

	static char buff[1024];
	static char statfp[32];

	sprintf(statfp, "/proc/%d/stat", p->pid);

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