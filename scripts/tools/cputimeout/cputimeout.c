
#include "cputimeout.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h> 
#include <sys/types.h>
#include <string.h>

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
	printf("%s\n",buff);
	
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



int main (int argc, char const *argv[])
{
	
	pid_t pid = atoi(argv[1]);
	struct ProcessStats stats= {};
	printf("pid:%d\n", pid);

	bool res = update_process_stats(pid, &stats);
	printf("Success?:%d\n", res);
	if (res){
		printf("%ld %ld\n", stats.utime, stats.stime);
	}
	
	return 0;
}