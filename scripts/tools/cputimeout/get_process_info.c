#include "get_process_info.h"
#include <assert.h>
#include <stdlib.h>

void print_proclist(Processes *our_processes){
	assert(our_processes);
	llprintf("printing proclist %p\n", our_processes);

	for (int i = 0; i < PIDHASH_SZ; ++i){
		if ( our_processes->list[i] ==  NULL){
			llprintf("list[%d] is null\n", i);
			continue;
		}
		llprintf("Is a list, list[%d]  count:%d\n", i,  get_list_count(our_processes->list[i]));
		struct list_node *tmp = our_processes->list[i]->first;
		while(tmp!=NULL) {
			struct ProcessStats *ps = tmp->data;
			assert(ps);
			assert(ps->pid > 1);
			llprintf("ps addr:%p\n", ps);
			llprintf("bucket:%d\n",i);
			llprintf("pid:%d\n", ps->pid );
			llprintf("stime:%ld utime:%ld \n", ps->stime, ps->utime );
			tmp=tmp->next;
		}
	}
	llprintf("printing proclist end\n");
}

void print_proclist2(Processes *our_processes){
	assert(our_processes);
	printf("printing proclist @ %p\n", our_processes);

	for (int i = 0; i < PIDHASH_SZ; ++i){
		if ( our_processes->list[i] ==  NULL){
			llprintf("list[%d] is null\n", i);
			continue;
		}
		printf("Is a list, list[%d]  count:%d\n", i,  get_list_count(our_processes->list[i]));
		struct list_node *tmp = our_processes->list[i]->first;
		while(tmp!=NULL) {
			struct ProcessStats *ps = tmp->data;
			assert(ps);
			assert(ps->pid > 1);
			printf("\tps addr:%p  pid:%d stime:%ld utime:%ld \n",ps, ps->pid, ps->stime, ps->utime );
			tmp=tmp->next;
		}
	}
}

void difference_in_times(Processes *p_old, Processes *p_new, struct ProcessStats *results){
	assert(p_old);
	assert(p_new);
	assert(results);

	// printf("p_old\n");
	// print_proclist2(p_old);
	// printf("p_new\n");
	// print_proclist2(p_new);

	llprintf("results: %p\n", results);
	llprintf("before stime:%ld \n", results->stime);
	llprintf("before utime:%ld \n", results->utime);

	long at_begining = results->stime + results->utime;

	for (int i = 0; i < PIDHASH_SZ; ++i){
		if (! p_old->list[i]){
			continue;
		}

		llprintf("bucket:%d\n",i);

		struct list_node *tmp = p_old->list[i]->first;
		while(tmp!=NULL) {
			llprintf("tmp:%p\n",tmp);
			struct ProcessStats *older = tmp->data;
			struct ProcessStats *newer = NULL;

			llprintf("pid_old:%ld\n", (long) older->pid);

			if ( (newer = xlocate_elem(p_new->list[i], older, 0, sizeof(pid_t))) != NULL){
				assert(newer != older);
				assert(older->pid == newer->pid);
				assert(newer->utime >= older->utime);
				assert(newer->stime >= older->stime);
				results->utime += newer->utime - older->utime;
				results->stime += newer->stime - older->stime;
			}

			tmp=tmp->next;
		}
	}

	llprintf("Results stime:%ld \n", results->stime);
	llprintf("Results utime:%ld \n", results->utime);

	assert(results->utime + results->stime >= at_begining);
}

void delete_proclist(Processes *our_processes){
	assert(our_processes);

	for (int i = 0; i < PIDHASH_SZ; ++i){
		if (! our_processes->list[i]){
			continue;
		}

		destroy_list(our_processes->list[i]);
		free(our_processes->list[i]);
	}
}
