#include "get_process_info.h"
#include <assert.h>
#include <stdlib.h>


void store_process(Processes *our_processes, pid_t pid, bool replace){
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

		if (update_process_stats(stats)){

			llprintf("bucket:%d pid:%ld stime:%ld utime:%ld\n",hashed, (long)pid, stats->stime, stats->utime );
			add_elem(our_processes->list[hashed], stats);

			int count_after = get_list_count(our_processes->list[hashed]);
			llprintf("added %ld,  count:%d \n", (long)pid, count_after);
			assert(count_after = count_before + 1);
		}else{
			printf("update_process_stats failed for %ld\n", (long)pid);
		}

		// print_proclist(our_processes);
	}

	llprintf("end\n");
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
