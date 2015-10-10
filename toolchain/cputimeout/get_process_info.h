#ifndef CPU_TIME_HH
#define CPU_TIME_HH

#include <unistd.h>
#include <limits.h>
#include <dirent.h>
#include <stdbool.h>
#include <stdio.h>
#include "list.h"
#include <libgen.h>

// from cpulimit
#define PIDHASH_SZ 8
#define pid_hashfn(x) ((((x) >> 8) ^ (x)) & (PIDHASH_SZ - 1))

//TODO bad idea?
typedef struct list *ProcList[PIDHASH_SZ];

struct ProcessStats {
	pid_t pid;
    long utime;
    long stime;
};

typedef struct Processes{
	ProcList list;
} Processes;


// from openssl
#ifndef HZ
# if defined(_SC_CLK_TCK) \
     && (!defined(OPENSSL_SYS_VMS) || __CTRL_VER >= 70000000)
#  define HZ ((double)sysconf(_SC_CLK_TCK))
# else
#  ifndef CLK_TCK
#   ifndef _BSD_CLK_TCK_ /* FreeBSD hack */
#    define HZ  100.0
#   else /* _BSD_CLK_TCK_ */
#    define HZ ((double)_BSD_CLK_T			CK_)
#   endif
#  else /* CLK_TCK */
#   define HZ ((double)CLK_TCK)
#  endif
# endif
#endif


#ifdef DEBUG

#define llprintf(fmt, ...) \
	printf("%13s:%20s:%i%s" fmt , basename(__FILE__),__func__, __LINE__,"   " ,## __VA_ARGS__);
#else
	#define llprintf(fmt, ...)
#endif


void store_process(Processes *our_processes, pid_t pid, bool replace);

bool update_our_processes(Processes *our_starting, Processes *out_current, pid_t monitored_pid);

void difference_in_times(Processes *old_times, Processes *new_times, struct ProcessStats *results);

bool update_process_stats(struct ProcessStats *p);

void delete_proclist(Processes *our_processes);

void print_proclist(Processes *our_processes);
void print_proclist2(Processes *our_processes);


#endif