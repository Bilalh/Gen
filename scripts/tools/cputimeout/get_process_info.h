#ifndef CPU_TIME_HH
#define CPU_TIME_HH

#include <unistd.h>
#include <limits.h>
#include <dirent.h>
#include <stdbool.h>
#include "list.h"

struct ProcessStats {
	pid_t pid;
    long utime;
    long stime;
};

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
#    define HZ ((double)_BSD_CLK_TCK_)
#   endif
#  else /* CLK_TCK */
#   define HZ ((double)CLK_TCK)
#  endif
# endif
#endif


struct OtherProcess{
	pid_t pid;
	pid_t parent;
	long last_updated;
};

// from cpulimit
#define PIDHASH_SZ 128
#define pid_hashfn(x) ((((x) >> 8) ^ (x)) & (PIDHASH_SZ - 1))

// list of ourProcesses
struct list *our_processes[PIDHASH_SZ];

bool update_process_stats(struct ProcessStats *p);

bool update_our_processes(pid_t monitored_pid);

#endif