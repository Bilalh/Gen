#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef __APPLE__
#include <sys/procfs.h>
#endif
#include <time.h>



#ifdef __linux__

#include "get_process_info_linux.c"

#elif defined __APPLE__

#include "get_process_info_linux_appl.c"

#else

#error OS not supported

#endif