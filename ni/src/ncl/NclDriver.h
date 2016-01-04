#ifndef _NclDriver_H
#define _NclDriver_H

#include   <stdio.h>
#include   <sys/types.h>
#include   <sys/stat.h>
#include   <fcntl.h>
#include   <unistd.h>
#include   <string.h>
#include   <strings.h>
#include   <dirent.h>
#include   <stdlib.h>
#include   <ctype.h>

#include   <ncarg/hlu/hlu.h>
#include   <ncarg/hlu/NresDB.h>
#include   <ncarg/hlu/Workstation.h>
#include   "defs.h"
#include   "NclExitCode.h"
#include   "Symbol.h"
#include   "NclData.h"
#include   "Machine.h"
#include   "DataSupport.h"
#include   "NclVar.h"
#include   "NclType.h"
#include   "TypeSupport.h"
#include   "NclProf.h"
#include   <ncarg/hlu/ConvertP.h>
#include   <ncarg/hlu/Error.h>
#include   <ncarg/hlu/App.h>
#include   <netcdf.h>
#ifdef _OPENMP
#include   <omp.h>
#endif

#if defined(HPUX)
#include   <dl.h>
#else
#include   <dlfcn.h>
#endif /*HPUX */

#include "NclGlobalParams.h"

extern char *nclf;

extern int NclReturnStatus;

extern NhlClass NhlworkstationClass;

/* for debugging/stack tracing */
extern FILE    *thefptr;
extern FILE    *theoptr;

extern int cmd_line;

extern int  cmd_line_is_set;
extern int  cur_line_number;
extern char *cur_line_text;
extern int  cur_line_maxsize;
extern char *cur_line_text_pos;
extern char *cur_load_file;

extern FILE *yyin;
extern int yyparse(int);

#define    BUFF_SIZE   512

extern int  number_of_constants;

extern void InitStdStreams(FILE* inp, FILE* out, FILE* err);
extern void nclprompt(void *user_data, int arg);
extern void InitializeReadLine(int opt);
extern void NclSetPromptFunc(NclPromptFunc prmf, void *user_data);

extern NhlErrorTypes _NclPreLoadScript(char *path, int status);

#endif

