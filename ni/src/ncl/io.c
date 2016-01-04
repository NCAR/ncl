#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>

#include "defs.h"
#include "Symbol.h"
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>

extern int errno;
extern int cmd_line;
int	cmd_line_is_set = 0;

#if 0
#define DONTUSEPAGER 1
#endif

struct str_load_list {
	char 	*buffer;
	int 	size;
	char	*ptr;
	int	cur_line_number;
	char 	*file_name;
	struct str_load_list *next;
};
struct file_load_list {
	FILE 	*fp;
	int	cur_line_number;
	char 	*file_name;
	struct file_load_list *next;
};

struct ninbuf{
	int using_buffer;
	char *buffer;
	char *ptr;
	int  size;
};

typedef struct str_load_list NclStrLoadList;
typedef struct file_load_list NclFileLoadList;
typedef struct ninbuf NclInputBuffer;

FILE *error_fp = NULL;
FILE *stdout_fp = NULL;
FILE *stdin_fp = NULL;
int pager_id;

void *prompt_user_data = NULL;
NclPromptFunc prompt = NULL;
extern char *readline();
extern void add_history();

NclInputBuffer ncl_input_buffer;
static int child_status = -1; 

static void child_stat_notify(int tmp)
{
	child_status = 0;
} 

void InitStdStreams(FILE* in, FILE* out, FILE* err) 
{
    stdin_fp = in;
    stdout_fp = out;
    error_fp = err;
}

void InitializeReadLine
#if	NhlNeedProto
(int opt)
#else
(opt)
int opt;
#endif
{
	ncl_input_buffer.using_buffer = opt;
	ncl_input_buffer.buffer = NULL;
	ncl_input_buffer.ptr = NULL;
	ncl_input_buffer.size = 0;
}
void nclprompt
#if     NhlNeedProto
(void * user_data,int arg)
#else
( user_data,arg)
void * user_data;
int arg;
#endif
{
	char prmpt[10];
	sprintf(prmpt,"ncl %d> ",arg);
	if (ncl_input_buffer.using_buffer) {
		if(ncl_input_buffer.buffer != NULL)
			NclFree(ncl_input_buffer.buffer); 
		ncl_input_buffer.buffer = readline(prmpt);
		if (ncl_input_buffer.buffer!=NULL) {
			ncl_input_buffer.size = strlen(ncl_input_buffer.buffer);
			add_history(ncl_input_buffer.buffer);
		}
		ncl_input_buffer.ptr = ncl_input_buffer.buffer;
	} 
}
char ncl_getc
#if	NhlNeedProto
(FILE *fp)
#else
(fp)
FILE *fp;
#endif
{
	if(ncl_input_buffer.using_buffer){
		if(ncl_input_buffer.buffer != NULL) {
			if(*ncl_input_buffer.ptr == '\0') {
				ncl_input_buffer.ptr++;
				return('\n');
			} else {
				return(*ncl_input_buffer.ptr++);
			}
		} else  {
			return((char)0);
		}
	} else {
		return(getc(fp));
	}
}

void NclSetPromptFunc
#if	NhlNeedProto
(NclPromptFunc prmf, void* user_data)
#else
(prmf,user_data)
#endif
{
	prompt = prmf;
	prompt_user_data = user_data;
	cmd_line = 2;
	cmd_line_is_set = 2;
}

void _NclCallPromptFunc
#if 	NhlNeedProto
(int lineno)
#else
(lineno)
int lineno;
#endif
{
	if(prompt != NULL) {
		(*prompt)(prompt_user_data,lineno);
	} else {
		fprintf(stdout,"ncl %d>",lineno);
	}
}

FILE *_NclGetErrorStream
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return(error_fp);
}
FILE *_NclGetInputStream
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return(stdin_fp);
}

void NclSetOutputStream
#if	NhlNeedProto
(
	FILE	*out
)
#else
(out)
	FILE	*out;
#endif
{
	stdout_fp = out;
}

FILE *_NclGetOutputStream
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return(stdout_fp);
}
/*
int (*pit)(
FILE *,
const char *,
va_list 
);
*/
NclVaPrintFunc pit = vfprintf;
int nclfprintf
#if NhlNeedVarArgProto
(FILE *fp,char *fmt,...)
#else
(fp,format,va_alist)
	FILE *fp;
	char *fmt;
	va_dcl
#endif
{
	int ret = 0;
	va_list ap;
	VA_START(ap,fmt);
	ret = (*pit)(fp,fmt,ap);
	va_end(ap);
	return(ret);
}

NclVaPrintFunc _NclSetPrintFunc
#if	NhlNeedProto
(NclVaPrintFunc thepit)
#else
(thepit)
NclVaPrintFunc thepit;
#endif
{
	NclVaPrintFunc tmp;

	tmp = pit;
	pit = thepit;
	return(tmp);
}
extern FILE *yyin;
extern char *cur_load_file;
extern int loading;
extern int cur_line_number;

int _nclfprintf_pager
#if NhlNeedProto
(FILE *fp,Const char *fmt,va_list ap)
#else
(fp,format,va_alist)
	FILE *fp;
	char *fmt;
	va_list ap;
#endif
{
#if 1
	char buffer[262144];
#else
	char buffer[32768];
#endif

	int ret = 0;
	int n;
	int id,status;

	if(child_status) {
#if     defined(SunOS) && (MAJOR == 4)
		vsprintf(buffer,fmt,ap);
		n = strlen(buffer) + 1;
#else
		n = vsprintf(buffer,fmt,ap);
#endif

		ret = write(fileno(fp),(void*)buffer,n);
	} else {
		ret = -1;
	}
	if(ret < 0) {
		_NclSetPrintFunc(vfprintf);
/*		signal(SIGPIPE,SIG_DFL);*/
		if(pager_id != -1) {
			while(( id = wait(&status)) != pager_id) {
				if(errno == ECHILD) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Pager failed0");
					pager_id = -1;
					break;
				}
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Pager failed1");
		}
		close(fileno(stdout_fp));
		stdout_fp = stdout;
	} 
	return(ret);
}

void _NclEndCmdLinePager
#if NhlNeedProto
(void)
#else
()
#endif
{
	int id;
	int status;
#ifndef DONTUSEPAGER
	_NclSetPrintFunc(vfprintf);
	close(fileno(stdout_fp));
	signal(SIGPIPE,SIG_DFL);
	signal(SIGCHLD,SIG_DFL);
	if(pager_id != -1) {
		while(( id = wait(&status)) != pager_id);
	}
	stdout_fp = stdout;
	pager_id = -1;
#endif
	return;
	
}
void _NclStartCmdLinePager
#if NhlNeedProto
(void)
#else
() 
#endif
{
	int fildes[2],new_pipe_fd;
	int ret;
	int id;
	char *pager =NULL;
	char *arg0 = NULL;
	int tmp = 1;

#ifndef DONTUSEPAGER
	ret = pipe(fildes);
	id = fork();
	if(id == 0) {
		close(fildes[1]);	
		close(fileno(stdin));
		new_pipe_fd = dup(fildes[0]);
		close(fildes[0]);
		if(fdopen(new_pipe_fd,"rw") == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error Forking pager");
			_NclExit(0);
		}
		pager = getenv("PAGER");
		if(pager == NULL) {
			pager = "more";
			arg0 = "more";
		} else {
			arg0 = strrchr(pager,(int)'/');
			if(arg0 == NULL) {
				arg0 = pager;
			} else {
				arg0++;
			}
		}
/*
		while(tmp == 1);
*/
		tmp = execlp(pager,arg0,NULL);
		NhlPError(NhlWARNING,NhlEUNKNOWN,"Error Forking pager check PAGER environment variable and restart, continuing using \"more\"");
		pager = "more";
		arg0 = "more";
		tmp = execlp(pager,arg0,NULL);
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Error Forking pager again, check PATH environment variable, can't continue");
		close(new_pipe_fd);
		/* cannot use exit() if we do not want the atexit functions to get called */
		_exit(1);
	} else {
		child_status = 1;
		signal(SIGPIPE,child_stat_notify);
		signal(SIGCHLD,child_stat_notify);
		close(fildes[0]);
		stdout_fp = fdopen(fildes[1],"w");
		if(stdout_fp == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error Forking pager");
			stdout_fp = stdout;
			pager_id = -1;
			_NclSetPrintFunc(vfprintf);
		} else {
			pager_id = id;
			_NclSetPrintFunc(_nclfprintf_pager);
		}
		return;
	}
#endif
}
