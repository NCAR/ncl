
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/VarArg.h>

#include "defs.h"
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
extern int errno;


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

typedef struct str_load_list NclStrLoadList;
typedef struct file_load_list NclFileLoadList;

FILE *error_fp = stderr;
FILE *stdout_fp = stdout;
FILE *stdin_fp = stdin;
int pager_id;

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

void _NclSetPrintFunc
#if	NhlNeedProto
(NclVaPrintFunc thepit)
#else
(thepit)
NclVaPrintFunc thepit;
#endif
{
	pit = thepit;
}
#if     defined(SunOS) && (MAJOR == 4)
extern FILE *nclin;
#else
extern FILE *yyin;
#endif
extern char *cur_load_file;
extern int loading;
extern int cur_line_number;

#ifdef MAKEAPI
NclStrLoadList str_stack ;
extern char *the_input_buffer;
extern char *the_input_buffer_ptr;
extern int the_input_buffer_size;


void _NclPushNewInputStr
#if	NhlNeedProto
(char* tmp_input,const char* name,int size,int cline_number)
#else
(tmp_input,name,size,cline_number)
	char *tmp_input;
	char *name;
	int  size;
	int  cline_number;
#endif
{
	static int first = 1;
	NclStrLoadList *tmp;
	if(first) {
		str_stack.buffer = NULL;
		str_stack.size = -1;
		str_stack.file_name = NULL;
		str_stack.ptr = NULL;
		str_stack.next = NULL;
		str_stack.cur_line_number = -1;
		first = 0;
	}
	tmp = (NclStrLoadList*)NclMalloc((unsigned)sizeof(NclStrLoadList));
	tmp->size = the_input_buffer_size;
	tmp->buffer = the_input_buffer;
	tmp->ptr = the_input_buffer_ptr;
	tmp->cur_line_number = cline_number;
	tmp->file_name = cur_load_file;
	tmp->next = str_stack.next;
	str_stack.next = tmp;

	if(name != NULL) {
		cur_load_file = NclMalloc((unsigned)strlen(name)+1);
		strcpy(cur_load_file,name);
	} else {
		cur_load_file = NULL;
	}
	the_input_buffer = the_input_buffer_ptr = tmp_input;
	the_input_buffer_size = size;
	return;
}
char *_NclPopInputStr
#if	NhlNeedProto
(void)
#else
()
#endif
{
	
	NclStrLoadList *tmp;

	tmp = str_stack.next;
	NclFree(the_input_buffer);
	the_input_buffer = tmp->buffer;
	cur_line_number = tmp->cur_line_number;
	the_input_buffer_ptr = tmp->ptr;
	cur_load_file = tmp->file_name;
	str_stack.next = str_stack.next->next;
	NclFree(tmp);
	loading--;
	return(the_input_buffer);
}
#endif


NclFileLoadList file_list;

void _NclPushNewInputFile
#if	NhlNeedProto
(FILE *fp,const char *name,int cline_number)
#else
(fp,name,cline_number)
FILE *fp;
char *name;
int cline_number;
#endif
{
	static int first = 1;
	NclFileLoadList *tmp;
	
	if(first) {
		file_list.fp = NULL;
		file_list.file_name = NULL;
		file_list.next = NULL;
		file_list.cur_line_number = -1;
		first = 0;
	}

	tmp = (NclFileLoadList*)NclMalloc((unsigned)sizeof(NclFileLoadList));
#if     defined(SunOS) && (MAJOR == 4)
	tmp->fp = nclin;
#else
	tmp->fp = yyin;
#endif
	tmp->file_name = cur_load_file;
	tmp->cur_line_number = cur_line_number;
	tmp->next = file_list.next;
	file_list.next = tmp;
	if(name != NULL) {
		cur_load_file = NclMalloc((unsigned)strlen(name)+1);
		strcpy(cur_load_file,name);
	} else {
		cur_load_file = NULL;
	}
#if     defined(SunOS) && (MAJOR == 4)
	nclin = fp;
#else
	yyin = fp;
#endif
	cur_line_number = 0;
	

	return;
}

FILE *_NclPopInputFile
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclFileLoadList *tmp;
	FILE *fp;

	tmp = file_list.next;
	fp = tmp->fp;
	cur_load_file = tmp->file_name;
	cur_line_number = tmp->cur_line_number;
	file_list.next = file_list.next->next;
	NclFree(tmp);
	return(fp);
}



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
	char buffer[2048];
	int ret = 0;
	int n;
	int id,status;

	n = vsprintf(buffer,fmt,ap);
	ret = write(fileno(fp),(void*)buffer,n);
	if(ret < 0) {
		_NclSetPrintFunc(vfprintf);
		signal(SIGPIPE,SIG_DFL);
		if(pager_id != -1) {
			while(( id = wait(&status)) != pager_id) {
				if(errno == ECHILD) {
					pager_id = -1;
					break;
				}
			}
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

	_NclSetPrintFunc(vfprintf);
	close(fileno(stdout_fp));
	signal(SIGPIPE,SIG_DFL);
	if(pager_id != -1) {
		while(( id = wait(&status)) != pager_id);
	}
	stdout_fp = stdout;
	pager_id = -1;
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

	
	ret = pipe(fildes);
	id = fork();
	if(id == 0) {
		close(fildes[1]);	
		close(fileno(stdin));
		new_pipe_fd = dup(fildes[0]);
		close(fildes[0]);
		if(fdopen(new_pipe_fd,"rw") == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error Forking pager");
			exit(0);
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
		execlp(pager,arg0,NULL);
		close(new_pipe_fd);
		exit(0);
	} else {
		signal(SIGPIPE,SIG_IGN);
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
}
