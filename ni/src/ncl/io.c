
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/VarArg.h>
#include "defs.h"

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

FILE *_NclGetErrorStream
#if __STDC__
(void)
#else
()
#endif
{
	return(error_fp);
}
FILE *_NclGetInputStream
#if __STDC__
(void)
#else
()
#endif
{
	return(stdin_fp);
}
FILE *_NclGetOutputStream
#if __STDC__
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
void nclfprintf
#if NhlNeedVarArgProto
(FILE *fp,char *fmt,...)
#else
(fp,format,va_alist)
	FILE *fp;
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
	VA_START(ap,fmt);
	(*pit)(fp,fmt,ap);
	va_end(ap);
	return;
}

void _NclSetPrintFunc
#if __STDC__
(NclVaPrintFunc thepit)
#else
(thepit)
NclVaPrintFunc thepit;
#endif
{
	pit = thepit;
}
#ifdef SunOS
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
#if __STDC__
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
#if __STDC__
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
#if __STDC__
(FILE *fp,const char *name,int cline_number)
#else
(fp,name,cline_number)
FILE *fp;
char *name;
cline_number;
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
#ifdef SunOS
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
#ifdef SunOS
	nclin = fp;
#else
	yyin = fp;
#endif
	cur_line_number = 0;
	

	return;
}

FILE *_NclPopInputFile
#if __STDC__
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
