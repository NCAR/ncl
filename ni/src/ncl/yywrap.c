#ifndef MAKEAPI

#include <ncarg/hlu/hlu.h>
#include <defs.h>
#include <Symbol.h>
#include <SrcTree.h>
#include <errno.h>
#include <y.tab.h>
extern int loading;
extern char *cur_load_file;
extern int cur_line_number;
extern int top_level_line;
extern FILE *yyin;
int yywrap() 
{
	if(loading) {
		yyin = stdin;
		loading = 0;
/*
* Yeah I know the loses the pointer but the allocated string must
* remain arround for error reporting even after the file
* has been loaded. This happens because load statments inside
* of a block are not executed untill the end of the block 
*/
		cur_line_number = top_level_line;
		
		cur_load_file = NULL;
		return(0);
	} else {
		return(1);
	}
}
#endif
