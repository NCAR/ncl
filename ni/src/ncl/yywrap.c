#ifdef __cplusplus
extern "C" {
#endif
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <errno.h>
extern int loading;
extern char *cur_load_file;
extern int cur_line_number;
extern int top_level_line;
extern int cmd_line;

#ifdef SunOS
extern FILE *nclin;
extern int ncllineno;
int nclwrap() 
#else 
extern FILE *yyin;
extern int yylineno;
int yywrap() 
#endif /*SunOs*/

{
	if(loading) {
#ifdef SunOS
		nclin = _NclPopInputFile();
#else
		yyin = _NclPopInputFile();
#endif
		loading -= 1;
/*
* Yeah I know the loses the pointer but the allocated string must
* remain arround for error reporting even after the file
* has been loaded. This happens because load statments inside
* of a block are not executed untill the end of the block 
*/
/*
		cur_line_number = top_level_line;
		cur_load_file = NULL;
*/
		cmd_line = isatty(fileno(yyin));
		if(cmd_line) {
			fprintf(stdout,"ncl %d> ",cur_line_number);

		}
		return(0);
	} else {
		return(1);
	}
}
#ifdef __cplusplus
}
#endif
