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
extern int cmd_line_is_set;
extern FILE *yyin;
extern void _NclResetScanner(
#if NhlNeedProto
void
#endif
);
int yywrap() 
{
#if NCLDEBUG
	fprintf(stderr,"In yywrap\n");
#endif
	if(loading) {
		_NclResetScanner();
		return(0);
	} 
	return(1);
}

#ifdef __cplusplus
}
#endif
