#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <data_objs/NclData.h>
#include <defs.h>
#include <Symbol.h>
#include <y.tab.h>
#include <Machine.h>
#include <unistd.h>

extern FILE *yyin;
FILE *thefptr;
FILE *theoptr;
int cmd_line;
extern int cur_line_number;
extern int yyparse();

#define BUFF_SIZE 512

main() {

#ifdef YYDEBUG
	extern int yydebug;
/*		
	extern FILE * yyerfp;
*/
	yydebug = 1;
/*
	yyerfp = fopen("ncl.trace","w");
*/

#endif

	cmd_line =isatty(fileno(stdin));

	thefptr = fopen("ncl.tree","w");
	theoptr = fopen("ncl.seq","w");
	_NclInitMachine();
	_NclInitSymbol();	

	if(cmd_line)	
		fprintf(stdout,"ncl %d> ",0);
	yyparse();
	fclose(thefptr);
	exit(0);
}
#ifdef __cplusplus
}
#endif
