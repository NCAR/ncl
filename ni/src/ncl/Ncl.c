#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <defs.h>
#include <Symbol.h>
#include <y.tab.h>
#include <Machine.h>

extern FILE *yyin;
FILE *thefptr;
FILE *theoptr;
int cmd_line;

#define BUFF_SIZE 512

main() {

	FILE *fp;
	char buffer[BUFF_SIZE];
	int i,j=0;
	int linenumber = 0;
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
		fprintf(stdout,"ncl 1> ");
	yyparse();
	fclose(thefptr);
	exit(0);
}
