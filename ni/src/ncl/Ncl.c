#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <defs.h>
#include <Symbol.h>
#include <y.tab.h>
#include <Machine.h>

extern FILE *yyin;
FILE *thefptr;
FILE *theoptr;
int cmd_line =1;

#define BUFF_SIZE 512

main() {

	int fd[2];
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

	thefptr = fopen("ncl.tree","w");
	theoptr = fopen("ncl.seq","w");
	_NclInitSymbol();	
	_NclInitMachine();
	
	fprintf(stdout,"ncl 1> ");
	yyparse();
	fclose(thefptr);
	exit(0);
}
