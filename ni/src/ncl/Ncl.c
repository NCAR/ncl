#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "NclData.h"
#include "Machine.h"
#include "DataSupport.h"
#include <unistd.h>
#include <ncarg/hlu/Convert.h>

FILE *thefptr;
FILE *theoptr;
int cmd_line;
extern int cur_line_number;

#ifdef SunOS
extern FILE *nclin;
extern int nclparse();
#else
extern FILE *yyin;
extern int yyparse();
#endif /*SunOs*/

#define BUFF_SIZE 512

static NhlErrorTypes 
NclCvtGenArrayToNclData
#if NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
	return(NhlNOERROR);
}
static NhlErrorTypes 
NclCvtScalarToNclData
#if NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
	return(NhlNOERROR);
}

main() {

#ifdef YYDEBUG
	extern int _yydebug;
/*		
	extern FILE * _yyerfp;
*/
	_yydebug = 1;
/*
	_yyerfp = fopen("ncl.trace","w");
*/

#endif

	cmd_line =isatty(fileno(stdin));

	thefptr = fopen("ncl.tree","w");
	theoptr = fopen("ncl.seq","w");
	NhlOpen();
	_NclInitMachine();
	_NclInitSymbol();	
	_NclInitDataClasses();

	_NhlRegSymConv(NhlTGenArray,NhlTNclData,NhlTGenArray,NhlTGenArray);
/*
	NhlRegisterConverter(NhlTScalar,NhlTNclData,NclCvtScalarToDataObj,NULL,0,False,NULL);
	NhlRegisterConverter(NhlTGenArray,NhlTNclData,NclCvtGenArrayToData,NULL,0,False,NULL);
*/

	if(cmd_line)	
		fprintf(stdout,"ncl %d> ",0);
#ifdef SunOS
	nclparse();
#else
	yyparse();
#endif
	fclose(thefptr);
	fprintf(stdout,"Number of unfreed objects %d\n",_NclNumObjs());
/*
	_NclPrintUnfreedObjs(stdout);
*/
	NhlClose();
	exit(0);
}





#ifdef __cplusplus
}
#endif
