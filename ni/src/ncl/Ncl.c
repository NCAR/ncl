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
#include "NclType.h"
#include "TypeSupport.h"
#include <unistd.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/Error.h>
#include <ncarg/hlu/App.h>
#include <netcdf.h>

FILE *thefptr;
FILE *theoptr;
int cmd_line;
extern int cur_line_number;
extern char *cur_line_text;
extern int cur_line_maxsize;
extern char *cur_line_text_pos;

/*
#if     defined(SunOS) && (MAJOR == 4)
extern FILE *nclin;
extern int nclparse(int);
#else
*/
extern FILE *yyin;
extern int yyparse(int);
/*
#endif 
*/


#define BUFF_SIZE 512

extern FILE *error_fp;
extern FILE *stdout_fp ;
extern FILE *stdin_fp ;
extern int number_of_constants;


extern void nclprompt(
#if	NhlNeedProto
void * user_data,
int arg
#endif
);

extern void InitializeReadLine(
#if	NhlNeedProto
int opt
#endif
);

main() {

	int errid = -1;
	int appid;
#ifdef YYDEBUG
	extern int yydebug;
	yydebug = 1;
#endif
	ncopts = NC_VERBOSE;

	cmd_line =isatty(fileno(stdin));

	error_fp = stderr;
	stdout_fp = stdout;

/*
	stdout_fp = fopen("/dev/null","w");
*/

	stdin_fp = stdin;
	cur_line_text = NclMalloc((unsigned)512);
	cur_line_maxsize = 512;
	cur_line_text_pos = &(cur_line_text[0]);


#ifdef NCLDEBUG
	thefptr = fopen("ncl.tree","w");
	theoptr = fopen("ncl.seq","w");
#else
	thefptr = NULL;
	theoptr = NULL;
#endif
	NhlInitialize();
	NhlVACreate(&appid,"ncl",NhlappClass,NhlDEFAULT_APP,
		NhlNappDefaultParent,1,
		NhlNappUsrDir,"./",NULL);
	errid = NhlErrGetID();
	NhlVASetValues(errid,
		NhlNerrFilePtr,stdout,NULL);
	_NclInitMachine();
	_NclInitSymbol();	
	_NclInitTypeClasses();
	_NclInitDataClasses();

	_NhlRegSymConv(NULL,NhlTGenArray,NhlTNclData,NhlTGenArray,NhlTGenArray);
/*
	if(cmd_line)	
		fprintf(stdout_fp,"ncl %d> ",0);
*/
	if(cmd_line == 1) {
		InitializeReadLine(1);
		NclSetPromptFunc(nclprompt,NULL);
		cmd_line = 1;
	} else {
		InitializeReadLine(0);
	}

/*
#if     defined(SunOS) && (MAJOR == 4)
	nclparse(1);
#else
*/
	yyparse(1);
/*
#endif
*/
#ifdef NCLDEBUG
	fclose(thefptr);
	fprintf(stdout,"Number of unfreed objects %d\n",_NclNumObjs());
	_NclObjsSize(stdout);
	_NclNumGetObjCals(stdout);
	_NclPrintUnfreedObjs(theoptr);
	fprintf(stdout,"Number of constants used %d\n",number_of_constants);
	fclose(theoptr);
#endif
	NhlClose();
	exit(0);
}





#ifdef __cplusplus
}
#endif
