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
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <dlfcn.h>




FILE *thefptr;
FILE *theoptr;
int cmd_line;
extern int cmd_line_is_set;
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

extern NhlErrorTypes _NclPreLoadScript(
#if     NhlNeedProto
char * path
#endif
);


main() {

	int errid = -1;
	int appid;
	int k;
	int reset = 1;
	DIR *d;
	struct dirent *ent;
	void *so_handle;
	char buffer[4*NCL_MAX_STRING];
	void (*init_function)(void);
	char *libpath;
	char *scriptpath;
	char *pt;

#ifdef YYDEBUG
	extern int yydebug;
	yydebug = 1;
#endif
	ncopts = NC_VERBOSE;

	cmd_line =isatty(fileno(stdin));

	error_fp = stderr;
	stdout_fp = stdout;
/*
         k = (mode_t)umask(22);
	fprintf(stdout,"%d\n",k);

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
* Now handle default directories
*/
	if((libpath = getenv("NCL_DEF_LIB_DIR"))!=NULL) {
		d = opendir(_NGResolvePath(libpath));
		if(d != NULL) {
			while((ent = readdir(d)) != NULL) {
				if(*ent->d_name != '.') {
					sprintf(buffer,"%s/%s",_NGResolvePath(libpath),ent->d_name);
					so_handle = dlopen(buffer,RTLD_NOW);
					if(so_handle != NULL) {
						init_function = dlsym(so_handle, "Init");
						if(init_function != NULL) {
							(*init_function)();
						} else {
							dlclose(so_handle);
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not find Init() in external file %s, file not loaded",buffer);
						}
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN," Could not open (%s), possibly not a shared object",buffer);
					}
				}
			}
		} else {
			closedir(d);
			NhlPError(NhlFATAL,NhlEUNKNOWN," Could not open (%s), no libraries loaded",libpath);
		}
	}
/*
	if(cmd_line)	
		fprintf(stdout_fp,"ncl %d> ",0);
*/
	if(cmd_line == 1) {
		InitializeReadLine(1);
		NclSetPromptFunc(nclprompt,NULL);
		cmd_line = 1;
		cmd_line_is_set = 1;
	} else {
		InitializeReadLine(0);
	}
	if((scriptpath = getenv("NCL_DEF_SCRIPTS_DIR"))!=NULL) {
		d = opendir(_NGResolvePath(scriptpath));
		if(d!= NULL) {
			while((ent = readdir(d)) != NULL) {
				if(*ent->d_name != '.') {
					sprintf(buffer,"%s/%s",_NGResolvePath(scriptpath),ent->d_name);
					pt = strrchr(buffer,'.');
					if(pt != NULL) {
						pt++;
						if(strncmp(pt,"ncl",3)==0) {
							if(_NclPreLoadScript(buffer) == NhlFATAL) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Error loading default script");
							} else {
								yyparse(reset);
/*
								if(reset)
									reset = 0;
*/
							}
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Scripts must have the \".ncl\" file extension");
						}
					} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Scripts must have the \".ncl\" file extension");
					}
				}
			}
		} else {
			closedir(d);
                        NhlPError(NhlFATAL,NhlEUNKNOWN," Could not open (%s), no scripts loaded",scriptpath);
		}

	}
/*
#if     defined(SunOS) && (MAJOR == 4)
	nclparse(1);
#else
*/
	yyparse(reset);
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
