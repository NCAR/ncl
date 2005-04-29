# ifdef __cplusplus
extern "C" {
# endif
# include   <stdio.h>
# include   <sys/types.h>
# include   <sys/stat.h>
# include   <fcntl.h>
# include   <unistd.h>
# include   <string.h>
# include   <dirent.h>

# include   <ncarg/hlu/hlu.h>
# include   <ncarg/hlu/NresDB.h>
# include   "defs.h"
# include   "Symbol.h"
# include   "NclData.h"
# include   "Machine.h"
# include   "DataSupport.h"
# include   "NclVar.h"
# include   "NclType.h"
# include   "TypeSupport.h"
# include   <ncarg/hlu/ConvertP.h>
# include   <ncarg/hlu/Error.h>
# include   <ncarg/hlu/App.h>
# include   <netcdf.h>

# if defined(HPUX)
# include   <dl.h>
# else
# include   <dlfcn.h>
# endif /*HPUX */

extern NhlClass NhlworkstationClass;

/* for debugging/stack tracing */
FILE    *thefptr;
FILE    *theoptr;

int cmd_line;

extern int  cmd_line_is_set;
extern int  cur_line_number;
extern char *cur_line_text;
extern int  cur_line_maxsize;
extern char *cur_line_text_pos;

extern FILE *yyin;
extern int yyparse(int);

# define    BUFF_SIZE   512

extern FILE *error_fp;
extern FILE *stdout_fp ;
extern FILE *stdin_fp ;
extern int  number_of_constants;


extern void nclprompt(
# if	NhlNeedProto
    void * user_data,
    int arg
# endif
);

extern void InitializeReadLine(
# if	NhlNeedProto
    int opt
# endif
);

extern NhlErrorTypes _NclPreLoadScript(
# if     NhlNeedProto
    char *  /* path */,
    int     /* status */
# endif
);


/*
 * Variables for command line options/arguments
 */
char    *myName;        /* argv[0]: program name (should be 'ncl') */
char    **NCL_ARGV;
int NCL_ARGC;

char    *s;
char    *t;
size_t  l;
char    *f;

char    **cargs;
int nargs = 0;

short   NCLverbose = 0;
short   NCLecho = 0;


int
main(int argc, char* argv[]) {

	int errid = -1;
	int appid;
	int i, j,
        k = 0;
	int reset = 1;
	DIR *d;
	struct dirent   *ent;
# if defined(HPUX)
	shl_t so_handle;
# else
	void *so_handle;
# endif
	char buffer[4 * NCL_MAX_STRING];
	void (*init_function) (void);
	char    *libpath;
	char    *scriptpath;
	char    *pt;
	char    *tmp = NULL;

    FILE    *tmpf;
    char    *tmpd;

# ifdef YYDEBUG
	extern int yydebug;
	yydebug = 1;
# endif

    error_fp = stderr;
    stdout_fp = stdout;
    stdin_fp = stdin;
	
    ncopts = NC_VERBOSE;

    cmd_line =isatty(fileno(stdin));

    myName = malloc(strlen(argv[0]));
    (void) strcpy(myName, argv[0]);

    /*
     * NCL argv, for command line processing
     */
    NCL_ARGV = (char **) malloc(argc  * sizeof(char *));
    for (i = 0; i < argc; i++) {
        NCL_ARGV[i] =  (char *) malloc((strlen(argv[i]) + 1) * sizeof(char *));
        (void) strcpy(NCL_ARGV[i], argv[i]);
    }
    NCL_ARGC = argc;

# ifdef NCLDEBUG
    for (i = 0; i < argc; i++, *argv++)
        (void) printf("NCL_ARGV[%d] = %s\n", i, *argv);
# endif /* NCLDEBUG */

    for (i = 1; i < NCL_ARGC; i++) {
        s = strchr(NCL_ARGV[i], '-');
        if (s != NULL)
            s++;
        else {
            /* no '-' sign, file arg? (<name>.ncl) */
            s = strstr(NCL_ARGV[i], ".ncl");
            if (s != NULL)
                f = NCL_ARGV[i];
            else {
                /* user-defined argument */
                if (nargs == 0)
                    cargs = (char **) malloc(sizeof(char *));
                else
                    cargs = (char **) realloc(cargs, sizeof(char *));
    
                /* need two (2) extra spaces for "\n\n" for arg stack */
                cargs[nargs] = (char *) malloc((strlen(NCL_ARGV[i]) + 2) * sizeof(char *));
                (void) sprintf(cargs[nargs], "%s\n\n", NCL_ARGV[i]);
                nargs++;
            }
            continue;
        }

        t = strchr(NCL_ARGV[i], '=');
        if (t != NULL) {
            l = strcspn(s, "=");
            t++;
        } else {
            /*
             * Got "-" sign(s)
             * Defined arguments
             *
             *  -x      echo: turns on command echo
             *  -v      verbose: turns on verbose mode
             *  -V      version: currently same as 'verbose'
             *
             * Take into account a combination of arguments (i.e. -xv or -vx)
             */
            while (strlen(s)) {
                if (!strncmp(s, "x", 1)) {
                    NCLecho = 1;
                    s++;
                    continue;
                }

                if (!strncmp(s, "v", 1)) {
                    NCLverbose = 1;
                    s++;
                    continue;
                }

                if (!strncmp(s, "V", 1)) {
                    NCLverbose = 1;
                    s++;
                    continue;
                }
            }
        }
    }

    error_fp = stderr;
    stdout_fp = stdout;
    
    if (NCLverbose) {
    	(void) fprintf(stdout," Copyright (C) 1995-2004 - All Rights Reserved   \n University Corporation for Atmospheric Research   \n NCAR Command Language Version %s   \n The use of this software is governed by a License Agreement.\n See http://ngwww.ucar.edu/ncl/ for more details.\n", GetNCARGVersion());
    }

    stdin_fp = stdin;
    cur_line_text = NclMalloc((unsigned int) 512);
    cur_line_maxsize = 512;
    cur_line_text_pos = &(cur_line_text[0]);

# ifdef NCLDEBUG
    thefptr = fopen("ncl.tree", "w");
    theoptr = fopen("ncl.seq", "w");
# else
    thefptr = NULL;
    theoptr = NULL;
# endif /* NCLDEBUG */

    NhlInitialize();
    NhlVACreate(&appid, "ncl", NhlappClass, NhlDEFAULT_APP,
        NhlNappDefaultParent, 1, NhlNappUsrDir, "./", NULL);
    NhlPalLoadColormapFiles(NhlworkstationClass);
    errid = NhlErrGetID();
    NhlVAGetValues(errid, NhlNerrFileName, &tmp, NULL);
	
    if ((tmp == NULL)  ||  (!strcmp(tmp, "stderr")))
        NhlVASetValues(errid, NhlNerrFilePtr, stdout, NULL);

    _NclInitMachine();
    _NclInitSymbol();	
    _NclInitTypeClasses();
    _NclInitDataClasses();

/*
 * Handle default directories
 */
    if ((libpath = getenv("NCL_DEF_LIB_DIR")) != NULL) {
        d = opendir(_NGResolvePath(libpath));
        if (d != NULL) {
            while((ent = readdir(d)) != NULL) {
                if (*ent->d_name != '.') {
                    (void) sprintf(buffer, "%s/%s", _NGResolvePath(libpath), ent->d_name);
# if defined (HPUX)
                    so_handle = shl_load(buffer, BIND_IMMEDIATE, 0L);
# else
                    so_handle = dlopen(buffer, RTLD_NOW);
                    if (so_handle == NULL) {
                        NhlPError(NhlFATAL, NhlEUNKNOWN,
                            "Could not open (%s): %s.", buffer, dlerror());
                    }
# endif /* HPUX */
                    if (so_handle != NULL) {
# if defined (HPUX)
                        init_function = NULL;
                        (void) shl_findsym(&so_handle, "Init",
                                TYPE_UNDEFINED, (void *) &init_function);
# else
                        init_function = dlsym(so_handle, "Init");
# endif /* HPUX */
                        if (init_function != NULL) {
                            (*init_function)();
                        } else {
# if defined (HPUX)
                            shl_unload(so_handle);
# else
                            dlclose(so_handle);
# endif /* HPUX */
                            NhlPError(NhlWARNING, NhlEUNKNOWN,
                                "Could not find Init() in external file %s, file not loaded.",
                                buffer);
                        }
                    } 
                }
            }
        } else {
            closedir(d);
            NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not open (%s), no libraries loaded.", libpath);
        }
        _NclResetNewSymStack();
    }

    if (cmd_line == 1) {
        InitializeReadLine(1);
        NclSetPromptFunc(nclprompt,NULL);
        cmd_line = 1;
        cmd_line_is_set = 1;
    } else {
        InitializeReadLine(0);
    }
	
    /* Load default scripts */
    if ((scriptpath = getenv("NCL_DEF_SCRIPTS_DIR")) != NULL) {
        d = opendir(_NGResolvePath(scriptpath));
        if (d!= NULL) {
            while((ent = readdir(d)) != NULL) {
                if (*ent->d_name != '.') {
                    (void) sprintf(buffer, "%s/%s", _NGResolvePath(scriptpath), ent->d_name);
                    pt = strrchr(buffer, '.');
                    if (pt != NULL) {
                        pt++;
                        if (strncmp(pt, "ncl", 3) == 0) {
                            if (_NclPreLoadScript(buffer, 1) == NhlFATAL) {
                                NhlPError(NhlFATAL, NhlEUNKNOWN, "Error loading default script.");
                            } else {
                                yyparse(reset);
                            }
                        } else {
                            NhlPError(NhlFATAL, NhlEUNKNOWN,
                                "Scripts must have the \".ncl\" file extension.");
                        }
                    } else {
                        NhlPError(NhlFATAL, NhlEUNKNOWN,
                            "Scripts must have the \".ncl\" file extension.");
                    }
                }
            }
        } else {
            (void) closedir(d);
            NhlPError(NhlFATAL, NhlEUNKNOWN, "Could not open (%s), no scripts loaded.", scriptpath);
        }
    }

    /*
     * Create the new args.
     *
     * Ideally this would be done using calls to the parser/stack engine but there is
     * no clean interface to that process.  Investigate _NclParseString() in the future.
     *
     * For now, create a temporary file with NCL commands and execute it.
     */
    if (nargs) {
        cmd_line = 0;   /* non-interactive */
        tmpd = (char *) _NGGetNCARGEnv("tmp");      /* defaults to: /tmp */
        (void) sprintf(buffer, "%s/ncl%d.ncl", tmpd, getpid());

        tmpf = fopen(buffer, "w");
        for (k = 0; k < nargs; k++) {
            (void) fwrite(cargs[k], strlen(cargs[k]), 1, tmpf);
        }
        (void) fclose(tmpf);
        
        if (_NclPreLoadScript(buffer, 1) == NhlFATAL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN, "Error initializing command line arguments.");
            (void) unlink(buffer);
        } else {
            yyparse(reset);
        }

        (void) unlink(buffer);
        cmd_line = 1;       /* reset to default: interactive */
    }

    if (f != (char *) NULL) {
        (void) strcpy(buffer, _NGResolvePath(f));
        if (_NclPreLoadScript(buffer, 0) == NhlFATAL)
            NhlPError(NhlFATAL, NhlEUNKNOWN, "Error loading provided NCL script.");
        else
            yyparse(reset);
    } else {
        yyparse(reset);
    }

# ifdef NCLDEBUG
	fclose(thefptr);
	fprintf(stdout,"Number of unfreed objects %d\n",_NclNumObjs());
	_NclObjsSize(stdout);
	_NclNumGetObjCals(stdout);
	_NclPrintUnfreedObjs(theoptr);
	fprintf(stdout,"Number of constants used %d\n",number_of_constants);
	fclose(theoptr);
# endif
	NhlClose();
	return;
    exit(0);
}

# ifdef __cplusplus
}
# endif
