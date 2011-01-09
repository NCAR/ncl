#ifdef __cplusplus

extern "C" {
#endif

#include   <stdio.h>
#include   <sys/types.h>
#include   <sys/stat.h>
#include   <fcntl.h>
#include   <unistd.h>
#include   <string.h>
#include   <strings.h>
#include   <dirent.h>
#include   <stdlib.h>
#include   <ctype.h>

#include   <ncarg/hlu/hlu.h>
#include   <ncarg/hlu/NresDB.h>
#include   <ncarg/hlu/Workstation.h>
#include   "defs.h"
#include   "Symbol.h"
#include   "NclData.h"
#include   "Machine.h"
#include   "DataSupport.h"
#include   "NclVar.h"
#include   "NclType.h"
#include   "TypeSupport.h"
#include   <ncarg/hlu/ConvertP.h>
#include   <ncarg/hlu/Error.h>
#include   <ncarg/hlu/App.h>
#include   <netcdf.h>

#if defined(HPUX)
#include   <dl.h>
#else
#include   <dlfcn.h>
#endif /*HPUX */

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

#define    BUFF_SIZE   512

extern FILE *error_fp;
extern FILE *stdout_fp ;
extern FILE *stdin_fp ;
extern int  number_of_constants;

extern void nclprompt(
#if NhlNeedProto
    void *user_data,
    int arg
#endif /* NhlNeedProto */
);

extern void InitializeReadLine(
#if NhlNeedProto
    int opt
#endif /* NhlNeedProto */
);

extern void NclSetPromptFunc(
#if	NhlNeedProto
NclPromptFunc /*prmf*/, 
void * /*user_data */
#endif
);

extern NhlErrorTypes _NclPreLoadScript(
#if NhlNeedProto
    char *  /* path */,
    int     /* status */
#endif /* NhlNeedProto */
);

/* Command line option variables */
short   NCLverbose = 1;
short   NCLecho = 0;            /* echo typed commands, off by default */
short   NCLoverrideEcho = 0;    /* override echo; non-advertised option */
short   NCLnoCopyright = 0;     /* override copyright notice; non-advertised option */
short   NCLnoPrintElem = 0;     /* don't enumerate values in print() */
short   NCLnoSysPager = 0;      /* don't pipe commands to system() to PAGER */
short   NCLoldBehavior = 0;     /* retain former behavior for certain backwards-incompatible changes */
	                        /* behaviors could be revised after an adoption period */

char    *nclf = NULL;           /* script of NCL commands, may or may not be provided */


int
main(int argc, char **argv) {

    int errid = -1;
    int appid;
    int i, k = 0;
    int reset = 1;
    DIR *d;
    struct dirent   *ent;
#if defined(HPUX)
    shl_t so_handle;
#else
    void *so_handle;
#endif /* defined(HPUX) */

    char buffer[4 * NCL_MAX_STRING];
    void (*init_function) (void);
    char    *libpath;
    char    *scriptpath;
    char    *pt;
    char    *tmp = NULL;

    /*
     * Variables for command line options/arguments
     */
    char    *myName;        /* argv[0]: program name (should be 'ncl') */
    char    **NCL_ARGV;
    int NCL_ARGC;           /* local argv/argc -- future use for NCL scripts? */

    int c;

    char    **cargs = NULL;
    int nargs = 0;

    struct stat sbuf;
    int sr;

    FILE    *tmpf = NULL;   /* file variables for creating arguments */
    char    *tmpd = NULL;

#ifdef YYDEBUG
    extern int yydebug;
    yydebug = 1;
#endif /* YYDEBUG */

    error_fp = stderr;
    stdout_fp = stdout;
    stdin_fp = stdin;
	
    ncopts = NC_VERBOSE;

    cmd_line =isatty(fileno(stdin));
    myName = NclMalloc(strlen(argv[0]) + 1);
    (void) strcpy(myName, argv[0]);

    /*
     * Save NCL argv, for command line processing later use
     */
    NCL_ARGV = (char **) NclMalloc(argc  * sizeof(char *));
    for (i = 0; i < argc; i++) {
        NCL_ARGV[i] =  (char *) NclMalloc((strlen(argv[i]) + 1) * sizeof(char *));
        (void) strcpy(NCL_ARGV[i], argv[i]);
    }
    NCL_ARGC = argc;

#ifdef NCLDEBUG
    for (i = 0; i < NCL_ARGC; i++, *NCL_ARGV++)
        (void) printf("NCL_ARGV[%d] = %s\n", i, *NCL_ARGV);
#endif /* NCLDEBUG */

    /*
     * Defined arguments
     *
     *  -n      element print: don't enumerate elements in print()
     *  -x      echo: turns on command echo
     *  -V      version: output NCARG/NCL version, exit
     *  -o      old behavior: retain former behavior for backwards incompatible changes
     *  -h      help: output options and exit
     *
     *  -X      override: echo every stmt regardless (unannounced option)
     *  -Q      override: don't echo copyright notice (unannounced option)
     */
    opterr = 0;     /* turn off getopt() msgs */
    while ((c = getopt (argc, argv, "hnoxVXQp")) != -1) {
        switch (c) {
            case 'p':
                NCLnoSysPager = 1;
                break;

            case 'n':
                NCLnoPrintElem = 1;
                break;

            case 'o':
                NCLoldBehavior = 1;
                break;

            case 'x':
                NCLecho = 1;
                break;

            /* NOT ADVERTISED!  Will override "no echo" and print EVERYTHING! */
            case 'X':
                NCLoverrideEcho = 1;
                break;

            /* NOT ADVERTISED!  Will not echo copyright notice! */
            case 'Q':
                NCLnoCopyright = 1;
                break;

            case 'V':
                (void) fprintf(stdout, "%s\n", GetNCLVersion());
                exit(0);
                break;

            case 'h':
                (void) fprintf(stdout, "Usage: ncl -hnpxV <args> <file.ncl>\n");
                (void) fprintf(stdout, "\t -n: don't enumerate values in print()\n");
                (void) fprintf(stdout, "\t -p: don't page output from the system() command\n");
                (void) fprintf(stdout, "\t -o: retain former behavior for certain backwards-incompatible changes\n");
                (void) fprintf(stdout, "\t -x: echo NCL commands\n");
                (void) fprintf(stdout, "\t -V: print NCL version and exit\n");
                (void) fprintf(stdout, "\t -h: print this message and exit\n");
                exit(0);
                break;

           case '?':
                if (isprint(optopt))
                    (void) fprintf(stderr, "Unknown option `-%c'\n", optopt);
                else
                    (void) fprintf(stderr, "Unknown option character `\\x%x'\n", optopt);
                break;

            default:
                break;
        }
    }

    /*
     * Announce NCL copyright notice, etc.
     */
    if (!NCLnoCopyright) 
        (void) fprintf(stdout,
            " Copyright (C) 1995-2011 - All Rights Reserved\n University Corporation for Atmospheric Research\n NCAR Command Language Version %s\n The use of this software is governed by a License Agreement.\n See http://www.ncl.ucar.edu/ for more details.\n", GetNCLVersion());

    /* Process any user-defined arguments */
    for (i = optind; i < argc; i++) {
#ifdef NCLDEBUG
        (void) printf("Non-option argument %s\n", argv[i]);
#endif /* NCLDEBUG */

        /*
         * Is this a file of NCL commands?  Can't assume ".ncl" tag, unfortunately.
         * Check for file's existence; the stat() call does not require access rights
         * but does require search path rights, so if this fails, the file could exist
         * but the user may not have permission to "see" it.
         */
        sr = stat(argv[i], &sbuf);
        if (sr == 0) {
#ifdef NCLDEBUG
            (void) printf("NCL commands file: %s\n", argv[i]);
#endif /* NCLDEBUG */
            nclf = argv[i];
            continue;
        }

        if (sr < 0) {
            if (!strchr(argv[i], '=')) {
                /* argument is intended to be a file; can't locate it */
                NhlPError(NhlFATAL, NhlEUNKNOWN, " can't find file \"%s\"\n", argv[i]);
                exit(NhlFATAL);
            } else {
                /* user-defined argument */
                if (nargs == 0)
                    cargs = (char **) NclMalloc(sizeof(char *));
                else
                    cargs = (char **) NclRealloc(cargs, (nargs + 1) * sizeof(char *));

                cargs[nargs] = (char *) NclMalloc((strlen(argv[i]) + 2) * sizeof(char *));
                (void) sprintf(cargs[nargs], "%s\n", argv[i]);
                nargs++;
            }
        }
    }

    error_fp = stderr;
    stdout_fp = stdout;
    stdin_fp = stdin;
    cur_line_text = NclMalloc((unsigned int) 512);
    cur_line_maxsize = 512;
    cur_line_text_pos = &(cur_line_text[0]);

#ifdef NCLDEBUG
    thefptr = fopen("ncl.tree", "w");
    theoptr = fopen("ncl.seq", "w");
#else
    thefptr = NULL;
    theoptr = NULL;
#endif /* NCLDEBUG */

    /*
     * Note: child processes should use _exit() instead of exit() to avoid calling the atexit()
     * functions prematurely 
     */

    NhlInitialize();
    NhlVACreate(&appid, "ncl", NhlappClass, NhlDEFAULT_APP,
        NhlNappDefaultParent, 1, NhlNappUsrDir, "./", NULL);
    NhlPalLoadColormapFiles(NhlworkstationClass,False);
    errid = NhlErrGetID();
    NhlVAGetValues(errid, NhlNerrFileName, &tmp, NULL);
	
    if ((tmp == NULL) || (!strcmp(tmp, "stderr")))
        NhlVASetValues(errid, NhlNerrFilePtr, stdout, NULL);

    _NclInitMachine();
    _NclInitSymbol();	
    _NclInitTypeClasses();
    _NclInitDataClasses();
    /* if the -o flag is specified do stuff to make NCL backwards compatible */
    if (NCLoldBehavior) {
	    _NclSetDefaultFillValues(NCL_5_DEFAULT_FILLVALUES);
    }

    /* Handle default directories */
    if ((libpath = getenv("NCL_DEF_LIB_DIR")) != NULL) {
        d = opendir(_NGResolvePath(libpath));
        if (d != NULL) {
            while((ent = readdir(d)) != NULL) {
                if (*ent->d_name != '.') {
                    (void) sprintf(buffer, "%s/%s", _NGResolvePath(libpath), ent->d_name);
#if defined (HPUX)
                    so_handle = shl_load(buffer, BIND_IMMEDIATE, 0L);
#else
                    so_handle = dlopen(buffer, RTLD_NOW);
                    if (so_handle == NULL) {
                        NhlPError(NhlFATAL, NhlEUNKNOWN,
                            "Could not open (%s): %s.", buffer, dlerror());
                    }
#endif /* HPUX */
           
                    if (so_handle != NULL) {
#if defined (HPUX)
                        init_function = NULL;
                        (void) shl_findsym(&so_handle, "Init",
                                TYPE_UNDEFINED, (void *) &init_function);
#else
                        init_function = dlsym(so_handle, "Init");
#endif /* HPUX */
                        if (init_function != NULL) {
                            (*init_function)();
                        } else {
#if defined (HPUX)
                            shl_unload(so_handle);
#else
                            dlclose(so_handle);
#endif /* HPUX */
                            NhlPError(NhlWARNING, NhlEUNKNOWN,
                                "Could not find Init() in external file %s, file not loaded.",
                                buffer);
                        }
                    } 
                }
            }
        } else {
            NhlPError(NhlWARNING, NhlEUNKNOWN,
                "Could not open default library path (%s), no libraries loaded.", libpath);
        }
        _NclResetNewSymStack();
    }

    if (cmd_line == 1) {
        InitializeReadLine(1);
/*
 * This next line is only to deal with an optimization bug with gcc
 * version 4.0.1 on MacOS 10.4. It apparently saw that "cmd_line"
 * was already of value 1 before it went into NclSetPromptFunc, so 
 * when it optimized the code, it ignored the "cmd_line = 1" line
 * right after the NclSetPromptFunc call.  Since NclSetPrompFunc
 * was setting cmd_line =2, this meant that the value of cmd_line
 * stayed 2, which is the wrong value.
 */
        cmd_line = 0;
        NclSetPromptFunc(nclprompt, NULL);
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
            NhlPError(NhlWARNING, NhlEUNKNOWN,
                " Could not open default script path (%s), no scripts loaded.", scriptpath);
        }
    }

    /*
     * Create the new args
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
            if ((strstr(cargs[k], "=")) == (char *) NULL) 
                NhlPError(NhlWARNING, NhlEUNKNOWN, " Improper assignment for variable %s", cargs[k]);
            else
                (void) fwrite(cargs[k], strlen(cargs[k]), 1, tmpf);
        }

        /* don't forget last newline; NCL requires it */
        (void) fwrite("\n", 1, 1, tmpf);
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

    /* Load any provided script */
    if (nclf != (char *) NULL) {
        (void) strcpy(buffer, _NGResolvePath(nclf));
        if (_NclPreLoadScript(buffer, 0) == NhlFATAL)
            NhlPError(NhlFATAL, NhlEUNKNOWN, "Error loading provided NCL script.");
        else
            yyparse(reset);
    } else {
        yyparse(reset);
    }

#ifdef NCLDEBUG
    (void) fclose(thefptr);
    (void) fprintf(stdout,"Number of unfreed objects %d\n",_NclNumObjs());
    _NclObjsSize(stdout);
    _NclNumGetObjCals(stdout);
    _NclPrintUnfreedObjs(theoptr);
    (void) fprintf(stdout,"Number of constants used %d\n",number_of_constants);
    (void) fclose(theoptr);
#endif /* NCLDEBUG */

    NclFree(myName);

    _NclExit(0);
}

#ifdef __cplusplus
}
#endif /* __cplusplus */
