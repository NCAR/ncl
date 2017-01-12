#ifdef __cplusplus
extern "C" {
#endif

#include   "NclDriver.h"

int NclReturnStatus = NclNoError;

int quark_comp(const void *q1, const void *q2)
{
	return(strcmp((const char*)NrmQuarkToString(*(NrmQuark *)q1),(const char*)NrmQuarkToString(*(NrmQuark *) q2)));
}

static int numberOfPreloadedScripts = 8;

char *preload_scripts[8] = {"$NCARG_ROOT/lib/ncarg/nclscripts/utilities.ncl",
                            "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl",
                            "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl",
                            "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl",
                            "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl",
                            "$NCARG_ROOT/lib/ncarg/nclscripts/csm/bootstrap.ncl",
                            "$NCARG_ROOT/lib/ncarg/nclscripts/csm/extval.ncl",
                            "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"};

int NclDriver(int argc, char **argv)
{
    int errid = -1;
    int appid;
    int i, k = 0;
    int reset = 1;
    short echoUserScript = 0;
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

#ifdef NCLDEBUG
    char    **NCL_ARGV;
    int NCL_ARGC;           /* local argv/argc -- future use for NCL scripts? */
#endif

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

#ifdef _OPENMP
    /* make sure that only one OMP thread runs unless OMP_NUM_THREADS is explicitly defined  -- on Darwin at least the default is not a single thread */

    if (! getenv("OMP_NUM_THREADS")) {
	    omp_set_num_threads(1);
    }
#endif

    strcpy(buffer,(char *)GetNCARGPath("tmp"));
    sr = access(buffer,W_OK|X_OK|F_OK);
    if(sr != 0) {
	    NhlPError(NhlWARNING,NhlEUNKNOWN,
		      "\"%s\" tmp dir does not exist or is not writable: NCL functionality may be limited -- check TMPDIR environment variable",
		      buffer);
    }

    InitStdStreams(stdin, stdout, stderr);
	
    ncopts = NC_VERBOSE;

    cmd_line =isatty(fileno(stdin));

#ifdef NCLDEBUG
    /*
     * Save NCL argv, for command line processing later use
     */
    NCL_ARGV = (char **) NclMalloc(argc  * sizeof(char *));
    for (i = 0; i < argc; i++) {
        NCL_ARGV[i] =  (char *) NclMalloc((strlen(argv[i]) + 1) * sizeof(char *));
        (void) strcpy(NCL_ARGV[i], argv[i]);
    }
    NCL_ARGC = argc;

    for (i = 0; i < NCL_ARGC; i++, *NCL_ARGV++)
        (void) printf("NCL_ARGV[%d] = %s\n", i, *NCL_ARGV);
#endif /* NCLDEBUG */

    /*
     * Defined arguments
     *
     *  -n      element print: don't enumerate elements in print()
     *  -x      echo: turns on command echo
     *  -Q      turn off echo of copyright and version information
     *  -V      version: output NCARG/NCL version, exit
     *  -m      turns on memory debug
     *  -o      old behavior: retain former behavior for backwards incompatible changes
     *  -h      help: output options and exit
     *  -s      disable pre-loading of default script files
     *
     *  -X      override: echo every stmt regardless (unannounced option)
     *  -Q      override: don't echo copyright notice (unannounced option)
     */
    opterr = 0;     /* turn off getopt() msgs */
    while ((c = getopt (argc, argv, "fhnodgmxVXQps")) != -1) {
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
                echoUserScript = 1;
                break;

            case 's':
		NCLnoPreload = 1;
                break;

#ifdef NCLDEBUG
            case 'm':
                NCLdebug_on = 1;
                break;

            case 'd':
                NCLdebug_on = 2;
                break;

            case 'g':
                NCLdebug_on = 3;
                break;

#endif
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

            case 'f':
                NCLuseAFS = 1;
                break;

            case 'h':
                (void) fprintf(stdout, "Usage: ncl -fhnopxQV <args> <file.ncl>\n");
	        (void) fprintf(stdout, "\t -f: use new file structure and NetCDF4 features when possible\n");
                (void) fprintf(stdout, "\t -h: print this message and exit\n");
                (void) fprintf(stdout, "\t -n: don't enumerate values in print()\n");
                (void) fprintf(stdout, "\t -o: retain former behavior for certain backwards-incompatible changes\n");
                (void) fprintf(stdout, "\t -p: don't page output from the system() command\n");
                (void) fprintf(stdout, "\t -x: echo NCL commands\n");
                (void) fprintf(stdout, "\t -s: disable pre-loading of default script files\n");
                (void) fprintf(stdout, "\t -Q: turn off echo of NCL version and copyright info\n");
                (void) fprintf(stdout, "\t -V: print NCL version and exit\n");
#ifdef NCLDEBUG
                (void) fprintf(stdout, "\t -m: turns on memory debug.\n");
                (void) fprintf(stdout, "\t -d: turns on detailed memory debug.\n");
                (void) fprintf(stdout, "\t -g: turns on deep detailed memory debug.\n");
#endif
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
            " Copyright (C) 1995-2017 - All Rights Reserved\n University Corporation for Atmospheric Research\n NCAR Command Language Version %s\n The use of this software is governed by a License Agreement.\n See http://www.ncl.ucar.edu/ for more details.\n", GetNCLVersion());

    if (NCLnoPreload) {
	    numberOfPreloadedScripts = 0;
    }

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

	if(nclf){
		NCL_PROF_INIT(nclf);
	}
	else{
		NCL_PROF_INIT("cmdline");
	}

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
    _NclInitFileClasses();
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
    /* These need to be loaded in alphabetical order to ensure that users can control
     * the order of loading. There is a BSD function scandir that would do it all but it 
     * might not be standardized enough to be uniformly available on all systems, so for
     * now it must be coded just using readdir.
     */
    
    if ((scriptpath = getenv("NCL_DEF_SCRIPTS_DIR")) != NULL) {
	    d = opendir(_NGResolvePath(scriptpath));
	    if (d!= NULL) {
		    int script_count = 0, alloc_count = 32;
		    NrmQuark *qscript_names = NclMalloc(alloc_count * sizeof(NrmQuark));
		    while((ent = readdir(d)) != NULL) {
			    if (*ent->d_name != '.') {
				    (void) sprintf(buffer, "%s/%s", _NGResolvePath(scriptpath), ent->d_name);
				    pt = strrchr(buffer, '.');
				    if (pt != NULL) {
					    pt++;
					    if (strncmp(pt, "ncl", 3) == 0) {
						    if (script_count == alloc_count) {
							    alloc_count *= 2;
							    qscript_names = NclRealloc(qscript_names,alloc_count * sizeof(NrmQuark));
						    }
						    qscript_names[script_count++] = NrmStringToQuark(ent->d_name);
					    }
				    }
			    }
		    }
		    if (script_count == 0)  {
			    NhlPError(NhlWARNING, NhlEUNKNOWN,
				      "No scripts found: scripts must have the \".ncl\" file extension.");
		    }
		    else {
			    qsort(qscript_names,script_count,sizeof(NrmQuark),quark_comp);
			    for (i = 0; i < script_count; i++) {
				    (void) sprintf(buffer, "%s/%s", _NGResolvePath(scriptpath), NrmQuarkToString(qscript_names[i]));
				    if (_NclPreLoadScript(buffer, 1) == NhlFATAL) {
					    NhlPError(NhlFATAL, NhlEUNKNOWN, "Error loading default script.");
				    } else {
					    yyparse(reset);
				    }
			    }
			    NclFree(qscript_names);
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

    /* Pre-Load script */

    for(i = 0; i < numberOfPreloadedScripts; ++i)
    {
        strcpy(buffer, _NGResolvePath(preload_scripts[i]));
#ifdef NCLDEBUG
        fprintf(stderr, "\tLoad predefined script %d: <%s>\n", i, preload_scripts[i]);
#endif

        sr = stat(buffer, &sbuf);
        if(0 == sr)
        {
            if(_NclPreLoadScript(buffer, 1) == NhlFATAL)
	    {
	        NclReturnStatus = NclFileNotFound;
                NhlPError(NhlINFO, NhlEUNKNOWN, "Error loading NCL utility script.");
	    }
            else
                yyparse(reset);
        }
    }

#if 0
#ifdef NCLDEBUG
    if(NCLdebug_on)
        _initializeNclMemoryRecord();
#endif
#endif

    /* Load any provided script */
    NCLecho = echoUserScript;
    if (nclf != (char *) NULL) {
        (void) strcpy(buffer, _NGResolvePath(nclf));
        if (_NclPreLoadScript(buffer, 0) == NhlFATAL)
	{
	    NclReturnStatus = NclFileNotFound;
            NhlPError(NhlFATAL, NhlEUNKNOWN, "Error loading provided NCL script.");
	}
        else
            yyparse(reset);
    } else {
        yyparse(reset);
    }

#if 0
#ifdef NCLDEBUG
        if(NCLdebug_on)
                _finalizeNclMemoryRecord();
#endif
#endif

#ifdef NCLDEBUG
    (void) fclose(thefptr);
    (void) fprintf(stdout,"Number of unfreed objects %d\n",_NclNumObjs());
    _NclObjsSize(stdout);
    _NclNumGetObjCals(stdout);
    _NclPrintUnfreedObjs(theoptr);
    (void) fprintf(stdout,"Number of constants used %d\n",number_of_constants);
    (void) fclose(theoptr);
#endif /* NCLDEBUG */

    NclFree(cur_line_text);

    _NclExit(NclReturnStatus);

    return NclReturnStatus;
}

#ifdef __cplusplus
}
#endif /* __cplusplus */
