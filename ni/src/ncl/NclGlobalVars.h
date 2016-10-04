#ifndef _NclGlobalVars_H
#define _NclGlobalVars_H
/* Command line option variables */
short   NCLnoPreload = 0;       /* disable pre-loading of default script files */
short   NCLverbose = 1;
short   NCLecho = 0;            /* echo typed commands, off by default */
short   NCLoverrideEcho = 0;    /* override echo; non-advertised option */
short   NCLnoCopyright = 0;     /* override copyright notice; non-advertised option */
short   NCLnoPrintElem = 0;     /* don't enumerate values in print() */
short   NCLnoSysPager = 0;      /* don't pipe commands to system() to PAGER */
short   NCLoldBehavior = 0;     /* retain former behavior for certain backwards-incompatible changes */
	                        /* behaviors could be revised after an adoption period */
short   NCLuseAFS = 0;          /* Use advanced file structure */
short	NCLdebug_on = 0;	/* Use for debug when NCLDEBUG is defined */

short   NCLusesOpenCL = 0;	/* Uses OpenCL (when run with '-c' and build with BuildOpenCL) */

#ifdef NCLDEBUG
NclMemoryRecord ncl_memory_record;
#endif

char    *nclf = NULL;           /* script of NCL commands, may or may not be provided */
int     cmd_line = 0;

FILE *thefptr = NULL;
FILE *theoptr = NULL;
#endif

