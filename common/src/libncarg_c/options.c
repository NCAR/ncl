/*
 *	$Id: options.c,v 1.11 1992-04-10 15:17:10 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/
/*LINTLIBRARY*/

/*
 *	options.c
 *
 *	Author		John Clyne
 *
 *	Date		Wed Apr 25 17:55:57 MDT 1990
 *
 *	This file manages a resource data base of valid command line 
 *	options. Valid options may be merged into the data base
 *	at any time and later extracted with their coresponding values
 *	as determined by the command line.
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <ncarv.h>
#include "options.h"



static	OptDescRec	*optDescRec = NULL;	/* the option table	*/
static	int		optDescRecSize = 0;	/* mem alloced to table */
static	int		optDescRecNum = 0;	/* num elements in table*/

/*
**
**	Type converters. The following functions convert string representations
**	of data into their primitive data formats. A NULL 'from' value
**	should result in a reasonable value.
**
*/

/*
 *	NCARGCvtToInt()
 *
 *	convert a ascii string to its integer value
 */
int	NCARGCvtToInt(from, to)
	char	*from;	/* the string	*/
	Voidptr	to;
{
	int	*iptr	= (int *) to;

	if (! from) {
		*iptr = 0;
	}
	else if (sscanf(from, "%d", iptr) != 1) {
		ESprintf(EINVAL, "Convert(%s) to int failed", from);
		return(-1);
	}
	return(1);
}

/*
 *	NCARGCvtToFloat()
 *
 *	convert a ascii string to its floating point value
 */
int	NCARGCvtToFloat(from, to)
	char	*from;	/* the string	*/
	Voidptr	to;
{
	float	*fptr	= (float *) to;
	if (! from) {
		*fptr = 0.0;
	}
	else if (sscanf(from, "%f", fptr) != 1) {
		ESprintf(EINVAL, "Convert(%s) to float failed", from);
		return(-1);
	}
	return(1);
}

/*
 *	NCARGCvtToChar()
 *
 *	convert a ascii string to a char.
 */
int	NCARGCvtToChar(from, to)
	char	*from;	/* the string	*/
	Voidptr	to;
{
	char	*cptr	= (char *) to;

	if (! from) {
		*cptr = '\0';
	}
	else if (sscanf(from, "%c", cptr) != 1) {
		ESprintf(EINVAL, "Convert(%s) to char failed", from);
		return(-1);
	}
	return(1);
}


/*
 *	NCARGConvetToBoolean()
 *
 *	convert a ascii string containing either "true" or "false" to
 *	to TRUE or FALSE
 */
int	NCARGCvtToBoolean(from, to)
	char	*from;	/* the string	*/
	Voidptr	to;
{
	boolean	*bptr	= (boolean *) to;

	if (! from) {
		*bptr = FALSE;
	}
	else if (strcmp("true", from) == 0) {
		*bptr = TRUE;
	}
	else if (strcmp("false", from) == 0) {
		*bptr = FALSE;
	}
	else {
		ESprintf(EINVAL, "Convert(%s) to boolean failed", from);
		return(-1);
	}
	return(1);
}

/*
 *	NCARGCvtToString()
 *
 */
int	NCARGCvtToString(from, to)
	char	*from;	/* the string	*/
	Voidptr	to;
{
	char	**sptr	= (char **) to;
	*sptr = from;
}

/*
 *	NCARGCvtToDimension2D()
 *
 *	convert a ascii string to a dimension.
 */
int	NCARGCvtToDimension2D(from, to)
	char	*from;	/* the string	*/
	Voidptr	to;
{
	Dimension2D	*dptr	= (Dimension2D *) to;

	if (! from) {
		dptr->nx = dptr->ny = 0;
	}
	else if (! (
		(sscanf(from, "%dx%d", &(dptr->nx), &(dptr->ny)) == 2) ||
		(sscanf(from, "%dX%d", &(dptr->nx), &(dptr->ny)) == 2))) {

		ESprintf(EINVAL, "Convert(%s) to dimension failed", from);
		return(-1);
	}
	return(1);
}


/*
 *	GetOptions
 *	[exported]
 *
 *	GetOptions may be called after ParseOptionTable in order to 
 *	retrieve values from the option table. The values in the option
 *	data base are converted into appropriate types by calling the 
 *	type converter specifed in 'options' for each option.
 *
 *	The fields of the Option struct are as follows:
 *
 *	char		*option_name;	the options name - used to look the
 *					requested option in the option table.
 *	int		(*type_conv)();	option type converter - converts the
 *					string representation of the option
 *					value into a specified format and store
 *					the result at address given by 'offset'
 *	Voidptr		offset;		offset of return address
 *	int		size;		size of option in bytes - if there are
 *					multiple arguments for a single option
 *					their destination address is computed
 *					by adding 'size' to 'offset' as
 *					appropriate.
 *
 *	GetOptions() invokes ESprintf() on failure.
 *
 *
 * on entry
 *	options		: Null terminated list of options to be returned
 *
 * on exit
 *	return		: -1 => failure, else OK
 */
GetOptions(options)
	Option		*options;
{

	int	i, j;
	char	*s;
	int	arg_count;

	OptDescRec	*optd, *get_option();
	Voidptr		offset;

	for (i = 0; options[i].option_name; i++ ) {

		optd = get_option(options[i].option_name);/* find the option */

		if (optd == (OptDescRec *) NULL) {
			ESprintf(
				EINVAL, "Option %s unknown", 
				options[i].option_name
			);
			return(-1);
		}

		/*
		 * zero arg_count options really do have a single argument
		 */
		arg_count = optd->arg_count ? optd->arg_count : 1;

		offset = options[i].offset;
		for(j=0,s=optd->value; j<arg_count; j++){
			if (options[i].type_conv(s, offset) < 0) {
				return(-1);
			}
			if (s) s += strlen(s) + 1;
			offset = (char *) offset + options[i].size;
		}
	}
	return(1);
}


/*
 *	LoadOptionTable
 *	[exported]
 *
 *	Add a list of valid application options to the option table. All 
 *	options are assumed to begin with a '-' and contain 0 or more arguments.
 *
 *	The fields of the OptDescRec struct are as follows: 
 *
 *	char	*option;	name of option without preceeding '-'
 *	int	arg_count;	num args expected by option
 *	char	*value;		default value for the argument - if 'arg_count'
 *				is zero 'value' is ignored. if 'arg_count' is 
 *				greater than 1 'value' is a string of space 
 *				separted arguments.
 *	char	*help;		help string for option
 *
 *	LoadOptionTable() invokes ESprintf() on error.
 *
 * on entry
 *	optd		: Null terminated list of options
 *
 * on exit
 *	return		: -1 => failure, else OK.
 */
LoadOptionTable(optd)
	OptDescRec	*optd;
{

	int	i,j,n;
	int	num;
	static	short	first = 1;

	extern	char	*strcpy();
	char	*fmt_opt_string();

	if (! optd[0].option) return (0);

	/*
	 * make sure there are no duplicate names in the table. This only 
	 * compares new entries with old entries. If there are duplicate
	 * entries within the new entries they won't be found. Count
	 * how many options there are as well
	 */
	for (j = 0, num = 0; optd[j].option; j++) {
		for ( i = 0; i < optDescRecNum; i++) {
			if (strcmp(optDescRec[i].option, optd[j].option) == 0){
				ESprintf(
					EINVAL, 
					"Option %s already in option table", 
					optd[j].option
				);
				return(-1);	/* duplicate entry	*/
			}
		}
		num++;
	}

	/*
	 * if first time called malloc memory, else realloc memory.
	 */
	if (first) {
		if (! (optDescRec = (OptDescRec *) malloc ((unsigned) 
			((num+1) * sizeof(OptDescRec))))) {

			ESprintf(errno,"malloc(%d)",(num+1)*sizeof(OptDescRec));
			return(-1);
		}
		optDescRecSize = num + 1;
		first = 0;
	}
	else {
		optDescRecSize += num;
		if (! (optDescRec = (OptDescRec *) realloc ((char *)optDescRec, 
			(unsigned) (sizeof (OptDescRec) * optDescRecSize)))) {

			ESprintf(errno,"malloc(%d)",(num+1)*sizeof(OptDescRec));
			return(-1);
		}
	}

	/*
	 * copy all the options into the option table allocating memory
	 * as necessary.
	 */
	for (i = 0, n = optDescRecNum; i < num; i++, optDescRecNum++, n++) {
		if (optd[i].arg_count == 0) optd[i].value = "false";

		if (! optd[i].option) {
			ESprintf(EINVAL, ("Invalid option descriptor"));
			return(-1);
		}

		if (optd[i].value) {
			optd[i].value = fmt_opt_string(
				optd[i].value,optd[i].arg_count
			);
			if (! optd[i].value) {
				return(-1);
			}
		}

		optDescRec[n].option = optd[i].option;
		optDescRec[n].arg_count = optd[i].arg_count;
		optDescRec[n].value = optd[i].value;
		optDescRec[n].help = optd[i].help;

	}
	optDescRec[n].option = NULL;	/* terminate list  */
	
	return(1);
}


/*
 *	ParseOptionTable
 *	[exported]
 *
 *	parse the option table with the command line options. After the 
 *	option table is created with LoadOptionTable this function may
 *	be called to change values of options in the table. We assume
 *	argv[0] is the program name and we leave it unmolested
 *
 *	ESprintf() is invoked on error.
 *
 * on entry:
 *	**argv		: list of command line args
 *	*argc		: num elements in argv
 *	*optds		: additional options to merge into the option table
 * on exit
 *	**argv		: contains options not found in the option table
 *	*argc		: num elements in argv
 *	return		: -1 => failure, else OK
 */
ParseOptionTable(argc, argv, optds)
	int	*argc;
	char	**argv;
	OptDescRec	*optds;
{
	int	i;
	char	**next = argv + 1;
	OptDescRec	*optd;
	int		new_argc = 1;

	extern	OptDescRec	*get_option();
	char	*copy_create_arg_string();

	/*
	 * if any options to be merged do so
	 */
	if (optds) {
		(void) LoadOptionTable(optds);
	}

	if (! argv) return;

	
	/*
	 * look for matches between elements in argv and in the option table
	 */
	for (i = 1; i < *argc; i++) {

		if (*argv[i] == '-') {	/* is it an option specifier?	*/
			optd = get_option((char *) (argv[i] + 1));
		}
		else {
			optd = (OptDescRec *) NULL;	/* not a specifier */ 
		}

		if (optd == (OptDescRec *) -1) {
			ESprintf(EINVAL, "Ambiguous option: %s", argv[i]);
			return(-1);
		}

		/*
		 * if no match found leave option in argv along with anything
		 * that follows that is not an option specifier
		 */
		if (optd == (OptDescRec *) NULL) {	/* not found	*/
			*next = argv[i];
			new_argc++;
			next++;
			while(i+1 < *argc && *argv[i+1] != '-') {
				i++;
				new_argc++;
				*next  = argv[i];
				next++;
			}
			continue;
		}

		/*
		 * make sure enough args for option
		 */
		if ((i + optd->arg_count) >= *argc) {
			ESprintf(
				EINVAL, "Option -%s expects %d args",
				optd->option, optd->arg_count
				);
			return(-1);
		}

		/*
		 * Options with no args are a special case. Assign them
		 * a value of true. They are false by default
		 */
		if (optd->arg_count == 0) {
			optd->value = "true";
			continue;
		}


		/*
		 * convert the arg list to a single string and stash it
		 * in the option table
		 */
		optd->value =copy_create_arg_string(&argv[i+1],optd->arg_count);
		if (! optd->value) {
			return(-1);
		}
		i += optd->arg_count;
				
	}
	*argc = new_argc;
	argv[*argc] = NULL;

	return(1);
}

/*
 *	ParseEnvOptions()
 *
 *	ParseEnvOptions() is analogous to ParseOptionTable except that
 *	it takes a list of environment variables and their coresponding
 *	option names instead of an argv list. If a given environment
 *	variable is set its value is used as the argument value for the 
 *	option with  which it is associated. If the environment variable
 *	is not set the option/environemnt variable pair are ignored.
 *
 * on entry
 *	*envv		: NUll-terminated list of option/env pairs
 *	*optds		: additional options to merge into the option table
 *
 * on exit
 *	return		: -1 => error, else OK
 */
ParseEnvOptions(envv, optds)
	EnvOpt		*envv;
	OptDescRec	*optds;
{
	int	envc;		/* size of envv list		*/
	EnvOpt	*envptr;	/* pointer to envv		*/
	char	**argv;		/* arg vector created from envv	*/
	int	argc;		/* size of argv list		*/
	char	*arg_string;	/* env variable value		*/
	char	buf[MAX_ATOARGV_STRING];

	/*
	 * if any options to be merged do so
	 */
	if (optds) {
		(void) LoadOptionTable(optds);
	}

	/*
 	 * look for environment variables. Generate the argument vector, argv
	 */
	for (envptr = envv; envptr->option; envptr++) {
		if (arg_string = getenv(envptr->env_var)) {

			(void) strcpy(buf, "-");
			(void) strcat(buf, envptr->option);
			(void) strcat(buf, " ");
			(void) strncat(buf,arg_string,sizeof(buf)-strlen(buf)-1);
			if (! (argv = AToArgv(buf, "dummy", &argc))) {
				return(-1);
			}
			if (ParseOptionTable(
				&argc, argv, (OptDescRec *) NULL) == -1) {

				return(-1);
			}

			if (argc != 1) {
				ESprintf(
					EINVAL, "Environment variable %s='%s'",
					envptr->env_var, arg_string
				);
				return(-1);
			}
		}
	}
	return(1);
}

/*
 *	PrintOptionHelp()
 *
 *	Print help about each option.
 *
 *	Each option known to the option table is printed, followed by
 *	the string "arg0 arg1 ... argN", where N+1 is the number of arguments
 *	expected by the option, followed by the contents of the 'help' field.
 *
 * on entry
 *	*fp		: file pointer where output is written.
 */
void	PrintOptionHelp(fp)
	FILE	*fp;
{
	int	i,j;
	char	buf[30];
	char	sbf[20];

	for(i=0; i<optDescRecNum; i++) {
		sprintf(buf, "    -%-8.8s", optDescRec[i].option);
		if (optDescRec[i].arg_count < 4) {
			for(j=0; j<optDescRec[i].arg_count; j++) {
				sprintf(sbf, " arg%d", j);
				if (strlen(sbf) + strlen(buf) < sizeof(buf)) {
					(void) strcat(buf, sbf);
				}
				else {
					break;
				}
			}
		}
		else {
			sprintf(sbf," arg0 .. arg%d",optDescRec[i].arg_count-1);
			(void) strcat(buf, sbf);
		}
		(void) fprintf(fp, buf);
		for(j=strlen(buf); j<sizeof(buf); j++) {
			putc(' ', fp);
		}
		if (optDescRec[i].help) {
			(void) fprintf(fp, "%s\n", optDescRec[i].help);
		}
		else {
			(void) fprintf(fp, "\n");
		}
	}
}

/*
 *	get_option
 *	[internal]
 *	
 * on entry
 *	*name		: name to lookup
 * on exit
 *	return		: if found return command obj ptr. If name is ambiguous
 *			  return -1. If not found return NULL.
 */
static	OptDescRec	*get_option (name)
	char	*name;
{
	char *p, *q;
	OptDescRec *o, *found;
	int nmatches, longest;

	longest = 0;
	nmatches = 0;
	found = NULL;

	for (o = optDescRec; o && (p = o->option); o++) {
		for (q = name; *q == *p++; q++) {
			if (*q == 0)            /* exact match? */
				return (o);
		}
		if (!*q) {                      /* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = o;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)	/* ambiguous	*/
		return ((OptDescRec *)-1);
	return (found);
}

/*
 * convert an array of strings into a singe contiguous array of strings
 */
static	char	*copy_create_arg_string(argv, argc)
	char	**argv;
	int	argc;
{
	char	*s,*t;
	int	i,
		len;	/* lenght of new arg string	*/

	for(len=0,i=0; i<argc; i++) {
		len += strlen(argv[i]);
		len++;	/* one for the space	*/
	}

	if ((s = (char *) malloc(len +1)) == NULL) {
		ESprintf(errno, "malloc(%d)", len+1);
		return(NULL);
	}
	s = strcpy(s, argv[0]);
	for(i=1, t=s; i<argc; i++) {
		t += strlen(t);
		*t++ = '\0';
		(void) strcpy(t, argv[i]);
	}
	return(s);
}

/*
 * convert a space seprated string to an array of contiguous strings
 */
char	*fmt_opt_string(arg, n)
	char	*arg;
	int	n;
{
	int	i;
	char	*s;

	for (i=1, s = arg; i<n; i++) {
		while(*s && ! isspace(*s)) s++;

		if (! *s) {
			ESprintf(EINVAL, "Arg string invalid: %s", arg);
			return(NULL);
		}
		*s++ = '\0';

		while(*s && isspace(*s)) s++;
	}
	return(arg);
}
