/*
 *	$Id: options.c,v 1.4 1991-03-22 10:16:15 clyne Exp $
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
#include <sys/types.h>
#include <ncarv.h>

static	OptDescRec	*optDescRec = NULL;	/* the option table	*/
static	int		optDescRecSize = 0;	/* mem alloced to table */
static	int		optDescRecNum = 0;	/* num elements in table*/

/*
 *	conv_to_int:
 *
 *	convert a ascii string to its integer value
 */
static	conv_to_int(s, value)
	char	*s;	/* the string	*/
	IntType_	*value;	/* it value	*/
{
	if (sscanf(s, "%d", value) != 1) {

		(void) fprintf(stderr, 
			"Type conversion for %s to int failed\n", s);

	}
}
/*
 *	conv_to_float:
 *
 *	convert a ascii string to its floating point value
 */
static	conv_to_float(s, value)
	char	*s;	/* the string	*/
	FloatType_	*value;	/* it value	*/
{
	if (sscanf(s, "%f", value) != 1) {

		(void) fprintf(stderr, 
			"Type conversion for %s to int failed\n", s);

	}
}
/*
 *	conv_to_bool:
 *
 *	convert a ascii string containing either "true" or "false" to
 *	to 1 or 0
 */
static	conv_to_bool(s, value)
	char	*s;
	BoolType_	*value;
{
	if (strcmp("true", s) == 0) {
		*value = 1;
		return;
	}

	if (strcmp("false", s) == 0) {
		*value = 0;
		return;
	}

	(void) fprintf(stderr, "Type conversion for %s to boolean failed\n", s);
}

/*
 *	getOptions
 *	[exported]
 *
 *	getOptions may be called after parseOptionTable in order to 
 *	retrieve values from the option table. The values in the option
 *	data base are converted into appropriate types as requested in
 *	"options". Currently only StringType, BoolType and IntType are
 *	supported.
 * on entry
 *	base		: the base address of the data object that will
 *			  contain option values returned by getOptions
 *	options		: Null terminated list of options to be returned
 */
getOptions(base, options)
	caddr_t		base;
	Option		*options;
{

	int	i;

	StringType_	string_value;
	IntType_	int_value;
	FloatType_	float_value;
	BoolType_	bool_value;
	OptDescRec	*opt;
	OptDescRec	*get_option();

	unsigned long	address;

	for (i = 0; options[i].option_name; i++ ) {

		opt = get_option(options[i].option_name);/* find the option */

		if (opt == (OptDescRec *) NULL) {
			(void) fprintf(stderr, "Couldn't find option: %s\n", 
				options[i].option_name);
			continue;
		}

		/*
		 * address of storage for option
		 */
		address = (unsigned long) base + options[i].option_offset;

		switch ((int) options[i].option_type) {
		case	StringType:
			string_value =  (StringType_) &opt->value[0];
			bcopy((char *) &string_value, (char *) address,
				(int) options[i].option_size);
			break;

		case	IntType:
			conv_to_int(opt->value, &int_value);
			bcopy((char *) &int_value, (char *) address,
				(int) options[i].option_size);
			break;

		case	BoolType:
			conv_to_bool(opt->value, &bool_value);
			bcopy((char *) &bool_value, (char *) address,
				(int) options[i].option_size);
			break;

		case	FloatType:
			conv_to_float(opt->value, &float_value);
			bcopy((char *) &float_value, (char *) address,
				(int) options[i].option_size);
			break;
		default	:
			(void) fprintf(stderr, "Unsupported option type\n");
			break;
		}
	}
}


/*
 *	buildOptionTable
 *	[exported]
 *
 *	Add a list of valid application options to the option table. All 
 *	options are assumed to begin with a '-' and contain 0 or 1 arguments.
 *	If an option has more than 1 arg it must be quoted and treated as a
 *	single string.
 * on entry
 *	opts		: Null terminated list of options
 */
buildOptionTable(opts)
	OptDescRec	*opts;
{

	int	i,j,n;
	int	num;
	static	short	first = 1;

	extern	char	*strcpy();

	if (! opts[0].option) return (0);

	/*
	 * make sure there are no duplicate names in the table. This only 
	 * compares new entries with old entries. If there are duplicate
	 * entries within the new entries they won't be found. Count
	 * how many options there are as well
	 */
	for (j = 0, num = 0; opts[j].option; j++) {
		for ( i = 0; i < optDescRecNum; i++) {
			if (strcmp(optDescRec[i].option, opts[j].option) == 0){
				return(-1);	/* duplicate entry	*/
			}
		}
		num++;
	}

	/*
	 * if first time called malloc memory, else realloc memory.
	 */
	if (first) {
		optDescRec = 
			(OptDescRec *) icMalloc ((unsigned) 
			((num+1) * sizeof(OptDescRec)));
		optDescRecSize = num + 1;
		first = 0;
	}
	else {
		optDescRecSize += num;
		optDescRec = (OptDescRec *) icRealloc ((char *) optDescRec, 
			(unsigned) (sizeof (OptDescRec) * optDescRecSize));
	}

	/*
	 * copy all the options into the option table allocating memory
	 * as necessary.
	 */
	for (i = 0, n = optDescRecNum; i < num; i++, optDescRecNum++, n++) {
		if (opts[i].option) {
			optDescRec[n].option = (char *) icMalloc 
				((unsigned) (strlen (opts[i].option) + 1));
			(void) strcpy(optDescRec[n].option, opts[i].option);
		}
		if (opts[i].value) {
			optDescRec[n].value = (char *) icMalloc 
				((unsigned) (strlen (opts[i].value) + 1));
			(void) strcpy(optDescRec[n].value, opts[i].value);
		}
		optDescRec[n].arg_type  = opts[i].arg_type;
	}
	optDescRec[n].option = NULL;	/* terminate list  */
	
	return(1);
}


/*
 *	parseOptionTable
 *	[exported]
 *
 *	parse the option table with the command line options. After the 
 *	option table is created with buildOptionTable this function may
 *	be called to change values of options in the table. We assume
 *	argv[0] is the program name and we leave it unmolested
 *
 * on entry:
 *	**argv		: list of command line args
 *	*argc		: num elements in argv
 *	*opts		: additional options to merge into the option table
 * on exit
 *	**argv		: contains options not found in the option table
 *	*argc		: num elements in argv
 */
parseOptionTable(argc, argv, opts)
	int	*argc;
	char	**argv;
	OptDescRec	*opts;
{
	int	i;
	char	**next = argv + 1;
	OptDescRec	*opt;
	int		new_argc = 1;

	extern	OptDescRec	*get_option();

	/*
	 * if any options to be merged do so
	 */
	if (opts) {
		(void) buildOptionTable(opts);
	}

	
	/*
	 * look for matches between elements in argv and in the option table
	 */
	for (i = 1; i < *argc; i++) {

		if (*argv[i] == '-') {	/* is it an option specifier?	*/
			opt = get_option((char *) (argv[i] + 1));
		}
		else {
			opt = (OptDescRec *) NULL;	/* not a specifier */ 
		}

		if (opt == (OptDescRec *) -1) {
			(void) fprintf(stderr,"Ambigous option: %s\n", argv[i]);
			exit(1);
		}

		/*
		 * if no match found leave option in argv along with anything
		 * that follows that is not an option specifier
		 */
		if (opt == (OptDescRec *) NULL) {	/* not found	*/
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
		 * found an option. add it to the found option
		 * table
		 */
		if (opt->arg_type == OptSepArg) {
			i++;
			if (i >= *argc) {
				(void) fprintf(stderr,
					"Missing arg to: %s\n",argv[i-1]);
				exit(1);
			}
			if (opt->value) cfree((char *) opt->value);
			opt->value = argv[i];
			continue;
		}

		if (opt->arg_type == OptIsArg) {
			if (!opt->value)
				opt->value = "true";
			else if (strcmp(opt->value, "true") == 0) {
				cfree((char *) opt->value);
				opt->value = "false";
			}
			else  {	/* assume false	*/
				cfree((char *) opt->value);
				opt->value = "true";
			}
			continue;
		}
				
	}
	*argc = new_argc;
	argv[*argc] = NULL;
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

	for (o = optDescRec; p = o->option; o++) {
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
