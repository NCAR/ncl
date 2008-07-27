/*
 *	$Id: spooler.c,v 1.17 2008-07-27 03:18:45 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/

/*
 *	spooler.c
 *
 *	Author		John Clyne
 *
 *	Date		Wed May 16 09:14:59 MDT 1990
 *
 *	This file contains operations for maintaining a list of printing
 *	spoolers
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<signal.h>
#include	<string.h>
#include	<ctype.h>
#include	<fcntl.h>
#include	<ncarg/c.h>
#include	"ictrans.h"
#include	"spooler.h"

static	Spoolers	spoolers = {NULL, 0, 0};
static	Spooler		*currentSpooler = NULL;
static	boolean		isInitialized = FALSE;

int	spoolerJobs = 0;	/* current number of spooler processes	*/




/*
 * Last - forks and execs the last process in the chain 
 *
 * on entry
 *	**argv		: arg list for process to be spawned
 *	*tpipe		: a pipe for communicating with other processes
 *	fd		: if fd > 0 it is a file descriptor for sending stdout
 *	log_fp		: message log file descriptor
 */
static	last(argv, tpipe, fd, log_fp)
	char	**argv;
	int *tpipe; 
	int	fd;
	FILE	*log_fp;
{

	int pid;

	switch (pid = fork()) {
	case -1:
		(void) fprintf(stderr,"Fork failure, process: %s\n",argv[0]);
		break;
	case 0:
		if (tpipe[0] != 0) {
			(void) close(0);
			(void) dup2(tpipe[0],0);
			(void) close(tpipe[0]);
		}
		(void) close(tpipe[1]);

		if (fd > 0) {
			(void) close(1);
			(void) dup(fd);
			(void) close(fd);
		}
		(void) execvp(argv[0],argv);
		(void) fprintf(stderr,"Execvp failure, process: %s\n",argv[0]);
		_exit(127);

	default:
		spoolerJobs++;
		if (log_fp) (void) fprintf(log_fp,"[%d] %d\n",spoolerJobs, pid);
		break;
	}

}

/*
 * first - forks and execs the first process in the chain 
 *
 */

static	first(argv, tpipe, log_fp)
	char	**argv;
	int *tpipe; 
	FILE	*log_fp;
{

	int pid;
	
	switch (pid = fork()) {
	case -1:
		(void) fprintf(stderr,"Fork failure, process: %s\n",argv[0]);
		break;
	case 0:
			
		if (tpipe[1] != 1) {
			(void) close(1);
			(void) dup2(tpipe[1],1);
			(void) close(tpipe[1]);
		}
		(void) close(tpipe[0]);
		(void) execvp(argv[0],argv);
		(void) fprintf(stderr,"Execvp failure, process: %s\n",argv[0]);
		_exit(127);
	default:
		spoolerJobs++;
		if (log_fp) (void) fprintf(log_fp,"[%d] %d\n",spoolerJobs,pid);
	}
}

/*
 * middle - forks and execs the center procesess in the chain 
 *
 */

static	middle(argv, out_pipe,in_pipe, log_fp)
	char	**argv;
	int *out_pipe,*in_pipe;
	FILE	*log_fp;
{
	
	int  pid;

	switch (pid = fork()) {
	case -1:
		(void) fprintf(stderr,"Fork failure, process: %s\n",argv[0]);
		break;
	case 0:
		if (in_pipe[0] != 0) { 
			(void) close(0);
			(void) dup2(in_pipe[0],0);
			(void) close(in_pipe[0]);
		}
		(void) close(in_pipe[1]);

		if (out_pipe[1] != 1) { 
			(void) close(1);
			(void) dup2(out_pipe[1],1);
			(void) close(out_pipe[1]);
		}
		(void) close(out_pipe[0]);
		(void) execvp(argv[0],argv);
		(void) fprintf(stderr,"Execvp failure, process: %s\n",argv[0]);
		_exit(127);
	default:
		spoolerJobs++;
		if (log_fp) (void) fprintf(log_fp,"[%d] %d\n",spoolerJobs,pid);
		break;
	}
}
		
/*
 *	parse_trans_args:
 *	[internal]
 *
 *	parse a agument list from a string which is terminated by a 
 *	':' character. Return a pointer to a Args token which contains the
 *	args. The memory malloced is exported
 * on entry
 *	*s		: string containing the filter;
 * on exit
 *	return		: a Args token or NULL if an error occured.
 */
static	Args	*parse_trans_args(s)
	char	*s;
{
	Args	*args;	/* pointer to the token	*/
	int	i, 
		count;		/* number of args returned in the token	*/
	char	*t;

	char	**argv;		/* the arg list for the token	*/
	int	argc;


	while (isspace (*s)) s++;

	/*
	 * count the args for the args and make memory management easier
	 * We parse the string twice this way but its much simpler
	 */
	t = s;
	argc = 0;
	while (*t && *t != ':'){
		while (*t && *t != ':' && !isspace(*t)) t++;

		argc++;
		while (isspace(*t)) t++;
	}

	if (!argc) return (NULL);

	if ( !(argv = (char **) malloc ((argc + 1) * sizeof (char **)))) {
		return(NULL);
	}

	/*
	 * now go back and grab the args
	 */
	count = 0;
	for (count = 0; count < argc; count++) {

		while (isspace(*s)) s++;

		for (t=s, i=0;*t && *t != ':' && !isspace(*t); t++)
			i++;

		if (! (argv[count] = malloc((unsigned) i + 1 ))) {
			return(NULL);
		}
		(void) strncpy(argv[count], s, i);
		argv[count][i] = '\0';
		
		s = t;

	}
	argv[argc] = NULL;

	if (! (args = (Args *) malloc (sizeof (Args )))) {
		return(NULL);
	}

	args->argc = argc;
	args->argv = argv;
	return (args);
}

/*
 *	parse_filter_args:
 *	[internal]
 *
 *	parse a single filter and its args from a string terminated by either
 *	a '|', '>', or a null character. Return a pointer to a Args 
 *	token which contains the filter name and its args. The memory 
 *	malloced is exported
 * on entry
 *	*s		: string containing the filter;
 * on exit
 *	return		: a Args token or NULL if an error occured.
 */
static	Args	*parse_filter_args(s)
	char	*s;
{
	Args	*args;	/* pointer to the token	*/
	int	i, 
		count;		/* number of args returned in the token	*/
	char	*t;

	char	**argv;		/* the arg list for the token	*/
	int	argc;


	while (isspace (*s)) s++;

	/*
	 * count the args for the args and make memory management easier
	 * We parse the string twice this way but its much simpler
	 */
	t = s;
	argc = 0;
	while (*t && *t != '|' && *t != '>') {
		while (*t && *t != '|' && *t != '>' && !isspace(*t)) t++;

		argc++;
		while (isspace(*t)) t++;
	}

	if (!argc) return (NULL);

	if ( !(argv = (char **) malloc ((argc + 1) * sizeof (char **)))) {
		return(NULL);
	}

	/*
	 * now go back and grab the args
	 */
	count = 0;
	for (count = 0; count < argc; count++) {

		while (isspace(*s)) s++;

		for (t=s, i=0;*t && *t != '|' && *t != '>' && !isspace(*t); t++)
			i++;

		if (! (argv[count] = malloc((unsigned) i + 1 ))) {
			return(NULL);
		}
		(void) strncpy(argv[count], s, i);
		argv[count][i] = '\0';
		
		s = t;

	}
	argv[argc] = NULL;

	if ( !(args = (Args *) malloc (sizeof (Args )))) {
		return(NULL);
	}

	args->argc = argc;
	args->argv = argv;
	return (args);
}

/*
 *	spool_parse
 *	[internal]
 *
 *	parse a spooler specification string and build a token to represent
 *	it data. The format of the string must be
 * 
 *	spooler_alias:translator argvs: ( | (argv)+ )* [ (>|>>) outfile ]
 *
 *	The memory malloced for the token is exported
 * on entry
 *	*s		: string containing the spooler specification
 * on exit
 *	return		: a spooler token or NULL if an error occured
 */
static	Spooler	*spool_parse( s )
	char	*s;
{
	char	*alias = NULL;	/* the spooler name		*/
	char	*input_string;	/* the input string s		*/
	Args	*filter;	/* a filter chain		*/
	Args	*trans_args;	/* command line args for trans	*/
	char	*file = NULL;	/* output file			*/
	int	num_filt;	/* num filters in the chain	*/

	int	open_flags = O_WRONLY | O_CREAT;

	Spooler	*spooler;	/* pointer to spooler token	*/

	int	i, count;
	char	*cptr;
	Args	*filt;	


	/*
	 * make a copy of the input string
	 */
	if (! (input_string = malloc ((unsigned) (strlen (s) + 1)))) {
		return(NULL);
	}
	(void) strcpy(input_string, s);

	/*
	 * get the alias name
	 */
	while (isspace(*s)) s++;
	for (i=0, cptr=s; *cptr && *cptr != ':' && !isspace(*cptr); cptr++, i++)
	;

	if (i == 0) return (NULL);	/* no name	*/

	if ( !(alias = malloc ((unsigned) i + 1))) {
		return(NULL);
	}
	(void) strncpy(alias, s, i);
	alias[i] = '\0';

	s = cptr;
	while (isspace(*s)) s++;

	if (*s != ':')  return (NULL);	/* bad field	*/
	
	s++;	/* skip the ':'	*/

	/*
	 * grab the translator args
	 */
	trans_args = parse_trans_args( s );

	/*
	 * skip past the translator arg list
	 */
	for ( ; *s && *s != ':'; s++)
	;
	if (*s != ':') {	/* trans arg must be terminated by ':'	*/
		if (alias) free ((Voidptr) alias);
		if (input_string) free ((Voidptr) input_string);
		return (NULL);
	}
		 
	
	/*
	 * grab the filters. Look for ( | args )*
	 * first count how many there are so we can malloc memory easily
	 */
	for (num_filt = 0, cptr = s; *cptr; cptr++) {
		if (*cptr == '|') num_filt++;
	}

	if (num_filt > (MAX_FILTER - 1)) {
		(void) fprintf(stderr, "Maximum filter count exceeded\n");
		if (alias) free ((Voidptr) alias);
		if (input_string) free ((Voidptr) input_string);
		return(NULL);
	}

	if (num_filt) {
		if ( !(filter = (Args *) malloc (num_filt * sizeof (Args )))) {
			return(NULL);
		}
	}

	for (count = 0; count < num_filt; count++) {
		while (*s != '|') s++;

		s++;	/* skip the '|'	*/

		if ((filt = parse_filter_args( s )) == NULL) {
			if (alias) free ((Voidptr) alias);
			if (input_string) free ((Voidptr) input_string);
			if (trans_args) free ((Voidptr) trans_args);
			if (filter) free ((Voidptr) filter);
			return(NULL);	/* error parsing single filter	*/
		}
		filter[count] = *filt;
	}

	/*
	 * look for a file redirect, '>'
	 */
	for ( ; *s && *s != '>'; s++)
	; 

	if (*s == '>') {	/* output redirect	*/
		s++;
		if (*s == '>') {	/* concatenate output	*/
			s++;
			open_flags |= O_APPEND;
		}
		else {			/* overwrite or create new file	*/
			open_flags |= O_TRUNC;
		}

		while (isspace(*s)) s++;
		for (i = 0, cptr = s; *cptr && !isspace(*cptr); cptr++, i++)
		;
		if (i == 0) {
			return (NULL);
		}

		if (! (file = malloc ((unsigned) i + 1))) {
			return(NULL);
		}
		(void) strncpy(file, s, i);
		file[i] = '\0';
	}

	if (! (spooler = (Spooler *) malloc (sizeof (Spooler)))) {
		return(NULL);
	}

	spooler->trans_args = trans_args;
	spooler->alias = alias;
	spooler->input_string = input_string;
	spooler->filter = filter;
	spooler->num_filt = num_filt;
	spooler->file = file;
	spooler->open_flags = open_flags;

	return (spooler);
}
				


	

/*
 *	file_get_spooler
 *	[internal]
 *
 *	read in a list of spoolers from a file and add them to the spoolers
 *	table. If a named spooler already exist in the table replace it with 
 *	the new one. Lines beginning with a '#' are ignored. The format of 
 *	a spooler designator is the following
 *
 *		spooler_name : (translator args)* : (| (args)+ )* [(>|>>) file]
 *
 * on entry
 *	*fp		: pointer to spooler file
 *	*spoolers	: the spooler table
 */
static	file_get_spooler(fp, spoolers)
	FILE	*fp;
	Spoolers	*spoolers;
	
{
	char	buf[MAX_LINE_LEN];
	int	i, match;
	char	*s;
	Spooler	*spool;

	while (fgets(buf, MAX_LINE_LEN, fp) != NULL) {

		buf[strlen(buf) - 1] = '\0';	/* nuke the newline	*/

		s = buf;
		while (isspace(*s)) s++;

		if ( !*s || *s == '#') continue;

		spool = spool_parse( buf );

		if ( ! spool ) {	/* error in file	*/
			(void) fprintf(stderr, 
			"Invalid spooler entry %s\n", buf);
			continue;
		}

		/*
		 * see if spooler alias already exist. If so overwrite it
		 */
		for (i = 0, match = 0; i < spoolers->num; i++ ) {
			if (! strcmp(spool->alias, spoolers->spool[i].alias)) {
				spoolers->spool[i] =  *spool;
				match = 1;
				break;
			}
		}
		if (match) continue;

		/*
		 * add the new spooler specification
		 */
		if (spoolers->num == spoolers->size) {
			spoolers->size += SMALL_MALLOC_BLOCK;
			spoolers->spool = (Spooler *) realloc( 
				(char *) spoolers->spool, 
				(unsigned) ((spoolers->size * sizeof (Spooler)))
			);
		}

		spoolers->spool[spoolers->num++] =  *spool;
	}
}




/*
 *	InitSpool
 *	[exported]
 *	
 *	Init the spooler code. Init_Spool reads in the list of spoolers from
 *	the environment. Init_Spool first checks to see if the NCARV_SPOOL
 *	environment is set. If so its contents become the default spooler.
 *	Next, Init_Spool looks for a user ./.SPOOL_FILE in the user's home
 *	directory. If it exists and NCARV_SPOOL was not set than the first
 *	spooler given in this file becomes the default. Next Init_Spool
 *	performs the same action on a system level ./SPOOL_FILE file. 
 *	SPOOL_FILE is defined in spooler.h
 */
int	InitSpool()
{

	FILE	*fp;
	char	*ncarv_spool = NULL;
	char	*home;
	char	*home_spool_file = NULL;
	char	*sys_spool_file = NULL;
	const char	*libpath;

	Spooler	*spool;

	spoolers.spool = (Spooler *) malloc (
		SMALL_MALLOC_BLOCK * sizeof (Spooler)
	);
	if (! spoolers.spool) {
		perror("malloc()");
		return(-1);
	}
	spoolers.num = 0;
	spoolers.size = SMALL_MALLOC_BLOCK;

	isInitialized = FALSE;

	/*
 	 * see if spooler given in the NCARV_SPOOL environment variable
	 */
	if ((ncarv_spool = getenv ("NCARV_SPOOL")) != NULL) {
		spool = spool_parse( ncarv_spool );
		if ( spool ) {
			spoolers.spool[spoolers.num++] =  *spool;
		}
		else {
			(void) fprintf(stderr,
			"Invalid spooler entry %s\n",ncarv_spool);
		}
	}

	if ( !(libpath = GetNCARGPath(LIBDIR))) {
		fprintf(stderr, "Warning: Cannot find system spooler config file [ %s ]\n", ErrGetMsg());
	}
	else {

		/*
		 * build path to system level spooler file
		 */
		sys_spool_file = malloc (
			(unsigned) ( strlen (libpath)
			+ strlen ("/")
			+ strlen (NCARGDIR)
			+ strlen ("/")
			+ strlen ((char *) SPOOL_FILE)
			+ 1)
		);
		if (! sys_spool_file) {
			perror("malloc()");
			return(-1);
		}

		(void) strcpy (sys_spool_file, libpath);
		(void) strcat (sys_spool_file, "/");
		(void) strcat (sys_spool_file, NCARGDIR);
		(void) strcat (sys_spool_file, "/");
		(void) strcat (sys_spool_file, (char *) SPOOL_FILE);

		/*
		 * read in spooler specifications from system file
		 */
		if ((fp = fopen(sys_spool_file, "r")) != NULL) {

			file_get_spooler(fp, &spoolers);
			(void) fclose(fp);
		}
	}

	/*
	 * build path to user spooler file
	 */
	if ((home = getenv ("HOME")) != NULL) {
		home_spool_file = malloc (
				(unsigned) (strlen (home) 
				+ strlen ("/.")
				+ strlen (SPOOL_FILE)
				+ 1)
		);
		if (! home_spool_file) {
			perror("malloc()");
			return(-1);
		}

		(void) strcpy (home_spool_file, home);
		(void) strcat (home_spool_file, "/.");
		(void) strcat (home_spool_file, SPOOL_FILE);

		/*
		 * read in spooler specifications from user spooler file
		 */
		if ((fp = fopen(home_spool_file, "r")) != NULL) {

			file_get_spooler(fp, &spoolers);
			(void) fclose(fp);
		}
		
	}

	if (spoolers.num != 0) 
		currentSpooler = spoolers.spool;

	if (sys_spool_file) free ((Voidptr) sys_spool_file);
	if (home_spool_file) free ((Voidptr) home_spool_file);
	isInitialized = TRUE;
}


/*
 *	AddSpooler
 *	[Exported]
 *
 *	Add a spooler specification to the spooler table If a named spooler 
 *	already exist in the table replace it with 
 *	the new one. The spooler added becomes the current spooler. If 
 *	AddSpooler fails the current spooler is unchanged. Lines beginning 
 *	with a '#' are ignored. The format of a spooler designator is the 
 *	following;
 *
 *		spooler_name : (translator args)* : (| (args)+ )* [(>|>>) file]
 *
 * on entry
 *	*s		: contains a spooler specification of above form
 * on exit
 *	return		: < 0 => failure
 */
AddSpooler(s)
	char	*s;
	
{
	int	i;
	Spooler	*spool;

	if (! isInitialized)  {
		return(-1);
	}


	spool = spool_parse( s );

	if ( ! spool ) {	/* error in file	*/
		(void) fprintf(stderr, "Invalid spooler entry %s\n", s);
		return( -1);
	}

	/*
	 * see if spooler alias already exist. If so overwrite it
	 */
	for (i = 0; i < spoolers.num; i++ ) {
		if (! strcmp(spool->alias, spoolers.spool[i].alias)) {
			spoolers.spool[i] =  *spool;
			return(1);
		}
	}

	/*
	 * add the new spooler specification
	 */
	if (spoolers.num == spoolers.size) {
		spoolers.size += SMALL_MALLOC_BLOCK;
		spoolers.spool = (Spooler *) malloc (
			(unsigned) (spoolers.size * sizeof (Spooler))
		);
	}

	spoolers.spool[spoolers.num++] =  *spool;
	currentSpooler = spool;

	return (1);
}

/*
 *	SetCurrentSpooler
 *	[exported]
 *	
 *	Set the current spooler specification. Subsequent calls to PipeLine
 *	will use whatever specification is associated with alias
 *
 * on entry 
 *	*alias		: name of spooler specifiction
 * on exit
 *	return		: < 0 => alias not in database, else ok
 */
SetCurrentAlias(alias)
	char	*alias;
{
	int	i;
	if (! isInitialized)  {
		return(-1);
	}

	for (i = 0; i < spoolers.num; i++) {
		if (strcmp(spoolers.spool[i].alias, alias) == 0) {
			currentSpooler = &spoolers.spool[i];
			return (1);
		}
	}

	return (-1);
}

/*
 *	GetCurrentAlias
 *	[exported]
 *	
 *	Get the current spooler specification name 
 *
 * on exit
 *	return		: current spooler alias, NULL if none set.
 */
char	*GetCurrentAlias()
{
	if (! isInitialized)  {
		return(NULL);
	}
	if (currentSpooler == NULL) 
		return (NULL);

	return (currentSpooler->alias);
}

/*
 *	GetSpoolers
 *	[exported]
 *
 *	Return the list of spoolers from the spooler table. The list returned
 *	is in the format used to originally specify the spooler. This list 
 *	returned is in private memory and should thus be copied.
 *
 * on entry
 *	*alias		: name of a spooler or NULL
 * on exit
 *	return		: if ! alias return all the spoolers in the spooler 
 *			  table. if alias return specification for that spooler
 *			  if it exist, else return NULL
 */
char	**GetSpoolers(alias) 
	char	*alias;
{
	int	i;

	static	char	**spool_list = NULL;

	if (! isInitialized)  {
		return(NULL);
	}

	/*
	 * free memory from last call and malloc enough memory for this one.
	 */
	if (spool_list)  free ((Voidptr) spool_list);
	spool_list = (char **) malloc (
		(unsigned) ((spoolers.num +1) * sizeof (char **))
	);
	

	if (alias) {
		for (i = 0; i < spoolers.num; i++) {
			if (strcmp(spoolers.spool[i].alias, alias) == 0) {

				spool_list[0] = spoolers.spool[i].input_string;
				spool_list[1] = NULL;
				return (spool_list);
			}
		}
		return (NULL);	/* no match	*/
	}

	/*
	 * return complete list
	 */
	for (i = 0; i < spoolers.num; i++) {
		spool_list[i] = spoolers.spool[i].input_string;
	}
	spool_list[i] = NULL;
	return (spool_list);

}

/*
 *	PipeLine
 *	[exported]
 *
 *	build a pipeline of commands to execute starting with the command
 *	given by argv. The rest of the chain comes from the filter list
 *	pointed to by currentSpooler. The processes are spawned and PipeLine
 *	returns without waiting for the children to terminate. PipeLine
 *	increments a global variable 'spoolerJobs' for every process it
 *	initiates. It is the calling programs responsibility to wait on 
 *	spawned jobs and decrement spoolerJobs appropriately.
 *
 * on entry:
 *	**argv		: null terminate command and option list
 *	argc		: num args in argv
 *	log_fp		: file descriptor for logging normal status messages
 *			: If NULL nothing gets logged.
 * on exit:
 *	return		: < 0 => failure
 */
PipeLine(argc, argv, log_fp)
	int	argc;
	char	**argv;
	FILE	*log_fp;

{
	int	i,j;
	int	fd = -1;
	int	out_pipe[2], in_pipe[2];

	char	*file = currentSpooler->file;
	int	flags = currentSpooler->open_flags;
	int	num_filt = currentSpooler->num_filt;
	Args	*trans_args = currentSpooler->trans_args;
	Args	*filter = currentSpooler->filter;

	char	**t_argv;
	int	t_argc;

	void	(*istat)();

	if (! isInitialized)  {
		return(-1);
	}

	/* 
	 * if output is to be redirected to a file see if we can open the 
	 * file before doing anything else
	 */
	if (file) {
		if ((fd = open (file, flags, 0644)) < 0) {
			perror ((char *) NULL);
			return(-1);
		}
	}

	/*
	 * combine args passed in as parameters with args from 
	 * currentSpooler->trans_args;
	 */
	t_argc = argc + trans_args->argc;
	if (! (t_argv = (char **) malloc ((t_argc +1) * sizeof(char **)))) {
		perror("malloc()");
		return(-1);
	}

	for (i = 0; i < argc; i++) {
		t_argv[i] = argv[i];
	}

	for (j = 0; i < t_argc; i++, j++) {
		t_argv[i] = trans_args->argv[j];
	}
	t_argv[t_argc] = NULL;

	/*
	 * turn off interupts for parent and all children
	 */
	istat = signal(SIGINT, SIG_IGN);
		

	/*
	 *	Spawn the processes in the pipeline in the reverse order
	 *	of the pipeline
	 */

	if (pipe(out_pipe) == -1) {
		perror("pipe");	
		return (-1);
	}

	/*
	 * the last filter in the chain
	 */
	if (num_filt) {	/* if no filters than translator is last process*/
		last(filter[num_filt-1].argv, out_pipe, fd, log_fp);
	}
	else {		/* the translator is first and last process	*/
		last(t_argv, out_pipe, fd, log_fp);
	}
	if (fd > 0) (void) close (fd);


	/*
	 * the middle filters in reverse order
	 */
	for (i = num_filt - 2; i >= 0; i--) {
		if (pipe(in_pipe) == -1) {
			perror("pipe");	
			return (-1);
		}
		middle(filter[i].argv, out_pipe,in_pipe, log_fp);

		(void) close(out_pipe[0]);
		(void) close(out_pipe[1]);
		out_pipe[0] = in_pipe[0];
		out_pipe[1] = in_pipe[1];

	}

	/* 
	 * fork translator if haven't alread done so as Last process
	 */
	if (num_filt) {
		first(t_argv, out_pipe, log_fp);

		(void) close(out_pipe[0]); 
		(void) close(out_pipe[1]); 
	}


	/*
	 * turn on interupts for parent 
	 */
	(void) signal(SIGINT, istat);
	return(1);
}

