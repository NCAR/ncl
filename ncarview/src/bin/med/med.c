/*
 *	$Id: med.c,v 1.9 2008-07-27 03:18:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/


/*
 *	med.c
 *
 *	Author		John Clyne
 * 
 *	Date		Fri Jan 26 16:43:39 MST 1990
 *
 *	Med is a interactive interface to the cgm_edit library. The interface
 *	was designed to be similar to the UNIX ed editor. Med performs 
 *	frame-level editing on a NCAR CGM. When invoked with a valid CGM
 *	med acts like a line editor. Each line in med coresponds to a frame
 *	in the metafile. Users may perform actions on "lines" in the buffer
 *	in much the same way as in a simple line editor. The results are
 *	reflected in the metafile produce when the buffer is written. The 
 *	syntax for addressing lines and giving commands is identical to 
 *	that in ed (almost).
 */
#include	<stdio.h>
#include	<stdlib.h>
#include	<ctype.h>
#include	<string.h>
#include	<signal.h>
#include	"med.h"

/*
 *	A global data structure for med
 */
static	MedData	medData = {
	NULL,			/* optional command file	*/
	{NULL,-1,-1,-1, NULL},	/* a parsed command		*/
	1,			/* the current line (frame)	*/
	0,			/* the last line in the buffer	*/
	FALSE,			/* is command input from a tty	*/
	NULL,			/* a command line med command list	*/
	NULL
	};

static	char	line_buf[MAX_LINE_LEN];	/* a single med command with data */

static	char	message_buf[80];

char	*localTmp = NULL;		/* tmp directory to use if	*/

extern	Cmd	cmdtab[];


/*
 *	get_line
 *	[internal]
 *
 *	get a line of text from either a file if there is a valid file pointer
 *	or from the commands buffer.
 */
static char	*get_line(med_data)
	MedData	*med_data;
{
	char	*line;
	char	*cptr;

	/*
	 * if commands are in a file then read them from file	
	 */
	if (med_data->fp) {
		line = fgets(line_buf, MAX_LINE_LEN, med_data->fp);
	}
	else {	/* read next command from command line	*/
		line = med_data->command_string;
		if (!line) return NULL;

		/*
		 * advance command pointer to next line
		 */
		cptr = strchr(med_data->command_string, '\n');
		if (cptr) {	/* more commands follow	*/
			*cptr = '\0';
			med_data->command_string = cptr+1;
		}
		else {		/* no more commands	*/
			med_data->command_string = NULL;
		}
	}

	return(line);
}

/*
 *	getcmd
 *	
 *	get a command object from the command table using its name for a
 *	lookup. Command names may be abreviated.
 * on entry
 *	*name		: name to lookup
 * on exit
 *	return		: if found return command obj ptr. If name is ambiguous
 *			  return -1. If not found return NULL.
 */
Cmd	*getcmd (name)
	char	*name;
{
	char *p, *q;
	Cmd *c, *found;
	int nmatches, longest;

	longest = 0;
	nmatches = 0;
	found = NULL;

	for (c = cmdtab; p = c->c_name; c++) {
		for (q = name; *q == *p++; q++) {
			if (*q == 0)            /* exact match? */
				return (c);
		}
		if (!*q) {                      /* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = c;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)	/* ambiguous	*/
		return ((Cmd *)-1);
	return (found);
}

/*
 * parse_command_name
 * [internal]
 *
 *	load the first string of alphabetic characters found in line into
 *	the command data structure in med_data.
 * on entry
 *	*line		: '\n' terminated string
 * on exit
 *	return		: NULL => failure
 */
static	char	*parse_command_name(line, med_data)
	char	*line;
	MedData	*med_data;
{
	
	static	char	name_buf[80];	/* static storage for the name	*/
	char	*cptr = name_buf;

	if (!line) return(line);	/* error			*/

	name_buf[0] = '\0';		/* clear the name buf		*/

	while(isspace(*line))
		line++;			/* skip white space		*/

	med_data->command_data.name = name_buf;

	/*
	 * a hack for a shell escape '!'
	 */
	if (*line == '!') 
		*cptr++ = *line++;
	else {
		while(isalpha(*line)) {
			*cptr++ = *line++;
		}
	}

	*cptr = '\0';
		
	return(line);
}	
	

/*
 *	parse_address:
 *	[internal]
 *
 *	parse a string looking for an address of the form :
 *	[ [0-9]* | '.' | '$'] [ <'+' | '-'> <0-9>* ]
 *
 * 	If a relative address is specified without the absloute part then
 *	the address is assumed relative to the current_frame.
 *
 * on exit
 *	return_address	: the address if found else -1; 
 *	return		: NULL if error, else pointer to next item in line 
 *	
 */

static	char	*parse_address(line, med_data, return_address)
	char	*line;
	MedData	*med_data;
	int	*return_address;
{
	int	tmp;
	char	plus;	/* boolean	*/

	*return_address = -1;


	if (!line) return(line);


	while(isspace(*line))
		line++;			/* skip white space		*/

	/*
	 * look for an absolute address specification
	 */
	if (isdigit(*line)) {
		(void) sscanf(line, "%d", return_address);
		while (isdigit(*++line))
		;

	} else if (*line == '.') {
		*return_address = med_data->current_frame;
		line++;
	} else if (*line == '$') {
		*return_address = med_data->last_frame;
		line++;
	}

	while(isspace(*line))
		line++;			/* skip white space		*/

	/*
	 * look for relative address specification to combine with absolute
	 * address. if no absolute address was given assume current frame
	 */
	if (*line == '+' || *line == '-') {
		plus = *line;		/* tmp record			*/
		while(isspace(*++line))	/* skip '+' and white space	*/
		;
		if (isdigit(*line)) {
			(void) sscanf(line, "%d", &tmp);
			while (isdigit(*++line))
			;
		}
		else {
			return(NULL);	/* error	*/
		}

		if (plus == '-') tmp *= -1;
		/*
		 * see if a absolute address was already specified. if not
		 * use current frame
		 */
		if (*return_address == -1) {
			*return_address = tmp + med_data->current_frame;
		}
		else {
			*return_address += tmp;
		}
	}

	return (line);
}


/*
 *	parse_command
 *	[internal]
 *
 *	parse a command string into a standard format and return it.
 *	A command may be of the form :
 *
 *	Command ::= [address [',' address]] [command] [address] [ file ]
 *
 *	address ::= < ['+' | '-'] int | '.' | '$' >
 *
 *	command ::= < 'a' - 'z' >*
 *
 *	file	::= < keyboard_character >*
 *	
 * on entry:
 *	*line			: contains user input terminated by a '\n'
 * on exit:
 *	med_data.command_data	: contains the command and data
 *
 */
static	char	*parse_command(med_data, line)
	MedData	*med_data;
	char	*line;
{

	int	*add_ptr = &(med_data->command_data.add1);

	char	*cptr;
	static	char	file_buf[80];


	/*
	 * clear the command_data
	 */
	med_data->command_data.name = NULL;
	*add_ptr = -1;
	*(add_ptr+1) = -1;
	*(add_ptr+2) = -1;
	med_data->command_data.file = NULL;


	/*
	 * scan the line
	 */

	/*
	 * get first address
 	 */
	if ((line = parse_address(line, med_data, add_ptr)) == NULL) {
		return(NULL);
	}

	/*
	 * see if there is a second address. if so grab it
	 */
	if (*line == ',') {
		line++;

		if ((line = parse_address(line, med_data, add_ptr+1)) == NULL){
			return(NULL);
		}
	}

	/*
	 * get command name
	 */
	if ((line = parse_command_name( line, med_data )) == NULL) {

		return (NULL);
	}

	while(isspace(*line))	/* skip white space	*/
		line++;
	/*
	 * see if there is a third address. If so grab it
	 */
	if (isdigit(*line) || *line == '.' || *line == '+' || *line == '-'
				|| *line == '$') {

		if ((line = parse_address(line, med_data, add_ptr+2)) == NULL) {
			return(NULL);
		}
	}
	if (*line) {
		cptr = file_buf;
		while (*line != '\n' && *line != '\0') {
			*cptr++ = *line++;
		}
		*cptr = '\0';

		/* 
		 * remove trailing blanks 
		 */
		cptr--;
		while (isspace(*cptr)) {
			*cptr = '\0';
			cptr--;
		}
	
		med_data->command_data.file = file_buf;
	}
#ifdef	DEAD
	else {	/* see if there is a final arg string	*/
		if (*line) {
			cptr = file_buf;
			while (*line != '\n' && *line != '\0') {
				*cptr++ = *line++;
			}
			*cptr = '\0';

			/* 
			 * remove trailing blanks 
			 */
			cptr--;
			while (isspace(*cptr)) {
				*cptr = '\0';
				cptr--;
			}
		
			med_data->command_data.file = file_buf;
		}
	}
#endif

	return (line);
}

/*
 *	interupt signal handler
 */
static  void    sigint_handler(sig)
        int     sig;
{
	medData.command_data.name = "quit!";
	medData.command_data.add1 =
	medData.command_data.add2 =
	medData.command_data.add3 = -1;
	medData.command_data.file = NULL;
	medData.c = getcmd(medData.command_data.name);

	medQuit(&medData);
}

		
static	usage(message) 
	char	*message;
{

	if (message) {
		(void) fprintf(stderr, "med: %s\n", message);
	}
	(void) fprintf (
		stderr, "med: usage: [-e script] [-f sfilename] [-l directory] [filename]\n"); 
	exit(1);
}
main (argc, argv)
	int	argc;
	char	**argv;
{
	char	*line;		/* a single unparsed med command	*/
	char	*comm_file = NULL;	/* file of med commands		*/
	char	*cptr;

	char	*file = NULL;	/* file to edit				*/
	char	*prog_name = argv[0];

	Cmd	*c;

	argc--; argv++;	/* skip argv[0]	*/

	medData.fp = stdin;
	medData.command_string = (char *) malloc (1);
	medData.command_string[0] = '\0';

	/*
	 * parse the command line
	 */
	while (argc > 0 && **argv == '-') {
		for (cptr = *argv + 1; *cptr; cptr++)
			switch (*cptr) {

			case 'e':	/* med commands on command line	*/
				medData.command_string = realloc(
					medData.command_string, (unsigned) (
					strlen(medData.command_string) + 
					strlen(*(++argv)) + 
					strlen("\n") + 1)
				);

				(void) strcat (medData.command_string, *argv);
				(void) strcat (medData.command_string, "\n");
				argc--;
				break;

			case 'f':	/* med commands in a file	*/
				comm_file = *(++argv);
				argc--;
				break;

			case 'l':	/* med commands in a file	*/
				localTmp = *(++argv);
				argc--;
				break;
			case 'V':	/* Version */
				PrintVersion(prog_name);
				exit(1);

			default:
				(void) sprintf(message_buf, 
						"unknown option <%c>", *cptr);
				usage(message_buf);
		}
		argc--, argv++;
	}

	if (argc > 0) {	/* look for a file to edit	*/
		file = *argv;
		if (--argc) {
			usage("too many files");
		}
	}

	/*
	 * if a command file was given then open it. If no command file is
	 * given and no commands were on the command line then we get them
	 * from the tty.
	 */
	if (comm_file) {
		if ((medData.fp = fopen(comm_file, "r")) == NULL) {
			perror("med");
			exit(1);
		}
	}

	/*
	 * if the command script comes from a file make sure its a tty
	 */
	if (!strlen(medData.command_string)) {
		medData.fromatty = isatty(fileno(medData.fp));
	}
	else {
		medData.fp = NULL;	/* commands on command line	*/
	}
		


	/* 
	 * if a meta file was given on the command line build a bogus
	 * medData structure containing the metafile name and invoke
	 * medEdit with the file name. This is equivellant to the user 
	 * specifying a metafile interatively.
	 */
	if (file) {
		medData.command_data.name = "edit";
		medData.command_data.add1 =
		medData.command_data.add2 =
		medData.command_data.add3 = -1;
		medData.command_data.file = file;
		medData.c = getcmd(medData.command_data.name);
	
		medEdit(&medData);
	}

	(void)signal(SIGINT,&sigint_handler);
		

	/*
	 * process commands from user forever.
	 */
	while (1) { 
		if (medData.fromatty) {
			(void) printf("med> ");
			(void) fflush(stdout);
		}
	
		/*
		 * get the next line of command input
		 */
		if ((line = get_line(&medData)) == NULL) {
			line = "quit!\n";/* force med to quit gracefully*/
		}
		if (line[0] == '\0') {
			line = "quit!\n";/* force med to quit gracefully*/
		}

		/*
		 * parse the new command string
		 */
		if (parse_command(&medData, line) == NULL) {
			error_message(&medData,0, "Invalid format?", MED_WARN);
			continue;
		}

		/*
		 * no command given, so do a print after advancing current frame
		 */
		if (*medData.command_data.name == '\0') {
			if ( medData.command_data.file == NULL) {
			
				medData.current_frame++;
				medData.command_data.name = "print\0\n";
			}
			else {
				error_message(&medData,0, "Syntax?", 
								MED_WARN);
				continue;
			}
		}


		/*
		 * find the command object in the command table
		 */
		c = getcmd(medData.command_data.name);

		if (c == (Cmd *) -1) {
			error_message(&medData,0,"Ambiguous command?",MED_WARN);
			continue;
		}
		if (c == (Cmd *) NULL) {
			error_message(&medData,0, "Invalid command?", MED_WARN);
			continue;
		}

		/*
		 * execute the command
		 */
		medData.c = c;
		(*c->c_handler) (&medData);
	}
}

