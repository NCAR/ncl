/*
 *	$Id: ictrans_mem.c,v 1.9 2008-07-27 03:18:45 haley Exp $
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
 *	ictrans_mem.c
 *
 *	Author		John Clyne
 *
 *	Date		Thu Jun  7 17:48:58 MDT 1990
 *
 *
 *	This module contains an interface for invoking ictrans on a CGM
 *	contained in memory. The memory resident CGM must be written using
 *	using the memory I/O functions in libcgm *after* creating a memory
 *	file using ictrans_open(). After the file is written with the libcgm
 *	routines it must be closed with ictrans_close(). It can than be
 *	previewed with ictrans by calling invoke_ictrans(). It is possible
 *	to have multiple memory files if desired. Each memory file has a 
 *	user defined unit number associated with it. 
 *	
 */

 
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <ncarg/c.h>
#include <ncarg/cgm_tools.h>
#include "ictrans_mem.h"


/*
 * structure for keeping track of all the memory files
 */
static	struct	{
	char	*fname;		/* name of a memory file	*/
	Cgm_fd	fd;		/* file descriptor returned at creation time */
	} open_files[MAX_MEM_FILE];

/*
 * structure for keeping track of ictrans args to be used when translating
 * a particular memory file
 */
static	char	*Args[MAX_MEM_FILE];

/*
 *	ictrans_args
 *	[exported]
 *
 *	create an argument list to be passed to ictrans when translating
 *	a memory file.
 *
 * on entry
 *	unit		: specifies memory file to associate arg list with
 *	*args		: a space separated arg list for ictrans
 */
ictrans_args(unit, args)
	int	unit;
	char	*args;
{
	int	i;
	static	int	init = 0;

	if (unit >= MAX_MEM_FILE) return (-1);

	if (!init) { 	/* one time initialization	*/
		for (i = 0; i < MAX_MEM_FILE; i++) {
			Args[i] = NULL;
		}
		init++;
	}

	if (Args[unit]) free((Voidptr) Args[unit]);

	Args[unit] = malloc((unsigned) (strlen(args) + 1));

	(void) strcpy(Args[unit], args);	/* store the arg list	*/
	return(1);
}


/*
 *	ictrans_open
 *	[exported]
 *
 *	Create a memory file and associate it with a unit number. 
 *
 * on entry
 *	unit		: unit number to associate with the new memory file
 *	record_size	: size of the CGM records in bytes
 * on exit
 *	return		: == -1 => error, else a file descriptor for a mem
 *			  file open for writing or reading via CGM I/O routines
 *			  in libcgm
 */
ictrans_open(unit, record_size)
	int	unit;
	int	record_size;
{
	static	int	init = 0;
	char	sunit[8];

	char	*fname;		/* name of the memory file created	*/
	int	fd;		/* file descriptor for the file		*/

	int	i;

	if (unit >= MAX_MEM_FILE) return (-1);


	if (!init) { 	/* one time initialization	*/
		for (i = 0; i < MAX_MEM_FILE; i++) {
			open_files[i].fname = NULL;
			open_files[i].fd = M_CLOSED;
		}
		init++;
	}

	/*
	 * create a unique name for the memory file to open
	 */
	(void) sprintf(sunit, "%d", unit);
	if ( !(fname = malloc(strlen(SCRATCH) + strlen(sunit) + 1))) {
		return(-1);
	}
	(void) strcpy(fname, SCRATCH);
	(void) strcat(fname, sunit);

	/*
	 * create a memory file open for reading and writing
	 */
	if ((fd = (int) CGM_open(fname, -(record_size), "w+")) < 0 ) {
		return (-1);
	}

	/*
	 * associate the file with the unit number
	 */
	open_files[unit].fname = fname;
	open_files[unit].fd = fd;

	return (fd);
}


/*
 *	ictrans_close:
 *	[exported]
 *
 *	close a memory file associated with unit number opened with 
 *	ictrans_open();
 *
 * on entry:
 *	unit		: identifies the file to close
 * on exit
 *	return		: < 0 => error, else ok
 */ 
ictrans_close(unit)
	int	unit;
{
	int	fd = open_files[unit].fd;

	open_files[unit].fd = M_CLOSED;

	return (CGM_close(fd));
}

/*
 *	ictrans_free:
 *	[exported]
 *
 *	remove a memory file created with ictrans_open(). The file must have
 *	been closed with ictrans_close(). ictrans_free() frees all resources
 *	associated with the memory file.
 *
 * on entry
 *	unit		: identifies the file to remove
 * on exit
 *	return		: < 0 => error, else ok
 */
ictrans_free(unit) 
	int	unit;
{
	int	fd = open_files[unit].fd;
	char	*fname = open_files[unit].fname;

	if (fd != M_CLOSED || fname == NULL) return (-1);

	CGM_unlinkMemFile(fname);	/* remove the file	*/

	if (fname) free((Voidptr)fname);
	
	open_files[unit].fname = NULL;	/* free an entry in the open_file tab*/
	open_files[unit].fd = -1;

	return(1);

}


/*
 *	ictrans_invoke
 *	[exported]
 *
 *	invoke ictrans to process a file created with ictrans_open. 
 *	ictrans_invoke looks a environment variable defined by ICTRANS_ARGS
 *	If it exists it is used as the argument list to pass to ictrans. If
 *	ICTRANS_ARGS does not exist ictrans_invoke looks for an entry in the
 *	Args table. If there is one it is used as the arg list for ictrans.
 *
 * on entry
 *	unit		: specifies the file created with ictrans_open
 * on exit
 *	return		: < 0 => error, else ok
 */
ictrans_invoke(unit)
	int	unit;
{

	int	argc = 0;
	char	**argv;

	char	*args;

	int	i,
		len;
	char	*s;

	char	*getenv();

	if (open_files[unit].fd != M_CLOSED || open_files[unit].fname == NULL){
		return (-1);
	}

	/*
	 * if ICTRANS_ARGS environment variable is set use its arg list. 
	 * else use Args if set.
	 */
	if ((args = getenv(ICTRANS_ARGS)) == NULL) {
		args = Args[unit];	/* get args from Args	*/
	}

	/*
	 * build argv if arg list exists
	 */
	argc = 0;
	if (args) {

		while (isspace(*args))	/* skip initial white space	*/
			args++;

		/*
		 * count number of args in args so we can alloc memory easily.
		 */
		for (s = args; *s != '\0'; ){
			argc++;
			while (! isspace(*s) && *s != '\0')
				s++;
			while ( isspace(*s))
				s++;
		}
	}

	argc++;	/* add one for program name	*/

	/*
	 * build the argv list from args;
	 */
	if (! (argv = (char **) malloc ((argc + 2) * sizeof (char *)))) {
		return(-1);
	}

	argv[0] = ICTRANS_NAME;
	for (i = 1, s = args; i < argc; i++) {
		while (! isspace(*s) && *s != '\0')
			s++;

		len = s - args;		/* length of arg	*/
		if ( !(argv[i] = malloc ((unsigned) len + 1))) {
			return(-1);
		}
		(void) strncpy(argv[i], args, len);
		argv[i][len] = '\0';

		while (isspace(*s))
			s++;
		args = s;
	}
	argv[i] = NULL;

				
	/*
	 *	call ictrans 
	 */
	if (ICTrans(argc, argv, open_files[unit].fname) < 0) {
		return(-1);
	}

	return (1);
}
