/*
 *	$Id: wks.c.sed,v 1.1.1.1 1992-04-17 22:34:01 ncargd Exp $
 */
/***********************************************************************
*                                                                      *
*		NCAR Graphics - UNIX Version 3.1.2                     *
*		Copyright (C) 1987, 1988, 1989, 1991                   *
*		University Corporation for Atmospheric Research        *
*		All Rights Reserved                                    *
*                                                                      *
*                                                                      *
***********************************************************************/

/***********************************************************************
*
*	File:		wks.c
*
*	Description:
*
*	This file contains a set of routines for basic file and pipe 
*	I/O for NCAR Graphics. These functions are generally called
*	from the Fortran layer of NCAR Graphics. The calling routines 
*	believe that these routines are performing I/O using Fortran
*	logical units. This is only simulated, however, and standard 
*	C I/O is used.
*
*	Output is by default directed to a file named "gmeta".
*	The environment variable NCARG_GKS_OUTPUT can be used
*	to redirect output to a different file name, if desired,
*	or even a translator process.
*
*	Examples:
*			setenv NCARG_GKS_OUTPUT myfile
*
*	causes CGM output to be place in "myfile".
*
*			setenv NCARG_GKS_OUTPUT "|ctrans"
*
*	causes the "ctrans" translator to be forked and CGM output
*	piped to it.
*
*			setenv NCARG_GKS_OUTPUT "|"
*
*	causes a default translator, defined in wks.h, to be forked
*	and CGM output to be piped to it. Note that not just any
*	process can be used here because wks.c assumes that
*	the translator is invoked as "translatorname -" where
*	the "-" indicates that the translator is to read from stdin.
*
*	If output is to a standard file, buffered I/O is used. On
*	some systems it is desirable to adjust this and several
*	options for this are available.
*	Buffer size is adjustable as follows:
*
*	1. If environment variable NCARG_GKS_BUFSIZE is set to N
*
*			Buffer Size = N * 1024 bytes.
*
*	2. Otherwise, if the value of DEFAULT_GKS_BUFSIZE in wks.h
*	   is edited by the Makefile to be non-zero.
*
*			Buffer Size = DEFAULT_GKS_BUFSIZE * 1024 bytes.
*
*	3. Otherwise, (when DEFAULT_GKS_BUFSIZE is 0)
*
*			Buffer Size = BUFSIZ (from stdio.h)
*
*
*	Author:		Don Middleton
*			NCAR Scientific Computing Division
*
*	Last Revised:	October 1991
*
***********************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/file.h>
#include "wks.h"

/*
The table "mftab" is used to maintain the list of fake Fortran
logical units. The file pointer, type of file, and any associated
I/O buffers are maintained here.
*/

static struct
{
	FILE	*fp;
	int	type;
	char	*buf;
} mftab[MAX_UNITS];

#ifdef STANDALONE
main()
{
	int	unit = 1;
	char	*string = "wooga";

	opnwks_(&unit);
	wrtwks_(&unit, (int *)string);
	clswks_(&unit);
}
#endif STANDALONE

/*************************************************************************
*
*	Function:	opnwks_(unit, string, status)
*
*			The filename contained in "string" is
*			opened an associated with a fake Fortran
*			LU number, "unit". The argument "status"
*			is loaded with a status code on return.
*
*	Called From:	Mostly Fortran routines in the NCAR GKS library.
*
*	Returns:	"status", which is encoded as per NCAR Graphics
*			GKS manual. Does not return a meaningful
*			function value.
*
************************************************************************/

#ifdef ardent
opnwks_(unit, string, status)
#else
opnwks_(unit, fname, status)
#endif
	int	*unit;
#ifdef ardent
	FortranString	*string;
#else
	char	*fname;
#endif
	int	*status;
{
	static int	init = 0;
	int		i, pipes[2], type, stat;
	char		*gks_output, *gks_translator;
	char		*getenv(), *p;
	char		*malloc();
	char		*bufsize_env = (char *) NULL;
	int		bufsize = 0;
#ifdef ardent
	char		*fname;

	fname = string->text;
#endif

#ifdef DEBUG
	(void) fprintf(stderr, "opnwks_(%d,%10s)\n",*unit,fname);
#endif
	/* Initialize the table that is used to track lu's */

	if (!init)
	{
		for(i=0; i<MAX_UNITS; i++)
		{
			mftab[i].fp   = MF_CLOSED;
			mftab[i].type = NO_OUTPUT;
			mftab[i].buf  = (char *) NULL;
		}
		init++;
	}

	if (*unit >= MAX_UNITS)
	{
		(void) fprintf(stderr,
		"Error in opnwks_(): Max workstations (%d) exceeded\n"
		,MAX_UNITS);
		*status = 304;
		return(0);
	}

	if (mftab[*unit].fp != MF_CLOSED)
	{
		(void) fprintf(stderr, 
		"Error in opnwks_(): Attempt to reopen unit %d\n", *unit);
		*status = 304;
		return(0);
	}

	/* Determine whether to use file or pipe I/O and related names. */

	if (!strcmp(fname, "GMETA"))
	{
		/* Catch the GMETA instance, its special */

		gks_output = getenv("NCARG_GKS_OUTPUT");
		if (gks_output  == (char *)NULL)
		{
			type = FILE_OUTPUT;
			gks_output = "gmeta";
		}
		else
		{
			for(p=gks_output; *p==' ' && *p != '\0'; p++);
	
			if (*p == '|')
			{
				for(p++; *p==' ' && *p != '\0'; p++);
	
				type = PIPE_OUTPUT;
				gks_translator = p;
			}
			else
			{
				type = FILE_OUTPUT;
				gks_output = p;
			}
		}
	}
	else
	{
		type = FILE_OUTPUT;
		gks_output = fname;
	}


	/*
	If GKS output is a file, open it for reading and writing.
	If its the special file "gmeta", truncate a pre-existing
	file or create a new one for writing. Otherwise
	create a new file if necessary, or append to the old.
	A stream is attached to the file and a buffer allocated
	of the size constant BUFSIZ from stdio.h or from
	the user-defined environment variable NCARG_GKS_BUFSIZE,
	which specifies the number of 1024byte blocks to
	use for I/O.

	If GKS output is a process, it is assumed to be
	a translator invoked as follows:

			translatorname -
	
	It is assumed to read from stdin. The process is forked and
	file streams are connected to pipe file descriptors. Stream
	I/O is unbuffered.
	*/

	if ( type == PIPE_OUTPUT )
	{
		if ( strlen(gks_translator) == 0 )
		{
			gks_translator = DEFAULT_TRANSLATOR;
		}

		(void) pipe(pipes);

#if defined(CRAY) || defined(RS6000) || (defined(sgi) && defined(mips))
		if ( fork() == 0 )
#else
		if ( vfork() == 0 )
#endif CRAY
		{
			(void) close(pipes[1]);
			(void) close(0); 
			(void) dup(pipes[0]);
			(void) execlp(gks_translator, 
				      gks_translator, "-", (char *) 0);
			(void) fprintf(stderr, 
			"Error in opngks_(): Could not fork translator\n");
			*status = 304;
			return(0);
		}
		else
		{
			(void) fflush(stderr);
			(void) close(pipes[0]);
			mftab[*unit].fp = fdopen(pipes[1], "w");
			/*setvbuf(mftab[*unit].fp, (char *) NULL, _IONBF, 0);*/
		}

	}
	else if (type == FILE_OUTPUT)
	{
#ifdef DEBUG
		(void) fprintf(stderr,"Open file output for <%s>\n",gks_output);
#endif
		if (!strcmp(gks_output, "gmeta")) {
			mftab[*unit].fp = fopen(gks_output, "w+");
			if (mftab[*unit].fp == (FILE *) NULL) {
				(void) fprintf(stderr, 
				"Error in opngks_(): Could not open <%s>\n", 
				gks_output);
				*status = 304;
				return(0);
			}
		}
		else {
			mftab[*unit].fp = fopen(gks_output, "a+");
			if (mftab[*unit].fp == (FILE *) NULL) {
				(void) fprintf(stderr, 
				"Error in opngks_(): Could not open <%s>\n", 
				gks_output);
				*status = 304;
				return(0);
			}
		}

		/*
		The environment variable NCARG_GKS_BUFSIZE, a macro
		definition, or a constant from stdio.h can determine
		buffer size used. See notes at the top of this file.
		*/

		bufsize_env = getenv("NCARG_GKS_BUFSIZE");
		if (bufsize_env == (char *) NULL) {
			if (DEFAULT_GKS_BUFSIZE == 0) {
				bufsize = BUFSIZ;
			}
			else {
				bufsize = 1024 * DEFAULT_GKS_BUFSIZE;
			}
		}
		else {
			bufsize = 1024 * atoi(bufsize_env);
			if (bufsize <= 0) {
			fprintf(stderr,
			"opnwks(): User-supplied buffer size too small\n");
			fprintf(stderr,
			"opnwks(): Using system default (%d)\n", BUFSIZ);
			}
		}
#ifdef DEBUG
		(void)fprintf(stderr,"Using stream bufsize    = %d\n", bufsize);
		(void)fprintf(stderr,"Streams default bufsize = %d\n", BUFSIZ);
#endif DEBUG

		mftab[*unit].buf = (char *) malloc( (unsigned) bufsize );
		if ( mftab[*unit].buf == (char *) NULL) {
			(void) fclose(mftab[*unit].fp);
			(void) fprintf(stderr,
			"Error in opngks_(): Memory allocation\n");
			*status = 304;
			return(0);
		}

		stat = setvbuf(mftab[*unit].fp, mftab[*unit].buf,
			       _IOFBF, bufsize);
		
		if (stat < 0) {
			(void) free(mftab[*unit].buf);
			(void) fclose(mftab[*unit].fp);
			(void) fprintf(stderr, 
				       "Error in opngks_(): setvbuf failed\n");
			*status = 304;
			return(0);
		}
			
	}

#ifdef DEBUG
	(void) fprintf(stderr, 
		"Opened %s as LU %d of type %d on fp %x and fd %d\n", 
		gks_output, *unit, type, mftab[*unit].fp, 
		fileno(mftab[*unit].fp));
#endif

	mftab[*unit].type = type;
	return(0);
}


/*************************************************************************
*
*	Function:	clswks_(unit, status)
*
*			The fake Fortran LU, "unit", is closed.
*			The file or pipe associated with it is
*			closed and any memory buffers associated
*			with it are freed. "status" is loaded
*			with a return code.
*
*	Called From:	Mostly Fortran routines in the NCAR GKS library.
*
*	Returns:	"status", which is encoded as per NCAR Graphics
*			GKS manual. Does not return a meaningful
*			function value.
*
*************************************************************************/

clswks_(unit, status)
	int	*unit;
	int	*status;
{
	int	child_status;
#ifdef DEBUG
	(void) fprintf(stderr, "clswks_(%d)\n", *unit);
#endif

	if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
	{
		(void) fprintf(stderr, "Invalid close on unit %d\n", *unit);
		*status = 304;
		return(0);
	}
	else
	{
		/*
		Close the file and adjust the table. The buffers are 
		not explicitly freed because this is apparently accomplished 
		in the file close.  In any event, a Cray will go flakey
		if you attempt such an act.
		*/

		(void) fflush(mftab[*unit].fp);
		(void) fclose(mftab[*unit].fp);

		/*
		Wait on any child translators out there. This is important 
		because, if the parent terminates while the translator is 
		still drawing to a graphics device, the shell prompt could
		be issued and then mixed with the graphics stream (on a tty). 
		The result is unpredictable, but generally unwanted.
		*/

		if ( mftab[*unit].type == PIPE_OUTPUT ) {
			(void) wait(&child_status);
		}

		mftab[*unit].fp   = MF_CLOSED;
		mftab[*unit].type = NO_OUTPUT;
		mftab[*unit].buf  = (char *) NULL;
	}
	return(0);
}

/*************************************************************************
*
*	Function:	wrtwks_(unit, buffer, length, status)
*
*			The integer buffer, "buffer", of "length"
*			*words* is written to the file or process 
*			associated with	the fake Fortran LU "unit".
*
*	Called From:	Mostly Fortran routines in the NCAR GKS library.
*
*	Returns:	"status", which is encoded as per NCAR Graphics
*			GKS manual. The function does not return a
*			meaningful value.
*
*************************************************************************/

wrtwks_(unit, buffer, length, status)
	int	*unit;
	int	*buffer;
	int	*length;
	int	*status;
{
	int		nb;
#if defined(ByteSwapped)
	static char	locbuf[RECORDSIZE];
#endif
#ifdef DEBUG
	(void) fprintf(stderr, "wrtwks_(%d,%d)\n",*unit,*length);
#endif

	if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
	{
		(void) fprintf(stderr, "Invalid unit (%d)\n", *unit);
		*status = 304;
		return(0);
	}

	if ( (*length * sizeof(long)) != RECORDSIZE)
	{
		(void) fprintf(stderr, 
			"Error in wrtwks_() : Invalid length (%d)\n",
			(*length * 4));
		*status = 304;
		return(0);
	}

#if defined(ByteSwapped)
	bcopyswap(buffer, locbuf, RECORDSIZE);
	nb = fwrite(locbuf, 1, RECORDSIZE, mftab[*unit].fp);
#else
	nb = fwrite((char *) buffer, 1, RECORDSIZE, mftab[*unit].fp);
#endif
	
	if (nb != RECORDSIZE)
	{
		(void) fprintf(stderr, 
			"Error in wrtwks_() : Writing metafile\n");
		*status = 304;
		return(0);
	}
	return(0);
}


/*************************************************************************
*
*	Function:	rdwks_(unit, buffer, length, status)
*
*			"length" *words* are read into "buffer"
*			from the file or pipe associated with
*			"unit". "status" is used to return any
*			error codes.
*
*	Called From:	Mostly Fortran routines in the NCAR GKS library.
*
*	Returns:	"status", which is encoded as per NCAR Graphics
*			GKS manual. The function does not return a
*			meaningful value.
*
*************************************************************************/
rdwks_(unit, buffer, length, status)
	int	*unit;
	int	*buffer;
	int	*length;
	int	*status;
{
	int		nb;
#if defined(ByteSwapped)
	static char	locbuf[RECORDSIZE];
#endif
#ifdef DEBUG
	(void) fprintf(stderr, "rdwks_(%d,%d)\n",*unit,*length);
#endif

	if (mftab[*unit].type != FILE_OUTPUT)
	{
		(void) fprintf(stderr, 
		"Error in rdwks_() : Cannot read from non-file output type\n");
		*status = 302;
		return(0);
	}

	if ( (*length * sizeof(long)) != RECORDSIZE)
	{
		(void) fprintf(stderr, 
			"Error in rdwks_() : Invalid length (%d)\n",
			(*length * 4));
		*status = 302;
		return(0);
	}

	if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
	{
		(void) fprintf(stderr, 
		"Error in rdwks_() : Invalid unit (%d)\n", *unit);
		*status = 302;
		return(0);
	}

#if defined(ByteSwapped)
	nb = fread( (char *) locbuf, 1, RECORDSIZE, mftab[*unit].fp );
	bcopyswap(locbuf, buffer, RECORDSIZE);
#else
	nb = fread( (char *) buffer, 1, RECORDSIZE, mftab[*unit].fp );
#endif
	
	if (nb == 0) {
		*status = -1;
	}
	else if (nb != RECORDSIZE) {
		(void) fprintf(stderr, 
			"Error in rdwks_() : Reading metafile\n");
		*status = 302;
		return(0);
	}
	return(0);
}

begwks_(unit, status)
	int	*unit;
	int	*status;
{
#ifdef DEBUG
	(void) fprintf(stderr, "begwks_(%d)\n",*unit);
#endif
	if (mftab[*unit].type != FILE_OUTPUT)
	{
		(void) fprintf(stderr, 
		"Error in begwks_() : Cannot seek on non-file output type\n");
		*status = 304;
		return(0);
	}

	if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
	{
		(void) fprintf(stderr, 
		"Error in begwks_() : Invalid unit (%d)\n", *unit);
		*status = 304;
		return(0);
	}

	if (fseek(mftab[*unit].fp, 0L, L_SET) == -1)
	{
		(void) fprintf(stderr, "Error in begwks_() : Seek failed\n");
		*status = 304;
		return(0);
	}
	return(0);
}

lstwks_(unit, status)
	int	*unit;
	int	*status;
{
#ifdef DEBUG
	(void) fprintf(stderr, "lstwks_(%d)\n",*unit);
#endif
	if (mftab[*unit].type != FILE_OUTPUT)
	{
		(void) fprintf(stderr, 
		"Error in lstwks_() : Cannot seek on non-file output type\n");
		*status = 304;
		return(0);
	}

	if (*unit >= MAX_UNITS || mftab[*unit].fp == MF_CLOSED)
	{
		(void) fprintf(stderr, 
		"Error in lstwks_() : Invalid unit (%d)\n", *unit);
		*status = 304;
		return(0);
	}

	if ( fseek(mftab[*unit].fp, -1440L, L_INCR) == -1)
	{
		(void) fprintf(stderr, "Error in lstwks_() : Seek failed\n");
		*status = 304;
		return(0);
	}
	return(0);
}
