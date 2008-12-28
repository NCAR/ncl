/*
 *	$Id: ictrans_wks.c,v 1.6 2008-12-28 13:31:53 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR GRAPHICS V3.00                         *
*                                                                      *
***********************************************************************/

/*
 *	ictrans_wks.c
 *
 *	Author		John Clyne
 *
 *	Date		Fri Jun  8 11:49:56 MDT 1990
 *
 *	This version of wks.c may be loaded in place of the default wks.c
 *	to facilitate translation of memory resident metafiles.
 */
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#ifdef	__FreeBSD__
#include <sys/filio.h>
#else
#include <sys/file.h>
#endif	/* __FreeBSD__ */
#include <unistd.h>
#ifdef cray
#include <fortran.h>
#endif

#include <ncarg/c.h>
#include <ncarg/cgm_tools.h>
#include "ictrans_wks.h"

/*LINTLIBRARY*/

/* The table which maintains information on open metafiles */
static struct
{
	int	fd;
	int	type;
} mftab[MAX_UNITS];

#ifdef STANDALONE
main()
{
	int	unit = 1;
	char	*string = "wooga";

	NGCALLF(opnwks,OPNWKS)(&unit);
	NGCALLF(wrtwks,WRTWKS)(&unit, (int *)string);
	NGCALLF(clswks,CLSWKS)(&unit);
}
#endif /* STANDALONE	*/

static	int	DoMem = 0;	/* if true open a memory file	*/

/*
 *	Procedure: initic_() (Fortran-callable)
 *
 *
 * initic_() provides a hook for creating a memory file that can later
 * be passed to a translater. If initic_() is called than the next and 
 * only next invocation of opnwks_ will produce a memory file
 *
 *
 * Warning: This procedure may be rewritten at install time
 * to reflect the C/FORTRAN calling characteristics of
 * the compilers in use.
 */
NGCALLF(initic,INITIC)()
{
	DoMem = 1;
}

/*
 *	Procedure: ictarg_() (Fortran-callable)
 *
 *
 * ictarg_() provide a means for passing an argument list to ictrans 
 * for translation of the memory resident metafile. status is set to -1
 * if an error occured.
 *
 * Warning: This procedure may be rewritten at install time
 * to reflect the C/FORTRAN calling characteristics of
 * the compilers in use.
 */
NGCALLF(ictarg,ICTARG)(unit, args, status)
	int	*unit;
	char	*args;
	int	*status;
{

	*status = ictrans_args(*unit, args);
}
	


#ifdef cray
NGCALLF(opnwks,OPNWKS)(unit, fname_, status)
	_fcd	fname_;
#else
NGCALLF(opnwks,OPNWKS)(unit, fname, status)
	char	*fname;
#endif
	int	*unit;
	int	*status;
{
	static int	init = 0;
	int		i, mfd, pipes[2], type;
	char		*gks_output, *gks_translator;
	char		*getenv(), *p;
#ifdef cray
	unsigned	length = _fcdlen(fname_);
	char		*fname;

	fname = (char *)malloc(sizeof(char)*length);
	strncpy( fname, _fcdtocp(fname_), length );
#endif

	/* Initialize the table that is used to track lu's */
#ifdef DEBUG
	(void) fprintf(stderr, "opnwks_(%d,%10s)\n",*unit,fname);
#endif

	if (!init)
	{
		for(i=0; i<MAX_UNITS; i++)
		{
			mftab[i].fd = MF_CLOSED;
			mftab[i].type = NO_OUTPUT;
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

	if (mftab[*unit].fd != MF_CLOSED)
	{
		(void) fprintf(stderr, 
		"Error in opnwks_(): Attempt to reopen unit %d\n", *unit);
		*status = 304;
		return(0);
	}

	/* Find out what output mechanism to use */

	if (DoMem) {	/* create a memory resident file	*/
		type = MEM_FILE_OUTPUT;
		DoMem = 0;
	} 
	else if (!strcmp(fname, "GMETA")) {
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


	/* if output is a translator, fork the proper one and link to it */
	/* if output is a file, just open it for writing */

	if ( type == PIPE_OUTPUT )
	{
		if ( strlen(gks_translator) == 0 )
		{
			gks_translator = DEFAULT_TRANSLATOR;
		}

		(void) pipe(pipes);

		if ( fork() == 0 )
		{
			(void) close(pipes[1]);
			(void) close(0); (void) dup(pipes[0]);
			(void) execlp(gks_translator, gks_translator, "-", (char *) 0);
			*status = 304;
			return(0);
		}
		else
		{
			(void) close(pipes[0]);
			mfd = pipes[1];
		}

	}
	else if (type == FILE_OUTPUT)
	{
#ifdef DEBUG
		(void) fprintf(stderr,"Open file output for <%s>\n",gks_output);
#endif
		if (!strcmp(gks_output, "gmeta"))
		{
		if((mfd=open(gks_output,O_RDWR | O_TRUNC | O_CREAT,0644)) == -1)
		{
			(void) fprintf(stderr, 
				"Error in opngks_(): Could not open <%s>\n", 
				gks_output);
			*status = 304;
			return(0);
		}
		}
		else
		{
		if((mfd=open(gks_output,O_RDWR | O_CREAT,0644)) == -1)
		{
			(void) fprintf(stderr, 
				"Error in opngks_(): Could not open <%s>\n", 
				gks_output);
			*status = 304;
			return(0);
		}
		}
	}
	else if (type == MEM_FILE_OUTPUT) {
		if((mfd = ictrans_open(*unit, RECORDSIZE)) == -1)
		{
			(void) fprintf(stderr, 
				"Error in opngks_(): Could not open <%s>\n", 
				gks_output);
			*status = 304;
			return(0);
		}
		

	}

#ifdef DEBUG
	(void) fprintf(stderr, "mftab[%d].fd = %d\n", *unit,mfd);
	(void) fprintf(stderr, "mftab[%d].type = %d\n", *unit,type);
#endif

	mftab[*unit].fd = mfd;
	mftab[*unit].type = type;
}

NGCALLF(clswks,CLSWKS)(unit, status)
	int	*unit;
	int	*status;
{
	int	child_status;
#ifdef DEBUG
	(void) fprintf(stderr, "clswks_(%d)\n", *unit);
#endif

	if (*unit >= MAX_UNITS || mftab[*unit].fd == MF_CLOSED)
	{
		(void) fprintf(stderr, "Invalid close on unit %d\n", *unit);
		*status = 304;
		return(0);
	}

	/*
	 * if file is memory resident than after closing the file invoke
	 * ictrans to translate it
 	 */
	if (mftab[*unit].type == MEM_FILE_OUTPUT) {
		(void) ictrans_close (*unit);

		if (ictrans_invoke(*unit) < 0) {
			(void) fprintf(stderr,
				"Translator failure on unit %d\n",*unit);
			*status = 304;
			return(0);
		}
		

		/*
		 * remove the memory file
		 */
		if (ictrans_free(*unit) < 0) {
			(void) fprintf(stderr,
				"Translator failure on unit %d\n",*unit);
			*status = 304;
			return(0);
		}
		
	} 
	else  {
		if ( mftab[*unit].type == PIPE_OUTPUT ) {
			(void) wait(&child_status);
		}
		(void) close(mftab[*unit].fd);
	}

	mftab[*unit].fd = MF_CLOSED;
	mftab[*unit].type = NO_OUTPUT;
}

NGCALLF(wrtwks,WRTWKS)(unit, buffer, length, status)
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

	if (*unit >= MAX_UNITS || mftab[*unit].fd == MF_CLOSED)
	{
		(void) fprintf(stderr, "Invalid unit (%d)\n", *unit);
		*status = 304;
		return(0);
	}

	if ( (*length * sizeof(int)) != RECORDSIZE)
	{
		(void) fprintf(stderr, 
			"Error in wrtwks_() : Invalid length (%d)\n",
			(*length * 4));
		*status = 304;
		return(0);
	}

#if defined(ByteSwapped)
	bcopyswap(buffer, locbuf, RECORDSIZE);
	if (mftab[*unit].type == MEM_FILE_OUTPUT) {
		nb = CGM_write(mftab[*unit].fd, (unsigned char *) locbuf);
	}
	else {
		nb = write(mftab[*unit].fd, (char *) locbuf, RECORDSIZE);
	}
#else
	if (mftab[*unit].type == MEM_FILE_OUTPUT) {
		nb = CGM_write(mftab[*unit].fd, (unsigned char *)buffer);
	}
	else {
		nb = write(mftab[*unit].fd, (char *) buffer, RECORDSIZE);
	}
#endif
	
	if (nb != RECORDSIZE)
	{
		(void)fprintf(stderr,"Error in wrtwks_() : Writing metafile\n");
		*status = 304;
		return(0);
	}
}

NGCALLF(rdwks,RDWKS)(unit, buffer, length, status)
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

	if (mftab[*unit].type == PIPE_OUTPUT)
	{
		(void) fprintf(stderr, 
		"Error in rdwks_() : Cannot read from non-file output type\n");
		*status = 302;
		return(0);
	}

	if ( (*length * sizeof(int)) != RECORDSIZE)
	{
		(void) fprintf(stderr, 
			"Error in rdwks_() : Invalid length (%d)\n",
			(*length * 4));
		*status = 302;
		return(0);
	}

	if (*unit >= MAX_UNITS || mftab[*unit].fd == MF_CLOSED)
	{
		(void) fprintf(stderr, 
		"Error in rdwks_() : Invalid unit (%d)\n", *unit);
		*status = 302;
		return(0);
	}

#if defined(ByteSwapped)
	if (mftab[*unit].type == MEM_FILE_OUTPUT) {
		nb = CGM_read(mftab[*unit].fd, (unsigned char *) locbuf);
	}
	else {
		nb = read(mftab[*unit].fd, (char *) locbuf, RECORDSIZE);
	}
	bcopyswap(locbuf, buffer, RECORDSIZE);
#else
	if (mftab[*unit].type == MEM_FILE_OUTPUT) {
		nb = CGM_read(mftab[*unit].fd, (unsigned char *) buffer);
	}
	else {
		nb = read(mftab[*unit].fd, (char *) buffer, RECORDSIZE);
	}
#endif
	
	if (nb == 0) {
		*status = -1;
	}
	else {
		(void)fprintf(stderr, "Error in rdwks_() : Reading metafile\n");
		*status = 302;
		return(0);
	}
}

NGCALLF(begwks,BEGWKS)(unit, status)
	int	*unit;
	int	*status;
{
#ifdef DEBUG
	(void) fprintf(stderr, "begwks_(%d)\n",*unit);
#endif
	if (!((mftab[*unit].type == FILE_OUTPUT) 
			|| (mftab[*unit].type == MEM_FILE_OUTPUT)))
	{
		(void) fprintf(stderr, 
		"Error in begwks_() : Cannot seek on non-file output type\n");
		*status = 304;
		return(0);
	}

	if (*unit >= MAX_UNITS || mftab[*unit].fd == 0)
	{
		(void) fprintf(stderr, 
		"Error in begwks_() : Invalid unit (%d)\n", *unit);
		*status = 304;
		return(0);
	}

	if (mftab[*unit].type == MEM_FILE_OUTPUT) {
		if (lseek(mftab[*unit].fd, 0L, SEEK_SET) == -1)
		{
			(void) fprintf(stderr, 
				"Error in begwks_() : Seek failed\n");
			*status = 304;
			return(0);
		}
	}
	else {
		if (CGM_lseek(mftab[*unit].fd, 0, SEEK_SET) == -1) {
			(void) fprintf(stderr, 
				"Error in begwks_() : Seek failed\n");
			*status = 304;
			return(0);
		}
	}
}

NGCALLF(lstwks,LSTWKS)(unit, status)
	int	*unit;
	int	*status;
{
#ifdef DEBUG
	(void) fprintf(stderr, "lstwks_(%d)\n",*unit);
#endif
	if (!((mftab[*unit].type == FILE_OUTPUT) 
			|| (mftab[*unit].type == MEM_FILE_OUTPUT)))
	{
		(void) fprintf(stderr, 
		"Error in lstwks_() : Cannot seek on non-file output type\n");
		*status = 304;
		return(0);
	}

	if (*unit >= MAX_UNITS || mftab[*unit].fd == 0)
	{
		(void) fprintf(stderr, 
		"Error in lstwks_() : Invalid unit (%d)\n", *unit);
		*status = 304;
		return(0);
	}

	if (mftab[*unit].type == MEM_FILE_OUTPUT) {
		if ( lseek(mftab[*unit].fd, -1440L, SEEK_CUR) == -1)
		{
			(void) fprintf(stderr, 
				"Error in lstwks_() : Seek failed\n");
			*status = 304;
			return(0);
		}
	} 
	else {
		if ( lseek(mftab[*unit].fd, -1L, SEEK_CUR) == -1) {
			(void) fprintf(stderr, 
				"Error in lstwks_() : Seek failed\n");
			*status = 304;
			return(0);
		}
	}
}

NGCALLF(flswks,FLSWKS)(unit, status)
	int	*unit;
	int	*status;
{
	int	rc;

	/* Make sure the requested unit is valid. */

	if (*unit >= MAX_UNITS || mftab[*unit].fd == MF_CLOSED)
	{
		(void) fprintf(stderr, "Invalid unit (%d)\n", *unit);
		*status = 304;
		return(0);
	}

	return(0);
}
