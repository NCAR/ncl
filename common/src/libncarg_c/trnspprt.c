/*
 *	$Id: trnspprt.c,v 1.5 2008-07-27 12:23:46 haley Exp $
 */
/***********************************************************************
*                                                                       *
*                          Copyright (C)  2000                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
*                                                                       *
*                          NCAR GRAPHICS V3.00                          *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*	
*	Package: trnspprt.c
*
*	Purpose: To provide low-level, possibly machine dependent
*		support for NCAR Graphics translators and related
*		software.
*
*	Warning: This package may be preprocessed at install time
*		to reflect FORTRAN-to-C calling conventions on a
*		specific system.
*
***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <ncarg/c.h>

#ifdef cray
#include <fortran.h>
#endif

#define FAIL	-1

/***********************************************************************
*	
*	Procedure: bclred_() (Fortran-callable)
*
*	Input Variables:
*		unit:	A UNIX file-descriptor, but passed
*			in from a calling FORTRAN procedure.
*
*	Output Variables:
*		ios:	The I/O status - valid only if "status"
*			is non-zero.
*
*		status:	Success status. 0 if good; non-zero if bad.
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/

void
NGCALLF(bclred,BCLRED)(unit, ios, status)
	int		*unit;
	int		*ios;
	int		*status;
{
	*status = 0;
	*ios = *status;
	(void) close(*unit);
}

/***********************************************************************
*	
*	Procedure: bincls_() (Fortran-callable)
*
*	Purpose: To close a file opened for sequential binary writing.
*		The file was opened by binopn_().
*
*	Input Variables:
*		unit:	A UNIX file-descriptor, but passed
*			in from a calling FORTRAN procedure.
*
*	Output Variables:
*		ios:	The I/O status - valid only if "status"
*			is non-zero.
*
*		status:	Success status. 0 if good; non-zero if bad.
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/
void
NGCALLF(bincls,BINCLS)(unit, ios, status)
	int	*unit, *ios, *status;
{
	*status = 0;
	*ios = *status;
	(void) close(*unit);
}

/***********************************************************************
*	
*	Procedure: binopn_() (Fortran-callable)
*
*	Purpose: To open a file for sequential writing.
*
*	Input Variables:
*		unit:	A UNIX file-descriptor, but passed
*			in from a calling FORTRAN procedure.
*
*		flname:	A FORTRAN CHARACTER*1 string terminated
*			by a blank character.
*
*	Output Variables:
*		ios:	The I/O status - valid only if "status"
*			is non-zero.
*
*		status:	Success status. 0 if good; non-zero if bad.
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/

void
#ifdef cray
NGCALLF(binopn,BINOPN)(unit, flname_, ios, status)
	_fcd	flname_;
#else
NGCALLF(binopn,BINOPN)(unit, flname, ios, status)
	char	flname[];
#endif
	int		*unit;
	int		*ios;
	int		*status;
{
	char		*envnm;
	int			i, st;
#ifdef cray
	unsigned	length = _fcdlen(flname_);
	char		*flname;

	flname = (char *)malloc(sizeof(char)*length);
	strncpy( flname, _fcdtocp(flname_), length );
#endif

	*status = 0;

	/* terminate the character string with a null  */

	i = 0;
	while (flname[i++] != ' ');
	flname[i - 1] = '\0';

	/* get the path name to the file given the environment name  */

	envnm = getenv(flname);
	if (envnm == 0)
		envnm = flname;

	st = creat(envnm, 511);

	/* if st is -1 then open error */

	if (st == -1)
		*status = 1;
	else
		/* successful set unit to the file descriptor  */
		*unit = st;
	
	*ios = *status;
}

/***********************************************************************
*	
*	Procedure:	binred_() (Fortran-callable)
*	Revision:	11/1/89
*	Purpose:	To read from an unformatted file,
*			opened by BOPRED, into a FORTRAN
*			INTEGER array.
*
*	Input Variables:
*
*	unit -	A UNIX file-descriptor, but passed
*		in from a calling FORTRAN procedure.
*	count - The number of integers to read. On most 16 and
*		32 bit systems, integers are 32 bits, and on
*		the Cray system, all integers are 64 bits, except
*		when compiling with Cray's Fortran 77 compiler
*		which uses 48 bit integers. Since this C function
*		is unlikely to be aware of what Fortran 77 is doing,
*		all code should be compiled for 64 bit arithmetic.
*
*
*	Output Variables:
*
*	buffer - The integer buffer to read the data into.
*
*	ios:	The I/O status - valid only if "status"
*		is non-zero.
*
*	status:	Success status. 0 if good; non-zero if bad.
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/
void
NGCALLF(binred,BINRED)(unit, count, buffer, ios, status)
	int		*unit, *count, buffer[], *ios, *status;
{
	int		nbytes, nbread;

	nbytes = *count * sizeof(int);

	nbread = read( *unit, (char *) buffer, nbytes);

	if (nbread != nbytes)
		*status = 1;
	else
		*status = 0;

	*ios = *status;
}

/***********************************************************************
*	
*	Procedure:	binwri_() (Fortran-callable)
*
*	Purpose:	To transfer the contents of a buffer to the named
*			file, using a sequential unformatted write. The
*			file was opened with binopn_().
*
*	Revision:	11/1/89
*
*	Input Variables:
*
*	unit -	A variable pretending to be a Fortran "logical unit",
*		but actually containing a UNIX file descriptor.
*
*	Output Variables:
*
*	count - The number of integers to write. On most 16 and
*		32 bit systems, integers are 32 bits, and on
*		the Cray system, all integers are 64 bits, except
*		when compiling with Cray's Fortran 77 compiler
*		which uses 48 bit integers. Since this C function
*		is unlikely to be aware of what Fortran 77 is doing,
*		all code should be compiled for 64 bit arithmetic.
*
*	buffer - The integer buffer from which to write the data.
*
*	ios -	The I/O status - valid only if "status"
*		is non-zero.
*
*	status - Success status. 0 if good; non-zero if bad.
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/
void
NGCALLF(binwri,BINWRI)(unit, count, buffer, ios, status)
	int		*unit, *count, buffer[], *ios, *status;
{
	int		nbytes, nbwritten;

	nbytes = *count * sizeof(int);

	*status = 0;

	/* write out the buffer */

	nbwritten = write(*unit, (char *) buffer, nbytes);

	if (nbwritten != nbytes)
		*status = 1;
	else
		*status = 0;
	
	*ios = *status;
}

/***********************************************************************
*	
*	Procedure:	chrcls_() (Fortran-callable)
*	Purpose:	To close a file opened for sequential character
*			reading. The file was opened by chropn_().
*
*	Input Variables:
*
*	unit -	A UNIX file-descriptor, but passed
*		in from a calling FORTRAN procedure.
*
*	Output Variables:
*
*	ios -	The I/O status - valid only if "status"
*		is non-zero.
*
*	status - Success status. 0 if good; non-zero if bad.
*
*	Revision History:
*	11/1/89 Don Middleton-Link Cleaning ,comments, and lint.
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/
void
NGCALLF(chrcls,CHRCLS)(unit, ios, status)
	int	*unit, *ios, *status;
{
	*status = 0;
	*ios = *status;
	(void) close(*unit);
}

/***********************************************************************
*	
*	Procedure:	chropn_() (Fortran-callable)
*
*	Purpose:	To open a file for sequential unformatted
*			reads. The file contains characters with a
*			maximum of 80 characters per record.
*
*	Input Variables:
*
*	unit -	A UNIX file-descriptor, but passed
*		in from a calling FORTRAN procedure.
*
*	flname - A CHARACTER*1 array containing the file name.
*
*	Output Variables:
*
*	ios -	The I/O status - valid only if "status"
*		is non-zero.
*
*	status - Success status. 0 if good; non-zero if bad.
*
*	Revision History:
*
*	11/1/89; Don Middleton-Link; Comments and lint testing
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/
void
#ifdef cray
NGCALLF(chropn,CHROPN)(unit, flname_, ios, status)
#else
NGCALLF(chropn,CHROPN)(unit, flname, ios, status)
#endif
/*
 * this routine opens a file for sequential unformatted reads the file is
 * character oriented and a max of 80 per line
 * 
 * input flname-fortran character*1 type string containing the file name
 * terminated by a blank output unit-the file descriptor to use for reading
 * ios-the i/o status valid only if status non zero status-the status flag
 * zero all ok non zero error opening the file
 */
	int		*unit, *ios, *status;
#ifdef cray
	_fcd	flname_;
#else
	char	flname[];
#endif
{
	char	*envnm, *getenv();
	int		i, st;
#ifdef cray
	unsigned	length = _fcdlen(flname_);
	char		*flname;

	flname = (char *)malloc(sizeof(char)*length);
	strncpy( flname, _fcdtocp(flname_), length );
#endif

	*status = 0;

	/* terminate the character string with a null  */

	i = 0;
	while (flname[i++] != ' ');
	flname[i - 1] = '\0';

	/* get the path name to the file given the environment name  */

	envnm = getenv(flname);
	if (envnm == 0)
		envnm = flname;

	/* attempt to open the file */

	st = open(envnm, 0);

	/* if st is -1 then open error */

	if (st == -1)
		*status = 1;
	else
		/* successful set unit to the file descriptor  */
		*unit = st;

	*ios = *status;
}

/***********************************************************************
*	
*	Procedure:	chrred_() (Fortran-callable)
*
*	Purpose:	To transfer characters from a file
*			opened by CHROPN into a buffer.
*
*	Input Variables:
*
*	unit -	A UNIX file-descriptor, but passed
*		in from a calling FORTRAN procedure.
*
*	count - The number of characters to transfer.
*
*	Output Variables:
*
*	buffer - A CHARACTER*1 array containing the characters.
*
*	iptr - The index of the first valid character in
*		"buffer" and should always be 1 in a FORTRAN
*		implementation.
*
*	ios -	The I/O status - valid only if "status"
*		is non-zero.
*
*	status - Success status. 0 if good; non-zero if bad.
*
*	Revision History:
*
*	11/1/89; Don Middleton-Link; Comments and lint testing
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/
#ifdef cray
int NGCALLF(chrred,CHRRED)(unit, count, buffer_, iptr, ios, status)
	_fcd	buffer_;
#else
int	NGCALLF(chrred,CHRRED)(unit, count, buffer, iptr, ios, status)
	char	buffer[];
#endif
	int		*unit, *count, *iptr, *ios, *status;
{
	static char		bread[512];
	static char		*bp = &bread[512];
	static int		ret = 512;
	int				cnt;
	char			*rp;
	int				tot;
#ifdef cray
	unsigned		length = _fcdlen(buffer_);
	char			*buffer;
	
	buffer = (char *)malloc(sizeof(char)*length);
	strncpy( buffer, _fcdtocp(buffer_), length );
#endif

	/* set status to ok */

	*status = 0;
	*ios = 0;

	/* init temp varibles */

	cnt = *count;
	rp = buffer;
	tot = 0;

	/* clear the input buffer  */
	do {
		*rp++ = ' ';
	} while (--cnt);

	rp = buffer;
	cnt = *count;

	do {
		/* read in a buffer */
		if (bp >= &bread[ret]) {
			if ((ret = read(*unit, bread, 512)) <= 0) {
				*status = 1;	/* return on error or EOF */
				return(-1);
			}
			bp = bread;
		}
		if (*bp == '\012' || *bp == '\015') {
			++bp;	/* bump past record terminator */
			break;	/* end of record, get out of this loop */
		}
		*rp++ = *bp++;	/* copy file char to user buffer */
		++tot;
	} while (--cnt);
	*ios = tot;		/* set character count read */

	*iptr = 1;		/* set the buffer pointer to the start */

#ifdef cray
	strncpy ( _fcdtocp(buffer_), buffer, length );
#endif
	return(0);
}

/***********************************************************************
*	
*	Procedure:	frprmp_() (Fortran-callable)
*
*	Purpose:	To send a "frame-finished" prompt and
*			wait for a response.
*
*	Input Variables:
*
*	buff -	A CHARACTER*1 string containing the prompt.
*
*	count - The number of characters in "buff".
*
*	Output Variables:
*
*	ios -	The I/O status - valid only if "status"
*		is non-zero.
*
*	status - Success status. 0 if good; non-zero if bad.
*
*	Revision History:
*
*	11/1/89; Don Middleton-Link; Comments and lint testing
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/
void
NGCALLF(frprmp,FRPRMP)(buff, count, ios, status)
	int		*buff;
	int		*count, *ios, *status;
{
	int		st, ii, dev;
	char	junk[4], prmp[51];

	*status = 0;

	/* move the prompt to the character array  */

	for (ii = 0; ii < *count; ii++)
		prmp[ii] = (char) buff[ii];
	prmp[*count] = '\000';

	/* send the prompt to the terminal */
	dev = open("/dev/tty", 2);
	st = write(dev, prmp, *count);
	st = read(dev, junk, 1);
	(void) close(dev);

	/* if st is negative then error so flag it  */

	if (st < 0) *status = 1;

	*ios = *status;
}

/***********************************************************************
*	
*	Procedure:	readit_() (Fortran-callable)
*
*	Purpose:	To read at most "max" characters into
*			"buf".
*
*	Input Variables:
*
*	max -	The maximum number of characters to read.
*
*	Output Variables:
*
*	buf -	The INTEGER buffer that the characters are
*		read into.
*
*	Revision History:
*
*	11/1/89; Don Middleton-Link; Comments and lint testing
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/
void
#ifdef cray
NGCALLF(readit,READIT)(buf_, max)
	_fcd	buf_;
#else
NGCALLF(readit,READIT)(buf, max)
	char	buf[];
#endif
	int		*max;
{
#ifdef cray
	unsigned	length = _fcdlen(buf_);
	char		*buf;

	buf = (char *)malloc(sizeof(char)*length);

	(void) read(0, buf, *max);

	strncpy( _fcdtocp(buf_), buf, length ); 
#else
	(void) read(0, buf, *max);
#endif
}

/***********************************************************************
*	
*	Procedure:	writit_() (Fortran-callable)
*
*	Purpose:	To write a a character string to stdout.
*
*	Input Variables:
*
*	count - The number of characters to write.
*
*	string - Contains the CHARACTER*1 string to write.
*
*	Revision History:
*
*	11/1/89; Don Middleton-Link; Comments and lint testing
*
*	Warning: This procedure may be rewritten at install time
*	to reflect the C/FORTRAN calling characteristics of
*	the compilers in use.
*
***********************************************************************/
void
#ifdef cray
NGCALLF(writit,WRITIT)(msg_, count)
	_fcd	msg_;
#else
NGCALLF(writit,WRITIT)(msg, count)
	char	msg[];
#endif
	int		*count;
{
	char	buffer[81];
	int		i;

#ifdef cray
	unsigned	length = _fcdlen(msg_);
	char		*msg;

	msg = (char *)malloc(sizeof(char)*length);
	strncpy(msg,_fcdtocp(msg_),length);
#endif

	/* transfer msg to output buffer */

	for (i = 0; i < *count; i++)
		buffer[i] = msg[i];

	/* null terminate */

	buffer[*count] = '\0';

	/* send msg to standard output */

	(void) printf("%s ", buffer);
	(void) fflush(stdout);
}
