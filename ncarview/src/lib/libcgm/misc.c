/*
 *	$Id: misc.c,v 1.11 2000-08-22 15:11:35 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <ncarg/c.h>
#ifdef	cray
#include <sys/unistd.h>
#endif
#include "cgm_tools.h"
#include "internals.h"

/*
 *	CGM_validCGM
 *	PUBLIC
 *
 *	Determine if a file is a valid CGM or not. CGM_validCGM() performs
 *	a few simple tests in an effort to determine whether a file is a 
 *	valid binary encoded NCAR CGM
 *
 * on exit
 *	return		: 1 => file is a ncar CGM
 *			: 0 => file is not a ncar CGM
 *			:-1 => an error occured (possibly as a result of
 *				the file not existing) errno is set.
 */
CGM_validCGM(ncar_cgm)
	char	*ncar_cgm;
{
	Cgm_fd	fd;
	unsigned char	*buf = NULL;

	unsigned long	data_len;

	Cgm_fd	CGM_open();

#ifdef	cray
	if (access(ncar_cgm, R_OK) == -1) return(-1);
#endif

	/*
	 * try and open the file. 
	 */
	if  ((fd = CGM_open (ncar_cgm, BUFSIZE, "r")) < 0) {
		return (-1);
	} 

	if (! (buf = (unsigned char *) malloc (BUFSIZE))) {
		return(-1);
	}

	/*
	 * read in first record of the file
	 */
	if (CGM_read (fd, buf) < 0) {
		if (buf) (void) free ((Voidptr) buf);
		(void) CGM_close(fd);
		return (-1);
	}

	/* 
	 * see if data len count is valid. Currently only 1436 bytes of valid
	 * data can be stored in a record
	 */
	data_len = buf[0] << 8 | buf[1];
	if (data_len > BUFSIZE - HEADERSIZE ) {
		if (buf) (void) free ((Voidptr) buf);
		(void) CGM_close(fd);
		return (0);
	}

	/*
	 * make sure valid NCAR CGM. Only record type supported now
	 */
	if (GETBITS(buf[2],TYPE_POS,TYPE_LEN) != NCAR_CGM) {
		if (buf) (void) free ((Voidptr) buf);
		(void) CGM_close(fd);
		return(0);
	}

	/*
	 * file is a NCAR binary encoded CGM
	 */
	if (buf) (void) free ((Voidptr) buf);
	(void) CGM_close(fd);
	return (1);
}
