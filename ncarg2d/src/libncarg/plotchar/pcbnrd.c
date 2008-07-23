/*
 * $Id: pcbnrd.c,v 1.5 2008-07-23 16:16:58 haley Exp $
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


#include <stdio.h>
#include <ncarg/c.h>

/***********************************************************************
*
*       Procedure:      pcbnrd_() (Fortran-callable)
*       Revision:       11/1/89
*       Purpose:        To read from an unformatted file,
*                       opened by BOPRED, into a FORTRAN
*                       INTEGER array.
*
*       Input Variables:
*
*       unit -  A UNIX file-descriptor, but passed
*               in from a calling FORTRAN procedure.
*       count - The number of integers to read. On most 16 and
*               32 bit systems, integers are 32 bits, and on
*               the Cray system, all integers are 64 bits, except
*               when compiling with Cray's Fortran 77 compiler
*               which uses 48 bit integers. Since this C function
*               is unlikely to be aware of what Fortran 77 is doing,
*               all code should be compiled for 64 bit arithmetic.
*
*
*       Output Variables:
*
*       buffer - The integer buffer to read the data into.
*
*       ios:    The I/O status - valid only if "status"
*               is non-zero.
*
*       status: Success status. 0 if good; non-zero if bad.
*
*       Warning: This procedure may be rewritten at install time
*       to reflect the C/FORTRAN calling characteristics of
*       the compilers in use.
*
***********************************************************************/
void
NGCALLF(pcbnrd,PCBNRD)(unit, count, buffer, ios, status)
	int		*unit, *count, buffer[], *ios, *status;
{
	int		nbytes, nbread;

	nbytes = *count * sizeof(int);

	nbread = read( *unit, (char *) buffer, nbytes);

	*status = 0;

	*ios = *status;
}
