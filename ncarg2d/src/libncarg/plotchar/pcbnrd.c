/*
 * $Id: pcbnrd.c,v 1.4 2000-08-22 15:05:21 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
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
