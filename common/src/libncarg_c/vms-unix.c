/***********************************************************************
*                                                                      *
*                          Copyright (C)  2000                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - VMS Release                   *
*                                                                      *
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
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/


#ifdef	VMS

/*      Author: Chris J. Phillips NCAR-MMM                             *
*       Purpose: To provide functions utilized by CTRANS that are      *
*               not supported directly by VAX C but are by most        *
*               UNIX implementations.  This module also contains       *
*               functions that are supported by VAX but do not work    *
*               in quite the same way.  This module also conatains     *
*               the code for the functions that are needed by the      *
*               new version.                                          */

#include <stdio.h>
#include <iodef.h> 
#include <string.h>
#include <file.h>

/* The following are mapping procedures which map a UNIX function with the
corresponding VMS function. */

void *bcopy (s1, s2, size)

void *s1, *s2;
size_t size;

{
        memcpy (s2, s1, size);
}

void *bzero (s, size)

void *s;
size_t size;

{
        memset (s, 0, 30);
}

int unlink (file_spec)

char *file_spec;

{
        delete (file_spec);
}

#undef	VMS
