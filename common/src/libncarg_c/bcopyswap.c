/*
 *	$Id: bcopyswap.c,v 1.3 2000-08-22 04:03:31 haley Exp $
 */
/************************************************************************
*                                                                       *
*			     Copyright (C)  2000	                        		*
*	     University Corporation for Atmospheric Research		        *
*			     All Rights Reserved			                        *
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
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 * Name:	bcopyswap
 *
 * Purpose:	Copies buffer "b1" to "b2", swapping bytes on
 *		32-bit boundaries in the process. "size" specifies
 *		the size of the buffer, and must be a multiple
 *		of 4.
 */
#include <stdio.h>

bcopyswap(b1, b2, size)
	char	*b1;
	char	*b2;
	int	size;
{
	int	loc;

	if ( (size%4) != 0)
	{
		(void) fprintf(stderr, 
			"Error in bcopyswap(): size not mult of 4\n");
		exit(1);
	}

	for (loc = 0; loc < size; loc+=4)
	{
		b2[loc + 0] = b1[loc + 3];
		b2[loc + 1] = b1[loc + 2];
		b2[loc + 2] = b1[loc + 1];
		b2[loc + 3] = b1[loc + 0];
	}
}
