/*
 *	$Id: logic64.c,v 1.3 2000-08-22 04:03:32 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  2000                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR GRAPHICS V2.00                         *
*                                                                      *
************************************************************************
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

#include <ncarg/c.h>

/*
	Logical operations for 32-bit systems.
*/


long
NGCALLF(ishift,ISHIFT)(i, nshift)
/* shift the 4 byte integer i by nshift bits  */
/* if nshift is negative, right shift end off zero fill  */
/* if nshift is positive, left shift end around  */
/* the routine behaves properly if magnitude of nshift > 32  */
	long           *i, *nshift;
{
	long            jshift, nbits;
	if (*nshift < 0) {
		nbits = (*nshift < -64 ? 64 : -*nshift);
		jshift = (*i >> nbits) & (0x7fffffffffffffff>> (nbits - 1));
	} else {
		nbits = *nshift % 64;
		jshift = (*i << nbits) | ((*i >> (64 - nbits))
				  & (~(0xffffffffffffffff<< nbits)));
	}
	return (jshift);
}

/* integer valued function to return logical AND of i and j */
/* i and j are assumed to be 4 byte integers  */
long
NGCALLF(iand,IAND)(i, j)
	long	*i, *j;
{
	return (*i & *j);
}

/* integer valued function to return logical OR of i and j  */
/* i and j are assumed to be 4 byte integers  */
long
NGCALLF(ior,IOR)(i, j)
	long	*i, *j;
{
	return (*i | *j);
}
