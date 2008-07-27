/*
 *	$Id: logic32.c,v 1.4 2008-07-27 12:23:45 haley Exp $
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
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/c.h>

/*
	Logical operations for 32-bit systems.
*/


int
NGCALLF(ishift,ISHIFT)(i, nshift)
/* shift the 4 byte integer i by nshift bits  */
/* if nshift is negative, right shift end off zero fill  */
/* if nshift is positive, left shift end around  */
/* the routine behaves properly if magnitude of nshift > 32  */
	int		*i, *nshift;
{
	int		jshift, nbits;
	if (*nshift < 0) {
		nbits = (*nshift < -32 ? 32 : -*nshift);
		jshift = (*i >> nbits) & (017777777777 >> (nbits - 1));
	} else {
		nbits = *nshift % 32;
		jshift = (*i << nbits) | ((*i >> (32 - nbits))
					  & (~(037777777777 << nbits)));
	}
	return (jshift);
}

/* integer valued function to return logical AND of i and j */
/* i and j are assumed to be 4 byte integers  */
int
NGCALLF(iand,IAND)(i, j)
	int		*i, *j;
{
	return (*i & *j);
}

/* integer valued function to return logical OR of i and j  */
/* i and j are assumed to be 4 byte integers  */
int
NGCALLF(ior,IOR)(i, j)
	int		*i, *j;
{
	return (*i | *j);
}
