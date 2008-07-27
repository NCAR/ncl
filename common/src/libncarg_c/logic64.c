/*
 *	$Id: logic64.c,v 1.4 2008-07-27 12:23:45 haley Exp $
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
