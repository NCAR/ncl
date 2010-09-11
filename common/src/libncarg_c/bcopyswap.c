/*
 *	$Id: bcopyswap.c,v 1.4 2008-07-27 12:23:44 haley Exp $
 */
/************************************************************************
*                                                                       *
*			     Copyright (C)  2000	                        		*
*	     University Corporation for Atmospheric Research		        *
*			     All Rights Reserved			                        *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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
#include <stdlib.h>

void bcopyswap(b1, b2, size)
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
