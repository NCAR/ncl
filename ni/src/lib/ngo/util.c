/*
 *      $Id: util.c,v 1.1 1996-10-10 18:55:28 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		util.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 4 11:23:43 MDT 1996
 *
 *	Description:	
 */

/*
 * Function:	NgHashString
 *
 * Description:	
 *		This function is almost an exact copy of hash_pjw from
 *		Symbol.c in ncl except that the number of buckets is
 *		not hard coded.  nbuckets should be a prime number.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
unsigned int
NgHashString
(
	char		*str,
	unsigned int	nbuckets
)
{
	char		*p;
	unsigned	h=0,g;

	for(p=str;*p != '\0';p++){
		h = (h<<4) + (*p);
		if(g = h & 0xf0000000){
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}

	return h % nbuckets;
}
