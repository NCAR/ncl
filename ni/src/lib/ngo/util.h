/*
 *      $Id: util.h,v 1.1 1996-10-10 18:55:29 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		util.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 4 11:34:46 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_UTIL_H
#define	_NG_UTIL_H

extern unsigned int NgHashString(
	char		*str,
	unsigned int	nbuckets
);

#endif	/* _NG_UTIL_H */
