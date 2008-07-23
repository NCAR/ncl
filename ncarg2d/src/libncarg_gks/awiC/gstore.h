/*
 *      $Id: gstore.h,v 1.4 2008-07-23 17:30:26 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		gstore.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 19 12:18:28 MDT 1994
 *
 *	Description:	
 */
#ifndef	_GSTORE_H_
#define	_GSTORE_H_

#include <ncarg/gks.h>

#define	GSTALLOCSIZE	(10)

struct _GstoreRec_ {
	int	num;
	void	*ptrs[GSTALLOCSIZE];
	Gstore	next;
};

/*
 * These are internal private functions to be used by gescape and any other
 * gks functions we may support in the future that take GStore.
 */
extern void *_gstore_alloc(
#ifdef	NeedFuncProto
	Gstore		stor,
	unsigned int	size
#endif
);

extern int _gstore_clear(
#ifdef	NeedFuncProto
	Gstore	stor
#endif
);

#endif	/* _GSTORE_H_	*/
