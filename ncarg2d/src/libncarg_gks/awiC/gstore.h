/*
 *      $Id: gstore.h,v 1.2 2000-07-12 17:06:05 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
