/*
 *      $Id: ngo.h,v 1.1 1996-10-10 18:55:27 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ngo.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 18 10:47:23 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_NGO_H_
#define	_NG_NGO_H_

#include <ncarg/hlu/hlu.h>

/*
 * Global resources
 */
#define	NgNAppMgr	"ngAppMgr"
#define	NgCAppMgr	"NgAppMgr"
#define	NgNNclState	"ngNclState"
#define	NgCNclState	"NgNclState"

#define	_NgStackAlloc(size,stack_array)	\
		(((size) > sizeof(stack_array))	\
		? NhlMalloc((unsigned)(size))	\
		: stack_array)

#define _NgStackFree(ptr,stack_array)	\
		if((ptr) != ((NhlPointer)(stack_array))) NhlFree(ptr); else

extern void NgDestroyMeCB(
	NhlArgVal	cbdata,
	NhlArgVal	udata
);

#endif	/* _NG_NGO_H_ */
