/*
 *      $Id: loadP.h,v 1.1 1996-10-16 16:21:20 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		loadP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct 14 16:39:29 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_LOADP_H_
#define	_NG_LOADP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/load.h>

typedef struct _NgLoadClassRec *NgLoadClass;
typedef struct _NgLoadRec *NgLoad;

typedef struct _NgLoadPart {
/* required fields */
	int		foo;

/* private fields */

} NgLoadPart;

typedef struct _NgLoadRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgLoadPart	load;
} NgLoadRec;

typedef struct _NgLoadClassPart {
	int		foo;
} NgLoadClassPart;

typedef struct _NgLoadClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgLoadClassPart		load_class;
} NgLoadClassRec;

extern NgLoadClassRec	NgloadClassRec;

#endif	/* _NG_LOADP_H_ */
