/*
 *      $Id: addfileP.h,v 1.1 1996-11-24 22:27:34 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		addfileP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct 14 16:39:29 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_ADDFILEP_H_
#define	_NG_ADDFILEP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/addfile.h>

typedef struct _NgAddFileClassRec *NgAddFileClass;
typedef struct _NgAddFileRec *NgAddFile;

typedef struct _NgAddFilePart {
/* required fields */
	int	foo;

/* private fields */
	Widget	vname;
	Widget	fname;
	Widget	optmenu;

} NgAddFilePart;

typedef struct _NgAddFileRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgAddFilePart	addfile;
} NgAddFileRec;

typedef struct _NgAddFileClassPart {
	int		foo;
} NgAddFileClassPart;

typedef struct _NgAddFileClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgAddFileClassPart	addfile_class;
} NgAddFileClassRec;

extern NgAddFileClassRec	NgaddFileClassRec;

#endif	/* _NG_ADDFILEP_H_ */
