/*
 *      $Id: StyleP.h,v 1.2 1997-07-25 21:12:36 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		StyleP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 20 18:10:29 MST 1996
 *
 *	Description:	Private header file for Style class
 */
#ifndef _NSTYLEP_h
#define _NSTYLEP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/Style.h>

/* private resource */

typedef struct _NhlStyleLayerPart{

	/* public resource fields */

	NhlBoolean	foo;

	/* private resources */

}NhlStyleLayerPart;

typedef struct _NhlStyleLayerRec{
	NhlBaseLayerPart	base;
	NhlStyleLayerPart	style;
}NhlStyleLayerRec;

typedef struct _NhlStyleClassPart {
	char *foo;
}NhlStyleClassPart;

typedef struct _NhlStyleClassRec{
	NhlBaseClassPart	base_class;
	NhlStyleClassPart	style_class;
}NhlStyleClassRec;

typedef struct _NhlStyleClassRec	*NhlStyleClass;
typedef struct _NhlStyleLayerRec	*NhlStyleLayer;

extern NhlStyleClassRec		NhlstyleClassRec;

#endif  /*_NSTYLEP_h*/
