/*
 *      $Id: GraphicStyleP.h,v 1.1 1996-02-26 21:45:54 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		GraphicStyleP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 20 18:10:29 MST 1996
 *
 *	Description:	Private header file for GraphicStyle class
 */
#ifndef _NGRAPHICSTYLEP_h
#define _NGRAPHICSTYLEP_h

#include <ncarg/hlu/StyleP.h>
#include <ncarg/hlu/GraphicStyle.h>
#include <ncarg/hlu/SubStylesP.h>

/* private resource */

typedef struct _NhlGraphicStyleLayerPart{

	NhlBoolean      clip_on;
	NhlGenArray     colors;
	NhlGenArray     segments;

	/* private fields */

	int		line_style_id;
	int		fill_style_id;
	int		edge_style_id;
	int		marker_style_id;
	int		text_style_id;

}NhlGraphicStyleLayerPart;

typedef struct _NhlGraphicStyleLayerRec{
	NhlBaseLayerPart		base;
	NhlStyleLayerPart		style;
	NhlGraphicStyleLayerPart	graphicstyle;
}NhlGraphicStyleLayerRec;

typedef struct _NhlGraphicStyleClassPart {
	char *foo;
}NhlGraphicStyleClassPart;

typedef struct _NhlGraphicStyleClassRec{
	NhlBaseClassPart		base_class;
	NhlStyleClassPart		style_class;
	NhlGraphicStyleClassPart	graphicstyle_class;
}NhlGraphicStyleClassRec;

typedef struct _NhlGraphicStyleClassRec	*NhlGraphicStyleClass;
typedef struct _NhlGraphicStyleLayerRec	*NhlGraphicStyleLayer;

extern NhlGraphicStyleClassRec		NhlgraphicStyleClassRec;

#endif  /*_NGRAPHICSTYLEP_h*/
