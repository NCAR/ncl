/*
 *      $Id: SubStylesP.h,v 1.3.12.1 2010-03-17 20:47:07 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		SubStylesP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 20 18:10:29 MST 1996
 *
 *	Description:	This private header file contains the declartions
 *                      for each of the individual subclasses of Style
 */
#ifndef _NSUBSTYLESP_h
#define _NSUBSTYLESP_h

#include <ncarg/hlu/StyleP.h>
#include <ncarg/hlu/GraphicStyle.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/Workstation.h>

/* LineStyle class */


typedef struct __NhlLineStyleInfo{
        NhlDashIndex	dash_pattern;
        float		line_dash_seglen;
        NhlColorIndex	line_color;
        float		line_opacity;
        float		line_thickness;
        NhlString	line_label_string;
	NhlFont		line_label_font;
	NhlColorIndex	line_label_font_color;
        float		line_label_font_height;
	float		line_label_font_aspect;
	float		line_label_font_thickness;
	NhlFontQuality	line_label_font_quality;
	float		line_label_const_spacing;
	char		line_label_func_code;
} _NhlLineStyleInfo;

#define _NhlNgsLineStyleInfo	".gsLineStyleInfo"
#define _NhlCgsLineStyleInfo	".GsLineStyleInfo"

typedef struct _NhlLineStyleLayerPart{

	_NhlLineStyleInfo	*lsp;
	_NhlLineStyleInfo	ls;
	
}NhlLineStyleLayerPart;

typedef struct _NhlLineStyleLayerRec{
	NhlBaseLayerPart		base;
	NhlStyleLayerPart		style;
	NhlLineStyleLayerPart		linestyle;
}NhlLineStyleLayerRec;

typedef struct _NhlLineStyleClassPart {
	char *foo;
}NhlLineStyleClassPart;

typedef struct _NhlLineStyleClassRec{
	NhlBaseClassPart		base_class;
	NhlStyleClassPart		style_class;
	NhlLineStyleClassPart		linestyle_class;
}NhlLineStyleClassRec;

typedef struct _NhlLineStyleClassRec	*NhlLineStyleClass;
typedef struct _NhlLineStyleLayerRec	*NhlLineStyleLayer;

extern NhlLineStyleClassRec		NhllineStyleClassRec;

/* FillStyle class */


typedef struct __NhlFillStyleInfo{
	NhlFillIndex	fill_index;
	NhlColorIndex	fill_color;
	float           fill_opacity;
	NhlColorIndex   fill_background;
	float		fill_scale_factor;
	float		fill_line_thickness;
	float		fill_dot_size;
	NhlBoolean	edges_on;
	NhlDashIndex	edge_dash_pattern;
	float		edge_thickness;
	float		edge_dash_seglen;
	NhlColorIndex	edge_color;
} _NhlFillStyleInfo;

#define _NhlNgsFillStyleInfo	".gsFillStyleInfo"
#define _NhlCgsFillStyleInfo	".GsFillStyleInfo"

typedef struct _NhlFillStyleLayerPart{

	_NhlFillStyleInfo	*fsp;
	_NhlFillStyleInfo fs;

}NhlFillStyleLayerPart;

typedef struct _NhlFillStyleLayerRec{
	NhlBaseLayerPart		base;
	NhlStyleLayerPart		style;
	NhlFillStyleLayerPart		fillstyle;
}NhlFillStyleLayerRec;

typedef struct _NhlFillStyleClassPart {
	char *foo;
}NhlFillStyleClassPart;

typedef struct _NhlFillStyleClassRec{
	NhlBaseClassPart		base_class;
	NhlStyleClassPart		style_class;
	NhlFillStyleClassPart		fillstyle_class;
}NhlFillStyleClassRec;

typedef struct _NhlFillStyleClassRec	*NhlFillStyleClass;
typedef struct _NhlFillStyleLayerRec	*NhlFillStyleLayer;

extern NhlFillStyleClassRec		NhlfillStyleClassRec;


/* MarkerStyle class */

typedef struct __NhlMarkerStyleInfo{
	NhlMarkerIndex	marker_index;
	NhlColorIndex	marker_color;
	float			marker_opacity;
	float		marker_size;
	float		marker_thickness;
} _NhlMarkerStyleInfo;

#define _NhlNgsMarkerStyleInfo	".gsMarkerStyleInfo"
#define _NhlCgsMarkerStyleInfo	".GsMarkerStyleInfo"

typedef struct _NhlMarkerStyleLayerPart{

	_NhlMarkerStyleInfo	*msp;
	_NhlMarkerStyleInfo	ms;

}NhlMarkerStyleLayerPart;

typedef struct _NhlMarkerStyleLayerRec{
	NhlBaseLayerPart		base;
	NhlStyleLayerPart		style;
	NhlMarkerStyleLayerPart		markerstyle;
}NhlMarkerStyleLayerRec;

typedef struct _NhlMarkerStyleClassPart {
	char *foo;
}NhlMarkerStyleClassPart;

typedef struct _NhlMarkerStyleClassRec{
	NhlBaseClassPart		base_class;
	NhlStyleClassPart		style_class;
	NhlMarkerStyleClassPart		markerstyle_class;
}NhlMarkerStyleClassRec;

typedef struct _NhlMarkerStyleClassRec	*NhlMarkerStyleClass;
typedef struct _NhlMarkerStyleLayerRec	*NhlMarkerStyleLayer;

extern NhlMarkerStyleClassRec		NhlmarkerStyleClassRec;


/* TextStyle class */

typedef struct __NhlTextStyleInfo{
	float			angle;
	NhlJustification	just;
	NhlTextDirection	direction;
	NhlFont			font;
	int			font_color;
	float			font_opacity;
	float			font_height;
	float			font_aspect;
	float 			font_thickness;
	NhlFontQuality		font_quality;
	float			constant_spacing;
	char			func_code;
} _NhlTextStyleInfo;

#define _NhlNgsTextStyleInfo	".gsTextStyleInfo"
#define _NhlCgsTextStyleInfo	".GsTextStyleInfo"

typedef struct _NhlTextStyleLayerPart{

	_NhlTextStyleInfo	*tsp;
	_NhlTextStyleInfo	ts;

}NhlTextStyleLayerPart;

typedef struct _NhlTextStyleLayerRec{
	NhlBaseLayerPart		base;
	NhlStyleLayerPart		style;
	NhlTextStyleLayerPart		textstyle;
}NhlTextStyleLayerRec;

typedef struct _NhlTextStyleClassPart {
	char *foo;
}NhlTextStyleClassPart;

typedef struct _NhlTextStyleClassRec{
	NhlBaseClassPart		base_class;
	NhlStyleClassPart		style_class;
	NhlTextStyleClassPart		textstyle_class;
}NhlTextStyleClassRec;

typedef struct _NhlTextStyleClassRec	*NhlTextStyleClass;
typedef struct _NhlTextStyleLayerRec	*NhlTextStyleLayer;

extern NhlTextStyleClassRec		NhltextStyleClassRec;


/* the class pointers -- private now -- should they be public ? */

extern NhlClass NhllineStyleClass;
extern NhlClass NhlfillStyleClass;
extern NhlClass NhlmarkerStyleClass;
extern NhlClass NhltextStyleClass;

#endif  /*_NSUBSTYLESP_h*/
