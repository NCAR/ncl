
/*
 *      $Id: TransObjP.h,v 1.2 1993-05-27 19:11:29 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TransObjP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 16 10:48:21 MDT 1992
 *
 *	Description:	This is the public header file for the TransObj class.
 *			The TransObjLayerClass is responsible for managing 
 *			transformations DATA==>VIEWPORT definitions.
 */
#ifndef _NTransObjP_h
#define  _NTransObjP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/TransObj.h>

typedef NhlErrorTypes (*NhlLineToProc)(
#if	NhlNeedProto
Layer   /* instance */,
Layer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);
typedef struct _TransObjLayerPart {
	/* Publicly set values */
	int dash_pattern;
	char *line_label;
	float line_thickness;	
	int line_color;	
	float line_label_font_height;
	float line_dash_seglen;
	/* private fields */
	int char_size;
	int dash_dollar_size;
}TransObjLayerPart;


typedef struct _TransObjLayerRec {
	BaseLayerPart	base;
	TransObjLayerPart	trobj;
}TransObjLayerRec;

typedef struct _TransObjLayerClassPart {
	NhlErrorTypes	(*set_trans)();
	NhlErrorTypes	(*trans_type)();
/*
* linear portion
*/
	NhlErrorTypes	(*win_to_ndc)();
	NhlErrorTypes	(*ndc_to_win)();
/*
* possibly not linear transformation
*/
	NhlErrorTypes	(*data_to_win)();
	NhlErrorTypes	(*win_to_data)();
/*
* intermediate transformations
*/
	NhlErrorTypes	(*data_to_compc)();
	NhlErrorTypes	(*compc_to_data)();
	NhlErrorTypes	(*win_to_compc)();
	NhlErrorTypes	(*compc_to_win)();
/*
* Drawing primatives
*/
	NhlLineToProc	data_lineto;
	NhlLineToProc	compc_lineto;
	NhlLineToProc	win_lineto;
	NhlLineToProc	NDC_lineto;
} TransObjLayerClassPart;

typedef struct _TransObjLayerClassRec {
	BaseLayerClassPart	base_class;
	TransObjLayerClassPart  trobj_class;
} TransObjLayerClassRec;

extern TransObjLayerClassRec transObjLayerClassRec;


extern char *dash_patterns[];

extern void _NhlTransClipLine(
#ifdef NhlNeedProto
float /*xl*/,
float /*xr*/,
float /*yt*/,
float /*yb*/,
float * /*x0*/,
float * /*y0*/,
float * /*x1*/,
float * /*y1*/,
float /*missing*/
#endif
);

#endif  /*_NTransObjP_h*/

