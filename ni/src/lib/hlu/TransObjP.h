/*
 *      $Id: TransObjP.h,v 1.21 2004-10-05 22:50:34 dbrown Exp $
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
 *			The NhlTransObjClass is responsible for managing 
 *			transformations DATA==>VIEWPORT definitions.
 */
#ifndef _NTransObjP_h
#define  _NTransObjP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/TransObjI.h>

typedef NhlErrorTypes (*NhlLineToProc)(
#if	NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

typedef NhlErrorTypes (*NhlPolygonProc)(
#if	NhlNeedProto
NhlLayer instance,
float   *x,
float   *y,
int     n
#endif
);

typedef NhlErrorTypes (*NhlTransformPoint)(
#if 	NhlNeedProto
NhlLayer	/* instance */,
float*  /* x */,
float*  /* y */,
int 	/* n */,
float*  /* xout */,
float*  /* yout */,
float*  /* xmissing */,
float*  /* ymissing */,
int*	/* status */		/* True if out of range value exists in input */
#endif
);

typedef NhlErrorTypes (*NhlSetTransFunc)(
#if	NhlNeedProto
NhlLayer	/* instance */,
NhlLayer 	/* parent */
#endif
);

typedef struct _NhlTransObjLayerPart {
	NhlBoolean	x_min_set;
	float 		x_min;
	NhlBoolean	x_max_set;
	float		x_max;
	NhlBoolean	x_reverse_set;
	int 		x_reverse;
        
	NhlBoolean	y_min_set;
	float 		y_min;
	NhlBoolean	y_max_set;
	float		y_max;
	NhlBoolean	y_reverse_set;
	int 		y_reverse;

	float		out_of_range;
	NhlBoolean	line_interpolation_on;
	float		resolution;
	NhlBoolean	grid_type_set;
        NhlGridType   	grid_type;
	NhlBoolean      xc_isbounds;
	NhlBoolean      yc_isbounds;
	NhlBoolean      do_bounds;
/*
 * Each time the trans obj changes this field is incremented
 */
	int		change_count;
	/*
	 * These fields are filled in by the "SetTrans" function, and then
	 * used by the LineTo and CoordToCoord functions.
	 */
	NhlLayer	wkptr;
	float		x;
	float		y;
	float		width;
	float		height;
	int		point_count;
	float		data_xstart;
	float		data_xend;
	float		data_ystart;
	float		data_yend;
        NhlBoolean	off_screen;
}NhlTransObjLayerPart;


typedef struct _NhlTransObjLayerRec {
	NhlObjLayerPart		base;
	NhlTransObjLayerPart	trobj;
}NhlTransObjLayerRec;

#define NhlInheritTransPoint	((NhlTransformPoint)_NhlInherit)
#define NhlInheritLineTo	((NhlLineToProc)_NhlInherit)
#define NhlInheritPolygon	((NhlPolygonProc)_NhlInherit)

typedef struct _NhlTransObjClassPart {
	NhlSetTransFunc set_trans;
	NhlErrorTypes	(*trans_type)();
/*
* linear portion
*/
	NhlTransformPoint win_to_ndc;
	NhlTransformPoint ndc_to_win;
/*
* possibly not linear transformation
*/
	NhlTransformPoint data_to_win;
	NhlTransformPoint win_to_data;
/*
* intermediate transformations
*/
	NhlTransformPoint data_to_compc;
	NhlTransformPoint compc_to_data;
	NhlTransformPoint win_to_compc;
	NhlTransformPoint compc_to_win;
/*
* Drawing primatives
*/
	NhlLineToProc	data_lineto;
	NhlLineToProc	compc_lineto;
	NhlLineToProc	win_lineto;
	NhlLineToProc	NDC_lineto;
	NhlPolygonProc  data_polygon;
} NhlTransObjClassPart;

typedef struct _NhlTransObjClassRec {
	NhlObjClassPart		base_class;
	NhlTransObjClassPart	trobj_class;
} NhlTransObjClassRec;

typedef struct _NhlTransObjClassRec *NhlTransObjClass;
typedef struct _NhlTransObjLayerRec *NhlTransObjLayer;

extern NhlTransObjClassRec NhltransObjClassRec;


extern void _NhlTransClipLine(
#if	NhlNeedProto
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

NhlErrorTypes _NhlTransLLUSet(
#if NhlNeedProto
    float vl,
    float vr,
    float vb,
    float vt,
    float wl,
    float wr,
    float wb,
    float wt,
    int lf,
    NhlBoolean *off_screen,
    NhlString entry
#endif
);

#endif  /*_NTransObjP_h*/
