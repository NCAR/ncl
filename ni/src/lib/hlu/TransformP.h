/*
 *      $Id: TransformP.h,v 1.26 2003-09-10 21:29:59 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TransformP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	Provides generic hooks for plot classes to assign
 *			functions that compute the forward and backward 
 *			data transformations to support point-n-click 
 *			features.
 */

#ifndef _NTRANSFORMP_h
#define _NTRANSFORMP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/PlotManagerI.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/TransObjI.h>
#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/TransformI.h>

#define NhlNtfOverlayObject	".tfOverlayObject"
#define NhlCtfOverlayObject	".TfOverlayObject"
#define NhlNtfOverlayTrans	".tfOverlayTrans"
#define NhlCtfOverlayTrans	".TfOverlayTrans"
#define NhlNtfOverlayStatus	".tfOverlayStatus"
#define NhlCtfOverlayStatus	".TfOverlayStatus"
#define NhlNtfBaseXF 		".tfBaseXF"
#define NhlCtfBaseXF 		".TfBaseXF"
#define NhlNtfBaseYF 		".tfBaseYF"
#define NhlCtfBaseYF 		".TfBaseYF"
#define NhlNtfBaseWidthF	".tfBaseWidthF"
#define NhlCtfBaseWidthF	".TfBaseWidthF"
#define NhlNtfBaseHeightF	".tfBaseHeightF"
#define NhlCtfBaseHeightF	".TfBaseHeightF"

/*
 * The Transform superclass marks resources as set when appropriate, but
 * leaves it to the subclasses to restore the ..._set flags to False
 * according to their own needs.
 */

typedef struct NhlTransformLayerPart {

	/* Public resource fields */

	NhlBoolean		plot_manager_on;
	NhlOverlayMode		do_ndc_overlay;
	NhlBoolean		line_interpolation_on;
	NhlGenArray		poly_draw_list;
	NhlDrawOrder		poly_draw_order;
	NhlBoolean		x_min_set;
	float 			x_min;
	NhlBoolean		x_max_set;
	float			x_max;
	NhlBoolean		x_axis_type_set;
        NhlAxisType		x_axis_type;
	NhlBoolean		x_log_set;
	NhlBoolean		x_log;
	NhlBoolean		x_reverse_set;
	NhlBoolean		x_reverse;
	NhlBoolean		y_min_set;
	float 			y_min;
	NhlBoolean		y_max_set;
	float			y_max;
	NhlBoolean		y_axis_type_set;
        NhlAxisType		y_axis_type;
	NhlBoolean		y_log_set;
	NhlBoolean		y_log;
	NhlBoolean		y_reverse_set;
	NhlBoolean		y_reverse;
	NhlBoolean		grid_type_set;
        NhlGridType   		grid_type;

	/* Private resource fields, set only by the overlay manager */

	NhlLayer		overlay_trans_obj;  
	NhlLayer		overlay_object; 
	NhltfOverlayStatus	overlay_status;
	float			bx, by, bw, bh; /* base viewport */

	/* 
	 * Private fields for members of the Transform class 
	 * All transform classes supporting overlays should fill in 
         * the first field. The data fields should be filled in by
	 * all classes that accept data objects.
	 */
 
	NhlLayer		trans_obj;
	float			data_xstart; /* start may be > than end */
	float			data_xend;
	float			data_ystart;
	float			data_yend;
	NhlBoolean		sticky_x_min_set;
	NhlBoolean		sticky_x_max_set;
	NhlBoolean		sticky_y_min_set;
	NhlBoolean		sticky_y_max_set;
        _NhlCBList		overlaystatuscb;
	NhlBoolean		poly_clip_on;

} NhlTransformLayerPart;

typedef struct _NhlTransformLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
} NhlTransformLayerRec;

typedef NhlErrorTypes (*NhlTransFunction)(
#ifdef	NhlNeedProto
        NhlLayer           /* plot */,
        float*          /* x */,
        float*          /* y */,
        int             /* n */,
        float*          /* xout */,
        float*          /* yout */,
        float*          /*xmissing*/,
        float*          /*ymissing*/,
	int *		/*status*/,
	float *		/*out_of_range*/
#endif
);

typedef enum _NhltfOverlayCapability {
	_tfNotOverlayCapable,
	_tfOverlayBaseOnly,
	_tfOverlayMemberOnly,
	_tfOverlayBaseOrMember
} NhltfOverlayCapability;

typedef NhlErrorTypes (*NhlTransPolyFunc)(
#ifdef	NhlNeedProto
        NhlLayer           /* plot */,
        float*          /* x */,
        float*          /* y */,
        int             /* n */
#endif
);

#define	NhlInheritTransFunc	((NhlTransFunction)_NhlInherit)
#define	NhlInheritPolyTransFunc	((NhlTransPolyFunc)_NhlInherit)

typedef struct NhlTransformClassPart{
	NhltfOverlayCapability	overlay_capability;
	NhlTransFunction	data_to_ndc;
	NhlTransFunction	ndc_to_data;
	NhlTransPolyFunc	data_polyline;
	NhlTransPolyFunc	ndc_polyline;
	NhlTransPolyFunc	data_polygon;
	NhlTransPolyFunc	ndc_polygon;
	NhlTransPolyFunc	data_polymarker;
	NhlTransPolyFunc	ndc_polymarker;
} NhlTransformClassPart;

typedef struct _NhlTransformClassRec{
	NhlBaseClassPart	base_class;
	NhlViewClassPart	view_class;
	NhlTransformClassPart	trans_class;
} NhlTransformClassRec;


typedef struct _NhlTransformClassRec *NhlTransformClass;
typedef struct _NhlTransformLayerRec *NhlTransformLayer;

extern NhlTransformClassRec NhltransformClassRec;


/*
 * Private class functions
 */

extern NhlErrorTypes _NhltfDrawSegment(
#if	NhlNeedProto
        NhlLayer	plot,
	NhlLayer	trobj,
        NhlTransDat	*transdat,
	NhlString	entry_name
#endif
);

extern NhlErrorTypes _NhltfInitSegment(
#if	NhlNeedProto
        NhlLayer	plot,
	NhlLayer	trobj,
	NhlTransDat	**transdat,					      
	NhlString	entry_name
#endif
);

extern NhlErrorTypes _NhltfCheckCoordBounds(
#if	NhlNeedProto
        NhlTransformLayer	new,
	NhlTransformLayer	old,
	NhlString		entry_name
#endif
);

#endif  /* _NTRANSFORMP_h */
