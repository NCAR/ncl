/*
 *      $Id: TransObjP.h,v 1.4 1993-12-13 23:35:06 ethan Exp $
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

typedef NhlErrorTypes (*NhlTransformPoint)(
#if 	NhlNeedProto
Layer	/* instance */,
Layer	/* parent */,
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
Layer	/* instance */,
Layer 	/* parent */
#endif
);

typedef struct _TransObjLayerPart {
	float out_of_range;
}TransObjLayerPart;


typedef struct _TransObjLayerRec {
	ObjLayerPart	base;
	TransObjLayerPart	trobj;
}TransObjLayerRec;


typedef struct _TransObjLayerClassPart {
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
} TransObjLayerClassPart;

typedef struct _TransObjLayerClassRec {
	ObjLayerClassPart	base_class;
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
