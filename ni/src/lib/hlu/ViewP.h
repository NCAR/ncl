/*
 *      $Id: ViewP.h,v 1.9 1995-02-19 08:19:10 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ViewP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 1 10:01:24 MDT 1992
 *
 *	Description:	Private header file for NhlViewLayerClass. Contains
 *			NhlSegTransList typedef for storing output from the
 *			Segments utility.
 */
#ifndef _NViewP_h
#define _NViewP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/ViewI.h>
#include <ncarg/hlu/Workstation.h>

#include <ncarg/hlu/Segments.h>

typedef struct _NhlSegTransListNode {
	float	seg_trans[6];
	struct _NhlTransDat	*seg_trans_dat;
	struct _NhlSegTransListNode *next;
} NhlSegTransListNode, *NhlSegTransList;

typedef struct _NhlLayerList{
	struct _NhlLayerRec *layer;
	struct _NhlLayerList *next;
} NhlLayerListNode, *NhlLayerList;

typedef struct _NhlViewLayerPart {
	/* User setable resource fields */

	float		x,y;
	float		width,height;
	NhlBoolean	keep_aspect;
	NhlBoolean	use_segments;

	/* Internal private fields */

	NhlBoolean	x_set;
	NhlBoolean	y_set;
	NhlBoolean	width_set;
	NhlBoolean	height_set;
	
	NhlSegTransList	plot_segments_list;
	NhlLayerList	children;
	int		segment_wksid;

/*
* Created at initialize this transformation data is used to compute an
* Intermediate transformation matrix that describes the movement needs to be generated. It is
* used to transform the x,y,width and height fields of all of the
* children. The children then compute their own private transformation
* matrix.
*/

	NhlTransDat	*thetrans_children;
/*
* this is an intermediate transformation matrix
*/
	float	trans_children[6];
	float	aspect;

	/* Export Values */

	float fr,fl,fb,ft;
	
	/* import Values */

	float ur,ul,ub,ut;	/* these are user coordinate values they are
					set by the plot level of the HLU */

} NhlViewLayerPart;

typedef struct _NhlViewLayerRec {
	NhlBaseLayerPart base;
	NhlViewLayerPart view;
} NhlViewLayerRec;

typedef struct _NhlViewLayerClassPart {
	int	segment_workstation;
	NhlErrorTypes	(*get_bb)();
} NhlViewLayerClassPart; 

typedef struct _NhlViewLayerClassRec {
	NhlBaseLayerClassPart	base_class;
	NhlViewLayerClassPart	view_class;
} NhlViewLayerClassRec;

extern NhlViewLayerClassRec NhlviewLayerClassRec;

#define NhlDEFAULT_SEG_WKS 2
#define NhlDEFAULT_CONNECTION_ID 1
#define NhlDEFAULT_SEG_WKS_TYPE 3

extern void _NhlAddViewChildLayer(
#if	NhlNeedProto
NhlLayer   /* instance */,
NhlLayer   /* child */
#endif
);
 
extern void _NhlDeleteViewChildLayer(
#if	NhlNeedProto
NhlLayer   /* instance */,
NhlLayer   /* child */
#endif
);

extern NhlTransDat *_NhlNewViewSegment(
#if	NhlNeedProto
NhlLayer   /* instance */
#endif
);

extern void _NhlDeleteViewSegment(
#if	NhlNeedProto
NhlLayer,  /* instance */
NhlTransDat*    /*trandat */
#endif
);

extern NhlErrorTypes _NhlResetViewSegment(
#if	NhlNeedProto
NhlLayer,  /* instance */
NhlTransDat */* segdat */
#endif
);

extern void _NhlAddBBInfo(
#if	NhlNeedProto
float,		/*t */
float,		/*b */
float,		/*r */
float,		/*l */
NhlBoundingBox*	/*thebox*/
#endif
);

extern void _NhlInternalSetView(
#if	NhlNeedProto
NhlViewLayer	/* theview */,
float		/* x */,
float		/* y */,
float		/* width */,
float		/* height */,
int		/* keep_asp */
#endif
);

extern NhlErrorTypes _NhlGetBB(
#if	NhlNeedProto
	NhlLayer	instance,
	NhlBoundingBox* /* thebox */
#endif
);


#define NHL_DEFAULT_VIEW_WIDTH  .6
#define NHL_DEFAULT_VIEW_HEIGHT .6
#define NHL_DEFAULT_VIEW_X	.2
#define NHL_DEFAULT_VIEW_Y	.8
#define NHL_DEFAULT_VIEW_WIDTH_STR  ".6"
#define NHL_DEFAULT_VIEW_HEIGHT_STR ".6"
#define NHL_DEFAULT_VIEW_X_STR	".2"
#define NHL_DEFAULT_VIEW_Y_STR	".8"

#endif	/*_NViewP_h*/
