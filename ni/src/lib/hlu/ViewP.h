/*
 *      $Id: ViewP.h,v 1.3 1993-10-19 17:53:10 boote Exp $
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
 *	Description:	Private header file for ViewLayerClass. Contains
 *			NhlSegTransList typedef for storing output from the
 *			Segments utility.
 */
#ifndef _NViewP_h
#define _NViewP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/View.h>

#include <ncarg/hlu/Segments.h>

typedef struct _NhlSegTransListNode {
	float	seg_trans[6];
	struct _NhlTransDat	*seg_trans_dat;
	struct _NhlSegTransListNode *next;
} NhlSegTransListNode, *NhlSegTransList;


typedef struct _ViewLayerPart {
	/* User setable resource fields */

	float x,y;
	float width,height;
	int	keep_aspect;

	/* Internal private fields */
	
	NhlSegTransList	plot_segments_list;
	LayerList	children;
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

} ViewLayerPart;

typedef struct _ViewLayerRec {
	BaseLayerPart base;
	ViewLayerPart view;
} ViewLayerRec;

typedef struct _ViewLayerClassPart {
	int	segment_workstation;
	NhlErrorTypes	(*get_bb)();
} ViewLayerClassPart; 

typedef struct _ViewLayerClassRec {
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
} ViewLayerClassRec;

extern ViewLayerClassRec viewLayerClassRec;

#define NhlDEFAULT_SEG_WKS 2
#define NhlDEFAULT_CONNECTION_ID 1
#define NhlDEFAULT_SEG_WKS_TYPE 3

extern void _NhlAddViewChildLayer(
#ifdef NhlNeedProto
Layer   /* instance */,
Layer   /* child */
#endif
);
 
extern void _NhlDeleteViewChildLayer(
#ifdef NhlNeedProto
Layer   /* instance */,
Layer   /* child */
#endif
);

extern NhlTransDat *_NhlNewViewSegment(
#ifdef NhlNeedProto
Layer   /* instance */
#endif
);

extern void _NhlDeleteViewSegment(
#ifdef NhlNeedProto
Layer,  /* instance */
NhlTransDat*    /*trandat */
#endif
);

extern NhlErrorTypes _NhlResetViewSegment(
#ifdef NhlNeedProto
Layer,  /* instance */
NhlTransDat */* segdat */
#endif
);

extern void _NhlAddBBInfo(
#ifdef NhlNeedProto
float,		/*t */
float,		/*b */
float,		/*r */
float,		/*l */
NhlBoundingBox*	/*thebox*/
#endif
);

extern void _NhlInternalSetView(
#ifdef NhlNeedProto
ViewLayer	/* theview */,
float		/* x */,
float		/* y */,
float		/* width */,
float		/* height */,
int		/* keep_asp */
#endif
);



#define NHL_DEFAULT_VIEW_WIDTH  .8
#define NHL_DEFAULT_VIEW_HEIGHT .8
#define NHL_DEFAULT_VIEW_X	.1
#define NHL_DEFAULT_VIEW_Y	.9
#define NHL_DEFAULT_VIEW_WIDTH_STR  ".8"
#define NHL_DEFAULT_VIEW_HEIGHT_STR ".8"
#define NHL_DEFAULT_VIEW_X_STR	".1"
#define NHL_DEFAULT_VIEW_Y_STR	".9"

#endif	/*_NViewP_h*/
