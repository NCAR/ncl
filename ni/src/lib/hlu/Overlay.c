/*
 *      $Id: Overlay.c,v 1.22 1994-09-23 23:36:54 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Overlay.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Manages the drawing for plot objects that must
 *			share a common overlay transformation
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/OverlayP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/AnnotationP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>


/*
 * Function:	ResourceUnset
 *
 * Description:	This function can be used to determine if a resource has
 *		been set at initialize time either in the Create call or
 *		from a resource data base. In order to use it the Boolean
 *		'..resource_set' variable MUST directly proceed the name
 *		of the resource variable it refers to in the LayerPart
 *		struct. Also a .nores Resource for the resource_set variable
 *		must directly preceed the Resource of interest in the 
 *		Resource initialization list in this module.
 *
 * In Args:	
 *		NrmName		name,
 *		NrmClass	class,
 *		NhlPointer	base,
 *		unsigned int	offset
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

/*ARGSUSED*/
static NhlErrorTypes
ResourceUnset
#if	__STDC__
(
	NrmName		name,
	NrmClass	class,
	NhlPointer	base,
	unsigned int	offset
)
#else
(name,class,base,offset)
	NrmName		name;
	NrmClass	class;
	NhlPointer	base;
	unsigned int	offset;
#endif
{
	char *cl = (char *) base;
	NhlBoolean *set = (NhlBoolean *)(cl + offset - sizeof(NhlBoolean));

	*set = False;

	return NhlNOERROR;
}

#define	Oset(field)	NhlOffset(NhlOverlayLayerRec,overlay.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNovOverlayIds,NhlCovOverlayIds,NhlTIntegerGenArray,
		sizeof(NhlPointer),
		Oset(overlay_ids),
		NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFreeGenArray},

/* Annotation resources  */

	{ NhlNovDisplayTitles,NhlCovDisplayTitles,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_titles),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{ NhlNovTitleZone,NhlCovTitleZone,NhlTInteger,sizeof(int),
		  Oset(title_zone),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlOV_DEF_TITLE_ZONE),0,NULL},
	{ NhlNovDisplayTickMarks,NhlCovDisplayTickMarks,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_tickmarks),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{ NhlNovTickMarkZone,NhlCovTickMarkZone,NhlTInteger,sizeof(int),
		  Oset(tickmark_zone),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlOV_DEF_TICKMARK_ZONE),0,NULL},

/* LabelBar resources */

	{ NhlNovDisplayLabelBar,NhlCovDisplayLabelBar,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_labelbar),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{ NhlNovLabelBarZone,NhlCovLabelBarZone,NhlTInteger,sizeof(int),
		  Oset(labelbar_zone),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlOV_DEF_LABELBAR_ZONE),0,NULL},
	{ NhlNovLabelBarWidthF, NhlCovLabelBarWidthF,NhlTFloat, sizeof(float),
		  Oset(lbar_width),
		  NhlTString,_NhlUSET("0.2" ),0,NULL},
	{ NhlNovLabelBarHeightF, NhlCovLabelBarHeightF,NhlTFloat, 
		  sizeof(float),
		  Oset(lbar_height),
		  NhlTString,_NhlUSET("0.5" ),0,NULL},
	{NhlNovLabelBarSide, NhlCovLabelBarSide, NhlTPosition, 
		 sizeof(NhlJustification),
		 Oset(lbar_side),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlRIGHT),0,NULL},
	{NhlNovLabelBarParallelPosF,NhlCovLabelBarParallelPosF,NhlTFloat,
		 sizeof(float),
		 Oset(lbar_para_pos),
		 NhlTString,_NhlUSET("0.5"),0,NULL},
	{NhlNovLabelBarOrthogonalPosF,NhlCovLabelBarOrthogonalPosF,NhlTFloat,
		 sizeof(float),
		 Oset(lbar_ortho_pos),
		 NhlTString,_NhlUSET("0.02"),0,NULL},
	{ NhlNovLabelBarXOffsetF, NhlCovLabelBarXOffsetF,NhlTFloat, 
		  sizeof(float),
		  Oset(lbar_x_off),
		  NhlTString,_NhlUSET("0.02" ),0,NULL},
	{ NhlNovLabelBarYOffsetF, NhlCovLabelBarYOffsetF,NhlTFloat, 
		  sizeof(float),
		  Oset(lbar_y_off),
		  NhlTString,_NhlUSET("0.00" ),0,NULL},
	{NhlNovLabelBarPosition, NhlCovLabelBarPosition, NhlTPosition, 
		 sizeof(NhlJustification),
		 Oset(lbar_pos),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlBOTTOM),0,NULL},


/* Legend resources */

	{ NhlNovDisplayLegend,NhlCovDisplayLegend,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_legend),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{ NhlNovLegendZone,NhlCovLegendZone,NhlTInteger,sizeof(int),
		  Oset(legend_zone),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlOV_DEF_LEGEND_ZONE),0,NULL},
	{ NhlNovLegendWidthF, NhlCovLegendWidthF,NhlTFloat, sizeof(float),
		  Oset(lgnd_width),
		  NhlTString,_NhlUSET("0.45" ),0,NULL},
	{ NhlNovLegendHeightF, NhlCovLegendHeightF,NhlTFloat, 
		  sizeof(float),
		  Oset(lgnd_height),
		  NhlTString,_NhlUSET("0.175" ),0,NULL},
	{NhlNovLegendSide, NhlCovLegendSide, NhlTPosition, 
		 sizeof(NhlPosition),
		 Oset(lgnd_side),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlBOTTOM),0,NULL},
	{NhlNovLegendParallelPosF,NhlCovLegendParallelPosF,NhlTFloat,
		 sizeof(float),
		 Oset(lgnd_para_pos),NhlTString,
		 _NhlUSET("0.5"),0,NULL},
	{NhlNovLegendOrthogonalPosF,NhlCovLegendOrthogonalPosF,NhlTFloat,
		 sizeof(float),
		 Oset(lgnd_ortho_pos),NhlTString,
		 _NhlUSET("0.02"),0,NULL},
	{NhlNovLegendPosition, NhlCovLegendPosition, NhlTPosition, 
		 sizeof(NhlPosition),
		 Oset(lgnd_pos),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTER),0,NULL},
	{ NhlNovLegendXOffsetF, NhlCovLegendXOffsetF,NhlTFloat, 
		  sizeof(float),
		  Oset(lgnd_x_off),
		  NhlTString,_NhlUSET("0.00" ),0,NULL},
	{ NhlNovLegendYOffsetF, NhlCovLegendYOffsetF,NhlTFloat, 
		  sizeof(float),
		  Oset(lgnd_y_off),
		  NhlTString,_NhlUSET("0.02" ),0,NULL},

/* End-documented-resources */

/* Unimplemented resources */

	{ NhlNovPreDrawOrder,NhlCovPreDrawOrder,NhlTIntegerGenArray,
		  sizeof(NhlPointer),
		  Oset(pre_draw_order),
		  NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNovPostDrawOrder,NhlCovPostDrawOrder,NhlTIntegerGenArray,
		  sizeof(NhlPointer),
		  Oset(post_draw_order),
		  NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFreeGenArray},

/* Private resources */

	{ NhlNovOverlayRecs,NhlCovOverlayRecs,NhlTGenArray,
		  sizeof(NhlPointer),
		  Oset(ov_rec_list),
		  NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNovUpdateReq,NhlCovUpdateReq,NhlTBoolean,
		  sizeof(NhlBoolean),
		  Oset(update_req),
		  NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},

/*
 * Intercepted tick mark resources
 */
	{ NhlNtmXBDataLeftF, NhlCtmXBDataLeftF,NhlTFloat, sizeof(float),
		  Oset(x_b_data_left),
		  NhlTString,_NhlUSET("0.0" ),0,NULL},
	{ NhlNtmXBDataRightF, NhlCtmXBDataRightF,NhlTFloat, sizeof(float),
		  Oset(x_b_data_right),
		  NhlTString,_NhlUSET("1.0" ),0,NULL},
	{ NhlNtmYLDataBottomF, NhlCtmYLDataBottomF,NhlTFloat, sizeof(float),
		  Oset(y_l_data_bottom),
		  NhlTString,_NhlUSET("0.0" ),0,NULL},
	{ NhlNtmYLDataTopF, NhlCtmYLDataTopF,NhlTFloat, sizeof(float),
		  Oset(y_l_data_top),
		  NhlTString,_NhlUSET("1.0" ),0,NULL},

/* 
 * Overlay only looks at the XLog and YLog directly
 */
	{ NhlNtrXMinF,NhlCtrXMinF,NhlTFloat,sizeof(float),
		Oset(x_min),
		NhlTString,_NhlUSET("0.0"),0,NULL},
	{ NhlNtrXMaxF,NhlCtrXMaxF,NhlTFloat,sizeof(float),
		Oset(x_max),
		NhlTString,_NhlUSET("1.0"),0,NULL},
	{ NhlNtrYMinF,NhlCtrYMinF,NhlTFloat,sizeof(float),
		Oset(y_min),
		NhlTString,_NhlUSET("0.0"),0,NULL},
	{ NhlNtrYMaxF,NhlCtrYMaxF,NhlTFloat,sizeof(float),
		Oset(y_max),
		NhlTString,_NhlUSET("1.0"),0,NULL},
	{ NhlNtrXLog, NhlCtrXLog,NhlTInteger, sizeof(int),
		  Oset(x_log),
		  NhlTImmediate,_NhlUSET((NhlPointer) 0 ),0,NULL},
	{ NhlNtrYLog, NhlCtrYLog,NhlTInteger, sizeof(int),
		  Oset(y_log),
		  NhlTImmediate,_NhlUSET((NhlPointer) 0 ),0,NULL},
	{ NhlNtrXReverse, NhlCtrXReverse,NhlTInteger, sizeof(int),
		  Oset(x_reverse),
		  NhlTImmediate,_NhlUSET((NhlPointer) 0 ),0,NULL},
	{ NhlNtrYReverse, NhlCtrYReverse,NhlTInteger, sizeof(int),
		  Oset(y_reverse),
		  NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL },
	{ NhlNtrYTensionF, NhlCtrYTensionF, NhlTFloat, sizeof(float),
		Oset(y_tension),
		NhlTString,_NhlUSET("2.0" ),0,NULL},
	{ NhlNtrXTensionF, NhlCtrXTensionF, NhlTFloat, sizeof(float),
		Oset(x_tension),
		NhlTString,_NhlUSET("2.0" ),0,NULL},
		
/*
 * Intercepted title resources
 */

	{NhlNtiMainOffsetXF,NhlCtiMainOffsetXF,NhlTFloat,sizeof(float),
		 Oset(ti_main_offset_x),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNtiXAxisOffsetXF,NhlCtiXAxisOffsetXF,NhlTFloat,sizeof(float),
		 Oset(ti_x_axis_offset_x),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNtiYAxisOffsetYF,NhlCtiYAxisOffsetYF,NhlTFloat,sizeof(float),
		 Oset(ti_y_axis_offset_y),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNtiXAxisPosition,NhlCtiXAxisPosition,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(ti_x_axis_position),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTER),0,NULL},
	{NhlNtiYAxisPosition,NhlCtiYAxisPosition,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(ti_y_axis_position),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTER),0,NULL},
	{NhlNtiMainPosition,NhlCtiMainPosition,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(ti_main_position),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTER),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(ti_main_font_height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNtiMainFontHeightF,NhlCtiTitleFontHeightsF,NhlTFloat,
		 sizeof(float),Oset(ti_main_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(ti_x_axis_font_height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNtiXAxisFontHeightF,NhlCtiTitleFontHeightsF,NhlTFloat,
		 sizeof(float),Oset(ti_x_axis_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(ti_y_axis_font_height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNtiYAxisFontHeightF,NhlCtiTitleFontHeightsF,
		 NhlTFloat,sizeof(float),Oset(ti_y_axis_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},

/* intercepted LabelBar resources */

	{NhlNlbLabelBar, NhlClbLabelBar, NhlTBoolean, 
		 sizeof(NhlBoolean),
		 Oset(lbar_on),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNlbJustification, NhlClbJustification, NhlTJustification, 
		 sizeof(NhlJustification),
		 Oset(lbar_just),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNlbOrientation, NhlClbOrientation, NhlTOrientation, 
		 sizeof(NhlOrientation),
		 Oset(lbar_orient),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlVERTICAL),0,NULL},

/* intercepted Legend resources */

	{NhlNlgLegend, NhlClgLegend, NhlTBoolean, 
		 sizeof(NhlBoolean),
		 Oset(lgnd_on),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNlgJustification, NhlClgJustification, NhlTJustification, 
		 sizeof(NhlJustification),
		 Oset(lgnd_just),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNlgOrientation, NhlClgOrientation, NhlTOrientation, 
		 sizeof(NhlOrientation),
		 Oset(lgnd_orient),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlVERTICAL),0,NULL}
};
#undef Oset

/* base methods */


static NhlErrorTypes OverlayClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes OverlayClassPartInitialize(
#ifdef NhlNeedProto
	NhlLayerClass	lc
#endif
);

static NhlErrorTypes OverlayInitialize(
#ifdef NhlNeedProto
        NhlLayerClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes OverlaySetValues(
#ifdef NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes 	OverlayGetValues(
#ifdef NhlNeedProto
	NhlLayer	layer,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes OverlayDestroy(
#ifdef NhlNeedProto
        NhlLayer	inst
#endif
);

static NhlErrorTypes OverlayDraw(
#ifdef NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes OverlayPreDraw(
#ifdef NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes OverlayPostDraw(
#ifdef NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes OverlayGetBB(
#ifdef NhlNeedProto
        NhlLayer        instance,
        NhlBoundingBox	*thebox
#endif
);

/* internal static functions */

static NhlErrorTypes InternalGetBB(
#ifdef NhlNeedProto
        NhlLayer	instance,
	NhlBoundingBox	*thebox,
        int  	        include_types,   
	char		*entry_name		   
#endif
);

static NhlAnnoRec *RecordAnnotation(
#ifdef NhlNeedProto
	NhlOverlayLayer ovl,
	ovAnnoType	type,
	int		status,
	int		zone
#endif
);

static NhlErrorTypes ManageAnnotations(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,				       
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes ManageExtAnnotation(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
#endif
);

static NhlJustification ConstrainJustification(
#ifdef NhlNeedProto
	NhlAnnoRec	*anno_rec
#endif
);

static NhlErrorTypes UpdateAnnoData(
#ifdef NhlNeedProto
	NhlAnnoRec	*anno_list,
	int		*max_zone,
	NhlString	entry_name
#endif
);

static NhlErrorTypes ManageTitles(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
#endif
);

static NhlErrorTypes ManageTickMarks(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
#endif
);

static NhlErrorTypes ManageLabelBar(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes ManageLegend(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes SetAnnoViews(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_list,
	int		zone,
	NhlBoolean	first,
	NhlString	entry_name
#endif
);

static NhlErrorTypes SetTickMarkView(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
#endif
);

static NhlErrorTypes SetTitleView(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
#endif
);

static NhlErrorTypes SetExternalView(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
#endif
);

static NhlErrorTypes SetViewTracking(
#ifdef NhlNeedProto
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
#endif
);

static NhlErrorTypes RestoreOverlayBase(
#ifdef NhlNeedProto
	NhlOverlayLayerPart	*ovp,
	int			plot_number
#endif
);

static NhlErrorTypes RemoveOverlayBase(
#ifdef NhlNeedProto
	NhlOverlayLayerPart	*ovp,
	int			plot_number
#endif
);

static NhlErrorTypes DissolveOverlay(
#ifdef NhlNeedProto
	NhlLayer		overlay_object
#endif
);

NhlOverlayLayerClassRec NhloverlayLayerClassRec = {
        {
/* class_name			*/      "Overlay",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlOverlayLayerRec),
/* class_inited			*/      False,
/* superclass			*/  (NhlLayerClass)&NhltransformLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	OverlayClassPartInitialize,
/* class_initialize		*/	OverlayClassInitialize,
/* layer_initialize		*/	OverlayInitialize,
/* layer_set_values		*/	OverlaySetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	OverlayGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	OverlayDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      OverlayDraw,

/* layer_pre_draw		*/      OverlayPreDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      OverlayPostDraw,
/* layer_clear			*/      NULL

        },
	{
/* segment_wkid			*/	0,
/* get_bb			*/	OverlayGetBB
	},
	{
/* overlay_capability 		*/	_tfNotOverlayCapable,
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL
	},
	{
/* wkspace_list			*/	NULL
	}
};

NhlLayerClass NhloverlayLayerClass = (NhlLayerClass)&NhloverlayLayerClassRec;

static NrmQuark Overlay_Ids;
static NrmQuark Overlay_Recs;
static NrmQuark Pre_Draw_Order;
static NrmQuark Post_Draw_Order;

/* static variables referenced by the low-level library mapping functions */

static NhlLayer Trans_Obj = NULL;
static NhlLayer Plot = NULL;
static NhlLayer Overlay_Trans_Obj = NULL;
static NhlLayer Overlay_Plot = NULL;

/*
 * Function:	OverlayClassInitialize
 *
 * Description:
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
OverlayClassInitialize
#if __STDC__
(
	void
)
#else
()
#endif
{
        _NhlEnumVals   annotationdisplaylist[] = {
		{NhlNOCREATE,		"nocreate"},
		{NhlNEVER,		"never"},
		{NhlCONDITIONAL,	"conditional"},
		{NhlALWAYS,		"always"}
        };

        _NhlRegisterEnumType(NhlTAnnotationDisplayMode,annotationdisplaylist,
					     NhlNumber(annotationdisplaylist));

	Overlay_Ids = NrmStringToQuark(NhlNovOverlayIds);
	Overlay_Recs = NrmStringToQuark(NhlNovOverlayRecs);
	Pre_Draw_Order = NrmStringToQuark(NhlNovPreDrawOrder);
	Post_Draw_Order = NrmStringToQuark(NhlNovPostDrawOrder);

	return NhlNOERROR;
}

/*
 * Function:	OverlayClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlOverlayLayerClassPart that cannot be initialized statically.
 *
 * In Args:	
 *		NhlLayerClass	lc	NhlLayer Class to init
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

static NhlErrorTypes
OverlayClassPartInitialize
#if	__STDC__
(
	NhlLayerClass	lc	/* NhlLayer Class to init	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* NhlLayer Class to init	*/
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;

	subret = _NhlRegisterChildClass(lc,NhltickMarkLayerClass,
					False,False,
					NhlNtmXBDataLeftF,
					NhlNtmXBDataRightF,
					NhlNtmYLDataBottomF,
					NhlNtmYLDataTopF,
					NULL);

	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;

	subret = _NhlRegisterChildClass(lc,NhltitleLayerClass,False,False,
					NhlNtiMainOffsetXF,
					NhlNtiXAxisOffsetXF, 
					NhlNtiYAxisOffsetYF, 
					NhlNtiXAxisPosition, 
					NhlNtiYAxisPosition, 
					NhlNtiMainPosition,
					NhlNtiMainFontHeightF,
					NhlNtiXAxisFontHeightF,
					NhlNtiYAxisFontHeightF,
					NULL);

	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;

	subret = _NhlRegisterChildClass(lc,NhllabelBarLayerClass,
					False,False,
					NhlNlbLabelBar,
					NhlNlbJustification,
					NhlNlbOrientation,
					NULL);

	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;

	subret = _NhlRegisterChildClass(lc,NhllegendLayerClass,
					False,False,
					NhlNlgLegend,
					NhlNlgJustification,
					NhlNlgOrientation,
					NULL);

	return MIN(subret,ret);
}


/*
 * Function:	OverlayInitialize
 *
 * Description: 
 *
 * In Args: 	class	objects layer_class
 *		req	instance record of requested values
 *		new	instance record of new object
 *		args	list of resources and values for reference
 *		num_args 	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes
OverlayInitialize
#if     __STDC__
(
	NhlLayerClass	class,
	NhlLayer		req,
	NhlLayer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlLayerClass      class;
        NhlLayer           req;
        NhlLayer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "OverlayInitialize";
	NhlOverlayLayer		ovnew = (NhlOverlayLayer) new;
	NhlOverlayLayerPart	*ovp = &(ovnew->overlay);
	NhlTransformLayer	parent = (NhlTransformLayer)ovnew->base.parent;
	NhlovRec			*ov_rec;
	int			i;
/*
 * Array and object initializations
 */
	ovp->tickmarks = NULL;
	ovp->titles = NULL;
	ovp->labelbar = NULL;
	ovp->legend = NULL;
	ovp->x_irr = NULL;
	ovp->y_irr = NULL;

/*
 * Make sure the transformation supplied is valid
 */
	if (parent->trans.trans_obj == NULL ||
	    ! _NhlIsTransObj(parent->trans.trans_obj)) {
		e_text = "%s: invalid transformation supplied";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	ovnew->trans.overlay_object = new;
	ovnew->trans.trans_obj = parent->trans.trans_obj;
/*
 * Allocate an array to store pointers to the overlay records. 
 * Then allocate an array for the first member element.
 */

	if ((ovp->ov_recs = (NhlovRec **) 
	     NhlMalloc(NhlOV_ALLOC_UNIT * sizeof(NhlovRec *))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if ((ov_rec = (NhlovRec *) 
	     NhlMalloc(NhlOV_ALLOC_UNIT * sizeof(NhlovRec))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	ovp->overlay_alloc = NhlOV_ALLOC_UNIT;
	ovp->overlay_count = 1;
	ov_rec->plot = parent;
	ov_rec->ov_obj = new;
	ov_rec->anno_list = NULL;
	ovp->ov_recs[0] = ov_rec;

	if (ovp->display_tickmarks > NhlNOCREATE) {
		if (ovp->tickmark_zone > NhlOV_DEF_TICKMARK_ZONE) {
			e_text = 
            "%s: Tickmarks cannot have zone number greater than 2: resetting";
			ovp->tickmark_zone = NhlOV_DEF_TICKMARK_ZONE;
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		}
		if (RecordAnnotation(ovnew,ovTICKMARK,ovp->display_tickmarks,
				     ovp->tickmark_zone) == NULL) {
			e_text = "%s: error creating annotation record";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	if (ovp->display_titles > NhlNOCREATE) {
		if (ovp->title_zone < 0) {
			e_text = "%s: Invalid title zone number: defaulting";
			ovp->title_zone = NhlOV_DEF_TITLE_ZONE;
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		}
		if (RecordAnnotation(ovnew,ovTITLE,ovp->display_titles,
			     ovp->title_zone) == NULL) {
			e_text = "%s: error creating annotation record";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	if (ovp->display_legend > NhlNOCREATE) {
		if (ovp->legend_zone < 0) {
			e_text = "%s: Invalid legend zone number: defaulting";
			ovp->legend_zone = NhlOV_DEF_LEGEND_ZONE;
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		}
		if (RecordAnnotation(ovnew,ovLEGEND,ovp->display_legend,
				     ovp->legend_zone) == NULL) {
			e_text = "%s: error creating annotation record";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (ovp->display_labelbar > NhlNOCREATE)
		if (ovp->labelbar_zone < 0) {
			e_text = "%s: Invalid legend zone number: defaulting";
			ovp->labelbar_zone = NhlOV_DEF_LABELBAR_ZONE;
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		}
		if (RecordAnnotation(ovnew,ovLABELBAR,ovp->display_labelbar,
			     ovp->labelbar_zone) == NULL) {
			e_text = "%s: error creating annotation record";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
		
	for (i = ovp->overlay_count; i < ovp->overlay_alloc; i++) 
		ovp->ov_recs[i] = NULL;
	
	ret = ManageAnnotations(ovnew,(NhlOverlayLayer)req,True,args,num_args);
 
	return ret;
}


/*
 * Function:	OverlaySetValues
 *
 * Description: If the parent creates a new overlay trans object, or
 *		separates its own trans object from the overlay trans
 *		object it must notify the overlay object via a SetValues.
 *
 * In Args:	old	copy of old instance record
 *		reference	requested instance record
 *		new	new instance record	
 *		args 	list of resources and values for reference
 *		num_args	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes OverlaySetValues
#if  __STDC__
(
	NhlLayer		old,
	NhlLayer		reference,
	NhlLayer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	NhlLayer		old;
	NhlLayer		reference;
	NhlLayer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "OverlaySetValues";
	NhlOverlayLayer		ovnew = (NhlOverlayLayer) new;
	NhlOverlayLayer		ovold = (NhlOverlayLayer) old;
	NhlOverlayLayerPart	*ovp = &(ovnew->overlay);
	NhlOverlayLayerPart	*oovp = &(ovold->overlay);
        NhlSArg			sargs[16];
        int			nargs = 0;
	int			i;
	NhlBoolean		update_req = False;

	if (ovnew->view.use_segments != ovold->view.use_segments) {
		ovnew->view.use_segments = ovold->view.use_segments;
		ret = MIN(ret,NhlWARNING);
		e_text = "%s: attempt to set create-only resource overridden";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
	}
	ovnew->trans.trans_obj = ovnew->trans.overlay_trans_obj;

/* 
 * The annotation children can only be created during initialization
 */
	if (ovp->display_tickmarks != NhlNOCREATE &&
	    ovp->tickmarks == NULL) {
		ret = MIN(ret,NhlWARNING);
		e_text = 
		 "%s: overlay TickMarks can be created only during NhlCreate";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNovDisplayTickMarks);
		ovp->display_tickmarks = NhlNOCREATE;
	}

	if (ovp->display_titles != NhlNOCREATE &&
	    ovp->titles == NULL) {
		ret = MIN(ret,NhlWARNING);
		e_text = 
		 "%s: overlay Titles can be created only during NhlCreate";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNovDisplayTitles);
		ovp->display_titles = NhlNOCREATE;
	}

	if (ovp->display_labelbar != NhlNOCREATE &&
	    ovp->labelbar == NULL) {
		ret = MIN(ret,NhlWARNING);
		e_text =
		 "%s: overlay LabelBar can be created only during NhlCreate";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNovDisplayLabelBar);
		ovp->display_labelbar = NhlNOCREATE;
	}

	if (ovp->display_legend != NhlNOCREATE &&
	    ovp->legend == NULL) {
		ret = MIN(ret,NhlWARNING);
		e_text =
		    "%s: overlay Legend can be created only during NhlCreate";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNovDisplayLegend);
		ovp->display_legend = NhlNOCREATE;
	}

/*
 * If the Overlay records resource is set replace the old overlay record
 * pointer array with the new one.
 */
	if (_NhlArgIsSet(args,num_args,NhlNovOverlayRecs)) {

		NhlovRec	**ov_recs = 
					(NhlovRec **) ovp->ov_rec_list->data;
		int	new_count = ovp->ov_rec_list->num_elements;
		
		if (ov_recs == NULL || ! _NhlIsTransform(ov_recs[0]->plot)) {
		     e_text = "%s: internally invalid overlay record resource";
		     NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		     return NhlFATAL;
		}
		if (new_count > ovp->overlay_alloc) {
			ovp->ov_recs = (NhlovRec **)
				NhlRealloc(ovp->ov_recs, sizeof(NhlovRec *) *
					   MAX(new_count,ovp->overlay_alloc + 
					       NhlOV_ALLOC_UNIT));
			if (ovp->ov_recs == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			ovp->overlay_alloc = MAX(new_count,ovp->overlay_alloc +
						 NhlOV_ALLOC_UNIT);
		}
		for (i = 0; i < MIN(new_count,ovp->overlay_count); i++) {
			memcpy((void*)ovp->ov_recs[i],(void*)ov_recs[i],
							sizeof(NhlovRec));
		}
			
		for (i=MIN(new_count,ovp->overlay_count);i<new_count;i++) {
			if ((ovp->ov_recs[i] = (NhlovRec *) 
			     NhlMalloc(sizeof(NhlovRec))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			memcpy((void*)ovp->ov_recs[i],(void*)ov_recs[i],
							sizeof(NhlovRec));
		}
		for (i = new_count; i < ovp->overlay_count; i++) {
			NhlFree(ovp->ov_recs[i]);
			ovp->ov_recs[i] = NULL;
		}
		ovp->overlay_count = new_count;
	}

	subret = ManageAnnotations(ovnew,ovold,False,args,num_args);
	ret = MIN(ret,subret);

	if (ovnew->trans.overlay_status == _tfCurrentOverlayMember ||
	    ovp->overlay_count < 2) {
		ovp->update_req = False;
		return ret;
	}


/*
 * Only a master overlay with member plots needs to execute the remaining code
 */

/*
 * If the base view has changed, modify the view of each overlay to 
 * match it. 
 */
	if (ovnew->view.x != ovold->view.x)
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovnew->view.x);
	if (ovnew->view.y != ovold->view.y)
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovnew->view.y);
	if (ovnew->view.width != ovold->view.width)
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,ovnew->view.width);
	if (ovnew->view.height != ovold->view.height)
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,ovnew->view.height);
/*
 * If the transformation has changed the annotation objects associated 
 * with member overlay plots need to be updated.
 */
	if (ovp->update_req ||
	    (ovp->x_log != oovp->x_log) ||
	    (ovp->y_log != oovp->y_log) ||
	    (ovp->x_min != oovp->x_min) ||
	    (ovp->y_min != oovp->y_min) ||
	    (ovp->x_max != oovp->x_max) ||
	    (ovp->y_max != oovp->y_max) ||
	    (ovp->x_reverse != oovp->x_reverse) ||
	    (ovp->y_reverse != oovp->y_reverse) ||
	    (ovp->x_tension != oovp->x_tension) ||
	    (ovp->y_tension != oovp->y_tension)) {
		NhlSetSArg(&sargs[nargs++],NhlNovUpdateReq,True);
		update_req = True;
	}
/*
 * Ensure that all the overlays are up to date; don't pass the
 * update req resource if the plot has no overlay of its own.
 */
	for (i = 1; i < ovp->overlay_count; i++) {

		NhlOverlayLayer ovl = (NhlOverlayLayer)ovp->ov_recs[i]->ov_obj;

		int num_args = update_req && ovl == NULL ? nargs - 1 : nargs;

		subret = NhlALSetValues(ovp->ov_recs[i]->plot->base.id,
					sargs,num_args);
		if ((ret = MIN(subret, ret)) < NhlWARNING) {
			e_text = "%s: error setting overlay plot view";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	ovp->update_req = False;
		
	return ret;
}

/*
 * Function:	OverlayGetValues
 *
 * Description:	
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 *	Memory is allocated when the following resources are retrieved:
 *		NhlNovOverlayIds
 *	The user is responsible for freeing this memory.
 */

static NhlErrorTypes	OverlayGetValues
#if __STDC__
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	NhlLayer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
	char			*entry_name = "OverlayGetValues";
	char			*e_text;
	NhlOverlayLayerPart	*ovp = &((NhlOverlayLayer) l)->overlay;
	int 			i,j;
	int			*ids;
	NhlGenArray		ga;
	NhlovRec			**ov_recs;

	for ( i = 0; i< num_args; i++ ) {

		if (args[i].quark == Overlay_Ids) {

			if ((ids = (int *) NhlMalloc(ovp->overlay_count * 
						     sizeof(int))) == NULL) {
				
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}

			for (j = 0; j < ovp->overlay_count; j++) {
				ids[j] = ovp->ov_recs[j]->plot->base.id; 
			}
			
			if ((ga = NhlCreateGenArray((NhlPointer)ids,
						    NhlTInteger,sizeof(int),
						    1,&ovp->overlay_count)) 
			    == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNovOverlayIds);
				return NhlFATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
		else if (args[i].quark == Overlay_Recs) {
				
			ov_recs = (NhlovRec **) 
			      NhlMalloc(ovp->overlay_count * 
					sizeof(NhlovRec *));
			if (ov_recs == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}

			for (j = 0; j < ovp->overlay_count; j++) {

				ov_recs[j] = (NhlovRec *)
					NhlMalloc(sizeof(NhlovRec));
				if (ov_recs[j] == NULL) {
					e_text = 
					 "%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}

				memcpy((char*)ov_recs[j],
				       (char*)ovp->ov_recs[j],
				       sizeof(NhlovRec));
			}
			
			ga = NhlCreateGenArray((NhlPointer)ov_recs,
					       NhlTPointer,sizeof(NhlovRec *),
					       1,&ovp->overlay_count);
			if (ga == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNovOverlayRecs);
				return NhlFATAL;
			}

			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
			
	}
	return(NhlNOERROR);
}

/*
 * Function:	OverlayDestroy
 *
 * Description:
 *
 * In Args:	inst		instance record pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes OverlayDestroy
#if __STDC__
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "OverlayDestroy";
	char			*e_text;
	NhlOverlayLayerPart	*ovp = &((NhlOverlayLayer) inst)->overlay;
	int			i;

/*
 * If there are overlay members release them
 */
	if (ovp->overlay_count > 1) {
		subret = DissolveOverlay(inst);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
	}

	if (ovp->overlay_count > 1) {
		e_text = "%s: inconsistency in overlay count";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
	}
/*
 * Free the overlay base record and the overlay record pointer array,
 * including each element's annotation list. External annotations 
 * belonging to the base overlay must be informed that they are no 
 * longer part of an overlay.
 * Also the irregular point gen arrays.
 */
	for (i=0; i < ovp->overlay_count; i++) {
		NhlAnnoRec	*anlp = ovp->ov_recs[i]->anno_list;
		while (anlp != NULL) {
			NhlAnnoRec *free_anno = anlp;
			anlp = anlp->next;
			if (i == 0 && free_anno->type == ovEXTERNAL) {
				NhlVASetValues(free_anno->anno_id,
					       NhlNanOverlayBaseId,-1,NULL);
			}
			NhlFree(free_anno);
		}
		NhlFree(ovp->ov_recs[i]);
	}

	NhlFree(ovp->ov_recs);
	NhlFreeGenArray(ovp->x_irr);
	NhlFreeGenArray(ovp->y_irr);

	return(ret);
}

/*
 * Function:	OverlayPreDraw
 *
 * Description: Performs the "background" draw for an overlay plot
 *
 * In Args:	layer	Overlay instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes OverlayPreDraw
#if  __STDC__
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "OverlayPreDraw";
	NhlOverlayLayer		ovl = (NhlOverlayLayer) layer;
	NhlOverlayLayerPart	*ovp = &(ovl->overlay);
	int			i,j;
	NhlBoolean		first;
	int			max_zone;

/*
 * Update the annotation data 
 */
	for (max_zone = 0, i = 0; i < ovp->overlay_count; i++) {
		subret = UpdateAnnoData(ovp->ov_recs[i]->anno_list,
					&ovp->ov_recs[i]->max_zone,
					entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ovp->ov_recs[i]->max_zone > max_zone) 
			max_zone = ovp->ov_recs[i]->max_zone;
	}
/*
 * Modify the annotation positions based on the current zonal information
 */
	for (first = True, i = 0; i <= max_zone; i++) {
		for (j = 0; j < ovp->overlay_count; j++) {

			subret = SetAnnoViews(ovl,ovp->ov_recs[j]->anno_list,
					      i,first,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
			first = False;
		}
	}

/*
 * Set the overlay trans.
 */

	Overlay_Trans_Obj = ovl->trans.overlay_trans_obj;
	Overlay_Plot = (NhlLayer) ovp->ov_recs[0]->plot;
	if (! _NhlIsTransObj(Overlay_Trans_Obj) || 
	    ! _NhlIsTransform(Overlay_Plot)) {
		e_text = "%s: invalid overlay trans object or plot class";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	subret = _NhlSetTrans(Overlay_Trans_Obj,Overlay_Plot);
        if ((ret = MIN(ret,subret)) < NhlWARNING) {
                e_text = "%s: error setting transformation";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
                return(ret);
        }

/*
 * Call the pre-draw methods for each plot object in turn.
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		Trans_Obj = ovp->ov_recs[i]->plot->trans.trans_obj;
		Plot = (NhlLayer) ovp->ov_recs[i]->plot;

		subret = _NhlPreDraw(Plot);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error in plot pre-draw";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
	}
	return ret;
}


/*
 * Function:	OverlayDraw
 *
 * Description:
 *
 * In Args:	layer	Overlay instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes OverlayDraw
#if  __STDC__
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "OverlayDraw";
	NhlOverlayLayer		ovl = (NhlOverlayLayer) layer;
	NhlOverlayLayerPart	*ovp = &(ovl->overlay);
	int			i;

/*
 * Set the overlay trans.
 */

	Overlay_Trans_Obj = ovl->trans.overlay_trans_obj;
	Overlay_Plot = (NhlLayer) ovp->ov_recs[0]->plot;
	if (! _NhlIsTransObj(Overlay_Trans_Obj) || 
	    ! _NhlIsTransform(Overlay_Plot)) {
		e_text = "%s: invalid overlay trans object or plot class";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	subret = _NhlSetTrans(Overlay_Trans_Obj,Overlay_Plot);
        if ((ret = MIN(ret,subret)) < NhlWARNING) {
                e_text = "%s: error setting transformation";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
                return(ret);
        }

/*
 * Call the draw methods for each plot object in turn. 
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		Trans_Obj = ovp->ov_recs[i]->plot->trans.trans_obj;
		Plot = (NhlLayer) ovp->ov_recs[i]->plot;

		subret = _NhlDraw(Plot);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error in plot draw";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
	}

	return ret;
}


/*
 * Function:	OverlayPostDraw
 *
 * Description:
 *
 * In Args:	layer	Overlay instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes OverlayPostDraw
#if  __STDC__
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "OverlayPostDraw";
	NhlOverlayLayer		ovl = (NhlOverlayLayer) layer;
	NhlOverlayLayerPart	*ovp = &(ovl->overlay);
	int			i;
	NhlBoolean		tickmarks_done = False,
				titles_done = False,
				labelbar_done = False,
				legend_done = False;

/*
 * Set the overlay trans.
 */

	Overlay_Trans_Obj = ovl->trans.overlay_trans_obj;
	Overlay_Plot = (NhlLayer) ovp->ov_recs[0]->plot;
	if (! _NhlIsTransObj(Overlay_Trans_Obj) || 
	    ! _NhlIsTransform(Overlay_Plot)) {
		e_text = "%s: invalid overlay trans object or plot class";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	subret = _NhlSetTrans(Overlay_Trans_Obj,Overlay_Plot);
        if ((ret = MIN(ret,subret)) < NhlWARNING) {
                e_text = "%s: error setting transformation";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
                return(ret);
        }

/*
 * Call the post-draw methods for each plot object in turn. 
 * The annotation items for now are part of the post draw.
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		NhlAnnoRec	*anlp = ovp->ov_recs[i]->anno_list;

		Trans_Obj = ovp->ov_recs[i]->plot->trans.trans_obj;
		Plot = (NhlLayer) ovp->ov_recs[i]->plot;

		subret = _NhlPostDraw(Plot);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error in plot post-draw";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}

		for ( ; anlp != NULL; anlp = anlp->next) {
				
			if (anlp->status == NhlNEVER)
				continue;
			else if (anlp->status == NhlCONDITIONAL) {
				switch (anlp->type) {
				case ovTICKMARK:
					if (tickmarks_done) continue;
					break;
				case ovTITLE:
					if (titles_done) continue;
					break;
				case ovLEGEND:
					if (legend_done) continue;
					break;
				case ovLABELBAR:
					if (labelbar_done) continue;
					break;
				case ovEXTERNAL:
				default:
					break;
				}
			}
			if (anlp->track_data && anlp->out_of_range)
				continue;
			if (anlp->plot_id < 1)
				continue;
			subret = NhlDraw(anlp->plot_id);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				e_text = "%s: error drawing annotation";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name);
				return(ret);
			}
			switch (anlp->type) {
			case ovTICKMARK:
				tickmarks_done = True;
				break;
			case ovTITLE:
				titles_done = True;
				break;
			case ovLEGEND:
				legend_done = True;
				break;
			case ovLABELBAR:
				labelbar_done = True;
				break;
			case ovEXTERNAL:
			default:
				break;
			}
		}
	}
	return ret;
}

/*
 * Function:    OverlayGetBB
 *
 * Description: Makes sure TextItems outside of view resources are included in
 *              the Bounding Box .
 *
 * In Args:     instance        the object instance record
 *              thebox          a data structure used to hold bounding box 
 *                              information.
 *
 * Out Args:    NONE
 *
 * Return Values:       Error Conditions
 *
 * Side Effects:        NONE
 */
static NhlErrorTypes OverlayGetBB
#if	__STDC__
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	char			*entry_name = "OverlayGetBB";
	char			*e_text;
	NhlOverlayLayer		ovl = (NhlOverlayLayer) instance;
	NhlOverlayLayerPart	*ovp = &(ovl->overlay);
	int			max_zone;
	int			i,j;
	NhlBoolean		first;


/*
 * Update the annotation data 
 */
	for (max_zone = 0, i = 0; i < ovp->overlay_count; i++) {
		subret = UpdateAnnoData(ovp->ov_recs[i]->anno_list,
					&ovp->ov_recs[i]->max_zone,
					entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ovp->ov_recs[i]->max_zone > max_zone) 
			max_zone = ovp->ov_recs[i]->max_zone;
	}
/*
 * Modify the annotation positions based on the current zonal information
 */
	for (first = True, i = 0; i <= max_zone; i++) {
		for (j = 0; j < ovp->overlay_count; j++) {

			subret = SetAnnoViews(ovl,ovp->ov_recs[j]->anno_list,
					      i,first,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
			first = False;
		}
	}

	ret = InternalGetBB(instance,thebox,max_zone,entry_name);

	if (ret < NhlWARNING) {
		e_text = "%s: error getting Bounding Box";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
	}

	return ret;
}

/*
 * Function:	RecordAnnotation
 *
 * Description: 
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlAnnoRec *RecordAnnotation
#if  __STDC__
(
	NhlOverlayLayer ovl,
	ovAnnoType	type,
	int		status,
	int		zone
)
#else
(ovl,type,status,zone)
	NhlOverlayLayer	ovl;
	ovAnnoType	type;
	int		status;
	int		zone;
#endif
{
	char			*e_text;
	char			*entry_name = "OverlayInitialize";
	NhlOverlayLayerPart	*ovp = &(ovl->overlay);
	NhlAnnoRec		*anno_rec, *anlp;

	if (status == NhlNOCREATE) {
		e_text = "%s: internal error calling RecordAnnotation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}

	if ((anno_rec = (NhlAnnoRec *)
	     NhlMalloc(sizeof(NhlAnnoRec))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if (zone < 0) {
		e_text = "%s: internally invalid zone number specified";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	anno_rec->ovl = (NhlLayer)ovl;
	anno_rec->anno_id = -1;
	anno_rec->plot_id = -1;
	anno_rec->zone = zone;
	anno_rec->type = type;
	anno_rec->status = status;
	anno_rec->next = NULL;
	anno_rec->track_data = False;
	anno_rec->out_of_range = False;
	anno_rec->resize_notify = False;
	anlp = ovp->ov_recs[0]->anno_list;

	if (anlp == NULL) {
		ovp->ov_recs[0]->anno_list = anno_rec;
		return anno_rec;
	}
	else if (anlp->zone <= anno_rec->zone) {
		anno_rec->next = anlp;
		ovp->ov_recs[0]->anno_list = anno_rec;
		return anno_rec;
	}
	while (anlp->next != NULL) {
		if (anlp->next->zone <= anno_rec->zone) {
			anno_rec->next = anlp->next;
			anlp->next = anno_rec;
			return anno_rec;
		}
		anlp = anlp->next;
	}
	anlp->next = anno_rec;
	return anno_rec;

}

/*
 * Function:	SetAnnoViews
 *
 * Description: 
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes SetAnnoViews
#if  __STDC__
(
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_list,
	int		zone,
	NhlBoolean	first,
	NhlString	entry_name
)
#else
(ovl,anno_list,zone,first,entry_name)
	NhlOverlayLayer	ovl;
	NhlAnnoRec	*anno_list;
	int		zone;
	NhlBoolean	first;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlAnnoRec		*anlp;
	static NhlBoolean	tickmarks_done,titles_done;
	static NhlBoolean	labelbar_done,legend_done;

	if (first)
		tickmarks_done = titles_done = 
			labelbar_done = legend_done = False;

	for (anlp = anno_list; anlp != NULL; anlp = anlp->next) {
		
		if (anlp->zone != zone)
			continue;
		else if (anlp->status == NhlNEVER)
			continue;
		else if (anlp->status == NhlCONDITIONAL) {
			switch (anlp->type) {
			case ovTICKMARK:
				if (tickmarks_done) continue;
				break;
			case ovTITLE:
				if (titles_done) continue;
				break;
			case ovLEGEND:
				if (legend_done) continue;
				break;
			case ovLABELBAR:
				if (labelbar_done) continue;
				break;
			case ovEXTERNAL:
			default:
				break;
			}
		}
		switch (anlp->type) {
		case ovTICKMARK:
			subret = SetTickMarkView(ovl,anlp,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
			tickmarks_done = True;
			break;
		case ovTITLE:
			SetTitleView(ovl,anlp,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
			titles_done = True;
			break;
		case ovLEGEND:
			SetExternalView(ovl,anlp,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
			legend_done = True;
			break;
		case ovLABELBAR:
			SetExternalView(ovl,anlp,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
			labelbar_done = True;
			break;
		case ovEXTERNAL:
			if (anlp->track_data) {
				SetViewTracking(ovl,anlp,entry_name);
				if ((ret = MIN(ret,subret)) < NhlWARNING)
					return ret;
				else if (ret > NhlWARNING) {
					break;
				}
			}
			SetExternalView(ovl,anlp,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
			break;
		default:
			break;
		}
	}
	return ret;

}

/*
 * Function:	SetTickMarkView
 *
 * Description: 
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes SetTickMarkView
#if  __STDC__
(
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
)
#else
(ovl,anno_rec,entry_name)
	NhlOverlayLayer	ovl;
	NhlAnnoRec	*anno_rec;
	NhlString	entry_name;
#endif
{
	return NhlVASetValues(anno_rec->plot_id,NULL);
}

/*
 * Function:	SetTitleView
 *
 * Description: 
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes SetTitleView
#if  __STDC__
(
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
)
#else
(ovl,anno_rec,entry_name)
	NhlOverlayLayer	ovl;
	NhlAnnoRec	*anno_rec;
	NhlString	entry_name;
#endif
{
	
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlBoundingBox		bbox;
	float			x_pos,y_pos,width,height;
	float			x_vp,y_vp,width_vp,height_vp;
	float			get_main_off_x,get_x_off_x,get_y_off_y;
	float			set_main_off_x,set_x_off_x,set_y_off_y;
	NhlPosition		x_axis_pos,y_axis_pos,main_pos;
        NhlSArg			sargs[16];
        int			nargs = 0;
	NhlOverlayLayer		an_ovl;
	int			zone;
	
/*
 * Get relevant title attributes
 */
	subret = NhlVAGetValues(anno_rec->plot_id,
				NhlNtiMainOffsetXF,&get_main_off_x,
				NhlNtiXAxisOffsetXF,&get_x_off_x,
				NhlNtiYAxisOffsetYF,&get_y_off_y,
				NhlNtiMainPosition,&main_pos,
				NhlNtiXAxisPosition,&x_axis_pos,
				NhlNtiYAxisPosition,&y_axis_pos,
				NhlNvpXF,&x_vp,
				NhlNvpYF,&y_vp,
				NhlNvpWidthF,&width_vp,
				NhlNvpHeightF,&height_vp,
				NULL);
	an_ovl = (NhlOverlayLayer) anno_rec->ovl;

	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error getting Title values";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}

/*
 * Get the bounding box, then set the title positions with respect to it.
 */
	bbox.set = 0;
	zone = anno_rec->zone < 2 ? anno_rec->zone : anno_rec->zone - 1; 
	subret = InternalGetBB((NhlLayer)ovl,&bbox,zone,entry_name);
	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error getting bounding box";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}

	x_pos = bbox.l; 
	y_pos = bbox.t;
	width = bbox.r - bbox.l; 
	height = bbox.t - bbox.b;
	
	switch(an_ovl->overlay.ti_main_position) {
	case NhlCENTER:
		set_main_off_x = an_ovl->overlay.ti_main_offset_x +
			((ovl->view.x + ovl->view.width/2.0) -
			 (x_pos + width/2.0));
		break;
	case NhlLEFT:
		set_main_off_x = an_ovl->overlay.ti_main_offset_x +
			(ovl->view.x - x_pos);
		break;
	case NhlRIGHT:
		set_main_off_x = an_ovl->overlay.ti_main_offset_x +
			((ovl->view.x + ovl->view.width) - 
			 (x_pos + width));
		break;
	case NhlTOP:
	case NhlBOTTOM:
	default:
		e_text = "%s: Invalid value for main axis title position";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}

	switch(an_ovl->overlay.ti_x_axis_position) {
	case NhlCENTER:
		set_x_off_x = an_ovl->overlay.ti_x_axis_offset_x +
			((ovl->view.x + ovl->view.width/2.0) -
			 (x_pos + width/2.0));
		break;
	case NhlLEFT:
		set_x_off_x = an_ovl->overlay.ti_x_axis_offset_x +
			(ovl->view.x - x_pos);
		break;
	case NhlRIGHT:
		set_x_off_x = an_ovl->overlay.ti_x_axis_offset_x +
			((ovl->view.x + ovl->view.width) - 
			 (x_pos + width));
		break;
	case NhlTOP:
	case NhlBOTTOM:
	default:
		e_text = "%s: Invalid value for x axis title position";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}

	switch(an_ovl->overlay.ti_y_axis_position) {
	case NhlCENTER:
		set_y_off_y = an_ovl->overlay.ti_y_axis_offset_y +
			((ovl->view.y - ovl->view.height/2.0) -
			 (y_pos - height/2.0));
		break;
	case NhlTOP:
		set_y_off_y = an_ovl->overlay.ti_y_axis_offset_y +
			(ovl->view.y - y_pos);
		break;
	case NhlBOTTOM:
		set_y_off_y = an_ovl->overlay.ti_y_axis_offset_y + 
			((ovl->view.y - ovl->view.height) - 
			 (y_pos - height));
		break;
	case NhlLEFT:
	case NhlRIGHT:
	default:
		e_text = "%s: Invalid value for y axis title position";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

/*
 * Reset the title if necessary
 */	
	if (x_pos != x_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,x_pos);
	if (y_pos != y_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,y_pos);
	if (width != width_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,width);
	if (height != height_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,height);
	
	if (set_main_off_x != get_main_off_x)
	    NhlSetSArg(&sargs[nargs++],NhlNtiMainOffsetXF,set_main_off_x);
	if (set_x_off_x != get_x_off_x)
		NhlSetSArg(&sargs[nargs++],NhlNtiXAxisOffsetXF,set_x_off_x);
	if (set_y_off_y != get_y_off_y)
		NhlSetSArg(&sargs[nargs++],NhlNtiYAxisOffsetYF,set_y_off_y);

	if (an_ovl->overlay.ti_main_position != main_pos)
		NhlSetSArg(&sargs[nargs++],NhlNtiMainPosition,
			   an_ovl->overlay.ti_main_position);
	if (an_ovl->overlay.ti_x_axis_position != x_axis_pos)
		NhlSetSArg(&sargs[nargs++],NhlNtiXAxisPosition,
			   an_ovl->overlay.ti_x_axis_position);
	if (an_ovl->overlay.ti_y_axis_position != y_axis_pos)
		NhlSetSArg(&sargs[nargs++],NhlNtiYAxisPosition,
			   an_ovl->overlay.ti_y_axis_position);

	subret = NhlALSetValues(anno_rec->plot_id,sargs,nargs);
	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error setting title values";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}

	return ret;
}


/*
 * Function:	SetViewTracking
 *
 * Description: 
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes SetViewTracking
#if  __STDC__
(
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
)
#else
(ovl,anno_rec,entry_name)
	NhlOverlayLayer	ovl;
	NhlAnnoRec	*anno_rec;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	float			x_pos,y_pos;
	float			x_vp,y_vp,width_vp,height_vp;
	int			status;
	float			oo_range = 1E12;
	
	if (anno_rec->status == NhlNEVER)
		return ret;

	NhlDataToNDC(ovl->trans.overlay_object->base.parent->base.id,
		     &anno_rec->data_x,&anno_rec->data_y,1,
		     &x_pos,&y_pos,NULL,NULL,&status,&oo_range);
	if (status) {
		NhlLayer anl = _NhlGetLayer(anno_rec->anno_id);
		e_text = 
	     "%s: annotation \"%s\" track point not in range: cannot display";
		ret = MIN(ret,NhlWARNING);
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,anl->base.name);
		anno_rec->out_of_range = True;
		return ret;
	}
	anno_rec->out_of_range = False;
/*
 * Get the viewport of the annotation's plot object
 */
	subret = NhlVAGetValues(anno_rec->plot_id,
				NhlNvpXF,&x_vp,
				NhlNvpYF,&y_vp,
				NhlNvpWidthF,&width_vp,
				NhlNvpHeightF,&height_vp,
				NULL);
	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error getting view values for annotation";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}

/*
 * Adjust the annotation position based on the justification value
 */
	switch (anno_rec->just) {
	case NhlTOPLEFT:
		break;
	case NhlTOPCENTER:
		x_pos = x_pos - width_vp / 2.0;
		break;
	case NhlTOPRIGHT:
		x_pos = x_pos - width_vp;
		break;
	case NhlCENTERLEFT:
		y_pos = y_pos + height_vp / 2.0;
		break;
	case NhlCENTERCENTER:
		x_pos = x_pos - width_vp / 2.0;
		y_pos = y_pos + height_vp / 2.0;
		break;
	case NhlCENTERRIGHT:
		x_pos = x_pos - width_vp;
		y_pos = y_pos + height_vp / 2.0;
		break;
	case NhlBOTTOMLEFT:
		y_pos = y_pos + height_vp;
		break;
	case NhlBOTTOMCENTER:
		x_pos = x_pos - width_vp / 2.0;
		y_pos = y_pos + height_vp;
		break;
	case NhlBOTTOMRIGHT:
		y_pos = y_pos + height_vp;
		x_pos = x_pos - width_vp;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

/*   
 * Reset the viewport of the annotation's plot object if required
 */
	if (x_pos != x_vp || y_pos != y_vp) {
		subret = NhlVASetValues(anno_rec->plot_id,
					NhlNvpXF,x_pos,
					NhlNvpYF,y_pos,
					NULL);
		if ((ret = MIN(subret,ret)) < NhlNOERROR) {
			e_text ="%s: Error setting view values for annotation";
			NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
			if (ret < NhlWARNING) return ret;
		}
	}

	return ret;

}

/*
 * Function:	SetExternalView
 *
 * Description: 
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes SetExternalView
#if  __STDC__
(
	NhlOverlayLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
)
#else
(ovl,anno_rec,entry_name)
	NhlOverlayLayer	ovl;
	NhlAnnoRec	*anno_rec;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlJustification	just;
	NhlBoundingBox		bbox;
	float			x_pos,y_pos,x_start,y_start;
	float			x_vp,y_vp,width_vp,height_vp;
	int			zone;
	float			sign;
	
	if (anno_rec->status == NhlNEVER)
		return ret;

/*
 * Get the viewport of the annotation's plot object
 */
	subret = NhlVAGetValues(anno_rec->plot_id,
				NhlNvpXF,&x_vp,
				NhlNvpYF,&y_vp,
				NhlNvpWidthF,&width_vp,
				NhlNvpHeightF,&height_vp,
				NULL);
	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error getting view values for annotation";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}

/*
 * Get the bounding box for the zone inside the annotation zone, 
 * then calculate the annotation's position with respect to it.
 */
	bbox.set = 0;
	zone = anno_rec->zone < 2 ? anno_rec->zone : anno_rec->zone - 1;
	subret = InternalGetBB((NhlLayer)ovl,&bbox,zone,entry_name);
	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error getting bounding box for annotation";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}

	x_start = anno_rec->zone != 0 ? ovl->view.x :
		ovl->view.x + 0.5 * ovl->view.width; 
	y_start = anno_rec->zone != 0 ? ovl->view.y - ovl->view.height :
		ovl->view.y - 0.5 * ovl->view.height;
	sign = anno_rec->zone == 1 ? 1.0 : - 1.0;

	switch (anno_rec->side) {
	case NhlBOTTOM:
		x_pos = x_start + anno_rec->para_pos * ovl->view.width;
		y_pos = bbox.b + sign * anno_rec->ortho_pos * ovl->view.height;
		break;
	case NhlTOP:
		x_pos = x_start + anno_rec->para_pos * ovl->view.width;
		y_pos = bbox.t - sign * anno_rec->ortho_pos * ovl->view.height;
		break;
	case NhlLEFT:
		x_pos = bbox.l + sign * anno_rec->ortho_pos * ovl->view.width;
		y_pos = y_start + anno_rec->para_pos * ovl->view.height;
		break;
	case NhlRIGHT:
		x_pos = bbox.r - sign * anno_rec->ortho_pos * ovl->view.width;
		y_pos = y_start + anno_rec->para_pos * ovl->view.height;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlTOPRIGHT) {
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->zone > 0)
		just = ConstrainJustification(anno_rec);
	else
		just = anno_rec->just;
/*
 * Adjust the annotation position based on the justification value
 */
	switch (just) {
	case NhlTOPLEFT:
		break;
	case NhlTOPCENTER:
		x_pos = x_pos - width_vp / 2.0;
		break;
	case NhlTOPRIGHT:
		x_pos = x_pos - width_vp;
		break;
	case NhlCENTERLEFT:
		y_pos = y_pos + height_vp / 2.0;
		break;
	case NhlCENTERCENTER:
		x_pos = x_pos - width_vp / 2.0;
		y_pos = y_pos + height_vp / 2.0;
		break;
	case NhlCENTERRIGHT:
		x_pos = x_pos - width_vp;
		y_pos = y_pos + height_vp / 2.0;
		break;
	case NhlBOTTOMLEFT:
		y_pos = y_pos + height_vp;
		break;
	case NhlBOTTOMCENTER:
		x_pos = x_pos - width_vp / 2.0;
		y_pos = y_pos + height_vp;
		break;
	case NhlBOTTOMRIGHT:
		y_pos = y_pos + height_vp;
		x_pos = x_pos - width_vp;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

/*
 * Reset the viewport of the annotation's plot object if required
 */
	if (x_pos != x_vp || y_pos != y_vp) {
		subret = NhlVASetValues(anno_rec->plot_id,
					NhlNvpXF,x_pos,
					NhlNvpYF,y_pos,
					NULL);
		if ((ret = MIN(subret,ret)) < NhlNOERROR) {
			e_text ="%s: Error setting view values for annotation";
			NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
			if (ret < NhlWARNING) return ret;
		}
	}

	return ret;

}

/*
 * Function:    InternalGetBB
 *
 * Description: 
 *
 * In Args:     instance        the object instance record
 *              thebox          a data structure used to hold bounding box 
 *                              information.
 *		zone		the zone for which the bounding box is
 *				to be calculated.
 *
 * Out Args:    NONE
 *
 * Return Values:       Error Conditions
 *
 * Side Effects:        NONE
 */
static NhlErrorTypes InternalGetBB
#if	__STDC__
(
	NhlLayer	instance,
	NhlBoundingBox	*thebox,
	int		zone,
	char		*entry_name
)
#else
(instance,thebox,zone,entry_name)
	NhlLayer	instance;
	NhlBoundingBox	*thebox;
	int		zone;
	char		*entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlOverlayLayer		ovl = (NhlOverlayLayer) instance;
	NhlOverlayLayerPart	*ovl_basep;
	float 			t,b,l,r;
	int			i;
	NhlBoolean		tickmarks_done = False,
				titles_done = False,
				labelbar_done = False,
				legend_done = False;
/* 
 * The view of all members of the overlay is the same. 
 * Start with the view of the current overlay. 
 * Less than 0 returns a point in the center of the viewport. Zone 0 
 * returns the overlay view.
 */
	if (zone == 0) {
		t = b = ovl->view.y - ovl->view.height / 2.0;
		l = r = ovl->view.x + ovl->view.width / 2.0;
	}
	else {
		t = ovl->view.y;
		b = t - ovl->view.height;
		l = ovl->view.x;
		r = l + ovl->view.width;
	}
	
	_NhlAddBBInfo(t,b,r,l,thebox);

	if (zone < 2) 
		return NhlNOERROR;

/*
 * First find the master overlay object, then search through the annotation
 * record of each member overlay and add the bounding boxes of each annotation
 * inside the requested zone. Exclude any objects not currently displayed.
 */
	ovl_basep = &((NhlOverlayLayer)ovl->trans.overlay_object)->overlay;

	for (i = 0; i < ovl_basep->overlay_count; i++) {
		NhlAnnoRec *anno_list = ovl_basep->ov_recs[i]->anno_list;

		for ( ; anno_list != NULL; anno_list = anno_list->next) {
			if (anno_list->zone > zone || anno_list->zone < 2)
				continue;
			else if (anno_list->status == NhlNEVER)
				continue;
			else if (anno_list->plot_id < 1)
				continue;
			else if (anno_list->status == NhlCONDITIONAL) {
				switch (anno_list->type) {
				case ovTICKMARK:
					if (tickmarks_done) continue;
					break;
				case ovTITLE:
					if (titles_done) continue;
					break;
				case ovLEGEND:
					if (legend_done) continue;
					break;
				case ovLABELBAR:
					if (labelbar_done) continue;
					break;
				case ovEXTERNAL:
				default:
					break;
				}
			}
			subret = _NhlGetBB(_NhlGetLayer(anno_list->plot_id),
					   thebox);
			if ((ret = MIN(subret,ret)) < NhlNOERROR) {
				e_text = 
				"%s: Error getting annotation bounding box";
				NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
				if (ret < NhlWARNING) return ret;
			}
			switch (anno_list->type) {
			case ovTICKMARK:
				tickmarks_done = True;
				break;
			case ovTITLE:
				titles_done = True;
				break;
			case ovLEGEND:
				legend_done = True;
				break;
			case ovLABELBAR:
				labelbar_done = True;
				break;
			case ovEXTERNAL:
			default:
				break;
			}
		}
	}
	return ret;
}

/*
 * Function:	ManageAnnotations
 *
 * Description: Manages annotations that belong to the overlay base only.
 *
 * In Args:	NhlOverlayLayer	ovnew - The new overlay layer
 *		NhlOverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageAnnotations
#if __STDC__
(
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
)
#else
(ovnew,ovold,init,args,num_args)
	NhlOverlayLayer	ovnew;
	NhlOverlayLayer	ovold;
	NhlBoolean	init;				       
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString		e_text;
	NhlString		entry_name;
	NhlOverlayLayerPart	*ovp = &ovnew->overlay;
	NhlAnnoRec		*anlp = ovp->ov_recs[0]->anno_list;

	entry_name = init ? "OverlayInitialize" : "OverlaySetValues";
	while (anlp != NULL) {
		if (anlp->status > NhlNOCREATE) {
			switch (anlp->type) {
			case ovTICKMARK:
				subret = ManageTickMarks(ovnew,ovold,
							 init,anlp);
				if ((ret = MIN(subret,ret)) < NhlWARNING)
					return ret;
				break;
			case ovTITLE:
				subret = ManageTitles(ovnew,ovold,init,anlp);
				if ((ret = MIN(subret,ret)) < NhlWARNING)
					return ret;
				break;
			case ovLEGEND:
				subret = ManageLegend(ovnew,ovold,init,anlp,
						      args,num_args);
				if ((ret = MIN(subret,ret)) < NhlWARNING)
					return ret;
				break;
			case ovLABELBAR:
				subret = ManageLabelBar(ovnew,ovold,init,anlp,
							args,num_args);
				if ((ret = MIN(subret,ret)) < NhlWARNING)
					return ret;
				break;
			case ovEXTERNAL:
				subret = ManageExtAnnotation(ovnew,ovold,
							     init,anlp);
				if ((ret = MIN(subret,ret)) < NhlWARNING)
					return ret;
				break;
			default:
				e_text = "%s: internal enumeration error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text, entry_name);
				return(ret);
			}
		}
		anlp = anlp->next;
	}
	return MIN(subret,ret);
		
}

/*
 * Function:	ConstrainJustification
 *
 * Description: Constrain justification depending on the annotation side;
 *
 * In Args:	NhlAnnoRec	anno_rec - the annotation record
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlJustification
ConstrainJustification
#if __STDC__
(
	NhlAnnoRec	*anno_rec
)
#else
(anno_rec)
	NhlAnnoRec	*anno_rec;
#endif
{
	switch (anno_rec->side) {
	case NhlBOTTOM:
		switch (anno_rec->just) {
		case NhlTOPLEFT:
		case NhlCENTERLEFT:
		case NhlBOTTOMLEFT:
			return NhlTOPLEFT;
		case NhlTOPCENTER:
		case NhlCENTERCENTER:
		case NhlBOTTOMCENTER:
			return NhlTOPCENTER;
		case NhlTOPRIGHT:
		case NhlCENTERRIGHT:
		case NhlBOTTOMRIGHT:
			return NhlTOPRIGHT;
		}
	case NhlTOP:
		switch (anno_rec->just) {
		case NhlTOPLEFT:
		case NhlCENTERLEFT:
		case NhlBOTTOMLEFT:
			return NhlBOTTOMLEFT;
		case NhlTOPCENTER:
		case NhlCENTERCENTER:
		case NhlBOTTOMCENTER:
			return NhlBOTTOMCENTER;
		case NhlTOPRIGHT:
		case NhlCENTERRIGHT:
		case NhlBOTTOMRIGHT:
			return NhlBOTTOMRIGHT;
		}
	case NhlLEFT:
		switch (anno_rec->just) {
		case NhlTOPLEFT:
		case NhlTOPCENTER:
		case NhlTOPRIGHT:
			return NhlTOPRIGHT;
		case NhlCENTERLEFT:
		case NhlCENTERCENTER:
		case NhlCENTERRIGHT:
			return NhlCENTERRIGHT;
		case NhlBOTTOMLEFT:
		case NhlBOTTOMCENTER:
		case NhlBOTTOMRIGHT:
			return NhlBOTTOMRIGHT;
		}
	case NhlRIGHT:
		switch (anno_rec->just) {
		case NhlTOPLEFT:
		case NhlTOPCENTER:
		case NhlTOPRIGHT:
			return NhlTOPLEFT;
		case NhlCENTERLEFT:
		case NhlCENTERCENTER:
		case NhlCENTERRIGHT:
			return NhlCENTERLEFT;
		case NhlBOTTOMLEFT:
		case NhlBOTTOMCENTER:
		case NhlBOTTOMRIGHT:
			return NhlBOTTOMLEFT;
		}
	case NhlBOTH:
	case NhlCENTER:
	default:
		break;
	}
	return (NhlJustification) NhlFATAL;

}
/*
 * Function:	ManageExtAnnotation
 *
 * Description: Internal function that manages external annotation objects
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	NhlOverlayLayer	ovnew - The new overlay layer
 *		NhlOverlayLayer	ovold - The old overlay layer
 *		NhlAnnoRec	anno_rec - the annotation record
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageExtAnnotation
#if __STDC__
(
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
)
#else
(ovnew,ovold,init,anno_rec)
	NhlOverlayLayer	ovnew;
	NhlOverlayLayer	ovold;
	NhlBoolean	init;				       
	NhlAnnoRec	*anno_rec;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlJustification	just;
	NhlBoundingBox		bbox;
	float			x_pos,y_pos,width,height;
	float			x_vp,y_vp,width_vp,height_vp;
        NhlSArg			sargs[4];
        int			nargs = 0;
	int			zone;
	float			sign;

	entry_name = (init) ? "OverlayInitialize" : "OverlaySetValues";

	if (anno_rec->status < NhlALWAYS)
		return ret;
/*
 * Get the viewport of the annotation's plot object
 */
	subret = NhlVAGetValues(anno_rec->plot_id,
				NhlNvpXF,&x_vp,
				NhlNvpYF,&y_vp,
				NhlNvpWidthF,&width_vp,
				NhlNvpHeightF,&height_vp,
				NULL);
	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error getting annotation view values";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}
	
	if (anno_rec->resize_notify &&
	    ((ovnew->view.width != ovold->view.width) ||
	    (ovnew->view.height != ovold->view.height))) {
		width = width_vp * ovnew->view.width / ovold->view.width;
		height = height_vp * ovnew->view.height / ovold->view.height;
	}
	else {
		height = height_vp;
		width = width_vp;
	}

/*
 * Get the bounding box for the zone inside the annotation zone, 
 * then calculate the annotation's position with respect to it.
 */
	bbox.set = 0;
	zone = anno_rec->zone < 2 ? anno_rec->zone : anno_rec->zone - 1;
	subret = InternalGetBB((NhlLayer)ovnew,&bbox,zone,entry_name);
	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error getting annotation bounding box";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}
	sign = anno_rec->zone == 1 ? 1.0 : -1.0;
	switch (anno_rec->side) {
	case NhlBOTTOM:
		x_pos = ovnew->view.x + 
			anno_rec->para_pos * ovnew->view.width;
		y_pos = bbox.b +
			sign * anno_rec->ortho_pos * ovnew->view.height;
		break;
	case NhlTOP:
		x_pos = ovnew->view.x + 
			anno_rec->para_pos * ovnew->view.width;
		y_pos = bbox.t -
			sign * anno_rec->ortho_pos * ovnew->view.height;
		break;
	case NhlLEFT:
		x_pos = bbox.l + 
			sign * anno_rec->ortho_pos * ovnew->view.width;
		y_pos = ovnew->view.y - ovnew->view.height +
			anno_rec->para_pos * ovnew->view.height;
		break;
	case NhlRIGHT:
		x_pos = bbox.r - 
			sign * anno_rec->ortho_pos * ovnew->view.width;
		y_pos = ovnew->view.y - ovnew->view.height +
			anno_rec->para_pos * ovnew->view.height;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlTOPRIGHT) {
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->zone > 0)
		just = ConstrainJustification(anno_rec);

/*
 * Adjust the annotation position based on the justification value
 */
	switch (just) {
	case NhlTOPLEFT:
		break;
	case NhlTOPCENTER:
		x_pos = x_pos - width_vp / 2.0;
		break;
	case NhlTOPRIGHT:
		x_pos = x_pos - width_vp;
		break;
	case NhlCENTERLEFT:
		y_pos = y_pos + height_vp / 2.0;
		break;
	case NhlCENTERCENTER:
		x_pos = x_pos - width_vp / 2.0;
		y_pos = y_pos + height_vp / 2.0;
		break;
	case NhlCENTERRIGHT:
		x_pos = x_pos - width_vp;
		y_pos = y_pos + height_vp / 2.0;
		break;
	case NhlBOTTOMLEFT:
		y_pos = y_pos + height_vp;
		break;
	case NhlBOTTOMCENTER:
		x_pos = x_pos - width_vp / 2.0;
		y_pos = y_pos + height_vp;
		break;
	case NhlBOTTOMRIGHT:
		y_pos = y_pos + height_vp;
		x_pos = x_pos - width_vp;
		break;
	}

/*
 * Reset the viewport of the annotation's plot object if required
 */
	if (x_pos != x_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,x_pos);
	if (y_pos != y_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,y_pos);
	if (width != width_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,width);
	if (height != height_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,height);

	subret = NhlALSetValues(anno_rec->plot_id,sargs,nargs);
	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error setting annotation values";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}

	return ret;
}

/*
 * Function:	UpdateAnnoData
 *
 * Description: Traverses the annotation list and does a GetValues on
 *		each external annotation in order to ensure that its
 *		values are current. Also determines the maximum zone 
 *		specified in the annotation list.
 *
 * In Args:	NhlOverlayLayer	ovnew - The new overlay layer
 *		NhlOverlayLayer	ovold - The old overlay layer
 *		NhlAnnoRec	anno_rec - the annotation record
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
UpdateAnnoData
#if __STDC__
(
	NhlAnnoRec	*anno_list,
	int		*max_zone,
	NhlString	entry_name
)
#else
(anno_list,max_zone,entry_name)
	NhlAnnoRec	*anno_list;
	int		*max_zone;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	NhlAnnoRec		*anlp;
	NhlBoolean		on;

	*max_zone	= 0;
	for (anlp = anno_list; anlp != NULL; anlp = anlp->next) {
		if (anlp->type == ovEXTERNAL) {
			subret = NhlVAGetValues(
				anlp->anno_id,
				NhlNanOn,&on,
				NhlNanPlotId,&anlp->plot_id,
				NhlNanResizeNotify,&anlp->resize_notify,
				NhlNanZone,&anlp->zone,
				NhlNanSide,&anlp->side,
				NhlNanJust,&anlp->just,
				NhlNanOrthogonalPosF,&anlp->ortho_pos,
				NhlNanParallelPosF,&anlp->para_pos,
				NhlNanTrackData,&anlp->track_data,
				NhlNanDataXF,&anlp->data_x,
				NhlNanDataYF,&anlp->data_y,
				NULL);
			if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
			anlp->status = on ? NhlALWAYS : NhlNEVER;
		}
		if (anlp->zone > *max_zone)
			*max_zone = anlp->zone;
	}

	return ret;
}

/*
 * Function:	ManageTickMarks
 *
 * Description: Internal function that manages the tickmark object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	NhlOverlayLayer	ovnew - The new overlay layer
 *		NhlOverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageTickMarks
#if __STDC__
(
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
)
#else
(ovnew,ovold,init,anno_rec)
	NhlOverlayLayer	ovnew;
	NhlOverlayLayer	ovold;
	NhlBoolean	init;				       
	NhlAnnoRec	*anno_rec;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlOverlayLayerPart	*ovp = &ovnew->overlay;
	NhlOverlayLayerPart	*oovp = &ovold->overlay;
	NhlTransformLayerPart	*tfp = &ovnew->trans;
	int			tmpid = -1;
	char			buffer[_NhlMAXFNAMELEN];
        NhlSArg			sargs[16];
        int			nargs = 0;
	NhlLayer			trobj;
	NhlString		trobj_name;
	float			x_irr[NhlOV_IRR_COUNT], y_irr[NhlOV_IRR_COUNT];
	float			*x_irrp, *y_irrp;
	NhlBoolean		set = False;
	int			i, count = 2, status = 0;
	float			*xmiss = NULL, *ymiss = NULL; 
	float			out_of_range;
	NhlTickMarkStyles	tm_style = NhlLINEAR;
	NhlBoolean		x_log,y_log;
	float			x_tension,y_tension;

	entry_name = (init) ? "OverlayInitialize" : "OverlaySetValues";

/*
 * Update the annotation record (tickmarks do not use all the fields).
 */
	anno_rec->status = ovp->display_tickmarks;

/*
 * If not displaying tickmarks just call the SetValues function --
 * this call is only to avoid the no set values called error.
 */
	if (! init) {
		if (ovp->tickmarks == NULL) {
			e_text = "%s: internal error: Tickmark layer NULL";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
		if (ovp->display_tickmarks == NhlNEVER) {
			return _NhlALSetValuesChild(ovp->tickmarks->base.id,
						    (NhlLayer)ovnew,
						    sargs,nargs);
		}
	}
	if ((trobj = tfp->overlay_trans_obj) == NULL) {
		e_text = "%s: No overlay trans found for TickMark object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}
	
	if (ovp->x_irr != NULL) {
		x_irrp = (float *) ovp->x_irr->data;
	}
	else {
		count = NhlOV_IRR_COUNT;
		x_irrp = (float *) NhlMalloc(count * sizeof(float));
		if (x_irrp == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		memset((void*)x_irrp,0,count * sizeof(float));
		ovp->x_irr = NhlCreateGenArray((NhlPointer)x_irrp,
					       NhlTFloat,sizeof(float),
					       1,&count);
		if (ovp->x_irr == NULL) {
			e_text = "%s: error creating %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtmXBIrregularPoints);
			return NhlFATAL;
		}
		ovp->x_irr->my_data  = True;
		
	}
	if (ovp->y_irr != NULL) {
		y_irrp = (float *) ovp->y_irr->data;
	}
	else {
		count = NhlOV_IRR_COUNT;
		y_irrp = (float *) NhlMalloc(count * sizeof(float));
		if (y_irrp == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		memset((void*)y_irrp,0,count * sizeof(float));
		ovp->y_irr = NhlCreateGenArray((NhlPointer)y_irrp,
					       NhlTFloat,sizeof(float),
					       1,&count);
		if (ovp->y_irr == NULL) {
			e_text = "%s: error creating %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNtmXBIrregularPoints);
			return NhlFATAL;
		}
		ovp->y_irr->my_data  = True;
	}

	if (tfp->overlay_status == _tfCurrentOverlayMember) {
		subret = NhlVAGetValues(tfp->overlay_object->base.id,
				      NhlNtrXLog,&x_log,
				      NhlNtrYLog,&y_log,
				      NhlNtrXTensionF, &x_tension,
				      NhlNtrYTensionF, &y_tension,
				      NULL);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error getting trans object values";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	else {
		x_log = ovp->x_log;
		y_log = ovp->y_log;
		x_tension = ovp->x_tension;
		y_tension = ovp->y_tension;
	}
	
	trobj_name = (trobj->base.layer_class)->base_class.class_name;

	if (! strcmp(trobj_name,"LogLinTransObj")) {
		ovp->x_tm_style = (x_log == 1) ? NhlLOG : NhlLINEAR;
		ovp->y_tm_style = (y_log == 1) ? NhlLOG : NhlLINEAR;
	}
	else if (! strcmp(trobj_name,"IrregularTransObj")) {
		ovp->x_tm_style = NhlLINEAR;
		ovp->y_tm_style = NhlLINEAR;
	}
	else if (! strcmp(trobj_name,"MapTransObj")) {
		e_text = 
	"%s: MAP tick mark style not yet implemented; turning tick marks off";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		if (init) {
			anno_rec->status = 
				ovp->display_tickmarks = NhlNOCREATE;
			return ret;
		}
		else {
			anno_rec->status = 
				ovp->display_tickmarks =  NhlNEVER;
			subret = _NhlALSetValuesChild(ovp->tickmarks->base.id,
						      (NhlLayer)ovnew,
						      sargs,nargs);
			return MIN(subret,ret);
		}
	}
	else if (! strcmp(trobj_name,"IrregularType2TransObj")) {
		tm_style = NhlIRREGULAR;
		ovp->x_tm_style = NhlIRREGULAR;
		ovp->y_tm_style = NhlIRREGULAR;
		ovp->x_tension = x_tension;
		ovp->y_tension = y_tension;
	}
	else {
		e_text = 
			"%s: unknown transformation; turning tick marks off";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		if (init) {
			anno_rec->status = 
				ovp->display_tickmarks = NhlNOCREATE;
			return ret;
		}
		else {
			anno_rec->status = 
				ovp->display_tickmarks =  NhlNEVER;
			subret = _NhlALSetValuesChild(ovp->tickmarks->base.id,
						      (NhlLayer)ovnew,
						      sargs,nargs);
			return MIN(subret,ret);
		}
	}

	if (tm_style == NhlLINEAR) {
		count = 2;
		x_irr[0] = ovnew->view.x;
		y_irr[0] = ovnew->view.y - ovnew->view.height;
		x_irr[1] = ovnew->view.x + ovnew->view.width;
		y_irr[1] = ovnew->view.y;
	}
	else if (tm_style == NhlIRREGULAR) {
		count = NhlOV_IRR_COUNT;
		for (i=0; i < count; i++) {
			x_irr[i] = ovnew->view.x + ovnew->view.width * 
				(float) i / (count - 1.0);
			y_irr[i] = ovnew->view.y - ovnew->view.height +
				ovnew->view.height * (float) i / (count - 1.0);
		}
	}

	subret = NhlNDCToData(ovp->ov_recs[0]->plot->base.id,
			      x_irr,y_irr,count,x_irr,y_irr,
			      xmiss,ymiss,&status,&out_of_range);
	ret = MIN(ret,subret);
	if (status  || ret < NhlWARNING) {
		e_text = 
		   "%s: can't transform NDC to Data; turning tick marks off";
		ret = MIN(ret,NhlWARNING);
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		if (init) {
			anno_rec->status = 
				ovp->display_tickmarks = NhlNOCREATE;
			return ret;
		}
		else {
			anno_rec->status = 
				ovp->display_tickmarks =  NhlNEVER;
			subret = _NhlALSetValuesChild(ovp->tickmarks->base.id,
						      (NhlLayer)ovnew,
						      sargs,nargs);
			return MIN(subret,ret);
		}
	}

	for (i=0; i < count; i++)
		if (x_irr[i] != x_irrp[i] || y_irr[i] != y_irrp[i]) {
			set = True;
			break;
		}

	memcpy((void*)x_irrp,(void*)x_irr,count*sizeof(float));
	memcpy((void*)y_irrp,(void*)y_irr,count*sizeof(float));

	if (set) {
		NhlSetSArg(&sargs[nargs++],NhlNtmXBDataLeftF,x_irr[0]);
		NhlSetSArg(&sargs[nargs++],NhlNtmXBDataRightF,x_irr[count-1]);
		NhlSetSArg(&sargs[nargs++],NhlNtmYLDataBottomF,y_irr[0]);
		NhlSetSArg(&sargs[nargs++],NhlNtmYLDataTopF,y_irr[count-1]);
		NhlSetSArg(&sargs[nargs++],NhlNtmXBIrregularPoints,ovp->x_irr);
		NhlSetSArg(&sargs[nargs++],NhlNtmYLIrregularPoints,ovp->y_irr);
	}
		 
		 
/*
 * If no tickmark object exists, create it; otherwise just set the relevant
 * resources.
 */
	if (ovp->tickmarks == NULL) {	
		NhlSetSArg(&sargs[nargs++],
			   NhlNvpUseSegments,ovnew->view.use_segments);
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovnew->view.x);
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovnew->view.y);
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,ovnew->view.width);
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,ovnew->view.height);
		NhlSetSArg(&sargs[nargs++],NhlNtmYLStyle,ovp->y_tm_style);
		NhlSetSArg(&sargs[nargs++],NhlNtmXBStyle,ovp->x_tm_style);
		NhlSetSArg(&sargs[nargs++],NhlNtmXBIrrTensionF,ovp->x_tension);
		NhlSetSArg(&sargs[nargs++],NhlNtmYLIrrTensionF,ovp->y_tension);

		strcpy(buffer,ovnew->base.name);
		strcat(buffer,".TickMark");
		subret = _NhlALCreateChild(&tmpid,buffer,NhltickMarkLayerClass,
					   (NhlLayer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < NhlWARNING || 
		    (ovp->tickmarks = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating TickMark object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			anno_rec->status = 
				ovp->display_tickmarks = init ? 
					NhlNOCREATE : NhlNEVER;
			return(NhlFATAL);
		}
		anno_rec->plot_id = tmpid;
	} else {
		if (ovnew->view.x != ovold->view.x)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpXF,ovnew->view.x);
		if (ovnew->view.y != ovold->view.y)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpYF,ovnew->view.y);
		if (ovnew->view.width != ovold->view.width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,ovnew->view.width);
		if (ovnew->view.height != ovold->view.height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,ovnew->view.height);
		if (ovp->x_tm_style != oovp->x_tm_style) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXBStyle,ovp->x_tm_style);
		}
		if (ovp->y_tm_style != oovp->y_tm_style) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYLStyle,ovp->y_tm_style);
		}
		if (ovp->x_tension != oovp->x_tension)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXBIrrTensionF,ovp->x_tension);
		if (ovp->y_tension != oovp->y_tension)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYLIrrTensionF,ovp->y_tension);
		

		subret = _NhlALSetValuesChild(ovp->tickmarks->base.id,
					      (NhlLayer)ovnew,sargs,nargs);

		if ((ret = MIN(subret,ret)) < NhlNOERROR) {
			e_text = "%s: Error setting TickMark values";
			NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
			if (ret < NhlWARNING) {
				anno_rec->status = 
					ovp->display_tickmarks = init ? 
						NhlNOCREATE : NhlNEVER;
				return(NhlFATAL);
			}
		}
	}

	return ret;
}

/*
 * Function:	ManageTitles
 *
 * Description: Internal function that manages the title object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	NhlOverlayLayer	ovnew - The new overlay layer
 *		NhlOverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageTitles
#if __STDC__
(
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,				       
	NhlAnnoRec	*anno_rec
)
#else
(ovnew,ovold,init,anno_rec)
	NhlOverlayLayer	ovnew;
	NhlOverlayLayer	ovold;
	NhlBoolean	init;				       
	NhlAnnoRec	*anno_rec;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlOverlayLayerPart	*ovp = &ovnew->overlay;
	NhlOverlayLayerPart	*oovp = &ovold->overlay;
	int			tmpid = -1;
	NhlBoundingBox		bbox;
	char			buffer[_NhlMAXFNAMELEN];
        NhlSArg			sargs[16];
        int			nargs = 0;
	float			x_vp,y_vp,width_vp,height_vp;
	int			zone;

	entry_name = (init) ? "OverlayInitialize" : "OverlaySetValues";

/*
 * Update the annotation record (titles do not use all the fields).
 */
	anno_rec->status = ovp->display_titles;

/*
 * If not displaying titles just call the SetValues function --
 * only to avoid the no set values called error.
 */
	if (! init) {
		if (ovp->titles == NULL) {
			e_text = "%s: internal error: Title layer NULL";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
		if (ovp->display_titles == NhlNEVER) {
			return _NhlALSetValuesChild(ovp->titles->base.id,
						    (NhlLayer)ovnew,
						    sargs,nargs);
		}
	}
/*
 * Adjust the font height based on the ratio of the set view to the
 * standard view at initialization only, because the title object takes
 * care of this at SetValues time
 */

	if (init) {
		if (! ovp->ti_main_font_height_set) 
			ovp->ti_main_font_height = NhlOV_DEF_TITLE_HEIGHT *
				ovnew->view.width / NhlOV_STD_VIEW_WIDTH;
		if (! ovp->ti_x_axis_font_height_set)
			ovp->ti_x_axis_font_height = NhlOV_DEF_TITLE_HEIGHT *
				ovnew->view.width / NhlOV_STD_VIEW_WIDTH;
		if (! ovp->ti_y_axis_font_height_set)
			ovp->ti_y_axis_font_height = NhlOV_DEF_TITLE_HEIGHT *
				ovnew->view.height / NhlOV_STD_VIEW_HEIGHT;
	}

/*
 * Get the bounding box, then set the title positions with respect to it.
 */
	bbox.set = 0;
	zone = anno_rec->zone < 2 ? anno_rec->zone : anno_rec->zone - 1;
	subret = InternalGetBB((NhlLayer)ovnew,&bbox,zone,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: Error getting bounding box";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}
	ovp->ti_x = bbox.l; 
	ovp->ti_y = bbox.t;
	ovp->ti_width = bbox.r - bbox.l; 
	ovp->ti_height = bbox.t - bbox.b;
	
	switch(ovp->ti_main_position) {
	case NhlCENTER:
		ovp->real_main_offset_x = ovp->ti_main_offset_x +
			((ovnew->view.x + ovnew->view.width/2.0) -
			 (ovp->ti_x + ovp->ti_width/2.0));
		break;
	case NhlLEFT:
		ovp->real_main_offset_x = ovp->ti_main_offset_x +
			(ovnew->view.x - ovp->ti_x);
		break;
	case NhlRIGHT:
		ovp->real_main_offset_x = ovp->ti_main_offset_x +
			((ovnew->view.x + ovnew->view.width) - 
			 (ovp->ti_x + ovp->ti_width));
		break;
	case NhlTOP:
	case NhlBOTTOM:
	default:
		e_text = "%s: Invalid value for main axis title position";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}

	switch(ovp->ti_x_axis_position) {
	case NhlCENTER:
		ovp->real_x_axis_offset_x = ovp->ti_x_axis_offset_x +
			((ovnew->view.x + ovnew->view.width/2.0) -
			 (ovp->ti_x + ovp->ti_width/2.0));
		break;
	case NhlLEFT:
		ovp->real_x_axis_offset_x = ovp->ti_x_axis_offset_x +
			(ovnew->view.x - ovp->ti_x);
		break;
	case NhlRIGHT:
		ovp->real_x_axis_offset_x = ovp->ti_x_axis_offset_x +
			((ovnew->view.x + ovnew->view.width) - 
			 (ovp->ti_x + ovp->ti_width));
		break;
	case NhlTOP:
	case NhlBOTTOM:
	default:
		e_text = "%s: Invalid value for x axis title position";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}

	switch(ovp->ti_y_axis_position) {
	case NhlCENTER:
		ovp->real_y_axis_offset_y = ovp->ti_y_axis_offset_y +
			((ovnew->view.y - ovnew->view.height/2.0) -
			 (ovp->ti_y - ovp->ti_height/2.0));
		break;
	case NhlTOP:
		ovp->real_y_axis_offset_y = ovp->ti_y_axis_offset_y +
			(ovnew->view.y - ovp->ti_y);
		break;
	case NhlBOTTOM:
		ovp->real_y_axis_offset_y = ovp->ti_y_axis_offset_y + 
			((ovnew->view.y - ovnew->view.height) - 
			 (ovp->ti_y - ovp->ti_height));
		break;
	case NhlLEFT:
	case NhlRIGHT:
	default:
		e_text = "%s: Invalid value for y axis title position";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

/*
 * If no title object exists, create it; otherwise just set the relevant
 * resources. 
 */
	if (ovp->titles == NULL) {	
		strcpy(buffer,ovnew->base.name);
		strcat(buffer,".Title");

		NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovp->ti_x);
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovp->ti_y);
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,ovp->ti_width);
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,ovp->ti_height);

 		NhlSetSArg(&sargs[nargs++],
			   NhlNtiMainFontHeightF,ovp->ti_main_font_height);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiXAxisFontHeightF,ovp->ti_x_axis_font_height);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiYAxisFontHeightF,ovp->ti_y_axis_font_height);
		NhlSetSArg(&sargs[nargs++],
			   NhlNvpUseSegments,ovnew->view.use_segments);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiMainOffsetXF,ovp->real_main_offset_x);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiXAxisOffsetXF,ovp->real_x_axis_offset_x);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiYAxisOffsetYF,ovp->real_y_axis_offset_y);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiXAxisPosition,ovp->ti_x_axis_position);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiYAxisPosition,ovp->ti_y_axis_position);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiMainPosition,ovp->ti_main_position);

		subret = _NhlALCreateChild(&tmpid,buffer,NhltitleLayerClass,
					   (NhlLayer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < NhlWARNING || 
		    (ovp->titles = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating Title object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
		anno_rec->plot_id = tmpid;
	} else {
		
		x_vp = ((NhlViewLayer) ovp->titles)->view.x;
		y_vp = ((NhlViewLayer) ovp->titles)->view.y;
		width_vp = ((NhlViewLayer) ovp->titles)->view.width;
		height_vp = ((NhlViewLayer) ovp->titles)->view.height;
		if (ovp->ti_x != oovp->ti_x)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpXF,ovp->ti_x);
		if (ovp->ti_y != oovp->ti_y)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpYF,ovp->ti_y);
		if (ovp->ti_width != oovp->ti_width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,ovp->ti_width);
		if (ovp->ti_height != oovp->ti_height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,ovp->ti_height);
		if (ovp->ti_main_font_height != oovp->ti_main_font_height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiMainFontHeightF,
				   ovp->ti_main_font_height);
		if (ovp->ti_x_axis_font_height != 
		    oovp->ti_x_axis_font_height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiXAxisFontHeightF,
				   ovp->ti_x_axis_font_height);
		if (ovp->ti_y_axis_font_height != 
		    oovp->ti_y_axis_font_height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiYAxisFontHeightF,
				   ovp->ti_y_axis_font_height);
		if (ovp->real_main_offset_x != oovp->real_main_offset_x)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiMainOffsetXF,ovp->real_main_offset_x);
		if (ovp->real_x_axis_offset_x != oovp->real_x_axis_offset_x)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiXAxisOffsetXF,
				   ovp->real_x_axis_offset_x);
		if (ovp->real_y_axis_offset_y != oovp->real_y_axis_offset_y)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiYAxisOffsetYF,
				   ovp->real_y_axis_offset_y);
		if (ovp->ti_x_axis_position != oovp->ti_x_axis_position)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiXAxisPosition,
				   ovp->ti_x_axis_position);
		if (ovp->ti_y_axis_position != oovp->ti_y_axis_position)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiYAxisPosition,
				   ovp->ti_y_axis_position);
		if (ovp->ti_main_position != oovp->ti_main_position)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiMainPosition,
				   ovp->ti_main_position);

		subret = _NhlALSetValuesChild(ovp->titles->base.id,
					      (NhlLayer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error updating Title object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
	}

	return ret;
}

/*
 * Function:	ManageLabelBar
 *
 * Description: Internal function that manages the labelbar object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	NhlOverlayLayer	ovnew - The new overlay layer
 *		NhlOverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageLabelBar
#if __STDC__
(
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec,
	_NhlArgList	args,
	int		num_args
)
#else
(ovnew,ovold,init,anno_rec,args,num_args)
	NhlOverlayLayer	ovnew;
	NhlOverlayLayer	ovold;
	NhlBoolean	init;				       
	NhlAnnoRec	*anno_rec;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlOverlayLayerPart	*ovp = &ovnew->overlay;
	NhlOverlayLayerPart	*oovp = &ovold->overlay;
	int			tmpid = -1;
	NhlBoundingBox		bbox;
	char			buffer[_NhlMAXFNAMELEN];
        NhlSArg			sargs[16];
        int			nargs = 0;
	float			wold, hold;
	int			zone;
	float			sign;

	entry_name = (init) ? "OverlayInitialize" : "OverlaySetValues";
/*
 * Update the annotation record
 */
	anno_rec->side = ovp->lbar_side;
	anno_rec->para_pos = ovp->lbar_para_pos;
	anno_rec->ortho_pos = ovp->lbar_ortho_pos;
	anno_rec->status = ovp->display_labelbar;

/*
 * If not displaying a labelbar just call the SetValues function --
 * only to avoid the no set values called error.
 */
	if (! init && ovp->labelbar == NULL) {
		e_text = "%s: internal error: LabelBar layer NULL";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}
	if (init || ovp->display_labelbar != oovp->display_labelbar) {
		if (ovp->display_labelbar > NhlNEVER)
			NhlSetSArg(&sargs[nargs++],NhlNlbLabelBar,True);
		else {
			NhlSetSArg(&sargs[nargs++],NhlNlbLabelBar,False);
			if (! init) 
				return _NhlALSetValuesChild(
						    ovp->labelbar->base.id,
						    (NhlLayer)ovnew,
							    sargs,nargs);
		}
	}
/*
 * If the view width or height has changed adjust the LabelBar width and
 * height if they have not been set explcitly by the user.
 * Also if the user has not set height and width explicitly exchange 
 * height and width to achieve the normally most appropriate shape given
 * the current orientation.
 */

	wold = init ? NhlOV_STD_VIEW_WIDTH : ovold->view.width;
	hold = init ? NhlOV_STD_VIEW_HEIGHT : ovold->view.height;
	if (! _NhlArgIsSet(args,num_args,NhlNovLabelBarWidthF) &&
	    ! _NhlArgIsSet(args,num_args,NhlNovLabelBarHeightF)) {
		ovp->lbar_width *= ovnew->view.height / wold;
		ovp->lbar_height *= ovnew->view.height / hold;
		if (init || ovp->lbar_orient != oovp->lbar_orient) {
			float t;

			if (ovp->lbar_orient == NhlVERTICAL) {
				t = MIN(ovp->lbar_height,ovp->lbar_width);
				ovp->lbar_height = MAX(ovp->lbar_height,
						       ovp->lbar_width);
				ovp->lbar_width = t;
			}
			else  {
				t = MIN(ovp->lbar_height,ovp->lbar_width);
				ovp->lbar_width = MAX(ovp->lbar_height,
						       ovp->lbar_width);
				ovp->lbar_height = t;
			}
		}
	}

/*
 * Get the bounding box for the zone inside the annotation zone, 
 * then calculate the labelbar's position with respect to it.
 */
	bbox.set = 0;
	zone = anno_rec->zone < 2 ? anno_rec->zone : anno_rec->zone - 1;
	ret = InternalGetBB((NhlLayer)ovnew,&bbox,zone,
			    entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: Error getting bounding box";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}
	sign = anno_rec->zone == 1 ? 1.0 : -1.0;

	switch (anno_rec->side) {
	case NhlBOTTOM:
		ovp->lbar_x = ovnew->view.x + 
			anno_rec->para_pos * ovnew->view.width;
		ovp->lbar_y = bbox.b + 
			sign * anno_rec->ortho_pos * ovnew->view.height;
		break;
	case NhlTOP:
		ovp->lbar_x = ovnew->view.x + 
			anno_rec->para_pos * ovnew->view.width;
		ovp->lbar_y = bbox.t - 
			sign * anno_rec->ortho_pos * ovnew->view.height;
		break;
	case NhlLEFT:
		ovp->lbar_x = bbox.l + 
			sign * anno_rec->ortho_pos * ovnew->view.width;
		ovp->lbar_y = ovnew->view.y - ovnew->view.height +
			anno_rec->para_pos * ovnew->view.height;
		break;
	case NhlRIGHT:
		ovp->lbar_x = bbox.r - 
			sign * anno_rec->ortho_pos * ovnew->view.width;
		ovp->lbar_y = ovnew->view.y - ovnew->view.height +
			anno_rec->para_pos * ovnew->view.height;
		break;
	default:
		e_text = "%s: internal enumeration error";
		anno_rec->plot_id = -1;
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	anno_rec->just = ovp->lbar_just;
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlTOPRIGHT) {
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->zone > 1)
		anno_rec->just = ConstrainJustification(anno_rec);
	ovp->real_lbar_just = anno_rec->just;

/*
 * Adjust the annotation position based on the justification value
 */
	switch (ovp->real_lbar_just) {
	case NhlTOPLEFT:
		break;
	case NhlTOPCENTER:
		ovp->lbar_x = ovp->lbar_x - ovp->lbar_width / 2.0;
		break;
	case NhlTOPRIGHT:
		ovp->lbar_x = ovp->lbar_x - ovp->lbar_width;
		break;
	case NhlCENTERLEFT:
		ovp->lbar_y = ovp->lbar_y + ovp->lbar_height / 2.0;
		break;
	case NhlCENTERCENTER:
		ovp->lbar_x = ovp->lbar_x - ovp->lbar_width / 2.0;
		ovp->lbar_y = ovp->lbar_y + ovp->lbar_height / 2.0;
		break;
	case NhlCENTERRIGHT:
		ovp->lbar_x = ovp->lbar_x - ovp->lbar_width;
		ovp->lbar_y = ovp->lbar_y + ovp->lbar_height / 2.0;
		break;
	case NhlBOTTOMLEFT:
		ovp->lbar_y = ovp->lbar_y + ovp->lbar_height;
		break;
	case NhlBOTTOMCENTER:
		ovp->lbar_x = ovp->lbar_x - ovp->lbar_width / 2.0;
		ovp->lbar_y = ovp->lbar_y + ovp->lbar_height;
		break;
	case NhlBOTTOMRIGHT:
		ovp->lbar_y = ovp->lbar_y + ovp->lbar_height;
		ovp->lbar_x = ovp->lbar_x - ovp->lbar_width;
		break;
	}

/*
 * If no title exists, create it; otherwise just set the relevant
 * resources.
 */
	if (ovp->labelbar == NULL) {	
		strcpy(buffer,ovnew->base.name);
		strcat(buffer,".LabelBar");
		subret = _NhlVACreateChild(&tmpid,buffer,NhllabelBarLayerClass,
				    (NhlLayer)ovnew,
				    NhlNvpUseSegments,ovnew->view.use_segments,
				    NhlNvpXF,ovp->lbar_x,
				    NhlNvpYF,ovp->lbar_y,
				    NhlNvpWidthF,ovp->lbar_width,
				    NhlNvpHeightF,ovp->lbar_height,
				    NhlNlbJustification,ovp->real_lbar_just,
				    NhlNlbOrientation,ovp->lbar_orient,
				    NULL);
		anno_rec->plot_id = tmpid;
		if ((ret = MIN(ret,subret)) < NhlWARNING || 
		    (ovp->labelbar = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating LabelBar object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
	} else {

		if (ovp->lbar_x != oovp->lbar_x)
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovp->lbar_x);
		if (ovp->lbar_y != oovp->lbar_y)
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovp->lbar_y);
		if (ovp->lbar_width != oovp->lbar_width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,ovp->lbar_width);
		if (ovp->lbar_height != oovp->lbar_height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,ovp->lbar_height);
		if (ovp->real_lbar_just != oovp->real_lbar_just)
			NhlSetSArg(&sargs[nargs++],
				   NhlNlbJustification,ovp->real_lbar_just);
		if (ovp->lbar_orient != oovp->lbar_orient)
			NhlSetSArg(&sargs[nargs++],
				   NhlNlbOrientation,ovp->lbar_orient);
		subret = _NhlALSetValuesChild(ovp->labelbar->base.id,
					      (NhlLayer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error updating LabelBar object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
	}

	return ret;
}


/*
 * Function:	ManageLegend
 *
 * Description: Internal function that manages the legend object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	NhlOverlayLayer	ovnew - The new overlay layer
 *		NhlOverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageLegend
#if __STDC__
(
	NhlOverlayLayer	ovnew,
	NhlOverlayLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec,
	_NhlArgList	args,
	int		num_args
)
#else
(ovnew,ovold,init,anno_rec,args,num_args)
	NhlOverlayLayer	ovnew;
	NhlOverlayLayer	ovold;
	NhlAnnoRec	*anno_rec;
	NhlBoolean	init;				       
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlOverlayLayerPart	*ovp = &ovnew->overlay;
	NhlOverlayLayerPart	*oovp = &ovold->overlay;
	int			tmpid = -1;
	NhlBoundingBox		bbox;
	char			buffer[_NhlMAXFNAMELEN];
        NhlSArg			sargs[16];
        int			nargs = 0;
	float			wold, hold;
	int			zone;
	float			sign;

	entry_name = (init) ? "OverlayInitialize" : "OverlaySetValues";

/*
 * Update the annotation record
 */
	anno_rec->side = ovp->lgnd_side;
	anno_rec->para_pos = ovp->lgnd_para_pos;
	anno_rec->ortho_pos = ovp->lgnd_ortho_pos;
	anno_rec->status = ovp->display_legend;

/*
 * If not displaying a legend just call the SetValues function --
 * only to avoid the no set values called error.
 */
	if (! init && ovp->legend == NULL) {
		e_text = "%s: internal error: Legend layer NULL";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}
	if (init || ovp->display_legend != oovp->display_legend) {
		if (ovp->display_legend > NhlNEVER)
			NhlSetSArg(&sargs[nargs++],NhlNlgLegend,True);
		else {
			NhlSetSArg(&sargs[nargs++],NhlNlgLegend,False);
			if (! init) 
				return _NhlALSetValuesChild(
						ovp->legend->base.id,
						(NhlLayer)ovnew,sargs,nargs);
		}
	}
/*
 * If the view width or height has changed adjust the Legend width and
 * height if they have not been set explcitly by the user
 */
	wold = init ? NhlOV_STD_VIEW_WIDTH : ovold->view.width;
	hold = init ? NhlOV_STD_VIEW_HEIGHT : ovold->view.height;
	if (! _NhlArgIsSet(args,num_args,NhlNovLegendWidthF) &&
	    ! _NhlArgIsSet(args,num_args,NhlNovLegendHeightF)) {
		ovp->lgnd_width *= ovnew->view.height / wold;
		ovp->lgnd_height *= ovnew->view.height / hold;
	}


/*
 * Get the bounding box for the zone inside the annotation zone, 
 * then calculate the legend's position with respect to it.
 */
	bbox.set = 0;
	zone = anno_rec->zone < 2 ? anno_rec->zone : anno_rec->zone - 1;
	subret = InternalGetBB((NhlLayer)ovnew,&bbox,zone,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: Error getting bounding box";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}
	sign = anno_rec->zone == 1 ? 1.0 : -1.0;
	switch (anno_rec->side) {
	case NhlBOTTOM:
		ovp->lgnd_x = ovnew->view.x + 
			anno_rec->para_pos * ovnew->view.width;
		ovp->lgnd_y = bbox.b + 
			sign * anno_rec->ortho_pos * ovnew->view.height;
		break;
	case NhlTOP:
		ovp->lgnd_x = ovnew->view.x + 
			anno_rec->para_pos * ovnew->view.width;
		ovp->lgnd_y = bbox.t - 
			sign * anno_rec->ortho_pos * ovnew->view.height;
		break;
	case NhlLEFT:
		ovp->lgnd_x = bbox.l + 
			sign * anno_rec->ortho_pos * ovnew->view.width;
		ovp->lgnd_y = ovnew->view.y - ovnew->view.height +
			anno_rec->para_pos * ovnew->view.height;
		break;
	case NhlRIGHT:
		ovp->lgnd_x = bbox.r - 
			sign * anno_rec->ortho_pos * ovnew->view.width;
		ovp->lgnd_y = ovnew->view.y - ovnew->view.height +
			anno_rec->para_pos * ovnew->view.height;
		break;
	default:
		e_text = "%s: internal enumeration error";
		anno_rec->plot_id = -1;
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	anno_rec->just = ovp->lgnd_just;
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlTOPRIGHT) {
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->zone > 0)
		anno_rec->just = ConstrainJustification(anno_rec);
	ovp->real_lgnd_just = anno_rec->just;

/*
 * Adjust the annotation position based on the justification value
 */
	switch (ovp->real_lgnd_just) {
	case NhlTOPLEFT:
		break;
	case NhlTOPCENTER:
		ovp->lgnd_x = ovp->lgnd_x - ovp->lgnd_width / 2.0;
		break;
	case NhlTOPRIGHT:
		ovp->lgnd_x = ovp->lgnd_x - ovp->lgnd_width;
		break;
	case NhlCENTERLEFT:
		ovp->lgnd_y = ovp->lgnd_y + ovp->lgnd_height / 2.0;
		break;
	case NhlCENTERCENTER:
		ovp->lgnd_x = ovp->lgnd_x - ovp->lgnd_width / 2.0;
		ovp->lgnd_y = ovp->lgnd_y + ovp->lgnd_height / 2.0;
		break;
	case NhlCENTERRIGHT:
		ovp->lgnd_x = ovp->lgnd_x - ovp->lgnd_width;
		ovp->lgnd_y = ovp->lgnd_y + ovp->lgnd_height / 2.0;
		break;
	case NhlBOTTOMLEFT:
		ovp->lgnd_y = ovp->lgnd_y + ovp->lgnd_height;
		break;
	case NhlBOTTOMCENTER:
		ovp->lgnd_x = ovp->lgnd_x - ovp->lgnd_width / 2.0;
		ovp->lgnd_y = ovp->lgnd_y + ovp->lgnd_height;
		break;
	case NhlBOTTOMRIGHT:
		ovp->lgnd_y = ovp->lgnd_y + ovp->lgnd_height;
		ovp->lgnd_x = ovp->lgnd_x - ovp->lgnd_width;
		break;
	}

/*
 * If no title exists, create it; otherwise just set the relevant
 * resources.
 */
	if (ovp->legend == NULL) {	
		strcpy(buffer,ovnew->base.name);
		strcat(buffer,".Legend");
		subret = _NhlVACreateChild(&tmpid,buffer,NhllegendLayerClass,
				    (NhlLayer)ovnew,
				    NhlNvpUseSegments,ovnew->view.use_segments,
				    NhlNvpXF,ovp->lgnd_x,
				    NhlNvpYF,ovp->lgnd_y,
				    NhlNvpWidthF,ovp->lgnd_width,
				    NhlNvpHeightF,ovp->lgnd_height,
				    NhlNlgJustification,ovp->real_lgnd_just,
				    NhlNlgOrientation,ovp->lgnd_orient,
				    NULL);
		anno_rec->plot_id = tmpid;
		if ((ret = MIN(ret,subret)) < NhlWARNING || 
		    (ovp->legend = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating Legend object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
	} else {

		if (ovp->lgnd_x != oovp->lgnd_x)
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovp->lgnd_x);
		if (ovp->lgnd_y != oovp->lgnd_y)
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovp->lgnd_y);
		if (ovp->real_lgnd_just != oovp->real_lgnd_just)
			NhlSetSArg(&sargs[nargs++],
				   NhlNlgJustification,ovp->real_lgnd_just);
		if (ovp->lgnd_width != oovp->lgnd_width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,ovp->lgnd_width);
		if (ovp->lgnd_height != oovp->lgnd_height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,ovp->lgnd_height);
		if (ovp->lgnd_orient != oovp->lgnd_orient)
			NhlSetSArg(&sargs[nargs++],
				   NhlNlgOrientation,ovp->lgnd_orient);
		subret = _NhlALSetValuesChild(ovp->legend->base.id,
					      (NhlLayer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error updating Legend object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
	}

	return ret;
}


/* low level overlay mapping functions */

void _NhlovCpMapXY
#if __STDC__
(float *xin,float *yin, float* xout, float* yout)
#else
(xin,yin,xout,yout)
        float *xin;
        float *yin;
        float *xout;
        float *yout;
#endif
{
        int status = 0;

        if (Overlay_Trans_Obj == NULL || Overlay_Trans_Obj == Trans_Obj) {
		_NhlCompcToWin(Trans_Obj,Plot,xin,yin,1,xout,yout,
			       &status,NULL,NULL);
	}
        else {
		_NhlCompcToData(Trans_Obj,Plot,xin,yin,1,xout,yout,
				&status,NULL,NULL);

		if (status) return;
#if 0
		fprintf (stderr,"inter: %f %f : ",*xout,*yout);
#endif

		_NhlDataToWin(Overlay_Trans_Obj,Overlay_Plot,
			     xout,yout,1,xout,yout,&status,NULL,NULL);
        }

#if 0
	fprintf (stderr,"%f %f : %f %f \n",*xin,*yin,*xout,*yout);
#endif

	return;
}


void _NhlovCpInvMapXY
#if __STDC__
(float *xin,float *yin, float* xout, float* yout)
#else
(xin,yin,xout,yout)
        float *xin;
        float *yin;
        float *xout;
        float *yout;
#endif
{
        int status = 0;

        if (Overlay_Trans_Obj == NULL || Overlay_Trans_Obj == Trans_Obj) {
		_NhlWinToCompc(Trans_Obj,Plot,xin,yin,1,xout,yout,
			       &status,NULL,NULL);
	}
        else {
		_NhlWinToData(Overlay_Trans_Obj,Overlay_Plot,
			      xin,yin,1,xout,yout,
			      &status,NULL,NULL);

		if (status) return;
#if 0
		fprintf (stderr,"inter: %f %f : ",*xout,*yout);
#endif

		_NhlDataToCompc(Trans_Obj,Plot,xout,yout,1,xout,yout,
				&status,NULL,NULL);
        }

#if 0
	fprintf (stderr,"%f %f : %f %f \n",*xin,*yin,*xout,*yout);
#endif

	return;
}


/*
 * The first two global functions allow the user to add, remove, and 
 * control the drawing sequence of plots in an overlay.
 */

/*
 * Function:	NhlAddToOverlay
 *
 * Description:	
 *
 * In Args:	base_id		id of overlay base plot
 *		plot_id		id of plot to add to overlay
 *		after_id	id of plot already a member of the overlay
 *				that the new plot should be drawn after --
 *				if < 0 then the new plot is placed at the
 *				end of the overlay sequence.
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlAddToOverlay
#if  __STDC__
(int base_id, int plot_id, int after_id)
#else
(base_id, plot_id, after_id)
        int base_id;
	int plot_id;
	int after_id;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlAddToOverlay";

	NhlLayer			base = _NhlGetLayer(base_id);
	NhlLayer			plot = _NhlGetLayer(plot_id);
	NhlLayer			after = _NhlGetLayer(after_id);

	NhlTransformLayerPart	*base_tfp;
	NhlOverlayLayer		ovl;
	NhlOverlayLayerPart	*ovp;
	NhlTransformLayerPart	*plot_tfp;
	NhlTransformLayerClassRec	*plot_classp;

	NhlGenArray		ga;
	int			plot_count = 0;
	NhlovRec			**sub_recs = NULL;
	int			i, j;

/*
 * Check validity of the plot layers, then root out the pointer to the overlay
 * layer.
 */
	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid base plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (plot == NULL || ! _NhlIsTransform(plot)) {
		e_text = "%s: invalid overlay plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (base == plot) {
		e_text = "%s: overlay member and base plot ids are the same";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		return NhlWARNING;
	}

	plot_tfp = &(((NhlTransformLayer)plot)->trans);
	base_tfp = &(((NhlTransformLayer)base)->trans);
	if (! base_tfp->overlay_plot_base ||
	    base_tfp->overlay_object == NULL || 
	    ! _NhlIsTransform(base_tfp->overlay_object)) {
		e_text = "%s: no overlay initialized for base plot";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	ovl = (NhlOverlayLayer) base_tfp->overlay_object;
	ovp = &(ovl->overlay);

/*
 * Test to ensure the added plot can handle becoming an overlay member. 
 */
	plot_classp = (NhlTransformLayerClassRec *) _NhlClass(plot);
	if (plot_classp->trans_class.overlay_capability ==
	    					_tfNotOverlayCapable ||
	    plot_classp->trans_class.overlay_capability ==
	    					_tfOverlayBaseOnly) {
		e_text = "%s: plot class %s cannot be overlay member";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  plot_classp->base_class.class_name);
		return NhlWARNING;
	}

/*
 * If the plot is already an overlay member (but not a base plot) it cannot
 * be part of another overlay (even the same overlay).
 * If the plot is an overlay base already, any overlays it has acquired 
 * become part of the overlay it has been added to. Get the record 
 * containing the list of overlays plots plus their associated overlay objs.
 * Also set the argument that will tell its overlay object it is no longer
 * a master overlay object.
 * If the plot is only not an overlay base or an overlay member, simply
 * allocate and fill in a single overlay record for it.
 */

	if (plot_tfp->overlay_status == _tfCurrentOverlayMember) {
		e_text = "%s: plot ID %d is already an overlay member";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,plot_id);
		return NhlWARNING;
	}
	else if (plot_tfp->overlay_status == _tfCurrentOverlayBase) {

		if (plot_tfp->overlay_object == NULL || 
		    ! _NhlIsTransform(plot_tfp->overlay_object)) {
		      e_text = "%s: plot ID %d has inconsistent overlay info";
		        NhlPError(NhlWARNING,NhlEUNKNOWN,
				  e_text,entry_name,plot_id);
			return NhlWARNING;
		}

		subret = NhlVAGetValues(plot_tfp->overlay_object->base.id,
				      NhlNovOverlayRecs, &ga, NULL);

		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			return ret;
		}
		if (ga == NULL || ga->size != sizeof(NhlovRec *)) {
			e_text = "%s: error retrieving internal gen array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		plot_count = ga->num_elements;
		sub_recs = (NhlovRec **) ga->data;

		ga->my_data = False;
		NhlFreeGenArray(ga);

	}
	else {
		if ((sub_recs = (NhlovRec **) 
		     NhlMalloc(sizeof(NhlovRec *))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if ((sub_recs[0] = (NhlovRec *) 
		     NhlMalloc(sizeof(NhlovRec))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		plot_count = 1;
		sub_recs[0]->plot = (NhlTransformLayer) _NhlGetLayer(plot_id);
		sub_recs[0]->ov_obj = NULL;
	}
			

/*
 * Reallocate the array of overlay record pointers if necessary
 */
	if (ovp->overlay_alloc < ovp->overlay_count + plot_count) {
		ovp->ov_recs = (NhlovRec **)
			NhlRealloc(ovp->ov_recs, sizeof(NhlovRec *) *
				   (ovp->overlay_count + 
				    MAX(NhlOV_ALLOC_UNIT,plot_count)));
		if (ovp->ov_recs == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		ovp->overlay_alloc += MAX(NhlOV_ALLOC_UNIT,plot_count);
	}

/*
 * If no after plot is specified put the new plot at the end of the array.
 * Otherwise, rearrange the array so that the new plot follows the plot
 * specified by the after plot id. An invalid after plot id generates a
 * NhlWARNING; the plot is placed at the end of the array.
 */
	
	if (after == NULL) {

		for (i = 0; i < plot_count; i++)
			ovp->ov_recs[ovp->overlay_count+i] = sub_recs[i]; 
	}
	else if (! _NhlIsTransform(after)) {

		e_text = "%s: invalid after plot id";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);

		for (i = 0; i < plot_count; i++)
			ovp->ov_recs[ovp->overlay_count+i] = sub_recs[i]; 
	}
	else {
		for (i = 0; i <= ovp->overlay_count; i++) {

			if (i == ovp->overlay_count) { /* after not found */
				e_text = "%s: invalid after plot id";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name);
				ret = MIN(ret,NhlWARNING);
				for (j = 0; j < plot_count; j++)
					ovp->ov_recs[ovp->overlay_count+j] = 
						sub_recs[j];

	}
			else if (after == (NhlLayer) ovp->ov_recs[i]->plot) {
				for (j = ovp->overlay_count - 1;j > i; j--) {
				      ovp->ov_recs[j+plot_count] =
					      ovp->ov_recs[j];
			        }
				for (j = 0; j < plot_count; j++)
					ovp->ov_recs[j+i+1] = sub_recs[j];
				break;
			}
		}
	}
	ovp->overlay_count += plot_count;
		
/*
 * Call set values for each plot added to the overlay to inform it of its
 * new status, adjusting its view to be identical to the overlay's view.
 */
	for (i = 0; i < plot_count; i++) {
		NhlSArg			sargs[10];
		int			nargs = 0;

		NhlSetSArg(&sargs[nargs++],NhlNtfOverlayStatus, 
			   _tfCurrentOverlayMember);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtfOverlayObject,base_tfp->overlay_object);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtfOverlayTrans, base_tfp->overlay_trans_obj);
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovl->view.x);
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovl->view.y);
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,ovl->view.width);
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,ovl->view.height);
		NhlSetSArg(&sargs[nargs++],NhlNovUpdateReq, True);

		subret = NhlALSetValues(sub_recs[i]->plot->base.id,
					sargs,nargs); 
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
	}
/*
 * Update the overlay in order that the annotation objects can adjust
 * to the possibility of some new annotations.
 */
	subret = NhlVASetValues(base_id,NhlNovUpdateReq,True,NULL);
	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;

/*
 * Bump up the overlay count, free the sub_recs pointer array, and NULL
 * added overlay record pointer array elements not yet in use.
 */
	NhlFree(sub_recs);
	for (i = ovp->overlay_count; i < ovp->overlay_alloc; i++) {
		ovp->ov_recs[i] = NULL;
	}

	return ret;
}

/*
 * Function:	nhlf_addtooverlay
 *
 * Description:	Fortran wrapper for NhlAddToOverlay
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private Fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlf_addtooverlay,NHLF_ADDTOOVERLAY)
#if	__STDC__
(
	int	*base,
	int	*plot,
	int	*after,
	int	*err
)
#else
(base,plot,after,err)
	int	*base;
	int	*plot;
	int	*after;
	int	*err;
#endif
{
	*err = NhlAddToOverlay(*base,*plot,*after);

	return;
}

/*
 * Function:	NhlRemoveFromOverlay
 *
 * Description:	
 *
 * In Args:	base_id		id of overlay base plot
 *		plot_id		id of plot to remove from overlay
 *		restore		if True, restores any
 *				member overlays that belonged initially to
 *				the plot being removed, thus removing them
 *				from the overlay based on the base_id. 
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlRemoveFromOverlay
#if  __STDC__
(int base_id, int plot_id, NhlBoolean restore)
#else
(base_id, plot_id,restore)
        int base_id;
	int plot_id;
	NhlBoolean restore;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveFromOverlay";
	NhlLayer			base = _NhlGetLayer(base_id);
	NhlLayer			plot = _NhlGetLayer(plot_id);

	NhlTransformLayerPart	*base_tfp;
	NhlOverlayLayerPart	*ovp;
	int			i, j;


/*
 * Check validity of the plot layers, then root out the pointer to the overlay
 * layer.
 */
	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid base plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (plot == NULL || ! _NhlIsTransform(plot)) {
		e_text = "%s: invalid overlay plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	base_tfp = &(((NhlTransformLayer)base)->trans);
	if (! base_tfp->overlay_plot_base ||
	    base_tfp->overlay_object == NULL || 
	    ! _NhlIsTransform(base_tfp->overlay_object)) {
		e_text = "%s: no overlay initialized for base plot";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	ovp = (NhlOverlayLayerPart *) 
		&(((NhlOverlayLayer)base_tfp->overlay_object)->overlay);
	if (ovp->ov_recs[0]->plot != (NhlTransformLayer) base) {
		e_text = "%s: base is not currently an overlay base plot";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		return NhlWARNING;
	}
/*
 * This code simply provides an alternative to the SetValues interface
 * removing the overlay object from a base plot. The restore parameter is
 * ignored in this situation
 */
	if (plot_id == base_id) {
		NhlSArg			sarg;

		NhlSetSArg(&sarg,NhlNtfOverlayPlotBase,False);
		subret = NhlALSetValues(base_id,&sarg,1); 
		return MIN(subret,ret);
	}

/*
 * Note: the value of ovp->overlay_count changes during this loop, but
 * not until the correct plot has been found. A break follows, so the 
 * no test is made after the change.
 */
	for (i = 0; i <= ovp->overlay_count; i++) {
		if (i == ovp->overlay_count) {
			e_text = "%s: plot not found in overlay";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			return NhlWARNING;
		}
		if (plot == (NhlLayer) ovp->ov_recs[i]->plot) {
			
			if (restore && ovp->ov_recs[i]->ov_obj != NULL) {
				subret = RestoreOverlayBase(ovp,i);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					return ret;
				}
			}
			else if (ovp->ov_recs[i]->ov_obj != NULL) {

				subret = RemoveOverlayBase(ovp,i);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					return ret;
				}
			}
			else {
				NhlSArg			sargs[3];
				int			nargs = 0;

				for (j = i; j < ovp->overlay_count - 1; j++) {
					ovp->ov_recs[j] = ovp->ov_recs[j+1];
				}
				NhlSetSArg(&sargs[nargs++],
					   NhlNtfOverlayObject, NULL);
				NhlSetSArg(&sargs[nargs++],
					   NhlNtfOverlayTrans,NULL);
				NhlSetSArg(&sargs[nargs++],
					   NhlNtfOverlayStatus,
					   _tfNotInOverlay);
				NhlSetSArg(&sargs[nargs++],
					   NhlNovUpdateReq, True);

				subret = NhlALSetValues(plot->base.id,
							sargs,nargs); 
				if ((ret = MIN(subret,ret)) < NhlWARNING)
					return ret;

				ovp->ov_recs[--ovp->overlay_count] = NULL;
			}
			break;
		}
	}
/*
 * Must update the remaining overlay members, so that their annotation
 * objects can adjust to the possibility that some annotations were 
 * removed.
 */
	subret = NhlVASetValues(base_id,NhlNovUpdateReq,True,NULL);
	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;

	return ret;
}

/*
 * Function:	nhlf_removefromoverlay
 *
 * Description:	Fortran wrapper for NhlRemoveFromOverlay
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlf_removefromoverlay,NHLF_REMOVEFROMOVERLAY)
#if	__STDC__
(
	int	*base,
	int	*plot,
	int	*restore,
	int	*err
)
#else
(base,plot,restore,err)
	int	*base;
	int	*plot;
	int	*restore;
	int	*err;
#endif
{
	*err = NhlRemoveFromOverlay(*base,*plot,*restore);

	return;
}

/*
 * Function:	NhlRegisterAnnotation
 *
 * Description:	Registers an annotation with an overlay base plot. An
 *		annotation is an object that specifies the position 
 *		of an arbitrary plot object relative to the overlay base 
*		plot's viewport. The only restriction on the plot object (not
 *		enforced) is that it should not draw outside its own 
 *		viewport. Note that the annotation always remains with the
 *		overlay base it was originally registered with (until 
 *		unregistered). This means that a plot object should always
 *		register the plot with its own overlay base if it has one.
 *		This ensures that the annotation will automatically be
 *		removed from an overlay when the plot it belongs to is 
 *		removed. If the plot does not have its own overlay base, 
 *		it needs to ensure that any registered annotations
 *		that belong to it are unregistered when it is removed from
 *		an overlay. 
 *
 * In Args:	overlay_base_id	id of overlay base plot
 *		annotation_id	id of annotation object
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlRegisterAnnotation
#if  __STDC__
(int overlay_base_id, int annotation_id)
#else
(overlay_base_id, annotation_id)
        int overlay_base_id;
	int annotation_id;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlRegisterAnnotation";
	NhlLayer		base = _NhlGetLayer(overlay_base_id);
	NhlLayer		annotation = _NhlGetLayer(annotation_id);
	NhlOverlayLayer		overlay;
	NhlTransformLayerPart	*base_tfp;
	NhlAnnoRec		*anrp;
	int			pid,zone;
	NhlPosition		side;
	NhlJustification	just;
	float			ortho, parallel,data_x,data_y;
	NhlBoolean		on,resize_notify,track_data;

	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid overlay plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
/*
 * Make sure the base plot has an overlay; if so extract the overlay
 * layer pointer from the base plot's transform layer.
 */
	base_tfp = &(((NhlTransformLayer)base)->trans);
	if (! base_tfp->overlay_plot_base ||
	    base_tfp->overlay_object == NULL || 
	    ! _NhlIsTransform(base_tfp->overlay_object)) {
		e_text = "%s: no overlay initialized for base plot";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	overlay = (NhlOverlayLayer) base_tfp->overlay_object;
/*
 * Test the annotation layer pointer; if valid get its resource values.
 */
	if (annotation == NULL) {
		e_text = "%s: invalid annotation id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	subret = NhlVAGetValues(annotation_id,
				NhlNanOn,&on,
				NhlNanResizeNotify,&resize_notify,
				NhlNanPlotId,&pid,
				NhlNanZone,&zone,
				NhlNanSide,&side,
				NhlNanJust,&just,
				NhlNanOrthogonalPosF,&ortho,
				NhlNanParallelPosF,&parallel,
				NhlNanTrackData,&track_data,
				NhlNanDataXF,&data_x,
				NhlNanDataYF,&data_y,
				NULL);

	if ((ret = MIN(subret,ret)) < NhlWARNING) return NhlFATAL;
/*
 * Record the annotation in the overlay's data structures; a pointer
 * to the new annotation record is returned.
 */ 
	if ((anrp = RecordAnnotation(overlay,ovEXTERNAL,
				     NhlALWAYS,zone)) == NULL) {
		e_text = "%s: error registering annotation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
/*
 * Fill in the fields of the annotation record that the recording function
 * does not initialize.
 */
	anrp->status = on ? NhlALWAYS : NhlNEVER;
	anrp->anno_id = annotation_id;
	anrp->plot_id = pid;
	anrp->resize_notify = resize_notify;
	anrp->side = side;
	anrp->just = just;
	anrp->para_pos = parallel;
	anrp->ortho_pos = ortho;
	anrp->track_data = track_data;
	anrp->data_x = data_x;
	anrp->data_y = data_y;

	subret = NhlVASetValues(annotation_id,
				NhlNanOverlayBaseId,overlay_base_id,
				NULL);
	ret = MIN(subret,ret);

	return ret;
}

/*
 * Function:	nhlf_registerannotation
 *
 * Description:	Fortran wrapper for NhlRegisterAnnotation
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlf_registerannotation,NHLF_REGISTERANNOTATION)
#if	__STDC__
(
	int	*overlay_base,
	int	*annotation,
	int	*err
)
#else
(overlay_base,annotation,err)
	int	*overlay_base;
	int	*annotation;
	int	*err;
#endif
{
	*err = NhlRegisterAnnotation(*overlay_base,*annotation);

	return;
}


/*
 * Function:	NhlUnregisterAnnotation
 *
 * Description:	
 *
 * In Args:	overlay_base_id	id of overlay plot
 *		annotation_id	id of annotation object
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlUnregisterAnnotation
#if  __STDC__
(int overlay_base_id, int annotation_id)
#else
(overlay_base_id, annotation_id)
        int overlay_base_id;
	int annotation_id;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlUnregisterAnnotation";
	NhlLayer		base = _NhlGetLayer(overlay_base_id);
	NhlLayer		annotation = _NhlGetLayer(annotation_id);
	NhlOverlayLayer		overlay;
	NhlTransformLayerPart	*base_tfp;
	NhlOverlayLayerPart	*ovp;
	NhlAnnoRec		*anrp;
	int			i, found = False;

	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid overlay plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
/*
 * Make sure the base plot has an overlay; if so extract the overlay
 * layer pointer from the base plot's transform layer.
 */
	base_tfp = &(((NhlTransformLayer)base)->trans);
	if (! base_tfp->overlay_plot_base ||
	    base_tfp->overlay_object == NULL || 
	    ! _NhlIsTransform(base_tfp->overlay_object)) {
		e_text = "%s: no overlay initialized for base plot";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	overlay = (NhlOverlayLayer) base_tfp->overlay_object;
/*
 * Test the annotation layer pointer.
 */
	if (annotation == NULL) {
		e_text = "%s: invalid annotation id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	ovp = &overlay->overlay;

	for (i = 0; i < ovp->overlay_count; i++) {
		
		if ((anrp = ovp->ov_recs[i]->anno_list) == NULL)
			continue;

		if (anrp->anno_id == annotation_id) {
			ovp->ov_recs[i]->anno_list = anrp->next;
			NhlFree(anrp);
			found = True;
			break;
		}
		while (anrp->next != NULL) {
			if (anrp->next->anno_id == annotation_id) {
				NhlAnnoRec	*free_anno = anrp->next;
				anrp->next = free_anno->next;
				NhlFree(free_anno);
				found = True;
				break;
			}
			anrp = anrp->next;
		}
	}
	if (! found) {
		e_text = "%s: annotation not found";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	ret = NhlVASetValues(annotation_id,
			     NhlNanOverlayBaseId,-1,NULL);

	return ret;

}

/*
 * Function:	nhlf_unregisterannotation
 *
 * Description:	Fortran wrapper for NhlUnregisterAnnotation
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlf_unregisterannotation,NHLF_UNREGISTERANNOTATION)
#if	__STDC__
(
	int	*overlay_base,
	int	*annotation,
	int	*err
)
#else
(overlay_base,annotation,err)
	int	*overlay_base;
	int	*annotation;
	int	*err;
#endif
{
	*err = NhlUnregisterAnnotation(*overlay_base,*annotation);

	return;
}

/*
 * Function:	DissolveOverlay
 *
 * Description: Internal function that breaks an overlay into its
 *		component parts. Invokes whenever an overlay is destroyed.
 *		Restores the state of overlay plot member to their
 *		condition before being added to the current overlay. 
 *
 * In Args:	NhlLayer overlay_object - the overlay object being dissolved
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
DissolveOverlay
#if __STDC__
(
	NhlLayer		overlay_object
)
#else
(overlay_object)
	NhlLayer			overlay_object;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlOverlayLayerPart	*ovp = 
				  &((NhlOverlayLayer)overlay_object)->overlay;
	int			i;
	NhlSArg			sargs[3];
	int			nargs = 0;

	while (ovp->overlay_count > 1) {
			
		if (ovp->ov_recs[1]->ov_obj != NULL) {
			subret = RestoreOverlayBase(ovp,1);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				return ret;
			}
		}
		else {
			NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject, NULL);
			NhlSetSArg(&sargs[nargs++],NhlNtfOverlayTrans,NULL);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtfOverlayStatus,_tfNotInOverlay);

			subret = NhlALSetValues(ovp->ov_recs[1]->plot->base.id,
						sargs,nargs); 
			if ((ret = MIN(subret,ret)) < NhlWARNING)
				return ret;

			for (i = 1; i < ovp->overlay_count - 1; i++) {
				ovp->ov_recs[i] = ovp->ov_recs[i+1];
			}
			ovp->ov_recs[--ovp->overlay_count] = NULL;
			nargs = 0;
		}
	}

	return MIN(subret,ret);
}

/*
 * Function:	RemoveOverlayBase
 *
 * Description: Internal function that removes an overlay base plot without
 *		removing any member plots that originally belonged to it.
 *
 * In Args:	NhlOverlayLayerPart ovp - overlay object that plot is being
 * 				       removed from
 *		int plot_number -- the array position in the overlay record of
 *			       	   the plot being removed
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
RemoveOverlayBase
#if __STDC__
(
	NhlOverlayLayerPart	*ovp,
	int			plot_number
)
#else
(ovp,plot_number)
	NhlOverlayLayerPart	*ovp;
	int			plot_number;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveFromOverlay";
	NhlOverlayLayer		plot_ovl = (NhlOverlayLayer) 
					ovp->ov_recs[plot_number]->ov_obj;
	NhlTransformLayer		plot = ovp->ov_recs[plot_number]->plot;
	NhlGenArray		ga;
	NhlovRec			**ov_recs = NULL;
	NhlSArg			sargs[10];
        int			nargs = 0;
	int			i;
	int			count = 1;
	NhlAnnoRec		*anno_list;
	int			max_zone;

	anno_list = ovp->ov_recs[plot_number]->anno_list;
	max_zone = ovp->ov_recs[plot_number]->max_zone;
	for (i = plot_number; i < ovp->overlay_count - 1; i++) {
		ovp->ov_recs[i] = ovp->ov_recs[i+1];
	}
	ovp->ov_recs[--ovp->overlay_count] = NULL;
/*
 * Create a GenArray of 1 element in order to set the OverlayRecs resource
 * of the overlay plot. This will erase its memory of its overlay plots.
 */

	ov_recs = (NhlovRec **) NhlMalloc(sizeof(NhlovRec *));
	if (ov_recs == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	ov_recs[0] = (NhlovRec *) 
		NhlMalloc(ovp->overlay_count * sizeof(NhlovRec));
	if (ov_recs[0] == NULL) {
		e_text ="%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	ov_recs[0]->plot = plot;
	ov_recs[0]->ov_obj = (NhlLayer) plot_ovl;
	ov_recs[0]->anno_list = anno_list;
	ov_recs[0]->max_zone = max_zone;

	ga = NhlCreateGenArray((NhlPointer)ov_recs,NhlTPointer,
			       sizeof(NhlovRec *),1,&count);
	if (ga == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NhlNovOverlayRecs);
		return NhlFATAL;
	}
	ga->my_data = True;

	NhlSetSArg(&sargs[nargs++],NhlNovOverlayRecs, ga);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayStatus,_tfCurrentOverlayBase);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject,plot_ovl);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayTrans,plot->trans.trans_obj);

	
	subret = NhlALSetValues(plot->base.id,sargs,nargs); 
	ret = MIN(subret,ret);
		  
	NhlFree(ov_recs[0]);
	NhlFreeGenArray(ga);

	return ret;

}

/*
 * Function:	RestoreOverlayBase
 *
 * Description: Internal function that restores the overlay state of a plot
 *		that was originally an overlay base when it is removed from 
 *		another overlay.
 *
 * In Args:	NhlOverlayLayerPart ovp - overlay object that plot is being
 * 				       removed from
 *		int plot_number -- the array position in the overlay record of
 *			       	   the plot being removed
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
RestoreOverlayBase
#if __STDC__
(
	NhlOverlayLayerPart	*ovp,
	int			plot_number
)
#else
(ovp,plot_number)
	NhlOverlayLayerPart	*ovp;
	int			plot_number;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveFromOverlay";
	NhlOverlayLayer		plot_ovl = (NhlOverlayLayer) 
					ovp->ov_recs[plot_number]->ov_obj;
	NhlTransformLayer		plot = ovp->ov_recs[plot_number]->plot;
	NhlGenArray		ga;
	NhlovRec			**sub_recs = NULL;
	NhlSArg			sargs[10];
        int			nargs = 0;
	int			i, j, k;
	int			new_plot_count;

/*
 * Assuming the overlay pointer for the plot is valid, fetch its overlay
 * record. Then loop through the overlay record and try to match each
 * record with a record in the current overlay object. For any record that
 * matches, remove it from the current overlay object. Any records that are
 * not found must have been removed from the current overlay object; they
 * must therefore be removed from the plot's overlay object as well.
 */
	if (! _NhlIsTransform((NhlLayer) plot_ovl)) {
		e_text = "%s: internal inconsistency in record of plot ID %d";
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  e_text,entry_name,plot->base.id);
		return NhlFATAL;
	}

	subret = NhlVAGetValues(plot_ovl->base.id,
			      NhlNovOverlayRecs, &ga, NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		return ret;
	}
	if (ga == NULL || ga->size != sizeof(NhlovRec *)) {
		e_text = "%s: error retrieving internal gen array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	new_plot_count = ga->num_elements;
	sub_recs = (NhlovRec **) ga->data;

	for (i = 0; i < new_plot_count; ) {
		for (j = 0; j <= ovp->overlay_count; j++) {
			if (j == ovp->overlay_count) {
				NhlFree(sub_recs[i]);
				for (k = i; k < new_plot_count - 1; k++) {
					sub_recs[k] = sub_recs[k+1];
				}
				sub_recs[--new_plot_count] = NULL;
			}
			else if (ovp->ov_recs[j]->plot == sub_recs[i]->plot) {
				NhlFree(ovp->ov_recs[j]);
				for (k = j; k < ovp->overlay_count - 1; k++) {
					ovp->ov_recs[k] = ovp->ov_recs[k+1];
				}
				ovp->ov_recs[--ovp->overlay_count] = NULL;
				i++;
				break;
			}
		}
	}

/*	
 * Update the overlay transform resources to indicate the new status
 * of each plot in the overlay that is being restored.
 */
	for (i = 1; i < new_plot_count; i++) {
		nargs = 0;
		NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject, plot_ovl);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtfOverlayTrans,plot->trans.trans_obj);

		subret = NhlALSetValues(sub_recs[i]->plot->base.id,
					sargs,nargs); 
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;

	}
			

/*
 * If the new plot count is different from the old, (indicating that a plot
 * that was a member of the plot overlay was removed while it was a member
 * of the current overlay) use the same GenArray to send the new contents 
 * of the overlay record to the plot (and therefore to its overlay object).
 */
	if (new_plot_count != ga->num_elements) {
		ga->num_elements = ga->len_dimensions[0] = new_plot_count;
		NhlSetSArg(&sargs[nargs++],NhlNovOverlayRecs, ga);
	}


/*
 * Set the trans and overlay resources for the newly restored 
 * overlay base plot.
 */

	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayStatus,_tfCurrentOverlayBase);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject,plot_ovl);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayTrans,plot->trans.trans_obj);
	
	subret = NhlALSetValues(plot->base.id,sargs,nargs); 
	ret = MIN(subret,ret);
		  
/*
 * Free the GenArray -- since the my_data flag is set True,
 * the pointer array will be freed by the GenArray free call, but it is still
 * necessary to free each of the overlay records.
 */

	for (i = 0; i < new_plot_count; i++) {
		NhlFree(sub_recs[i]);
	}
	NhlFreeGenArray(ga); /* frees sub_recs since my_data is True */

	return ret;

}


/*
 * Function:	_NhlManageOverlay
 *
 * Description: Generic function that manages an overlay for a plot.
 *		Calls Initialize or SetValues for an overlay depending
 *		on the conditions. Additional resources may be set.
 *
 * In Args:	
 *		xnew		new instance record
 *		xold		old instance record if not initializing
 *		init		true if initialization
 *		sargs   	set args list
 *              nargs   	number of arguments
 *
 * Out Args:	overlay_object (in/out) pointer to private storage for the
 *				        overlay ptr
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
extern NhlErrorTypes _NhlManageOverlay
#if  __STDC__
(
	NhlLayer	*overlay_object,
	NhlLayer	lnew,
	NhlLayer	lold,
	NhlBoolean	init,
	NhlSArgList	sargs,
	int		nargs,
	char		*entry_name
)
#else 
(overlay_object,lnew,lold,init,sargs,nargs,entry_name)
	NhlLayer	*overlay_object;
	NhlLayer	lnew;
	NhlLayer	lold;
	NhlBoolean	init;
	NhlSArgList	sargs;
	int		nargs;
	char		*entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char 			*e_text;
	NhlTransformLayerPart	*tfp = &(((NhlTransformLayer)lnew)->trans);
	NhlTransformLayerPart	*otfp = &(((NhlTransformLayer)lold)->trans);
	NhlViewLayerPart		*vwp = &(((NhlViewLayer)lnew)->view);
	int			tmpid = -1;
	char			buffer[_NhlMAXFNAMELEN];
	NhlSArg			*lsargs;
	int			lsarg_count = 8; /* Keep up to date!!! */

	if (*overlay_object == NULL) {
		if (! tfp->overlay_plot_base)
			return ret;
		else if (! init) {
			e_text = "%s: resetting create-only resource: %s";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
				  entry_name,NhlNtfOverlayPlotBase);
			tfp->overlay_plot_base = False;
			return NhlWARNING;
		}
	}

	if (! init && ! _NhlIsTransform(*overlay_object)) {
		e_text = "%s: invalid overlay object passed in";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
		
	lsargs = (NhlSArg *) NhlMalloc((nargs+lsarg_count) * sizeof(NhlSArg));
	if (lsargs == NULL) {
		e_text ="%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (nargs > 0) {
		memcpy((void*)lsargs,(void*)sargs,nargs*sizeof(NhlSArg));
	}

	if (init) {

		strcpy(buffer,lnew->base.name);
		strcat(buffer,".Overlay");
		NhlSetSArg(&lsargs[nargs++],
			   NhlNvpUseSegments,vwp->use_segments);
		NhlSetSArg(&lsargs[nargs++],NhlNvpXF,vwp->x);
		NhlSetSArg(&lsargs[nargs++],NhlNvpYF,vwp->y);
		NhlSetSArg(&lsargs[nargs++],NhlNvpWidthF,vwp->width);
		NhlSetSArg(&lsargs[nargs++],NhlNvpHeightF,vwp->height);
		NhlSetSArg(&lsargs[nargs++],
			   NhlNtfOverlayStatus,_tfCurrentOverlayBase);
		NhlSetSArg(&lsargs[nargs++],NhlNtfOverlayTrans,tfp->trans_obj);
		
		tfp->overlay_trans_obj = tfp->trans_obj;
		subret = _NhlALCreateChild(&tmpid,buffer,NhloverlayLayerClass,
					   lnew,lsargs,nargs);

		ret = MIN(ret,subret);

		if (ret < NhlWARNING || (*overlay_object = 
				      _NhlGetLayer(tmpid)) == NULL) {
			tfp->overlay_trans_obj = NULL;
			e_text = "%s: overlay creation failure";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(NhlFATAL);
		}
		tfp->overlay_object = *overlay_object;
		tfp->overlay_status = _tfCurrentOverlayBase;
		return ret;
	}
	else if (tfp->overlay_plot_base == False) {

		if (tfp->overlay_status == _tfCurrentOverlayMember) {
			e_text = 
	       "%s: must remove from overlay before destroying overlay base";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			tfp->overlay_plot_base = True;
		}
		else {
			subret = _NhlDestroyChild((*overlay_object)->base.id,
						  lnew);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				return ret;
			}
			tfp->overlay_object = NULL;
			tfp->overlay_status = _tfNotInOverlay;
			tfp->overlay_trans_obj = NULL;
			*overlay_object = NULL;
		}
	}
	
/*
 * If this plot is an overlay member (not the base plot), its view should
 * be the same as that of the overlay. If it is not, the user must have
 * set it: in this case issue a warning and set it to the view of the
 * overlay.
 */
	
	if (tfp->overlay_status == _tfCurrentOverlayBase) {
		tfp->overlay_trans_obj = tfp->trans_obj;
	}
	else if (tfp->overlay_status == _tfCurrentOverlayMember) {

		NhlViewLayer ovvl = (NhlViewLayer) tfp->overlay_object;

		if (tfp->overlay_object == NULL || 
		    ! _NhlIsTransform(tfp->overlay_object)) {
			e_text = "%s: inconsistent overlay state";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(NhlFATAL);
		}

		if (vwp->x != ovvl->view.x ||
		    vwp->y != ovvl->view.y ||
		    vwp->width != ovvl->view.width ||
		    vwp->height != ovvl->view.height) {

			vwp->x = ovvl->view.x;
			vwp->y = ovvl->view.y;
			vwp->width = ovvl->view.width;
			vwp->height = ovvl->view.height;

			e_text =
			"%s: attempt to set overlay member plot view ignored";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		}
	}

		
	if (*overlay_object != NULL) {
		NhlViewLayerPart	*ovwp = &(((NhlViewLayer)lold)->view);

		if (vwp->x != ovwp->x)
			NhlSetSArg(&lsargs[nargs++],NhlNvpXF,vwp->x);
		if (vwp->y != ovwp->y)
			NhlSetSArg(&lsargs[nargs++],NhlNvpYF,vwp->y);
		if (vwp->width != ovwp->width)
			NhlSetSArg(&lsargs[nargs++],NhlNvpWidthF,vwp->width);
		if (vwp->height != ovwp->height)
			NhlSetSArg(&lsargs[nargs++],NhlNvpHeightF,vwp->height);

		if (tfp->overlay_object != otfp->overlay_object)
			NhlSetSArg(&lsargs[nargs++],
				   NhlNtfOverlayObject,tfp->overlay_object);
		if (tfp->overlay_status != otfp->overlay_status)
			NhlSetSArg(&lsargs[nargs++],
				   NhlNtfOverlayStatus,tfp->overlay_status);
		if (tfp->overlay_trans_obj != otfp->overlay_trans_obj)
			NhlSetSArg(&lsargs[nargs++],
				   NhlNtfOverlayTrans,tfp->overlay_trans_obj);
		
		subret = _NhlALSetValuesChild((*overlay_object)->base.id,
					      lnew,lsargs,nargs);

		if ((ret = MIN(subret, ret)) < NhlWARNING) {
			e_text = "%s: error setting overlay object view";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	NhlFree(lsargs);

	return ret;
}
