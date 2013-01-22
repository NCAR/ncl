/*
 *      $Id: PlotManager.c,v 1.74 2010-03-19 22:44:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		PlotManager.c
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
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/PlotManagerP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/CurvilinearTransObj.h>
#include <ncarg/hlu/SphericalTransObj.h>
#include <ncarg/hlu/TriMeshTransObj.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/AnnoManagerP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
/*
 * shouldn't be including the private Title data, but it's necessary to
 * solve the font height/class resource problem -- see note in ManageTitles
 */
#include <ncarg/hlu/TitleP.h>

#define	Oset(field)	NhlOffset(NhlPlotManagerLayerRec,plotmanager.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNpmOverlaySequenceIds,NhlCpmOverlaySequenceIds,NhlTObjIdGenArray,
         sizeof(NhlPointer),Oset(overlay_seq_ids),
         NhlTImmediate,_NhlUSET(NULL),_NhlRES_SGONLY,
         (NhlFreeFunc)NhlFreeGenArray},

/* Annotation resources  */

	{NhlNpmAnnoViews,NhlCpmAnnoViews,NhlTObjIdGenArray,
		sizeof(NhlPointer),Oset(anno_view_ids),
		NhlTImmediate,_NhlUSET(NULL),
         	_NhlRES_NORACCESS,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNpmAnnoManagers,NhlCpmAnnoManagers,NhlTObjIdGenArray,
		sizeof(NhlPointer),Oset(annomanager_ids),
		NhlTImmediate,_NhlUSET(NULL),_NhlRES_GONLY,
         	(NhlFreeFunc)NhlFreeGenArray},


	{ NhlNpmTitleDisplayMode,NhlCpmTitleDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_titles),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{ NhlNpmTitleZone,NhlCpmTitleZone,NhlTInteger,sizeof(int),
		  Oset(title_zone),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlOV_DEF_TITLE_ZONE),0,NULL},
	{ NhlNpmTickMarkDisplayMode,NhlCpmTickMarkDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_tickmarks),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{ NhlNpmTickMarkZone,NhlCpmTickMarkZone,NhlTInteger,sizeof(int),
		  Oset(tickmark_zone),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlOV_DEF_TICKMARK_ZONE),0,NULL},

/* LabelBar resources */

	{ NhlNpmLabelBarDisplayMode,NhlCpmLabelBarDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_labelbar),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{ NhlNpmLabelBarZone,NhlCpmLabelBarZone,NhlTInteger,sizeof(int),
		  Oset(labelbar_zone),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlOV_DEF_LABELBAR_ZONE),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(lbar_width_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNpmLabelBarWidthF, NhlCpmLabelBarWidthF,NhlTFloat, sizeof(float),
		  Oset(lbar_width),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(lbar_height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNpmLabelBarHeightF, NhlCpmLabelBarHeightF,NhlTFloat, 
		  sizeof(float),
		  Oset(lbar_height),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNpmLabelBarKeepAspect, NhlCpmLabelBarKeepAspect,NhlTBoolean, 
		  sizeof(NhlBoolean),Oset(lbar_keep_aspect),
		  NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNpmLabelBarSide, NhlCpmLabelBarSide, NhlTPosition, 
		 sizeof(NhlJustification),
		 Oset(lbar_side),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlRIGHT),0,NULL},
	{NhlNpmLabelBarParallelPosF,NhlCpmLabelBarParallelPosF,NhlTFloat,
		 sizeof(float),
		 Oset(lbar_para_pos),
		 NhlTString,_NhlUSET("0.5"),0,NULL},
	{NhlNpmLabelBarOrthogonalPosF,NhlCpmLabelBarOrthogonalPosF,NhlTFloat,
		 sizeof(float),
		 Oset(lbar_ortho_pos),
		 NhlTString,_NhlUSET("0.02"),0,NULL},

/* Legend resources */

	{ NhlNpmLegendDisplayMode,NhlCpmLegendDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_legend),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{ NhlNpmLegendZone,NhlCpmLegendZone,NhlTInteger,sizeof(int),
		  Oset(legend_zone),NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlOV_DEF_LEGEND_ZONE),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(lgnd_width_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNpmLegendWidthF, NhlCpmLegendWidthF,NhlTFloat, sizeof(float),
		  Oset(lgnd_width),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(lgnd_height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNpmLegendHeightF, NhlCpmLegendHeightF,NhlTFloat, 
		  sizeof(float),
		  Oset(lgnd_height),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{ NhlNpmLegendKeepAspect, NhlCpmLegendKeepAspect,NhlTBoolean, 
		  sizeof(NhlBoolean),Oset(lgnd_keep_aspect),
		  NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNpmLegendSide, NhlCpmLegendSide, NhlTPosition, 
		 sizeof(NhlPosition),
		 Oset(lgnd_side),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlBOTTOM),0,NULL},
	{NhlNpmLegendParallelPosF,NhlCpmLegendParallelPosF,NhlTFloat,
		 sizeof(float),
		 Oset(lgnd_para_pos),NhlTString,
		 _NhlUSET("0.5"),0,NULL},
	{NhlNpmLegendOrthogonalPosF,NhlCpmLegendOrthogonalPosF,NhlTFloat,
		 sizeof(float),
		 Oset(lgnd_ortho_pos),NhlTString,
		 _NhlUSET("0.02"),0,NULL},

/* End-documented-resources */

#if 0

/* Unimplemented resources */

	{ NhlNpmPreDrawOrder,NhlCpmPreDrawOrder,NhlTIntegerGenArray,
		  sizeof(NhlPointer),
		  Oset(pre_draw_order),
		  NhlTImmediate,_NhlUSET(NULL),_NhlRES_NOACCESS,
		  (NhlFreeFunc)NhlFreeGenArray},
	{ NhlNpmPostDrawOrder,NhlCpmPostDrawOrder,NhlTIntegerGenArray,
		  sizeof(NhlPointer),
		  Oset(post_draw_order),
		  NhlTImmediate,_NhlUSET(NULL),_NhlRES_NOACCESS,
		  (NhlFreeFunc)NhlFreeGenArray},

/* Bounding Box resources -- not implemented */

	{ NhlNpmFitToBB,NhlCpmFitToBB,NhlTBoolean,
		  sizeof(NhlBoolean),Oset(fit_to_bb),
		  NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{ NhlNpmBBLeftF, NhlCpmBBLeftF,NhlTFloat, sizeof(float),
		  Oset(bb_left),NhlTString,
		  _NhlUSET("0.0"),0,NULL},
	{ NhlNpmBBRightF, NhlCpmBBRightF,NhlTFloat, sizeof(float),
		  Oset(bb_right),NhlTString,
		  _NhlUSET("1.0"),0,NULL},
	{ NhlNpmBBBottomF,NhlCpmBBBottomF,NhlTFloat, sizeof(float),
		  Oset(bb_left),NhlTString,
		  _NhlUSET("0.0"),0,NULL},
	{ NhlNpmBBTopF, NhlCpmBBTopF,NhlTFloat, sizeof(float),
		  Oset(bb_left),NhlTString,
		  _NhlUSET("1.0"),0,NULL},
#endif

/* Private resources */

	{ NhlNpmPlotManagerRecs,NhlCpmPlotManagerRecs,NhlTGenArray,
		  sizeof(NhlPointer),
		  Oset(pm_rec_list),NhlTImmediate,
		  _NhlUSET(NULL),_NhlRES_PRIVATE,(NhlFreeFunc)NhlFreeGenArray},
	{ NhlNpmUpdateReq,NhlCpmUpdateReq,NhlTBoolean,
		  sizeof(NhlBoolean),
		  Oset(update_req),NhlTImmediate,
		  _NhlUSET((NhlPointer) False),_NhlRES_PRIVATE,NULL},
	{ NhlNpmUpdateAnnoReq,NhlCpmUpdateAnnoReq,NhlTBoolean,
		  sizeof(NhlBoolean),
		  Oset(update_anno_req),NhlTImmediate,
		  _NhlUSET((NhlPointer) False),_NhlRES_PRIVATE,NULL},
/*
 * Intercepted title resources
 */

	{NhlNtiMainOffsetXF,NhlCtiMainOffsetXF,NhlTFloat,sizeof(float),
		 Oset(ti_main_offset_x),
		 NhlTString,_NhlUSET("0.0"),_NhlRES_INTERCEPTED,NULL},
	{NhlNtiXAxisOffsetXF,NhlCtiXAxisOffsetXF,NhlTFloat,sizeof(float),
		 Oset(ti_x_axis_offset_x),
		 NhlTString,_NhlUSET("0.0"),_NhlRES_INTERCEPTED,NULL},
	{NhlNtiYAxisOffsetYF,NhlCtiYAxisOffsetYF,NhlTFloat,sizeof(float),
		 Oset(ti_y_axis_offset_y),
		 NhlTString,_NhlUSET("0.0"),_NhlRES_INTERCEPTED,NULL},
	{NhlNtiXAxisPosition,NhlCtiXAxisPosition,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(ti_x_axis_position),NhlTImmediate,
         	 _NhlUSET((NhlPointer)NhlCENTER),_NhlRES_INTERCEPTED,NULL},
	{NhlNtiYAxisPosition,NhlCtiYAxisPosition,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(ti_y_axis_position),NhlTImmediate,
         	 _NhlUSET((NhlPointer)NhlCENTER),_NhlRES_INTERCEPTED,NULL},
	{NhlNtiMainPosition,NhlCtiMainPosition,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(ti_main_position),NhlTImmediate,
         	 _NhlUSET((NhlPointer)NhlCENTER),_NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(ti_main_font_height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtiMainFontHeightF,NhlCFontHeightF,NhlTFloat,
		 sizeof(float),Oset(ti_main_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),
         	 _NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(ti_x_axis_font_height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtiXAxisFontHeightF,NhlCFontHeightF,NhlTFloat,
		 sizeof(float),Oset(ti_x_axis_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),
         	 _NhlRES_INTERCEPTED,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(ti_y_axis_font_height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtiYAxisFontHeightF,NhlCFontHeightF,
		 NhlTFloat,sizeof(float),Oset(ti_y_axis_font_height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),
         	 _NhlRES_INTERCEPTED,NULL},

/* intercepted LabelBar resources */

	{NhlNlbJustification, NhlClbJustification, NhlTJustification, 
		 sizeof(NhlJustification),
		 Oset(lbar_just),NhlTImmediate,
         	 _NhlUSET((NhlPointer)NhlCENTERCENTER),
         	 _NhlRES_INTERCEPTED,NULL},
	{NhlNlbOrientation, NhlClbOrientation, NhlTOrientation, 
		 sizeof(NhlOrientation),
		 Oset(lbar_orient),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlVERTICAL),
         	 _NhlRES_INTERCEPTED,NULL},

/* intercepted Legend resources */

	{NhlNlgJustification, NhlClgJustification, NhlTJustification, 
		 sizeof(NhlJustification),
		 Oset(lgnd_just),NhlTImmediate,
         	 _NhlUSET((NhlPointer)NhlCENTERCENTER),
         	 _NhlRES_INTERCEPTED,NULL},
	{NhlNlgOrientation, NhlClgOrientation, NhlTOrientation, 
		 sizeof(NhlOrientation),
		 Oset(lgnd_orient),NhlTImmediate,
         	_NhlUSET((NhlPointer)NhlVERTICAL),_NhlRES_INTERCEPTED,NULL},
        
/* blocked Transform resources */
        
	{ "no.res","No.res",NhlTFloat,sizeof(float),
          NhlOffset(NhlPlotManagerLayerRec,trans.x_min),NhlTString,
          _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTFloat,sizeof(float),
          NhlOffset(NhlPlotManagerLayerRec,trans.x_max),NhlTString,
          _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTAxisType,sizeof(NhlAxisType),
          NhlOffset(NhlPlotManagerLayerRec,trans.x_axis_type),NhlTImmediate,
          _NhlUSET((NhlPointer)NhlLINEARAXIS),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
          NhlOffset(NhlPlotManagerLayerRec,trans.x_log),NhlTImmediate,
          _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
          NhlOffset(NhlPlotManagerLayerRec,trans.x_reverse),NhlTImmediate,
          _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTFloat,sizeof(float),
          NhlOffset(NhlPlotManagerLayerRec,trans.y_min),NhlTString,
          _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTFloat,sizeof(float),
          NhlOffset(NhlPlotManagerLayerRec,trans.y_max),NhlTString,
          _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTAxisType,sizeof(NhlAxisType),
          NhlOffset(NhlPlotManagerLayerRec,trans.y_axis_type),NhlTImmediate,
          _NhlUSET((NhlPointer)NhlLINEARAXIS),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
          NhlOffset(NhlPlotManagerLayerRec,trans.y_log),NhlTImmediate,
          _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
          NhlOffset(NhlPlotManagerLayerRec,trans.y_reverse),NhlTImmediate,
          _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL}
        
};
#undef Oset

/* base methods */


static NhlErrorTypes PlotManagerClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes PlotManagerClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes PlotManagerInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes PlotManagerSetValues(
#if	NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes 	PlotManagerGetValues(
#if	NhlNeedProto
	NhlLayer	layer,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes PlotManagerDestroy(
#if	NhlNeedProto
        NhlLayer	inst
#endif
);

static NhlErrorTypes PlotManagerReparent(
#if	NhlNeedProto
	NhlLayer	layer,
	NhlLayer	new_parent
#endif
);

static NhlErrorTypes PlotManagerDraw(
#if	NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes PlotManagerPreDraw(
#if	NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes PlotManagerPostDraw(
#if	NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes PlotManagerGetBB(
#if	NhlNeedProto
        NhlLayer        instance,
        NhlBoundingBox	*thebox
#endif
);

/* internal static functions */

static NhlErrorTypes InternalGetBB(
#if	NhlNeedProto
        NhlLayer	instance,
	NhlBoundingBox	*thebox,
        int  	        zone,   
	char		*entry_name		   
#endif
);

static NhlErrorTypes AddSpecialZonesBB(
#if	NhlNeedProto
        NhlLayer	instance,
	NhlBoundingBox	*thebox,
	char		*entry_name		   
#endif
);

static NhlAnnoRec *RecordAnnotation(
#if	NhlNeedProto
	NhlPlotManagerLayer ovl,
	ovAnnoType	type,
	int		status,
	int		zone
#endif
);

static NhlErrorTypes ManageAnnotations(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,				       
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes ManageExtAnnotation(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
#endif
);

static NhlJustification ConstrainJustification(
#if	NhlNeedProto
	NhlAnnoRec	*anno_rec
#endif
);

static NhlErrorTypes UpdateAnnoData(
#if	NhlNeedProto
        NhlTransformLayer	plot,
	NhlAnnoRec		*anno_list,
	int			*max_zone,
	NhlString		entry_name
#endif
);

static NhlErrorTypes ManageTitles(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
#endif
);

static NhlErrorTypes ManageTickMarks(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
#endif
);

static NhlErrorTypes ManageLabelBar(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes ManageLegend(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes SetAnnoViews(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovl,
	NhlTransformLayer	plot,
	NhlAnnoRec	*anno_list,
	int		zone,
	NhlBoolean	*flags,
	NhlString	entry_name
#endif
);

static NhlErrorTypes SetTickMarkView(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
#endif
);

static NhlErrorTypes SetTitleView(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
#endif
);

static NhlErrorTypes SetExternalView(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
#endif
);

static NhlErrorTypes SetViewTracking(
#if	NhlNeedProto
	NhlPlotManagerLayer	ovl,
	NhlTransformLayer	plot,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
#endif
);

static NhlErrorTypes RestoreOverlayBase(
#if	NhlNeedProto
	NhlPlotManagerLayerPart	*ovp,
	int			plot_number
#endif
);

static NhlErrorTypes RemoveOverlayBase(
#if	NhlNeedProto
	NhlPlotManagerLayerPart	*ovp,
	int			plot_number
#endif
);

static NhlErrorTypes DissolveOverlay(
#if	NhlNeedProto
	NhlLayer		overlay_object
#endif
);


static NhlAnnoRec *UnregisterAnnotation(
#if	NhlNeedProto
        NhlLayer	plot,
	NhlLayer	annomanager,
	NhlString	entry_name
#endif
);

NhlPlotManagerClassRec NhlplotManagerClassRec = {
        {
/* class_name			*/      "plotManagerClass",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlPlotManagerLayerRec),
/* class_inited			*/      False,
/* superclass			*/  (NhlClass)&NhltransformClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	PlotManagerClassPartInitialize,
/* class_initialize		*/	PlotManagerClassInitialize,
/* layer_initialize		*/	PlotManagerInitialize,
/* layer_set_values		*/	PlotManagerSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	PlotManagerGetValues,
/* layer_reparent		*/	PlotManagerReparent,
/* layer_destroy		*/	PlotManagerDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      PlotManagerDraw,

/* layer_pre_draw		*/      PlotManagerPreDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      PlotManagerPostDraw,
/* layer_clear			*/      NULL

        },
	{
/* segment_wkid			*/	0,
/* get_bb			*/	PlotManagerGetBB
	},
	{
/* overlay_capability 		*/	_tfNotOverlayCapable,
/* data_to_ndc			*/	NhlInheritTransFunc,
/* ndc_to_data			*/	NhlInheritTransFunc,
/* data_polyline		*/	NhlInheritPolyTransFunc,
/* ndc_polyline			*/	NhlInheritPolyTransFunc,
/* data_polygon			*/	NhlInheritPolyTransFunc,
/* ndc_polygon			*/	NhlInheritPolyTransFunc,
/* data_polymarker		*/	NhlInheritPolyTransFunc,
/* ndc_polymarker		*/	NhlInheritPolyTransFunc
	},
	{
/* wkspace_list			*/	NULL
	}
};

NhlClass NhlplotManagerClass = 
		(NhlClass)&NhlplotManagerClassRec;

static NrmQuark Qoverlay_seq_ids;
static NrmQuark Qoverlay_recs;
static NrmQuark Qpre_draw_order;
static NrmQuark Qpost_draw_order;
static NrmQuark Qanno_views;
static NrmQuark Qannomanagers;

/* static variables referenced by the low-level library mapping functions */

static NhlLayer Trans_Obj = NULL;
static NhlLayer Plot = NULL;
static NhlLayer PlotManager_Trans_Obj = NULL;
static NhlLayer PlotManager_Plot = NULL;

/*
 * Function:	PlotManagerClassInitialize
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
PlotManagerClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
        _NhlEnumVals   annotationdisplaylist[] = {
		{NhlNOCREATE,		"NoCreate"},
		{NhlNEVER,		"Never"},
		{NhlCONDITIONAL,	"Conditional"},
		{NhlALWAYS,		"Always"},
		{NhlFORCEALWAYS,	"ForceAlways"},

        };

        _NhlRegisterEnumType(NhlviewClass,NhlTAnnotationDisplayMode,
		annotationdisplaylist,NhlNumber(annotationdisplaylist));

	Qoverlay_seq_ids = NrmStringToQuark(NhlNpmOverlaySequenceIds);
	Qoverlay_recs = NrmStringToQuark(NhlNpmPlotManagerRecs);
	Qpre_draw_order = NrmStringToQuark(NhlNpmPreDrawOrder);
	Qpost_draw_order = NrmStringToQuark(NhlNpmPostDrawOrder);
	Qanno_views = NrmStringToQuark(NhlNpmAnnoViews);
	Qannomanagers = NrmStringToQuark(NhlNpmAnnoManagers);

	return NhlNOERROR;
}

/*
 * Function:	PlotManagerClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlPlotManagerClassPart that cannot be initialized statically.
 *
 * In Args:	
 *		NhlClass	lc	NhlLayer Class to init
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

static NhlErrorTypes
PlotManagerClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* NhlLayer Class to init	*/
)
#else
(lc)
	NhlClass	lc;	/* NhlLayer Class to init	*/
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;

	subret = _NhlRegisterChildClass(lc,NhltitleClass,False,False,
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

	subret = _NhlRegisterChildClass(lc,NhltickMarkClass,
					False,False,
					NhlNtmXBDataLeftF,
					NhlNtmXBDataRightF,
					NhlNtmXBStyle,
					NhlNtmXBIrregularPoints,
					NhlNtmXBIrrTensionF,
					NhlNtmXTDataLeftF,
					NhlNtmXTDataRightF,
					NhlNtmXTStyle,
					NhlNtmXTIrregularPoints,
					NhlNtmXTIrrTensionF,
					NhlNtmYLDataBottomF,
					NhlNtmYLDataTopF,
					NhlNtmYLStyle,
					NhlNtmYLIrregularPoints,
					NhlNtmYLIrrTensionF,
					NhlNtmYRDataBottomF,
					NhlNtmYRDataTopF,
					NhlNtmYRStyle,
					NhlNtmYRIrregularPoints,
					NhlNtmYRIrrTensionF,
					NULL);

	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;
        
	subret = _NhlRegisterChildClass(lc,NhllegendClass,
					False,False,
					NhlNlgLegendOn,
					NhlNlgJustification,
					NhlNlgOrientation,
					NULL);

	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;

	subret = _NhlRegisterChildClass(lc,NhllabelBarClass,
					False,False,
					NhlNlbLabelBarOn,
					NhlNlbJustification,
					NhlNlbOrientation,
					NULL);

	return MIN(subret,ret);
}


/*
 * Function:	PlotManagerInitialize
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
PlotManagerInitialize
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer		req,
	NhlLayer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlClass      class;
        NhlLayer           req;
        NhlLayer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "PlotManagerInitialize";
	NhlPlotManagerLayer		ovnew = (NhlPlotManagerLayer) new;
	NhlPlotManagerLayerPart	*ovp = &(ovnew->plotmanager);
	NhlTransformLayer	parent = (NhlTransformLayer)ovnew->base.parent;
	NhlpmRec		*pm_rec;
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
	ovp->anno_alloc = 0;
	ovp->anno_count = 0;
	ovp->view_ids = NULL;
	ovp->anno_ids = NULL;

/*
 * Initialize unitialized resource values
 */

	if (! ovp->lbar_width_set)
		ovp->lbar_width = 0.18;
	if (! ovp->lbar_height_set)
		ovp->lbar_height = 0.6;
	if (! ovp->lgnd_width_set)
		ovp->lgnd_width = 0.55;
	if (! ovp->lgnd_height_set)
		ovp->lgnd_height = 0.18;

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
	ovnew->trans.do_ndc_overlay = parent->trans.do_ndc_overlay;
	ovnew->trans.overlay_trans_obj = ovnew->trans.trans_obj;
	ovp->trans_change_count = 0;
	ovp->trans_changed = True;

/*
 * Allocate an array to store pointers to the overlay records. 
 * Then allocate an array for the first member element.
 */

	if ((ovp->pm_recs = (NhlpmRec **) 
	     NhlMalloc(NhlOV_ALLOC_UNIT * sizeof(NhlpmRec *))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if ((pm_rec = (NhlpmRec *) 
	     NhlMalloc(NhlOV_ALLOC_UNIT * sizeof(NhlpmRec))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	ovp->overlay_alloc = NhlOV_ALLOC_UNIT;
	ovp->overlay_count = 1;
	pm_rec->plot = parent;
	pm_rec->ov_obj = new;
	pm_rec->anno_list = NULL;
	ovp->pm_recs[0] = pm_rec;

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
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
							entry_name);
			return NhlFATAL;
		}
	}
	if (ovp->display_labelbar > NhlNOCREATE){
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
		ovp->pm_recs[i] = NULL;
	
	ret = ManageAnnotations(ovnew,(NhlPlotManagerLayer)req,
				True,args,num_args);
 
	return ret;
}

static NhlErrorTypes RearrangePlotSequence
#if	NhlNeedProto
(
	NhlPlotManagerLayerPart	*ovp,
	char			*entry_name
)
#else
(ovp,entry_name)
	NhlPlotManagerLayerPart	*ovp;
	char			*entry_name;
#endif
{
        char *e_text;
        int i,j;
        int *seq_ids = (int *)ovp->overlay_seq_ids->data;

        if (seq_ids[0] != ovp->pm_recs[0]->plot->base.id) {
                e_text =
                      "%s: ignoring %s: base plot must be first in sequence";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
                          entry_name,NhlNpmOverlaySequenceIds);
                return NhlWARNING;
        }

/*
 * need to ensure both that all of the new sequence ids have entries in
 * the plot manager records and that each entry in the plot manager
 * records has a corresponding id in the seq_ids array. (There could be
 * duplicate seq_ids)
 */
        for (i = 0; i < ovp->overlay_count; i++) {
                NhlBoolean found_rec = False, found_id = False;

                for (j = 0; j < ovp->overlay_count; j++) {
                        if (ovp->pm_recs[j]->plot->base.id == seq_ids[i]) {
                                found_rec = True;
                                break;
                        }
                }
                for (j = 0; j < ovp->overlay_count; j++) {
                        if (seq_ids[j] == ovp->pm_recs[i]->plot->base.id) {
                                found_id = True;
                                break;
                        }
                }
                if (! (found_rec && found_id)) {
                        e_text =
                          "%s: ignoring %s: ids do not match overlay plot ids";
                        NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
                                  entry_name,NhlNpmOverlaySequenceIds);
                        return NhlWARNING;
                }
        }
        for (i = 0; i < ovp->overlay_count; i++) {
                NhlpmRec *pm_rec;
                
                for (j = 0; j < ovp->overlay_count; j++) {
                        if (ovp->pm_recs[j]->plot->base.id == seq_ids[i]) {
                                if (i == j)
                                        continue;
                                pm_rec = ovp->pm_recs[i];
                                ovp->pm_recs[i] = ovp->pm_recs[j];
                                ovp->pm_recs[j] = pm_rec;
                        }
                }
        }
        return NhlNOERROR;
                                
}

/*
 * Function:	PlotManagerSetValues
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
static NhlErrorTypes PlotManagerSetValues
#if	NhlNeedProto
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
	char			*entry_name = "PlotManagerSetValues";
	NhlPlotManagerLayer		ovnew = (NhlPlotManagerLayer) new;
	NhlPlotManagerLayer		ovold = (NhlPlotManagerLayer) old;
	NhlPlotManagerLayerPart	*ovp = &(ovnew->plotmanager);
	NhlTransformLayer	parent = (NhlTransformLayer)ovnew->base.parent;
	NhlTransformLayer       baseplotmanager = (NhlTransformLayer)ovnew->trans.overlay_object;
	int			i;
	int			trans_change_count;
	NhlBoolean		is_map = False;
	float			x,y,w,h;

	if (_NhlArgIsSet(args,num_args,NhlNpmLabelBarWidthF))
		ovp->lbar_width_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNpmLabelBarHeightF))
		ovp->lbar_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNpmLegendWidthF))
		ovp->lgnd_width_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNpmLegendHeightF))
		ovp->lgnd_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNpmLegendHeightF))
		ovp->lgnd_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtiMainFontHeightF))
		ovp->ti_main_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtiXAxisFontHeightF))
		ovp->ti_x_axis_font_height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtiYAxisFontHeightF))
		ovp->ti_y_axis_font_height_set = True;

	if (ovnew->trans.overlay_trans_obj != baseplotmanager->trans.overlay_trans_obj) {
		/*
		 * The baseplot may have changed its trans_obj.
		 */
		ovp->trans_changed = True;
		ovnew->trans.overlay_trans_obj = baseplotmanager->trans.overlay_trans_obj;
		parent->trans.overlay_trans_obj = baseplotmanager->trans.overlay_trans_obj;
	}

	subret = NhlVAGetValues(ovnew->trans.overlay_trans_obj->base.id,
				NhlNtrChangeCount,&trans_change_count,
				NULL);
	if (trans_change_count != ovp->trans_change_count ||
	    ovnew->trans.trans_obj != ovnew->trans.overlay_trans_obj)
		ovp->trans_changed = True;
	else
		ovp->trans_changed = False;
	
	ovnew->trans.trans_obj = ovnew->trans.overlay_trans_obj;
	ovp->trans_change_count = trans_change_count;
	ovnew->trans.do_ndc_overlay = parent->trans.do_ndc_overlay;
/* 
 * The annotation children can only be created during initialization;
 * If NOCREATE is set after annotation created, silently change it to NEVER
 */
	if (ovp->display_tickmarks != NhlNOCREATE &&
	    ovp->tickmarks == NULL) {
		ret = MIN(ret,NhlWARNING);
		e_text = 
		    "%s: TickMark annotation cannot be added after NhlCreate";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNpmTickMarkDisplayMode);
		ovp->display_tickmarks = NhlNOCREATE;
	}
        else if (ovp->tickmarks && ovp->display_tickmarks == NhlNOCREATE)
                ovp->display_tickmarks = NhlNEVER;
        

	if (ovp->display_titles != NhlNOCREATE &&
	    ovp->titles == NULL) {
		ret = MIN(ret,NhlWARNING);
		e_text = 
		 "%s: Title annotation cannot be added after NhlCreate";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNpmTitleDisplayMode);
		ovp->display_titles = NhlNOCREATE;
	}
        else if (ovp->titles && ovp->display_titles == NhlNOCREATE)
                ovp->display_titles = NhlNEVER;

	if (ovp->display_labelbar != NhlNOCREATE &&
	    ovp->labelbar == NULL) {
		ret = MIN(ret,NhlWARNING);
		e_text =
		 "%s: LabelBar annotation cannot be added after NhlCreate";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNpmLabelBarDisplayMode);
		ovp->display_labelbar = NhlNOCREATE;
	}
        else if (ovp->labelbar && ovp->display_labelbar == NhlNOCREATE)
                ovp->display_labelbar = NhlNEVER;

	if (ovp->display_legend != NhlNOCREATE &&
	    ovp->legend == NULL) {
		ret = MIN(ret,NhlWARNING);
		e_text =
		    "%s: Legend annotation cannot be added after NhlCreate";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNpmLegendDisplayMode);
		ovp->display_legend = NhlNOCREATE;
	}
        else if (ovp->legend && ovp->display_legend == NhlNOCREATE)
                ovp->display_legend = NhlNEVER;

/*
 * If the PlotManager records resource is set replace the old overlay record
 * pointer array with the new one.
 */
	if (_NhlArgIsSet(args,num_args,NhlNpmPlotManagerRecs)) {

		NhlpmRec	**pm_recs = 
					(NhlpmRec **) ovp->pm_rec_list->data;
		int	new_count = ovp->pm_rec_list->num_elements;
		
		if (pm_recs == NULL || ! _NhlIsTransform(pm_recs[0]->plot)) {
		     e_text = "%s: internally invalid resource";
		     NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		     return NhlFATAL;
		}
		if (new_count > ovp->overlay_alloc) {
			ovp->pm_recs = (NhlpmRec **)
				NhlRealloc(ovp->pm_recs, sizeof(NhlpmRec *) *
					   MAX(new_count,ovp->overlay_alloc + 
					       NhlOV_ALLOC_UNIT));
			if (ovp->pm_recs == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			ovp->overlay_alloc = MAX(new_count,ovp->overlay_alloc +
						 NhlOV_ALLOC_UNIT);
		}
		for (i = 0; i < MIN(new_count,ovp->overlay_count); i++) {
			memcpy((void*)ovp->pm_recs[i],(void*)pm_recs[i],
							sizeof(NhlpmRec));
		}
			
		for (i=MIN(new_count,ovp->overlay_count);i<new_count;i++) {
			if ((ovp->pm_recs[i] = (NhlpmRec *) 
			     NhlMalloc(sizeof(NhlpmRec))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			memcpy((void*)ovp->pm_recs[i],(void*)pm_recs[i],
							sizeof(NhlpmRec));
		}
		for (i = new_count; i < ovp->overlay_count; i++) {
			NhlFree(ovp->pm_recs[i]);
			ovp->pm_recs[i] = NULL;
		}
		ovp->overlay_count = new_count;
	}

        if (_NhlArgIsSet(args,num_args,NhlNpmOverlaySequenceIds)) {
                if (ovp->overlay_seq_ids->num_elements != ovp->overlay_count) {
                        ret = MIN(ret,NhlWARNING);
                        e_text = "%s: invalid number of elements in %s";
                        NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
                                  entry_name,NhlNpmOverlaySequenceIds);
                }
                else {
                        subret = RearrangePlotSequence(ovp,entry_name);
                        if ((ret = MIN(ret,subret)) < NhlWARNING)
                                return ret;
                }
        }

	subret = ManageAnnotations(ovnew,ovold,False,args,num_args);
	ret = MIN(ret,subret);

	if (ovnew->trans.overlay_status == _tfCurrentOverlayMember ||
	    ovp->overlay_count < 2) {
		ovp->update_req = False;
		ovp->update_anno_req = False;
		return ret;
	}

/*
 * Only a base plotmanager with overlay plots needs to execute
 * the remaining code.
 */

/*
 * If the base view has changed, modify the view of each overlay to 
 * match it. There is a special case if the transobj is a mapTrans and
 * the MapPlot mpShapeMode resource is set to FixedAspectNoFitBB. In this
 * case, NDCOverlays must be set to the location of the map data - not to
 * the viewport.
 */
	if (_NhlIsClass(ovnew->trans.trans_obj,NhlmapTransObjClass)) {
		float l,r,t,b;
		NhlVAGetValues(ovnew->trans.trans_obj->base.id,
			       NhlNmpLeftNDCF,&l,
			       NhlNmpRightNDCF,&r,
			       NhlNmpTopNDCF,&t,
			       NhlNmpBottomNDCF,&b,
			       NULL);
		x = l;
		y = t;
		w = r - l;
		h = t - b;
		is_map = True;
	}
/*
 * Ensure that all the base plots are up to date; don't pass the
 * update req resources if the plot has no PlotManager of its own.
 */
	for (i = 1; i < ovp->overlay_count; i++) {
		NhlTransformLayer	plot = ovp->pm_recs[i]->plot;
		NhlTransformLayerPart	*plot_tfp = &plot->trans;
		NhlPlotManagerLayer ovl = 
			(NhlPlotManagerLayer)ovp->pm_recs[i]->ov_obj;
		NhlSArg		sargs[16];
		int		nargs = 0;

		/*
		 * If the transformation has changed the annotations 
		 * associated with member overlay plots need to be updated.
		 */

		if (ovp->trans_changed) {
			plot_tfp->overlay_trans_obj = ovnew->trans.trans_obj;
		}
		if (ovl && (ovp->update_req || ovp->trans_changed))
			NhlSetSArg(&sargs[nargs++],NhlNpmUpdateReq,True);
		if (ovl && ovp->update_anno_req)
			NhlSetSArg(&sargs[nargs++],NhlNpmUpdateAnnoReq,True);

		if (is_map && plot_tfp->do_ndc_overlay == NhlNDCDATAEXTENT) {
			float cx,cy,cw,ch;
			NhlVAGetValues(plot->base.id,
				       NhlNvpXF,&cx,
				       NhlNvpYF,&cy,
				       NhlNvpWidthF,&cw,
				       NhlNvpHeightF,&ch,
				       NULL);
			if (x != cx)
				NhlSetSArg(&sargs[nargs++],NhlNvpXF,x);
			if (y != cy)
				NhlSetSArg(&sargs[nargs++],NhlNvpYF,y);
			if (w != cw)
				NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,w);
			if (h != ch)
				NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,h);
		}
		else {
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
		}
		if (ovnew->view.x != ovold->view.x) 
			NhlSetSArg(&sargs[nargs++],NhlNtfBaseXF,ovnew->view.x);
		if (ovnew->view.y != ovold->view.y)
			NhlSetSArg(&sargs[nargs++],NhlNtfBaseYF,ovnew->view.y);
		if (ovnew->view.width != ovold->view.width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtfBaseWidthF,ovnew->view.width);
		if (ovnew->view.height != ovold->view.height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtfBaseHeightF,ovnew->view.height);
		
		subret = NhlALSetValues(plot->base.id,sargs,nargs);
		if ((ret = MIN(subret, ret)) < NhlWARNING) {
			e_text = "%s: error setting overlay plot member view";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
#if 0
		if (ovl) {
			ovl->trans.bx = ovnew->view.x;
			ovl->trans.by = ovnew->view.y;
			ovl->trans.bw = ovnew->view.width;
			ovl->trans.bh = ovnew->view.height;
		}
#endif
	}

	ovp->update_req = False;
	ovp->update_anno_req = False;
		
	return ret;
}
#if 0
static NhlAnnoRec *CopyAnnoList
(
NhlAnnoRec *list
)
{
	NhlAnnoRec *anlp,*to_anlp,**to_list;

	if (! list)
		return NULL;

	to_list = &to_anlp;
	for (anlp = list; anlp; anlp = anlp->next) {
		to_anlp = NhlMalloc(sizeof(NhlAnnoRec));
		memcpy(to_anlp,anlp,sizeof(NhlAnnoRec));
		to_anlp = to_anlp->next;
	}
	return *to_list;
}
#endif

/*
 * Function:	PlotManagerGetValues
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
 *		NhlNpmOverlaySequenceIds
 *	The user is responsible for freeing this memory.
 */

static NhlErrorTypes	PlotManagerGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	NhlLayer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
	char			*entry_name = "PlotManagerGetValues";
	char			*e_text;
	NhlPlotManagerLayerPart	*ovp = &((NhlPlotManagerLayer) l)->plotmanager;
	int 			i;
	int			*ids;
	NhlGenArray		ga;
	NhlpmRec		**pm_recs;
	ng_size_t		j,count;

	for ( i = 0; i< num_args; i++ ) {

		if (args[i].quark == Qoverlay_seq_ids) {
			count = ovp->overlay_count;

			if ((ids = (int *) NhlMalloc(count * 
						     sizeof(int))) == NULL) {
				
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}

			for (j = 0; j < count; j++) {
				ids[j] = ovp->pm_recs[j]->plot->base.id; 
			}
			if ((ga = NhlCreateGenArray((NhlPointer)ids,
						    NhlTInteger,sizeof(int),
						    1, &count)) 
			    == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNpmOverlaySequenceIds);
				return NhlFATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
		else if (args[i].quark == Qoverlay_recs) {
			count = ovp->overlay_count;
			pm_recs = (NhlpmRec **) 
			      NhlMalloc(count * sizeof(NhlpmRec *));
			if (pm_recs == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}

			for (j = 0; j < count; j++) {

				pm_recs[j] = (NhlpmRec *)
					NhlMalloc(sizeof(NhlpmRec));
				if (pm_recs[j] == NULL) {
					e_text = 
					 "%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}

				memcpy((char*)pm_recs[j],
				       (char*)ovp->pm_recs[j],
				       sizeof(NhlpmRec));
				/*
				pm_recs[j]->anno_list = CopyAnnoList
					(ovp->pm_recs[j]->anno_list);
				*/
			}
			
			ga = NhlCreateGenArray((NhlPointer)pm_recs,
					       NhlTPointer,sizeof(NhlpmRec *),
					       1,  &count);
			if (ga == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNpmPlotManagerRecs);
				return NhlFATAL;
			}

			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
		else if (args[i].quark == Qanno_views) {

			count = ovp->anno_count;
			if (count) {
				ids = (int *) NhlMalloc(count * sizeof(int));

				if (ids == NULL) {
					e_text = 
					 "%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				memcpy(ids,ovp->view_ids,count * sizeof(int));

				if ((ga = NhlCreateGenArray
				     ((NhlPointer)ids,NhlTInteger,sizeof(int),
				      1,&count)) == NULL) {
					e_text = 
					     "%s: error creating %s GenArray";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name,
						  NhlNpmAnnoViews);
					return NhlFATAL;
				}
				ga->my_data = True;
			}
			*((NhlGenArray *)(args[i].value.ptrval)) = 
				count ? ga : NULL;

		}
		else if (args[i].quark == Qannomanagers) {
			count = ovp->anno_count;
			if (count) {
				ids = (int *) NhlMalloc(count * sizeof(int));

				if (ids == NULL) {
					e_text = 
					"%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				memcpy(ids,ovp->anno_ids,count * sizeof(int));

				if ((ga = NhlCreateGenArray
				     ((NhlPointer)ids,NhlTInteger,sizeof(int),
				      1,&count)) == NULL) {
					e_text = 
					     "%s: error creating %s GenArray";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name,
						  NhlNpmAnnoManagers);
					return NhlFATAL;
				}
				ga->my_data = True;
			}
			*((NhlGenArray *)(args[i].value.ptrval)) = 
				count ? ga : NULL;
		}
	}
	return(NhlNOERROR);
}

/*
 * Function:	PlotManagerDestroy
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
static NhlErrorTypes PlotManagerDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "PlotManagerDestroy";
	char			*e_text;
	NhlPlotManagerLayerPart	*ovp = &((NhlPlotManagerLayer) inst)->plotmanager;
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
 * Remove any annotations added by the user
 */

	while (ovp->anno_count > 0) {
		subret = _NhlRemoveAnnotation(inst,
			      _NhlGetLayer(ovp->anno_ids[ovp->anno_count - 1]),
					   entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
	}
	if (ovp->view_ids != NULL) {
		NhlFree(ovp->view_ids);
		NhlFree(ovp->anno_ids);
	}

/*
 * Free the overlay base record and the overlay record pointer array,
 * including each element's annotation list. External annotations 
 * belonging to the base overlay must be informed that they are no 
 * longer part of an overlay.
 * Destroy intrinsic child annotations
 * Free the irregular point gen arrays.
 */
	for (i=0; i < ovp->overlay_count; i++) {
		NhlAnnoRec	*anlp = ovp->pm_recs[i]->anno_list;
		while (anlp != NULL) {
			NhlAnnoRec *free_anno = anlp;
			anlp = anlp->next;
			if (i == 0) {
				if (free_anno->type == ovEXTERNAL) {
					NhlVASetValues(free_anno->anno_id,
						       NhlNamOverlayId,
						       NhlNULLOBJID,NULL);
				}
				else if (free_anno->status > NhlNOCREATE) {
					(void) _NhlDestroyChild(
						  free_anno->plot_id,inst);
				}
			}
			NhlFree(free_anno);
		}
		NhlFree(ovp->pm_recs[i]);
	}

	NhlFree(ovp->pm_recs);
	NhlFreeGenArray(ovp->x_irr);
	NhlFreeGenArray(ovp->y_irr);

	return(ret);
}


/*
 * Function:	PlotManagerReparent
 *
 * Description:
 *
 * In Args:	layer		PlotManager layer
 *		new_parent	new parent
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes PlotManagerReparent
#if	NhlNeedProto
(
	NhlLayer layer,
	NhlLayer new_parent
)
#else
(layer,new_parent)
	NhlLayer layer;
	NhlLayer new_parent;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
#if 0
	char			*e_text;
	char			*entry_name = "PlotManagerReparent";
#endif
	NhlPlotManagerLayer		ovl = (NhlPlotManagerLayer) layer;
	NhlPlotManagerLayerPart	*ovp = &(ovl->plotmanager);
	int			i;

/*
 * Note that children of the PlotManager are reparented automatically;
 * however, Member Plots and External Annotations need to be 
 * reparented explicitly.
 * Only the PlotManager Base Plot should do this.
 * All external annotations should be top level.
 */
	if (ovl->trans.overlay_status == _tfCurrentOverlayMember)
		return NhlNOERROR;

	for (i = 0; i < ovp->overlay_count; i++) {
		NhlAnnoRec	*anlp = ovp->pm_recs[i]->anno_list;
		
/*
 * Reparent member plot
 */
		if (i > 0) {
			subret = _NhlReparent((NhlLayer)ovp->pm_recs[i]->plot,
					      layer->base.wkptr);
			if (MIN(ret,subret) < NhlWARNING) return ret;
		}
/*
 * Reparent any external annotation whose parent is a Workstation.
 */
		for ( ; anlp != NULL; anlp = anlp->next) {

			if (anlp->type == ovEXTERNAL && 
			    anlp->plot_id != NhlNULLOBJID) {
				NhlLayer plot = _NhlGetLayer(anlp->plot_id);

				if (_NhlIsWorkstation(plot->base.parent)) {
					subret = _NhlReparent(plot,
							 layer->base.wkptr);
					if (MIN(ret,subret) < NhlWARNING) 
						return ret;
				}
			}
		}
	}
	return ret;

}

/*
 * Function:	PlotManagerPreDraw
 *
 * Description: Performs the "background" draw for an overlay plot
 *
 * In Args:	layer	PlotManager instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes PlotManagerPreDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "PlotManagerPreDraw";
	NhlPlotManagerLayer		ovl = (NhlPlotManagerLayer) layer;
	NhlPlotManagerLayerPart	*ovp = &(ovl->plotmanager);
	int			i,j;
	NhlBoolean		flags[4];
	int			max_zone;

/*
 * Update the annotation data 
 */
	for (max_zone = 0, i = 0; i < ovp->overlay_count; i++) {
		subret = UpdateAnnoData(ovp->pm_recs[i]->plot,
                                        ovp->pm_recs[i]->anno_list,
					&ovp->pm_recs[i]->max_zone,
					entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ovp->pm_recs[i]->max_zone > max_zone) 
			max_zone = ovp->pm_recs[i]->max_zone;
	}
/*
 * Modify the annotation positions based on the current zonal information
 */
	flags[0] = flags[1] = flags[2] = flags[3] = False;
	for (i = 0; i <= max_zone; i++) {
		for (j = 0; j < ovp->overlay_count; j++) {
                        if (! _NhlViewOn((NhlLayer)ovp->pm_recs[j]->plot))
                                continue;
			subret = SetAnnoViews(ovl,ovp->pm_recs[j]->plot,
					      ovp->pm_recs[j]->anno_list,
					      i,flags,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		}
	}

/*
 * Set the overlay trans.
 */

	PlotManager_Trans_Obj = ovl->trans.overlay_trans_obj;
	PlotManager_Plot = (NhlLayer) ovp->pm_recs[0]->plot;
	if (! _NhlIsTransObj(PlotManager_Trans_Obj) || 
	    ! _NhlIsTransform(PlotManager_Plot)) {
		e_text = "%s: invalid trans object or plot class";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	subret = _NhlSetTrans(PlotManager_Trans_Obj,PlotManager_Plot);
        if ((ret = MIN(ret,subret)) < NhlWARNING) {
                e_text = "%s: error setting transformation";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
                return(ret);
        }

/*
 * Call the pre-draw methods for each plot object in turn.
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		NhlTransformLayerPart	*tfp;

		Trans_Obj = ovp->pm_recs[i]->plot->trans.trans_obj;
		Plot = (NhlLayer) ovp->pm_recs[i]->plot;

		subret = _NhlPreDraw(Plot);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error in plot pre-draw";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
		tfp = &((NhlTransformLayer)Plot)->trans;
		if (tfp->do_ndc_overlay) {
			subret = _NhlSetTrans(PlotManager_Trans_Obj,
					      PlotManager_Plot);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = "%s: error setting transformation";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text, entry_name);
				return(ret);
			}
		}
	}
	return ret;
}


/*
 * Function:	PlotManagerDraw
 *
 * Description:
 *
 * In Args:	layer	PlotManager instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes PlotManagerDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "PlotManagerDraw";
	NhlPlotManagerLayer		ovl = (NhlPlotManagerLayer) layer;
	NhlPlotManagerLayerPart	*ovp = &(ovl->plotmanager);
	int			i;

/*
 * Set the overlay trans.
 */

	PlotManager_Trans_Obj = ovl->trans.overlay_trans_obj;
	PlotManager_Plot = (NhlLayer) ovp->pm_recs[0]->plot;
	if (! _NhlIsTransObj(PlotManager_Trans_Obj) || 
	    ! _NhlIsTransform(PlotManager_Plot)) {
		e_text = "%s: invalid trans object or plot class";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	subret = _NhlSetTrans(PlotManager_Trans_Obj,PlotManager_Plot);
        if ((ret = MIN(ret,subret)) < NhlWARNING) {
                e_text = "%s: error setting transformation";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
                return(ret);
        }

/*
 * Call the draw methods for each plot object in turn. 
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		NhlTransformLayerPart	*tfp;

		Trans_Obj = ovp->pm_recs[i]->plot->trans.trans_obj;
		Plot = (NhlLayer) ovp->pm_recs[i]->plot;

		subret = _NhlDraw(Plot);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error in plot draw";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
		tfp = &((NhlTransformLayer)Plot)->trans;
		if (tfp->do_ndc_overlay) {
			subret = _NhlSetTrans(PlotManager_Trans_Obj,
					      PlotManager_Plot);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = "%s: error setting transformation";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text, entry_name);
				return(ret);
			}
		}
	}

	return ret;
}


/*
 * Function:	PlotManagerPostDraw
 *
 * Description:
 *
 * In Args:	layer	PlotManager instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes PlotManagerPostDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "PlotManagerPostDraw";
	NhlPlotManagerLayer		ovl = (NhlPlotManagerLayer) layer;
	NhlPlotManagerLayerPart	*ovp = &(ovl->plotmanager);
	int			i;
	NhlBoolean		tickmarks_done = False,
				titles_done = False,
				labelbar_done = False,
				legend_done = False;

/*
 * Set the overlay trans.
 */

	PlotManager_Trans_Obj = ovl->trans.overlay_trans_obj;
	PlotManager_Plot = (NhlLayer) ovp->pm_recs[0]->plot;
	if (! _NhlIsTransObj(PlotManager_Trans_Obj) || 
	    ! _NhlIsTransform(PlotManager_Plot)) {
		e_text = "%s: invalid trans object or plot class";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	subret = _NhlSetTrans(PlotManager_Trans_Obj,PlotManager_Plot);
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
		NhlTransformLayerPart	*tfp;

		Trans_Obj = ovp->pm_recs[i]->plot->trans.trans_obj;
		Plot = (NhlLayer) ovp->pm_recs[i]->plot;

		subret = _NhlPostDraw(Plot);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error in plot post-draw";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
		tfp = &((NhlTransformLayer)Plot)->trans;
		if (tfp->do_ndc_overlay) {
			subret = _NhlSetTrans(PlotManager_Trans_Obj,
					      PlotManager_Plot);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = "%s: error setting transformation";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text, entry_name);
				return(ret);
			}
		}
	}
	for (i = 0; i < ovp->overlay_count; i++) {
		NhlAnnoRec	*anlp = ovp->pm_recs[i]->anno_list;

                if (! _NhlViewOn((NhlLayer) ovp->pm_recs[i]->plot))
                        continue;
		for ( ; anlp != NULL; anlp = anlp->next) {

			NhlLayer view;
			if (! anlp->viewable)
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
			if (anlp->plot_id == NhlNULLOBJID)
				continue;
			view = _NhlGetLayer(anlp->plot_id);
			if (view == NULL || ! _NhlIsView(view)) {
				e_text = "%s: invalid annotation";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return(NhlFATAL);
			}
			subret = _NhlPlotManagerDraw(view);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				e_text = "%s: error drawing annotation";
				NhlPError(ret,
					  NhlEUNKNOWN,e_text,entry_name);
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
 * Function:    PlotManagerGetBB
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
static NhlErrorTypes PlotManagerGetBB
#if	NhlNeedProto
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	char			*entry_name = "PlotManagerGetBB";
	char			*e_text;
	NhlPlotManagerLayer		ovl = (NhlPlotManagerLayer) instance;
	NhlPlotManagerLayerPart	*ovp = &(ovl->plotmanager);
	int			max_zone;
	int			i,j;
	NhlBoolean		flags[4];


/*
 * Update the annotation data 
 */
	for (max_zone = 0, i = 0; i < ovp->overlay_count; i++) {
		subret = UpdateAnnoData(ovp->pm_recs[i]->plot,
                                        ovp->pm_recs[i]->anno_list,
					&ovp->pm_recs[i]->max_zone,
					entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ovp->pm_recs[i]->max_zone > max_zone) 
			max_zone = ovp->pm_recs[i]->max_zone;
	}
/*
 * Modify the annotation positions based on the current zonal information
 */
	flags[0] = flags[1] = flags[2] = flags[3] = False;
	for (i = 0; i <= max_zone; i++) {
		for (j = 0; j < ovp->overlay_count; j++) {
                        if (! _NhlViewOn((NhlLayer) ovp->pm_recs[j]->plot))
                                continue;
			subret = SetAnnoViews(ovl,ovp->pm_recs[j]->plot,
					      ovp->pm_recs[j]->anno_list,
					      i,flags,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		}
	}

	ret = InternalGetBB(instance,thebox,max_zone,entry_name);
	ret = AddSpecialZonesBB(instance,thebox,entry_name);

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
#if	NhlNeedProto
(
	NhlPlotManagerLayer ovl,
	ovAnnoType	type,
	int		status,
	int		zone
)
#else
(ovl,type,status,zone)
	NhlPlotManagerLayer	ovl;
	ovAnnoType	type;
	int		status;
	int		zone;
#endif
{
	char			*e_text;
	char			*entry_name = "PlotManagerInitialize";
	NhlPlotManagerLayerPart	*ovp = &(ovl->plotmanager);
	NhlAnnoRec		*anno_rec, *anlp;
	NhlTransformLayerPart	*tfp = &(ovl->trans);
	NhlPlotManagerLayer	base_pm = NULL;

	if (tfp->overlay_status == _tfCurrentOverlayMember)
		base_pm = (NhlPlotManagerLayer) tfp->overlay_object;

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
	anno_rec->anno_id = NhlNULLOBJID;
	anno_rec->plot_id = NhlNULLOBJID;
	anno_rec->zone = zone;
	anno_rec->type = type;
	anno_rec->status = status;
	anno_rec->next = NULL;
	anno_rec->track_data = False;
	anno_rec->out_of_range = False;
	anno_rec->resize_notify = False;
	anlp = ovp->pm_recs[0]->anno_list;

	if (anlp == NULL) {
		ovp->pm_recs[0]->anno_list = anno_rec;
		return anno_rec;
	}
	else if (anlp->zone <= anno_rec->zone) {
		anno_rec->next = anlp;
		ovp->pm_recs[0]->anno_list = anno_rec;
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

	if (base_pm) {
		NhlpmRec **pm_recs = base_pm->plotmanager.pm_recs;
		int i;

		for (i = 0; i < base_pm->plotmanager.overlay_count; i++) {
			if (pm_recs[i]->ov_obj == (NhlLayer)ovl) {
				pm_recs[i]->anno_list = 
					ovp->pm_recs[0]->anno_list;
				break;
			}
		}
	}

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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovl,
	NhlTransformLayer	plot,
	NhlAnnoRec	*anno_list,
	int		zone,
	NhlBoolean	*flags,
	NhlString	entry_name
)
#else
(ovl,plot,anno_list,zone,flags,entry_name)
	NhlPlotManagerLayer	ovl;
	NhlTransformLayer	plot;
	NhlAnnoRec	*anno_list;
	int		zone;
	NhlBoolean	*flags;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlAnnoRec		*anlp;

/*
 * Flags tells which each intrinsic annotations has been completed;
 * indexes arranged as follows: 0, tickmarks; 1, titles; 2, labelbar;
 * 3, legend.
 */

	for (anlp = anno_list; anlp != NULL; anlp = anlp->next) {
		
		if (anlp->zone != zone)
			continue;
		else if (anlp->status <= NhlNEVER) {
                        anlp->viewable = False;
			continue;
                }
		else if (anlp->status == NhlCONDITIONAL) {
			switch (anlp->type) {
			case ovTICKMARK:
				if (flags[0]) continue;
				break;
			case ovTITLE:
				if (flags[1]) continue;
				break;
			case ovLEGEND:
				if (flags[2]) continue;
				break;
			case ovLABELBAR:
				if (flags[3]) continue;
				break;
			case ovEXTERNAL:
			default:
				break;
			}
		}
                anlp->viewable = True;
		switch (anlp->type) {
		case ovTICKMARK:
			subret = SetTickMarkView(ovl,anlp,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
			flags[0] = True;
			break;
		case ovTITLE:
			SetTitleView(ovl,anlp,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
                        if (anlp->viewable)
                                flags[1] = True;
			break;
		case ovLEGEND:
			SetExternalView(ovl,anlp,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
			flags[2] = True;
			break;
		case ovLABELBAR:
			SetExternalView(ovl,anlp,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
			flags[3] = True;
			break;
		case ovEXTERNAL:
			if (anlp->track_data) {
				SetViewTracking(ovl,plot,anlp,entry_name);
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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
)
#else
(ovl,anno_rec,entry_name)
	NhlPlotManagerLayer	ovl;
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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
)
#else
(ovl,anno_rec,entry_name)
	NhlPlotManagerLayer	ovl;
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
	NhlPlotManagerLayer	an_ovl;
	int			zone;
        NhlBoolean		main_on,x_on,y_on;
	
/*
 * Get relevant title attributes
 */
	subret = NhlVAGetValues(anno_rec->plot_id,
                                NhlNtiMainOn,&main_on,
                                NhlNtiXAxisOn,&x_on,
                                NhlNtiYAxisOn,&y_on,
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
	an_ovl = (NhlPlotManagerLayer) anno_rec->ovl;

	if ((ret = MIN(subret,ret)) < NhlNOERROR) {
		e_text = "%s: Error getting Title values";
		NhlPError(ret,NhlEUNKNOWN,e_text, entry_name);
		if (ret < NhlWARNING) return ret;
	}
        if (! (main_on || x_on || y_on)) {
                anno_rec->viewable = False;
                return ret;
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
	
	switch(an_ovl->plotmanager.ti_main_position) {
	case NhlTOP:
	case NhlBOTTOM:
	default:
		e_text = 
    "%s: Invalid value for Main axis title position, defaulting to NhlCENTER";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
		ret = MIN(NhlWARNING,ret);
		an_ovl->plotmanager.ti_main_position = NhlCENTER;
	case NhlCENTER:
		set_main_off_x = an_ovl->plotmanager.ti_main_offset_x +
			((ovl->view.x + ovl->view.width/2.0) -
			 (x_pos + width/2.0));
		break;
	case NhlLEFT:
		set_main_off_x = an_ovl->plotmanager.ti_main_offset_x +
			(ovl->view.x - x_pos);
		break;
	case NhlRIGHT:
		set_main_off_x = an_ovl->plotmanager.ti_main_offset_x +
			((ovl->view.x + ovl->view.width) - 
			 (x_pos + width));
		break;
	}

	switch(an_ovl->plotmanager.ti_x_axis_position) {
	case NhlTOP:
	case NhlBOTTOM:
	default:
		e_text = 
    "%s: Invalid value for X-Axis title position, defaulting to NhlCENTER";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
		ret = MIN(NhlWARNING,ret);
		an_ovl->plotmanager.ti_x_axis_position = NhlCENTER;
	case NhlCENTER:
		set_x_off_x = an_ovl->plotmanager.ti_x_axis_offset_x +
			((ovl->view.x + ovl->view.width/2.0) -
			 (x_pos + width/2.0));
		break;
	case NhlLEFT:
		set_x_off_x = an_ovl->plotmanager.ti_x_axis_offset_x +
			(ovl->view.x - x_pos);
		break;
	case NhlRIGHT:
		set_x_off_x = an_ovl->plotmanager.ti_x_axis_offset_x +
			((ovl->view.x + ovl->view.width) - 
			 (x_pos + width));
		break;
	}

	switch(an_ovl->plotmanager.ti_y_axis_position) {
	case NhlLEFT:
	case NhlRIGHT:
	default:
		e_text = 
    "%s: Invalid value for Y-Axis title position, defaulting to NhlCENTER";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
		ret = MIN(NhlWARNING,ret);
		an_ovl->plotmanager.ti_y_axis_position = NhlCENTER;
	case NhlCENTER:
		set_y_off_y = an_ovl->plotmanager.ti_y_axis_offset_y +
			((ovl->view.y - ovl->view.height/2.0) -
			 (y_pos - height/2.0));
		break;
	case NhlTOP:
		set_y_off_y = an_ovl->plotmanager.ti_y_axis_offset_y +
			(ovl->view.y - y_pos);
		break;
	case NhlBOTTOM:
		set_y_off_y = an_ovl->plotmanager.ti_y_axis_offset_y + 
			((ovl->view.y - ovl->view.height) - 
			 (y_pos - height));
		break;
	}

/*
 * Reset the title if necessary
 */	
	if (x_pos != x_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,x_pos);
	if (y_pos != y_vp)
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,y_pos);
	if (width != width_vp) {
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,width);
		NhlSetSArg(&sargs[nargs++],NhlNtiMainFontHeightF,
			   an_ovl->plotmanager.ti_main_font_height);
		NhlSetSArg(&sargs[nargs++],NhlNtiXAxisFontHeightF,
			   an_ovl->plotmanager.ti_x_axis_font_height);
	}
	if (height != height_vp) {
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,height);
		NhlSetSArg(&sargs[nargs++],NhlNtiYAxisFontHeightF,
			   an_ovl->plotmanager.ti_y_axis_font_height);
	}
	
	if (set_main_off_x != get_main_off_x)
	    NhlSetSArg(&sargs[nargs++],NhlNtiMainOffsetXF,set_main_off_x);
	if (set_x_off_x != get_x_off_x)
		NhlSetSArg(&sargs[nargs++],NhlNtiXAxisOffsetXF,set_x_off_x);
	if (set_y_off_y != get_y_off_y)
		NhlSetSArg(&sargs[nargs++],NhlNtiYAxisOffsetYF,set_y_off_y);

	if (an_ovl->plotmanager.ti_main_position != main_pos)
		NhlSetSArg(&sargs[nargs++],NhlNtiMainPosition,
			   an_ovl->plotmanager.ti_main_position);
	if (an_ovl->plotmanager.ti_x_axis_position != x_axis_pos)
		NhlSetSArg(&sargs[nargs++],NhlNtiXAxisPosition,
			   an_ovl->plotmanager.ti_x_axis_position);
	if (an_ovl->plotmanager.ti_y_axis_position != y_axis_pos)
		NhlSetSArg(&sargs[nargs++],NhlNtiYAxisPosition,
			   an_ovl->plotmanager.ti_y_axis_position);

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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovl,
	NhlTransformLayer	plot,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
)
#else
(ovl,plot,anno_rec,entry_name)
	NhlPlotManagerLayer	ovl;
	NhlTransformLayer	plot;
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
        
	NhlDataToNDC(plot->base.id,
		     &anno_rec->data_x,&anno_rec->data_y,1,
		     &x_pos,&y_pos,NULL,NULL,&status,&oo_range);
	if (status) {
#if 0
		NhlLayer anl = _NhlGetLayer(anno_rec->anno_id);

		e_text = 
	     "%s: annotation \"%s\" track point not in range: cannot display";
		ret = MIN(ret,NhlWARNING);
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,anl->base.name);
#endif
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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovl,
	NhlAnnoRec	*anno_rec,
	NhlString	entry_name
)
#else
(ovl,anno_rec,entry_name)
	NhlPlotManagerLayer	ovl;
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
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlBOTTOMRIGHT) {
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->zone > 1)
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
#if	NhlNeedProto
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
	NhlPlotManagerLayer		ovl = (NhlPlotManagerLayer) instance;
	NhlPlotManagerLayerPart	*ovl_basep;
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
 * First find the base PlotManager, then search through the annotation
 * record of each member overlay and add the bounding boxes of each annotation
 * inside the requested zone. Exclude any objects not currently displayed.
 */
	ovl_basep = 
		&((NhlPlotManagerLayer)ovl->trans.overlay_object)->plotmanager;

	for (i = 0; i < ovl_basep->overlay_count; i++) {
		NhlAnnoRec *anno_list = ovl_basep->pm_recs[i]->anno_list;

                if (! _NhlViewOn((NhlLayer) ovl_basep->pm_recs[i]->plot))
                                continue;
		for ( ; anno_list != NULL; anno_list = anno_list->next) {
			if (anno_list->plot_id <= NhlNULLOBJID)
				continue;
			else if (anno_list->zone > zone || anno_list->zone < 2)
				continue;
			else if (! anno_list->viewable)
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
 * Function:    AddSpecialZonesBB
 *
 * Description: The BBs of zone 0 and 1 annotations do not affect the placement
 *              of outer zones, but need to be accounted for in the final 
 *              BoundingBox calculation.
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
static NhlErrorTypes AddSpecialZonesBB
#if	NhlNeedProto
(
	NhlLayer	instance,
	NhlBoundingBox	*thebox,
	char		*entry_name
)
#else
(instance,thebox,entry_name)
	NhlLayer	instance;
	NhlBoundingBox	*thebox;
	char		*entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlPlotManagerLayer		ovl = (NhlPlotManagerLayer) instance;
	NhlPlotManagerLayerPart	*ovl_basep;
	int			i;
	
/*
 * First find the base PlotManager, then search through the annotation
 * record of each member overlay and add the bounding boxes of each annotation
 * inside the requested zone. Exclude any objects not currently displayed.
 */
	ovl_basep = 
		&((NhlPlotManagerLayer)ovl->trans.overlay_object)->plotmanager;

	for (i = 0; i < ovl_basep->overlay_count; i++) {
		NhlAnnoRec *anno_list = ovl_basep->pm_recs[i]->anno_list;

                if (! _NhlViewOn((NhlLayer) ovl_basep->pm_recs[i]->plot))
                                continue;
		for ( ; anno_list != NULL; anno_list = anno_list->next) {
			if (anno_list->plot_id <= NhlNULLOBJID ||
			    anno_list->zone > 1 ||
			    ! anno_list->viewable ||
			    anno_list->status != NhlALWAYS)
				continue;
			subret = _NhlGetBB(_NhlGetLayer(anno_list->plot_id),
					   thebox);
			if ((ret = MIN(subret,ret)) < NhlNOERROR) {
				e_text = 
				"%s: Error getting annotation bounding box";
				NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
				if (ret < NhlWARNING) return ret;
			}
		}
	}
	return ret;

}

/*
 * Function:	SetAnnotations
 *
 * Description: Sets up annotations specified in the ovAnnoViews resource
 *
 * In Args:	NhlPlotManagerLayerPart	*ovp - pointer to the overlay part
 *		int			init - called from Initialize?
 *		NhlString		entry_name
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
SetAnnotations
#if	NhlNeedProto
(
	NhlPlotManagerLayer		ovl,
	NhlBoolean		init,
	NhlString		entry_name
)
#else
(ovl,init,entry_name)
	NhlPlotManagerLayer		ovl;
	NhlBoolean		init;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString	e_text;
	NhlPlotManagerLayerPart	*ovp = &ovl->plotmanager;
	int 		count = ovp->anno_view_ids->num_elements;
	int 		*idp = (int *) ovp->anno_view_ids->data;
	int 		i, j, anno_id;
	int		*save_view_ids, *save_anno_ids;
	int		lanno_count;

	if (init || ovp->anno_count == 0) {
		for (i = 0; i < count; i++) {
			NhlLayer view = _NhlGetLayer(idp[i]);

			if (view == NULL || ! _NhlIsView(view)) {
				e_text = "%s: invalid View object id";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			if (_NhlIsPlotMember(idp[i])) {
				e_text = 
			    "%s: view is already an annotation or overlay: %d";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,idp[i]);
				return NhlFATAL;
			}
			if ((anno_id = _NhlAddAnnotation((NhlLayer)ovl,
						      view,entry_name)) < 0) {
				return (NhlErrorTypes) anno_id;
			}
		}
		return NhlNOERROR;
	}

/*
 * Remove all public annotations that are not in the SetValues list;
 * note that the anno_count member is decremented for each annotation
 * that gets removed.
 */
	for (i = 0; ;) {
		NhlBoolean found = False;

		if (i == ovp->anno_count)
			break;
		for (j=0; j < count; j++) {
			if (idp[j] == ovp->view_ids[i]) {
				found = True;
				break;
			}
		}
		if (found)
			i++;
		else {
			subret = _NhlRemoveAnnotation((NhlLayer)ovl,
					 _NhlGetLayer(ovp->anno_ids[i]),
						      entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
		}
	}

	lanno_count = ovp->anno_count;
	
/*
 * Now save the remaining lists in temporary storage
 */	

	if (lanno_count > 0) {
		save_view_ids = NhlMalloc(sizeof(int) * ovp->anno_count);
		save_anno_ids = NhlMalloc(sizeof(int) * ovp->anno_count);
		if (save_view_ids == NULL || save_anno_ids == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		memcpy(save_view_ids,
		       ovp->view_ids,sizeof(int) * ovp->anno_count);
		memcpy(save_anno_ids,
		       ovp->anno_ids,sizeof(int) * ovp->anno_count);
	}
/*
 * Make sure the original anno lists are big enough for the specified
 * annotation array
 */
	while (ovp->anno_alloc <  count) {
		ovp->anno_alloc += NhlOV_ALLOC_UNIT;

		ovp->view_ids = (int *) 
			NhlRealloc(ovp->view_ids,
				   ovp->anno_alloc * sizeof(int));
		ovp->anno_ids = (int *) 
			NhlRealloc(ovp->anno_ids,
				   ovp->anno_alloc * sizeof(int));
		if (ovp->view_ids == NULL || ovp->anno_ids == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return  NhlFATAL;
		}
	}
		
/*
 * Reset the anno_count to 0, and rebuild the anno-list
 */
	ovp->anno_count = 0;

	for (i = 0; i < count; i++) {
		NhlBoolean found = False;

		/* found should never get set True if lanno_count == 0 */

		for (j = 0; j < lanno_count; j++) {
			if (idp[i] == save_view_ids[j]) {
				found = True;
				break;
			}
		}
		if (found) {
			ovp->view_ids[i] = save_view_ids[j];
			ovp->anno_ids[i] = save_anno_ids[j];
			ovp->anno_count++;
		}
		else {
			/* Note: anno_count incremented in AddAnnotation */

			NhlLayer view = _NhlGetLayer(idp[i]);

			if (view == NULL || ! _NhlIsView(view)) {
				e_text = "%s: invalid View object id";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			if ((anno_id = _NhlAddAnnotation((NhlLayer)ovl,view,
							 entry_name)) < 0) {
				return (NhlErrorTypes) anno_id;
			}
		}
	}

	if (lanno_count > 0) {
		NhlFree(save_view_ids);
		NhlFree(save_anno_ids);
	}

	return NhlNOERROR;

}
/*
 * Function:	ManageAnnotations
 *
 * Description: Manages annotations that belong to the overlay base only.
 *
 * In Args:	NhlPlotManagerLayer	ovnew - The new overlay layer
 *		NhlPlotManagerLayer	ovold - The old overlay layer
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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
)
#else
(ovnew,ovold,init,args,num_args)
	NhlPlotManagerLayer	ovnew;
	NhlPlotManagerLayer	ovold;
	NhlBoolean	init;				       
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString		e_text;
	NhlString		entry_name;
	NhlPlotManagerLayerPart	*ovp = &ovnew->plotmanager;
	NhlAnnoRec		*anlp = ovp->pm_recs[0]->anno_list;

	entry_name = init ? "PlotManagerInitialize" : "PlotManagerSetValues";

	if ((init && ovp->anno_view_ids != NULL) ||
	    _NhlArgIsSet(args,num_args,NhlNpmAnnoViews)) {
		subret = SetAnnotations(ovnew,init,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
	}

	subret = _NhlSetTrans(ovnew->trans.overlay_trans_obj,
			      (NhlLayer)ovp->pm_recs[0]->plot);
        if ((ret = MIN(ret,subret)) < NhlWARNING) {
                e_text = "%s: error setting transformation";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
                return(ret);
        }
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

	ovp->lbar_width_set = False;
	ovp->lbar_height_set = False;
	ovp->lgnd_width_set = False;
	ovp->lgnd_height_set = False;
	ovp->ti_main_font_height_set = False;
	ovp->ti_x_axis_font_height_set = False;
	ovp->ti_y_axis_font_height_set = False;

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
#if	NhlNeedProto
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
 * In Args:	NhlPlotManagerLayer	ovnew - The new overlay layer
 *		NhlPlotManagerLayer	ovold - The old overlay layer
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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
)
#else
(ovnew,ovold,init,anno_rec)
	NhlPlotManagerLayer	ovnew;
	NhlPlotManagerLayer	ovold;
	NhlBoolean	init;				       
	NhlAnnoRec	*anno_rec;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlTransformLayerPart   *tfp = &((NhlTransformLayer)ovnew)->trans;
	NhlTransformLayerPart   *otfp = &((NhlTransformLayer)ovold)->trans;
	NhlJustification	just = NhlCENTERCENTER;
	NhlBoundingBox		bbox;
	float			x_pos,y_pos,x_start,y_start,width,height;
	float			x_vp,y_vp,width_vp,height_vp;
        NhlSArg			sargs[4];
        int			nargs = 0;
	int			zone;
	float			sign;
	NhlAnnoManagerLayer 	aml; 
	NhlAnnoManagerLayerPart *amp;
	NhlBoolean		on;

	entry_name = (init) ? "PlotManagerInitialize" : "PlotManagerSetValues";

	aml = (NhlAnnoManagerLayer)_NhlGetLayer(anno_rec->anno_id);
	if (! aml) {
		e_text = "%s: Invalid AnnoManager layer";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return NhlFATAL;
	}
	amp = &aml->annomanager;
/*
 *	Update the annotation rec based on the AnnoManager values
 */
	on = amp->on;
	anno_rec->plot_id = amp->view_id;
	anno_rec->resize_notify = amp->resize_notify;
	anno_rec->zone = amp->zone;
	anno_rec->side = amp->side;
	anno_rec->just = amp->just;
	anno_rec->ortho_pos = amp->ortho_pos;
	anno_rec->para_pos = amp->para_pos;
	anno_rec->track_data = amp->track_data;
	anno_rec->data_x = amp->data_x;
	anno_rec->data_y = amp->data_y;
	anno_rec->status = on ? NhlALWAYS : NhlNEVER;
	anno_rec->viewable = on;

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
	    ((tfp->bw != otfp->bw) ||
	    (tfp->bh != otfp->bh))) {
		width = width_vp * tfp->bw / otfp->bw;
		height = height_vp * tfp->bh / otfp->bh;
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
	x_start = anno_rec->zone != 0 ? ovnew->view.x :
		ovnew->view.x + 0.5 * tfp->bw; 
	y_start = anno_rec->zone != 0 ? ovnew->view.y - tfp->bh :
		ovnew->view.y - 0.5 * tfp->bh;
	sign = anno_rec->zone == 1 ? 1.0 : -1.0;
	switch (anno_rec->side) {
	case NhlBOTTOM:
		x_pos = x_start + anno_rec->para_pos * tfp->bw;
		y_pos = bbox.b +
                        sign * anno_rec->ortho_pos * tfp->bh;
		break;
	case NhlTOP:
		x_pos = x_start + anno_rec->para_pos * tfp->bw;
		y_pos = bbox.t -
			sign * anno_rec->ortho_pos * tfp->bh;
		break;
	case NhlLEFT:
		x_pos = bbox.l + 
			sign * anno_rec->ortho_pos * tfp->bw;
		y_pos = y_start + anno_rec->para_pos * tfp->bh;
		break;
	case NhlRIGHT:
		x_pos = bbox.r - 
			sign * anno_rec->ortho_pos * tfp->bw;
		y_pos = y_start + anno_rec->para_pos * tfp->bh;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlBOTTOMRIGHT) {
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->zone > 1)
		just = ConstrainJustification(anno_rec);

/*
 * Adjust the annotation position based on the justification value
 */
	switch (just) {
	case NhlTOPLEFT:
		break;
	case NhlTOPCENTER:
		x_pos = x_pos - width / 2.0;
		break;
	case NhlTOPRIGHT:
		x_pos = x_pos - width;
		break;
	case NhlCENTERLEFT:
		y_pos = y_pos + height / 2.0;
		break;
	case NhlCENTERCENTER:
		x_pos = x_pos - width / 2.0;
		y_pos = y_pos + height / 2.0;
		break;
	case NhlCENTERRIGHT:
		x_pos = x_pos - width;
		y_pos = y_pos + height / 2.0;
		break;
	case NhlBOTTOMLEFT:
		y_pos = y_pos + height;
		break;
	case NhlBOTTOMCENTER:
		x_pos = x_pos - width / 2.0;
		y_pos = y_pos + height;
		break;
	case NhlBOTTOMRIGHT:
		y_pos = y_pos + height;
		x_pos = x_pos - width;
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
 * In Args:	NhlPlotManagerLayer	ovnew - The new overlay layer
 *		NhlPlotManagerLayer	ovold - The old overlay layer
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
#if	NhlNeedProto
(
        NhlTransformLayer	plot,
	NhlAnnoRec		*anno_list,
	int			*max_zone,
	NhlString		entry_name
)
#else
(plot,anno_list,max_zone,entry_name)
	NhlTransformLayer	plot;
	NhlAnnoRec		*anno_list;
	int			*max_zone;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlAnnoRec		*anlp;
	NhlBoolean		on;
        NhlBoolean		viewable = _NhlViewOn((NhlLayer) plot);

	*max_zone	= 0;
	for (anlp = anno_list; anlp != NULL; anlp = anlp->next) {
                anlp->viewable = viewable;
                if (! viewable)
                        continue;
		if (anlp->type == ovEXTERNAL) {
			NhlAnnoManagerLayer aml = (NhlAnnoManagerLayer) 
				_NhlGetLayer(anlp->anno_id);
			NhlAnnoManagerLayerPart *amp;
			if (! aml) {
				anlp->plot_id = NhlNULLOBJID;
				anlp->viewable = False;
				anlp->status = NhlNEVER;
			}
			else {
				amp = &aml->annomanager;
				on = amp->on;
				anlp->plot_id = amp->view_id;
				anlp->resize_notify = amp->resize_notify;
				anlp->zone = amp->zone;
				anlp->side = amp->side;
				anlp->just = amp->just;
				anlp->ortho_pos = amp->ortho_pos;
				anlp->para_pos = amp->para_pos;
				anlp->track_data = amp->track_data;
				anlp->data_x = amp->data_x;
				anlp->data_y = amp->data_y;
				anlp->status = on ? NhlALWAYS : NhlNEVER;
				anlp->viewable = on;
			}
		}
		if (anlp->viewable && anlp->zone > *max_zone)
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
 * In Args:	NhlPlotManagerLayer	ovnew - The new overlay layer
 *		NhlPlotManagerLayer	ovold - The old overlay layer
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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec
)
#else
(ovnew,ovold,init,anno_rec)
	NhlPlotManagerLayer	ovnew;
	NhlPlotManagerLayer	ovold;
	NhlBoolean	init;				       
	NhlAnnoRec	*anno_rec;
#endif
{
#define ovTMARGCOUNT	32

	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlPlotManagerLayerPart	*ovp = &ovnew->plotmanager;
	NhlPlotManagerLayerPart	*oovp = &ovold->plotmanager;
	NhlTransformLayerPart	*tfp = &ovnew->trans;
	NhlTransformLayerPart	*otfp = &ovold->trans;
	int			tmpid = NhlNULLOBJID;
	char			buffer[_NhlMAXRESNAMLEN];
        NhlSArg			sargs[ovTMARGCOUNT];
        int			nargs = 0;
	NhlLayer		trobj;
	NhlString		trobj_name;
	NhlBoolean		x_log,y_log,x_reverse,y_reverse;
	float			x_min,x_max,y_min,y_max;
	float			x_tension,y_tension;
	NhlBoolean		view_changed = False;
	float			d_left,d_right,d_bottom,d_top;
	float			xbd_left,xbd_right,yld_bottom,yld_top;
	float			xtd_left,xtd_right,yrd_bottom,yrd_top;
	NhlAxisType		y_axis,x_axis;
	NhlGenArray		x_coord_ga = NULL,y_coord_ga = NULL;

	entry_name = (init) ? "PlotManagerInitialize" : "PlotManagerSetValues";

/*
 * Update the annotation record (tickmarks do not use all the fields).
 */
	anno_rec->status = ovp->display_tickmarks;
	anno_rec->zone = ovp->tickmark_zone > NhlOV_DEF_TICKMARK_ZONE ?
		NhlOV_DEF_TICKMARK_ZONE : ovp->tickmark_zone;

	if (tfp->bx != otfp->bx ||
	    tfp->by != otfp->by ||
	    tfp->bw != otfp->bw ||
	    tfp->bh != otfp->bh)
		view_changed = True;
/*
 * If not displaying tickmarks or if the trans object and the view have
 * not changed just call the SetValues function
 */
	if (! init) {
		if (ovp->tickmarks == NULL) {
			e_text = "%s: internal error: Tickmark layer NULL";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
		if (ovp->display_tickmarks == NhlNEVER &&
		    ! ovp->trans_changed && ! view_changed) {
			return _NhlALSetValuesChild(ovp->tickmarks->base.id,
						    (NhlLayer)ovnew,
						    sargs,nargs);
		}
	}
	if (tfp->do_ndc_overlay && ovp->display_tickmarks == NhlALWAYS)
	       trobj = 
		    ((NhlTransformLayer)ovnew->base.parent)->trans.trans_obj;
	else
		trobj = tfp->overlay_trans_obj;

	if (trobj == NULL) {
		e_text = "%s: No trans obj found for TickMark object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}
	trobj_name = (trobj->base.layer_class)->base_class.class_name;

	if (trobj_name == NhlmapTransObjClass->base_class.class_name &&
	    ! _NhlIsClass(ovnew->base.parent,NhlmapPlotClass)) {
               if (ovp->display_tickmarks > NhlNEVER) {
                       e_text = 
       "%s: Map tick marks only supported for NhlmapPlotClass; turning tick marks off";
                       NhlPError(NhlINFO,NhlEUNKNOWN,e_text,entry_name);
               }
               anno_rec->status = NhlNEVER;
	       x_min = 0.0;
	       x_max = 1.0;
	       x_log = False;
	       x_reverse = False;
	       y_min = 0.0;
	       y_max = 1.0;
	       y_log = False;
	       y_reverse = False;
	       ovp->x_tm_style = NhlLINEAR;
	       ovp->y_tm_style = NhlLINEAR;
	}
	else if (trobj_name == NhlmapTransObjClass->base_class.class_name) {
		float lndc,rndc,bndc,tndc;
		float lw,rw,bw,tw;
		NhlViewLayerPart *vwp = &(((NhlViewLayer)ovnew)->view);
		NhlVAGetValues(trobj->base.id,
			       NhlNmpLeftWindowF,&lw,
			       NhlNmpRightWindowF,&rw,
			       NhlNmpBottomWindowF,&bw,
			       NhlNmpTopWindowF,&tw,
			       NhlNmpLeftNDCF,&lndc,
			       NhlNmpRightNDCF,&rndc,
			       NhlNmpBottomNDCF,&bndc,
			       NhlNmpTopNDCF,&tndc,
			       NULL);
		if (_NhlCmpFAny(vwp->x,lndc,6) == 0.0 &&
		    _NhlCmpFAny(vwp->y,tndc,6) == 0.0 &&
		    _NhlCmpFAny(vwp->width,rndc-lndc,6) == 0.0 &&
		    _NhlCmpFAny(vwp->height,tndc-bndc,6) == 0.0) {
			x_min = lw;
			x_max = rw;
			y_min = bw;
			y_max = tw;
		}
		else {
			float w_w = vwp->width * (rw - lw) / (rndc - lndc);
			float w_h = vwp->height * (tw - bw) / (tndc - bndc);
			if (lndc <= vwp->x)
				x_min = lw;
			else {
				x_min = lw - w_w * 
					(lndc - vwp->x) / vwp->width;
			}
			if (rndc >= vwp->x + vwp->width)
				x_max = rw;
			else {
				x_max = rw + w_w *
				      (vwp->x + vwp->width - rndc)/vwp->width;
			}
			if (bndc <= vwp->y - vwp->height)
				y_min = bw;
			else {
				y_min = bw - w_h * 
				   (bndc - vwp->y + vwp->height)/vwp->height;
			}
			if (tndc >= vwp->y)
				y_max = tw;
			else {
				y_max = tw + w_h *
				      (vwp->y - tndc) / vwp->height;
			}
		}

		x_log = False;
		x_reverse = False;
		y_log = False;
		y_reverse = False;
		ovp->x_tm_style = NhlLINEAR;
		ovp->y_tm_style = NhlLINEAR;
	}
	else if (trobj_name == 
		 NhllogLinTransObjClass->base_class.class_name) {
		NhlVAGetValues(trobj->base.id,
			       NhlNtrXMinF,&x_min,
			       NhlNtrXMaxF,&x_max,
			       NhlNtrXLog,&x_log,
			       NhlNtrXReverse,&x_reverse,
			       NhlNtrYMinF,&y_min,
			       NhlNtrYMaxF,&y_max,
			       NhlNtrYLog,&y_log,
			       NhlNtrYReverse,&y_reverse,
			       NULL);
		ovp->x_tm_style = (x_log == 1) ? NhlLOG : NhlLINEAR;
		ovp->y_tm_style = (y_log == 1) ? NhlLOG : NhlLINEAR;
	}
	else if (trobj_name == 
		 NhltriMeshTransObjClass->base_class.class_name) {
		NhlVAGetValues(trobj->base.id,
			       NhlNtrXMinF,&x_min,
			       NhlNtrXMaxF,&x_max,
			       NhlNtrYMinF,&y_min,
			       NhlNtrYMaxF,&y_max,
			       NhlNtrXReverse,&x_reverse,
			       NhlNtrYReverse,&y_reverse,
			       NULL);
		x_log = False;
		y_log = False;
		ovp->x_tm_style = NhlLINEAR;
		ovp->y_tm_style = NhlLINEAR;
	}
	else if (trobj_name== 
		 NhlirregularTransObjClass->base_class.class_name) {
		NhlVAGetValues(trobj->base.id,
			       NhlNtrXMinF,&x_min,
			       NhlNtrXMaxF,&x_max,
			       NhlNtrXAxisType,&x_axis,
			       NhlNtrXReverse,&x_reverse,
			       NhlNtrXCoordPoints,&x_coord_ga,
			       NhlNtrXTensionF,&x_tension,
			       NhlNtrYMinF,&y_min,
			       NhlNtrYMaxF,&y_max,
			       NhlNtrYAxisType,&y_axis,
			       NhlNtrYReverse,&y_reverse,
			       NhlNtrYCoordPoints,&y_coord_ga,
			       NhlNtrYTensionF,&y_tension,
			       NULL);

		switch (x_axis) {
		case NhlLINEARAXIS:
			ovp->x_tm_style = NhlLINEAR;
			break;
		case NhlLOGAXIS:
			ovp->x_tm_style = NhlLOG;
			break;
		case NhlIRREGULARAXIS:
			ovp->x_tm_style = NhlIRREGULAR;
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXBIrregularPoints,x_coord_ga);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXTIrregularPoints,x_coord_ga);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXBIrrTensionF,x_tension);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXTIrrTensionF,x_tension);
			break;
		}
		switch (y_axis) {
		case NhlLINEARAXIS:
			ovp->y_tm_style = NhlLINEAR;
			break;
		case NhlLOGAXIS:
			ovp->y_tm_style = NhlLOG;
			break;
		case NhlIRREGULARAXIS:
			ovp->y_tm_style = NhlIRREGULAR;
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYLIrregularPoints,y_coord_ga);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYRIrregularPoints,y_coord_ga);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYLIrrTensionF,y_tension);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYRIrrTensionF,y_tension);
			break;
		}
	}
	else {
		if (trobj_name== 
		    NhlcurvilinearTransObjClass->base_class.class_name ||
		    trobj_name== 
		    NhlsphericalTransObjClass->base_class.class_name) {
			ret = MIN(ret,NhlINFO);
			e_text = 
	     "%s: tick marks not supported for 2D coordinate transformations, except as map overlays";
                }
		else {	
			ret = MIN(ret,NhlWARNING);
			e_text = 
			 "%s: unknown transformation; turning tick marks off";
		}
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
	if (x_reverse) {
		d_left = x_max; 
		d_right = x_min;
	}
	else {
		d_left = x_min; 
		d_right = x_max;
	}
	if (y_reverse) {
		d_bottom = y_max; 
		d_top = y_min;
	}
	else {
		d_bottom = y_min; 
		d_top = y_max;
	}
	if (ovp->tickmarks == NULL) {
		NhlSetSArg(&sargs[nargs++],NhlNtmXBDataLeftF,d_left);
		NhlSetSArg(&sargs[nargs++],NhlNtmXBDataRightF,d_right);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtmYLDataBottomF,d_bottom);
		NhlSetSArg(&sargs[nargs++],NhlNtmYLDataTopF,d_top);
		NhlSetSArg(&sargs[nargs++],NhlNtmXTDataLeftF,d_left);
		NhlSetSArg(&sargs[nargs++],NhlNtmXTDataRightF,d_right);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtmYRDataBottomF,d_bottom);
		NhlSetSArg(&sargs[nargs++],NhlNtmYRDataTopF,d_top);
	}
	else {
		NhlVAGetValues(ovp->tickmarks->base.id,
			       NhlNtmXBDataLeftF,&xbd_left,
			       NhlNtmXBDataRightF,&xbd_right,
			       NhlNtmYLDataBottomF,&yld_bottom,
			       NhlNtmYLDataTopF,&yld_top,
			       NhlNtmXTDataLeftF,&xtd_left,
			       NhlNtmXTDataRightF,&xtd_right,
			       NhlNtmYRDataBottomF,&yrd_bottom,
			       NhlNtmYRDataTopF,&yrd_top,
			       NULL);
		if (xbd_left != d_left)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXBDataLeftF,d_left);
		if (xtd_left != d_left)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXTDataLeftF,d_left);
		if (xbd_right != d_right)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXBDataRightF,d_right);
		if (xtd_right != d_right)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXTDataRightF,d_right);
		if (yld_bottom != d_bottom)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYLDataBottomF,d_bottom);
		if (yrd_bottom != d_bottom) 
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYRDataBottomF,d_bottom);
		if (yld_top != d_top)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYLDataTopF,d_top);
		if (yrd_top != d_top)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYRDataTopF,d_top);
	}
		 
/*
 * If no tickmark object exists, create it; otherwise just set the relevant
 * resources.
 */
	if (ovp->tickmarks == NULL) {	
		NhlSetSArg(&sargs[nargs++],
			   NhlNvpUseSegments,ovnew->view.use_segments);
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,tfp->bx);
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,tfp->by);
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,tfp->bw);
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,tfp->bh);
		NhlSetSArg(&sargs[nargs++],NhlNtmYLStyle,ovp->y_tm_style);
		NhlSetSArg(&sargs[nargs++],NhlNtmXBStyle,ovp->x_tm_style);
		NhlSetSArg(&sargs[nargs++],NhlNtmYRStyle,ovp->y_tm_style);
		NhlSetSArg(&sargs[nargs++],NhlNtmXTStyle,ovp->x_tm_style);

		if (nargs >= ovTMARGCOUNT) {
			e_text = "%s: TickMark setargs array exceeded";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
		strcpy(buffer,ovnew->base.parent->base.name);
		strcat(buffer,".TickMark");
		subret = _NhlALCreateChild(&tmpid,buffer,NhltickMarkClass,
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
		if (tfp->bx != otfp->bx)
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,tfp->bx);
		if (tfp->by != otfp->by)
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,tfp->by);
		if (tfp->bw != otfp->bw)
			NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,tfp->bw);
		if (tfp->bh != otfp->bh)
			NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,tfp->bh);
		if (ovnew->view.use_segments != ovold->view.use_segments)
			NhlSetSArg(&sargs[nargs++],
                                   NhlNvpUseSegments,ovnew->view.use_segments);
		if (ovp->x_tm_style != oovp->x_tm_style) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXBStyle,ovp->x_tm_style);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXTStyle,ovp->x_tm_style);
		}
		if (ovp->y_tm_style != oovp->y_tm_style) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYLStyle,ovp->y_tm_style);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYRStyle,ovp->y_tm_style);
		}
		if (nargs >= ovTMARGCOUNT) {
			e_text = "%s: TickMark setargs array exceeded";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
		subret = _NhlALSetValuesChild(ovp->tickmarks->base.id,
					      (NhlLayer)ovnew,sargs,nargs);

		if ((ret = MIN(subret,ret)) < NhlNOERROR) {
			e_text = "%s: Error setting TickMark values";
			NHLPERROR((ret,NhlEUNKNOWN,e_text, entry_name));
			if (ret < NhlWARNING) {
				anno_rec->status = 
					ovp->display_tickmarks = init ? 
						NhlNOCREATE : NhlNEVER;
				return(NhlFATAL);
			}
		}
	}
	if (x_coord_ga != NULL)
		NhlFreeGenArray(x_coord_ga);
	if (y_coord_ga != NULL)
		NhlFreeGenArray(y_coord_ga);

        anno_rec->viewable = ovp->display_tickmarks > NhlNEVER;
        
	return ret;
}

/*
 * Function:	ManageTitles
 *
 * Description: Internal function that manages the title object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	NhlPlotManagerLayer	ovnew - The new overlay layer
 *		NhlPlotManagerLayer	ovold - The old overlay layer
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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,				       
	NhlAnnoRec	*anno_rec
)
#else
(ovnew,ovold,init,anno_rec)
	NhlPlotManagerLayer	ovnew;
	NhlPlotManagerLayer	ovold;
	NhlBoolean	init;				       
	NhlAnnoRec	*anno_rec;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlPlotManagerLayerPart	*ovp = &ovnew->plotmanager;
	NhlPlotManagerLayerPart	*oovp = &ovold->plotmanager;
	NhlTransformLayerPart   *tfp = &((NhlTransformLayer)ovnew)->trans;
	NhlTransformLayerPart   *otfp = &((NhlTransformLayer)ovold)->trans;
	int			tmpid = NhlNULLOBJID;
	NhlBoundingBox		bbox;
	char			buffer[_NhlMAXRESNAMLEN];
        NhlSArg			sargs[16];
        int			nargs = 0;
	int			zone;

	entry_name = (init) ? "PlotManagerInitialize" : "PlotManagerSetValues";

/*
 * Update the annotation record (titles do not use all the fields).
 */
	anno_rec->status = ovp->display_titles;
        anno_rec->viewable = ovp->display_titles > NhlNEVER;
	anno_rec->zone = ovp->title_zone > 0 ? ovp->title_zone : 0;

/*
 * If not displaying titles just call the SetValues function --
 * only to avoid the no set values called error.
 */
	if (init) {
		NhlTitleLayer tl;
		float main_height,x_axis_height,y_axis_height;
		strcpy(buffer,ovnew->base.parent->base.name);
		strcat(buffer,".Title");
		subret = _NhlVACreateChild(&tmpid,buffer,NhltitleClass,
					   (NhlLayer)ovnew,NULL);

		if ((ret = MIN(ret,subret)) < NhlWARNING || 
		    (ovp->titles = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating Title object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
		anno_rec->plot_id = tmpid;
/*
 * Title font height setting requires special treatment because these
 * resources are intercepted but we want to be able to set them in the
 * resource file based on them belonging to the titleClass. Because the
 * code here explicitly sets the font height in the Title object based
 * on the intercepted value of the resource, values set by class in the
 * resource file are ignored (they do not result in the intercepted resource
 * value getting set -- only the original value in the Title object itself).
 * However, explicitly setting the value in the create call causes the 
 * intercepted resource value to change. The solution for this difficulty
 * is to check the intercepted value first to see if it has been set. If
 * it has, do nothing. Otherwise, if the child object's value has been
 * set use that value. Use the default only if it has not been set at
 * either level. Shouldn't be looking at the private Title layer data, but
 * it's the only way to catch the case of setting the height (by class in
 * the resource file) explicitly to the default value, without creating 
 * some new resources just for seeing if the resource has been set. 
 */
		NhlVAGetValues(tmpid,
			       NhlNtiMainFontHeightF,&main_height,
			       NhlNtiXAxisFontHeightF,&x_axis_height,
			       NhlNtiYAxisFontHeightF,&y_axis_height,
			       NULL);
		tl = (NhlTitleLayer) _NhlGetLayer(tmpid);

		if (! ovp->ti_main_font_height_set) {
			if (tl->title.main_font_height_set) {
				ovp->ti_main_font_height_set = True;
				ovp->ti_main_font_height = main_height;
			}
			else {
				ovp->ti_main_font_height = NhlDEF_TITLE_HEIGHT;
			}
		}
		if (! ovp->ti_x_axis_font_height_set) {
			if (tl->title.x_axis_font_height_set) {
				ovp->ti_x_axis_font_height_set = True;
				ovp->ti_x_axis_font_height = x_axis_height;
			}
			else {
				ovp->ti_x_axis_font_height = 
					NhlDEF_TITLE_HEIGHT;
			}
		}
		if (! ovp->ti_y_axis_font_height_set) {
			if (tl->title.y_axis_font_height_set) {
				ovp->ti_y_axis_font_height_set = True;
				ovp->ti_y_axis_font_height = y_axis_height;
			}
			else {
				ovp->ti_y_axis_font_height = 
					NhlDEF_TITLE_HEIGHT;
			}
		}
	}
	else {
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

	if (init) {
		if (! ovp->ti_main_font_height_set) 
			ovp->ti_main_font_height = NhlDEF_TITLE_HEIGHT *
				tfp->bw / NHL_DEFAULT_VIEW_WIDTH;
		if (! ovp->ti_x_axis_font_height_set)
			ovp->ti_x_axis_font_height = NhlDEF_TITLE_HEIGHT *
				tfp->bw / NHL_DEFAULT_VIEW_WIDTH;
		if (! ovp->ti_y_axis_font_height_set)
			ovp->ti_y_axis_font_height = NhlDEF_TITLE_HEIGHT *
				tfp->bh / NHL_DEFAULT_VIEW_HEIGHT;
	}
	else {
		if (! ovp->ti_main_font_height_set) 
			ovp->ti_main_font_height *= tfp->bw / otfp->bw;
		if (! ovp->ti_x_axis_font_height_set)
			ovp->ti_x_axis_font_height *= tfp->bw / otfp->bw;
		if (! ovp->ti_y_axis_font_height_set)
			ovp->ti_y_axis_font_height *= tfp->bh / otfp->bh;
	}

	switch(ovp->ti_main_position) {
	case NhlTOP:
	case NhlBOTTOM:
	default:
		e_text = 
    "%s: Invalid value for Main axis title position, defaulting to NhlCENTER";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
		ret = MIN(NhlWARNING,ret);
		ovp->ti_main_position = NhlCENTER;
	case NhlCENTER:
		ovp->real_main_offset_x = ovp->ti_main_offset_x +
			((tfp->bx + tfp->bw/2.0) -
			 (ovp->ti_x + ovp->ti_width/2.0));
		break;
	case NhlLEFT:
		ovp->real_main_offset_x = ovp->ti_main_offset_x +
			(tfp->bx - ovp->ti_x);
		break;
	case NhlRIGHT:
		ovp->real_main_offset_x = ovp->ti_main_offset_x +
			((tfp->bx + tfp->bw) - (ovp->ti_x + ovp->ti_width));
		break;
	}

	switch(ovp->ti_x_axis_position) {
	case NhlTOP:
	case NhlBOTTOM:
	default:
		e_text = 
    "%s: Invalid value for X-Axis title position, defaulting to NhlCENTER";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
		ret = MIN(NhlWARNING,ret);
		ovp->ti_x_axis_position = NhlCENTER;
	case NhlCENTER:
		ovp->real_x_axis_offset_x = ovp->ti_x_axis_offset_x +
			((tfp->bx + tfp->bw/2.0) -
			 (ovp->ti_x + ovp->ti_width/2.0));
		break;
	case NhlLEFT:
		ovp->real_x_axis_offset_x = ovp->ti_x_axis_offset_x +
			(tfp->bx - ovp->ti_x);
		break;
	case NhlRIGHT:
		ovp->real_x_axis_offset_x = ovp->ti_x_axis_offset_x +
			((tfp->bx + tfp->bw) - (ovp->ti_x + ovp->ti_width));
		break;
	}

	switch(ovp->ti_y_axis_position) {
	case NhlLEFT:
	case NhlRIGHT:
	default:
		e_text = 
    "%s: Invalid value for Y-Axis title position, defaulting to NhlCENTER";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
		ret = MIN(NhlWARNING,ret);
		ovp->ti_y_axis_position = NhlCENTER;
	case NhlCENTER:
		ovp->real_y_axis_offset_y = ovp->ti_y_axis_offset_y +
			((tfp->by - tfp->bh/2.0) -
			 (ovp->ti_y - ovp->ti_height/2.0));
		break;
	case NhlTOP:
		ovp->real_y_axis_offset_y = ovp->ti_y_axis_offset_y +
			(tfp->by - ovp->ti_y);
		break;
	case NhlBOTTOM:
		ovp->real_y_axis_offset_y = ovp->ti_y_axis_offset_y + 
			((tfp->by - tfp->bh) - (ovp->ti_y - ovp->ti_height));
		break;
	}

/*
 * If no title object exists, create it; otherwise just set the relevant
 * resources. 
 */
	if (init) {	

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
	} else {
		
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
                if (ovnew->view.use_segments  != ovold->view.use_segments)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNvpUseSegments,ovnew->view.use_segments);
		if ((ovp->ti_main_font_height != oovp->ti_main_font_height)
		    || (ovp->ti_width != oovp->ti_width))
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiMainFontHeightF,
				   ovp->ti_main_font_height);
		if ((ovp->ti_x_axis_font_height != oovp->ti_x_axis_font_height)
		    || (ovp->ti_width != oovp->ti_width))
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiXAxisFontHeightF,
				   ovp->ti_x_axis_font_height);
		if ((ovp->ti_y_axis_font_height != oovp->ti_y_axis_font_height)
		    || (ovp->ti_height != oovp->ti_height))
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

	}
	subret = _NhlALSetValuesChild(ovp->titles->base.id,
				      (NhlLayer)ovnew,sargs,nargs);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: Error updating Title object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}

	return ret;
}

/*
 * Function:	ManageLabelBar
 *
 * Description: Internal function that manages the labelbar object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	NhlPlotManagerLayer	ovnew - The new PlotManager layer
 *		NhlPlotManagerLayer	ovold - The old PlotManager layer
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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec,
	_NhlArgList	args,
	int		num_args
)
#else
(ovnew,ovold,init,anno_rec,args,num_args)
	NhlPlotManagerLayer	ovnew;
	NhlPlotManagerLayer	ovold;
	NhlBoolean	init;				       
	NhlAnnoRec	*anno_rec;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlPlotManagerLayerPart	*ovp = &ovnew->plotmanager;
	NhlPlotManagerLayerPart	*oovp = &ovold->plotmanager;
	NhlTransformLayerPart   *tfp = &((NhlTransformLayer)ovnew)->trans;
	NhlTransformLayerPart   *otfp = &((NhlTransformLayer)ovold)->trans;
	int			tmpid = NhlNULLOBJID;
	NhlBoundingBox		bbox;
	char			buffer[_NhlMAXRESNAMLEN];
        NhlSArg			sargs[16];
        int			nargs = 0;
	float			wold, hold;
	int			zone;
	float			sign;

	entry_name = (init) ? "PlotManagerInitialize" : "PlotManagerSetValues";
/*
 * Update the annotation record
 */
	anno_rec->side = ovp->lbar_side;
	anno_rec->zone = ovp->labelbar_zone > 0 ? ovp->labelbar_zone : 0;
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
		if (ovp->display_labelbar > NhlNEVER) {
                        anno_rec->viewable = True;
			NhlSetSArg(&sargs[nargs++],NhlNlbLabelBarOn,True);
                }
		else {
                        anno_rec->viewable = False;
			NhlSetSArg(&sargs[nargs++],NhlNlbLabelBarOn,False);
#if 0
			if (! init) 
				return _NhlALSetValuesChild(
						    ovp->labelbar->base.id,
						    (NhlLayer)ovnew,
							    sargs,nargs);
#endif
		}
	}
/*
 * If the view width or height has changed adjust the LabelBar width and
 * height if they have not been set explcitly by the user.
 * Also if the user has not set height and width explicitly exchange 
 * height and width to achieve the normally most appropriate shape given
 * the current orientation.
 */

	wold = init ? NHL_DEFAULT_VIEW_WIDTH : otfp->bw;
	hold = init ? NHL_DEFAULT_VIEW_HEIGHT : otfp->bh;
	if (ovp->lbar_keep_aspect) {
		float ratio;

		switch (ovp->lbar_side) {
		default:
		case NhlTOP:
		case NhlBOTTOM:
			ratio = tfp->bw / wold;
			break;
		case NhlLEFT:
		case NhlRIGHT:
			ratio = tfp->bh / hold;
			break;
		}
		if (! ovp->lbar_width_set)
			ovp->lbar_width *= ratio;
		if (! ovp->lbar_height_set)
			ovp->lbar_height *= ratio;
	}
	else { 
		if (! ovp->lbar_width_set)
			ovp->lbar_width *= tfp->bw / wold;
		if (! ovp->lbar_height_set)
			ovp->lbar_height *= tfp->bh / hold;

		if (! ovp->lbar_width_set && ! ovp->lbar_height_set) {

			if (init || ovp->lbar_orient != oovp->lbar_orient) {
				float t;

				if (ovp->lbar_orient == NhlVERTICAL) {
					t = MIN(ovp->lbar_height,
						ovp->lbar_width);
					ovp->lbar_height = 
						MAX(ovp->lbar_height,
						    ovp->lbar_width);
					ovp->lbar_width = t;
				}
				else  {
					t = MIN(ovp->lbar_height,
						ovp->lbar_width);
					ovp->lbar_width = 
						MAX(ovp->lbar_height,
						    ovp->lbar_width);
					ovp->lbar_height = t;
				}
			}
		}
	}

/*
 * Get the bounding box for the zone inside the annotation zone, 
 * then calculate the labelbar's position with respect to it.
 */
	bbox.set = 0;
	zone = anno_rec->zone < 2 ? anno_rec->zone : anno_rec->zone - 1;
	subret = InternalGetBB((NhlLayer)ovnew,&bbox,zone,
			    entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: Error getting bounding box";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}
	sign = anno_rec->zone == 1 ? 1.0 : -1.0;

	switch (anno_rec->side) {
	case NhlBOTTOM:
		ovp->lbar_x = tfp->bx + anno_rec->para_pos * tfp->bw;
		ovp->lbar_y = bbox.b + sign * anno_rec->ortho_pos * tfp->bh;
		break;
	case NhlTOP:
		ovp->lbar_x = tfp->bx + anno_rec->para_pos * tfp->bw;
		ovp->lbar_y = bbox.t - sign * anno_rec->ortho_pos * tfp->bh;
		break;
	case NhlLEFT:
		ovp->lbar_x = bbox.l + sign * anno_rec->ortho_pos * tfp->bw;
		ovp->lbar_y = tfp->by - tfp->bh + anno_rec->para_pos * tfp->bh;
		break;
	case NhlRIGHT:
		ovp->lbar_x = bbox.r - sign * anno_rec->ortho_pos * tfp->bw;
		ovp->lbar_y = tfp->by - tfp->bh + anno_rec->para_pos * tfp->bh;
		break;
	default:
		e_text = "%s: internal enumeration error";
		anno_rec->plot_id = NhlNULLOBJID;
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	anno_rec->just = ovp->lbar_just;
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlBOTTOMRIGHT) {
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
		strcpy(buffer,ovnew->base.parent->base.name);
		strcat(buffer,".LabelBar");
		NhlSetSArg(&sargs[nargs++],
			   NhlNvpUseSegments,ovnew->view.use_segments);
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovp->lbar_x);
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovp->lbar_y);
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,ovp->lbar_width);
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,ovp->lbar_height);
		NhlSetSArg(&sargs[nargs++],
			   NhlNvpKeepAspect,ovp->lbar_keep_aspect);
		NhlSetSArg(&sargs[nargs++],
			   NhlNlbJustification,ovp->real_lbar_just);
		NhlSetSArg(&sargs[nargs++],
			   NhlNlbOrientation,ovp->lbar_orient);
		subret = _NhlALCreateChild(&tmpid,buffer,NhllabelBarClass,
					   (NhlLayer)ovnew,sargs,nargs);
		anno_rec->plot_id = tmpid;
		if ((ret = MIN(ret,subret)) < NhlWARNING || 
		    (ovp->labelbar = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating LabelBar object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
	} else {
		if (ovnew->view.use_segments != ovold->view.use_segments)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpUseSegments,ovnew->view.use_segments);
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

		if (ovp->lbar_keep_aspect != oovp->lbar_keep_aspect)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpKeepAspect,ovp->lbar_keep_aspect);
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
/*
 * Need to retrieve the height and width because the LabelBar might 
 * change these values, particularly if auto_manage is set false.
 */
	subret = NhlVAGetValues(ovp->labelbar->base.id,
				NhlNvpWidthF,&ovp->lbar_width,
				NhlNvpHeightF,&ovp->lbar_height,
				NULL);

	return MIN(ret,subret);
}


/*
 * Function:	ManageLegend
 *
 * Description: Internal function that manages the legend object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	NhlPlotManagerLayer	ovnew - The new overlay layer
 *		NhlPlotManagerLayer	ovold - The old overlay layer
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
#if	NhlNeedProto
(
	NhlPlotManagerLayer	ovnew,
	NhlPlotManagerLayer	ovold,
	NhlBoolean	init,
	NhlAnnoRec	*anno_rec,
	_NhlArgList	args,
	int		num_args
)
#else
(ovnew,ovold,init,anno_rec,args,num_args)
	NhlPlotManagerLayer	ovnew;
	NhlPlotManagerLayer	ovold;
	NhlAnnoRec	*anno_rec;
	NhlBoolean	init;				       
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlPlotManagerLayerPart	*ovp = &ovnew->plotmanager;
	NhlPlotManagerLayerPart	*oovp = &ovold->plotmanager;
	NhlTransformLayerPart   *tfp = &((NhlTransformLayer)ovnew)->trans;
	NhlTransformLayerPart   *otfp = &((NhlTransformLayer)ovold)->trans;
	int			tmpid = NhlNULLOBJID;
	NhlBoundingBox		bbox;
	char			buffer[_NhlMAXRESNAMLEN];
        NhlSArg			sargs[16];
        int			nargs = 0;
	float			wold, hold;
	int			zone;
	float			sign;

	entry_name = (init) ? "PlotManagerInitialize" : "PlotManagerSetValues";

/*
 * Update the annotation record
 */
	anno_rec->side = ovp->lgnd_side;
	anno_rec->zone = ovp->legend_zone > 0 ? ovp->legend_zone : 0;
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
		if (ovp->display_legend > NhlNEVER) {
                        anno_rec->viewable = True;
			NhlSetSArg(&sargs[nargs++],NhlNlgLegendOn,True);
                }
		else {
                        anno_rec->viewable = False;
			NhlSetSArg(&sargs[nargs++],NhlNlgLegendOn,False);
#if 0
			if (! init) 
				return _NhlALSetValuesChild(
						ovp->legend->base.id,
						(NhlLayer)ovnew,sargs,nargs);
#endif
		}
	}
/*
 * If the view width or height has changed adjust the Legend width and
 * height if they have not been set explcitly by the user
 */
	wold = init ? NHL_DEFAULT_VIEW_WIDTH : otfp->bw;
	hold = init ? NHL_DEFAULT_VIEW_HEIGHT : otfp->bh;
	if (ovp->lgnd_keep_aspect) {
		float ratio;

		switch (ovp->lgnd_side) {
		default:
		case NhlTOP:
		case NhlBOTTOM:
			ratio = tfp->bw / wold;
			break;
		case NhlLEFT:
		case NhlRIGHT:
			ratio = tfp->bh / hold;
			break;
		}
		if (! ovp->lgnd_width_set)
			ovp->lgnd_width *= ratio;
		if (! ovp->lgnd_height_set)
			ovp->lgnd_height *= ratio;
	}
	else { 
		if (! ovp->lgnd_width_set)
			ovp->lgnd_width *= tfp->bw / wold;
		if (! ovp->lgnd_height_set)
			ovp->lgnd_height *= tfp->bh / hold;
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
		ovp->lgnd_x = tfp->bx + anno_rec->para_pos * tfp->bw;
		ovp->lgnd_y = bbox.b + sign * anno_rec->ortho_pos * tfp->bh;
		break;
	case NhlTOP:
		ovp->lgnd_x = tfp->bx + anno_rec->para_pos * tfp->bw;
		ovp->lgnd_y = bbox.t - sign * anno_rec->ortho_pos * tfp->bh;
		break;
	case NhlLEFT:
		ovp->lgnd_x = bbox.l + sign * anno_rec->ortho_pos * tfp->bw;
		ovp->lgnd_y = tfp->by - tfp->bh + anno_rec->para_pos * tfp->bh;
		break;
	case NhlRIGHT:
		ovp->lgnd_x = bbox.r - sign * anno_rec->ortho_pos * tfp->bw;
		ovp->lgnd_y = tfp->by - tfp->bh + anno_rec->para_pos * tfp->bh;
		break;
	default:
		e_text = "%s: internal enumeration error";
		anno_rec->plot_id = NhlNULLOBJID;
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	anno_rec->just = ovp->lgnd_just;
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlBOTTOMRIGHT) {
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->zone > 1)
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
 * If no legend exists, create it; otherwise just set the relevant
 * resources.
 */
	if (ovp->legend == NULL) {	
		strcpy(buffer,ovnew->base.parent->base.name);
		strcat(buffer,".Legend");

		NhlSetSArg(&sargs[nargs++],
			   NhlNvpUseSegments,ovnew->view.use_segments);
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovp->lgnd_x);
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovp->lgnd_y);
		NhlSetSArg(&sargs[nargs++],
			   NhlNlgJustification,ovp->real_lgnd_just);
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,ovp->lgnd_width);
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,ovp->lgnd_height);
		NhlSetSArg(&sargs[nargs++],
				   NhlNlgOrientation,ovp->lgnd_orient);
		NhlSetSArg(&sargs[nargs++],
			   NhlNvpKeepAspect,ovp->lgnd_keep_aspect);
		subret = _NhlALCreateChild(&tmpid,buffer,NhllegendClass,
					   (NhlLayer)ovnew,sargs,nargs);
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
		if (ovp->lgnd_keep_aspect != oovp->lgnd_keep_aspect)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpKeepAspect,ovp->lgnd_keep_aspect);
                if (ovnew->view.use_segments != ovold->view.use_segments)
			NhlSetSArg(&sargs[nargs++],
                                   NhlNvpUseSegments,ovnew->view.use_segments);
		subret = _NhlALSetValuesChild(ovp->legend->base.id,
					      (NhlLayer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error updating Legend object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(NhlFATAL);
		}
	}
/*
 * Need to retrieve the height and width because the Legend might 
 * change these values, particularly if auto_manage is set false.
 */
	subret = NhlVAGetValues(ovp->legend->base.id,
				NhlNvpWidthF,&ovp->lgnd_width,
				NhlNvpHeightF,&ovp->lgnd_height,
				NULL);

	return MIN(ret,subret);
}


/*
 * The first two global functions allow the user to add, remove, and 
 * control the drawing sequence of plots in an overlay.
 */

/*
 * Function:	NhlAddOverlay
 *
 * Description:	
 *
 * In Args:	base_id		id of overlay base plot
 *		transform_id	id of transform to add as an overlay
 *		after_id	id of transform already an overlay
 *				that the new transform should be drawn after --
 *				if < 0 then the new transform is placed at the
 *				end of the overlay sequence.
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlAddOverlay
#if	NhlNeedProto
(int base_id, int transform_id, int after_id)
#else
(base_id, transform_id, after_id)
        int base_id;
	int transform_id;
	int after_id;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlAddOverlay";

	NhlLayer		base = _NhlGetLayer(base_id);
	NhlLayer		plot = _NhlGetLayer(transform_id);
	NhlLayer		after = _NhlGetLayer(after_id);

	NhlTransformLayerPart	*base_tfp;
	NhlPlotManagerLayer	ovl;
	NhlPlotManagerLayerPart	*ovp;
	NhlTransformLayerPart	*plot_tfp;
	NhlTransformClassRec	*plot_classp;

	NhlGenArray		ga;
	int			plot_count = 0;
	NhlpmRec		**sub_recs = NULL;
	int			i, j;
	float			ox,oy,owidth,oheight;
        NhlArgVal       	cbdata,dummy;
        _NhlOverlayStatusCBDataRec overlay_status;
	float			x,y,w,h;
	NhlBoolean		is_map = False;
        
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
		e_text = "%s: invalid transform id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (base == plot) {
		e_text = "%s: transform is the same as the base plot";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (base->base.wkptr != plot->base.wkptr) {
		e_text = 
 	"%s: transform and base plot belong to different workstations";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	plot_tfp = &(((NhlTransformLayer)plot)->trans);
	base_tfp = &(((NhlTransformLayer)base)->trans);
	if (! base_tfp->plot_manager_on ||
	    base_tfp->overlay_object == NULL || 
	    ! _NhlIsTransform(base_tfp->overlay_object)) {
		e_text = "%s: base is not a plot object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	ovl = (NhlPlotManagerLayer) base_tfp->overlay_object;
	ovp = &(ovl->plotmanager);

/*
 * Test to ensure the added plot can handle becoming an overlay member. 
 */
	plot_classp = (NhlTransformClassRec *) _NhlClass(plot);
	if (plot_classp->trans_class.overlay_capability ==
	    					_tfNotOverlayCapable ||
	    plot_classp->trans_class.overlay_capability ==
	    					_tfOverlayBaseOnly) {
		e_text = "%s: plot class %s cannot be overlay plot member";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  plot_classp->base_class.class_name);
		return NhlFATAL;
	}

/*
 * If the plot is already an overlay member (an annotation or an
 * overlay member plot) it cannot be added as an overlay again.
 * (even the same overlay).
 * If the plot is an overlay base already, any overlays it has acquired 
 * become part of the overlay it has been added to. Get the record 
 * containing the list of overlays plots plus their associated overlay objs.
 * Also set the argument that will tell its overlay object it is no longer
 * a master overlay object.
 * If the plot is not an overlay base or an overlay member, simply
 * allocate and fill in a single overlay record for it.
 */

	if (_NhlIsPlotMember(transform_id)) {
		e_text = 
		       "%s: tranform is already an annotation or overlay: %d";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,transform_id);
		return NhlFATAL;
	}
	else if (plot_tfp->overlay_status == _tfCurrentOverlayBase) {

		if (plot_tfp->overlay_object == NULL || 
		    ! _NhlIsTransform(plot_tfp->overlay_object)) {
		      e_text = 
			 "%s: transform state is internally inconsistent: %d";
		        NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,transform_id);
			return NhlFATAL;
		}

		subret = NhlVAGetValues(plot_tfp->overlay_object->base.id,
					NhlNpmPlotManagerRecs,&ga,
					NULL);

		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			return ret;
		}
		if (ga == NULL || ga->size != sizeof(NhlpmRec *)) {
			e_text = "%s: error retrieving internal gen array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		plot_count = ga->num_elements;
		sub_recs = (NhlpmRec **) ga->data;

		ga->my_data = False;
		NhlFreeGenArray(ga);
	}
	else {
		if ((sub_recs = (NhlpmRec **) 
		     NhlMalloc(sizeof(NhlpmRec *))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if ((sub_recs[0] = (NhlpmRec *) 
		     NhlMalloc(sizeof(NhlpmRec))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		plot_count = 1;
		sub_recs[0]->plot = 
			(NhlTransformLayer) _NhlGetLayer(transform_id);
		sub_recs[0]->ov_obj = NULL;
	}
/*
 * Get the current viewport of the transform and store it in sub_rec[0]
 * so it can be restored if the transform is removed later.
 */
	
	subret = NhlVAGetValues(transform_id,
				NhlNvpXF,&ox,
				NhlNvpYF,&oy,
				NhlNvpWidthF,&owidth,
				NhlNvpHeightF,&oheight,
				NULL);
	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;
	sub_recs[0]->ox = ox;
	sub_recs[0]->oy = oy;
	sub_recs[0]->owidth = owidth;
	sub_recs[0]->oheight = oheight;

/*
 * Reallocate the array of overlay record pointers if necessary
 */
	if (ovp->overlay_alloc < ovp->overlay_count + plot_count) {
                ovp->pm_recs = (NhlpmRec **)
			NhlRealloc(ovp->pm_recs, sizeof(NhlpmRec *) *
				   (ovp->overlay_count + 
				    MAX(NhlOV_ALLOC_UNIT,plot_count)));
		if (ovp->pm_recs == NULL) {
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
                for (i = 0; i < plot_count; i++) {
                        ovp->pm_recs[ovp->overlay_count+i] = sub_recs[i];
                }
	}
	else if (! _NhlIsTransform(after)) {
		
		e_text = "%s: the after transform id is invalid";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);

		for (i = 0; i < plot_count; i++) {
			ovp->pm_recs[ovp->overlay_count+i] = sub_recs[i]; 
                }
	}
	else {
		for (i = 0; i <= ovp->overlay_count; i++) {

			if (i == ovp->overlay_count) { /* after not found */
				e_text = "%s: invalid after transform id";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name);
				ret = MIN(ret,NhlWARNING);
				for (j = 0; j < plot_count; j++) {
					ovp->pm_recs[ovp->overlay_count+j] = 
						sub_recs[j];
                                }
                        }
			else if (after ==  (NhlLayer) ovp->pm_recs[i]->plot) {
				for (j = ovp->overlay_count - 1;j > i; j--) {
				      ovp->pm_recs[j+plot_count] =
					      ovp->pm_recs[j];
			        }
				for (j = 0; j < plot_count; j++) {
					ovp->pm_recs[j+i+1] = sub_recs[j];
                                }
				break;
			}
		}
	}
	ovp->overlay_count += plot_count;
		
/*
 * Call set values for each plot added to the overlay to inform it of its
 * new status, adjusting its view to be identical to the overlay's view.
 */
	if (_NhlIsClass(base_tfp->overlay_trans_obj,NhlmapTransObjClass)) {
		float r,b;
		NhlVAGetValues(base_tfp->overlay_trans_obj->base.id,
			       NhlNmpLeftNDCF,&x,
			       NhlNmpRightNDCF,&r,
			       NhlNmpTopNDCF,&y,
			       NhlNmpBottomNDCF,&b,
			       NULL);
		w = r - x;
		h = y - b;
		is_map = True;
	}
		
	for (i = 0; i < plot_count; i++) {
		NhlSArg			sargs[32];
		int			nargs = 0;
		NhlTransformLayer	plot = sub_recs[i]->plot;
		NhlTransformLayerPart	*plot_tfp = &plot->trans;

		NhlSetSArg(&sargs[nargs++],NhlNtfOverlayStatus, 
			   _tfCurrentOverlayMember);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtfOverlayObject,base_tfp->overlay_object);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtfOverlayTrans, base_tfp->overlay_trans_obj);
		if (is_map && plot_tfp->do_ndc_overlay == NhlNDCDATAEXTENT) {
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,x);
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,y);
			NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,w);
			NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,h);
		}
		else {
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovl->view.x);
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovl->view.y);
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,ovl->view.width);
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,ovl->view.height);
		}
		NhlSetSArg(&sargs[nargs++],NhlNpmUpdateReq, True);
		NhlSetSArg(&sargs[nargs++],NhlNtfBaseXF,ovl->view.x);
		NhlSetSArg(&sargs[nargs++],NhlNtfBaseYF,ovl->view.y);
		NhlSetSArg(&sargs[nargs++],NhlNtfBaseWidthF,ovl->view.width);
		NhlSetSArg(&sargs[nargs++],NhlNtfBaseHeightF,ovl->view.height);

		subret = NhlALSetValues(sub_recs[i]->plot->base.id,
					sargs,nargs); 
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
	}
/*
 * Update the overlay in order that the annotation objects can adjust
 * to the possibility of some new annotations.
 */
	subret = NhlVASetValues(base_id,NhlNpmUpdateAnnoReq,True,NULL);
	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;

/*
 * Bump up the overlay count, free the sub_recs pointer array, and NULL
 * added overlay record pointer array elements not yet in use.
 */
	NhlFree(sub_recs);
	for (i = ovp->overlay_count; i < ovp->overlay_alloc; i++) {
		ovp->pm_recs[i] = NULL;
	}

	NhlINITVAR(dummy);
	NhlINITVAR(cbdata);
        overlay_status.id = transform_id;
        overlay_status.base_id = base_id;
        overlay_status.status = _tfCurrentOverlayMember;
        cbdata.ptrval = &overlay_status;
        _NhlCallObjCallbacks(plot,_NhlCBtfOverlayStatus,dummy,cbdata);

	return ret;
}

/*
 * Function:	nhlfpaddoverlay
 *
 * Description:	Fortran wrapper for NhlAddOverlay
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
_NHLCALLF(nhlfpaddoverlay,NHLFPADDOVERLAY)
#if	NhlNeedProto
(
	int	*base,
	int	*transform,
	int	*after,
	int	*err
)
#else
(base,plot,after,err)
	int	*base;
	int	*transform;
	int	*after;
	int	*err;
#endif
{
	*err = NhlAddOverlay(*base,*transform,*after);

	return;
}

/*
 * Function:	NhlRemoveOverlay
 *
 * Description:	
 *
 * In Args:	base_id		id of the base plot
 *		overlay_id	id of the overlay to remove
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

NhlErrorTypes NhlRemoveOverlay
#if	NhlNeedProto
(int base_id, int overlay_id, NhlBoolean restore)
#else
(base_id, overlay_id,restore)
        int base_id;
	int overlay_id;
	NhlBoolean restore;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveOverlay";
	NhlLayer		base = _NhlGetLayer(base_id);
	NhlLayer		plot = _NhlGetLayer(overlay_id);

	NhlTransformLayerPart	*base_tfp;
	NhlPlotManagerLayerPart	*ovp;
	int			i, j;
        NhlArgVal       	cbdata,dummy;
        _NhlOverlayStatusCBDataRec overlay_status;
        NhltfOverlayStatus	ostatus = _tfNotInOverlay;

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
		e_text = "%s: invalid overlay id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	base_tfp = &(((NhlTransformLayer)base)->trans);
	if (! base_tfp->plot_manager_on ||
	    base_tfp->overlay_object == NULL || 
	    ! _NhlIsTransform(base_tfp->overlay_object)) {
		e_text = "%s: base is not a plot object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	ovp = (NhlPlotManagerLayerPart *) 
	       &(((NhlPlotManagerLayer)base_tfp->overlay_object)->plotmanager);
	if (ovp->pm_recs[0]->plot != (NhlTransformLayer) base) {
		e_text = "%s: base is not currently a primary base plot";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		return NhlWARNING;
	}
/*
 * This code simply provides an alternative to the SetValues interface
 * removing the overlay object from a base plot. The restore parameter is
 * ignored in this situation
 */
	if (overlay_id == base_id) {
		NhlSArg			sarg;

		NhlSetSArg(&sarg,NhlNtfPlotManagerOn,False);
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
			e_text = "%s: plot not found in overlay sequence";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			return NhlWARNING;
		}
		if (plot == (NhlLayer) ovp->pm_recs[i]->plot) {
			
			if (restore && ovp->pm_recs[i]->ov_obj != NULL) {
				subret = RestoreOverlayBase(ovp,i);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					return ret;
				}
                                ostatus = _tfCurrentOverlayBase;
			}
			else if (ovp->pm_recs[i]->ov_obj != NULL) {

				subret = RemoveOverlayBase(ovp,i);
				if ((ret = MIN(subret,ret)) < NhlWARNING) {
					return ret;
				}
                                ostatus = _tfCurrentOverlayBase;
			}
			else {
				NhlSArg			sargs[3];
				int			nargs = 0;

				for (j = i; j < ovp->overlay_count - 1; j++) {
					ovp->pm_recs[j] = ovp->pm_recs[j+1];
				}
				ovp->pm_recs[--ovp->overlay_count] = NULL;
                                ostatus = _tfNotInOverlay;

				NhlSetSArg(&sargs[nargs++],
					   NhlNtfOverlayObject, NULL);
				NhlSetSArg(&sargs[nargs++],
					   NhlNtfOverlayTrans,NULL);
				NhlSetSArg(&sargs[nargs++],
					   NhlNtfOverlayStatus,
					   _tfNotInOverlay);
				NhlSetSArg(&sargs[nargs++],
					   NhlNpmUpdateReq, True);

				subret = NhlALSetValues(plot->base.id,
							sargs,nargs); 
				if ((ret = MIN(subret,ret)) < NhlWARNING)
					return ret;

			}
			break;
		}
	}
/*
 * Must update the remaining overlay members, so that their annotation
 * objects can adjust to the possibility that some annotations were 
 * removed.
 */
	subret = NhlVASetValues(base_id,NhlNpmUpdateAnnoReq,True,NULL);
	if ((ret = MIN(subret,ret)) < NhlWARNING)
		return ret;

	NhlINITVAR(dummy);
	NhlINITVAR(cbdata);
        overlay_status.id = overlay_id;
        overlay_status.base_id = base_id;
        overlay_status.status = ostatus;
        cbdata.ptrval = &overlay_status;
        _NhlCallObjCallbacks(plot,_NhlCBtfOverlayStatus,dummy,cbdata);

	return ret;
}

/*
 * Function:	nhlfpremoveoverlay
 *
 * Description:	Fortran wrapper for NhlRemoveOverlay
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
_NHLCALLF(nhlfpremoveoverlay,NHLFPREMOVEOVERLAY)
#if	NhlNeedProto
(
	int	*base,
	int	*overlay,
	int	*restore,
	int	*err
)
#else
(base,plot,restore,err)
	int	*base;
	int	*overlay;
	int	*restore;
	int	*err;
#endif
{
	*err = NhlRemoveOverlay(*base,*overlay,*restore);

	return;
}


/*
 * Function:	GetPlotOverlay
 *
 * Description:	Gets the PlotManager belonging to the plot itself. 
 *		For member plots looks through the base plot overlay
 *		data structures.
 *
 * In Args:	transform	transform layer
 *		entry_name      calling function name
 *
 * Out Args:	
 *
 * Return Values: overlay layer; NULL on error;
 *
 * Side Effects: NONE
 */	

NhlLayer GetPlotOverlay
#if	NhlNeedProto
(
	NhlTransformLayer	transform, 
	NhlString		entry_name
)
#else
(transform, entry_name)
	NhlTransformLayer	transform;
	NhlString		entry_name;
#endif
{
	char			*e_text;
	NhlTransformLayerPart	*tfp = &transform->trans;
	NhlPlotManagerLayer	ovl = (NhlPlotManagerLayer)tfp->overlay_object;

	if (! tfp->plot_manager_on  ||
	    tfp->overlay_status == _tfNotInOverlay ||
	    ovl == NULL || ! _NhlIsTransform((NhlLayer) ovl) ||
	    (ovl->base.layer_class)->base_class.class_name !=
	    NhlplotManagerClass->base_class.class_name) {
		e_text = "%s: overlay plot id not a plot object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	
/*
 * If the plot is an base plot, then simply return the base
 * overlay. Otherwise, if the plot is currently an Overlay Member Plot
 * we must look through its base plot's PlotManager data in order to find 
 * its own PlotManager. (It might not have one; that is an error on
 * the part of the caller.)
 */
	if (tfp->overlay_status == _tfCurrentOverlayBase) {
		return (NhlLayer) ovl;
	}
	else {    /* tfp->overlay_status == _tfCurrentOverlayMember */
		NhlPlotManagerLayerPart *ovp = &ovl->plotmanager;
		int i;
		NhlpmRec *pm_rec = NULL;

		for (i = 0; i < ovp->overlay_count; i++) {
			if (ovp->pm_recs[i]->plot == transform) {
				pm_rec = ovp->pm_recs[i];
				break;
			}
		}
		if (pm_rec == NULL) {
			e_text = "%s: inconsistent state";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		if (pm_rec->ov_obj == NULL) {
			e_text = "%s: overlay plot id not a plot object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		return (NhlLayer) pm_rec->ov_obj;
	}
}
/*
 * Function:	NhlAddAnnotation
 *
 * Description:	
 *
 * In Args:	plot_id		id of plot object
 *		anno_view_id	id of View object
 *
 * Out Args:	
 *
 * Return Values: if successful, anno_manager_id: id of AnnoManager object 
 *		  otherwise: Error Conditions
 *
 * Side Effects: NONE
 */	

int NhlAddAnnotation
#if	NhlNeedProto
(int plot_id, int anno_view_id)
#else
(plot_id, anno_view_id)
        int plot_id;
	int anno_view_id;
#endif
{
	char			*e_text;
	char			*entry_name = "NhlAddAnnotation";
	NhlLayer		base = _NhlGetLayer(plot_id);
	NhlLayer		anno_view = _NhlGetLayer(anno_view_id);
	NhlLayer		plot_overlay;

	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	plot_overlay = GetPlotOverlay((NhlTransformLayer)base,entry_name);
	if (plot_overlay == NULL)
		return NhlFATAL;
/*
 * Test the anno view layer pointer.
 */
	if (anno_view == NULL || ! _NhlIsView(anno_view)) {
		e_text = "%s: invalid view id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

/*
 * Ensure that the overlay and the annotation view belong to the 
 * same workstation
 */
	if (anno_view->base.wkptr != base->base.wkptr) {
		e_text = 
	   "%s: view object and Overlay Plot belong to different Workstations";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
/*
 * Ensure that the annotation view is not a current Overlay Member
 */

	if (_NhlIsPlotMember(anno_view_id)) {
		e_text = "%s: view is already an annotation or overlay: %d";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,anno_view_id);
		return NhlFATAL;
	}

	return (_NhlAddAnnotation(plot_overlay,anno_view,entry_name));


}

/*
 * Function:	nhlf_addannotation
 *
 * Description:	Fortran wrapper for NhlAddAnnotation
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
_NHLCALLF(nhlfpaddannotation,NHLFPADDANNOTATION)
#if	NhlNeedProto
(
	int	*plot_id,
	int	*view_id,
	int	*anno_manager_id
)
#else
(plot_id,view_id,anno_manager_id)
	int	*plot_id;
	int	*view_id;
	int	*anno_manager_id;
#endif
{
	*anno_manager_id = NhlAddAnnotation(*plot_id,*view_id);

	return;
}

/*
 * Function:	_NhlAddAnnotation
 *
 * Description:	Private interface for adding an annotation.
 *		Assumes that anno_view and the plot have already
 *		been checked for validity
 *
 * In Args:	plot		PlotManager layer
 *		anno_view	View layer
 *		entry_name	interface name reported to caller in case
 *				of error. If NULL will be set to
 *				_NhlAddAnnotation
 * Out Args:	NONE
 *
 * Return Values: id of AnnoManager object if successful;
 *		  otherwise: Error Conditions
 *
 * Side Effects: NONE
 */	

int _NhlAddAnnotation
#if	NhlNeedProto
(
	NhlLayer	plotmanager, 
	NhlLayer	anno_view,
	NhlString	entry_name
)
#else
(plotmanager, anno_view, entry_name)
	NhlLayer	plotmanager; 
	NhlLayer	anno_view;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlPlotManagerLayerPart	*ovp =
		&((NhlPlotManagerLayer)plotmanager)->plotmanager;
	int			anno_manager_id;
        NhlArgVal       	cbdata,dummy;
        _NhlAnnoStatusCBDataRec	anno_status;
        
	if (entry_name == NULL) entry_name = "_NhlAddAnnotation";
		
/*
 * Make sure there are enough elements in the anno_view and annotation
 * arrays.
 */
	if (ovp->anno_alloc == ovp->anno_count) {
		ovp->anno_alloc += NhlOV_ALLOC_UNIT;
		if (ovp->view_ids == NULL)
			ovp->view_ids = (int *) 
				NhlMalloc(ovp->anno_alloc * sizeof(int));
		else
			ovp->view_ids = (int *) 
				NhlRealloc(ovp->view_ids,
					   ovp->anno_alloc * sizeof(int));
		if (ovp->view_ids == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return (int) NhlFATAL;
		}
		if (ovp->anno_ids == NULL)
			ovp->anno_ids = (int *) 
				NhlMalloc(ovp->anno_alloc * sizeof(int));
		else
			ovp->anno_ids = (int *) 
				NhlRealloc(ovp->anno_ids,
					   ovp->anno_alloc * sizeof(int));
		if (ovp->anno_ids == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return (int) NhlFATAL;
		}
	}
	
/*
 * Add the View to the anno_view array
 */

	ovp->view_ids[ovp->anno_count] = anno_view->base.id;

/*
 * Create an AnnoManager object, giving it the same name as the View
 * object, and making its parent the Overlay base plot object.
 * Then add the AnnoManager to the annomanager object array and 
 * increment the annotation count.
 */

	subret = NhlVACreate(&anno_manager_id,anno_view->base.name,
			     NhlannoManagerClass,
			     plotmanager->base.parent->base.id,
			     NhlNamViewId, anno_view->base.id,
			     NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return (int) ret;

	ovp->anno_ids[ovp->anno_count] = anno_manager_id;
	ovp->anno_count++;

/*
 * Register the annotation
 */

	subret = _NhlRegisterAnnotation(plotmanager,
					_NhlGetLayer(anno_manager_id),
					entry_name);

	ret =  MIN(subret,ret);

	subret = _NhlSetAnnoView((NhlViewLayer)anno_view,
				 plotmanager->base.id,anno_manager_id);

	ret =  MIN(subret,ret);

	NhlINITVAR(dummy);
	NhlINITVAR(cbdata);
        anno_status.id = anno_view->base.id;
        anno_status.base_id = plotmanager->base.parent->base.id;
        anno_status.anno_manager_id = anno_manager_id;
        anno_status.isanno = True;
        cbdata.ptrval = &anno_status;
        _NhlCallObjCallbacks(anno_view,_NhlCBvpAnnoStatus,dummy,cbdata);

	return (ret < NhlNOERROR) ? (int) ret : anno_manager_id;

}

/*
 * Function:	NhlRemoveAnnotation
 *
 * Description:	
 *
 * In Args:	plot_id		id of plot object
 *		anno_manager_id		id of AnnoManager object
 *                                --- modified 11/13/98 to also work with
 *                               id of the View object.
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlRemoveAnnotation
#if	NhlNeedProto
(
	int plot_id, 
	int anno_manager_id
)
#else
(plot_id, anno_manager_id)
        int plot_id;
	int anno_manager_id;
#endif
{
	char			*e_text;
	char			*entry_name = "NhlRemoveAnnotation";
	NhlLayer		base = _NhlGetLayer(plot_id);
	NhlLayer		l = _NhlGetLayer(anno_manager_id);
	NhlLayer		annomanager;
	NhlLayer		plot_overlay;

	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}


	plot_overlay = GetPlotOverlay((NhlTransformLayer)base,entry_name);
	if (plot_overlay == NULL)
		return NhlFATAL;

	if (! l) {
		e_text = "%s: invalid annotation id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (! _NhlIsView(l)) {
		annomanager = l;
	}
	else {
		int id;
		NhlVAGetValues(anno_manager_id,
			       NhlNvpAnnoManagerId,&id,
			       NULL);
		annomanager = _NhlGetLayer(id);
	}
/*
 * Test the annomanager layer pointer.
 */
	if (annomanager == NULL || ! _NhlIsObj(annomanager) ||
	(annomanager->base.layer_class)->base_class.class_name !=
	    NhlannoManagerClass->base_class.class_name) {
		e_text = "%s: invalid annomanager id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	return (_NhlRemoveAnnotation(plot_overlay,annomanager,entry_name));

}

/*
 * Function:	nhlf_removeannotation
 *
 * Description:	Fortran wrapper for NhlRemoveAnnotation
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
_NHLCALLF(nhlfpremoveannotation,NHLFPREMOVEANNOTATION)
#if	NhlNeedProto
(
	int	*plot,
	int	*anno_manager,
	int	*err
)
#else
(plot,anno_manager,err)
	int	*overlay_plot;
	int	*anno_manager;
	int	*err;
#endif
{
	*err = NhlRemoveAnnotation(*plot,*anno_manager);

	return;
}

/*
 * Function:	_NhlRemoveAnnotation
 *
 * Description:	
 *
 * In Args:	plotmanager		Plotmanager object layer
 *		annomanager	AnnoManager object layer
 *		entry_name	interface name reported to caller in case
 *				of error. If NULL will be set to
 *				_NhlRemoveAnnotation
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes _NhlRemoveAnnotation
#if	NhlNeedProto
(
	NhlLayer	plotmanager, 
	NhlLayer	annomanager,
	NhlString	entry_name
)
#else
(plotmanager, annomanager, entry_name)
	NhlLayer	plotmanager; 
	NhlLayer	annomanager;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlPlotManagerLayerPart	*ovp = 
		&((NhlPlotManagerLayer)plotmanager)->plotmanager;
	int			anno_ix = -1;
        NhlLayer		anno_view;
        NhlArgVal       	cbdata,dummy;
	int			i;
	NhlAnnoRec		*anrp;
        _NhlAnnoStatusCBDataRec	anno_status;


	if (entry_name == NULL) entry_name = "_NhlRemoveAnnotation";

	for (i = 0; i < ovp->anno_count; i++) {
		if (ovp->anno_ids[i] == annomanager->base.id) {
			anno_ix = i;
			break;
		}
	}
	if (anno_ix < 0) {
		e_text = "%s: AnnoManager not associated with plot object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
/*
 * Unregister the AnnoManager
 */

	anrp = UnregisterAnnotation(plotmanager,annomanager,entry_name);
	if (! anrp) 
		return NhlFATAL;
        
        anno_view = _NhlGetLayer(ovp->view_ids[anno_ix]);
	if (! anno_view->base.being_destroyed)
		subret = _NhlSetAnnoView((NhlViewLayer)anno_view,
					 NhlNULLOBJID,NhlNULLOBJID);

/*
 * Delete the AnnoManager and View ids from their respective arrays
 */

	for (i = anno_ix; i < ovp->anno_count - 1; i++) {
		ovp->view_ids[i] = ovp->view_ids[i+1];
		ovp->anno_ids[i] = ovp->anno_ids[i+1];
	}
	ovp->anno_count--;

/*
 * Destroy the AnnoManager
 */
	subret = NhlDestroy(annomanager->base.id);

	ret = MIN(subret,ret);
/*
 * Restore the view to its original size and position
 */
	if (! anno_view->base.being_destroyed)
		subret = NhlVASetValues(anno_view->base.id,
					NhlNvpXF,anrp->orig_x,
					NhlNvpYF,anrp->orig_y,
					NhlNvpWidthF,anrp->orig_width,
					NhlNvpHeightF,anrp->orig_height,
					NULL);
	ret = MIN(subret,ret);
/*
 * Free the anno rec.
 */
	NhlFree(anrp);
        
/*
 * Do the anno status callback
 */
	NhlINITVAR(dummy);
	NhlINITVAR(cbdata);
        anno_status.id = anno_view->base.id;
        anno_status.isanno = False;
        anno_status.base_id = plotmanager->base.parent->base.id;
        anno_status.anno_manager_id = NhlNULLOBJID;
        cbdata.ptrval = &anno_status;
        _NhlCallObjCallbacks(anno_view,_NhlCBvpAnnoStatus,dummy,cbdata);

	return ret;
}

/*
 * Function:	NhlRegisterAnnotation
 *
 * Description:	Registers an annotation with an overlay base plot. The
 *		AnnoManager specifies the position of an arbitrary view 
 *		object relative to the overlay base 
 *		plot's viewport.
 *		Note that the annotation always remains with the
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
#if	NhlNeedProto
(int plot_id, int annomanager_id)
#else
(plot_id, annomanager_id)
        int plot_id;
	int annomanager_id;
#endif
{
	char			*e_text;
	char			*entry_name = "NhlRegisterAnnotation";
	NhlLayer		base = _NhlGetLayer(plot_id);
	NhlLayer		annomanager = _NhlGetLayer(annomanager_id);
	NhlLayer		plot_overlay;

	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid base plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	plot_overlay = GetPlotOverlay((NhlTransformLayer)base,entry_name);
	if (plot_overlay == NULL)
		return NhlFATAL;

/*
 * Test the annomanager layer pointer; if valid get its resource values.
 */
	if (annomanager == NULL) {
		e_text = "%s: invalid AnnoManager id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

/*
 * Ensure that the overlay and the annomanager belong to the 
 * same workstation
 */
	if (annomanager->base.wkptr != base->base.wkptr) {
		e_text = 
	  "%s: View object and Overlay Plot belong to different Workstations";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	return (_NhlRegisterAnnotation(plot_overlay,annomanager,entry_name));
}

/*
 * Function:	_NhlRegisterAnnotation
 *
 * Description:	
 *
 * In Args:	overlay		Overlay object layer
 *		annotation	Annotation object layer
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes _NhlRegisterAnnotation
#if	NhlNeedProto
(
	NhlLayer	overlay, 
	NhlLayer	annomanager,
	NhlString	entry_name
)
#else
(overlay, annomanager, entry_name)
	NhlLayer	overlay; 
	NhlLayer	annomanager;
	NhlString	entry_name;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlAnnoRec		*anrp;
	int			pid,zone;
	NhlPosition		side;
	NhlJustification	just;
	float			ortho, parallel,data_x,data_y;
	NhlBoolean		on,resize_notify,track_data;

	if (entry_name == NULL) entry_name = "_NhlRegisterAnnotation";

	subret = NhlVAGetValues(annomanager->base.id,
				NhlNamOn,&on,
				NhlNamResizeNotify,&resize_notify,
				NhlNamViewId,&pid,
				NhlNamZone,&zone,
				NhlNamSide,&side,
				NhlNamJust,&just,
				NhlNamOrthogonalPosF,&ortho,
				NhlNamParallelPosF,&parallel,
				NhlNamTrackData,&track_data,
				NhlNamDataXF,&data_x,
				NhlNamDataYF,&data_y,
				NULL);

	if ((ret = MIN(subret,ret)) < NhlWARNING) return NhlFATAL;

/*
 * Record the annotation in the overlay's data structures; a pointer
 * to the new annotation record is returned.
 */ 
	if ((anrp = RecordAnnotation((NhlPlotManagerLayer)overlay,ovEXTERNAL,
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
        anrp->viewable = on; /* later this will be adjusted based on the
                                viewable status of the owner plot */
	anrp->anno_id = annomanager->base.id;
	anrp->plot_id = pid;
	anrp->resize_notify = resize_notify;
	anrp->side = side;
	anrp->just = just;
	anrp->para_pos = parallel;
	anrp->ortho_pos = ortho;
	anrp->track_data = track_data;
	anrp->data_x = data_x;
	anrp->data_y = data_y;
/*
 * Get the current view of the annotation so that it can be restored if
 * the annotation is removed.
 */
	NhlVAGetValues(pid,
		       NhlNvpXF,&anrp->orig_x,
		       NhlNvpYF,&anrp->orig_y,
		       NhlNvpWidthF,&anrp->orig_width,
		       NhlNvpHeightF,&anrp->orig_height,
		       NULL);
		       
	subret = NhlVASetValues(annomanager->base.id,
				NhlNamOverlayId,overlay->base.id,
				NULL);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	return ret;
}

/*
 * Function:	NhlUnregisterAnnotation
 *
 * Description:	
 *
 * In Args:	plot_id		id of the annotation's base plot
 *		annomanager_id	id of AnnoManager Object
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlUnregisterAnnotation
#if	NhlNeedProto
(int plot_id, int annomanager_id)
#else
(plot_id, annomanager_id)
        int plot_id;
	int annomanager_id;
#endif
{
	char			*e_text;
	char			*entry_name = "NhlUnregisterAnnotation";
	NhlLayer		base = _NhlGetLayer(plot_id);
	NhlLayer		annomanager = _NhlGetLayer(annomanager_id);
	NhlLayer		plot_overlay;
	NhlAnnoRec		*anrp;

	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid base plot id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	plot_overlay = GetPlotOverlay((NhlTransformLayer)base,entry_name);
	if (plot_overlay == NULL)
		return NhlFATAL;

/*
 * Test the annomanager layer pointer.
 */
	if (annomanager == NULL) {
		e_text = "%s: invalid AnnoManager id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	anrp = UnregisterAnnotation(plot_overlay,annomanager,entry_name);
	if (! anrp) {
		e_text = "%s: annotation not found";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	NhlFree(anrp);

	return NhlNOERROR;
}

/*
 * Function:	UnregisterAnnotation
 *
 * Description:	
 *
 * In Args:	overlay		Overlay object layer
 *		annomanager	AnnoManager object layer
 *		entry_name	interface name reported to caller in case
 *				of error. If NULL will be set to
 *				UnregisterAnnotation
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static
NhlAnnoRec *UnregisterAnnotation
#if	NhlNeedProto
(
	NhlLayer	overlay, 
	NhlLayer	annomanager,
	NhlString	entry_name
)
#else
(overlay, annomanager, entry_name)
	NhlLayer	overlay; 
	NhlLayer	annomanager;
	NhlString	entry_name;
#endif
{
	NhlPlotManagerLayerPart	*ovp;
	NhlAnnoRec		**tanrp;
	NhlAnnoRec		*anrp = NULL;
	int			i;
	NhlTransformLayerPart	*tfp = &((NhlPlotManagerLayer)overlay)->trans;
	NhlPlotManagerLayer	base_pm = NULL;

	if (entry_name == NULL) entry_name = "UnregisterAnnotation";

	if (tfp->overlay_status == _tfCurrentOverlayMember)
		base_pm = (NhlPlotManagerLayer) tfp->overlay_object;

	ovp = &((NhlPlotManagerLayer)overlay)->plotmanager;

	for (i = 0; i < ovp->overlay_count; i++) {
		
		tanrp = &ovp->pm_recs[i]->anno_list;
		while (*tanrp) {
			if ((*tanrp)->anno_id == annomanager->base.id) {
				anrp = *tanrp;
				*tanrp = anrp->next;
				anrp->next = NULL;
				break;
			}
			tanrp = &(*tanrp)->next;
		}
		if (anrp)
			break;
	}
	if (base_pm) {
		NhlpmRec **pm_recs = base_pm->plotmanager.pm_recs;
		int i;

		for (i = 0; i < base_pm->plotmanager.overlay_count; i++) {
			if (pm_recs[i]->ov_obj == overlay) {
				pm_recs[i]->anno_list = 
					ovp->pm_recs[0]->anno_list;
				break;
			}
		}
	}

	if (! annomanager->base.being_destroyed)
		NhlVASetValues(annomanager->base.id,
			       NhlNamOverlayId,NhlNULLOBJID,NULL);

	return anrp;

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
#if	NhlNeedProto
(
	NhlLayer		overlay_object
)
#else
(overlay_object)
	NhlLayer			overlay_object;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlPlotManagerLayerPart	*ovp = 
				  &((NhlPlotManagerLayer)overlay_object)->plotmanager;
	int			i;
	NhlSArg			sargs[10];
	int			nargs = 0;
        NhlArgVal       	cbdata,dummy;
        _NhlOverlayStatusCBDataRec overlay_status;
        NhltfOverlayStatus	ostatus;
	NhlLayer		plot;

	while (ovp->overlay_count > 1) {
		NhlpmRec *orec = ovp->pm_recs[1];
		plot = (NhlLayer) orec->plot;
		if (orec->ov_obj != NULL) {
			subret = RestoreOverlayBase(ovp,1);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				return ret;
			}
			ostatus = _tfCurrentOverlayBase;
		}
		else {
			NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject, NULL);
			NhlSetSArg(&sargs[nargs++],NhlNtfOverlayTrans,NULL);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtfOverlayStatus,_tfNotInOverlay);
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,orec->ox);
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,orec->oy);
			NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,orec->owidth);
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,orec->oheight);

			subret = NhlALSetValues(orec->plot->base.id,
						sargs,nargs); 
			if ((ret = MIN(subret,ret)) < NhlWARNING)
				return ret;

			for (i = 1; i < ovp->overlay_count - 1; i++) {
				ovp->pm_recs[i] = ovp->pm_recs[i+1];
			}
			ovp->pm_recs[--ovp->overlay_count] = NULL;
			nargs = 0;
			ostatus = _tfNotInOverlay;
		}

		NhlINITVAR(dummy);
		NhlINITVAR(cbdata);
		overlay_status.id = plot->base.id;
		overlay_status.base_id = overlay_object->base.parent->base.id;
		overlay_status.status = ostatus;
		cbdata.ptrval = &overlay_status;
		_NhlCallObjCallbacks(plot,_NhlCBtfOverlayStatus,dummy,cbdata);
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
#if	NhlNeedProto
(
	NhlPlotManagerLayerPart	*ovp,
	int			plot_number
)
#else
(ovp,plot_number)
	NhlPlotManagerLayerPart	*ovp;
	int			plot_number;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveOverlay";
	NhlPlotManagerLayer	plot_ovl = (NhlPlotManagerLayer) 
					ovp->pm_recs[plot_number]->ov_obj;
	NhlTransformLayer	plot = ovp->pm_recs[plot_number]->plot;
	NhlGenArray		ga;
	NhlpmRec		**pm_recs = NULL;
	NhlSArg			sargs[10];
        int			nargs = 0;
	int			i;
	ng_size_t		count = 1;

/*
 * Create a GenArray of 1 element in order to set the PlotManagerRecs resource
 * of the overlay plot. This will erase its memory of its overlay plots.
 */

	pm_recs = (NhlpmRec **) NhlMalloc(sizeof(NhlpmRec *));
	if (pm_recs == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	pm_recs[0] = ovp->pm_recs[plot_number];
	pm_recs[0]->plot = plot;
	pm_recs[0]->ov_obj = (NhlLayer) plot_ovl;

	for (i = plot_number; i < ovp->overlay_count - 1; i++) {
		ovp->pm_recs[i] = ovp->pm_recs[i+1];
	}
	ovp->pm_recs[--ovp->overlay_count] = NULL;

	ga = NhlCreateGenArray((NhlPointer)pm_recs,NhlTPointer,
			       sizeof(NhlpmRec *),1,&count);
	if (ga == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NhlNpmPlotManagerRecs);
		return NhlFATAL;
	}
	ga->my_data = True;

	NhlSetSArg(&sargs[nargs++],NhlNpmPlotManagerRecs, ga);
	NhlSetSArg(&sargs[nargs++],
		   NhlNtfOverlayStatus,_tfCurrentOverlayBase);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject,plot_ovl);
	NhlSetSArg(&sargs[nargs++],
		   NhlNtfOverlayTrans,plot->trans.trans_obj);
	
	subret = NhlALSetValues(plot->base.id,sargs,nargs); 
	ret = MIN(subret,ret);

		  
	NhlFree(pm_recs[0]);
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
 * In Args:	NhlPlotManagerLayerPart ovp - overlay object that plot is being
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
#if	NhlNeedProto
(
	NhlPlotManagerLayerPart	*ovp,
	int			plot_number
)
#else
(ovp,plot_number)
	NhlPlotManagerLayerPart	*ovp;
	int			plot_number;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveOverlay";
	NhlPlotManagerLayer	plot_ovl = (NhlPlotManagerLayer) 
					ovp->pm_recs[plot_number]->ov_obj;
	NhlTransformLayer	plot = ovp->pm_recs[plot_number]->plot;
	NhlGenArray		ga;
	NhlpmRec		**sub_recs = NULL;
	NhlpmRec		*orec = ovp->pm_recs[plot_number];
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
				NhlNpmPlotManagerRecs,&ga,
				NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		return ret;
	}
	if (ga == NULL || ga->size != sizeof(NhlpmRec *)) {
		e_text = "%s: error retrieving internal gen array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	new_plot_count = ga->num_elements;
	sub_recs = (NhlpmRec **) ga->data;

	for (i = 0; i < new_plot_count; ) {
		for (j = 0; j <= ovp->overlay_count; j++) {
			if (j == ovp->overlay_count) {
				NhlFree(sub_recs[i]);
				for (k = i; k < new_plot_count - 1; k++) {
					sub_recs[k] = sub_recs[k+1];
				}
				sub_recs[--new_plot_count] = NULL;
			}
			else if (ovp->pm_recs[j]->plot == sub_recs[i]->plot) {
				if (ovp->pm_recs[j] != orec)  
					NhlFree(ovp->pm_recs[j]);
				for (k = j; k < ovp->overlay_count - 1; k++) {
					ovp->pm_recs[k] = ovp->pm_recs[k+1];
				}
				ovp->pm_recs[--ovp->overlay_count] = NULL;
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
		NhlLayer overlay = (NhlLayer) sub_recs[i]->plot;

		nargs = 0;
		NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject, plot_ovl);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtfOverlayTrans,plot->trans.trans_obj);
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,orec->ox);
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,orec->oy);
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,orec->owidth);
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,orec->oheight);

		subret = NhlALSetValues(overlay->base.id,sargs,nargs); 
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
	}
	nargs = 0;

/*
 * If the new plot count is different from the old, (indicating that a plot
 * that was a member of the plot overlay was removed while it was a member
 * of the current overlay) use the same GenArray to send the new contents 
 * of the overlay record to the plot (and therefore to its overlay object).
 */

	if (new_plot_count != ga->num_elements) {
		ga->num_elements =ga->len_dimensions[0] = new_plot_count;
		NhlSetSArg(&sargs[nargs++],NhlNpmPlotManagerRecs, ga);
	}


/*
 * Set the trans and overlay resources for the newly restored 
 * overlay base plot.
 */

	NhlSetSArg(&sargs[nargs++],
		   NhlNtfOverlayStatus,_tfCurrentOverlayBase);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject,plot_ovl);
	NhlSetSArg(&sargs[nargs++],
		   NhlNtfOverlayTrans,plot->trans.trans_obj);
	NhlSetSArg(&sargs[nargs++],NhlNpmUpdateReq,True);
	NhlSetSArg(&sargs[nargs++],NhlNvpXF,orec->ox);
	NhlSetSArg(&sargs[nargs++],NhlNvpYF,orec->oy);
	NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,orec->owidth);
	NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,orec->oheight);
	
	subret = NhlALSetValues(plot->base.id,sargs,nargs); 
	ret = MIN(subret,ret);
		  
/*
 * Free the GenArray -- since the my_data flag is set True,
 * the pointer array will be freed by the GenArray free call, but it is still
 * necessary to free each of the overlay records.
 */

	NhlFree(orec);
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
#if	NhlNeedProto
(
	NhlLayer	*overlay_object,
	NhlLayer	lnew,
	NhlLayer	lold,
	_NhlCalledFrom	method,
	NhlSArgList	sargs,
	int		nargs,
	char		*entry_name
)
#else 
(overlay_object,lnew,lold,method,sargs,nargs,entry_name)
	NhlLayer	*overlay_object;
	NhlLayer	lnew;
	NhlLayer	lold;
	_NhlCalledFrom	method;
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
	int			tmpid = NhlNULLOBJID;
	char			buffer[_NhlMAXRESNAMLEN];
	NhlSArg			*lsargs;
	int			lsarg_count = 32; /* Keep up to date!!! */

	if (*overlay_object == NULL) {
		if (! tfp->plot_manager_on)
			return ret;
		else if (method != _NhlCREATE) {
			e_text = "%s: resetting create-only resource: %s";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
				  entry_name,NhlNtfPlotManagerOn);
			tfp->plot_manager_on = False;
			return NhlWARNING;
		}
	}

	if ((method != _NhlCREATE) && ! _NhlIsTransform(*overlay_object)) {
		e_text = "%s: invalid overlay object passed in";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if(method == _NhlUPDATEDATA){
		return NhlALSetValues((*overlay_object)->base.id,sargs,nargs);
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

	if(method == _NhlCREATE){

		strcpy(buffer,lnew->base.name);
		strcat(buffer,".PlotManager");
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
		subret = _NhlALCreateChild(&tmpid,buffer,
					   NhlplotManagerClass,
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

		NhlFree(lsargs);

		return ret;
	}
	else if (tfp->plot_manager_on == False) {

		if (tfp->overlay_status == _tfCurrentOverlayMember) {
			e_text = 
	       "%s: must remove from overlay before destroying overlay base";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			tfp->plot_manager_on = True;
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
 * Actually there is an exception; if the overlay_object (the plot manager) 
 * has changed, it means that the overlay has been moved from one base
 * plot to another. This can happen when NhlRemoveOverlay is called with
 * restore set to True.
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

		if (tfp->overlay_object->base.id ==
		    otfp->overlay_object->base.id) {
			NhlBoolean error = False;
			if (_NhlIsClass(tfp->overlay_trans_obj,
					NhlmapTransObjClass) &&
			    tfp->do_ndc_overlay == NhlNDCDATAEXTENT) {
				float l,r,t,b;
				NhlVAGetValues(tfp->overlay_trans_obj->base.id,
					       NhlNmpLeftNDCF,&l,
					       NhlNmpRightNDCF,&r,
					       NhlNmpTopNDCF,&t,
					       NhlNmpBottomNDCF,&b,
					       NULL);
				if (_NhlCmpFAny(vwp->x,l,6) != 0.0 ||
				    _NhlCmpFAny(vwp->y,t,6) != 0.0 ||
				    _NhlCmpFAny(vwp->width,r-l,6) != 0.0 ||
				    _NhlCmpFAny(vwp->height,t-b,6) != 0.0) {
					_NhlInternalSetView
						((NhlViewLayer)lnew,
						 l,t,r-l,t-b,
						 ovvl->view.keep_aspect);
					error = True;			
				}
			}
			else if (vwp->x != ovvl->view.x ||
				 vwp->y != ovvl->view.y ||
				 vwp->width != ovvl->view.width ||
				 vwp->height != ovvl->view.height) {

				_NhlInternalSetView((NhlViewLayer)lnew,
						    ovvl->view.x,
						    ovvl->view.y,
						    ovvl->view.width,
						    ovvl->view.height,
						    ovvl->view.keep_aspect);
				error = True;
			}
			if (error) {
				e_text =
			"%s: attempt to set overlay member plot view ignored";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name);
				ret = MIN(ret,NhlWARNING);
			}
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

		if (tfp->bx != otfp->bx)
			NhlSetSArg(&lsargs[nargs++],NhlNtfBaseXF,tfp->bx);
		if (tfp->by != otfp->by)
			NhlSetSArg(&lsargs[nargs++],NhlNtfBaseYF,tfp->by);
		if (tfp->bw != otfp->bw)
			NhlSetSArg(&lsargs[nargs++],NhlNtfBaseWidthF,tfp->bw);
		if (tfp->bh != otfp->bh)
			NhlSetSArg(&lsargs[nargs++],NhlNtfBaseHeightF,tfp->bh);

		if (vwp->use_segments != ovwp->use_segments)
			NhlSetSArg(&lsargs[nargs++],
                                   NhlNvpUseSegments,vwp->use_segments);

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
