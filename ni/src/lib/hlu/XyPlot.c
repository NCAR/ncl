/*
 *      $Id: XyPlot.c,v 1.45 1995-04-29 18:53:49 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		XyPlot.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Dec 30 13:46:21 MST 1992
 *
 *	Description:	Source for XyPlot hlu.
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/XyPlotP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/TransObjI.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/CoordArrTableFloatP.h>

#define	XMISS_SET	0x01
#define	YMISS_SET	0x02
#define	DEF_SEG_FACTOR (0.25)
#define	DEF_FHEIGHT_FACTOR (0.015)


/*
 * Resource Default Functions.
 */
/*
 * Function:	ResUnset
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
ResUnset
#if	NhlNeedProto
(
	NrmName		name,
	NrmClass	cname,
	NhlPointer	base,
	unsigned int	offset
)
#else
(name,cname,base,offset)
	NrmName		name;
	NrmClass	cname;
	NhlPointer	base;
	unsigned int	offset;
#endif
{
	char *cl = (char *)base;
	NhlBoolean *set = (NhlBoolean *)(cl + offset - sizeof(NhlBoolean));

	*set = False;

	return NhlNOERROR;
}

#define	Oset(field)	NhlOffset(NhlXyDataSpecLayerRec,xydata.field)
static NhlResource data_resources[] = {

/* Begin-documented-resources */

	{NhlNxyLineColor,NhlCxyLineColor,NhlTColorIndex,sizeof(NhlColorIndex),
		Oset(color),NhlTImmediate,(NhlPointer)NhlFOREGROUND,
		_NhlRES_DEFAULT,NULL},
	{NhlNxyLineColors,NhlCxyLineColors,NhlTColorIndexGenArray,
		sizeof(NhlGenArray),Oset(colors),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoLineColor,NhlCxyMonoLineColor,NhlTBoolean,sizeof(NhlBoolean),
		Oset(mono_color),NhlTImmediate,(NhlPointer)False,
		_NhlRES_DEFAULT,NULL},

	{NhlNxyDashPattern,NhlCxyDashPattern,NhlTDashIndex,sizeof(NhlDashIndex),
		Oset(dash),NhlTImmediate,(NhlPointer)NhlSOLIDLINE,
		_NhlRES_DEFAULT,NULL},
	{NhlNxyDashPatterns,NhlCxyDashPatterns,NhlTDashIndexGenArray,
		sizeof(NhlGenArray), Oset(dashes),NhlTImmediate,
		(NhlPointer)NULL, _NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoDashPattern,NhlCxyMonoDashPattern,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_dash),NhlTImmediate,
		(NhlPointer)False,_NhlRES_DEFAULT,NULL},

	{NhlNxyLineThicknessF,NhlCxyLineThicknessF,NhlTFloat,sizeof(float),
		Oset(line_thickness),NhlTString,(NhlPointer)"1.0",
		_NhlRES_DEFAULT,NULL},
	{NhlNxyLineThicknesses,NhlCxyLineThicknesses,NhlTFloatGenArray,
		sizeof(NhlGenArray), Oset(line_thicknesses),NhlTImmediate,
		(NhlPointer)NULL, _NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoLineThickness,NhlCxyMonoLineThickness,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_line_thickness),NhlTImmediate,
		(NhlPointer)False,_NhlRES_DEFAULT,NULL},

	{NhlNxyMarkLineMode,NhlCxyMarkLineMode,NhlTMarkLineMode,
		sizeof(NhlMarkLineMode),Oset(marker_mode),NhlTImmediate,
		(NhlPointer)NhlLINES,_NhlRES_DEFAULT,(NhlFreeFunc)NULL},
	{NhlNxyMarkLineModes,NhlCxyMarkLineModes,NhlTMarkLineModeGenArray,
		sizeof(NhlGenArray),Oset(marker_modes),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarkLineMode,NhlCxyMonoMarkLineMode,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker_mode),NhlTImmediate,
		(NhlPointer)False,_NhlRES_DEFAULT,NULL},

	{NhlNxyMarker,NhlCxyMarker,NhlTMarkerIndex,sizeof(NhlMarkerIndex),
		Oset(marker),NhlTImmediate,(NhlPointer)0,_NhlRES_DEFAULT,NULL},
	{NhlNxyMarkers,NhlCxyMarkers,NhlTMarkerIndexGenArray,
		sizeof(NhlGenArray),Oset(markers),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarker,NhlCxyMonoMarker,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker),NhlTImmediate,
		(NhlPointer)False,_NhlRES_DEFAULT,NULL},

	{NhlNxyMarkerColor,NhlCxyMarkerColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(marker_color),NhlTImmediate,
		(NhlPointer)NhlFOREGROUND,_NhlRES_DEFAULT,(NhlFreeFunc)NULL},
	{NhlNxyMarkerColors,NhlCxyMarkerColors,NhlTColorIndexGenArray,
		sizeof(NhlGenArray),Oset(marker_colors),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarkerColor,NhlCxyMonoMarkerColor,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker_color),NhlTImmediate,
		(NhlPointer)False,_NhlRES_DEFAULT,NULL},

	{".not",".not",NhlTBoolean,sizeof(NhlBoolean),Oset(marker_size_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{NhlNxyMarkerSizeF,NhlCxyMarkerSizeF,NhlTFloat,
		sizeof(float),Oset(marker_size),NhlTProcedure,
		(NhlPointer)ResUnset,_NhlRES_DEFAULT,(NhlFreeFunc)NULL},
	{NhlNxyMarkerSizes,NhlCxyMarkerSizes,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(marker_sizes),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarkerSize,NhlCxyMonoMarkerSize,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker_size),NhlTImmediate,
		(NhlPointer)False,_NhlRES_DEFAULT,NULL},

	{NhlNxyMarkerThicknessF,NhlCxyMarkerThicknessF,NhlTFloat,sizeof(float),
		Oset(marker_thickness),NhlTString,(NhlPointer)"1.0",
		_NhlRES_DEFAULT,NULL},
	{NhlNxyMarkerThicknesses,NhlCxyMarkerThicknesses,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(marker_thicknesses),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarkerThickness,NhlCxyMonoMarkerThickness,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker_thickness),NhlTImmediate,
		(NhlPointer)False,_NhlRES_DEFAULT,NULL},

	{NhlNxyLabelMode,NhlCxyLabelMode,NhlTLineLabelMode,
		sizeof(NhlLineLabelMode),
		Oset(label_mode),NhlTImmediate,(NhlPointer)NhlNOLABELS,
		_NhlRES_DEFAULT,NULL},
	{NhlNxyExplicitLabels,NhlCxyExplicitLabels,NhlTStringGenArray,
		sizeof(NhlGenArray),Oset(labels),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},

	{NhlNxyLineLabelFontColor,NhlCxyLineLabelFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(label_color),NhlTImmediate,
		(NhlPointer)NhlFOREGROUND,_NhlRES_DEFAULT,(NhlFreeFunc)NULL},
	{NhlNxyLineLabelFontColors,NhlCxyLineLabelFontColors,
		NhlTColorIndexGenArray,
		sizeof(NhlGenArray),Oset(label_colors),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoLineLabelFontColor,NhlCxyMonoLineLabelFontColor,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_label_color),NhlTImmediate,
		(NhlPointer)False,_NhlRES_DEFAULT,NULL},

	{NhlNxyExplicitLegendLabels,NhlCxyExplicitLegendLabels,
		NhlTStringGenArray,
		sizeof(NhlGenArray),Oset(lg_label_strings),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},

	{".not",".not",NhlTBoolean,sizeof(NhlBoolean),Oset(dash_seg_len_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{NhlNxyLineDashSegLenF,NhlCxyLineDashSegLenF,NhlTFloat,
		sizeof(float),Oset(dash_seg_len),NhlTProcedure,
		(NhlPointer)ResUnset,_NhlRES_DEFAULT,NULL},

	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(llabel_fheight_set),NhlTImmediate,(NhlPointer)True,
		_NhlRES_NOACCESS,NULL},
	{NhlNxyLineLabelFontHeightF,NhlCxyLineLabelFontHeightF,NhlTFloat,
		sizeof(float),Oset(llabel_fheight),NhlTProcedure,
		(NhlPointer)ResUnset,_NhlRES_DEFAULT,NULL},

/* End-documented-resources */

};
#undef Oset

#define	Oset(field)	NhlOffset(NhlXyPlotLayerRec,xyplot.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNxyCoordData,NhlCxyCoordData,_NhlTDataList,sizeof(NhlGenArray),
		Oset(curve_data),NhlTImmediate,NULL,_NhlRES_NORACCESS,
						(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyCoordDataSpec,NhlCxyCoordDataSpec,_NhlTDataSpecList,
		sizeof(NhlGenArray),Oset(dspeclist),NhlTImmediate,NULL,
		_NhlRES_GONLY,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyXStyle,NhlCxyXStyle,NhlTTickMarkStyle,sizeof(NhlTickMarkStyle),
		Oset(x_style),NhlTImmediate,(NhlPointer)NhlLINEAR,
		_NhlRES_DEFAULT,NULL},
	{NhlNxyYStyle,NhlCxyYStyle,NhlTTickMarkStyle,sizeof(NhlTickMarkStyle),
		Oset(y_style),NhlTImmediate,(NhlPointer)NhlLINEAR,
		_NhlRES_DEFAULT,NULL},
	{NhlNtrXTensionF,NhlCtrXTensionF,NhlTFloat,sizeof(float),
		Oset(x_tension),NhlTString,"2.0",_NhlRES_DEFAULT,NULL},
	{NhlNtrYTensionF,NhlCtrYTensionF,NhlTFloat,sizeof(float),
		Oset(y_tension),NhlTString,"2.0",_NhlRES_DEFAULT,NULL},

	{NhlNxyXIrregularPoints,NhlCxyXIrregularPoints,NhlTFloatGenArray,
		sizeof(NhlPointer),Oset(x_irregular_points),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyYIrregularPoints,NhlCxyYIrregularPoints,NhlTFloatGenArray,
		sizeof(NhlPointer),Oset(y_irregular_points),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},

	{NhlNtrXReverse,NhlCtrXReverse,NhlTBoolean,sizeof(NhlBoolean),
		Oset(x_reverse),NhlTImmediate,False,_NhlRES_DEFAULT,NULL},
	{NhlNtrYReverse,NhlCtrYReverse,NhlTBoolean,sizeof(NhlBoolean),
		Oset(y_reverse),NhlTImmediate,False,_NhlRES_DEFAULT,NULL},

	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(comp_x_min_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(comp_x_max_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(comp_y_max_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(comp_y_min_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},

	{NhlNxyComputeXMin,NhlCxyComputeExtent,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_x_min),NhlTProcedure,(NhlPointer)ResUnset,
		_NhlRES_DEFAULT,NULL},
	{NhlNxyComputeXMax,NhlCxyComputeExtent,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_x_max),NhlTProcedure,(NhlPointer)ResUnset,
		_NhlRES_DEFAULT,NULL},
	{NhlNxyComputeYMax,NhlCxyComputeExtent,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_y_max),NhlTProcedure,(NhlPointer)ResUnset,
		_NhlRES_DEFAULT,NULL},
	{NhlNxyComputeYMin,NhlCxyComputeExtent,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_y_min),NhlTProcedure,(NhlPointer)ResUnset,
		_NhlRES_DEFAULT,NULL},

	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(x_min_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(x_max_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(y_max_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(y_min_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},

	{NhlNtrXMinF,NhlCtrXMinF,NhlTFloat,sizeof(float),Oset(x_min),
		NhlTProcedure,(NhlPointer)ResUnset,_NhlRES_DEFAULT,NULL},
	{NhlNtrXMaxF,NhlCtrXMaxF,NhlTFloat,sizeof(float),Oset(x_max),
		NhlTProcedure,(NhlPointer)ResUnset,_NhlRES_DEFAULT,NULL},
	{NhlNtrYMaxF,NhlCtrYMaxF,NhlTFloat,sizeof(float),Oset(y_max),
		NhlTProcedure,(NhlPointer)ResUnset,_NhlRES_DEFAULT,NULL},
	{NhlNtrYMinF,NhlCtrYMinF,NhlTFloat,sizeof(float),Oset(y_min),
		NhlTProcedure,(NhlPointer)ResUnset,_NhlRES_DEFAULT,NULL},

/*
 * These resources have not been implimented yet.
 */
#ifdef	NOT
	{NhlNxyXAlternate,NhlCxyXAlternate,NhlTAlternatePlace,
		sizeof(NhlAlternatePlace),Oset(x_alternate),NhlTImmediate,
		(NhlPointer)NhlNONE,_NhlRES_DEFAULT,NULL},
	{NhlNxyYAlternate,NhlCxyYAlternate,NhlTAlternatePlace,
		sizeof(NhlAlternatePlace),Oset(y_alternate),NhlTImmediate,
		(NhlPointer)NhlNONE,_NhlRES_DEFAULT,NULL},
	{NhlNxyXAlternateCoords,NhlCxyXAlternateCoords,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(x_alternate_coords),NhlTImmediate,NULL,
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyXOriginalCoords,NhlCxyXOriginalCoords,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(x_original_coords),NhlTImmediate,NULL,
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyYAlternateCoords,NhlCxyYAlternateCoords,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(y_alternate_coords),NhlTImmediate,NULL,
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyYOriginalCoords,NhlCxyYOriginalCoords,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(y_original_coords),NhlTImmediate,NULL,
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
#endif
/* End-documented-resources */

	{NhlNpmTitleDisplayMode,NhlCpmTitleDisplayMode,
		NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		Oset(display_titles),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlCONDITIONAL),_NhlRES_DEFAULT,NULL},
	{NhlNpmTickMarkDisplayMode,NhlCpmTickMarkDisplayMode,
		NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		Oset(display_tickmarks),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlCONDITIONAL),_NhlRES_DEFAULT,NULL},
	{NhlNpmLegendDisplayMode,NhlCpmLegendDisplayMode,
		NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		Oset(display_legend),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlNEVER),_NhlRES_DEFAULT,NULL},
	{_NhlNxyDSpecChanged,_NhlCxyDSpecChanged,NhlTBoolean,sizeof(NhlBoolean),
		Oset(dspec_changed),NhlTImmediate,NULL,_NhlRES_SONLY,NULL},
};
#undef Oset


/* base methods */

static NhlErrorTypes XyDataSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes XyPlotSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes XyDataGetValues(
#if	NhlNeedProto
        NhlLayer	l,
        _NhlArgList	args,
        int		nargs
#endif
);

static NhlErrorTypes XyPlotGetValues(
#if	NhlNeedProto
        NhlLayer	l,
        _NhlArgList	args,
        int		nargs
#endif
);

static NhlErrorTypes XyDataInitialize(
#if	NhlNeedProto
        NhlClass,     /* cname */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes XyPlotInitialize(
#if	NhlNeedProto
        NhlClass,     /* cptr */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes XyPlotClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes XyDataClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes XyPlotClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes XyPlotDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes XyPlotDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

/* 
* View Methods
*/

static NhlErrorTypes XyPlotGetBB(
#if	NhlNeedProto
        NhlLayer          /* instance */,
        NhlBoundingBox * /*thebox*/
#endif
);

/*
* Transform Methods
*/

static NhlErrorTypes XyPlotUpdateData(
#if	NhlNeedProto
	NhlDataCommLayer	new,
	NhlDataCommLayer	old
#endif
);

static NhlErrorTypes CheckValues(
#if	NhlNeedProto
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
#endif
);

static NhlErrorTypes InternalizePointers(
#if	NhlNeedProto
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
#endif
);

static NhlErrorTypes ComputeDataExtents(
#if	NhlNeedProto
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
#endif
);

static NhlErrorTypes SetUpTransObjs(
#if	NhlNeedProto
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
#endif
);

static NhlErrorTypes SetUpDataSpec(
#if	NhlNeedProto
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
#endif
);

static NhlErrorTypes SetUpTicks(
#if	NhlNeedProto
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom,
	NhlSArg		*sargs,
	int		*nargs
#endif
);
static NhlErrorTypes SetUpTitles(
#if	NhlNeedProto
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom,
	NhlSArg		*sargs,
	int		*nargs
#endif
);
static NhlErrorTypes SetUpLegend(
#if	NhlNeedProto
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes DrawCurves(
#if	NhlNeedProto
	NhlXyPlotLayer	xlayer
#endif
);

NhlXyDataSpecClassRec NhlxyDataSpecClassRec = {
	/* base_class */
        {
/* class_name			*/	"xyDataSpecClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlXyDataSpecLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)
						&NhldataSpecClassRec,

/* layer_resources		*/	data_resources,
/* num_resources		*/	NhlNumber(data_resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	XyDataClassInitialize,
/* layer_initialize		*/	XyDataInitialize,
/* layer_set_values		*/	XyDataSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	XyDataGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	},
	/* dataspec_class */
	{
/* foo				*/	0
	},
	/* xydatadep_class */
	{
/* foo				*/	0
	}
};

NhlXyPlotClassRec NhlxyPlotClassRec = {
	/* base_class */
        {
/* class_name                   */      "xyPlotClass",
/* nrm_class                    */      NrmNULLQUARK,
/* layer_size                   */      sizeof(NhlXyPlotLayerRec),
/* class_inited                 */      False,
/* superclass                   */      (NhlClass)
						&NhldataCommClassRec,

/* layer_resources              */      resources,
/* num_resources                */      NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize        */      XyPlotClassPartInitialize,
/* class_initialize             */      XyPlotClassInitialize,
/* layer_initialize             */      XyPlotInitialize,
/* layer_set_values             */      XyPlotSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values             */      XyPlotGetValues,
/* layer_reparent               */      NULL,
/* layer_destroy                */      XyPlotDestroy,

/* child_resources              */      NULL,

/* layer_draw                   */      XyPlotDraw,

/* layer_pre_draw               */      NULL,
/* layer_draw_segonly           */      NULL,
/* layer_post_draw              */      NULL,
/* layer_clear                  */      NULL
        },
	/* view_class */
	{
/* segment_workstation		*/	-1,
/* get_bb			*/	XyPlotGetBB
	},
	/* trans_class */
	{
/* overlay_capability 		*/	_tfOverlayBaseOrMember,
/* data_to_ndc			*/	NhlInheritTransFunc,
/* ndc_to_data			*/	NhlInheritTransFunc,
/* data_polyline		*/	NhlInheritPolyTransFunc,
/* ndc_polyline			*/	NhlInheritPolyTransFunc
	},
	/* datacomm_class */
	{
/* data_offsets			*/	NULL,
/* update_data			*/	XyPlotUpdateData
	},
	/* xyplot_class */
	{
/* foo				*/	NULL
	}
};

NhlClass NhlxyDataSpecClass =
				(NhlClass)&NhlxyDataSpecClassRec;
NhlClass NhlxyPlotClass = (NhlClass)&NhlxyPlotClassRec;

/*
 * Function:	nhlfxyplotclass
 *
 * Description:	fortran ref to this class
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NHLCALLF(nhlfxyplotclass,NHLFXYPLOTCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlxyPlotClass;
}

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;

static NrmQuark QXirreg = NrmNULLQUARK;
static NrmQuark QYirreg = NrmNULLQUARK;
static NrmQuark QXalt = NrmNULLQUARK;
static NrmQuark QYalt = NrmNULLQUARK;
static NrmQuark QXorig = NrmNULLQUARK;
static NrmQuark QYorig = NrmNULLQUARK;

static NrmQuark Qcolors = NrmNULLQUARK;
static NrmQuark Qdpatterns = NrmNULLQUARK;
static NrmQuark Qmarkmodes = NrmNULLQUARK;
static NrmQuark Qmarkers = NrmNULLQUARK;
static NrmQuark Qmarksizes = NrmNULLQUARK;
static NrmQuark Qmarkercolors = NrmNULLQUARK;
static NrmQuark Qlabels = NrmNULLQUARK;
static NrmQuark Qlabelcolors = NrmNULLQUARK;
static NrmQuark Qlinethicknesses = NrmNULLQUARK;
static NrmQuark Qmarkerthicknesses = NrmNULLQUARK;
static NrmQuark Qlglabelstrings = NrmNULLQUARK;
static NrmQuark Qmarksize = NrmNULLQUARK;
static NrmQuark Qdseglen = NrmNULLQUARK;
static NrmQuark Qllfontheight = NrmNULLQUARK;

/*
 * Function:	XyDataClassInitialize
 *
 * Description:	init quark for latter use.
 *
 * In Args:	
 *		void
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
XyDataClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlEnumVals	lblmode[] = {
		{NhlNOLABELS,	"nolabels"},
		{NhlLETTERED,	"lettered"},
		{NhlCUSTOM,	"custom"}
	};

	_NhlRegisterEnumType(NhlTLineLabelMode,lblmode,NhlNumber(lblmode));
	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);

	Qcolors = NrmStringToQuark(NhlNxyLineColors);
	Qdpatterns = NrmStringToQuark(NhlNxyDashPatterns);
	Qmarkmodes = NrmStringToQuark(NhlNxyMarkLineModes);
	Qmarkers = NrmStringToQuark(NhlNxyMarkers);
	Qmarksizes = NrmStringToQuark(NhlNxyMarkerSizes);
	Qmarkercolors = NrmStringToQuark(NhlNxyMarkerColors);
	Qlabels = NrmStringToQuark(NhlNxyExplicitLabels);
	Qlabelcolors = NrmStringToQuark(NhlNxyLineLabelFontColors);
	Qlinethicknesses = NrmStringToQuark(NhlNxyLineThicknesses);
	Qmarkerthicknesses = NrmStringToQuark(NhlNxyMarkerThicknesses);
	Qlglabelstrings = NrmStringToQuark(NhlNxyExplicitLegendLabels);
	Qmarksize = NrmStringToQuark(NhlNxyMarkerSizeF);
	Qdseglen = NrmStringToQuark(NhlNxyLineDashSegLenF);
	Qllfontheight = NrmStringToQuark(NhlNxyLineLabelFontHeightF);

	return NhlNOERROR;
}

/*
 * Function:	XyPlotClassInitialize
 *
 * Description:	Add type converters for types added to support this class.
 *
 * In Args:	NhlNONE
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NhlNONE
 */
static NhlErrorTypes
XyPlotClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	Qfloat = NrmStringToQuark(NhlTFloat);

	QXirreg = NrmStringToQuark(NhlNxyXIrregularPoints);
	QYirreg = NrmStringToQuark(NhlNxyYIrregularPoints);

/*
 * These resources have not been implimented yet...
 */
#ifdef	NOT
	_NhlEnumVals	altplace[] = {
		{NhlNONE,	"none"},
		{NhlLEFTAXIS,	"leftaxis"},
		{NhlRIGHTAXIS,	"rightaxis"},
		{NhlTOPAXIS,	"topaxis"},
		{NhlBOTTOMAXIS,	"bottomaxis"}
	};

	_NhlRegisterEnumType(NhlTAlternatePlace,altplace,NhlNumber(altplace));

	QXalt = NrmStringToQuark(NhlNxyXAlternateCoords);
	QYalt = NrmStringToQuark(NhlNxyYAlternateCoords);
	QXorig = NrmStringToQuark(NhlNxyXOriginalCoords);
	QYorig = NrmStringToQuark(NhlNxyYOriginalCoords);
#endif

	return NhlNOERROR;
}

/*
 * Function:	XyPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the NhlXyPlotClassPart
 *		that can not be done by static initialization.
 *		Takes care of calling _NhlRegisterChildClass for the title
 *		and the tick mark objects. This needs to be done so resource
 *		forwarding can work. All Title resources are forwarded with
 *		the exception of the Offset*F resources. 
 *		The tick mark object has several resources blocked. These 
 *		resources all have dependencies to the tm**Style resources.
 *		Since these dependencies exist all of the resouces must be
 *		set in the same SetValues or Create calls. Therefore they
 *		are blocked and storage exists, in the XyPlot Object instance
 *		record, for these resources. The primary reason for blocking
 *		these resouces is the tm**Style resources. These are controled
 *		by the XyPlot so that both of the x axis and the y axis can
 *		have correct transformation.
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
XyPlotClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* NhlLayer Class to init	*/
)
#else
(lc)
	NhlClass	lc;	/* NhlLayer Class to init	*/
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes lret = NhlNOERROR;

	/*
	 * Register children objects
	 */
	ret = _NhlRegisterChildClass(lc,NhlplotManagerClass,False,False,
			NhlNlgDashIndex,NhlNlgDashIndexes,NhlNlgItemCount,
			NhlNlgItemType,NhlNlgItemTypes,NhlNlgLabelStrings,
			NhlNlgLineColor,NhlNlgLineColors,NhlNlgLineDashSegLenF,
			NhlNlgMonoLineDashSegLen,NhlNlgLineDashSegLens,
			NhlNlgLineLabelFontColor,NhlNlgLineLabelFontColors,
			NhlNlgLineLabelConstantSpacingF,
			NhlNlgLineLabelFont,NhlNlgLineLabelFontAspectF,
			NhlNlgLineLabelFontHeightF,NhlNlgLineLabelFontHeights,
			NhlNlgLineLabelFontQuality,
			NhlNlgLineLabelFontThicknessF,NhlNlgLineLabelFuncCode,
			NhlNlgLineLabelStrings,NhlNlgLineThicknessF,
			NhlNlgLineThicknesses,
			NhlNlgMarkerColor,NhlNlgMarkerColors,
			NhlNlgMarkerIndex,NhlNlgMarkerIndexes,
			NhlNlgMarkerSizeF,NhlNlgMarkerSizes,
			NhlNlgMarkerThicknessF,NhlNlgMarkerThicknesses,
			NhlNlgMonoDashIndex,NhlNlgMonoItemType,
			NhlNlgMonoLineColor,NhlNlgMonoLineLabelFontColor,
			NhlNlgMonoLineLabelFontHeight,NhlNlgMonoLineThickness,
			NhlNlgMonoMarkerColor,NhlNlgMonoMarkerIndex,
			NhlNlgMonoMarkerSize,NhlNlgMonoMarkerThickness,
			NhlNpmLabelBarDisplayMode,
			NULL);

	/*
	 * Register Data Resources
	 */

	lret = _NhlRegisterDataRes((NhlDataCommClass)lc,NhlNxyCoordData,
			NhlNxyCoordDataSpec,NhlxyDataSpecClass,
			NhlcoordArrTableFloatClass,NULL);
	return MIN(lret,ret);
}

/*
 * Function:	XyPlotChanges
 *
 * Description:	called by setvalues and initialize to do all the common things.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
XyPlotChanges
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
)
#else
(xnew,xold,calledfrom)
	NhlXyPlotLayer	xnew;
	NhlXyPlotLayer	xold;
	_NhlCalledFrom	calledfrom;
#endif
{
	NhlErrorTypes	ret1 = NhlNOERROR;
	NhlErrorTypes	ret2 = NhlNOERROR;
	NhlSArg		sargs[128];
	int		nsargs=0;

	if((calledfrom == _NhlSETVALUES) && xnew->xyplot.dspec_changed){

		xnew->xyplot.dspec_changed = False;

		ret2 = SetUpDataSpec(xnew,xold,calledfrom);
		if(ret2 < NhlWARNING)
			return(ret2);
		ret1 = MIN(ret1,ret2);

		ret2 = SetUpLegend(xnew,xold,calledfrom,sargs,&nsargs);
		if(ret2 < NhlWARNING)
			return(ret2);
		ret1 = MIN(ret1,ret2);

		ret2 = _NhlManageOverlay(&xnew->xyplot.overlay,(NhlLayer)xnew,
			(NhlLayer)xold,(calledfrom == _NhlCREATE),sargs,nsargs,
			"XyPlotChanges");
		ret1 = MIN(ret1,ret2);

		return ret1;
	}

	ret2 = InternalizePointers(xnew,xold,calledfrom);
	if(ret2 < NhlWARNING)
		return ret2;
	ret1 = MIN(ret1,ret2);

	ret2 = CheckValues(xnew,xold,calledfrom);
	if(ret2 < NhlWARNING)
		return ret2;
	ret1 = MIN(ret1,ret2);

	ret2 = ComputeDataExtents(xnew,xold,calledfrom);
	if(ret2 < NhlWARNING)
		return ret2;
	ret1 = MIN(ret1,ret2);

	ret2 = SetUpTransObjs(xnew,xold,calledfrom);
	if(ret2 < NhlWARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);

	ret2 = SetUpDataSpec(xnew,xold,calledfrom);
	if(ret2 < NhlWARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);


	if(xnew->xyplot.check_ranges){
		NhlSetSArg(&sargs[nsargs++],NhlNpmUpdateReq,True);
		xnew->xyplot.check_ranges = False;
	}

	ret2 = SetUpTitles(xnew,xold,calledfrom,sargs,&nsargs);
	if(ret2 < NhlWARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);

	ret2 = SetUpTicks(xnew,xold,calledfrom,sargs,&nsargs);
	if(ret2 < NhlWARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);

	ret2 = SetUpLegend(xnew,xold,calledfrom,sargs,&nsargs);
	if(ret2 < NhlWARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);

	ret2 = _NhlManageOverlay(&xnew->xyplot.overlay,(NhlLayer)xnew,
			(NhlLayer)xold,(calledfrom == _NhlCREATE),sargs,nsargs,
			"XyPlotChanges");
	ret1 = MIN(ret1,ret2);

	return ret1;
}

/*
 * Function:	XyDataInitialize
 *
 * Description:	Initializes the XyData Dependent class instance.
 *
 * In Args:	
 *		NhlClass	class,
 *		NhlLayer		req,
 *		NhlLayer		new,
 *		_NhlArgList	args,
 *		int		num_args
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
XyDataInitialize
#if	NhlNeedProto
(
	NhlClass	cptr,
	NhlLayer		req,
	NhlLayer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(cptr,req,new,args,num_args)
        NhlClass      cptr;
        NhlLayer           req;
        NhlLayer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	char			*error_lead = "XyDataInitialize";
	NhlXyDataSpecLayerPart	*dnp = &((NhlXyDataSpecLayer)new)->xydata;
	NhlViewLayerPart	*vp =
				&((NhlViewLayer)new->base.parent)->view;
	float			ave;

	dnp->dashes=_NhlCopyGenArray(dnp->dashes,True);
	dnp->marker_modes=_NhlCopyGenArray(dnp->marker_modes,True);
	dnp->lg_label_strings=_NhlCopyGenArray(dnp->lg_label_strings,True);
	dnp->colors=_NhlCopyGenArray(dnp->colors,True);
	dnp->label_colors=_NhlCopyGenArray(dnp->label_colors,True);
	dnp->labels=_NhlCopyGenArray(dnp->labels,True);
	dnp->line_thicknesses=_NhlCopyGenArray(dnp->line_thicknesses,True);
	dnp->marker_colors=_NhlCopyGenArray(dnp->marker_colors,True);
	dnp->markers=_NhlCopyGenArray(dnp->markers,True);
	dnp->marker_sizes=_NhlCopyGenArray(dnp->marker_sizes,True);
	dnp->marker_thicknesses=_NhlCopyGenArray(dnp->marker_thicknesses,True);

	/*
	 * Can't use XyPlot vp_average, since we may not have gotten through
	 * XyPlotInitialize yet.
	 */
	ave = (vp->width + vp->height)/2.0;
	if(dnp->marker_size_set)
		dnp->marker_size = dnp->marker_size / ave;
	else
		dnp->marker_size = DEF_FHEIGHT_FACTOR;

	if(dnp->dash_seg_len_set)
		dnp->dash_seg_len = dnp->dash_seg_len / ave;
	else
		dnp->dash_seg_len = DEF_SEG_FACTOR;

	if(dnp->llabel_fheight_set)
		dnp->llabel_fheight = dnp->llabel_fheight / ave;
	else
		dnp->llabel_fheight = DEF_FHEIGHT_FACTOR;

	return NhlNOERROR;
}

/*
 * Function:	XyPlotInitialize
 *
 * Description: 
 *
 * In Args: 	class	objects layer_class
 *		req	instance record of requested values
 *		new	instance record of new object
 *		args	list of resources and values for reference
 *		num_args 	number of elements in args.
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	state change in GKS due to mapping transformations.
 */
/*ARGSUSED*/
static NhlErrorTypes
XyPlotInitialize
#if	NhlNeedProto
(
	NhlClass	cptr,
	NhlLayer		req,
	NhlLayer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(cptr,req,new,args,num_args)
        NhlClass      cptr;
        NhlLayer           req;
        NhlLayer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlXyPlotLayer		xnew = (NhlXyPlotLayer)new;
	NhlXyPlotLayerPart	*xp = &xnew->xyplot;
	NhlTransformLayerPart	*tfp = &xnew->trans;

	if(!xp->comp_x_min_set) xp->compute_x_min = False;
	if(!xp->comp_x_max_set) xp->compute_x_max = False;
	if(!xp->comp_y_max_set) xp->compute_y_max = False;
	if(!xp->comp_y_min_set) xp->compute_y_min = False;

	if(!xp->x_min_set) xp->x_min = 1.0;
	if(!xp->x_max_set) xp->x_max = 2.0;
	if(!xp->y_min_set) xp->y_min = 1.0;
	if(!xp->y_max_set) xp->y_max = 2.0;

	xp->thetrans = NULL;
	tfp->trans_obj = NULL;
	xp->data_ranges_set = False;
	xp->check_ranges = True;
	xp->overlay = NULL;

	xp->vp_average = (xnew->view.width + xnew->view.height) / 2.0;

	xp->num_cpairs = 0;
	xp->size_cpair_arrays = 0;

	xp->dash_indexes = _NhlCreateGenArray(NULL,NhlTDashIndex,
					sizeof(NhlDashIndex),0,NULL,True);
	xp->item_types = _NhlCreateGenArray(NULL,NhlTMarkLineMode,
					sizeof(NhlMarkLineMode),0,NULL,True);
	xp->lg_label_strings = _NhlCreateGenArray(NULL,NhlTString,
					sizeof(NhlString),0,NULL,True);
	xp->line_colors = _NhlCreateGenArray(NULL,NhlTColorIndex,
					sizeof(NhlColorIndex),0,NULL,True);
	xp->dash_seg_lens = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
	xp->llabel_colors = _NhlCreateGenArray(NULL,NhlTColorIndex,
					sizeof(NhlColorIndex),0,NULL,True);
	xp->llabel_strings = _NhlCreateGenArray(NULL,NhlTString,
					sizeof(NhlString),0,NULL,True);
	xp->llabel_fheights = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
	xp->line_thicknesses = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
	xp->marker_colors = _NhlCreateGenArray(NULL,NhlTColorIndex,
					sizeof(NhlColorIndex),0,NULL,True);
	xp->marker_indexes = _NhlCreateGenArray(NULL,NhlTMarkerIndex,
					sizeof(NhlMarkerIndex),0,NULL,True);
	xp->marker_sizes = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
	xp->marker_thicknesses = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
	xp->xvectors = _NhlCreateGenArray(NULL,NhlTPointer,
					sizeof(NhlPointer),0,NULL,True);
	xp->yvectors = _NhlCreateGenArray(NULL,NhlTPointer,
					sizeof(NhlPointer),0,NULL,True);
	xp->len_vectors = _NhlCreateGenArray(NULL,NhlTInteger,
					sizeof(int),0,NULL,True);
	xp->missing_set = _NhlCreateGenArray(NULL,NhlTInteger,
					sizeof(int),0,NULL,True);
	xp->xmissing = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
	xp->ymissing = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);

	if(!xp->dash_indexes || !xp->item_types || !xp->lg_label_strings ||
		!xp->line_colors || !xp->dash_seg_lens || !xp->llabel_colors ||
		!xp->llabel_strings || !xp->llabel_fheights ||
		!xp->line_thicknesses || !xp->marker_colors ||
		!xp->marker_indexes || !xp->marker_sizes ||
		!xp->marker_thicknesses || !xp->xvectors || !xp->yvectors ||
		!xp->len_vectors || !xp->missing_set || !xp->xmissing ||
		!xp->ymissing){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}

	return XyPlotChanges((NhlXyPlotLayer)new,NULL,_NhlCREATE);
}

/*
 * Function:	XyDataSetValues
 *
 * Description:	Initializes the XyData Dependent class instance.
 *
 * In Args:	
 *		NhlClass	class,
 *		NhlLayer		req,
 *		NhlLayer		new,
 *		_NhlArgList	args,
 *		int		num_args
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
XyDataSetValues
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
	char			func[] = "XyDataSetValues";
	NhlXyDataSpecLayerPart	*xdnp = &((NhlXyDataSpecLayer)new)->xydata;
	NhlXyDataSpecLayerPart	*xdop = &((NhlXyDataSpecLayer)old)->xydata;
	NhlGenArray		gen;
	NhlBoolean		status = False;
	int			i;
	float			*fptr;
	NhlXyPlotLayerPart	*xyp =
				&((NhlXyPlotLayer)new->base.parent)->xyplot;

	if(xdnp->dashes != xdop->dashes){
		gen = xdnp->dashes;
		xdnp->dashes = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->dashes){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyDashPatterns);
			xdnp->dashes = xdop->dashes;
		}
		else{
			NhlFreeGenArray(xdop->dashes);
			status = True;
		}
	}

	if(xdnp->marker_modes != xdop->marker_modes){
		gen = xdnp->marker_modes;
		xdnp->marker_modes = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->marker_modes){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyMarkLineMode);
			xdnp->marker_modes = xdop->marker_modes;
		}
		else{
			NhlFreeGenArray(xdop->marker_modes);
			status = True;
		}
	}

	if(xdnp->lg_label_strings != xdop->lg_label_strings){
		gen = xdnp->lg_label_strings;
		xdnp->lg_label_strings = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->lg_label_strings){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyExplicitLegendLabels);
			xdnp->lg_label_strings = xdop->lg_label_strings;
		}
		else{
			NhlFreeGenArray(xdop->lg_label_strings);
			status = True;
		}
	}

	if(xdnp->colors != xdop->colors){
		gen = xdnp->colors;
		xdnp->colors = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->colors){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyLineColors);
			xdnp->colors = xdop->colors;
		}
		else{
			NhlFreeGenArray(xdop->colors);
			status = True;
		}
	}

	if(xdnp->label_colors != xdop->label_colors){
		gen = xdnp->label_colors;
		xdnp->label_colors = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->label_colors){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyLineLabelFontColors);
			xdnp->label_colors = xdop->label_colors;
		}
		else{
			NhlFreeGenArray(xdop->label_colors);
			status = True;
		}
	}

	if(xdnp->labels != xdop->labels){
		gen = xdnp->labels;
		xdnp->labels = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->labels){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyExplicitLabels);
			xdnp->labels = xdop->labels;
		}
		else{
			NhlFreeGenArray(xdop->labels);
			status = True;
		}
	}

	if(xdnp->line_thicknesses != xdop->line_thicknesses){
		gen = xdnp->line_thicknesses;
		xdnp->line_thicknesses = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->line_thicknesses){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyLineThicknesses);
			xdnp->line_thicknesses = xdop->line_thicknesses;
		}
		else{
			NhlFreeGenArray(xdop->line_thicknesses);
			status = True;
		}
	}

	if(xdnp->marker_colors != xdop->marker_colors){
		gen = xdnp->marker_colors;
		xdnp->marker_colors = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->marker_colors){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyMarkerColors);
			xdnp->marker_colors = xdop->marker_colors;
		}
		else{
			NhlFreeGenArray(xdop->marker_colors);
			status = True;
		}
	}

	if(xdnp->markers != xdop->markers){
		gen = xdnp->markers;
		xdnp->markers = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->markers){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyMarkers);
			xdnp->markers = xdop->markers;
		}
		else{
			NhlFreeGenArray(xdop->markers);
			status = True;
		}
	}

	if(xdnp->marker_sizes != xdop->marker_sizes){
		gen = xdnp->marker_sizes;
		xdnp->marker_sizes = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->marker_sizes){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyMarkerSizes);
			xdnp->marker_sizes = xdop->marker_sizes;
		}
		else{
			NhlFreeGenArray(xdop->marker_sizes);
			status = True;

			fptr = (float*)xdnp->marker_sizes->data;
			for(i=0;i<xdnp->marker_sizes->num_elements;i++)
				fptr[i] = fptr[i] / xyp->vp_average;
		}
	}

	if(xdnp->marker_thicknesses != xdop->marker_thicknesses){
		gen = xdnp->marker_thicknesses;
		xdnp->marker_thicknesses = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->marker_thicknesses){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyMarkerThicknesses);
			xdnp->marker_thicknesses = xdop->marker_thicknesses;
		}
		else{
			NhlFreeGenArray(xdop->marker_thicknesses);
			status = True;
		}
	}

	if(xdnp->marker_size != xdop->marker_size){
		xdnp->marker_size = xdnp->marker_size / xyp->vp_average;
	}
	if(xdnp->dash_seg_len != xdop->dash_seg_len){
		xdnp->dash_seg_len = xdnp->dash_seg_len / xyp->vp_average;
	}
	if(xdnp->llabel_fheight != xdop->llabel_fheight){
		xdnp->llabel_fheight = xdnp->llabel_fheight / xyp->vp_average;
	}

	return NhlVASetValues(new->base.parent->base.id,
				_NhlNxyDSpecChanged,	True,
				NULL);
}

/*
 * Function:	XyPlotSetValues
 *
 * Description: 
 *
 * In Args:	old	copy of old instance record
 *		reference	requested instance record
 *		new	new instance record	
 *		args 	list of resources and values for reference
 *		num_args	number of elements in args.
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes
XyPlotSetValues
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
	NhlXyPlotLayer	xn = (NhlXyPlotLayer)new;
	NhlXyPlotLayer	xo = (NhlXyPlotLayer)old;

	if((xn->view.width != xo->view.width) ||
		(xn->view.height != xo->view.height))
		xn->xyplot.vp_average = (xn->view.width+xn->view.height)/2.0;

	return XyPlotChanges(xn,xo,_NhlSETVALUES);
}

/*
 * Function:	XyDataGetValues
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
XyDataGetValues
#if	NhlNeedProto
(
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
)
#else
(l,args,nargs)
	NhlLayer	l;
	_NhlArgList	args;
	int		nargs;
#endif
{
	char			func[] = "XyDataGetValues";
	NhlXyDataSpecLayerPart	*xyp = &((NhlXyDataSpecLayer)l)->xydata;
	NhlXyPlotLayerPart	*pp = &((NhlXyPlotLayer)l->base.parent)->xyplot;
	int			i,j;
	NhlGenArray		ga,gen;
	NhlBoolean		scale;
	float			*fptr;

	NhlErrorTypes		ret = NhlNOERROR;

	for(i=0;i<nargs;i++){
		ga = NULL;
		scale = False;

		if(args[i].quark == Qdpatterns){
			ga = xyp->dashes;
		}
		else if(args[i].quark == Qmarkmodes){
			ga = xyp->marker_modes;
		}
		else if(args[i].quark == Qlglabelstrings){
			ga = xyp->lg_label_strings;
		}
		else if(args[i].quark == Qcolors){
			ga = xyp->colors;
		}
		else if(args[i].quark == Qlabelcolors){
			ga = xyp->label_colors;
		}
		else if(args[i].quark == Qlabels){
			ga = xyp->labels;
		}
		else if(args[i].quark == Qlinethicknesses){
			ga = xyp->line_thicknesses;
		}
		else if(args[i].quark == Qmarkercolors){
			ga = xyp->marker_colors;
		}
		else if(args[i].quark == Qmarkers){
			ga = xyp->markers;
		}
		else if(args[i].quark == Qmarksizes){
			ga = xyp->marker_sizes;
			scale = True;
		}
		else if(args[i].quark == Qmarkerthicknesses){
			ga = xyp->marker_thicknesses;
		}
		else if((args[i].quark == Qmarksize) ||
			(args[i].quark == Qdseglen) ||
			(args[i].quark == Qllfontheight)){
			scale = True;
		}

		if(ga != NULL){
			*((NhlGenArray *)args[i].value.ptrval) =
						_NhlCopyGenArray(ga,True);
			if(!*(NhlGenArray *)args[i].value.ptrval){
				NhlPError(NhlWARNING,ENOMEM,
					"%s:Unable to retrieve %s",func,
					NrmQuarkToString(args[i].quark));
				ret = NhlWARNING;
			}
			else if(scale){
				gen = *((NhlGenArray *)args[i].value.ptrval);
				fptr = gen->data;
				for(j=0;j<gen->num_elements;j++)
					fptr[j] = fptr[j] * pp->vp_average;
			}
		}
		else if(scale){
			fptr = (float *)args[i].value.ptrval;
			*fptr = *fptr * pp->vp_average;
		}
	}

	return ret;
}

/*
 * Function:	XyPlotGetValues
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
XyPlotGetValues
#if	NhlNeedProto
(
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
)
#else
(l,args,nargs)
	NhlLayer	l;
	_NhlArgList	args;
	int		nargs;
#endif
{
	char			func[] = "XyPlotGetValues";
	NhlXyPlotLayerPart	*xyp = &((NhlXyPlotLayer)l)->xyplot;
	int			i;
	NhlGenArray		ga;
	NhlErrorTypes		ret = NhlNOERROR;

	for(i=0;i<nargs;i++){
		ga = NULL;

		if(args[i].quark == QXirreg){
			ga = xyp->x_irregular_points;
		}
		else if(args[i].quark == QYirreg){
			ga = xyp->y_irregular_points;
		}
/*
 * These resources have not been implimented yet...
 */
#ifdef	NOT
		else if(args[i].quark == QXalt){
			ga = xyp->x_alternate_coords;
		}
		else if(args[i].quark == QYalt){
			ga = xyp->y_alternate_coords;
		}
		else if(args[i].quark == QXorig){
			ga = xyp->x_original_coords;
		}
		else if(args[i].quark == QYorig){
			ga = xyp->y_original_coords;
		}
#endif

		if(ga != NULL){
			*((NhlGenArray *)args[i].value.ptrval) =
						_NhlCopyGenArray(ga,True);
			if(!*(NhlGenArray *)args[i].value.ptrval){
				NhlPError(NhlWARNING,ENOMEM,
					"%s:Unable to retrieve %s",func,
					NrmQuarkToString(args[i].quark));
				ret = NhlWARNING;
			}
		}
	}

	return ret;
}

/*
 * Function:	CleanStrings
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static void
CleanStrings
#if	NhlNeedProto
(
	NhlGenArray	gen
)
#else
(gen)
	NhlGenArray	gen;
#endif
{
	NhlString	*str = gen->data;
	int		i;

	for(i=0;i < gen->num_elements;i++){
		NhlFree(str[i]);
		str[i] = NULL;
	}

	return;
}

/*
 * Function:	GrowGen
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
GrowGen
#if	NhlNeedProto
(
	int		new_size,
	NhlGenArray	gen
)
#else
(new_size,gen)
	int		new_size;
	NhlGenArray	gen;
#endif
{
	int		i,j;
	NhlString	c,*str;
	gen->data = NhlRealloc(gen->data,gen->size * new_size);
	if(!gen->data){
		gen->num_elements = 0;
		return NhlFATAL;
	}

	memset((char*)gen->data,0,gen->size*new_size);
	gen->num_elements = new_size;

	return NhlNOERROR;
}

/*
 * Function:	SetUpDataSpec
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
 */
static NhlErrorTypes
SetUpDataSpec
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
)
#else 
(xnew,xold,calledfrom)
	NhlXyPlotLayer	xnew;
	NhlXyPlotLayer	xold;
	_NhlCalledFrom	calledfrom;
#endif
{
	NhlErrorTypes			ret = NhlNOERROR;
	NhlXyPlotLayerPart		*xlp = &xnew->xyplot;
	_NhlDataNodePtr			*datalist = NULL;
	NhlBoolean			new;
	int				current_letter=0;
	int				num_data;
	int				i,j;
	int				index;
	char				buffer[80];
	NhlCoordArrTableFloatLayer	datal;
	NhlXyDataSpecLayer		dataspec;
	NhlXyDataSpecLayerPart		*dsp;
	NhlDashIndex			*dash_indexes;
	NhlMarkLineMode			*item_types;
	NhlString			*lg_label_strings;
	NhlColorIndex			*line_colors;
	float				*dash_seg_lens;
	NhlColorIndex			*llabel_colors;
	NhlString			*llabel_strings;
	float				*llabel_fheights;
	float				*line_thicknesses;
	NhlColorIndex			*marker_colors;
	NhlMarkerIndex			*marker_indexes;
	float				*marker_sizes;
	float				*marker_thicknesses;
	float				**xvectors;
	float				**yvectors;
	int				*len_vectors;
	int				*missing_set;
	float				*xmissing;
	float				*ymissing;
	int				tint;

	memset(buffer,'\0',sizeof(buffer));

	/*
	 * clean out string arrays.  This could end up happening, even if
	 * the strings are not changing, but I can't think of another
	 * reasonable way to prevent a memory leak.
	 */
	CleanStrings(xlp->lg_label_strings);
	CleanStrings(xlp->llabel_strings);

	if(xlp->num_cpairs > xlp->size_cpair_arrays){
		ret = GrowGen(xlp->num_cpairs,xlp->dash_indexes);
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->item_types));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->lg_label_strings));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->line_colors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->dash_seg_lens));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_colors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_strings));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_fheights));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->line_thicknesses));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->marker_colors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->marker_indexes));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->marker_sizes));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->marker_thicknesses));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->xvectors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->yvectors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->len_vectors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->missing_set));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->xmissing));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->ymissing));

		if(ret < NhlNOERROR){
			xlp->size_cpair_arrays = 0;
			return NhlFATAL;
		}

		xlp->size_cpair_arrays = xlp->num_cpairs;
	}

	dash_indexes = xlp->dash_indexes->data;
	item_types = xlp->item_types->data;
	lg_label_strings = xlp->lg_label_strings->data;
	line_colors = xlp->line_colors->data;
	dash_seg_lens = xlp->dash_seg_lens->data;
	llabel_colors = xlp->llabel_colors->data;
	llabel_strings = xlp->llabel_strings->data;
	llabel_fheights = xlp->llabel_fheights->data;
	line_thicknesses = xlp->line_thicknesses->data;
	marker_colors = xlp->marker_colors->data;
	marker_indexes = xlp->marker_indexes->data;
	marker_sizes = xlp->marker_sizes->data;
	marker_thicknesses = xlp->marker_thicknesses->data;
	xvectors = xlp->xvectors->data;
	yvectors = xlp->yvectors->data;
	len_vectors = xlp->len_vectors->data;
	missing_set = xlp->missing_set->data;
	xmissing = xlp->xmissing->data;
	ymissing = xlp->ymissing->data;

	memset(missing_set,0,
			xlp->missing_set->size*xlp->missing_set->num_elements);

	num_data = _NhlGetDataInfo(xlp->curve_data,&datalist);
	if(num_data <= 0){
		xlp->data_ranges_set = False;
		xlp->num_cpairs = 0;

		return NhlNOERROR;
	}

	for(i=0,index=0;i < num_data;i++){
		NhlString	label;
		float		**yvalues;
		float		**xvalues;
		int		*len_yvalues;
		int		*len_xvalues;
		NhlBoolean	impx;
		NhlBoolean	impy;
		int		num_curves=0;

		int		*dashtable=NULL;
		int		len_dashtable=0;
		NhlMarkLineMode	*markermodetable=NULL;
		int		len_markermodetable= 0;
		NhlString	*lglabeltable=NULL;
		int		len_lglabeltable=0;
		int		*ctable=NULL;
		int		len_ctable=0;
		int		*labelcolortable= NULL;
		int		len_labelcolortable = 0;
		NhlString	*labeltable=NULL;
		int		len_labeltable =0;
		float		*linethicktable = NULL;
		int		len_linethicktable = 0;
		int		*markercolortable= NULL;
		int		len_markercolortable = 0;
		int		*markertable=NULL;
		int		len_markertable= 0;
		float		*markersizetable=NULL;
		float		len_markersizetable= 0;
		float		*markerthicktable = NULL;
		int		len_markerthicktable = 0;

		/*
		 * Retrieve Data Information
		 */
		datal=(NhlCoordArrTableFloatLayer)_NhlGetDataSet(datalist[i],
									&new);
		if(datal == NULL){
			xlp->data_ranges_set = False;
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Data Problem???");
			return NhlFATAL;
		}
		if(datal->flt.ytable != NULL){
			yvalues = (float**)datal->flt.ytable->data;
			len_yvalues = (int*)datal->flt.ytable_lens->data;
			num_curves = datal->flt.ytable->len_dimensions[0];
			impy = False;
		}
		else{
			yvalues = NULL;
			len_yvalues = NULL;
			impy = True;
		}
		if(datal->flt.xtable != NULL){
			xvalues = (float**)datal->flt.xtable->data;
			len_xvalues = (int*)datal->flt.xtable_lens->data;
			impx = False;
			if(impy)
				num_curves =
				datal->flt.xtable->len_dimensions[0];
			else
				num_curves =
					MIN(num_curves,
				datal->flt.xtable->len_dimensions[0]);
		}
		else{
			xvalues = NULL;
			len_xvalues = NULL;
			impx = True;
			if(impy){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Data has implied X and implied Y?");
				return NhlFATAL;
			}
		}

		/*
		 * Retrieve Data Specific information
		 */
		dataspec = (NhlXyDataSpecLayer)datalist[i]->dataspec;
		dsp = &dataspec->xydata;

		/*
		 * dash patterns
		 */
		if(dsp->dashes != NULL && !dsp->mono_dash){
			dashtable = (int*)dsp->dashes->data;
			len_dashtable = dsp->dashes->len_dimensions[0];
		}
		else
			len_dashtable = 0;

		/*
		 * marker mode
		 */
		if(dsp->marker_modes != NULL && !dsp->mono_marker_mode){
			markermodetable = (NhlMarkLineMode*)
					dsp->marker_modes->data;
			len_markermodetable =
				dsp->marker_modes->len_dimensions[0];
		}
		else
			len_markermodetable= 0;

		/*
		 * Lg label strings
		 */
		if(dsp->lg_label_strings != NULL){
			lglabeltable = (NhlString*)dsp->lg_label_strings->data;
			len_lglabeltable =
				dsp->lg_label_strings->len_dimensions[0];
		}
		else
			len_lglabeltable = 0;

		/*
		 * line colors
		 */
		if(dsp->colors != NULL && !dsp->mono_color){
			ctable = (int*)dsp->colors->data;
			len_ctable = dsp->colors->len_dimensions[0];
		}
		else
			len_ctable = 0;

		/*
		 * Line Label Color
		 */
		if(dsp->label_colors != NULL && !dsp->mono_label_color){
			labelcolortable = (NhlColorIndex*)
						dsp->label_colors->data;
			len_labelcolortable =
					dsp->label_colors->len_dimensions[0];
		}
		else
			len_labelcolortable = 0;

		/*
		 * line label strings
		 */
		if(dsp->labels != NULL){
			labeltable = (NhlString*)dsp->labels->data;
			len_labeltable =dsp->labels->len_dimensions[0];
		}
		else
			len_labeltable = 0;

		/*
		 * line thickness
		 */
		if(dsp->line_thicknesses != NULL && !dsp->mono_line_thickness){
			linethicktable = (float*)dsp->line_thicknesses->data;
			len_linethicktable =
				dsp->line_thicknesses->len_dimensions[0];
		}
		else
			len_linethicktable = 0;

		/*
		 * Marker Color
		 */
		if(dsp->marker_colors != NULL && !dsp->mono_marker_color){
			markercolortable = (NhlColorIndex*)
					dsp->marker_colors->data;
			len_markercolortable =
					dsp->marker_colors->len_dimensions[0];
		}
		else
			len_markercolortable = 0;

		/*
		 * Marker indexes
		 */
		if(dsp->markers != NULL && !dsp->mono_marker){
			markertable = (NhlMarkerIndex*) dsp->markers->data;
			len_markertable = dsp->markers->len_dimensions[0];
		}
		else
			len_markertable = 0;

		/*
		 * Marker Size
		 */
		if(dsp->marker_sizes != NULL && !dsp->mono_marker_size){
			markersizetable = (float*) dsp->marker_sizes->data;
			len_markersizetable =
					dsp->marker_sizes->len_dimensions[0];
		}
		else
			len_markersizetable = 0;

		/*
		 * Marker Thicknesses
		 */
		if(dsp->marker_thicknesses != NULL &&
						!dsp->mono_marker_thickness){
			markerthicktable =(float*)dsp->marker_thicknesses->data;
			len_markerthicktable =
				dsp->marker_thicknesses->len_dimensions[0];
		}
		else
			len_markerthicktable = 0;

		for(j=0;j < num_curves && index < xlp->num_cpairs;j++,index++){

			if(!impy && !impx){
				xvectors[index] = xvalues[j];
				yvectors[index] = yvalues[j];
				len_vectors[index] =
					MIN(len_yvalues[j],len_xvalues[j]);
			}
			else if(!impx){
				xvectors[index] = xvalues[j];
				yvectors[index] = NULL;
				len_vectors[index] = len_xvalues[j];
			}
			else if(!impy){
				yvectors[index] = yvalues[j];
				xvectors[index] = NULL;
				len_vectors[index] = len_yvalues[j];
			}
			else{
				/*
				 * If this happens, there is a bug in the
				 * Data object.
				 */
				xvectors[index] = NULL;
				yvectors[index] = NULL;
				len_vectors[index] = 0;
			}

			if(j < len_dashtable)
				dash_indexes[index] = dashtable[j];
			else
				dash_indexes[index] = dsp->dash;

			if(j < len_markermodetable) 
				item_types[index] = markermodetable[j];
			else 
				item_types[index] = dsp->marker_mode;

			if(j < len_ctable)
				line_colors[index] = ctable[j];
			else
				line_colors[index] = dsp->color;

			dash_seg_lens[index] = dsp->dash_seg_len;

			if(j < len_labelcolortable)
				llabel_colors[index] = labelcolortable[j];
			else
				llabel_colors[index] = dsp->label_color;
	
			/*
			 * clear buffer
			 */
			tint = strlen(buffer);
			if(tint > 0)
				memset(buffer,'\0',sizeof(char)*(tint-1));
			switch(dsp->label_mode) {
				case NhlNOLABELS:
					label = NULL;
				break;
				case NhlCUSTOM:	
					if(j < len_labeltable)
						label = labeltable[j];
					else{
						sprintf(buffer,"%s%d",
							dataspec->base.name,
									j+1);
						label = buffer;
					}
				break;
				case NhlLETTERED:
					buffer[0] =
					(char)((int)'A' + current_letter % 26);
					current_letter++;
					label = buffer;
				break;
			}

			if(label){
				llabel_strings[index] = NhlMalloc(sizeof(char)*
							(strlen(label)+1));
				if(!llabel_strings[index]){
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return NhlFATAL;
				}
				strcpy(llabel_strings[index],label);
			}
			else
				llabel_strings[index] = NULL;

			/*
			 * lg Labels
			 */
			tint = strlen(buffer);
			if(tint > 0)
				memset(buffer,'\0',sizeof(char)*(tint-1));
			label = NULL;
			if(j < len_lglabeltable)
				label = lglabeltable[j];

			if(!label){
				if(llabel_strings[index])
					label = llabel_strings[index];
				else{
					sprintf(buffer,"%s%d",
							dataspec->base.name,
									j+1);
					label = buffer;
				}
			}

			lg_label_strings[index] = NhlMalloc(sizeof(char)*
							(strlen(label)+1));
			if(!lg_label_strings[index]){
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
			strcpy(lg_label_strings[index],label);

			llabel_fheights[index] = dsp->llabel_fheight;

			if(j < len_linethicktable)
				line_thicknesses[index] = linethicktable[j];
			else
				line_thicknesses[index] = dsp->line_thickness;

			if(j < len_markercolortable)
				marker_colors[index] = markercolortable[j];
			else
				marker_colors[index] = dsp->marker_color;
	
			if(j < len_markertable)
				marker_indexes[index] = markertable[j];
			else
				marker_indexes[index] = dsp->marker;

			if(j < len_markersizetable)
				marker_sizes[index] = markersizetable[j];
			else 
				marker_sizes[index] = dsp->marker_size ;

			if(j < len_markerthicktable)
				marker_thicknesses[index] = markerthicktable[j];
			else
				marker_thicknesses[index] =
							dsp->marker_thickness;

			if(datal->flt.missing_x_set){
				missing_set[index] |= XMISS_SET;
				xmissing[index] = datal->flt.missing_x;
			}
			if(datal->flt.missing_y_set){
				missing_set[index] |= YMISS_SET;
				ymissing[index] = datal->flt.missing_y;
			}

		}
	}

	return ret;
}

/*
 * Function:	DrawCurves
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
DrawCurves
#if	NhlNeedProto
(
	NhlXyPlotLayer	xlayer
)
#else
(xlayer)
	NhlXyPlotLayer	xlayer;
#endif
{
	char			func[] = "DrawCurves";
	int			i,j;
	NhlErrorTypes		ret = NhlNOERROR;
	NhlErrorTypes		ret1 = NhlNOERROR;
	int			upflag = 1;
	NhlTransformLayerPart	*tfp = &xlayer->trans;
	NhlLayer		thetrans = NULL;
	NhlXyPlotLayerPart	*xlp = &xlayer->xyplot;
	int			*dash_indexes = xlp->dash_indexes->data;
	int			*item_types = xlp->item_types->data;
	NhlString		*lg_label_strings = xlp->lg_label_strings->data;
	float			*dash_seg_lens = xlp->dash_seg_lens->data;
	int			*line_colors = xlp->line_colors->data;
	int			*llabel_colors = xlp->llabel_colors->data;
	NhlString		*llabel_strings = xlp->llabel_strings->data;
	float			*llabel_fheights = xlp->llabel_fheights->data;
	float			*line_thicknesses = xlp->line_thicknesses->data;
	int			*marker_colors = xlp->marker_colors->data;
	int			*marker_indexes = xlp->marker_indexes->data;
	float			*marker_sizes = xlp->marker_sizes->data;
	float			*marker_thicknesses =
						xlp->marker_thicknesses->data;
	float			**xvectors = xlp->xvectors->data;
	float			**yvectors = xlp->yvectors->data;
	int			*len_vectors = xlp->len_vectors->data;
	int			*missing_set = xlp->missing_set->data;
	float			*xmissing = xlp->xmissing->data;
	float			*ymissing = xlp->ymissing->data;
	float			*tx,*ty;
	int			size;

	/*
	 * If there is no data, then don't do anything.
	 */
	if(!xlp->num_cpairs) return NhlNOERROR;
	size = *len_vectors;
	for(i=1;i<xlp->num_cpairs;i++){
		size = MAX(len_vectors[i],size);
	}
	if(size <= 0)
		return NhlNOERROR;

	ret = _NhlActivateWorkstation((NhlLayer)xlayer->base.wkptr);	
	if(ret < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to activate workstation",func);
		return(NhlFATAL);
	}
	ret1 = MIN(ret,ret1);

	if(tfp->overlay_status == _tfCurrentOverlayMember &&
		tfp->overlay_trans_obj != NULL){
		thetrans = tfp->overlay_trans_obj;
	}
	else{
		thetrans = xlp->thetrans;
		ret = _NhlSetTrans(thetrans,(NhlLayer)xlayer);
		if(ret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to set transformation",func);
			return(NhlFATAL);
		}
		ret1 = MIN(ret,ret1);
	}

	/*
	 * Get some memory to transform points into.
	 */
	tx = NhlMalloc(sizeof(float)*size);
	ty = NhlMalloc(sizeof(float)*size);
	if(!tx || !ty){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}

	NhlVASetValues(xlayer->base.wkptr->base.id,
		_NhlNwkReset,	True,
		NULL);


	for(i=0;i < xlp->num_cpairs;i++){
		float		xtmp,ytmp;
		int		status;
		float 		out_of_range;
		float		*xvect;
		float		*yvect;

		NhlVASetValues(xlayer->base.wkptr->base.id,
			_NhlNwkLineDashSegLenF,
					dash_seg_lens[i]*xlp->vp_average,
			_NhlNwkLineLabelFontHeightF,
					llabel_fheights[i]*xlp->vp_average,
			_NhlNwkDashPattern,	dash_indexes[i],
			_NhlNwkLineColor,	line_colors[i],
			_NhlNwkLineLabelFontColor,	llabel_colors[i],
			_NhlNwkLineLabel,	llabel_strings[i],
			_NhlNwkLineThicknessF,	line_thicknesses[i],
			_NhlNwkMarkerColor,	marker_colors[i],
			_NhlNwkMarkerIndex,	marker_indexes[i],
			_NhlNwkMarkerSizeF,	marker_sizes[i]*xlp->vp_average,
			_NhlNwkMarkerThicknessF,	marker_thicknesses[i],
			NULL);

		if(xvectors[i])
			xvect = xvectors[i];
		else{
			for(j=0;j<len_vectors[i];j++)
				tx[j] = (float)j+1;
			xvect = tx;
		}
		if(yvectors[i])
			yvect = yvectors[i];
		else{
			for(j=0;j<len_vectors[i];j++)
				ty[j] = (float)j+1;
			yvect = ty;
		}


		if(item_types[i] != NhlMARKERS){
			_NhlSetLineInfo(xlayer->base.wkptr,(NhlLayer)xlayer);

			upflag = 1;

			if(missing_set[i] & XMISS_SET & YMISS_SET){
				int 	status;

				for(j=0;j < len_vectors[i];j++){
					if((xvect[j] == xmissing[i]) ||
						(yvect[j] == ymissing[i]))
						upflag = 1;
					else{
						_NhlDataLineTo(thetrans,
							xvect[j],
							yvect[j],
							upflag);
						upflag = 0;
					}
				}
			}
			else if(missing_set[i] & XMISS_SET){
				for(j=0;j < len_vectors[i];j++){
					if(xvect[j] == xmissing[i])
						upflag = 1;
					else{
						_NhlDataLineTo(thetrans,
								xvect[j],
								yvect[j],
								upflag);
						upflag = 0;
					}
				}
			}
			else if(missing_set[i] & YMISS_SET){
				for(j=0;j < len_vectors[i];j++){
					if(yvect[j] == ymissing[i])
						upflag = 1;
					else{
						_NhlDataLineTo(thetrans,
								xvect[j],
								yvect[j],
								upflag);
						upflag = 0;
					}
				}
			}
			else{
				for(j=0;j < len_vectors[i];j++){
					_NhlDataLineTo(thetrans,
						xvect[j],yvect[j],upflag);
					upflag = 0;
				}
			}
			/*
			 * This is called here to flush the "lineto" buffer.
			 */
			_NhlWorkstationLineTo(xlayer->base.wkptr,1.0,1.0,1);
		}

		if(item_types[i] != NhlLINES){
			float	oor;
			float	*xmiss = NULL;
			float	*ymiss = NULL;

			if(missing_set[i] & XMISS_SET)
				xmiss = &xmissing[i];
			if(missing_set[i] & YMISS_SET)
				ymiss = &ymissing[i];

			NhlVAGetValues(thetrans->base.id,
				NhlNtrOutOfRangeF,	&oor,
				NULL);

			ret = _NhlDataToWin(thetrans,xvect,yvect,len_vectors[i],
					tx,ty,&status,xmiss,ymiss);
			if(ret < NhlWARNING){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:Unable to transform Marker Points",
					func);
				ret1 = MIN(ret1,NhlWARNING);
				/*
				 * Skip this set of markers.
				 */
				continue;
			}
			ret = _NhlWinToNDC(thetrans,tx,ty,len_vectors[i],tx,ty,
						&status,&oor,&oor);
			if(ret < NhlWARNING){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:Unable to transform Marker Points",
					func);
				ret1 = MIN(ret1,NhlWARNING);
				/*
				 * Skip this set of markers.
				 */
				continue;
			}

			_NhlSetMarkerInfo(xlayer->base.wkptr,(NhlLayer)xlayer);

			if(status){
				for(j=0;j<len_vectors[i];j++){
					if((tx[j] == oor)||(ty[j] == oor))
						continue;
					_NhlWorkstationMarker(
						xlayer->base.wkptr,
						&tx[j],&ty[j],1);
				}
			}
			else{
				_NhlWorkstationMarker(xlayer->base.wkptr,
					tx,ty,len_vectors[i]);
			}
		}
	}


	ret = _NhlDeactivateWorkstation(xlayer->base.wkptr);	
	ret1 = MIN(ret,ret1);

	NhlFree(tx);
	NhlFree(ty);

	return ret1;
}

/*
 * Function:	XyPlotDraw
 *
 * Description:	Draw method for the XyPlot object. This function calls 
 *		NhlDraw for the TickMarks and the Titles and then calls 
 *		DrawCurves to set up and call AUTOGRAPH. 
 *
 * In Args:	layer	XyPlot instance
 *
 * Out Args:	NhlNONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NhlNONE
 */	

static NhlErrorTypes XyPlotDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlXyPlotLayer	xlayer = (NhlXyPlotLayer) layer;
	NhlErrorTypes ret1 = NhlNOERROR;
	NhlErrorTypes ret = NhlNOERROR;

	if((!xlayer->xyplot.data_ranges_set) ||
				(xlayer->xyplot.thetrans == NULL)){
		return NhlNOERROR;
	}

	ret = DrawCurves(xlayer);
	if(ret < NhlWARNING)
		return ret;
	ret1 = MIN(ret,ret1);

	return ret1;
}

/*
 * Function:	XyPlotDestroy
 *
 * Description:	Calls NhlDestroy for the TickMarks, Titles and the TransObj
 *		Calls NhlFree for the fields:
 *		*_values
 *		*_final_values
 *		curve_line_labels
 *		curve_colors
 *		curve_lengths
 *		curve_dashes
 *		*_irregular_points
 *		dummy_array
 *		*_alternate_coords
 *		*_original_coords
 *
 * In Args:	inst		instance record pointer
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NhlNONE
 */
static NhlErrorTypes XyPlotDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlXyPlotLayer		xinst = (NhlXyPlotLayer)inst;
	NhlXyPlotLayerPart	*xp = &xinst->xyplot;
	NhlTransformLayerPart	*tfp = &xinst->trans;
	NhlErrorTypes		ret = NhlNOERROR;
	NhlErrorTypes		ret1 = NhlNOERROR;

	if(tfp->overlay_status == _tfCurrentOverlayMember)
		ret =
		NhlRemoveOverlay(tfp->overlay_object->base.parent->base.id,
							inst->base.id,False);

	if(xp->overlay != NULL){
		ret1 = _NhlDestroyChild(xp->overlay->base.id,inst);
		xp->overlay = NULL;
	}
	ret = MIN(ret,ret1);

	if(xp->thetrans != NULL){
		ret1 = NhlDestroy(xp->thetrans->base.id);
		xinst->trans.trans_obj = NULL;
		xp->thetrans = NULL;
	}
	ret = MIN(ret,ret1);

	NhlFreeGenArray(xp->x_irregular_points);
	NhlFreeGenArray(xp->y_irregular_points);

/*
 * NOT implimented yet...
 */
#ifdef	NOT
	NhlFreeGenArray(xp->x_original_coords);
	NhlFreeGenArray(xp->x_alternate_coords);
	NhlFreeGenArray(xp->y_alternate_coords);
	NhlFreeGenArray(xp->y_original_coords);
#endif

	NhlFreeGenArray(xp->dash_indexes);
	NhlFreeGenArray(xp->item_types);
	NhlFreeGenArray(xp->lg_label_strings);
	NhlFreeGenArray(xp->line_colors);
	NhlFreeGenArray(xp->dash_seg_lens);
	NhlFreeGenArray(xp->llabel_colors);
	NhlFreeGenArray(xp->llabel_strings);
	NhlFreeGenArray(xp->llabel_fheights);
	NhlFreeGenArray(xp->line_thicknesses);
	NhlFreeGenArray(xp->marker_colors);
	NhlFreeGenArray(xp->marker_indexes);
	NhlFreeGenArray(xp->marker_sizes);
	NhlFreeGenArray(xp->marker_thicknesses);
	NhlFreeGenArray(xp->xvectors);
	NhlFreeGenArray(xp->yvectors);
	NhlFreeGenArray(xp->len_vectors);
	NhlFreeGenArray(xp->missing_set);
	NhlFreeGenArray(xp->xmissing);
	NhlFreeGenArray(xp->ymissing);

	return(ret1);
}

/*
 * Function:	XyPlotGetBB
 *
 * Description: Calls NhlGetBB on the TickMark object and the Title object
 *
 * In Args:	instance 	object instance record
 *		thebox		data structure, provided by the user to
 *				hold boudning box information.
 *
 * Out Args:	NhlNONE
 *
 * Return Values:  Error Conditions
 *
 * Side Effects:   NhlNONE.
 */
static NhlErrorTypes XyPlotGetBB
#if	NhlNeedProto
(NhlLayer instance, NhlBoundingBox* thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	NhlXyPlotLayer		xinst = (NhlXyPlotLayer)instance;
	NhlXyPlotLayerPart	*xp = &xinst->xyplot;
	NhlViewLayerPart	*vp = &xinst->view;
	NhlTransformLayerPart	*tfp = &xinst->trans;
	NhlErrorTypes		ret = NhlNOERROR;

	if(tfp->overlay_status == _tfCurrentOverlayBase)
		return _NhlGetBB(tfp->overlay_object,thebox);

	_NhlAddBBInfo(vp->y,(vp->y-vp->height),(vp->x+vp->width),vp->x,thebox);

	return ret;
}

/*
 * Function:	XyPlotUpdateData
 *
 * Description:	This function is called whenever the data pointed to by the
 *		data resources change.  This function needs to check if
 *		this specific data resource changed - it may have been another
 *		data resource in a sub/super class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
XyPlotUpdateData
#if	NhlNeedProto
(
	NhlDataCommLayer	new,
	NhlDataCommLayer	old
)
#else
(new,old)
	NhlDataCommLayer	new;
	NhlDataCommLayer	old;
#endif
{
	NhlXyPlotLayer		xl = (NhlXyPlotLayer)new;
	NhlXyPlotLayer		xlold = (NhlXyPlotLayer)old;
	NhlErrorTypes		ret1=NhlNOERROR,ret2=NhlNOERROR;
	NhlSArg			sargs[1];
	int			nsargs=0;

	ret2 = ComputeDataExtents(xl,xlold,_NhlUPDATEDATA);
	if(ret2 < NhlWARNING)
		return ret2;
	ret1 = MIN(ret1,ret2);

	ret2 = SetUpTransObjs(xl,xlold,_NhlUPDATEDATA);
	if(ret2 < NhlWARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);

	ret2 = SetUpDataSpec(xl,xlold,_NhlUPDATEDATA);
	if(ret2 < NhlWARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);

	if(xl->xyplot.check_ranges){
		NhlSetSArg(&sargs[nsargs++],NhlNpmUpdateReq,True);
		xl->xyplot.check_ranges = False;
	}


	ret2 = _NhlManageOverlay(&xl->xyplot.overlay,(NhlLayer)xl,
		(NhlLayer)xlold,_NhlUPDATEDATA,sargs,nsargs,"XyPlotUpdateData");
	if(ret2 < NhlWARNING)
		return ret2;
	ret1 = MIN(ret1,ret2);

	return ret1;
}

/*
 * Function:	CheckExtent
 *
 * Description:	This function is used by CheckValues to check the extent
 *		resources.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
CheckExtent
#if	NhlNeedProto
(
	NhlBoolean	extent_change,
	NhlBoolean	compute_change,
	NhlBoolean	*compute_value,
	NhlString	comp_res,
	NhlString	extent_res,
	NhlString	error_lead
)
#else
(extent_change,compute_change,compute_value,comp_res,extent_res,error_lead)
	NhlBoolean	extent_change;
	NhlBoolean	compute_change;
	NhlBoolean	*compute_value;
	NhlString	comp_res;
	NhlString	extent_res;
	NhlString	error_lead;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;

	if(extent_change && *compute_value){
		*compute_value = False;

		if(compute_change){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Setting %s to False because %s was specified",
						error_lead,comp_res,extent_res);
			ret = NhlWARNING;
		}
	}

	return ret;
}


/*
 * Function:	CheckValues
 *
 * Description:	This function makes sure the following resources agree.
 *			style
 *			alt_coords
 *			compute_extents
 *
 * In Args:
 *		NhlXyPlotLayer	xnew,
 *		NhlXyPlotLayer	xold,
 *		_NhlCalledFrom	calledfrom
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
static NhlErrorTypes
CheckValues
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
)
#else
(xnew,xold,calledfrom)
	NhlXyPlotLayer	xnew;
	NhlXyPlotLayer	xold;
	_NhlCalledFrom	calledfrom;
#endif
{
	char*		error_lead;
	NhlErrorTypes	ret = NhlNOERROR, lret = NhlNOERROR;

	if(calledfrom == _NhlCREATE)
		error_lead = "XyPlotInitialize";
	else
		error_lead = "XyPlotSetValues";

	/*
	 * take care of style resources
	 */
	if((xnew->xyplot.x_style == NhlIRREGULAR) &&
				(xnew->xyplot.x_irregular_points == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: cannot be NhlIRREGULAR unless %s is set:setting %s to NhlLINEAR",
			NhlNxyXStyle,NhlNxyXIrregularPoints,NhlNxyXStyle);

		xnew->xyplot.x_style = NhlLINEAR;
		ret = MIN(ret,NhlWARNING);
	}
	if((xnew->xyplot.y_style == NhlIRREGULAR) &&
				(xnew->xyplot.y_irregular_points == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: cannot be NhlIRREGULAR unless %s is set:setting %s to NhlLINEAR",
			NhlNxyYStyle,NhlNxyYIrregularPoints,NhlNxyYStyle);

		xnew->xyplot.y_style = NhlLINEAR;
		ret = MIN(ret,NhlWARNING);
	}

/*
 * NOT implimented yet...
 */
#ifdef	NOT
	/*
	 * Alternate Coord's are not yet implimented - so make sure x_alternate
	 * and y_alternate are set to NhlNONE.
	 * (Eventually this part should check and make sure the coord arrays
	 * exist if x_alternate and y_alternate are not = to NhlNONE)
	 */
	if(xnew->xyplot.x_alternate != NhlNONE){
		xnew->xyplot.x_alternate = NhlNONE;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s only supports a value of NhlNONE at this time",
						error_lead,NhlNxyXAlternate);
		ret = MIN(ret,NhlWARNING);
	}
	if(xnew->xyplot.y_alternate != NhlNONE){
		xnew->xyplot.y_alternate = NhlNONE;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s only supports a value of NhlNONE at this time",
						error_lead,NhlNxyYAlternate);
		ret = MIN(ret,NhlWARNING);
	}
#endif

	/*
	 * Check Extents - left right top bottom
	 */

	if(calledfrom == _NhlCREATE){
		lret = CheckExtent(xnew->xyplot.x_min_set,
			xnew->xyplot.comp_x_min_set,&xnew->xyplot.compute_x_min,
			NhlNxyComputeXMin,NhlNtrXMinF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent(xnew->xyplot.x_max_set,
			xnew->xyplot.comp_x_max_set,&xnew->xyplot.compute_x_max,
			NhlNxyComputeXMax,NhlNtrXMaxF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent(xnew->xyplot.y_max_set,
			xnew->xyplot.comp_y_max_set,&xnew->xyplot.compute_y_max,
			NhlNxyComputeYMax,NhlNtrYMaxF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent(xnew->xyplot.y_min_set,
			xnew->xyplot.comp_y_min_set,&xnew->xyplot.compute_y_min,
			NhlNxyComputeYMin,NhlNtrYMinF,error_lead);
		ret = MIN(lret,ret);
	}
	else{
		lret = CheckExtent((xold->xyplot.x_min!=xnew->xyplot.x_min),
			xnew->xyplot.compute_x_min,&xnew->xyplot.compute_x_min,
			NhlNxyComputeXMin,NhlNtrXMinF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent((xold->xyplot.x_max!=xnew->xyplot.x_max),
			xnew->xyplot.compute_x_max,&xnew->xyplot.compute_x_max,
			NhlNxyComputeXMax,NhlNtrXMaxF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent((xold->xyplot.y_max!=xnew->xyplot.y_max),
			xnew->xyplot.compute_y_max,&xnew->xyplot.compute_y_max,
			NhlNxyComputeYMax,NhlNtrYMaxF,error_lead);
		ret = MIN(lret,ret);

		lret=CheckExtent((xold->xyplot.y_min!=xnew->xyplot.y_min),
			xnew->xyplot.compute_y_min,&xnew->xyplot.compute_y_min,
			NhlNxyComputeYMin,NhlNtrYMinF,error_lead);
		ret = MIN(lret,ret);

	}

	if(!xnew->xyplot.compute_x_min && xnew->xyplot.x_min_set &&
		(xnew->xyplot.x_style == NhlLOG) && (xnew->xyplot.x_min <= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is NhlLOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNxyXStyle,NhlNtrXMinF,NhlNxyComputeXMin);

		xnew->xyplot.compute_x_min = True;
		ret = MIN(ret,NhlWARNING);
	}

	if(!xnew->xyplot.compute_x_max && xnew->xyplot.x_max_set &&
		(xnew->xyplot.x_style == NhlLOG) && (xnew->xyplot.x_max <= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is NhlLOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNxyXStyle,NhlNtrXMaxF,NhlNxyComputeXMax);

		xnew->xyplot.compute_x_max = True;
		ret = MIN(ret,NhlWARNING);
	}

	if(!xnew->xyplot.compute_x_min && xnew->xyplot.x_min_set &&
		!xnew->xyplot.compute_x_max && xnew->xyplot.x_max_set &&
		(xnew->xyplot.x_max < xnew->xyplot.x_min)){

		float tfloat;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:%s is < %s: Swapping",
					error_lead,NhlNtrXMaxF,NhlNtrXMinF);
		tfloat = xnew->xyplot.x_max;
		xnew->xyplot.x_max = xnew->xyplot.x_min;
		xnew->xyplot.x_min = tfloat;
	}

	if(!xnew->xyplot.compute_y_min && xnew->xyplot.y_min_set &&
		(xnew->xyplot.y_style == NhlLOG) && (xnew->xyplot.y_min <= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is NhlLOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNxyYStyle,NhlNtrYMinF,NhlNxyComputeYMin);

		xnew->xyplot.compute_y_min = True;
		ret = MIN(ret,NhlWARNING);
	}

	if(!xnew->xyplot.compute_y_max && xnew->xyplot.y_max_set &&
		(xnew->xyplot.y_style == NhlLOG) && (xnew->xyplot.y_max <= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is NhlLOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNxyYStyle,NhlNtrYMaxF,NhlNxyComputeYMax);

		xnew->xyplot.compute_y_max = True;
		ret = MIN(ret,NhlWARNING);
	}

	if(!xnew->xyplot.compute_y_min && xnew->xyplot.y_min_set &&
		!xnew->xyplot.compute_y_max && xnew->xyplot.y_max_set &&
		(xnew->xyplot.y_max < xnew->xyplot.y_min)){

		float tfloat;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:%s is < %s: Swapping",
					error_lead,NhlNtrYMaxF,NhlNtrYMinF);
		tfloat = xnew->xyplot.y_max;
		xnew->xyplot.y_max = xnew->xyplot.y_min;
		xnew->xyplot.y_min = tfloat;
	}

	if((calledfrom == _NhlSETVALUES) &&
		((xold->xyplot.x_style != xnew->xyplot.x_style) ||
		(xold->xyplot.y_style != xnew->xyplot.y_style))){

		xnew->xyplot.check_ranges = True;
	}

	return ret;
}

/*
 * Function:	InternalizePointers
 *
 * Description:	This function checks the irregular_points resources and the
 *		style resources to make sure they agree.
 *		This function allocates memory for the alternate_coords stuff.
 *		This function allocates memory for the title strings.
 *
 * In Args:
 *		NhlXyPlotLayer	xnew,
 *		NhlXyPlotLayer	xold,
 *		_NhlCalledFrom	calledfrom
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
static NhlErrorTypes
InternalizePointers
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
)
#else
(xnew,xold,calledfrom)
	NhlXyPlotLayer	xnew;
	NhlXyPlotLayer	xold;
	_NhlCalledFrom	calledfrom;
#endif
{
	char		*error_lead;
	NhlGenArray	gen;
	NhlBoolean	skip_x_irreg_pts = False, skip_y_irreg_pts = False;
	NhlBoolean	free_x_irreg = False, free_y_irreg = False;
	NhlBoolean	skip_x_alt_coord = False, skip_y_alt_coord = False;
	NhlBoolean	free_x_alt_coord = False, free_y_alt_coord = False;
	NhlBoolean	skip_x_orig_coord = False, skip_y_orig_coord = False;
	NhlBoolean	free_x_orig_coord = False, free_y_orig_coord = False;
	NhlErrorTypes	ret = NhlNOERROR;

	if(calledfrom == _NhlSETVALUES) {
		error_lead = "XyPlotSetValues";
	} else {
		error_lead = "XyPlotInitialize";
	}

	/*
	 * take care of irregular_points
	 */
	if(calledfrom == _NhlSETVALUES){
		if(xold->xyplot.x_irregular_points !=
						xnew->xyplot.x_irregular_points)
			free_x_irreg = True;
		else
			skip_x_irreg_pts = True;
		if(xold->xyplot.y_irregular_points !=
						xnew->xyplot.y_irregular_points)
			free_y_irreg = True;
		else
			skip_y_irreg_pts = True;
	}

	if((xnew->xyplot.x_irregular_points != NULL) && !skip_x_irreg_pts) {
		gen = (NhlGenArray)xnew->xyplot.x_irregular_points;
		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
		(gen->num_dimensions != 1) || (gen->len_dimensions[0] < 3)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:%s must be a 1 dim float array with a min of 3 elements: resetting",
					error_lead,NhlNxyXIrregularPoints);

			if(calledfrom == _NhlSETVALUES)
				xnew->xyplot.x_irregular_points =
						xold->xyplot.x_irregular_points;
			else
				xnew->xyplot.x_irregular_points = NULL;

			free_x_irreg = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			float	*tarr;

			xnew->xyplot.x_irregular_points =
						_NhlCopyGenArray(gen,True);
			if(xnew->xyplot.x_irregular_points == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
			tarr = (float*)gen->data;
			xnew->xyplot.x_irreg_min =
				MIN(tarr[0],tarr[gen->len_dimensions[0]-1]);
			xnew->xyplot.x_irreg_max =
				MAX(tarr[0],tarr[gen->len_dimensions[0]-1]);
			xnew->xyplot.check_ranges = True;
		}
	}
	if(free_x_irreg)
		NhlFreeGenArray(xold->xyplot.x_irregular_points);

	if((xnew->xyplot.y_irregular_points != NULL) && !skip_y_irreg_pts){
		gen = (NhlGenArray)xnew->xyplot.y_irregular_points;
		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
		(gen->num_dimensions != 1) || (gen->len_dimensions[0] < 3)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:%s must be a 1 dim float array with a min of 3 elements: ignoring",
					error_lead,NhlNxyYIrregularPoints);

			if(calledfrom == _NhlSETVALUES)
				xnew->xyplot.y_irregular_points =
						xold->xyplot.y_irregular_points;
			else
				xnew->xyplot.y_irregular_points = NULL;

			free_y_irreg = False;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			float	*tarr;

			xnew->xyplot.y_irregular_points =
						_NhlCopyGenArray(gen,True);
			if(xnew->xyplot.y_irregular_points == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
			tarr = (float*)gen->data;
			xnew->xyplot.y_irreg_min =
				MIN(tarr[0],tarr[gen->len_dimensions[0]-1]);
			xnew->xyplot.y_irreg_max =
				MAX(tarr[0],tarr[gen->len_dimensions[0]-1]);
			xnew->xyplot.check_ranges = True;
		}
	}
	if(free_y_irreg)
		NhlFreeGenArray(xold->xyplot.y_irregular_points);

/*
 * These resources have not been implimented yet...
 */
#ifdef	NOT
	/*
	 * take care of alt coords and orig coords
	 */
	if(calledfrom == _NhlSETVALUES){
		if(xold->xyplot.x_alternate_coords !=
						xnew->xyplot.x_alternate_coords)
			free_x_alt_coord = True;
		else
			skip_x_alt_coord = True;

		if(xold->xyplot.y_alternate_coords !=
						xnew->xyplot.y_alternate_coords)
			free_y_alt_coord = True;
		else
			skip_y_alt_coord = True;

		if(xold->xyplot.x_original_coords !=
						xnew->xyplot.x_original_coords)
			free_x_orig_coord = True;
		else
			skip_x_orig_coord = True;

		if(xold->xyplot.y_original_coords !=
						xnew->xyplot.y_original_coords)
			free_y_orig_coord = True;
		else
			skip_y_orig_coord = True;
	}

	if((xnew->xyplot.x_alternate_coords != NULL) && !skip_x_alt_coord){
		gen = (NhlGenArray)xnew->xyplot.x_alternate_coords;
		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
						(gen->num_dimensions != 1)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s must be set with a 1-dim float array: ignoring",
					error_lead,NhlNxyXAlternateCoords);

			if(calledfrom == _NhlSETVALUES)
				xnew->xyplot.x_alternate_coords =
						xold->xyplot.x_alternate_coords;
			else
				xnew->xyplot.x_alternate_coords = NULL;
			free_x_alt_coord = False;

			ret = MIN(ret,NhlWARNING);
		}
		else{
			xnew->xyplot.x_alternate_coords =
						_NhlCopyGenArray(gen,True);
			if(xnew->xyplot.x_alternate_coords == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
	}
	if(free_x_alt_coord)
		NhlFreeGenArray(xold->xyplot.x_alternate_coords);

	if((xnew->xyplot.y_alternate_coords != NULL) && !skip_y_alt_coord){
		gen = (NhlGenArray)xnew->xyplot.y_alternate_coords;
		if((gen->typeQ == Qfloat) && (gen->size == sizeof(float)) &&
						(gen->num_dimensions == 1)){
			xnew->xyplot.y_alternate_coords =
						_NhlCopyGenArray(gen,True);
			if(xnew->xyplot.y_alternate_coords == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
		else{
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s must be set with a 1-dim float array: ignoring",
					error_lead,NhlNxyYAlternateCoords);

			if(calledfrom == _NhlSETVALUES)
				xnew->xyplot.y_alternate_coords =
						xold->xyplot.y_alternate_coords;
			else
				xnew->xyplot.y_alternate_coords = NULL;
			free_y_alt_coord = False;

			ret = MIN(ret,NhlWARNING);
		}
	}
	if(free_y_alt_coord)
		NhlFreeGenArray(xold->xyplot.y_alternate_coords);

	if((xnew->xyplot.x_original_coords != NULL) && !skip_x_orig_coord){
		gen = (NhlGenArray)xnew->xyplot.x_original_coords;
		if((gen->typeQ == Qfloat) && (gen->size == sizeof(float)) &&
						(gen->num_dimensions == 1)){
			xnew->xyplot.x_original_coords =
						_NhlCopyGenArray(gen,True);
			if(xnew->xyplot.x_original_coords == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
		else{
			NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:%s must be set with a generic 1-dim float array: ignoring",
					error_lead,NhlNxyXOriginalCoords);

			if(calledfrom == _NhlSETVALUES)
				xnew->xyplot.x_original_coords =
						xold->xyplot.x_original_coords;
			else
				xnew->xyplot.x_original_coords = NULL;
			free_x_orig_coord = False;

			ret = MIN(ret,NhlWARNING);
		}
	}
	if(free_x_orig_coord)
		NhlFreeGenArray(xold->xyplot.x_original_coords);

	if((xnew->xyplot.y_original_coords != NULL) && !skip_y_orig_coord){
		gen = (NhlGenArray)xnew->xyplot.y_original_coords;
		if((gen->typeQ == Qfloat) && (gen->size == sizeof(float)) &&
						(gen->num_dimensions == 1)){
			xnew->xyplot.y_original_coords =
						_NhlCopyGenArray(gen,True);
			if(xnew->xyplot.y_original_coords == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
		}
		else{
			NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:%s must be set with a generic float array: ignoring",
					error_lead,NhlNxyYOriginalCoords);

			if(calledfrom == _NhlSETVALUES)
				xnew->xyplot.y_original_coords =
						xold->xyplot.y_original_coords;
			else
				xnew->xyplot.y_original_coords = NULL;
			free_y_orig_coord = False;

			ret = MIN(ret,NhlWARNING);
		}
	}
	if(free_y_orig_coord)
		NhlFreeGenArray(xold->xyplot.y_original_coords);
#endif	/* NOT */

	return ret;
}

/*
 * Function:	ComputeDataExtents
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
 */
static NhlErrorTypes
ComputeDataExtents
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
)
#else 
(xnew,xold,calledfrom)
	NhlXyPlotLayer	xnew;
	NhlXyPlotLayer	xold;
	_NhlCalledFrom	calledfrom;
#endif
{
	_NhlDataNodePtr		*datalist = NULL;
	int			num_data,i;
	int			num_pairs=0,di_num_pairs;
	NhlBoolean		new,impy;
	NhlCoordArrTableFloatLayer	datal = NULL;
	char			*error_lead;
	NhlErrorTypes		ret = NhlNOERROR;

	if(calledfrom == _NhlCREATE){
		error_lead = "XyPlotInitialize";
		if(xnew->xyplot.curve_data == NULL){
			xnew->xyplot.data_ranges_set = False;
			return NhlNOERROR;
		}
	}
	else if(calledfrom == _NhlSETVALUES){
		error_lead = "XyPlotSetValues";
	}
	else if(calledfrom == _NhlUPDATEDATA){
		error_lead = "XyPlotUpdateData";
	}
	else{
		NhlPError(NhlFATAL,NhlEUNKNOWN,"BadCall");
		return NhlFATAL;
	}

	if((calledfrom == _NhlCREATE) || (calledfrom == _NhlUPDATEDATA) ||
			(xnew->xyplot.curve_data != xold->xyplot.curve_data)){

		xnew->xyplot.data_ranges_set = True;
		xnew->xyplot.check_ranges = True;

		num_data = _NhlGetDataInfo(xnew->xyplot.curve_data,&datalist);
		if(num_data <= 0){
			xnew->xyplot.data_ranges_set = False;
			return NhlNOERROR;
		}

		/*
		 * Data Conversion Happens Here if anywhere.
		 */
		datal=(NhlCoordArrTableFloatLayer)_NhlGetDataSet(datalist[0],&new);
		if(datal == NULL){
			xnew->xyplot.data_ranges_set = False;
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Data Problem???");
			return NhlFATAL;
		}

		xnew->xyplot.x_data_min = datal->flt.min_x;
		xnew->xyplot.x_data_max = datal->flt.max_x;
		xnew->xyplot.y_data_min = datal->flt.min_y;
		xnew->xyplot.y_data_max = datal->flt.max_y;

		if(datal->flt.ytable != NULL){
			num_pairs = datal->flt.ytable->len_dimensions[0];
			impy = False;
		}
		else
			impy = True;

		if(datal->flt.xtable != NULL){
			if(impy)
				num_pairs =
					datal->flt.xtable->len_dimensions[0];
			else
				num_pairs = MIN(num_pairs,
					datal->flt.xtable->len_dimensions[0]);
		}


		for(i=1;i < num_data;i++){
			datal=(NhlCoordArrTableFloatLayer)
					_NhlGetDataSet(datalist[i],&new);
			if(datal == NULL){
				xnew->xyplot.data_ranges_set = False;
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Data Problem???");
				return NhlFATAL;
			}
			xnew->xyplot.x_data_min =
			MIN(xnew->xyplot.x_data_min,datal->flt.min_x);
			xnew->xyplot.x_data_max =
			MAX(xnew->xyplot.x_data_max,datal->flt.max_x);
			xnew->xyplot.y_data_min =
			MIN(xnew->xyplot.y_data_min,datal->flt.min_y);
			xnew->xyplot.y_data_max =
			MAX(xnew->xyplot.y_data_max,datal->flt.max_y);


			if(datal->flt.ytable != NULL){
				di_num_pairs =
					datal->flt.ytable->len_dimensions[0];
				impy = False;
			}
			else
				impy = True;

			if(datal->flt.xtable != NULL){
				if(impy)
					di_num_pairs =
					datal->flt.xtable->len_dimensions[0];
				else
					di_num_pairs = MIN(di_num_pairs,
					datal->flt.xtable->len_dimensions[0]);
			}

			num_pairs += di_num_pairs;
		}
		xnew->xyplot.num_cpairs = num_pairs;
	}

	if(xnew->xyplot.check_ranges){

		/*
		 * Set Initial default for left,right,top,bottom
		 * (should only happen if user didn't set it themself, and it
		 * will only be used if the compute resources are False.)
		 *
		 * Also set if compute resources are true.
		 */
		if(!xnew->xyplot.x_min_set || xnew->xyplot.compute_x_min){
			xnew->xyplot.x_min = xnew->xyplot.x_data_min;
			xnew->xyplot.x_min_set = True;
		}
		if(!xnew->xyplot.x_max_set || xnew->xyplot.compute_x_max){
			xnew->xyplot.x_max = xnew->xyplot.x_data_max;
			xnew->xyplot.x_max_set = True;
		}
		if(!xnew->xyplot.y_min_set || xnew->xyplot.compute_y_min){
			xnew->xyplot.y_min = xnew->xyplot.y_data_min;
			xnew->xyplot.y_min_set = True;
		}
		if(!xnew->xyplot.y_max_set || xnew->xyplot.compute_y_max){
			xnew->xyplot.y_max = xnew->xyplot.y_data_max;
			xnew->xyplot.y_max_set = True;
		}

		/*
		 * Make sure data extents will work in a NhlLOG trans, if NhlLOG
		 * is specified.
		 */
		if((xnew->xyplot.x_data_min <= 0.0) &&
					(xnew->xyplot.x_style == NhlLOG)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s:The Minimum X value is <= 0.0 NhlLOG invalid:Changing %s to NhlLINEAR",
						error_lead,NhlNxyXStyle);
			ret = MIN(ret,NhlWARNING);
			xnew->xyplot.x_style = NhlLINEAR;
		}
		if((xnew->xyplot.y_data_min <= 0.0) &&
					(xnew->xyplot.y_style == NhlLOG)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s:The Minimum Y value is <= 0.0 NhlLOG invalid:Changing %s to NhlLINEAR",
						error_lead,NhlNxyYStyle);
			ret = MIN(ret,NhlWARNING);
			xnew->xyplot.y_style = NhlLINEAR;
		}

		/*
		 * Make sure data extents will work in an NhlIRREGULAR trans,
		 * if NhlIRREGULAR is specifed.
		 */
		if(xnew->xyplot.x_style == NhlIRREGULAR){
			if(xnew->xyplot.x_min < xnew->xyplot.x_irreg_min){

				if(!xnew->xyplot.compute_x_min){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNtrXMinF,
						NhlNxyXIrregularPoints,
						xnew->xyplot.x_irreg_min);
					ret = MIN(ret,NhlWARNING);
				}
				xnew->xyplot.x_min = xnew->xyplot.x_irreg_min;
			}
			if(xnew->xyplot.x_max > xnew->xyplot.x_irreg_max){

				if(!xnew->xyplot.compute_x_max){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNtrXMaxF,
						NhlNxyXIrregularPoints,
						xnew->xyplot.x_irreg_max);
					ret = MIN(ret,NhlWARNING);
				}
				xnew->xyplot.x_max = xnew->xyplot.x_irreg_max;
			}
		}
		if(xnew->xyplot.y_style == NhlIRREGULAR){
			if(xnew->xyplot.y_min < xnew->xyplot.y_irreg_min){

				if(!xnew->xyplot.compute_y_min){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNtrYMinF,
						NhlNxyYIrregularPoints,
						xnew->xyplot.y_irreg_min);
					ret = MIN(ret,NhlWARNING);
				}
				xnew->xyplot.y_min = xnew->xyplot.y_irreg_min;
			}
			if(xnew->xyplot.y_max > xnew->xyplot.y_irreg_max){

				if(!xnew->xyplot.compute_y_max){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNtrYMaxF,
						NhlNxyYIrregularPoints,
						xnew->xyplot.y_irreg_max);
					ret = MIN(ret,NhlWARNING);
				}
				xnew->xyplot.y_max = xnew->xyplot.y_irreg_max;
			}
		}
	}

	return ret;
}


/*
 * Function:	SetUpTransObjs
 *
 * Description: Creates, Sets and Destroys the main tranformation object
 *		for the XyPlot. For log and linear plots the tranformation
 *		object is not destroyed when changes in rsources affecting
 *		the tranformation are changed (i.e. data extents). However,
 *		IrregularType2TransObjs have to be freed whenever the 
 *		data extent increases but not when it decreases.  The 
 *		LogLinTransObjs are only destroyed when the style is switched
 *		from log or linear to irregular.  This function uses two 
 *		switch statements to switch through the 25 possible combinations
 *		of (XStyle, YStyle). This is needed since one tranformation 
 *		object handles both x and y axis. The only real tricks here
 *		happen when either XStyle or YStyle is NhlIRREGULAR and the other
 *		is not. When this happens an IrregularTransObj is created 
 *		and one of the IrregularTranObj is "fooled" into a linear
 *		or log tranformation. This is facilitated for log axis by 
 *		a resource that instructs the IrragularTransObj to take
 *		the logs of the input values and create an approximation of
 *		the logs of the data values.
 *
 * In Args:	xnew	new instance record
 *		xold	old instance record if calledfrom == _NhlSETVALUES
 *		calledfrom  set to _NhlCREATE or _NhlSETVALUES
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes
SetUpTransObjs
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom
)
#else 
(xnew,xold,calledfrom)
	NhlXyPlotLayer	xnew;
	NhlXyPlotLayer	xold;
	_NhlCalledFrom	calledfrom;
#endif
{
	NhlSArg		sargs[30];
	int		nargs = 0;
	char		buffer[_NhlMAXRESNAMLEN];
	int		tmpid;
	float		tmpcoords[3];
	char		*error_lead=NULL;
	NhlClass	trans_class = NULL;
	NhlGenArray	gen;
	NhlXyPlotLayerPart	*newxy = &xnew->xyplot;
	NhlXyPlotLayerPart	*oldxy=NULL;
	NhlTransformLayerPart	*tfp = &xnew->trans;

/*
 * Now create main transformation object
 */	
	if(calledfrom == _NhlCREATE){
		error_lead = "XyPlotInitialize";
	}
	else{
		oldxy = &xold->xyplot;

		if(calledfrom == _NhlSETVALUES){
			error_lead = "XyPlotSetValues";
		}
		else if (calledfrom == _NhlUPDATEDATA){
		/*
		 * If we are coming from UpdateData - The only resources that
		 * could have changed are min and max - if they haven't changed
		 * return immediately.
		 */
			if((newxy->x_min == oldxy->x_min) &&
				(newxy->x_max == oldxy->x_max) &&
				(newxy->y_min == oldxy->y_min) &&
				(newxy->y_max == oldxy->y_max)){
				return NhlNOERROR;
			}
			error_lead = "XyPlotUpdateData";
		}
	}

	/*
	 * If a new trans object needs to be created, do this.
	 */
	if(	(newxy->thetrans == NULL)
		||
		(calledfrom == _NhlCREATE)
		||
		(	(	(newxy->x_style == NhlIRREGULAR) ||
				(newxy->y_style == NhlIRREGULAR)
			) &&
			!oldxy->have_irreg_trans
		)
		||
		(	(newxy->x_style != NhlIRREGULAR) &&
			(newxy->y_style != NhlIRREGULAR) &&
			oldxy->have_irreg_trans
		)
									){

		if(newxy->thetrans != NULL){
			(void)NhlDestroy(newxy->thetrans->base.id);
			newxy->thetrans = NULL;
			tfp->trans_obj = NULL;
		}

		sprintf(buffer,"%s",xnew->base.name);
		strcat(buffer,".Trans");

		newxy->fake_x = newxy->fake_y = False;

		if(newxy->y_style == NhlIRREGULAR){

			trans_class = NhlirregularType2TransObjClass;
			newxy->have_irreg_trans = True;

			gen = newxy->y_irregular_points;
			NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								gen->data);
			NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,
							gen->len_dimensions[0]);
			NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,
							newxy->y_tension);

			if(newxy->x_style == NhlIRREGULAR){

				gen = newxy->x_irregular_points;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								gen->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
							gen->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newxy->x_tension);
			}
			else{

				newxy->fake_x = True;
				newxy->fake_x_min = tmpcoords[0] = newxy->x_min;
				newxy->fake_x_max = tmpcoords[2] = newxy->x_max;

				if(newxy->x_style == NhlLINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->x_style == NhlLOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0]) +
						log10(tmpcoords[2])) / 2.0);
					NhlSetSArg(&sargs[nargs++],
							NhlNtrXUseLog,True);
				}

				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);

			}
		}
		/*
		 * Y is not IRREG
		 */
		else{
			if(newxy->x_style == NhlIRREGULAR){

				trans_class = NhlirregularType2TransObjClass;
				newxy->have_irreg_trans = True;

				gen = newxy->x_irregular_points;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								gen->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
							gen->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newxy->x_tension);

				newxy->fake_y = True;
				newxy->fake_y_min = tmpcoords[0] = newxy->y_min;
				newxy->fake_y_max = tmpcoords[2] = newxy->y_max;

				if(newxy->y_style == NhlLINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->y_style == NhlLOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0]) +
						log10(tmpcoords[2])) / 2.0);
					NhlSetSArg(&sargs[nargs++],
							NhlNtrYUseLog,True);
				}

				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);

			}
			/*
			 * X is not IRREG
			 */
			else{
				trans_class = NhllogLinTransObjClass;
				newxy->have_irreg_trans = False;

				if(newxy->x_style == NhlLOG)
					NhlSetSArg(&sargs[nargs++],NhlNtrXLog,
									True);
				if(newxy->y_style == NhlLOG)
					NhlSetSArg(&sargs[nargs++],NhlNtrYLog,
									True);

			}
		}
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,newxy->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,newxy->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,newxy->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,newxy->y_max);

		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,newxy->x_reverse);
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,newxy->y_reverse);

		(void)NhlALCreate(&tmpid,buffer,trans_class,xnew->base.id,
								sargs,nargs);

		newxy->thetrans = _NhlGetLayer(tmpid);
		if(newxy->thetrans == NULL){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to continue without transformation",
								error_lead);
			return NhlFATAL;
		}
		tfp->trans_obj = newxy->thetrans;

		return NhlNOERROR;
	}

	/*
	 * SetValues/UpdateData in existing trans object
	 */

	/*
	 * if we are tricking an irreg object into being a log or lin - take
	 * care of setting the transformation.
	 */
	if(newxy->have_irreg_trans){
		if(newxy->fake_x){
			if(newxy->x_style == NhlIRREGULAR){
				newxy->fake_x = False;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
					newxy->x_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
				newxy->x_irregular_points->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newxy->x_tension);
			}
			else if((newxy->x_style != oldxy->x_style) ||
				(newxy->x_min < newxy->fake_x_min) ||
				(newxy->x_max > newxy->fake_x_max)){

				newxy->fake_x_min = tmpcoords[0] = newxy->x_min;
				newxy->fake_x_max = tmpcoords[2] = newxy->x_max;
				if(newxy->x_style == NhlLINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->x_style == NhlLOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0]) +
						log10(tmpcoords[2])) / 2.0);
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);
			}
		}
		else {
			if(newxy->x_style != NhlIRREGULAR){
				newxy->fake_x = True;
				newxy->fake_x_min = tmpcoords[0] = newxy->x_min;
				newxy->fake_x_max = tmpcoords[2] = newxy->x_max;
				if(newxy->x_style == NhlLINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->x_style == NhlLOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0]) +
						log10(tmpcoords[2])) / 2.0);
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);

			}
		}
		if(newxy->fake_y){
			if(newxy->y_style == NhlIRREGULAR){
				newxy->fake_y = False;
				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
					newxy->y_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,
				newxy->y_irregular_points->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,
							newxy->y_tension);
			}
			else if((newxy->y_style != oldxy->y_style) ||
				(newxy->y_min < newxy->fake_y_min) ||
				(newxy->y_max > newxy->fake_y_max)){

				newxy->fake_y_min = tmpcoords[0] = newxy->y_min;
				newxy->fake_y_max = tmpcoords[2] = newxy->y_max;
				if(newxy->y_style == NhlLINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->y_style == NhlLOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0]) +
						log10(tmpcoords[2])) / 2.0);
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);
			}
		}
		else {
			if(newxy->y_style != NhlIRREGULAR){
				newxy->fake_y = True;
				newxy->fake_y_min = tmpcoords[0] = newxy->y_min;
				newxy->fake_y_max = tmpcoords[2] = newxy->y_max;
				if(newxy->y_style == NhlLINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->y_style == NhlLOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0]) +
						log10(tmpcoords[2])) / 2.0);
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);

			}
		}
	}
		
	if(newxy->x_min != oldxy->x_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,newxy->x_min);
	if(newxy->x_max != oldxy->x_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,newxy->x_max);
	if(newxy->y_min != oldxy->y_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,newxy->y_min);
	if(newxy->y_max != oldxy->y_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,newxy->y_max);

	if(newxy->x_reverse != oldxy->x_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,newxy->x_reverse);
	if(newxy->y_reverse != oldxy->y_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,newxy->y_reverse);

	if(newxy->x_style != oldxy->x_style){
		if(newxy->have_irreg_trans)
			NhlSetSArg(&sargs[nargs++],NhlNtrXUseLog,
						(newxy->x_style == NhlLOG));
		else
			NhlSetSArg(&sargs[nargs++],NhlNtrXLog,
						(newxy->x_style == NhlLOG));
	}

	if(newxy->y_style != oldxy->y_style){
		if(newxy->have_irreg_trans)
			NhlSetSArg(&sargs[nargs++],NhlNtrYUseLog,
						(newxy->y_style == NhlLOG));
		else
			NhlSetSArg(&sargs[nargs++],NhlNtrYLog,
						(newxy->y_style == NhlLOG));
	}

	return NhlALSetValues(newxy->thetrans->base.id,sargs,nargs);
}

/*
 * Function:	SetUpTicks
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
 */
/*ARGSUSED*/
static NhlErrorTypes
SetUpTicks
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(xnew,xold,calledfrom)
	NhlXyPlotLayer	xnew;
	NhlXyPlotLayer	xold;
	_NhlCalledFrom	calledfrom;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlXyPlotLayerPart	*oxp = NULL;
	NhlXyPlotLayerPart	*nxp = &xnew->xyplot;
	NhlTransformLayerPart	*tfp = &xnew->trans;

	if(calledfrom == _NhlUPDATEDATA)
		return NhlNOERROR;

	if(calledfrom == _NhlSETVALUES)
		oxp = &xold->xyplot;

	if(!tfp->overlay_on ||
		nxp->display_tickmarks == NhlNOCREATE)
		return NhlNOERROR;

	if((calledfrom == _NhlCREATE) ||
		(nxp->display_tickmarks != oxp->display_tickmarks)){
		NhlSetSArg(&sargs[(*nargs)++],NhlNpmTickMarkDisplayMode,
			nxp->display_tickmarks);
	}

	return NhlNOERROR;
}

/*
 * Function:	SetUpTitles
 *
 * Description: 
 *
 * In Args:	xnew 	new instance record
 *		xold	old instance record
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects: Objects created or states changed. 	
 */
/*ARGSUSED*/
static NhlErrorTypes SetUpTitles
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom,
	NhlSArg		*sargs,
	int		*nargs
) 
#else 
(xnew,xold,calledfrom,sargs,nargs)
	NhlXyPlotLayer	xnew;
	NhlXyPlotLayer	xold;
	_NhlCalledFrom	calledfrom;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlXyPlotLayerPart	*oxp = NULL;
	NhlXyPlotLayerPart	*nxp = &xnew->xyplot;
	NhlTransformLayerPart	*tfp = &xnew->trans;

	if(calledfrom == _NhlUPDATEDATA)
		return NhlNOERROR;

	if(calledfrom == _NhlSETVALUES)
		oxp = &xold->xyplot;

	if(!tfp->overlay_on ||
		nxp->display_titles == NhlNOCREATE)
		return NhlNOERROR;

	if((calledfrom == _NhlCREATE) ||
		(nxp->display_titles != oxp->display_titles)){
		NhlSetSArg(&sargs[(*nargs)++],NhlNpmTitleDisplayMode,
			nxp->display_titles);
	}

	return NhlNOERROR;
}

/*
 * Function:	SetUpLegend
 *
 * Description: 
 *
 * In Args:	xnew 	new instance record
 *		xold	old instance record
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects: Objects created or states changed. 	
 */
/*ARGSUSED*/
static NhlErrorTypes SetUpLegend
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew,
	NhlXyPlotLayer	xold,
	_NhlCalledFrom	calledfrom,
	NhlSArg		*sargs,
	int		*nargs
) 
#else 
(xnew,xold,calledfrom,sargs,nargs)
	NhlXyPlotLayer	xnew;
	NhlXyPlotLayer	xold;
	_NhlCalledFrom	calledfrom;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlXyPlotLayerPart	*oxp = NULL;
	NhlXyPlotLayerPart	*nxp = &xnew->xyplot;
	NhlTransformLayerPart	*tfp = &xnew->trans;

	if(calledfrom == _NhlUPDATEDATA)
		return NhlNOERROR;

	if(calledfrom == _NhlSETVALUES)
		oxp = &xold->xyplot;

	if(!tfp->overlay_on ||
		nxp->display_legend == NhlNOCREATE)
		return NhlNOERROR;

	if((calledfrom == _NhlCREATE) ||
		(nxp->display_legend != oxp->display_legend)){
		NhlSetSArg(&sargs[(*nargs)++],NhlNpmLegendDisplayMode,
			nxp->display_legend);
	}

	if(calledfrom == _NhlCREATE){
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoDashIndex,False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoItemType,False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoLineColor,False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoLineDashSegLen,False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoLineLabelFontColor,
			   						False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoLineLabelFontHeight,
									False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoLineThickness,False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoMarkerColor,False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoMarkerIndex,False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoMarkerSize,False);
		NhlSetSArg(&sargs[(*nargs)++],NhlNlgMonoMarkerThickness,False);
	}

	if(!nxp->data_ranges_set)
		return NhlNOERROR;

	NhlSetSArg(&sargs[(*nargs)++],NhlNlgItemCount,nxp->num_cpairs);

	NhlSetSArg(&sargs[(*nargs)++],NhlNlgDashIndexes,nxp->dash_indexes);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgItemTypes,nxp->item_types);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgLabelStrings,nxp->lg_label_strings);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgLineColors,nxp->line_colors);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgLineDashSegLens,
						nxp->dash_seg_lens);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgLineLabelFontColors,
		   					nxp->llabel_colors);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgLineLabelStrings,
							nxp->llabel_strings);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgLineLabelFontHeights,
						nxp->llabel_fheights);

	NhlSetSArg(&sargs[(*nargs)++],NhlNlgLineThicknesses,
							nxp->line_thicknesses);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgMarkerColors,nxp->marker_colors);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgMarkerIndexes,nxp->marker_indexes);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgMarkerSizes,nxp->marker_sizes);
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgMarkerThicknesses,
						nxp->marker_thicknesses);

	return NhlNOERROR;
}
