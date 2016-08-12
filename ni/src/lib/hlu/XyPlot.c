/*
 *      $Id: XyPlot.c,v 1.94 2008-06-17 00:01:48 dbrown Exp $
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
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/XyPlotP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/MapTransObj.h>
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

	{NhlNxyLineColor,NhlCLineColor,NhlTColorIndex,sizeof(NhlColorIndex),
		Oset(color),NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),
		_NhlRES_DEFAULT,NULL},
	{NhlNxyLineColors,NhlCxyLineColors,NhlTColorIndexGenArray,
		sizeof(NhlGenArray),Oset(colors),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoLineColor,NhlCxyMonoLineColor,NhlTBoolean,sizeof(NhlBoolean),
		Oset(mono_color),NhlTImmediate,_NhlUSET((NhlPointer)False),
		_NhlRES_DEFAULT,NULL},
	{NhlNxyLineOpacityF,NhlCxyLineOpacityF,NhlTFloat,
		sizeof(float),Oset(line_opacity),NhlTString,
		_NhlUSET((NhlPointer)"1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNxyLineOpacities,NhlCxyLineOpacities,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(line_opacities),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},

	{NhlNxyDashPattern,NhlCLineDashPattern,NhlTDashIndex,sizeof(NhlDashIndex),
		Oset(dash),NhlTImmediate,_NhlUSET((NhlPointer)NhlSOLIDLINE),
		_NhlRES_DEFAULT,NULL},
	{NhlNxyDashPatterns,NhlCxyDashPatterns,NhlTDashIndexGenArray,
		sizeof(NhlGenArray), Oset(dashes),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL), _NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoDashPattern,NhlCxyMonoDashPattern,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_dash),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},

	{NhlNxyLineThicknessF,NhlCLineThicknessF,NhlTFloat,sizeof(float),
		Oset(line_thickness),NhlTString,_NhlUSET((NhlPointer)"1.0"),
		_NhlRES_DEFAULT,NULL},
	{NhlNxyLineThicknesses,NhlCxyLineThicknesses,NhlTFloatGenArray,
		sizeof(NhlGenArray), Oset(line_thicknesses),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL), _NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoLineThickness,NhlCxyMonoLineThickness,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_line_thickness),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},

	{NhlNxyMarkLineMode,NhlCxyMarkLineMode,NhlTMarkLineMode,
		sizeof(NhlMarkLineMode),Oset(marker_mode),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlLINES),_NhlRES_DEFAULT,(NhlFreeFunc)NULL},
	{NhlNxyMarkLineModes,NhlCxyMarkLineModes,NhlTMarkLineModeGenArray,
		sizeof(NhlGenArray),Oset(marker_modes),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarkLineMode,NhlCxyMonoMarkLineMode,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker_mode),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},

	{NhlNxyMarker,NhlCMarkerIndex,NhlTMarkerIndex,sizeof(NhlMarkerIndex),
		Oset(marker),NhlTImmediate,_NhlUSET((NhlPointer)0),_NhlRES_DEFAULT,NULL},
	{NhlNxyMarkers,NhlCxyMarkers,NhlTMarkerIndexGenArray,
		sizeof(NhlGenArray),Oset(markers),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarker,NhlCxyMonoMarker,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},

	{NhlNxyMarkerColor,NhlCMarkerColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(marker_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_DEFAULT,(NhlFreeFunc)NULL},
	{NhlNxyMarkerColors,NhlCxyMarkerColors,NhlTColorIndexGenArray,
		sizeof(NhlGenArray),Oset(marker_colors),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarkerColor,NhlCxyMonoMarkerColor,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker_color),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},
	{NhlNxyMarkerOpacityF,NhlCxyMarkerOpacityF,NhlTFloat,
		sizeof(float),Oset(marker_opacity),NhlTString,
		_NhlUSET((NhlPointer)"1.0"),_NhlRES_DEFAULT,NULL},
	{NhlNxyMarkerOpacities,NhlCxyMarkerOpacities,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(marker_opacities),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
         	Oset(marker_size_set),NhlTImmediate,
         	_NhlUSET((NhlPointer)True),_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{NhlNxyMarkerSizeF,NhlCMarkerSizeF,NhlTFloat,
		sizeof(float),Oset(marker_size),NhlTProcedure,
		_NhlUSET((NhlPointer)ResUnset),_NhlRES_DEFAULT,(NhlFreeFunc)NULL},
	{NhlNxyMarkerSizes,NhlCxyMarkerSizes,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(marker_sizes),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarkerSize,NhlCxyMonoMarkerSize,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker_size),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},

	{NhlNxyMarkerThicknessF,NhlCMarkerThicknessF,NhlTFloat,sizeof(float),
		Oset(marker_thickness),NhlTString,_NhlUSET((NhlPointer)"1.0"),
		_NhlRES_DEFAULT,NULL},
	{NhlNxyMarkerThicknesses,NhlCxyMarkerThicknesses,NhlTFloatGenArray,
		sizeof(NhlGenArray),Oset(marker_thicknesses),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoMarkerThickness,NhlCxyMonoMarkerThickness,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_marker_thickness),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},

	{NhlNxyLabelMode,NhlCxyLabelMode,NhlTLineLabelMode,
		sizeof(NhlLineLabelMode),
		Oset(label_mode),NhlTImmediate,_NhlUSET((NhlPointer)NhlNOLABELS),
		_NhlRES_DEFAULT,NULL},
	{NhlNxyExplicitLabels,NhlCxyExplicitLabels,NhlTStringGenArray,
		sizeof(NhlGenArray),Oset(labels),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},

	{NhlNxyLineLabelFontColor,NhlCFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(label_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_DEFAULT,(NhlFreeFunc)NULL},
	{NhlNxyLineLabelFontColors,NhlCxyLineLabelFontColors,
		NhlTColorIndexGenArray,
		sizeof(NhlGenArray),Oset(label_colors),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyMonoLineLabelFontColor,NhlCxyMonoLineLabelFontColor,NhlTBoolean,
		sizeof(NhlBoolean),Oset(mono_label_color),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},

	{NhlNxyExplicitLegendLabels,NhlCxyExplicitLegendLabels,
		NhlTStringGenArray,
		sizeof(NhlGenArray),Oset(lg_label_strings),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
         	Oset(dash_seg_len_set),NhlTImmediate,
         	_NhlUSET((NhlPointer)True),_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{NhlNxyLineDashSegLenF,NhlCLineDashSegLenF,NhlTFloat,
		sizeof(float),Oset(dash_seg_len),NhlTProcedure,
		_NhlUSET((NhlPointer)ResUnset),_NhlRES_DEFAULT,NULL},

	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(llabel_fheight_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
		_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{NhlNxyLineLabelFontHeightF,NhlCFontHeightF,NhlTFloat,
		sizeof(float),Oset(llabel_fheight),NhlTProcedure,
		_NhlUSET((NhlPointer)ResUnset),_NhlRES_DEFAULT,NULL},
	{NhlNxyLineLabelFont,NhlCFont,NhlTFont,
		sizeof(NhlFont),Oset(llabel_font),NhlTImmediate,
		_NhlUSET((NhlPointer) 21),0,NULL},
	{NhlNxyLineLabelFontAspectF,NhlCFontAspectF,NhlTFloat,
	 	sizeof(float),Oset(llabel_faspect),NhlTString,
	 	_NhlUSET("1.3125"),0,NULL},
        {NhlNxyLineLabelFontThicknessF,NhlCFontThicknessF,
                 NhlTFloat,sizeof(float),Oset(llabel_fthickness),
                 NhlTString, _NhlUSET("1.0"),0,NULL},
        {NhlNxyLineLabelFontQuality,NhlCFontQuality,
                 NhlTFontQuality,
                 sizeof(NhlFontQuality),Oset(llabel_fquality),
                 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
        {NhlNxyLineLabelConstantSpacingF,NhlCTextConstantSpacingF,
                 NhlTFloat,sizeof(float),Oset(llabel_cspacing),
                 NhlTString,_NhlUSET("0.0"),0,NULL},
        {NhlNxyLineLabelFuncCode,NhlCTextFuncCode,NhlTCharacter,
                 sizeof(char),Oset(llabel_func_code),
                 NhlTString, _NhlUSET("~"),0,NULL}

/* End-documented-resources */

};
#undef Oset

#define	Oset(field)	NhlOffset(NhlXyPlotLayerRec,xyplot.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNxyCoordData,NhlCxyCoordData,_NhlTDataList,sizeof(NhlGenArray),
		 Oset(curve_data),NhlTImmediate,_NhlUSET(NULL),_NhlRES_NORACCESS,
	 	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyCoordDataSpec,NhlCxyCoordDataSpec,_NhlTDataSpecList,
		 sizeof(NhlGenArray),Oset(dspeclist),NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_GONLY,(NhlFreeFunc)NhlFreeGenArray},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(x_style_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
		_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{NhlNxyXStyle,NhlCxyXStyle,NhlTTickMarkStyle,sizeof(NhlTickMarkStyle),
		Oset(x_style),NhlTProcedure,
		_NhlUSET((NhlPointer)ResUnset),_NhlRES_DEFAULT,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(y_style_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
		_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{NhlNxyYStyle,NhlCxyYStyle,NhlTTickMarkStyle,sizeof(NhlTickMarkStyle),
		Oset(y_style),NhlTProcedure,
		_NhlUSET((NhlPointer)ResUnset),_NhlRES_DEFAULT,NULL},

	{NhlNxyXIrregularPoints,NhlCxyXIrregularPoints,NhlTFloatGenArray,
		sizeof(NhlPointer),Oset(x_irregular_points),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyYIrregularPoints,NhlCxyYIrregularPoints,NhlTFloatGenArray,
		sizeof(NhlPointer),Oset(y_irregular_points),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},

	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(comp_x_min_set),
		NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(comp_x_max_set),
		NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(comp_y_max_set),
		NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(comp_y_min_set),
		NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},

	{NhlNxyComputeXMin,NhlCxyComputeExtent,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_x_min),NhlTProcedure,_NhlUSET((NhlPointer)ResUnset),
		_NhlRES_DEFAULT,NULL},
	{NhlNxyComputeXMax,NhlCxyComputeExtent,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_x_max),NhlTProcedure,_NhlUSET((NhlPointer)ResUnset),
		_NhlRES_DEFAULT,NULL},
	{NhlNxyComputeYMax,NhlCxyComputeExtent,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_y_max),NhlTProcedure,_NhlUSET((NhlPointer)ResUnset),
		_NhlRES_DEFAULT,NULL},
	{NhlNxyComputeYMin,NhlCxyComputeExtent,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_y_min),NhlTProcedure,_NhlUSET((NhlPointer)ResUnset),
		_NhlRES_DEFAULT,NULL},
 	{NhlNxyCurveDrawOrder,NhlCxyCurveDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(curve_order),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlDRAW),0,NULL},

	{NhlNtrXTensionF,NhlCtrXTensionF,NhlTFloat,sizeof(float),
		 Oset(x_tension),NhlTString,_NhlUSET("2.0"),
         	_NhlRES_DEFAULT|_NhlRES_INTERCEPTED,NULL},
	{NhlNtrYTensionF,NhlCtrYTensionF,NhlTFloat,sizeof(float),
		 Oset(y_tension),NhlTString,_NhlUSET("2.0"),
         	_NhlRES_DEFAULT|_NhlRES_INTERCEPTED,NULL},
	{ NhlNtrLineInterpolationOn,NhlCtrLineInterpolationOn,
		  NhlTBoolean,sizeof(NhlBoolean),
		  NhlOffset(NhlXyPlotLayerRec,trans.line_interpolation_on),
		  NhlTImmediate,_NhlUSET((NhlPointer)False),
	  	_NhlRES_INTERCEPTED,NULL},
	{ NhlNvpClipOn,NhlCvpClipOn,
		  NhlTBoolean,sizeof(NhlBoolean),
		  NhlOffset(NhlXyPlotLayerRec,view.clip_on),
		  NhlTImmediate,_NhlUSET((NhlPointer)False),
	  	_NhlRES_INTERCEPTED,NULL},

/*
 * These resources have not been implimented yet.
 */
#ifdef	NOT
	{NhlNxyXAlternate,NhlCxyXAlternate,NhlTAlternatePlace,
		sizeof(NhlAlternatePlace),Oset(x_alternate),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlNONE),_NhlRES_DEFAULT,NULL},
	{NhlNxyYAlternate,NhlCxyYAlternate,NhlTAlternatePlace,
		sizeof(NhlAlternatePlace),Oset(y_alternate),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlNONE),_NhlRES_DEFAULT,NULL},
	{NhlNxyXAlternateCoords,NhlCxyXAlternateCoords,NhlTFloatGenArray,
		 sizeof(NhlGenArray),Oset(x_alternate_coords),NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyXOriginalCoords,NhlCxyXOriginalCoords,NhlTFloatGenArray,
		 sizeof(NhlGenArray),Oset(x_original_coords),NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyYAlternateCoords,NhlCxyYAlternateCoords,NhlTFloatGenArray,
		 sizeof(NhlGenArray),Oset(y_alternate_coords),NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNxyYOriginalCoords,NhlCxyYOriginalCoords,NhlTFloatGenArray,
		 sizeof(NhlGenArray),Oset(y_original_coords),NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
#endif
/* End-documented-resources */

	{NhlNpmTitleDisplayMode,NhlCpmTitleDisplayMode,
		NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		Oset(display_titles),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlCONDITIONAL),
         	_NhlRES_DEFAULT|_NhlRES_INTERCEPTED,NULL},
	{NhlNpmTickMarkDisplayMode,NhlCpmTickMarkDisplayMode,
		NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		Oset(display_tickmarks),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlCONDITIONAL),
         	_NhlRES_DEFAULT|_NhlRES_INTERCEPTED,NULL},
	{NhlNpmLegendDisplayMode,NhlCpmLegendDisplayMode,
		NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		Oset(display_legend),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlNEVER),
         	_NhlRES_DEFAULT|_NhlRES_INTERCEPTED,NULL},
	{_NhlNxyDSpecChanged,_NhlCxyDSpecChanged,NhlTBoolean,
		 sizeof(NhlBoolean),
	 Oset(dspec_changed),NhlTImmediate,_NhlUSET(NULL),
         	 _NhlRES_SONLY|_NhlRES_PRIVATE,NULL},
	{NhlNpmUpdateReq,NhlCpmUpdateReq,NhlTInteger,sizeof(int),
		  Oset(update_req),NhlTImmediate,
         	  _NhlUSET((NhlPointer) False),_NhlRES_PRIVATE,NULL}

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

static NhlErrorTypes XyDataDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
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


static NhlErrorTypes XyPlotPreDraw(
#if	NhlNeedProto
        NhlLayer	/* layer */
#endif
);

static NhlErrorTypes XyPlotDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes XyPlotPostDraw(
#if	NhlNeedProto
        NhlLayer	/* layer */
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
	NhlXyPlotLayer	xlayer,
	NhlLayer	thetrans,
	NhlDrawOrder	order,
	NhlString	func
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
/* cvt_table			*/	NULL,

/* layer_resources		*/	data_resources,
/* num_resources		*/	NhlNumber(data_resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	XyDataClassInitialize,
/* layer_initialize		*/	XyDataInitialize,
/* layer_set_values		*/	XyDataSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	XyDataGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	XyDataDestroy
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
/* cvt_table			*/	NULL,

/* layer_resources              */      resources,
/* num_resources                */      NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

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

/* layer_pre_draw               */      XyPlotPreDraw,
/* layer_draw_segonly           */      NULL,
/* layer_post_draw              */      XyPlotPostDraw,
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
/* ndc_polyline			*/	NhlInheritPolyTransFunc,
/* data_polygon			*/	NhlInheritPolyTransFunc,
/* ndc_polygon			*/	NhlInheritPolyTransFunc,
/* data_polymarker		*/	NhlInheritPolyTransFunc,
/* ndc_polymarker		*/	NhlInheritPolyTransFunc
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
		{NhlNOLABELS,	"NoLabels"},
		{NhlLETTERED,	"Lettered"},
		{NhlCUSTOM,	"Custom"}
	};

	_NhlRegisterEnumType(NhlxyDataSpecClass,NhlTLineLabelMode,lblmode,
		NhlNumber(lblmode));
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
	char		*e_text;
	char		*entry_name = "XyPlotClassPartInitialize";

	/*
	 * Register children objects:
         * NOTE: order of registration should be the reverse of the
         * desired 'canonical' order
	 */
	lret = _NhlRegisterChildClass(lc,NhlplotManagerClass,False,False,
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
			NhlNpmLabelBarDisplayMode,NhlNpmLabelBarZone,
                        NhlNpmLabelBarWidthF,NhlNpmLabelBarHeightF,
                        NhlNpmLabelBarSide,NhlNpmLabelBarParallelPosF,
                        NhlNpmLabelBarOrthogonalPosF,              
			NULL);

	if ((ret = MIN(ret,lret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlplotManagerClass");
		return(NhlFATAL);
	}
        
        lret = _NhlRegisterChildClass(lc,NhlirregularTransObjClass,
					False,True,NULL);
	if ((ret = MIN(ret,lret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlirregularTransObjClass");
		return(NhlFATAL);
	}

	lret = _NhlRegisterChildClass(lc,NhllogLinTransObjClass,
					False,True,NULL);
	if ((ret = MIN(ret,lret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhltransObjClass");
		return(NhlFATAL);
	}

	/*
	 * Register Data Resources
	 */

	lret = _NhlRegisterDataRes((NhlDataCommClass)lc,NhlNxyCoordData,
			NhlNxyCoordDataSpec,NhlxyDataSpecClass,
			NhlcoordArrTableFloatClass,NULL);
        
	if ((ret = MIN(ret,lret)) < NhlWARNING) {
		e_text = "%s: error registering data resource %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlcoordArrTableFloatClass");
		return(NhlFATAL);
	}
	return ret;
}

/*
 * Function:	XyResetExtents
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
XyResetExtents
#if	NhlNeedProto
(
	NhlXyPlotLayer	xnew
)
#else
(xnew)
	NhlXyPlotLayer	xnew;
#endif
{
	char			func[] = "XyResetExtents";
	NhlSArg			sargs[30];
	NhlGArg			gargs[30];
	int			nargs;
	float			xb_start,xb_end,yl_start,yl_end;
	float			xt_start,xt_end,yr_start,yr_end;
	NhlTickMarkMode		xb_mode,xt_mode,yl_mode,yr_mode;
	NhlXyPlotLayerPart	*newxy = &xnew->xyplot;
	NhlTransformLayerPart	*tfp = &xnew->trans;
	NhlErrorTypes		ret;

	if (! xnew->trans.plot_manager_on ||
	    newxy->display_tickmarks < NhlALWAYS)
		return NhlNOERROR;
	    
	nargs = 0;
	NhlSetGArg(&gargs[nargs++],NhlNtmXBMode,&xb_mode);
	NhlSetGArg(&gargs[nargs++],NhlNtmXBTickStartF,&xb_start);
	NhlSetGArg(&gargs[nargs++],NhlNtmXBTickEndF,&xb_end);
	NhlSetGArg(&gargs[nargs++],NhlNtmXTMode,&xt_mode);
	NhlSetGArg(&gargs[nargs++],NhlNtmXTTickStartF,&xt_start);
	NhlSetGArg(&gargs[nargs++],NhlNtmXTTickEndF,&xt_end);
	NhlSetGArg(&gargs[nargs++],NhlNtmYLMode,&yl_mode);
	NhlSetGArg(&gargs[nargs++],NhlNtmYLTickStartF,&yl_start);
	NhlSetGArg(&gargs[nargs++],NhlNtmYLTickEndF,&yl_end);
	NhlSetGArg(&gargs[nargs++],NhlNtmYRMode,&yr_mode);
	NhlSetGArg(&gargs[nargs++],NhlNtmYRTickStartF,&yr_start);
	NhlSetGArg(&gargs[nargs++],NhlNtmYRTickEndF,&yr_end);
	ret = NhlALGetValues(newxy->overlay->base.id,gargs,nargs);
	if(ret < NhlNOERROR){
		NhlPError(ret,NhlEUNKNOWN,
			"%s:Nice data extents will not be determined",func);
		return ret;
	}

	nargs = 0;
	/*
	 * Only reset extent's if tickmark mode is automatic, otherwise
	 * the already computed data min/max are the correct values.
	 * Also don't do it if the data is constant.
	 */
	if(((xb_mode == NhlAUTOMATIC) || (xt_mode == NhlAUTOMATIC)) &&
	   (_NhlCmpFAny2(newxy->x_data_min,newxy->x_data_max,5,1e-32) != 0)){
		if(newxy->compute_x_min) {
			tfp->x_min = (xb_mode == NhlAUTOMATIC)?
							xb_start:xt_start;
			NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
		}
		if(newxy->compute_x_max){
			tfp->x_max = (xb_mode == NhlAUTOMATIC)?xb_end:xt_end;
			NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
		}
	}
	if(((yl_mode == NhlAUTOMATIC) || (yr_mode == NhlAUTOMATIC)) &&
	   (_NhlCmpFAny2(newxy->y_data_min,newxy->y_data_max,5,1e-32) != 0)) {
		if(newxy->compute_y_min){
			tfp->y_min = (yl_mode == NhlAUTOMATIC)?
							yl_start:yr_start;
			NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
		}
		if(newxy->compute_y_max){
			tfp->y_max = (yl_mode == NhlAUTOMATIC)?yl_end:yr_end;
			NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
		}
	}

	if(!nargs)
		return NhlNOERROR;

	ret = NhlALSetValues(newxy->thetrans->base.id,sargs,nargs);
	if(ret < NhlNOERROR){
		NhlPError(ret,NhlEUNKNOWN,
			"%s:Nice data extents will not be determined",func);
		return ret;
	}

	nargs = 0;
	NhlSetSArg(&sargs[nargs++],NhlNpmUpdateReq,True);
	newxy->update_req = False;
	return _NhlManageOverlay(&newxy->overlay,(NhlLayer)xnew,
			(NhlLayer)xnew,_NhlUPDATEDATA,sargs,nargs,func);

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

		if(xnew->xyplot.update_req){
			NhlSetSArg(&sargs[nsargs++],NhlNpmUpdateReq,True);
			xnew->xyplot.update_req = False;
		}

		ret2 = _NhlManageOverlay(&xnew->xyplot.overlay,(NhlLayer)xnew,
			(NhlLayer)xold,calledfrom,sargs,nsargs,
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


	if(xnew->xyplot.check_ranges || xnew->xyplot.update_req){
		NhlSetSArg(&sargs[nsargs++],NhlNpmUpdateReq,True);
		xnew->xyplot.update_req = False;
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
			(NhlLayer)xold,calledfrom,sargs,nsargs,
			"XyPlotChanges");
	ret1 = MIN(ret1,ret2);

	if (xnew->xyplot.check_ranges) {
		ret2 = XyResetExtents(xnew);
	}
	ret1 = MIN(ret1,ret2);

	xnew->xyplot.check_ranges = False;
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
	NhlXyDataSpecLayerPart	*dnp = &((NhlXyDataSpecLayer)new)->xydata;
	NhlViewLayerPart	*vp =
				&((NhlViewLayer)new->base.parent)->view;
	float			ave;

	dnp->dashes=_NhlCopyGenArray(dnp->dashes,True);
	dnp->marker_modes=_NhlCopyGenArray(dnp->marker_modes,True);
	dnp->lg_label_strings=_NhlCopyGenArray(dnp->lg_label_strings,True);
	dnp->colors=_NhlCopyGenArray(dnp->colors,True);
        dnp->line_opacities = _NhlCopyGenArray(dnp->line_opacities, True);
	dnp->label_colors=_NhlCopyGenArray(dnp->label_colors,True);
	dnp->labels=_NhlCopyGenArray(dnp->labels,True);
	dnp->line_thicknesses=_NhlCopyGenArray(dnp->line_thicknesses,True);
	dnp->marker_colors=_NhlCopyGenArray(dnp->marker_colors,True);
        dnp->marker_opacities = _NhlCopyGenArray(dnp->marker_opacities, True);
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

	if (dnp->marker_sizes != NULL) {
		int	i;
		float *marker_sizes = (float *) dnp->marker_sizes->data;

		for (i=0;i < dnp->marker_sizes->num_elements; i++) {
			marker_sizes[i] /= ave;
		}
	}

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

	xp->new_draw_req = True;
	if(!xp->comp_x_min_set) xp->compute_x_min = !tfp->x_min_set;
	if(!xp->comp_x_max_set) xp->compute_x_max = !tfp->x_max_set;
	if(!xp->comp_y_min_set) xp->compute_y_min = !tfp->y_min_set;
	if(!xp->comp_y_max_set) xp->compute_y_max = !tfp->y_max_set;

	if(!tfp->x_min_set) tfp->x_min = 1.0;
	if(!tfp->x_max_set) tfp->x_max = 2.0;
	if(!tfp->y_min_set) tfp->y_min = 1.0;
	if(!tfp->y_max_set) tfp->y_max = 2.0;

	xp->x_data_min = xp->x_irreg_min = tfp->x_min;
	xp->x_data_max = xp->x_irreg_max = tfp->x_max;
	xp->y_data_min = xp->y_irreg_min = tfp->y_min;
	xp->y_data_max = xp->y_irreg_max = tfp->y_max;
	
	xnew->trans.data_xstart = tfp->x_min;
	xnew->trans.data_xend = tfp->x_max;
	xnew->trans.data_ystart = tfp->y_min;
	xnew->trans.data_yend = tfp->y_max;

	xp->thetrans = NULL;
	tfp->trans_obj = NULL;
	xp->data_ranges_set = False;
	xp->check_ranges = True;
	xp->overlay = NULL;
	xp->predraw_dat = NULL;
	xp->draw_dat = NULL;
	xp->postdraw_dat = NULL;

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
	xp->line_opacities = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
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
	xp->marker_opacities = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
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
	xp->llabel_fonts = _NhlCreateGenArray(NULL,NhlTFont,
					sizeof(NhlFont),0,NULL,True);
	xp->llabel_faspects = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
	xp->llabel_fthicknesses = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
	xp->llabel_fqualities = _NhlCreateGenArray(NULL,NhlTFontQuality,
					sizeof(NhlFontQuality),0,NULL,True);
	xp->llabel_cspacings = _NhlCreateGenArray(NULL,NhlTFloat,
					sizeof(float),0,NULL,True);
	xp->llabel_func_codes = _NhlCreateGenArray(NULL,NhlTCharacter,
					sizeof(char),0,NULL,True);

	if(!xp->dash_indexes || !xp->item_types || !xp->lg_label_strings ||
		!xp->line_colors || !xp->line_opacities || !xp->dash_seg_lens || !xp->llabel_colors ||
		!xp->llabel_strings || !xp->llabel_fheights ||
		!xp->line_thicknesses || !xp->marker_colors || !xp->marker_opacities ||
		!xp->marker_indexes || !xp->marker_sizes ||
		!xp->marker_thicknesses || !xp->xvectors || !xp->yvectors ||
		!xp->len_vectors || !xp->missing_set || !xp->xmissing ||
		!xp->ymissing || !xp->llabel_fonts || !xp->llabel_faspects ||
		!xp->llabel_fthicknesses || !xp->llabel_fqualities ||
		!xp->llabel_cspacings || !xp->llabel_func_codes){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyDashPatterns)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyMarkLineModes)){
		gen = xdnp->marker_modes;
		xdnp->marker_modes = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->marker_modes){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyMarkLineModes);
			xdnp->marker_modes = xdop->marker_modes;
		}
		else{
			NhlFreeGenArray(xdop->marker_modes);
			status = True;
		}
	}

	if(_NhlArgIsSet(args,num_args,NhlNxyExplicitLegendLabels)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyLineColors)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyLineOpacities)){
		gen = xdnp->line_opacities;
		xdnp->line_opacities = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->line_opacities){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyLineOpacities);
			xdnp->line_opacities = xdop->line_opacities;
		}
		else{
			NhlFreeGenArray(xdop->line_opacities);
			status = True;
		}
	}

	if(_NhlArgIsSet(args,num_args,NhlNxyLineLabelFontColors)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyExplicitLabels)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyLineThicknesses)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyMarkerColors)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyMarkerOpacities)){
		gen = xdnp->marker_opacities;
		xdnp->marker_opacities = _NhlCopyGenArray(gen,True);
		if(gen && !xdnp->marker_opacities){
			NhlPError(NhlWARNING,ENOMEM,
				"%s:Resetting %s to previous value",
				func,NhlNxyMarkerOpacities);
			xdnp->marker_opacities = xdop->marker_opacities;
		}
		else{
			NhlFreeGenArray(xdop->marker_opacities);
			status = True;
		}
	}

	if(_NhlArgIsSet(args,num_args,NhlNxyMarkers)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyMarkerSizes)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyMarkerThicknesses)){
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

	if(_NhlArgIsSet(args,num_args,NhlNxyMarkerSizeF)){
		xdnp->marker_size = xdnp->marker_size / xyp->vp_average;
	}
	if(_NhlArgIsSet(args,num_args,NhlNxyLineDashSegLenF)){
		xdnp->dash_seg_len = xdnp->dash_seg_len / xyp->vp_average;
	}
	if(_NhlArgIsSet(args,num_args,NhlNxyLineLabelFontHeightF)){
		xdnp->llabel_fheight = xdnp->llabel_fheight / xyp->vp_average;
	}

	return NhlVASetValues(new->base.parent->base.id,
				_NhlNxyDSpecChanged,	True,
				NULL);
}


/*ARGSUSED*/
static NhlBoolean NewDrawArgs
#if	NhlNeedProto
(
	_NhlArgList	args,
	int		num_args
)
#else
(args,num_args)
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlString pass_args[] = {
		NhlNvpXF,
		NhlNvpYF,
		NhlNvpWidthF,
		NhlNvpHeightF,
                NhlNvpOn,
		NhlNtfBaseXF,
		NhlNtfBaseYF,
		NhlNtfBaseWidthF,
		NhlNtfBaseHeightF,
		NhlNpmLegendDisplayMode,
		NhlNpmTickMarkDisplayMode,
		NhlNpmTitleDisplayMode
	};
	int i,pass_count = 0;

	for (i = 0; i < NhlNumber(pass_args); i++)
		if (_NhlArgIsSet(args,num_args,pass_args[i]))
			pass_count++;
	if (num_args > pass_count) 
		return True;
	return False;
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
	
	if (xn->view.use_segments != xo->view.use_segments) {
		xn->xyplot.new_draw_req = True;
	}
	if (xn->view.use_segments) {
                NhlTransDat *trans_dat = NULL;
                
		if (NewDrawArgs(args,num_args))
			xn->xyplot.new_draw_req = True;
                
                if (xn->xyplot.draw_dat)
                        trans_dat = xn->xyplot.draw_dat;
                else if (xn->xyplot.postdraw_dat)
                        trans_dat = xn->xyplot.postdraw_dat;
                else if (xn->xyplot.predraw_dat)
                        trans_dat = xn->xyplot.predraw_dat;
                if (! _NhlSegmentSpansArea(trans_dat,
                                           xn->view.x,
                                           xn->view.x + xn->view.width,
                                           xn->view.y - xn->view.height,
                                           xn->view.y))
                        xn->xyplot.new_draw_req = True;

	}

	if((xn->view.width != xo->view.width) ||
		(xn->view.height != xo->view.height))
		xn->xyplot.vp_average = (xn->view.width+xn->view.height)/2.0;

	if (_NhlArgIsSet(args,num_args,NhlNxyXStyle))
		xn->xyplot.x_style_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNxyYStyle))
		xn->xyplot.y_style_set = True;
	
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
	ng_size_t	new_size,
	NhlGenArray	gen
)
#else
(new_size,gen)
	ng_size_t	new_size;
	NhlGenArray	gen;
#endif
{

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
	char				buffer[_NhlMAXRESNAMLEN];
	NhlCoordArrTableFloatLayer	datal;
	NhlXyDataSpecLayer		dataspec;
	NhlXyDataSpecLayerPart		*dsp;
	NhlDashIndex			*dash_indexes;
	NhlMarkLineMode			*item_types;
	NhlString			*lg_label_strings;
	NhlColorIndex			*line_colors;
        float                           *line_opacities;
	float				*dash_seg_lens;
	NhlColorIndex			*llabel_colors;
	NhlString			*llabel_strings;
	float				*llabel_fheights;
	float				*line_thicknesses;
	NhlColorIndex			*marker_colors;
        float                           *marker_opacities;
	NhlMarkerIndex			*marker_indexes;
	float				*marker_sizes;
	float				*marker_thicknesses;
	float				**xvectors;
	float				**yvectors;
	int				*len_vectors;
	int				*missing_set;
	float				*xmissing;
	float				*ymissing;
	NhlFont				*llabel_fonts;
	float				*llabel_faspects;
	float				*llabel_fthicknesses;
	NhlFontQuality			*llabel_fqualities;
	float				*llabel_cspacings;
	char				*llabel_func_codes;
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
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->line_opacities));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->dash_seg_lens));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_colors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_strings));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_fheights));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->line_thicknesses));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->marker_colors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->marker_opacities));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->marker_indexes));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->marker_sizes));
		ret = MIN(ret,GrowGen
			  (xlp->num_cpairs,xlp->marker_thicknesses));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->xvectors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->yvectors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->len_vectors));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->missing_set));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->xmissing));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->ymissing));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_fonts));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_faspects));
		ret = MIN(ret,GrowGen
			  (xlp->num_cpairs,xlp->llabel_fthicknesses));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_fqualities));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_cspacings));
		ret = MIN(ret,GrowGen(xlp->num_cpairs,xlp->llabel_func_codes));

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
        line_opacities = xlp->line_opacities->data;
	dash_seg_lens = xlp->dash_seg_lens->data;
	llabel_colors = xlp->llabel_colors->data;
	llabel_strings = xlp->llabel_strings->data;
	llabel_fheights = xlp->llabel_fheights->data;
	line_thicknesses = xlp->line_thicknesses->data;
	marker_colors = xlp->marker_colors->data;
        marker_opacities = xlp->marker_opacities->data;
	marker_indexes = xlp->marker_indexes->data;
	marker_sizes = xlp->marker_sizes->data;
	marker_thicknesses = xlp->marker_thicknesses->data;
	xvectors = xlp->xvectors->data;
	yvectors = xlp->yvectors->data;
	len_vectors = xlp->len_vectors->data;
	missing_set = xlp->missing_set->data;
	xmissing = xlp->xmissing->data;
	ymissing = xlp->ymissing->data;
	llabel_fonts = xlp->llabel_fonts->data;
	llabel_faspects = xlp->llabel_faspects->data;
	llabel_fthicknesses = xlp->llabel_fthicknesses->data;
	llabel_fqualities = xlp->llabel_fqualities->data;
	llabel_cspacings = xlp->llabel_cspacings->data;
	llabel_func_codes = xlp->llabel_func_codes->data;

	memset((char*)missing_set,0,
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
                float           *opacities=NULL;
                int             len_opacities=0;
		int		*labelcolortable= NULL;
		int		len_labelcolortable = 0;
		NhlString	*labeltable=NULL;
		int		len_labeltable =0;
		float		*linethicktable = NULL;
		int		len_linethicktable = 0;
		int		*markercolortable= NULL;
		int		len_markercolortable = 0;
                float           *markeropacities=NULL;
                int             len_markeropacities=0;
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
		 * line colors and opacities
		 */
		if(dsp->colors != NULL && !dsp->mono_color){
			ctable = (int*)dsp->colors->data;
			len_ctable = dsp->colors->len_dimensions[0];
		}
		else
			len_ctable = 0;
                
		if(dsp->line_opacities != NULL){
			opacities = (float*)dsp->line_opacities->data;
			len_opacities = dsp->line_opacities->len_dimensions[0];
		}
		else
			len_opacities = 0;

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
		 * Marker Colors and opacities
		 */
		if(dsp->marker_colors != NULL && !dsp->mono_marker_color){
			markercolortable = (NhlColorIndex*)
					dsp->marker_colors->data;
			len_markercolortable =
					dsp->marker_colors->len_dimensions[0];
		}
		else
			len_markercolortable = 0;

		if(dsp->marker_opacities != NULL){
			markeropacities = (float*)dsp->marker_opacities->data;
			len_markeropacities = dsp->marker_opacities->len_dimensions[0];
		}
		else
			len_markeropacities = 0;


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
			markerthicktable =
				(float*)dsp->marker_thicknesses->data;
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

			if(j < len_opacities)
				line_opacities[index] = opacities[j];
			else
				line_opacities[index] = dsp->line_opacity;

			dash_seg_lens[index] = 
				dsp->dash_seg_len * xlp->vp_average;

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
				else if(j < len_labeltable)
					label = labeltable[j];
				else{
					sprintf(buffer,"%s%d",
							dataspec->base.name,
									j+1);
					label = buffer;
				}
			}
			if(label) {

				lg_label_strings[index] = NhlMalloc
					(sizeof(char)* (strlen(label)+1));
				if(!lg_label_strings[index]){
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return NhlFATAL;
				}
				strcpy(lg_label_strings[index],label);
			} else {
				lg_label_strings[index] = NULL;
			}

			llabel_fheights[index] = 
				dsp->llabel_fheight * xlp->vp_average;

			if(j < len_linethicktable)
				line_thicknesses[index] = linethicktable[j];
			else
				line_thicknesses[index] = dsp->line_thickness;

			if(j < len_markercolortable)
				marker_colors[index] = markercolortable[j];
			else
				marker_colors[index] = dsp->marker_color;
	
			if(j < len_markeropacities)
				marker_opacities[index] = markeropacities[j];
			else
				marker_opacities[index] = dsp->marker_opacity;

			if(j < len_markertable)
				marker_indexes[index] = markertable[j];
			else
				marker_indexes[index] = dsp->marker;

			if(j < len_markersizetable)
				marker_sizes[index] = 
					markersizetable[j] * xlp->vp_average;
			else 
				marker_sizes[index] = 
					dsp->marker_size  * xlp->vp_average;

			if(j < len_markerthicktable)
				marker_thicknesses[index] =markerthicktable[j];
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
			llabel_fonts[index] = dsp->llabel_font;
			llabel_faspects[index] = dsp->llabel_faspect;
			llabel_fthicknesses[index] = dsp->llabel_fthickness;
			llabel_fqualities[index] = dsp->llabel_fquality;
			llabel_cspacings[index] = dsp->llabel_cspacing;
			llabel_func_codes[index] = dsp->llabel_func_code;
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
	NhlXyPlotLayer	xlayer,
	NhlLayer	thetrans,
	NhlDrawOrder	order,
	NhlString	func
)
#else
(xlayer,thetrans,order,func)
	NhlXyPlotLayer	xlayer;
	NhlLayer	thetrans;
	NhlDrawOrder	order;
	NhlString	func;
#endif
{
	int			i,j;
	NhlErrorTypes		ret = NhlNOERROR;
	NhlErrorTypes		ret1 = NhlNOERROR;
	int			upflag = 1;
	NhlTransformLayerPart	*tfp = &xlayer->trans;
	NhlXyPlotLayerPart	*xlp = &xlayer->xyplot;
	int			*dash_indexes = xlp->dash_indexes->data;
	int			*item_types = xlp->item_types->data;
	/*NhlString		*lg_label_strings = xlp->lg_label_strings->data;*/
	float			*dash_seg_lens = xlp->dash_seg_lens->data;
	int			*line_colors = xlp->line_colors->data;
        float                   *line_opacities = xlp->line_opacities->data;
	int			*llabel_colors = xlp->llabel_colors->data;
	NhlString		*llabel_strings = xlp->llabel_strings->data;
	float			*llabel_fheights = xlp->llabel_fheights->data;
	float			*line_thicknesses = xlp->line_thicknesses->data;
	int			*marker_colors = xlp->marker_colors->data;
        float                   *marker_opacities = xlp->marker_opacities->data;
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
	NhlFont			*llabel_fonts = xlp->llabel_fonts->data;
	float			*llabel_faspects = xlp->llabel_faspects->data;
	float		*llabel_fthicknesses = xlp->llabel_fthicknesses->data;
	NhlFontQuality	*llabel_fqualities = xlp->llabel_fqualities->data;
	float		*llabel_cspacings = xlp->llabel_cspacings->data;
	char		*llabel_func_codes = xlp->llabel_func_codes->data;
	float			*tx,*ty;
	int			size;
        Gint		        err_ind;
        Gclip           	clip_ind_rect;

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

	ginq_clip(&err_ind,&clip_ind_rect);
        gset_clip_ind(xlayer->view.clip_on ? GIND_CLIP : GIND_NO_CLIP);


	for(i=0;i < xlp->num_cpairs;i++){
		float		*xvect;
		float		*yvect;

		NhlVASetValues(xlayer->base.wkptr->base.id,
			_NhlNwkLineDashSegLenF,dash_seg_lens[i],
			_NhlNwkLineLabelFontHeightF,llabel_fheights[i],
			_NhlNwkDashPattern,	dash_indexes[i],
			_NhlNwkLineColor,	line_colors[i],
                        _NhlNwkLineOpacityF,    line_opacities[i],
			_NhlNwkLineLabelFontColor,	llabel_colors[i],
			_NhlNwkLineLabel,	llabel_strings[i],
			_NhlNwkLineThicknessF,	line_thicknesses[i],
			_NhlNwkLineLabelFont,	llabel_fonts[i],
			_NhlNwkLineLabelFontAspectF, llabel_faspects[i],
			_NhlNwkLineLabelFontThicknessF,llabel_fthicknesses[i], 
			_NhlNwkLineLabelFontQuality, llabel_fqualities[i],
			_NhlNwkLineLabelConstantSpacingF, llabel_cspacings[i],
			_NhlNwkLineLabelFuncCode, llabel_func_codes[i], 
			_NhlNwkMarkerColor,	marker_colors[i],
                        _NhlNwkMarkerOpacityF,  marker_opacities[i],
			_NhlNwkMarkerIndex,	marker_indexes[i],
			_NhlNwkMarkerSizeF,	marker_sizes[i],
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

			if((missing_set[i] & XMISS_SET) &&
			   (missing_set[i] & YMISS_SET)) {

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
			c_plotif(0.,0.,2);	
		}

		if(item_types[i] != NhlLINES){
			float	oor;
			float	*xmiss = NULL;
			float	*ymiss = NULL;
			int status;
			int xin = 0, yin = 0;

			if(missing_set[i] & XMISS_SET) {
				xmiss = &xmissing[i];
				if (*xmiss >= tfp->x_min 
				    && *xmiss <= tfp->x_max)
					xin = 1;
			}
			if(missing_set[i] & YMISS_SET) {
				ymiss = &ymissing[i];
				if (*ymiss >= tfp->y_min
				    && *ymiss <= tfp->y_max)
					yin = 1;
			}

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

			if (xin && yin) {
				for(j=0;j<len_vectors[i];j++){
					if((xvect[j] == *xmiss) ||
						(yvect[j] == *ymiss))
						continue;
					_NhlWorkstationMarker(
						xlayer->base.wkptr,
						&tx[j],&ty[j],1);
				}
			}
			else if (xin) {
				for(j=0;j<len_vectors[i];j++){
					if(xvect[j] == *xmiss)
						continue;
					_NhlWorkstationMarker(
						xlayer->base.wkptr,
						&tx[j],&ty[j],1);
				}
			}
			else if (yin) {
				for(j=0;j<len_vectors[i];j++){
					if(yvect[j] == *ymiss)
						continue;
					_NhlWorkstationMarker(
						xlayer->base.wkptr,
						&tx[j],&ty[j],1);
				}
			}
			else if(status){
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


	NhlFree(tx);
	NhlFree(ty);

	gset_clip_ind(clip_ind_rect.clip_ind);

	return ret1;
}


/*
 * Function:	xyUpdateTrans
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes xyUpdateTrans
#if	NhlNeedProto
(
	NhlXyPlotLayer	xyl,
	NhlLayer	*thetrans,
	NhlString	entry_name
)
#else
(xyl,thetrans,entry_name)
        NhlXyPlotLayer	xyl;
	NhlLayer	*thetrans;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlTransformLayerPart	*tfp = &(xyl->trans);

/*
 * If the plot is an overlay member, use the overlay manager's trans object.
 */
	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    ! tfp->do_ndc_overlay &&
	    tfp->overlay_trans_obj != NULL) {
		*thetrans = tfp->overlay_trans_obj;
                if (((*thetrans)->base.layer_class)->base_class.class_name
		    == NhlmapTransObjClass->base_class.class_name) {
			subret = NhlVASetValues
				((*thetrans)->base.id,
				 NhlNtrDataXStartF,tfp->data_xstart,
				 NhlNtrDataXEndF,tfp->data_xend,
				 NULL);

			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				return(ret);
			}
                }
		
	}
	else {
		*thetrans = tfp->trans_obj;
		if (tfp->overlay_status == _tfNotInOverlay ||
		    tfp->do_ndc_overlay) {
			subret = _NhlSetTrans(*thetrans,(NhlLayer)xyl);
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
 * Function:	XyPlotPreDraw
 *
 * Description:	
 *
 * In Args:	layer	XyPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes XyPlotPreDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString		e_text,entry_name = "XyPlotPreDraw";
	NhlXyPlotLayer		xyl = (NhlXyPlotLayer) layer;
	NhlXyPlotLayerPart	*xyp = &xyl->xyplot;
	NhlLayer		thetrans;

	if((!xyp->data_ranges_set) || (xyp->thetrans == NULL)){
		return NhlNOERROR;
	}
	if (xyp->curve_order != NhlPREDRAW) {
		return NhlNOERROR;
	}

	subret = xyUpdateTrans(xyl,&thetrans,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		return ret;
	}
	if (xyl->view.use_segments && ! xyp->new_draw_req &&
	    xyp->predraw_dat && xyp->predraw_dat->id != NgNOT_A_SEGMENT) {
		ret = _NhltfDrawSegment((NhlLayer)xyl,thetrans,
					xyp->predraw_dat,entry_name);
		return ret;
	}

	subret = _NhlActivateWorkstation(xyl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (xyl->view.use_segments) {
		subret = _NhltfInitSegment((NhlLayer)xyl,thetrans,
					    &xyp->predraw_dat,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			return ret;
		}
	}

	subret = DrawCurves(xyl,thetrans,NhlPREDRAW,entry_name);
	ret = MIN(ret,subret);

	if (xyl->view.use_segments) {
		_NhlEndSegment(xyp->predraw_dat);
	}

	subret = _NhlDeactivateWorkstation(xyl->base.wkptr);	

	return MIN(subret,ret);
}

/*
 * Function:	XyPlotDraw
 *
 * Description:	
 *
 * In Args:	layer	XyPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes XyPlotDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlXyPlotLayer		xyl = (NhlXyPlotLayer) layer;
	NhlXyPlotLayerPart	*xyp = &xyl->xyplot;
	NhlString		e_text,entry_name = "XyPlotDraw";
	NhlLayer		thetrans;


	if((!xyp->data_ranges_set) || (xyp->thetrans == NULL)){
		return NhlNOERROR;
	}
	if (xyp->curve_order != NhlDRAW) {
		return NhlNOERROR;
	}

	subret = xyUpdateTrans(xyl,&thetrans,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		return ret;
	}

	if (xyl->view.use_segments && ! xyp->new_draw_req &&
	    xyp->draw_dat && xyp->draw_dat->id != NgNOT_A_SEGMENT) {
		ret = _NhltfDrawSegment((NhlLayer)xyl,thetrans,
					xyp->draw_dat,entry_name);
		return ret;
	}

	subret = _NhlActivateWorkstation(xyl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (xyl->view.use_segments) {
		subret = _NhltfInitSegment((NhlLayer)xyl,thetrans,
					    &xyp->draw_dat,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			return ret;
		}
	}

	subret = DrawCurves((NhlXyPlotLayer)layer,
			    thetrans,NhlDRAW,entry_name);
	ret = MIN(ret,subret);

	if (xyl->view.use_segments) {
		_NhlEndSegment(xyp->draw_dat);
	}

	subret = _NhlDeactivateWorkstation(xyl->base.wkptr);	
	return MIN(subret,ret);
}

/*
 * Function:	XyPlotPostDraw
 *
 * Description:	
 *
 * In Args:	layer	XyPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes XyPlotPostDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlXyPlotLayer		xyl = (NhlXyPlotLayer) layer;
	NhlXyPlotLayerPart	*xyp = &xyl->xyplot;
	NhlString		e_text,entry_name = "XyPostPlotDraw";
	NhlLayer		thetrans;

	if((!xyp->data_ranges_set) || (xyp->thetrans == NULL)){
		return NhlNOERROR;
	}
	if (xyp->curve_order == NhlPOSTDRAW) {
		subret = xyUpdateTrans(xyl,&thetrans,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			return ret;
		}
		if (xyl->view.use_segments && ! xyp->new_draw_req &&
		    xyp->postdraw_dat && 
		    xyp->postdraw_dat->id != NgNOT_A_SEGMENT) {
			ret = _NhltfDrawSegment((NhlLayer)xyl,thetrans,
						xyp->postdraw_dat,entry_name);
			return ret;
		}
	
		subret = _NhlActivateWorkstation(xyl->base.wkptr);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: Error activating workstation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		if (xyl->view.use_segments) {
			subret = _NhltfInitSegment((NhlLayer)xyl,thetrans,
					       &xyp->postdraw_dat,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				return ret;
			}
		}
		subret = DrawCurves((NhlXyPlotLayer)layer,thetrans,
				    NhlPOSTDRAW,entry_name);
		ret = MIN(ret,subret);

		if (xyl->view.use_segments) {
			_NhlEndSegment(xyp->postdraw_dat);
		}

		subret = _NhlDeactivateWorkstation(xyl->base.wkptr);	
	}

	xyp->new_draw_req = False;

	return MIN(subret,ret);
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
        NhlFreeGenArray(xp->line_opacities);
	NhlFreeGenArray(xp->dash_seg_lens);
	NhlFreeGenArray(xp->llabel_colors);
	NhlFreeGenArray(xp->llabel_strings);
	NhlFreeGenArray(xp->llabel_fheights);
	NhlFreeGenArray(xp->llabel_faspects);
	NhlFreeGenArray(xp->llabel_fthicknesses);
	NhlFreeGenArray(xp->llabel_fqualities);
	NhlFreeGenArray(xp->llabel_cspacings);
	NhlFreeGenArray(xp->llabel_func_codes);
	NhlFreeGenArray(xp->llabel_fonts);
	NhlFreeGenArray(xp->line_thicknesses);
	NhlFreeGenArray(xp->marker_colors);
        NhlFreeGenArray(xp->marker_opacities);
	NhlFreeGenArray(xp->marker_indexes);
	NhlFreeGenArray(xp->marker_sizes);
	NhlFreeGenArray(xp->marker_thicknesses);
	NhlFreeGenArray(xp->xvectors);
	NhlFreeGenArray(xp->yvectors);
	NhlFreeGenArray(xp->len_vectors);
	NhlFreeGenArray(xp->missing_set);
	NhlFreeGenArray(xp->xmissing);
	NhlFreeGenArray(xp->ymissing);

	if (xp->predraw_dat != NULL)
		_NhlDeleteViewSegment(inst,xp->predraw_dat);
	if (xp->draw_dat != NULL)
		_NhlDeleteViewSegment(inst,xp->draw_dat);
	if (xp->postdraw_dat != NULL)
		_NhlDeleteViewSegment(inst,xp->postdraw_dat);

	return(ret1);
}


/*
 * Function:	XyDataDestroy
 *
 * Description:	
 *
 * In Args:	inst		instance record pointer
 *
 * Out Args:	NhlNONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NhlNONE
 */
static NhlErrorTypes XyDataDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlXyDataSpecLayer	xdinst = (NhlXyDataSpecLayer)inst;
	NhlXyDataSpecLayerPart	*xdp = &xdinst->xydata;
	NhlErrorTypes		ret1 = NhlNOERROR;

	NhlFreeGenArray(xdp->dashes);
	NhlFreeGenArray(xdp->marker_modes);
	NhlFreeGenArray(xdp->lg_label_strings);
	NhlFreeGenArray(xdp->colors);
	NhlFreeGenArray(xdp->label_colors);
	NhlFreeGenArray(xdp->labels);
	NhlFreeGenArray(xdp->line_thicknesses);
	NhlFreeGenArray(xdp->marker_colors);
	NhlFreeGenArray(xdp->markers);
	NhlFreeGenArray(xdp->marker_sizes);
	NhlFreeGenArray(xdp->marker_thicknesses);

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

/*
 * a data change forces a new draw
 */
	xl->xyplot.new_draw_req = True;

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

	if(xl->xyplot.check_ranges || xl->xyplot.update_req){
		NhlSetSArg(&sargs[nsargs++],NhlNpmUpdateReq,True);
		xl->xyplot.check_ranges = False;
		xl->xyplot.update_req = False;
	}


	ret2 = _NhlManageOverlay(&xl->xyplot.overlay,(NhlLayer)xl,
		(NhlLayer)xlold,_NhlUPDATEDATA,sargs,nsargs,"XyPlotUpdateData");
	if(ret2 < NhlWARNING)
		return ret2;
	ret1 = MIN(ret1,ret2);

	ret2 = XyResetExtents(xl);
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
			NhlPError(NhlINFO,NhlEUNKNOWN,
			"%s:Setting %s to False because %s was specified",
						error_lead,comp_res,extent_res);
			ret = NhlINFO;
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
	NhlXyPlotLayerPart	*nxp = &xnew->xyplot;
	NhlTransformLayerPart	*tfp = &xnew->trans;
	NhlTransformLayerPart	*otfp = NULL;

	if(calledfrom == _NhlCREATE)
		error_lead = "XyPlotInitialize";
	else {
                otfp = &xold->trans;
		error_lead = "XyPlotSetValues";
        }

	/*
	 * take care of style resources
	 */

	if (nxp->x_style_set) {
		switch (nxp->x_style) {
		case NhlGEOGRAPHIC:
		case NhlTIME:
		case NhlLINEAR:
			nxp->x_style = NhlLINEAR;
			tfp->x_log = False;
			tfp->x_axis_type = NhlLINEARAXIS;
			break;
		case NhlLOG:
			tfp->x_log = True;
			tfp->x_axis_type = NhlLOGAXIS;
			break;
		case NhlIRREGULAR:
			tfp->x_log = False;
			tfp->x_axis_type = NhlIRREGULARAXIS;
			break;
		}
	}
	else if (tfp->x_axis_type_set) {
		switch (tfp->x_axis_type) {
		case NhlLINEARAXIS:
			nxp->x_style = NhlLINEAR;
			tfp->x_log = False;
			break;
		case NhlLOGAXIS:
			nxp->x_style = NhlLOG;
			tfp->x_log = True;
			break;
		case NhlIRREGULARAXIS:
			nxp->x_style = NhlIRREGULAR;
			tfp->x_log = False;
			break;
		}
	}
	else if (tfp->x_log_set) {
		switch (tfp->x_log) {
		case True:
			nxp->x_style = NhlLOG;
			tfp->x_axis_type = NhlLOGAXIS;
			break;
		case False:
			nxp->x_style = NhlLINEAR;
			tfp->x_axis_type = NhlLINEARAXIS;
			break;
		}
	}
	else if (calledfrom == _NhlCREATE) {
		nxp->x_style = NhlLINEAR;
	}
	if (nxp->y_style_set) {
		switch (nxp->y_style) {
		case NhlGEOGRAPHIC:
		case NhlTIME:
		case NhlLINEAR:
			nxp->y_style = NhlLINEAR;
			tfp->y_log = False;
			tfp->y_axis_type = NhlLINEARAXIS;
			break;
		case NhlLOG:
			tfp->y_log = True;
			tfp->y_axis_type = NhlLOGAXIS;
			break;
		case NhlIRREGULAR:
			tfp->y_log = False;
			tfp->y_axis_type = NhlIRREGULARAXIS;
			break;
		}
	}
	else if (tfp->y_axis_type_set) {
		switch (tfp->y_axis_type) {
		case NhlLINEARAXIS:
			nxp->y_style = NhlLINEAR;
			tfp->y_log = False;
			break;
		case NhlLOGAXIS:
			nxp->y_style = NhlLOG;
			tfp->y_log = True;
			break;
		case NhlIRREGULARAXIS:
			nxp->y_style = NhlIRREGULAR;
			tfp->y_log = False;
			break;
		}
	}
	else if (tfp->y_log_set) {
		switch (tfp->y_log) {
		case True:
			nxp->y_style = NhlLOG;
			tfp->y_axis_type = NhlLOGAXIS;
			break;
		case False:
			nxp->y_style = NhlLINEAR;
			tfp->y_axis_type = NhlLINEARAXIS;
			break;
		}
	}
	else if (calledfrom == _NhlCREATE) {
		nxp->y_style = NhlLINEAR;
	}

	nxp->x_style_set = tfp->x_axis_type_set = tfp->x_log_set = False;
	nxp->y_style_set = tfp->y_axis_type_set = tfp->y_log_set = False;
	  
	if((nxp->x_style == NhlIRREGULAR) &&
				(nxp->x_irregular_points == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: cannot be NhlIRREGULAR unless %s is set:setting %s to NhlLINEAR",
			NhlNxyXStyle,NhlNxyXIrregularPoints,NhlNxyXStyle);

		nxp->x_style = NhlLINEAR;
		tfp->x_axis_type = NhlLINEARAXIS;
		ret = MIN(ret,NhlWARNING);
	}
	if((nxp->y_style == NhlIRREGULAR) &&
				(nxp->y_irregular_points == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: cannot be NhlIRREGULAR unless %s is set:setting %s to NhlLINEAR",
			NhlNxyYStyle,NhlNxyYIrregularPoints,NhlNxyYStyle);

		nxp->y_style = NhlLINEAR;
		tfp->y_axis_type = NhlLINEARAXIS;
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
	if(nxp->x_alternate != NhlNONE){
		nxp->x_alternate = NhlNONE;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s only supports a value of NhlNONE at this time",
						error_lead,NhlNxyXAlternate);
		ret = MIN(ret,NhlWARNING);
	}
	if(nxp->y_alternate != NhlNONE){
		nxp->y_alternate = NhlNONE;
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
		lret = CheckExtent(tfp->x_min_set,
			nxp->comp_x_min_set,&nxp->compute_x_min,
			NhlNxyComputeXMin,NhlNtrXMinF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent(tfp->x_max_set,
			nxp->comp_x_max_set,&nxp->compute_x_max,
			NhlNxyComputeXMax,NhlNtrXMaxF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent(tfp->y_max_set,
			nxp->comp_y_max_set,&nxp->compute_y_max,
			NhlNxyComputeYMax,NhlNtrYMaxF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent(tfp->y_min_set,
			nxp->comp_y_min_set,&nxp->compute_y_min,
			NhlNxyComputeYMin,NhlNtrYMinF,error_lead);
		ret = MIN(lret,ret);
	}
	else{
		lret = CheckExtent((otfp->x_min!=tfp->x_min),
			nxp->compute_x_min,&nxp->compute_x_min,
			NhlNxyComputeXMin,NhlNtrXMinF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent((otfp->x_max!=tfp->x_max),
			nxp->compute_x_max,&nxp->compute_x_max,
			NhlNxyComputeXMax,NhlNtrXMaxF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent((otfp->y_max!=tfp->y_max),
			nxp->compute_y_max,&nxp->compute_y_max,
			NhlNxyComputeYMax,NhlNtrYMaxF,error_lead);
		ret = MIN(lret,ret);

		lret=CheckExtent((otfp->y_min!=tfp->y_min),
			nxp->compute_y_min,&nxp->compute_y_min,
			NhlNxyComputeYMin,NhlNtrYMinF,error_lead);
		ret = MIN(lret,ret);

	}

	if(!nxp->compute_x_min && tfp->x_min_set &&
		(nxp->x_style == NhlLOG) && (tfp->x_min <= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is NhlLOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNxyXStyle,NhlNtrXMinF,NhlNxyComputeXMin);

		nxp->compute_x_min = True;
		ret = MIN(ret,NhlWARNING);
	}

	if(!nxp->compute_x_max && tfp->x_max_set &&
		(nxp->x_style == NhlLOG) && (tfp->x_max <= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is NhlLOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNxyXStyle,NhlNtrXMaxF,NhlNxyComputeXMax);

		nxp->compute_x_max = True;
		ret = MIN(ret,NhlWARNING);
	}

	if(!nxp->compute_x_min && tfp->x_min_set &&
		!nxp->compute_x_max && tfp->x_max_set &&
		(tfp->x_max < tfp->x_min)){

		float tfloat;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:%s is < %s: Swapping",
					error_lead,NhlNtrXMaxF,NhlNtrXMinF);
		tfloat = tfp->x_max;
		tfp->x_max = tfp->x_min;
		tfp->x_min = tfloat;
	}

	if(!nxp->compute_y_min && tfp->y_min_set &&
		(nxp->y_style == NhlLOG) && (tfp->y_min <= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is NhlLOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNxyYStyle,NhlNtrYMinF,NhlNxyComputeYMin);

		nxp->compute_y_min = True;
		ret = MIN(ret,NhlWARNING);
	}

	if(!nxp->compute_y_max && tfp->y_max_set &&
		(nxp->y_style == NhlLOG) && (tfp->y_max <= 0)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is NhlLOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNxyYStyle,NhlNtrYMaxF,NhlNxyComputeYMax);

		nxp->compute_y_max = True;
		ret = MIN(ret,NhlWARNING);
	}

	if(!nxp->compute_y_min && tfp->y_min_set &&
		!nxp->compute_y_max && tfp->y_max_set &&
		(tfp->y_max < tfp->y_min)){

		float tfloat;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:%s is < %s: Swapping",
					error_lead,NhlNtrYMaxF,NhlNtrYMinF);
		tfloat = tfp->y_max;
		tfp->y_max = tfp->y_min;
		tfp->y_min = tfloat;
	}

	if((calledfrom == _NhlSETVALUES) &&
		((xold->xyplot.x_style != nxp->x_style) ||
		(xold->xyplot.y_style != nxp->y_style))){
		nxp->check_ranges = True;
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

		xnew->trans.data_xstart = xnew->xyplot.x_data_min;
		xnew->trans.data_xend = xnew->xyplot.x_data_max;
		xnew->trans.data_ystart = xnew->xyplot.y_data_min;
		xnew->trans.data_yend = xnew->xyplot.y_data_max;
	}

	if(xnew->xyplot.check_ranges){
		NhlBoolean xmin_set, xmax_set, ymin_set, ymax_set;

		xmin_set = xmax_set = ymin_set = ymax_set = False;

		/*
		 * Set Initial default for left,right,top,bottom
		 * (should only happen if user didn't set it themself, and it
		 * will only be used if the compute resources are False.)
		 *
		 * Also set if compute resources are true.
		 */
		if(!xnew->trans.x_min_set || xnew->xyplot.compute_x_min){
			xnew->trans.x_min = xnew->xyplot.x_data_min;
			xnew->trans.x_min_set = True;
			xmin_set = True;
		}
		if(!xnew->trans.x_max_set || xnew->xyplot.compute_x_max){
			xnew->trans.x_max = xnew->xyplot.x_data_max;
			xnew->trans.x_max_set = True;
			xmax_set = True;
		}
		if(!xnew->trans.y_min_set || xnew->xyplot.compute_y_min){
			xnew->trans.y_min = xnew->xyplot.y_data_min;
			xnew->trans.y_min_set = True;
			ymin_set = True;
		}
		if(!xnew->trans.y_max_set || xnew->xyplot.compute_y_max){
			xnew->trans.y_max = xnew->xyplot.y_data_max;
			xnew->trans.y_max_set = True;
			ymax_set = True;
		}
		if (xmin_set && xmax_set) {
			if (_NhlCmpFAny2(xnew->trans.x_min,xnew->trans.x_max,
					 5,1e-32) == 0) {
				if (fabs(xnew->trans.x_min) > 1e-32) {
					xnew->trans.x_min -= 0.0001 *
						fabs(xnew->trans.x_min);
					xnew->trans.x_max += 0.0001 *
						fabs(xnew->trans.x_max);
				}
				else {
					xnew->trans.x_min = -1;
					xnew->trans.x_max = 1;
				}
			}
		}
		if (ymin_set && ymax_set) {
			if (_NhlCmpFAny2(xnew->trans.y_min,xnew->trans.y_max,
					 5,1e-32) == 0) {
				if (fabs(xnew->trans.y_min) > 1e-32) {
					xnew->trans.y_min -= 0.0001 *
						fabs(xnew->trans.y_min);
					xnew->trans.y_max += 0.0001 *
						fabs(xnew->trans.y_max);
				}
				else {
					xnew->trans.y_min = -1;
					xnew->trans.y_max = 1;
				}
			}
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
			if(xnew->trans.x_min < xnew->xyplot.x_irreg_min){

				if(!xnew->xyplot.compute_x_min){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNtrXMinF,
						NhlNxyXIrregularPoints,
						xnew->xyplot.x_irreg_min);
					ret = MIN(ret,NhlWARNING);
				}
				xnew->trans.x_min = xnew->xyplot.x_irreg_min;
			}
			if(xnew->trans.x_max > xnew->xyplot.x_irreg_max){

				if(!xnew->xyplot.compute_x_max){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNtrXMaxF,
						NhlNxyXIrregularPoints,
						xnew->xyplot.x_irreg_max);
					ret = MIN(ret,NhlWARNING);
				}
				xnew->trans.x_max = xnew->xyplot.x_irreg_max;
			}
		}
		if(xnew->xyplot.y_style == NhlIRREGULAR){
			if(xnew->trans.y_min < xnew->xyplot.y_irreg_min){

				if(!xnew->xyplot.compute_y_min){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNtrYMinF,
						NhlNxyYIrregularPoints,
						xnew->xyplot.y_irreg_min);
					ret = MIN(ret,NhlWARNING);
				}
				xnew->trans.y_min = xnew->xyplot.y_irreg_min;
			}
			if(xnew->trans.y_max > xnew->xyplot.y_irreg_max){

				if(!xnew->xyplot.compute_y_max){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNtrYMaxF,
						NhlNxyYIrregularPoints,
						xnew->xyplot.y_irreg_max);
					ret = MIN(ret,NhlWARNING);
				}
				xnew->trans.y_max = xnew->xyplot.y_irreg_max;
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
 *		IrregularTransObjs have to be freed whenever the 
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
	char		*error_lead=NULL;
	NhlClass	trans_class = NULL;
	NhlGenArray	gen;
	NhlXyPlotLayerPart	*newxy = &xnew->xyplot;
	NhlXyPlotLayerPart	*oldxy=NULL;
	NhlTransformLayerPart	*tfp = &xnew->trans;
	NhlTransformLayerPart	*otfp = NULL;

/*
 * Now create main transformation object
 */	
	if(calledfrom == _NhlCREATE){
		error_lead = "XyPlotInitialize";
	}
	else{
		oldxy = &xold->xyplot;
                otfp = &xold->trans;

		if(calledfrom == _NhlSETVALUES){
			error_lead = "XyPlotSetValues";
		}
		else if (calledfrom == _NhlUPDATEDATA){
		/*
		 * If we are coming from UpdateData - The only resources that
		 * could have changed are min and max - if they haven't changed
		 * return immediately.
		 */
			if((tfp->x_min == otfp->x_min) &&
				(tfp->x_max == otfp->x_max) &&
				(tfp->y_min == otfp->y_min) &&
				(tfp->y_max == otfp->y_max)){
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
#if 0
		newxy->fake_x = newxy->fake_y = False;
#endif
		if(newxy->y_style == NhlIRREGULAR){

			trans_class = NhlirregularTransObjClass;
			newxy->have_irreg_trans = True;

			gen = newxy->y_irregular_points;
			NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,gen);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrYAxisType,NhlIRREGULARAXIS);
			NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,
							newxy->y_tension);

			if(newxy->x_style == NhlIRREGULAR){

				gen = newxy->x_irregular_points;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								gen);
				NhlSetSArg(&sargs[nargs++],
					   NhlNtrXAxisType,NhlIRREGULARAXIS);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newxy->x_tension);
			}
			else if (newxy->x_style == NhlLOG){
				NhlSetSArg(&sargs[nargs++],
					   NhlNtrXAxisType,NhlLOGAXIS);
			}
			else if (newxy->x_style == NhlLINEAR){
				NhlSetSArg(&sargs[nargs++],
					   NhlNtrXAxisType,NhlLINEARAXIS);
			}
		}
		/*
		 * Y is not IRREG
		 */
		else{
			if(newxy->x_style == NhlIRREGULAR){

				trans_class = NhlirregularTransObjClass;
				newxy->have_irreg_trans = True;

				gen = newxy->x_irregular_points;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								gen);
				NhlSetSArg(&sargs[nargs++],
					   NhlNtrXAxisType,NhlIRREGULARAXIS);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newxy->x_tension);

				if(newxy->y_style == NhlLINEAR){
					NhlSetSArg(&sargs[nargs++],
						   NhlNtrYAxisType,
						   NhlLINEARAXIS);
				}
				else if(newxy->y_style == NhlLOG){
					NhlSetSArg(&sargs[nargs++],
						   NhlNtrYAxisType,
						   NhlLOGAXIS);
				}
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
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);

		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,tfp->x_reverse);
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,tfp->y_reverse);

		NhlSetSArg(&sargs[nargs++],NhlNtrLineInterpolationOn,
			   tfp->line_interpolation_on);

		xnew->trans.grid_type = newxy->have_irreg_trans ?
			NhltrIRREGULAR : NhltrLOGLIN;

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
		newxy->new_draw_req = True;

		return NhlNOERROR;
	}

	/*
	 * SetValues/UpdateData in existing trans object
	 */

	/*
	 * if we are tricking an irreg object into being a log or lin - take
	 * care of setting the transformation.
	 */
	if (newxy->have_irreg_trans &&
	   newxy->x_style != oldxy->x_style){
		if(newxy->x_style == NhlIRREGULAR){
			NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
				   newxy->x_irregular_points);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrXAxisType,NhlIRREGULARAXIS);
			NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
				   newxy->x_tension);
		}
		else if (newxy->x_style == NhlLOG)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrXAxisType,NhlLOGAXIS);
		else if (newxy->x_style == NhlLINEAR)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrXAxisType,NhlLINEARAXIS);
	}
	else if (newxy->x_style != oldxy->x_style) {
		if (newxy->x_style == NhlLOG)
			NhlSetSArg(&sargs[nargs++],NhlNtrXLog,True);
		else
			NhlSetSArg(&sargs[nargs++],NhlNtrXLog,False);
	}

	if (newxy->have_irreg_trans &&
	    newxy->y_style != oldxy->y_style){
		if(newxy->y_style == NhlIRREGULAR){
			NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
				   newxy->y_irregular_points);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrYAxisType,NhlIRREGULARAXIS);
			NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,
				   newxy->y_tension);
		}
		else if (newxy->y_style == NhlLOG)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrYAxisType,NhlLOGAXIS);
		else if (newxy->y_style == NhlLINEAR)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrYAxisType,NhlLINEARAXIS);
	}
	else if (newxy->y_style != oldxy->y_style) {
		if (newxy->y_style == NhlLOG)
			NhlSetSArg(&sargs[nargs++],NhlNtrYLog,True);
		else
			NhlSetSArg(&sargs[nargs++],NhlNtrYLog,False);
	}
		
	if(tfp->x_min != otfp->x_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
	if(tfp->x_max != otfp->x_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
	if(tfp->y_min != otfp->y_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
	if(tfp->y_max != otfp->y_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);

	if(tfp->x_reverse != otfp->x_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,tfp->x_reverse);
	if(tfp->y_reverse != otfp->y_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,tfp->y_reverse);
	if(tfp->line_interpolation_on != otfp->line_interpolation_on)
		NhlSetSArg(&sargs[nargs++],NhlNtrLineInterpolationOn,
			   tfp->line_interpolation_on);

	if (nargs > 0)	
		newxy->new_draw_req = True;

	xnew->trans.grid_type = newxy->have_irreg_trans ?
			NhltrIRREGULAR : NhltrLOGLIN;
		
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

 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (nxp->display_tickmarks == NhlNOCREATE) {
                if (calledfrom == _NhlCREATE ||
                    oxp->display_tickmarks == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        nxp->display_tickmarks = NhlNEVER;
        }

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

 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (nxp->display_titles == NhlNOCREATE) {
                if (calledfrom == _NhlCREATE ||
                    oxp->display_titles == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        nxp->display_titles = NhlNEVER;
        }

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
	NhlFont			*llabel_fonts = nxp->llabel_fonts->data;
	float			*llabel_faspects = nxp->llabel_faspects->data;
	float		*llabel_fthicknesses = nxp->llabel_fthicknesses->data;
	NhlFontQuality	*llabel_fqualities = nxp->llabel_fqualities->data;
	float		*llabel_cspacings = nxp->llabel_cspacings->data;
	char		*llabel_func_codes = nxp->llabel_func_codes->data;

	if(calledfrom == _NhlUPDATEDATA)
		return NhlNOERROR;

	if(calledfrom == _NhlSETVALUES)
		oxp = &xold->xyplot;

 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (nxp->display_legend == NhlNOCREATE) {
                if (calledfrom == _NhlCREATE ||
                    oxp->display_legend == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        nxp->display_legend = NhlNEVER;
        }

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
	/* 
	 * Unfortunately Legend doesn't yet support array resources for
	 * these line label attributes (yet)
	 */
	NhlSetSArg(&sargs[(*nargs)++],NhlNlgLineLabelFont,llabel_fonts[0]);
	NhlSetSArg(&sargs[(*nargs)++],
		   NhlNlgLineLabelFontAspectF,llabel_faspects[0]);
	NhlSetSArg(&sargs[(*nargs)++],
		   NhlNlgLineLabelFontThicknessF,llabel_fthicknesses[0]);
	NhlSetSArg(&sargs[(*nargs)++],
		   NhlNlgLineLabelFontQuality,llabel_fqualities[0]);
	NhlSetSArg(&sargs[(*nargs)++],
		   NhlNlgLineLabelConstantSpacingF,llabel_cspacings[0]);
	NhlSetSArg(&sargs[(*nargs)++],
		   NhlNlgLineLabelFuncCode,llabel_func_codes[0]);

		   
	return NhlNOERROR;
}
