/*
 *      $Id: LabelBar.c,v 1.22 1994-12-23 01:21:02 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LabelBar.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	
 *		
 *		Creates and manages a LabelBar
 */

#include <math.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/LabelBarP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/ConvertersP.h>

/* default pattern list */

static int def_patterns[] = { 
	1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
static int def_colors[] = { 
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

/* SUPPRESS 112 */

#define DEFSTRING "NOTHING"
#define DEGTORAD 0.017453293

static NhlResource resources[] = { 

/* Begin-documented-resources */

{NhlNlbLabelBar, NhlClbLabelBar, NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.labelbar_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL}, 
{NhlNlbOrientation, NhlClbOrientation, NhlTOrientation,sizeof(NhlOrientation),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.orient),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlVERTICAL),0,NULL},
{NhlNlbJustification, NhlClbJustification, NhlTJustification, 
	 sizeof(NhlJustification),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.just),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlBOTTOMLEFT),0,NULL},
{NhlNlbBoxMajorExtentF, NhlClbBoxMajorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.box_major_ext),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNlbBoxMinorExtentF, NhlClbBoxMinorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.box_minor_ext),
	 NhlTString,_NhlUSET("0.33"),0,NULL},
{NhlNlbBoxCount, NhlClbBoxCount, NhlTInteger,
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.box_count),
	 NhlTImmediate,_NhlUSET((NhlPointer) 16),0,NULL},
{NhlNlbBoxSizing, NhlClbBoxSizing, NhlTlbBoxSizingMode,
	 sizeof(NhllbBoxSizingMode),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_sizing),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhllbUNIFORMSIZING),0,NULL},

{NhlNlbAutoManage, NhlClbAutoManage, NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.auto_manage),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
{NhlNlbMonoFillColor, NhlClbMonoFillColor, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.mono_fill_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
{NhlNlbMonoFillPattern, NhlClbMonoFillPattern, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.mono_fill_pattern),
	 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
{NhlNlbMonoFillScale, NhlClbMonoFillScale, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.mono_fill_scale),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
{NhlNlbLabelOffsetF, NhlClbLabelOffsetF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.label_off),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNlbTitleOffsetF, NhlClbTitleOffsetF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.title_off),
	 NhlTString,_NhlUSET("0.03"),0,NULL},
{NhlNlbLeftMarginF, NhlClbLeftMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.margin.l),
	 NhlTString,_NhlUSET("0.05"),0,NULL},
{NhlNlbRightMarginF, NhlClbRightMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.margin.r),
	 NhlTString,_NhlUSET("0.05"),0,NULL},
{NhlNlbBottomMarginF, NhlClbBottomMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.margin.b),
	 NhlTString,_NhlUSET("0.05"),0,NULL},
{NhlNlbTopMarginF, NhlClbTopMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.margin.t),
	 NhlTString,_NhlUSET("0.05"),0,NULL},

{NhlNlbFillPatterns, NhlClbFillPatterns, NhlTIntegerGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_patterns),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlbFillColors, NhlClbFillColors, NhlTIntegerGenArray,
	 sizeof(NhlPointer),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_colors),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlbFillScales, NhlClbFillScales, NhlTFloatGenArray,
	 sizeof(NhlPointer),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_scales),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlbLabelStrings, NhlClbLabelStrings, NhlTStringGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_strings),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
{NhlNlbBoxFractions, NhlClbBoxFractions,
	 NhlTFloatGenArray,sizeof(NhlPointer), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_fractions),
	 NhlTImmediate,
	 _NhlUSET((NhlPointer) NULL ),0,(NhlFreeFunc)NhlFreeGenArray},
	
{NhlNlbDrawLabels, NhlClbDrawLabels, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.labels_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) 1),0,NULL},
{NhlNlbLabelPosition, NhlClbLabelPosition, NhlTPosition, 
	 sizeof(NhlPosition), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_pos),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlRIGHT),0,NULL},
{NhlNlbLabelAngleF, NhlClbLabelAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.label_angle),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNlbLabelAlignment, NhlClbLabelAlignment,NhlTlbLabelAlignmentMode, 
	 sizeof(NhllbLabelAlignmentMode), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_alignment),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhllbBOXCENTERS),0,NULL},
{NhlNlbLabelDirection,NhlClbLabelDirection,NhlTTextDirection,
	 sizeof(NhlTextDirection),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_direction),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlACROSS),0,NULL},
{NhlNlbLabelJust, NhlClbLabelJust, NhlTJustification, sizeof(NhlJustification),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_just),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
{NhlNlbLabelFont, NhlCFont, NhlTFont, 
	 sizeof(NhlFont), NhlOffset(NhlLabelBarLayerRec,labelbar.label_font),
	 NhlTImmediate,_NhlUSET((NhlPointer) 1),0,NULL},
{NhlNlbLabelFontColor, NhlClbLabelFontColor, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.label_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlLB_DEF_COLOR),0,NULL},
{NhlNlbLabelFontHeightF, NhlClbLabelFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.label_height),
	 NhlTString,_NhlUSET("0.02"),0,NULL},
{NhlNlbLabelFontAspectF, NhlClbLabelFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.label_aspect),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNlbLabelFontThicknessF, NhlClbLabelFontThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNlbLabelFontQuality, NhlClbLabelFontQuality, NhlTFQuality, 
	 sizeof(NhlFontQuality), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_quality),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
{NhlNlbLabelConstantSpacingF, NhlClbLabelConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_const_spacing),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNlbLabelFuncCode, NhlClbLabelFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(NhlLabelBarLayerRec,labelbar.label_func_code),
	 NhlTString,_NhlUSET(":"),0,NULL},
{NhlNlbLabelStride, NhlClbLabelStride, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.label_stride),
	 NhlTImmediate,_NhlUSET((NhlPointer) 1),0,NULL},

{NhlNlbTitleString, NhlClbTitleString, NhlTString, 
	 sizeof(char *), NhlOffset(NhlLabelBarLayerRec,labelbar.title_string),
	 NhlTImmediate,_NhlUSET(DEFSTRING),0,(NhlFreeFunc)NhlFree},
{NhlNlbDrawTitle, NhlClbDrawTitle, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.title_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
{NhlNlbTitlePosition, NhlClbTitlePosition, NhlTInteger, 
	 sizeof(NhlPosition), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_pos),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlTOP),0,NULL},
{NhlNlbMaxTitleExtentF, NhlClbMaxTitleExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.max_title_ext),
	 NhlTString,_NhlUSET("0.15"),0,NULL},
{NhlNlbTitleAngleF, NhlClbTitleAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.title_angle),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNlbTitleDirection,NhlClbTitleDirection,NhlTTextDirection,
	 sizeof(NhlTextDirection),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_direction),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlACROSS),0,NULL},
{NhlNlbTitleFont, NhlCFont, NhlTFont, 
	 sizeof(NhlFont), NhlOffset(NhlLabelBarLayerRec,labelbar.title_font),
	 NhlTImmediate,_NhlUSET((NhlPointer) 1),0,NULL},
{NhlNlbTitleFontColor, NhlClbTitleFontColor, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.title_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlLB_DEF_COLOR),0,NULL},
{NhlNlbTitleJust, NhlClbTitleJust, NhlTJustification, sizeof(NhlJustification),
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_just),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
{NhlNlbTitleFontHeightF, NhlClbTitleFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.title_height),
	 NhlTString,_NhlUSET("0.025"),0,NULL},
{NhlNlbTitleFontAspectF, NhlClbTitleFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(NhlLabelBarLayerRec,labelbar.title_aspect),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNlbTitleFontThicknessF, NhlClbTitleFontThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNlbTitleFontQuality, NhlClbTitleFontQuality, NhlTFQuality, 
	 sizeof(NhlFontQuality), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_quality),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
{NhlNlbTitleConstantSpacingF, NhlClbTitleConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.title_const_spacing),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNlbTitleFuncCode, NhlClbTitleFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(NhlLabelBarLayerRec,labelbar.title_func_code),
	 NhlTString,_NhlUSET(":"),0,NULL},
	
{NhlNlbDrawBoxLines, NhlClbDrawBoxLines, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.box_line_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) 1),0,NULL},
{NhlNlbBoxLineColor, NhlClbBoxLineColor, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.box_line_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlLB_DEF_COLOR),0,NULL},
{NhlNlbBoxLineThicknessF, NhlClbBoxLineThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_line_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNlbBoxLineDashPattern, NhlClbBoxLineDashPattern, NhlTInteger, 
	 sizeof(int), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_line_dash_pattern),
	 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
{NhlNlbBoxLineDashLengthF, NhlClbBoxLineDashLengthF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.box_line_dash_length),
	 NhlTString,_NhlUSET("0.15"),0,NULL},

{NhlNlbDrawPerim, NhlClbDrawPerim, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.perim_on),
	 NhlTImmediate,_NhlUSET((NhlPointer) 1),0,NULL},
{NhlNlbPerimColor, NhlClbPerimColor, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.perim_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
{NhlNlbPerimFill, NhlClbPerimFill, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.perim_fill),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlHOLLOWFILL),0,NULL},
{NhlNlbPerimFillColor, NhlClbPerimFillColor, NhlTInteger, 
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.perim_fill_color),
	 NhlTImmediate,_NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
{NhlNlbPerimThicknessF, NhlClbPerimThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.perim_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNlbPerimDashPattern, NhlClbPerimDashPattern, NhlTInteger, 
	 sizeof(int), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.perim_dash_pattern),
	 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
{NhlNlbPerimDashLengthF, NhlClbPerimDashLengthF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.perim_dash_length),
	 NhlTString,_NhlUSET("0.15"),0,NULL},

{NhlNlbFillBackground, NhlClbFillBackground, NhlTInteger,
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.fill_background),
	 NhlTImmediate,_NhlUSET((NhlPointer) -1),0,NULL},
{NhlNlbFillLineThicknessF, NhlClbFillLineThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.fill_line_thickness),
	 NhlTString,_NhlUSET("1.0"),0,NULL},

/* End-documented-resources */

{NhlNlbMarginMode, NhlClbMarginMode, NhlTInteger,
	 sizeof(int), NhlOffset(NhlLabelBarLayerRec,labelbar.margin_mode),
	 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
{NhlNlbMaxLabelAngleAdditionF, NhlClbMaxLabelAngleAdditionF, NhlTFloat,
	 sizeof(float), 
	 NhlOffset(NhlLabelBarLayerRec,labelbar.label_angle_add),
	 NhlTString,_NhlUSET("0.15"),0,NULL}

};

/*
* Base Methods used
*/


static NhlErrorTypes    LabelBarInitialize(
#if	NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes LabelBarSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes 	LabelBarGetValues(
#if	NhlNeedProto
	NhlLayer,		/* l */
	_NhlArgList, 	/* args */
	int		/* num_args */
#endif
);

static NhlErrorTypes	LabelBarDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes	LabelBarDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes 	LabelBarClassInitialize();

/*
* Private functions
*/


static NhlErrorTypes    InitializeDynamicArrays(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    ManageDynamicArrays(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    SetLabelBarGeometry(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    SetTitle(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    SetBoxLocations(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    ManageBoxFractionsArray(
#if	NhlNeedProto
	float	*box_fractions,
	int	count
#endif
);

static void CreateIntermediates(
#if	NhlNeedProto
	float*,		/* *flist */
	int,		/* start  */
	int		/* end    */
#endif
);

static NhlErrorTypes    SetLabels(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes   	AdjustLabels(
#if	NhlNeedProto
	NhlLabelBarLayerPart *lb_p,
	float		height,
	float		avail_space,
	int		max_strlen,
	float		area_x,
	float		area_y
#endif
);

static NhlErrorTypes    AdjustGeometry(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlGenArray GenArraySubsetCopy(
#if	NhlNeedProto
	NhlGenArray	ga,
	int		length
#endif
);

#if 0
static NhlBoolean    	LabelBarChanged(
#if	NhlNeedProto
	NhlLayer,		/* new		*/ 
	NhlLayer,		/* old		*/
	int,            /* init         */
	_NhlArgList	args,
	int		num_args
#endif
);
#endif

NhlLabelBarLayerClassRec NhllabelBarLayerClassRec = {
	{
/* class_name			*/	"labelBarLayerClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlLabelBarLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlviewLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	LabelBarClassInitialize,
/* layer_initialize		*/	LabelBarInitialize,
/* layer_set_values		*/	LabelBarSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	LabelBarGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	LabelBarDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	LabelBarDraw,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	{
/* segment_workstation */ -1,
/* get_bb */		NULL, /* ---------> Do I need one?<---------*/
	},
	{
			NULL
	}
};

NhlLayerClass NhllabelBarLayerClass = (NhlLayerClass)&NhllabelBarLayerClassRec;

/*
 * Function:	nhlflabelbarclass
 *
 * Description:	fortran ref to contour class
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlLayerClass
 * Side Effect:	
 */
NhlLayerClass
_NHLCALLF(nhlflabelbarlayerclass,NHLFLABELBARLAYERCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhllabelBarLayerClass;
}

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark	Qfill_patterns = NrmNULLQUARK;
static NrmQuark	Qfill_colors = NrmNULLQUARK;
static NrmQuark	Qfill_scales = NrmNULLQUARK;
static NrmQuark	Qlabel_strings = NrmNULLQUARK;
static NrmQuark	Qbox_fractions = NrmNULLQUARK;
static NrmQuark	Qtitle_string = NrmNULLQUARK;

/*
 * Function:	LabelBarInitialize
 *
 * Description:	Performs initialization of LabelBar. 
 *              1) Initialize some internal parameters, and coerce
 *                 several others into the proper boundaries.
 *              2) Copy the view settings.
 *              2) Create a default title string.
 *              3) InitializeDynamicArrays
 *		4) Set up the LabelBar Geometry
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: 
 */
/*ARGSUSED*/
static NhlErrorTypes    LabelBarInitialize
#if	NhlNeedProto
	(NhlLayerClass class, 
	 NhlLayer req, 
	 NhlLayer new, 
	 _NhlArgList args,
	 int num_args)
#else
(class,req,new,args,num_args)
	NhlLayerClass	class;
	NhlLayer		req;
	NhlLayer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlErrorTypes	ret=NhlNOERROR,ret1 = NhlNOERROR;

	lb_p->new_draw_req = True;
	lb_p->labels_id = -1;
	lb_p->title_id = -1;
	lb_p->stride_labels = NULL;
	lb_p->box_locs = NULL;
	lb_p->label_locs = NULL;

/*
 * Ensure that the label and title angles range is between 0 and 360
 */
	lb_p->label_angle = fmod(lb_p->label_angle,360.0);
	lb_p->title_angle = fmod(lb_p->title_angle,360.0);

	if (lb_p->box_count < 1) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"Minimum box count is 1");
		ret = NhlWARNING;
		lb_p->box_count = 1;
	}

/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlbTitlePosition) &&
	    !_NhlArgIsSet(args,num_args,NhlNlbTitleDirection)) {
		switch (lb_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lb_p->title_direction = NhlACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lb_p->title_direction = NhlDOWN;
			break;
		}
	}

	lb_p->lb_x = tnew->view.x;
	lb_p->lb_y = tnew->view.y;
	lb_p->lb_width = tnew->view.width;
	lb_p->lb_height = tnew->view.height;

	lb_p->perim.l = lb_p->lb_x;
	lb_p->perim.r = lb_p->lb_x + lb_p->lb_width;
	lb_p->perim.b = lb_p->lb_y - lb_p->lb_height;
	lb_p->perim.t = lb_p->lb_y;

/*
 * Set the orientation flag so it can be consulted on the first set call.
 */
	if (!_NhlArgIsSet(args,num_args,NhlNlbOrientation))
		lb_p->orient_set = False;
	else
		lb_p->orient_set = True;
/*
 * Set up array resources
 */

	ret1 = InitializeDynamicArrays(new,req,args,num_args);
	ret = MIN(ret,ret1);

/*
 * Return now if the labelbar is turned off
 */

	if (! lb_p->labelbar_on)
		return ret;

/*
 * Calculate labelbar geometry
 */

	ret1 = SetLabelBarGeometry(new,args,num_args);
	ret = MIN(ret1,ret);
/*
 * Set up the title using a text object
 */

	ret1 = SetTitle(new,req,1,args,num_args);
	ret = MIN(ret1,ret);

/*
 * Set the box locations
 */
	ret1 = SetBoxLocations(new,req,1,args,num_args);
	ret = MIN(ret1,ret);

/*
 * Set up the labels using a multitext object
 */

	ret1 = SetLabels(new,req,1,args,num_args);
	ret = MIN(ret1,ret);

/*
  Adjust the geometry
  */

	ret1 = AdjustGeometry(new,req,1,args,num_args);
	ret = MIN(ret1,ret);
	

	return(MIN(ret,ret1));
}

/*
 * Function:	LabelBarSetValues
 *
 * Description: Handles setting all the LabelBar object resources. Calls a
 *	number of internal subroutines to deal with each phase of the 
 *	operation of setting up the LabelBar object. Calls many of the same
 *	routines used by the LabelBar initialize call.
 *	Note: the LabelBar object has not been performance tuned. It is
 *	possible that a performance saving could result from eliminating
 *	some calls under certain conditions. 
 *
 *
 * In Args:	Standard SetValues args
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: GKS and plotchar state changes.
 */
/*ARGSUSED*/
static NhlErrorTypes LabelBarSetValues
#if	NhlNeedProto
	(NhlLayer old,
	NhlLayer reference,
	NhlLayer new,
	_NhlArgList args,
	int num_args)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int	num_args;
#endif
{
	NhlLabelBarLayer	told = (NhlLabelBarLayer) old;
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart	*olb_p = &(told->labelbar);
	NhlLabelBarLayerPart	*lb_p = &(tnew->labelbar);
	NhlErrorTypes		ret = NhlNOERROR,ret1 = NhlNOERROR;
	char 			*e_text;
	char			*entry_name = "LabelBarSetValues";
	int			view_args = 0;

	if (tnew->view.use_segments != told->view.use_segments) {
		tnew->view.use_segments = told->view.use_segments;
		ret = MIN(ret,NhlWARNING);
		e_text = "%s: attempt to set create-only resource overridden";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
	}
	if (_NhlArgIsSet(args,num_args,NhlNvpXF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpYF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpWidthF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpHeightF)) view_args++;

	if (num_args > view_args)
		lb_p->new_draw_req = True;

	if (lb_p->box_count < 1) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"Minimum box count is 1");
		ret = NhlWARNING;
		lb_p->box_count = 1;
	}

/*
 * Ensure that the label and title angles range is between 0 and 360
 */
	lb_p->label_angle = fmod(lb_p->label_angle,360.0);
	lb_p->title_angle = fmod(lb_p->title_angle,360.0);


	ret1 = ManageDynamicArrays(new,old,args,num_args);
	ret = MIN(ret,ret1);

/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlbTitlePosition) &&
	    !_NhlArgIsSet(args,num_args,NhlNlbTitleDirection)) {
		switch (lb_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lb_p->title_direction = NhlACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lb_p->title_direction = NhlDOWN;
			break;
		}
	}

	if (tnew->view.x != told->view.x
	    || tnew->view.y != told->view.y) {

		_NhlEvalTrans(tnew->view.trans_children,
			      lb_p->lb_x,lb_p->lb_y,
			      &lb_p->lb_x,&lb_p->lb_y);
	}
	if (tnew->view.width != told->view.width
	    || tnew->view.height != told->view.height) {

		
		float tx, ty;
		float ow = lb_p->lb_width;
		float oh = lb_p->lb_height;

		_NhlEvalTrans(tnew->view.trans_children,
			      olb_p->lb_x + lb_p->lb_width, 
			      olb_p->lb_y - lb_p->lb_height,
			      &tx, &ty);

		lb_p->lb_width  = tx - lb_p->lb_x;
		lb_p->lb_height = lb_p->lb_y - ty;

		tx = lb_p->lb_width / ow;
		ty = lb_p->lb_height / oh;


		if (! _NhlArgIsSet(args,num_args,NhlNlbLabelFontHeightF)) {

			if (lb_p->label_direction == NhlACROSS)
				lb_p->label_height *= tx;
			else 
				lb_p->label_height *= ty;
		}
		if (! _NhlArgIsSet(args,num_args,NhlNlbTitleFontHeightF)) {

			if (lb_p->title_direction == NhlACROSS)
				lb_p->title_height *= tx;
			else
				lb_p->title_height *= ty;
		}
	}
#if 0		

		if (LabelBarChanged(tnew, told, 0, args, num_args)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"LabelBarSetValues: Can not change x,y,width,and height when other labelbar attribute changes have been requested also, proceding with other text attribute requests");
                  ret1 = NhlWARNING;
			
		}
	}
	else {

		ret1 = TransformPoints(tnew);
#endif


	lb_p->perim.l = lb_p->lb_x;
	lb_p->perim.r = lb_p->lb_x + lb_p->lb_width;
	lb_p->perim.b = lb_p->lb_y - lb_p->lb_height;
	lb_p->perim.t = lb_p->lb_y;

/*
 * Set the orientation according to the aspect ratio if it is not explicitly
 * set: one time only!
 */
	if (!lb_p->orient_set && 
	    !_NhlArgIsSet(args,num_args,NhlNlbOrientation)) {

		if (lb_p->lb_height/lb_p->lb_width > 1.0)
			lb_p->orient = NhlVERTICAL;
		else
			lb_p->orient = NhlHORIZONTAL;
	}
	lb_p->orient_set = True;
/*
 * Return now if the labelbar is turned off
 */

	if (! lb_p->labelbar_on)
		return ret;
/*
 * Calculate labelbar geometry
 */

	ret1 = SetLabelBarGeometry(new,args,num_args);
	ret = MIN(ret1,ret);
	ret1 = SetTitle(new,old,0,args,num_args);
	ret = MIN(ret1,ret);
/*
 * Set the box locations
 */
	ret1 = SetBoxLocations(new,old,0,args,num_args);
	ret = MIN(ret1,ret);

	ret1 = SetLabels(new,old,0,args,num_args);
	ret = MIN(ret1,ret);

	ret1 = AdjustGeometry(new,old,0,args,num_args);
	ret = MIN(ret1,ret);
		     
        return(MIN(ret,ret1));
}

/*
 * Function:  InitializeDynamicArrays
 *
 * Description: Creates internal copies of each of the LabelBar GenArrays and
 *	populates the copies with the values specified via a LabelBarCreate
 *	call. Assigns default values and sizes to any array whose resource
 *	the caller has not set.
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
static NhlErrorTypes    InitializeDynamicArrays
#if	NhlNeedProto
	(NhlLayer		new, 
	NhlLayer		old,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret_1 = NhlNOERROR;
	int i,count;
	int len;
	char number[10];
	NhlGenArray ga;
	char *entry_name = "LabelBarInitialize";
	char *e_text;
	int *i_p;
	float * f_p;
	NhlString *s_p;

/*=======================================================================*/
/* 
 * Initialize the color array:  
 * Create an array that contains the larger of the default box
 * count and the supplied box count. Fill it with the default color array 
 * values up to the default box count size, and the single default color value
 * for the rest of the elements. 
 * Then if the user has supplied a generic array copy it over the 
 * created array. Then check each element to ensure that
 * it is a valid color index.
 * Finally, create a private array for GKS color indices, convert each
 * workstation index into a GKS index, and copy into the GKS array.
 */

	NhlVAGetValues(tnew->base.wkptr->base.id,
		     NhlNwkColorMapLen, &len, NULL);
	count = MAX(lb_p->box_count, NhlLB_DEF_BOX_COUNT);
	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillColors);
		return NhlFATAL;
	}
	for (i=0; i < NhlLB_DEF_BOX_COUNT; i++) 
		i_p[i] = def_colors[i];
	for (i=NhlLB_DEF_BOX_COUNT; i<count; i++)
		i_p[i] = i < len ? i : NhlLB_DEF_COLOR;
			
	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTInteger,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillColors);
		return NhlFATAL;
	}
	ga->my_data = True;

	if (lb_p->fill_colors != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->fill_colors,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->fill_colors = ga;

	/* check for validity, and copy the GKS index into a private array */

	if ((lb_p->gks_colors = 
	     (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating private storage array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	i_p = (int *) lb_p->fill_colors->data;
	for (i=0; i<count; i++) {
		if (i_p[i] < 0 || i_p[i] > len) {
			e_text =
	       "%s: %s index %d holds an invalid color value, %d: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillColors, i, i_p[i]);
		        ret = MIN(ret, NhlWARNING);
			i_p[i] = NhlLB_DEF_COLOR;
		}
		lb_p->gks_colors[i] =
			_NhlGetGksCi(tnew->base.wkptr,i_p[i]);
	}

/*=======================================================================*/
/* The fill patterns array
 */

	count = MAX(lb_p->box_count, NhlLB_DEF_BOX_COUNT);
	NhlVAGetValues(tnew->base.wkptr->base.id,
		     NhlNwkFillTableLength, &len, NULL);

	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillPatterns);
		return NhlFATAL;
	}
	for (i=0; i < NhlLB_DEF_BOX_COUNT; i++) 
		i_p[i] = def_patterns[i];
	for (i=NhlLB_DEF_BOX_COUNT; i<count; i++)
		i_p[i] = i + 1;

	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTInteger,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillPatterns);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If an item types resource array has been passed in, copy it to the ga */

	if (lb_p->fill_patterns != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->fill_patterns,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillPatterns, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->fill_patterns = ga;

/*
 * Replace any invalid elements in the array with the default value
 */
	i_p = (int *) lb_p->fill_patterns->data;
	for (i=0; i<count; i++) {
		if (i_p[i] < NhlHOLLOWFILL) {
			e_text =
	       "%s: %s index %d holds an invalid color value, %d: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillPatterns, i, i_p[i]);
		        ret = MIN(ret, NhlWARNING);
			i_p[i] = NhlLB_DEF_PATTERN;
		}
	}

/*=======================================================================*/
/* The fill_scales array */

	count = MAX(lb_p->box_count, NhlLB_DEF_BOX_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillScales);
		return NhlFATAL;
	}
	for (i=0; i < count; i++) 
		f_p[i] = 1.0;

	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbFillScales);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If an item types resource array has been passed in, copy it to the ga */

	if (lb_p->fill_scales != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->fill_scales,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillScales, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->fill_scales = ga;
/*
 * Replace any invalid elements in the array with the default value
 */
	f_p = (float *) lb_p->fill_scales->data;
	for (i=0; i<count; i++) {
		if (f_p[i] <= 0.0) {
			e_text =
	       "%s: %s index %d holds an invalid item type: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillScales, i);
		        ret = MIN(ret, NhlWARNING);
			f_p[i] = 1.0;
		}
	}

/*=======================================================================*/

/* 
 * The label strings array: 
 * Subsidary arrays managed in SetLabels hold the
 * position of labels that will be drawn, and copies of pointers to
 * the strings selected by the stride function. The two variables 
 * initialized to 0 here, keep track of the currently allocated size of
 * these arrays (called label_locs and stride_labels).
 */

	lb_p->max_label_stride_count = 0;
	lb_p->max_label_draw_count = 0;
	if (lb_p->label_alignment == NhllbBOXCENTERS)
		lb_p->current_label_count = lb_p->box_count;
	else if (lb_p->label_alignment == NhllbINTERIOREDGES)
		lb_p->current_label_count = lb_p->box_count - 1;
	else
		lb_p->current_label_count = lb_p->box_count + 1;

	count = MAX(lb_p->current_label_count, NhlLB_DEF_BOX_COUNT);
	if ((s_p = (NhlString *) 
	     NhlMalloc(count * sizeof(NhlString))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbLabelStrings);
		return NhlFATAL;
	}
	for (i=0;i<count;i++) {
		sprintf(number,"%d",i);
		if ((s_p[i] = (char *)
		     NhlMalloc(strlen(NhlLB_DEF_STRING) + 
			       strlen(number) + 1)) == NULL) {
			e_text = "%s: error creating %s string";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbLabelStrings);
			return NhlFATAL;
		}
		strcpy(s_p[i], NhlLB_DEF_STRING);
		strcat(s_p[i], number);
	}

	if ((ga = NhlCreateGenArray((NhlPointer)s_p,NhlTString,
				    sizeof(NhlString),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbLabelStrings);
		return NhlFATAL;
	}
	ga->my_data = True;

/* If a label strings resource array has been passed in, copy it to the ga */

	if (lb_p->label_strings != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->label_strings,
						  NhlLB_MAX_LBL_STRINGS,
						  True,False,
						  NhlNlbLabelStrings, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->label_strings = ga;

/*
 * Copy function should have replaced any NULL strings - nothing else is
 * invalid.
 */


/*=======================================================================*/
/* 
 * Set up the box_fraction array. If no array
 * is provided create a uniform array, allowing future modification of 
 * the sizing. Set it up whether or not it is going to be used.
 */
	
	count = MAX(lb_p->box_count, NhlLB_DEF_BOX_COUNT) + 1;
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbBoxFractions);
		return NhlFATAL;
	}
	
	f_p[0] = 0.0;
	for (i=1;i<lb_p->box_count; i++)
		f_p[i] = -1.0;
	f_p[lb_p->box_count] = 1.0;
	
	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNlbBoxFractions);
		return NhlFATAL;
	}
	ga->my_data = True;
		
/* If a box fractions array has been passed in, copy it to the ga */

	if (lb_p->box_fractions != NULL) {
		ret_1 = _NhlValidatedGenArrayCopy(&ga,lb_p->box_fractions,
						  NhlLB_MAX_BOXES+1,
						  True,False,
						  NhlNlbBoxFractions, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
	}
	lb_p->box_fractions = ga;
		
/*
 * Allocate the location array: use one more than
 * the current box count so that both ends of the labelbar can be stored
 */

	lb_p->box_locs = (float *) 
		NhlMalloc((lb_p->box_count+1) * sizeof(float));
	
	return (ret);
		
}

/*
 * Function:    ManageDynamicArrays
 *
 * Description:	Handles changes to any of the LabelBar object GenArrays after
 *	their initial creation. 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The internal copy of each GenArray is modified to reflect
 *	changes requested via LabelBarSetValues
 */

/*ARGSUSED*/
static NhlErrorTypes    ManageDynamicArrays
#if	NhlNeedProto
	(NhlLayer		new, 
	NhlLayer		old,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayer	told = (NhlLabelBarLayer) old;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlLabelBarLayerPart *olb_p = &(told->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret_1 = NhlNOERROR;
	int i;
	int count, len;
	char number[10];
	char *entry_name = "LabelBarSetValues";
	char *e_text;
	int  *i_p;
	float *f_p;
	NhlString *s_p;

/*=======================================================================*/
/*
 * Manage the colors array: if the array has changed copy the new
 * array elements, check them for validity and get the GKS index.
 * Then if the box count is greater than the current array size, enlarge
 * the array and give initial values to the new elements.
 */

	NhlVAGetValues(tnew->base.wkptr->base.id,
		     NhlNwkColorMapLen, &len, NULL);
	count = lb_p->mono_fill_color ? 1 : lb_p->box_count;

	if (lb_p->fill_colors != olb_p->fill_colors) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->fill_colors),
						  lb_p->fill_colors,
						  NhlLB_MAX_BOXES,True,False,
						  NhlNlbFillColors, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lb_p->fill_colors = olb_p->fill_colors;
		i_p = (int *) lb_p->fill_colors->data;

		for (i=0;i<MIN(count,lb_p->fill_colors->num_elements); i++) {
			if (i_p[i] < 0 || i_p[i] > len) {
				e_text =
		"%s: %s index %d holds an invalid color value, %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,
					  NhlNlbFillColors, i, i_p[i]);
				ret = MIN(ret, NhlWARNING);
				i_p[i] = NhlLB_DEF_COLOR;
			}
			lb_p->gks_colors[i] =
				_NhlGetGksCi(tnew->base.wkptr, i_p[i]);
		}
	}

	if (lb_p->fill_colors->num_elements < count) {
		i_p = (int *) lb_p->fill_colors->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillColors);
			return NhlFATAL;
		}
		if ((lb_p->gks_colors = (int *)
		     NhlRealloc(lb_p->gks_colors, 
				count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating private %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillColors);
			return NhlFATAL;
		}
		for (i=lb_p->fill_colors->num_elements; i<count; i++) {
			i_p[i] = i < len ? i : NhlLB_DEF_COLOR;
			lb_p->gks_colors[i] =
				_NhlGetGksCi(tnew->base.wkptr, i_p[i]);
		}

		lb_p->fill_colors->data = (NhlPointer) i_p;
		lb_p->fill_colors->num_elements = count;
	}
/*=======================================================================*/

/*
 * Manage the fill pattern array
 */

	count = lb_p->mono_fill_pattern ? 1 : lb_p->box_count;
	NhlVAGetValues(tnew->base.wkptr->base.id,
		     NhlNwkFillTableLength, &len, NULL);

	if (lb_p->fill_patterns != olb_p->fill_patterns) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->fill_patterns),
						  lb_p->fill_patterns,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillPatterns, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lb_p->fill_patterns = olb_p->fill_patterns;
		i_p = (int *) lb_p->fill_patterns->data;
		for (i=0; i<MIN(count,
				lb_p->fill_patterns->num_elements); i++) {
			if (i_p[i] < NhlHOLLOWFILL) {
				e_text =
	      "%s: %s index %d holds an invalid pattern value, %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNlbFillPatterns, i, i_p[i]);
				ret = MIN(ret, NhlWARNING);
				i_p[i] = NhlLB_DEF_PATTERN;
			}

		}
	}

	if (lb_p->fill_patterns->num_elements < count) {
		i_p = (int *) lb_p->fill_patterns->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillPatterns);
			return NhlFATAL;
		}
		for (i=lb_p->fill_patterns->num_elements; i<count; i++) {
			i_p[i] = i + 1;
		}

		lb_p->fill_patterns->data = (NhlPointer) i_p;
		lb_p->fill_patterns->num_elements = count;
	}

/*=======================================================================*/
/* 
 * Handle the fill scales
 */

	count = lb_p->mono_fill_scale ? 1 : lb_p->box_count;

	if (lb_p->fill_scales != olb_p->fill_scales) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->fill_scales),
						  lb_p->fill_scales,
						  NhlLB_MAX_BOXES,
						  True,False,
						  NhlNlbFillScales, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lb_p->fill_scales = olb_p->fill_scales;
		f_p = (float *) lb_p->fill_scales->data;
		for (i=0; i<MIN(count,lb_p->fill_scales->num_elements); i++) {
			if (f_p[i] <= 0.0) {
				e_text =
	      "%s: %s index %d holds an invalid fill scale value: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
					  NhlNlbFillScales, i);
				ret = MIN(ret, NhlWARNING);
				f_p[i] = 1.0;
			}
		}
	}

	if (lb_p->fill_scales->num_elements < count) {
		f_p = (float *) lb_p->fill_scales->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, count * sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbFillScales);
			return NhlFATAL;
		}
		for (i=lb_p->fill_scales->num_elements; i<count; i++) {
			f_p[i] = 1.0;
		}

		lb_p->fill_scales->data = (NhlPointer) f_p;
		lb_p->fill_scales->num_elements = count;
	}

/*======================================================================*/

/* 
 * Handle the label strings. Copy the new array into the old.
 * NULL strings generate an error message and are replaced by empty 
 * (single byte null terminator) strings within the Validated copy function. 
 * If the box count has increased, but the string gen array does not 
 * contain enough elements, the array is resized and default strings are
 * created for the additional elements.
 */

	if (lb_p->label_alignment == NhllbBOXCENTERS)
		count = lb_p->box_count;
	else if (lb_p->label_alignment == NhllbINTERIOREDGES)
		count = lb_p->box_count - 1;
	else
		count = lb_p->box_count + 1;
	lb_p->current_label_count = count;

	if (lb_p->label_strings != olb_p->label_strings) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->label_strings),
						  lb_p->label_strings,
						  NhlLB_MAX_LBL_STRINGS,
						  True,False,
						  NhlNlbLabelStrings, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
				return ret;
		lb_p->label_strings = olb_p->label_strings;
	}

	if (lb_p->label_strings->num_elements < count) {
		s_p = (NhlString *) lb_p->label_strings->data;
		if ((s_p = (NhlString *)
		     NhlRealloc(s_p, count * sizeof (NhlString))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbLabelStrings);
			return NhlFATAL;
		}
		for (i=lb_p->label_strings->num_elements; i<count; i++) {
			sprintf(number,"%d",i);
			if ((s_p[i] = (char *)
			     NhlMalloc(strlen(NhlLB_DEF_STRING) + 
				       strlen(number) + 1)) == NULL) {
				e_text = "%s: error creating %s string";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,NhlNlbLabelStrings);
				return NhlFATAL;
			}
			strcpy(s_p[i],(Const char *)NhlLB_DEF_STRING);
			strcat(s_p[i],number);
		}

		lb_p->label_strings->data = (NhlPointer) s_p;
		lb_p->label_strings->num_elements = count;
	}


/*======================================================================*/
/* 
 * Manage the box_fraction array. If the box count changes then reset the
 * box fraction array to a default (uniform) state. This is because there
 * is no obvious way to modify explicitly set sizes to handle a different
 * number of boxes. Even if the box_fraction resource is set in the 
 * same call the reset occurs because it is difficult to ensure that 
 * the passed in array contains the appropriate elements for the new 
 * box_count. Note that error checking for the elements of this resource 
 * is handled later. The box fraction array contains one more element than
 * the box count.
 */
	count = lb_p->box_count+1;
	if (lb_p->box_fractions != olb_p->box_fractions) {
		ret_1 = _NhlValidatedGenArrayCopy(&(olb_p->box_fractions),
						  lb_p->box_fractions,
						  NhlLB_MAX_BOXES+1,
						  True,False,
						  NhlNlbBoxFractions, 
						  entry_name);
		
		if ((ret = MIN(ret,ret_1)) < NhlWARNING) 
			return ret;
		lb_p->box_fractions = olb_p->box_fractions;
	}
	
	if (lb_p->box_fractions->num_elements < count) {
		f_p = (float *) lb_p->box_fractions->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, (count) * 
				sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlbBoxFractions);
			return NhlFATAL;
		}

		lb_p->box_fractions->data = (NhlPointer) f_p;
		lb_p->box_fractions->num_elements = count;
	}

	if (lb_p->box_count != olb_p->box_count) {
		f_p = (float *) lb_p->box_fractions->data;
		f_p[0] = 0.0;
		for (i=1;i<lb_p->box_count; i++)
			f_p[i] = -1.0;
		f_p[lb_p->box_count] = 1.0;
	}

/*======================================================================*/
	     
/*
 * Allocate or reallocate the location arrays: use one more than
 * the current box count so that both ends of the labelbar can be stored
 */

	if (lb_p->box_count > olb_p->box_count) {
		if (lb_p->box_locs == NULL) {
			lb_p->box_locs = (float *) 
				NhlMalloc((lb_p->box_count+1) * sizeof(float));
		}
		else {
			lb_p->box_locs = (float *) 
				NhlRealloc(lb_p->box_locs, 
					   (lb_p->box_count+1) *
					   sizeof(float));
		}
	}
	
	return (ret);
}

/*
 * Function: SetLabelBarGeometry
 *
 * Description:	Performs a rough division of the space available for the
 *		LabelBar into item area, label area and title area. Depending
 *		on the situation some of these areas are adjusted later.
 *
 * In Args:
 *
 * Out Args: Error conditions
 *
 * Return Values:
 *
 * Side Effects: There are no calls to Get or Set Values so the args and
 *               num_args arguments are not required.
 */
 
/*ARGSUSED*/
static NhlErrorTypes    SetLabelBarGeometry
#if	NhlNeedProto
	 (NhlLayer new, 
	 _NhlArgList args,
	 int num_args)
#else
(new,args,num_args)
	NhlLayer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "lbSetLabelBarGeometry";
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	enum {NO_TITLE, MINOR_AXIS, MAJOR_AXIS} title_loc;
	float bar_ext, max_title_ext, adj_perim_width, adj_perim_height;
	float small_axis;
	float title_off = 0.0, angle_adj = 0.0;
	float title_angle, tan_t;

/* Calculate the ndc margin from the fractional margin */

	small_axis = MIN(lb_p->lb_width, lb_p->lb_height);

	lb_p->adj_perim.l = lb_p->perim.l + lb_p->margin.l * small_axis;
	lb_p->adj_perim.r = lb_p->perim.r - lb_p->margin.r * small_axis;
	lb_p->adj_perim.b = lb_p->perim.b + lb_p->margin.b * small_axis;
	lb_p->adj_perim.t = lb_p->perim.t - lb_p->margin.t * small_axis;
	adj_perim_width = lb_p->adj_perim.r - lb_p->adj_perim.l;
	adj_perim_height = lb_p->adj_perim.t - lb_p->adj_perim.b;
		
/*
 * Check the title extent to make sure it does not exceed the hard-coded
 * limit; then locate the title
 */
	if (lb_p->auto_manage && 
	    lb_p->max_title_ext + lb_p->title_off > 0.5) {
		/* need a NhlWARNING */
		lb_p->max_title_ext = NhlLB_DEF_MAX_TITLE_EXT;
		lb_p->title_off = NhlLB_DEF_TITLE_OFF;
	}
	title_angle = lb_p->title_angle * DEGTORAD;
	tan_t = fabs(sin(title_angle)) / 
		     MAX(0.01, fabs(cos(title_angle)));
		    
	if (!lb_p->title_on)
		title_loc = NO_TITLE;
	else if (lb_p->orient == NhlHORIZONTAL) {
		if (lb_p->title_pos == NhlRIGHT ||
		    lb_p->title_pos == NhlLEFT) {
			title_loc = MAJOR_AXIS;
			title_off = lb_p->title_off * adj_perim_width;
			if (lb_p->title_direction == NhlACROSS)
				angle_adj = adj_perim_height / tan_t;
			else
				angle_adj = adj_perim_height * tan_t;
						
		}
		else {
			title_loc = MINOR_AXIS;
			title_off = lb_p->title_off * adj_perim_height;
			if (lb_p->title_direction == NhlACROSS)
				angle_adj = adj_perim_width * tan_t; 
			else
				angle_adj = adj_perim_width / tan_t;
		}
	}
	else {
		if (lb_p->title_pos == NhlRIGHT ||
		    lb_p->title_pos == NhlLEFT) {
			title_loc = MINOR_AXIS;
			title_off = lb_p->title_off * adj_perim_width;
			if (lb_p->title_direction == NhlACROSS)
				angle_adj = adj_perim_height / tan_t;
			else
				angle_adj = adj_perim_height * tan_t;
		}
		else {
			title_loc = MAJOR_AXIS;
			title_off = lb_p->title_off * adj_perim_height;
			if (lb_p->title_direction == NhlACROSS)
				angle_adj = adj_perim_width * tan_t; 
			else
				angle_adj = adj_perim_width / tan_t;
		}
	}
/*
 * If not in auto manage mode the title offset should make the label bar
 * grow. By setting it to 0.0 here, it will be added into the total size
 * in the AdjustGeometry routine.
 */

	if (! lb_p->auto_manage) {
		title_off = 0.0;
	}

/*
 * Silently modify the label position if not appropriate for the
 * current labelbar orientation. Also determine the maximum critical
 * angle for label angle adjustments.
 */

	if (lb_p->orient == NhlHORIZONTAL) {
		switch (lb_p->label_pos) {
		default:
		case NhlTOP:
		case NhlLEFT:
			lb_p->label_pos = NhlTOP;
			break;
		case NhlBOTTOM:
		case NhlRIGHT:
			lb_p->label_pos = NhlBOTTOM;
			break;
		}
	}
	else {
		switch (lb_p->label_pos) {
		default:
		case NhlRIGHT:
		case NhlBOTTOM:
			lb_p->label_pos = NhlRIGHT;
			break;
		case NhlLEFT:
		case NhlTOP:
			lb_p->label_pos = NhlLEFT;
			break;
		}

	}
		
/*
 * Determine dimensions: first pass determines basic position for
 * title, bar and labels. Bar dimensions may be adjusted later to 
 * ensure that labels are visible.
 */
	if (lb_p->orient == NhlHORIZONTAL) {
		bar_ext = adj_perim_height * lb_p->box_minor_ext;
		switch (title_loc) {

		case NO_TITLE:
			lb_p->bar.l = lb_p->adj_perim.l;
			lb_p->bar.r = lb_p->adj_perim.r;
			lb_p->labels.l = lb_p->bar.l;
			lb_p->labels.r = lb_p->bar.r;
					
			if (! lb_p->labels_on || 
			    lb_p->label_pos == NhlCENTER) {
				lb_p->bar.b = lb_p->adj_perim.b +
					(adj_perim_height - bar_ext) / 2.0;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.b;
				lb_p->labels.t = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.t;
				lb_p->labels.t = lb_p->adj_perim.t;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->bar.b = lb_p->adj_perim.t - bar_ext;
				lb_p->bar.t = lb_p->adj_perim.t;
				lb_p->labels.b = lb_p->adj_perim.b;
				lb_p->labels.t = lb_p->bar.b;
			}
			break;

		case MAJOR_AXIS:
			
			max_title_ext = 
				MIN(lb_p->max_title_ext * adj_perim_width,
					adj_perim_height / 
					strlen(lb_p->title_string) + 
					angle_adj);
			lb_p->title.b = lb_p->adj_perim.b;
			lb_p->title.t = lb_p->adj_perim.t;
				
			if (lb_p->title_pos == NhlLEFT) {
				lb_p->bar.l = lb_p->adj_perim.l +
					max_title_ext + title_off;
				lb_p->bar.r = lb_p->adj_perim.r;
				lb_p->title.l = lb_p->adj_perim.l;
				lb_p->title.r = lb_p->bar.l - title_off;
			}
			else if (lb_p->title_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->adj_perim.r - 
					max_title_ext - title_off;
				lb_p->title.l = lb_p->bar.r + title_off;
				lb_p->title.r = lb_p->adj_perim.r;
			}
			lb_p->labels.l = lb_p->bar.l;
			lb_p->labels.r = lb_p->bar.r;

			if (! lb_p->labels_on || 
			    lb_p->label_pos == NhlCENTER) {
				lb_p->bar.b = lb_p->adj_perim.b +
					(adj_perim_height - bar_ext) / 2.0;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.b;
				lb_p->labels.t = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.t;
				lb_p->labels.t = lb_p->adj_perim.t;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->bar.b = lb_p->adj_perim.t - bar_ext;
				lb_p->bar.t = lb_p->adj_perim.t;
				lb_p->labels.b = lb_p->adj_perim.b;
				lb_p->labels.t = lb_p->bar.b;
			}
			break;

		case MINOR_AXIS:

			max_title_ext = 
				MIN(lb_p->max_title_ext * adj_perim_height,
					adj_perim_width / 
					strlen(lb_p->title_string) + 
					angle_adj);
			if (max_title_ext + title_off + bar_ext > 
			    adj_perim_height) {
				e_text = 
				  "%s: Maximum bar size exceeded, defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name);
				ret = MIN(ret, NhlWARNING);
				bar_ext = adj_perim_height - 
					max_title_ext - title_off;
				lb_p->box_minor_ext = 
					bar_ext / adj_perim_height;
			}
			lb_p->title.l = lb_p->adj_perim.l;
			lb_p->title.r = lb_p->adj_perim.r;
			lb_p->bar.l = lb_p->adj_perim.l;
			lb_p->bar.r = lb_p->adj_perim.r;
			lb_p->labels.l = lb_p->adj_perim.l;
			lb_p->labels.r = lb_p->adj_perim.r;
				
			if (lb_p->title_pos == NhlBOTTOM) {
				lb_p->title.b = lb_p->adj_perim.b;
				lb_p->title.t = lb_p->adj_perim.b + 
					max_title_ext;
				lb_p->bar.b = lb_p->title.t + title_off;
				lb_p->bar.t = lb_p->adj_perim.t;
			}
			else if (lb_p->title_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->adj_perim.t - 
					max_title_ext - title_off;
				lb_p->title.b = lb_p->bar.t + title_off;
				lb_p->title.t = lb_p->adj_perim.t;
			}

			
			if (!lb_p->labels_on || lb_p->label_pos == NhlCENTER) {
				lb_p->bar.b = (lb_p->bar.t - lb_p->bar.b - 
					       bar_ext) / 2.0 + lb_p->bar.b;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.b;
				lb_p->labels.t = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlTOP) {
				lb_p->labels.t = lb_p->bar.t;
				lb_p->bar.t = lb_p->bar.b + bar_ext;
				lb_p->labels.b = lb_p->bar.t;
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->labels.b = lb_p->bar.b;
				lb_p->bar.b = lb_p->bar.t - bar_ext;
				lb_p->labels.t = lb_p->bar.b;
			}
			break;
		default:
			e_text = "%s: Invalid title location value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		/* get the box size */
		
		lb_p->box_size.x = 
			(lb_p->bar.r - lb_p->bar.l) / lb_p->box_count;
		lb_p->box_size.y = lb_p->bar.t - lb_p->bar.b;
	}

	else { /* NhlVERTICAL */

		bar_ext = adj_perim_width * lb_p->box_minor_ext;

		switch (title_loc) {

		case NO_TITLE:
			lb_p->bar.b = lb_p->adj_perim.b;
			lb_p->bar.t = lb_p->adj_perim.t;
			lb_p->labels.b = lb_p->adj_perim.b;
			lb_p->labels.t = lb_p->adj_perim.t;
					
			if (!lb_p->labels_on || lb_p->label_pos == NhlCENTER) {
				lb_p->bar.l = lb_p->adj_perim.l +
					(adj_perim_width - bar_ext) / 2.0;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.l;
				lb_p->labels.r = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.r;
				lb_p->labels.r = lb_p->adj_perim.r;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				lb_p->bar.l = lb_p->adj_perim.r - bar_ext;
				lb_p->bar.r = lb_p->adj_perim.r;
				lb_p->labels.l = lb_p->adj_perim.l;
				lb_p->labels.r = lb_p->bar.l;
			}
			break;

		case MAJOR_AXIS:
			max_title_ext = 
				MIN(lb_p->max_title_ext * adj_perim_height,
					adj_perim_width / 
					strlen(lb_p->title_string) + 
					angle_adj);

			lb_p->title.l = lb_p->adj_perim.l;
			lb_p->title.r = lb_p->adj_perim.r;
				
			if (lb_p->title_pos == NhlBOTTOM) {
				lb_p->bar.b = lb_p->adj_perim.b + 
					max_title_ext + title_off;
				lb_p->bar.t = lb_p->adj_perim.t;
				lb_p->title.b = lb_p->adj_perim.b;
				lb_p->title.t = lb_p->bar.b - title_off;
			}
			else if (lb_p->title_pos == NhlTOP) {
				lb_p->bar.b = lb_p->adj_perim.b;
				lb_p->bar.t = lb_p->adj_perim.t - 
					max_title_ext - title_off;
				lb_p->title.b = lb_p->bar.t + title_off;
				lb_p->title.t = lb_p->adj_perim.t;
			}
			lb_p->labels.b = lb_p->bar.b;
			lb_p->labels.t = lb_p->bar.t;

			if (! lb_p->labels_on || 
			    lb_p->label_pos == NhlCENTER) {
				lb_p->bar.l = lb_p->adj_perim.l +
					(adj_perim_width - bar_ext) / 2.0;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.l;
				lb_p->labels.r = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.r;
				lb_p->labels.r = lb_p->adj_perim.r;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				lb_p->bar.l = lb_p->adj_perim.r - bar_ext;
				lb_p->bar.r = lb_p->adj_perim.r;
				lb_p->labels.l = lb_p->adj_perim.l;
				lb_p->labels.r = lb_p->bar.l;
			}
			break;

		case MINOR_AXIS:

			max_title_ext = 
				MIN(lb_p->max_title_ext * adj_perim_width,
					adj_perim_height / 
					strlen(lb_p->title_string) + 
					angle_adj);
			if (max_title_ext + title_off + bar_ext > 
			    adj_perim_width) {
				e_text = 
				  "%s: Maximum bar size exceeded, defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name);
				ret = MIN(ret, NhlWARNING);
				bar_ext = adj_perim_width - 
					max_title_ext - title_off;
				lb_p->box_minor_ext = 
					bar_ext / adj_perim_width;
			}

			lb_p->title.b = lb_p->adj_perim.b;
			lb_p->title.t = lb_p->adj_perim.t;
			lb_p->bar.b = lb_p->adj_perim.b;
			lb_p->bar.t = lb_p->adj_perim.t;
			lb_p->labels.b = lb_p->adj_perim.b;
			lb_p->labels.t = lb_p->adj_perim.t;
				
			if (lb_p->title_pos == NhlLEFT) {
				lb_p->title.l = lb_p->adj_perim.l;
				lb_p->title.r = lb_p->adj_perim.l + 
					max_title_ext;
				lb_p->bar.l = lb_p->title.r + title_off;
				lb_p->bar.r = lb_p->adj_perim.r;
			}
			else if (lb_p->title_pos == NhlRIGHT) {
				lb_p->bar.l = lb_p->adj_perim.l;
				lb_p->bar.r = lb_p->adj_perim.r - 
					max_title_ext - title_off;
				lb_p->title.l = lb_p->bar.r + title_off;
				lb_p->title.r = lb_p->adj_perim.r;
			}

			
			if (!lb_p->labels_on || lb_p->label_pos == NhlCENTER) {
				lb_p->bar.l = (lb_p->bar.r - lb_p->bar.l - 
					       bar_ext) / 2.0 + lb_p->bar.l;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.l;
				lb_p->labels.r = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlRIGHT) {
				lb_p->labels.r = lb_p->bar.r;
				lb_p->bar.r = lb_p->bar.l + bar_ext;
				lb_p->labels.l = lb_p->bar.r;
			}
			else if (lb_p->label_pos == NhlLEFT) {
				lb_p->labels.l = lb_p->bar.l;
				lb_p->bar.l = lb_p->bar.r - bar_ext;
				lb_p->labels.r = lb_p->bar.l;
			}
			break;
		default:
			e_text = "%s: Invalid title location value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		lb_p->box_size.x = lb_p->bar.r - lb_p->bar.l;
		lb_p->box_size.y = 
			(lb_p->bar.t - lb_p->bar.b) / lb_p->box_count;
	}
	return ret;
				
}

/*
 * Function: SetTitle
 *
 * Description:	Calculates the title extent, then creates or sets the
 *	attributes of child text object used for the title. In auto-manage 
 *	mode, retrieves the bounding box of the child after a preliminary
 *	setting of the text height, and then resets the text height to fit
 *	into the space available.
 *	This routine could use performance evaluation and tuning.
 *	
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 *	Calls to SetValues or Create modify attributes of the child
 *	text object.
 */

/*ARGSUSED*/
static NhlErrorTypes    SetTitle
#if	NhlNeedProto
	(NhlLayer		new, 
	NhlLayer		old,
	int		init,
	_NhlArgList	args,
	int		 num_args)
#else
(new,old,init,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayer	told = (NhlLabelBarLayer) old;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlLabelBarLayerPart *olb_p = &(told->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;
	char buffer[_NhlMAXRESNAMLEN];
	char *c_p;
	NhlBoundingBox titleBB;
	float w, h, wta, hta, factor, height;

/*
 * Only initialize a text item for the title if it is turned on
 * and the maximum title extent is greater than 0.0.
 */
	if (!lb_p->title_on || lb_p->max_title_ext <= 0.0)
		return ret;

/*
 * If the title string is NULL, create a default string.
 * The default string is the name of the label bar object
 */
	if (lb_p->title_string == NULL) {
                lb_p->title_string = (char*)
                        NhlMalloc((unsigned)strlen(tnew->base.name) +1);
                strcpy(lb_p->title_string,tnew->base.name);
        } 
	else if (init) {
                c_p = lb_p->title_string;
                lb_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
                strcpy(lb_p->title_string,c_p);
        }
	else if (lb_p->title_string != olb_p->title_string) {
		NhlFree(olb_p->title_string);
		c_p = lb_p->title_string;
		lb_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
		strcpy(lb_p->title_string, c_p);
	}

	switch (lb_p->title_just) {
	case NhlBOTTOMLEFT:
		lb_p->title_x = lb_p->title.l;
		lb_p->title_y = lb_p->title.b;
		break;
	case NhlBOTTOMCENTER:
		lb_p->title_x = lb_p->title.l +
			(lb_p->title.r - lb_p->title.l)/2.0;
		lb_p->title_y = lb_p->title.b;
		break;
	case NhlBOTTOMRIGHT:
		lb_p->title_x = lb_p->title.r;
		lb_p->title_y = lb_p->title.b;
		break;
	case NhlCENTERLEFT:
		lb_p->title_x = lb_p->title.l;
		lb_p->title_y = lb_p->title.b +
			(lb_p->title.t - lb_p->title.b)/2.0;
		break;
	case NhlCENTERCENTER:
	default:
		lb_p->title_x = lb_p->title.l +
			(lb_p->title.r - lb_p->title.l)/2.0;
		lb_p->title_y = lb_p->title.b +
			(lb_p->title.t - lb_p->title.b)/2.0;
		break;
	case NhlCENTERRIGHT:
		lb_p->title_x = lb_p->title.r;
		lb_p->title_y = lb_p->title.b +
			(lb_p->title.t - lb_p->title.b)/2.0;
		break;
	case NhlTOPLEFT:
		lb_p->title_x = lb_p->title.l;
		lb_p->title_y = lb_p->title.t;
		break;
	case NhlTOPCENTER:
		lb_p->title_x = lb_p->title.l +
			(lb_p->title.r - lb_p->title.l)/2.0;
		lb_p->title_y = lb_p->title.t;
		break;
	case NhlTOPRIGHT:
		lb_p->title_x = lb_p->title.r;
		lb_p->title_y = lb_p->title.t;
		break;
	}

	if (lb_p->title_height <= 0.0) 
		height = NhlLB_DEF_CHAR_HEIGHT;
	else 
		height = lb_p->title_height;

	if (init || lb_p->title_id < 0) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Title");
		ret1 = NhlVACreate(&lb_p->title_id,
				 buffer,NhltextItemLayerClass,
				 tnew->base.id,
				 NhlNtxFont,lb_p->title_font,
				 NhlNtxString,lb_p->title_string,
				 NhlNtxPosXF,lb_p->title_x,
				 NhlNtxPosYF,lb_p->title_y,
				 NhlNtxDirection,lb_p->title_direction,
				 NhlNtxAngleF,lb_p->title_angle,
				 NhlNtxJust,(int)lb_p->title_just,
				 NhlNtxFontColor,lb_p->title_color,
				 NhlNtxFontHeightF,height,
				 NhlNtxFontAspectF,lb_p->title_aspect,
				 NhlNtxConstantSpacingF,
				 	lb_p->title_const_spacing,
				 NhlNtxFontQuality,lb_p->title_quality,
				 NhlNtxFuncCode,lb_p->title_func_code,
				 NhlNtxFontThicknessF,lb_p->title_thickness,
				 NULL);
		if (ret1 < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Text item create error");
			return(NhlFATAL);
		}
		ret = MIN(ret,ret1);
	}
	else {
		ret1 = NhlVASetValues(lb_p->title_id,
				    NhlNtxFont,lb_p->title_font,
				    NhlNtxString,lb_p->title_string,
				    NhlNtxPosXF,lb_p->title_x,
				    NhlNtxPosYF,lb_p->title_y,
				    NhlNtxDirection,lb_p->title_direction,
				    NhlNtxAngleF,lb_p->title_angle,
				    NhlNtxJust,(int)lb_p->title_just,
				    NhlNtxFontColor,lb_p->title_color,
				    NhlNtxFontHeightF,height,
				    NhlNtxFontAspectF,lb_p->title_aspect,
				    NhlNtxConstantSpacingF,
				    	lb_p->title_const_spacing,
				    NhlNtxFontQuality,lb_p->title_quality,
				    NhlNtxFuncCode,lb_p->title_func_code,
				    NhlNtxFontThicknessF,lb_p->title_thickness,
				    NULL);
		if (ret1 < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "Text item set values error");
			return(NhlFATAL);
		}
		ret = MIN(ret,ret1);
	}
	

/*
 * If title_height is 0.0 or auto-manage is in effect adjust the title
 * to the space available
 */
	ret1 = NhlGetBB(lb_p->title_id, &titleBB);
	if (ret1 < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NhlGetBB error");
		return(NhlFATAL);
	}
	ret = MIN(ret,ret1);

	w=titleBB.r-titleBB.l;
	h=titleBB.t-titleBB.b;
	if (w <= 0.0 || h <= 0.0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"no area in bounding box");
	}
	wta=lb_p->title.r-lb_p->title.l;
	hta=lb_p->title.t-lb_p->title.b;
	if (wta <= 0.0 || hta <= 0.0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"no title area");
	}
	if (lb_p->title_height <= 0.0 || lb_p->auto_manage) {
		factor = wta / w < hta / h ? wta / w : hta / h;
		lb_p->title_height = height * factor;
	}
	ret1 = NhlVASetValues(lb_p->title_id,
			   NhlNtxFontHeightF,lb_p->title_height,
			   NULL);
	if (ret1 < NhlWARNING) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"error setting text item values");
		return(NhlFATAL);
	}
	ret = MIN(ret,ret1);

	return ret;
}

/*
 * Function:  SetBoxLocations
 *
 * Description:	Sets up an array containing the beginning and ending
 *	coordinate of each box of the label bar along the major axis.
 *	If the box sizing mode is explicit, first calls an internal 
 *	function to check and coerce the user-supplied values in the
 *	box sizing GenArray into a proper sequence of values.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The box_sizing GenArray may be modified.
 */

/*ARGSUSED*/
static NhlErrorTypes    SetBoxLocations
#if	NhlNeedProto
(
	NhlLayer		new, 
	NhlLayer		old,
	int		init,
	_NhlArgList	args,
	int		num_args
)
#else
(new,old,init,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;
	float bar_len;
	int i;
	float *box_fractions = (float *)lb_p->box_fractions->data;

	if (lb_p->box_sizing == NhllbEXPLICITSIZING) {
		ret1 = ManageBoxFractionsArray(box_fractions,lb_p->box_count);
	}
	ret = MIN(ret,ret1);

/* 
 * Adjust boundary of bar and label area, depending on the label state.
 */

	memcpy((void *)&lb_p->adj_bar, (Const void *)&lb_p->bar, 
	       sizeof(NhlBoundingBox));
	memcpy((void *)&lb_p->adj_box_size, (Const void *)&lb_p->box_size,
	       sizeof(NhlCoord));
	if (lb_p->label_alignment == NhllbEXTERNALEDGES) {
		if (lb_p->orient == NhlHORIZONTAL) {
			lb_p->adj_box_size.x = lb_p->box_size.x * 
				lb_p->box_count / (lb_p->box_count + 1);
			lb_p->adj_bar.l = 
				lb_p->bar.l + lb_p->adj_box_size.x / 2.0;
			lb_p->adj_bar.r = 
				lb_p->bar.r - lb_p->adj_box_size.x / 2.0;
		}
		else {
			lb_p->adj_box_size.y = lb_p->box_size.y * 
				lb_p->box_count / (lb_p->box_count + 1);
			lb_p->adj_bar.b = 
				lb_p->bar.b + lb_p->adj_box_size.y / 2.0;
			lb_p->adj_bar.t = 
				lb_p->bar.t - lb_p->adj_box_size.y / 2.0;
		}
	}
	else if (lb_p->label_alignment == NhllbINTERIOREDGES) {
		if (lb_p->orient == NhlHORIZONTAL) {
			lb_p->labels.l = lb_p->bar.l + lb_p->box_size.x / 2.0;
			lb_p->labels.r = lb_p->bar.r - lb_p->box_size.x / 2.0;
		}
		else {
			lb_p->labels.b = lb_p->bar.b + lb_p->box_size.y / 2.0;
			lb_p->labels.t = lb_p->bar.t - lb_p->box_size.y / 2.0;
		}
	}

/*
 * create an array of the left or bottom varying coordinates -- 
 */
		
	if (lb_p->orient == NhlHORIZONTAL &&
	    lb_p->box_sizing == NhllbUNIFORMSIZING) {
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.l + 
				(float) i * lb_p->adj_box_size.x;
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.r;
	}
	if (lb_p->orient == NhlVERTICAL &&
	    lb_p->box_sizing == NhllbUNIFORMSIZING) {
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.b + 
				(float) i * lb_p->adj_box_size.y;
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.t;
	}
	if (lb_p->orient == NhlHORIZONTAL &&
	    lb_p->box_sizing == NhllbEXPLICITSIZING) {
		bar_len = lb_p->adj_bar.r - lb_p->adj_bar.l;
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.l + 
				bar_len * box_fractions[i];
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.r;
	}
	if (lb_p->orient == NhlVERTICAL &&
	    lb_p->box_sizing == NhllbEXPLICITSIZING) {
		bar_len = lb_p->adj_bar.t - lb_p->adj_bar.b;
		for (i=0; i < lb_p->box_count; i++) {
			lb_p->box_locs[i] = lb_p->adj_bar.b + 
				bar_len * box_fractions[i];
		}
		lb_p->box_locs[lb_p->box_count] = lb_p->adj_bar.t;
	}

	return(ret);

}

/*
 * Function: ManageBoxSizingArray
 *
 * Description:	Finds and modifies out-of-range values in the item positions
 *	array, so that a monotonically increasing set of values is returned. 
 *	The assumption is that negative values are placed intentionally, in
 *	order to force linear division of the subspace between two properly
 *	ascending bounding values in the array. Values that are out-of-range
 *	in some other way, such as exceeding the value of a subsequent 
 *	element, are flagged with an informational message but still are
 *	coerced into a proper sequence. Note that the box_sizing GenArray
 *	must contain one element more than the box count, and the 
 *	first element must contain 0.0 and the last element must contain 1.0.
 *	
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The box_sizing GenArray may be modified.
 */

/*ARGSUSED*/
static NhlErrorTypes    ManageBoxFractionsArray
#if	NhlNeedProto
	(float *box_fractions, 
	 int count) 
#else
(box_fractions, count)
	float *box_fractions;
	int count;
#endif

{
	int i, first_neg = -1;
	int ret = NhlNOERROR;
			
/*
 * At this point it should be guaranteed that the box_fraction array exists
 * and it is of the correct length.
 * Now check the validity of the explicit sizing fractions. Negative values
 * are not an error, but cause equal divisions between the two surrounding
 * specified values. Non-monotonically increasing positive values, or
 * values greater than 1.0 are an error causing a warning. However, this
 * function modifies incorrect values to make them usable.
 */
	/* deal with first element manually, since in the loop the
	   previous element must be compared */
	/* trial version */

	/* the first value must be 0.0 and the last 1.0 */

	if (box_fractions[0] < 0.0)
		box_fractions[0] = 0.0;
	else if (box_fractions[0] > 0.0) {
		NhlPError(NhlINFO,NhlEUNKNOWN,
			  "Modifying invalid box fraction array element: 0");
		ret = NhlINFO;
		box_fractions[0] = 0.0;
	}
	if (box_fractions[count] < 0.0)
		box_fractions[count] = 1.0;
	else if (box_fractions[count] != 1.0) {
		NhlPError(NhlINFO,NhlEUNKNOWN,
			  "Modifying invalid box fraction array element: %d",
			  count);
		ret = NhlINFO;
		box_fractions[count] = 1.0;
	}

	for (i=1; i<count+1;i++) {
		if (box_fractions[i] < 0.0) {
			if (first_neg == -1)
				first_neg = i;
		}
		else if (first_neg != -1) {
			if (box_fractions[i] > 1.0 ||
			    box_fractions[i] <=
			    box_fractions[first_neg-1]) {
				NhlPError(NhlINFO,NhlEUNKNOWN,
		    "Modifying invalid box fraction array element: %d",i);
				ret = NhlINFO;
				box_fractions[i] = -1.0;
			}
			else {
				CreateIntermediates(box_fractions, 
						    first_neg, i);
				first_neg = -1;
			}
		}
		else if (box_fractions[i] > 1.0 ||
			 (box_fractions[i] <= box_fractions[i-1])) {
			NhlPError(NhlINFO,NhlEUNKNOWN,
		        "Modifying invalid box fraction array element: %d",i);
			ret = NhlINFO;
			box_fractions[i] = -1.0;
			first_neg = i;
		}
	}
	return (ret);
}

/*
 * Function: CreateIntermediates
 *
 * Description:	
 *
 * 	Creates evenly spaced values to replace negative values in the
 * 	item positions array. The start value is the index of the first
 * 	negative value in the chain; the end value is the index of the
 * 	first non-negative value. Since the zeroth value has been dealt with
 *	separately, it is not necessary to test for it here.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: Modifies the contents of the box_fractions GenArray.
 */

static void CreateIntermediates
#if	NhlNeedProto
	(float		*flist,
	 int		start,
	 int		end)
#else
(flist,start,end)
	float		*flist;
	int		start;
	int		end;
#endif
{
	float	spacing;
	int	count;
	int	i;

	count = end - start + 1;
	spacing = (flist[end] - flist[start - 1]) / count;
	for (i=0; i<count-1; i++) {
		flist[start+i] = flist[start-1] + (float) (i+1) * spacing;
	}
}


/*
 * Function:  SetLabels
 *
 * Description:	Calculates the attributes of the LabelBar labels
 *	and creates or sets the child multi-text object that is responsible
 *	for rendering the labels. If in auto-manage mode, inquires the 
 *	bounding box of the multitext object after a preliminary setting of
 *	the attributes, then calls an internal function to
 *	adjust the height and/or justification of the multi-text object in
 *	order to ensure that the labels do not overlap on rotation. 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: Modifies attibutes of the child multi-text object.
 */

/*ARGSUSED*/
static NhlErrorTypes    SetLabels
#if	NhlNeedProto
	(NhlLayer		new, 
	NhlLayer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "lbSetLabels";
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	char buffer[_NhlMAXRESNAMLEN];
	char **labels_p;
	int count; 
	int itmp, max_strlen = 0;
	int i,j,ix;
	float label_offset;
	NhlMTextOrientatonType mtext_orient;
	float label_height, char_space, avail_char_space;
	float base_pos = 0.0, offset = 0.0, increment = 0.0;
	NhlCoord larea;
	float c_frac = 1.0;

		
	if (! lb_p->labels_on)
		return NhlNOERROR;

/*
 * Determine the multitext orientation and the NDC label offset
 */

	if (lb_p->orient == NhlHORIZONTAL) {
		mtext_orient = NhlMTEXT_Y_CONST;
		label_offset = lb_p->label_off * 
			(lb_p->adj_perim.t - lb_p->adj_perim.b);
	}
	else {
		mtext_orient = NhlMTEXT_X_CONST;
		label_offset = lb_p->label_off *
			(lb_p->adj_perim.r - lb_p->adj_perim.l);
	}
#if 0
/*
 * If not in auto-manage mode the label offset should cause the 
 * bar to grow. Set it to 0.0 here so that it will take effect in the
 * AdjustGeometry routine.
 */ 
	if (! lb_p->auto_manage) 
		label_offset = 0.0;
#endif
/*
 * The number of labels varies depending on the label alignment
 */

	count = lb_p->current_label_count;
		
/*
 * If the label stride is greater than 1, find the number of
 * labels to use. If greater than the previous size, reallocate
 * the array used to point to the correct labels.
 */
	
	lb_p->label_draw_count = count;
	labels_p = (NhlString *) lb_p->label_strings->data;
	if (lb_p->label_stride > 1) {
		count = (count % lb_p->label_stride == 0) ?
			count / lb_p->label_stride :  
			count / lb_p->label_stride + 1;
		if (count > lb_p->max_label_stride_count) {
			if (lb_p->stride_labels == NULL) {
				lb_p->stride_labels = (char **) 
					NhlMalloc(count * sizeof(char *));
			}
			else {
				lb_p->stride_labels = (char **)
					NhlRealloc(lb_p->stride_labels,
						   count * sizeof(char *));
			}
			lb_p->max_label_stride_count = count;
		}
		labels_p = lb_p->stride_labels;
		for (i=0,j=0; i<count; i++,j+=lb_p->label_stride) {
			labels_p[i] = 
				((NhlString *)lb_p->label_strings->data)[j];
		}
		lb_p->label_draw_count = count;
	}

/*
 * Now allocate the location array
 */
	if (count > lb_p->max_label_draw_count) {
		if (lb_p->label_locs == NULL) {
			lb_p->label_locs = (float *) 
				NhlMalloc(count * sizeof(float));
		}
		else {
			lb_p->label_locs = (float *)
				NhlRealloc(lb_p->label_locs,
					   count * sizeof(float));
		}
		lb_p->max_label_draw_count = count;
	}

/*
 * Find the size of the longest text string, 
 * then determine a preliminary text size - text angle is accounted for later.
 * If auto-manage is off the size set by the user will override this
 * value.
 */
	for (i=0; i<count; i++) {
		if ((itmp = strlen(labels_p[i])) > max_strlen) {
			max_strlen = itmp;
		}
	}
	
	if (lb_p->label_pos == NhlCENTER) {
		if (lb_p->orient == NhlHORIZONTAL) {
			larea.x = lb_p->labels.r - lb_p->labels.l;
			larea.y = lb_p->adj_bar.t - lb_p->adj_bar.b;
		}
		else {
			larea.x = lb_p->adj_bar.r - lb_p->adj_bar.l;
			larea.y = lb_p->labels.t - lb_p->labels.b;
		}
	}
	else {
		if (lb_p->orient == NhlHORIZONTAL) {
			larea.x = lb_p->labels.r - lb_p->labels.l;
			larea.y = lb_p->labels.t - lb_p->labels.b - 
				label_offset;
		}
		else {
			larea.x = lb_p->labels.r - lb_p->labels.l - 
				label_offset;
			larea.y = lb_p->labels.t - lb_p->labels.b;
		}
	}
		
/*
 * Account for the box_major_ext fraction if centered or label
 * offset is negative
 */
	if (lb_p->label_pos == NhlCENTER || label_offset < 0.0) {
		if (lb_p->label_alignment == NhllbBOXCENTERS) {
			c_frac = lb_p->box_major_ext;
		}
		else {
			c_frac = 1.0 - lb_p->box_major_ext;
		}
	}

	 
	if (lb_p->orient == NhlHORIZONTAL && 
	    lb_p->label_direction == NhlACROSS) {

		char_space = 0.8 * larea.y / (max_strlen + 1.0);
		avail_char_space = c_frac * 0.5 * 
			larea.x / lb_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		switch (lb_p->label_pos) {
		case NhlLEFT:
		case NhlRIGHT:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlBOTTOM:
			lb_p->const_pos = lb_p->labels.t -
				label_height - label_offset;
			break;
		case NhlCENTER:
			lb_p->const_pos = lb_p->adj_bar.b + 
				lb_p->adj_box_size.y / 2.0;
			break;
		case NhlTOP:
			lb_p->const_pos = lb_p->labels.b + 
				label_height + label_offset;
			break;
		}
	}
	else if (lb_p->orient == NhlHORIZONTAL){ /* NhlDOWN or UP */

		/* Set the font height */

		char_space = 0.8 * larea.y / (max_strlen + 1.0);
		avail_char_space = c_frac * 0.5 * 
			larea.x / lb_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		switch (lb_p->label_pos) {
		case NhlLEFT:
		case NhlRIGHT:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlBOTTOM:
			lb_p->const_pos = lb_p->labels.t - 
				label_height - label_offset;
			break;
		case NhlCENTER:
			lb_p->const_pos = lb_p->adj_bar.b + 
				lb_p->adj_box_size.y / 2.0;
			break;
		case NhlTOP:
			lb_p->const_pos = lb_p->labels.b + 
				label_height + label_offset;
			break;
		}
	}
	else if (lb_p->orient == NhlVERTICAL && 
		 lb_p->label_direction == NhlACROSS) {

		char_space = 0.8 * larea.x / (max_strlen + 1.0);
		avail_char_space = c_frac * 0.5 * 
			larea.y / lb_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		switch (lb_p->label_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlLEFT:
			lb_p->const_pos = lb_p->labels.r - 
				label_height - label_offset;
			break;
		case NhlCENTER:
			lb_p->const_pos = lb_p->adj_bar.l + 
				lb_p->adj_box_size.x / 2.0;
			break;
		case NhlRIGHT:
			lb_p->const_pos = lb_p->labels.l + 
				label_height + label_offset;
			break;
		}
	}
	else { /* NhlVERTICAL NhlDOWN or UP */

		char_space = 0.8 * larea.x / (max_strlen + 1.0);
		avail_char_space = c_frac * 0.5 * 
			larea.y / lb_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		switch (lb_p->label_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			e_text = "%s: Invalid label position, defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
		case NhlLEFT:
			lb_p->const_pos = lb_p->labels.r - 
				label_height - label_offset;
			break;
		case NhlCENTER:
			lb_p->const_pos = lb_p->adj_bar.l + 
				lb_p->adj_box_size.x / 2.0;
			break;
		case NhlRIGHT:
			lb_p->const_pos = lb_p->labels.l + 
				label_height + label_offset;
			break;
		}
	}

/*
 * Now find the variable label positions
 */

	if (lb_p->box_sizing == NhllbUNIFORMSIZING) {

		if (lb_p->orient == NhlHORIZONTAL) {
		
			/* position array contains X values */

			base_pos = lb_p->adj_bar.l;
		       
			/* determine offset */
			if (lb_p->label_alignment == NhllbBOXCENTERS)
				offset = lb_p->adj_box_size.x / 2.0;
			else if (lb_p->label_alignment == NhllbINTERIOREDGES)
				offset = lb_p->adj_box_size.x;
			else
				offset = 0;

			increment = lb_p->adj_box_size.x * lb_p->label_stride;
		}
		else if (lb_p->orient == NhlVERTICAL) {
		
			/* position array contains Y values */

			base_pos = lb_p->adj_bar.b;
		       
			/* determine offset */
			if (lb_p->label_alignment == NhllbBOXCENTERS)
				offset = lb_p->adj_box_size.y / 2.0;
			else if (lb_p->label_alignment == NhllbINTERIOREDGES)
				offset = lb_p->adj_box_size.y;
			else
				offset = 0;

			increment = lb_p->adj_box_size.y * lb_p->label_stride;
		}
		for (i=0; i<lb_p->label_draw_count; i++) 
			lb_p->label_locs[i] = base_pos + offset + 
				(float) i * increment;
	}
	else {
		for (i=0; i < lb_p->label_draw_count; i++) {

			ix = i * lb_p->label_stride;
			if (lb_p->label_alignment == NhllbBOXCENTERS)
				lb_p->label_locs[i] = lb_p->box_locs[ix] +
					(lb_p->box_locs[ix+1] -
					  lb_p->box_locs[ix]) / 2.0;
			else if (lb_p->label_alignment == NhllbINTERIOREDGES)
				lb_p->label_locs[i] = lb_p->box_locs[ix+1];
			else
				lb_p->label_locs[i] = lb_p->box_locs[ix];

		}
	}

	if (! lb_p->auto_manage && lb_p->label_height > 0.0) 
		label_height = lb_p->label_height;

	if (init) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Labels");
		subret = NhlVACreate(&(lb_p->labels_id),buffer,
				 NhlmultiTextLayerClass,tnew->base.id,
				 NhlNMtextNumStrings,lb_p->label_draw_count,
				 NhlNMtextStrings,labels_p,
				 NhlNMtextOrientation,mtext_orient,
				 NhlNMtextConstPosF,lb_p->const_pos ,
				 NhlNMtextPosArray,lb_p->label_locs,
				 NhlNtxAngleF,lb_p->label_angle,
				 NhlNtxFont,lb_p->label_font,
				 NhlNtxJust,lb_p->label_just,
				 NhlNtxFontHeightF,label_height,
				 NhlNtxFontAspectF,lb_p->label_aspect,
				 NhlNtxDirection,lb_p->label_direction,
				 NhlNtxConstantSpacingF,
				 lb_p->label_const_spacing,
				 NhlNtxFontColor,lb_p->label_color,
				 NhlNtxFontThicknessF,lb_p->label_thickness,
				 NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error creating MultiText object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	} 
	else {
		subret = NhlVASetValues(lb_p->labels_id,
				    NhlNMtextNumStrings,lb_p->label_draw_count,
				    NhlNMtextStrings,labels_p,
				    NhlNMtextOrientation,mtext_orient,
				    NhlNMtextConstPosF,lb_p->const_pos,
				    NhlNMtextPosArray,lb_p->label_locs,
				    NhlNtxAngleF,lb_p->label_angle,
				    NhlNtxFont,lb_p->label_font,
				    NhlNtxJust,lb_p->label_just,
				    NhlNtxFontHeightF,label_height,
				    NhlNtxFontAspectF,lb_p->label_aspect,
				    NhlNtxDirection,lb_p->label_direction,
				    NhlNtxFontColor,lb_p->label_color,
				    NhlNtxFontThicknessF,lb_p->label_thickness,
				    NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error setting MultiText object values";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}


	if (lb_p->auto_manage || lb_p->label_height <= 0.0) {

		subret = AdjustLabels(lb_p, label_height, avail_char_space, 
				   max_strlen, larea.x, larea.y);
		ret = MIN(ret, subret);
	}

	return (ret);
}

/*
 * Function: AdjustLabels
 *
 * Description:	Adjusts the label height to fit the size it has available. 
 *	This should probably be replaced by an option
 * 	within Multitext itself. The routine tries to ensure that under 
 * 	any rotation, no piece of multitext overlaps another piece of the 
 * 	multi-text. Adjusts the text size based on some primitive heuristics
 * 	to accomplish this goal. Also the label text justification is
 * 	managed to ensure that the label always lines up with its correct box.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: Modifies the text height and justification of the child
 *	multi-text object
 */

#define NhlLB_TRANSITIONANGLE 7.5
static NhlErrorTypes   	AdjustLabels
#if	NhlNeedProto
	(NhlLabelBarLayerPart *lb_p,
	 float		height,
	 float		avail_space,
	 int		max_strlen,
	 float		area_x,
	 float		area_y)
#else
(lb_p, height, avail_space, max_strlen, area_x, area_y)
	NhlLabelBarLayerPart *lb_p;
	float		height;
	float		avail_space;
	int		max_strlen;
	float		area_x;
	float		area_y;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "lbAdjustLabels";
	float tmp, theta1, theta2, theta3, theta4;
	NhlBoundingBox stringBB;
	float w, h;
	float wb, wt, hb, ht;
	float c_angle;
	float t1,t2;
	float theta, ct, st;

/*
 * Get the multitext bounding box.Then figure out the size of an
 * individual (maximum size) string in the multitext. Also figure out
 * the amount of space the string has available
 */
	subret = NhlGetBB(lb_p->labels_id, &stringBB);
	w=stringBB.r-stringBB.l; 	    
	h=stringBB.t-stringBB.b;
	if ((ret = MIN(ret,subret)) < NhlWARNING || w <= 0.0 || h <= 0.0) {
		e_text = "%s: Error getting bounding box";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if (lb_p->orient == NhlHORIZONTAL) {
		wb = area_x / lb_p->label_draw_count;
		wt = wb - area_x + w;
		hb = area_y;
		ht = h;
	}
	else {
		wb = area_x;
		wt = w;
		hb = area_y / lb_p->label_draw_count;
		ht = hb - area_y + h;
	}

/*
 * If labels are centered in the boxes just fit the text in the box
 * then get out.
 */

	if (lb_p->label_pos == NhlCENTER) {
		t1 = wb / wt < hb / ht ?  wb / wt  : hb / ht;
		height *= t1 * 0.8;
		height = MIN(height,avail_space);
		lb_p->label_just = NhlCENTERCENTER;
		lb_p->label_height = height;
		ret = NhlVASetValues(lb_p->labels_id,
				   NhlNtxFontHeightF,lb_p->label_height,
				   NhlNtxJust,lb_p->label_just,
				   NULL);
		return (ret);
	}

/*
 * Get the sin and cos of the label angle - then create some
 * permutations of the angle in order to set the justification properly
 */
	theta = DEGTORAD * lb_p->label_angle;
	ct = fabs(cos(theta));
	st = fabs(sin(theta));
	
	if (ct < 0.01) ct = 0.01;
	if (st < 0.01) st = 0.01;
	
	theta1 = lb_p->label_angle < 360.0 - lb_p->label_angle ?
		lb_p->label_angle : 360.0 - lb_p->label_angle;
	theta2 = fabs(90.0 - lb_p->label_angle);
	theta3 = fabs(180.0 - lb_p->label_angle);
	theta4 = fabs(270.0 - lb_p->label_angle);

/*
 * Modify the text height: the critical angle is the point where text
 * begins to overlap. Outside the critical area the size of the text
 * is adjusted to make it as large as possible with no increase in the
 * size of the labelbar's minor axis. Inside the critical area, the size
 * of the text is reduced using (at the moment) a rather clumsy algorithm,
 * until it reaches a minimum size at the point where the text line is 
 * parallel to the labelbar major axis.
 * Then manage the text justification.
 */
	if (lb_p->orient == NhlHORIZONTAL && 
	    lb_p->label_direction == NhlACROSS) {

		height /= st;
		height = MIN(height, avail_space);
		tmp = height / lb_p->adj_box_size.x > 1.0 ? 1.0 :
			height / lb_p->adj_box_size.x;
 		c_angle = asin(tmp) / DEGTORAD;

		if (theta1 <= c_angle || theta3 <= c_angle) {
			c_angle = MIN(c_angle,45.0);
			t1 = 0.8 * wb / (max_strlen +2);
			t2 = t1 + (st / fabs(sin(DEGTORAD * c_angle))) *
				fabs(0.8 *avail_space - t1);
			height = height < t2 ? height : t2;
		}

		if (theta1 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (theta3 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
		else if (lb_p->label_angle < 90.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPRIGHT;
			}
			else {
				lb_p->label_just = NhlBOTTOMLEFT;
			}
		}
		else if (lb_p->label_angle < 180.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMRIGHT;
			}
			else {
				lb_p->label_just = NhlTOPLEFT;
			}
		}
		else if (lb_p->label_angle < 270.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMLEFT;
			}
			else {
				lb_p->label_just = NhlTOPRIGHT;
			}
		}
		else {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPLEFT;
			}
			else {
				lb_p->label_just = NhlBOTTOMRIGHT;
			}
		}
	}
	else if (lb_p->orient == NhlHORIZONTAL){ /* NhlDOWN or UP */

		height /= ct;
		height = MIN(height, avail_space);
		tmp = height / lb_p->adj_box_size.x > 1.0 ? 1.0 :
			height / lb_p->adj_box_size.x;
 		c_angle = asin(tmp) / DEGTORAD;

		if (theta2 <= c_angle || theta4 <= c_angle) {
			c_angle = MIN(c_angle,45.0);
			t1 = 0.8 * hb / (max_strlen +2);
			t2 = t1 + (ct / fabs(cos(DEGTORAD * c_angle))) *
				fabs(0.8 * avail_space - t1);
			height = MIN(height, t2);
		}

		if (theta2 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta4 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (lb_p->label_angle > 270.0 ||
			 lb_p->label_angle < 90.0) {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else {
			if (lb_p->label_pos == NhlBOTTOM) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
			 
	}
	else if (lb_p->orient == NhlVERTICAL && 
		 lb_p->label_direction == NhlACROSS) {

		height /= ct;
		height = MIN(height, avail_space);
		tmp = height / lb_p->adj_box_size.y > 1.0 ? 1.0 :
			height / lb_p->adj_box_size.y;
 		c_angle = asin(tmp) / DEGTORAD;

		if (theta2 <= c_angle || theta4 <= c_angle) {
			c_angle = MIN(c_angle,45.0);
			t1 = 0.8 * hb / (max_strlen +2);
			t2 = t1 + (ct / fabs(cos(DEGTORAD * c_angle))) *
				fabs(0.8 * avail_space - t1);
			height = MIN(height, t2);
		}

		if (theta2 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lb_p->label_just = NhlTOPCENTER;
			}
		}
		else if (theta4 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlTOPCENTER;
			}
			else {
				lb_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (lb_p->label_angle > 270.0  ||
			 lb_p->label_angle < 90.0) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
			 
	}
	else { /* NhlVERTICAL NhlDOWN or UP */

		height /= st;
		height = MIN(height, avail_space);
		tmp = height / lb_p->adj_box_size.y > 1.0 ? 1.0 :
			height / lb_p->adj_box_size.y;
 		c_angle = asin(tmp) / DEGTORAD;
		if (theta1 <= c_angle || theta3 <= c_angle) {
			c_angle = MIN(c_angle,45.0);
			t1 = 0.8 * wb / (max_strlen +2);
			t2 = t1 + (st / fabs(sin(DEGTORAD * c_angle))) *
				fabs(0.8 * avail_space - t1);
			height = height < t2 ? height : t2;
		}

		if (theta1 <=NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lb_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta3 <= NhlLB_TRANSITIONANGLE) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlCENTERLEFT;
			}
			else {
				lb_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (lb_p->label_angle < 180.0) {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlBOTTOMRIGHT;
			}
			else {
				lb_p->label_just = NhlTOPLEFT;
			}
		}
		else {
			if (lb_p->label_pos == NhlLEFT) {
				lb_p->label_just = NhlTOPLEFT;
			}
			else {
				lb_p->label_just = NhlBOTTOMRIGHT;
			}
		}
	}

/*
 * Set the newly determined text height and justification
 */
	lb_p->label_height = height;
	ret = NhlVASetValues(lb_p->labels_id,
			   NhlNtxFontHeightF,lb_p->label_height,
			   NhlNtxJust,lb_p->label_just,
			   NULL);
	return (ret);
}

/*
 * Function: AdjustGeometry
 *
 * Description: Called after all individual elements of the LabelBar have been
 *	sized in order to ensure that no elements overlap and that the 
 *	overall size of the LabelBar is big enough to accommodate the specified
 *	size of each element in the LabelBar. When auto-manage mode is turned
 *     	off both the width and height of the LabelBar may be expanded; when 
 *	auto-manage is on, only one direction (the major axis) may expand.
 *	Elements of the LabelBar may be moved relative to each other in order
 *	to eliminate overlap. Finally the routine adjusts the LabelBar position
 *	as a whole depending on the setting of the LabelBar justification
 *	resource.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: The bounding box of the LabelBar may be modified, and the
 *	title and label child objects may be repositioned.
 */

/*ARGSUSED*/
static NhlErrorTypes    AdjustGeometry
#if	NhlNeedProto
	(NhlLayer		new, 
	NhlLayer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlLabelBarLayer	tnew = (NhlLabelBarLayer) new;
	NhlLabelBarLayerPart *lb_p = &(tnew->labelbar);
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;
	NhlBoundingBox titleBB;
	NhlBoundingBox labelsBB;
	NhlBoundingBox labelbarBB;
	NhlBoundingBox tmpBB;
	float title_x = lb_p->title_x;
	float title_y = lb_p->title_y;
	float pos_offset = 0.0;
	float obj_offset = 0.0;
	float x_off, y_off;
	float small_axis;
	float minor_off;
	float major_off;
	float center_off;
	int i;

	small_axis = MIN(lb_p->lb_width, lb_p->lb_height);
/*
 * Get the bounding box of the labels, then adjust the box if it
 * overlaps the bar boundary. Next combine the two bounding boxes,
 * and adjust both the bar and the labels to the center of the combined box.
 */
	if (! lb_p->labels_on) {
		labelbarBB.l = lb_p->adj_bar.l;
		labelbarBB.r = lb_p->adj_bar.r;
		labelbarBB.b = lb_p->adj_bar.b;
		labelbarBB.t = lb_p->adj_bar.t;
	}
	else {
		if ((ret1 = NhlGetBB(lb_p->labels_id, &labelsBB)) < NhlWARNING)
			return ret1;
		ret = MIN(ret1,ret);
		
		if (lb_p->orient == NhlHORIZONTAL) {
			
			obj_offset = lb_p->label_off * 
				(lb_p->adj_perim.t - lb_p->adj_perim.b);
			if (lb_p->label_pos == NhlTOP) {
				if (labelsBB.b < 
				    lb_p->adj_bar.t + obj_offset) {
					pos_offset = lb_p->adj_bar.t + 
						obj_offset - labelsBB.b;
				}
			}
			else if (lb_p->label_pos == NhlBOTTOM) {
				if (labelsBB.t > 
				    lb_p->adj_bar.b - obj_offset) {
					pos_offset = lb_p->adj_bar.b - 
						obj_offset - labelsBB.t;
				}
			}
			labelsBB.b += pos_offset;
			labelsBB.t += pos_offset;
		}
		else {
			
			obj_offset = lb_p->label_off * 
				(lb_p->adj_perim.r - lb_p->adj_perim.l);
			if (lb_p->label_pos == NhlRIGHT) {
				if (labelsBB.l < 
				    lb_p->adj_bar.r + obj_offset) {
					pos_offset = lb_p->adj_bar.r + 
						obj_offset - labelsBB.l;
				}
			}
			else if (lb_p->label_pos == NhlLEFT) {
				if (labelsBB.r > 
				    lb_p->adj_bar.l - obj_offset) {
					pos_offset = lb_p->adj_bar.l - 
						obj_offset - labelsBB.r;
				}
				
			}
			labelsBB.l += pos_offset;
			labelsBB.r += pos_offset;
		}
		
		tmpBB.l = MIN(labelsBB.l, lb_p->adj_bar.l);
		tmpBB.r = MAX(labelsBB.r, lb_p->adj_bar.r);
		tmpBB.b = MIN(labelsBB.b, lb_p->adj_bar.b);
		tmpBB.t = MAX(labelsBB.t, lb_p->adj_bar.t);
		labelbarBB.l = MIN(tmpBB.l, lb_p->labels.l);
		labelbarBB.r = MAX(tmpBB.r, lb_p->labels.r);
		labelbarBB.b = MIN(tmpBB.b, lb_p->labels.b);
		labelbarBB.t = MAX(tmpBB.t, lb_p->labels.t);

		if (lb_p->orient == NhlHORIZONTAL) {

			center_off = (labelbarBB.t - tmpBB.t + 
				      labelbarBB.b - tmpBB.b) / 2.0;
			labelsBB.b += center_off;
			labelsBB.t += center_off;
			lb_p->adj_bar.b += center_off;
			lb_p->adj_bar.t += center_off;
			pos_offset += center_off;
		}
		else {

			center_off = (labelbarBB.r - tmpBB.r +
				      labelbarBB.l - tmpBB.l) / 2.0;
			labelsBB.l += center_off;
			labelsBB.r += center_off;
			lb_p->adj_bar.l += center_off;
			lb_p->adj_bar.r += center_off;
			pos_offset += center_off;
		}		
		
	}

/*
 * handle the title
 */
	if (! lb_p->title_on || lb_p->max_title_ext <= 0.0) {
		titleBB.l = labelbarBB.l;
		titleBB.r = labelbarBB.r;
		titleBB.b = labelbarBB.b;
		titleBB.t = labelbarBB.t;
	}
	else {
		if ((ret1 = NhlGetBB(lb_p->title_id, &titleBB)) < NhlWARNING)
			return ret1;
		ret = MIN(ret1,ret);
		
		
		/*
		 * Adjust for the title: 
		 * first move it out of the way of the labelbar, 
		 * then adjust for possibly larger justification area.
		 */
		
		
		if (lb_p->title_pos == NhlBOTTOM || lb_p->title_pos == NhlTOP)
			obj_offset = lb_p->title_off *
				(lb_p->adj_perim.t - lb_p->adj_perim.b);
		else
			obj_offset = lb_p->title_off *
				(lb_p->adj_perim.r - lb_p->adj_perim.l);
		if (lb_p->title_pos == NhlBOTTOM &&
		    titleBB.t > labelbarBB.b - obj_offset) {
			title_y -= titleBB.t + obj_offset - labelbarBB.b;
		}
		else if (lb_p->title_pos == NhlTOP &&
			 titleBB.b < labelbarBB.t + obj_offset) {
			title_y += labelbarBB.t + obj_offset - titleBB.b;
		}
		else if (lb_p->title_pos == NhlLEFT &&
			 titleBB.r > labelbarBB.l - obj_offset) {
			title_x -= titleBB.r + obj_offset - labelbarBB.l;
		}
		else if (lb_p->title_pos == NhlRIGHT &&
			 titleBB.l < labelbarBB.r + obj_offset) {
			title_x += labelbarBB.r + obj_offset - titleBB.l;
		}
		
		if (lb_p->title_pos == NhlTOP
		    || lb_p->title_pos == NhlBOTTOM) {
			switch (lb_p->title_just) {
			case NhlBOTTOMLEFT:
			case NhlCENTERLEFT:
			case NhlTOPLEFT:
				title_x = labelbarBB.l;
				break;
			case NhlBOTTOMCENTER:
			case NhlCENTERCENTER:
			case NhlTOPCENTER:
			default:
				title_x = labelbarBB.l + 
					(labelbarBB.r - labelbarBB.l) / 2.0;
				break;
			case NhlBOTTOMRIGHT:
			case NhlCENTERRIGHT:
			case NhlTOPRIGHT:
				title_x = labelbarBB.r;
				break;
			}
		}
		else if (lb_p->title_pos == NhlLEFT
			 || lb_p->title_pos == NhlRIGHT) {
			switch (lb_p->title_just) {
			case NhlBOTTOMLEFT:
			case NhlBOTTOMCENTER:
			case NhlBOTTOMRIGHT:
				title_y = labelbarBB.b;
				break;
			case NhlCENTERLEFT:
			case NhlCENTERCENTER:
			case NhlCENTERRIGHT:
			default:
				title_y = labelbarBB.b + 
					(labelbarBB.t - labelbarBB.b) / 2.0;
				break;
			case NhlTOPLEFT:
			case NhlTOPCENTER:
			case NhlTOPRIGHT:
				title_y = labelbarBB.t;
				break;
			}
		}
		titleBB.l += title_x - lb_p->title_x;
		titleBB.r += title_x - lb_p->title_x;
		titleBB.b += title_y - lb_p->title_y;
		titleBB.t += title_y - lb_p->title_y;
		titleBB.l = MIN(titleBB.l, lb_p->title.l);
		titleBB.r = MAX(titleBB.r, lb_p->title.r);
		titleBB.b = MIN(titleBB.b, lb_p->title.b);
		titleBB.t = MAX(titleBB.t, lb_p->title.t);
	}
/*
 * Determine the real perimeter and set the view accordingly
 */

	lb_p->real_perim.l = MIN(labelbarBB.l - lb_p->margin.l * small_axis, 
				 titleBB.l - lb_p->margin.l * small_axis);
	lb_p->real_perim.r = MAX(labelbarBB.r + lb_p->margin.r * small_axis, 
				 titleBB.r + lb_p->margin.r * small_axis);
	lb_p->real_perim.b = MIN(labelbarBB.b - lb_p->margin.b * small_axis, 
				 titleBB.b - lb_p->margin.b * small_axis);
	lb_p->real_perim.t = MAX(labelbarBB.t + lb_p->margin.t * small_axis, 
				 titleBB.t + lb_p->margin.t * small_axis);

/*
 * Adjust position based on the justification
 */
	switch (lb_p->just) {

	case NhlBOTTOMLEFT:
		x_off = lb_p->real_perim.l - lb_p->perim.l;
		y_off = lb_p->real_perim.b - lb_p->perim.b;
		break;
	case NhlCENTERLEFT:
		x_off = lb_p->real_perim.l - lb_p->perim.l;
		y_off = lb_p->real_perim.b + 
			(lb_p->real_perim.t - lb_p->real_perim.b)/2.0 -
				(lb_p->lb_y - lb_p->lb_height/2.0);
		break;
	case NhlTOPLEFT:
		x_off = lb_p->real_perim.l - lb_p->perim.l;
		y_off = lb_p->real_perim.t - lb_p->perim.t;
		break;
	case NhlBOTTOMCENTER:
		x_off = lb_p->real_perim.l + 
			(lb_p->real_perim.r - lb_p->real_perim.l)/2.0 -
				(lb_p->lb_x + lb_p->lb_width/2.0);
		y_off = lb_p->real_perim.b - lb_p->perim.b;
		break;
	case NhlCENTERCENTER:
	default:
		x_off = lb_p->real_perim.l + 
			(lb_p->real_perim.r - lb_p->real_perim.l)/2.0 -
				(lb_p->lb_x + lb_p->lb_width/2.0);
		y_off = lb_p->real_perim.b + 
			(lb_p->real_perim.t - lb_p->real_perim.b)/2.0 -
				(lb_p->lb_y - lb_p->lb_height/2.0);
		break;
	case NhlTOPCENTER:
		x_off = lb_p->real_perim.l + 
			(lb_p->real_perim.r - lb_p->real_perim.l)/2.0 -
				(lb_p->lb_x + lb_p->lb_width/2.0);
		y_off = lb_p->real_perim.t - lb_p->perim.t;
		break;
	case NhlBOTTOMRIGHT:
		x_off = lb_p->real_perim.r - lb_p->perim.r;
		y_off = lb_p->real_perim.b - lb_p->perim.b;
		break;
	case NhlCENTERRIGHT:
		x_off = lb_p->real_perim.r - lb_p->perim.r;
		y_off = lb_p->real_perim.b + 
			(lb_p->real_perim.t - lb_p->real_perim.b)/2.0 -
				(lb_p->lb_y - lb_p->lb_height/2.0);
		break;
	case NhlTOPRIGHT:
		x_off = lb_p->real_perim.r - lb_p->perim.r;
		y_off = lb_p->real_perim.t - lb_p->perim.t;
	}

	minor_off = (lb_p->orient == NhlHORIZONTAL) ? y_off : x_off;
	major_off = (lb_p->orient == NhlHORIZONTAL) ? x_off : y_off;
/*
 * Adjust the perimeter
 */

	lb_p->real_perim.l -= x_off;
	lb_p->real_perim.r -= x_off;
	lb_p->real_perim.b -= y_off;
	lb_p->real_perim.t -= y_off;
/*
 * Adjust the bar position
 */

	lb_p->adj_bar.l -= x_off;
	lb_p->adj_bar.r -= x_off;
	lb_p->adj_bar.b -= y_off;
	lb_p->adj_bar.t -= y_off;
	for (i=0; i<lb_p->box_count+1; i++) {
		lb_p->box_locs[i] -= major_off;
	}
/*
 * Adjust the labels position
 */

	if (lb_p->labels_on) {
		for (i=0; i<lb_p->label_draw_count; i++) {
			lb_p->label_locs[i] -= major_off;
		}
		
		if ((ret1 = NhlVASetValues(lb_p->labels_id,
					 NhlNMtextConstPosF,
					 lb_p->const_pos + 
					 pos_offset - minor_off,
					 NhlNMtextPosArray,lb_p->label_locs,
					 NULL)) < NhlWARNING)
			return (ret1);
		ret = MIN(ret1,ret);
	}
/*
 * Set the title position
 */

	if (lb_p->title_on && lb_p->max_title_ext > 0.0) {
		title_x -= x_off;
		title_y -= y_off;
		if ((ret1 = NhlVASetValues(lb_p->title_id,
					 NhlNtxPosXF, title_x,
					 NhlNtxPosYF, title_y,
					 NULL)) < NhlWARNING)
			return (ret1);
		ret = MIN(ret1,ret);
	}

	_NhlInternalSetView((NhlViewLayer)tnew,
			    lb_p->real_perim.l, lb_p->real_perim.t,
			    lb_p->real_perim.r - lb_p->real_perim.l,
			    lb_p->real_perim.t - lb_p->real_perim.b,
			    False);
	return (ret);

}

/*
 * Function:	LabelBarGetValues
 *
 * Description:	Retrieves the current setting of one or more LabelBar
 *	resources. This routine only retrieves resources that require 
 *	special methods that the generic GetValues method cannot handle. 
 *	For now this means the GenArray resources. Note that space is 
 *	allocated; the user is responsible for freeing this space.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 *	Memory is allocated when the following resources are retrieved:
 *		NhlNlbFillPatterns
 *		NhlNlbFillColors
 *		NhlNlbFillScales
 *		NhlNlbLabelStrings
 *		NhlNlbBoxFractions
 *              NhlNlbTitleString
 *	The caller is responsible for freeing this memory.
 */

static NhlErrorTypes	LabelBarGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	NhlLayer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
	NhlLabelBarLayer lbl = (NhlLabelBarLayer)l;
	NhlLabelBarLayerPart *lb_p = &(lbl->labelbar);
	NhlGenArray ga;
	char *e_text;
	int i, count = 0;
	char *type = "";
	
	for( i = 0; i< num_args; i++ ) {

		ga = NULL;
		if(args[i].quark == Qfill_patterns) {
			ga = lb_p->fill_patterns;
			count = lb_p->mono_fill_pattern ? 1 : lb_p->box_count;
			type = NhlNlbFillPatterns;
		}
		else if (args[i].quark == Qfill_colors) {
			ga = lb_p->fill_colors;
			count = lb_p->mono_fill_color ? 1 : lb_p->box_count;
			type = NhlNlbFillColors;
		}
		else if (args[i].quark == Qfill_scales) {
			ga = lb_p->fill_scales;
			count = lb_p->mono_fill_scale ? 1 : lb_p->box_count;
			type = NhlNlbFillScales; 
		}
		else if (args[i].quark == Qlabel_strings) {
			ga = lb_p->label_strings;
			count = lb_p->current_label_count;
			type = NhlNlbLabelStrings;
		}
		else if (args[i].quark == Qbox_fractions) {
			ga = lb_p->box_fractions;
			count = lb_p->box_count+1;
			type = NhlNlbBoxFractions;
		} 
		else if(args[i].quark == Qtitle_string) {
			if(lb_p->title_string != NULL) {
				(*(NhlString*)args[i].value.ptrval) = (NhlString)
					NhlMalloc(strlen(lb_p->title_string) +1);
				strcpy((*(NhlString*)args[i].value.ptrval),lb_p->title_string);
			} else {
				(*(NhlString*)args[i].value.ptrval) = NULL;
			}
		}
		if (ga != NULL) {
			if ((ga = GenArraySubsetCopy(ga, count)) == NULL) {
				e_text = "%s: error copying %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  "LabelBarGetValues",type);
				return NhlFATAL;
			}
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
	}

	return(NhlNOERROR);

}

/*
 * Function:  GenArraySubsetCopy
 *
 * Description:	Since the internal GenArrays maintained by the LabelBar object
 *	may be bigger than the size currently in use, this function allows
 *	a copy of only a portion of the array to be created. This is for
 * 	use by the GetValues routine when returning GenArray resources to
 *	the usr level. The array is assumed to be valid. The only pointer 
 *	type arrays that the routine can handle are NhlString arrays.
 *	Note: this might be another candidate for inclusion as a global 
 *	routine.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

static NhlGenArray GenArraySubsetCopy
#if	NhlNeedProto
	(NhlGenArray	ga,
	int		length)
#else
(ga,length)
	NhlGenArray	ga;
	int		length;
#endif
{
	NhlGenArray gto;

	if (length > ga->num_elements)
		return NULL;

	if ((gto = _NhlCopyGenArray(ga,False)) == NULL) {
		return NULL;
	}
	if ((gto->data = (NhlPointer) NhlMalloc(length * ga->size)) == NULL) {
		return NULL;
	}
	if (ga->typeQ != Qstring) {
		memcpy((void *)gto->data, (Const void *) ga->data, 
		       length * ga->size);
	}
	else {
		NhlString *cfrom = (NhlString *) ga->data;
		NhlString *cto = (NhlString *) gto->data;
		int i;
		for (i=0; i<length; i++) {
			if ((*cto = (char *) 
			     NhlMalloc(strlen(*cfrom)+1)) == NULL) {
				return NULL;
			}
			strcpy(*cto++,*cfrom++);
		}
	}
	gto->num_elements = length;
	return gto;
			
}


/*
 * Function:	LabelBarDraw
 *
 * Description:  Activates parent workstation, then draws the LabelBar 
 *		 perimeter and LabelBar boxes. Deactivates the workstation
 *		 and calls the draw functions for the LabelBar title
 *		 text object and the LabelBar labels multitext object.
 *
 * In Args: the instance to be drawn
 *
 * Out Args: NONE
 *
 * Return Values: Error conditions
 *
 * Side Effects: 
 */

static NhlErrorTypes    LabelBarDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
	NhlLayer 	layer;
#endif
{
	NhlLabelBarLayer lbl = (NhlLabelBarLayer) layer;
	NhlLabelBarLayerPart *lb_p = &(lbl->labelbar);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *e_text;
	char *entry_name = "LabelBarDraw";
	float xpoints[5];
	float ypoints[5];
	int i;
	int fill_color, fill_pattern;
	int *colors, *patterns;
	float fill_scale, *fill_scales;
	float frac, dist;

	if (! lb_p->labelbar_on)
		return(ret);

	if (lbl->view.use_segments && ! lb_p->new_draw_req) {
                subret = _NhlActivateWorkstation(lbl->base.wkptr);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDrawSegment(lb_p->trans_dat,
				_NhlWorkstationId(lbl->base.wkptr));
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDeactivateWorkstation(lbl->base.wkptr);
		return MIN(subret,ret);
	}
	lb_p->new_draw_req = False;

	subret = _NhlActivateWorkstation(lbl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	if (lbl->view.use_segments) {
		if (lb_p->trans_dat != NULL)
			_NhlDeleteViewSegment(layer, lb_p->trans_dat);
		if ((lb_p->trans_dat = _NhlNewViewSegment(layer)) == NULL) {
			e_text = "%s: error opening segment";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
		_NhlStartSegment(lb_p->trans_dat);
	}

/* first draw the perimeter: it may have a solid background */

	if (lb_p->perim_on) {

		xpoints[0] = lb_p->real_perim.l;
		ypoints[0] = lb_p->real_perim.b;
		xpoints[1] = lb_p->real_perim.r;;
		ypoints[1] = lb_p->real_perim.b;
		xpoints[2] = lb_p->real_perim.r;
		ypoints[2] = lb_p->real_perim.t;
		xpoints[3] = lb_p->real_perim.l;
		ypoints[3] = lb_p->real_perim.t;
		xpoints[4] = lb_p->real_perim.l;
		ypoints[4] = lb_p->real_perim.b;

		NhlVASetValues(lbl->base.wkptr->base.id,
			     NhlNwkDrawEdges, 1,
			     NhlNwkEdgeDashPattern, lb_p->perim_dash_pattern,
			     NhlNwkEdgeThicknessF, lb_p->perim_thickness,
			     NhlNwkEdgeDashSegLenF, lb_p->perim_dash_length,
			     NhlNwkEdgeColor, lb_p->perim_color,
			     NhlNwkFillColor, lb_p->perim_fill_color,
			     NhlNwkFillIndex, lb_p->perim_fill,
			     NULL);
			
		_NhlSetFillInfo(lbl->base.wkptr, layer);
		_NhlWorkstationFill(lbl->base.wkptr,
				    xpoints,ypoints,5);
			
	}

/*
 * Set the values that remain constant for all boxes
 */

	NhlVASetValues(lbl->base.wkptr->base.id,
		     NhlNwkDrawEdges, lb_p->box_line_on,
		     NhlNwkEdgeDashPattern, lb_p->box_line_dash_pattern,
		     NhlNwkEdgeThicknessF, lb_p->box_line_thickness,
		     NhlNwkEdgeDashSegLenF, lb_p->box_line_dash_length,
		     NhlNwkEdgeColor, lb_p->box_line_color,
		     NhlNwkFillLineThicknessF, lb_p->fill_line_thickness,
		     NhlNwkFillBackground, lb_p->fill_background,
		     NULL);
				     
/* 
 * Draw the boxes
 */
	frac = (1.0 - lb_p->box_major_ext) / 2.0;
	colors = (int *)lb_p->fill_colors->data;
	patterns = (int *)lb_p->fill_patterns->data;
	fill_scales = (float *) lb_p->fill_scales->data;

	if (lb_p->orient == NhlHORIZONTAL) {

		ypoints[0] = lb_p->adj_bar.b;
		ypoints[1] = lb_p->adj_bar.b;
		ypoints[2] = lb_p->adj_bar.t;
		ypoints[3] = lb_p->adj_bar.t;
		ypoints[4] = lb_p->adj_bar.b;
		for (i=0; i<lb_p->box_count; i++) {
			dist = lb_p->box_locs[i+1] - lb_p->box_locs[i];
			xpoints[0] = lb_p->box_locs[i] + dist * frac;
			xpoints[1] = lb_p->box_locs[i+1] - dist * frac;
			xpoints[2] = xpoints[1];
			xpoints[3] = xpoints[0];
			xpoints[4] = xpoints[0];
			
			if (lb_p->mono_fill_color)
				fill_color = colors[0];
			else
				fill_color = colors[i];

			if (lb_p->mono_fill_pattern)
				fill_pattern = patterns[0];
			else
				fill_pattern = patterns[i];
			
			if (lb_p->mono_fill_scale)
				fill_scale = fill_scales[0];
			else
				fill_scale = fill_scales[i];

			NhlVASetValues(lbl->base.wkptr->base.id,
				     NhlNwkFillIndex, fill_pattern,
				     NhlNwkFillColor, fill_color,
				     NhlNwkFillScaleFactorF, fill_scale,
				     NULL);
			
			_NhlSetFillInfo(lbl->base.wkptr, layer);
			_NhlWorkstationFill(lbl->base.wkptr,
					    xpoints,ypoints,5);
			
		}
	}
	else {
		xpoints[0] = lb_p->adj_bar.l;
		xpoints[1] = lb_p->adj_bar.r;
		xpoints[2] = lb_p->adj_bar.r;
		xpoints[3] = lb_p->adj_bar.l;
		xpoints[4] = lb_p->adj_bar.l;
		for (i=0; i< lb_p->box_count; i++) {
			dist = lb_p->box_locs[i+1] - lb_p->box_locs[i];
			ypoints[0] = lb_p->box_locs[i] + dist * frac;
			ypoints[1] = ypoints[0];
			ypoints[2] = lb_p->box_locs[i+1] - dist * frac;
			ypoints[3] = ypoints[2];
			ypoints[4] = ypoints[0];

			
			if (lb_p->mono_fill_color)
				fill_color = colors[0];
			else
				fill_color = colors[i];

			if (lb_p->mono_fill_pattern)
				fill_pattern = patterns[0];
			else
				fill_pattern = patterns[i];
			
			if (lb_p->mono_fill_scale)
				fill_scale = fill_scales[0];
			else
				fill_scale = fill_scales[i];

			NhlVASetValues(lbl->base.wkptr->base.id,
				     NhlNwkFillIndex, fill_pattern,
				     NhlNwkFillColor, fill_color,
				     NhlNwkFillScaleFactorF, fill_scale,
				     NULL);
			
			_NhlSetFillInfo(lbl->base.wkptr, layer);
			_NhlWorkstationFill(lbl->base.wkptr,
					    xpoints,ypoints,5);
			
		}
	}

	if (lbl->view.use_segments) {

		if (lb_p->title_on && lb_p->max_title_ext > 0.0)
			_NhlSegDraw(_NhlGetLayer(lb_p->title_id));
		
		if (lb_p->labels_on )
			_NhlSegDraw(_NhlGetLayer(lb_p->labels_id));

		_NhlEndSegment();
		_NhlDeactivateWorkstation(lbl->base.wkptr);
	}
	else {
		_NhlDeactivateWorkstation(lbl->base.wkptr);

		if (lb_p->title_on && lb_p->max_title_ext > 0.0)
			NhlDraw(lb_p->title_id);
		
		if (lb_p->labels_on )
			NhlDraw(lb_p->labels_id);
	}

	return(ret);
}

/*
 * Function:	LabelBarClassInitialize
 *
 * Description: Initializes the child classes, TextItem and MultiText
 *              Gets Quark values for the array data types.
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error condition
 *
 * Side Effects: 	NONE
 */
static NhlErrorTypes    LabelBarClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{
        _NhlEnumVals   labelalignmentlist[] = {
	{NhllbBOXCENTERS, "boxcenters"},
	{NhllbINTERIOREDGES, "interioredges"},
	{NhllbEXTERNALEDGES, "externaledges"}
        };

        _NhlEnumVals   boxsizinglist[] = {
        {NhllbUNIFORMSIZING,	"uniformsizing"},
        {NhllbEXPLICITSIZING,	"explicitsizing"},
        };

	_NhlRegisterEnumType(NhlTlbLabelAlignmentMode,labelalignmentlist,
			     NhlNumber(labelalignmentlist));
	_NhlRegisterEnumType(NhlTlbBoxSizingMode,boxsizinglist,
			     NhlNumber(boxsizinglist));

	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qfill_patterns = NrmStringToQuark(NhlNlbFillPatterns);
	Qfill_colors = NrmStringToQuark(NhlNlbFillColors);
	Qfill_scales = NrmStringToQuark(NhlNlbFillScales);
	Qlabel_strings = NrmStringToQuark(NhlNlbLabelStrings);
	Qbox_fractions = NrmStringToQuark(NhlNlbBoxFractions);

	_NhlInitializeLayerClass(NhltextItemLayerClass);
	_NhlInitializeLayerClass(NhlmultiTextLayerClass);

	return(NhlNOERROR);	
}

/*
 * Function:	LabelBarDestroy
 *
 * Description: Frees all dynamically allocated memory
 *
 * In Args:	NhlLayer inst	instance of LabelBar
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */

static NhlErrorTypes    LabelBarDestroy
#if	NhlNeedProto
(NhlLayer  inst)
#else
(inst)
	NhlLayer	inst;
#endif
{
	NhlLabelBarLayer tinst = (NhlLabelBarLayer) inst;
	NhlLabelBarLayerPart *lb_p = &(tinst->labelbar);
	
	NhlFree(lb_p->gks_colors);
	if (lb_p->stride_labels != NULL)
		NhlFree(lb_p->stride_labels);
	if (lb_p->label_locs != NULL)
		NhlFree(lb_p->label_locs);
	if (lb_p->box_locs != NULL)
		NhlFree(lb_p->box_locs);

	NhlFreeGenArray(lb_p->label_strings);
	NhlFreeGenArray(lb_p->fill_colors);
	NhlFreeGenArray(lb_p->fill_patterns);
	NhlFreeGenArray(lb_p->fill_scales);
	NhlFreeGenArray(lb_p->box_fractions);

	if (lb_p->labels_id >=0)
		NhlDestroy(lb_p->labels_id);

	if (lb_p->title_string != NULL) {
		NhlFree(lb_p->title_string);
	}
	if (lb_p->title_id >=0)
		NhlDestroy(lb_p->title_id);

	return(NhlNOERROR);
}
