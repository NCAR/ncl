/*
 *      $Id: Legend.c,v 1.2 1993-10-19 17:51:23 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Legend.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	
 *		
 *		Creates and manages a Legend
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/LegendP.h>
#include <ncarg/hlu/Workstation.h>

/* default pattern list */

#if 0
static int def_patterns[] = { 
	1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
#endif
static int def_colors[] = { 
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

/* SUPPRESS 112 */

#define DEFSTRING "NOTHING"
#define DEGTORAD 0.017453293

static NhlResource resources[] = { 

{NhlNlgLegend, NhlClgLegend, NhlTInteger,
	 sizeof(int), NhlOffset(LegendLayerRec,legend.legend_on),
	 NhlTImmediate,(NhlPointer) 1}, 
{NhlNlgOrientation, NhlClgOrientation, NhlTOrientation,
	 sizeof(NhlOrientation), NhlOffset(LegendLayerRec,legend.orient),
	 NhlTImmediate,(NhlPointer) NhlVERTICAL},
{NhlNlgJustification, NhlClgJustification, NhlTJustification, 
	 sizeof(NhlJustification),
	 NhlOffset(LegendLayerRec,legend.just),
	 NhlTImmediate,(NhlPointer)NhlBOTTOMLEFT},
{NhlNlgBoxMajorExtentF, NhlClgBoxMajorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.box_major_ext),
	 NhlTString,"0.5"},
{NhlNlgBoxMinorExtentF, NhlClgBoxMinorExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.box_minor_ext),
	 NhlTString,"0.6"},
{NhlNlgBoxCount, NhlClgBoxCount, NhlTInteger,
	 sizeof(int), NhlOffset(LegendLayerRec,legend.box_count),
	 NhlTImmediate,(NhlPointer) 16},
{NhlNlgBoxSizing, NhlClgBoxSizing, NhlTInteger,
	 sizeof(int), NhlOffset(LegendLayerRec,legend.box_sizing),
	 NhlTImmediate,(NhlPointer) NhlLG_UNIFORMSIZING},
{NhlNlgBoxBackground, NhlClgBoxBackground, NhlTInteger,
	 sizeof(int), NhlOffset(LegendLayerRec,legend.box_background),
	 NhlTImmediate,(NhlPointer) -1},

{NhlNlgAutoManage, NhlClgAutoManage, NhlTBoolean,
	 sizeof(NhlBoolean), NhlOffset(LegendLayerRec,legend.auto_manage),
	 NhlTImmediate,(NhlPointer) True},
{NhlNlgMaxLabelAngleAdditionF, NhlClgMaxLabelAngleAdditionF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.label_angle_add),
	 NhlTString,"0.15"},
{NhlNlgLabelOffsetF, NhlClgLabelOffsetF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.label_off),
	 NhlTString,"0.0"},
{NhlNlgTitleOffsetF, NhlClgTitleOffsetF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.title_off),
	 NhlTString,"0.03"},
{NhlNlgLeftMarginF, NhlClgLeftMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.margin.l),
	 NhlTString,"0.05"},
{NhlNlgLeftMarginF, NhlClgLeftMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.margin.l),
	 NhlTString,"0.05"},
{NhlNlgRightMarginF, NhlClgRightMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.margin.r),
	 NhlTString,"0.05"},
{NhlNlgBottomMarginF, NhlClgBottomMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.margin.b),
	 NhlTString,"0.05"},
{NhlNlgTopMarginF, NhlClgTopMarginF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.margin.t),
	 NhlTString,"0.05"},

	
{NhlNlgItemIndexes, NhlClgItemIndexes, NhlTGenArray,
	 sizeof(NhlPointer), NhlOffset(LegendLayerRec,legend.item_indexes),
	 NhlTImmediate,(NhlPointer) NULL},
{NhlNlgItemStrings, NhlClgItemStrings, NhlTGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(LegendLayerRec,legend.item_strings),
	 NhlTImmediate,(NhlPointer) NULL},
{NhlNlgMonoItemType, NhlClgMonoItemType, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(LegendLayerRec,legend.mono_item_type),
	 NhlTImmediate,(NhlPointer) True},
{NhlNlgItemTypes, NhlClgItemTypes, NhlTGenArray,
	 sizeof(NhlPointer), NhlOffset(LegendLayerRec,legend.item_types),
	 NhlTImmediate,(NhlPointer) NULL},
{NhlNlgMonoItemColor, NhlClgMonoItemColor, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(LegendLayerRec,legend.mono_item_color),
	 NhlTImmediate,(NhlPointer) False},
{NhlNlgItemColors, NhlClgItemColors, NhlTGenArray,
	 sizeof(NhlPointer), NhlOffset(LegendLayerRec,legend.item_colors),
	 NhlTImmediate,(NhlPointer) NULL},
{NhlNlgMonoItemThickness, NhlClgMonoItemThickness, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(LegendLayerRec,legend.mono_item_thickness),
	 NhlTImmediate,(NhlPointer) True},
{NhlNlgItemThicknesses, NhlClgItemThicknesses, NhlTGenArray,
	 sizeof(NhlPointer), NhlOffset(LegendLayerRec,legend.item_thicknesses),
	 NhlTImmediate,(NhlPointer) NULL},
{NhlNlgMonoItemTextHeight, NhlClgMonoItemTextHeight, NhlTBoolean,
	 sizeof(NhlBoolean), 
	 NhlOffset(LegendLayerRec,legend.mono_item_text_height),
	 NhlTImmediate,(NhlPointer) True},
{NhlNlgItemTextHeights, NhlClgItemTextHeights, NhlTGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(LegendLayerRec,legend.item_text_heights),
	 NhlTImmediate,(NhlPointer) NULL },
{NhlNlgLabelStrings, NhlClgLabelStrings, NhlTGenArray,
	 sizeof(NhlPointer), 
	 NhlOffset(LegendLayerRec,legend.label_strings),
	 NhlTImmediate,(NhlPointer) NULL},
{NhlNlgBoxFractions, NhlClgBoxFractions, NhlTFloatPtr,
	 sizeof(float *), NhlOffset(LegendLayerRec,legend.box_fractions),
	 NhlTImmediate,(NhlPointer) NULL },
	
{NhlNlgDrawLabels, NhlClgDrawLabels, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.labels_on),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgLabelPosition, NhlClgLabelPosition, NhlTPosition, 
	 sizeof(NhlPosition), NhlOffset(LegendLayerRec,legend.label_pos),
	 NhlTImmediate,(NhlPointer) NhlRIGHT},
{NhlNlgLabelAngleF, NhlClgLabelAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(LegendLayerRec,legend.label_angle),
	 NhlTString,"0.0"},
{NhlNlgLabelAlignment, NhlClgLabelAlignment, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.label_alignment),
	 NhlTImmediate,(NhlPointer) NhlLG_BOXCENTERS},
{NhlNlgLabelDirection,NhlClgLabelDirection,NhlTTextDirection,
	 sizeof(TextDirection),
	 NhlOffset(LegendLayerRec,legend.label_direction),
	 NhlTImmediate,(NhlPointer)ACROSS},
{NhlNlgLabelJust, NhlClgLabelJust, NhlTJustification, sizeof(NhlJustification),
	 NhlOffset(LegendLayerRec,legend.label_just),
	 NhlTImmediate,(NhlPointer)NhlCENTERCENTER},
{NhlNlgLabelFont, NhlClgLabelFont, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.label_font),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgLabelFontColor, NhlClgLabelFontColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.label_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgLabelFontHeightF, NhlClgLabelFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(LegendLayerRec,legend.label_height),
	 NhlTString,"0.02"},
{NhlNlgLabelFontAspectF, NhlClgLabelFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(LegendLayerRec,legend.label_aspect),
	 NhlTString,"1.0"},
{NhlNlgLabelFontThicknessF, NhlClgLabelFontThicknessF, NhlTFloat, 
	 sizeof(float), NhlOffset(LegendLayerRec,legend.label_thickness),
	 NhlTString,"1.0"},
{NhlNlgLabelFontQuality, NhlClgLabelFontQuality, NhlTFQuality, 
	 sizeof(FontQuality), 
	 NhlOffset(LegendLayerRec,legend.label_quality),
	 NhlTImmediate,(NhlPointer) HIGH},
{NhlNlgLabelConstantSpacingF, NhlClgLabelConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LegendLayerRec,legend.label_const_spacing),
	 NhlTString,"0.0"},
{NhlNlgLabelFuncCode, NhlClgLabelFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(LegendLayerRec,legend.label_func_code),
	 NhlTString,":"},
{NhlNlgLabelStride, NhlClgLabelStride, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.label_stride),
	 NhlTImmediate,(NhlPointer) 1},

{NhlNlgTitleString, NhlClgTitleString, NhlTString, 
	 sizeof(char *), NhlOffset(LegendLayerRec,legend.title_string),
	 NhlTImmediate,DEFSTRING},
{NhlNlgDrawTitle, NhlClgDrawTitle, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.title_on),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgTitlePosition, NhlClgTitlePosition, NhlTInteger, 
	 sizeof(NhlPosition), NhlOffset(LegendLayerRec,legend.title_pos),
	 NhlTImmediate,(NhlPointer) NhlTOP},
{NhlNlgMaxTitleExtentF, NhlClgMaxTitleExtentF, NhlTFloat,
	 sizeof(float), NhlOffset(LegendLayerRec,legend.max_title_ext),
	 NhlTString,"0.15"},
{NhlNlgTitleAngleF, NhlClgTitleAngleF, NhlTFloat, 
	 sizeof(float), NhlOffset(LegendLayerRec,legend.title_angle),
	 NhlTString,"0.0"},
{NhlNlgTitleDirection,NhlClgTitleDirection,NhlTTextDirection,
	 sizeof(TextDirection),
	 NhlOffset(LegendLayerRec,legend.title_direction),
	 NhlTImmediate,(NhlPointer)ACROSS},
{NhlNlgTitleFont, NhlClgTitleFont, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.title_font),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgTitleFontColor, NhlClgTitleFontColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.title_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgTitleJust, NhlClgTitleJust, NhlTJustification, sizeof(NhlJustification),
	 NhlOffset(LegendLayerRec,legend.title_just),
	 NhlTImmediate,(NhlPointer)NhlCENTERCENTER},
{NhlNlgTitleFontHeightF, NhlClgTitleFontHeightF, NhlTFloat, 
	 sizeof(float), NhlOffset(LegendLayerRec,legend.title_height),
	 NhlTString,"0.025"},
{NhlNlgTitleFontAspectF, NhlClgTitleFontAspectF, NhlTFloat, 
	 sizeof(float), NhlOffset(LegendLayerRec,legend.title_aspect),
	 NhlTString,"1.0"},
{NhlNlgTitleFontThicknessF, NhlClgTitleFontThicknessF, NhlTFloat, 
	 sizeof(float), NhlOffset(LegendLayerRec,legend.title_thickness),
	 NhlTString,"1.0"},
{NhlNlgTitleFontQuality, NhlClgTitleFontQuality, NhlTFQuality, 
	 sizeof(FontQuality), 
	 NhlOffset(LegendLayerRec,legend.title_quality),
	 NhlTImmediate,(NhlPointer) HIGH},
{NhlNlgTitleConstantSpacingF, NhlClgTitleConstantSpacingF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LegendLayerRec,legend.title_const_spacing),
	 NhlTString,"0.0"},
{NhlNlgTitleFuncCode, NhlClgTitleFuncCode, NhlTCharacter, 
	 sizeof(char), NhlOffset(LegendLayerRec,legend.title_func_code),
	 NhlTString,":"},
	
{NhlNlgDrawBoxLines, NhlClgDrawBoxLines, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.box_line_on),
	 NhlTImmediate,(NhlPointer) 0},
{NhlNlgBoxLineColor, NhlClgBoxLineColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.box_line_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgBoxLineThicknessF, NhlClgBoxLineThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LegendLayerRec,legend.box_line_thickness),
	 NhlTString,"1.0"},
{NhlNlgBoxLineDashPattern, NhlClgBoxLineDashPattern, NhlTInteger, 
	 sizeof(int), 
	 NhlOffset(LegendLayerRec,legend.box_line_dash_pattern),
	 NhlTImmediate,(NhlPointer) 0},
{NhlNlgBoxLineDashLengthF, NhlClgBoxLineDashLengthF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LegendLayerRec,legend.box_line_dash_length),
	 NhlTString,"0.15"},

{NhlNlgDrawPerim, NhlClgDrawPerim, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.perim_on),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgPerimColor, NhlClgPerimColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.perim_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgPerimFill, NhlClgPerimFill, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.perim_fill),
	 NhlTImmediate,(NhlPointer) NhlHOLLOWFILL},
{NhlNlgPerimFillColor, NhlClgPerimFillColor, NhlTInteger, 
	 sizeof(int), NhlOffset(LegendLayerRec,legend.perim_fill_color),
	 NhlTImmediate,(NhlPointer) 1},
{NhlNlgPerimThicknessF, NhlClgPerimThicknessF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LegendLayerRec,legend.perim_thickness),
	 NhlTString,"1.0"},
{NhlNlgPerimDashPattern, NhlClgPerimDashPattern, NhlTInteger, 
	 sizeof(int), 
	 NhlOffset(LegendLayerRec,legend.perim_dash_pattern),
	 NhlTImmediate,(NhlPointer) 0},
{NhlNlgPerimDashLengthF, NhlClgPerimDashLengthF, NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(LegendLayerRec,legend.perim_dash_length),
	 NhlTString,"0.15"},

};

/*
* Base Methods used
*/

static NhlErrorTypes LegendSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes 	LegendGetValues(
#ifdef NhlNeedProto
	Layer,		/* l */
	_NhlArgList, 	/* args */
	int		/* num_args */
#endif
);

static NhlErrorTypes    LegendInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes	LegendDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

static NhlErrorTypes	LegendDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes 	LegendClassInitialize();

/*
* Private functions
*/

static NhlErrorTypes    SetLegendGeometry(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    SetTitle(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    SetBoxLocations(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);


static NhlErrorTypes    ManageBoxFractionsArray(
#ifdef NhlNeedProto
	float	*box_fractions,
	int	count
#endif
);

static void CreateIntermediates(
#ifdef NhlNeedProto
	float*,		/* *flist */
	int,		/* start  */
	int		/* end    */
#endif
);

static NhlErrorTypes    SetLabels(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    AdjustGeometry(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    ManageDynamicArrays(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes    InitializeDynamicArrays(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	_NhlArgList,	/* args		*/
	int		/* num_args	*/
#endif
);

static NhlErrorTypes   	AdjustLabels(
#ifdef NhlNeedProto
	LegendLayerPart *lg_p,
	float		height,
	float		avail_space,
	int		max_strlen,
	float		area_x,
	float		area_y
#endif
);

static NhlErrorTypes ValidatedGenArrayCopy(
#ifdef NhlNeedProto
	NhlGenArray	*gnew, 
	NhlGenArray	gold,
	NrmQuark	type,
	int		max_ex,
	void		*def_val,
	char		*res_name,
	char		*caller
#endif
);

static NhlGenArray GenArraySubsetCopy(
#ifdef NhlNeedProto
	NhlGenArray	ga,
	int		length
#endif
);

#if 0
static NhlBoolean    	LegendChanged(
#ifdef NhlNeedProto
	Layer,		/* new		*/ 
	Layer,		/* old		*/
	int,            /* init         */
	_NhlArgList	args,
	int		num_args
#endif
);
#endif

LegendLayerClassRec legendLayerClassRec = {
	{
/* class_name			*/	"Legend",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(LegendLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&viewLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	LegendClassInitialize,
/* layer_initialize		*/	LegendInitialize,
/* layer_set_values		*/	LegendSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	LegendGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	LegendDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	LegendDraw,

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

LayerClass legendLayerClass = (LayerClass)&legendLayerClassRec;

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark	Qitem_types = NrmNULLQUARK;
static NrmQuark	Qitem_indexes = NrmNULLQUARK;
static NrmQuark	Qitem_strings = NrmNULLQUARK;
static NrmQuark	Qitem_colors = NrmNULLQUARK;
static NrmQuark	Qitem_thicknesses = NrmNULLQUARK;
static NrmQuark	Qitem_text_heights = NrmNULLQUARK;
static NrmQuark	Qlabel_strings = NrmNULLQUARK;
static NrmQuark	Qbox_fractions = NrmNULLQUARK;

/*
 * Function:	LegendInitialize
 *
 * Description:	Performs initialization of Legend. 
 *              1) Initialize some internal parameters, and coerce
 *                 several others into the proper boundaries.
 *              2) Copy the view settings.
 *              2) Create a default title string.
 *              3) InitializeDynamicArrays
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
static NhlErrorTypes    LegendInitialize
#if __STDC__
	(LayerClass class, 
	 Layer req, 
	 Layer new, 
	 _NhlArgList args,
	 int num_args)
#else
(class,req,new,args,num_args)
	LayerClass	class;
	Layer		req;
	Layer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	LegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes	ret=NOERROR,ret1 = NOERROR;

	lg_p->labels_id = -1;
	lg_p->title_id = -1;
	lg_p->stride_labels = NULL;
	lg_p->box_locs = NULL;
	lg_p->label_locs = NULL;

/*
 * Ensure that the label and title angles range is between 0 and 360
 */
	lg_p->label_angle = fmod(lg_p->label_angle,360.0);
	lg_p->title_angle = fmod(lg_p->title_angle,360.0);

	if (lg_p->box_count < 1) {
		NhlPError(WARNING,E_UNKNOWN,"Minimum box count is 1");
		ret = WARNING;
		lg_p->box_count = 1;
	}

/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlgTitlePosition) &&
	    !_NhlArgIsSet(args,num_args,NhlNlgTitleDirection)) {
		switch (lg_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lg_p->title_direction = ACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lg_p->title_direction = DOWN;
			break;
		}
	}

	lg_p->lg_x = tnew->view.x;
	lg_p->lg_y = tnew->view.y;
	lg_p->lg_width = tnew->view.width;
	lg_p->lg_height = tnew->view.height;

	lg_p->perim.l = lg_p->lg_x;
	lg_p->perim.r = lg_p->lg_x + lg_p->lg_width;
	lg_p->perim.b = lg_p->lg_y - lg_p->lg_height;
	lg_p->perim.t = lg_p->lg_y;


/*
 * Create a default title string
 */

	lg_p->title_string = (char*)
		NhlMalloc((unsigned)strlen(tnew->base.name) +1);
	strcpy(lg_p->title_string,tnew->base.name);

	ret1 = InitializeDynamicArrays(new,req,args,num_args);
	ret = MIN(ret,ret1);

/*
 * Calculate legend geometry
 */

	ret1 = SetLegendGeometry(new,args,num_args);
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
 * Function:	LegendSetValues
 *
 * Description: Performs same operations as LegendInitialize except if 
 *		a move or resize has ocurred font_height is automatically 
 *		scaled.
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
static NhlErrorTypes LegendSetValues
#if __STDC__
	(Layer old,
	Layer reference,
	Layer new,
	_NhlArgList args,
	int num_args)
#else
(old,reference,new,args,num_args)
	Layer	old;
	Layer	reference;
	Layer	new;
	_NhlArgList	args;
	int	num_args;
#endif
{
	LegendLayer told = (LegendLayer) old;
	LegendLayer tnew = (LegendLayer) new;
	LegendLayerPart *olg_p = &(told->legend);
	LegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes ret = NOERROR,ret1 = NOERROR;

	if (lg_p->box_count < 1) {
		NhlPError(WARNING,E_UNKNOWN,"Minimum box count is 1");
		ret = WARNING;
		lg_p->box_count = 1;
	}
	
/*
 * Ensure that the label and title angles range is between 0 and 360
 */
	lg_p->label_angle = fmod(lg_p->label_angle,360.0);
	lg_p->title_angle = fmod(lg_p->title_angle,360.0);

	ret1 = ManageDynamicArrays(new,old,args,num_args);
	ret = MIN(ret,ret1);


/*
 * Adjust the title direction according to the position unless
 * it is explicitly set.
 */
	if (_NhlArgIsSet(args,num_args,NhlNlgTitlePosition) &&
	    !_NhlArgIsSet(args,num_args,NhlNlgTitleDirection)) {
		switch (lg_p->title_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		default:
			lg_p->title_direction = ACROSS;
			break;
		case NhlRIGHT:
		case NhlLEFT:
			lg_p->title_direction = DOWN;
			break;
		}
	}
	
	if (tnew->view.x != told->view.x
	    || tnew->view.y != told->view.y) {

		_NhlEvalTrans(tnew->view.trans_children,
			      lg_p->lg_x,lg_p->lg_y,
			      &lg_p->lg_x,&lg_p->lg_y);
	}
	if (tnew->view.width != told->view.width
	    || tnew->view.height != told->view.height) {

		
		float tx, ty;
		float ow = lg_p->lg_width;
		float oh = lg_p->lg_height;

		_NhlEvalTrans(tnew->view.trans_children,
			      olg_p->lg_x + lg_p->lg_width, 
			      olg_p->lg_y - lg_p->lg_height,
			      &tx, &ty);

		lg_p->lg_width  = tx - lg_p->lg_x;
		lg_p->lg_height = lg_p->lg_y - ty;

		tx = lg_p->lg_width / ow;
		ty = lg_p->lg_height / oh;


		if (! _NhlArgIsSet(args,num_args,NhlNlgLabelFontHeightF)) {

			if (lg_p->label_direction == ACROSS)
				lg_p->label_height *= tx;
			else 
				lg_p->label_height *= ty;
		}
		if (! _NhlArgIsSet(args,num_args,NhlNlgTitleFontHeightF)) {

			if (lg_p->title_direction == ACROSS)
				lg_p->label_height *= tx;
			else
				lg_p->label_height *= ty;
		}
	}

#if 0		

	if (LegendChanged(tnew, told, 0, args, num_args)) {
		NhlPError(WARNING,E_UNKNOWN,"LegendSetValues: Can not change x,y,width,and height when other legend attribute changes have been requested also, proceding with other text attribute requests");
		ret1 = WARNING;
			
	}

	else {

		ret1 = TransformPoints(tnew);
	}
#endif


	lg_p->perim.l = lg_p->lg_x;
	lg_p->perim.r = lg_p->lg_x + lg_p->lg_width;
	lg_p->perim.b = lg_p->lg_y - lg_p->lg_height;
	lg_p->perim.t = lg_p->lg_y;


/*
 * Calculate legend geometry
 */

	ret1 = SetLegendGeometry(new,args,num_args);
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
 * Function:	LegendGetValues
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
 *		NhlNlgItemTypes
 *		NhlNlgItemIndexes
 *		NhlNlgItemStrings
 *		NhlNlgItemColors
 *		NhlNlgItemThicknesses
 *		NhlNlgItemTextHeights
 *		NhlNlgLabelStrings
 *		NhlNlgBoxFractions
 *	The caller is responsible for freeing this memory.
 */

static NhlErrorTypes	LegendGetValues
#if __STDC__
(Layer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	Layer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
	LegendLayer lgl = (LegendLayer)l;
	LegendLayerPart *lg_p = &(lgl->legend);
	NhlGenArray ga;
	char *e_text;
	int i, count = 0;
	char *type = "";
	
	for( i = 0; i< num_args; i++ ) {

		ga = NULL;
		if(args[i].quark == Qitem_types) {
			ga = lg_p->item_types;
			count = lg_p->mono_item_type ? 1 : lg_p->box_count;
			type = NhlNlgItemTypes;
		}
		else if (args[i].quark == Qitem_indexes) {
			ga = lg_p->item_indexes;
			count = lg_p->box_count;
			type = NhlNlgItemIndexes;
		}
		else if (args[i].quark == Qitem_strings) {
			ga = lg_p->item_strings;
			count = lg_p->box_count;
			type = NhlNlgItemStrings; 
		}
		else if (args[i].quark == Qitem_colors) {
			ga = lg_p->item_colors;
			count = lg_p->mono_item_color ? 1 : lg_p->box_count;
			type = NhlNlgItemColors;
		}
		else if (args[i].quark == Qitem_thicknesses) {
			ga = lg_p->item_thicknesses;
			count = lg_p->mono_item_thickness ? 
				1 : lg_p->box_count;
			type = NhlNlgItemThicknesses;
		}
		else if (args[i].quark == Qitem_text_heights) {
			ga = lg_p->item_text_heights;
			count = lg_p->mono_item_text_height ? 
				1 : lg_p->box_count;
			type = NhlNlgItemTextHeights;
		}
		else if (args[i].quark == Qlabel_strings) {
			ga = lg_p->label_strings;
			count = lg_p->box_count;
			type = NhlNlgLabelStrings;
		}
		else if (args[i].quark == Qbox_fractions) {
			ga = lg_p->box_fractions;
			count = lg_p->box_count+1;
			type = NhlNlgBoxFractions;
		}
		if (ga != NULL) {
			if ((ga = GenArraySubsetCopy(ga, count)) == NULL) {
				e_text = "%s: error copying %s GenArray";
				NhlPError(FATAL,E_UNKNOWN,e_text,
					  "LegendGetValues",type);
				return FATAL;
			}
			*((NhlGenArray *)(args[i].value)) = ga;
		}
	}

	return(NOERROR);

}
static NhlGenArray GenArraySubsetCopy
#if __STDC__
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
#if 0
/*ARGSUSED*/
static NhlBoolean    	LegendChanged
#if __STDC__
	(Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	LegendLayer	told = (LegendLayer) old;
	LegendLayerPart *lg_p = &(tnew->legend);
	LegendLayerPart *olg_p = &(told->legend);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;

	if (memcmp((const void *) lg_p, (const void *) olg_p, 
		   sizeof(LegendLayerPart)) == 0) 
		return (False);
	else
		return (True);
#if 0
	if (lg_p->orient != olg_p->orient    ||
	    lg_p->box_major_ext != olg_p->box_major_ext    ||
	    lg_p->box_minor_ext != olg_p->box_minor_ext    ||
	    lg_p->box_count != olg_p->box_count    ||
	    lg_p->box_sizing != olg_p->box_sizing    ||
	    lg_p->label_strings != olg_p->label_strings    ||
	    lg_p->box_fractions != olg_p->box_fractions    ||
	    lg_p->labels_on != olg_p->labels_on    ||
	    lg_p->label_angle != olg_p->label_angle    ||
	    lg_p->label_font != olg_p->label_font    ||
	    lg_p->label_color != olg_p->label_color    ||
	    lg_p->label_height != olg_p->label_height    ||
	    lg_p->label_aspect != olg_p->label_aspect    ||
	    lg_p->label_thickness != olg_p->label_thickness    ||
	    lg_p->label_quality != olg_p->label_quality    ||
	    lg_p->label_const_spacing != olg_p->label_const_spacing    ||
	    lg_p->label_func_code != olg_p->label_func_code    ||
	    lg_p->label_direction != olg_p->label_direction    ||
	    lg_p->max_title_ext != olg_p->max_title_ext    ||
	    lg_p->title_string != olg_p->title_string    ||
	    lg_p->title_on != olg_p->title_on    ||
	    lg_p->title_pos != olg_p->title_pos    ||
	    lg_p->title_just != olg_p->title_just    ||
	    lg_p->title_direction != olg_p->title_direction    ||
	    lg_p->title_angle != olg_p->title_angle    ||
	    lg_p->title_font != olg_p->title_font    ||
	    lg_p->title_color != olg_p->title_color    ||
	    lg_p->title_height != olg_p->title_height    ||
	    lg_p->title_aspect != olg_p->title_aspect    ||
	    lg_p->title_thickness != olg_p->title_thickness    ||
	    lg_p->title_quality != olg_p->title_quality    ||
	    lg_p->title_const_spacing != olg_p->title_const_spacing ||
	    lg_p->title_func_code != olg_p->title_func_code) {
		
		return True;
	}

	return False;

#endif		
}
#endif

/*ARGSUSED*/
static NhlErrorTypes    ManageDynamicArrays
#if __STDC__
	(Layer		new, 
	Layer		old,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,args,num_args)
	Layer		new;
	Layer		old;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	LegendLayer	told = (LegendLayer) old;
	LegendLayerPart *lg_p = &(tnew->legend);
	LegendLayerPart *olg_p = &(told->legend);
	NhlErrorTypes ret = NOERROR, ret_1 = NOERROR;
	int i;
	int count, len_1, len_2;
	char number[10];
	char *entry_name = "LegendSetValues";
	char *e_text;
	float *f_p;
	int *i_p,*i2_p;
	NhlString *s_p;
	int def_int;
	float def_float;
	char *def_string;

/*=======================================================================*/
/* 
 * Handle the item types array first: the item indexes array depends on
 * it being set up correctly.
 */


	count = lg_p->mono_item_type ? 1 : lg_p->box_count;

	if (lg_p->item_types != olg_p->item_types) {
		
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_types),
					      olg_p->item_types,Qint,
					      NhlLG_MAX_BOXES,NhlLG_LINES,
					      NhlNlgItemTypes, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		i_p = (int *) lg_p->item_types->data;
		for (i=0; i<MIN(count,lg_p->item_types->num_elements); i++) {
			if (i_p[i] != NhlLG_LINES && 
			    i_p[i] != NhlLG_MARKERS) {
				e_text =
		"%s: %s index %d holds an invalid type value: defaulting";
				NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
					  NhlNlgItemTypes, i);
				ret = MIN(ret, WARNING);
				i_p[i] = NhlLG_LINES;
			}
		}
	}

	if (lg_p->item_types->num_elements < count) {
		i_p = (int *) lg_p->item_types->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemTypes);
			return FATAL;
		}
		for (i=lg_p->item_types->num_elements; i<count; i++) {
			i_p[i] = NhlLG_LINES;
		}

		lg_p->item_types->data = (NhlPointer) i_p;
		lg_p->item_types->num_elements = count;
	}

/*======================================================================*/

/*
 * Copy the new information if the resource has changed. 
 * If number of label boxes has increased, gcheck all added values for a
 * valid pattern index, setting the default pattern index if an invalid
 * value is found.
 */
	
	
	i2_p = (int *) lg_p->item_types->data;

	if (lg_p->item_indexes != olg_p->item_indexes) {
		
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_indexes),
					      olg_p->item_indexes,Qint,
					      NhlLG_MAX_BOXES,NhlLG_LINES,
					      NhlNlgItemIndexes, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		
		NhlGetValues(tnew->base.wkptr->base.id,
			     NhlNwkDashTableLength, &len_1,
			     NhlNwkMarkerTableLength, &len_2, NULL);
		i_p = (int *) lg_p->item_indexes->data;
		count = MIN(lg_p->box_count,lg_p->item_indexes->num_elements);

		if (lg_p->mono_item_type && *i2_p == NhlLG_LINES) {
			for (i=0; i<count; i++)
				if (i_p[i] > len_1 || 
				    i_p[i] < NhlLG_MIN_LINE_INDEX)
					i_p[i] = NhlLG_DEF_LINE_INDEX;
		}
		else if (lg_p->mono_item_type) {
			for (i=0; i<count; i++)
				if (i_p[i] > len_2 || 
				    i_p[i] < NhlLG_MIN_MARKER_INDEX)
					i_p[i] = NhlLG_DEF_MARKER_INDEX;
		}
		else {
			for (i=0; i<count; i++)
				if (i2_p[i] == NhlLG_LINES)
					if (i_p[i] > len_1 ||
					    i_p[i] < NhlLG_MIN_LINE_INDEX)
						i_p[i] = NhlLG_DEF_LINE_INDEX;
				else
					if (i_p[i] > len_2 || 
					    i_p[i] < NhlLG_MIN_MARKER_INDEX)
						i_p[i]= NhlLG_DEF_MARKER_INDEX;
			
		}
		
	}
	
	if (lg_p->item_indexes->num_elements < lg_p->box_count) {
		count = lg_p->item_indexes->num_elements;
		i_p = (int *) lg_p->item_indexes->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, 
				lg_p->box_count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemIndexes);
			return FATAL;
		}
		if (lg_p->mono_item_type && *i2_p == NhlLG_LINES) {
			for (i=count;i<lg_p->box_count;i++)
				i_p[i] = NhlLG_DEF_LINE_INDEX;
		}
		else if (lg_p->mono_item_type) {
			for (i=count;i<lg_p->box_count;i++)
				i_p[i] = NhlLG_DEF_MARKER_INDEX;
		}
		else {
			for (i=count;i<lg_p->box_count;i++) 
				if (i2_p[i] == NhlLG_LINES)
					i_p[i] = NhlLG_DEF_LINE_INDEX;
				else
					i_p[i] = NhlLG_DEF_MARKER_INDEX;
		}
		lg_p->item_indexes->data = (NhlPointer) i_p;
		lg_p->item_indexes->num_elements = lg_p->box_count;
	}
	
/*=======================================================================*/

/* 
 * Handle the item strings. Copy the new array into the old.
 * NULL strings generate an error message and are replaced by empty 
 * (single byte null terminator) strings within the Validated copy function. 
 * If the box count has increased, but the string gen array does not 
 * contain enough elements, the array is resized and default strings are
 * created for the additional elements.
 */
	def_string = "";

	if (lg_p->item_strings != olg_p->item_strings) {
		
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_strings),
					      olg_p->item_strings,Qstring,
					      NhlLG_MAX_BOXES,&def_string,
					      NhlNlgItemStrings, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
	}

	if (lg_p->item_strings->num_elements < lg_p->box_count) {
		s_p = (NhlString *) lg_p->item_strings->data;
		if ((s_p = (NhlString *)
		     NhlRealloc(s_p, lg_p->box_count * 
				sizeof (NhlString))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemStrings);
			return FATAL;
		}
		for (i=lg_p->item_strings->num_elements; 
		     i<lg_p->box_count; i++) {
			sprintf(number,"%d",i);
			if ((s_p[i] = (char *)
			     NhlMalloc(strlen(NhlLG_DEF_ITEM_STRING) + 
				       strlen(number) + 1)) == NULL) {
				e_text = "%s: error creating %s string";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNlgItemStrings);
				return FATAL;
			}
			strcpy(s_p[i],(Const char *)NhlLG_DEF_ITEM_STRING);
			strcat(s_p[i],number);
		}

		lg_p->item_strings->data = (NhlPointer) s_p;
		lg_p->item_strings->num_elements = lg_p->box_count;
	}

/*=======================================================================*/
/*
 * Manage the colors array: if the array has changed copy the new
 * array elements, check them for validity and get the GKS index.
 * Then if the box count is greater than the current array size, enlarge
 * the array and give initial values to the new elements.
 */

	NhlGetValues(tnew->base.wkptr->base.id,
		     NhlNwkColorMapLen, &len_1, NULL);
	count = lg_p->mono_item_color ? 1 : lg_p->box_count;

	if (lg_p->item_colors != olg_p->item_colors) {
		def_int = NhlLG_DEF_COLOR;
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_colors),
					      olg_p->item_colors,Qint,
					      NhlLG_MAX_BOXES,&def_int,
					      NhlNlgItemColors, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		i_p = (int *) lg_p->item_colors->data;
		for (i=0; i<MIN(count,lg_p->item_colors->num_elements); i++) {
			if (i_p[i] < 0 || i_p[i] > len_1) {
				e_text =
		"%s: %s index %d holds an invalid color value, %d: defaulting";
				NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
					  NhlNlgItemColors, i, i_p[i]);
				ret = MIN(ret, WARNING);
				i_p[i] = NhlLG_DEF_COLOR;
			}
			lg_p->gks_colors[i] =
				_NhlGetGksCi(tnew->base.wkptr, i_p[i]);
		}
	}

	if (lg_p->item_colors->num_elements < count) {
		i_p = (int *) lg_p->item_colors->data;
		if ((i_p = (int *)
		     NhlRealloc(i_p, count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemColors);
			return FATAL;
		}
		if ((lg_p->gks_colors = (int *)
		     NhlRealloc(lg_p->gks_colors, 
				count * sizeof (int))) == NULL) {
			e_text = "%s: error allocating private %s data";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemColors);
			return FATAL;
		}
		for (i=lg_p->item_colors->num_elements; i<count; i++) {
			i_p[i] = i < len_1 ? i : NhlLG_DEF_COLOR;
			lg_p->gks_colors[i] =
				_NhlGetGksCi(tnew->base.wkptr, i_p[i]);
		}

		lg_p->item_colors->data = (NhlPointer) i_p;
		lg_p->item_colors->num_elements = count;
	}
/*=======================================================================*/
/* 
 * Handle the item_thickness
 */

	count = lg_p->mono_item_thickness ? 1 : lg_p->box_count;
	def_float = 1.0;

	if (lg_p->item_thicknesses != olg_p->item_thicknesses) {
		
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_thicknesses),
					      olg_p->item_thicknesses,Qfloat,
					      NhlLG_MAX_BOXES,&def_float,
					      NhlNlgItemThicknesses, 
					      entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		f_p = (float *) lg_p->item_thicknesses->data;
		for (i=0; i<MIN(count,
				lg_p->item_thicknesses->num_elements); i++) {
			if (f_p[i] <= 0.0) {
				e_text =
        "%s: %s index %d holds an invalid item thickness value: defaulting";
				NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
					  NhlNlgItemThicknesses, i);
				ret = MIN(ret, WARNING);
				f_p[i] = 1.0;
			}
		}
	}

	if (lg_p->item_thicknesses->num_elements < count) {
		f_p = (float *) lg_p->item_thicknesses->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, count * sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemThicknesses);
			return FATAL;
		}
		for (i=lg_p->item_thicknesses->num_elements; i<count; i++) {
			f_p[i] = 1.0;
		}

		lg_p->item_thicknesses->data = (NhlPointer) f_p;
		lg_p->item_thicknesses->num_elements = count;
	}

/*=======================================================================*/
/* Item text height is handled similarly to thickness */

	count = lg_p->mono_item_text_height ? 1 : lg_p->box_count;
	def_float = 0.01;

	if (lg_p->item_text_heights != olg_p->item_text_heights) {
		
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_text_heights),
					      olg_p->item_text_heights,Qfloat,
					      NhlLG_MAX_BOXES,&def_float,
					      NhlNlgItemTextHeights, 
					      entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		f_p = (float *) lg_p->item_text_heights->data;
		for (i=0; i<MIN(count,
				lg_p->item_text_heights->num_elements); i++) {
			if (f_p[i] <= 0.0) {
				e_text =
        "%s: %s index %d holds an invalid item thickness value: defaulting";
				NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
					  NhlNlgItemTextHeights, i);
				ret = MIN(ret, WARNING);
				f_p[i] = 0.01;
			}
		}
	}

	if (lg_p->item_text_heights->num_elements < count) {
		f_p = (float *) lg_p->item_text_heights->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, count * sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemTextHeights);
			return FATAL;
		}
		for (i=lg_p->item_text_heights->num_elements; i<count; i++) {
			f_p[i] = 0.01;
		}

		lg_p->item_text_heights->data = (NhlPointer) f_p;
		lg_p->item_text_heights->num_elements = count;
	}

/*=======================================================================*/

/* 
 * Handle the label strings. Copy the new array into the old.
 * NULL strings generate an error message and are replaced by empty 
 * (single byte null terminator) strings within the Validated copy function. 
 * If the box count has increased, but the string gen array does not 
 * contain enough elements, the array is resized and default strings are
 * created for the additional elements.
 */
	def_string = "";

	if (lg_p->label_strings != olg_p->label_strings) {
		
		ret_1 = ValidatedGenArrayCopy(&(lg_p->label_strings),
					      olg_p->label_strings,Qstring,
					      NhlLG_MAX_BOXES,&def_string,
					      NhlNlgLabelStrings, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
	}

	if (lg_p->label_strings->num_elements < lg_p->box_count) {
		s_p = (NhlString *) lg_p->label_strings->data;
		if ((s_p = (NhlString *)
		     NhlRealloc(s_p, lg_p->box_count * 
				sizeof (NhlString))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgLabelStrings);
			return FATAL;
		}
		for (i=lg_p->label_strings->num_elements; 
		     i<lg_p->box_count; i++) {
			sprintf(number,"%d",i);
			if ((s_p[i] = (char *)
			     NhlMalloc(strlen(NhlLG_DEF_STRING) + 
				       strlen(number) + 1)) == NULL) {
				e_text = "%s: error creating %s string";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNlgLabelStrings);
				return FATAL;
			}
			strcpy(s_p[i],(Const char *)NhlLG_DEF_STRING);
			strcat(s_p[i],number);
		}

		lg_p->label_strings->data = (NhlPointer) s_p;
		lg_p->label_strings->num_elements = lg_p->box_count;
	}

/*=======================================================================*/
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
	count = lg_p->box_count+1;
	if (lg_p->box_fractions != olg_p->box_fractions) {
		def_float = -1.0;
		ret_1 = ValidatedGenArrayCopy(&(lg_p->box_fractions),
					      olg_p->box_fractions,Qfloat,
					      NhlLG_MAX_BOXES+1,&def_float,
					      NhlNlgBoxFractions, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
			return ret;
	}
	
	if (lg_p->box_fractions->num_elements < count) {
		f_p = (float *) lg_p->box_fractions->data;
		if ((f_p = (float *)
		     NhlRealloc(f_p, (count) * 
				sizeof (float))) == NULL) {
			e_text = "%s: error allocating %s data";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgBoxFractions);
			return FATAL;
		}

		lg_p->box_fractions->data = (NhlPointer) f_p;
		lg_p->box_fractions->num_elements = count;
	}

	if (lg_p->box_count != olg_p->box_count) {
		f_p = (float *) lg_p->box_fractions->data;
		f_p[0] = 0.0;
		for (i=1;i<lg_p->box_count; i++)
			f_p[i] = -1.0;
		f_p[lg_p->box_count] = 1.0;
	}

/*=======================================================================*/
/*
 * Allocate or reallocate the location arrays: use one more than
 * the current box count so that both ends of the legend can be stored
 */

	if (lg_p->box_count > olg_p->box_count) {
		if (lg_p->box_locs == NULL) {
			lg_p->box_locs = (float *) 
				NhlMalloc((lg_p->box_count+1) * sizeof(float));
		}
		else {
			lg_p->box_locs = (float *) 
				NhlRealloc(lg_p->box_locs, 
					   (lg_p->box_count+1) *
					   sizeof(float));
		}
	}
	
	return (ret);
}
/*
 * Function:    ValidatedGenArrayCopy
 *
 * Description: Copies int, float or string generic arrays, with checking.
 *		If gold is NULL, or gold has fewer elements than gnew,
 *              makes a copy of the array. If gold has enough room, the
 *              data only is copied from gnew to the already allocated gold, 
 *              then gnew is made to point to the gold array.
 *		The type and element size of gnew are checked, as well as
 *		the number of elements in gnew. If any of these are invalid,
 *              A warning is returned and gnew is pointed to the unmodified
 *		gold array.
 *
 * In Args:     
 *
 * Out Args:
 *
 * Scope:       
 * Returns:     if successful NOERROR, FATAL on memory allocation errors
 *              WARNING if the new GenArray is invalid in some way.
 *
 * Side Effect: the pointer to the new gen array is modified to point to
 *              allocated data with the new array data. The gold array may
 *              no longer be allocated. 
 */

static NhlErrorTypes ValidatedGenArrayCopy
#if __STDC__
	(NhlGenArray	*gnew, 
	 NhlGenArray	gold,
	 NrmQuark	type,
	 int		max_el,
	 void		*def_val,
	 char		*res_name,
	 char		*caller)
#else
(gnew,gold,type,max_el,def_val,res_name,caller)
	(NhlGenArray	*gnew; 
	 NhlGenArray	gold;
	 NrmQuark	type;
	 int		max_el;
	 void		*def_value;
	 char		*res_name;
	 char		*caller;
#endif
{
	NhlGenArray 	gen;
	char		*e_text;
	int		i;

	gen = *gnew;
	
	if (gen == NULL) {
		e_text = 
		 "%s: %s NULL array passed in: ignoring";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
		*gnew = gold;
		return WARNING;
	}
	if (gen->num_elements <= 0) {
		e_text = 
		 "%s: %s invalid element count: ignoring";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
		*gnew=gold;
		return WARNING;
	}
	else if (gen->num_elements > max_el) {
		e_text =
		 "%s: %s exceeds maximum number of elements, %d: ignoring";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name,max_el);
		*gnew=gold;
		return WARNING;
	}

	if (type == Qint) {
		if (gen->typeQ != Qint || gen->size != sizeof(int)) {
			e_text =
			   "%s: %s must be a generic integer array: ignoring";
			NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
			*gnew = gold;
			return WARNING; 
		}
	}
	else if (type == Qfloat) {
		if (gen->typeQ != Qfloat || gen->size != sizeof(float)) {
			e_text =
			   "%s: %s must be a generic float array: ignoring";
			NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
			*gnew = gold;
			return WARNING; 
		}
	}
	else if (type == Qstring) {
		if (gen->typeQ != Qstring || gen->size != sizeof(char *)) {
			e_text =
			   "%s: %s must be a generic string array: ignoring";
			NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
			*gnew = gold;
			return WARNING; 
		}
	}
	else {
		e_text = "%s: %s type is unrecognized: ignoring";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
		*gnew = gold;
		return WARNING; 
	}

	if (gold == NULL) { 
		if ((*gnew = _NhlCopyGenArray(gen,True)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(FATAL,E_UNKNOWN,e_text,caller, res_name);
			return FATAL;
		}
	}
	else if (gen->num_elements > gold->num_elements) {
		if ((*gnew = _NhlCopyGenArray(gen,True)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(FATAL,E_UNKNOWN,e_text,caller,res_name);
			return FATAL;
		}
		NhlFreeGenArray(gold);
	}
	else if (type == Qint) {
		memcpy((void *)gold->data, (Const void *)gen->data,
			       gen->num_elements * sizeof(int));
		*gnew = gold;
	} 
	else if (type == Qfloat) {
		memcpy((void *)gold->data, (Const void *)gen->data,
			       gen->num_elements * sizeof(float));
		*gnew = gold;
	}
	else if (type == Qstring) {
		char **from = (char **) gen->data;
		char **to = (char **) gold->data;
		for (i=0; i<gen->num_elements; i++) {
			if (from[i] == NULL) {
				if (to[i] != NULL) 
					NhlFree(to[i]);
				to[i] = (char *) 
					NhlMalloc(strlen((char *)def_val)+1);
				if (to[i] == NULL) {
					e_text = "%s: error copying %s string";
					NhlPError(FATAL,E_UNKNOWN,e_text,
						  caller,res_name);
					return FATAL;
				}
				strcpy(to[i],(char *)def_val);
			}
			else if (to[i] == NULL) {
				to[i] = (char *) NhlMalloc(strlen(from[i])+1);
				if (to[i] == NULL) {
					e_text = "%s: error copying %s string";
					NhlPError(FATAL,E_UNKNOWN,e_text,
						  caller,res_name);
					return FATAL;
				}
				strcpy(to[i],from[i]);
			}
			else if (strcmp(to[i],from[i])) {
				NhlFree(to[i]);
				to[i] = (char *) NhlMalloc(strlen(from[i])+1);
				if (to[i] == NULL) {
					e_text = "%s: error copying %s string";
					NhlPError(FATAL,E_UNKNOWN,e_text,
						  caller,res_name);
					return FATAL;
				}
				strcpy(to[i],from[i]);
			}
		}
		*gnew = gold;
	}

	return NOERROR;

}

/*ARGSUSED*/
static NhlErrorTypes    InitializeDynamicArrays
#if __STDC__
	(Layer		new, 
	Layer		old,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,args,num_args)
	Layer		new;
	Layer		old;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	LegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes ret = NOERROR, ret_1;
	int i,count;
	int len_1, len_2;
	char number[10];
	NhlGenArray ga;
	char *entry_name = "LegendInitialize";
	char *e_text;
	float * f_p;
	int *i_p, *i2_p;
	NhlString *s_p;
	int def_int;
	float def_float;
	char *def_string;

/*=======================================================================*/
/* Handle the item types array first: the item indexes array depends on
 * it being set up correctly. Note that the data ownership flag is 
 * explicitly set so that the gen array free routine will free the data.
 */

	count = MAX(lg_p->box_count, NhlLG_DEF_BOX_COUNT);
	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemTypes);
		return FATAL;
	}
	for (i=0; i < count; i++) 
		i_p[i] = NhlLG_LINES;

	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTInteger,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemTypes);
		return FATAL;
	}
	ga->my_data = True;

/* If an item types resource array has been passed in, copy it to the ga */

	if (lg_p->item_types == NULL) {
		lg_p->item_types = ga;
	}
	else {
		def_int=NhlLG_LINES;
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_types),
					      ga,Qint,
					      NhlLG_MAX_BOXES,&def_int,
					      NhlNlgItemTypes, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		i_p = (int *) lg_p->item_types->data;
	}
/*
 * Replace any invalid elements in the array with the default value
 */
	for (i=0; i<count; i++) {
		if (i_p[i] != NhlLG_LINES && i_p[i] != NhlLG_MARKERS) {
			e_text =
	       "%s: %s index %d holds an invalid item type: defaulting";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemTypes, i);
		        ret = MIN(ret, WARNING);
			i_p[i] = NhlLG_LINES;
		}
	}

/*=======================================================================*/

/*
 * The item indexes array is complicated by the fact that the default value
 * may be different, depending on the item type. Therefore it is necessary
 * to pass in an artificial default value to the Validated copy routine
 */

	NhlGetValues(tnew->base.wkptr->base.id,
		     NhlNwkDashTableLength, &len_1,
		     NhlNwkMarkerTableLength, &len_2, NULL);

	count = MAX(lg_p->box_count, NhlLG_DEF_BOX_COUNT);
	i_p = (int *) NhlMalloc(count * sizeof(int));

	i2_p = (int *)lg_p->item_types->data;
	if (lg_p->mono_item_type && *i2_p == NhlLG_LINES) { 
		for (i=0; i<MIN(count,len_1);i++)
			i_p[i] = NhlLG_MIN_LINE_INDEX + i;
		for (i=len_1; i<count;i++)
			i_p[i] = NhlLG_DEF_LINE_INDEX;
	}
	else if (lg_p->mono_item_type) {
		for (i=0; i<MIN(count,len_2);i++)
			i_p[i] = NhlLG_MIN_LINE_INDEX + i;
		for (i=len_2; i<count;i++)
			i_p[i] = NhlLG_DEF_MARKER_INDEX;
	}
	else {
		for (i=0; i<MIN(count,MIN(len_1,len_2));i++)
			i_p[i] = i+1;
		for (i=MIN(len_1,len_2); i<MIN(count,MAX(len_1,len_2));i++) {
			if (i2_p[i] == NhlLG_LINES) 
				i_p[i] = i < len_1 ? 
					NhlLG_MIN_LINE_INDEX + i : 
						NhlLG_DEF_LINE_INDEX;
			else
				i_p[i] = i < len_2 ? 
					NhlLG_MIN_MARKER_INDEX + i : 
						NhlLG_DEF_MARKER_INDEX;
		}
		for (i=MAX(len_1,len_2); i<count;i++) {
			if (i2_p[i] == NhlLG_LINES) 
				i_p[i] = NhlLG_DEF_LINE_INDEX;
			else
				i_p[i] = NhlLG_DEF_MARKER_INDEX;
		}
	}
	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTInteger,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemColors);
		return FATAL;
	}
	ga->my_data = True;

	def_int = -9999;
	if (lg_p->item_indexes == NULL) {
		lg_p->item_indexes = ga; 
	}
	else {
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_indexes),
					      ga,Qint,
					      NhlLG_MAX_BOXES,&def_int,
					      NhlNlgItemIndexes, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		i_p = (int *) lg_p->item_indexes->data;
	}
/*
 * Replace any invalid elements in the array with the default value
 */

	if (lg_p->mono_item_type && *i2_p == NhlLG_LINES) { 
		for (i=0; i<count;i++)
			if (i_p[i] < NhlLG_MIN_LINE_INDEX || i_p[i] > len_1) { 
				i_p[i] = NhlLG_DEF_LINE_INDEX;
			}
	}
	else if (lg_p->mono_item_type) {
		for (i=0; i<count;i++)
			if (i_p[i] < NhlLG_MIN_MARKER_INDEX || 
			    i_p[i] > len_2) { 
				i_p[i] = NhlLG_DEF_MARKER_INDEX;
			}
	}
	else {
		for (i=0; i<count;i++) {
			if (i2_p[i] == NhlLG_LINES) {
				if (i_p[i] < NhlLG_MIN_LINE_INDEX || 
				    i_p[i] > len_1)  
					i_p[i] = NhlLG_DEF_LINE_INDEX;
			}
			else {
				if (i_p[i] < NhlLG_MIN_MARKER_INDEX || 
				    i_p[i] > len_2)  
					i_p[i] = NhlLG_DEF_MARKER_INDEX;
			}
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

	count = MAX(lg_p->box_count, NhlLG_DEF_BOX_COUNT);
	if ((s_p = (NhlString *) 
	     NhlMalloc(count * sizeof(NhlString))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemStrings);
		return FATAL;
	}
	for (i=0;i<count;i++) {
		sprintf(number,"%d",i);
		if ((s_p[i] = (char *)
		     NhlMalloc(strlen(NhlLG_DEF_ITEM_STRING) + 
			       strlen(number) + 1)) == NULL) {
			e_text = "%s: error creating %s string";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemStrings);
			return FATAL;
		}
		strcpy(s_p[i], 
		       (Const char *)NhlLG_DEF_ITEM_STRING);
		strcat(s_p[i],number);
	}

	if ((ga = NhlCreateGenArray((NhlPointer)s_p,NhlTString,
				    sizeof(NhlString),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemStrings);
		return FATAL;
	}
	ga->my_data = True;

/* If an item strings resource array has been passed in, copy it to the ga */

	if (lg_p->item_strings == NULL) {
		lg_p->item_strings = ga;
	}
	else {
		def_string="";
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_strings),
					      ga,Qstring,
					      NhlLG_MAX_LBL_STRINGS,
					      &def_string,
					      NhlNlgItemStrings, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
	}
/*
 * Copy function should have replaced any NULL strings - nothing else is
 * invalid.
 */

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

	NhlGetValues(tnew->base.wkptr->base.id,
		     NhlNwkColorMapLen, &len_1, NULL);
	count = MAX(lg_p->box_count, NhlLG_DEF_BOX_COUNT);
	if ((i_p = (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemColors);
		return FATAL;
	}
	for (i=0; i < NhlLG_DEF_BOX_COUNT; i++) 
		i_p[i] = def_colors[i];
	for (i=NhlLG_DEF_BOX_COUNT; i<count; i++)
		i_p[i] = i < len_1 ? i : NhlLG_DEF_COLOR;
			
	if ((ga = NhlCreateGenArray((NhlPointer)i_p,NhlTInteger,
				    sizeof(int),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemColors);
		return FATAL;
	}
	ga->my_data = True;

	if (lg_p->item_colors == NULL) {
		lg_p->item_colors = ga;
	}
	else {
		def_int =  NhlLG_DEF_COLOR;
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_colors),
					      ga,Qint,
					      NhlLG_MAX_BOXES,&def_int,
					      NhlNlgItemColors, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		i_p = (int *) lg_p->item_colors->data;
	}

	/* check for validity, and copy the GKS index into a private array */

	if ((lg_p->gks_colors = 
	     (int *) NhlMalloc(count * sizeof(int))) == NULL) {
		e_text = "%s: error creating private storage array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}

	for (i=0; i<count; i++) {
		if (i_p[i] < 0 || i_p[i] > len_1) {
			e_text =
	       "%s: %s index %d holds an invalid color value, %d: defaulting";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemColors, i, i_p[i]);
		        ret = MIN(ret, WARNING);
			i_p[i] = NhlLG_DEF_COLOR;
		}
		lg_p->gks_colors[i] =
			_NhlGetGksCi(tnew->base.wkptr,i_p[i]);
	}

/*=======================================================================*/

/* The item thicknesses array */

	count = MAX(lg_p->box_count, NhlLG_DEF_BOX_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemThicknesses);
		return FATAL;
	}
	for (i=0; i < count; i++) 
		f_p[i] = 1.0;

	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemThicknesses);
		return FATAL;
	}
	ga->my_data = True;

/* If an item thicknesses resource array has been passed in, 
 * copy it to the ga */

	if (lg_p->item_thicknesses == NULL) {
		lg_p->item_thicknesses = ga;
	}
	else {
		def_float=1.0;
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_thicknesses),
					      ga,Qfloat,
					      NhlLG_MAX_BOXES,&def_float,
					      NhlNlgItemThicknesses, 
					      entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		f_p = (float *) lg_p->item_thicknesses->data;
	}
/*
 * Replace any invalid elements in the array with the default value
 */
	for (i=0; i<count; i++) {
		if (f_p[i] <= 0.0) {
			e_text =
	       "%s: %s index %d holds an invalid item thickness: defaulting";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemThicknesses, i);
		        ret = MIN(ret, WARNING);
			f_p[i] = 1.0;
		}
	}

/*=======================================================================*/

/* The item_text_heights array */

	count = MAX(lg_p->box_count, NhlLG_DEF_BOX_COUNT);
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemTextHeights);
		return FATAL;
	}
	for (i=0; i < count; i++) 
		f_p[i] = 0.01;

	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgItemTextHeights);
		return FATAL;
	}
	ga->my_data = True;

/* If an item text heights resource array has been passed in, 
 * copy it to the ga */

	if (lg_p->item_text_heights == NULL) {
		lg_p->item_text_heights = ga;
	}
	else {
		def_float=0.01;
		ret_1 = ValidatedGenArrayCopy(&(lg_p->item_text_heights),
					      ga,Qfloat,
					      NhlLG_MAX_BOXES,&def_float,
					      NhlNlgItemTextHeights, 
					      entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
		f_p = (float *) lg_p->item_text_heights->data;
	}
/*
 * Replace any invalid elements in the array with the default value
 */
	for (i=0; i<count; i++) {
		if (f_p[i] <= 0.0) {
			e_text =
	       "%s: %s index %d holds an invalid item thickness: defaulting";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNlgItemTextHeights, i);
		        ret = MIN(ret, WARNING);
			f_p[i] = 0.01;
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

	lg_p->max_label_stride_count = 0;
	lg_p->max_label_draw_count = 0;

	count = MAX(lg_p->box_count, NhlLG_DEF_BOX_COUNT);
	if ((s_p = (NhlString *) 
	     NhlMalloc(count * sizeof(NhlString))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgLabelStrings);
		return FATAL;
	}
	for (i=0;i<count;i++) {
		sprintf(number,"%d",i);
		if ((s_p[i] = (char *)
		     NhlMalloc(strlen(NhlLG_DEF_STRING) + 
			       strlen(number) + 1)) == NULL) {
			e_text = "%s: error creating %s string";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
				  NhlNlgLabelStrings);
			return FATAL;
		}
		strcpy(s_p[i], NhlLG_DEF_STRING);
		strcat(s_p[i], number);
	}

	if ((ga = NhlCreateGenArray((NhlPointer)s_p,NhlTString,
				    sizeof(NhlString),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgLabelStrings);
		return FATAL;
	}
	ga->my_data = True;

/* If a label strings resource array has been passed in, copy it to the ga */

	if (lg_p->label_strings == NULL) {
		lg_p->label_strings = ga;
	}
	else {
		def_string="";
		ret_1 = ValidatedGenArrayCopy(&(lg_p->label_strings),
					      ga,Qstring,
					      NhlLG_MAX_LBL_STRINGS,
					      &def_string,
					      NhlNlgLabelStrings, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
	}
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
	
	count = MAX(lg_p->box_count, NhlLG_DEF_BOX_COUNT) + 1;
	if ((f_p = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgBoxFractions);
		return FATAL;
	}
	
	f_p[0] = 0.0;
	for (i=1;i<lg_p->box_count; i++)
		f_p[i] = -1.0;
	f_p[lg_p->box_count] = 1.0;
	
	if ((ga = NhlCreateGenArray((NhlPointer)f_p,NhlTFloat,
				    sizeof(float),1,&count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNlgBoxFractions);
		return FATAL;
	}
	ga->my_data = True;
		
/* If a box fractions array has been passed in, copy it to the ga */

	if (lg_p->box_fractions == NULL) {
		lg_p->box_fractions = ga;
	}
	else {
		def_float=-1.0;
		ret_1 = ValidatedGenArrayCopy(&(lg_p->box_fractions),
					      ga,Qfloat,
					      NhlLG_MAX_BOXES+1,&def_float,
					      NhlNlgBoxFractions, entry_name);
		
		if ((ret = MIN(ret,ret_1)) < WARNING) 
				return ret;
	}
	     
/*=======================================================================*/
/*
 * Allocate the location array: use one more than
 * the current box count so that both ends of the legend can be stored
 */

	lg_p->box_locs = (float *) 
		NhlMalloc((lg_p->box_count+1) * sizeof(float));
	
	return (ret);
}

/*ARGSUSED*/
static NhlErrorTypes    AdjustGeometry
#if __STDC__
	(Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	LegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;
	NhlBoundingBox titleBB;
	NhlBoundingBox labelsBB;
	NhlBoundingBox legendBB;
	NhlBoundingBox tmpBB;
	float title_x = lg_p->title_x;
	float title_y = lg_p->title_y;
	float pos_offset = 0.0;
	float obj_offset = 0.0;
	float x_off, y_off;
	float small_axis;
	float minor_off;
	float major_off;
	float center_off;
	int i;

	small_axis = MIN(lg_p->lg_width, lg_p->lg_height);
/*
 * Get the bounding box of the labels, then adjust the box if it
 * overlaps the bar boundary. Next combine the two bounding boxes,
 * and adjust both the bar and the labels to the center of the combined box.
 */
	if (! lg_p->labels_on) {
		legendBB.l = lg_p->adj_bar.l;
		legendBB.r = lg_p->adj_bar.r;
		legendBB.b = lg_p->adj_bar.b;
		legendBB.t = lg_p->adj_bar.t;
	}
	else {
		if ((ret1 = NhlGetBB(lg_p->labels_id, &labelsBB)) < WARNING)
			return ret1;
		ret = MIN(ret1,ret);
		
		if (lg_p->orient == NhlHORIZONTAL) {
			
			obj_offset = lg_p->label_off * 
				(lg_p->adj_perim.t - lg_p->adj_perim.b);
			if (lg_p->label_pos == NhlTOP) {
				if (labelsBB.b < 
				    lg_p->adj_bar.t + obj_offset) {
					pos_offset = lg_p->adj_bar.t + 
						obj_offset - labelsBB.b;
				}
			}
			else if (lg_p->label_pos == NhlBOTTOM) {
				if (labelsBB.t > 
				    lg_p->adj_bar.b - obj_offset) {
					pos_offset = lg_p->adj_bar.b - 
						obj_offset - labelsBB.t;
				}
			}
			labelsBB.b += pos_offset;
			labelsBB.t += pos_offset;
		}
		else {
			
			obj_offset = lg_p->label_off * 
				(lg_p->adj_perim.r - lg_p->adj_perim.l);
			if (lg_p->label_pos == NhlRIGHT) {
				if (labelsBB.l < 
				    lg_p->adj_bar.r + obj_offset) {
					pos_offset = lg_p->adj_bar.r + 
						obj_offset - labelsBB.l;
				}
			}
			else if (lg_p->label_pos == NhlLEFT) {
				if (labelsBB.r > 
				    lg_p->adj_bar.l - obj_offset) {
					pos_offset = lg_p->adj_bar.l - 
						obj_offset - labelsBB.r;
				}
				
			}
			labelsBB.l += pos_offset;
			labelsBB.r += pos_offset;
		}
		
		tmpBB.l = MIN(labelsBB.l, lg_p->adj_bar.l);
		tmpBB.r = MAX(labelsBB.r, lg_p->adj_bar.r);
		tmpBB.b = MIN(labelsBB.b, lg_p->adj_bar.b);
		tmpBB.t = MAX(labelsBB.t, lg_p->adj_bar.t);
		legendBB.l = MIN(tmpBB.l, lg_p->labels.l);
		legendBB.r = MAX(tmpBB.r, lg_p->labels.r);
		legendBB.b = MIN(tmpBB.b, lg_p->labels.b);
		legendBB.t = MAX(tmpBB.t, lg_p->labels.t);

		if (lg_p->orient == NhlHORIZONTAL) {

			center_off = (legendBB.t - tmpBB.t + 
				      legendBB.b - tmpBB.b) / 2.0;
			labelsBB.b += center_off;
			labelsBB.t += center_off;
			lg_p->adj_bar.b += center_off;
			lg_p->adj_bar.t += center_off;
			pos_offset += center_off;
		}
		else {

			center_off = (legendBB.r - tmpBB.r +
				      legendBB.l - tmpBB.l) / 2.0;
			labelsBB.l += center_off;
			labelsBB.r += center_off;
			lg_p->adj_bar.l += center_off;
			lg_p->adj_bar.r += center_off;
			pos_offset += center_off;
		}		
		
	}

/*
 * handle the title
 */
	if (! lg_p->title_on || lg_p->max_title_ext <= 0.0) {
		titleBB.l = legendBB.l;
		titleBB.r = legendBB.r;
		titleBB.b = legendBB.b;
		titleBB.t = legendBB.t;
	}
	else {
		if ((ret1 = NhlGetBB(lg_p->title_id, &titleBB)) < WARNING)
			return ret1;
		ret = MIN(ret1,ret);
		
		
		/*
		 * Adjust for the title: 
		 * first move it out of the way of the legend, 
		 * then adjust for possibly larger justification area.
		 */
		
		
		if (lg_p->title_pos == NhlBOTTOM || lg_p->title_pos == NhlTOP)
			obj_offset = lg_p->title_off *
				(lg_p->adj_perim.t - lg_p->adj_perim.b);
		else
			obj_offset = lg_p->title_off *
				(lg_p->adj_perim.r - lg_p->adj_perim.l);
		if (lg_p->title_pos == NhlBOTTOM &&
		    titleBB.t > legendBB.b - obj_offset) {
			title_y -= titleBB.t + obj_offset - legendBB.b;
		}
		else if (lg_p->title_pos == NhlTOP &&
			 titleBB.b < legendBB.t + obj_offset) {
			title_y += legendBB.t + obj_offset - titleBB.b;
		}
		else if (lg_p->title_pos == NhlLEFT &&
			 titleBB.r > legendBB.l - obj_offset) {
			title_x -= titleBB.r + obj_offset - legendBB.l;
		}
		else if (lg_p->title_pos == NhlRIGHT &&
			 titleBB.l < legendBB.r + obj_offset) {
			title_x += legendBB.r + obj_offset - titleBB.l;
		}
		
		if (lg_p->title_pos == NhlTOP
		    || lg_p->title_pos == NhlBOTTOM) {
			switch (lg_p->title_just) {
			case NhlBOTTOMLEFT:
			case NhlCENTERLEFT:
			case NhlTOPLEFT:
				title_x = legendBB.l;
				break;
			case NhlBOTTOMCENTER:
			case NhlCENTERCENTER:
			case NhlTOPCENTER:
			default:
				title_x = legendBB.l + 
					(legendBB.r - legendBB.l) / 2.0;
				break;
			case NhlBOTTOMRIGHT:
			case NhlCENTERRIGHT:
			case NhlTOPRIGHT:
				title_x = legendBB.r;
				break;
			}
		}
		else if (lg_p->title_pos == NhlLEFT
			 || lg_p->title_pos == NhlRIGHT) {
			switch (lg_p->title_just) {
			case NhlBOTTOMLEFT:
			case NhlBOTTOMCENTER:
			case NhlBOTTOMRIGHT:
				title_y = legendBB.b;
				break;
			case NhlCENTERLEFT:
			case NhlCENTERCENTER:
			case NhlCENTERRIGHT:
			default:
				title_y = legendBB.b + 
					(legendBB.t - legendBB.b) / 2.0;
				break;
			case NhlTOPLEFT:
			case NhlTOPCENTER:
			case NhlTOPRIGHT:
				title_y = legendBB.t;
				break;
			}
		}
		titleBB.l += title_x - lg_p->title_x;
		titleBB.r += title_x - lg_p->title_x;
		titleBB.b += title_y - lg_p->title_y;
		titleBB.t += title_y - lg_p->title_y;
		titleBB.l = MIN(titleBB.l, lg_p->title.l);
		titleBB.r = MAX(titleBB.r, lg_p->title.r);
		titleBB.b = MIN(titleBB.b, lg_p->title.b);
		titleBB.t = MAX(titleBB.t, lg_p->title.t);
	}
/*
 * Determine the real perimeter and set the view accordingly
 */

	lg_p->real_perim.l = MIN(legendBB.l - lg_p->margin.l * small_axis, 
				 titleBB.l - lg_p->margin.l * small_axis);
	lg_p->real_perim.r = MAX(legendBB.r + lg_p->margin.r * small_axis, 
				 titleBB.r + lg_p->margin.r * small_axis);
	lg_p->real_perim.b = MIN(legendBB.b - lg_p->margin.b * small_axis, 
				 titleBB.b - lg_p->margin.b * small_axis);
	lg_p->real_perim.t = MAX(legendBB.t + lg_p->margin.t * small_axis, 
				 titleBB.t + lg_p->margin.t * small_axis);

/*
 * Adjust position based on the justification
 */
	switch (lg_p->just) {

	case NhlBOTTOMLEFT:
		x_off = lg_p->real_perim.l - lg_p->perim.l;
		y_off = lg_p->real_perim.b - lg_p->perim.b;
		break;
	case NhlCENTERLEFT:
		x_off = lg_p->real_perim.l - lg_p->perim.l;
		y_off = lg_p->real_perim.b + 
			(lg_p->real_perim.t - lg_p->real_perim.b)/2.0 -
				(lg_p->lg_y - lg_p->lg_height/2.0);
		break;
	case NhlTOPLEFT:
		x_off = lg_p->real_perim.l - lg_p->perim.l;
		y_off = lg_p->real_perim.t - lg_p->perim.t;
		break;
	case NhlBOTTOMCENTER:
		x_off = lg_p->real_perim.l + 
			(lg_p->real_perim.r - lg_p->real_perim.l)/2.0 -
				(lg_p->lg_x + lg_p->lg_width/2.0);
		y_off = lg_p->real_perim.b - lg_p->perim.b;
		break;
	case NhlCENTERCENTER:
	default:
		x_off = lg_p->real_perim.l + 
			(lg_p->real_perim.r - lg_p->real_perim.l)/2.0 -
				(lg_p->lg_x + lg_p->lg_width/2.0);
		y_off = lg_p->real_perim.b + 
			(lg_p->real_perim.t - lg_p->real_perim.b)/2.0 -
				(lg_p->lg_y - lg_p->lg_height/2.0);
		break;
	case NhlTOPCENTER:
		x_off = lg_p->real_perim.l + 
			(lg_p->real_perim.r - lg_p->real_perim.l)/2.0 -
				(lg_p->lg_x + lg_p->lg_width/2.0);
		y_off = lg_p->real_perim.t - lg_p->perim.t;
		break;
	case NhlBOTTOMRIGHT:
		x_off = lg_p->real_perim.r - lg_p->perim.r;
		y_off = lg_p->real_perim.b - lg_p->perim.b;
		break;
	case NhlCENTERRIGHT:
		x_off = lg_p->real_perim.r - lg_p->perim.r;
		y_off = lg_p->real_perim.b + 
			(lg_p->real_perim.t - lg_p->real_perim.b)/2.0 -
				(lg_p->lg_y - lg_p->lg_height/2.0);
		break;
	case NhlTOPRIGHT:
		x_off = lg_p->real_perim.r - lg_p->perim.r;
		y_off = lg_p->real_perim.t - lg_p->perim.t;
	}

	minor_off = (lg_p->orient == NhlHORIZONTAL) ? y_off : x_off;
	major_off = (lg_p->orient == NhlHORIZONTAL) ? x_off : y_off;
/*
 * Adjust the perimeter
 */

	lg_p->real_perim.l -= x_off;
	lg_p->real_perim.r -= x_off;
	lg_p->real_perim.b -= y_off;
	lg_p->real_perim.t -= y_off;
/*
 * Adjust the bar position
 */

	lg_p->adj_bar.l -= x_off;
	lg_p->adj_bar.r -= x_off;
	lg_p->adj_bar.b -= y_off;
	lg_p->adj_bar.t -= y_off;
	for (i=0; i<lg_p->box_count+1; i++) {
		lg_p->box_locs[i] -= major_off;
	}
/*
 * Adjust the labels position
 */

	if (lg_p->labels_on) {
		for (i=0; i<lg_p->label_draw_count; i++) {
			lg_p->label_locs[i] -= major_off;
		}
		
		if ((ret1 = NhlSetValues(lg_p->labels_id,
					 NhlNMtextConstPosF,
					 lg_p->const_pos + 
					 pos_offset - minor_off,
					 NhlNMtextPosArray,lg_p->label_locs,
					 NULL)) < WARNING)
			return (ret1);
		ret = MIN(ret1,ret);
	}
/*
 * Set the title position
 */

	if (lg_p->title_on && lg_p->max_title_ext > 0.0) {
		title_x -= x_off;
		title_y -= y_off;
		if ((ret1 = NhlSetValues(lg_p->title_id,
					 NhlNtxPosXF, title_x,
					 NhlNtxPosYF, title_y,
					 NULL)) < WARNING)
			return (ret1);
		ret = MIN(ret1,ret);
	}

	_NhlInternalSetView((ViewLayer)tnew,
			    lg_p->real_perim.l, lg_p->real_perim.t,
			    lg_p->real_perim.r - lg_p->real_perim.l,
			    lg_p->real_perim.t - lg_p->real_perim.b,
			    False);
	return (ret);

}

/*ARGSUSED*/
static NhlErrorTypes    SetBoxLocations
#if __STDC__
(
	Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		num_args
)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	LegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;
	float bar_len;
	int i;
	float *box_fractions = (float *)lg_p->box_fractions->data;

	if (lg_p->box_sizing == NhlLG_EXPLICITSIZING) {
		ret1 = ManageBoxFractionsArray(box_fractions,lg_p->box_count);
	}
	ret = MIN(ret,ret1);

/* 
 * Adjust boundary of bar and label area, depending on the label state.
 */

	memcpy((void *)&lg_p->adj_bar, (Const void *)&lg_p->bar, 
	       sizeof(NhlBoundingBox));
	memcpy((void *)&lg_p->adj_box_size, (Const void *)&lg_p->box_size,
	       sizeof(NhlCoord));
	if (lg_p->label_alignment == NhlLG_ABOVEBOXES) {
		if (lg_p->orient == NhlHORIZONTAL) {
			lg_p->adj_box_size.x = lg_p->box_size.x * 
				lg_p->box_count / (lg_p->box_count + 0.5);
			lg_p->adj_bar.r = 
				lg_p->bar.r - lg_p->adj_box_size.x / 2.0;
			lg_p->labels.l = 
				lg_p->bar.l + lg_p->adj_box_size.x / 2.0;
		}
		else {
			lg_p->adj_box_size.y = lg_p->box_size.y * 
				lg_p->box_count / (lg_p->box_count + 0.5);
			lg_p->adj_bar.t = 
				lg_p->bar.t - lg_p->adj_box_size.y / 2.0;
			lg_p->labels.b = 
				lg_p->bar.b + lg_p->adj_box_size.y / 2.0;
		}
	}
	else if (lg_p->label_alignment == NhlLG_BELOWBOXES) {
		if (lg_p->orient == NhlHORIZONTAL) {
			lg_p->adj_box_size.x = lg_p->box_size.x * 
				lg_p->box_count / (lg_p->box_count + 0.5);
			lg_p->adj_bar.l = 
				lg_p->bar.l + lg_p->adj_box_size.x / 2.0;
			lg_p->labels.l = lg_p->bar.l;
			lg_p->labels.r = 
				lg_p->bar.r - lg_p->adj_box_size.x / 2.0;
		}
		else {
			lg_p->adj_box_size.y = lg_p->box_size.y * 
				lg_p->box_count / (lg_p->box_count + 0.5);
			lg_p->adj_bar.b = 
				lg_p->bar.b + lg_p->adj_box_size.y / 2.0;
			lg_p->labels.t = 
				lg_p->bar.t - lg_p->adj_box_size.y / 2.0;
		}
	}

/*
 * create an array of the left or bottom varying coordinates -- 
 */
		
	if (lg_p->orient == NhlHORIZONTAL &&
	    lg_p->box_sizing == NhlLG_UNIFORMSIZING) {
		for (i=0; i < lg_p->box_count; i++) {
			lg_p->box_locs[i] = lg_p->adj_bar.l + 
				(float) i * lg_p->adj_box_size.x;
		}
		lg_p->box_locs[lg_p->box_count] = lg_p->adj_bar.r;
	}
	if (lg_p->orient == NhlVERTICAL &&
	    lg_p->box_sizing == NhlLG_UNIFORMSIZING) {
		for (i=0; i < lg_p->box_count; i++) {
			lg_p->box_locs[i] = lg_p->adj_bar.b + 
				(float) i * lg_p->adj_box_size.y;
		}
		lg_p->box_locs[lg_p->box_count] = lg_p->adj_bar.t;
	}
	if (lg_p->orient == NhlHORIZONTAL &&
	    lg_p->box_sizing == NhlLG_EXPLICITSIZING) {
		bar_len = lg_p->adj_bar.r - lg_p->adj_bar.l;
		for (i=0; i < lg_p->box_count; i++) {
			lg_p->box_locs[i] = lg_p->adj_bar.l + 
				bar_len * box_fractions[i];
		}
		lg_p->box_locs[lg_p->box_count] = lg_p->adj_bar.r;
	}
	if (lg_p->orient == NhlVERTICAL &&
	    lg_p->box_sizing == NhlLG_EXPLICITSIZING) {
		bar_len = lg_p->adj_bar.t - lg_p->adj_bar.b;
		for (i=0; i < lg_p->box_count; i++) {
			lg_p->box_locs[i] = lg_p->adj_bar.b + 
				bar_len * box_fractions[i];
		}
		lg_p->box_locs[lg_p->box_count] = lg_p->adj_bar.t;
	}

	return(ret);

}

/*ARGSUSED*/
static NhlErrorTypes    ManageBoxFractionsArray
#if __STDC__
	(float *box_fractions, 
	 int count) 
#else
(box_fractions, count)
	float *box_fractions;
	int count;
#endif

{
	int i, first_neg = -1;
	int ret = NOERROR;
			
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
		NhlPError(WARNING,E_UNKNOWN,
			  "Modifying invalid box fraction array element: 0");
		ret = WARNING;
		box_fractions[0] = 0.0;
	}
	if (box_fractions[count] < 0.0)
		box_fractions[count] = 1.0;
	else if (box_fractions[count] != 1.0) {
		NhlPError(WARNING,E_UNKNOWN,
			  "Modifying invalid box fraction array element: %d",
			  count);
		ret = WARNING;
		box_fractions[count] = 1.0;
	}

	for (i=1; i<count+1;i++) {
		if (box_fractions[i] < 0.0) {
			if (first_neg == -1)
				first_neg = i;
		}
		else if (first_neg != -1) {
			if (box_fractions[i] > 1.0 ||
			    box_fractions[i] <
			    box_fractions[first_neg-1]) {
				NhlPError(WARNING,E_UNKNOWN,
		    "Modifying invalid box fraction array element: %d",i);
				ret = WARNING;
				box_fractions[i] = -1.0;
			}
			else {
				CreateIntermediates(box_fractions, 
						    first_neg, i);
				first_neg = -1;
			}
		}
		else if (box_fractions[i] > 1.0 ||
			 (box_fractions[i] < box_fractions[i-1])) {
			NhlPError(WARNING,E_UNKNOWN,
		        "Modifying invalid box fraction array element: %d",i);
			ret = WARNING;
			box_fractions[i] = -1.0;
			first_neg = i;
		}
	}
	return (ret);
}

/*
 * Creates evenly spaced values to replace negative values in the
 * box fractions array. The start value is the index of the first
 * negative value in the chain; the end value is the index of the
 * first non-negative value. Since the zeroth value has been dealt with
 * separately, it is not necessary to test for it here.
 */

static void CreateIntermediates
#if __STDC__
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

/*ARGSUSED*/
static NhlErrorTypes    SetLabels
#if __STDC__
	(Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	LegendLayerPart *lg_p = &(tnew->legend);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;
	char buffer[MAXRESNAMLEN];
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

	if (! lg_p->labels_on)
		return NOERROR;

/*
 * Determine the multitext orientation and the NDC label offset
 */

	if (lg_p->orient == NhlHORIZONTAL) {
		mtext_orient = MTEXT_Y_CONST;
		label_offset = lg_p->label_off * 
			(lg_p->adj_perim.t - lg_p->adj_perim.b);
	}
	else {
		mtext_orient = MTEXT_X_CONST;
		label_offset = lg_p->label_off *
			(lg_p->adj_perim.r - lg_p->adj_perim.l);
	}
/*
 * If not in auto-manage mode the label offset should cause the 
 * bar to grow. Set it to 0.0 here so that it will take effect in the
 * AdjustGeometry routine.
 */ 

	count = lg_p->box_count;
		
/*
 * If the label stride is greater than 1, find the number of
 * labels to use. If greater than the previous size, reallocate
 * the array used to point to the correct labels.
 */
	
	lg_p->label_draw_count = count;
	labels_p = (NhlString *)lg_p->label_strings->data;
	if (lg_p->label_stride > 1) {
		count = (count % lg_p->label_stride == 0) ?
			count / lg_p->label_stride :  
			count / lg_p->label_stride + 1;
		if (count > lg_p->max_label_stride_count) {
			if (lg_p->stride_labels == NULL) {
				lg_p->stride_labels = (char **) 
					NhlMalloc(count * sizeof(char *));
			}
			else {
				lg_p->stride_labels = (char **)
					NhlRealloc(lg_p->stride_labels,
						   count * sizeof(char *));
			}
			lg_p->max_label_stride_count = count;
		}
		labels_p = lg_p->stride_labels;
		for (i=0,j=0; i<count; i++,j+=lg_p->label_stride) {
			labels_p[i] = 
				((NhlString *) lg_p->label_strings->data)[j];
		}
		lg_p->label_draw_count = count;
	}

/*
 * Now allocate the location array
 */
	if (count > lg_p->max_label_draw_count) {
		if (lg_p->label_locs == NULL) {
			lg_p->label_locs = (float *) 
				NhlMalloc(count * sizeof(float));
		}
		else {
			lg_p->label_locs = (float *)
				NhlRealloc(lg_p->label_locs,
					   count * sizeof(float));
		}
		lg_p->max_label_draw_count = count;
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

/*
 * Determine the overall area available for the labels
 */
	if (lg_p->label_pos == NhlCENTER) {
		if (lg_p->orient == NhlHORIZONTAL) {
			larea.x = lg_p->labels.r - lg_p->labels.l;
			larea.y = lg_p->adj_bar.t - lg_p->adj_bar.b;
		}
		else {
			larea.x = lg_p->adj_bar.r - lg_p->adj_bar.l;
			larea.y = lg_p->labels.t - lg_p->labels.b;
		}
	}
	else {
		if (lg_p->orient == NhlHORIZONTAL) {
			larea.x = lg_p->labels.r - lg_p->labels.l;
			larea.y = lg_p->labels.t - lg_p->labels.b - 
				label_offset;
		}
		else {
			larea.x = lg_p->labels.r - lg_p->labels.l - 
				label_offset;
			larea.y = lg_p->labels.t - lg_p->labels.b;
		}
	}
		
/*
 * Account for the box_major_ext fraction if centered or label
 * offset is negative
 */
	if (lg_p->label_pos == NhlCENTER || label_offset < 0.0) {
		if (lg_p->label_alignment == NhlLG_BOXCENTERS) {
			c_frac = lg_p->box_major_ext;
		}
		else {
			c_frac = 1.0 - lg_p->box_major_ext;
		}
	}
		
	if (lg_p->orient == NhlHORIZONTAL && 
	    lg_p->label_direction == ACROSS) {

		char_space = 0.8 * larea.y / (max_strlen + 1.0);
		avail_char_space = 0.5 * c_frac * 
			larea.x / lg_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		if (lg_p->label_pos == NhlBOTTOM) {
				
			lg_p->const_pos = lg_p->labels.t -
				label_height - label_offset;
		}
		else if (lg_p->label_pos == NhlCENTER) {
			lg_p->const_pos = lg_p->adj_bar.b + 
				lg_p->adj_box_size.y / 2.0;
		}
		else if (lg_p->label_pos == NhlTOP) {
			lg_p->const_pos = lg_p->labels.b + 
				label_height + label_offset;
		}
		else if (lg_p->label_pos == NhlBOTH) {
			printf("NhlBOTH not implemented\n");
		}
	}
	else if (lg_p->orient == NhlHORIZONTAL){ /* DOWN or UP */

		/* Set the font height */

		char_space = 0.8 * larea.y / (max_strlen + 1.0);
		avail_char_space = 0.5 * c_frac *
			larea.x / lg_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant Y axis value and the justification */
		if (lg_p->label_pos == NhlBOTTOM) {
			lg_p->const_pos = lg_p->labels.t - 
				label_height - label_offset;
		}
		else if (lg_p->label_pos == NhlCENTER) {
			lg_p->const_pos = lg_p->adj_bar.b + 
				lg_p->adj_box_size.y / 2.0;
		}
		else if (lg_p->label_pos == NhlTOP) {
			lg_p->const_pos = lg_p->labels.b + 
				label_height + label_offset;
		}
		else if (lg_p->label_pos == NhlBOTH) {
			printf("NhlBOTH not implemented\n");
		}
	}
	else if (lg_p->orient == NhlVERTICAL && 
		 lg_p->label_direction == ACROSS) {

		char_space = 0.8 * larea.x / (max_strlen + 1.0);
		avail_char_space = 0.5 * c_frac *
			larea.y / lg_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		if (lg_p->label_pos == NhlLEFT) {
			lg_p->const_pos = lg_p->labels.r - 
				label_height - label_offset;
		}
		else if (lg_p->label_pos == NhlCENTER) {
			lg_p->const_pos = lg_p->adj_bar.l + 
				lg_p->adj_box_size.x / 2.0;
		}
		else if (lg_p->label_pos == NhlRIGHT) {
			lg_p->const_pos = lg_p->labels.l + 
				label_height + label_offset;
		}
		else if (lg_p->label_pos == NhlBOTH) {
			printf("NhlBOTH not implemented\n");
		}
	}
	else { /* NhlVERTICAL DOWN or UP */

		char_space = 0.8 * larea.x / (max_strlen + 1.0);
		avail_char_space = 0.5 * c_frac *
			larea.y / lg_p->label_draw_count;

		if (char_space < avail_char_space)
			label_height = char_space;
		else
			label_height = avail_char_space;

		/* Set the constant X axis position and the justification */
		if (lg_p->label_pos == NhlLEFT) {
			lg_p->const_pos = lg_p->labels.r - 
				label_height - label_offset;
		}
		else if (lg_p->label_pos == NhlCENTER) {
			lg_p->const_pos = lg_p->adj_bar.l + 
				lg_p->adj_box_size.x / 2.0;
		}
		else if (lg_p->label_pos == NhlRIGHT) {
			lg_p->const_pos = lg_p->labels.l + 
				label_height + label_offset;
		}
		else if (lg_p->label_pos == NhlBOTH) {
			printf("NhlBOTH not implemented\n");
		}
	}

/*
 * Now find the variable label positions
 */

	if (lg_p->box_sizing == NhlLG_UNIFORMSIZING) {

		if (lg_p->orient == NhlHORIZONTAL) {
		
			/* position array contains X values */

			base_pos = lg_p->adj_bar.l;
		       
			/* determine offset */
			if (lg_p->label_alignment == NhlLG_BOXCENTERS)
				offset = lg_p->adj_box_size.x / 2.0;
			else if (lg_p->label_alignment == NhlLG_ABOVEBOXES)
				offset = lg_p->adj_box_size.x;
			else
				offset = 0;

			increment = lg_p->adj_box_size.x * lg_p->label_stride;
		}
		else if (lg_p->orient == NhlVERTICAL) {
		
			/* position array contains Y values */

			base_pos = lg_p->adj_bar.b;
		       
			/* determine offset */
			if (lg_p->label_alignment == NhlLG_BOXCENTERS)
				offset = lg_p->adj_box_size.y / 2.0;
			else if (lg_p->label_alignment == NhlLG_ABOVEBOXES)
				offset = lg_p->adj_box_size.y;
			else
				offset = 0;

			increment = lg_p->adj_box_size.y * lg_p->label_stride;
		}
		for (i=0; i<lg_p->label_draw_count; i++) 
			lg_p->label_locs[i] = base_pos + offset + 
				(float) i * increment;
	}
	else {
		for (i=0; i < lg_p->label_draw_count; i++) {

			ix = i * lg_p->label_stride;
			if (lg_p->label_alignment == NhlLG_BOXCENTERS)
				lg_p->label_locs[i] = lg_p->box_locs[ix] +
					(lg_p->box_locs[ix+1] -
					  lg_p->box_locs[ix]) / 2.0;
			else if (lg_p->label_alignment == NhlLG_ABOVEBOXES)
				lg_p->label_locs[i] = lg_p->box_locs[ix+1];
			else
				lg_p->label_locs[i] = lg_p->box_locs[ix];

		}
	}

	if (! lg_p->auto_manage && lg_p->label_height > 0.0) 
		label_height = lg_p->label_height;

	if (init) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Labels");
		ret1 = NhlCreate(&(lg_p->labels_id),buffer,
				 multiTextLayerClass,tnew->base.id,
				 NhlNMtextNumStrings,lg_p->label_draw_count,
				 NhlNMtextStrings,labels_p,
				 NhlNMtextOrientation,mtext_orient,
				 NhlNMtextConstPosF,lg_p->const_pos ,
				 NhlNMtextPosArray,lg_p->label_locs,
				 NhlNtxAngleF,lg_p->label_angle,
				 NhlNtxFont,lg_p->label_font,
				 NhlNtxJust,lg_p->label_just,
				 NhlNtxFontHeightF,label_height,
				 NhlNtxFontAspectF,lg_p->label_aspect,
				 NhlNtxDirection,lg_p->label_direction,
				 NhlNtxConstantSpacingF,
				 lg_p->label_const_spacing,
				 NhlNtxFontColor,lg_p->label_color,
				 NhlNtxFontThicknessF,lg_p->label_thickness,
				 NULL);
		if(ret1 < WARNING) {
			NhlPError(FATAL,E_UNKNOWN,"MultiText create error");
			return(FATAL);
		}
		ret = MIN(ret,ret1);

	} 
	else {
		ret1 = NhlSetValues(lg_p->labels_id,
				    NhlNMtextNumStrings,lg_p->label_draw_count,
				    NhlNMtextStrings,labels_p,
				    NhlNMtextOrientation,mtext_orient,
				    NhlNMtextConstPosF,lg_p->const_pos,
				    NhlNMtextPosArray,lg_p->label_locs,
				    NhlNtxAngleF,lg_p->label_angle,
				    NhlNtxFont,lg_p->label_font,
				    NhlNtxJust,lg_p->label_just,
				    NhlNtxFontHeightF,label_height,
				    NhlNtxFontAspectF,lg_p->label_aspect,
				    NhlNtxDirection,lg_p->label_direction,
				    NhlNtxFontColor,lg_p->label_color,
				    NhlNtxFontThicknessF,lg_p->label_thickness,
				    NULL);
		if(ret1 < WARNING) {
			NhlPError(FATAL,E_UNKNOWN,
				  "Multitext SetValues error");
			return(FATAL);
		}
		ret = MIN(ret,ret1);

	}


	if (lg_p->auto_manage || lg_p->label_height <= 0.0) {

		ret1 = AdjustLabels(lg_p, label_height, avail_char_space, 
				   max_strlen, larea.x, larea.y);
		ret = MIN(ret, ret1);
	}

	return (ret);
}

/*
 * This function adjusts the label height to fit
 * the size it has available. This should probably be replaced by an option
 * within Multitext itself. The routine tries to ensure that under 
 * any rotation, no piece of multitext overlap another piece of the 
 * same text. The text size is adjusted based on some primitive 
 * heuristics to accomplish this. Also the label text justification is
 * managed to ensure that the label always lines up with its correct box.
 */

#define NhlLG_TRANSITIONANGLE 7.5
static NhlErrorTypes   	AdjustLabels
#if __STDC__
	(LegendLayerPart *lg_p,
	 float		height,
	 float		avail_space,
	 int		max_strlen,
	 float		area_x,
	 float		area_y)
#else
(lg_p, height, avail_space, max_strlen, area_x, area_y)
	LegendLayerPart *lg_p;
	float		height;
	float		avail_space;
	int		max_strlen;
	float		area_x;
	float		area_y;
#endif
{
	float tmp, theta1, theta2, theta3, theta4;
	NhlErrorTypes ret = NOERROR;
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
	ret = NhlGetBB(lg_p->labels_id, &stringBB);

	w=stringBB.r-stringBB.l; 	    
	h=stringBB.t-stringBB.b;
	if (w <= 0.0 || h <= 0.0) {
		printf("error getting BB\n");
	}
	if (lg_p->orient == NhlHORIZONTAL) {
		wb = area_x / lg_p->label_draw_count;
		wt = wb - area_x + w;
		hb = area_y;
		ht = h;
	}
	else {
		wb = area_x;
		wt = w;
		hb = area_y / lg_p->label_draw_count;
		ht = hb - area_y + h;
	}

/*
 * If labels are centered in the boxes just fit the text in the box
 * then get out.
 */

	if (lg_p->label_pos == NhlCENTER) {
		t1 = wb / wt < hb / ht ?  wb / wt  : hb / ht;
		height *= t1 * 0.8;
		height = MIN(height,avail_space);
		lg_p->label_just = NhlCENTERCENTER;
		lg_p->label_height = height;
		ret = NhlSetValues(lg_p->labels_id,
				   NhlNtxFontHeightF,lg_p->label_height,
				   NhlNtxJust,lg_p->label_just,
				   NULL);
		return (ret);
	}

/*
 * Get the sin and cos of the label angle - then create some
 * permutations of the angle in order to set the justification properly
 */
	theta = DEGTORAD * lg_p->label_angle;
	ct = fabs(cos(theta));
	st = fabs(sin(theta));
	
	if (ct < 0.01) ct = 0.01;
	if (st < 0.01) st = 0.01;
	
	theta1 = lg_p->label_angle < 360.0 - lg_p->label_angle ?
		lg_p->label_angle : 360.0 - lg_p->label_angle;
	theta2 = fabs(90.0 - lg_p->label_angle);
	theta3 = fabs(180.0 - lg_p->label_angle);
	theta4 = fabs(270.0 - lg_p->label_angle);

/*
 * Modify the text height: the critical angle is the point where text
 * begins to overlap. Outside the critical area the size of the text
 * is adjusted to make it as large as possible with no increase in the
 * size of the legend's minor axis. Inside the critical area, the size
 * of the text is reduced using (at the moment) a rather clumsy algorithm,
 * until it reaches a minimum size at the point where the text line is 
 * parallel to the legend major axis.
 * Then manage the text justification.
 */
	if (lg_p->orient == NhlHORIZONTAL && 
	    lg_p->label_direction == ACROSS) {

		height /= st;
		height = MIN(height, avail_space);
		tmp = height / lg_p->adj_box_size.x > 1.0 ? 1.0 :
			height / lg_p->adj_box_size.x;
 		c_angle = asin(tmp) / DEGTORAD;

		if (theta1 <= c_angle || theta3 <= c_angle) {
			c_angle = MIN(c_angle,45.0);
			t1 = 0.8 * wb / (max_strlen +2);
			t2 = t1 + (st / fabs(sin(DEGTORAD * c_angle))) *
				fabs(0.8 *avail_space - t1);
			height = height < t2 ? height : t2;
		}

		if (theta1 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlTOPCENTER;
			}
			else {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (theta3 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lg_p->label_just = NhlTOPCENTER;
			}
		}
		else if (lg_p->label_angle < 90.0) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlTOPRIGHT;
			}
			else {
				lg_p->label_just = NhlBOTTOMLEFT;
			}
		}
		else if (lg_p->label_angle < 180.0) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlBOTTOMRIGHT;
			}
			else {
				lg_p->label_just = NhlTOPLEFT;
			}
		}
		else if (lg_p->label_angle < 270.0) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlBOTTOMLEFT;
			}
			else {
				lg_p->label_just = NhlTOPRIGHT;
			}
		}
		else {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlTOPLEFT;
			}
			else {
				lg_p->label_just = NhlBOTTOMRIGHT;
			}
		}
	}
	else if (lg_p->orient == NhlHORIZONTAL){ /* DOWN or UP */

		height /= ct;
		height = MIN(height, avail_space);
		tmp = height / lg_p->adj_box_size.x > 1.0 ? 1.0 :
			height / lg_p->adj_box_size.x;
 		c_angle = asin(tmp) / DEGTORAD;

		if (theta2 <= c_angle || theta4 <= c_angle) {
			c_angle = MIN(c_angle,45.0);
			t1 = 0.8 * hb / (max_strlen +2);
			t2 = t1 + (ct / fabs(cos(DEGTORAD * c_angle))) *
				fabs(0.8 * avail_space - t1);
			height = MIN(height, t2);
		}

		if (theta2 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lg_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta4 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlCENTERLEFT;
			}
			else {
				lg_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (lg_p->label_angle > 270.0 ||
			 lg_p->label_angle < 90.0) {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlTOPCENTER;
			}
			else {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else {
			if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lg_p->label_just = NhlTOPCENTER;
			}
		}
			 
	}
	else if (lg_p->orient == NhlVERTICAL && 
		 lg_p->label_direction == ACROSS) {

		height /= ct;
		height = MIN(height, avail_space);
		tmp = height / lg_p->adj_box_size.y > 1.0 ? 1.0 :
			height / lg_p->adj_box_size.y;
 		c_angle = asin(tmp) / DEGTORAD;

		if (theta2 <= c_angle || theta4 <= c_angle) {
			c_angle = MIN(c_angle,45.0);
			t1 = 0.8 * hb / (max_strlen +2);
			t2 = t1 + (ct / fabs(cos(DEGTORAD * c_angle))) *
				fabs(0.8 * avail_space - t1);
			height = MIN(height, t2);
		}

		if (theta2 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
			else {
				lg_p->label_just = NhlTOPCENTER;
			}
		}
		else if (theta4 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlTOPCENTER;
			}
			else {
				lg_p->label_just = NhlBOTTOMCENTER;
			}
		}
		else if (lg_p->label_angle > 270.0  ||
			 lg_p->label_angle < 90.0) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lg_p->label_just = NhlCENTERLEFT;
			}
		}
		else {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlCENTERLEFT;
			}
			else {
				lg_p->label_just = NhlCENTERRIGHT;
			}
		}
			 
	}
	else { /* NhlVERTICAL DOWN or UP */

		height /= st;
		height = MIN(height, avail_space);
		tmp = height / lg_p->adj_box_size.y > 1.0 ? 1.0 :
			height / lg_p->adj_box_size.y;
 		c_angle = asin(tmp) / DEGTORAD;
		if (theta1 <= c_angle || theta3 <= c_angle) {
			c_angle = MIN(c_angle,45.0);
			t1 = 0.8 * wb / (max_strlen +2);
			t2 = t1 + (st / fabs(sin(DEGTORAD * c_angle))) *
				fabs(0.8 * avail_space - t1);
			height = height < t2 ? height : t2;
		}

		if (theta1 <=NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlCENTERRIGHT;
			}
			else {
				lg_p->label_just = NhlCENTERLEFT;
			}
		}
		else if (theta3 <= NhlLG_TRANSITIONANGLE) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlCENTERLEFT;
			}
			else {
				lg_p->label_just = NhlCENTERRIGHT;
			}
		}
		else if (lg_p->label_angle < 180.0) {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlBOTTOMRIGHT;
			}
			else {
				lg_p->label_just = NhlTOPLEFT;
			}
		}
		else {
			if (lg_p->label_pos == NhlLEFT) {
				lg_p->label_just = NhlTOPLEFT;
			}
			else {
				lg_p->label_just = NhlBOTTOMRIGHT;
			}
		}
	}

/*
 * Set the newly determined text height and justification
 */
	lg_p->label_height = height;
	ret = NhlSetValues(lg_p->labels_id,
			   NhlNtxFontHeightF,lg_p->label_height,
			   NhlNtxJust,lg_p->label_just,
			   NULL);
	return (ret);
}

/*ARGSUSED*/
static NhlErrorTypes    SetTitle
#if __STDC__
	(Layer		new, 
	Layer		old,
	int		init,
	_NhlArgList	args,
	int		 num_args)
#else
(new,old,init,args,num_args)
	Layer		new;
	Layer		old;
	int		init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	LegendLayer	told = (LegendLayer) old;
	LegendLayerPart *lg_p = &(tnew->legend);
	LegendLayerPart *olg_p = &(told->legend);
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;
	char buffer[MAXRESNAMLEN];
	char *c_p;
	NhlBoundingBox titleBB;
	float w, h, wta, hta, factor, height;

/*
 * Only initialize a text item for the title if it is turned on
 * and the maximum title extent is greater than 0.0.
 */
	if (!lg_p->title_on || lg_p->max_title_ext <= 0.0)
		return ret;

/*
 * If the title string is NULL, create a default string.
 * The default string is the name of the label bar object
 */
	if (lg_p->title_string == NULL) {
                lg_p->title_string = (char*)
                        NhlMalloc((unsigned)strlen(tnew->base.name) +1);
                strcpy(lg_p->title_string,tnew->base.name);
        } 
	else if (init) {
                c_p = lg_p->title_string;
                lg_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
                strcpy(lg_p->title_string,c_p);
        }
	else if (lg_p->title_string != olg_p->title_string) {
		NhlFree(olg_p->title_string);
		c_p = lg_p->title_string;
		lg_p->title_string = (char*)NhlMalloc((unsigned)strlen(c_p)+1);
		strcpy(lg_p->title_string, c_p);
	}

	switch (lg_p->title_just) {
	case NhlBOTTOMLEFT:
		lg_p->title_x = lg_p->title.l;
		lg_p->title_y = lg_p->title.b;
		break;
	case NhlBOTTOMCENTER:
		lg_p->title_x = lg_p->title.l +
			(lg_p->title.r - lg_p->title.l)/2.0;
		lg_p->title_y = lg_p->title.b;
		break;
	case NhlBOTTOMRIGHT:
		lg_p->title_x = lg_p->title.r;
		lg_p->title_y = lg_p->title.b;
		break;
	case NhlCENTERLEFT:
		lg_p->title_x = lg_p->title.l;
		lg_p->title_y = lg_p->title.b +
			(lg_p->title.t - lg_p->title.b)/2.0;
		break;
	case NhlCENTERCENTER:
	default:
		lg_p->title_x = lg_p->title.l +
			(lg_p->title.r - lg_p->title.l)/2.0;
		lg_p->title_y = lg_p->title.b +
			(lg_p->title.t - lg_p->title.b)/2.0;
		break;
	case NhlCENTERRIGHT:
		lg_p->title_x = lg_p->title.r;
		lg_p->title_y = lg_p->title.b +
			(lg_p->title.t - lg_p->title.b)/2.0;
		break;
	case NhlTOPLEFT:
		lg_p->title_x = lg_p->title.l;
		lg_p->title_y = lg_p->title.t;
		break;
	case NhlTOPCENTER:
		lg_p->title_x = lg_p->title.l +
			(lg_p->title.r - lg_p->title.l)/2.0;
		lg_p->title_y = lg_p->title.t;
		break;
	case NhlTOPRIGHT:
		lg_p->title_x = lg_p->title.r;
		lg_p->title_y = lg_p->title.t;
		break;
	}

	if (lg_p->title_height <= 0.0) 
		height = NhlLG_DEF_CHAR_HEIGHT;
	else 
		height = lg_p->title_height;

	if (init || lg_p->title_id < 0) {
		strcpy(buffer,tnew->base.name);
		strcat(buffer,".Title");
		ret1 = NhlCreate(&lg_p->title_id,
				 buffer,textItemLayerClass,
				 tnew->base.id,
				 NhlNtxFont,lg_p->title_font,
				 NhlNtxString,lg_p->title_string,
				 NhlNtxPosXF,lg_p->title_x,
				 NhlNtxPosYF,lg_p->title_y,
				 NhlNtxDirection,lg_p->title_direction,
				 NhlNtxAngleF,lg_p->title_angle,
				 NhlNtxJust,(int)lg_p->title_just,
				 NhlNtxFontColor,lg_p->title_color,
				 NhlNtxFontHeightF,height,
				 NhlNtxFontAspectF,lg_p->title_aspect,
				 NhlNtxConstantSpacingF,
				 	lg_p->title_const_spacing,
				 NhlNtxFontQuality,lg_p->title_quality,
				 NhlNtxFuncCode,lg_p->title_func_code,
				 NhlNtxFontThicknessF,lg_p->title_thickness,
				 NULL);
		
	}
	else {
		ret1 = NhlSetValues(lg_p->title_id,
				    NhlNtxFont,lg_p->title_font,
				    NhlNtxString,lg_p->title_string,
				    NhlNtxPosXF,lg_p->title_x,
				    NhlNtxPosYF,lg_p->title_y,
				    NhlNtxDirection,lg_p->title_direction,
				    NhlNtxAngleF,lg_p->title_angle,
				    NhlNtxJust,(int)lg_p->title_just,
				    NhlNtxFontColor,lg_p->title_color,
				    NhlNtxFontHeightF,height,
				    NhlNtxFontAspectF,lg_p->title_aspect,
				    NhlNtxConstantSpacingF,
				    	lg_p->title_const_spacing,
				    NhlNtxFontQuality,lg_p->title_quality,
				    NhlNtxFuncCode,lg_p->title_func_code,
				    NhlNtxFontThicknessF,lg_p->title_thickness,
				    NULL);
	}
	ret = MIN(ret,ret1);

/*
 * If title_height is 0.0 or auto-manage is in effect adjust the title
 * to the space available
 */
	ret1 = NhlGetBB(lg_p->title_id, &titleBB);
	ret = MIN(ret,ret1);
	
	w=titleBB.r-titleBB.l;
	h=titleBB.t-titleBB.b;
	if (w <= 0.0 || h <= 0.0) {
		printf("error getting BB\n");
	}
	wta=lg_p->title.r-lg_p->title.l;
	hta=lg_p->title.t-lg_p->title.b;
	if (wta <= 0.0 || hta <= 0.0) {
		printf("error in title area\n");
	}
	if (lg_p->title_height <= 0.0 || lg_p->auto_manage) {
		factor = wta / w < hta / h ? wta / w : hta / h;
		lg_p->title_height = height * factor;
	}
	ret1 = NhlSetValues(lg_p->title_id,
			    NhlNtxFontHeightF,lg_p->title_height,
			    NULL);
	return MIN(ret1,ret);
}

/*ARGSUSED*/
static NhlErrorTypes    SetLegendGeometry
#if __STDC__
	 (Layer new, 
	 _NhlArgList args,
	 int num_args)
#else
(new,args,num_args)
	Layer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	LegendLayerPart *lg_p = &(tnew->legend);
	enum {NO_TITLE, MINOR_AXIS, MAJOR_AXIS} title_loc;
	float bar_ext, max_title_ext, adj_perim_width, adj_perim_height;
	float small_axis;
	float title_off = 0.0, angle_adj = 0.0;
	float title_angle, tan_t;

/* Calculate the ndc margin from the fractional margin */

	small_axis = MIN(lg_p->lg_width, lg_p->lg_height);

	lg_p->adj_perim.l = lg_p->perim.l + lg_p->margin.l * small_axis;
	lg_p->adj_perim.r = lg_p->perim.r - lg_p->margin.r * small_axis;
	lg_p->adj_perim.b = lg_p->perim.b + lg_p->margin.b * small_axis;
	lg_p->adj_perim.t = lg_p->perim.t - lg_p->margin.t * small_axis;
	adj_perim_width = lg_p->adj_perim.r - lg_p->adj_perim.l;
	adj_perim_height = lg_p->adj_perim.t - lg_p->adj_perim.b;
		
/*
 * Check the title extent to make sure it does not exceed the hard-coded
 * limit; then locate the title
 */
	if (lg_p->auto_manage && 
	    lg_p->max_title_ext + lg_p->title_off > 0.5) {
		/* need a WARNING */
		lg_p->max_title_ext = NhlLG_DEF_MAX_TITLE_EXT;
		lg_p->title_off = NhlLG_DEF_TITLE_OFF;
	}
	title_angle = lg_p->title_angle * DEGTORAD;
	tan_t = fabs(sin(title_angle)) / 
		     MAX(0.01, fabs(cos(title_angle)));
		    
	if (!lg_p->title_on)
		title_loc = NO_TITLE;
	else if (lg_p->orient == NhlHORIZONTAL) {
		if (lg_p->title_pos == NhlRIGHT ||
		    lg_p->title_pos == NhlLEFT) {
			title_loc = MAJOR_AXIS;
			title_off = lg_p->title_off * adj_perim_width;
			if (lg_p->title_direction == ACROSS)
				angle_adj = adj_perim_height / tan_t;
			else
				angle_adj = adj_perim_height * tan_t;
						
		}
		else {
			title_loc = MINOR_AXIS;
			title_off = lg_p->title_off * adj_perim_height;
			if (lg_p->title_direction == ACROSS)
				angle_adj = adj_perim_width * tan_t; 
			else
				angle_adj = adj_perim_width / tan_t;
		}
	}
	else {
		if (lg_p->title_pos == NhlRIGHT ||
		    lg_p->title_pos == NhlLEFT) {
			title_loc = MINOR_AXIS;
			title_off = lg_p->title_off * adj_perim_width;
			if (lg_p->title_direction == ACROSS)
				angle_adj = adj_perim_height / tan_t;
			else
				angle_adj = adj_perim_height * tan_t;
		}
		else {
			title_loc = MAJOR_AXIS;
			title_off = lg_p->title_off * adj_perim_height;
			if (lg_p->title_direction == ACROSS)
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

	if (! lg_p->auto_manage) {
		title_off = 0.0;
	}

/*
 * Silently modify the label position if not appropriate for the
 * current legend orientation. Also determine the maximum critical
 * angle for label angle adjustments.
 */

	if (lg_p->orient == NhlHORIZONTAL) {
		switch (lg_p->label_pos) {
		case NhlLEFT:
			lg_p->label_pos = NhlTOP;
			break;
		case NhlRIGHT:
			lg_p->label_pos = NhlBOTTOM;
			break;
		}
	}
	else {
		switch (lg_p->label_pos) {
		case NhlBOTTOM:
			lg_p->label_pos = NhlRIGHT;
			break;
		case NhlTOP:
			lg_p->label_pos = NhlLEFT;
			break;
		}

	}
		
/*
 * Determine dimensions: first pass determines basic position for
 * title, bar and labels. Bar dimensions may be adjusted later to 
 * ensure that labels are visible.
 */
	if (lg_p->orient == NhlHORIZONTAL) {
		bar_ext = adj_perim_height * lg_p->box_minor_ext;
		switch (title_loc) {

		case NO_TITLE:
			lg_p->bar.l = lg_p->adj_perim.l;
			lg_p->bar.r = lg_p->adj_perim.r;
			lg_p->labels.l = lg_p->bar.l;
			lg_p->labels.r = lg_p->bar.r;
					
			if (! lg_p->labels_on || 
			    lg_p->label_pos == NhlCENTER) {
				lg_p->bar.b = lg_p->adj_perim.b +
					(adj_perim_height - bar_ext) / 2.0;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.b;
				lg_p->labels.t = lg_p->bar.t;
			}
			else if (lg_p->label_pos == NhlTOP) {
				lg_p->bar.b = lg_p->adj_perim.b;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.t;
				lg_p->labels.t = lg_p->adj_perim.t;
			}
			else if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->bar.b = lg_p->adj_perim.t - bar_ext;
				lg_p->bar.t = lg_p->adj_perim.t;
				lg_p->labels.b = lg_p->adj_perim.b;
				lg_p->labels.t = lg_p->bar.b;
			}
			break;

		case MAJOR_AXIS:
			
			max_title_ext = 
				MIN(lg_p->max_title_ext * adj_perim_width,
					adj_perim_height / 
					strlen(lg_p->title_string) + 
					angle_adj);
			lg_p->title.b = lg_p->adj_perim.b;
			lg_p->title.t = lg_p->adj_perim.t;
				
			if (lg_p->title_pos == NhlLEFT) {
				lg_p->bar.l = lg_p->adj_perim.l +
					max_title_ext + title_off;
				lg_p->bar.r = lg_p->adj_perim.r;
				lg_p->title.l = lg_p->adj_perim.l;
				lg_p->title.r = lg_p->bar.l - title_off;
			}
			else if (lg_p->title_pos == NhlRIGHT) {
				lg_p->bar.l = lg_p->adj_perim.l;
				lg_p->bar.r = lg_p->adj_perim.r - 
					max_title_ext - title_off;
				lg_p->title.l = lg_p->bar.r + title_off;
				lg_p->title.r = lg_p->adj_perim.r;
			}
			lg_p->labels.l = lg_p->bar.l;
			lg_p->labels.r = lg_p->bar.r;

			if (! lg_p->labels_on || 
			    lg_p->label_pos == NhlCENTER) {
				lg_p->bar.b = lg_p->adj_perim.b +
					(adj_perim_height - bar_ext) / 2.0;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.b;
				lg_p->labels.t = lg_p->bar.t;
			}
			else if (lg_p->label_pos == NhlTOP) {
				lg_p->bar.b = lg_p->adj_perim.b;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.t;
				lg_p->labels.t = lg_p->adj_perim.t;
			}
			else if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->bar.b = lg_p->adj_perim.t - bar_ext;
				lg_p->bar.t = lg_p->adj_perim.t;
				lg_p->labels.b = lg_p->adj_perim.b;
				lg_p->labels.t = lg_p->bar.b;
			}
			break;

		case MINOR_AXIS:

			max_title_ext = 
				MIN(lg_p->max_title_ext * adj_perim_height,
					adj_perim_width / 
					strlen(lg_p->title_string) + 
					angle_adj);
			if (max_title_ext + title_off + bar_ext > 
			    adj_perim_height) {
				/* need a WARNING */
				printf("adjusting bar size smaller\n");
				bar_ext = adj_perim_height - 
					max_title_ext - title_off;
				lg_p->box_minor_ext = 
					bar_ext / adj_perim_height;
			}
			lg_p->title.l = lg_p->adj_perim.l;
			lg_p->title.r = lg_p->adj_perim.r;
			lg_p->bar.l = lg_p->adj_perim.l;
			lg_p->bar.r = lg_p->adj_perim.r;
			lg_p->labels.l = lg_p->adj_perim.l;
			lg_p->labels.r = lg_p->adj_perim.r;
				
			if (lg_p->title_pos == NhlBOTTOM) {
				lg_p->title.b = lg_p->adj_perim.b;
				lg_p->title.t = lg_p->adj_perim.b + 
					max_title_ext;
				lg_p->bar.b = lg_p->title.t + title_off;
				lg_p->bar.t = lg_p->adj_perim.t;
			}
			else if (lg_p->title_pos == NhlTOP) {
				lg_p->bar.b = lg_p->adj_perim.b;
				lg_p->bar.t = lg_p->adj_perim.t - 
					max_title_ext - title_off;
				lg_p->title.b = lg_p->bar.t + title_off;
				lg_p->title.t = lg_p->adj_perim.t;
			}

			
			if (!lg_p->labels_on || lg_p->label_pos == NhlCENTER) {
				lg_p->bar.b = (lg_p->bar.t - lg_p->bar.b - 
					       bar_ext) / 2.0 + lg_p->bar.b;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.b;
				lg_p->labels.t = lg_p->bar.t;
			}
			else if (lg_p->label_pos == NhlTOP) {
				lg_p->labels.t = lg_p->bar.t;
				lg_p->bar.t = lg_p->bar.b + bar_ext;
				lg_p->labels.b = lg_p->bar.t;
			}
			else if (lg_p->label_pos == NhlBOTTOM) {
				lg_p->labels.b = lg_p->bar.b;
				lg_p->bar.b = lg_p->bar.t - bar_ext;
				lg_p->labels.t = lg_p->bar.b;
			}
			break;
		default:
			/* need error message */
			return FATAL;
		}
		/* get the box size */
		
		lg_p->box_size.x = 
			(lg_p->bar.r - lg_p->bar.l) / lg_p->box_count;
		lg_p->box_size.y = lg_p->bar.t - lg_p->bar.b;
	}

	else { /* NhlVERTICAL */

		bar_ext = adj_perim_width * lg_p->box_minor_ext;

		switch (title_loc) {

		case NO_TITLE:
			lg_p->bar.b = lg_p->adj_perim.b;
			lg_p->bar.t = lg_p->adj_perim.t;
			lg_p->labels.b = lg_p->adj_perim.b;
			lg_p->labels.t = lg_p->adj_perim.t;
					
			if (!lg_p->labels_on || lg_p->label_pos == NhlCENTER) {
				lg_p->bar.l = lg_p->adj_perim.l +
					(adj_perim_width - bar_ext) / 2.0;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.l;
				lg_p->labels.r = lg_p->bar.r;
			}
			else if (lg_p->label_pos == NhlRIGHT) {
				lg_p->bar.l = lg_p->adj_perim.l;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.r;
				lg_p->labels.r = lg_p->adj_perim.r;
			}
			else if (lg_p->label_pos == NhlLEFT) {
				lg_p->bar.l = lg_p->adj_perim.r - bar_ext;
				lg_p->bar.r = lg_p->adj_perim.r;
				lg_p->labels.l = lg_p->adj_perim.l;
				lg_p->labels.r = lg_p->bar.l;
			}
			break;

		case MAJOR_AXIS:
			max_title_ext = 
				MIN(lg_p->max_title_ext * adj_perim_height,
					adj_perim_width / 
					strlen(lg_p->title_string) + 
					angle_adj);

			lg_p->title.l = lg_p->adj_perim.l;
			lg_p->title.r = lg_p->adj_perim.r;
				
			if (lg_p->title_pos == NhlBOTTOM) {
				lg_p->bar.b = lg_p->adj_perim.b + 
					max_title_ext + title_off;
				lg_p->bar.t = lg_p->adj_perim.t;
				lg_p->title.b = lg_p->adj_perim.b;
				lg_p->title.t = lg_p->bar.b - title_off;
			}
			else if (lg_p->title_pos == NhlTOP) {
				lg_p->bar.b = lg_p->adj_perim.b;
				lg_p->bar.t = lg_p->adj_perim.t - 
					max_title_ext - title_off;
				lg_p->title.b = lg_p->bar.t + title_off;
				lg_p->title.t = lg_p->adj_perim.t;
			}
			lg_p->labels.b = lg_p->bar.b;
			lg_p->labels.t = lg_p->bar.t;

			if (! lg_p->labels_on || 
			    lg_p->label_pos == NhlCENTER) {
				lg_p->bar.l = lg_p->adj_perim.l +
					(adj_perim_width - bar_ext) / 2.0;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.l;
				lg_p->labels.r = lg_p->bar.r;
			}
			else if (lg_p->label_pos == NhlRIGHT) {
				lg_p->bar.l = lg_p->adj_perim.l;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.r;
				lg_p->labels.r = lg_p->adj_perim.r;
			}
			else if (lg_p->label_pos == NhlLEFT) {
				lg_p->bar.l = lg_p->adj_perim.r - bar_ext;
				lg_p->bar.r = lg_p->adj_perim.r;
				lg_p->labels.l = lg_p->adj_perim.l;
				lg_p->labels.r = lg_p->bar.l;
			}
			break;

		case MINOR_AXIS:

			max_title_ext = 
				MIN(lg_p->max_title_ext * adj_perim_width,
					adj_perim_height / 
					strlen(lg_p->title_string) + 
					angle_adj);
			if (max_title_ext + title_off + bar_ext > 
			    adj_perim_width) {
				/* need a WARNING */
				printf("adjusting bar size smaller\n");
				bar_ext = adj_perim_width - 
					max_title_ext - title_off;
				lg_p->box_minor_ext = 
					bar_ext / adj_perim_width;
			}

			lg_p->title.b = lg_p->adj_perim.b;
			lg_p->title.t = lg_p->adj_perim.t;
			lg_p->bar.b = lg_p->adj_perim.b;
			lg_p->bar.t = lg_p->adj_perim.t;
			lg_p->labels.b = lg_p->adj_perim.b;
			lg_p->labels.t = lg_p->adj_perim.t;
				
			if (lg_p->title_pos == NhlLEFT) {
				lg_p->title.l = lg_p->adj_perim.l;
				lg_p->title.r = lg_p->adj_perim.l + 
					max_title_ext;
				lg_p->bar.l = lg_p->title.r + title_off;
				lg_p->bar.r = lg_p->adj_perim.r;
			}
			else if (lg_p->title_pos == NhlRIGHT) {
				lg_p->bar.l = lg_p->adj_perim.l;
				lg_p->bar.r = lg_p->adj_perim.r - 
					max_title_ext - title_off;
				lg_p->title.l = lg_p->bar.r + title_off;
				lg_p->title.r = lg_p->adj_perim.r;
			}

			
			if (!lg_p->labels_on || lg_p->label_pos == NhlCENTER) {
				lg_p->bar.l = (lg_p->bar.r - lg_p->bar.l - 
					       bar_ext) / 2.0 + lg_p->bar.l;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.l;
				lg_p->labels.r = lg_p->bar.r;
			}
			else if (lg_p->label_pos == NhlRIGHT) {
				lg_p->labels.r = lg_p->bar.r;
				lg_p->bar.r = lg_p->bar.l + bar_ext;
				lg_p->labels.l = lg_p->bar.r;
			}
			else if (lg_p->label_pos == NhlLEFT) {
				lg_p->labels.l = lg_p->bar.l;
				lg_p->bar.l = lg_p->bar.r - bar_ext;
				lg_p->labels.r = lg_p->bar.l;
			}
			break;
		default:
			/* need error message */
			return FATAL;
		}
		lg_p->box_size.x = lg_p->bar.r - lg_p->bar.l;
		lg_p->box_size.y = 
			(lg_p->bar.t - lg_p->bar.b) / lg_p->box_count;
	}

	return NOERROR;
				
}

/*
 * Function:	LegendDraw
 *
 * Description:  Activates parent workstation, calls plotchar parameter 
 *		setting functions and then GKS attributes for linewidth, 
 *		fill styles, fill colors and line colors.
 *
 * In Args: the instance to be drawn
 *
 * Out Args: NONE
 *
 * Return Values: Error conditions
 *
 * Side Effects: GKS and plotchar state affected. 
 *		Does do a get_set before makeing internal set.
 */
static NhlErrorTypes    LegendDraw
#if  __STDC__
(Layer layer)
#else
(layer)
	Layer 	layer;
#endif
{
	LegendLayer tlayer = (LegendLayer) layer;
	LegendLayerPart *lg_p = &(tlayer->legend);
	NhlErrorTypes ret = NOERROR;

	float xpoints[5];
	float ypoints[5];
	int i;
	int item_color;
	int item_type;
	float item_text_height, item_thickness;
	float frac, dist, tcoord;
	int *colors;
	int *indexes;
	int *types;
	float *thicknesses;
	float *text_heights;
	NhlString *item_strings;

	if (! lg_p->legend_on)
		return(ret);

	_NhlActivateWorkstation(tlayer->base.wkptr);

/* first draw the perimeter: it may have a solid background */

	if (lg_p->perim_on) {

		xpoints[0] = lg_p->real_perim.l;
		ypoints[0] = lg_p->real_perim.b;
		xpoints[1] = lg_p->real_perim.r;;
		ypoints[1] = lg_p->real_perim.b;
		xpoints[2] = lg_p->real_perim.r;
		ypoints[2] = lg_p->real_perim.t;
		xpoints[3] = lg_p->real_perim.l;
		ypoints[3] = lg_p->real_perim.t;
		xpoints[4] = lg_p->real_perim.l;
		ypoints[4] = lg_p->real_perim.b;

		NhlSetValues(tlayer->base.wkptr->base.id,
			     NhlNwkDrawEdges, 1,
			     NhlNwkEdgeDashPattern, lg_p->perim_dash_pattern,
			     NhlNwkEdgeThicknessF, lg_p->perim_thickness,
			     NhlNwkEdgeDashSegLenF, lg_p->perim_dash_length,
			     NhlNwkEdgeColor, lg_p->perim_color,
			     NhlNwkFillColor, lg_p->perim_fill_color,
			     NhlNwkFillIndex, lg_p->perim_fill,
			     NULL);
			
		_NhlSetFillInfo(tlayer->base.wkptr, layer);
		_NhlWorkstationFill(tlayer->base.wkptr,
				    xpoints,ypoints,5);
			
	}

/*
 * Set the values that remain constant for all boxes
 */

	NhlSetValues(tlayer->base.wkptr->base.id,
		     NhlNwkDrawEdges, lg_p->box_line_on,
		     NhlNwkEdgeDashPattern, lg_p->box_line_dash_pattern,
		     NhlNwkEdgeThicknessF, lg_p->box_line_thickness,
		     NhlNwkEdgeDashSegLenF, lg_p->box_line_dash_length,
		     NhlNwkEdgeColor, lg_p->box_line_color,
		     NhlNwkFillColor, lg_p->box_background,
		     NhlNwkFillIndex, NhlSOLIDFILL,
		     NULL);
				     
/* 
 * Draw the boxes
 */
	frac = (1.0 - lg_p->box_major_ext) / 2.0;
	colors = (int *) lg_p->item_colors->data;
	indexes = (int *) lg_p->item_indexes->data;
	types = (int *) lg_p->item_types->data;
	thicknesses = (float *) lg_p->item_thicknesses->data;
	text_heights = (float *) lg_p->item_text_heights->data;
	item_strings = (NhlString *) lg_p->item_strings->data;

	if (lg_p->orient == NhlHORIZONTAL) {

		ypoints[0] = lg_p->adj_bar.b;
		ypoints[1] = lg_p->adj_bar.b;
		ypoints[2] = lg_p->adj_bar.t;
		ypoints[3] = lg_p->adj_bar.t;
		ypoints[4] = lg_p->adj_bar.b;
		tcoord = ypoints[0] + (ypoints[2] - ypoints[1]) / 2.0;
		for (i=0; i<lg_p->box_count; i++) {
			dist = lg_p->box_locs[i+1] - lg_p->box_locs[i];
			xpoints[0] = lg_p->box_locs[i] + dist * frac;
			xpoints[1] = lg_p->box_locs[i+1] - dist * frac;
			xpoints[2] = xpoints[1];
			xpoints[3] = xpoints[0];
			xpoints[4] = xpoints[0];
			
			_NhlSetFillInfo(tlayer->base.wkptr, layer);
			_NhlWorkstationFill(tlayer->base.wkptr,
					    xpoints,ypoints,5);

			if (lg_p->mono_item_type)
				item_type = types[0];
			else
				item_type = types[i];

			if (lg_p->mono_item_color)
				item_color = colors[0];
			else
				item_color = colors[i];

			if (lg_p->mono_item_thickness)
				item_thickness = thicknesses[0];
			else
				item_thickness = thicknesses[i];
			
			if (lg_p->mono_item_text_height)
				item_text_height = text_heights[0];
			else
				item_text_height = text_heights[i];

			if (item_type == NhlLG_LINES) {
				NhlSetValues(tlayer->base.wkptr->base.id,
					     NhlNwkLineLabel, 
					        item_strings[i],
					     NhlNwkDashPattern, 
					        indexes[i],
					     NhlNwkLineThicknessF, 
					        item_thickness,
					     NhlNwkLineLabelFontHeightF,
					        item_text_height,
					     NhlNwkLineColor, item_color, 
					     NULL);
				_NhlSetLineInfo(tlayer->base.wkptr,layer);
				xpoints[0] = xpoints[0] + 
					(xpoints[1] - xpoints[0]) / 2.0;
				_NhlWorkstationLineTo(tlayer->base.wkptr, 
						      xpoints[0],
						      ypoints[0], 1);
				_NhlWorkstationLineTo(tlayer->base.wkptr, 
						      xpoints[0],
						      ypoints[2], 0);
		        }
			else {
				NhlSetValues(tlayer->base.wkptr->base.id,
					     NhlNwkMarkerString, 
					        item_strings[i],
					     NhlNwkMarkerIndex, 
					        indexes[i],
					     NhlNwkMarkerThicknessF, 
					        item_thickness,
					     NhlNwkMarkerSizeF,
					        item_text_height,
					     NhlNwkMarkerColor, item_color, 
					     NULL);
				_NhlSetMarkerInfo(tlayer->base.wkptr,layer);
				xpoints[0] = xpoints[0] + 
					(xpoints[1] - xpoints[0]) / 2.0;
				_NhlWorkstationMarker(tlayer->base.wkptr, 
						      xpoints,
						      &tcoord, 1);
			}
			
		}
	}
	else {
		xpoints[0] = lg_p->adj_bar.l;
		xpoints[1] = lg_p->adj_bar.r;
		xpoints[2] = lg_p->adj_bar.r;
		xpoints[3] = lg_p->adj_bar.l;
		xpoints[4] = lg_p->adj_bar.l;
		tcoord = xpoints[0] + (xpoints[1] - xpoints[0]) / 2.0;
		for (i=0; i< lg_p->box_count; i++) {
			dist = lg_p->box_locs[i+1] - lg_p->box_locs[i];
			ypoints[0] = lg_p->box_locs[i] + dist * frac;
			ypoints[1] = ypoints[0];
			ypoints[2] = lg_p->box_locs[i+1] - dist * frac;
			ypoints[3] = ypoints[2];
			ypoints[4] = ypoints[0];

			_NhlSetFillInfo(tlayer->base.wkptr, layer);
			_NhlWorkstationFill(tlayer->base.wkptr,
					    xpoints,ypoints,5);

			if (lg_p->mono_item_type)
				item_type = types[0];
			else
				item_type = types[i];

			if (lg_p->mono_item_color)
				item_color = colors[0];
			else
				item_color = colors[i];

			if (lg_p->mono_item_thickness)
				item_thickness = thicknesses[0];
			else
				item_thickness = thicknesses[i];
			
			if (lg_p->mono_item_text_height)
				item_text_height = text_heights[0];
			else
				item_text_height = text_heights[i];

			if (item_type == NhlLG_LINES) {
				NhlSetValues(tlayer->base.wkptr->base.id,
					     NhlNwkLineLabel, 
					        item_strings[i],
					     NhlNwkDashPattern, 
					        indexes[i],
					     NhlNwkLineThicknessF, 
					        item_thickness,
					     NhlNwkLineLabelFontHeightF,
					        item_text_height,
					     NhlNwkLineColor, item_color, 
					     NULL);
			   _NhlSetLineInfo(tlayer->base.wkptr,layer);
			   ypoints[0] = ypoints[0] + 
				   (ypoints[2] - ypoints[0]) / 2.0;
			   _NhlWorkstationLineTo(tlayer->base.wkptr, 
						 xpoints[0],
						 ypoints[0], 1);
			   _NhlWorkstationLineTo(tlayer->base.wkptr, 
						 xpoints[1],
						 ypoints[0], 0);
		        }
			else {
				NhlSetValues(tlayer->base.wkptr->base.id,
					     NhlNwkMarkerString, 
					        item_strings[i],
					     NhlNwkMarkerIndex, 
					        indexes[i],
					     NhlNwkMarkerThicknessF, 
					        item_thickness,
					     NhlNwkMarkerSizeF,
					        item_text_height,
					     NhlNwkMarkerColor, item_color, 
					     NULL);
				_NhlSetMarkerInfo(tlayer->base.wkptr,layer);
				ypoints[0] = ypoints[0] + 
					(ypoints[2] - ypoints[1]) / 2.0;
				_NhlWorkstationMarker(tlayer->base.wkptr, 
						      &tcoord,
						      ypoints, 1);
			}
			
		}
	}

	_NhlDeactivateWorkstation(tlayer->base.wkptr);

/*
 * Draw the child objects
 */
	if (lg_p->title_on && lg_p->max_title_ext > 0.0)
		NhlDraw(lg_p->title_id);

	if (lg_p->labels_on )
		NhlDraw(lg_p->labels_id);

	return(ret);
}

/*
 * Function:	LegendClassInitialize
 *
 * Description: 
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error condition
 *
 * Side Effects: 	NONE
 */
static NhlErrorTypes    LegendClassInitialize
#if  __STDC__
(void)
#else
()
#endif
{
	
	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qitem_types = NrmStringToQuark(NhlNlgItemTypes);
	Qitem_indexes = NrmStringToQuark(NhlNlgItemIndexes);
	Qitem_strings = NrmStringToQuark(NhlNlgItemStrings);
	Qitem_colors = NrmStringToQuark(NhlNlgItemColors);
	Qitem_thicknesses = NrmStringToQuark(NhlNlgItemThicknesses);
	Qitem_text_heights = NrmStringToQuark(NhlNlgItemTextHeights);
	Qlabel_strings = NrmStringToQuark(NhlNlgLabelStrings);
	Qbox_fractions = NrmStringToQuark(NhlNlgBoxFractions);

	_NhlInitializeLayerClass(textItemLayerClass);
	_NhlInitializeLayerClass(multiTextLayerClass);
	return(NOERROR);	
}


/*
 * Function:	LegendDestroy
 *
 * Description: Frees all dynamically allocated memory
 *
 * In Args:	Layer inst	instance of Legend
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes    LegendDestroy
#if  __STDC__
(Layer  inst)
#else
(inst)
	Layer	inst;
#endif
{
	LegendLayer tinst = (LegendLayer) inst;
	LegendLayerPart *lg_p = &(tinst->legend);

	NhlFree(lg_p->gks_colors);

	if (lg_p->stride_labels != NULL)
		NhlFree(lg_p->stride_labels);
	if (lg_p->label_locs != NULL)
		NhlFree(lg_p->label_locs);
	if (lg_p->box_locs != NULL)
		NhlFree(lg_p->box_locs);

	NhlFreeGenArray(lg_p->item_strings);
	NhlFreeGenArray(lg_p->label_strings);
	NhlFreeGenArray(lg_p->item_types);
	NhlFreeGenArray(lg_p->item_indexes);
	NhlFreeGenArray(lg_p->item_colors);
	NhlFreeGenArray(lg_p->item_thicknesses);
	NhlFreeGenArray(lg_p->item_text_heights);
	NhlFreeGenArray(lg_p->box_fractions);

	if (lg_p->labels_id >=0)
		NhlDestroy(lg_p->labels_id);

	if (lg_p->title_string != NULL) {
		NhlFree(lg_p->title_string);
		NhlDestroy(lg_p->title_id);
	}

	return(NOERROR);
}

