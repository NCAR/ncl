/*
 *      $Id: Contour.c,v 1.7 1994-03-02 01:43:47 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Contour.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a Contour plot object
 */

#include <stdio.h>
#include <math.h>
#include <float.h>
#include <ncarg/hlu/ContourP.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/IrregularType2TransObj.h>

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

static NhlResource resources[] = {
	{ NhlNcnOutOfRangeValF,NhlCcnOutOfRangeValF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.out_of_range_val),
		NhlTString,{"1.0E12"}},
	{ NhlNcnSpecialValF,NhlCcnSpecialValF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.special_val),
		NhlTString,{"-9999.0"}},
	{ NhlNcnLevelCount,NhlCcnLevelCount,NhlTInteger,sizeof(int),
		NhlOffset(NhlContourLayerRec,contour.level_count),
		NhlTImmediate,{(NhlPointer) 16}},
	{ NhlNcnLevelSelectionMode,NhlCcnLevelSelectionMode,
		  NhlTInteger,sizeof(int),
		  NhlOffset(NhlContourLayerRec,contour.level_selection_mode),
		  NhlTImmediate,{(NhlPointer) Nhl_cnAUTOMATIC}},
	{ NhlNcnMaxLevelCount,NhlCcnMaxLevelCount,NhlTInteger,sizeof(int),
		NhlOffset(NhlContourLayerRec,contour.max_level_count),
		NhlTImmediate,{(NhlPointer) 16}},
	{ NhlNcnLevelSpacingF,NhlCcnLevelSpacingF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.level_spacing),
		NhlTString,{"5.0"}},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.min_level_set),
		NhlTImmediate,{(NhlPointer)True}},
	{ NhlNcnMinLevelValF,NhlCcnMinLevelValF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.min_level_val),
		NhlTProcedure,{(NhlPointer)ResourceUnset}},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.max_level_set),
		NhlTImmediate,{(NhlPointer)True}},
	{ NhlNcnMaxLevelValF,NhlCcnMaxLevelValF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.max_level_val),
		NhlTProcedure,{(NhlPointer)ResourceUnset}},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.line_label_interval_set),
		NhlTImmediate,{(NhlPointer)True}},
	{ NhlNcnLineLabelInterval,NhlCcnLineLabelInterval,
		  NhlTFloat,sizeof(float),
		  NhlOffset(NhlContourLayerRec,contour.line_label_interval),
		  NhlTProcedure,{(NhlPointer)ResourceUnset}},
	{NhlNcnMonoLevelFlag, NhlCcnMonoLevelFlag, NhlTBoolean,
		 sizeof(NhlBoolean), 
		 NhlOffset(NhlContourLayerRec,contour.mono_level_flag),
		 NhlTImmediate,{(NhlPointer) False}},
	{NhlNcnMonoFillColor, NhlCcnMonoFillColor, NhlTBoolean,
		 sizeof(NhlBoolean), 
		 NhlOffset(NhlContourLayerRec,contour.mono_fill_color),
		 NhlTImmediate,{(NhlPointer) False}},
	{NhlNcnMonoFillPattern, NhlCcnMonoFillPattern, NhlTBoolean,
		 sizeof(NhlBoolean), 
		 NhlOffset(NhlContourLayerRec,contour.mono_fill_pattern),
		 NhlTImmediate,{(NhlPointer) False}},
	{NhlNcnMonoFillScale, NhlCcnMonoFillScale, NhlTBoolean,
		 sizeof(NhlBoolean), 
		 NhlOffset(NhlContourLayerRec,contour.mono_fill_scale),
		 NhlTImmediate,{(NhlPointer) True}},
	{NhlNcnMonoLineColor, NhlCcnMonoLineColor, NhlTBoolean,
		 sizeof(NhlBoolean), 
		 NhlOffset(NhlContourLayerRec,contour.mono_line_color),
		 NhlTImmediate,{(NhlPointer) True}},
	{NhlNcnMonoLineDashPattern, NhlCcnMonoLineDashPattern, NhlTBoolean,
		 sizeof(NhlBoolean), 
		 NhlOffset(NhlContourLayerRec,contour.mono_line_dash_pattern),
		 NhlTImmediate,{(NhlPointer) True}},

	{NhlNcnMonoLineThickness, NhlCcnMonoLineThickness, NhlTBoolean,
		 sizeof(NhlBoolean), 
		 NhlOffset(NhlContourLayerRec,contour.mono_line_thickness),
		 NhlTImmediate,{(NhlPointer) True}},
	{NhlNcnMonoLineLabelColor, NhlCcnMonoLineLabelColor, NhlTBoolean,
		 sizeof(NhlBoolean), 
		 NhlOffset(NhlContourLayerRec,contour.mono_line_label_color),
		 NhlTImmediate,{(NhlPointer) True}},

/* Array resources */

	{NhlNcnLevels, NhlCcnLevels, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.levels),
		 NhlTImmediate,{(NhlPointer) NULL}},
	{NhlNcnLevelFlags, NhlCcnLevelFlags, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.level_flags),
		 NhlTImmediate,{(NhlPointer) NULL}},
	{NhlNcnFillColors, NhlCcnFillColors, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.fill_colors),
		 NhlTImmediate,{(NhlPointer) NULL}},
	{NhlNcnFillPatterns, NhlCcnFillPatterns, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.fill_patterns),
		 NhlTImmediate,{(NhlPointer) NULL}},
	{NhlNcnFillScales, NhlCcnFillScales, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.fill_scales),
		 NhlTImmediate,{(NhlPointer) NULL} },
	{NhlNcnLineColors, NhlCcnLineColors, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.line_colors),
		 NhlTImmediate,{(NhlPointer) NULL}},
	{NhlNcnLineDashPatterns, NhlCcnLineDashPatterns, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.line_dash_patterns),
		 NhlTImmediate,{(NhlPointer) NULL}},
	{NhlNcnLineThicknesses, NhlCcnLineThicknesses, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.line_thicknesses),
		 NhlTImmediate,{(NhlPointer) NULL}},
	{NhlNcnLineLabelStrings, NhlCcnLineLabelStrings, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.line_label_strings),
		 NhlTImmediate,{(NhlPointer) NULL}},
	{NhlNcnLineLabelColors, NhlCcnLineLabelColors, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.line_label_colors),
		 NhlTImmediate,{(NhlPointer) NULL}},


        { NhlNcnLineDashSegLenF, NhlCcnLineDashSegLenF,NhlTFloat,sizeof(float),
		  NhlOffset(NhlContourLayerRec,contour.line_dash_seglen),
		  NhlTString,{ ".15" }},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
	      NhlOffset(NhlContourLayerRec,contour.line_label_text_height_set),
		NhlTImmediate,{(NhlPointer)True}},
        { NhlNcnLineLabelTextHeightF,NhlCcnLineLabelTextHeightF,
		  NhlTFloat,sizeof(float),
		  NhlOffset(NhlContourLayerRec,contour.line_label_text_height),
		  NhlTProcedure,{(NhlPointer)ResourceUnset}},

	{ NhlNtrXMinF,NhlCtrXMinF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.x_min),
		NhlTString,{"0.0"}},
	{ NhlNtrXMaxF,NhlCtrXMaxF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.x_max),
		NhlTString,{"1.0"}},
	{ NhlNtrXLog,NhlCtrXLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.x_log),
		NhlTImmediate,{(NhlPointer)False}},
	{ NhlNtrXReverse,NhlCtrXReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.x_reverse),
		NhlTImmediate,{(NhlPointer)False}},
	{ NhlNtrYMinF,NhlCtrYMinF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.y_min),
		NhlTString,{"0.0"}},
	{ NhlNtrYMaxF,NhlCtrYMaxF,NhlTFloat,sizeof(float),
		NhlOffset(NhlContourLayerRec,contour.y_max),
		NhlTString,{"1.0"}},
	{ NhlNtrYLog,NhlCtrYLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.y_log),
		NhlTImmediate,{(NhlPointer)False}},
	{ NhlNtrYReverse,NhlCtrYReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlContourLayerRec,contour.y_reverse),
		NhlTImmediate,{(NhlPointer)False}},
	{ NhlNovDisplayLabelBar,NhlCovDisplayLabelBar,NhlTInteger,sizeof(int),
		  NhlOffset(NhlContourLayerRec,contour.display_labelbar),
		  NhlTImmediate,{(NhlPointer) Nhl_ovNoCreate}},
	{ NhlNovDisplayLegend,NhlCovDisplayLegend,NhlTInteger,sizeof(int),
		  NhlOffset(NhlContourLayerRec,contour.display_legend),
		  NhlTImmediate,{(NhlPointer) Nhl_ovNoCreate}},
	{ NhlNovUpdateReq,NhlCovUpdateReq,NhlTInteger,sizeof(int),
		  NhlOffset(NhlContourLayerRec,contour.update_req),
		  NhlTImmediate,{(NhlPointer) False}},
	{NhlNlgLabelStrings, NhlClgLabelStrings, NhlTGenArray,
		 sizeof(NhlPointer), 
		 NhlOffset(NhlContourLayerRec,contour.legend_labels),
		 NhlTImmediate,{(NhlPointer) NULL}},
};

/* base methods */


static NhlErrorTypes ContourClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes ContourClassPartInitialize(
#ifdef NhlNeedProto
	NhlLayerClass	lc
#endif
);

static NhlErrorTypes ContourInitialize(
#ifdef NhlNeedProto
        NhlLayerClass,  /* class */
        NhlLayer,       /* req */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes ContourSetValues(
#ifdef NhlNeedProto
        NhlLayer,       /* old */
        NhlLayer,       /* reference */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes    ContourGetValues(
#ifdef NhlNeedProto
	NhlLayer,       /* l */
	_NhlArgList,    /* args */
	int             /* num_args */
#endif
);

static NhlErrorTypes ContourDestroy(
#ifdef NhlNeedProto
        NhlLayer        /* inst */
#endif
);

static NhlErrorTypes ContourDraw(
#ifdef NhlNeedProto
        NhlLayer	/* layer */
#endif
);

/* internal static functions */

static NhlErrorTypes SetUpTransObj(
#ifdef NhlNeedProto
	NhlContourLayer	cnew,
	NhlContourLayer	cold,
	NhlBoolean	init
#endif
);


static NhlErrorTypes ManageLegend(
#ifdef NhlNeedProto
	NhlContourLayer	cnew,
	NhlContourLayer	cold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes ManageLabelBar(
#ifdef NhlNeedProto
	NhlContourLayer	cnew,
	NhlContourLayer	cold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes    SetupLevels(
#ifdef NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
        NhlBoolean	init,					    
	float		**levels,
	NhlBoolean	*modified				    
#endif
);

static NhlErrorTypes    SetupLevelsManual(
#ifdef NhlNeedProto
	NhlContourLayer	cnew, 
	NhlContourLayer	cold,
	float		**levels,
	char		*entry_name
#endif
);

static NhlErrorTypes    SetupLevelsEqual(
#ifdef NhlNeedProto
	NhlContourLayer	cnew, 
	NhlContourLayer	cold,
	float		**levels,
	char		*entry_name
#endif
);

static NhlErrorTypes    SetupLevelsAutomatic(
#ifdef NhlNeedProto
	NhlContourLayer	cnew, 
	NhlContourLayer	cold,
	float		**levels,
	char		*entry_name
#endif
);

static NhlErrorTypes    SetupLevelsExplicit(
#ifdef NhlNeedProto
	NhlContourLayer	cnew, 
	NhlContourLayer	cold,
	float		**levels,
	char		*entry_name
#endif
);

static NhlErrorTypes ChooseSpacingLin(
#ifdef NhlNeedProto
float * /*tstart*/,
float * /*tend*/,
float * /*spacing*/,
int /*convert_precision*/,
int /*max_ticks*/
#endif
);

static float	compare(
#ifdef NhlNeedProto
	float a,
	float b,
	int sig_dig
#endif
);

static float roundit(
#ifdef NhlNeedProto
	float a,
	int sig_digit
#endif
);

static NhlErrorTypes    ManageViewDepResources(
#ifdef NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
        NhlBoolean	init,					    
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    ManageDynamicArrays(
#ifdef NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
        NhlBoolean	init,					    
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    ManageGenArray(
#ifdef NhlNeedProto
	NhlGenArray	*ga,
	int		count,
	NhlGenArray	copy_ga,
	NrmQuark	type,
	NhlPointer	init_val,
	int		*old_count,
	int		*init_count,
	NhlBoolean	*need_check,
	NhlString	resource_name,
	NhlString	entry_name
#endif
);

static NhlErrorTypes	CheckColorArray(
#ifdef NhlNeedProto
	NhlContourLayer	cl,
	NhlGenArray	ga,
	int		count,
	int		init_count,
	int		last_count,
	int		**gks_colors,
	NhlString	resource_name,
	NhlString	entry_name
#endif
);

static NhlGenArray GenArraySubsetCopy(
#ifdef NhlNeedProto
        NhlGenArray     ga,
        int             length
#endif
);

NhlContourLayerClassRec NhlcontourLayerClassRec = {
        {
/* class_name			*/      "Contour",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlContourLayerRec),
/* class_inited			*/      False,
/* superclass			*/  (NhlLayerClass)&NhltransformLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	ContourClassPartInitialize,
/* class_initialize		*/	ContourClassInitialize,
/* layer_initialize		*/	ContourInitialize,
/* layer_set_values		*/	ContourSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	ContourGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	ContourDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      ContourDraw,

/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL

        },
	{
/* segment_wkid			*/	0,
/* get_bb			*/	NULL
	},
	{
/* overlay_capability 		*/	_tfOverlayBaseOrMember,
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL,
	},
	{
/* iwrk_len			*/	0,
/* iwrk				*/	NULL,
/* fwrk_len			*/	0,
/* fwrk				*/	NULL
	}
};
	
NhlLayerClass NhlcontourLayerClass = (NhlLayerClass)&NhlcontourLayerClassRec;

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark	Qlevels = NrmNULLQUARK; 
static NrmQuark	Qlevel_flags = NrmNULLQUARK; 
static NrmQuark	Qfill_colors = NrmNULLQUARK;
static NrmQuark	Qfill_patterns = NrmNULLQUARK;
static NrmQuark	Qfill_scales = NrmNULLQUARK;
static NrmQuark	Qline_colors = NrmNULLQUARK; 
static NrmQuark	Qline_dash_patterns = NrmNULLQUARK; 
static NrmQuark	Qline_thicknesses = NrmNULLQUARK; 
static NrmQuark	Qline_label_strings = NrmNULLQUARK;
static NrmQuark	Qline_label_colors = NrmNULLQUARK; 

static int Def_Colors[] = { 
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
static int Def_Colors_Count = NhlNumber(Def_Colors); 
static NhlString	*Dash_Table;
static int		Dash_Table_Len;
static int		*Dash_Patterns;
static int		Mono_Dash_Pattern;
static float		Dash_Size;

static int M = 73;
static int N = 10;
static float T[73*10]; 

/*
 * Function:	ContourClassInitialize
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
ContourClassInitialize
#if __STDC__
(
	void
)
#else
()
#endif
{
	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qlevels = NrmStringToQuark(NhlNcnLevels);
	Qlevel_flags = NrmStringToQuark(NhlNcnLevelFlags);
	Qfill_colors = NrmStringToQuark(NhlNcnFillColors);
	Qfill_patterns = NrmStringToQuark(NhlNcnFillPatterns);
	Qfill_scales = NrmStringToQuark(NhlNcnFillScales);
	Qline_colors = NrmStringToQuark(NhlNcnLineColors);
	Qline_dash_patterns = NrmStringToQuark(NhlNcnLineDashPatterns);
	Qline_thicknesses = NrmStringToQuark(NhlNcnLineThicknesses);
	Qline_label_strings = NrmStringToQuark(NhlNcnLineLabelStrings);
	Qline_label_colors = NrmStringToQuark(NhlNcnLineLabelColors);

	return NhlNOERROR;
}

/*
 * Function:	ContourClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlContourLayerClassPart that cannot be initialized statically.
 *		Calls _NhlRegisterChildClass for the overlay manager object.
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
ContourClassPartInitialize
#if	__STDC__
(
	NhlLayerClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;
	char		*entry_name = "ContourClassPartInitialize";

/*
 * Register children objects
 */
	subret = _NhlRegisterChildClass(lc,NhloverlayLayerClass,
					False,False,
					NhlNlgItemCount,
					NhlNlgItemIndexes,
					NhlNlgLabelStrings,
					NhlNlgMonoItemColor,
					NhlNlgItemColors,
					NhlNlgMonoItemThickness,
					NhlNlgItemThicknesses,
					NhlNlgItemStrings,
					NhlNlgMonoItemStringColor,
					NhlNlgItemStringColors,
					NhlNlgMonoItemTextHeight,
					NhlNlgItemTextHeights,
					NhlNovDisplayLabelBar,
					NhlNovDisplayLegend,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "overlayLayerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhllogLinTransObjLayerClass,
					False,False,
					NhlNtrXMinF,
					NhlNtrXMaxF,
					NhlNtrXLog,
					NhlNtrXReverse,
					NhlNtrYMinF,
					NhlNtrYMaxF,
					NhlNtrYLog,
					NhlNtrYLog,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "logLinTransObjLayerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlirregularType2TransObjLayerClass,
					False,False,
					NhlNtrXMinF,
					NhlNtrXMaxF,
					NhlNtrXLog,
					NhlNtrXReverse,
					NhlNtrYMinF,
					NhlNtrYMaxF,
					NhlNtrYLog,
					NhlNtrYLog,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "irregularType2TransObjLayerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlirregularTransObjLayerClass,
					False,False,
					NhlNtrXMinF,
					NhlNtrXMaxF,
					NhlNtrXLog,
					NhlNtrXReverse,
					NhlNtrYMinF,
					NhlNtrYMaxF,
					NhlNtrYLog,
					NhlNtrYLog,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "irregularTransObjLayerClass");
		return(NhlFATAL);
	}

	return ret;
}


/*
 * Function:	ContourInitialize
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
 * Side Effects:	state change in GKS due to mapping transformations.
 */
/*ARGSUSED*/
static NhlErrorTypes
ContourInitialize
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
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "ContourInitialize";
	char			*e_text;
	NhlContourLayer		cnew = (NhlContourLayer) new;
	NhlContourLayerPart	*cnp = &(cnew->contour);
	NhlContourLayerClassPart *cnclp = (NhlContourLayerClassPart *)
	      &((NhlContourLayerClass)cnew->base.layer_class)->contour_class;
	NhlSArg			sargs[20];
	int			nargs = 0;


/* initialize a scalar field of contour data (temporary) */

	int i,j;
	float x,y;
	M=25;
	N=25;
	for (i=-N/2;i<=N/2;i++) 
		for (j=-M/2;j<=M/2;j++) {
			x = 8.0 * i;
			y = 8.0 * j;
			*(T+(M*(i+N/2)+j+M/2)) = sqrt((double)(x*x + y*y));
		}

/* Initialize Contour class workspace if necessary */

	if (cnclp->iwrk == NULL) {
		if ((cnclp->iwrk = (int *) 
		     NhlMalloc(Nhl_cnINT_WKSPACE * sizeof(int))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		cnclp->iwrk_len = Nhl_cnINT_WKSPACE;
	}
	if (cnclp->fwrk == NULL) {
		if ((cnclp->fwrk = (float *) 
		     NhlMalloc(Nhl_cnFLOAT_WKSPACE * 
			       sizeof(float))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		cnclp->fwrk_len = Nhl_cnFLOAT_WKSPACE;
	}

/* Initialize unset resources */
	
	if (! cnp->min_level_set) cnp->min_level_val = FLT_MAX;
	if (! cnp->max_level_set) cnp->max_level_val = FLT_MIN;
	if (! cnp->line_label_interval_set) cnp->line_label_interval = 2;
	if (! cnp->line_label_text_height_set) 
		cnp->line_label_text_height = 0.013;

/* Initialize private members */

	cnp->new_draw_req = True;
	cnp->trans_dat = NULL;
	cnp->update_req = False;
	cnp->overlay_object = NULL;
	cnp->data_changed = True;
	cnp->ll_text_heights = NULL;
	cnp->ll_strings = NULL;

/* Set view dependent resources */

	subret = ManageViewDepResources(new,req,True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set up the dynamic arrays */

	subret = ManageDynamicArrays(new,req,True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error initializing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set up the contour object transformation  */

	subret = SetUpTransObj(cnew, (NhlContourLayer) req, True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting up transformation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage Legend object */

	subret = ManageLegend(cnew,(NhlContourLayer) req,True,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing Legend";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage LabelBar object */

	subret = ManageLabelBar(cnew,(NhlContourLayer) req,True,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing LabelBar";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
		
		
/* Manage the overlay */

	subret = _NhlManageOverlay(&cnp->overlay_object,new,req,
			       True,sargs,nargs,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

	cnp->data_changed = False;
	cnp->min_level_set = False;
	cnp->max_level_set = False;
	cnp->line_label_interval_set = False;
	cnp->line_label_text_height_set = False;

	return ret;
}

/*
 * Function:	ContourSetValues
 *
 * Description: 
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
static NhlErrorTypes ContourSetValues
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
	char			*entry_name = "ContourSetValues";
	char			*e_text;
	NhlContourLayer		cnew = (NhlContourLayer) new;
	NhlContourLayerPart	*cnp = &(cnew->contour);
	NhlContourLayer		cold = (NhlContourLayer) old;
	NhlContourLayerPart	*ocnp = &(cold->contour);
	/* Note that both ManageLegend and ManageLabelBar add to sargs */
	NhlSArg			sargs[21];
	int			nargs = 0;


	if (cnew->view.use_segments != cold->view.use_segments) {
		cnew->view.use_segments = cold->view.use_segments;
		ret = MIN(ret,NhlWARNING);
		e_text = "%s: attempt to set create-only resource overridden";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
	}

	if (_NhlArgIsSet(args,num_args,NhlNcnMinLevelValF))
		cnp->min_level_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNcnMaxLevelValF))
		cnp->max_level_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNcnLineLabelInterval))
		cnp->line_label_interval_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNcnLineLabelTextHeightF))
		cnp->line_label_text_height_set = True;

/* Set view dependent resources */

	subret = ManageViewDepResources(new,old,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage the dynamic arrays */

	subret = ManageDynamicArrays(new,old,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error initializing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set up the contour object's transformation */

	subret = SetUpTransObj(cnew, (NhlContourLayer) old, False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;


/* Manage Legend object */

	subret = ManageLegend(cnew,cold,False,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing Legend";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage LabelBar object */

	subret = ManageLabelBar(cnew,cold,False,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing LabelBar";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
		
/* Manage the overlay */

	if (cnp->update_req) {
		cnp->new_draw_req = True;
		NhlSetSArg(&sargs[nargs++],NhlNovUpdateReq,True);
	}
		
	subret = _NhlManageOverlay(&cnp->overlay_object,new,old,
				   False,sargs,nargs,entry_name);
	ret = MIN(ret,subret);
	cnp->update_req = False;
	cnp->data_changed = False;
	cnp->min_level_set = False;
	cnp->max_level_set = False;
	cnp->line_label_interval_set = False;

	return ret;
}

/*
 * Function:    ContourGetValues
 *
 * Description: Retrieves the current setting of one or more Legend resources.
 *      This routine only retrieves resources that require special methods
 *      that the generic GetValues method cannot handle. For now this means
 *      all the GenArray resources. Note that space is allocated; the user
 *      is responsible for freeing this space.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when any of the following resources are retrieved:
 *              NhlNcnLevels
 *              NhlNcnLevelFlags
 *              NhlNcnFillColors
 *              NhlNcnFillPatterns
 *              NhlNcnFillScales
 *              NhlNcnLineColors
 *              NhlNcnLineDashPatterns
 *              NhlNcnLineThicknesses
 *		NhlNcnLineLabelStrings
 *		NhlNcnLineLabelColors
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    ContourGetValues
#if __STDC__
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlContourLayer cl = (NhlContourLayer)l;
        NhlContourLayerPart *cnp = &(cl->contour);
        NhlGenArray ga;
        char *e_text;
        int i, count = 0;
        char *type = "";

        for( i = 0; i< num_args; i++ ) {

                ga = NULL;
                if(args[i].quark == Qlevels) {
                        ga = cnp->levels;
                        count = cnp->level_count;
                        type = NhlNcnLevels;
                }
                else if (args[i].quark == Qlevel_flags) {
                        ga = cnp->level_flags;
                        count = cnp->mono_level_flag ? 1 : cnp->level_count;
                        type = NhlNcnLevelFlags;
                }
                else if (args[i].quark == Qfill_colors) {
                        ga = cnp->fill_colors;
                        count = cnp->mono_fill_color ? 1 : cnp->level_count;
                        type = NhlNcnFillColors;
                }
                else if (args[i].quark == Qfill_patterns) {
                        ga = cnp->fill_patterns;
                        count = cnp->mono_fill_pattern ? 1 : cnp->level_count;
                        type = NhlNcnFillPatterns;
                }
                else if (args[i].quark == Qfill_scales) {
                        ga = cnp->fill_scales;
                        count = cnp->mono_fill_scale ? 1 : cnp->level_count;
                        type = NhlNcnFillScales;
                }
                else if (args[i].quark == Qline_colors) {
                        ga = cnp->line_colors;
                        count = cnp->mono_line_color ? 1 : cnp->level_count;
                        type = NhlNcnLineColors;
                }
                else if (args[i].quark == Qline_dash_patterns) {
                        ga = cnp->line_dash_patterns;
                        count = cnp->mono_line_dash_pattern ? 
				1 : cnp->level_count;
                        type = NhlNcnLineDashPatterns;
                }
                else if (args[i].quark == Qline_thicknesses) {
                        ga = cnp->line_thicknesses;
                        count = cnp->mono_line_thickness ? 
				1 : cnp->level_count;
                        type = NhlNcnLineThicknesses;
                }
                else if (args[i].quark == Qline_label_strings) {
                        ga = cnp->line_label_strings;
                        count = cnp->level_count;
                        type = NhlNcnLineLabelStrings;
                }
                else if (args[i].quark == Qline_label_colors) {
                        ga = cnp->line_label_colors;
                        count = cnp->mono_line_label_color ? 
				1 : cnp->level_count;
                        type = NhlNcnLineLabelColors;
                }
                if (ga != NULL) {
                        if ((ga = GenArraySubsetCopy(ga, count)) == NULL) {
                                e_text = "%s: error copying %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          "ContourGetValues",type);
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
 * Description: Since the internal GenArrays maintained by the Contour object
 *      may be bigger than the size currently in use, this function allows
 *      a copy of only a portion of the array to be created. This is for
 *      use by the GetValues routine when returning GenArray resources to
 *      the user level. The array is assumed to be valid. The only pointer
 *      type arrays that the routine can handle are NhlString arrays.
 *      Note: this might be another candidate for inclusion as a global
 *      routine.
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
#if __STDC__
        (NhlGenArray    ga,
        int             length)
#else
(ga,length)
        NhlGenArray     ga;
        int             length;
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
 * Function:	ContourDestroy
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
static NhlErrorTypes ContourDestroy
#if __STDC__
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
#if 0
	char			*e_text;
	char			*entry_name = "ContourDestroy";
#endif
	NhlContourLayerPart	*cnp = &(((NhlContourLayer) inst)->contour);
	NhlTransformLayerPart	*cntp = &(((NhlTransformLayer) inst)->trans);

	if (cntp->overlay_status == _tfCurrentOverlayMember) {
		subret = NhlRemoveFromOverlay(
				cntp->overlay_object->base.parent->base.id,
					      inst->base.id,False);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return NhlFATAL;
	}

	if (cnp->overlay_object != NULL) {
		(void) _NhlDestroyChild(cnp->overlay_object->base.id,inst);
		cnp->overlay_object = NULL;
	}
	if (cntp->trans_obj != NULL) {
		(void) _NhlDestroyChild(cntp->trans_obj->base.id,inst);
		cntp->trans_obj = NULL;
	}

	NhlFreeGenArray(cnp->levels);
	NhlFreeGenArray(cnp->level_flags);
	NhlFreeGenArray(cnp->fill_colors);
	NhlFreeGenArray(cnp->fill_patterns);
	NhlFreeGenArray(cnp->fill_scales);
	NhlFreeGenArray(cnp->line_colors);
	NhlFreeGenArray(cnp->line_dash_patterns);
	NhlFreeGenArray(cnp->line_thicknesses);
	NhlFreeGenArray(cnp->line_label_strings);
	NhlFreeGenArray(cnp->line_label_colors);
	NhlFreeGenArray(cnp->legend_labels);
	if (cnp->ll_text_heights != NULL)
		NhlFreeGenArray(cnp->ll_text_heights);
	if (cnp->ll_strings != NULL) {
		NhlFree(cnp->ll_strings->data);
		NhlFreeGenArray(cnp->ll_strings);
	}
	NhlFree(cnp->gks_fill_colors);
	NhlFree(cnp->gks_line_colors);
	NhlFree(cnp->gks_line_label_colors);

	return(ret);
}

/*
 * Function:	ContourDraw
 *
 * Description:	
 *
 * In Args:	layer	Contour instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes ContourDraw
#if  __STDC__
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "ContourDraw";
	NhlContourLayer		cl = (NhlContourLayer) layer;
	NhlContourLayerPart	*cnp = &(cl->contour);
	NhlContourLayerClassPart *cnclp = (NhlContourLayerClassPart *)
		&((NhlContourLayerClass)cl->base.layer_class)->contour_class;
	NhlTransformLayerPart	*tfp = &(cl->trans);
	NhlLayer			trans_obj;
	float			out_of_range_val;
	float			*clvp, *cllp;
	int			*clup, *clcp, *llcp;
	int			i,ll;
	char			buffer[30];
	float			fl,fr,fb,ft,ul,ur,ub,ut;
	NhlString		*lltp;
	int			*area_map;
	float			chh;
/*
 * Set up static values
 */
	
	Dash_Size = cnp->line_dash_seglen;
	Dash_Table = (NhlString *) cnp->dash_table->data;
	Dash_Table_Len = cnp->dash_table->num_elements;
	Dash_Patterns = (int *) cnp->line_dash_patterns->data;
	Mono_Dash_Pattern = cnp->mono_line_dash_pattern;

	if (cl->view.use_segments && ! cnp->new_draw_req) {
                subret = _NhlActivateWorkstation(cl->base.wkptr);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDrawSegment(cnp->trans_dat,
				_NhlWorkstationId(cl->base.wkptr));
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDeactivateWorkstation(cl->base.wkptr);
		return MIN(subret,ret);
		
	}
	cnp->new_draw_req = False;
	
	subret = _NhlActivateWorkstation(cl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	if (cl->view.use_segments) {
		if (cnp->trans_dat != NULL)
			_NhlDeleteViewSegment(layer, cnp->trans_dat);
		if ((cnp->trans_dat = _NhlNewViewSegment(layer)) == NULL) {
			e_text = "%s: error opening segment";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
		_NhlStartSegment(cnp->trans_dat);
	}

	
/*
 * If the plot is an overlay member, use the overlay manager's trans object.
 */
	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    tfp->overlay_trans_obj != NULL) {
		trans_obj = tfp->overlay_trans_obj;
	}
	else {
		trans_obj = tfp->trans_obj;
		subret = _NhlSetTrans(tfp->trans_obj, layer);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
	}

	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);

	NhlVAGetValues(trans_obj->base.id, 
		     NhlNtrOutOfRangeF, &out_of_range_val,
		     NULL);
        c_cpsetr("ORV",out_of_range_val);

	if (! cnp->x_reverse) {
		c_cpsetr("XC1",cnp->x_min);
		c_cpsetr("XCM",cnp->x_max);
	}
	else {
		c_cpsetr("XC1",cnp->x_max);
		c_cpsetr("XCM",cnp->x_min);
	}
	if (! cnp->y_reverse) {
		c_cpsetr("YC1",cnp->y_min);
		c_cpsetr("YCN",cnp->y_max);
	}
	else {
		c_cpsetr("YC1",cnp->y_max);
		c_cpsetr("YCN",cnp->y_min);
	}

	gset_line_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_fill_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_text_colr_ind((Gint)_NhlGetGksCi(cl->base.wkptr,0));
	gset_linewidth(1.0);

	c_cpseti("CLS",0);		/* Conpack not to select levels */
	c_cpseti("NCL",cnp->level_count); 
	clvp = (float *) cnp->levels->data;
	clup = (int *) cnp->level_flags->data;
	clcp = (int *) cnp->gks_line_colors;
	cllp = (float *) cnp->line_thicknesses->data;
	llcp = (int *) cnp->gks_line_label_colors;
	lltp = (NhlString *) cnp->line_label_strings->data;



	for (i=0; i<cnp->level_count; i++) {
		int dpix;

		c_cpseti("PAI",i+1);
		c_cpsetr("CLV",clvp[i]);
		c_cpseti("CLU",clup[i]);
		c_cpseti("CLC", cnp->mono_line_color ? clcp[0] : clcp[i]);
		c_cpsetr("CLL",cnp->mono_line_thickness ? cllp[0] : cllp[i]);
		c_cpseti("LLC",cnp->mono_line_label_color 
			 ? llcp[0] : llcp[i]);
		dpix = Mono_Dash_Pattern ? 
			Dash_Patterns[0] : Dash_Patterns[i];
		dpix = dpix % Dash_Table_Len;
		strncpy(buffer,Dash_Table[i],29);
		buffer[29] = '\0';
		c_cpsetc("CLD",buffer);
		c_cpsetc("LLT",lltp[i]);
	}

	c_cpseti("LLB",2); /* must set pl fill style */
	c_cpseti("LBC",0);
        c_cpsetr("DPS",cnp->line_label_text_height * cl->view.width);
        c_cpsetr("DPV",.01); /* dash pattern vector length */
	c_cpseti("DPU",1); /* dash pattern use flag */
	c_cpseti("LLP",2); /* line label positioning */
	c_cpseti("LLO",1); /* line label orientation */
	c_cpsetr("LLS",cnp->line_label_text_height / cl->view.width);
	c_cpsetc("ILT"," ");
	c_cpsetc("HLT"," ");
        c_cpseti("SET",0);
        c_cpseti("MAP",3);

	c_cprect(T,M,M,N,
		 cnclp->fwrk,cnclp->fwrk_len,cnclp->iwrk,cnclp->iwrk_len);
        c_cpcldr(T,cnclp->fwrk,cnclp->iwrk);

	c_pcseti("CC", -1);
	gset_fill_int_style(GSTYLE_SOLID);
	chh= 1.0;
	gset_char_ht(chh);
	c_cplbdr(T,cnclp->fwrk,cnclp->iwrk);

	if (cl->view.use_segments) {
		_NhlEndSegment();
	}
        subret = _NhlDeactivateWorkstation(cl->base.wkptr);

	return MIN(subret,ret);

}
/*
 * Function:	SetUpTransObjs
 *
 * Description: Sets up the LogLinear transformation object for the generic
 *		LogLinear plot object. Note that since this trans object
 *		does not require any dynamic memory, the same trans object
 *		persists for the life of the Contour object.
 *
 * In Args:	xnew	new instance record
 *		xold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes SetUpTransObj
#if  __STDC__
(
	NhlContourLayer	cnnew,
	NhlContourLayer	cnold,
	NhlBoolean	init
)
#else 
(cnnew,cnold,init)
	NhlContourLayer	cnnew;
	NhlContourLayer	cnold;
	NhlBoolean	init;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourLayerPart	*cnp = &(cnnew->contour);
	NhlContourLayerPart	*ocnp = &(cnold->contour);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[16];
        int			nargs = 0;

	entry_name = (init) ? "ContourInitialize" : "ContourSetValues";
/*
 * Since no dynamic memory is involved a LogLin transformation only
 * needs to be created once. It will not be freed until the object
 * is destroyed.
 */
	if (init || tfp->trans_obj == NULL) {

		cnp->new_draw_req = True;
		NhlSetSArg(&sargs[nargs++],NhlNtrXLog,cnp->x_log);
		NhlSetSArg(&sargs[nargs++],NhlNtrYLog,cnp->y_log);

		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,cnp->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,cnp->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,cnp->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,cnp->y_max);

		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,cnp->x_reverse);
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,cnp->y_reverse);

		sprintf(buffer,"%s",cnnew->base.name);
		strcat(buffer,".Trans");

		subret = _NhlALCreateChild(&tmpid,buffer,
					   NhllogLinTransObjLayerClass,
					   (NhlLayer)cnnew,sargs,nargs);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		return ret;
	}
		
	if (cnp->x_min != ocnp->x_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,cnp->x_min);
	if (cnp->x_max != ocnp->x_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,cnp->x_max);
	if (cnp->y_min != ocnp->y_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,cnp->y_min);
	if (cnp->y_max != ocnp->y_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,cnp->y_max);

	if (cnp->x_reverse != ocnp->x_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,cnp->x_reverse);
	if (cnp->y_reverse != ocnp->y_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,cnp->y_reverse);

	if (cnp->x_log != ocnp->x_log)
		NhlSetSArg(&sargs[nargs++],NhlNtrXLog,cnp->x_log);
	if (cnp->y_log != ocnp->y_log)
		NhlSetSArg(&sargs[nargs++],NhlNtrYLog,cnp->y_log);


	subret = _NhlALSetValuesChild(tfp->trans_obj->base.id,
				      (NhlLayer) cnnew,sargs,nargs);
	if (nargs > 0)
		cnp->new_draw_req = True;
	return MIN(ret,subret);

}

/*
 * Function:	ManageLegend
 *
 * Description: If the Contour object has an overlay object attached, and
 *		the Legend is activated, manages the Legend resources 
 *		relevant to the Contour object.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageLegend
#if  __STDC__
(
	NhlContourLayer	cnnew,
	NhlContourLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew,cnold,init)
	NhlContourLayer	cnnew;
	NhlContourLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	char			*e_text;
	char			*entry_name;
	NhlContourLayerPart	*cnp = &(cnnew->contour);
	NhlContourLayerPart	*ocnp = &(cnold->contour);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);

	entry_name = (init) ? "ContourInitialize" : "ContourSetValues";

	if (! tfp->overlay_plot_base) return NhlNOERROR;

	if (init || cnp->display_legend != ocnp->display_legend) {
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNovDisplayLegend,cnp->display_legend);
	}
	if (cnp->display_legend == Nhl_ovNoCreate) return NhlNOERROR;

	if (init) {
		int		count = 1;

		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgItemCount,cnp->level_count);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgItemIndexes,cnp->line_dash_patterns);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgLabelStrings,cnp->line_label_strings);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoItemColor,cnp->mono_line_color);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgItemColors,cnp->line_colors);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoItemThickness,cnp->mono_line_thickness);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgItemThicknesses,cnp->line_thicknesses);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoItemStringColor,
			   cnp->mono_line_label_color);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgItemStringColors,cnp->line_label_colors);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgItemStrings,cnp->ll_strings);
  
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgMonoItemTextHeight,True);
	        if ((cnp->ll_text_heights = 
		     NhlCreateGenArray((NhlPointer)
				       &cnp->line_label_text_height,
				       NhlTFloat,sizeof(float),1,&count))
		    == NULL) {
			e_text = "%s: error creating %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNlgItemTextHeights);
			return NhlFATAL;
		}
		cnp->ll_text_heights->my_data = False;
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlgItemTextHeights,cnp->ll_text_heights);
	}
	else {
		if (cnp->level_count != ocnp->level_count)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgItemCount,cnp->level_count);
		if (cnp->line_dash_patterns != ocnp->line_dash_patterns)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgItemIndexes,cnp->line_dash_patterns);
		if (cnp->line_label_strings != ocnp->line_label_strings)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgLabelStrings,cnp->line_label_strings);
		if (cnp->mono_line_color != ocnp->mono_line_color)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgMonoItemColor,cnp->mono_line_color);
		if (cnp->line_colors != ocnp->line_colors)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgItemColors,cnp->line_colors);
		if (cnp->mono_line_thickness != ocnp->mono_line_thickness)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgMonoItemThickness,
				   cnp->mono_line_thickness);
		if (cnp->line_thicknesses != ocnp->line_thicknesses)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgItemThicknesses,
				   cnp->line_thicknesses);
		if (cnp->mono_line_label_color != ocnp->mono_line_label_color)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgMonoItemStringColor,
				   cnp->mono_line_label_color);
		if (cnp->line_label_colors != ocnp->line_label_colors)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgItemStringColors,
				   cnp->line_label_colors);
		if (cnp->line_label_strings != ocnp->line_label_strings ||
		    cnp->level_flags != ocnp->level_flags ||
		    cnp->level_count != ocnp->level_count ||
		    cnp->line_label_interval_set) 
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgItemStrings,cnp->ll_strings);
		if (cnp->line_label_text_height
		    != ocnp->line_label_text_height)
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNlgItemTextHeights,cnp->ll_text_heights);
	}

	return NhlNOERROR;
}


/*
 * Function:	ManageLabelBar
 *
 * Description: If the Contour object has an overlay object attached, and
 *		the LabelBar is activated, manages the LabelBar resources 
 *		relevant to the Contour object.
 *
 * In Args:	cnnew	new instance record
 *		cnold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageLabelBar
#if  __STDC__
(
	NhlContourLayer	cnnew,
	NhlContourLayer	cnold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(cnnew,cnold,init)
	NhlContourLayer	cnnew;
	NhlContourLayer	cnold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourLayerPart	*cnp = &(cnnew->contour);
	NhlContourLayerPart	*ocnp = &(cnold->contour);
	NhlTransformLayerPart	*tfp = &(cnnew->trans);

	entry_name = (init) ? "ContourInitialize" : "ContourSetValues";

	return NhlNOERROR;
}

/*
 * Function:  ManageViewDepResources
 *
 * Description: Modifies resources that may need to change when the view
 *	is modified.
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
static NhlErrorTypes    ManageViewDepResources
#if __STDC__
	(NhlLayer	new, 
	NhlLayer	old,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	NhlLayer	new;
	NhlLayer	old;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlContourLayer	cnew = (NhlContourLayer) new;
	NhlContourLayerPart *cnp = &(cnew->contour);
	NhlContourLayer	cold = (NhlContourLayer) old;
	NhlErrorTypes ret = NhlNOERROR;

/* Adjust line label text height */

	if (cnp->line_label_text_height_set) {
		cnp->ll_text_height_2vpw = 1.0  /cnew->view.width;
	}
	else if (init) {
		cnp->line_label_text_height *= 
			cnew->view.width / Nhl_cnSTD_VIEW_WIDTH;
		cnp->ll_text_height_2vpw = 1.0 / cnew->view.width;
	}
	else if (cnew->view.width != cold->view.width) {
		cnp->line_label_text_height *= 
			cnew->view.width / cold->view.width;
	}

	return ret;
}
/*
 * Function:  ManageDynamicArrays
 *
 * Description: Creates and manages internal copies of each of the 
 *	Contour GenArrays. Populates the copies with the values specified 
 *	via ContourCreate or ContourSetValues calls. Assigns default 
 *	values and sizes to any array resource not set by the caller.
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
static NhlErrorTypes    ManageDynamicArrays
#if __STDC__
	(NhlLayer		new, 
	NhlLayer		old,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args)
#else
(new,old,init,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlContourLayer	cnew = (NhlContourLayer) new;
	NhlContourLayerPart *cnp = &(cnew->contour);
	NhlContourLayer	cold = (NhlContourLayer) old;
	NhlContourLayerPart *ocnp = &(cold->contour);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	int i,count;
	NhlGenArray ga;
	char *entry_name;
	char *e_text;
	int *ip;
	float *fp;
	float fval;
	int	init_count;
	NhlString sval;
	NhlBoolean need_check;
	int old_count;
	float *levels = NULL;
	NhlBoolean levels_modified = False, flags_modified = False;

	entry_name =  init ? "ContourInitialize" : "ContourSetValues";

/* Determine the contour level state */

	subret = SetupLevels(new,old,init,&levels,&levels_modified);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error getting contour level information";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	count = cnp->level_count;

/*=======================================================================*/

/* 
 * The levels array 
 */
	ga = init ? NULL : ocnp->levels;
	subret = ManageGenArray(&ga,count,cnp->levels,Qfloat,NULL,
				&old_count,&init_count,&need_check,
				NhlNcnLevels,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->levels = cnp->levels;
	cnp->levels = ga;
	if (levels_modified) {
		if (levels == NULL) {
			e_text = "%s: internal error getting levels";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		NhlFree(cnp->levels->data);
		cnp->levels->data = (NhlPointer) levels;
	}
		

/*=======================================================================*/

/*
 * Level flags
 */

	flags_modified = 
		(ga = init ? NULL : ocnp->level_flags) != cnp->level_flags;
	subret = ManageGenArray(&ga,count,cnp->level_flags,Qint,NULL,
				&old_count,&init_count,&need_check,
				NhlNcnLevelFlags,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->level_flags = cnp->level_flags;
	cnp->level_flags = ga;

	ip = (int *) cnp->level_flags->data;
	if (! flags_modified && 
	    (levels_modified || cnp->line_label_interval_set)) {
		flags_modified = True;
		if (cnp->line_label_interval <= 0) {
			for (i = 0; i < count; i++) 
				ip[i] = Nhl_cnLINEONLY;
		}
		else {
			for (i = 0; i < count; i++)
				ip[i] = (i+1) % cnp->line_label_interval == 0 ?
					Nhl_cnLINEANDLABEL : Nhl_cnLINEONLY;
		}
	}
	else if (need_check) {
		flags_modified = True;
		if (cnp->line_label_interval <= 0) {
			for (i = init_count; i < count; i++) 
				ip[i] = Nhl_cnLINEONLY;
		}
		else {
			for (i = init_count; i < count; i++)
				ip[i] = (i+1) % cnp->line_label_interval == 0 ?
					Nhl_cnLINEANDLABEL : Nhl_cnLINEONLY;
		}
		for (i=0; i<init_count; i++) {
			if (ip[i] < Nhl_cnNOLINE || 
			    ip[i] > Nhl_cnLINEANDLABEL) {
				e_text =
	      "%s: %s index %d contains an invalid level flag, %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNcnLevelFlags,i,ip[i]);
				ret = MIN(ret, NhlWARNING);
				ip[i] = Nhl_cnLINEONLY;
			}
		}
	}

			
/*=======================================================================*/
	
/*
 * Fill colors
 */

	if (init) {
		ga = NULL;
		count = cnp->level_count;
	}
	else {
		ga = ocnp->fill_colors;
		count = cnp->mono_fill_color ? 1 : cnp->level_count;
	}
	subret = ManageGenArray(&ga,count,cnp->fill_colors,Qint,NULL,
				&old_count,&init_count,&need_check,
				NhlNcnFillColors,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ocnp->fill_colors = cnp->fill_colors;
	cnp->fill_colors = ga;

	if (need_check) {
		subret = CheckColorArray(cnew,ga,count,init_count,old_count,
					 &cnp->gks_fill_colors,
					 NhlNcnFillColors, entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
			return ret;
	}
/*=======================================================================*/
	
/*
 * Fill patterns
 */

	if (init) {
		ga = NULL;
		count = cnp->level_count;
	}
	else {
		ga = ocnp->fill_patterns;
		count = cnp->mono_fill_pattern ? 1 : cnp->level_count;
	}
	if (ga != cnp->fill_patterns) cnp->new_draw_req = True;
	subret = ManageGenArray(&ga,count,cnp->fill_patterns,Qint,NULL,
				&old_count,&init_count,&need_check,
				NhlNcnFillPatterns,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ocnp->fill_patterns = cnp->fill_patterns;
	cnp->fill_patterns = ga;

	if (need_check) {
		ip = (int *) ga->data;
		for (i=init_count; i < count; i++) {
			ip[i] = i + 1;
		}
		for (i=0; i<init_count; i++) {
			if (ip[i] < NhlHOLLOWFILL) {
				e_text =
	      "%s: %s index %d holds an invalid pattern value, %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,
					  NhlNcnFillPatterns,i,ip[i]);
				ret = MIN(ret, NhlWARNING);
				ip[i] = NhlHOLLOWFILL;
			}
		}
	}
	
/*=======================================================================*/
	
/*
 * Fill scales
 */

	if (init) {
		ga = NULL;
		count = cnp->level_count;
	}
	else {
		ga = ocnp->fill_scales;
		count = cnp->mono_fill_scale ? 1 : cnp->level_count;
	}
	fval = 1.0;
	subret = ManageGenArray(&ga,count,cnp->fill_scales,Qfloat,&fval,
				&old_count,&init_count,&need_check,
				NhlNcnFillScales,entry_name);
	
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->fill_scales = cnp->fill_scales;
	cnp->fill_scales = ga;
	
	if (need_check) {
		fp = (float *) ga->data;
		for (i=0; i<count; i++) {
			if (fp[i] <= 0.0) {
				e_text =
	            "%s: %s index %d holds an invalid fill scale: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNcnFillScales,i);
				ret = MIN(ret, NhlWARNING);
				fp[i] = 1.0;
			}
		}
	}

/*=======================================================================*/
	
/*
 * Line colors
 */

	if (init) {
		ga = NULL;
		count = cnp->level_count;
	}
	else {
		ga = ocnp->line_colors;
		count = cnp->mono_line_color ? 1 : cnp->level_count;
	}
	subret = ManageGenArray(&ga,count,cnp->line_colors,Qint,NULL,
				&old_count,&init_count,&need_check,
				NhlNcnLineColors,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ocnp->line_colors = cnp->line_colors;
	cnp->line_colors = ga;

		
	if (need_check) {
		subret = CheckColorArray(cnew,ga,count,init_count,old_count,
					 &cnp->gks_line_colors,
					 NhlNcnLineColors,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
			return ret;
	}
/*=======================================================================*/

/*
 * Line dash patterns
 * Since Conpack needs to know the actual strings, it is necessary to 
 * get a copy of the dash table.
 */
		
	if (init) {
		subret = NhlVAGetValues(cnew->base.wkptr->base.id,
				      NhlNwkDashTable, &ga, NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: NhlFATAL error retrieving dash table";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		cnp->dash_table = ga;

		ga = NULL;
		count = cnp->level_count;
	}
	else {
		ga = ocnp->line_dash_patterns;
		count = cnp->mono_line_dash_pattern ? 1 : cnp->level_count;
	}
	subret = ManageGenArray(&ga,count,cnp->line_dash_patterns,Qint,NULL,
				&old_count,&init_count,&need_check,
				NhlNcnLineDashPatterns,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->line_dash_patterns = cnp->line_dash_patterns;
	cnp->line_dash_patterns = ga;

	if (need_check) {
		ip = (int *) ga->data;
		for (i=init_count; i < count; i++) {
			ip[i] = i;
		}
		for (i=0; i<init_count; i++) {
			if (ip[i] < 1) {
				e_text =
			 "%s: %s index %d has invalid value: %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,
					  NhlNcnLineDashPatterns,i,ip[i]);
				ret = MIN(ret, NhlWARNING);
				ip[i] = Nhl_cnDEF_DASH_PATTERN;
			}
		}
	}

/*=======================================================================*/
	
/*
 * Line thicknesses
 */

	if (init) {
		ga = NULL;
		count = cnp->level_count;
	}
	else {
		ga = ocnp->line_thicknesses;
		count = cnp->mono_line_thickness ? 1 : cnp->level_count;
	}
	fval = 1.0;
	subret = ManageGenArray(&ga,count,cnp->line_thicknesses,Qfloat,&fval,
				&old_count,&init_count,&need_check,
				NhlNcnLineThicknesses,entry_name);
	
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->line_thicknesses = cnp->line_thicknesses;
	cnp->line_thicknesses = ga;
	
	if (need_check) {
		fp = (float *) ga->data;
		for (i=0; i<count; i++) {
			if (fp[i] <= 0.0) {
				e_text =
	        "%s: %s index %d holds an invalid line thickness: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNcnLineThicknesses,i);
				ret = MIN(ret, NhlWARNING);
				fp[i] = 1.0;
			}
		}
	}

/*=======================================================================*/
	
/*
 * Line Label strings
 * Need both the user accessible array that holds all the Line Label strings,
 * and a private array containing pointers to the strings that get drawn,
 * for the benefit of Legend. Since this GenArray only allocates the 
 * pointer array, not the strings, it will need to be freed specially.
 */
	count = cnp->level_count;
	ga = init ? NULL : ocnp->line_label_strings;
	subret = ManageGenArray(&ga,count,cnp->line_label_strings,
				Qstring,NULL,
				&old_count,&init_count,&need_check,
				NhlNcnLineLabelStrings,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ocnp->line_label_strings = cnp->line_label_strings;
	cnp->line_label_strings = ga;

	if (levels_modified || need_check) {
		NhlString *sp = (NhlString *) ga->data;
		fp = (float *) cnp->levels->data;

		init_count = levels_modified && 
			cnp->line_label_strings == ocnp->line_label_strings ?
				0 : init_count;

		for (i=init_count; i<count; i++) {
			if (sp[i] != NULL) NhlFree(sp[i]);
			sp[i] = (char *) NhlMalloc(7);
			sprintf(sp[i],"%6.2f\0",fp[i]);
		}
	}
	if (flags_modified && init) {
		NhlString *sp;

		if ((sp = (NhlString *) 
		     NhlMalloc(count * sizeof(NhlString))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if ((cnp->ll_strings = 
		     NhlCreateGenArray((NhlPointer)sp,NhlTString,
				       sizeof(NhlString),1,&count)) == NULL) {
			e_text = "%s: error creating GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		cnp->ll_strings->my_data = False;
	}
	if (flags_modified || need_check) {
		NhlString *sp = cnp->ll_strings->data;
		NhlString *llsp = cnp->line_label_strings->data;

		if (count > cnp->ll_strings->num_elements) {
			if ((sp = (NhlString *) 
			     NhlRealloc(sp, 
					count * sizeof(NhlString))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
		}
		ip = (int *) cnp->level_flags->data;
		for (i = 0; i < count; i++) {
			if (ip[i] < Nhl_cnLABELONLY) {
				sp[i] = NULL;
			}
			else {
				sp[i] = llsp[i];
			}
		}
	}
				

/*=======================================================================*/
	
/*
 * Line Label colors
 */

	if (init) {
		ga = NULL;
		count = cnp->level_count;
	}
	else {
		ga = ocnp->line_label_colors;
		count = cnp->mono_line_label_color ? 1 : cnp->level_count;
	}
	subret = ManageGenArray(&ga,count,cnp->line_label_colors,Qint,NULL,
				&old_count,&init_count,&need_check,
				NhlNcnLineLabelColors,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ocnp->line_label_colors = cnp->line_label_colors;
	cnp->line_label_colors = ga;


	if (need_check) {
		subret = CheckColorArray(cnew,ga,count,init_count,old_count,
					 &cnp->gks_line_label_colors,
					 NhlNcnLineLabelColors,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
			return ret;
	}

	if (flags_modified ||
	    cnp->fill_colors != ocnp->fill_colors ||
	    cnp->mono_fill_color != ocnp->mono_fill_color ||
	    cnp->fill_patterns != ocnp->fill_patterns ||
	    cnp->mono_fill_pattern != ocnp->mono_fill_pattern ||
	    cnp->fill_scales != ocnp->fill_scales ||
	    cnp->mono_fill_scale != ocnp->mono_fill_scale ||
	    cnp->line_colors != ocnp->line_colors ||
	    cnp->mono_line_color != ocnp->mono_line_color ||
	    cnp->line_dash_patterns != ocnp->line_dash_patterns ||
	    cnp->mono_line_dash_pattern != ocnp->mono_line_dash_pattern ||
	    cnp->line_thicknesses != ocnp->line_thicknesses ||
	    cnp->mono_line_thickness != ocnp->mono_line_thickness ||
	    cnp->line_label_strings != ocnp->line_label_strings ||
	    cnp->line_label_colors != ocnp->line_label_colors ||
	    cnp->mono_line_label_color != ocnp->mono_line_label_color) {
		cnp->new_draw_req = True;
	}

	return ret;
}

/*
 * Function:    ManageGenArray
 *
 * Description:	Handles details of managing a GenArray
 *
 * In Args:	count		number of elements to create in the GenArray
 *		copy_ga 	GenArray to copy values from - if
 *				NULL it is ignored.
 *		type		type of GenArray to create - int,float,string
 *		*init_val	if non-null an initialization value to use -
 *				strings have the array index appended
 *		resource_name	name of the GenArray resource 		
 *		entry_name	name of the high level caller of the routine 
 *
 * Out Args:	*ga		If non-NULL on input, contains a previously
 *				allocated GenArray, whose data will be 
 *				replaced if necessary.
 *				Out: An allocated GenArray with allocated data
 *		*old_count	the previous count in the old gen array
 *		*init_count	number of values initialized - if init_val is
 *				non-NULL, will contain count; if init_val is
 *				NULL will contain MIN(count,number of 
 *				elements in copy_ga); if copy_ga is also NULL
 *				will contain 0.
 *		*need_check     True if a GenArray copy occurs or the number
 *				of elements increases and no initialization
 *				value is supplied. False otherwise.
 *
 *
 * Return Values:
 *
 * Side Effects: The internal copy of each GenArray is modified to reflect
 *	changes requested via ContourSetValues
 *	If the data changes in any way the new_draw_req flag is set True,
 *	so that the segment will be redrawn.
 */

/*ARGSUSED*/
static NhlErrorTypes    ManageGenArray
#if __STDC__
	(NhlGenArray	*ga,
	 int		count,
	 NhlGenArray	copy_ga,
	 NrmQuark	type,
	 NhlPointer	init_val,
	 int		*old_count,
	 int		*init_count,
	 NhlBoolean	*need_check,
	 NhlString	resource_name,
	 NhlString	entry_name)
#else
(ga,count,copy_ga,type,init_val,resource_name,entry_name)
	NhlGenArray	*ga;
	int		count;
	NhlGenArray	copy_ga;
	NrmQuark	type;
	NhlPointer	init_val;
	int		*old_count,
	int		*init_count,
	NhlBoolean	*need_check,
	NhlString	resource_name;
	NhlString	entry_name;
#endif
{
	char		*str_type;
	NhlErrorTypes	ret = NhlNOERROR;
	int		i, size;
	NhlPointer	*datap;
	char		*e_text;

	*init_count = 0;
	*need_check = False;
	*old_count = 0;

	if (type == Qint) {
		str_type = NhlTInteger;
		size = sizeof(int);
	}
	else if (type == Qfloat) {
		str_type = NhlTFloat;
		size = sizeof(float);
	}
	else if (type == Qstring) {
		str_type = NhlTString;
		size = sizeof(NhlString);
	}
	else {
		e_text = "%s: internal error; unsupported type for %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  resource_name);
		return NhlFATAL;
	}

	if (*ga != NULL) {
		datap = (*ga)->data;
		*old_count = (*ga)->num_elements;

		if (count > (*ga)->num_elements) {
			if ((datap = (NhlPointer)
			     NhlRealloc(datap, count * size)) == NULL) {
				e_text = "%s: error reallocating %s data";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,resource_name);
				return NhlFATAL;
			}
			(*ga)->num_elements = count;
		}
		else if (*ga == copy_ga) {
			*init_count = (*ga)->num_elements;
			return ret;
		}
	}
	else {
		if ((datap = (NhlPointer) NhlMalloc(count * size)) == NULL) {
			e_text = "%s: error creating %s array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  resource_name);
			return NhlFATAL;
		}

		if ((*ga = NhlCreateGenArray((NhlPointer)datap,str_type,
					     size,1,&count)) == NULL) {
			e_text = "%s: error creating %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  resource_name);
			return NhlFATAL;
		}
		(*ga)->my_data = True;
	}

/* 
 * If there is a GenArray to copy, copy it; then initialize all remaining
 * uninitialized elements if an initialization value has been passed in.
 */

	if (copy_ga != NULL && copy_ga != *ga) {

		*need_check = True;
		ret = _NhlValidatedGenArrayCopy(ga,copy_ga,Nhl_cnMAX_LEVELS,
						True,False,resource_name, 
						entry_name);
		if (ret < NhlWARNING) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,
				  resource_name);
			return ret;
		}
		*init_count = copy_ga->num_elements;
	}

	if (*init_count < count) {

		if (init_val == NULL) {
			if (type == Qstring) 
				for (i = *init_count; i< count; i++)
					((NhlString *)datap)[i] = NULL;
			*need_check = True;
			return ret;
		}
		else if (type == Qint)
			for (i = *init_count; i< count; i++)
				((int *)datap)[i] = *((int *)init_val);
		else if (type == Qfloat)
			for (i = *init_count; i< count; i++)
				((float *)datap)[i] = *((float *)init_val);
		else if (type == Qstring) {
			char *sp;
			char *init_str = (char *) init_val;
			char numstr[10];
			for (i = *init_count; i< count; i++) {
				sprintf(numstr,"%d",i);
				if ((sp = (char *) 
				     NhlMalloc(sizeof(init_str)+
					       sizeof(numstr)+1)) == NULL) {
					e_text = "%s: error creating %s array";
					NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						  entry_name,resource_name);
					return NhlFATAL;
				}
				((char **)datap)[i] = sp;
				strcpy(sp,init_str);
				strcat(sp,numstr);
			}
		}
		*init_count = count;
	}

	return ret;
}


/*
 * Function:    CheckColorArray
 *
 * Description:	Checks color array values for validity, initializing any
 *		currently uninitialized values and keeps the GKS index
 *		arrays up-to-date.
 *
 * In Args:	count		number of elements to create in the GenArray
 *		resource_name	name of the GenArray resource 		
 *		entry_name	name of the high level caller of the routine 
 *
 * Out Args:	*ga		If non-NULL on input, contains a previously
 *				allocated GenArray, whose data will be 
 *				replaced if necessary.
 *				Out: An allocated GenArray with allocated data
 *		*init_count	number of values initialized - if init_val is
 *				non-NULL, will contain count; if init_val is
 *				NULL will contain MIN(count,number of 
 *				elements in copy_ga); if copy_ga is also NULL
 *				will contain 0.
 *
 * Return Values:
 *
 * Side Effects: The internal copy of each GenArray is modified to reflect
 *	changes requested via ContourSetValues
 */

/*ARGSUSED*/

static NhlErrorTypes	CheckColorArray
#if __STDC__
	(NhlContourLayer	cl,
	NhlGenArray	ga,
	int		count,
	int		init_count,
	int		last_count,
	int		**gks_colors,
	NhlString	resource_name,
	NhlString	entry_name)
#else
(cl,ga,count,init_count,last_count,gks_colors,resource_name,entry_name)
	NhlContourLayer	cl;
	NhlGenArray	ga;
	int		count;
	int		init_count;
	int		last_count;
	int		**gks_colors;
	NhlString	resource_name;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	char *e_text;
	NhlContourLayerPart *cnp = &(cl->contour);
	int *ip;
	int i, len, spacing;
	

	ip = (int *) ga->data;
	NhlVAGetValues(cl->base.wkptr->base.id,
		     NhlNwkColorMapLen, &len, NULL);
#if 0
	for (i=init_count; i < MIN(count,Def_Colors_Count); i++) 
		ip[i] = Def_Colors[i] < len ? 
			Def_Colors[i] : Def_Colors[i] % len + 1;
	for (i=MAX(init_count,Def_Colors_Count); i<count; i++)
		ip[i] = i < len ? i : i % len + 1;
#endif
	
	spacing = MAX(len / count, 1); 
	for (i=init_count; i < count; i++) 
		ip[i] = 1 + i * spacing;

	if (last_count == 0) {
		if ((*gks_colors = 
		     (int *) NhlMalloc(count * sizeof(int))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	else if (count > last_count) {
		if ((*gks_colors = 
		     (int *) NhlRealloc(*gks_colors,
					count * sizeof(int))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	for (i=0; i<count; i++) {
		if (ip[i] < 0 || ip[i] > len) {
			e_text =
	       "%s: %s index %d holds an invalid color value, %d: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  resource_name, i, ip[i]);
		        ret = MIN(ret, NhlWARNING);
			ip[i] = Nhl_cnDEF_COLOR;
		}

		(*gks_colors)[i] =
			_NhlGetGksCi(cl->base.wkptr,ip[i]);
	}
}



/*
 * Function:  SetupLevels
 *
 * Description: Depending on the setting of the LevelCount resource,
 *		decides whether to allow Conpack to determine the 
 *		number of Contour levels. If so, makes the appropriate
 *		Contour calls.
 *
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
static NhlErrorTypes    SetupLevels
#if __STDC__
	(NhlLayer		new, 
	 NhlLayer		old,
	 NhlBoolean	init,
	 float		**levels,
	 NhlBoolean	*modified)
#else
(new,old,init,args,num_args)
	NhlLayer		new;
	NhlLayer		old;
	NhlBoolean	init;
	float		**levels;
	NhlBoolean	*modified;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlContourLayer		cnew = (NhlContourLayer) new;
	NhlContourLayerPart	*cnp = &(cnew->contour);
	NhlContourLayer		cold = (NhlContourLayer) old;
	NhlContourLayerPart	*ocnp = &(cold->contour);
	float			zmin,zmax;
	int			i;

	entry_name = init ? "ContourInitialize" : "ContourSetValues";
	*modified = False;

/*
 * If the data has changed -- find the data max and min 
 */ 
	if (cnp->data_changed) {
		float *fp = T;
		zmin = FLT_MAX;
		zmax = FLT_MIN;
		for (i=0;i<M*N;i++,fp++) {
			if (*fp < zmin) zmin = *fp;
			if (*fp > zmax) zmax = *fp;
		}
		cnp->zmin = zmin;
		cnp->zmax = zmax;
		if (! cnp->min_level_set) cnp->min_level_val = cnp->zmin; 
		if (! cnp->max_level_set) cnp->max_level_val = cnp->zmax; 
	}
		
	if (init || cnp->data_changed ||
	    cnp->levels != ocnp->levels ||
	    cnp->level_selection_mode != ocnp->level_selection_mode ||
	    cnp->max_level_count != ocnp->max_level_count ||
	    cnp->level_spacing != ocnp->level_spacing ||
	    cnp->min_level_set || cnp->max_level_set) {

		cnp->new_draw_req = True;
		if (cnp->min_level_val >= cnp->zmax) {
			if (! cnp->data_changed) {
				ret = MIN(NhlWARNING,ret);
				e_text =
     "%s: Set minimum level exceeds or equals data maximum value: defaulting";
				NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			}
			cnp->min_level_val = cnp->zmin;
		}
		if (cnp->max_level_val <= cnp->zmin) {
			if (! cnp->data_changed) {
				ret = MIN(NhlWARNING,ret);
				e_text =
  "%s: Set maximum level less than or equal to data mimimum value: defaulting";
				NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			}
			cnp->max_level_val = cnp->zmax;
		}

		switch (cnp->level_selection_mode) {

		case Nhl_cnMANUAL:

			subret = SetupLevelsManual(cnew,cold,
						   levels,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				return ret;
			}
			*modified = True;
			break;

		case Nhl_cnEQUALSPACING:

			subret = SetupLevelsEqual(cnew,cold,
						  levels,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				return ret;
			}
			*modified = True;
			break;

		case Nhl_cnAUTOMATIC:

			subret = SetupLevelsAutomatic(cnew,cold,
						      levels,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				return ret;
			}
			*modified = True;
			break;
			
		case Nhl_cnEXPLICIT:

			subret = SetupLevelsExplicit(cnew,cold,
						     levels,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				return ret;
			}
			*modified = True;
			break;

		default:
			ret = NhlFATAL;
			e_text = "%s: Invalid level selection mode";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	return ret;

}

/*
 * Function:  SetupLevelsManual
 *
 * Description: Sets up Manual mode levels
 *
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
static NhlErrorTypes    SetupLevelsManual
#if __STDC__
	(NhlContourLayer	cnew, 
	 NhlContourLayer	cold,
	 float		**levels,
	 char		*entry_name)
#else
(cnew,levels,entry_name)
	NhlContourLayer	cnew;
	NhlContourLayer	cold;
	float		**levels;
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourLayerPart	*cnp = &(cnew->contour);
	NhlContourLayerPart	*ocnp = &(cold->contour);
	int			i, count;
	float			zmin,zmax;
	float			*fp;

	zmax = MIN(cnp->max_level_val, cnp->zmax);
	zmin = MAX(cnp->min_level_val, cnp->zmin);
	count = (zmax - zmin) / cnp->level_spacing + 1;

	if (count > cnp->max_level_count) {
		ret = MIN(NhlWARNING,ret);
		e_text =
      "%s: Level max count exceeded with specified level spacing: defaulting";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		subret = ChooseSpacingLin(&zmin,&zmax,&cnp->level_spacing,7,
					  cnp->max_level_count);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error choosing spacing";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		count = (zmax - zmin) / cnp->level_spacing + 1;
	}

	if ((*levels = (float *) 
	     NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	for (i=0, fp = *levels; i<count; i++) {
		*(fp++) = zmin + i * cnp->level_spacing;
	}

	cnp->level_count = count;

	return ret;
}

/*
 * Function:  SetupLevelsEqual
 *
 * Description: Sets up Equally spaced levels
 *
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
static NhlErrorTypes    SetupLevelsEqual
#if __STDC__
	(NhlContourLayer	cnew,
	 NhlContourLayer	cold,
	 float		**levels,
	 char		*entry_name)
#else
(cnew,levels,entry_name)
	NhlContourLayer	cnew;
	NhlContourLayer	cold;
	float		**levels;
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourLayerPart	*cnp = &(cnew->contour);
	NhlContourLayerPart	*ocnp = &(cold->contour);
	int			i;
	float			zmin,zmax,size;

	zmin = MAX(cnp->min_level_val, cnp->zmin);
	zmax = MIN(cnp->max_level_val, cnp->zmax);
	size = (zmax - zmin) / (cnp->max_level_count - 1);

	if ((*levels = (float *) 
	     NhlMalloc(cnp->max_level_count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	for (i=0; i<cnp->max_level_count; i++) {
		(*levels)[i] = cnp->zmin + i * size;
	}

	cnp->level_count = cnp->max_level_count;

	return ret;
}


/*
 * Function:  SetupLevelsAutomatic
 *
 * Description: Sets up Automatic mode levels
 *
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
static NhlErrorTypes    SetupLevelsAutomatic
#if __STDC__
	(NhlContourLayer	cnew, 
	 NhlContourLayer	cold,
	 float		**levels,
	 char		*entry_name)
#else
(cnew,levels,entry_name)
	NhlContourLayer	cnew;
	NhlContourLayer	cold;
	float		**levels;
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourLayerPart	*cnp = &(cnew->contour);
	NhlContourLayerPart	*ocnp = &(cold->contour);
	int			i, count;
	float			zmin,zmax,spacing;

	zmin = MAX(cnp->min_level_val, cnp->zmin);
	zmax = MIN(cnp->max_level_val, cnp->zmax);

	subret = ChooseSpacingLin(&zmin,&zmax,&spacing,7,cnp->max_level_count);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: error choosing spacing";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		return ret;
	}

	count = (zmax - zmin) / spacing + 1;
	if ((*levels = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	for (i=0; i<count; i++) {
		(*levels)[i] = zmin + i * spacing;
	}

	cnp->level_count = count;
	cnp->level_spacing = spacing;

	return ret;
}


/*
 * Function:  SetupLevelsExplicit
 *
 * Description: Sets up Explicit mode levels
 *
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
static NhlErrorTypes    SetupLevelsExplicit
#if __STDC__
	(NhlContourLayer	cnew, 
	 NhlContourLayer	cold,
	 float		**levels,
	 char		*entry_name)
#else
(cnew,levels,entry_name)
	NhlContourLayer	cnew;
	NhlContourLayer	cold;
	float		**levels;
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlContourLayerPart	*cnp = &(cnew->contour);
	NhlContourLayerPart	*ocnp = &(cold->contour);
	int			i, count, ixmin, ixmax;
	float			*fp;
	float			zmin, zmax, spacing;
	NhlBoolean		do_auto = False;

	zmin = MAX(cnp->min_level_val, cnp->zmin);
	zmax = MIN(cnp->max_level_val, cnp->zmax);

	if ((count = cnp->levels->num_elements) > cnp->max_level_count) {
		count = cnp->max_level_count;
		ret = MIN(NhlWARNING,ret);
		e_text = 
	  "%s: Explicit level array count exceeds max level count: defaulting";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
	}
	fp = (float *)cnp->levels->data;

	ixmin = 0;
	if (fp[0] >= zmax) {
		do_auto = True;
		ret = MIN(NhlWARNING,ret);
		e_text = "%s: Out of range explicit level array: defaulting";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
	}
	if (fp[0] < zmin) ixmin = 1;
		
	ixmax = count;
	for (i=1; i < count; i++) {
		if (fp[i] < fp[i-1]) {
			do_auto = True;
			ret = MIN(NhlWARNING,ret);
			e_text =
		"%s: Invalid non-monotonic explicit level array: defaulting";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			break;
		}
		if (fp[i] < zmin) ixmin = i + 1;
		if (fp[i] > zmax && ixmax == count) ixmax = i;
	}
	if (! (ixmin < ixmax)) {
		do_auto = True;
		ret = MIN(NhlWARNING,ret);
		e_text = "%s: Out of range explicit level array: defaulting";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
	}
	count = ixmax - ixmin;

	if (do_auto) {
		subret = ChooseSpacingLin(&zmin,&zmax,&spacing,
					  7,cnp->max_level_count);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error choosing spacing";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		count = (zmax - zmin) / spacing + 1;
	}

	if ((*levels = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	
	if (do_auto) {
		for (i=0; i<count; i++) {
			(*levels)[i] = zmin + i * spacing;
		}
		cnp->level_spacing = spacing;
	}
	else {
		for (i = ixmin; i < ixmax; i++) {
			(*levels)[i-ixmin] = fp[i];
		}
	}

	cnp->level_count = count;

	return ret;
}

/*
 * Function:	ChooseSpacingLin
 *
 * Description: Ethan's tick mark spacing code adapted to choosing 'nice'
 *		contour values; adapted by exchanging the ciel and floor
 *		functions - since the max and min contour values must be
 *		within the data space rather than just outside it as is 
 *		appropriate for tick marks. (Eventually should probably
 *		generalize the code with another parameter to handle either
 *		situation.)
 *
 * In Args:	tstart	requested tick starting location
 *		tend	requested tick ending location
 *		convert_precision  precision used to compare with
 *		max_ticks the maximum number of ticks to be choosen
 * Out Args:
 *		tstart  new start
 *		tend	new end
 *		spacing new spacing
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes ChooseSpacingLin
#if	__STDC__
(float *tstart,float *tend,float *spacing,int convert_precision,int max_ticks)
#else
(tstart,tend,spacing,convert_precision,max_ticks)
float *tstart;
float *tend;
float *spacing;
int	convert_precision;
int	max_ticks;
#endif
{
	double table[10],d,u,t,am1,am2=0.0,ax1,ax2=0.0;
	int	npts,i;

	table[0] = 1.0;
	table[1] = 2.0;
	table[2] = 3.0;
	table[3] = 4.0;
	table[4] = 5.0;
	table[5] = 10.0;
	table[6] = 20.0;
	table[7] = 30.0;
	table[8] = 40.0;
	table[9] = 50.0;
	npts = 10;

	if(compare(*tend,*tstart,8)<=0.0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ChooseSpacingLin: An impossible condition has been detected, TickEnd and TickStart are nearly equal, cannot continue");
		return(NhlFATAL);
	}

	d = pow(10.0,floor(log10(*tend-*tstart)) - 1.0);
	u = *spacing = 1e30;
	for(i=0;i<npts; i++) {
		t = table[i] * d;
		am1 = ceil(*tstart/t) *t;
		ax1 = floor(*tend/t) * t;
		if(((i>=npts-1)&&(*spacing == u))||((t <= *spacing)
			&&(compare((ax1-am1)/t,(double)max_ticks,convert_precision) <= 0.0))){
			*spacing = t;
			ax2 = ax1;
			am2 = am1;
		}
	}
	*tstart = am2;
	*tend = ax2;
	return(NhlNOERROR);
	
}

/*
 * Function:	compare
 *
 * Description: Compare two floating point numbers to a certain number of
 *		significant digits. This was a tricky algorythm to come up
 *		with. Essentially what compare does is figure out what it
 *		takes to shift one of the numbers to the immediate left of
 *		the decimal place. It divides both numbers by this number.
 *		Then it multiplies both numbers by 10**sig_digit, rounds them
 *		then casts the resulting number into an integer. These two
 *		integers are subtracted from each other and the result returned
 *		
 *
 * In Args:	a	first floating point number
 *		b	second floating point number
 *		sig_dig	<=7 represents number of significant digits to compare.
 *
 * Out Args:	NONE
 *
 * Return Values: 0 if equal, <0 if a<b, and >0 if a>b
 *
 * Side Effects: NONE
 */
static float	compare
#if	__STDC__
(float a, float b, int sig_dig)
#else
(a,b,sig_dig)
	float a;
	float b;
	int sig_dig;
#endif
{
	float	a_final;
	float	b_final;
	long a_int;
	long b_int;
	int exp;
	int signa;
	int signb;
	float tmp;
	
/*
* If sig_dig is > 6, a_int and b_int will overflow and cause problems
*/
	if(sig_dig > 7) 
		sig_dig = 7;

/*
* Get ride of easy cases:
* These actually didn't end up being easy since large numbers compared againts
* zero cause a_int and b_int to overflow. So I added the fabs checks to make
* sure that the absolute value of non-zero numbers are at least between 
* 0 and 1.0.
*/
	if((a == 0.0)&&(b!=0.0)&&(log10(fabs(b))<=0.0)) {
		a_int = 0;
		b_final = b * (float)pow(10.0,(double)sig_dig);
		b_int = (long)b_final;
		return((float)(a_int - b_int));
	} else if((a!=0.0)&&(b==0.0)&&(log10(fabs(a))<=0.0)){
		b_int = 0;
		a_final = a * (float)pow(10.0,(double)sig_dig);
		a_int = (long)a_final;
		return((float)(a_int - b_int));
	} else if((a==0.0)&&(b==0.0)){
		return(0.0);
	}
/*
* If I get here and either a or b is zero then than means one of them is
* greater that 1 and one is 0.0
*/
	if((a==0.0)||(b==0.0)) {
		return(a - b);
	}

	
/*
* store sign info and make sure both numbers are positive so log10 can be
* used. 
*/
	signa = ((float)fabs(a))/a;
	signb = ((float)fabs(b))/b;
	a_final = fabs(a);
	b_final = fabs(b);
/*
* Now compute the exponent needed to shift a to the decimal position immediately
* right of the decimal point for the value of a
*/
	if(a_final>b_final){ 
		tmp = (float)log10(a_final);
		exp = (long)ceil(log10(a_final));
		if((float)exp == tmp)
			exp++;
	} else {
		tmp = (float)log10(b_final);
		exp = (long)ceil(log10(b_final));
		if((float)exp == tmp)
			exp++;
	}

/*
* Now divide through by the exponent determined above
*/
	a_final = a_final/(float)pow(10.0,(double)exp);
	b_final = b_final/(float)pow(10.0,(double)exp);

/*
* Since a and possibly b are now shifted to the immediate left of the decimal,
* multipling by pow(10.0,sig_dig), rounding , setting appropriate sign  and 
* truncating the decimal produces two integers that can be compared.
*/
	a_final = a_final * pow(10.0,(double)sig_dig);
	b_final = b_final * pow(10.0,(double)sig_dig);
	a_final = roundit(a_final,sig_dig);
	b_final = roundit(b_final,sig_dig);
	a_final *= signa;
	b_final *= signb;
	a_int = (long)a_final;
	b_int = (long)b_final;
	return((float)a_int-(float)b_int);
}


/*
 * Function:	roundit
 *
 * Description: Used to round a floating point number to a certain amount of
 *		significant digits. First it converts number to [0.0,1.0]
 *		exclusive and removes the sign. Then it multiplies by 
 * 		10**sig_digit. Then it adds .5 and casts the number to a long, 
 *		(i.e truncates decimal. Next, it converts the number back
 *		into its original magnitude and sign and returns. 
 *
 * In Args:	a	floating point values
 *		sig_dig	number of digit to round to must be <=7
 *
 * Out Args:	NONE
 *
 * Return Values: rounded value
 *
 * Side Effects: NONE
 */
static float roundit
#if	__STDC__
(float a,int sig_digit)
#else
(a,sig_digit)
float a;
int sig_digit;
#endif
{
	double	exp1;
	double	exp2;
	double 	a_final;
	long  ltmp;
	int sign;

/* 
* if its equal to zero just return
*/
	if(a == 0.0)
		return(a);
/*
* floats are only accurate to 7 decimal places when 32bit IEEE used
*/
	if(sig_digit>7)
		sig_digit = 7;
/*
* need to convert <a> so its not a negative, so logs can be taken
*/
	a_final = fabs(a);
	sign = (int)(a_final/a);
	

/*
* Converts number to value between 0.0 and 1.0
*/
	exp1 = (float)ceil(log10(a_final));
	a_final = a_final/pow(10.0,exp1);

/*
* Now perform significant digit computation
*/
	exp2 = pow(10.0,(double)sig_digit);
	ltmp = (long)(a_final * exp2 + .5);
	a_final = ((double)ltmp)/exp2;

/*
* Now convert back to original magnitude
*/

	a_final = (double)sign * a_final * pow(10.0,exp1);

	return((float)a_final);
}
#if 0

/*
 * Function:  cpchcl_
 *
 * Description: C version of the CPCHCL function that is called from
 *              the Conpack CPCLDR and CPCLDM functions. Used here in
 *		order to allow use of the hlu Workstation dash patterns,
 *		which contain longer strings than the Conpack CLD array
 *		allows.
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
void  cpchcl_
#if __STDC__
	(int	*iflg)
#else
(iflg)
	int	*iflg;
#endif

{
	int pai, dpix, jcrt;
	float p0, p1;

	if (*iflg != 1) return;
	
	c_cpgeti("PAI", &pai);

	if (pai > 0) {

		dpix = Mono_Dash_Pattern ? 
			Dash_Patterns[0] : Dash_Patterns[pai-1];
		dpix = dpix % Dash_Table_Len;

		p0 = 0.0;
		p1 = Dash_Size;
		p0 = (float) c_kfpy(p0);
		p1 = (float) c_kfpy(p1);
		jcrt = (p1-p0) / strlen(Dash_Table[dpix]);
		jcrt = jcrt * 2;
		c_dashdc(Dash_Table[dpix],jcrt,3);
	}
	return;
}

#endif
