/*
 *      $Id: MapPlot.c,v 1.11 1994-09-08 17:15:32 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapPlot.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a generic Map plot that
 *			can serve as an overlay base plot or be used for
 *			general map plots that do not use other hlu
 *			plot objects.
 */

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/MapPlotP.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ctype.h>
#include <stdio.h>

/*
 * Function:	ResourceUnset
 *
 * Description:	This function can be used to determine if a resource has
 *		been set at initialize time either in the Create call or
 *		from a resource data base. In order to use it a Boolean
 *		variable (by convention '<var_name>_resource_set')
 *		MUST directly proceed the declaration of the subject
 *		resource variable in the LayerPart struct. Also a .nores 
 *		NhlResource struct for the resource_set variable
 *		must directly preceed the Resource of interest in the 
 *		Resource initialization list of this module.
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

#define Oset(field)	NhlOffset(NhlMapPlotLayerRec,mapplot.field)
static NhlResource resources[] = {
	{NhlNmpOutlineOn, NhlCmpOutlineOn, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(outline_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpOutlineBoundarySets,NhlCmpOutlineBoundarySets,
                 NhlTMapBoundarySets,sizeof(NhlMapBoundarySets),
                 Oset(outline_boundaries),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlmpGEOPHYSICAL),0,NULL},
	{NhlNmpOutlineDrawOrder,NhlCmpOutlineDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(outline_order),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlDRAW),0,NULL},
	{NhlNmpFillOn, NhlCmpFillOn, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(fill_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNmpFillBoundarySets,NhlCmpFillBoundarySets,
                 NhlTMapBoundarySets,sizeof(NhlMapBoundarySets),
                 Oset(fill_boundaries),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlmpGEOPHYSICAL),0,NULL},
	{NhlNmpFillDrawOrder,NhlCmpFillDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(fill_order),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlDRAW),0,NULL},
	{NhlNmpFillGroupCount,NhlCmpFillGroupCount,NhlTInteger,
		 sizeof(int),Oset(fill_group_count),NhlTImmediate,
		 _NhlUSET((NhlPointer) Nhl_mpMIN_FILL_GROUPS),0,NULL},
	{NhlNmpFillAreaSpecifiers,NhlCmpFillAreaSpecifiers, 
		 NhlTStringGenArray,sizeof(NhlPointer),Oset(fill_area_specs),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpMaskAreaSpecifiers,NhlCmpMaskAreaSpecifiers,NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(mask_area_specs),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpOutlineSpecifiers,NhlCmpOutlineSpecifiers,NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(outline_specs),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpFillPatternBackground,NhlCmpFillPatternBackground,NhlTInteger,
		 sizeof(int),Oset(fill_pattern_background),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNmpAreaNames,NhlCmpAreaNames,
		 NhlTStringGenArray,sizeof(NhlPointer),
		 Oset(area_names),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpAreaTypes,NhlCmpAreaTypes,
		 NhlTIntegerGenArray,sizeof(NhlPointer),
		 Oset(area_types),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpAreaGroups,NhlCmpAreaGroups,
		 NhlTIntegerGenArray,sizeof(NhlPointer),
		 Oset(area_groups),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpFillAreaColors,NhlCmpFillAreaColors,
		 NhlTIntegerGenArray,sizeof(NhlPointer),
		 Oset(fill_area_colors),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpDirectFillAreaColor, NhlCmpDirectFillAreaColor, 
		 NhlTBoolean,sizeof(NhlBoolean),Oset(direct_fill_area_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNmpAreaGroupPriority,NhlCmpAreaGroupPriority,NhlTAreaGroupPriority,
		 sizeof(NhlAreaGroupPriority),Oset(group_priority),
		 NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlmpGEOPHYSICALPRIORITY),0,NULL},

	{NhlNmpMonoFillGroupColor, NhlCmpMonoFillGroupColor, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_fill_group_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNmpMonoFillGroupPattern, NhlCmpMonoFillGroupPattern, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_fill_group_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpMonoFillGroupScale, NhlCmpMonoFillGroupScale, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(mono_fill_group_scale),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpFillGroupColors, NhlCmpFillGroupColors, NhlTIntegerGenArray,
		 sizeof(NhlPointer),Oset(fill_group_colors),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpFillGroupPatterns, NhlCmpFillGroupPatterns, NhlTIntegerGenArray,
		 sizeof(NhlPointer),Oset(fill_group_patterns),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNmpFillGroupScales, NhlCmpFillGroupScales,  NhlTFloatGenArray,
		 sizeof(NhlPointer),Oset(fill_group_scales),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},

	{NhlNmpDefaultFillColor,NhlCmpDefaultFillColor,NhlTInteger,
		 sizeof(int),Oset(fill_default.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlmpUNSETCOLOR),0,NULL},
	{NhlNmpDefaultFillPattern,NhlCmpDefaultFillPattern,NhlTInteger,
		 sizeof(int),Oset(fill_default.pattern),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlmpUNSETFILLPATTERN),0,NULL},
	{NhlNmpDefaultFillScaleF,NhlCmpDefaultFillScaleF,NhlTFloat,
		 sizeof(float),Oset(fill_default.scale),
		 NhlTString,_NhlUSET("NhlmpUNSETFILLSCALE"),0,NULL},
	{NhlNmpOceanFillColor,NhlCmpOceanFillColor,NhlTInteger,
		 sizeof(int),Oset(ocean.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlmpUNSETCOLOR),0,NULL},
	{NhlNmpOceanFillPattern,NhlCmpOceanFillPattern,NhlTInteger,
		 sizeof(int),Oset(ocean.pattern),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlmpUNSETFILLPATTERN),0,NULL},
	{NhlNmpOceanFillScaleF,NhlCmpOceanFillScaleF,NhlTFloat,
		 sizeof(float),Oset(ocean.scale),
		 NhlTString,_NhlUSET("NhlmpUNSETFILLSCALE"),0,NULL},
	{NhlNmpLandFillColor,NhlCmpLandFillColor,NhlTInteger,
		 sizeof(int),Oset(land.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlmpUNSETCOLOR),0,NULL},
	{NhlNmpLandFillPattern,NhlCmpLandFillPattern,NhlTInteger,
		 sizeof(int),Oset(land.pattern),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlmpUNSETFILLPATTERN),0,NULL},
	{NhlNmpLandFillScaleF,NhlCmpLandFillScaleF,NhlTFloat,
		 sizeof(float),Oset(land.scale),
		 NhlTString,_NhlUSET("NhlmpUNSETFILLSCALE"),0,NULL},
	{NhlNmpInlandWaterFillColor,NhlCmpInlandWaterFillColor,NhlTInteger,
		 sizeof(int),Oset(inland_water.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlmpUNSETCOLOR),0,NULL},
	{NhlNmpInlandWaterFillPattern,NhlCmpInlandWaterFillPattern,NhlTInteger,
		 sizeof(int),Oset(inland_water.pattern),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlmpUNSETFILLPATTERN),0,NULL},
	{NhlNmpInlandWaterFillScaleF,NhlCmpInlandWaterFillScaleF,NhlTFloat,
		 sizeof(float),Oset(inland_water.scale),
		 NhlTString,_NhlUSET("NhlmpUNSETFILLSCALE"),0,NULL},

	{NhlNmpGeophysicalLineColor,NhlCmpGeophysicalLineColor,NhlTInteger,
		 sizeof(int),Oset(geophysical.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpGeophysicalLineDashPattern,NhlCmpGeophysicalLineDashPattern,
		 NhlTInteger,sizeof(int),Oset(geophysical.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(geophysical.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNmpGeophysicalLineDashSegLenF,NhlCmpGeophysicalLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(geophysical.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{NhlNmpGeophysicalLineThicknessF,NhlCmpGeophysicalLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(geophysical.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNmpUSStateLineColor,NhlCmpUSStateLineColor,NhlTInteger,
		 sizeof(int),Oset(us_state.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpUSStateLineDashPattern,NhlCmpUSStateLineDashPattern,
		 NhlTInteger,sizeof(int),Oset(us_state.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(us_state.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNmpUSStateLineDashSegLenF,NhlCmpUSStateLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(us_state.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{NhlNmpUSStateLineThicknessF,NhlCmpUSStateLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(us_state.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNmpNationalLineColor,NhlCmpNationalLineColor,NhlTInteger,
		 sizeof(int),Oset(national.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpNationalLineDashPattern,NhlCmpNationalLineDashPattern,
		 NhlTInteger,sizeof(int),Oset(national.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(national.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNmpNationalLineDashSegLenF,NhlCmpNationalLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(national.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{NhlNmpNationalLineThicknessF,NhlCmpNationalLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(national.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNmpRelativeGridSpacing,NhlCmpRelativeGridSpacing,
		 NhlTBoolean,sizeof(NhlBoolean),Oset(relative_grid_spacing),
		 NhlTImmediate, _NhlUSET((NhlPointer)True),0,NULL},
	{NhlNmpGridSpacingF,NhlCmpGridSpacingF,
		 NhlTFloat,sizeof(float),Oset(grid_spacing),
		 NhlTString, _NhlUSET("0.1"),0,NULL},
	{NhlNmpGridMaskMode,NhlCmpGridMaskMode,NhlTMapGridMaskMode,
		 sizeof(NhlMapGridMaskMode),Oset(grid_mask_mode),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlmpMASKNONE),0,NULL},
	{NhlNmpGridAndLimbOn,NhlCmpGridAndLimbOn,NhlTBoolean,
		 sizeof(long),Oset(grid.on),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpGridAndLimbDrawOrder,NhlCmpGridAndLimbDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(grid.order),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlDRAW),0,NULL},
	{NhlNmpGridLineColor,NhlCmpGridLineColor,NhlTInteger,
		 sizeof(int),Oset(grid.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpGridLineDashPattern,NhlCmpGridLineDashPattern,
		 NhlTInteger,sizeof(int),Oset(grid.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(grid.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNmpGridLineDashSegLenF,NhlCmpGridLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(grid.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{NhlNmpGridLineThicknessF,NhlCmpGridLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(grid.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNmpLimbLineColor,NhlCmpLimbLineColor,NhlTInteger,
		 sizeof(int),Oset(limb.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpLimbLineDashPattern,NhlCmpLimbLineDashPattern,
		 NhlTInteger,sizeof(int),Oset(limb.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(limb.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNmpLimbLineDashSegLenF,NhlCmpLimbLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(limb.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{NhlNmpLimbLineThicknessF,NhlCmpLimbLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(limb.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},


	{NhlNmpPerimOn,NhlCmpPerimOn,NhlTBoolean,
		 sizeof(long),Oset(perim.on),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpPerimDrawOrder,NhlCmpPerimDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(perim.order),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlDRAW),0,NULL},
	{NhlNmpPerimLineColor,NhlCmpPerimLineColor,NhlTInteger,
		 sizeof(int),Oset(perim.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpPerimLineDashPattern,NhlCmpPerimLineDashPattern,
		 NhlTInteger,sizeof(int),Oset(perim.dash_pat),
		 NhlTImmediate,_NhlUSET(0),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(perim.dash_seglen_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNmpPerimLineDashSegLenF,NhlCmpPerimLineDashSegLenF,
		 NhlTFloat,sizeof(float),Oset(perim.dash_seglen),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{NhlNmpPerimLineThicknessF,NhlCmpPerimLineThicknessF,
		 NhlTFloat,sizeof(float),Oset(perim.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNmpLabelsOn,NhlCmpLabelsOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(labels.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNmpLabelDrawOrder,NhlCmpLabelDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(labels.order),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlDRAW),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(labels.height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
        {NhlNmpLabelTextHeightF,NhlCmpLabelTextHeightF,
		 NhlTFloat,sizeof(float),Oset(labels.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
        {NhlNmpLabelTextDirection,NhlCmpLabelTextDirection,
		 NhlTTextDirection,sizeof(NhlTextDirection),
		 Oset(labels.direction),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlACROSS),0,NULL},
	{NhlNmpLabelFont,NhlCmpLabelFont,NhlTFont, 
		 sizeof(int),Oset(labels.font),
		 NhlTImmediate,_NhlUSET((NhlPointer) 1),0,NULL},
	{NhlNmpLabelFontColor,NhlCmpLabelFontColor,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(labels.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpLabelFontAspectF,NhlCmpLabelFontAspectF,NhlTFloat, 
		 sizeof(float),Oset(labels.aspect),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNmpLabelFontThicknessF,NhlCmpLabelFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(labels.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNmpLabelFontQuality,NhlCmpLabelFontQuality,
		 NhlTFQuality, 
		 sizeof(NhlFontQuality),Oset(labels.quality),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNmpLabelConstantSpacingF,NhlCmpLabelConstantSpacingF,
		 NhlTFloat,sizeof(float),Oset(labels.cspacing),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNmpLabelAngleF,NhlCmpLabelAngleF,
		 NhlTFloat,sizeof(float),Oset(labels.angle),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNmpLabelFuncCode,NhlCmpLabelFuncCode,NhlTCharacter, 
		 sizeof(char),Oset(labels.fcode[0]),
		 NhlTString, _NhlUSET(":"),0,NULL},
	{NhlNmpLabelBackgroundColor,NhlCmpLabelBackgroundColor,
		 NhlTInteger,sizeof(int),Oset(labels.back_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNmpLabelPerim,NhlCmpLabelPerim,NhlTInteger,
		 sizeof(int),Oset(labels.perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNmpLabelPerimSpaceF,NhlCmpLabelPerimSpaceF,
		 NhlTFloat,sizeof(float),Oset(labels.perim_space),
		 NhlTString,_NhlUSET("0.33"),0,NULL},
	{NhlNmpLabelPerimColor,NhlCmpLabelPerimColor,NhlTInteger,
		 sizeof(int),Oset(labels.perim_lcolor),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNmpLabelPerimThicknessF,NhlCmpLabelFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(labels.perim_lthick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
};
#undef Oset

/* base methods */


static NhlErrorTypes MapPlotClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes MapPlotClassPartInitialize(
#ifdef NhlNeedProto
	NhlLayerClass	lc
#endif
);

static NhlErrorTypes MapPlotInitialize(
#ifdef NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes MapPlotSetValues(
#ifdef NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes    MapPlotGetValues(
#ifdef NhlNeedProto
	NhlLayer,       /* l */
	_NhlArgList,    /* args */
	int             /* num_args */
#endif
);

static NhlErrorTypes MapPlotDestroy(
#ifdef NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes MapPlotPreDraw(
#ifdef NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes MapPlotDraw(
#ifdef NhlNeedProto
        NhlLayer   /* layer */
#endif
);


static NhlErrorTypes MapPlotPostDraw(
#ifdef NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlGenArray mpGetNewGenArray(
#ifdef NhlNeedProto
	NhlMapPlotLayerPart	*mpp,
	NrmQuark		quark,
	NhlString		type
#endif
);

static NhlGenArray mpGenArraySubsetCopy(
#ifdef NhlNeedProto
	NhlGenArray    ga,
        int             length
#endif
);

static NhlErrorTypes mpDraw(
#ifdef NhlNeedProto
        NhlMapPlotLayer	mpl,
	NhlDrawOrder	order
#endif
);

static NhlErrorTypes mpFill(
#ifdef NhlNeedProto
	NhlMapPlotLayer	mp,
	NhlString	entry_name
#endif
);

static NhlErrorTypes mpGrid(
#ifdef NhlNeedProto
	NhlMapPlotLayer	mp,
	NhlString	entry_name
#endif
);

static NhlErrorTypes mpOutline(
#ifdef NhlNeedProto
	NhlMapPlotLayer	mp,
	NhlString	entry_name
#endif
);

static NhlErrorTypes mpSetUpAreamap(
#ifdef NhlNeedProto
	NhlMapPlotLayer	mp,
	NhlWorkspace	**aws,
	int 		amap_type,
	NhlString	entry_name
#endif
);

static NhlErrorTypes mpSetUpDrawIds(
#ifdef NhlNeedProto
	NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	NhlString		entry_name
#endif
);

static NhlErrorTypes mpSetUpStateDrawIds(
#ifdef NhlNeedProto
	NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	NhlString		entry_name
#endif
);

static NhlErrorTypes mpExpandId(
#ifdef NhlNeedProto
	NhlMapPlotLayerPart	*mpp,
	int			ix,
	int			draw_mode,
	int			color,
	NhlString		entry_name
#endif
);

static NhlErrorTypes    mpManageViewDepResources(
#ifdef NhlNeedProto
	NhlMapPlotLayer mpnew,
	NhlMapPlotLayer	mpold,
        NhlBoolean	init					    
#endif
);

static NhlErrorTypes    SetLineAttrs(
#ifdef NhlNeedProto
	NhlMapPlotLayer mpnew,
	NhlMapPlotLayer	mpold,
        NhlBoolean	init					    
#endif
);

static NhlErrorTypes    mpManageDynamicArrays(
#ifdef NhlNeedProto
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    mpBuildFillDrawList(
#ifdef NhlNeedProto
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    mpBuildOutlineDrawList(
#ifdef NhlNeedProto
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    mpUpdateDrawGroups(
#ifdef NhlNeedProto
	NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	int			draw_mode,
	int			color,
	int			index,
	char			*entry_name
#endif
);

static NhlErrorTypes mpUpdateNameRecs(
#ifdef NhlNeedProto
	NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	NhlString		name,
	int			draw_mode,
	int			color,
	NhlString		entry_name
#endif
);

static NhlErrorTypes    mpManageGenArray(
#ifdef NhlNeedProto
	NhlGenArray	*ga,
	int		count,
	NhlGenArray	copy_ga,
	NrmQuark	type,
	NhlPointer	init_val,
	int		*old_count,
	int		*init_count,
	NhlBoolean	*need_check,
	NhlBoolean	*changed,				       
	NhlString	resource_name,
	NhlString	entry_name
#endif
);

static void mpAdjustDashSegLen(
#ifdef NhlNeedProto
	float		*seglen,
	NhlBoolean	init,
	float		new_vpwidth,
	float		old_vpwidth
#endif
);

static NhlErrorTypes    mpAdjustText(
#ifdef NhlNeedProto
	NhlmpLabelAttrs *lbl_attrs,
	NhlMapPlotLayer	new, 
	NhlMapPlotLayer	old,
	NhlBoolean	init
#endif
);

static NhlErrorTypes mpSetUpTransObj(
#ifdef NhlNeedProto
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init
#endif
);

extern void   (_NHLCALLF(mapusr,MAPUSR))(
#ifdef NhlNeedProto
	int	*iprt
#endif
);

void   (_NHLCALLF(mapeod,MAPEOD))(
#ifdef NhlNeedProto
	int *nout,
	int *nseg,
	int *idls,
	int *idrs,
	int *npts,
	float *pnts
#endif
);

extern int (_NHLCALLF(nhlezmapfill,NHLEZMAPFILL))(
#ifdef NhlNeedProto
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
#endif
);

extern int (_NHLCALLF(nhlmaskgrid,NHLMASKGRID))(
#ifdef NhlNeedProto
	float *xcra, 
	float *ycra, 
	int *ncra, 
	int *iaai, 
	int *iagi, 
	int *nogi
#endif
);

NhlMapPlotLayerClassRec NhlmapPlotLayerClassRec = {
        {
/* class_name			*/      "MapPlot",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlMapPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (NhlLayerClass)&NhltransformLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	MapPlotClassPartInitialize,
/* class_initialize		*/	MapPlotClassInitialize,
/* layer_initialize		*/	MapPlotInitialize,
/* layer_set_values		*/	MapPlotSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	MapPlotGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	MapPlotDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      MapPlotDraw,

/* layer_pre_draw		*/      MapPlotPreDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      MapPlotPostDraw,
/* layer_clear			*/      NULL

        },
	{
/* segment_wkid			*/	0,
/* get_bb			*/	NULL
	},
	{
/* overlay_capability 		*/	_tfOverlayBaseOnly,
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL
	},
	{
/* foo				*/	NULL
	}
};

NhlLayerClass NhlmapPlotLayerClass = (NhlLayerClass)&NhlmapPlotLayerClassRec;

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark Qfill_area_specs = NrmNULLQUARK;
static NrmQuark Qmask_area_specs = NrmNULLQUARK;
static NrmQuark Qoutline_specs = NrmNULLQUARK;
static NrmQuark Qarea_names = NrmNULLQUARK;
static NrmQuark Qarea_types = NrmNULLQUARK;
static NrmQuark Qarea_groups = NrmNULLQUARK;
static NrmQuark Qfill_area_colors = NrmNULLQUARK;
static NrmQuark Qfill_group_colors = NrmNULLQUARK;
static NrmQuark Qfill_group_patterns = NrmNULLQUARK;
static NrmQuark Qfill_group_scales = NrmNULLQUARK;

static NhlMapPlotLayerPart *Mpp, *Ompp;
static NhlMapPlotLayer Mpl, Ompl;
static NhlBoolean Global_Amap_Inited;
static NhlBoolean US_Amap_Inited;

/* Areamap types - not to be confused with area set list types */

#define mpGLOBAL_AMAP	0
#define mpUSSTATES_AMAP	1

static int Init_Colors[] ={106,104,57,104,10,20,30,40,60,70,80,90,100,110};

static mpOutlineRec *OutRecs = NULL;
static int OutRec_Count = 0;
static int Otype_Start[NhlmpOUTLINE_TYPE_COUNT + 1];
static mpDrawOp Draw_Op;
static mpOutlineSet Outline_Set;

static mpDrawIdRec DrawIds[600];

static short Id_Offset[4] = { 1,1004,223,440};
static short Id_End_Reg[4] = { 222,1361,439,1003 };
static short Draw_Special_Check[] = { 2,1005,223,441 };
static short Draw_Check;
static short US_Border;
static short US_Color;

#define mpUS_PO_IX	1031
#define mpUS_PS_IX	1015

/* draw Modes */

#define mpBACKGROUND	0
#define mpDRAW		1
#define mpMASK		2
#define mpSYSEXCLUDE	3
#define mpDRAWSPECIAL	4

static char *BGroup_Names[] = { 
	NhlmpALLNATIONAL,
	NhlmpALLGEOPHYSICAL,
	NhlmpLAND,
	NhlmpWATER,
	NhlmpINLANDWATER,
	NhlmpOCEANS,
	NhlmpCONTINENTS,
	NhlmpISLANDS,
	NhlmpLARGEISLANDS,
	NhlmpSMALLISLANDS,
	NhlmpALLUSSTATES,
	NhlmpUSSTATESLAND,
	NhlmpUSSTATESWATER };

/*
 * The following areas representing the US and the Great Lakes must
 * be excluded from the national and USState set when the USState set is used
 * with the National set.
 */

#if 0
static short National_Excludes[] = { 1031,1058,1065,1071,1072,1081 };
#endif
static short USState_Excludes[] = { 223 };

/*
 * The following area groupings allow areas from the Geophysical map set
 * (the Ezmap 'CO' set) to be simulated using the National set (Ezmap 'PO').
 */

static short NSAmerica[] = { /* 1362 */
	1014,1026,1031,1037,1057,1061,1063,1064,1066,1068,1072,1074,1076,
	1077,1082,1090,1094,1097,1098,1101,1109,1113,1119,1120,1122 };

static short Tierra_Del_Fuego[] = { /* 1363 */
	1091,1103};

static short Dominican_Republic_and_Haiti[] = {  /* 1364 */
	1092,1100 };

static short Africa_Eurasia[] = { /* 1365 */
	1006,1127,1128,1129,1131,1132,1134,1137,1138,1139,1140,1142,1143,
	1144,1145,1151,1153,1154,1155,1156,1157,1158,1160,1161,1162,1163,
	1164,1165,1166,1167,1168,1169,1171,1174,1175,1176,1177,1178,1180,
	1184,1185,1186,1187,1188,1189,1190,1193,1194,1195,1196,1197,1198,
	1200,1201,1202,1203,1204,1205,1208,1209,1211,1213,1214,1215,1216,
	1219,1221,1222,1223,1225,1226,1229,1230,1231,1233,1236,1237,1238,
	1239,1240,1241,1242,1243,1245,1246,1248,1251,1252,1254,1260,1264,
	1265,1267,1270,1272,1274,1275,1276,1277,1279,1282,1285,1286,1287,
	1314,1317,1361 };

static short England_Scotland_Wales[] = { /* 1366 */
	1149,1150,1152 };

static short Ireland[] = { /* 1367 */
	1141,1146 };

static short Borneo[] = { /* 1368 */
	1293,1294,1298 };

static short New_Guinea[] = { /* 1369 */
	1324,1324,1336 };

static short PS_NSAmerica[] = { /* 1004 */
	450,462,1015,491,549,567,572,582,588,596,617,630,632,647,
	693,705,710,713,734,751,755,761,762,764};

static short PS_Tierra_Del_Fuego[] = {  /* 1005 */
	699,739};

static short PS_Dominican_Republic_and_Haiti[] = {  /* 1006 */
	700,724};

static short PS_Africa_Eurasia[] = { /* 1007 */
	442,769,770,771,773,774,776,779,780,781,782,784,785,786,787,793,
	795,796,797,798,799,800,802,803,804,805,806,807,808,809,810,811,
	813,816,817,818,819,820,822,826,827,828,829,830,831,832,835,836,
	837,838,839,840,842,843,848,845,846,847,850,851,853,855,856,857,
	858,861,863,864,865,867,868,871,872,873,875,878,879,880,881,882,
	883,884,885,887,888,890,893,894,896,902,906,907,909,912,914,916,
	917,918,919,921,924,927,928,929,956,959,1003};

static short PS_England_Scotland_Wales[] = { /* 1008 */
	791,792,794};

static short PS_Ireland[] = {  /* 1009 */
	783,788};

static short PS_Borneo[] = { /* 1010 */
	935,936,940};

static short PS_New_Guinea[] = {  /* 1011 */
	966,966,978};

static short PS_Bahamas_1[] = { /* 1012 */
	649,652,653,655,658 };

static short PS_Bahamas_2[] = { /* 1013 */
	687,663,664,665 };

static short PS_Bahamas_3[] = {  /* 1014 */
	648,656,657,654,659,660 };

static short *Exp_Ids[][2] = { 
	{NSAmerica,PS_NSAmerica},
	{Tierra_Del_Fuego,PS_Tierra_Del_Fuego},
	{Dominican_Republic_and_Haiti,PS_Dominican_Republic_and_Haiti},
	{Africa_Eurasia,PS_Africa_Eurasia},
	{England_Scotland_Wales,PS_England_Scotland_Wales},
	{Ireland,PS_Ireland},
	{Borneo,PS_Borneo},
	{New_Guinea,PS_New_Guinea},
	{NULL,PS_Bahamas_1},
	{NULL,PS_Bahamas_2},
	{NULL,PS_Bahamas_3},
	{NULL,NULL} };

static short Exp_Ids_Count[][2] = { 
	{NhlNumber(NSAmerica),NhlNumber(PS_NSAmerica)},
	{NhlNumber(Tierra_Del_Fuego),NhlNumber(PS_Tierra_Del_Fuego)},
	{NhlNumber(Dominican_Republic_and_Haiti),
		 NhlNumber(PS_Dominican_Republic_and_Haiti)},
	{NhlNumber(Africa_Eurasia),NhlNumber(PS_Africa_Eurasia)},
	{NhlNumber(England_Scotland_Wales),
		 NhlNumber(PS_England_Scotland_Wales)},
	{NhlNumber(Ireland),NhlNumber(PS_Ireland)},
	{NhlNumber(Borneo),NhlNumber(PS_Borneo)},
	{NhlNumber(New_Guinea),NhlNumber(PS_New_Guinea)},
	{0,NhlNumber(PS_Bahamas_1)},
	{0,NhlNumber(PS_Bahamas_2)},
	{0,NhlNumber(PS_Bahamas_3)},
	{0,0} };

#if 0
/* north+southamerica */
static int NatCOSegs[] = {
/* n+s-america */
	21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
	42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
/* tierra-del-fuego */
	84,85,86,
/* haiti-dominican-republic */
	94,95,96,
/* africa-eurasia */
	163,164,165,166,167,168,169,170,171,172,173,
	174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,
	190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,
	206,207,208,209,211,212,213,214,215,216,217,218,219,220,221,222,
	223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,
	239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,
	255,256,257,258,259,
/* england-scotland-wales */
	349,350,351,352,353,
/* ireland */
	354,355,356,
/* borneo */
	403,404,405,
	406,407,
/* new-guinea */
	416,417,418 };
#endif

/*
 * Function:	nhlfmapplotclass
 *
 * Description:	fortran ref to mapplot class
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
_NHLCALLF(nhlfmapplotclass,NHLFMAPPLOTCLASS)
#if	__STDC__
(
	void
)
#else
()
#endif
{
	return NhlmapPlotLayerClass;
}

/*
 * Function:	MapPlotClassInitialize
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
MapPlotClassInitialize
#if __STDC__
(
	void
)
#else
()
#endif
{
        _NhlEnumVals   mapboundarysetslist[] = {
        {NhlmpNOBOUNDARIES,		"noboundaries"},
        {NhlmpGEOPHYSICAL, 		"geophysical"},
        {NhlmpNATIONAL,			"national"},
        {NhlmpUSSTATES,      		"usstates"},
	{NhlmpGEOPHYSICALANDUSSTATES,	"geophysicalandusstates"},
        {NhlmpALLBOUNDARIES,    	"allboundaries"},
        };

	_NhlEnumVals areagroupprioritylist[] =  {
	{NhlmpGEOPHYSICALPRIORITY,	"geophysicalpriority"},
	{NhlmpPOLITICALPRIORITY,	"politicalpriority"}
	};

	_NhlEnumVals mapgridmaskmodelist[] =  {
	{NhlmpMASKNONE,		"masknone"},
	{NhlmpMASKOCEAN,	"maskocean"},
	{NhlmpMASKNOTOCEAN,	"masknotocean"},
	{NhlmpMASKLAND,		"maskland"},
	{NhlmpMASKNOTLAND,	"masknotland"},
	{NhlmpMASKFILLAREA,	"maskfillarea"},
	{NhlmpMASKMASKAREA,	"maskmaskarea"}
	};

        _NhlRegisterEnumType(NhlTMapBoundarySets,mapboundarysetslist,
                             NhlNumber(mapboundarysetslist));

        _NhlRegisterEnumType(NhlTAreaGroupPriority,areagroupprioritylist,
                             NhlNumber(areagroupprioritylist));

        _NhlRegisterEnumType(NhlTMapGridMaskMode,mapgridmaskmodelist,
                             NhlNumber(mapgridmaskmodelist));

	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qfill_area_specs = NrmStringToQuark(NhlNmpFillAreaSpecifiers);
	Qmask_area_specs = NrmStringToQuark(NhlNmpMaskAreaSpecifiers);
	Qoutline_specs = NrmStringToQuark(NhlNmpOutlineSpecifiers);
	Qarea_names = NrmStringToQuark(NhlNmpAreaNames);
	Qarea_types = NrmStringToQuark(NhlNmpAreaTypes);
	Qarea_groups = NrmStringToQuark(NhlNmpAreaGroups);
	Qfill_area_colors = NrmStringToQuark(NhlNmpFillAreaColors);
	Qfill_group_colors = NrmStringToQuark(NhlNmpFillGroupColors);
	Qfill_group_patterns = NrmStringToQuark(NhlNmpFillGroupPatterns);
	Qfill_group_scales = NrmStringToQuark(NhlNmpFillGroupScales);

	return NhlNOERROR;
}

/*
 * Function:	MapPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlMapPlotLayerClassPart that cannot be initialized statically.
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
MapPlotClassPartInitialize
#if	__STDC__
(
	NhlLayerClass	lc	/* NhlLayer Class to init	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* NhlLayer Class to init	*/
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "MapPlotClassPartInitialize";

	/*
	 * Register children objects
	 */

	subret = _NhlRegisterChildClass(lc,NhloverlayLayerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhloverlayLayerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlmapTransObjLayerClass,
					False,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlmapTransObjLayerClass");
		return(NhlFATAL);
	}

	return ret;
}


void mpLowerCase(char *string)
{
	char *cp = string;

	while (*cp != '\0') {
		*cp = tolower(*cp);
		cp++;
	}
}
#if 0
void mpGetOutRecsByName(mpOutlineRec *orp, int nrecs, NhlString name)
{
	mpOutlineRec *lorp = orp;
	int i;
	int len = strlen(name) - 1;
	char tname[128];

	strcpy(tname,name);
	mpLowerCase(tname);
	if (name[len] == '*') {
		for (i = 0; i < nrecs; i++) {
			if (! strncmp(lorp->name,tname,len)) {
				printf("%s -- %d, %d\n",
				       lorp->name,lorp->id[0],lorp->id[1]);
			}
			lorp++;
		}
	}
	else {
		for (i = 0; i < nrecs; i++) {
			if (! strcmp(lorp->name,tname)) {
				printf("%s -- %d, %d\n",
				       lorp->name,lorp->id[0],lorp->id[1]);
			}
			lorp++;
		}
	}
}

void mpGetOutRecsByType(mpOutlineRec *orp, int nrecs, mpOutlineType type)
{
	mpOutlineRec *lorp = orp;
	int i;

	for (i = 0; i < nrecs; i++) {
		if (lorp->type == type)
			printf("%s -- %d, %d\n",
			       lorp->name,lorp->id[0],lorp->id[1]);
		lorp++;
	}
}

void mpprintids(FILE *fp,short *idlist, int count)
{
	int i, j;
	for (i = 0; i < count; i++) 
		for (j = 0; j < OutRec_Count; j++)
			if (OutRecs[j].id[1] == idlist[i]) {
				fprintf(fp,"%d,",OutRecs[j].id[2]);
				break;
			}
}
#endif
	
NhlErrorTypes Init_Outline_Recs(NhlString entry_name)
{
	char *e_text;
	FILE *fp;
	char buf[256],name[128];
	int id0,id1,id2,cix0,cix1,type;
	mpOutlineRec *orp;
	int nalloced = 0;
	int bytes = 0, count;
	mpOutlineType last_type;
	Const char *db_path;
	char *full_name;

	if ((db_path = GetNCARGPath("database")) == NULL) {
		e_text = "%s: cannot find path to NCARG database";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if ((full_name = NhlMalloc(strlen(db_path) + 
				   strlen(Nhl_mpMAPDATAFILE) + 2)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	strcpy(full_name,db_path);
	strcat(full_name,"/");
	strcat(full_name,Nhl_mpMAPDATAFILE);
		      
	if ((fp = fopen(full_name,"r")) == NULL) {
		e_text = "%s: cannot open map data file: %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  e_text,entry_name,Nhl_mpMAPDATAFILE);
		return NhlFATAL;
	}

	if ((OutRecs = (mpOutlineRec *) 
		NhlMalloc(mpALLOC_UNIT * sizeof(mpOutlineRec))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	nalloced = mpALLOC_UNIT;

	last_type = mpOcean;
	Otype_Start[mpOcean] = 0;
	while (fgets(buf,128,fp) != NULL) {
		sscanf(buf,"%d:%d:%d:%d:%d:%d%s",&type,
		       &id0,&cix0,&id1,&cix1,&id2,name);

		if (OutRec_Count == nalloced) {
			if ((OutRecs = (mpOutlineRec *)
			     NhlRealloc(OutRecs,(nalloced + mpALLOC_UNIT) 
					* sizeof(mpOutlineRec))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			nalloced += mpALLOC_UNIT;
		}

		orp = &OutRecs[OutRec_Count];

		orp->id[0] = id0;
		orp->cix[0] = cix0;
		orp->id[1] = id1;
		orp->cix[1] = cix1;
		orp->id[2] = id2;
		orp->type = (mpOutlineType) type;
		mpLowerCase(name);
		count = strlen(name) + 1;
		orp->name = (char *) malloc(count);
		bytes += count;
		strcpy(orp->name,name);
		if (orp->type != last_type) {
			Otype_Start[orp->type] = OutRec_Count;
			last_type = orp->type;
		}
		OutRec_Count++;
	}
	Otype_Start[last_type + 1] = OutRec_Count; 
	fclose(fp);
	NhlFree(full_name);
#if 0
	printf("records read %d, bytes %d + record size %d\n", OutRec_Count, 
	       bytes,OutRec_Count * sizeof(mpOutlineRec));

	mpGetOutRecsByType(OutRecs,OutRec_Count,mpInlandWater);
	mpGetOutRecsByName(OutRecs,OutRec_Count,"CANAD*");
#endif
	return NhlNOERROR;
}    

/*
 * Function:	MapPlotInitialize
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
MapPlotInitialize
#if     __STDC__
(
	NhlLayerClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlLayerClass   class;
        NhlLayer        req;
        NhlLayer        new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapPlotInitialize";
	char			*e_text;

	Mpl = (NhlMapPlotLayer) new;
	Mpp = &(Mpl->mapplot);
	
/* Initialize private fields */

	Mpp->overlay_object = NULL;
	Mpp->dash_table = NULL;
	Mpp->co_aws_id = -1;
	Mpp->po_aws_id = -1;
	Mpp->us_aws_id = -1;
	Mpp->predraw_dat = NULL;
	Mpp->draw_dat = NULL;
	Mpp->postdraw_dat = NULL;
	Mpp->new_draw_req = True;
	Mpp->update_req = False;
	Mpp->fill_rec_alloc = 0;
	Mpp->fill_rec_count = 0;
	Mpp->fill_recs = NULL;
	Mpp->outline_rec_alloc = 0;
	Mpp->outline_rec_count = 0;
	Mpp->outline_recs = NULL;
	Mpp->limb.on = Mpp->grid.on;
	Mpp->limb.order = Mpp->grid.order;
		
/*
 * If the Outline Records are not yet initialized, do it now.
 */
	if (OutRecs == NULL) {
		subret = Init_Outline_Recs(entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error initializing map outline records";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}

/* Set view dependent resources */

	subret = mpManageViewDepResources(Mpl,(NhlMapPlotLayer)req,True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

	subret = SetLineAttrs(Mpl,(NhlMapPlotLayer)req,True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage the dynamic arrays */

	subret = mpManageDynamicArrays(Mpl,(NhlMapPlotLayer)req,
				     True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Build the fill draw list */

	subret = mpBuildFillDrawList(Mpl,(NhlMapPlotLayer)req,
				     True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error building map draw list";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Build the outline draw list */

	subret = mpBuildOutlineDrawList(Mpl,(NhlMapPlotLayer)req,
				     True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error building map draw list";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set up the Map transformation */

	subret = mpSetUpTransObj(Mpl, (NhlMapPlotLayer) req, True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	subret = _NhlManageOverlay(&Mpp->overlay_object,new,req,
				   True,NULL,0,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

	return ret;
}

/*
 * Function:	MapPlotSetValues
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
static NhlErrorTypes MapPlotSetValues
#if  __STDC__
(
	NhlLayer	old,
	NhlLayer	reference,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapPlotSetValues";
	char			*e_text;
	int			view_args = 0;
        NhlSArg			sargs[16];
        int			nargs = 0;

	Mpl = (NhlMapPlotLayer) new;
	Mpp = &(Mpl->mapplot);
	Ompl = (NhlMapPlotLayer) old;
	Ompp = &(Ompl->mapplot);

	if (_NhlArgIsSet(args,num_args,NhlNvpXF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpYF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpWidthF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpHeightF)) view_args++;

	if (num_args > view_args)
		Mpp->new_draw_req = True;

	Mpp->limb.on = Mpp->grid.on;
	Mpp->limb.order = Mpp->grid.order;

/* Set view dependent resources */

	subret = mpManageViewDepResources(Mpl,Ompl,False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

	subret = SetLineAttrs(Mpl,Ompl,False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage the dynamic arrays */

	subret = mpManageDynamicArrays(Mpl,Ompl,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Build the map draw list */

	subret = mpBuildFillDrawList(Mpl,Ompl,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error building map draw list";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Build the map draw list */

	subret = mpBuildOutlineDrawList(Mpl,Ompl,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error building map draw list";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set up the Map transformation */

	subret = mpSetUpTransObj(Mpl,Ompl,False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Manage the overlay */

	/* 1 arg */
	if (Mpp->update_req) {
		NhlSetSArg(&sargs[(nargs)++],NhlNovUpdateReq,True);
	}
	
	subret = _NhlManageOverlay(&Mpp->overlay_object,new,old,
			       False,sargs,nargs,entry_name);
	ret = MIN(ret,subret);

	Mpp->update_req = False;

	return ret;
}


/*
 * Function:    MapPlotGetValues
 *
 * Description: Retrieves the current setting of one or more MapPlot resources.
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
 *		NhlNmpFillAreaSpecifiers
 *		NhlNmpMaskAreaSpecifiers
 *		NhlNmpOutlineSpecifiers
 *		NhlNmpAreaNames
 *		NhlNmpAreaTypes
 *		NhlNmpAreaGroups
 *		NhlNmpFillAreaColors
 *		NhlNmpFillGroupColors
 *		NhlNmpFillGroupPatterns
 *		NhlNmpFillGroupScales
 *
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    MapPlotGetValues
#if __STDC__
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlMapPlotLayer mp = (NhlMapPlotLayer) l;
        NhlMapPlotLayerPart *mpp = &(mp->mapplot);
        NhlGenArray ga;
        char *e_text;
        int i, count = 0;
        char *type = "";
	NhlBoolean create_it;

        for (i = 0; i < num_args; i++ ) {

		create_it = False;
                ga = NULL;
                if(args[i].quark == Qfill_area_specs) {
                        ga = mpp->fill_area_specs;
                        count = ga ? ga->num_elements : 0;
                        type = NhlNmpFillAreaSpecifiers;
                }
                else if (args[i].quark == Qmask_area_specs) {
                        ga = mpp->mask_area_specs;
                        count = ga ? ga->num_elements : 0;
                        type = NhlNmpMaskAreaSpecifiers;
                }
                else if (args[i].quark == Qoutline_specs) {
                        ga = mpp->outline_specs;
                        count = ga ? ga->num_elements : 0;
                        type = NhlNmpOutlineSpecifiers;
                }
                else if (args[i].quark == Qarea_names) {
			create_it = True;
                        ga = mpp->area_names;
                        count = ga ? ga->num_elements : 0;
                        type = NhlNmpAreaNames;
                }
                else if (args[i].quark == Qarea_types) {
			create_it = True;
                        ga = mpp->area_types;
                        count = ga ? ga->num_elements : 0;
                        type = NhlNmpAreaTypes;
                }
                else if (args[i].quark == Qarea_groups) {
			create_it = True;
                        ga = mpp->area_groups;
                        count = ga ? ga->num_elements : 0;
                        type = NhlNmpAreaGroups;
                }
                else if (args[i].quark == Qfill_area_colors) {
                        ga = mpp->fill_area_colors;
                        count = ga ? ga->num_elements : 0;
                        type = NhlNmpFillAreaColors;
                }
                else if (args[i].quark == Qfill_group_colors) {
                        ga = mpp->fill_group_colors;
                        count = mpp->mono_fill_group_color ? 
				1 : mpp->fill_group_count;
                        type = NhlNmpFillGroupColors;
                }
                else if (args[i].quark == Qfill_group_patterns) {
                        ga = mpp->fill_group_patterns;
                        count = mpp->mono_fill_group_pattern ? 
				1 : mpp->fill_group_count;
                        type = NhlNmpFillGroupPatterns;
                }
                else if (args[i].quark == Qfill_group_scales) {
                        ga = mpp->fill_group_scales;
                        count = mpp->mono_fill_group_scale ? 
				1 : mpp->fill_group_count;
                        type = NhlNmpFillGroupScales;
                }
                if (ga != NULL) {
                        if ((ga = mpGenArraySubsetCopy(ga, count)) == NULL) {
                                e_text = "%s: error copying %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          "MapPlotGetValues",type);
                                return NhlFATAL;
                        }
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
                }
		else if (create_it) {
			if ((ga = mpGetNewGenArray(mpp,args[i].quark,type)) 
			    == NULL) {
				e_text = "%s: error getting %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          "MapPlotGetValues",type);
                                return NhlFATAL;
                        }
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
        }

        return(NhlNOERROR);

}

/*
 * Function:	mpGetNewGenArray
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

static NhlGenArray mpGetNewGenArray
#if  __STDC__
(
	NhlMapPlotLayerPart	*mpp,
	NrmQuark		quark,
	NhlString		type
)
#else
(mpp,quark,type)
	NhlMapPlotLayerPart	*mpp;
	NrmQuark		quark;
	NhlString		type;
#endif
{
	char *e_text;
	int i, len;
	NhlGenArray ga;

	if (quark == Qarea_names) {
		NhlString	*sp;
		len = OutRec_Count;
		if ((sp = NhlMalloc(sizeof(NhlString)*len)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,"MapPlotGetValues");
			return NULL;
		}
		for (i = 0; i < len; i++) {
			if ((sp[i] = 
			     NhlMalloc(strlen(OutRecs[i].name) + 1)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,"MapPlotGetValues");
				return NULL;
			}
			strcpy(sp[i],OutRecs[i].name);
		}
		if ((ga = NhlCreateGenArray(sp,NhlTString,sizeof(NhlString),
					    1,&len)) == NULL) {
			e_text = "%s: error creating gen array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,"MapPlotGetValues");
			return NULL;
		}
		return ga;
	}
	else if (quark == Qarea_types) {
		int	*ip;
		len = OutRec_Count;
		if ((ip = NhlMalloc(sizeof(int)*len)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,"MapPlotGetValues");
			return NULL;
		}
		for (i = 0; i < len; i++) {
			ip[i] = (int) OutRecs[i].type;
		}
		if ((ga = NhlCreateGenArray(ip,NhlTInteger,sizeof(int),
					    1,&len)) == NULL) {
			e_text = "%s: error creating gen array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,"MapPlotGetValues");
			return NULL;
		}
		return ga;

	}
	else if (quark == Qarea_groups) {
		int	*ip;
		len = OutRec_Count;
		if ((ip = NhlMalloc(sizeof(int)*len)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,"MapPlotGetValues");
			return NULL;
		}
		for (i = 0; i < len; i++) {
			ip[i] = (int) OutRecs[i].cix[1];
		}
		if ((ga = NhlCreateGenArray(ip,NhlTInteger,sizeof(int),
					    1,&len)) == NULL) {
			e_text = "%s: error creating gen array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,"MapPlotGetValues");
			return NULL;
		}
		return ga;
	}
	return NULL;
}

/*
 * Function:  mpGenArraySubsetCopy
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

static NhlGenArray mpGenArraySubsetCopy
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
 * Function:	MapPlotDestroy
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
static NhlErrorTypes MapPlotDestroy
#if __STDC__
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlMapPlotLayerPart	*mpp = &(((NhlMapPlotLayer) inst)->mapplot);
	NhlTransformLayerPart	*mptp = &(((NhlTransformLayer) inst)->trans);
	NhlErrorTypes		ret = NhlNOERROR;

	if (mpp->overlay_object != NULL) {
		(void) _NhlDestroyChild(mpp->overlay_object->base.id,inst);
		mpp->overlay_object = NULL;
	}
	if (mptp->trans_obj != NULL) {
		(void) _NhlDestroyChild(mptp->trans_obj->base.id,inst);
		mptp->trans_obj = NULL;
	}
	NhlFreeGenArray(mpp->dash_table);
	NhlFreeGenArray(mpp->fill_area_specs);
	NhlFreeGenArray(mpp->mask_area_specs);
	NhlFreeGenArray(mpp->outline_specs);
	NhlFreeGenArray(mpp->fill_group_colors);
	NhlFreeGenArray(mpp->fill_group_patterns);
	NhlFreeGenArray(mpp->fill_group_scales);

	if (mpp->fill_recs != NULL)
		NhlFree(mpp->fill_recs);
	if (mpp->outline_recs != NULL)
		NhlFree(mpp->outline_recs);
	if (mpp->predraw_dat != NULL)
		_NhlDeleteViewSegment(inst,mpp->predraw_dat);
	if (mpp->draw_dat != NULL)
		_NhlDeleteViewSegment(inst,mpp->draw_dat);
	if (mpp->postdraw_dat != NULL)
		_NhlDeleteViewSegment(inst,mpp->postdraw_dat);

	return(ret);
}


/*
 * Function:	MapPlotPreDraw
 *
 * Description:	
 *
 * In Args:	layer	MapPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes MapPlotPreDraw
#if  __STDC__
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	Global_Amap_Inited = False;
	US_Amap_Inited = False;
	return mpDraw((NhlMapPlotLayer) layer,NhlPREDRAW);
}

/*
 * Function:	MapPlotDraw
 *
 * Description:	
 *
 * In Args:	layer	MapPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes MapPlotDraw
#if  __STDC__
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{

	return mpDraw((NhlMapPlotLayer) layer,NhlDRAW);
}

/*
 * Function:	MapPlotPostDraw
 *
 * Description:	
 *
 * In Args:	layer	MapPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes MapPlotPostDraw
#if  __STDC__
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes ret;
	NhlMapPlotLayerPart	*mpp = &(((NhlMapPlotLayer)layer)->mapplot);

	ret = mpDraw((NhlMapPlotLayer) layer,NhlPOSTDRAW);
	mpp->new_draw_req = False;
	return ret;
}

/*
 * Function:	mpDraw
 *
 * Description:	
 *
 * In Args:	layer	MapPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpDraw
#if  __STDC__
(
	NhlMapPlotLayer	mp,
	NhlDrawOrder	order
)
#else
(mp,order)
        NhlMapPlotLayer mp;
	NhlDrawOrder	order;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlMapPlotLayerPart	*mpp = &(mp->mapplot);
	NhlTransformLayerPart	*tfp = &(mp->trans);
	NhlBoolean		do_labels = False,
				do_perim = False;
	NhlTransDat		*seg_dat;

	if (mpp->fill_order != order &&
	    mpp->outline_order != order &&
	    mpp->grid.order != order &&
	    mpp->perim.order != order &&
	    mpp->labels.order != order)
		return NhlNOERROR;
/* 
 * set the static MapPlot layer part pointer for the benefit of the
 * mapusr_ function. Then set the correct entry name.
 */
	Mpp = mpp;
	Mpl = mp;

	switch (order) {
	case NhlPREDRAW:
		entry_name = "MapPlotPreDraw";
		seg_dat = mpp->predraw_dat;
		break;
	case NhlDRAW:
		entry_name = "MapPlotDraw";
		seg_dat = mpp->draw_dat;
		break;
	case NhlPOSTDRAW:
		entry_name = "MapPlotPostDraw";
		seg_dat = mpp->postdraw_dat;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}

	if (mp->view.use_segments && ! mpp->new_draw_req) {
                subret = _NhlActivateWorkstation(mp->base.wkptr);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDrawSegment(seg_dat,
				_NhlWorkstationId(mp->base.wkptr));
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDeactivateWorkstation(mp->base.wkptr);
		return MIN(subret,ret);
	}

	subret = _NhlActivateWorkstation(mp->base.wkptr);

	if (mp->view.use_segments) {
		if (seg_dat != NULL)
			_NhlDeleteViewSegment((NhlLayer)mp,seg_dat);
		if ((seg_dat = _NhlNewViewSegment((NhlLayer)mp)) == NULL) {
			e_text = "%s: error opening segment";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
		_NhlStartSegment(seg_dat);
		switch (order) {
		case NhlPREDRAW:
			mpp->predraw_dat = seg_dat;
			break;
		case NhlDRAW:
			mpp->draw_dat = seg_dat;
			break;
		case NhlPOSTDRAW:
			mpp->postdraw_dat = seg_dat;
			break;
		}
	}

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	subret = _NhlSetTrans((NhlLayer)tfp->trans_obj,(NhlLayer)mp);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error setting transformation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

/* Do the fill first */

	if (mpp->fill_on && mpp->fill_order == order) {
		Draw_Op = mpDRAWFILL;
		subret = mpFill(mp,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
        }

/* Draw the grid and the limb line */

	if (mpp->grid.on && mpp->grid.order == order) {
		subret = mpGrid(mp,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
	}

/* Set up the map outlines */

	if (mpp->outline_on && mpp->outline_order == order) {
		Draw_Op = mpDRAWOUTLINE;
		subret = mpOutline(mp,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
        }

/* Draw labels and/or the perimeter */

	if (mpp->labels.on && mpp->labels.order == order) {
 		c_mpseti("C3",mpp->labels.gks_color);
		c_mpseti("LA",1);
		do_labels = True;
	}
	else {
		c_mpseti("LA",0);
	}
	
	if (mpp->perim.on && mpp->perim.order == order) {
		c_mpseti("C1",mpp->perim.gks_color);
		c_mpseti("PE",1);
		do_perim = True;
	}
	else {
		c_mpseti("PE",0);
	}
	if (do_labels || do_perim)  {
		c_mpsetc("OU","NO");
		c_maplbl();
	}

	if (mp->view.use_segments) {
		_NhlEndSegment();
	}
	subret = _NhlDeactivateWorkstation(mp->base.wkptr);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error setting deactivating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	return ret;
}



/*
 * Function:	mpOutline
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

static NhlErrorTypes mpOutline
#if  __STDC__
(
	NhlMapPlotLayer	mp,
	NhlString	entry_name
)
#else
(mp,entry_name)
        NhlMapPlotLayer mp;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mp->mapplot);

	c_mpseti("C5",mpp->geophysical.gks_color);
	c_mpseti("C6",mpp->us_state.gks_color);
	c_mpseti("C7",mpp->national.gks_color);

	if (mpp->global_outline_mode != mpNONE) {
		subret = mpSetUpDrawIds(mpp,mpDRAWOUTLINE,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
#if 0
		switch (Outline_Set) {
		case mpCO:
			printf("using CO for outlines\n");
			break;
		case mpPO:
			printf("using PO for outlines\n");
			break;
		case mpPS:
			printf("using PS for outlines\n");
			break;
		default:
			printf("outline set not correct\n");
		}
#endif
		c_maplot();
	}

        if (mpp->usstates_outline_mode != mpNOSET) {
		subret = mpSetUpStateDrawIds(mpp,mpDRAWOUTLINE,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
#if 0
		printf("using US for outlines\n");
#endif
		c_maplot();
	}

	return ret;
}


/*
 * Function:	mpFill
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

static NhlErrorTypes mpFill
#if  __STDC__
(
	NhlMapPlotLayer	mp,
	NhlString	entry_name
)
#else
(mp,entry_name)
        NhlMapPlotLayer mp;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mp->mapplot);
        NhlWorkspace		*aws = NULL;

/*
 * for efficiency if the ustates are drawn they go into a separate 
 * area map. Eventually there may be a number of sub-area maps.
 */
#if 0
        float flx,frx,fby,fuy,wlx,wrx,wby,wuy; int ll;
        c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
        printf("getset - %f,%f,%f,%f,%f,%f,%f,%f\n",
               flx,frx,fby,fuy,wlx,wrx,wby,wuy); 
#endif

	if (mpp->global_fill_mode != mpNONE) {

		subret = mpSetUpAreamap(mp,&aws,mpGLOBAL_AMAP,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
#if 0
		switch (Outline_Set) {
		case mpCO:
			printf("using CO for fill\n");
			break;
		case mpPO:
			printf("using PO for fill\n");
			break;
		case mpPS:
			printf("using PS for fill\n");
			break;
		default:
			printf("outline set not correct\n");
		}
#endif
		subret = _NhlArpram(aws,0,0,0,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		subret = _NhlArscam(aws,(_NHLCALLF(nhlezmapfill,NHLEZMAPFILL)),
				    entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		subret = _NhlIdleWorkspace(aws);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
	}
        if (mpp->usstates_fill_mode != mpNOSET) {

		subret = mpSetUpAreamap(mp,&aws,mpUSSTATES_AMAP,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
#if 0
		printf("using US for fill\n");
#endif
		subret = _NhlArpram(aws,0,0,0,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		subret = _NhlArscam(aws,(_NHLCALLF(nhlezmapfill,NHLEZMAPFILL)),
				    entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		subret = _NhlIdleWorkspace(aws);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
        }
	return ret;
}

/*
 * Function:	mpGrid
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

static NhlErrorTypes mpGrid
#if  __STDC__
(
	NhlMapPlotLayer	mp,
	NhlString	entry_name
)
#else
(mp,entry_name)
        NhlMapPlotLayer mp;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mp->mapplot);
	NhlWorkspace		*aws;
        float flx,frx,fby,fuy,wlx,wrx,wby,wuy,lon1,lon2,lat1,lat2,spacing;
	float avlat,avlon;
	int ll,status;

	c_mpseti("C2",mpp->grid.gks_color);
	c_mpseti("C4", mpp->limb.gks_color);

	if (! mpp->relative_grid_spacing) {
		spacing = mpp->grid_spacing;
	}
	else {
		c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
		avlat = (wby + wuy) / 2.0;
		avlon = (wrx + wlx) / 2.0;
		_NhlWinToData(mp->trans.trans_obj,(NhlLayer)mp,&avlon,&avlat,
			      1,&lon1,&lat1,&status,NULL,NULL);
		_NhlWinToData(mp->trans.trans_obj,(NhlLayer)mp,&wrx,&avlat,
			      1,&lon2,&lat2,&status,NULL,NULL);
		
		spacing = 2.0 * (lon2 - lon1) * mpp->grid_spacing;
		spacing = spacing < 0 ? -spacing : spacing;
	}
	c_mpsetr("GR",spacing);

	if (mpp->grid_mask_mode == NhlmpMASKNONE) {
		c_mapgrd();
		return ret;
	}
	if (mpp->global_fill_mode != mpNONE) {
		
		subret = mpSetUpAreamap(mp,&aws,mpGLOBAL_AMAP,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
#if 0
		switch (Outline_Set) {
		case mpCO:
			printf("using CO for grid\n");
			break;
		case mpPO:
			printf("using PO for grid\n");
			break;
		case mpPS:
			printf("using PS for grid\n");
			break;
		default:
			printf("outline set not correct\n");
		}
#endif
		subret = _NhlMapgrm(aws,
				    (_NHLCALLF(nhlmaskgrid,NHLMASKGRID)),
				    entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		subret = _NhlIdleWorkspace(aws);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
	}
	if (mpp->usstates_fill_mode != mpNOSET) {

		subret = mpSetUpAreamap(mp,&aws,mpUSSTATES_AMAP,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
#if 0
		printf("using US for grid\n");
#endif
		subret = _NhlMapgrm(aws,
				    (_NHLCALLF(nhlmaskgrid,NHLMASKGRID)),
				    entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		subret = _NhlIdleWorkspace(aws);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
	}
	return ret;
}


/*
 * Function:	mpSetUpAreamap
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

static NhlErrorTypes mpSetUpAreamap
#if  __STDC__
(
	NhlMapPlotLayer	mp,
	NhlWorkspace	**aws,
	int 		amap_type,
	NhlString	entry_name
)
#else
(mp,aws,amap_type,entry_name)
        NhlMapPlotLayer mp;
	NhlWorkspace	**aws;
	int		amap_type;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlMapPlotLayerPart	*mpp = &(mp->mapplot);
	int			aws_id;
	NhlBoolean		inited;

	switch (amap_type) {
	case mpGLOBAL_AMAP:
		aws_id = mpp->co_aws_id;
		inited = Global_Amap_Inited;
		break;
	case mpUSSTATES_AMAP:
		aws_id = mpp->us_aws_id;
		inited = US_Amap_Inited;
		break;
	}

	if (aws_id < 0) {
		aws_id = _NhlNewWorkspace(NhlwsAREAMAP,NhlwsDISK,
					  200000*sizeof(int));
		if (aws_id < 0) 
			return MIN(ret,aws_id);
	}
	if ((*aws = _NhlUseWorkspace(aws_id)) == NULL) {
		e_text = "%s: error reserving area map workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	switch (amap_type) {
	case mpGLOBAL_AMAP:
		subret = mpSetUpDrawIds(mpp,mpDRAWFILL,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
		break;
	case mpUSSTATES_AMAP:
		subret = mpSetUpStateDrawIds(mpp,mpDRAWFILL,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
		break;
	}
	if (! inited) {
		c_mpseti("VS",0);
		subret = _NhlArinam(*aws,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		subret = _NhlMapbla(*aws,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
	}
	switch (amap_type) {
	case mpGLOBAL_AMAP:
		mpp->co_aws_id = aws_id;
		Global_Amap_Inited = True;
		break;
	case mpUSSTATES_AMAP:
		mpp->us_aws_id = aws_id;
		US_Amap_Inited = True;
		break;
	}
	return ret;
}

/*
 * Function:	mpSetUpDrawIds
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

static NhlErrorTypes mpSetUpDrawIds
#if  __STDC__
(
	NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	NhlString		entry_name
)
#else
(mpp,draw_op,entry_name)
	NhlMapPlotLayerPart	*mpp;
	mpDrawOp		draw_op;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;
	int		id_ix,cix_ix,start_ix,end_ix,ix,i,j;
	mpNameRec	*groups;
	mpNameRec	*nrecs;
	int		count;
	unsigned char	draw_mode;
	mpGlobalSetMode gmode;
	mpStateSetMode	smode;
	int		color;
	int		*fgp;
	int		*agroups = NULL;
	NhlBoolean	use_agroups = False;
	int		us_ix = -999;

	US_Border = 0;
	US_Color = -999;

	switch (draw_op) {
	case mpDRAWFILL:
		groups = mpp->fill_groups;
		nrecs = mpp->fill_recs;
		count = mpp->fill_rec_count;
		gmode = mpp->global_fill_mode;
		smode = mpp->usstates_fill_mode;
		break;
	case mpDRAWOUTLINE:
		groups = mpp->outline_groups;
		nrecs = mpp->outline_recs;
		count = mpp->outline_rec_count;
		gmode = mpp->global_outline_mode;
		smode = mpp->usstates_outline_mode;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	switch (gmode) {
	case mpGEO:
		c_mpsetc("OU","CO");
		Outline_Set = mpCO;
		id_ix = 0;
		cix_ix = 0;
		break;
	case mpNAT:
		if (smode > mpNOSET) {
			c_mpsetc("OU","PS");
			Outline_Set = mpPS;
			id_ix = 2;
		}
		else {
			c_mpsetc("OU","PO");
			Outline_Set = mpPO;
			id_ix = 1;
		}
		cix_ix = 1;
		if (mpp->area_groups != NULL) {
			agroups = (int *) mpp->area_groups->data;
			use_agroups = True;
		}
		break;
	case mpIMPLIED_NAT:
		if (smode > mpNOSET) {
			c_mpsetc("OU","PS");
			Outline_Set = mpPS;
			id_ix = 2;
		}
		else {
			c_mpsetc("OU","PO");
			Outline_Set = mpPO;
			id_ix = 1;
		}
		cix_ix = 0;
		break;
	default:
		return ret;
	}
	start_ix = Id_Offset[Outline_Set];
	end_ix = Id_End_Reg[Outline_Set];
	Draw_Check = Draw_Special_Check[Outline_Set];

	fgp = (int *) mpp->fill_group_colors->data;
	for (i = 0; i < NhlNumber(DrawIds); i++) {
		DrawIds[i].draw_mode = mpBACKGROUND;
		DrawIds[i].cix = fgp[NhlmpDEFAULTGROUPINDEX];
	}

/*
 * Set Up the Draw List for the map boundary catagories. The specification
 * lists are parsed later and thus overrride the general catagories.
 */
	for (i = 0; i < NhlmpOUTLINE_TYPE_COUNT - 2; i++) {
		if (groups[i].draw_mode > mpBACKGROUND) {
			draw_mode = groups[i].draw_mode;
			for (j = Otype_Start[i]; j < Otype_Start[i+1]; j++) {
				ix = OutRecs[j].id[id_ix];
				if (groups[i].cix != NhlmpUNSETCOLOR)
					color = groups[i].cix;
				else
					color = use_agroups ?
						fgp[agroups[i]] :
						fgp[OutRecs[j].cix[cix_ix]];
				if (ix > end_ix) {
					if (draw_op == mpDRAWOUTLINE &&
					    draw_mode == mpDRAW)
						draw_mode = mpDRAWSPECIAL;
					subret = mpExpandId(mpp,
						      ix-end_ix-1,draw_mode,
						       color,entry_name);
					if ((ret = MIN(ret,subret) 
					     < NhlWARNING)) return ret;
				}
				else {
					ix -= start_ix;
					DrawIds[ix].draw_mode = draw_mode;
					DrawIds[ix].cix = (unsigned char)color;
				}
			}
		}
	}
/*
 * Parse the spec lists twice. The first time only set the composite ids.
 * Thus in the second parse the specific ids will override the composites.
 * The US outline needs special treatment, since its border is drawn using
 * the PS outline while the interior state borders are drawn using the US
 * outline set. Also its setting must override the N+S-America setting, but
 * in turn individual state settings must override it. Therefore set it
 * in between the two loops if it appears.
 */
	for (i = 0; i < count; i++) {
		j = nrecs[i].name_ix;
		ix = OutRecs[j].id[id_ix];

		if (ix <= end_ix)
			continue;
		else if (ix == mpUS_PS_IX && Outline_Set == mpPS) {
			us_ix = i;
			continue;
		}
		draw_mode = nrecs[i].draw_mode;
		color = nrecs[i].cix;
		if (draw_op == mpDRAWOUTLINE && draw_mode == mpDRAW)
			draw_mode = mpDRAWSPECIAL;
		subret = mpExpandId(mpp,
				    ix-end_ix-1,draw_mode,color,entry_name);
		if ((ret = MIN(ret,subret) < NhlWARNING)) return ret;
	}
	if (us_ix >= 0) {
		j = nrecs[us_ix].name_ix;
		ix = OutRecs[j].id[id_ix];
		draw_mode = nrecs[us_ix].draw_mode;
		color = nrecs[us_ix].cix;
		if (draw_mode == mpMASK) {
			subret = mpExpandId(mpp,ix-end_ix-1,
					    draw_mode,color,entry_name);
			if ((ret = MIN(ret,subret) < NhlWARNING)) return ret;
			groups[mpUSStateLand].draw_mode = draw_mode;
		}
		else {
			US_Border = 2;
			US_Color = color;
		}
	}

	for (i = 0; i < count; i++) {
		j = nrecs[i].name_ix;
		ix = OutRecs[j].id[id_ix];
		color = nrecs[i].cix;
		draw_mode = nrecs[i].draw_mode;
		if (OutRecs[j].type > mpNational && draw_mode != mpMASK)
			continue;
		ix -= start_ix;
		DrawIds[ix].draw_mode = draw_mode;
		DrawIds[ix].cix = (unsigned char) color;
	}
/*
 * If the US set is in implied set mode, set the draw mode for the groups
 * to the color that has been set up for the US border. Is the DrawId for
 * this index always set????
 */
	if (smode == mpIMPLIED_SET) {
		us_ix = (Outline_Set == mpPO ?
			 mpUS_PO_IX : mpUS_PS_IX) - start_ix;
		draw_mode = DrawIds[us_ix].draw_mode;
		if (draw_op == mpDRAWOUTLINE && draw_mode == mpDRAW)
			draw_mode = mpDRAWSPECIAL;
		groups[mpUSStateLand].draw_mode = draw_mode;
	}

	return ret;
}

/*
 * Function:	mpSetUpStateDrawIds
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

static NhlErrorTypes mpSetUpStateDrawIds
#if  __STDC__
(
	NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	NhlString		entry_name
)
#else
(mpp,draw_op,entry_name)
	NhlMapPlotLayerPart	*mpp;
	mpDrawOp		draw_op;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	int		offset,ix,i,j;
	mpNameRec	*land_group,*water_group;
	mpNameRec	*nrecs;
	int		count;
	unsigned char	draw_mode;
	mpGlobalSetMode gmode;
	mpStateSetMode	smode;
	int		*fgp;
	int		*agroups = NULL;
	NhlBoolean	use_agroups = False;

	switch (draw_op) {
	case mpDRAWFILL:
		land_group = &mpp->fill_groups[mpUSStateLand];
		water_group = &mpp->fill_groups[mpUSStateWater];
		nrecs = mpp->fill_recs;
		count = mpp->fill_rec_count;
		gmode = mpp->global_fill_mode;
		smode = mpp->usstates_fill_mode;
		break;
	case mpDRAWOUTLINE:
		land_group = &mpp->outline_groups[mpUSStateLand];
		water_group = &mpp->outline_groups[mpUSStateWater];
		nrecs = mpp->outline_recs;
		count = mpp->outline_rec_count;
		gmode = mpp->global_outline_mode;
		smode = mpp->usstates_outline_mode;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	c_mpsetc("OU","US");
	Outline_Set = mpUS;
	offset = Id_Offset[2];
	if (gmode == mpNONE || US_Border == 2)
		Draw_Check = Draw_Special_Check[Outline_Set];
	else
		Draw_Check = -999;

	fgp = (int *) mpp->fill_group_colors->data;
	for (i = 0; i < NhlNumber(DrawIds); i++) {
		DrawIds[i].draw_mode = (unsigned char) mpBACKGROUND;
		DrawIds[i].cix = (unsigned char) fgp[NhlmpDEFAULTGROUPINDEX];
	}
	if (mpp->area_groups != NULL) {
		agroups = (int *) mpp->area_groups->data;
		use_agroups = True;
	}

	if (land_group->draw_mode > mpBACKGROUND) {
		draw_mode = land_group->draw_mode;
		for (j = Otype_Start[mpUSStateLand]; 
		     j < Otype_Start[mpUSStateLand + 1]; j++) {
			ix = OutRecs[j].id[1] - offset;
			DrawIds[ix].draw_mode = draw_mode;
			if (smode == mpIMPLIED_SET && US_Color > 0)
				DrawIds[ix].cix = (unsigned char) US_Color;
			else if (land_group->cix != NhlmpUNSETCOLOR)
				DrawIds[ix].cix = 
					(unsigned char) land_group->cix;
			else
				DrawIds[ix].cix = (unsigned char) use_agroups ?
					fgp[agroups[j]] :
					fgp[OutRecs[j].cix[1]];
			}
	}
	if (water_group->draw_mode > mpBACKGROUND) {
		draw_mode = water_group->draw_mode;
		for (j = Otype_Start[mpUSStateWater]; 
		     j < Otype_Start[mpUSStateWater + 1]; j++) {
			ix = OutRecs[j].id[1] - offset;
			DrawIds[ix].draw_mode = draw_mode;
			if (water_group->cix != NhlmpUNSETCOLOR) 
				DrawIds[ix].cix = 
					(unsigned char) water_group->cix;
			else
				DrawIds[ix].cix = (unsigned char) use_agroups ?
					fgp[agroups[j]] :
					fgp[OutRecs[j].cix[1]];
			}
	}

	for (i = 0; i < count; i++) {
		j = nrecs[i].name_ix;
		draw_mode = nrecs[i].draw_mode;
		if (OutRecs[j].type < mpUSStateLand)
			continue;
		ix = OutRecs[j].id[1] - offset;
		DrawIds[ix].draw_mode = draw_mode;
		DrawIds[ix].cix = (unsigned char) nrecs[i].cix;
	}

	if (draw_op != mpDRAWOUTLINE) {
		for (i = 0; i < NhlNumber(USState_Excludes); i++) {
			DrawIds[USState_Excludes[i]-offset].draw_mode = 
				mpSYSEXCLUDE;
		}
	}

	return ret;
}

/*
 * Function:	mpExpandId
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

static NhlErrorTypes mpExpandId
#if  __STDC__
(
	NhlMapPlotLayerPart	*mpp,
	int			id,
	int			draw_mode,
	int			color,	
 	NhlString		entry_name
)
#else
(mpp,id,draw_mode,color,entry_name)
	NhlMapPlotLayerPart	*mpp;
	int			id;
	int			draw_mode;
	int			color;
	NhlString		entry_name;
#endif
{
	char 	*e_text;
	short	*idp;
	int	count,ix,i,set;
	NhlBoolean do_us = False;
	int	*fgp;
	int		*agroups = NULL;
	NhlBoolean	use_agroups = False;

	fgp = (int *) mpp->fill_group_colors->data;
	count = NhlNumber(Exp_Ids);
	if (id >= count) {
		e_text = "%s: invalid expansion id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	set = Outline_Set == mpPO ? 0 : 1;
	idp = Exp_Ids[id][set];
	count = Exp_Ids_Count[id][set];

	if (id == 11 && set == 1) {	/* US outline */
		US_Border = 2;
		if (mpp->area_groups != NULL) {
			agroups = (int *) mpp->area_groups->data;
			use_agroups = True;
		}
		US_Color = color;
		do_us = True;
	}
	else if (id == 0 && set == 1)  { /* North-and-South-America */
		US_Border = 1;
		US_Color = color;
		do_us = True;
	}
	else if (idp == NULL) {
		e_text = "%s: invalid expansion id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	if (idp != NULL) {
		for (i = 0; i < count; i++) {
			ix = idp[i] - Id_Offset[Outline_Set];
			DrawIds[ix].draw_mode = draw_mode;
			DrawIds[ix].cix = (unsigned char) color;
		}
	}
	if (do_us) {
		for (i = Otype_Start[mpUSStateLand]; 
		     i < Otype_Start[mpUSStateLand + 1]; i++) {
			ix = OutRecs[i].id[2] - Id_Offset[Outline_Set];
			DrawIds[ix].draw_mode = draw_mode;
			DrawIds[ix].cix = (unsigned char)color;
		}
		for (i = Otype_Start[mpUSStateWater]; 
		     i < Otype_Start[mpUSStateWater + 1]; i++) {
			ix = OutRecs[i].id[2] - Id_Offset[Outline_Set];
			DrawIds[ix].draw_mode = draw_mode;
			DrawIds[ix].cix = (unsigned char) use_agroups ?
				fgp[agroups[i]] :
				fgp[OutRecs[i].cix[US_Border-1]];
		}
	}
	return NhlNOERROR;
}

/*
 * Function:  mpAdjustText
 *
 * Description: Adjusts the text height and aspect ratio
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
static NhlErrorTypes    mpAdjustText
#if __STDC__
(
	NhlmpLabelAttrs *lbl_attrs,
	NhlMapPlotLayer	new, 
	NhlMapPlotLayer	old,
	NhlBoolean	init
)
#else
(lbl_attrs,new,old,init)
	NhlmpLabelAttrs *lbl_attrs;
	NhlLayer	new;
	NhlLayer	old;
	NhlBoolean	init;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char 			*e_text;
	char			*entry_name;
	NhlMapPlotLayer		mpnew = (NhlMapPlotLayer) new;
	NhlMapPlotLayer		mpold = (NhlMapPlotLayer) old;

	entry_name = (init) ? "MapPlotInitialize" : "MapPlotSetValues";

/* 
 * Adjust text height. Then determine principal width and height
 * and the "real " text height based on aspect ratio. 21.0 is the default 
 * principle height. This code handles aspect ratio like the TextItem.
 */

	if (! lbl_attrs->height_set) {
		if (init) {
			lbl_attrs->height = Nhl_mpDEF_LABEL_HEIGHT *
				mpnew->view.width / Nhl_mpSTD_VIEW_WIDTH;
		}
		else if (mpnew->view.width != mpold->view.width) {
			lbl_attrs->height *= 
				mpnew->view.width / mpold->view.width;
		}
	}

        if (lbl_attrs->aspect <= 0.0 ) {
		e_text = "%s: Invalid value for text aspect ratio %d";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,lbl_attrs->aspect);
                ret = NhlWARNING;
                lbl_attrs->aspect = 1.3125;
        }
        if (lbl_attrs->aspect <= 1.0) {
                lbl_attrs->pheight = 21.0 * lbl_attrs->aspect;
                lbl_attrs->pwidth = 21.0;
        } else {
                lbl_attrs->pwidth = 21.0 * 1.0/lbl_attrs->aspect;
                lbl_attrs->pheight = 21.0;
        }
        lbl_attrs->real_height = 
		1.0 / lbl_attrs->aspect * lbl_attrs->height;

	return ret;
}

/*
 * Function:  mpAdjustDashSegLen
 *
 * Description: Adjust the dash segment length
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
static void mpAdjustDashSegLen
#if __STDC__
(
	float		*seglen,
	NhlBoolean	init,
	float		new_vpwidth,
	float		old_vpwidth
)
#else
(seglen,init,new_vpwidth,old_vpwidth)
	float		*seglen;
	NhlBoolean	init;
	float		new_vpwidth;
	float		old_vpwidth;
#endif
{
	
	if (init)
		*seglen = Nhl_mpDEF_DASH_SEGLEN * 
			new_vpwidth / Nhl_mpSTD_VIEW_WIDTH;
	else if (new_vpwidth != old_vpwidth)
		*seglen *= new_vpwidth / old_vpwidth;
	return;
}

/*
 * Function:  mpManageViewDepResources
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
static NhlErrorTypes    mpManageViewDepResources
#if __STDC__
(
	NhlMapPlotLayer mpnew, 
	NhlMapPlotLayer mpold,
	NhlBoolean	init
)
#else
(mpnew,mpold,init)
	NhlMapPlotLayer mpnew; 
	NhlMapPlotLayer mpold;
	NhlBoolean	init;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpnew->mapplot);

/* Adjust line dash segment lengths */

	if (! mpp->geophysical.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->geophysical.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->us_state.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->us_state.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->national.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->national.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->grid.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->grid.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->limb.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->limb.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);
	if (! mpp->perim.dash_seglen_set)
		mpAdjustDashSegLen(&mpp->perim.dash_seglen,init,
				 mpnew->view.width,mpold->view.width);

	subret = mpAdjustText(&mpp->labels,mpnew,mpold,init);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	mpp->geophysical.dash_seglen_set = False;
	mpp->us_state.dash_seglen_set = False;
	mpp->national.dash_seglen_set = False;
	mpp->grid.dash_seglen_set = False;
	mpp->limb.dash_seglen_set = False;
	mpp->perim.dash_seglen_set = False;
	mpp->labels.height_set = False;

	return ret;
}


/*
 * Function:  SetLineAttrs
 *
 * Description: Sets line resources (mainly gks colors right now)
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
static NhlErrorTypes    SetLineAttrs
#if __STDC__
(
	NhlMapPlotLayer mpnew, 
	NhlMapPlotLayer mpold,
	NhlBoolean	init
)
#else
(mpnew,mpold,init)
	NhlMapPlotLayer mpnew; 
	NhlMapPlotLayer mpold;
	NhlBoolean	init;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpnew->mapplot);

	mpp->grid.gks_color = _NhlGetGksCi(mpnew->base.wkptr,mpp->grid.color);
	mpp->limb.gks_color = _NhlGetGksCi(mpnew->base.wkptr,mpp->limb.color);
	mpp->geophysical.gks_color = 
		_NhlGetGksCi(mpnew->base.wkptr,mpp->geophysical.color);
	mpp->national.gks_color = 
		_NhlGetGksCi(mpnew->base.wkptr,mpp->national.color);
	mpp->us_state.gks_color = 
		_NhlGetGksCi(mpnew->base.wkptr,mpp->us_state.color);
	mpp->perim.gks_color = 
		_NhlGetGksCi(mpnew->base.wkptr,mpp->perim.color);
	mpp->labels.gks_color = 
		_NhlGetGksCi(mpnew->base.wkptr,mpp->labels.color);

	return ret;
}

/*
 * Function:  mpManageDynamicArrays
 *
 * Description: Creates and manages internal copies of each of the 
 *	MapPlot GenArrays. Populates the copies with the values specified 
 *	via MapPlotCreate or MapPlotSetValues calls. Assigns default 
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
static NhlErrorTypes    mpManageDynamicArrays
#if __STDC__
(
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args)
#else
(mpnew,mpold,init,args,num_args)
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlMapPlotLayerPart *mpp = &(mpnew->mapplot);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *entry_name;
	char *e_text;
	NhlGenArray ga;
	NhlMapPlotLayerPart *ompp = &(mpold->mapplot);
	int i,count;
	int *ip;
	float *fp;
	float fval;
	int	init_count;
	NhlBoolean need_check,changed;
	int old_count;
	int cmap_len = 0;
	NhlBoolean use_default;

	entry_name =  init ? "MapPlotInitialize" : "MapPlotSetValues";

/* 
 * need a copy of the dash table
 */	
	if (init) {
		subret = NhlVAGetValues(mpnew->base.wkptr->base.id,
					NhlNwkDashTable, &ga, NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: NhlFATAL error retrieving dash table";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		mpp->dash_table = ga;
	}
/*
 * The fill group count has minimum and maximum values
 */

	if (mpp->fill_group_count > Nhl_mpMAX_FILL_GROUPS) {
		e_text = "%s: %s exceeds maximum value, %d: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNmpFillGroupCount,Nhl_mpMAX_FILL_GROUPS);
		ret = MIN(ret, NhlWARNING);
		mpp->fill_group_count = Nhl_mpMAX_FILL_GROUPS;
	}
	else if (mpp->fill_group_count < Nhl_mpMIN_FILL_GROUPS) {
		e_text = "%s: %s less than minimum value, %d: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNmpFillGroupCount,Nhl_mpMIN_FILL_GROUPS);
		ret = MIN(ret, NhlWARNING);
		mpp->fill_group_count = Nhl_mpMIN_FILL_GROUPS;
	}
/*
 * Fill group color and fill group patterns cannot both be set to mono
 */ 
	if (mpp->mono_fill_group_color && mpp->mono_fill_group_pattern) {
		e_text = 
			"%s: %s and %s must both be True; setting %s False";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNmpMonoFillGroupColor,NhlNmpMonoFillGroupPattern,
			  NhlNmpMonoFillGroupPattern);
		ret = MIN(ret, NhlWARNING);
		mpp->mono_fill_group_pattern = False;
	}
/*
 * Fill group colors
 */

	if (init) {
		ga = NULL;
		count = mpp->fill_group_count;
	}
	else {
		ga = ompp->fill_group_colors;
		count = mpp->mono_fill_group_color ? 1 : mpp->fill_group_count;
	}
  	subret = mpManageGenArray(&ga,count,mpp->fill_group_colors,Qint,NULL,
				  &old_count,&init_count,&need_check,&changed,
				  NhlNmpFillGroupColors,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ompp->fill_group_colors = changed ? NULL : mpp->fill_group_colors;
	mpp->fill_group_colors = ga;

	ip = (int *) ga->data;
	NhlVAGetValues(mpnew->base.wkptr->base.id,
		       NhlNwkColorMapLen,&cmap_len,NULL);
	if (need_check) {
		for (i=init_count; i < count; i++) {
			if (i < NhlNumber(Init_Colors))
				ip[i] = Init_Colors[i];
			else if (ip[i] < cmap_len)
				ip[i] = i;
			else 
				ip[i] = NhlFOREGROUND;
		}
	}
	
/*
 * If any of the individual convenience fill color resources are set --
 * override the corresponding array resource.
 */
	if ((init && mpp->fill_default.color != NhlmpUNSETCOLOR) ||
	    mpp->fill_default.color != ompp->fill_default.color) {
		if (mpp->fill_default.color > NhlTRANSPARENT &&
		    mpp->fill_default.color < cmap_len)
			ip[NhlmpDEFAULTGROUPINDEX] = mpp->fill_default.color;
	}
	if ((init && mpp->ocean.color != NhlmpUNSETCOLOR) ||
	    mpp->ocean.color != ompp->ocean.color) {
		if (mpp->ocean.color > NhlTRANSPARENT &&
		    mpp->ocean.color < cmap_len)
			ip[NhlmpOCEANGROUPINDEX] = mpp->ocean.color;
	}
	if ((init && mpp->land.color != NhlmpUNSETCOLOR) ||
	    mpp->land.color != ompp->land.color) {
		if (mpp->land.color > NhlTRANSPARENT &&
		    mpp->land.color < cmap_len)
			ip[NhlmpLANDGROUPINDEX] = mpp->land.color;
	}
	if ((init && mpp->inland_water.color != NhlmpUNSETCOLOR) ||
	    mpp->inland_water.color != ompp->inland_water.color) {
		if (mpp->inland_water.color > NhlTRANSPARENT &&
		    mpp->inland_water.color < cmap_len)
			ip[NhlmpINLANDWATERGROUPINDEX] =
				mpp->inland_water.color;
	}
/*
 * Fill patterns
 */
	if (init) {
		ga = NULL;
		count = mpp->fill_group_count;
	}
	else {
		ga = ompp->fill_group_patterns;
		count = mpp->mono_fill_group_pattern ? 
			1 : mpp->fill_group_count;
	}
	subret = mpManageGenArray(&ga,count,mpp->fill_group_patterns,Qint,NULL,
				  &old_count,&init_count,&need_check,&changed,
				  NhlNmpFillGroupPatterns,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ompp->fill_group_patterns = changed ? NULL : mpp->fill_group_patterns;
	mpp->fill_group_patterns = ga;

	if (need_check) {
		ip = (int *) ga->data;
		if (mpp->mono_fill_group_pattern && init_count == 0)
			ip[0] = NhlSOLIDFILL;
		else
			for (i=init_count; i < count; i++) {
				ip[i] = i + 1;
			}
		for (i=0; i<init_count; i++) {
			if (ip[i] < NhlHOLLOWFILL) {
				e_text =
	      "%s: %s index %d holds an invalid pattern value, %d: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,
					  NhlNmpFillGroupPatterns,i,ip[i]);
				ret = MIN(ret, NhlWARNING);
				ip[i] = NhlHOLLOWFILL;
			}
		}
	}
	
/*
 * If any of the individual convenience fill pattern resources are set --
 * override the corresponding array resource.
 */
	if ((init && mpp->fill_default.pattern != NhlmpUNSETFILLPATTERN) ||
	    mpp->fill_default.pattern != ompp->fill_default.pattern) {
		if (mpp->fill_default.pattern >= NhlHOLLOWFILL)
			ip[NhlmpDEFAULTGROUPINDEX] = mpp->fill_default.pattern;
	}
	if ((init && mpp->ocean.pattern != NhlmpUNSETFILLPATTERN) ||
	    mpp->ocean.pattern != ompp->ocean.pattern) {
		if (mpp->ocean.pattern >= NhlHOLLOWFILL)
			ip[NhlmpOCEANGROUPINDEX] = mpp->ocean.pattern;
	}
	if ((init && mpp->land.pattern != NhlmpUNSETFILLPATTERN) ||
	    mpp->land.pattern != ompp->land.pattern) {
		if (mpp->land.pattern >= NhlHOLLOWFILL)
			ip[NhlmpLANDGROUPINDEX] = mpp->land.pattern;
	}
	if ((init && mpp->inland_water.pattern != NhlmpUNSETFILLPATTERN) ||
	    mpp->inland_water.pattern != ompp->inland_water.pattern) {
		if (mpp->inland_water.pattern >= NhlHOLLOWFILL)
			ip[NhlmpINLANDWATERGROUPINDEX] =
				mpp->inland_water.pattern;
	}
/*
 * Fill scales
 */

	if (init) {
		ga = NULL;
		count = mpp->fill_group_count;
	}
	else {
		ga = ompp->fill_group_scales;
		count = mpp->mono_fill_group_scale ? 1 : mpp->fill_group_count;
	}
	fval = 1.0;
	subret = mpManageGenArray(&ga,count,mpp->fill_group_scales,
				  Qfloat,&fval,&old_count,&init_count,
				  &need_check,&changed,
				  NhlNmpFillGroupScales,entry_name);
	
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ompp->fill_group_scales = changed ? NULL : mpp->fill_group_scales;
	mpp->fill_group_scales = ga;
	
	if (need_check) {
		fp = (float *) ga->data;
		for (i=0; i<count; i++) {
			if (fp[i] <= 0.0) {
				e_text =
	            "%s: %s index %d holds an invalid fill scale: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNmpFillGroupScales,i);
				ret = MIN(ret, NhlWARNING);
				fp[i] = 1.0;
			}
		}
	}

/*
 * If any of the individual convenience fill pattern resources are set --
 * override the corresponding array resource.
 */
	if ((init && mpp->fill_default.scale != NhlmpUNSETFILLSCALE) ||
	    mpp->fill_default.scale != ompp->fill_default.scale) {
		if (mpp->fill_default.scale > 0.0)
			ip[NhlmpDEFAULTGROUPINDEX] = mpp->fill_default.scale;
	}
	if ((init && mpp->ocean.scale != NhlmpUNSETFILLSCALE) ||
	    mpp->ocean.scale != ompp->ocean.scale) {
		if (mpp->ocean.scale > 0.0)
			ip[NhlmpOCEANGROUPINDEX] = mpp->ocean.scale;
	}
	if ((init && mpp->land.scale != NhlmpUNSETFILLSCALE) ||
	    mpp->land.scale != ompp->land.scale) {
		if (mpp->land.scale > 0.0)
			ip[NhlmpLANDGROUPINDEX] = mpp->land.scale;
	}
	if ((init && mpp->inland_water.scale != NhlmpUNSETFILLSCALE) ||
	    mpp->inland_water.scale != ompp->inland_water.scale) {
		if (mpp->inland_water.scale > 0.0)
			ip[NhlmpINLANDWATERGROUPINDEX] =
				mpp->inland_water.scale;
	}
/*
 * Fill area specifiers
 */

	ga = init ? NULL : ompp->fill_area_specs;

	if (ga != mpp->fill_area_specs) {
		NhlFreeGenArray(ga);
		if ((ga = _NhlCopyGenArray(mpp->fill_area_specs,
					   True)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpFillAreaSpecifiers);
			return NhlFATAL;
		}
		mpp->fill_area_specs = ga;

	}
		
/*
 * Mask area specifiers
 */
	ga = init ? NULL : ompp->mask_area_specs;

	if (ga != mpp->mask_area_specs) {
		NhlFreeGenArray(ga);
		if ((ga = _NhlCopyGenArray(mpp->mask_area_specs,
					   True)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpMaskAreaSpecifiers);
			return NhlFATAL;
		}
		mpp->mask_area_specs = ga;
	}
		
/*
 * Outline specifiers
 */
	ga = init ? NULL : ompp->outline_specs;

	if (ga != mpp->outline_specs) {
		NhlFreeGenArray(ga);
		if ((ga = _NhlCopyGenArray(mpp->outline_specs,
					   True)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpOutlineSpecifiers);
			return NhlFATAL;
		}
		mpp->outline_specs = ga;
	}
		
/*
 * Area Name specifiers
 */
	ga = init ? NULL : ompp->area_names;

	if (ga != mpp->area_names) {
		if (mpp->area_names->num_elements != OutRec_Count) {
			e_text = 
			  "%s: %s GenArray must contain %d elements: ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpAreaNames,OutRec_Count);
			ret = MIN(NhlWARNING,ret);
		}
		else {
			NhlFreeGenArray(ga);
			if ((ga = _NhlCopyGenArray(mpp->area_names,
						   True)) == NULL) {
				e_text = "%s: error copying %s GenArray";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name,
					  NhlNmpAreaNames);
				return NhlFATAL;
			}
		}
		mpp->area_names = ga;
	}

/*
 * Area Groups
 */
	ga = init ? NULL : ompp->area_groups;

	need_check = False;
	if (ga != mpp->area_groups) {
		if (mpp->area_groups->num_elements != OutRec_Count) {
			e_text = 
			  "%s: %s GenArray must contain %d elements: ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpAreaGroups,OutRec_Count);
			ret = MIN(NhlWARNING,ret);
		}
		else {
			NhlFreeGenArray(ga);
			if ((ga = _NhlCopyGenArray(mpp->area_groups,
						   True)) == NULL) {
				e_text = "%s: error copying %s GenArray";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name,
					  NhlNmpAreaNames);
				return NhlFATAL;
			}
			mpp->area_groups = ga;
			need_check = True;
		}

	}
	if (need_check || mpp->fill_group_count < ompp->fill_group_count) {
		ip = (int *) ga->data;
		for (i=0; i < ga->num_elements; i++) {
			use_default = False;
			if (ip[i] < NhlmpOCEANGROUPINDEX)
				use_default = True;
			else if (ip[i] > mpp->fill_group_count - 1) {
				e_text =
	         "%s: %s index %d holds an invalid fill group id: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNmpAreaGroups,i);
				ret = MIN(ret, NhlWARNING);
				use_default = True;
			}
			if (use_default)
				ip[i] = OutRecs[i].cix[1];
		}
	}

/*
 * Area types are read only
 */
	ga = init ? NULL : ompp->area_types;

	if (ga != mpp->area_types) {
		e_text = "%s: attempt to set read-only resource %s ignored";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNmpAreaTypes);
		ret = MIN(ret, NhlWARNING);
		mpp->area_types = ga;
	}

/*
 * Fill Colors; fill colors may be indexed either to the fill groups
 * or directly to the hlu color index depending on the setting of 
 * mpDirectFillAreaColor. Issue a warning if the number of elements is
 * not the same as the elements in the fill_area_specs list.
 */		

	ga = init ? NULL : ompp->fill_area_colors;

	if (ga != mpp->fill_area_colors) {
		int max_val;
		if (mpp->fill_area_colors == NULL) {
			NhlFreeGenArray(ga);
			count = 0;
		}
		else {
			NhlFreeGenArray(ga);
			ga = _NhlCopyGenArray(mpp->fill_area_colors,True);
			if (ga == NULL) {
				e_text = "%s: error copying %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,NhlNmpFillAreaColors);
				return NhlFATAL;
			}
			mpp->fill_area_colors = ga;
			count = mpp->fill_area_colors->num_elements;
		}
		if (mpp->fill_area_specs == NULL ||
		    count != mpp->fill_area_specs->num_elements) {
			e_text = 
		"%s: %s and %s element count unequal: defaulting as necessary";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpFillAreaColors,
				  NhlNmpFillAreaSpecifiers);
			ret = MIN(ret, NhlWARNING);
		}
		if (mpp->direct_fill_area_color)
			NhlVAGetValues(mpnew->base.wkptr->base.id,
				       NhlNwkColorMapLen, &max_val, NULL);
		else
			max_val = mpp->fill_group_count - 1;

		ip = (int *) mpp->fill_area_colors->data; 
		for (i = 0; i < count; i++) {
			if (ip[i] > max_val) {
				e_text = 
	           "%s: %s index %d holds an invalid color index: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNmpFillAreaColors,i);
				ret = MIN(ret, NhlWARNING);
				ip[i] = NhlmpUNSETCOLOR;
			}
		}
	}

	return ret;
}

/*
 * Function:  mpUpdateDrawGroups
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
static NhlErrorTypes    mpUpdateDrawGroups
#if __STDC__
(
	NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	int			draw_mode,
	int			color,
	int			group_ix,
	char			*entry_name
)
#else
(mpp,draw_op,draw_mode,color,group_ix,entry_name)
	NhlMapPlotLayerPart	*mpp;
	mpDrawOp		draw_op;
	int			draw_mode;
	int			color;
	int			group_ix;
	char			*entry_name;

#endif
{
	char		*e_text;
	mpNameRec	*groups;
	mpGlobalSetMode *gmode;
	mpStateSetMode	*smode;
	int		*fgp;

	switch (draw_op) {
	case mpDRAWFILL:
		groups = mpp->fill_groups;
		gmode = &mpp->global_fill_mode;
		smode = &mpp->usstates_fill_mode;
		break;
	case mpDRAWOUTLINE:
		groups = mpp->outline_groups;
		gmode = &mpp->global_outline_mode;
		smode = &mpp->usstates_outline_mode;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	fgp = (int *) mpp->fill_group_colors->data;
	if (! mpp->direct_fill_area_color && color > NhlTRANSPARENT)
		color = fgp[color];

/* 
 * Note that the color parameter is ignored for groupings named "ALL..."
 */
	switch ((mpBGroups) group_ix) {
	case mpALLNATIONAL:
		groups[mpNational].draw_mode = draw_mode;
		groups[mpOcean].draw_mode = draw_mode;
		groups[mpContinent].draw_mode = draw_mode;
		groups[mpLargeIsland].draw_mode = draw_mode;
		groups[mpSmallIsland].draw_mode = draw_mode;
		groups[mpInlandWater].draw_mode = draw_mode;
		groups[mpNational].cix = color;
		groups[mpOcean].cix = color;
		groups[mpContinent].cix = color;
		groups[mpLargeIsland].cix = color;
		groups[mpSmallIsland].cix = color;
		groups[mpInlandWater].cix = color;
		*gmode = mpNAT;
		break;
	case mpALLGEOPHYSICAL:
		groups[mpOcean].draw_mode = draw_mode;
		groups[mpContinent].draw_mode = draw_mode;
		groups[mpLargeIsland].draw_mode = draw_mode;
		groups[mpSmallIsland].draw_mode = draw_mode;
		groups[mpInlandWater].draw_mode = draw_mode;
		groups[mpOcean].cix = color;
		groups[mpContinent].cix = color;
		groups[mpLargeIsland].cix = color;
		groups[mpSmallIsland].cix = color;
		groups[mpInlandWater].cix = color;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpLAND:
		groups[mpContinent].draw_mode = draw_mode;
		groups[mpLargeIsland].draw_mode = draw_mode;
		groups[mpSmallIsland].draw_mode = draw_mode;
		groups[mpContinent].cix = color;
		groups[mpLargeIsland].cix = color;
		groups[mpSmallIsland].cix = color;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpWATER:
		groups[mpOcean].draw_mode = draw_mode;
		groups[mpInlandWater].draw_mode = draw_mode;
		groups[mpOcean].cix = color;
		groups[mpInlandWater].cix = color;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpINLANDWATER:
		groups[mpInlandWater].draw_mode = draw_mode;
		groups[mpInlandWater].cix = color;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpOCEANS:
		groups[mpOcean].draw_mode = draw_mode;
		groups[mpOcean].cix = color;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpCONTINENTS:
 		groups[mpContinent].draw_mode = draw_mode;
 		groups[mpContinent].cix = color;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpISLANDS:
		groups[mpLargeIsland].draw_mode = draw_mode;
		groups[mpSmallIsland].draw_mode = draw_mode;
		groups[mpLargeIsland].cix = color;
		groups[mpSmallIsland].cix = color;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpLARGEISLANDS:
		groups[mpLargeIsland].draw_mode = draw_mode;
		groups[mpLargeIsland].cix = color;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpSMALLISLANDS:
		groups[mpSmallIsland].draw_mode = draw_mode;
		groups[mpSmallIsland].cix = color;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpALLUSSTATES:
		groups[mpUSStateLand].draw_mode = draw_mode;
		groups[mpUSStateWater].draw_mode= draw_mode;
		groups[mpUSStateLand].cix = color;
		groups[mpUSStateWater].cix= color;
		*smode = mpSET;
		break;
	case mpUSSTATESLAND:
		groups[mpUSStateLand].draw_mode = draw_mode;
		groups[mpUSStateLand].cix = color;
		*smode = mpSET;
		break;
	case mpUSSTATESWATER:
		groups[mpUSStateWater].draw_mode= draw_mode;
		groups[mpUSStateWater].cix= color;
		*smode = MAX(*smode,mpIMPLIED_SET);
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	return NhlNOERROR;
}

/*
 * Function:	mpUpdateNameRecs
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

static NhlErrorTypes mpUpdateNameRecs
#if  __STDC__
(
	NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	NhlString		name,
	int			draw_mode,
	int			color,
	NhlString		entry_name
)
#else
(mpp,draw_op,name,draw_mode,color,entry_name)
	NhlMapPlotLayerPart	*mpp;
	mpDrawOp		draw_op;
	NhlString		name;
	int			draw_mode;
	int			color;
	NhlString		entry_name;
#endif
{
	char		*e_text;
	int		i;
	int		len = strlen(name);
	NhlBoolean	found = False, found_all = False, found_one = False;
	NhlString	*names = NULL;
	int		*agroups = NULL;
	char		*cp, *np = name;
	NhlBoolean	invert = False;
	NhlString	comp_name;
	typedef enum _mpCompType {
		mpSTRCMP,mpSTRNCMP,mpSTRSTR,mpSTRSTREND
	} mpCompType;
	mpCompType comp_type = mpSTRCMP;
	mpNameRec	**nrecs;
	int		*alloc;
	int		*count;
	mpGlobalSetMode *gmode;
	mpStateSetMode	*smode;
	int		*fgp;

	switch (draw_op) {
	case mpDRAWFILL:
		nrecs = &mpp->fill_recs;
		alloc = &mpp->fill_rec_alloc;
		count = &mpp->fill_rec_count;
		gmode = &mpp->global_fill_mode;
		smode = &mpp->usstates_fill_mode;
		break;
	case mpDRAWOUTLINE:
		nrecs = &mpp->outline_recs;
		alloc = &mpp->outline_rec_alloc;
		count = &mpp->outline_rec_count;
		gmode = &mpp->global_outline_mode;
		smode = &mpp->usstates_outline_mode;
		fgp = NULL;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	if (mpp->area_names != NULL)
		names = (NhlString *) mpp->area_names->data;
	if (mpp->area_groups != NULL)
		agroups = (int *) mpp->area_groups->data;

	if (*nrecs == NULL) {
		if ((*nrecs = (mpNameRec *) 
		     NhlMalloc(sizeof(mpNameRec) * mpALLOC_UNIT)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		mpp->fill_rec_alloc = mpALLOC_UNIT;
	}

	if (name[len - 1] == '*') {
		comp_type = mpSTRNCMP;
		len --;
	}

	if (np[0] == '!') {
		invert = True;
		len --;
		np++;
	}
	if (np[0] == '*') {
		if (comp_type == mpSTRNCMP)
			comp_type = mpSTRSTR;
		else
			comp_type = mpSTRSTREND;
		len--;
		np++;
	}
	
	fgp = (int *) mpp->fill_group_colors->data;
	for (i = 0; i < OutRec_Count; i++) {
		comp_name = names ? names[i] : OutRecs[i].name; 
		if (*count == *alloc) {
			if ((*nrecs = (mpNameRec *) 
			     NhlRealloc(*nrecs,sizeof(mpNameRec)
					* (*alloc + mpALLOC_UNIT))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			*alloc += mpALLOC_UNIT;
		}
		switch (comp_type) {
		case mpSTRCMP:
			if (! strcmp(comp_name,np)) {
				found = True;
				found_one = True;
				found_all = True;
			}
			break;
		case mpSTRNCMP:
			if (! strncmp(comp_name,np,len)) {
				found_one = True;
				found = True;
			}
			break;
		case mpSTRSTR:
			if (strstr(comp_name,np) != NULL) {
				found_one = True;
				found = True;
			}
			break;
		case mpSTRSTREND:
			if ((cp = strstr(comp_name,np)) != NULL &&
			    (comp_name + strlen(comp_name) - cp == len)) {
				found_one = True;
				found = True;
			}
			break;
		}
		if (found) {
			(*nrecs)[*count].name_ix = i;
			(*nrecs)[*count].draw_mode = draw_mode;
			if (color == NhlTRANSPARENT)
				(*nrecs)[*count].cix = color;
			else if (color > NhlTRANSPARENT) 
				if (mpp->direct_fill_area_color)
					(*nrecs)[*count].cix = color;
				else 
					(*nrecs)[*count].cix = fgp[color];
			else if (OutRecs[i].id[0] < 0)
				(*nrecs)[*count].cix = agroups ?
				      fgp[agroups[i]] : fgp[OutRecs[i].cix[1]];
			else if (mpp->group_priority == 
				 NhlmpPOLITICALPRIORITY && ! invert)
				(*nrecs)[*count].cix = agroups ?
				      fgp[agroups[i]] : fgp[OutRecs[i].cix[1]];
			else if (mpp->group_priority == 
				 NhlmpGEOPHYSICALPRIORITY && invert)
				(*nrecs)[*count].cix = agroups ?
				      fgp[agroups[i]] : fgp[OutRecs[i].cix[1]];
			else
				(*nrecs)[*count].cix = fgp[OutRecs[i].cix[0]];

			if (OutRecs[i].type == mpNational) {
				*gmode = MAX(*gmode,mpIMPLIED_NAT);
			}
			else if (OutRecs[i].type == mpUSStateLand ||
					OutRecs[i].type == mpUSStateWater) {
				*smode = MAX(*smode,mpIMPLIED_SET);
			}
			found = False;
			(*count)++;
		}
		if (found_all)
			break;
	}

	if (! found_one) {
		e_text = "%s: invalid boundary specification string: \"%s\"";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,name);
		return NhlWARNING;
	}
	return NhlNOERROR;
}

/*
 * Function:  mpBuildFillDrawList
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
static NhlErrorTypes    mpBuildFillDrawList
#if __STDC__
(
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args)
#else
(mpnew,mpold,init,args,num_args)
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	NhlMapPlotLayerPart	*mpp = &(mpnew->mapplot);
	NhlMapPlotLayerPart	*ompp = &(mpold->mapplot);
	int			i,j,color_count,color;
	NhlString		*sp;
	NhlBoolean		found;
	int			*clrs;

	entry_name = (init) ? "MapPlotInitialize" : "MapPlotSetValues";
/*
 * If the National set is specified all countries are drawn using the 
 * national color scheme. If however, the geophysical set is specified along
 * with one or more explicitly specified national areas, the national set
 * must be used, but areas not explicitly specified are drawn using the
 * geophysical set generic land color. In this case the National set is
 * specified implicitly.
 */

/*
 * don't go through all this if nothing has changed
 */
	if (! init && 
	    mpp->fill_boundaries == ompp->fill_boundaries &&
	    mpp->fill_area_specs == ompp->fill_area_specs &&
	    mpp->mask_area_specs == ompp->mask_area_specs &&
	    mpp->fill_area_colors == ompp->fill_area_colors &&
	    mpp->direct_fill_area_color == ompp->direct_fill_area_color &&
	    mpp->area_names == ompp->area_names &&
	    mpp->area_groups == ompp->area_groups)
		return ret;

	clrs = (int *) mpp->fill_group_colors->data;
	for (i = 0; i < NhlNumber(mpp->fill_groups); i++) {
		mpp->fill_groups[i].draw_mode = mpBACKGROUND;
		mpp->fill_groups[i].cix = clrs[NhlmpDEFAULTGROUPINDEX];
	}
	mpp->global_fill_mode = mpNONE;
	mpp->usstates_fill_mode = mpNOSET;
	mpp->fill_rec_count = 0;
	mpp->usstates_color = -999;

/* 
 * Determine which map element types are required and consequently which
 * outline sets to use; first examine the boundary sets enumerative, then
 * the include and exclude lists.
 */
	switch (mpp->fill_boundaries) {
	case NhlmpNOBOUNDARIES:
	default:
		break;
	case NhlmpGEOPHYSICAL:
		subret = mpUpdateDrawGroups(mpp,mpDRAWFILL,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLGEOPHYSICAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlmpNATIONAL:
		subret = mpUpdateDrawGroups(mpp,mpDRAWFILL,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLNATIONAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlmpUSSTATES:
		subret = mpUpdateDrawGroups(mpp,mpDRAWFILL,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlmpGEOPHYSICALANDUSSTATES:
		subret = mpUpdateDrawGroups(mpp,mpDRAWFILL,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLGEOPHYSICAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = mpUpdateDrawGroups(mpp,mpDRAWFILL,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlmpALLBOUNDARIES:
		subret = mpUpdateDrawGroups(mpp,mpDRAWFILL,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLNATIONAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = mpUpdateDrawGroups(mpp,mpDRAWFILL,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	}

	if (mpp->fill_area_specs != NULL) {
		if (mpp->fill_area_colors != NULL) {
			color_count = mpp->fill_area_colors->num_elements;
			clrs = (int *) mpp->fill_area_colors->data;
		}
		else {
			color_count = 0;
			clrs = NULL;
		}
		sp = (NhlString *) mpp->fill_area_specs->data;
		for (i = 0; i < mpp->fill_area_specs->num_elements; i++) {
			found = False;
			mpLowerCase(sp[i]);
			color = (i < color_count) ? clrs[i] : NhlmpUNSETCOLOR;
			for (j = 0; j < NhlNumber(BGroup_Names); j++) {
				if (! strcmp(sp[i],BGroup_Names[j])) {
					subret = mpUpdateDrawGroups(mpp,
					       mpDRAWFILL,mpDRAW,
					       color,j,entry_name);
					if ((ret = MIN(ret,subret)) < 
					    NhlWARNING)
						return ret;
					found = True;
					break;
				}
			}   
			if (! found) 
				subret = mpUpdateNameRecs(mpp,mpDRAWFILL,sp[i],
						     mpDRAW,color,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;

		}
	}

	if (mpp->mask_area_specs != NULL) {
		sp = (NhlString *) mpp->mask_area_specs->data;
		for (i = 0; i < mpp->mask_area_specs->num_elements; i++) {
			found = False;
			mpLowerCase(sp[i]);
			for (j = 0; j < NhlNumber(BGroup_Names); j++) {
				if (! strcmp(sp[i],BGroup_Names[j])) {
					subret = mpUpdateDrawGroups(mpp,
					       mpDRAWFILL,mpMASK,
					       NhlTRANSPARENT,j,entry_name);
					if ((ret = MIN(ret,subret)) < 
					    NhlWARNING)
						return ret;
					found = True;
					break;
				}
			}   
			if (! found) 
				subret = mpUpdateNameRecs(mpp,mpDRAWFILL,sp[i],
					     mpMASK,NhlTRANSPARENT,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
		}
	}
/*
 * If usstates are to be used promote to the global set if necessary
 */
	if (mpp->usstates_fill_mode > mpNOSET && 
	    mpp->global_fill_mode == mpGEO) {
		mpp->global_fill_mode = mpIMPLIED_NAT;
	}

	return ret;
}

/*
 * Function:  mpBuildOutlineDrawList
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
static NhlErrorTypes    mpBuildOutlineDrawList
#if __STDC__
(
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args)
#else
(mpnew,mpold,init,args,num_args)
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *entry_name;
	NhlMapPlotLayerPart *mpp = &(mpnew->mapplot);
	NhlMapPlotLayerPart *ompp = &(mpold->mapplot);
	int i,j;
	NhlString	*sp;
	NhlBoolean	found;

	entry_name = (init) ? "MapPlotInitialize" : "MapPlotSetValues";

/*
 * don't go through all this if nothing has changed
 */
	if (! init && 
	    mpp->outline_boundaries == ompp->outline_boundaries &&
	    mpp->outline_specs == ompp->outline_specs)
		return ret;

	for (i = 0; i < NhlNumber(mpp->outline_groups); i++) {
		mpp->outline_groups[i].draw_mode = mpBACKGROUND;
		mpp->outline_groups[i].cix = NhlmpUNSETCOLOR;
	}
	mpp->global_outline_mode = mpNONE;
	mpp->usstates_outline_mode = mpNOSET;
	mpp->outline_rec_count = 0;

/* 
 * Determine which map element types are required and consequently which
 * outline sets to use; first examine the boundary sets enumerative, then
 * the include and exclude lists.
 */
	switch (mpp->outline_boundaries) {
	case NhlmpNOBOUNDARIES:
	default:
		break;
	case NhlmpGEOPHYSICAL:
		subret = mpUpdateDrawGroups(mpp,mpDRAWOUTLINE,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLGEOPHYSICAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlmpNATIONAL:
		subret = mpUpdateDrawGroups(mpp,mpDRAWOUTLINE,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLNATIONAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlmpUSSTATES:
		subret = mpUpdateDrawGroups(mpp,mpDRAWOUTLINE,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlmpGEOPHYSICALANDUSSTATES:
		subret = mpUpdateDrawGroups(mpp,mpDRAWOUTLINE,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLGEOPHYSICAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = mpUpdateDrawGroups(mpp,mpDRAWOUTLINE,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlmpALLBOUNDARIES:
		subret = mpUpdateDrawGroups(mpp,mpDRAWOUTLINE,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLNATIONAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = mpUpdateDrawGroups(mpp,mpDRAWOUTLINE,mpDRAW,
					    NhlmpUNSETCOLOR,
					    mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	}

	if (mpp->outline_specs != NULL) {
		sp = (NhlString *) mpp->outline_specs->data;
		for (i = 0; i < mpp->outline_specs->num_elements; i++) {
			found = False;
			mpLowerCase(sp[i]);
			for (j = 0; j < NhlNumber(BGroup_Names); j++) {
				if (! strcmp(sp[i],BGroup_Names[j])) {
					subret = mpUpdateDrawGroups(mpp,
						    mpDRAWOUTLINE,mpDRAW,
						 NhlmpUNSETCOLOR,j,entry_name);
					if ((ret = MIN(ret,subret)) < 
					    NhlWARNING)
						return ret;
					found = True;
					break;
				}
			}   
			if (! found) 
				subret = mpUpdateNameRecs(mpp,mpDRAWOUTLINE,
						      sp[i],mpDRAW,
						   NhlmpUNSETCOLOR,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;

		}
	}
/*
 * If usstates are to be used promote the global set if necessary
 */
	if (mpp->usstates_outline_mode > mpNOSET && 
	    mpp->global_outline_mode == mpGEO) {
		mpp->global_outline_mode = mpIMPLIED_NAT;
	}

	return ret;
}


/*
 * Function:    mpManageGenArray
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
 *		*changed	True if the data has been modified in any way.
 *
 *
 * Return Values:
 *
 * Side Effects: The internal copy of each GenArray is modified to reflect
 *	changes requested via MapPlotSetValues
 */

/*ARGSUSED*/
static NhlErrorTypes    mpManageGenArray
#if __STDC__
	(NhlGenArray	*ga,
	 int		count,
	 NhlGenArray	copy_ga,
	 NrmQuark	type,
	 NhlPointer	init_val,
	 int		*old_count,
	 int		*init_count,
	 NhlBoolean	*need_check,
	 NhlBoolean	*changed,
	 NhlString	resource_name,
	 NhlString	entry_name)
#else
(ga,count,copy_ga,type,init_val,old_count,init_count,
 need_check,changed,resource_name,entry_name)
	NhlGenArray	*ga;
	int		count;
	NhlGenArray	copy_ga;
	NrmQuark	type;
	NhlPointer	init_val;
	int		*old_count;
	int		*init_count;
	NhlBoolean	*need_check;
	NhlBoolean	*changed;
	NhlString	resource_name;
	NhlString	entry_name;
#endif
{
	char		*str_type;
	NhlErrorTypes	ret = NhlNOERROR;
	int		i, size;
	NhlPointer	datap;
	char		*e_text;

	*init_count = 0;
	*need_check = False;
	*changed = False;
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
			(*ga)->data = datap;
			(*ga)->num_elements = count;
			*changed = True;
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
		*changed = True;
	}

/* 
 * If there is a GenArray to copy, copy it; then initialize all remaining
 * uninitialized elements if an initialization value has been passed in.
 */

	if (copy_ga != NULL && copy_ga != *ga) {

		*need_check = True;
		ret = _NhlValidatedGenArrayCopy(ga,copy_ga,
                                                Nhl_mpMAX_FILL_GROUPS,
						True,False,resource_name, 
						entry_name);
		if (ret < NhlWARNING) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,
				  resource_name);
			return ret;
		}
		*init_count = copy_ga->num_elements;
		*changed = True;
	}

	if (*init_count < count) {

		if (init_val == NULL) {
			if (type == Qstring) {
				NhlString *sp = (NhlString *) datap;
				for (i = *init_count; i< count; i++) {
					if (i < *old_count) NhlFree(sp[i]);
					sp[i] = NULL;
				}
			}
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
		*changed = True;
	}

	return ret;
}


/*
 * Function:	mpSetUpTransObjs
 *
 * Description: Sets up the Map transformation object for the generic
 *		Map plot object. Note that since this trans object
 *		does not require any dynamic memory, the same trans object
 *		persists for the life of the MapPlot object.
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
/*ARGSUSED*/
static NhlErrorTypes mpSetUpTransObj
#if  __STDC__
(
	NhlMapPlotLayer	mpnew,
	NhlMapPlotLayer	mpold,
	NhlBoolean	init
)
#else 
(mpnew,mpold,init)
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlTransformLayerPart	*tfp = &(mpnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[16];
        int			nargs = 0;
	NhlBoolean		trans_changed;


	entry_name = (init) ? "MapPlotInitialize" : "MapPlotSetValues";
/*
 * Since no dynamic memory is involved a Map transformation only
 * needs to be created once. It will not be freed until the object
 * is destroyed. For now all map trans resources are simply passed through.
 */
	if (init || tfp->trans_obj == NULL) {

		mpnew->mapplot.new_draw_req = True;
		sprintf(buffer,"%s",mpnew->base.name);
		strcat(buffer,".Trans");

		subret = _NhlVACreateChild(&tmpid,buffer,
					   NhlmapTransObjLayerClass,
					   (NhlLayer) mpnew, NULL);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		return ret;
	}

	subret = _NhlALSetValuesChild(tfp->trans_obj->base.id,
				      (NhlLayer) mpnew,sargs,nargs);

	NhlVAGetValues(tfp->trans_obj->base.id,
		       NhlNmpTransChanged,&trans_changed,
		       NULL);

	if (trans_changed) {
		mpnew->mapplot.update_req = True;
		mpnew->mapplot.new_draw_req = True;
	}

	return MIN(ret,subret);

}

/*
 * Function:  mapusr_
 *
 * Description: C version of the MAPUSR function that EZMAP invokes 
 *		before drawing each class of map objects.
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
void   (_NHLCALLF(mapusr,MAPUSR))
#if __STDC__
(
	int	*iprt
)
#else
(iprt)
	int	*iprt;
#endif

{
	float	thickness;
	int	dpat;
	float	dseglen;
	NhlString *sp;
	float	p0,p1,jcrt;
	int	slen;
	char	buffer[128];

	switch (*iprt) {
	case 1:		/* perimeter */
		thickness = Mpp->perim.thickness;
		dpat = Mpp->perim.dash_pat;
		dseglen = Mpp->perim.dash_seglen;
		break;
	case 2:		/* grid */
		thickness = Mpp->grid.thickness;
		dpat = Mpp->grid.dash_pat;
		dseglen = Mpp->grid.dash_seglen;
		break;
	case 3:		/* labels */
		return;
	case 4:		/* limb lines */
		thickness = Mpp->limb.thickness;
		dpat = Mpp->limb.dash_pat;
		dseglen = Mpp->limb.dash_seglen;
		break;
	case 5:		/* geophysical outlines */
		thickness = Mpp->geophysical.thickness;
		dpat = Mpp->geophysical.dash_pat;
		dseglen = Mpp->geophysical.dash_seglen;
		break;
	case 6:		/* us states outlines */
		thickness = Mpp->us_state.thickness;
		dpat = Mpp->us_state.dash_pat;
		dseglen = Mpp->us_state.dash_seglen;
		break;
	case 7:		/* countries */
		thickness = Mpp->national.thickness;
		dpat = Mpp->national.dash_pat;
		dseglen = Mpp->national.dash_seglen;
		break;
	default:
		return;
	}
		
 	c_pcseti("CC",-1);
	c_pcseti("OC",-1);
	gset_linewidth(thickness);

	dpat %= Mpp->dash_table->num_elements;
	sp = (NhlString *) Mpp->dash_table->data;
	slen = strlen(sp[dpat]);
	p0 =  (float) c_kfpy(0.0);
	p1 = dseglen;
	p1 = (float) c_kfpy(p1);
	jcrt = (int) ((p1 - p0) / slen + 0.5);
	jcrt = jcrt > 1 ? jcrt : 1;
	strcpy(buffer,sp[dpat]);
	
	c_dashdc(buffer,jcrt,4);

}

/*
 * Function:  mapeod_
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
void   (_NHLCALLF(mapeod,MAPEOD))
#if __STDC__
(
	int *nout,
	int *nseg,
	int *idls,
	int *idrs,
	int *npts,
	float *pnts
)
#else
(nout,nseg,idls,idrs,npts,pnts)
	int *nout;
	int *nseg;
	int *idls;
	int *idrs;
	int *npts;
	float *pnts;
#endif
{
	int ir,il;
	NhlBoolean keep = False;

	switch (Draw_Op) {
	case mpDRAWOUTLINE:
		il = *idls - Id_Offset[Outline_Set];
		ir = *idrs - Id_Offset[Outline_Set];
		if (il >= 0) {
			switch (DrawIds[il].draw_mode) {
			case mpDRAW:
				keep = True;
				break;
			case mpDRAWSPECIAL:
				if (*idrs == Draw_Check)
					keep = True;
				break;
			default:
				break;
			}
		}
		if (ir >= 0) {
			switch (DrawIds[ir].draw_mode) {
			case mpDRAW:
				keep = True;
				break;
			case mpDRAWSPECIAL:
				if (*idls == Draw_Check)
					keep = True;
				break;
			default:
				break;
			}
		}
		if (! keep)
			*npts = 0;
		break;
	case mpDRAWFILL:
		il = *idls - Id_Offset[Outline_Set];
		ir = *idrs - Id_Offset[Outline_Set];
		if (il >= 0 && DrawIds[il].draw_mode != mpBACKGROUND)
			keep = True;

		else if (ir >= 0 && DrawIds[ir].draw_mode != mpBACKGROUND)
			keep = True;

		if (! keep)
			*npts = 0;
		else if (DrawIds[ir].draw_mode == DrawIds[il].draw_mode &&
			 DrawIds[ir].cix == DrawIds[il].cix) {
			*npts = 0;
		}
		break;
	default:
		return;
	}
	
#if 0

	printf("nseg %d idls %d, idrs %d, npts %d\n", 
	       *nseg,*idls,*idrs,*npts);
#endif

	return;
}

/*
 * Function:  nhlmaskgrid_
 *
 * Description: C version of ULPR user routine called from within MAPGRM 
 *		to mask a grid based on an areamap.
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

int (_NHLCALLF(nhlmaskgrid,NHLMASKGRID))
#if __STDC__
(
	float *xcra, 
	float *ycra, 
	int *ncra, 
	int *iai, 
	int *iag, 
	int *nai
)
#else
(xcra,ycra,ncra,iai,iag,nai)
	float *xcra; 
	float *ycra; 
	int *ncra; 
	int *iai; 
	int *iag; 
	int *nai;
#endif
{
	NhlBoolean draw_line = False;
	int i,id,ix;
	mpOutlineSet type;

	id = *iai - Id_Offset[Outline_Set];
	if (id < 0)
		return 0;
	switch (Mpp->grid_mask_mode) {
	case NhlmpMASKNONE:
	default:
		draw_line = True;
		break;
	case NhlmpMASKOCEAN:
		if (id != 1)
			draw_line = True;
		break;
	case NhlmpMASKNOTOCEAN:
		if (id == 1 ||
		    DrawIds[id].draw_mode == mpBACKGROUND) {
			draw_line = True;
		}
		break;
	case NhlmpMASKLAND:
		switch (Outline_Set) {
		case mpEMPTY:
			draw_line = True;
			break;
		case mpCO:
			ix = 0;
			type = mpInlandWater;
			break;
		case mpPO:
			ix = 1;
			type = mpInlandWater;
			break;
		case mpUS:
			ix = 1;
			type = mpUSStateWater;
			break;
		case mpPS:
			ix = 2;
			type = mpInlandWater;
			break;
		}

		if (id == 1) {
			draw_line = True;
		}
		else {
			for (i = Otype_Start[mpInlandWater]; 
			     i < Otype_Start[mpInlandWater + 1]; i++) {
				if (id == OutRecs[i].id[ix] - 
				    Id_Offset[Outline_Set]) {
					draw_line = True;
					break;
				}
			}
		}
		break;
	case NhlmpMASKNOTLAND:
		draw_line = True;
		switch (Outline_Set) {
		case mpEMPTY:
			draw_line = True;
			break;
		case mpCO:
			ix = 0;
			type = mpInlandWater;
			break;
		case mpPO:
			ix = 1;
			type = mpInlandWater;
			break;
		case mpUS:
			ix = 0;
			type = mpUSStateWater;
			break;
		case mpPS:
			ix = 2;
			type = mpInlandWater;
			break;
		}

		if (id == 1) {
			draw_line = False;
		}
		else {
			for (i = Otype_Start[mpInlandWater]; 
			     i < Otype_Start[mpInlandWater + 1]; i++) {
				if (id == OutRecs[i].id[ix] - 
				    Id_Offset[Outline_Set]) {
					draw_line = False;
					break;
				}
			}
		}
		break;
	case NhlmpMASKFILLAREA:
		if (DrawIds[id].draw_mode == mpBACKGROUND) {
			draw_line = True;
		}
		break;
	case NhlmpMASKMASKAREA:
		if (DrawIds[id].draw_mode < mpMASK) {
			draw_line = True;
		}
		break;
	}
	if (! draw_line)
		return 0;
		
	NhlVASetValues(Mpl->base.wkptr->base.id,
		       NhlNwkLineLabel,"",
		       NhlNwkDashPattern,Mpp->grid.dash_pat,
		       NhlNwkLineDashSegLenF,Mpp->grid.dash_seglen,
		       NhlNwkLineThicknessF,Mpp->grid.thickness,
		       NhlNwkLineColor,Mpp->grid.gks_color, 
		       NULL);

	_NhlSetLineInfo(Mpl->base.wkptr,(NhlLayer) Mpl);
	_NhlWorkstationLineTo(Mpl->base.wkptr, 
			      xcra[0],ycra[0],1);
	for (i = 1; i < *ncra; i++)
		_NhlWorkstationLineTo(Mpl->base.wkptr, 
				      xcra[i],ycra[i],0);
	_NhlWorkstationLineTo(Mpl->base.wkptr,0.0,0.0,1);

	return 0;
}


/*
 * Function:  nhlezmapfill_
 *
 * Description: C version of APR user routine called from within ARSCAM 
 *		to fill areas based on the area ID.
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
int (_NHLCALLF(nhlezmapfill,NHLEZMAPFILL))
#if __STDC__
(
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
)
#else
(xcs,ycs,ncs,iai,iag,nai)
	float *xcs; 
	float *ycs; 
	int *ncs; 
	int *iai; 
	int *iag; 
	int *nai;
#endif
{
	int ix, pat_ix, col_ix, id;
	int *ip, *cip;
	float *fp;
	float fscale;

	ip = (int *) Mpp->fill_group_patterns->data;
	cip = (int *) Mpp->fill_group_colors->data;
	fp = (float *) Mpp->fill_group_scales->data;

	if (*iai < 1) return 0;
	id = *iai - Id_Offset[Outline_Set];
#if 0
	printf("iai %d id %d use %d color %d\n", *iai,id,
	       DrawIds[id].draw_mode,DrawIds[id].cix);
#endif
	if (id < 0 || DrawIds[id].draw_mode > mpDRAW)
		return 0;

	ix = DrawIds[id].cix;
	col_ix = Mpp->mono_fill_group_color ? cip[0] : ix;
	pat_ix = Mpp->mono_fill_group_pattern ? ip[0] : ip[ix];
	fscale = Mpp->mono_fill_group_scale ? fp[0] : fp[ix];

	NhlVASetValues(Mpl->base.wkptr->base.id,
		       NhlNwkFillBackground, Mpp->fill_pattern_background,
		       NhlNwkFillIndex, pat_ix,
		       NhlNwkFillColor, col_ix,
		       NhlNwkFillScaleFactorF,fscale,
		       NhlNwkDrawEdges,0,
		       NULL);
	
	_NhlSetFillInfo(Mpl->base.wkptr, (NhlLayer) Mpl);
	_NhlWorkstationFill(Mpl->base.wkptr,xcs,ycs,*ncs);

	return 0;
}
