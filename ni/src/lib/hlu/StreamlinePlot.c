/*
 *      $Id: StreamlinePlot.c,v 1.9 1996-05-03 23:51:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		StreamlinePlot.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 28 11:02:12 MDT 1995
 *
 *	Description:	Creates and manages a StreamlinePlot plot object
 */

#include <stdio.h>
#include <math.h>
#include <errno.h>

#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/StreamlinePlotP.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/IrregularTransObjP.h>
#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>

/*
 * Function:	ResourceUnset
 *
 * Description:	This function can be used to determine if a resource has
 *		been set at initialize time either in the Create call or
 *		from a resource data base. In order to use it a Boolean
 *		variable (by convention '<var_name>_set')
 *		MUST directly proceed the declaration of the subject
 *		resource variable in the LayerPart struct. Also a .nores 
 *		NhlResource struct for the <var_name>_set variable
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
#if	NhlNeedProto
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

#define	Oset(field)	NhlOffset(NhlStreamlinePlotDataDepLayerRec,stdata.field)
static NhlResource data_resources[] = {

	{NhlNstFoo,NhlCstFoo,NhlTInteger,sizeof(int),
		 Oset(foo),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL}
};
#undef Oset

#define Oset(field)     NhlOffset(NhlStreamlinePlotLayerRec,streamlineplot.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

/* Data resources */

	{NhlNstVectorFieldData,NhlCstVectorFieldData,_NhlTDataList,
		 sizeof(NhlGenArray),
		 Oset(vector_field_data),NhlTImmediate,_NhlUSET(NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
	{NhlNstScalarFieldData,NhlCstScalarFieldData,_NhlTDataList,
		 sizeof(NhlGenArray),
		 Oset(scalar_field_data),NhlTImmediate,_NhlUSET(NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},

	{NhlNstMapDirection,NhlCstMapDirection,
		  NhlTBoolean,sizeof(NhlBoolean),
		  Oset(map_direction),NhlTImmediate,
		  _NhlUSET((NhlPointer) True),0,NULL},

	{NhlNstLineThicknessF,NhlCstLineThicknessF,
		  NhlTFloat,sizeof(float),Oset(line_thickness),NhlTString,
		  _NhlUSET("1.0"),0,NULL},
	{NhlNstLineColor, NhlCstLineColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(line_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(arrow_length_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{NhlNstArrowLengthF,NhlCstArrowLengthF,
		  NhlTFloat,sizeof(float),Oset(arrow_length),NhlTProcedure,
		  _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(step_size_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{NhlNstStepSizeF,NhlCstStepSizeF,
		  NhlTFloat,sizeof(float),Oset(step_size),NhlTProcedure,
		  _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(min_line_length_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{NhlNstMinLineLengthF,NhlCstMinLineLengthF,
		  NhlTFloat,sizeof(float),Oset(min_line_length),NhlTProcedure,
		  _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(min_line_spacing_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{NhlNstMinLineSpacingF,NhlCstMinLineSpacingF,
		  NhlTFloat,sizeof(float),Oset(min_line_spacing),NhlTProcedure,
		  _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(min_arrow_spacing_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{NhlNstMinArrowSpacingF,NhlCstMinArrowSpacingF,
		  NhlTFloat,sizeof(float),
                 Oset(min_arrow_spacing),NhlTProcedure,
		  _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{NhlNstMinStepFactorF,NhlCstMinStepFactorF,NhlTFloat,sizeof(float),
		  Oset(min_step_factor),NhlTString,
		  _NhlUSET("2.0"),0,NULL},
	{NhlNstLengthCheckCount,NhlCstLengthCheckCount,NhlTInteger,sizeof(int),
		  Oset(length_check_count),NhlTImmediate,
		  _NhlUSET((NhlPointer) 35),0,NULL},
	{NhlNstCrossoverCheckCount,NhlCstCrossoverCheckCount,NhlTInteger,
		 sizeof(int),Oset(crossover_check_count),NhlTImmediate,
		  _NhlUSET((NhlPointer) -1),0,NULL},

	{NhlNstLineStartStride,NhlCstLineStartStride,NhlTInteger,
		 sizeof(int),Oset(line_start_stride),NhlTImmediate,
		  _NhlUSET((NhlPointer) 2),0,NULL},
	{NhlNstArrowStride,NhlCstArrowStride,NhlTInteger,
		 sizeof(int),Oset(arrow_stride),NhlTImmediate,
		  _NhlUSET((NhlPointer) 2),0,NULL},

/* Level resources */

	{NhlNstLevels, NhlCstLevels,  NhlTFloatGenArray,
		 sizeof(NhlPointer),Oset(levels),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{ NhlNstLevelCount,NhlCstLevelCount,NhlTInteger,sizeof(int),
		  Oset(level_count),NhlTImmediate,
		  _NhlUSET((NhlPointer) 16),_NhlRES_GONLY,NULL},
	{ NhlNstLevelSelectionMode,NhlCstLevelSelectionMode,
		  NhlTLevelSelectionMode,sizeof(NhlLevelSelectionMode),
		  Oset(level_selection_mode),
		  NhlTImmediate,
		  _NhlUSET((NhlPointer) NhlAUTOMATICLEVELS),0,NULL},
	{ NhlNstMaxLevelCount,NhlCstMaxLevelCount,NhlTInteger,sizeof(int),
		  Oset(max_level_count),NhlTImmediate,
		  _NhlUSET((NhlPointer) 16),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(level_spacing_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNstLevelSpacingF,NhlCstLevelSpacingF,NhlTFloat,sizeof(float),
		  Oset(level_spacing),NhlTProcedure,
		  _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(min_level_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNstMinLevelValF,NhlCstMinLevelValF,NhlTFloat,sizeof(float),
		  Oset(min_level_val),
		  NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(max_level_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNstMaxLevelValF,NhlCstMaxLevelValF,NhlTFloat,sizeof(float),
		  Oset(max_level_val),
		  NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},

	{NhlNstUseScalarArray,NhlCstUseScalarArray,
		  NhlTBoolean,sizeof(NhlBoolean),
		  Oset(use_scalar_array),NhlTImmediate,
		  _NhlUSET((NhlPointer) False),0,NULL},
	{NhlNstMonoStreamlineLineColor,NhlCstMonoStreamlineLineColor,
		  NhlTBoolean,sizeof(NhlBoolean),
		  Oset(mono_streamline_line_color),NhlTImmediate,
		  _NhlUSET((NhlPointer) True),0,NULL},
	{NhlNstStreamlineLineColor, NhlCstStreamlineLineColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(streamline_line_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNstMonoStreamlineFillColor,NhlCstMonoStreamlineFillColor,
		  NhlTBoolean,sizeof(NhlBoolean),
		  Oset(mono_streamline_fill_color),NhlTImmediate,
		  _NhlUSET((NhlPointer) True),0,NULL},
	{NhlNstStreamlineFillColor, NhlCstStreamlineFillColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(streamline_fill_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNstStreamlineColors, NhlCstStreamlineColors,NhlTColorIndexGenArray,
		 sizeof(NhlGenArray),Oset(streamline_colors),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNstScalarMissingValColor,NhlCstScalarMissingValColor, 
		 NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(scalar_mval_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
 	{NhlNstStreamlineDrawOrder,NhlCstStreamlineDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(streamline_order),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlDRAW),0,NULL},


	{NhlNstNoDataLabelOn,NhlCstNoDataLabelOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(zerof_lbl.string2_on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNstNoDataLabelString,NhlCstNoDataLabelString,
		 NhlTString,sizeof(NhlString),Oset(zerof_lbl.string2),
		 NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree},

/* Zero field label resources */

	{NhlNstZeroFLabelOn,NhlCstZeroFLabelOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(zerof_lbl.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNstZeroFLabelString,NhlCstZeroFLabelString,
		 NhlTString,sizeof(NhlString),Oset(zerof_lbl.string1),
		 NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(zerof_lbl.height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
        {NhlNstZeroFLabelFontHeightF,NhlCstZeroFLabelFontHeightF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},
        {NhlNstZeroFLabelTextDirection,NhlCstZeroFLabelTextDirection,
		 NhlTTextDirection,sizeof(NhlTextDirection),
		 Oset(zerof_lbl.direction),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlACROSS),0,NULL},
	{NhlNstZeroFLabelFont,NhlCstZeroFLabelFont,NhlTFont, 
		 sizeof(int),Oset(zerof_lbl.font),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNstZeroFLabelFontColor,NhlCstZeroFLabelFontColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(zerof_lbl.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNstZeroFLabelFontAspectF,NhlCstZeroFLabelFontAspectF,NhlTFloat, 
		 sizeof(float),Oset(zerof_lbl.aspect),
		 NhlTString, _NhlUSET("1.3125"),0,NULL},
	{NhlNstZeroFLabelFontThicknessF,NhlCstZeroFLabelFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNstZeroFLabelFontQuality,NhlCstZeroFLabelFontQuality,
		 NhlTFontQuality, 
		 sizeof(NhlFontQuality),Oset(zerof_lbl.quality),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNstZeroFLabelConstantSpacingF,NhlCstZeroFLabelConstantSpacingF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.cspacing),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNstZeroFLabelAngleF,NhlCstZeroFLabelAngleF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.angle),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNstZeroFLabelFuncCode,NhlCstZeroFLabelFuncCode,NhlTCharacter, 
		 sizeof(char),Oset(zerof_lbl.fcode[0]),
		 NhlTString, _NhlUSET(":"),0,NULL},
	{NhlNstZeroFLabelBackgroundColor,NhlCstZeroFLabelBackgroundColor,
		 NhlTColorIndex,sizeof(NhlColorIndex),
		 Oset(zerof_lbl.back_color),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNstZeroFLabelPerimOn,NhlCstZeroFLabelPerimOn,
                 NhlTBoolean,sizeof(NhlBoolean),Oset(zerof_lbl.perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNstZeroFLabelPerimSpaceF,NhlCstZeroFLabelPerimSpaceF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.perim_space),
		 NhlTString,_NhlUSET("0.33"),0,NULL},
	{NhlNstZeroFLabelPerimColor,NhlCstZeroFLabelPerimColor,
		 NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(zerof_lbl.perim_lcolor),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNstZeroFLabelPerimThicknessF,NhlCstZeroFLabelFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.perim_lthick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNstZeroFLabelZone,NhlCstZeroFLabelZone,NhlTInteger,
		 sizeof(int),Oset(zerof_lbl_rec.zone),NhlTImmediate,
		 _NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNstZeroFLabelSide,NhlCstZeroFLabelSide,NhlTPosition,
		 sizeof(NhlPosition),Oset(zerof_lbl_rec.side),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlBOTTOM),0,NULL},
	{NhlNstZeroFLabelJust,NhlCstZeroFLabelJust,
		 NhlTJustification,sizeof(NhlJustification),
		 Oset(zerof_lbl_rec.just),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNstZeroFLabelParallelPosF,NhlCstZeroFLabelParallelPosF,NhlTFloat,
		 sizeof(float),Oset(zerof_lbl_rec.para_pos),NhlTString,
		 _NhlUSET("0.0"),0,NULL},
	{NhlNstZeroFLabelOrthogonalPosF,NhlCstZeroFLabelOrthogonalPosF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl_rec.ortho_pos),
		 NhlTString,_NhlUSET("0.0"),0,NULL},

/* General numerical string format option */

	{NhlNstMagnitudeScalingMode,NhlCstMagnitudeScalingMode,
                 NhlTScalingMode,sizeof(NhlScalingMode),
                 Oset(mag_scale.mode),NhlTImmediate,
                 _NhlUSET((NhlPointer) NhlSCALEFACTOR),0,NULL},
        {NhlNstMagnitudeScaleValueF,NhlCstMagnitudeScaleValueF,
                 NhlTFloat,sizeof(float),Oset(mag_scale.scale_value),
                 NhlTString,_NhlUSET("1.0"),0,NULL},
        {NhlNstMagnitudeScaleFactorF,NhlCstMagnitudeScaleFactorF,
                 NhlTFloat,sizeof(float),Oset(mag_scale.scale_factor),
                 NhlTString,_NhlUSET("1.0"),_NhlRES_GONLY,NULL},
	{NhlNstMagnitudeFormat,NhlCstMagnitudeFormat,
		 NhlTString,sizeof(NhlString),
		 Oset(mag_scale.format.fstring),NhlTImmediate,
		 _NhlUSET("*+^sg"),0,(NhlFreeFunc)NhlFree},

	{NhlNstScalarValueScalingMode,NhlCstScalarValueScalingMode,
                 NhlTScalingMode,sizeof(NhlScalingMode),
                 Oset(svalue_scale.mode),NhlTImmediate,
                 _NhlUSET((NhlPointer) NhlSCALEFACTOR),0,NULL},
        {NhlNstScalarValueScaleValueF,NhlCstScalarValueScaleValueF,
                 NhlTFloat,sizeof(float),Oset(svalue_scale.scale_value),
                 NhlTString,_NhlUSET("1.0"),0,NULL},
        {NhlNstScalarValueScaleFactorF,NhlCstScalarValueScaleFactorF,
                 NhlTFloat,sizeof(float),Oset(svalue_scale.scale_factor),
                 NhlTString,_NhlUSET("1.0"),_NhlRES_GONLY,NULL},
	{NhlNstScalarValueFormat,NhlCstScalarValueFormat,
		 NhlTString,sizeof(NhlString),
		 Oset(svalue_scale.format.fstring),NhlTImmediate,
		 _NhlUSET("*+^sg"),0,(NhlFreeFunc)NhlFree},

	{NhlNstExplicitLabelBarLabelsOn,NhlCstExplicitLabelBarLabelsOn,
		 NhlTBoolean,sizeof(NhlBoolean),
		 Oset(explicit_lbar_labels_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNstLabelBarEndLabelsOn,NhlCstLabelBarEndLabelsOn,
		 NhlTBoolean,sizeof(NhlBoolean),
		 Oset(lbar_end_labels_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},

/* General label resources */

	{NhlNstLabelsOn,NhlCstLabelsOn,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(lbls.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNstLabelsUseStreamlineColor,NhlCstLabelsUseStreamlineColor,
		 NhlTBoolean,
		 sizeof(NhlBoolean),Oset(labels_use_vec_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNstLabelFontColor,NhlCstLabelFontColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(lbls.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(lbls.height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
        {NhlNstLabelFontHeightF,NhlCstLabelFontHeightF,
		 NhlTFloat,sizeof(float),Oset(lbls.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset),0,NULL},

/* End-documented-resources */

	{NhlNstDataChanged,NhlCstDataChanged,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(data_changed),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},

/* Intercepted resources */

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(x_min_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNtrXMinF,NhlCtrXMinF,NhlTFloat,sizeof(float),
		 Oset(x_min),NhlTProcedure,
		 _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(x_max_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNtrXMaxF,NhlCtrXMaxF,NhlTFloat,sizeof(float),
		 Oset(x_max),NhlTProcedure,
		 _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{ NhlNtrXLog,NhlCtrXLog,NhlTBoolean,sizeof(NhlBoolean),
		Oset(x_log),NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{ NhlNtrXReverse,NhlCtrXReverse,NhlTBoolean,sizeof(NhlBoolean),
		Oset(x_reverse),
		  NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(y_min_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtrYMinF,NhlCtrYMinF,NhlTFloat,sizeof(float),
		Oset(y_min),NhlTProcedure,
		 _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(y_max_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtrYMaxF,NhlCtrYMaxF,NhlTFloat,sizeof(float),
		Oset(y_max),NhlTProcedure,
		 _NhlUSET((NhlPointer)ResourceUnset),0,NULL},
	{ NhlNtrYLog,NhlCtrYLog,NhlTBoolean,sizeof(NhlBoolean),
		Oset(y_log),NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{ NhlNtrYReverse,NhlCtrYReverse,NhlTBoolean,sizeof(NhlBoolean),
		Oset(y_reverse),
		NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},

	{ NhlNpmLabelBarDisplayMode,NhlCpmLabelBarDisplayMode,
		 NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		 Oset(display_labelbar),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{NhlNlbLabelStrings, NhlClbLabelStrings, NhlTStringGenArray,
		 sizeof(NhlPointer),Oset(lbar_labels_res),
		 NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
		 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNlbLabelFuncCode, NhlClbLabelFuncCode, NhlTCharacter,
		 sizeof(char),Oset(lbar_func_code),
		 NhlTString,_NhlUSET(":"),0,NULL },
	{NhlNlbLabelAlignment,NhlClbLabelAlignment,NhlTlbLabelAlignmentMode, 
		 sizeof(NhllbLabelAlignmentMode), 
		 Oset(lbar_alignment),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlINTERIOREDGES),0,NULL},
		
	{NhlNpmLegendDisplayMode,NhlCpmLegendDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_legend),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),0,NULL},
	{NhlNpmTickMarkDisplayMode,NhlCpmTickMarkDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_tickmarks),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlCONDITIONAL),0,NULL},
	{NhlNpmTitleDisplayMode,NhlCpmTitleDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_titles),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlCONDITIONAL),0,NULL},
	{ NhlNpmUpdateReq,NhlCpmUpdateReq,NhlTInteger,sizeof(int),
		  Oset(update_req),
		  NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL}
};
#undef Oset


typedef enum _stCoord { stXCOORD, stYCOORD} stCoord;


/* base methods */

static NhlErrorTypes StreamlinePlotClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes StreamlinePlotClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes StreamlinePlotInitialize(
#if	NhlNeedProto
        NhlClass,  /* class */
        NhlLayer,       /* req */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes StreamlinePlotSetValues(
#if	NhlNeedProto
        NhlLayer,       /* old */
        NhlLayer,       /* reference */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);


static NhlErrorTypes    StreamlinePlotGetValues(
#if	NhlNeedProto
	NhlLayer,       /* l */
	_NhlArgList,    /* args */
	int             /* num_args */
#endif
);

static NhlErrorTypes StreamlinePlotDestroy(
#if	NhlNeedProto
        NhlLayer        /* inst */
#endif
);

static NhlErrorTypes StreamlinePlotGetBB(
#if	NhlNeedProto
        NhlLayer        instance,
        NhlBoundingBox	*thebox
#endif
);

static NhlErrorTypes StreamlinePlotPreDraw(
#if	NhlNeedProto
        NhlLayer	/* layer */
#endif
);

static NhlErrorTypes StreamlinePlotDraw(
#if	NhlNeedProtof
        NhlLayer	/* layer */
#endif
);

static NhlErrorTypes StreamlinePlotPostDraw(
#if	NhlNeedProto
        NhlLayer	/* layer */
#endif
);


static NhlErrorTypes stDraw(
#if	NhlNeedProto
        NhlStreamlinePlotLayer	stl,
	NhlDrawOrder	order
#endif
);

static NhlErrorTypes stInitSegment(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stl,
	NhlTransDat	**seg_dat,
	NhlString	entry_name
#endif
);

static NhlErrorTypes stSegDraw(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stl,
	NhlDrawOrder	order
#endif
);

static NhlErrorTypes stInitDraw(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stl,
	NhlString	entry_name
#endif
);

static NhlErrorTypes StreamlinePlotUpdateData(
#if	NhlNeedProto
	NhlDataCommLayer	new,
	NhlDataCommLayer	old
#endif
);

static NhlErrorTypes StreamlinePlotDataInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

/* internal static functions */

static NhlErrorTypes InitCoordBounds(
#if	NhlNeedProto
        NhlStreamlinePlotLayerPart	*stp,
	char			*entry_name
#endif
);

static NhlErrorTypes SetUpLLTransObj(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init
#endif
);

static NhlErrorTypes SetCoordBounds(
#if	NhlNeedProto
	NhlStreamlinePlotLayerPart	*stp,
	stCoord			ctype,
	int			count,
	NhlString		entry_name
#endif
);

static NhlErrorTypes SetUpIrrTransObj(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init
#endif
);

static NhlErrorTypes SetFormat(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init
#endif
);

static NhlErrorTypes ManageLabels(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes SetScale(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlstScaleInfo		*sip,
	NhlstScaleInfo		*osip,
	NhlBoolean		do_levels,
	NhlBoolean		init
#endif
);

static NhlErrorTypes ManageOverlay(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes ManageTickMarks(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);


static NhlErrorTypes ManageTitles(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);


static NhlErrorTypes ManageLabelBar(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes ManageZeroFLabel(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
#endif
);

static NhlErrorTypes ManageAnnotation(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew,
	NhlBoolean		init,
	NhlAnnotationRec	*rec,
	NhlAnnotationRec	*orec,
	int			*idp,
	NhlBoolean		on
#endif
);

static NhlErrorTypes SetTextPosition(
#if	NhlNeedProto
	NhlStreamlinePlotLayer		stnew,
	NhlStreamlinePlotLayerPart	*ostp,
	_stAnnoType		atype,
	NhlBoolean		*pos_changed,
	NhlString		entry_name
#endif
);

static NhlErrorTypes ReplaceSubstitutionChars(
#if	NhlNeedProto
	NhlStreamlinePlotLayerPart	*stp,
	NhlStreamlinePlotLayerPart	*ostp,
	NhlBoolean		init,
	_stAnnoType		atype,
	NhlBoolean		*text_changed,
	NhlString		entry_name
#endif
);


static NhlErrorTypes SetFormatRec(
#if	NhlNeedProto
	NhlFormatRec	*format,
	NhlString	resource,
	NhlString	entry_name
#endif
);

static NhlErrorTypes    SetupLevels(
#if	NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
        NhlBoolean	init,					    
	float		**levels,
	NhlBoolean	*modified				    
#endif
);

static NhlErrorTypes    SetupLevelsManual(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew, 
	NhlStreamlinePlotLayer	stold,
	float			**levels,
        float			min,
	float			max,
	char			*entry_name
#endif
);

static NhlErrorTypes    SetupLevelsEqual(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew, 
	NhlStreamlinePlotLayer	stold,
	float			**levels,
        float			min,
	float			max,
	char			*entry_name
#endif
);

static NhlErrorTypes    SetupLevelsAutomatic(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew, 
	NhlStreamlinePlotLayer	stold,
	float			**levels,
        float			min,
	float			max,
	char			*entry_name
#endif
);

static NhlErrorTypes    SetupLevelsExplicit(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew, 
	NhlStreamlinePlotLayer	stold,
	NhlBoolean		init,
	float			**levels,
        float			min,
	float			max,
	char			*entry_name
#endif
);

static NhlErrorTypes ChooseSpacingLin(
#if	NhlNeedProto
	float		*tstart,
	float		*tend,
	float		*spacing,
	int		convert_precision,
	int		max_ticks,
	NhlString	entry_name
#endif
);

static NhlErrorTypes    ManageVectorData(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew, 
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    ManageScalarData(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	stnew, 
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    ManageViewDepResources(
#if	NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
        NhlBoolean	init,					    
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    AdjustText(
#if	NhlNeedProto
	NhlstLabelAttrs *lbl_attrp,
	NhlStreamlinePlotLayer	new, 
	NhlStreamlinePlotLayer	old,
	NhlBoolean	init
#endif
);

static NhlErrorTypes    ManageDynamicArrays(
#if	NhlNeedProto
	NhlLayer	new,
	NhlLayer	old,
        NhlBoolean	init,					    
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes    ManageGenArray(
#if	NhlNeedProto
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

static NhlErrorTypes	CheckColorArray(
#if	NhlNeedProto
	NhlStreamlinePlotLayer	cl,
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
#if	NhlNeedProto
        NhlGenArray     ga,
        int             length
#endif
);

extern void (_NHLCALLF(hlustmpxy,HLUSTMPXY))(
#if	NhlNeedProto
	float *xda,
	float *yda, 
	float *xus, 
	float *yus, 
	int   *ist
#endif
);

extern void (_NHLCALLF(stmpxy,STMPXY))(
#if	NhlNeedProto
	float *xda,
	float *yda, 
	float *xus, 
	float *yus, 
	int   *ist
#endif
);

extern void (_NHLCALLF(hlustimxy,HLUSTIMXY))(
#if	NhlNeedProto
	float *xus,
	float *yus, 
	float *xda, 
	float *yda, 
	int   *ist
#endif
);

extern void (_NHLCALLF(stimxy,STIMXY))(
#if	NhlNeedProto
	float *xus,
	float *yus, 
	float *xda, 
	float *yda, 
	int   *ist
#endif
);

extern void (_NHLCALLF(hlustmpta,HLUSTMPTA))(
#if	NhlNeedProto
	float *xda, 
	float *yda, 
	float *xus,
	float *yus, 
	float *xnd,
	float *ynd, 
	float *du,
	float *dv, 
	float *ta, 
	int *ist
#endif
);

extern void (_NHLCALLF(stmpta,STMPTA))(
#if	NhlNeedProto
	float *xda, 
	float *yda, 
	float *xus,
	float *yus, 
	float *xnd,
	float *ynd, 
	float *du,
	float *dv, 
	float *ta, 
	int *ist
#endif
);

extern void (_NHLCALLF(stgetmapinfo,STGETMAPINFO))(
#if	NhlNeedProto
 int *imp,
 int *itr,
 float *vnl,
 float *dfm,
 float *xmn,
 float *xmx,
 float *ymn,
 float *ymx,
 float *xdl,
 float *xdh,
 float *ydl,
 float *ydh
#endif
);

static void   load_hlust_routines(
#if	NhlNeedProto
	NhlBoolean	flag
#endif
);

NhlStreamlinePlotDataDepClassRec NhlstreamlinePlotDataDepClassRec = {
	/* base_class */
        {
/* class_name			*/	"streamlinePlotDataDepClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlStreamlinePlotDataDepLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)
						&NhldataSpecClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	data_resources,
/* num_resources		*/	NhlNumber(data_resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	StreamlinePlotDataInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	},
	/* dataspec_class */
	{
/* foo				*/	0
	},
	/* streamline datadep_class */
	{
/* foo				*/	0
	}
};

NhlStreamlinePlotClassRec NhlstreamlinePlotClassRec = {
	/* base_class */
        {
/* class_name			*/      "streamlinePlotClass",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlStreamlinePlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/  (NhlClass)&NhldataCommClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	StreamlinePlotClassPartInitialize,
/* class_initialize		*/	StreamlinePlotClassInitialize,
/* layer_initialize		*/	StreamlinePlotInitialize,
/* layer_set_values		*/	StreamlinePlotSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	StreamlinePlotGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	StreamlinePlotDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      StreamlinePlotDraw,

/* layer_pre_draw		*/      StreamlinePlotPreDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      StreamlinePlotPostDraw,
/* layer_clear			*/      NULL

        },
	/* view_class */
	{
/* segment_wkid			*/	0,
/* get_bb			*/	StreamlinePlotGetBB
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
/* update_data			*/	StreamlinePlotUpdateData
	},
	{
/* foo				*/	NULL
	}
};
	

NhlClass NhlstreamlinePlotDataDepClass =
		(NhlClass) &NhlstreamlinePlotDataDepClassRec;
NhlClass NhlstreamlinePlotClass = 
		(NhlClass) &NhlstreamlinePlotClassRec;

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qcolorindex = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark	Qlevels = NrmNULLQUARK; 
static NrmQuark	Qstreamline_colors = NrmNULLQUARK; 
static NrmQuark	Qmax_magnitude_format = NrmNULLQUARK; 
static NrmQuark	Qmax_svalue_format = NrmNULLQUARK; 
static NrmQuark	Qno_data_label_string = NrmNULLQUARK; 
static NrmQuark	Qzerof_label_string = NrmNULLQUARK; 
static NrmQuark	Qlb_label_strings = NrmNULLQUARK;

static char *InitName = "StreamlinePlotInitialize";
static char *SetValuesName = "StreamlinePlotSetValues";

static NhlStreamlinePlotLayer	Stl = NULL;
static NhlStreamlinePlotLayerPart	*Stp = NULL;
static NhlBoolean		Need_Info;
static NhlBoolean		Over_Map;
static NhlLayer			Trans_Obj,Overlay_Trans_Obj;
static int			Imap,Itrt;
static float			Vnml,Dfmg;
static float			Wxmn,Wxmx,Wymn,Wymx;
static float			Xdlo,Xdhi,Ydlo,Ydhi;

/*
 * Function:	nhlfstreamlineplotlayerclass
 *
 * Description:	fortran ref to streamline class
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
_NHLCALLF(nhlfstreamlineplotclass,NHLFSTREAMLINEPLOTCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlstreamlinePlotClass;
}

/*
 * Function:	nhlfstreamlineplotdatadeplayerclass
 *
 * Description:	fortran ref to streamlineplot datadep class
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
_NHLCALLF(nhlfstreamlineplotdatadepclass,NHLFSTREAMLINEPLOTDATADEPCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlstreamlinePlotDataDepClass;
}

/*
 * Function:	StreamlinePlotDataInitialize
 *
 * Description:	Initializes the StreamlinePlotData Dependent class instance.
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
StreamlinePlotDataInitialize
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
	NhlErrorTypes	ret = NhlNOERROR;

	return ret;
}


/*
 * Function:	StreamlinePlotUpdateData
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
/*ARGSUSED*/
static NhlErrorTypes
StreamlinePlotUpdateData
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
	NhlErrorTypes		ret = NhlNOERROR;

/*
 * For now simply call SetValues setting the data changed resource true
 */
	NhlVASetValues(new->base.id,NhlNstDataChanged,True,
		       NULL);

	return ret;
}

/*
 * Function:	StreamlinePlotClassInitialize
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
StreamlinePlotClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{


	load_hlust_routines(False);

	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qcolorindex = NrmStringToQuark(NhlTColorIndex);
	Qlevels = NrmStringToQuark(NhlNstLevels);
	Qstreamline_colors = NrmStringToQuark(NhlNstStreamlineColors);
	Qmax_magnitude_format = NrmStringToQuark(NhlNstMagnitudeFormat);
	Qmax_svalue_format = NrmStringToQuark(NhlNstScalarValueFormat);
	Qzerof_label_string = NrmStringToQuark(NhlNstZeroFLabelString);
	Qno_data_label_string = NrmStringToQuark(NhlNstNoDataLabelString);
	Qlb_label_strings = NrmStringToQuark(NhlNlbLabelStrings);

	return NhlNOERROR;
}

/*
 * Function:	StreamlinePlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlStreamlinePlotClassPart that cannot be initialized statically.
 *		Calls _NhlRegisterChildClass for the overlay manager object.
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
StreamlinePlotClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	NhlClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;
	char		*entry_name = "StreamlinePlotClassPartInitialize";

/*
 * Register children objects
 */
	subret = _NhlRegisterChildClass(lc,NhlplotManagerClass,
					False,False,
					NhlNpmLegendDisplayMode,
					NhlNpmLabelBarDisplayMode,
					NhlNpmTickMarkDisplayMode,
					NhlNpmTitleDisplayMode,
					NhlNlbBoxCount,
					NhlNlbLabelAlignment,
					NhlNlbLabelStrings,
					NhlNlbLabelFuncCode,
					NhlNlbMonoFillColor,
					NhlNlbFillColor,
					NhlNlbFillColors,
					NhlNlbMonoFillPattern,
					NhlNlbFillPattern,
					NhlNlbFillPatterns,
					NhlNlbMonoFillScale,
					NhlNlbFillScaleF,
					NhlNlbFillScales,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlplotManagerClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterDataRes((NhlDataCommClass)lc,
				     NhlNstVectorFieldData,
				     NULL,
				     NhlstreamlinePlotDataDepClass,
				     NhlvectorFieldFloatClass,
				     NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering data resource %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlcoordArrTableFloatClass");
		return(NhlFATAL);
	}


	subret = _NhlRegisterDataRes((NhlDataCommClass)lc,
				     NhlNstScalarFieldData,
				     NULL,
				     NhlstreamlinePlotDataDepClass,
				     NhlscalarFieldFloatClass,
				     NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering data resource %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlcoordArrTableFloatClass");
		return(NhlFATAL);
	}

	return ret;
}

/*
 * Function:	StreamlinePlotInitialize
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
StreamlinePlotInitialize
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
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = InitName;
	char			*e_text;
	NhlStreamlinePlotLayer	stnew = (NhlStreamlinePlotLayer) new;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlSArg			sargs[64];
	int			nargs = 0;

	stp->zerof_anno_id = NhlNULLOBJID;
	stp->zerof_lbl_rec.id = NhlNULLOBJID;

/* Initialize unset resources */

	if (! stp->arrow_length_set) stp->arrow_length = 0.008;
        if (! stp->step_size_set) stp->step_size = 0.012;
	if (! stp->min_line_spacing_set) stp->min_line_spacing = 0.01;
	if (! stp->min_arrow_spacing_set) stp->min_arrow_spacing = 0.0;
	if (! stp->min_line_length_set) stp->min_line_length = 0.0;
	       
	if (! stp->level_spacing_set) stp->level_spacing = 5.0;
	if (! stp->min_level_set) stp->min_level_val = -FLT_MAX;
	if (! stp->max_level_set) stp->max_level_val = FLT_MAX;

	if (! stp->lbls.height_set) 
		stp->lbls.height = 0.010;
	if (! stp->zerof_lbl.height_set) 
		stp->zerof_lbl.height = 0.01;
	if (! stp->x_min_set)
		stp->x_min = 0.0;
	if (! stp->x_max_set)
		stp->x_max = 1.0;
	if (! stp->y_min_set)
		stp->y_min = 0.0;
	if (! stp->y_max_set)
		stp->y_max = 1.0;

/* Initialize private members */

	stp->lbls.fcode[1] = '\0';
	stp->zerof_lbl.fcode[1] = '\0';
	stp->new_draw_req = True;
	stp->predraw_dat = NULL;
	stp->draw_dat = NULL;
	stp->postdraw_dat = NULL;
	stp->update_req = False;
	stp->overlay_object = NULL;
	stp->data_changed = True;
	stp->data_init = False;
	stp->scalar_data_init = False;
	stp->use_irr_trans = False;
	stp->zero_field = False;
	stp->lbar_labels_set = False;
	stp->lbar_labels = NULL;
	stp->lbar_labels_res_set = stp->lbar_labels_res ? True : False;
	stp->vfp = NULL;
	stp->ovfp = NULL;
	stp->sfp = NULL;
	stp->osfp = NULL;
	stp->fws_id = NhlNULLOBJID;

/*
 * Set up the data
 */
	subret = ManageVectorData(stnew,(NhlStreamlinePlotLayer) req,
				  True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	subret = ManageScalarData(stnew,(NhlStreamlinePlotLayer) req,
				  True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
 
	subret = InitCoordBounds(stp,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return(ret);

/* Set view dependent resources */

	subret = ManageViewDepResources(new,req,True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set the label formats - must precede dynamic array handling */

	subret = SetFormat(stnew,(NhlStreamlinePlotLayer)req,True);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting label formats";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Set up the dynamic arrays (line label array is handled here) */

	subret = ManageDynamicArrays(new,req,True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error initializing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* 
 * Set up the labels (except for the line label array) 
 * Note: may add arguments to the PlotManager argument list.
 */

	subret = ManageLabels(stnew,(NhlStreamlinePlotLayer)req,True,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error initializing labels";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Set up the streamline object transformation  */

	stp->do_low_level_log = False;
	if (stp->use_irr_trans) {
		subret = SetUpIrrTransObj(stnew,(NhlStreamlinePlotLayer) req,True);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}
	else {
		subret = SetUpLLTransObj(stnew,(NhlStreamlinePlotLayer) req,True);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}

/* 
 * Manage the PlotManager (including setting up the annotations managed by it)
 */
	subret = ManageOverlay(stnew,
			       (NhlStreamlinePlotLayer)req,True,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		return(ret);
	}

	if (stnew->trans.overlay_status != _tfNotInOverlay) {
		if (stp->zerof_lbl.on || stp->zerof_lbl.string2_on) {
			subret = ManageAnnotation(stnew,True,
						  &stp->zerof_lbl_rec,
						  NULL,
						  &stp->zerof_anno_id,
						  stp->display_zerof_no_data);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
		}
	}

	stp->data_changed = False;
	stp->level_spacing_set = False;
	stp->lbls.height_set = False;
	stp->zerof_lbl.height_set = False;
	stp->lbar_labels_res_set = False;
	stp->arrow_length_set = False;
	stp->step_size_set = False;
	stp->min_line_spacing_set = False;
	stp->min_arrow_spacing_set = False;
	stp->min_line_length_set = False;

	return ret;
}

/*
 * Function:	StreamlinePlotSetValues
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
static NhlErrorTypes StreamlinePlotSetValues
#if	NhlNeedProto
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
	char			*entry_name = SetValuesName;
	char			*e_text;
	NhlStreamlinePlotLayer		stnew = (NhlStreamlinePlotLayer) new;
 	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayer		stold = (NhlStreamlinePlotLayer) old;
 	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	/* Note that ManageLabelBar add to sargs */
	NhlSArg			sargs[128];
	int			nargs = 0;

	if (stnew->view.use_segments != stold->view.use_segments) {
		stnew->view.use_segments = stold->view.use_segments;
		ret = MIN(ret,NhlWARNING);
		e_text = "%s: attempt to set create-only resource overridden";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
	}

	if (_NhlArgIsSet(args,num_args,NhlNstArrowLengthF))
		stp->arrow_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNstStepSizeF))
		stp->step_size_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNstMinLineSpacingF))
		stp->min_line_spacing_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNstMinArrowSpacingF))
		stp->min_arrow_spacing_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNstMinLineLengthF))
		stp->min_line_length_set = True;

	if (_NhlArgIsSet(args,num_args,NhlNstLevelSpacingF))
		stp->level_spacing_set = True;

	if (_NhlArgIsSet(args,num_args,NhlNstLabelFontHeightF))
		stp->lbls.height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNstZeroFLabelFontHeightF))
		stp->zerof_lbl.height_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtrXMinF))
		stp->x_min_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtrXMaxF))
		stp->x_max_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtrYMinF))
		stp->y_min_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNtrYMaxF))
		stp->y_max_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNlbLabelStrings))
		stp->lbar_labels_res_set = True;

	if (_NhlArgIsSet(args,num_args,NhlNstMinLevelValF))
		stp->min_level_set = True;
	else if (stp->use_scalar_array != ostp->use_scalar_array) {
		stp->min_level_val = -FLT_MAX;
		stp->min_level_set = False;
	}

	if (_NhlArgIsSet(args,num_args,NhlNstMaxLevelValF))
		stp->max_level_set = True;
	else if (stp->use_scalar_array != ostp->use_scalar_array) {
		stp->max_level_val = FLT_MAX;
		stp->max_level_set = False;
	}

/* Manage the data */

	subret = ManageVectorData(stnew,stold,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	subret = ManageScalarData(stnew,stold,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

	subret = InitCoordBounds(stp,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return(ret);

/* Set view dependent resources */

	subret = ManageViewDepResources(new,old,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Set the label formats - must precede dynamic array handling */

	subret = SetFormat(stnew,stold,False);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting label formats";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Manage the dynamic arrays  (line labels handled here) */

	subret = ManageDynamicArrays(new,old,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error initializing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* 
 * Set up the labels (except for the line label array) 
 * Note: may add arguments to the PlotManager argument list.
 */

	subret = ManageLabels(stnew,stold,False,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing labels";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Set up the streamline object transformation  */

	stp->do_low_level_log = False;
	if (stp->use_irr_trans) {
		subret = SetUpIrrTransObj(stnew,
					  (NhlStreamlinePlotLayer) old,False);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}
	else {
		subret = SetUpLLTransObj(stnew,(NhlStreamlinePlotLayer) old,False);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}
/* 
 * Manage the PlotManager (including the PlotManager annotations)
 */
	subret = ManageOverlay(stnew,stold,False,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		return(ret);
	}

	stp->update_req = False;
	stp->data_changed = False;
	stp->level_spacing_set = False;
	stp->lbls.height_set = False;
	stp->zerof_lbl.height_set = False;
	stp->lbar_labels_res_set = False;
	stp->arrow_length_set = False;
	stp->step_size_set = False;
	stp->min_line_spacing_set = False;
	stp->min_arrow_spacing_set = False;
	stp->min_line_length_set = False;

	return ret;
}

/*
 * Function:    StreamlinePlotGetValues
 *
 * Description: Retrieves the current setting of StreamlinePlot resources.
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
 *              NhlNstLevels
 *              NhlNstLineColors
 *              NhlNstLineThicknesses
 *		NhlNstLineLabelStrings
 *		NhlNstLineLabelFontColors
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    StreamlinePlotGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlStreamlinePlotLayer cl = (NhlStreamlinePlotLayer)l;
        NhlStreamlinePlotLayerPart *stp = &(cl->streamlineplot);
        NhlGenArray ga;
	NhlString ts;
        char *e_text;
        int i, count = 0;
        char *type = "";

        for( i = 0; i< num_args; i++ ) {

                ga = NULL;
                if(args[i].quark == Qlevels) {
                        ga = stp->levels;
                        count = stp->level_count;
                        type = NhlNstLevels;
                }
                else if (args[i].quark == Qstreamline_colors) {
                        ga = stp->streamline_colors;
                        count = stp->level_count + 1;
                        type = NhlNstStreamlineColors;
                }
                if (ga != NULL) {
                        if ((ga = GenArraySubsetCopy(ga, count)) == NULL) {
                                e_text = "%s: error copying %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          "StreamlinePlotGetValues",type);
                                return NhlFATAL;
                        }
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
			continue;
                }

		ts = NULL;
		if (args[i].quark == Qmax_magnitude_format){
			ts = stp->mag_scale.format.fstring;
		}
		else if (args[i].quark == Qmax_svalue_format){
			ts = stp->svalue_scale.format.fstring;
		}
		else if(args[i].quark == Qno_data_label_string){
			ts = stp->zerof_lbl.string2;
		}
		else if(args[i].quark == Qzerof_label_string){
			ts = stp->zerof_lbl.string1;
		}
                if (ts != NULL) {
			*((NhlString*)(args[i].value.ptrval)) =
				NhlMalloc(strlen(ts)+1);
			if(!*((NhlString*)(args[i].value.ptrval))){
                                e_text = "%s: error copying String";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  "StreamlinePlotGetValues");
                                return NhlFATAL;
                        }
			strcpy(*((NhlString*)(args[i].value.ptrval)),ts);
			continue;
                }
		ga = NULL;
		if (args[i].quark == Qlb_label_strings){
			if (stp->overlay_object != NULL)
				NhlVAGetValues(stp->overlay_object->base.id,
					       NhlNlbLabelStrings,&ga,NULL);
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
        }

        return(NhlNOERROR);
}


/*
 * Function:	InitCoordBounds
 *
 * Description: Sets coordinate boundary variables that must be 
 *              initialized regardless of whether any data has been supplied
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
static NhlErrorTypes InitCoordBounds
#if	NhlNeedProto
(
        NhlStreamlinePlotLayerPart	*stp,
	char			*entry_name
)
#else
(stp,entry_name)
        NhlStreamlinePlotLayerPart	*stp;
	char			*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	float		ftmp;

	if (stp->data_init) {
		if (! stp->x_min_set)
			stp->x_min = MIN(stp->vfp->x_start,stp->vfp->x_end);
		if (! stp->x_max_set)
			stp->x_max = MAX(stp->vfp->x_start,stp->vfp->x_end);
		if (! stp->y_min_set)
			stp->y_min = MIN(stp->vfp->y_start,stp->vfp->y_end);
		if (! stp->y_max_set)
			stp->y_max = MAX(stp->vfp->y_start,stp->vfp->y_end);
	}

	if (stp->x_min == stp->x_max) {
		e_text = "%s: Zero X coordinate span: defaulting";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		stp->x_min = 0.0; 
		stp->x_max = 1.0;
	}
	else if (stp->x_min > stp->x_max) {
		e_text = "%s: Min X coordinate exceeds max: exchanging";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ftmp = stp->x_min;
		stp->x_min = stp->x_max;
		stp->x_max = ftmp;
	}
	if (stp->x_log && stp->x_min <= 0.0) {
		e_text = 
		   "%s: Log style invalid for X coordinates: setting %s off";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,NhlNtrXLog);
		stp->x_log = False;
	}

	if (stp->y_min == stp->y_max) {
		e_text = "%s: Zero Y coordinate span: defaulting";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		stp->y_min = 0.0; 
		stp->y_max = 1.0;
	}
	else if (stp->y_min > stp->y_max) {
		e_text = "%s: Min Y coordinate exceeds max: exchanging";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ftmp = stp->y_min;
		stp->y_min = stp->y_max;
		stp->y_max = ftmp;
	}
	if (stp->y_log && stp->y_min <= 0.0) {
		e_text = 
		    "%s: Log style invalid for Y coordinates: setting %s off";
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,NhlNtrYLog);
		stp->y_log = False;
	}
	if (! stp->data_init) {
		if (! stp->x_reverse) {
			stp->xc1 = stp->x_min;
			stp->xcm = stp->x_max;
		}
		else {
			stp->xc1 = stp->x_max;
			stp->xcm = stp->x_min;
		}
		if (! stp->y_reverse) {
			stp->yc1 = stp->y_min;
			stp->ycn = stp->y_max;
		}
		else {
			stp->yc1 = stp->y_max;
			stp->ycn = stp->y_min;
		}
	}
	return ret;
}

/*
 * Function:  GenArraySubsetCopy
 *
 * Description: Since the internal GenArrays maintained by StreamlinePlot 
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
#if	NhlNeedProto
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
	gto->my_data = True;
        return gto;
}


/*
 * Function:	StreamlinePlotDestroy
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
static NhlErrorTypes StreamlinePlotDestroy
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
NhlLayer inst;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlStreamlinePlotLayerPart	*stp = 
		&(((NhlStreamlinePlotLayer) inst)->streamlineplot);
	NhlTransformLayerPart	*sttp = &(((NhlTransformLayer) inst)->trans);
	int			ovbase_id;

/*
 * Note that the transform layer and the streamline layer overlay objects
 * may be the same or different. The code must handle either case.
 */

	if (sttp->overlay_status == _tfCurrentOverlayMember ||
	    sttp->overlay_status == _tfCurrentOverlayBase) {
		if (stp->zerof_anno_id != NhlNULLOBJID) {
			subret = _NhlUnregisterAnnotation(sttp->overlay_object,
					 _NhlGetLayer(stp->zerof_anno_id),
							  NULL);
			if ((ret = MIN(subret,ret)) < NhlWARNING)
				return NhlFATAL;
		}
	}
	if (sttp->overlay_status == _tfCurrentOverlayMember) {
		ovbase_id = sttp->overlay_object->base.parent->base.id;
		subret = NhlRemoveOverlay(ovbase_id,inst->base.id,False);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return NhlFATAL;
	}

	if (stp->overlay_object != NULL) {
		(void) _NhlDestroyChild(stp->overlay_object->base.id,inst);
		stp->overlay_object = NULL;
	}
	if (sttp->trans_obj != NULL) {
		(void) NhlDestroy(sttp->trans_obj->base.id);
		sttp->trans_obj = NULL;
	}
	if (stp->zerof_anno_id != NhlNULLOBJID) {
		(void) NhlDestroy(stp->zerof_anno_id);
	}
	if (stp->zerof_lbl_rec.id != NhlNULLOBJID) {
 		(void) NhlDestroy(stp->zerof_lbl_rec.id);
	}

	NhlFreeGenArray(stp->levels);
	NhlFreeGenArray(stp->streamline_colors);
	NhlFree(stp->gks_streamline_colors);
	if (stp->ovfp != NULL)
		NhlFree(stp->ovfp);
	if (stp->osfp != NULL)
		NhlFree(stp->osfp);

	if (stp->lbar_labels != NULL) {
		NhlFreeGenArray(stp->lbar_labels);
	}
	if (stp->level_strings != NULL) {
		int i;
		for (i = 0; i < stp->level_count; i++) {
			if (stp->level_strings[i] != NULL)
				NhlFree(stp->level_strings[i]);
		}
		NhlFree(stp->level_strings);
	}
	
        if (stp->mag_scale.format.fstring != NULL)
                NhlFree(stp->mag_scale.format.fstring);
        if (stp->svalue_scale.format.fstring != NULL)
                NhlFree(stp->svalue_scale.format.fstring);

        if (stp->zerof_lbl.string2 != NULL)
                NhlFree(stp->zerof_lbl.string2);
        if (stp->zerof_lbl.string1 != NULL)
                NhlFree(stp->zerof_lbl.string1);
	if (stp->zerof_lbl.text1 != NULL)
                NhlFree(stp->zerof_lbl.text1);
	if (stp->zerof_lbl.text2 != NULL)
                NhlFree(stp->zerof_lbl.text2);

	return(ret);
}


/*
 * Function:    StreamlinePlotGetBB
 *
 * Description: 
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
static NhlErrorTypes StreamlinePlotGetBB
#if	NhlNeedProto
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	char			*entry_name  = "StreamlinePlotGetBB";
	char			*e_text;
	NhlStreamlinePlotLayer		stl = (NhlStreamlinePlotLayer) instance;
	NhlStreamlinePlotLayerPart	*stp = &(stl->streamlineplot);
	NhlTransformLayerPart	*sttp = &(((NhlTransformLayer)stl)->trans);
	NhlViewLayerPart	*stvp = &(((NhlViewLayer) stl)->view);
	float			x,y,width,height;

	if (! _NhlIsTransform(instance)) {
		e_text = "%s: invalid object id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
/*
 * If the StreamlinePlot object is a overlay base, return the bounding box
 * of the complete overlay. If it is a member of an overlay, return
 * only the StreamlinePlot's viewport, since it does not 'own' any of its
 * annotations. If it is not in an overlay at all, return its viewport
 * plus the info label and constant field annotation viewports 
 * (instantiated directly by the StreamlinePlot) as appropriate.
 */
	if (sttp->overlay_status == _tfCurrentOverlayBase) {
		return _NhlGetBB(sttp->overlay_object,thebox);
	}

	_NhlAddBBInfo(stvp->y,stvp->y - stvp->height,
		      stvp->x + stvp->width,stvp->x,thebox);

	if (sttp->overlay_status == _tfCurrentOverlayMember)
		return ret;

	if (stp->zerof_anno_id != NhlNULLOBJID && 
	    stp->display_zerof_no_data) {
		subret = NhlVAGetValues(stp->zerof_anno_id,
					NhlNvpXF,&x,
					NhlNvpYF,&y,
					NhlNvpWidthF,&width,
					NhlNvpHeightF, &height, NULL);

		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		_NhlAddBBInfo(y,y-height,x+width,x,thebox);
	}

	return ret;
}

/*
 * Function:	stSegDraw
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

static NhlErrorTypes stSegDraw
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stl,
	NhlDrawOrder	order
)
#else
(stl,order)
        NhlStreamlinePlotLayer stl;
	NhlDrawOrder	order;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name  = NULL;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &(stl->streamlineplot);
	NhlTransDat		*seg_dat;

	switch (order) {
	case NhlPREDRAW:
		entry_name = "StreamlinePlotPreDraw";
		seg_dat = stp->predraw_dat;
		break;
	case NhlDRAW:
		entry_name = "StreamlinePlotDraw";
		seg_dat = stp->draw_dat;
		break;
	case NhlPOSTDRAW:
		entry_name = "StreamlinePlotPostDraw";
		seg_dat = stp->postdraw_dat;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	if (seg_dat == NULL)
		return NhlNOERROR;

	subret = _NhlActivateWorkstation(stl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = _NhlDrawSegment(seg_dat,_NhlWorkstationId(stl->base.wkptr));
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = _NhlDeactivateWorkstation(stl->base.wkptr);
	return MIN(subret,ret);
}


/*
 * Function:	stInitDraw
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

static NhlErrorTypes stInitDraw
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stl,
	NhlString	entry_name
)
#else
(stl,entry_name)
        NhlStreamlinePlotLayer stl;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlStreamlinePlotLayerPart	*stp = &(stl->streamlineplot);
	NhlString		e_text;

	stp->fws = NULL;
	stp->wk_active = False;
	stp->seg_open = False;

	NhlVASetValues(stl->base.wkptr->base.id,
		       _NhlNwkReset,	True,
		       NULL);

	if (stp->fws_id == NhlNULLOBJID) {
		if ((stp->fws_id = 
		     _NhlNewWorkspace(NhlwsOTHER,NhlwsNONE,
			       2 * stp->vfp->fast_len * stp->vfp->slow_len *
				      sizeof(float))) < 0) {
			e_text = "%s: float workspace allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	if ((stp->fws = _NhlUseWorkspace(stp->fws_id)) == NULL) {
		e_text = "%s: error reserving float workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

	return ret;
}

/*
 * Function:	stInitSegment
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

static NhlErrorTypes stInitSegment
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stl,
	NhlTransDat	**seg_dat,
	NhlString	entry_name
)
#else
(stl,seg_dat,entry_name)
        NhlStreamlinePlotLayer stl;
	NhlTransDat	**seg_dat;
	NhlString	entry_name;
#endif
{
	char			*e_text;

	if (*seg_dat != NULL)
		_NhlDeleteViewSegment((NhlLayer) stl,*seg_dat);
	if ((*seg_dat = _NhlNewViewSegment((NhlLayer) stl)) == NULL) {
		e_text = "%s: error opening segment";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	_NhlStartSegment(*seg_dat);
	stl->streamlineplot.seg_open = True;

	return NhlNOERROR;
}
/*
 * Function:	StreamlinePlotPreDraw
 *
 * Description:	
 *
 * In Args:	layer	StreamlinePlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes StreamlinePlotPreDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString	entry_name = "StreamlinePlotPreDraw";
	NhlStreamlinePlotLayer		stl = (NhlStreamlinePlotLayer) layer;
	NhlStreamlinePlotLayerPart	*stp = &stl->streamlineplot;

	Stp = stp;
	Stl = stl;

	if (! stp->data_init || stp->display_zerof_no_data) {
		Stp = NULL;
		return NhlNOERROR;
	}

	if (stl->view.use_segments && ! stp->new_draw_req) {
		ret = stSegDraw(stl,NhlPREDRAW);
		Stp = NULL;
		return ret;
	}

	subret = stInitDraw(stl,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		Stp = NULL;
		return ret;
	}

	subret = stDraw(stl,NhlPREDRAW);

	Stp = NULL;
	return MIN(subret,ret);
}

/*
 * Function:	StreamlineAbortDraw
 *
 * Description:	cleans up if a fatal error occurs while drawing
 *
 * In Args:	layer	StreamlinePlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static void StreamlineAbortDraw
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stl
)
#else
(stl)
	NhlStreamlinePlotLayer	stl;
#endif
{
	NhlStreamlinePlotLayerPart	*stp = &stl->streamlineplot;
	NhlTransformLayerPart	*tfp = &(stl->trans);
	char *e_text;

	Stp = NULL;
	Stl = NULL;

	if (stl->view.use_segments && stp->seg_open)
		_NhlEndSegment();

	if (stp->wk_active)
		_NhlDeactivateWorkstation(stl->base.wkptr);

	if (stp->do_low_level_log) 
		NhlVASetValues(tfp->trans_obj->base.id,
			       NhlNtrLowLevelLogOn,False,NULL);

	if (stp->fws != NULL) {
		_NhlIdleWorkspace(stp->fws);
		stp->fws = NULL;
	}

	e_text = "%s: draw error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,"StreamlinePlotDraw");
}

/*
 * Function:	StreamlinePlotDraw
 *
 * Description:	
 *
 * In Args:	layer	StreamlinePlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes StreamlinePlotDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes ret;
	NhlStreamlinePlotLayer	stl = (NhlStreamlinePlotLayer) layer;
	NhlStreamlinePlotLayerPart	*stp = &stl->streamlineplot;

	Stp = stp;
	Stl = stl;

	if (! stp->data_init || stp->display_zerof_no_data) {
		Stp = NULL;
		return NhlNOERROR;
	}

	if (stl->view.use_segments && ! stp->new_draw_req) {
		ret = stSegDraw(stl,NhlDRAW);
		Stp = NULL;
		return ret;
	}

	ret = stDraw((NhlStreamlinePlotLayer) layer,NhlDRAW);
	Stp = NULL;
	return ret;
}

/*
 * Function:	StreamlinePlotPostDraw
 *
 * Description:	
 *
 * In Args:	layer	StreamlinePlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes StreamlinePlotPostDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
        NhlLayer layer;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlStreamlinePlotLayer		stl = (NhlStreamlinePlotLayer) layer;
	NhlStreamlinePlotLayerPart	*stp = &stl->streamlineplot;
	NhlTransformLayerPart	*tfp = &stl->trans;

	Stp = stp;
	Stl = stl;

	if (stp->display_zerof_no_data) {
		if (tfp->overlay_status == _tfNotInOverlay) {
			subret = NhlDraw(stp->zerof_lbl_rec.id);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				Stp = NULL;
				return ret;
			}
		}
		Stp = NULL;
		return ret;
	}

	if (stl->view.use_segments && ! stp->new_draw_req) {
		ret = stSegDraw(stl,NhlPOSTDRAW);
		Stp = NULL;
		return ret;
	}

	ret = stDraw((NhlStreamlinePlotLayer) layer,NhlPOSTDRAW);
	stp->new_draw_req = False;
	Stp = NULL;

	if (stp->fws != NULL) {
		subret = _NhlIdleWorkspace(stp->fws);
		ret = MIN(subret,ret);
		stp->fws = NULL;
	}

	return ret;
}


/*
 * Function:	stDraw
 *
 * Description:	
 *
 * In Args:	layer	StreamlinePlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes stDraw
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stl,
	NhlDrawOrder	order
)
#else
(stl,order)
        NhlStreamlinePlotLayer stl;
	NhlDrawOrder	order;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = NULL;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &(stl->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stl->trans);
	NhlBoolean		low_level_log = False;
	float			*u_data,*v_data,*p_data;
	int			cix;

	if (stp->streamline_order != order)
		return NhlNOERROR;

	switch (order) {
	case NhlPREDRAW:
		entry_name = "StreamlinePlotPreDraw";
		break;
	case NhlDRAW:
		entry_name = "StreamlinePlotDraw";
		break;
	case NhlPOSTDRAW:
		entry_name = "StreamlinePlotPostDraw";
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	c_strset();
	subret = _NhlActivateWorkstation(stl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		StreamlineAbortDraw(stl);
		return NhlFATAL;
	}
	stp->wk_active = True;

	if (stl->view.use_segments) {
		switch (order) {
		case NhlPREDRAW:
			subret = stInitSegment(stl,&stp->draw_dat,
					       entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				StreamlineAbortDraw(stl);
				return ret;
			}
			break;
		case NhlDRAW:
			subret = stInitSegment(stl,&stp->draw_dat,
					       entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				StreamlineAbortDraw(stl);
				return ret;
			}
			break;
		case NhlPOSTDRAW:
			subret = stInitSegment(stl,&stp->postdraw_dat,
					       entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING) {
				StreamlineAbortDraw(stl);
				return ret;
			}
			break;
		}
	}
	
/*
 * If the plot is an overlay member, use the overlay manager's trans object.
 */
        Over_Map = False;
        Overlay_Trans_Obj = NULL;
        Trans_Obj = tfp->trans_obj;
	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    tfp->overlay_trans_obj != NULL) {
		stp->trans_obj = tfp->overlay_trans_obj;
                Overlay_Trans_Obj = tfp->overlay_trans_obj;
                if ((
                  stp->trans_obj->base.layer_class)->base_class.class_name ==
                    NhlmapTransObjClass->base_class.class_name) {
                        Over_Map = True;	
                }
		if (stp->do_low_level_log) {
			if (stp->x_log) {
				subret = NhlVASetValues(
						   tfp->trans_obj->base.id,
						NhlNtrXAxisType,NhlLINEARAXIS,
						NULL);
			}
			else {
				subret = NhlVASetValues(
						   tfp->trans_obj->base.id,
						NhlNtrYAxisType,NhlLINEARAXIS,
						NULL);
			}
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				StreamlineAbortDraw(stl);
				return(ret);
			}
		}
	}
	else {
		stp->trans_obj = tfp->trans_obj;

		if (stp->do_low_level_log) {
			subret = NhlVASetValues(stp->trans_obj->base.id,
						NhlNtrLowLevelLogOn,True,
						NULL);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				StreamlineAbortDraw(stl);
				return(ret);
			}
			low_level_log = True;
		}
		subret = _NhlSetTrans(tfp->trans_obj, (NhlLayer)stl);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			StreamlineAbortDraw(stl);
			return(ret);
 		}
	}
	
	switch (stp->vfp->miss_mode) {
	case vfBOTH:
		c_stsetr("USV",stp->vfp->u_missing_value);
		c_stsetr("VSV",stp->vfp->v_missing_value);
		c_stseti("SVF",1);
		break;
	case vfUONLY:
		c_stsetr("USV",stp->vfp->u_missing_value);
		c_stseti("SVF",1);
		break;
	case vfVONLY:
		c_stsetr("VSV",stp->vfp->v_missing_value);
		c_stseti("SVF",1);
		break;
	case vfNONE:
		c_stseti("SVF",0);
		break;
	}

	if (low_level_log && stp->x_log) {
		c_stsetr("XC1",(float)stp->vfp->x_start);
		c_stsetr("XCM",(float)stp->vfp->x_end);
	}
	else {
		c_stsetr("XC1",(float)stp->xc1);
		c_stsetr("XCM",(float)stp->xcm);
	}
	if (low_level_log && stp->y_log) {
		c_stsetr("YC1",(float)stp->vfp->y_start);
		c_stsetr("YCN",(float)stp->vfp->y_end);
	}
	else {
		c_stsetr("YC1",(float)stp->yc1);
		c_stsetr("YCN",(float)stp->ycn);
	}
        c_stseti("SET",0);
        c_vvseti("MAP",NhlstMAPVAL);

	if (stp->map_direction)
		c_stseti("TRT",1);
	else
		c_stseti("TRT",0);

	c_stseti("GBS",0);
	c_stsetr("LWD",stp->line_thickness);
	c_stsetr("ARL",stp->arrow_length / stl->view.width);
	c_stsetr("DFM",stp->step_size / stl->view.width);
	c_stsetr("SSP",stp->min_line_spacing / stl->view.width);
	c_stsetr("AMD",stp->min_arrow_spacing / stl->view.width);
/*
	c_stsetr("SMD",stp->min_line_length / stl->view.width);
*/
	c_stsetr("CDS",stp->min_step_factor);
	c_stseti("CKP",stp->length_check_count < 1 ? 
		 35 : stp->length_check_count);
	c_stseti("CKX",stp->crossover_check_count);
	c_stseti("SGD",stp->line_start_stride);
	c_stseti("AGD",stp->arrow_stride);

	cix = stp->line_color < 0 ? 
		_NhlGetGksCi(stl->base.wkptr,NhlFOREGROUND) :
		_NhlGetGksCi(stl->base.wkptr,stp->line_color);

	gset_line_colr_ind(cix);

	/* Draw the streamlines */


	gset_clip_ind(GIND_CLIP);
	Need_Info = True;

	u_data = &((float *) stp->vfp->u_arr->data)[stp->vfp->begin]; 
	v_data = &((float *) stp->vfp->v_arr->data)[stp->vfp->begin];

	if (stp->scalar_data_init) {
		p_data = &((float *) stp->sfp->d_arr->data)[stp->sfp->begin];
		subret = _NhlStinit(u_data,stp->vfp->fast_dim,
				    v_data,stp->vfp->fast_dim,
				    p_data,stp->sfp->fast_dim,
				    stp->vfp->fast_len,stp->vfp->slow_len,
				    stp->fws,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error drawing streamlines";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			StreamlineAbortDraw(stl);
			return(ret);
 		}
		subret = _NhlStream(u_data,v_data,p_data,
				    NULL,NULL,stp->fws,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error drawing streamlines";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			StreamlineAbortDraw(stl);
			return(ret);
 		}
	}
	else {
		subret = _NhlStinit(u_data,stp->vfp->fast_dim,
				    v_data,stp->vfp->fast_dim,
				    NULL,0,
				    stp->vfp->fast_len,stp->vfp->slow_len,
				    stp->fws,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error drawing streamlines";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			StreamlineAbortDraw(stl);
			return(ret);
 		}
		subret = _NhlStream(u_data,v_data,NULL,
				    NULL,NULL,stp->fws,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error drawing streamlines";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			StreamlineAbortDraw(stl);
			return(ret);
 		}
	}


	gset_clip_ind(GIND_NO_CLIP);
	if (stl->view.use_segments) {
		_NhlEndSegment();
	}

	if (low_level_log) {
		subret = NhlVASetValues(stp->trans_obj->base.id,
					NhlNtrLowLevelLogOn,False,NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			StreamlineAbortDraw(stl);
			return(ret);
		}
	}
        subret = _NhlDeactivateWorkstation(stl->base.wkptr);
	ret = MIN(subret,ret);

	return MIN(subret,ret);
}



/*
 * Function:	SetCoordBounds
 *
 * Description: Sets the max and min coord bounds
 *
 * In Args:	stp	pointer to VectorPlot layer part
 *		ctype	which coordinate
 *		count	length of irregular coord array if > 0
 *                      if 0 indicates a non-irregular transformation
 *		entry_name the high level entry point
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:   A number of fields in the VectorPlotLayerPart are set
 */

static NhlErrorTypes SetCoordBounds
#if	NhlNeedProto
(
	NhlStreamlinePlotLayerPart	*stp,
	stCoord			ctype,
	int			count,
	NhlString		entry_name
)
#else 
(stp,ctype,count,entry_name)
	NhlStreamlinePlotLayerPart	*stp;
	stCoord			ctype;
	int			count;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	float		tmin,tmax;
	NhlBoolean	rev;

	if (ctype == stXCOORD) {

		rev = stp->vfp->x_start > stp->vfp->x_end;
		if (! rev) {
			tmin = MAX(stp->vfp->x_start,stp->x_min); 
			tmax = MIN(stp->vfp->x_end,stp->x_max);
		}
		else {
			tmin = MAX(stp->vfp->x_end,stp->x_min); 
			tmax = MIN(stp->vfp->x_start,stp->x_max);
		}
		stp->xlb = tmin;
		stp->xub = tmax;

		if (count == 0) {
			stp->xc1 = stp->vfp->x_start;
			stp->xcm = stp->vfp->x_end;
		}
		else {
			stp->xc1 = 0;
			stp->xcm = count - 1;

			if (tmin > stp->x_min) {
				e_text = 
"%s: irregular transformation requires %s to be within data coordinate range: resetting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNtrXMinF);
				ret = MIN(ret,NhlWARNING);
				stp->x_min = tmin;
			}
			if (tmax < stp->x_max) {
				e_text = 
"%s: irregular transformation requires %s to be within data coordinate range: resetting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNtrXMaxF);
				ret = MIN(ret,NhlWARNING);
				stp->x_max = tmax;
			}
		}
	}
	else if (ctype == stYCOORD) {

		rev = stp->vfp->y_start > stp->vfp->y_end;
		if (! rev) {
			tmin = MAX(stp->vfp->y_start,stp->y_min); 
			tmax = MIN(stp->vfp->y_end,stp->y_max);
		}
		else {
			tmin = MAX(stp->vfp->y_end,stp->y_min); 
			tmax = MIN(stp->vfp->y_start,stp->y_max);
		}
		stp->ylb = tmin;
		stp->yub = tmax;

		if (count == 0) {
			stp->yc1 = stp->vfp->y_start;
			stp->ycn = stp->vfp->y_end;
		}
		else {
			stp->yc1 = 0;
			stp->ycn = count - 1;
			if (tmin > stp->y_min) {
				e_text = 
"%s: irregular transformation requires %s to be within data coordinate range: resetting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNtrYMinF);
				ret = MIN(ret,NhlWARNING);
				stp->y_min = tmin;
			}
			if (tmax < stp->y_max) {
				e_text = 
"%s: irregular transformation requires %s to be within data coordinate range: resetting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNtrYMaxF);
				ret = MIN(ret,NhlWARNING);
				stp->y_max = tmax;
			}
		}
	}
	return ret;
}

/*
 * Function:	SetUpLLTransObj
 *
 * Description: Sets up a LogLinear transformation object.
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
static NhlErrorTypes SetUpLLTransObj
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init
)
#else 
(stnew,stold,init)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[16];
        int			nargs = 0;
	NhlBoolean		yrev = False,xrev = False,oyrev,oxrev;
#if 0
	float			x,y,xr,yb;
#endif

	entry_name = (init) ? InitName : SetValuesName;

#if 0
	x = stnew->view.x;
	y = stnew->view.y;
	xr = x + stnew->view.width;
	yb = y - stnew->view.height;
	if (x < 0.0 || y > 1.0 || xr > 1.0 || yb < 0.0) {
		e_text = "%s: View extent is outside NDC range: constraining";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
		x = MAX(x,0.0);
		xr = MIN(xr,1.0);
		y = MIN(y,1.0);
		yb = MAX(yb,0.0);
		_NhlInternalSetView((NhlViewLayer) stnew,x,y,
				    xr - x, y - yb, stnew->view.keep_aspect);
	}
#endif
	if (init)
		tfp->trans_obj = NULL;
	else if (ostp->use_irr_trans && tfp->trans_obj != NULL) {
		subret = NhlDestroy(tfp->trans_obj->base.id);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error destroying irregular trans object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		tfp->trans_obj = NULL;
	}

	if (stp->data_init) {
		subret = SetCoordBounds(stp,stXCOORD,0,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
		subret = SetCoordBounds(stp,stYCOORD,0,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
	}

	if (tfp->trans_obj == NULL) {

		stp->new_draw_req = True;
		NhlSetSArg(&sargs[nargs++],NhlNtrXLog,stp->x_log);
		NhlSetSArg(&sargs[nargs++],NhlNtrYLog,stp->y_log);

		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,stp->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,stp->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,stp->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,stp->y_max);

		if (stp->x_min_set && stp->x_max_set)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrXReverse,stp->x_reverse);
		else {
			xrev = (xrev && stp->x_reverse) || 
				(! xrev && ! stp->x_reverse) ? False : True;
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,xrev);
		}
		if (stp->y_min_set && stp->y_max_set)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrYReverse,stp->y_reverse);
		else {
			yrev = (yrev && stp->y_reverse) || 
				(! yrev && ! stp->y_reverse) ? False : True;
			NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,yrev);
		}
		sprintf(buffer,"%s",stnew->base.name);
		strcat(buffer,".Trans");

		subret = NhlALCreate(&tmpid,buffer,
				     NhllogLinTransObjClass,
				     stnew->base.id,sargs,nargs);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		return ret;
	}

	if (stp->x_min != ostp->x_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,stp->x_min);
	if (stp->x_max != ostp->x_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,stp->x_max);
	if (stp->y_min != ostp->y_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,stp->y_min);
	if (stp->y_max != ostp->y_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,stp->y_max);

	if (stp->ovfp == NULL) {
		oxrev = False;
		oyrev = False;
	}
	else {
		oxrev = stp->ovfp->x_start > stp->ovfp->x_end;
		oyrev = stp->ovfp->y_start > stp->ovfp->y_end;
	}

	if (stp->x_reverse != ostp->x_reverse || oxrev != xrev) {
		if (stp->x_min_set && stp->x_max_set)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrXReverse,stp->x_reverse);
		else {
			xrev = (xrev && stp->x_reverse) || 
				(! xrev && ! stp->x_reverse) ? False : True;
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,xrev);
		}
	}
	if (stp->y_reverse != ostp->y_reverse || oyrev != yrev) {
		if (stp->y_min_set && stp->y_max_set)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrYReverse,stp->y_reverse);
		else {
			yrev = (yrev && stp->y_reverse) || 
				(! yrev && ! stp->y_reverse) ? False : True;
			NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,yrev);
		}
	}
	if (stp->x_log != ostp->x_log)
		NhlSetSArg(&sargs[nargs++],NhlNtrXLog,stp->x_log);
	if (stp->y_log != ostp->y_log)
		NhlSetSArg(&sargs[nargs++],NhlNtrYLog,stp->y_log);

	subret = NhlALSetValues(tfp->trans_obj->base.id,sargs,nargs);
	if (nargs > 0) {
		stp->new_draw_req = True;
		stp->update_req = True;
	}
	return MIN(ret,subret);

}
/*
 * Function:	SetUpIrrTransObj
 *
 * Description: Sets up an Irregular transformation object.
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
static NhlErrorTypes SetUpIrrTransObj
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init
)
#else 
(stnew,stold,init)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
#endif
{
 	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stnew->trans);
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;
        NhlSArg			sargs[16];
        int			nargs = 0;
	NhlBoolean		x_irr,y_irr;
	NhlBoolean		xrev,yrev,oxrev,oyrev;
	int			count;

	entry_name = (init) ? InitName : SetValuesName;

	if (! init &&
	    ! ostp->use_irr_trans && 
	    tfp->trans_obj != NULL) {
		subret = NhlDestroy(tfp->trans_obj->base.id);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error destroying irregular trans object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		tfp->trans_obj = NULL;
	}

	if (! stp->data_init) return ret;

	x_irr = stp->vfp->x_arr == NULL ? False : True;
	y_irr = stp->vfp->y_arr == NULL ? False : True;
	xrev = stp->vfp->x_start > stp->vfp->x_end;
	yrev = stp->vfp->y_start > stp->vfp->y_end;
	if (! x_irr && ! y_irr) {
		e_text = "%s: Internal inconsistency setting irregular trans";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (x_irr && stp->x_log) {
		e_text = 
"%s: X Axis cannot be logarithmic and irregular simultaneously: turning %s off";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,NhlNtrXLog);
		stp->x_log = False;
		ret = MIN(NhlWARNING,ret);
	}
	if (y_irr && stp->y_log) {
		e_text = 
"%s: Y Axis cannot be logarithmic and irregular simultaneously: turning %s off";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,NhlNtrYLog);
		stp->y_log = False;
		ret = MIN(NhlWARNING,ret);
	}

	if (stp->x_log || stp->y_log)
		stp->do_low_level_log = True;

	if (init || tfp->trans_obj == NULL ||
	    stp->ovfp == NULL ||
	    stp->vfp->x_arr != stp->ovfp->x_arr ||
	    stp->vfp->x_start != stp->ovfp->x_start ||
	    stp->vfp->x_end != stp->ovfp->x_end ||
	    stp->x_min != ostp->x_min ||
	    stp->x_max != ostp->x_max ||
	    stp->x_log != ostp->x_log) {

		if (x_irr) {
			NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
				   stp->vfp->x_arr);
			count = stp->vfp->x_arr->len_dimensions[0];
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrXAxisType,NhlIRREGULARAXIS);
		}
		else {
			if (stp->x_log)
				NhlSetSArg(&sargs[nargs++],
					   NhlNtrXAxisType,NhlLOGAXIS);
			else
				NhlSetSArg(&sargs[nargs++],
					   NhlNtrXAxisType,NhlLINEARAXIS);
			count = 3;
		}
		subret = SetCoordBounds(stp,stXCOORD,count,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,stp->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,stp->x_max);
	}

	if (init || tfp->trans_obj == NULL ||
	    stp->ovfp == NULL ||
	    stp->vfp->y_arr != stp->ovfp->y_arr ||
	    stp->vfp->y_start != stp->ovfp->y_start ||
	    stp->vfp->y_end != stp->ovfp->y_end ||
	    stp->y_min != ostp->y_min ||
	    stp->y_max != ostp->y_max ||
	    stp->y_log != ostp->y_log) {

		if (y_irr) {
			NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
				   stp->vfp->y_arr);
			count = stp->vfp->y_arr->len_dimensions[0];
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrYAxisType,NhlIRREGULARAXIS);
		}
		else {
			if (stp->y_log)
				NhlSetSArg(&sargs[nargs++],
					   NhlNtrYAxisType,NhlLOGAXIS);
			else
				NhlSetSArg(&sargs[nargs++],
					   NhlNtrYAxisType,NhlLINEARAXIS);
			count = 3;
		}
		subret = SetCoordBounds(stp,stYCOORD,count,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;

		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,stp->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,stp->y_max);
	}

	if (init || tfp->trans_obj == NULL) {

		stp->new_draw_req = True;

		if (stp->x_min_set && stp->x_max_set)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrXReverse,stp->x_reverse);
		else {
			xrev = (xrev && stp->x_reverse) || 
				(! xrev && ! stp->x_reverse) ? False : True;
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,xrev);
		}
		if (stp->y_min_set && stp->y_max_set)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrYReverse,stp->y_reverse);
		else {
			yrev = (yrev && stp->y_reverse) || 
				(! yrev && ! stp->y_reverse) ? False : True;
			NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,yrev);
		}
		sprintf(buffer,"%s",stnew->base.name);
		strcat(buffer,".Trans");

		subret = NhlALCreate(&tmpid,buffer,
				     NhlirregularTransObjClass,
				     stnew->base.id,sargs,nargs);

		ret = MIN(subret,ret);

		tfp->trans_obj = _NhlGetLayer(tmpid);

		if(tfp->trans_obj == NULL){
			e_text = "%s: Error creating transformation object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		return ret;
	}

	if (stp->ovfp == NULL) {
		oxrev = False;
		oyrev = False;
	}
	else {
		oxrev = stp->ovfp->x_start > stp->ovfp->x_end;
		oyrev = stp->ovfp->y_start > stp->ovfp->y_end;
	}
	if (stp->x_reverse != ostp->x_reverse || xrev != oxrev) {
		if (stp->x_min_set && stp->x_max_set)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrXReverse,stp->x_reverse);
		else {
			xrev = (xrev && stp->x_reverse) || 
				(! xrev && ! stp->x_reverse) ? False : True;
			NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,xrev);
		}
	}
	if (stp->y_reverse != ostp->y_reverse || yrev != oyrev) {
		if (stp->y_min_set && stp->y_max_set)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtrYReverse,stp->y_reverse);
		else {
			yrev = (yrev && stp->y_reverse) || 
				(! yrev && ! stp->y_reverse) ? False : True;
			NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,yrev);
		}
	}
	subret = NhlALSetValues(tfp->trans_obj->base.id,sargs,nargs);

	if (nargs > 0) {
		stp->new_draw_req = True;
		stp->update_req = True;
	}
	return MIN(ret,subret);

}


/*
 * Function:	SetFormat
 *
 * Description: Sets up the format records for all the Conpack label.
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes SetFormat
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init
)
#else 
(stnew,stold,init)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString e_text;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	NhlString entry_name;

	entry_name =  init ? InitName : SetValuesName;
/*
 * check the constant spacing value - by the name of the routine this
 * does not belong here -- but for now, it will do
 */
	e_text = 
		"%s: Constant spacing cannot be less than zero, defaulting %s";
	if (stp->zerof_lbl.cspacing < 0.0) {
		stp->zerof_lbl.cspacing = 0.0;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNstZeroFLabelConstantSpacingF);
		ret = MIN(NhlWARNING,ret);
	}

	if (init) {
		subret = SetFormatRec(&stp->mag_scale.format,
				      NhlNstMagnitudeFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = SetFormatRec(&stp->svalue_scale.format,
				      NhlNstScalarValueFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		
		return ret;
	}
	if (stp->mag_scale.format.fstring != 
	    ostp->mag_scale.format.fstring) {
		subret = SetFormatRec(&stp->mag_scale.format,
				      NhlNstMagnitudeFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ostp->mag_scale.format.fstring != NULL)
			NhlFree(ostp->mag_scale.format.fstring);
		ostp->mag_scale.format.fstring = NULL;
	}
	if (stp->svalue_scale.format.fstring != 
	    ostp->svalue_scale.format.fstring) {
		subret = SetFormatRec(&stp->svalue_scale.format,
				      NhlNstScalarValueFormat,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		if (ostp->svalue_scale.format.fstring != NULL)
			NhlFree(ostp->svalue_scale.format.fstring);
		ostp->svalue_scale.format.fstring = NULL;
	}
	return ret;
}

/*
 * Function:	ManageLabels
 *
 * Description: Manages all the non-array label types (not line labels).
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageLabels
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(stnew,stold, init, sargs, nargs)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlString entry_name, e_text;

	entry_name =  init ? InitName : SetValuesName;

	if (init) {
		stp->zerof_lbl.name = ".ZeroField";
		stp->zerof_lbl.text1 = NULL;
		stp->zerof_lbl.text2 = NULL;
		stp->zerof_lbl.string1_on = False;
	}
		

/* Manage constant field label */

	subret = ManageZeroFLabel(stnew,stold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing constant field label";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	
	return ret;
}

/*
 * Function:	SetScale
 *
 * Description: Determines the label scale factor based on the label
 *		scale mode and the label scale value resources. Note that
 *		the scale factor is the amount by which the label values
 *		are multiplied to arrive at the true values in the streamline
 *		field data. Therefore the data values are divided by the
 *		scale factor to get the label values.
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	
 */
static NhlErrorTypes SetScale
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlstScaleInfo		*sip,
	NhlstScaleInfo		*osip,
	NhlBoolean		do_levels,
	NhlBoolean		init
)
#else 
(stnew,stold,sip,osip,init)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlstScaleInfo		*sip;
	NhlstScaleInfo		*osip;
	NhlBoolean		do_levels;
	NhlBoolean		init;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	NhlString entry_name, e_text;
	float sigval,t;
	int power,i,count;
	int divpwr,sig_digits;
	float *fp;
	int max_digit = 0;
	float test_high,test_low,max_fac = 1.0;

	if (! init &&
	    (stp->levels == ostp->levels) &&
	    (sip->mode == osip->mode) &&
	    (sip->scale_value == osip->scale_value) &&
	    (sip->min_val == osip->min_val) &&
	    (sip->max_val == osip->max_val))
		return ret;

	entry_name =  init ? InitName : SetValuesName;

	sigval = MAX(fabs(sip->max_val),fabs(sip->min_val));
	subret = _NhlGetScaleInfo(sigval,&divpwr,&sig_digits,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		return NhlFATAL;
	}
	sip->left_sig_digit = divpwr - 1;
	sip->sig_digits = 4;
	sig_digits = (sip->format.sig_digits_flag == NhlffUNSPECED) ?
		sip->sig_digits : sip->format.sig_digits;

	switch (sip->mode) {
	case NhlSCALEFACTOR:
		if (sip->scale_value <= 0.0) {
			e_text = 
			     "%s: invalid value for scale value: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
			ret = MIN(ret,NhlWARNING);
			sip->scale_value = 1.0;
		}
		sip->scale_factor = sip->scale_value;
		break;
	case NhlCONFINETORANGE:
		if (sip->scale_value <= 0.0) {
			e_text = 
			     "%s: invalid value for scale value: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
			ret = MIN(ret,NhlWARNING);
			sip->scale_value = 1.0;
		}
		power = 1;
		if (sigval >= sip->scale_value) {
			for (t = sigval/10.0;
			     t >=sip->scale_value; t /= 10.0) {
				power++;
			}
			sip->scale_factor = pow(10.0,-(double)power);
		}
		else {
			for (t = sigval * 10;
			     t < sip->scale_value; t *= 10.0) {
				power++;
			}
			power--;
			sip->scale_factor = pow(10.0,(double)power);
		}
		break;
	case NhlTRIMZEROS:
		if (divpwr < 0) 
			power = divpwr;
		else
			power = MAX(0,divpwr - sig_digits);
		sip->scale_factor = pow(10.0,-(double)power);
		break;
	case NhlMAXSIGDIGITSLEFT:
		power = divpwr - sig_digits;
		sip->scale_factor = pow(10.0,-(double)power);
		break;
	case NhlINTEGERLINELABELS:
		if (! do_levels) {
			fp = &sigval;
			count = 1;
		}
		else {
			fp = (float *) stp->levels->data;
			count = stp->level_count;
		}
		test_high = pow(10.0,sig_digits);
		test_low  = pow(10.0,sig_digits - 1);

		for (i = 0; i < count; i++) {
			int	j;
			float	test_fac = 1.0;
			char	buf[32];

			t = fabs(fp[i]);
			if (t == 0.0) 
				continue;
			if (fabs(fp[i]) < test_low) {
				while (t < test_low) {
					t *= 10.0;
					test_fac *= 10.0;
				}
			}
			else if (fabs(fp[i]) >= test_high) {
				while (t >= test_high) {
					t /= 10.0;
					test_fac /= 10.0;
				}
			}
			t = (float) (int) (t + 0.5);

			sprintf(buf,"%f",t);
			j = strcspn(buf,"0.");
			if (j > max_digit) {
				max_digit = j;
				max_fac = test_fac;
			}
		}
		while ((t = sig_digits) > max_digit) {
			max_fac /= 10.0;
			t--;
		}
	
		sip->scale_factor = max_fac;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	return ret;
}
/*
 * Function:	ManageOverlay
 *
 * Description: Sets up arguments for annotations handled internally by
 *		the overlay object (TickMark,Title,LabelBar), then
 *		calls the overlay interface function _NhlManageOverlay.
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	
 */
static NhlErrorTypes ManageOverlay
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(stnew,stold, init, sargs, nargs)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlString entry_name, e_text;

	entry_name =  init ? InitName : SetValuesName;

/* Manage TickMarks object */

	/* 18 arguments possible */
	subret = ManageTickMarks(stnew,stold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing TickMarks";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage Legend object */

	/* 18 arguments possible */
	subret = ManageTitles(stnew,stold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing Titles";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Manage LabelBar object */

	subret = ManageLabelBar(stnew,stold,init,sargs,nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing LabelBar";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/* Manage the overlay */

	/* 1 arg */
	if (stp->update_req) {
		stp->new_draw_req = True;
		NhlSetSArg(&sargs[(*nargs)++],NhlNpmUpdateReq,True);
	}
		
	subret = _NhlManageOverlay(&stp->overlay_object,
				   (NhlLayer)stnew,(NhlLayer)stold,init,
				   sargs,*nargs,entry_name);
	ret = MIN(ret,subret);
	return ret;

}
/*
 * Function:	ManageTickMarks
 *
 * Description: If the StreamlinePlot object has an overlay object attached, and
 *		the TickMarks are activated, manages the TickMark resources 
 *		relevant to the StreamlinePlot object.
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageTickMarks
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(stnew, stold, init, sargs, nargs)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*entry_name;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stnew->trans);

	entry_name = (init) ? InitName : SetValuesName;

 	if (! tfp->plot_manager_on ||
	    stp->display_tickmarks == NhlNOCREATE) 
		return NhlNOERROR;

	if (init || 
	    stp->display_tickmarks != ostp->display_tickmarks) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmTickMarkDisplayMode,
				   stp->display_tickmarks);
	}

	return ret;
}

/*
 * Function:	ManageTitles
 *
 * Description: If the StreamlinePlot object has an overlay object attached, and
 *		the Titles are activated, manages the Title resources 
 *		relevant to the StreamlinePlot object.
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageTitles
#if	NhlNeedProto
(
 	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(stnew, stold, init, sargs, nargs)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*entry_name;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stnew->trans);

	entry_name = (init) ? InitName : SetValuesName;

 	if (! tfp->plot_manager_on ||
	    stp->display_titles == NhlNOCREATE) 
		return NhlNOERROR;

	if (init || 
	    stp->display_titles != ostp->display_titles) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmTitleDisplayMode,
				   stp->display_titles);
	}

	return ret;
}


/*
 * Function:	ManageLabelBar
 *
 * Description: If the StreamlinePlot object has an overlay object attached, and
 *		the LabelBar is activated, manages the LabelBar resources 
 *		relevant to the StreamlinePlot object.
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageLabelBar
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(stnew,stold, init, sargs, nargs)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stnew->trans);
	NhlBoolean		redo_level_strings = False;
	NhlBoolean		set_all = False;

	entry_name = (init) ? InitName : SetValuesName;

 	if (! tfp->plot_manager_on ||
	    stp->display_labelbar == NhlNOCREATE) 
		return NhlNOERROR;

	if (init || 
	    stp->display_labelbar != ostp->display_labelbar ||
	    stp->zero_field != ostp->zero_field ||
	    stp->data_init != ostp->data_init) {

		if ( stp->zero_field) {
			e_text = "%s: zero field: turning Labelbar off";
			NhlPError(NhlINFO,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlINFO);
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmLabelBarDisplayMode,NhlNEVER);
			return ret;
		}
		else {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmLabelBarDisplayMode,
				   stp->display_labelbar);
			if (init || stp->zero_field != ostp->zero_field) 
				set_all = True;
		}
	}

	if (stp->zero_field) return ret;

	if (! stp->explicit_lbar_labels_on) {
		stp->lbar_labels_set = False;
		if (init || set_all ||
		    stp->lbar_end_labels_on != ostp->lbar_end_labels_on ||
		    stp->explicit_lbar_labels_on 
		    			!= ostp->explicit_lbar_labels_on ||
		    stp->level_strings != ostp->level_strings) {
			redo_level_strings = True;
		}
		if (stp->lbar_end_labels_on)
			stp->lbar_alignment = NhlEXTERNALEDGES;
		else
			stp->lbar_alignment = NhlINTERIOREDGES;
	}
	else {
		if (stp->lbar_labels_res_set) {
			NhlGenArray ga;
			if (stp->lbar_labels != NULL) 
				NhlFreeGenArray(stp->lbar_labels);

			if ((ga = _NhlCopyGenArray(stp->lbar_labels_res,
						   True)) == NULL) {
				e_text = "%s: error copying GenArray";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			stp->lbar_labels = ga;
			ostp->lbar_labels = NULL;
		}
		else if (! stp->lbar_labels_set) {
			redo_level_strings = True;
			if (stp->lbar_end_labels_on)
				stp->lbar_alignment = NhlEXTERNALEDGES;
			else
				stp->lbar_alignment = NhlINTERIOREDGES;
		}
		stp->lbar_labels_set = True;
	}

	if (redo_level_strings) {
		NhlGenArray ga;
		NhlString *to_sp, *from_sp;
		NhlString s;
		int i, count;
		NhlBoolean copy = False;

		from_sp = (NhlString *) stp->level_strings;
		if (stp->lbar_labels != NULL) 
			NhlFreeGenArray(stp->lbar_labels);

		if (! stp->lbar_end_labels_on) {
			/* level_strings is used for lbar_labels data */
			count = stp->level_count;
			to_sp = from_sp;
		}
		else {
			float fval;
			NhlFormatRec *frec;
			NhlstScaleInfo	*sip;

			sip=(stp->use_scalar_array && stp->scalar_data_init) ?
				   &stp->svalue_scale : &stp->mag_scale;
			frec = &sip->format;
			copy = True;
			count = stp->level_count + 2;
			to_sp = NhlMalloc(sizeof(NhlString) * count);
			if (to_sp == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}

			fval = sip->min_val * sip->scale_factor;
			s = _NhlFormatFloat(frec,fval,NULL,
					    &sip->sig_digits,
					    &sip->left_sig_digit,
					    NULL,NULL,NULL,
					    stp->lbar_func_code,
					    entry_name);
			if (s == NULL) return NhlFATAL;
			to_sp[0] = NhlMalloc(strlen(s) + 1);
			if (to_sp[0] == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			strcpy(to_sp[0],s);
			for (i = 1; i < count - 1; i++) {
				to_sp[i] = NhlMalloc(strlen(from_sp[i-1]) + 1);
				if (to_sp[i] == NULL) {
					e_text = 
					"%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				strcpy(to_sp[i],from_sp[i-1]);
			}
			fval = sip->max_val * sip->scale_factor;
			s = _NhlFormatFloat(frec,fval,NULL,
					    &sip->sig_digits,
					    &sip->left_sig_digit,
					    NULL,NULL,NULL,
					    stp->lbar_func_code,
					    entry_name);
			if (s == NULL) return NhlFATAL;
			to_sp[count - 1] = NhlMalloc(strlen(s) + 1);
			if (to_sp[count - 1] == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			strcpy(to_sp[count - 1],s);
		}
		ga = NhlCreateGenArray((NhlPointer)to_sp,NhlTString,
				       sizeof(NhlString),1,&count);
		if (ga == NULL) {
			e_text = "%s: error creating GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		ga->my_data = copy ? True : False;
		stp->lbar_labels = ga;
		ostp->lbar_labels = NULL;
	}
	if (init || set_all) {

		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbBoxCount,stp->level_count + 1);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelAlignment,stp->lbar_alignment);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelStrings,stp->lbar_labels);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbMonoFillColor,False);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillColors,stp->streamline_colors);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbMonoFillPattern,True);
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbMonoFillScale,True);
		return ret;
	}

	if (stp->level_count != ostp->level_count)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbBoxCount,stp->level_count + 1);
	if (stp->lbar_alignment != ostp->lbar_alignment)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelAlignment,stp->lbar_alignment);
	if (stp->lbar_labels != ostp->lbar_labels)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbLabelStrings,stp->lbar_labels);
	if (stp->streamline_colors != ostp->streamline_colors)
		NhlSetSArg(&sargs[(*nargs)++],
			   NhlNlbFillColors,stp->streamline_colors);
	return ret;
}


/*
 * Function:	ManageZeroFLabel
 *
 * Description: If a constant field is detected a constant field label
 *		is created, or turned on.
 *		If there is an PlotManager an AnnoManager object is 
 *		created so that the overlay object can manage the
 *		annotations.
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageZeroFLabel
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	NhlSArg		*sargs,
	int		*nargs
)
#else 
(stnew,stold, init, sargs, nargs)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
	NhlSArg		*sargs;
	int		*nargs;
#endif
{
	char			*e_text;
	char			*entry_name;
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stnew->trans);
	NhlstLabelAttrs		*cflp = &stp->zerof_lbl;
	NhlstLabelAttrs		*ocflp = &ostp->zerof_lbl;
	NhlString		lstring, tstring;
	NhlBoolean		text_changed = False, pos_changed = False;
	NhlSArg			targs[24];
	int			targc = 0;
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;

	entry_name = (init) ? InitName : SetValuesName;

/*
 * The constant field label resource must be turned on AND a constant
 * field condition must exist for the constant field annotation  
 * to be displayed.
 */


	if (init || stp->zerof_lbl.string1 != ostp->zerof_lbl.string1) {
		text_changed = True;
		tstring = stp->zerof_lbl.string1 == NULL ?
			NhlstDEF_ZEROF_LABEL : stp->zerof_lbl.string1; 
		if ((lstring = NhlMalloc(strlen(tstring) + 1)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy(lstring,tstring);
		stp->zerof_lbl.string1 = lstring;
	}
	if (init || stp->zerof_lbl.string2 != ostp->zerof_lbl.string2) {
		text_changed = True;
		tstring = stp->zerof_lbl.string2 == NULL ?
			NhlstDEF_NODATA_LABEL : stp->zerof_lbl.string2; 
		if ((lstring = NhlMalloc(strlen(tstring) + 1)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy(lstring,tstring);
		stp->zerof_lbl.string2 = lstring;
	}
	if (! stp->data_init)
		stp->zerof_no_data_string = stp->zerof_lbl.string2;
	else
		stp->zerof_no_data_string = stp->zerof_lbl.string1;
	if (text_changed)
		ostp->zerof_no_data_string = NULL;

	stp->display_zerof_no_data = 
		(cflp->on && stp->zero_field) || 
			(stp->zerof_lbl.string2_on && ! stp->data_init);

	subret = ReplaceSubstitutionChars(stp,ostp,init,_stZEROF,
				  &text_changed,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;

	if (init || stp->zerof_lbl_rec.id == NhlNULLOBJID) {
		if (pos_changed) {
			NhlSetSArg(&targs[(targc)++],NhlNtxPosXF,cflp->x_pos);
			NhlSetSArg(&targs[(targc)++],NhlNtxPosYF,cflp->y_pos);
			NhlSetSArg(&targs[(targc)++],NhlNtxJust,cflp->just);
		}
		NhlSetSArg(&targs[(targc)++],NhlNtxString,
			   (NhlString)cflp->text1);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontHeightF,cflp->height);
		NhlSetSArg(&targs[(targc)++],NhlNtxDirection,cflp->direction);
		NhlSetSArg(&targs[(targc)++],NhlNtxAngleF,cflp->angle);
		NhlSetSArg(&targs[(targc)++],NhlNtxFont,cflp->font);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontColor,cflp->color);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontAspectF,cflp->aspect);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxFontThicknessF,cflp->thickness);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxConstantSpacingF,cflp->cspacing);
		NhlSetSArg(&targs[(targc)++],NhlNtxFontQuality,cflp->quality);
		NhlSetSArg(&targs[(targc)++],NhlNtxFuncCode,cflp->fcode[0]);

		NhlSetSArg(&targs[(targc)++],NhlNtxPerimOn,cflp->perim_on);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxPerimColor,cflp->perim_lcolor);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxPerimThicknessF,cflp->perim_lthick);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxPerimSpaceF,cflp->perim_space);
		NhlSetSArg(&targs[(targc)++],
			   NhlNtxBackgroundFillColor,cflp->back_color);

		sprintf(buffer,"%s",stnew->base.name);
		strcat(buffer,cflp->name);
		subret = NhlALCreate(&tmpid,buffer,NhltextItemClass,
				     stnew->base.id,targs,targc);
		
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error creating information label";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		stp->zerof_lbl_rec.id = tmpid;

		if (tfp->overlay_status == _tfNotInOverlay) {
			targc = 0; 
			/* go on so text position can be set */
		}
		else {
			subret = ManageAnnotation(stnew,init,
						  &stp->zerof_lbl_rec,
						  &ostp->zerof_lbl_rec,
						  &stp->zerof_anno_id,
						  stp->display_zerof_no_data);
			return MIN(ret,subret);
		}
	}

	if (tfp->overlay_status == _tfNotInOverlay) {
		NhlStreamlinePlotLayerPart *op;
		op = init ? NULL : ostp;
		subret = SetTextPosition(stnew,op,_stZEROF,
					 &pos_changed,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
	}
	if (pos_changed) {
		NhlSetSArg(&targs[(targc)++],NhlNtxPosXF,cflp->x_pos);
		NhlSetSArg(&targs[(targc)++],NhlNtxPosYF,cflp->y_pos);
		NhlSetSArg(&targs[(targc)++],NhlNtxJust,cflp->just);
	}
	if (! init) {
		if (text_changed)
			NhlSetSArg(&targs[(targc)++],NhlNtxString,
				   (NhlString)cflp->text1);
		if (cflp->height != ocflp->height)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontHeightF,cflp->height);
		if (cflp->direction != ocflp->direction)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxDirection,cflp->direction);
		if (cflp->angle != ocflp->angle)
			NhlSetSArg(&targs[(targc)++],NhlNtxAngleF,cflp->angle);
		if (cflp->font != ocflp->font)
			NhlSetSArg(&targs[(targc)++],NhlNtxFont,cflp->font);
		if (cflp->color != ocflp->color)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontColor,cflp->color);
		if (cflp->aspect != ocflp->aspect)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontAspectF,cflp->aspect);
		if (cflp->thickness != ocflp->thickness)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontThicknessF,cflp->thickness);
		if (cflp->cspacing != ocflp->cspacing)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxConstantSpacingF,cflp->cspacing);
		if (cflp->quality != ocflp->quality)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFontQuality,cflp->quality);
		if (cflp->fcode[0] != ocflp->fcode[0])
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxFuncCode,cflp->fcode[0]);
		
		if (cflp->perim_on != ocflp->perim_on)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimOn,cflp->perim_on);
		if (cflp->perim_lcolor != ocflp->perim_lcolor)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimColor,cflp->perim_lcolor);
		if (cflp->perim_lthick != ocflp->perim_lthick)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimThicknessF,cflp->perim_lthick);
		if (cflp->perim_space != ocflp->perim_space)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxPerimSpaceF,cflp->perim_space);
		if (cflp->back_color != ocflp->back_color)
			NhlSetSArg(&targs[(targc)++],
				   NhlNtxBackgroundFillColor,cflp->back_color);
	}
	
	subret = NhlALSetValues(stp->zerof_lbl_rec.id,targs,targc);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: error setting values forinformation label";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (tfp->overlay_status != _tfNotInOverlay) {
		subret = ManageAnnotation(stnew,init,
					  &stp->zerof_lbl_rec,
					  &ostp->zerof_lbl_rec,
					  &stp->zerof_anno_id,
					  stp->display_zerof_no_data);
		ret = MIN(ret,subret);
	}
	return ret;
}

/*
 * Function:	ManageAnnotation
 *
 * Description: 
 *		
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ManageAnnotation
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew,
	NhlBoolean		init,
	NhlAnnotationRec	*rec,
	NhlAnnotationRec	*orec,
	int			*idp,
	NhlBoolean		on
)
#else 
(stnew,init,rec,orec,idp,on)
	NhlStreamlinePlotLayer	stnew;
	NhlBoolean		init;
	NhlAnnotationRec	*rec;
	NhlAnnotationRec	*orec;
	int			*idp;
	NhlBoolean		on;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stnew->trans);
	NhlSArg			sargs[16];
	int			nargs = 0;
	char			buffer[_NhlMAXRESNAMLEN];
	int			tmpid;

	entry_name = (init) ? InitName : SetValuesName;

	rec->on = on;

	if (*idp <= NhlNULLOBJID) {
		NhlSetSArg(&sargs[(nargs)++],NhlNamOn,rec->on);
		NhlSetSArg(&sargs[(nargs)++],NhlNamViewId,rec->id);
		NhlSetSArg(&sargs[(nargs)++],NhlNamZone,rec->zone);
		NhlSetSArg(&sargs[(nargs)++],NhlNamSide,rec->side);
		NhlSetSArg(&sargs[(nargs)++],NhlNamJust,rec->just);
		NhlSetSArg(&sargs[(nargs)++],
			   NhlNamParallelPosF,rec->para_pos);
		NhlSetSArg(&sargs[(nargs)++],
			   NhlNamOrthogonalPosF,rec->ortho_pos);
		sprintf(buffer,"%s",stnew->base.name);
		strcat(buffer,".AnnoManager");
		subret = NhlALCreate(&tmpid,buffer,NhlannoManagerClass,
				     stnew->base.id,sargs,nargs);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error creating AnnoManager layer";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		*idp = tmpid;
/*
 * If the StreamlinePlot plot is an overlay plot base register the AnnoManager
 * with its own base, ensuring that it will always follow the overlay.
 */
		if (tfp->plot_manager_on)
			subret = _NhlRegisterAnnotation(stp->overlay_object,
							_NhlGetLayer(*idp),
							NULL);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error registering annotation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		return ret;
	}
	if (rec->on != orec->on) 
		NhlSetSArg(&sargs[(nargs)++],NhlNamOn,rec->on);
	if (rec->zone != orec->zone) 
		NhlSetSArg(&sargs[(nargs)++],NhlNamZone,rec->zone);
	if (rec->side != orec->side) 
		NhlSetSArg(&sargs[(nargs)++],NhlNamSide,rec->side);
	if (rec->just != orec->just) 
		NhlSetSArg(&sargs[(nargs)++],NhlNamJust,rec->just);
	if (rec->para_pos != orec->para_pos) 
		NhlSetSArg(&sargs[(nargs)++],
			   NhlNamParallelPosF,rec->para_pos);
	if (rec->ortho_pos != orec->ortho_pos) 
		NhlSetSArg(&sargs[(nargs)++],
			   NhlNamOrthogonalPosF,rec->ortho_pos);
	
	subret = NhlALSetValues(*idp,sargs,nargs);

	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: error setting AnnoManager object values";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	return ret;
}

/*
 * Function:	ReplaceSubstitutionChars
 *
 * Description: Replaces the substitution characters in the info and
 *		zero field labels with the correct numerical values.
 *
 * In Args:	stnew	new instance record
 *		stold	old instance record if not initializing
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes ReplaceSubstitutionChars
#if	NhlNeedProto
(
	NhlStreamlinePlotLayerPart	*stp,
	NhlStreamlinePlotLayerPart	*ostp,
	NhlBoolean		init,
	_stAnnoType		atype,
	NhlBoolean		*text_changed,
	NhlString		entry_name
)
#else 
(stp,ostp,init,atype,text_changed,entry_name)
	NhlStreamlinePlotLayerPart	*stp;
	NhlStreamlinePlotLayerPart	*ostp;
	NhlBoolean		init;
	_stAnnoType		atype;
	NhlBoolean		*text_changed;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			buffer[256];

	*text_changed = False;

	if (! init && (stp->zmax == ostp->zmax) &&
	    (stp->zerof_no_data_string == 
	     ostp->zerof_no_data_string))
		return NhlNOERROR;
	strcpy(buffer,stp->zerof_no_data_string);
	if (stp->zerof_lbl.text1 != NULL)
		NhlFree(stp->zerof_lbl.text1);
	if ((stp->zerof_lbl.text1 = 
	     NhlMalloc(strlen(buffer)+1)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	strcpy((NhlString)stp->zerof_lbl.text1,buffer);

	*text_changed = True;

	return ret;
}

/*
 * Function:	SetFormatRec
 *
 * Description: sets up the format record for a label type
 *
 * In Args:	NhlFormatRec *format -> to the format record
 *		NhlString    resource    the format resource - for error.
 *		NhlString    entry_name  the caller
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects: 
 */
static NhlErrorTypes SetFormatRec
#if	NhlNeedProto
(
	NhlFormatRec	*format,
	NhlString	resource,
	NhlString	entry_name
)
#else 
(format,resource,entry_name)
	NhlFormatRec	*format;
	NhlString	resource;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlFormatRec	*frec;


	if (format->fstring == NULL || format->fstring[0] == '\0') {
		e_text = "%s: empty format string supplied for %s; defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,resource);
		ret = NhlWARNING;
		format->fstring = NhlstDEF_FORMAT;
	}
	if ((frec = _NhlScanFString(format->fstring,entry_name)) == NULL) {
		e_text = "%s: error in format string for %s: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,resource);
		ret = NhlWARNING;
		format->fstring = NhlstDEF_FORMAT;
		if ((frec = _NhlScanFString(format->fstring,
					    entry_name)) == NULL) {
			e_text = "%s: internal error getting format";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(NhlFATAL);
		}
	}
	memcpy((void *)format,(Const void *)frec,sizeof(NhlFormatRec));

/* 
 * Since at this point the format string itself is not owned by the 
 * StreamlinePlot object, make a copy.
 */
	if (format->fstring != NULL) {
		char *cp;
		if ((cp = NhlMalloc(strlen(format->fstring)+1)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy(cp,format->fstring);
		format->fstring = cp;
	}

	return ret;
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
	NhlAnnotationRec	*anno_rec
)
#else
(anno_rec)
	NhlAnnotationRec	*anno_rec;
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
 * Function:	SetTextPosition
 *
 * Description: Sets the text positional attribute fields in label 
 *		annotation strings when they are not members of an overlay, 
 *		and therefore have no AnnoManager.
 *
 * In Args:	
 *		
 *		
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
static NhlErrorTypes SetTextPosition
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer		stnew,
	NhlStreamlinePlotLayerPart	*ostp,
	_stAnnoType		atype,
	NhlBoolean		*pos_changed,
	NhlString		entry_name
)
#else 
(stnew,ostp,atype,pos_changed,entry_name)
	NhlStreamlinePlotLayer		stnew;
	NhlStreamlinePlotLayerPart	*ostp;
	_stAnnoType		atype;
	NhlBoolean		*pos_changed;
	NhlString		entry_name;
#endif
{
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR,subret = NhlNOERROR;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlAnnotationRec	*anno_rec;
	NhlstLabelAttrs		*lap;
	NhlstLabelAttrs		*olap;
	float			width_vp, height_vp;
	float			x_start, y_start;
	int			sign;

	anno_rec = &stp->zerof_lbl_rec;
	lap = &stp->zerof_lbl;
	olap = ostp == NULL ? NULL : &ostp->zerof_lbl;

	subret = NhlVAGetValues(anno_rec->id,
				NhlNvpWidthF,&width_vp,
				NhlNvpHeightF,&height_vp,
				NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error getting embedded annotation values";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		return ret;
	}

	x_start = anno_rec->zone != 0 ? stnew->view.x :
		stnew->view.x + 0.5 * stnew->view.width; 
	y_start = anno_rec->zone != 0 ? stnew->view.y - stnew->view.height :
		stnew->view.y - 0.5 * stnew->view.height;
	sign = anno_rec->zone == 1 ? 1.0 : - 1.0;


	switch (anno_rec->side) {
	case NhlBOTTOM:
		lap->x_pos = x_start + anno_rec->para_pos * stnew->view.width;
		lap->y_pos = y_start - 
			sign * anno_rec->ortho_pos * stnew->view.height;
		break;
	case NhlTOP:
		lap->x_pos = x_start + anno_rec->para_pos * stnew->view.width;
		lap->y_pos = y_start + 
			sign * anno_rec->ortho_pos * stnew->view.height;
		break;
	case NhlLEFT:
		lap->x_pos = x_start - 
			sign * anno_rec->ortho_pos * stnew->view.width;
		lap->y_pos = y_start +
			anno_rec->para_pos * stnew->view.height;
		break;
	case NhlRIGHT:
		lap->x_pos = x_start + stnew->view.width + 
			sign * anno_rec->ortho_pos * stnew->view.width;
		lap->y_pos = y_start +
			anno_rec->para_pos * stnew->view.height;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}
	if (anno_rec->just < NhlTOPLEFT || anno_rec->just > NhlBOTTOMRIGHT) {
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(NhlFATAL);
	}

	if (anno_rec->zone > 0)
		lap->just = ConstrainJustification(anno_rec);
	else 
		lap->just = anno_rec->just;

	if (olap == NULL ||
	    lap->x_pos != olap->x_pos ||
	    lap->y_pos != olap->y_pos ||
	    lap->just != olap->just) {
		*pos_changed = True;
	}
	return ret;
}

/*
 * Function:  ManageVectorData
 *
 * Description: Handles updating of the streamline data
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
static NhlErrorTypes    ManageVectorData
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew, 
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
)
#else
(stnew,stold,init,args,num_args)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &stnew->streamlineplot;
	NhlVectorFieldFloatLayer	vfl;
	_NhlDataNodePtr			*dlist = NULL;
	NhlBoolean			new;
	int				ndata = 0;


	entry_name = (init) ? InitName : SetValuesName;

	if (! stp->data_changed && 
	    ! _NhlArgIsSet(args,num_args,NhlNstVectorFieldData))
		return NhlNOERROR;

	if (stp->vector_field_data != NULL)
		ndata = _NhlGetDataInfo(stp->vector_field_data,&dlist);
	if (ndata <= 0) {
		stp->mag_scale.min_val = stp->zmin = 0.01;
		stp->mag_scale.max_val = stp->zmax = 1.0;
		stp->data_init = False;
		stp->vfp = NULL;
		return NhlNOERROR;
	}
	else if (ndata != 1) {
		stp->data_init = False;
		e_text = "%s: internal error retrieving data info";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (stp->vfp != NULL && stp->ovfp == NULL) {
		stp->ovfp = NhlMalloc(sizeof(NhlVectorFieldFloatLayerPart));
		if (stp->ovfp == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	if (stp->vfp != NULL) {
		memcpy(stp->ovfp,
		       stp->vfp,sizeof(NhlVectorFieldFloatLayerPart));	
	}

 	vfl = (NhlVectorFieldFloatLayer) _NhlGetDataSet(dlist[0],&new);
	if (vfl == NULL) {
		stp->data_init = False;
		e_text = "%s: internal error retrieving data set";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	
	stp->vfp = (NhlVectorFieldFloatLayerPart *) &vfl->vfieldfloat;

	stp->zmin = stp->vfp->mag_min;
	stp->zmax = stp->vfp->mag_max;

	stp->mag_scale.max_val = stp->zmax;
	stp->mag_scale.min_val = stp->zmin;

	stp->zero_field = _NhlCmpFAny(stp->zmax,stp->zmin,8) <= 0.0 ?
		True : False;
	if (stp->zero_field) {
		e_text = 
		 "%s: zero vector field; no StreamlinePlot possible";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(NhlWARNING,ret);
	}

	stp->data_init = True;
	stp->data_changed = True;
	stp->use_irr_trans = (stp->vfp->x_arr == NULL &&
			      stp->vfp->y_arr == NULL) ? False : True;

	return ret;
}


/*
 * Function:  ManageScalarData
 *
 * Description: Handles updating of the scalar data
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
static NhlErrorTypes    ManageScalarData
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stnew, 
	NhlStreamlinePlotLayer	stold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
)
#else
(stnew,stold,init,args,num_args)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*entry_name;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &stnew->streamlineplot;
	NhlScalarFieldFloatLayer	sfl;
	_NhlDataNodePtr			*dlist = NULL;
	NhlBoolean			new;
	int				ndata = 0;


	entry_name = (init) ? InitName : SetValuesName;

	if (! stp->data_changed && 
	    ! _NhlArgIsSet(args,num_args,NhlNstScalarFieldData))
		return NhlNOERROR;

	if (stp->scalar_field_data != NULL)
		ndata = _NhlGetDataInfo(stp->scalar_field_data,&dlist);
	if (ndata != 1) {
		stp->svalue_scale.min_val = stp->scalar_min = 0.0;
		stp->svalue_scale.max_val = stp->scalar_max = 1.0;
		stp->scalar_data_init = False;
		stp->sfp = NULL;
		if (ndata > 1) {
			e_text = "%s: internal error retrieving data info";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		else  {
			return NhlNOERROR;
		}
	}

	if (stp->sfp != NULL && stp->osfp == NULL) {
		stp->osfp = NhlMalloc(sizeof(NhlScalarFieldFloatLayerPart));
		if (stp->osfp == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	if (stp->sfp != NULL) {
		memcpy(stp->osfp,
		       stp->sfp,sizeof(NhlScalarFieldFloatLayerPart));	
	}

 	sfl = (NhlScalarFieldFloatLayer) _NhlGetDataSet(dlist[0],&new);
	if (sfl == NULL) {
		stp->scalar_data_init = False;
		e_text = "%s: internal error retrieving data set";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	
	stp->sfp = (NhlScalarFieldFloatLayerPart *) &sfl->sfieldfloat;

	if (stp->data_init && 
	    (stp->sfp->fast_len != stp->vfp->fast_len ||
	     stp->sfp->slow_len != stp->vfp->slow_len)) {
		e_text = "%s: ignoring %s: size does not match %s";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNstScalarFieldData,NhlNstVectorFieldData);
		ret = NhlWARNING;
		stp->scalar_data_init = False;
		stp->sfp = NULL;
		stp->svalue_scale.min_val = stp->scalar_min = 0.0;
		stp->svalue_scale.max_val = stp->scalar_max = 1.0;
	}
	else {
		stp->svalue_scale.min_val = 
			stp->scalar_min = stp->sfp->data_min;
		stp->svalue_scale.max_val = 
			stp->scalar_max = stp->sfp->data_max;
		stp->scalar_data_init = True;
		stp->data_changed = True;
	}
	return ret;
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
#if	NhlNeedProto
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
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name;
	NhlStreamlinePlotLayer		stnew = (NhlStreamlinePlotLayer) new;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayer		stold = (NhlStreamlinePlotLayer) old;
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	NhlBoolean		view_changed;

	entry_name = (init) ? InitName : SetValuesName;

/* adjust the reference length if it is not set */

	view_changed = init || 
		(stnew->view.width != stold->view.width) ||
			(stnew->view.height != stold->view.height);

	if (! stp->data_init) {
		stp->grid_cell_size = stnew->view.width * 0.05;
	}
	else if (view_changed || stp->data_changed) {
		int nx,ny;
		float sx,sy;
		nx = stp->vfp->fast_len;
		ny = stp->vfp->slow_len;
		sx = stnew->view.width / nx;
		sy = stnew->view.height / ny;
		stp->grid_cell_size = sqrt((sx*sx + sy*sy) / 2.0);
	}

	if (! stp->arrow_length_set || stp->arrow_length <= 0.0) {
		if (init || stp->arrow_length <= 0.0) {
			stp->arrow_length = 0.33 * stp->grid_cell_size; 
		}
		else if (stp->grid_cell_size != ostp->grid_cell_size) {
			stp->arrow_length *= 
				stp->grid_cell_size / ostp->grid_cell_size;
		}
	}
	if (! stp->step_size_set || stp->step_size <= 0.0) {
		if (init || stp->step_size <= 0.0) {
			stp->step_size = 0.33 * stp->grid_cell_size;
		}
		else if (stp->grid_cell_size != ostp->grid_cell_size) {
			stp->step_size *= 
				stp->grid_cell_size / ostp->grid_cell_size;
		}
	}
	if (! stp->min_line_spacing_set || stp->min_line_spacing <= 0.0) {
		if (init || stp->min_line_spacing <= 0.0) {
			stp->min_line_spacing = 0.5 * stp->grid_cell_size;
		}
		else if (stp->grid_cell_size != ostp->grid_cell_size) {
			stp->min_line_spacing *= 
				stp->grid_cell_size / ostp->grid_cell_size;
		}
	}
	if (! stp->min_arrow_spacing_set) {
		if (init) {
			stp->min_arrow_spacing *= 
				stnew->view.width / Nhl_stSTD_VIEW_WIDTH;
		}
		else if (stnew->view.width != stold->view.width) {
			stp->min_arrow_spacing *= 
				stnew->view.width / stold->view.width;
		}
	}
	if (! stp->min_line_length_set) {
		if (init) {
			stp->min_line_length *= 
				stnew->view.width / Nhl_stSTD_VIEW_WIDTH;
		}
		else if (stnew->view.width != stold->view.width) {
			stp->min_line_length *= 
				stnew->view.width / stold->view.width;
		}
	}

	stp->lbls.aspect = 1.325;
	subret = AdjustText(&stp->lbls,stnew,stold,init);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = AdjustText(&stp->zerof_lbl,stnew,stold,init);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	return ret;
}

/*
 * Function:  AdjustText
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
static NhlErrorTypes    AdjustText
#if	NhlNeedProto
(
	NhlstLabelAttrs *lbl_attrp,
	NhlStreamlinePlotLayer	new, 
	NhlStreamlinePlotLayer	old,
	NhlBoolean	init
)
#else
(lbl_attrp,new,old,init)
	NhlstLabelAttrs *lbl_attrp;
	NhlLayer	new;
	NhlLayer	old;
	NhlBoolean	init;
#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char 			*e_text;
	char			*entry_name;
	NhlStreamlinePlotLayer		stnew = (NhlStreamlinePlotLayer) new;
	NhlStreamlinePlotLayer		stold = (NhlStreamlinePlotLayer) old;

	entry_name = (init) ? InitName : SetValuesName;

/* 
 * Adjust text height. Then determine principal width and height
 * and the "real " text height based on aspect ratio. 21.0 is the default 
 * principle height. This code handles aspect ratio like the TextItem.
 */

	if (! lbl_attrp->height_set) {
		if (init) {
			lbl_attrp->height *= 
				stnew->view.width / Nhl_stSTD_VIEW_WIDTH;
		}
		else if (stnew->view.width != stold->view.width) {
			lbl_attrp->height *= 
				stnew->view.width / stold->view.width;
		}
	}

        if (lbl_attrp->aspect <= 0.0 ) {
		e_text = "%s: Invalid value for text aspect ratio %d";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,lbl_attrp->aspect);
                ret = NhlWARNING;
                lbl_attrp->aspect = 1.3125;
        }
        if (lbl_attrp->aspect <= 1.0) {
                lbl_attrp->pheight = 21.0 * lbl_attrp->aspect;
                lbl_attrp->pwidth = 21.0;
        } else {
                lbl_attrp->pwidth = 21.0 * 1.0/lbl_attrp->aspect;
                lbl_attrp->pheight = 21.0;
        }
	/*
	 * The 1.125 factor compensates for the PLOTCHAR 'SA' parameter
	 */
        lbl_attrp->real_height = 
		1.0 / lbl_attrp->aspect * lbl_attrp->height * 1.125;

	return ret;
}
/*
 * Function:  ManageDynamicArrays
 *
 * Description: Creates and manages internal copies of each of the 
 *	StreamlinePlot GenArrays. Populates the copies with the values specified 
 *	via StreamlinePlotCreate or StreamlinePlotSetValues calls. Assigns default 
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
#if	NhlNeedProto
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
	NhlStreamlinePlotLayer	stnew = (NhlStreamlinePlotLayer) new;
	NhlStreamlinePlotLayerPart *stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayer	stold = (NhlStreamlinePlotLayer) old;
	NhlStreamlinePlotLayerPart *ostp = &(stold->streamlineplot);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	int i,count;
	NhlGenArray ga;
	char *entry_name;
	char *e_text;
	int	init_count;
	NhlBoolean need_check,changed;
	int old_count;
	float *levels = NULL;
	NhlBoolean levels_modified = False;
	NhlstScaleInfo 		*sip,*osip;
	NhlBoolean		scalar_labels, mag_labels;

	entry_name =  init ? InitName : SetValuesName;

/* 
 * If constant field don't bother setting up the arrays: they will not
 * be used -- but the label scaling still needs to be set up for the
 * benefit of the constant field label
 */

	if (stp->zero_field) {
		subret = SetScale(stnew,stold,
				  &stp->mag_scale,&ostp->mag_scale,
				  False,init);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up label scaling";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
		return NhlNOERROR;
	}

/* Determine the streamline level state */

	subret = SetupLevels(new,old,init,&levels,&levels_modified);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error getting streamline level information";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	count = stp->level_count;

/*=======================================================================*/

/* 
 * The levels array 
 */
	ga = init ? NULL : ostp->levels;
	subret = ManageGenArray(&ga,count,stp->levels,Qfloat,NULL,
				&old_count,&init_count,&need_check,&changed,
				NhlNstLevels,entry_name);

	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;

	ostp->levels = changed || levels_modified ? NULL : stp->levels;
	stp->levels = ga;
	if (levels_modified) {
		if (levels == NULL) {
			e_text = "%s: internal error getting levels";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		NhlFree(stp->levels->data);
		stp->levels->data = (NhlPointer) levels;
#if 0
		printf("no of levels: %d\n", stp->level_count);
		for (i= 0; i < stp->level_count; i++)
			printf("level %d: %f\n", i, levels[i]);
#endif
	}


/* Set up label scaling - the levels must have been set */

	if (stp->use_scalar_array && stp->scalar_data_init) {
		sip = &stp->svalue_scale;
		osip = &ostp->svalue_scale;
		scalar_labels = True;
		mag_labels = False;
	}
	else {
		sip = &stp->mag_scale;
		osip = &ostp->mag_scale;
		scalar_labels = False;
		mag_labels = True;
	}

	subret = SetScale(stnew,stold,&stp->mag_scale,
			  &ostp->mag_scale,mag_labels,init);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting up label scaling";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	subret = SetScale(stnew,stold,&stp->svalue_scale,
			  &ostp->svalue_scale,scalar_labels,init);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting up label scaling";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

/*=======================================================================*/
	
/*
 * Streamline colors
 */
	ga = init ? NULL : ostp->streamline_colors;
	count = stp->level_count + 1;
	subret = ManageGenArray(&ga,count,stp->streamline_colors,Qcolorindex,NULL,
				&old_count,&init_count,&need_check,&changed,
				NhlNstStreamlineColors,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING)
		return ret;
	ostp->streamline_colors = changed ? NULL : stp->streamline_colors;
	stp->streamline_colors = ga;

		
	if (need_check) {
		subret = CheckColorArray(stnew,ga,count,init_count,old_count,
					 &stp->gks_streamline_colors,
					 NhlNstStreamlineColors,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
			return ret;
	}
/*=======================================================================*/
	
/*
 * Level String Values
 */
	count = stp->level_count;
	if (init) stp->level_strings = NULL;

	if (init || levels_modified || 
	    sip->format.fstring != osip->format.fstring ||
	    sip->mode != osip->mode ||
	    sip->scale_value != osip->scale_value ||
	    sip->scale_factor != osip->scale_factor) {
		NhlBoolean modified = False;
		NhlString cp;
		float *fp = (float *) stp->levels->data;
		NhlString *sp = stp->level_strings;

		if (sp != NULL) {
			int i;
			for (i = 0; i < ostp->level_count; i++) {
				if (sp[i] != NULL)
					NhlFree(sp[i]);
			}
			NhlFree(sp);
		}
		if ((sp = (NhlString *) 
		     NhlMalloc(count * sizeof(NhlString))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		for (i=0; i<count; i++) {
			float fval = fp[i] * sip->scale_factor;
			NhlFormatRec *frec = &sip->format;

			cp = _NhlFormatFloat(frec,fval,NULL,
					     &sip->sig_digits,
					     &sip->left_sig_digit,
                                             NULL,NULL,NULL,
					     stp->lbar_func_code,
					     entry_name);
			if (cp == NULL) return NhlFATAL;
			if ((sp[i] = (char *) 
			     NhlMalloc(strlen(cp)+1)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			strcpy(sp[i],cp);
			modified = True;
		}
		stp->level_strings = sp;
		ostp->level_strings = NULL;
	}
				

/*=======================================================================*/

/*
 * Test for changes that require a new draw (when in segment mode)
 */
	if (
	    stp->streamline_colors != ostp->streamline_colors ||
	    stp->mono_streamline_fill_color != ostp->mono_streamline_fill_color ||
	    stp->streamline_fill_color != ostp->streamline_fill_color ||
	    stp->mono_streamline_line_color != ostp->mono_streamline_line_color ||
	    stp->streamline_line_color != ostp->streamline_line_color ||
	    stp->levels != ostp->levels ||
	    stp->level_strings != ostp->level_strings) {
		stp->new_draw_req = True;
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
 *		*changed	True if the data has been modified in any way.
 *
 *
 * Return Values:
 *
 * Side Effects: The internal copy of each GenArray is modified to reflect
 *	changes requested via StreamlinePlotSetValues
 */

/*ARGSUSED*/
static NhlErrorTypes    ManageGenArray
#if	NhlNeedProto
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
	else if (type == Qcolorindex) {
		str_type = NhlTColorIndex;
		size = sizeof(NhlColorIndex);
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
		*init_count = *old_count;

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
		ret = _NhlValidatedGenArrayCopy(ga,copy_ga,Nhl_stMAX_LEVELS+1,
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
		else if (type == Qcolorindex)
			for (i = *init_count; i< count; i++)
				((NhlColorIndex *)datap)[i] =
						*((NhlColorIndex *)init_val);
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
 *	changes requested via StreamlinePlotSetValues
 */

/*ARGSUSED*/

static NhlErrorTypes	CheckColorArray
#if	NhlNeedProto
	(NhlStreamlinePlotLayer	cl,
	NhlGenArray	ga,
	int		count,
	int		init_count,
	int		last_count,
	int		**gks_colors,
	NhlString	resource_name,
	NhlString	entry_name)
#else
(cl,ga,count,init_count,last_count,gks_colors,resource_name,entry_name)
	NhlStreamlinePlotLayer	cl;
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
	int *ip;
	int i;
	

	ip = (int *) ga->data;

	for (i=init_count; i < count; i++) {
		ip[i] = 1 + i;
	}

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
                if (ip[i] == NhlTRANSPARENT)
                        (*gks_colors)[i] = NhlTRANSPARENT;
                else
                        (*gks_colors)[i] =
                                _NhlGetGksCi(cl->base.wkptr,ip[i]);
	}
	return ret;
}


/*
 * Function:  SetupLevels
 *
 * Description:
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
#if	NhlNeedProto
	(NhlLayer	new, 
	 NhlLayer	old,
	 NhlBoolean	init,
	 float		**levels,
	 NhlBoolean	*modified)
#else
(new,old,init,levels,modified)
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
	NhlStreamlinePlotLayer		stnew = (NhlStreamlinePlotLayer) new;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayer		stold = (NhlStreamlinePlotLayer) old;
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	float			min,max;

	entry_name = init ? InitName : SetValuesName;
	*modified = False;

	if ((! init) && 
	    (! stp->data_changed) &&
	    (! stp->level_spacing_set) && 
	    (stp->levels == ostp->levels) &&
	    (stp->level_selection_mode == ostp->level_selection_mode) &&
	    (stp->max_level_count == ostp->max_level_count) &&
	    (stp->min_level_val == ostp->min_level_val) &&
	    (stp->max_level_val == ostp->max_level_val) &&
	    (stp->zero_field == ostp->zero_field) &&
	    (stp->use_scalar_array == ostp->use_scalar_array))
		return ret;

	stp->new_draw_req = True;
	if (stp->level_spacing_set && stp->level_spacing <= 0.0) {
		e_text = 
			"%s: Invalid level spacing value set: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
		stp->level_spacing = 5.0;
	}
	

	if (! stp->use_scalar_array) {
		min = stp->zmin;
		max = stp->zmax;
	}
	else if (! stp->scalar_data_init) {
		e_text = 
		      "%s: No scalar data: using streamline magnitude for levels";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
		min = stp->zmin;
		max = stp->zmax;
	}
	else {
		min = stp->scalar_min;
		max = stp->scalar_max;
	}
		
	if (stp->min_level_val >= stp->max_level_val) {
		e_text =
		"%s: Invalid level values set: defaulting to AUTOMATIC mode ";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
		stp->min_level_val = min;
		stp->max_level_val = max;
		stp->level_selection_mode = NhlAUTOMATICLEVELS;
	}
	if (max <= stp->min_level_val || min >=  stp->max_level_val) {
		e_text =
	   "%s: Data values and min/max levels are disjoint sets: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
		stp->min_level_val = min;
		stp->max_level_val = max;
	}
	
	switch (stp->level_selection_mode) {

	case NhlMANUALLEVELS:

		if (! stp->min_level_set) {
			subret = SetupLevelsAutomatic(stnew,stold,levels,
						      min,max,entry_name);
		}
		else {
			subret = SetupLevelsManual(stnew,stold,levels,
						   min,max,entry_name);
		}
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			return ret;
		}
		*modified = True;
		break;

	case NhlEQUALSPACEDLEVELS:

		subret = SetupLevelsEqual(stnew,stold,levels,
					  min,max,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			return ret;
		}
		*modified = True;
		break;

	case NhlAUTOMATICLEVELS:

		subret = SetupLevelsAutomatic(stnew,stold,levels,
					      min,max,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			return ret;
		}
		*modified = True;
		break;
			
	case NhlEXPLICITLEVELS:

		if (init && stp->levels == NULL) {
			subret = SetupLevelsAutomatic(stnew,stold,levels,
						      min,max,entry_name);
		}
		else {
			subret = SetupLevelsExplicit(stnew,stold,init,levels,
						     max,min,entry_name);
		}
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

	stp->min_level_set = True;
	stp->max_level_set = True;

		
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
#if	NhlNeedProto
	(NhlStreamlinePlotLayer	stnew, 
	 NhlStreamlinePlotLayer	stold,
	 float		**levels,
        float		min,
	float		max,
	 char		*entry_name)
#else
(stnew,stold,levels,entry_name)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	float		**levels;
        float		min,
	float		max,
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	int			i, count;
	float			lmin,lmax,spacing;
	float			*fp;

	spacing = stp->level_spacing;
	if (stp->min_level_set) {
		lmin = stp->min_level_val;
	}
	else {
		lmin = min;
	}
	
	if (stp->max_level_set) {
		lmax = stp->max_level_val;
	}
	else {
		lmax = floor(((max - lmin) / spacing) * spacing + lmin);
		if (_NhlCmpFAny(lmax,stp->zmax,6) == 0.0) {
			lmax -= spacing;
		}
		stp->max_level_val = lmax;
	}

	if (fmod((lmax - lmin), stp->level_spacing) > 0.0)
		count =	(lmax - lmin) / stp->level_spacing + 2.0;
	else
		count =	(lmax - lmin) / stp->level_spacing + 1.5;

	if (count <= 0) {
		e_text = 
	  "%s: stLevelSpacingF value exceeds or equals data range: defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = SetupLevelsAutomatic(stnew,stold,levels,
					   min,max,entry_name);
		return MIN(NhlWARNING,ret);
	}
	if (count >  Nhl_stMAX_LEVELS) {
		ret = MIN(NhlWARNING,ret);
		e_text = 
 "%s: stLevelSpacingF value causes level count to exceed maximum: defaulting";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		stp->level_spacing = (lmax - lmin) / 
			(stp->max_level_count - 1);
		count = stp->max_level_count;
	}
	else {
		stp->max_level_count = MAX(stp->max_level_count, count);
	}

	if ((*levels = (float *) 
	     NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	for (i=0, fp = *levels; i < count - 1; i++) {
		*(fp++) = lmin + i * stp->level_spacing;
	}
	*fp = lmax;

	stp->level_count = count;

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
#if	NhlNeedProto
	(NhlStreamlinePlotLayer	stnew,
	 NhlStreamlinePlotLayer	stold,
	 float		**levels,
        float		min,
	float		max,
	 char		*entry_name)
#else
(stnew,stold,levels,entry_name)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	float		**levels;
        float		min,
	float		max,
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	int			i;
	float			lmin,lmax,size;

	lmin = min;
	lmax = max;
	size = (lmax - lmin) / (stp->max_level_count + 1);

	stp->level_count = stp->max_level_count;
	if ((*levels = (float *) 
	     NhlMalloc(stp->level_count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	for (i=0; i < stp->level_count; i++) {
		(*levels)[i] = stp->zmin + (i+1) * size;
	}
	
	stp->min_level_val = (*levels)[0];
	stp->max_level_val = (*levels)[stp->level_count - 1];
	stp->level_spacing = size;

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
#if	NhlNeedProto
	(NhlStreamlinePlotLayer	stnew, 
	 NhlStreamlinePlotLayer	stold,
	 float		**levels,
        float		min,
	float		max,
	 char		*entry_name)
#else
(stnew,stold,levels,entry_name)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	float		**levels;
        float		min,
	float		max,
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	int			i,count = 0;

	float			lmin,lmax,ftmp,ftest;
	float			spacing;
	NhlBoolean		choose_spacing = True;

	lmin = min;
	lmax = max;

	if (stp->level_spacing_set) {
		spacing = stp->level_spacing;
		lmin = ceil(lmin / spacing) * spacing;
		lmax = MIN(lmax,floor(lmax / spacing) * spacing);
		count =	(int)((lmax - lmin) / stp->level_spacing + 1.5);
		if (_NhlCmpFAny(lmin,min,6) == 0.0) {
			lmin += spacing;
			count--;
		}
		if (_NhlCmpFAny(lmax,max,6) == 0.0) {
			lmax -= spacing;
			count--;
		}
		if (count <= 0) {
			ret = MIN(NhlWARNING,ret);
			lmin = min;
			lmax = max;
			e_text = 
	  "%s: stLevelSpacingF value exceeds or equals data range: defaulting";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		}
		else if (count >  Nhl_stMAX_LEVELS) {
			ret = MIN(NhlWARNING,ret);
			e_text = 
 "%s: stLevelSpacingF value causes level count to exceed maximum: defaulting";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		}
		else {
			stp->max_level_count = 
				MAX(stp->max_level_count, count);
			choose_spacing = False;
		}
	}
	if (choose_spacing) {
		subret = ChooseSpacingLin(&lmin,&lmax,&spacing,7,
					  stp->max_level_count,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			e_text = "%s: error choosing spacing";
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		if (_NhlCmpFAny(lmin,min,6) == 0.0) {
			lmin += spacing;
		}
		ftmp = lmin;
		ftest = max;
		count = 0;
		while (_NhlCmpFAny(ftmp,ftest,6) < 0.0) {
			count++;
			ftmp = lmin + count * spacing;
		}
	}
	if (count == 0) {
		e_text = "%s: error setting automatic levels";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

	if ((*levels = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	for (i =  0; i <  count; i++)
		(*levels)[i] = lmin + i * spacing;

	stp->level_spacing = spacing;
	stp->level_count = count;
	stp->min_level_val = lmin;
	stp->max_level_val = (*levels)[count - 1];

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
#if	NhlNeedProto
	(NhlStreamlinePlotLayer	stnew, 
	 NhlStreamlinePlotLayer	stold,
	 NhlBoolean		init,
	 float			**levels,
	 float			min,
	 float			max,
	 char			*entry_name)
#else
(stnew,stold,levels,entry_name)
	NhlStreamlinePlotLayer	stnew;
	NhlStreamlinePlotLayer	stold;
	NhlBoolean	init;
	float		**levels;
        float		min,
	float		max,
	char		*entry_name;

#endif

{
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &(stnew->streamlineplot);
	NhlStreamlinePlotLayerPart	*ostp = &(stold->streamlineplot);
	int			i,j,count;
	float			*fp;
	float			ftmp;

	if (stp->levels == NULL || stp->levels->num_elements < 1) {
		ret = MIN(NhlWARNING,ret);
		e_text = 
	      "%s: %s is NULL: defaulting to Automatic level selection mode";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,NhlNstLevels);
		return SetupLevelsAutomatic(stnew,stold,levels,
					    min,max,entry_name);
	}
	if (init || stp->levels != ostp->levels)
		count = stp->levels->num_elements;
	else 
		count = stp->level_count;

	if (count > Nhl_stMAX_LEVELS) {
		ret = MIN(NhlWARNING,ret);
		e_text = 
  "%s: Explicit level array count exceeds max level count: defaulting to Automatic level selection mode";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		return SetupLevelsAutomatic(stnew,stold,levels,
					    min,max,entry_name);
	}
/*
 * Allocate space for the levels
 */
	fp = (float *) stp->levels->data;
	if ((*levels = (float *) NhlMalloc(count * sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}
	for (i = 0; i < count; i++)
		(*levels)[i] = fp[i];

	fp = *levels;
		
/*
 * Sort the array into ascending order
 */
	for (i = 0; i < count; i++) {
		int min = i;
		for (j = i + 1; j < count; j++)
			if (fp[j] < fp[min])
				min = j;
		if (min != i) {
			ftmp = fp[min];
			fp[min] = fp[i];
			fp[i] = ftmp;
		}
	}
/*
 * Find the average spacing
 */
	ftmp = 0;
	for (i = 1; i < count; i++) {
		ftmp += fp[i] - fp[i-1];
	}

	stp->level_spacing = ftmp / (count - 1);
	stp->min_level_val = fp[0];
	stp->max_level_val = fp[count - 1];

	stp->level_count = count;

	return ret;
}

/*
 * Function:	ChooseSpacingLin
 *
 * Description: Ethan's tick mark spacing code adapted to choosing 'nice'
 *		StreamlinePlot values; adapted by exchanging the ciel and floor
 *		functions - since the max and min streamline values must be
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
#if	NhlNeedProto
(
	float *tstart,
	float *tend,
	float *spacing,
	int convert_precision,
	int max_ticks,
	NhlString entry_name)
#else
(tstart,tend,spacing,convert_precision,max_ticks,entry_name)
	float *tstart;
	float *tend;
	float *spacing;
	int	convert_precision;
	int	max_ticks;
#endif
{
	double	table[] = 
	{ 1.0,2.0,2.5,4.0,5.0,
		  10.0,20.0,25.0,40.0,50.0,
		  100.0,200.0,250.0,400.0,500.0 };
	double	d,u,t,am1,ax1;
	double	am2=0.0,ax2=0.0;
	int	npts = 15;
	int	i;
	char	*e_text;

	if(_NhlCmpFAny(*tend,*tstart,8)<=0.0) {
		e_text = "%s: Streamline field is constant";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
		
	d = pow(10.0,floor(log10(*tend-*tstart)) - 2.0);
	u = *spacing = 1e30;
	for(i=0;i<npts; i++) {
		t = table[i] * d;
		am1 = ceil(*tstart/t) *t;
		ax1 = floor(*tend/t) * t;
		if(((i>=npts-1)&&(*spacing == u))||
		   ((t <= *spacing)&&
		    (_NhlCmpFAny((ax1-am1)/t,(double)max_ticks,
				 convert_precision) <= 0.0))){
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
 * Function:  hlustmpxy
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

void (_NHLCALLF(hlustmpxy,HLUSTMPXY))
#if	NhlNeedProto
(
	float *xda,
	float *yda, 
	float *xus, 
	float *yus, 
	int *ist
)
#else
(xda,yda,xus,yus,ist)
	float *xda;
	float *yda; 
	float *xus; 
	float *yus; 
	int   *ist;
#endif
{
	int status = 1;
	float xdata,ydata;

	*ist = 0;
        
        if (Stp == NULL) {
		_NHLCALLF(stmpxy,STMPXY)(xda,yda,xus,yus,ist);
		return;
	}

        if (Need_Info) {
		_NHLCALLF(stgetmapinfo,STGETMAPINFO) 
                        (&Imap,&Itrt,&Vnml,&Dfmg,&Wxmn,&Wxmx,&Wymn,&Wymx,
                         &Xdlo,&Xdhi,&Ydlo,&Ydhi);
                Need_Info = False;
#if 0
		printf("%f,%f,%f,%f\n",Wxmn,Wxmx,Wymn,Wymx);
#endif
        }

	if (Overlay_Trans_Obj == NULL) {

		_NhlCompcToWin(Trans_Obj,xda,yda,1,xus,yus,&status,NULL,NULL);
		if(status) {
			*ist = -5;
			return;
		}
	} else { /* do overlay */

		_NhlCompcToData(Trans_Obj,xda,yda,1,&xdata,&ydata,
			&status,NULL,NULL);
		if(status) {
			*ist = -5;
			return;
		}

		_NhlDataToWin(Overlay_Trans_Obj,
			      &xdata, &ydata,1,xus,yus,
			      &status,NULL,NULL);
		if(status) {
			*ist = -5;
			return;
		}
			
	}
#if 0
	printf("compc %f %f : win %f %f\n", *xda, *yda, *xus, *yus);
#endif

	return;
}


/*
 * Function:  hlustimxy
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

void (_NHLCALLF(hlustimxy,HLUSTIMXY))
#if	NhlNeedProto
(
	float *xus,
	float *yus, 
	float *xda, 
	float *yda, 
	int   *ist
)
#else
(xus,yus,xda,yda,ist)
	float *xus;
	float *yus; 
	float *xda; 
	float *yda; 
	int   *ist;
#endif
{
	int status = 1;
	float xdata,ydata;

        if (Stp == NULL) {
		_NHLCALLF(stimxy,STIMXY)(xus,yus,xda,yda,ist);
		return;
	}
	*ist = 0;

	if (Overlay_Trans_Obj == NULL) {
		_NhlWinToCompc(Trans_Obj,xus,yus,1,xda,yda,&status,NULL,NULL);
		if(status) {
			*ist = -5;
			return;
		}
	} else { /* do overlay */

		_NhlWinToData(Overlay_Trans_Obj,
			      xus,yus,1,&xdata,&ydata,
			      &status,NULL,NULL);
		if(status) {
			*ist = -5;
			return;
		}
		_NhlDataToCompc(Trans_Obj,&xdata,&ydata,1,xda,yda,
			&status,NULL,NULL);
		if(status) {
			*ist = -5;
			return;
		}
			
	}
	if (*xda < Xdlo || *xda > Xdhi ||
	    *yda < Ydlo || *yda > Ydhi) {
		*ist = -1;
	}
#if 0
	printf("win %f %f : compc %f %f", *xus, *yus, *xda, *yda);
#endif

	return;
}

/*
 * Function:  hlustmpta
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

void (_NHLCALLF(hlustmpta,HLUSTMPTA))
#if	NhlNeedProto
(
	float *xda, 
	float *yda, 
	float *xus,
	float *yus, 
	float *xnd,
	float *ynd, 
	float *du,
	float *dv, 
	float *ta, 
	int *ist
)
#else
(xda,yda,xus,yus,xnd,ynd,du,dv,ta,ist)
	float *xda; 
	float *yda; 
	float *xus;
	float *yus; 
	float *xnd;
	float *ynd; 
	float *du;
	float *dv; 
	float *ta; 
	int *ist;
#endif
{
#define DEG2RAD 0.017453292519943 

        NhlLayer trans_p;
	int status = 1;
        float xe,ye,xt,yt,xtf,ytf,xd,yd;
        float dv1,dv2,duv;
        int count = 0,max_count = 25;
        float sign = 1.0,prec_fac = 1.0e5,pvfrac = 0.1;
	float xdata,ydata;
	NhlBoolean sign_changed = False;

        if (Stp == NULL) {
		_NHLCALLF(stmpta,STMPTA)
			(xda,yda,xus,yus,xnd,ynd,du,dv,ta,ist);
		return;
	}

	*ist = 0;
        trans_p = (Overlay_Trans_Obj == NULL) ? Trans_Obj : Overlay_Trans_Obj;

        if (! Over_Map) {
		xe = *xnd + *du;
		ye = *ynd + *dv;

                if (Stp->map_direction) {
			_NhlCompcToData(trans_p,xda,yda,1,&xdata,&ydata,
					&status,NULL,NULL);
			if(status) {
				*ist = -5;
				return;
			}
                        dv1 = sqrt((*du)*(*du)+(*dv)*(*dv));

		retry0:
                        /* set up inital increment factor */

                        duv = pvfrac / Vnml;

                        while (count < max_count) {
                                xt = xdata + sign * *du * duv;
                                yt = ydata + sign * *dv * duv;
				_NhlDataToWin(trans_p,&xt,&yt,1,&xt,&yt,
					      &status,NULL,NULL);

                                if (xt < Wxmn || xt > Wxmx ||
                                    yt < Wymn || yt > Wymx) {
                                        if (sign == -1.0) {
                                                *ist = -4;
                                                return;
                                        }
                                        sign = -1.0;
                                        continue;
                                }
                                xtf = c_cufx(xt);
                                ytf = c_cufy(yt);
                                xd = xtf - *xnd;
                                yd = ytf - *ynd;
                                dv2 = sqrt(xd*xd + yd*yd);
                                if (10*dv2 > Dfmg) {
                                        count += 1;
                                        duv /= 2.0;
                                        continue;
                                }
                                else if (100*dv2 < Dfmg) {
                                        count += 1;
                                        duv *= 2.0;
                                        continue;
                                }
				break;
			}
			xe = *xnd + sign*xd*dv1/dv2;
			ye = *ynd + sign*yd*dv1/dv2;
			if (count == max_count) {
				if (! sign_changed) {
					sign_changed = True;
					sign = -1.0;
					count = 0;
					goto retry0;
				}
				else {
					*ist = -5;
					return;
				}
			}
		}
		errno = 0;
		*ta = atan2((ye-*ynd),(xe-*xnd));
		if (errno == EDOM)
			printf("ye %f *ynd %f xe %f *xnd %f\n",
			       ye,*ynd,xe,*xnd);
		else if (errno == ERANGE)
			printf("*ta %f\n",*ta);
		else if (errno != 0)
			printf ("errno - %d\n",errno);
	}
	else {
		float cos_lat;
		float costest = 90.0e5;
		float dtx,dty,dnx,dny,xdata,ydata;

		_NhlCompcToData(Trans_Obj,xda,yda,1,&xdata,&ydata,
			&status,NULL,NULL);
		if(status) {
			*ist = -5;
			return;
		}
		if ((int)(ydata*prec_fac+0.5) >= (int)costest) {
			*ist = - 1;
			return;
		}
	retry1:
		duv = pvfrac / Vnml;
		cos_lat = cos(*yda * DEG2RAD);
		dtx = *du / cos_lat;
		dty = *dv;
		while ( count < max_count) {
			dnx = xdata + sign * dtx * duv;
			dny = ydata + sign * dty * duv;
/*
			c_maptra(ydata+sign*dny,xdata+sign*dnx,&xt,&yt);
*/
			_NhlDataToWin(trans_p,&dnx,&dny,1,&xt,&yt,
			      &status,NULL,NULL);
			if (status)
				xt = dnx, yt = dny;

			if (xt < Wxmn || xt > Wxmx ||
			    yt < Wymn || yt > Wymx) {
				if (sign == -1.0) {
					*ist = -4;
					return;
				}
				sign = -1.0;
				continue;
			}
			xtf = c_cufx(xt);
			ytf = c_cufy(yt);
			xd = xtf - *xnd;
			yd = ytf - *ynd;
			dv2 = sqrt(xd*xd + yd*yd);
			if (10*dv2 > Dfmg) {
				count += 1;
				duv /= 2.0;
				continue;
			}
			else if (100*dv2 < Dfmg) {
				count += 1;
				duv *= 2.0;
				continue;
			}
			break;
		}
		if (count == max_count) {
			if (! sign_changed) {
				sign_changed = True;
				sign = -1.0;
				count = 0;
				goto retry1;
			}
			else {
				*ist = -5;
				return;
			}
		}
		errno = 0;
		*ta = atan2(sign*yd,sign*xd);
		if (errno == EDOM)
			printf("ytf %f *ynd %f xtf %f *xnd %f\n",
			       ytf,*ynd,xtf,*xnd);
		else if (errno == ERANGE)
			printf("*ta %f\n",*ta);
		else if (errno != 0)
			printf ("errno - %d\n",errno);
	}

			
	return;
}


/*
 * Function:  load_hlust_routines
 *
 * Description: Forces the hlust... routines to load from the HLU library
 *
 * In Args:   NhlBoolean flag - should always be False - dont actually
 *			        want to call the routines.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static void   load_hlust_routines
#if	NhlNeedProto
(
	NhlBoolean	flag
)
#else
(flag)
	NhlBoolean	flag;
#endif
{
	if (flag) {
		float fdm;
		int   idm;
		(_NHLCALLF(hlustmpxy,HLUSTMPXY))
			(&fdm,&fdm,&fdm,&fdm,&idm);
		(_NHLCALLF(hlustimxy,HLUSTIMXY))
			(&fdm,&fdm,&fdm,&fdm,&idm);
		(_NHLCALLF(hlustmpta,HLUSTMPTA))
			(&fdm,&fdm,&fdm,&fdm,&fdm,&fdm,&fdm,&fdm,&fdm,&idm);
	}
	return;
}

