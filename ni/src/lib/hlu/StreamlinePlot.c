/*
 *      $Id: StreamlinePlot.c,v 1.52 1999-03-29 18:31:34 dbrown Exp $
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
 	{NhlNstStreamlineDrawOrder,NhlCstStreamlineDrawOrder,NhlTDrawOrder,
		 sizeof(NhlDrawOrder),Oset(streamline_order),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlDRAW),0,NULL},

	{NhlNstMapDirection,NhlCstMapDirection,
		  NhlTBoolean,sizeof(NhlBoolean),
		  Oset(map_direction),NhlTImmediate,
		  _NhlUSET((NhlPointer) True),0,NULL},

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(step_size_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNstStepSizeF,NhlCstStepSizeF,
		  NhlTFloat,sizeof(float),Oset(step_size),NhlTProcedure,
		  _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(min_line_spacing_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNstMinLineSpacingF,NhlCstMinLineSpacingF,
		  NhlTFloat,sizeof(float),Oset(min_line_spacing),NhlTProcedure,
		  _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
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
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(arrow_length_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNstArrowLengthF,NhlCstArrowLengthF,
		  NhlTFloat,sizeof(float),Oset(arrow_length),NhlTProcedure,
		  _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(min_arrow_spacing_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNstMinArrowSpacingF,NhlCstMinArrowSpacingF,
		  NhlTFloat,sizeof(float),
                 Oset(min_arrow_spacing),NhlTProcedure,
		  _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNstLineThicknessF,NhlCLineThicknessF,
		  NhlTFloat,sizeof(float),Oset(line_thickness),NhlTString,
		  _NhlUSET("1.0"),0,NULL},
	{NhlNstLineColor, NhlCLineColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(line_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},


	{NhlNstNoDataLabelOn,NhlCAnnotationLabelsOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(zerof_lbl.string2_on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNstNoDataLabelString,NhlCstNoDataLabelString,
		 NhlTString,sizeof(NhlString),Oset(zerof_lbl.string2),
		 NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree},

/* Zero field label resources */

	{NhlNstZeroFLabelOn,NhlCAnnotationLabelsOn,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(zerof_lbl.on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNstZeroFLabelString,NhlCstZeroFLabelString,
		 NhlTString,sizeof(NhlString),Oset(zerof_lbl.string1),
		 NhlTImmediate,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(zerof_lbl.height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
        {NhlNstZeroFLabelFontHeightF,NhlCFontHeightF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
        {NhlNstZeroFLabelTextDirection,NhlCTextDirection,
		 NhlTTextDirection,sizeof(NhlTextDirection),
		 Oset(zerof_lbl.direction),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlACROSS),0,NULL},
	{NhlNstZeroFLabelFont,NhlCFont,NhlTFont, 
		 sizeof(int),Oset(zerof_lbl.font),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNstZeroFLabelFontColor,NhlCFontColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(zerof_lbl.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNstZeroFLabelFontAspectF,NhlCFontAspectF,NhlTFloat, 
		 sizeof(float),Oset(zerof_lbl.aspect),
		 NhlTString, _NhlUSET("1.3125"),0,NULL},
	{NhlNstZeroFLabelFontThicknessF,NhlCFontThicknessF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.thickness),
		 NhlTString, _NhlUSET("1.0"),0,NULL},
	{NhlNstZeroFLabelFontQuality,NhlCFontQuality,
		 NhlTFontQuality, 
		 sizeof(NhlFontQuality),Oset(zerof_lbl.quality),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlHIGH),0,NULL},
	{NhlNstZeroFLabelConstantSpacingF,NhlCTextConstantSpacingF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.cspacing),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNstZeroFLabelAngleF,NhlCTextAngleF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.angle),
		 NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNstZeroFLabelFuncCode,NhlCTextFuncCode,NhlTCharacter, 
		 sizeof(char),Oset(zerof_lbl.fcode[0]),
		 NhlTString, _NhlUSET(":"),0,NULL},
	{NhlNstZeroFLabelBackgroundColor,NhlCFillBackgroundColor,
		 NhlTColorIndex,sizeof(NhlColorIndex),
		 Oset(zerof_lbl.back_color),NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlBACKGROUND),0,NULL},
	{NhlNstZeroFLabelPerimOn,NhlCEdgesOn,
                 NhlTBoolean,sizeof(NhlBoolean),Oset(zerof_lbl.perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL},
	{NhlNstZeroFLabelPerimSpaceF,NhlCEdgeBorderWidthF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.perim_space),
		 NhlTString,_NhlUSET("0.33"),0,NULL},
	{NhlNstZeroFLabelPerimColor,NhlCEdgeColor,
		 NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(zerof_lbl.perim_lcolor),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNstZeroFLabelPerimThicknessF,NhlCEdgeThicknessF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl.perim_lthick),
		 NhlTString, _NhlUSET("1.0"),0,NULL},

	{NhlNstZeroFLabelZone,NhlCstZeroFLabelZone,NhlTInteger,
		 sizeof(int),Oset(zerof_lbl_rec.zone),NhlTImmediate,
		 _NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNstZeroFLabelSide,NhlCstZeroFLabelSide,NhlTPosition,
		 sizeof(NhlPosition),Oset(zerof_lbl_rec.side),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlBOTTOM),0,NULL},
	{NhlNstZeroFLabelJust,NhlCTextJustification,
		 NhlTJustification,sizeof(NhlJustification),
		 Oset(zerof_lbl_rec.just),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNstZeroFLabelParallelPosF,NhlCstZeroFLabelParallelPosF,NhlTFloat,
		 sizeof(float),Oset(zerof_lbl_rec.para_pos),NhlTString,
		 _NhlUSET("0.0"),0,NULL},
	{NhlNstZeroFLabelOrthogonalPosF,NhlCstZeroFLabelOrthogonalPosF,
		 NhlTFloat,sizeof(float),Oset(zerof_lbl_rec.ortho_pos),
		 NhlTString,_NhlUSET("0.0"),0,NULL},

/* End-documented-resources */

	{NhlNstDataChanged,NhlCstDataChanged,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(data_changed),
		 NhlTImmediate,_NhlUSET((NhlPointer) True),
         	 _NhlRES_PRIVATE,NULL},

/* Intercepted resources */
	{NhlNtrXTensionF,NhlCtrXTensionF,NhlTFloat,sizeof(float),
		Oset(x_tension),NhlTString,"2.0",
         	_NhlRES_DEFAULT|_NhlRES_INTERCEPTED,NULL},
	{NhlNtrYTensionF,NhlCtrYTensionF,NhlTFloat,sizeof(float),
		Oset(y_tension),NhlTString,"2.0",
         	_NhlRES_DEFAULT|_NhlRES_INTERCEPTED,NULL},

#if 0        
	{ NhlNpmLabelBarDisplayMode,NhlCpmLabelBarDisplayMode,
		 NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		 Oset(display_labelbar),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),
          	 _NhlRES_INTERCEPTED,NULL},
	{NhlNpmLegendDisplayMode,NhlCpmLegendDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_legend),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlNOCREATE),
         	  _NhlRES_INTERCEPTED,NULL},
#endif        
	{NhlNpmTickMarkDisplayMode,NhlCpmTickMarkDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_tickmarks),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlCONDITIONAL),
         	  _NhlRES_INTERCEPTED,NULL},
	{NhlNpmTitleDisplayMode,NhlCpmTitleDisplayMode,
		  NhlTAnnotationDisplayMode,sizeof(NhlAnnotationDisplayMode),
		  Oset(display_titles),
		  NhlTImmediate,_NhlUSET((NhlPointer) NhlCONDITIONAL),
         	  _NhlRES_INTERCEPTED,NULL},
	{ NhlNpmUpdateReq,NhlCpmUpdateReq,NhlTInteger,sizeof(int),
		  Oset(update_req),
		  NhlTImmediate,_NhlUSET((NhlPointer) False),
          	  _NhlRES_PRIVATE,NULL}
        
/* unused resources
 * These are resources that may be used when streamlines is enhanced to
 * do colored and/or variable width streamlines
 */

/* Level resources */
#if 0
	{NhlNstScalarFieldData,NhlCstScalarFieldData,_NhlTDataList,
		 sizeof(NhlGenArray),
		 Oset(scalar_field_data),NhlTImmediate,_NhlUSET(NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
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
		 _NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
	{ NhlNstLevelSpacingF,NhlCstLevelSpacingF,NhlTFloat,sizeof(float),
		  Oset(level_spacing),NhlTProcedure,
		  _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(min_level_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNstMinLevelValF,NhlCstMinLevelValF,NhlTFloat,sizeof(float),
		  Oset(min_level_val),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(max_level_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{ NhlNstMaxLevelValF,NhlCstMaxLevelValF,NhlTFloat,sizeof(float),
		  Oset(max_level_val),
		  NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},

	{NhlNstUseScalarArray,NhlCstUseScalarArray,
		  NhlTBoolean,sizeof(NhlBoolean),
		  Oset(use_scalar_array),NhlTImmediate,
		  _NhlUSET((NhlPointer) False),0,NULL},
	{NhlNstMonoStreamlineLineColor,NhlCstMonoStreamlineLineColor,
		  NhlTBoolean,sizeof(NhlBoolean),
		  Oset(mono_streamline_line_color),NhlTImmediate,
		  _NhlUSET((NhlPointer) True),0,NULL},
	{NhlNstStreamlineLineColor, NhlCLineColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(streamline_line_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNstMonoStreamlineFillColor,NhlCstMonoStreamlineFillColor,
		  NhlTBoolean,sizeof(NhlBoolean),
		  Oset(mono_streamline_fill_color),NhlTImmediate,
		  _NhlUSET((NhlPointer) True),0,NULL},
	{NhlNstStreamlineFillColor, NhlCFillColor, NhlTColorIndex,
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
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(min_line_length_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNstMinLineLengthF,NhlCstMinLineLengthF,
		  NhlTFloat,sizeof(float),Oset(min_line_length),NhlTProcedure,
		  _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
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
	{NhlNstMagnitudeFormat,NhlCNumberFormat,
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
	{NhlNstScalarValueFormat,NhlCNumberFormat,
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
	{NhlNstLabelFontColor,NhlCFontColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(lbls.color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(lbls.height_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	 _NhlRES_PRIVATE,NULL},
        {NhlNstLabelFontHeightF,NhlCFontHeightF,
		 NhlTFloat,sizeof(float),Oset(lbls.height),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
#endif        
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
	NhlDrawOrder	order,
	NhlString	entry_name
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
        NhlStreamlinePlotLayer	stl,
        NhlStreamlinePlotLayer	ostl,
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

static NhlErrorTypes    ManageVectorData(
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
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

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
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

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
static NrmQuark	Qno_data_label_string = NrmNULLQUARK; 
static NrmQuark	Qzerof_label_string = NrmNULLQUARK; 

static char *InitName = "StreamlinePlotInitialize";
static char *SetValuesName = "StreamlinePlotSetValues";

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
	Qzerof_label_string = NrmStringToQuark(NhlNstZeroFLabelString);
	Qno_data_label_string = NrmStringToQuark(NhlNstNoDataLabelString);

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
 * NOTE: order of registration should be the reverse of the
 * desired 'canonical' order
 */
	subret = _NhlRegisterChildClass(lc,NhlplotManagerClass,
					False,False,
#if 0                                        
					NhlNpmLegendDisplayMode,
					NhlNpmLabelBarDisplayMode,
#endif                                        
					NhlNpmTickMarkDisplayMode,
					NhlNpmTitleDisplayMode,
					NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlplotManagerClass");
		return(NhlFATAL);
	}
        
        subret = _NhlRegisterChildClass(lc,NhlirregularTransObjClass,
					False,True,NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlirregularTransObjClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhllogLinTransObjClass,
					False,True,NULL);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhltransObjClass");
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
			  "NhlvectorFieldFloatClass");
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
#if 0        
	if (! stp->min_line_length_set) stp->min_line_length = 0.0;
#endif        
	       
	if (! stp->zerof_lbl.height_set) 
		stp->zerof_lbl.height = 0.01;

/* Initialize private members */

	stp->new_draw_req = True;
	stp->predraw_dat = NULL;
	stp->draw_dat = NULL;
	stp->postdraw_dat = NULL;
	stp->update_req = False;
	stp->overlay_object = NULL;
	stp->data_changed = True;
	stp->data_init = False;
	stp->use_irr_trans = False;
	stp->zero_field = False;
	stp->vfp = NULL;
	stp->ovfp = NULL;
	stp->fws_id = -1;

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

/* 
 * Set up the labels (except for the line label array) 
 * Note: may add arguments to the PlotManager argument list.
 */

	subret = ManageLabels(stnew,(NhlStreamlinePlotLayer)req,
			      True,sargs,&nargs);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error initializing labels";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}


/* Set up the streamline object transformation  */

 
	subret = InitCoordBounds(stnew,NULL,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return(ret);
        
	if (stp->use_irr_trans) {
		subret = SetUpIrrTransObj(stnew,
					  (NhlStreamlinePlotLayer) req,True);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error setting up transformation";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}
	else {
		subret = SetUpLLTransObj(stnew,
					 (NhlStreamlinePlotLayer) req,True);
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
	stp->zerof_lbl.height_set = False;
	stp->arrow_length_set = False;
	stp->step_size_set = False;
	stp->min_line_spacing_set = False;
	stp->min_arrow_spacing_set = False;
#if 0        
	stp->min_line_length_set = False;
#endif        
        stnew->trans.x_reverse_set = stnew->trans.y_reverse_set = False;
        stnew->trans.x_log_set = stnew->trans.y_log_set = False;
        stnew->trans.x_axis_type_set = stnew->trans.y_axis_type_set = False;
        stnew->trans.x_min_set = stnew->trans.y_min_set = False;
        stnew->trans.x_max_set = stnew->trans.y_max_set = False;

	return ret;
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
		NhlNpmTickMarkDisplayMode,
		NhlNpmTitleDisplayMode,
		NhlNpmLegendDisplayMode,
		NhlNpmLabelBarDisplayMode,
		NhlNstNoDataLabelOn,
		NhlNstNoDataLabelString,
		NhlNstZeroFLabelOn,
		NhlNstZeroFLabelString,
		NhlNstZeroFLabelFormat,
		NhlNstZeroFLabelFontHeightF,
		NhlNstZeroFLabelTextDirection,
		NhlNstZeroFLabelFont,
		NhlNstZeroFLabelFontColor,
		NhlNstZeroFLabelFontAspectF,
		NhlNstZeroFLabelFontThicknessF,
		NhlNstZeroFLabelFontQuality,
		NhlNstZeroFLabelConstantSpacingF,
		NhlNstZeroFLabelAngleF,
		NhlNstZeroFLabelFuncCode,
		NhlNstZeroFLabelBackgroundColor,
		NhlNstZeroFLabelPerimOn,
		NhlNstZeroFLabelPerimSpaceF,
		NhlNstZeroFLabelPerimColor,
		NhlNstZeroFLabelPerimThicknessF,
		NhlNstZeroFLabelZone,
		NhlNstZeroFLabelSide,
		NhlNstZeroFLabelJust,
		NhlNstZeroFLabelParallelPosF,
		NhlNstZeroFLabelOrthogonalPosF
			 
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
		stp->new_draw_req = True;
	}
	if (stnew->view.use_segments) {
                NhlTransDat *trans_dat = NULL;
                
		if (NewDrawArgs(args,num_args))
			stp->new_draw_req = True;
                
                if (stp->draw_dat)
                        trans_dat = stp->draw_dat;
                else if (stp->postdraw_dat)
                        trans_dat = stp->postdraw_dat;
                else if (stp->predraw_dat)
                        trans_dat = stp->predraw_dat;
                if (! _NhlSegmentSpansArea(trans_dat,
                                           stnew->view.x,
                                           stnew->view.x + stnew->view.width,
                                           stnew->view.y - stnew->view.height,
                                           stnew->view.y))
                        stp->new_draw_req = True;

	}

	if (_NhlArgIsSet(args,num_args,NhlNstArrowLengthF))
		stp->arrow_length_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNstStepSizeF))
		stp->step_size_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNstMinLineSpacingF))
		stp->min_line_spacing_set = True;
	if (_NhlArgIsSet(args,num_args,NhlNstMinArrowSpacingF))
		stp->min_arrow_spacing_set = True;
#if 0        
	if (_NhlArgIsSet(args,num_args,NhlNstMinLineLengthF))
		stp->min_line_length_set = True;
#endif        

	if (_NhlArgIsSet(args,num_args,NhlNstZeroFLabelFontHeightF))
		stp->zerof_lbl.height_set = True;

/* Manage the data */

	subret = ManageVectorData(stnew,stold,False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting view dependent resources";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

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

	subret = InitCoordBounds(stnew,(NhlStreamlinePlotLayer)old,entry_name);
	if ((ret = MIN(ret,subret)) < NhlWARNING) return(ret);
        
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
		subret = SetUpLLTransObj(stnew,
					 (NhlStreamlinePlotLayer) old,False);
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
	stp->zerof_lbl.height_set = False;
	stp->arrow_length_set = False;
	stp->step_size_set = False;
	stp->min_line_spacing_set = False;
	stp->min_arrow_spacing_set = False;
#if 0        
	stp->min_line_length_set = False;
#endif        

        stnew->trans.x_reverse_set = stnew->trans.y_reverse_set = False;
        stnew->trans.x_log_set = stnew->trans.y_log_set = False;
        stnew->trans.x_axis_type_set = stnew->trans.y_axis_type_set = False;
        stnew->trans.x_min_set = stnew->trans.y_min_set = False;
        stnew->trans.x_max_set = stnew->trans.y_max_set = False;

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
		ts = NULL;
		if(args[i].quark == Qno_data_label_string){
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
        }

        return(NhlNOERROR);
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
			subret = NhlUnregisterAnnotation
				(inst->base.id,stp->zerof_anno_id);
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
        
        if (stp->zerof_lbl.string2 != NULL)
                NhlFree(stp->zerof_lbl.string2);
        if (stp->zerof_lbl.string1 != NULL)
                NhlFree(stp->zerof_lbl.string1);
	if (stp->zerof_lbl.text1 != NULL)
                NhlFree(stp->zerof_lbl.text1);
	if (stp->zerof_lbl.text2 != NULL)
                NhlFree(stp->zerof_lbl.text2);

	if (stp->predraw_dat != NULL)
		_NhlDeleteViewSegment(inst,stp->predraw_dat);
	if (stp->draw_dat != NULL)
		_NhlDeleteViewSegment(inst,stp->draw_dat);
	if (stp->postdraw_dat != NULL)
		_NhlDeleteViewSegment(inst,stp->postdraw_dat);
        
        if (stp->fws_id > 0)
                _NhlFreeWorkspace(stp->fws_id);

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
	NhlTransformLayerPart		*tfp = &(stl->trans);
	NhlString		e_text;

	NhlVASetValues(stl->base.wkptr->base.id,
		       _NhlNwkReset,	True,
		       NULL);
/*
 * Set up LLU interface coordinate boundaries 
 */
        stp->xlb = MAX(tfp->x_min,MIN(tfp->data_xstart,tfp->data_xend));
        stp->xub = MIN(tfp->x_max,MAX(tfp->data_xstart,tfp->data_xend));
        stp->ylb = MAX(tfp->y_min,MIN(tfp->data_ystart,tfp->data_yend));
        stp->yub = MIN(tfp->y_max,MAX(tfp->data_ystart,tfp->data_yend));
        
	if (!stp->use_irr_trans) {
                stp->xc1 = tfp->data_xstart;
                stp->xcm = tfp->data_xend;
                stp->yc1 = tfp->data_ystart;
                stp->ycn = tfp->data_yend;
        }
        else {
                int xcount,ycount;
                
                xcount = tfp->x_axis_type == NhlIRREGULARAXIS ?
                        stp->vfp->x_arr->len_dimensions[0] : 3;
                ycount = tfp->y_axis_type == NhlIRREGULARAXIS ?
                        stp->vfp->y_arr->len_dimensions[0] : 3;
                
                stp->xc1 = 0;
                stp->xcm = xcount - 1;
                stp->yc1 = 0;
                stp->ycn = ycount - 1;
        }

	return ret;
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

	if (stl->view.use_segments && stp->current_trans_dat) {
		_NhlEndSegment(stp->current_trans_dat);
	}
	stp->current_trans_dat = NULL;

	if (stp->wk_active) {
		_NhlDeactivateWorkstation(stl->base.wkptr);
		stp->wk_active = False;
	}

	if (stp->low_level_log_on) {
		NhlVASetValues(tfp->trans_obj->base.id,
			       NhlNtrLowLevelLogOn,False,NULL);
                stp->low_level_log_on = False;
        }

	if (stp->fws != NULL) {
		_NhlIdleWorkspace(stp->fws);
		stp->fws = NULL;
	}

	e_text = "%s: draw error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,"StreamlinePlotDraw");
}

/*
 * Function:	stUpdateTrans
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

static NhlErrorTypes stUpdateTrans
#if	NhlNeedProto
(
	NhlStreamlinePlotLayer	stl,
        NhlBoolean		seg_draw,
	NhlString		entry_name
)
#else
(stl,seg_draw,entry_name)
        NhlStreamlinePlotLayer stl;
        NhlBoolean		seg_draw;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &(stl->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stl->trans);

/*
 * If the plot is an overlay member, use the overlay manager's trans object.
 */
        Over_Map = False;
        Overlay_Trans_Obj = NULL;
        Trans_Obj = tfp->trans_obj;
        stp->low_level_log_on = False;
        
	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    ! tfp->do_ndc_overlay &&
	    tfp->overlay_trans_obj != NULL) {
		stp->trans_obj = tfp->overlay_trans_obj;
                Overlay_Trans_Obj = tfp->overlay_trans_obj;
                if ((stp->trans_obj->base.layer_class)->base_class.class_name
		    == NhlmapTransObjClass->base_class.class_name) {
                        Over_Map = True;	
			subret = NhlVASetValues
				(stp->trans_obj->base.id,
				 NhlNtrDataXStartF,tfp->data_xstart,
				 NhlNtrDataXEndF,tfp->data_xend,
				 NULL);

			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				return(ret);
			}
                }
		else if (stp->do_low_level_log) {
			if (tfp->x_axis_type == NhlLOGAXIS) {
				subret = NhlVASetValues
                                        (tfp->trans_obj->base.id,
                                         NhlNtrXAxisType,NhlLINEARAXIS,
                                         NULL);
			}
			else {
				subret = NhlVASetValues
                                        (tfp->trans_obj->base.id,
                                         NhlNtrYAxisType,NhlLINEARAXIS,
                                         NULL);
			}
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				return(ret);
			}
		}
	}
	else {
		stp->trans_obj = tfp->trans_obj;

		if (stp->do_low_level_log && ! seg_draw) {
			subret = NhlVASetValues(tfp->trans_obj->base.id,
						NhlNtrLowLevelLogOn,True,
						NULL);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				return(ret);
			}
                        stp->low_level_log_on = True;
		}
		if (stp->do_low_level_log ||
                    tfp->overlay_status == _tfNotInOverlay ||
		    tfp->do_ndc_overlay) {
			subret = _NhlSetTrans(tfp->trans_obj, (NhlLayer)stl);
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
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlString		e_text,entry_name = "StreamlinePlotPreDraw";
	NhlStreamlinePlotLayer		stl = (NhlStreamlinePlotLayer) layer;
	NhlStreamlinePlotLayerPart	*stp = &stl->streamlineplot;
        NhlBoolean		seg_draw;

	stp->fws = NULL;
	stp->wk_active = False;
	stp->current_trans_dat = NULL;

	if (! stp->data_init || stp->zero_field) {
		return NhlNOERROR;
	}

	Stp = stp;

	subret = stInitDraw(stl,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		Stp = NULL;
		return ret;
	}

	if (stp->streamline_order != NhlPREDRAW) {
		Stp = NULL;
		return NhlNOERROR;
	}
        
        seg_draw = stl->view.use_segments && ! stp->new_draw_req &&
		stp->predraw_dat && stp->predraw_dat->id != NgNOT_A_SEGMENT;
        
	subret = stUpdateTrans(stl,seg_draw,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		StreamlineAbortDraw(stl);
		return ret;
	}
        
	if (seg_draw) {
		ret = _NhltfDrawSegment((NhlLayer)stl,stp->trans_obj,
					stp->predraw_dat,entry_name);
	}
        else {
                subret = stDraw(stl,NhlPREDRAW,entry_name);
        }

	Stp = NULL;
	return MIN(subret,ret);
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
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlStreamlinePlotLayer	stl = (NhlStreamlinePlotLayer) layer;
	NhlStreamlinePlotLayerPart	*stp = &stl->streamlineplot;
	NhlString		e_text,entry_name = "StreamlinePlotDraw";
        NhlBoolean		seg_draw;

	if (! stp->data_init || stp->zero_field) {
		return NhlNOERROR;
	}
	if (stp->streamline_order != NhlDRAW) {
		return NhlNOERROR;
	}

	Stp = stp;
        
        seg_draw = stl->view.use_segments && ! stp->new_draw_req &&
		stp->draw_dat && stp->draw_dat->id != NgNOT_A_SEGMENT;
        
	subret = stUpdateTrans(stl,seg_draw,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		StreamlineAbortDraw(stl);
		return ret;
	}

	if (seg_draw) {
		subret = _NhltfDrawSegment((NhlLayer)stl,stp->trans_obj,
					stp->draw_dat,entry_name);
	}
        else {
                subret = stDraw
                        ((NhlStreamlinePlotLayer) layer,NhlDRAW,entry_name);
        }
        
	Stp = NULL;
	return MIN(subret,ret);
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
	NhlTransformLayerPart		*tfp = &stl->trans;
	NhlString		e_text,entry_name = "StreamlinePostPlotDraw";
        NhlBoolean		seg_draw;


	Stp = stp;

	if (! stp->data_init || stp->zero_field) {
		if (stp->display_zerof_no_data &&
		    tfp->overlay_status == _tfNotInOverlay) {
			subret = NhlDraw(stp->zerof_lbl_rec.id);
			ret = MIN(subret,ret);
		}
		Stp = NULL;
		return ret;
	}

        seg_draw = stl->view.use_segments && ! stp->new_draw_req &&
		stp->postdraw_dat && stp->postdraw_dat->id != NgNOT_A_SEGMENT;
        
        if (stp->streamline_order == NhlPOSTDRAW) {
                subret = stUpdateTrans(stl,seg_draw,entry_name);
                if ((ret = MIN(subret,ret)) < NhlWARNING) {
                        StreamlineAbortDraw(stl);
                        return ret;
                }
                if (seg_draw) {
                        subret = _NhltfDrawSegment
                                ((NhlLayer)stl,stp->trans_obj,
                                 stp->postdraw_dat,entry_name);
                }
                else {
                        subret = stDraw((NhlStreamlinePlotLayer) layer,
                                        NhlPOSTDRAW,entry_name);
                }
		ret = MIN(subret,ret);
	}

	stp->new_draw_req = False;
	Stp = NULL;


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
	NhlDrawOrder	order,
	NhlString	entry_name
)
#else
(stl,order,entry_name)
        NhlStreamlinePlotLayer stl;
	NhlDrawOrder	order;
	NhlString	entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlStreamlinePlotLayerPart	*stp = &(stl->streamlineplot);
	NhlTransformLayerPart	*tfp = &(stl->trans);
	float			*u_data,*v_data,*p_data;
	int			cix;

	subret = _NhlActivateWorkstation(stl->base.wkptr);
	if ((ret = MIN(subret,ret)) < NhlWARNING) {
		e_text = "%s: Error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		StreamlineAbortDraw(stl);
		return NhlFATAL;
	}
	stp->wk_active = True;

	if (stl->view.use_segments) {
                NhlTransDat **trans_dat_pp;
                switch (order) {
                    case NhlPREDRAW:
                            trans_dat_pp = &stp->predraw_dat;
                            break;
                    case NhlDRAW:
                            trans_dat_pp = &stp->draw_dat;
                            break;
                    case NhlPOSTDRAW:
                            trans_dat_pp = &stp->postdraw_dat;
                            break;
                }
		subret = _NhltfInitSegment((NhlLayer)stl,stp->trans_obj,
					    trans_dat_pp,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) {
			StreamlineAbortDraw(stl);
			return ret;
		}
		stp->current_trans_dat = *trans_dat_pp;
	}
        
	c_strset();
	
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

	if (stp->low_level_log_on && tfp->x_axis_type == NhlLOGAXIS) {
		c_stsetr("XC1",(float)tfp->data_xstart);
		c_stsetr("XCM",(float)tfp->data_xend);
	}
	else {
		c_stsetr("XC1",(float)stp->xc1);
		c_stsetr("XCM",(float)stp->xcm);
	}
	if (stp->low_level_log_on && tfp->y_axis_type == NhlLOGAXIS) {
		c_stsetr("YC1",(float)tfp->data_ystart);
		c_stsetr("YCN",(float)tfp->data_yend);
	}
	else {
		c_stsetr("YC1",(float)stp->yc1);
		c_stsetr("YCN",(float)stp->ycn);
	}
        c_stseti("SET",0);
        c_stseti("MAP",NhlstMAPVAL);

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

	/* set up the workspace */

	if (stp->fws_id < 1) {
		if ((stp->fws_id = 
		     _NhlNewWorkspace(NhlwsOTHER,NhlwsNONE,
			       2 * stp->vfp->fast_len * stp->vfp->slow_len *
				      sizeof(float))) < 1) {
			e_text = "%s: float workspace allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			StreamlineAbortDraw(stl);
			return NhlFATAL;
		}
	}
	if ((stp->fws = _NhlUseWorkspace(stp->fws_id)) == NULL) {
		e_text = "%s: error reserving float workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		StreamlineAbortDraw(stl);
		return(ret);
	}

	/* Draw the streamlines */

	Need_Info = True;

	u_data = &((float *) stp->vfp->u_arr->data)[stp->vfp->begin]; 
	v_data = &((float *) stp->vfp->v_arr->data)[stp->vfp->begin];

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

	if (stl->view.use_segments) {
		_NhlEndSegment(stp->current_trans_dat);
	}
	stp->current_trans_dat = NULL;

	if (stp->low_level_log_on) {
		subret = NhlVASetValues(tfp->trans_obj->base.id,
					NhlNtrLowLevelLogOn,False,NULL);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			StreamlineAbortDraw(stl);
			return(ret);
		}
                stp->low_level_log_on = False;
	}
        subret = _NhlDeactivateWorkstation(stl->base.wkptr);
	stp->wk_active = False;
	ret = MIN(subret,ret);

	if (stp->fws != NULL) {
		subret = _NhlIdleWorkspace(stp->fws);
		ret = MIN(subret,ret);
		stp->fws = NULL;
	}

	return MIN(subret,ret);
}


/*
 * Function:	InitCoordBounds
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
static NhlErrorTypes InitCoordBounds
#if	NhlNeedProto
(
        NhlStreamlinePlotLayer	stl,
        NhlStreamlinePlotLayer	ostl,
	char			*entry_name
)
#else
(stl,ostl,entry_name)
        NhlStreamlinePlotLayer	stl;
        NhlStreamlinePlotLayer	ostl;
	char			*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
        NhlStreamlinePlotLayerPart	*stp = &stl->streamlineplot;
        NhlTransformLayerPart	*tfp = &stl->trans;
	char		*e_text;
	NhlBoolean	x_data_reversed,y_data_reversed;

	stp->do_low_level_log = False;
        stp->use_irr_trans = False;
        
	if (! stp->data_init) {
                tfp->data_xstart = tfp->data_xend = 0.0;
                tfp->data_ystart = tfp->data_yend = 0.0;
                
		if (! tfp->x_reverse) {
			stp->xlb = stp->xc1 = tfp->x_min;
			stp->xub = stp->xcm = tfp->x_max;
		}
		else {
			stp->xub = stp->xc1 = tfp->x_max;
			stp->xlb = stp->xcm = tfp->x_min;
		}
		if (! tfp->y_reverse) {
			stp->ylb = stp->yc1 = tfp->y_min;
			stp->yub = stp->ycn = tfp->y_max;
		}
		else {
			stp->yub = stp->yc1 = tfp->y_max;
			stp->ylb = stp->ycn = tfp->y_min;
		}
                return ret;
	}
        
	x_data_reversed = stp->vfp->x_start > stp->vfp->x_end;
	y_data_reversed = stp->vfp->y_start > stp->vfp->y_end;

        tfp->data_xstart = stp->vfp->x_start;
        tfp->data_xend = stp->vfp->x_end;
        tfp->data_ystart = stp->vfp->y_start;
        tfp->data_yend = stp->vfp->y_end;
        
        if (stp->vfp->x_arr || stp->vfp->y_arr)
                stp->use_irr_trans = True;
        
        if (stp->use_irr_trans) {
                if (stp->vfp->x_arr && ! tfp->x_axis_type_set) {
			if (! stp->ovfp || (stp->data_changed  &&
			    (stp->vfp->changed & _NhlvfXARR_CHANGED)))
				tfp->x_axis_type = NhlIRREGULARAXIS;
		}
                if (! stp->vfp->x_arr && tfp->x_axis_type == NhlIRREGULARAXIS)
                        tfp->x_axis_type = NhlLINEARAXIS;
                if (stp->vfp->x_arr && tfp->x_axis_type != NhlIRREGULARAXIS) {
                        tfp->data_xstart = stp->vfp->ix_start;
                        tfp->data_xend = stp->vfp->ix_end;
			x_data_reversed = False;
                }
                if (stp->vfp->y_arr && ! tfp->y_axis_type_set) {
			if (! stp->ovfp || (stp->data_changed  &&
			    (stp->vfp->changed & _NhlvfYARR_CHANGED)))
				tfp->y_axis_type = NhlIRREGULARAXIS;
		}
                if (! stp->vfp->y_arr && tfp->y_axis_type == NhlIRREGULARAXIS)
                        tfp->y_axis_type = NhlLINEARAXIS;
                if (stp->vfp->y_arr && tfp->y_axis_type != NhlIRREGULARAXIS) {
                        tfp->data_ystart = stp->vfp->iy_start;
                        tfp->data_yend = stp->vfp->iy_end;
			y_data_reversed = False;
                }
        }
        ret = _NhltfCheckCoordBounds
                ((NhlTransformLayer)stl,(NhlTransformLayer)ostl,
                 stp->use_irr_trans,entry_name);
        
	return ret;
}

/*
 * Function:	SetUpLLTransObj
 *
 * Description: Sets up a LogLinear transformation object.
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
        NhlSArg			sargs[32];
        int			nargs = 0;

	entry_name = (init) ?
                "StreamlinePlotInitialize" : "StreamlinePlotSetValues";
        
	if (init)
		tfp->trans_obj = NULL;
	if (tfp->trans_obj &&
                 tfp->trans_obj->base.layer_class->base_class.class_name !=
                 NhllogLinTransObjClass->base_class.class_name) {
		subret = NhlDestroy(tfp->trans_obj->base.id);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error destroying irregular trans object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		tfp->trans_obj = NULL;
	}
        
        if (tfp->x_reverse_set)
                NhlSetSArg(&sargs[nargs++],
                           NhlNtrXReverse,tfp->x_reverse);
        if (tfp->y_reverse_set)
                NhlSetSArg(&sargs[nargs++],
                           NhlNtrYReverse,tfp->y_reverse);
        
	if (tfp->trans_obj == NULL) {

		stp->new_draw_req = True;
                stp->update_req = True;
		NhlSetSArg(&sargs[nargs++],NhlNtrXLog,tfp->x_log);
		NhlSetSArg(&sargs[nargs++],NhlNtrYLog,tfp->y_log);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
                
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

	}
        else {
                if (tfp->x_min != stold->trans.x_min)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
                if (tfp->x_max != stold->trans.x_max)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
                if (tfp->y_min != stold->trans.y_min)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
                if (tfp->y_max != stold->trans.y_max)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
                if (tfp->x_log != stold->trans.x_log)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXLog,tfp->x_log);
                if (tfp->y_log != stold->trans.y_log)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYLog,tfp->y_log);

                subret = NhlALSetValues(tfp->trans_obj->base.id,sargs,nargs);
                if (nargs > 0) {
                        stp->new_draw_req = True;
                        stp->update_req = True;
                }
        }
        
        NhlVAGetValues(tfp->trans_obj->base.id,
                       NhlNtrXReverse,&tfp->x_reverse,
                       NhlNtrYReverse,&tfp->y_reverse,
                       NhlNtrXLog,&tfp->x_log,
                       NhlNtrYLog,&tfp->y_log,
                       NhlNtrXMinF,&tfp->x_min,
                       NhlNtrXMaxF,&tfp->x_max,
                       NhlNtrYMinF,&tfp->y_min,
                       NhlNtrYMaxF,&tfp->y_max,
                       NULL);
	tfp->x_axis_type = tfp->x_log ? NhlLOGAXIS : NhlLINEARAXIS;
	tfp->y_axis_type = tfp->y_log ? NhlLOGAXIS : NhlLINEARAXIS;

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
        NhlSArg			sargs[32];
        int			nargs = 0;

	entry_name = (init) ?
                "StreamlinePlotInitialize" : "StreamlinePlotSetValues";

	if (init)
		tfp->trans_obj = NULL;
	if (tfp->trans_obj &&
            tfp->trans_obj->base.layer_class->base_class.class_name !=
	    NhlirregularTransObjClass->base_class.class_name) {
		subret = NhlDestroy(tfp->trans_obj->base.id);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: Error destroying irregular trans object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		tfp->trans_obj = NULL;
	}
	if (! stp->data_init) return ret;
        
        if (tfp->x_reverse_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,tfp->x_reverse);
        if (tfp->y_reverse_set)
                NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,tfp->y_reverse);

	if (init || tfp->trans_obj == NULL) {

		stp->new_draw_req = True;
                stp->update_req = True;
                
		if (stp->vfp->x_arr)
			NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
				   stp->vfp->x_arr);
		if (stp->vfp->y_arr)
			NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
				   stp->vfp->y_arr);
                
                NhlSetSArg(&sargs[nargs++],NhlNtrXAxisType,tfp->x_axis_type);
                NhlSetSArg(&sargs[nargs++],NhlNtrYAxisType,tfp->y_axis_type);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
                NhlSetSArg(&sargs[nargs++],NhlNtrDataXStartF,tfp->data_xstart);
                NhlSetSArg(&sargs[nargs++],NhlNtrDataXEndF,tfp->data_xend);
                NhlSetSArg(&sargs[nargs++],NhlNtrDataYStartF,tfp->data_ystart);
                NhlSetSArg(&sargs[nargs++],NhlNtrDataYEndF,tfp->data_yend);
                NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,stp->x_tension);
                NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,stp->y_tension);
                
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
	}
        else {
                if (stp->data_changed && stp->vfp->x_arr &&
		    (stp->vfp->changed & _NhlvfXARR_CHANGED))
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrXCoordPoints,stp->vfp->x_arr);
                if (stp->data_changed && stp->vfp->y_arr &&
		    (stp->vfp->changed & _NhlvfXARR_CHANGED))
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrYCoordPoints,stp->vfp->y_arr);
        
                if (tfp->x_axis_type != stold->trans.x_axis_type)        
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrXAxisType,tfp->x_axis_type);
                if (tfp->y_axis_type != stold->trans.y_axis_type)        
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrYAxisType,tfp->y_axis_type);
        
                if (tfp->x_min != stold->trans.x_min)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,tfp->x_min);
                if (tfp->x_max != stold->trans.x_max)
                        NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,tfp->x_max);
                if (tfp->y_min != stold->trans.y_min)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,tfp->y_min);
                if (tfp->y_max != stold->trans.y_max)
                        NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,tfp->y_max);
        
                if (tfp->data_xstart != stold->trans.data_xstart)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrDataXStartF,tfp->data_xstart);
                if (tfp->data_xend != stold->trans.data_xend)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrDataXEndF,tfp->data_xend);
                if (tfp->data_ystart != stold->trans.data_ystart)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrDataYStartF,tfp->data_ystart);
                if (tfp->data_yend != stold->trans.data_yend)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrDataYEndF,tfp->data_yend);
        
                if (stp->x_tension != ostp->x_tension)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrXTensionF,stp->x_tension);
                if (stp->y_tension != ostp->y_tension)
                        NhlSetSArg(&sargs[nargs++],
                                   NhlNtrYTensionF,stp->y_tension);
                subret = NhlALSetValues(tfp->trans_obj->base.id,sargs,nargs);

                if (nargs > 0) {
                        stp->new_draw_req = True;
                        stp->update_req = True;
                }
        }
        
        NhlVAGetValues(tfp->trans_obj->base.id,
                       NhlNtrXReverse,&tfp->x_reverse,
                       NhlNtrYReverse,&tfp->y_reverse,
                       NhlNtrXAxisType,&tfp->x_axis_type,
                       NhlNtrYAxisType,&tfp->y_axis_type,
                       NhlNtrDataXStartF,&tfp->data_xstart,
                       NhlNtrDataXEndF,&tfp->data_xend,
                       NhlNtrDataYStartF,&tfp->data_ystart,
                       NhlNtrDataYEndF,&tfp->data_yend,
                       NhlNtrXMinF,&tfp->x_min,
                       NhlNtrXMaxF,&tfp->x_max,
                       NhlNtrYMinF,&tfp->y_min,
                       NhlNtrYMaxF,&tfp->y_max,
                       NULL);

        tfp->x_log = tfp->x_axis_type == NhlLOGAXIS ? True : False;
        tfp->y_log = tfp->y_axis_type == NhlLOGAXIS ? True : False;
       
        stp->do_low_level_log = tfp->x_axis_type == NhlLOGAXIS ||
                tfp->y_axis_type == NhlLOGAXIS ? True : False;
        
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

/* Manage the overlay */

	/* 1 arg */
	if (stp->update_req) {
		NhlSetSArg(&sargs[(*nargs)++],NhlNpmUpdateReq,True);
	}
		
	subret = _NhlManageOverlay(&stp->overlay_object,
				   (NhlLayer)stnew,(NhlLayer)stold,
				   (init)?_NhlCREATE:_NhlSETVALUES,
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

 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (stp->display_tickmarks == NhlNOCREATE) {
                if (init || ostp->display_tickmarks == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        stp->display_tickmarks = NhlNEVER;
        }

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
 * Description: If the StreamlinePlot object has an overlay object 
  *		and the Titles are activated, manages the Title resources 
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
        
 	if (! tfp->plot_manager_on)
		return NhlNOERROR;

        if (stp->display_titles == NhlNOCREATE) {
                if (init || ostp->display_titles == NhlNOCREATE)
                        return NhlNOERROR;
                else
                        stp->display_titles = NhlNEVER;
        }

	if (init || 
	    stp->display_titles != ostp->display_titles) {
			NhlSetSArg(&sargs[(*nargs)++],
				   NhlNpmTitleDisplayMode,
				   stp->display_titles);
	}

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


	if (init || ! stp->zerof_lbl.string1 ||
	    stp->zerof_lbl.string1 != ostp->zerof_lbl.string1) {
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
	if (init || ! stp->zerof_lbl.string2 ||
	    stp->zerof_lbl.string2 != ostp->zerof_lbl.string2) {
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
		NhlSetSArg(&sargs[(nargs)++],NhlNamTrackData,False);
		NhlSetSArg(&sargs[(nargs)++],NhlNamResizeNotify,False);
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
 * Description: Handles updating of the vector data
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

	if (! stp->data_changed && 
	    ! _NhlArgIsSet(args,num_args,NhlNstVectorFieldData))
		return NhlNOERROR;

	if (stp->vector_field_data != NULL)
		ndata = _NhlGetDataInfo(stp->vector_field_data,&dlist);
	if (ndata <= 0) {
		stp->zmin = 0.01;
		stp->zmax = MAX(1.0,stp->zmin * 10.0);
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

 	vfl = (NhlVectorFieldFloatLayer) _NhlGetDataSet(dlist[0],&new);
	if (vfl == NULL) {
		stp->data_init = False;
		e_text = "%s: internal error retrieving data set";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	
	stp->vfp = (NhlVectorFieldFloatLayerPart *) &vfl->vfieldfloat;

	if (stp->vfp->miss_mode != vfNONE) {
		if (stp->vfp->miss_mode == vfVONLY &&
		     stp->vfp->mag_max == stp->vfp->v_missing_value)
		    stp->data_init = False;
		else if (stp->vfp->mag_max == stp->vfp->u_missing_value)
		    stp->data_init = False;
		else
		    stp->data_init = True;

		if (! stp->data_init) {
			e_text = 
          "%s: no valid values in vector field; StreamlinePlot not possible";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			stp->zmin = 0.01;
			stp->zmax = MAX(1.0,stp->zmin * 10.0);
			stp->data_changed = True;
			ret = MIN(NhlWARNING,ret);
			return ret;
		}
	}

	stp->zmin = stp->vfp->mag_min;
	stp->zmax = stp->vfp->mag_max;


	stp->zero_field = _NhlCmpFAny(stp->zmax,stp->zmin,8) <= 0.0 ?
		True : False;
	if (stp->zero_field) {
		e_text = 
		 "%s: zero vector field; StreamlinePlot not possible";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(NhlWARNING,ret);
	}

	stp->data_init = True;
	stp->data_changed = True;
	stp->use_irr_trans = (stp->vfp->x_arr == NULL &&
			      stp->vfp->y_arr == NULL) ? False : True;

	stnew->trans.data_xstart = MIN(stp->vfp->x_start,stp->vfp->x_end);
	stnew->trans.data_xend = MAX(stp->vfp->x_start,stp->vfp->x_end);
	stnew->trans.data_ystart = MIN(stp->vfp->y_start,stp->vfp->y_end);
	stnew->trans.data_yend = MAX(stp->vfp->y_start,stp->vfp->y_end);

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
				stnew->view.width / NHL_DEFAULT_VIEW_WIDTH;
		}
		else if (stnew->view.width != stold->view.width) {
			stp->min_arrow_spacing *= 
				stnew->view.width / stold->view.width;
		}
	}
#if 0        
	if (! stp->min_line_length_set) {
		if (init) {
			stp->min_line_length *= 
				stnew->view.width / NHL_DEFAULT_VIEW_WIDTH;
		}
		else if (stnew->view.width != stold->view.width) {
			stp->min_line_length *= 
				stnew->view.width / stold->view.width;
		}
	}
#endif        

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
				stnew->view.width / NHL_DEFAULT_VIEW_WIDTH;
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
#if 0
	printf("compc %f %f : win %f %f\n", *xda, *yda, *xus, *yus);
#endif
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
	printf("win %f %f : compc %f %f\n", *xus, *yus, *xda, *yda);
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
			_NhlCompcToData(Trans_Obj,xda,yda,1,&xdata,&ydata,
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
                if (errno)
                        *ist = -6;
#if 0                
		if (errno == EDOM)
			printf("ye %f *ynd %f xe %f *xnd %f\n",
			       ye,*ynd,xe,*xnd);
		else if (errno == ERANGE)
			printf("*ta %f\n",*ta);
		else if (errno != 0)
			printf ("errno - %d\n",errno);
#endif                
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
		cos_lat = cos(ydata * DEG2RAD);
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
                if (errno)
                        *ist = -6;
#if 0                
		if (errno == EDOM)
			printf("ytf %f *ynd %f xtf %f *xnd %f\n",
			       ytf,*ynd,xtf,*xnd);
		else if (errno == ERANGE)
			printf("*ta %f\n",*ta);
		else if (errno != 0)
			printf ("errno - %d\n",errno);
#endif                
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

