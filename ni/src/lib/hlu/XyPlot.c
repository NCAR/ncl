/*
 *      $Id: XyPlot.c,v 1.5 1993-06-03 15:57:57 ethan Exp $
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
#include <string.h>
#include <math.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/XyPlotP.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/IrregularType2TransObj.h>
#include <ncarg/hlu/LogLinTransObj.h>

#define CREATE 1
#define SET 0

static char *dash[] = { "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
			"$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'",
			"$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'",
			"$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'",
			"$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'",
			"$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'",
			"$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'",
			"$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'",
			"$$$$'$$'$'$$'$$$$'$$'$'$$'$$$$'$$'$'$$'$$$$'$$'$'$$'",
			"$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''",
			"$$$$$$''$$$$$$''$$$$$$''$$$$$$''$$$$$$''$$$$$$''",
			"$$$'$$$''$$$'$$$''$$$'$$$''$$$'$$$''$$$'$$$''",
			"$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''",
			"$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''",
			"$$$$$'$'$$$$$'$'$$$$$'$'$$$$$'$$$$$'$'$$$$$'$'",
			"$$$$$'$'$'$$$$$'$'$'$$$$$'$'$'$$$$$'$'$'$$$$$'$'$'",
};

static char main[] = "Main";
static char x_axis[] = "X-Axis";
static char y_axis[] = "Y-Axis";

static NhlResource resources[] = {
	{ NhlNxyNumCurves, NhlCxyNumCurves, NhlTInteger,sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.num_curves),NhlTString,"0"},
	{ NhlNxyXValues, NhlCxyXValues, NhlTFloatPtrPtr,sizeof(float**),
		NhlOffset(XyPlotLayerRec,xyplot.x_values),NhlTImmediate,NULL},
	{ NhlNxyYValues, NhlCxyYValues, NhlTFloatPtrPtr,sizeof(float**),
		NhlOffset(XyPlotLayerRec,xyplot.y_values),NhlTImmediate,NULL},
	{ NhlNxyCurveColors, NhlCxyCurveColors, NhlTIntegerPtr,sizeof(int*),
		NhlOffset(XyPlotLayerRec,xyplot.curve_colors),NhlTImmediate,NULL},
	{ NhlNxyCurveLengths, NhlCxyCurveLengths, NhlTIntegerPtr,sizeof(int*),
		NhlOffset(XyPlotLayerRec,xyplot.curve_lengths),NhlTImmediate,NULL},
	{ NhlNxyCurveDashPatterns, NhlCxyCurveDashPatterns, NhlTIntegerPtr,sizeof(int*),
		NhlOffset(XyPlotLayerRec,xyplot.curve_dash_patterns),NhlTImmediate,
		NULL},
	{ NhlNxyCurveLineLabels, NhlCxyCurveLineLabels, NhlTStringPtr,sizeof(char**),
		NhlOffset(XyPlotLayerRec,xyplot.curve_line_labels),NhlTImmediate,
		NULL},
	{ NhlNxyCurveLineLabelMode, NhlCxyCurveLineLabelMode, NhlTLineLabelModes,sizeof(LineLabelModes),
		NhlOffset(XyPlotLayerRec,xyplot.curve_line_label_mode),NhlTImmediate,
		(LineLabelModes)NOLABELS},
	{ NhlNxyCurveThicknessF, NhlCxyCurveThicknessF, NhlTFloat,sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.curve_thickness),NhlTString,"1.0"},
	{ NhlNxyXMissingF, NhlCxyXMissingF, NhlTFloat,sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.x_missing),NhlTString,"1e30" },
	{ NhlNxyYMissingF, NhlCxyYMissingF, NhlTFloat,sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.y_missing),NhlTString,"1e30" },
	{ NhlNxyXStyle, NhlCxyXStyle, NhlTTickMarkStyles,sizeof(TickMarkStyles),
		NhlOffset(XyPlotLayerRec,xyplot.x_style),
		NhlTImmediate,(NhlPointer)LINEAR },
	{ NhlNxyYStyle, NhlCxyYStyle, NhlTTickMarkStyles,sizeof(TickMarkStyles),
		NhlOffset(XyPlotLayerRec,xyplot.y_style),
		NhlTImmediate,(NhlPointer)LINEAR },
	{ NhlNxyClip, NhlCxyClip, NhlTInteger,sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.clip), NhlTString,"1"},
	{ NhlNxyXIrregularPoints, NhlCxyXIrregularPoints, NhlTFloatPtr,
		sizeof(float*),
		NhlOffset(XyPlotLayerRec,xyplot.x_irregular_points), 
		NhlTImmediate,(NhlPointer)NULL},
	{ NhlNxyYIrregularPoints, NhlCxyYIrregularPoints, NhlTFloatPtr,
		sizeof(float*),
		NhlOffset(XyPlotLayerRec,xyplot.y_irregular_points), 
		NhlTImmediate,(NhlPointer)NULL},
	{ NhlNxyYNumIrregularPoints, NhlCxyYNumIrregularPoints, NhlTInteger,
		sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.y_num_irregular_points), 
		NhlTString,"0"},
	{ NhlNxyXNumIrregularPoints, NhlCxyXNumIrregularPoints, NhlTInteger,
		sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.x_num_irregular_points), 
		NhlTString,"0"},
	{ NhlNxyXReverse, NhlCxyXReverse, NhlTInteger, sizeof(int), 
		NhlOffset(XyPlotLayerRec,xyplot.x_reverse), NhlTString,"0"},
	{ NhlNxyYReverse, NhlCxyYReverse, NhlTInteger, sizeof(int), 
		NhlOffset(XyPlotLayerRec,xyplot.y_reverse), NhlTString,"0"},
	{ NhlNxyXLeftF, NhlCxyXLeftF, NhlTFloat, sizeof(float), 
		NhlOffset(XyPlotLayerRec,xyplot.x_left), NhlTString,"0.0"},
	{ NhlNxyXRightF, NhlCxyXRightF, NhlTFloat, sizeof(float), 
		NhlOffset(XyPlotLayerRec,xyplot.x_right), NhlTString,"0.0"},
	{ NhlNxyYTopF, NhlCxyYTopF, NhlTFloat, sizeof(float), 
		NhlOffset(XyPlotLayerRec,xyplot.y_top), NhlTString,"0.0"},
	{ NhlNxyYBottomF, NhlCxyYBottomF, NhlTFloat, sizeof(float), 
		NhlOffset(XyPlotLayerRec,xyplot.y_bottom), NhlTString,"0.0"},
	{ NhlNxyTitles, NhlCxyTitles, NhlTInteger, sizeof(int), 
		NhlOffset(XyPlotLayerRec,xyplot.titles), NhlTString,"1"},
	{ NhlNxyXAlternate, NhlCxyXAlternate, NhlTAlternatePlace, 
		sizeof(AlternatePlace), 
		NhlOffset(XyPlotLayerRec,xyplot.x_alternate), NhlTImmediate,
		(NhlPointer)NONE},
	{ NhlNxyYAlternate, NhlCxyYAlternate, NhlTAlternatePlace, 
		sizeof(AlternatePlace), 
		NhlOffset(XyPlotLayerRec,xyplot.y_alternate), NhlTImmediate,
		(NhlPointer)NONE},
	{ NhlNxyXNumAlternateCoords, NhlCxyXNumAlternateCoords, NhlTInteger, 
		sizeof(int), 
		NhlOffset(XyPlotLayerRec,xyplot.x_num_alternate_coords), 
		NhlTString, "0"},
	{ NhlNxyYNumAlternateCoords, NhlCxyYNumAlternateCoords, NhlTInteger, 
		sizeof(int), 
		NhlOffset(XyPlotLayerRec,xyplot.y_num_alternate_coords), 
		NhlTString, "0"},
	{ NhlNxyYAlternateCoords, NhlCxyYAlternateCoords, NhlTFloatPtr, 
		sizeof(float*), 
		NhlOffset(XyPlotLayerRec,xyplot.y_alternate_coords), 
		NhlTImmediate, NULL},
	{ NhlNxyXAlternateCoords, NhlCxyXAlternateCoords, NhlTFloatPtr, 
		sizeof(float*), 
		NhlOffset(XyPlotLayerRec,xyplot.x_alternate_coords), 
		NhlTImmediate, NULL},
	{ NhlNxyXOriginalCoords, NhlCxyXOriginalCoords, NhlTFloatPtr, 
		sizeof(float*), 
		NhlOffset(XyPlotLayerRec,xyplot.x_original_coords), 
		NhlTImmediate, NULL},
	{ NhlNxyYOriginalCoords, NhlCxyYOriginalCoords, NhlTFloatPtr, 
		sizeof(float*), 
		NhlOffset(XyPlotLayerRec,xyplot.y_original_coords), 
		NhlTImmediate, NULL},
	{ NhlNxyDashSegmentLengthF, NhlCxyDashSegmentLengthF, NhlTFloat,
		sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.dash_segment_length),
		NhlTString,".15" },
	{ NhlNxyLineLabelFontHeightF, NhlCxyLineLabelFontHeightF, NhlTFloat,
		sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.line_label_font_height),
		NhlTString,".01" },
/*
* Title resources of special importance are intercepted here
*/
	{ NhlNtiMainOffsetXF, NhlCtiMainOffsetYF,NhlTFloat,sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.ti_main_offset_x),
		NhlTString,"0.0" },
	{ NhlNtiXAxisOffsetXF, NhlCtiXAxisOffsetYF,NhlTFloat,sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.ti_x_axis_offset_x),
		NhlTString,"0.0" },
	{ NhlNtiYAxisOffsetYF, NhlCtiXAxisOffsetYF,NhlTFloat,sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.ti_y_axis_offset_y),
		NhlTString,"0.0" },
	{ NhlNtiXAxisPosition, NhlCtiXAxisPosition,NhlTTitlePositions,
		sizeof(TitlePositions),
		NhlOffset(XyPlotLayerRec,xyplot.ti_x_axis_position),
		NhlTImmediate,(NhlPointer)CENTER },
	{ NhlNtiYAxisPosition, NhlCtiYAxisPosition,NhlTTitlePositions,
		sizeof(TitlePositions),
		NhlOffset(XyPlotLayerRec,xyplot.ti_y_axis_position),
		NhlTImmediate,(NhlPointer)CENTER },
	{ NhlNtiMainPosition, NhlCtiMainPosition,NhlTTitlePositions,
		sizeof(TitlePositions),
		NhlOffset(XyPlotLayerRec,xyplot.ti_main_position),
		NhlTImmediate,(NhlPointer)CENTER },
	{ NhlNtiMainOn, NhlCtiMainOn,NhlTInteger, sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.ti_main_on),
		NhlTString,"1"},
	{ NhlNtiXAxisOn, NhlCtiXAxisOn,NhlTInteger, sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.ti_x_axis_on),
		NhlTString,"1"},
	{ NhlNtiYAxisOn, NhlCtiYAxisOn,NhlTInteger, sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.ti_y_axis_on),
		NhlTString,"1"},
	{ NhlNtiMainString, NhlCtiMainString,NhlTString, sizeof(char*),
		NhlOffset(XyPlotLayerRec,xyplot.ti_main_string),
		NhlTImmediate,(NhlPointer)main},
	{ NhlNtiXAxisString, NhlCtiXAxisString,NhlTString, sizeof(char*),
		NhlOffset(XyPlotLayerRec,xyplot.ti_x_axis_string),
		NhlTImmediate,(NhlPointer)x_axis},
	{ NhlNtiYAxisString, NhlCtiYAxisString,NhlTString, sizeof(char*),
		NhlOffset(XyPlotLayerRec,xyplot.ti_y_axis_string),
		NhlTImmediate,(NhlPointer)y_axis},
/* 
* Tick Mark resources of special importance are interecepted here
*/
	{ NhlNtmXBMode, NhlCtmXBMode, NhlTTickMarkModes,sizeof(TickMarkModes),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_b_mode),
		NhlTImmediate,(NhlPointer)AUTOMATIC },
	{ NhlNtmXTMode, NhlCtmXTMode, NhlTTickMarkModes,sizeof(TickMarkModes),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_t_mode),
		NhlTImmediate,(NhlPointer)AUTOMATIC },
	{ NhlNtmYRMode, NhlCtmYRMode, NhlTTickMarkModes,sizeof(TickMarkModes),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_r_mode),
		NhlTImmediate,(NhlPointer)AUTOMATIC },
	{ NhlNtmYLMode, NhlCtmYLMode, NhlTTickMarkModes,sizeof(TickMarkModes),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_l_mode),
		NhlTImmediate,(NhlPointer)AUTOMATIC },
	{ NhlNtmXBTickStartF, NhlCtmXBTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_b_tick_start),
		NhlTString, "0.0" },
	{ NhlNtmXBTickEndF, NhlCtmXBTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_b_tick_end),
		NhlTString, "0.0" },
	{ NhlNtmXBTickSpacingF, NhlCtmXBTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_b_tick_spacing),
		NhlTString, "0.0" },
	{ NhlNtmXBSpacingType, NhlCtmXBSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_b_spacing_type),
		NhlTString, "0.0" },
	{ NhlNtmXTTickStartF, NhlCtmXTTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_t_tick_start),
		NhlTString, "0.0" },
	{ NhlNtmXTTickEndF, NhlCtmXTTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_t_tick_end),
		NhlTString, "0.0" },
	{ NhlNtmXTTickSpacingF, NhlCtmXTTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_t_tick_spacing),
		NhlTString, "0.0" },
	{ NhlNtmXTSpacingType, NhlCtmXTSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.tm_x_t_spacing_type),
		NhlTString, "0.0" },
	{ NhlNtmYLTickStartF, NhlCtmYLTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_l_tick_start),
		NhlTString, "0.0" },
	{ NhlNtmYLTickEndF, NhlCtmYLTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_l_tick_end),
		NhlTString, "0.0" },
	{ NhlNtmYLTickSpacingF, NhlCtmYLTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_l_tick_spacing),
		NhlTString, "0.0" },
	{ NhlNtmYLSpacingType, NhlCtmYLSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_l_spacing_type),
		NhlTString, "0.0" },
	{ NhlNtmYRTickStartF, NhlCtmYRTickStartF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_r_tick_start),
		NhlTString, "0.0" },
	{ NhlNtmYRTickEndF, NhlCtmYRTickEndF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_r_tick_end),
		NhlTString, "0.0" },
	{ NhlNtmYRTickSpacingF, NhlCtmYRTickSpacingF, NhlTFloat, sizeof(float),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_r_tick_spacing),
		NhlTString, "0.0" },
	{ NhlNtmYRSpacingType, NhlCtmYRSpacingType, NhlTInteger, sizeof(int),
		NhlOffset(XyPlotLayerRec,xyplot.tm_y_r_spacing_type),
		NhlTString, "0.0" }
	
};
	
	
/* base methods */

static NhlErrorTypes XyPlotSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes XyPlotInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes XyPlotClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes XyPlotDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes XyPlotDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

/* 
* View Methods
*/

static NhlErrorTypes XyPlotGetBB(
#ifdef NhlNeedProto
        Layer          /* instance */,
        NhlBoundingBox * /*thebox*/
#endif
);

/*
* Transform Methods
*/

static NhlErrorTypes XyPlotDataToNDC(
#ifdef NhlNeedProto
	Layer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */,
	float*		/* xout */,
	float*		/* yout */,
	float*		/*xmissing*/,
	float*		/*ymissing*/
#endif
);

static NhlErrorTypes XyPlotNDCToData(
#ifdef NhlNeedProto
	Layer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */,
	float*		/* xout */,
	float*		/* yout */,
	float*		/*xmissing*/,
	float*		/*ymissing*/
#endif
);

/*
* Static local functions
*/
static NhlErrorTypes ScaleForMove(
#ifdef NhlNeedProto
XyPlotLayer /*xnew*/,
XyPlotLayer /*xold */,
_NhlArgList	/* args */,
int		/* num_args*/,
int 		/*c_or_s*/
#endif
);

static NhlErrorTypes CheckValues(
#ifdef NhlNeedProto
XyPlotLayer /*xnew*/,
XyPlotLayer /*xold */,
int 		/*c_or_s*/
#endif
);
static NhlErrorTypes InternalizePointers(
#ifdef NhlNeedProto
XyPlotLayer /*xnew*/,
XyPlotLayer /*xold */,
int 		/*c_or_s*/
#endif
);
static void SetMinMax(
#ifdef NhlNeedProto
XyPlotLayer /*xnew*/
#endif
);
static NhlErrorTypes SetUpTransObjs(
#ifdef NhlNeedProto
XyPlotLayer /*xnew*/,
XyPlotLayer /*xold */,
int 		/*c_or_s*/
#endif
);
static NhlErrorTypes SetUpTicks(
#ifdef NhlNeedProto
XyPlotLayer /*xnew*/,
XyPlotLayer /*xold */,
int 		/*c_or_s*/
#endif
);
static NhlErrorTypes SetUpTitles(
#ifdef NhlNeedProto
XyPlotLayer /*xnew*/,
XyPlotLayer /*xold */,
int 		/*c_or_s*/
#endif
);

static NhlErrorTypes DrawCurves(
#ifdef NhlNeedProto
XyPlotLayer xlayer
#endif
);

XyPlotLayerClassRec xyPlotLayerClassRec = {
        {
/* superclass                   */      (LayerClass)&transformLayerClassRec,
/* class_name                   */      "XyPlot",
/* nrm_class                    */      NrmNULLQUARK,
/* layer_size                   */      sizeof(XyPlotLayerRec),
/* layer_resources              */      resources,
/* num_resources                */      NhlNumber(resources),
/* child_resources              */      NULL,
/* all_resources                */      NULL,
/* class_part_initialize        */      NULL,
/* class_inited                 */      False,
/* class_initialize             */      XyPlotClassInitialize,
/* layer_initialize             */      XyPlotInitialize,
/* layer_set_values             */      XyPlotSetValues,
/* layer_set_values_not         */      NULL,
/* layer_get_values             */      NULL,
/* layer_pre_draw               */      NULL,
/* layer_draw                   */      XyPlotDraw,
/* layer_draw_segonly           */      NULL,
/* layer_post_draw              */      NULL,
/* layer_clear                  */      NULL,
/* layer_destroy                */      XyPlotDestroy
        },
	{
/* segment_workstation */	-1,
/* get_bb 	*/	XyPlotGetBB
	},
	{
/* data_to_ndc */	XyPlotDataToNDC,
/* ndc_to_data */	XyPlotNDCToData
	},
	{
		NULL
	}
};

LayerClass xyPlotLayerClass = (LayerClass)&xyPlotLayerClassRec;


/*
 * Function:	XyPlotSetValues
 *
 * Description: Calls the following functions in the following order:
 *		CheckValues - confirms resources are valid
 *		InternalizePointers - copies arrays into internal storage
 *		SetMinMax - sets up data extent information
 *		SetUpTransObjs - Creates and sets transformation objects
 *		SetUpTicks - Creates and sets ticks
 *		SetUpTitles - Creates and sets titles
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
 * Side Effects: State change in GKS due to mapping tranformations.
 */
/*ARGSUSED*/
static NhlErrorTypes XyPlotSetValues
#if  __STDC__
(Layer old, Layer reference, Layer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
        Layer   old;
        Layer   reference;
        Layer   new;
        _NhlArgList     args;
        int     num_args;
#endif
{
	XyPlotLayer xnew = (XyPlotLayer)new;
	XyPlotLayer xold = (XyPlotLayer)old;
	NhlErrorTypes ret1 = NOERROR;
	NhlErrorTypes ret2 = NOERROR;

	ret1 = ScaleForMove(xnew,xold,args,num_args,SET);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	ret1 = CheckValues(xnew,xold,SET);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	ret1 = InternalizePointers(xnew,xold,SET);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	SetMinMax(xnew);

	ret1 = SetUpTransObjs(xnew,xold,SET);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	ret1 = SetUpTicks(xnew,xold,SET);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	ret1 = SetUpTitles(xnew,xold,SET);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	return(ret2);
}


/*
 * Function:	XyPlotInitialize
 *
 * Description: Calls the following functions in the following order:
 *              CheckValues - confirms resources are valid
 *              InternalizePointers - copies arrays into internal storage
 *              SetMinMax - sets up data extent information
 *              SetUpTransObjs - Creates and sets transformation objects
 *              SetUpTicks - Creates and sets ticks
 *              SetUpTitles - Creates and sets titles
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
static NhlErrorTypes XyPlotInitialize
#if     __STDC__
( LayerClass class, Layer req, Layer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        LayerClass      class;
        Layer           req;
        Layer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	XyPlotLayer xnew = (XyPlotLayer)new;
	NhlErrorTypes ret1 = NOERROR;
	NhlErrorTypes ret2 = NOERROR;

	ret1 = CheckValues(xnew,NULL,CREATE);
	if(ret1 < WARNING)
		return(ret1);

	ret1 = InternalizePointers(xnew,NULL,CREATE);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	SetMinMax(xnew);

	ret1 = SetUpTransObjs(xnew,NULL,CREATE);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	ret1 = SetUpTicks(xnew,NULL,CREATE);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	ret1 = SetUpTitles(xnew,NULL,CREATE);
	if(ret1 < WARNING)
		return(ret1);
	else if(ret1 < ret2)
		ret2= ret1;

	return(ret2);
}

static NhlErrorTypes
NhlCvtStringToAlternatePlace
#if     __STDC__
(
        NrmValue        *from,  /* ptr to from data     */
        NrmValue        *to,    /* ptr to to data       */
        NrmValue        *args,  /* add'n args for conv  */
        int             nargs   /* number of args       */
)
#else
(from,to,args,nargs)
        NrmValue        *from;  /* ptr to from data     */
        NrmValue        *to;    /* ptr to to data       */
        NrmValue        *args;  /* add'n args for conv  */
        int             nargs;  /* number of args       */
#endif
{
        AlternatePlace tmp;
        NhlErrorTypes ret = NOERROR;

        if(nargs != 0) {
                ret = WARNING;
        }
        if(strncmp((char*)from->addr,"NONE",strlen("NONE"))==0)
                tmp = NONE;
        else if(strncmp((char*)from->addr,"LEFTAXIS",strlen("LEFTAXIS"))==0)
                tmp = LEFTAXIS;
        else if(strncmp((char*)from->addr,"RIGHTAXIS",strlen("RIGHTAXIS"))==0)
                tmp = RIGHTAXIS;
        else if(strncmp((char*)from->addr,"TOPAXIS",strlen("TOPAXIS"))==0)
                tmp = TOPAXIS;
        else if(strncmp((char*)from->addr,"BOTTOMAXIS",strlen("BOTTOMAXIS"))==0)
                tmp = BOTTOMAXIS;
        else {
                NhlPError(WARNING,E_UNKNOWN,"NhlCvtStringToAlternatePlace: Could not convert %s to either NONE, LEFTAXIS, RIGHTAXIS, TOPAXIS or BOTTOMAXIS, incorrect string",(char*)from->addr);
                return(WARNING);
        }

        if((to->size >0) && (to->addr != NULL)) {
                /* caller provided space */
                if(to->size < sizeof(AlternatePlace)) {
                        to->size = (unsigned int)sizeof(AlternatePlace);
                        return(FATAL);
                }
                to->size = (unsigned int)sizeof(AlternatePlace);
                *((AlternatePlace*)(to->addr)) = tmp;
                return(ret);
        } else {
                static AlternatePlace val;
                to->size = (unsigned int)sizeof(AlternatePlace);
                val = tmp;
                to->addr = &val;
                return(ret);
        }
}
static NhlErrorTypes
NhlCvtStringToLineLabelModes
#if     __STDC__
(
        NrmValue        *from,  /* ptr to from data     */
        NrmValue        *to,    /* ptr to to data       */
        NrmValue        *args,  /* add'n args for conv  */
        int             nargs   /* number of args       */
)
#else
(from,to,args,nargs)
        NrmValue        *from;  /* ptr to from data     */
        NrmValue        *to;    /* ptr to to data       */
        NrmValue        *args;  /* add'n args for conv  */
        int             nargs;  /* number of args       */
#endif
{
        LineLabelModes tmp;
        NhlErrorTypes ret = NOERROR;

        if(nargs != 0) {
                ret = WARNING;
        }
        if(strncmp((char*)from->addr,"NOLABELS",strlen("NOLABEL"))==0)
                tmp = NOLABELS;
        else if(strncmp((char*)from->addr,"LETTERED",strlen("LETTERED"))==0)
                tmp = LETTERED;
        else if(strncmp((char*)from->addr,"CUSTOM",strlen("CUSTOM"))==0)
                tmp = CUSTOM;
        else {
                NhlPError(WARNING,E_UNKNOWN,"NhlCvtStringToLineLabelModes: Could not convert %s to either CUSTOM , LETTERED, NOLABELS, incorrect string",(char*)from->addr);
                return(WARNING);
        }

        if((to->size >0) && (to->addr != NULL)) {
                /* caller provided space */
                if(to->size < sizeof(LineLabelModes)) {
                        to->size = (unsigned int)sizeof(LineLabelModes);
                        return(FATAL);
                }
                to->size = (unsigned int)sizeof(LineLabelModes);
                *((LineLabelModes*)(to->addr)) = tmp;
                return(ret);
        } else {
                static LineLabelModes val;
                to->size = (unsigned int)sizeof(LineLabelModes);
                val = tmp;
                to->addr = &val;
                return(ret);
        }
}

/*
 * Function:	XyPlotClassInitialize
 *
 * Description: Takes care of calling _NhlRegisterChildClass for the title
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
 *		This function also calls NrmStringToQuark to make sure the
 *		new typename NhlTAlternatePlace is registered.
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes XyPlotClassInitialize
#if __STDC__
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NOERROR;
/*
	_NhlInitializeLayerClass(tickMarkLayerClass);
	_NhlInitializeLayerClass(titleLayerClass);
*/

	NhlRegisterConverter(NhlTString,NhlTAlternatePlace,
                NhlCvtStringToAlternatePlace,NULL,0,False,NULL);
	NhlRegisterConverter(NhlTString,NhlTLineLabelModes,
                NhlCvtStringToLineLabelModes,NULL,0,False,NULL);


	ret = _NhlRegisterChildClass(xyPlotLayerClass,titleLayerClass,False,
			False,
			NhlNtiMainOffsetXF,
        		NhlNtiXAxisOffsetXF, 
        		NhlNtiYAxisOffsetYF, 
        		NhlNtiXAxisPosition, 
        		NhlNtiYAxisPosition, 
        		NhlNtiMainPosition ,
			NhlNtiMainString,
			NhlNtiXAxisString,
			NhlNtiYAxisString,
			NhlNtiMainOn,
			NhlNtiXAxisOn,
			NhlNtiYAxisOn,NULL);
	if(ret < WARNING) {
		return(ret);
	}
	ret = _NhlRegisterChildClass(xyPlotLayerClass,tickMarkLayerClass,False,
			False,
			NhlNtmXBDataLeftF,NhlNtmXBDataRightF,NhlNtmXTDataRightF,
			NhlNtmXTDataLeftF,NhlNtmYLDataTopF,NhlNtmYLDataBottomF,
			NhlNtmYRDataTopF,NhlNtmYRDataBottomF,NhlNtmYLStyle, 
			NhlNtmYRStyle, NhlNtmXBStyle, NhlNtmXTStyle,
			NhlNtmXBMode,NhlNtmXTMode,NhlNtmYLMode,NhlNtmYRMode, 
			NhlNtmXBTickStartF,NhlNtmXBTickEndF, 
			NhlNtmXBTickSpacingF, NhlNtmXBSpacingType,
			NhlNtmXTTickStartF,NhlNtmXTTickEndF, 
			NhlNtmXTTickSpacingF, NhlNtmXTSpacingType,
			NhlNtmYLTickStartF,NhlNtmYLTickEndF, 
			NhlNtmYLTickSpacingF, NhlNtmYLSpacingType,
			NhlNtmYRTickStartF,NhlNtmYRTickEndF, 
			NhlNtmYRTickSpacingF, NhlNtmYRSpacingType,
			NhlNtmXBIrregularPoints, NhlNtmXBNumIrregularPoints,
			NhlNtmXTIrregularPoints, NhlNtmXTNumIrregularPoints,
			NhlNtmYLIrregularPoints, NhlNtmYLNumIrregularPoints,
			NhlNtmYRIrregularPoints, NhlNtmYRNumIrregularPoints,
			NULL);
	return(ret);
}

/*
 * Function:	DrawCurves
 *
 * Description: Takes care of the actual drawing of the XyPlot curves. 
 *		DrawCurves uses the AUTOGRAPH function c_agcurv to do the
 *		actual drawing. Several AUTOGRAPH parameters are set here also.
 *		These parameters make sure FRAME and SET are *not* called by
 *		AUTOGRAPH. Also controls for the ticks and axis are turned off.
 *		And all of the dash patterns are registered here. This type
 *		of dash pattern registering is temporary until a universal
 *		mechanism can be designed. This function also activates
 *		the workstation and calls NhlSetTrans to set the tranformation.
 *		Data in the  *_final_values fields has been converted to the
 *		the window by the function ConvertDataToWin that is called from
 *		XyPlotInitialize or XyPlotSetValues.
 *
 * In Args:	xlayer	xyplot instance
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	GKS state changes awa AUTOGRAPH state changes
 */
static NhlErrorTypes DrawCurves
#if __STDC__
(XyPlotLayer xlayer)
#else
(xlayer)
XyPlotLayer xlayer;
#endif
{
	int i,j;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;
	char buffer[80];
	int upordownflag = 1;

	for(i = 0; i< 80; i++)
		buffer[i] = '\0';

	ret = _NhlActivateWorkstation((Layer)xlayer->base.wkptr);	
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"DrawCurves: Could not activate workstation no data curves will be drawn");
		return(FATAL);
	} else if(ret < ret1)
		ret1 = ret;
	
	ret = _NhlSetTrans((Layer)xlayer->xyplot.thetrans,(Layer)xlayer);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"DrawCurves: Could not set transformation no data curves will be drawn");
		return(FATAL);
	} else if(ret < ret1)
		ret1 = ret;
	
	for( i = 0; i< xlayer->xyplot.num_curves; i++ ) {	
		upordownflag = 1;
		switch(xlayer->xyplot.curve_line_label_mode) {
		case NOLABELS:
		NhlSetValues(xlayer->xyplot.thetrans->base.id,
			NhlNtrDashPattern,((xlayer->xyplot.curve_dash_patterns == NULL)?1:xlayer->xyplot.curve_dash_patterns[i]),
			NhlNtrLineThicknessF,xlayer->xyplot.curve_thickness,
			NhlNtrLineColor,((xlayer->xyplot.curve_colors == NULL)?0:xlayer->xyplot.curve_colors[i]),
			NhlNtrLineLabelFontHeightF, xlayer->xyplot.line_label_font_height,
			NhlNtrLineLabel,NULL,
			NULL);
			break;
		case CUSTOM:	
		NhlSetValues(xlayer->xyplot.thetrans->base.id,
			NhlNtrDashPattern,((xlayer->xyplot.curve_dash_patterns == NULL)?1:xlayer->xyplot.curve_dash_patterns[i]),
			NhlNtrLineThicknessF,xlayer->xyplot.curve_thickness,
			NhlNtrLineColor,((xlayer->xyplot.curve_colors == NULL)?0:xlayer->xyplot.curve_colors[i]),
			NhlNtrLineLabelFontHeightF, xlayer->xyplot.line_label_font_height,
			NhlNtrLineLabel,((xlayer->xyplot.curve_line_labels == NULL)? NULL: xlayer->xyplot.curve_line_labels[i]),
			NULL);
			break;
		case LETTERED:
			buffer[0] = (char)((int)'A' + i % 26);
			buffer[1] = '\0';
		NhlSetValues(xlayer->xyplot.thetrans->base.id,
			NhlNtrDashPattern,((xlayer->xyplot.curve_dash_patterns == NULL)?1:xlayer->xyplot.curve_dash_patterns[i]),
			NhlNtrLineThicknessF,xlayer->xyplot.curve_thickness,
			NhlNtrLineColor,((xlayer->xyplot.curve_colors == NULL)?0:xlayer->xyplot.curve_colors[i]),
			NhlNtrLineLabelFontHeightF, xlayer->xyplot.line_label_font_height,
			NhlNtrLineLabel,buffer,
			NULL);
			break;
		}
		_NhlSetLineInfo(xlayer->xyplot.thetrans,(Layer)xlayer);


		if((xlayer->xyplot.thexmissing == NULL)&&(xlayer->xyplot.theymissing == NULL)) {
			if(xlayer->xyplot.noxvalues) {
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					_NhlDataLineTo(xlayer->xyplot.thetrans,
						(Layer)xlayer,
						xlayer->xyplot.dummy_array[j],
						(xlayer->xyplot.y_values[i])[j],	
						upordownflag);
					if(upordownflag) {
						upordownflag = 0;
					}
				}
		
			} else if(xlayer->xyplot.noyvalues){
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					_NhlDataLineTo(xlayer->xyplot.thetrans,
						(Layer)xlayer,
						(xlayer->xyplot.x_values[i])[j],
						xlayer->xyplot.dummy_array[j],	
						upordownflag);
					if(upordownflag) {
						upordownflag = 0;
					}
				}
			} else {
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					_NhlDataLineTo(xlayer->xyplot.thetrans,
						(Layer)xlayer,
						(xlayer->xyplot.x_values[i])[j],
						(xlayer->xyplot.y_values[i])[j],	
						upordownflag);
					if(upordownflag) {
						upordownflag = 0;
					}
				}
			}
		} else if(xlayer->xyplot.thexmissing == NULL) {
			if(xlayer->xyplot.noxvalues) {
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					if((xlayer->xyplot.y_values[i])[j] != *xlayer->xyplot.theymissing) {
						_NhlDataLineTo(xlayer->xyplot.thetrans,
							(Layer)xlayer,
							xlayer->xyplot.dummy_array[j],
							(xlayer->xyplot.y_values[i])[j],	
							upordownflag);
						if(upordownflag) {
							upordownflag = 0;
						}
					} else {
						upordownflag = 1;
					}
				}
		
			} else if(xlayer->xyplot.noyvalues){
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					_NhlDataLineTo(xlayer->xyplot.thetrans,
						(Layer)xlayer,
						(xlayer->xyplot.x_values[i])[j],
						xlayer->xyplot.dummy_array[j],	
						upordownflag);
					if(upordownflag) {
						upordownflag = 0;
					}
				}
			} else {
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					if((xlayer->xyplot.y_values[i])[j] != *xlayer->xyplot.theymissing) {
						_NhlDataLineTo(xlayer->xyplot.thetrans,
							(Layer)xlayer,
							(xlayer->xyplot.x_values[i])[j],
							(xlayer->xyplot.y_values[i])[j],	
							upordownflag);
						if(upordownflag) {
							upordownflag = 0;
						}
					} else {
						upordownflag = 1;
					}
				}
			}
		} else if(xlayer->xyplot.theymissing == NULL) {
			if(xlayer->xyplot.noxvalues) {
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					_NhlDataLineTo(xlayer->xyplot.thetrans,
						(Layer)xlayer,
						xlayer->xyplot.dummy_array[j],
						(xlayer->xyplot.y_values[i])[j],	
						upordownflag);
					if(upordownflag) {
						upordownflag = 0;
					}
				}
		
			} else if(xlayer->xyplot.noyvalues){
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					if((xlayer->xyplot.x_values[i])[j] != *xlayer->xyplot.thexmissing) {
						_NhlDataLineTo(xlayer->xyplot.thetrans,
							(Layer)xlayer,
							(xlayer->xyplot.x_values[i])[j],
							xlayer->xyplot.dummy_array[j],	
							upordownflag);
						if(upordownflag) {
							upordownflag = 0;
						}
					} else {	
						upordownflag = 1;
					}
				}
			} else {
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					if((xlayer->xyplot.x_values[i])[j] != *xlayer->xyplot.thexmissing) {
						_NhlDataLineTo(xlayer->xyplot.thetrans,
							(Layer)xlayer,
							(xlayer->xyplot.x_values[i])[j],
							(xlayer->xyplot.y_values[i])[j],	
							upordownflag);
						if(upordownflag) {
							upordownflag = 0;
						}
					} else {
						upordownflag = 1;
					}
				}
			}
		} else {
			if(xlayer->xyplot.noxvalues) {
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					if((xlayer->xyplot.y_values[i])[j]!= *xlayer->xyplot.theymissing) {
						_NhlDataLineTo(xlayer->xyplot.thetrans,
							(Layer)xlayer,
							xlayer->xyplot.dummy_array[j],	
							(xlayer->xyplot.y_values[i])[j],
							upordownflag);
						if(upordownflag) {
							upordownflag = 0;
						}
					} else {
						upordownflag = 1;
					}
				}
		
			} else if(xlayer->xyplot.noyvalues){
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					if((xlayer->xyplot.x_values[i])[j]!= *xlayer->xyplot.thexmissing) {
						_NhlDataLineTo(xlayer->xyplot.thetrans,
							(Layer)xlayer,
							(xlayer->xyplot.x_values[i])[j],
							xlayer->xyplot.dummy_array[j],	
							upordownflag);
						if(upordownflag) {
							upordownflag = 0;
						}
					} else {
						upordownflag = 1;
					}
				}
			} else {
				for(j = 0 ; j< xlayer->xyplot.curve_lengths[i]; j++) {	
					if(((xlayer->xyplot.x_values[i])[j]!= *xlayer->xyplot.thexmissing)&&((xlayer->xyplot.y_values[i])[j]!= *xlayer->xyplot.theymissing)) {
						_NhlDataLineTo(xlayer->xyplot.thetrans,
							(Layer)xlayer,
							(xlayer->xyplot.x_values[i])[j],
							(xlayer->xyplot.y_values[i])[j],	
							upordownflag);
						if(upordownflag) {
							upordownflag = 0;
						}
					} else {
						upordownflag = 1;
					}
				}
			}
		}
	}
	
	ret = _NhlDeactivateWorkstation(xlayer->base.wkptr);	
	if(ret < ret1)
		ret1 = ret;
	return(ret);
	
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
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes XyPlotDraw
#if  __STDC__
(Layer layer)
#else
(layer)
        Layer layer;
#endif
{
	XyPlotLayer	xlayer = (XyPlotLayer) layer;
	NhlErrorTypes ret1 = NOERROR;
	NhlErrorTypes ret = NOERROR;

/*
* Should probably have resource for letting user draw curves on
* top of or below Ticks
*/
	ret = DrawCurves(xlayer);
	if(ret < ret1)
		ret1 = ret;


	if(xlayer->xyplot.titles)
		ret = NhlDraw(xlayer->xyplot.ttitles->base.id);
	if(ret < ret1)
		ret1 = ret;

	ret = NhlDraw(xlayer->xyplot.ticks->base.id);
	if(ret < ret1)
		ret1 = ret;

	return(ret1);
}


/*
 * Function:	XyPlotDataToNDC
 *
 * Description: This is the Data to NDC method of the transform class. It
 *		maps data to normalized device coordinates using the XyPlot
 *		object's TransObj which is referenced through 
 *		xplot->xyplot.thetrans . The tranformation is set using 
 *		_NhlSetTrans and then mapped using the TranObjs method entry
 *		points _NhlDataToWin and then _NhlWinToNDC. This is the 
 *		standard way in which plot objects will present their 
 *		data transformation funtions to the user. Having these
 *		functions call the TransObj instead of haveing the user call
 *		it directly is to facilitate the eventual support of overlays
 * 		These will require the plot object to intercede in the 
 *		data tranformation progress.
 *
 * In Args:	plot	instance record
 *		x	array of x axis values in data coordinates
 * 		y	array of y axis values in data coordinates
 *		n	number of elements
 *		xout	storage provided for output x ndc vals
 *		yout	storage provided for output y ndc vals
 *		xmissing	holds value of missing value in x if NULL
 *				no missing value
 *		ymissing	holds value of missing value in y if NULL
 *				no missing value
 *
 * Out Args:	xout	does not allocate storage
 *		yout	does not allocate storage
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes XyPlotDataToNDC
#if __STDC__
(Layer plot,float* x,float* y,int n,float* xout,float* yout,float* xmissing,float* ymissing)
#else
(plot,x,y,n,xout,yout,xmissing,ymissing)
	Layer		plot;
	float*		x;
	float*		y;
	int		n;
	float*		xout;
	float*		yout;
	float*		xmissing;
	float*		ymissing;
#endif
{
	XyPlotLayer xplot = (XyPlotLayer)plot;
	int istrans = 0;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;

	 ret = _NhlSetTrans(xplot->xyplot.thetrans,plot);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"XyPlotDataToNDC: A FATAL error occured while setting the tranformation of XyPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if( ret < ret1 )
		ret1 = ret; 
		

	ret = _NhlDataToWin(xplot->xyplot.thetrans,plot,x,y,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING){
		NhlPError(FATAL,E_UNKNOWN,"XyPlotNDCToData: A FATAL error occured while transforming input to window, XyPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if( ret < ret1)
		ret1 = ret;

	if(!istrans) {
		if(x != xout)
		bcopy((char*)x,(char*)xout,sizeof(float)*n);
		if(y != yout)
		bcopy((char*)y,(char*)yout,sizeof(float)*n);
	}

	istrans = 0;
	ret = _NhlWinToNDC(xplot->xyplot.thetrans,plot,xout,yout,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"XyPlotNDCToData: A FATAL error occured while transforming from window to NDC, XyPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if( ret < ret1)
		ret1 = ret;

	return(ret1);

}
/*
 * Function:	XyPlotNDCToData
 *
 * Description:	Transform objects NDC to Data method for the XyPlot. 
 *		Takes one or more x,y pairs of NDC points and converts them
 *		into their respective data values.
 *
 * In Args:	plot 	instance record pointer
 *		x	array of x ndc vals to convert
 *		y	array of y ndc vals to convert
 *		n	number of elements in x,y,xout and yout
 *	 	xout	storage provided by user for conversion output
 *		yout	storage provided by user for conversion output
 *		xmissing  missing values in x input
 *		ymissing  missing values in y input
 *
 * Out Args:	xout	but does not allocate storage
 *		yout	but does not allocate storage
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	 NONE
 */
static NhlErrorTypes XyPlotNDCToData
#if __STDC__
(Layer plot,float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing)
#else
(plot,x,y,n,xout,yout,xmissing,ymissing)
	Layer		plot;
	float*		x;
	float*		y;
	int		n;
	float*		xout;
	float*		yout;
	float*		xmissing;
	float*		ymissing;
#endif
{
	XyPlotLayer xplot = (XyPlotLayer)plot;
	int istrans = 0;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;

	ret = _NhlSetTrans(xplot->xyplot.thetrans,plot);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"XyPlotNDCToData: A FATAL error occured while setting the tranformation of XyPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if(ret < ret1)
		ret1 = ret;


	ret = _NhlNDCToWin(xplot->xyplot.thetrans,plot,x,y,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING){
		NhlPError(FATAL,E_UNKNOWN,"XyPlotNDCToData: A FATAL error occured while transforming input to window, XyPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if(ret < ret1)
		ret1 = ret;


	if(!istrans) {
		if(x != xout)
		bcopy((char*)x,(char*)xout,sizeof(float)*n);
		if(y != yout)
		bcopy((char*)y,(char*)yout,sizeof(float)*n);
	}


	istrans = 0;
	ret = _NhlWinToData(xplot->xyplot.thetrans,plot,xout,yout,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"XyPlotNDCToData: A FATAL error occured while transforming from window to data, XyPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if(ret < ret1)
		ret1 = ret;


	return(ret1);

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
 *		curve_dash_patterns
 *		*_irregular_points
 *		dummy_array
 *		dummy_array_final
 *		*_alternate_coords
 *		*_original_coords
 *
 * In Args:	inst		instance record pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes XyPlotDestroy
#if __STDC__
(Layer inst)
#else
(inst)
Layer inst;
#endif
{
	XyPlotLayer xinst = (XyPlotLayer)inst;
	int i;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;


	if(xinst->xyplot.ticks != NULL)
		ret = _NhlDestroyChild(xinst->xyplot.ticks->base.id,inst);
	if(ret < ret1)
		ret1 = ret;

	if(xinst->xyplot.ttitles != NULL)
		ret = _NhlDestroyChild(xinst->xyplot.ttitles->base.id,inst);
	if(ret < ret1)
		ret1 = ret;

	if(xinst->xyplot.thetrans != NULL)
		ret = NhlDestroy(xinst->xyplot.thetrans->base.id);
	if(ret < ret1)
		ret1 = ret;

	for(i = 0; i< xinst->xyplot.num_curves; i++) {
		if(!xinst->xyplot.noxvalues) {
			NhlFree(xinst->xyplot.x_values[i]);
		}
		if(!xinst->xyplot.noyvalues) {
			NhlFree(xinst->xyplot.y_values[i]);
		}
		if((xinst->xyplot.curve_line_labels != NULL)&&(xinst->xyplot.curve_line_labels[i] != NULL)) {
			NhlFree(xinst->xyplot.curve_line_labels[i]);
		}
	}
	if(xinst->xyplot.curve_colors != NULL) {
		NhlFree(xinst->xyplot.curve_colors);
	}
	if(xinst->xyplot.curve_lengths != NULL) {
		NhlFree(xinst->xyplot.curve_lengths);
	}
	if(xinst->xyplot.curve_dash_patterns != NULL) {
		NhlFree(xinst->xyplot.curve_dash_patterns);
	}
	if(xinst->xyplot.x_irregular_points != NULL) {
		NhlFree(xinst->xyplot.x_irregular_points);
	}
	if(xinst->xyplot.y_irregular_points != NULL) {
		NhlFree(xinst->xyplot.y_irregular_points);
	}
	if(xinst->xyplot.dummy_array != NULL) {
		NhlFree(xinst->xyplot.dummy_array);
	}
	if(xinst->xyplot.y_alternate_coords!= NULL) {
		NhlFree(xinst->xyplot.y_alternate_coords);
	}
	if(xinst->xyplot.x_alternate_coords!= NULL) {
		NhlFree(xinst->xyplot.x_alternate_coords);
	}
	if(xinst->xyplot.x_original_coords!= NULL) {
		NhlFree(xinst->xyplot.x_original_coords);
	}
	if(xinst->xyplot.y_original_coords!= NULL) {
		NhlFree(xinst->xyplot.y_original_coords);
	}
	
	
	

	if(xinst->xyplot.ti_main_string != main) {
		NhlFree(xinst->xyplot.ti_main_string);
	}
	if(xinst->xyplot.ti_x_axis_string != x_axis) {
		NhlFree(xinst->xyplot.ti_x_axis_string);
	}
	if(xinst->xyplot.ti_y_axis_string != y_axis) {
		NhlFree(xinst->xyplot.ti_y_axis_string);
	}
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
 * Out Args:	NONE
 *
 * Return Values:  Error Conditions
 *
 * Side Effects:   NONE.
 */
static NhlErrorTypes XyPlotGetBB
#if  __STDC__
(Layer instance, NhlBoundingBox* thebox)
#else
(instance,thebox)
	Layer instance;
	NhlBoundingBox *thebox;
#endif
{
	XyPlotLayer xinst = (XyPlotLayer)instance;
	NhlErrorTypes ret = NOERROR;

	if(xinst->xyplot.ticks != NULL) {
		ret = _NhlGetBB(xinst->xyplot.ticks,thebox);
		if(ret < WARNING) 
			return(ret);
	}
	
	if(xinst->xyplot.ttitles != NULL) {
		return(MIN(ret,_NhlGetBB(xinst->xyplot.ttitles,thebox)));
	} else {
		return(ret);
	}
}


/*
 * Function:	CheckValues
 *
 * Description:	Checks important resourceS to make sure they are set. It also
 *		allocates a dummy array which is used when one of the *Values
 *		resources is not set. This function makes sure data extents
 *		are set as well as data values.
 *
 * In Args:	xnew	new instance record
 *		xold	old instance record if c_or_s == SET
 *		c_or_s	SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes CheckValues
#if  __STDC__
(XyPlotLayer xnew,XyPlotLayer xold,int c_or_s)
#else
(xnew,xold,c_or_s)
	XyPlotLayer xnew;
	XyPlotLayer xold;
	int	c_or_s;
#endif
{ 	
	int i,j;
	char *error_lead;

	if(c_or_s == CREATE) {
		error_lead = "XyPlotInitialize";
	} else {
		error_lead = "XyPlotSetValues";
	}
	
	if(xnew->xyplot.x_values == NULL) {
		xnew->xyplot.noxvalues = 1;
	} else {
		xnew->xyplot.noxvalues = 0;
	}
	if(xnew->xyplot.y_values == NULL) {
		xnew->xyplot.noyvalues = 1;
	} else {
		xnew->xyplot.noyvalues = 0;
	}
	if(xnew->xyplot.x_missing == 1e30) {
		xnew->xyplot.thexmissing = NULL;
	} else {
		xnew->xyplot.thexmissing = &xnew->xyplot.x_missing;
	}
	if(xnew->xyplot.y_missing == 1e30) {
		xnew->xyplot.theymissing = NULL;
	} else {
		xnew->xyplot.theymissing = &xnew->xyplot.y_missing;
	}

	if((xnew->xyplot.num_curves > 0)&&(xnew->xyplot.curve_lengths != NULL)) {
		if((xnew->xyplot.noxvalues)&&(xnew->xyplot.noyvalues)) {
			if((xnew->xyplot.x_left == 0.0) 
				&&(xnew->xyplot.x_right==0.0) ){
				NhlPError(FATAL,E_UNKNOWN,"%s: Insufficient information, cannot configure XYPLOT X Axis data extents",error_lead);
				return(FATAL);
			}
			if((xnew->xyplot.y_top == 0.0) 
				&&(xnew->xyplot.y_bottom == 0.0)){
				NhlPError(FATAL,E_UNKNOWN,"%s: Insufficient information, cannot configure XYPLOT Y Axis data extents",error_lead);
				return(FATAL);
			}
		} else if(xnew->xyplot.noxvalues) {
			if((xnew->xyplot.x_left == 0.0) 
				&&(xnew->xyplot.x_right==0.0) ){
				xnew->xyplot.x_left = 1.0;
				for(i = 0; i<xnew->xyplot.num_curves; i++) {
					if((float)xnew->xyplot.curve_lengths[i] 
						> xnew->xyplot.x_right){
						xnew->xyplot.x_right =
							xnew->xyplot.curve_lengths[i];
					}
				}
				xnew->xyplot.dummy_array_length = (int)xnew->xyplot.x_right;
			} else {
				xnew->xyplot.dummy_array_length = 0;
				for(i = 0; i<xnew->xyplot.num_curves; i++) {
					if(xnew->xyplot.curve_lengths[i] >
						xnew->xyplot.dummy_array_length)
						xnew->xyplot.dummy_array_length=
							xnew->xyplot.curve_lengths[i];
				}
			}
/* 
* Because of how transformations are designed a dummy_array and a 
* dummy_array_final need to be created so _NhlxxxxToxxxx functions can 
* be called.
*/
			if(c_or_s == SET){
				if(xold->xyplot.dummy_array != NULL) {
					NhlFree((void*)xold->xyplot.dummy_array);
				}
			}
			xnew->xyplot.dummy_array = 
				(float*)NhlMalloc((unsigned)sizeof(float)*
					xnew->xyplot.dummy_array_length);
			if( xnew->xyplot.dummy_array == NULL) {
				NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
				return(FATAL);
			}
			for(i=1; i<= xnew->xyplot.dummy_array_length; i++) {
				xnew->xyplot.dummy_array[i-1] = (float)i;
			}
			if((xnew->xyplot.y_top == 0.0)
				&&(xnew->xyplot.y_bottom == 0.0)) {
				xnew->xyplot.y_top = -1e30;
				xnew->xyplot.y_bottom = 1e30;
				for(i = 0; i<xnew->xyplot.num_curves; i++){
					for(j=0;j<xnew->xyplot.curve_lengths[i];j++){

if((xnew->xyplot.y_top < (xnew->xyplot.y_values[i])[j])&&((xnew->xyplot.theymissing == NULL)||(*xnew->xyplot.theymissing != (xnew->xyplot.y_values[i])[j])))
	xnew->xyplot.y_top = (xnew->xyplot.y_values[i])[j];

if((xnew->xyplot.y_bottom > (xnew->xyplot.y_values[i])[j])&&((xnew->xyplot.theymissing == NULL)||(*xnew->xyplot.theymissing != (xnew->xyplot.y_values[i])[j])))
	xnew->xyplot.y_bottom = (xnew->xyplot.y_values[i])[j];
					}
				}	
			}
		} else if(xnew->xyplot.noyvalues) {
			if((xnew->xyplot.y_top == 0.0) 
				&&(xnew->xyplot.y_bottom==0.0) ){
				xnew->xyplot.y_bottom = 1.0;
				for(i = 0; i<xnew->xyplot.num_curves; i++) {
					if((float)xnew->xyplot.curve_lengths[i] 
						> xnew->xyplot.y_top) {
						xnew->xyplot.y_top=
							xnew->xyplot.curve_lengths[i];
					}
				}
				xnew->xyplot.dummy_array_length = (int)xnew->xyplot.y_top;
			} else {
				xnew->xyplot.dummy_array_length = 0;
				for(i = 0; i<xnew->xyplot.num_curves; i++) {
					if(xnew->xyplot.curve_lengths[i] >
						xnew->xyplot.dummy_array_length)
						xnew->xyplot.dummy_array_length=
							xnew->xyplot.curve_lengths[i];
				}
			}
			if(c_or_s == SET) {
				if(xold->xyplot.dummy_array != NULL){
					NhlFree((void*)xold->xyplot.dummy_array);
				}
			}
			xnew->xyplot.dummy_array = 
				(float*)NhlMalloc((unsigned)sizeof(float)*
					xnew->xyplot.dummy_array_length);
			if( xnew->xyplot.dummy_array == NULL) {
				NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
				return(FATAL);
			}
			for(i=1; i<= xnew->xyplot.dummy_array_length; i++) {
				xnew->xyplot.dummy_array[i-1] = (float)i;
			}
			if((xnew->xyplot.x_left == 0.0)
				&&(xnew->xyplot.x_right == 0.0)) {
				xnew->xyplot.x_right = -1e30;
				xnew->xyplot.x_left = 1e30;
				for(i = 0; i<xnew->xyplot.num_curves; i++){
					for(j=0;j<xnew->xyplot.curve_lengths[i];j++){
if((xnew->xyplot.x_right < (xnew->xyplot.x_values[i])[j])&&((xnew->xyplot.thexmissing == NULL)||(*xnew->xyplot.thexmissing != (xnew->xyplot.x_values[i])[j])))
	xnew->xyplot.x_right = (xnew->xyplot.x_values[i])[j];

if((xnew->xyplot.x_left > (xnew->xyplot.x_values[i])[j])&&((xnew->xyplot.thexmissing == NULL)||(*xnew->xyplot.thexmissing != (xnew->xyplot.x_values[i])[j])))
	xnew->xyplot.x_left= (xnew->xyplot.x_values[i])[j];
					}
				}	
			}
		} else {
			xnew->xyplot.dummy_array = NULL;
			if((xnew->xyplot.x_left == 0.0)
				&&(xnew->xyplot.x_right == 0.0)) {
				xnew->xyplot.x_right = -1e30;
				xnew->xyplot.x_left = 1e30;
				for(i = 0; i<xnew->xyplot.num_curves; i++){
					for(j=0;j<xnew->xyplot.curve_lengths[i];j++){
if((xnew->xyplot.x_right < (xnew->xyplot.x_values[i])[j])&&((xnew->xyplot.thexmissing == NULL)||(*xnew->xyplot.thexmissing != (xnew->xyplot.x_values[i])[j])))
	xnew->xyplot.x_right = (xnew->xyplot.x_values[i])[j];

if((xnew->xyplot.x_left > (xnew->xyplot.x_values[i])[j])&&((xnew->xyplot.thexmissing == NULL)||(*xnew->xyplot.thexmissing != (xnew->xyplot.x_values[i])[j])))
	xnew->xyplot.x_left= (xnew->xyplot.x_values[i])[j];
					}
				}	
			}
			if((xnew->xyplot.y_top == 0.0)
				&&(xnew->xyplot.y_bottom == 0.0)) {
				xnew->xyplot.y_top = -1e30;
				xnew->xyplot.y_bottom = 1e30;
				for(i = 0; i<xnew->xyplot.num_curves; i++){
					for(j=0;j<xnew->xyplot.curve_lengths[i];j++){
if((xnew->xyplot.y_top < (xnew->xyplot.y_values[i])[j])&&((xnew->xyplot.theymissing == NULL)||(*xnew->xyplot.theymissing != (xnew->xyplot.y_values[i])[j])))
	xnew->xyplot.y_top = (xnew->xyplot.y_values[i])[j];

if((xnew->xyplot.y_bottom > (xnew->xyplot.y_values[i])[j])&&((xnew->xyplot.theymissing == NULL)||(*xnew->xyplot.theymissing != (xnew->xyplot.y_values[i])[j])))
	xnew->xyplot.y_bottom = (xnew->xyplot.y_values[i])[j];
					}
				}	
			}
		}
	} else if(xnew->xyplot.num_curves <= 0) {
			if((xnew->xyplot.x_left == 0.0) 
				&&(xnew->xyplot.x_right==0.0) ){
				NhlPError(FATAL,E_UNKNOWN,"%s: Insufficient information, cannot configure XYPLOT X Axis data extents",error_lead);
				return(FATAL);
			}
			if((xnew->xyplot.y_top == 0.0) 
				&&(xnew->xyplot.y_bottom == 0.0)){
				NhlPError(FATAL,E_UNKNOWN,"%s: Insufficient information, cannot configure XYPLOT Y Axis data extents",error_lead);
				return(FATAL);
			}
	} else {
		NhlPError(FATAL,E_UNKNOWN,"%s: Curve lengths not set, cannot configure XYPLOT data",error_lead);
		return(FATAL);
	}

	return(NOERROR);
}


/*
 * Function:	InternalizePointers
 *
 * Description:	Takes care of allocation and deallocation of all array resources
 *		as well as copying their contents. The resources and fields 
 *		affected are:
 *		*_values
 *		curve_colors
 *		curve_lengths
 *		curve_dash_patterns
 *		curve_line_labels
 *
 * In Args:	xnew	new instance
 *		xold	old instance record if c_or_s == SET
 *		c_or_s  SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Memory allocation and deallocation
 */
static NhlErrorTypes InternalizePointers
#if  __STDC__
(XyPlotLayer xnew, XyPlotLayer xold, int c_or_s)
#else
(xnew,xold,c_or_s)
	XyPlotLayer xnew;
	XyPlotLayer xold;
	int c_or_s;
#endif
{
	float **tmpfptrptr;
	int i;
	int *tmpiptr;
	char *tmpcptr;
	char** tmpcptrptr;
	float *tmpfptr;
	char* error_lead;
	NhlErrorTypes ret = NOERROR;

	if(c_or_s == SET) {
		error_lead = "XyPlotSetValues";
	} else {
		error_lead = "XyPlotInitialize";
	}
/*
* Now internalize pointer types
*/
	if((!xnew->xyplot.noxvalues)&&((c_or_s == CREATE)||(xnew->xyplot.x_values != xold->xyplot.x_values))) {
		if((c_or_s == SET)&&(!xold->xyplot.noxvalues)){
			for(i=0; i < xold->xyplot.num_curves;i++) {
				NhlFree(xold->xyplot.x_values[i]);
			}
			NhlFree(xold->xyplot.x_values);
		}
		tmpfptrptr = (float**)NhlMalloc((unsigned)sizeof(float*)
				*xnew->xyplot.num_curves);
		if(tmpfptrptr == NULL) {
			NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
			return(FATAL);
		}
		
		for(i=0; i < xnew->xyplot.num_curves;i++) {
			tmpfptrptr[i] = (float*)NhlMalloc(
					(unsigned)sizeof(float)*
					xnew->xyplot.curve_lengths[i]);
			if(tmpfptrptr[i] == NULL) {
				NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
				return(FATAL);
			}
			
			bcopy((char*)(xnew->xyplot.x_values[i]),
				(char*)(tmpfptrptr[i]),
				sizeof(float)*xnew->xyplot.curve_lengths[i]);
		}
		xnew->xyplot.x_values = tmpfptrptr;
	} 
	if((!xnew->xyplot.noyvalues)&&((c_or_s == CREATE)||(xnew->xyplot.y_values != xold->xyplot.y_values))) {
		if((c_or_s == SET)&&(!xold->xyplot.noyvalues)) {
			for(i=0; i < xold->xyplot.num_curves;i++) {
				NhlFree(xold->xyplot.y_values[i]);
			}
			NhlFree(xold->xyplot.y_values);
		} 
		tmpfptrptr = (float**)NhlMalloc(
				(unsigned)sizeof(float*)*
				xnew->xyplot.num_curves);
		if(tmpfptrptr == NULL) {
			NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
			return(FATAL);
		}

		for(i=0; i < xnew->xyplot.num_curves;i++) {
			tmpfptrptr[i] = (float*)NhlMalloc(
				(unsigned)sizeof(float)*
				xnew->xyplot.curve_lengths[i]);
			if( tmpfptrptr[i] == NULL) {
				NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
				return(FATAL);
			}
			bcopy((char*)(xnew->xyplot.y_values[i]),
				(char*)(tmpfptrptr[i]),
				sizeof(float)*xnew->xyplot.curve_lengths[i]);
		}
		xnew->xyplot.y_values = tmpfptrptr;
	} 
	if((xnew->xyplot.curve_colors != NULL)&&((c_or_s == CREATE)||(xnew->xyplot.curve_colors != xold->xyplot.curve_colors))) {
		if((c_or_s == SET)&&(xold->xyplot.curve_colors != NULL)) {
			NhlFree(xold->xyplot.curve_colors);
		}
		tmpiptr = (int*) NhlMalloc((unsigned)sizeof(int)*
				xnew->xyplot.num_curves);
		if( tmpiptr== NULL) {
			NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
			return(FATAL);
		}

		bcopy((char*)(xnew->xyplot.curve_colors),
			(char*)(tmpiptr),
			sizeof(int)*xnew->xyplot.num_curves);
		xnew->xyplot.curve_colors = tmpiptr;
	}
	if((xnew->xyplot.curve_lengths != NULL)&&((c_or_s == CREATE)||(xnew->xyplot.curve_lengths != xold->xyplot.curve_lengths))) {
		if((c_or_s == SET)&&(xold->xyplot.curve_lengths != NULL)){
			NhlFree(xold->xyplot.curve_lengths);
		} 
		tmpiptr = (int*) NhlMalloc((unsigned)sizeof(int)*
				xnew->xyplot.num_curves);
		if( tmpiptr== NULL) {
			NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
			return(FATAL);
		}
		bcopy((char*)(xnew->xyplot.curve_lengths),
			(char*)(tmpiptr),
			sizeof(int)*xnew->xyplot.num_curves);
		xnew->xyplot.curve_lengths = tmpiptr;
	}
	if((xnew->xyplot.curve_dash_patterns != NULL)&&((c_or_s == CREATE)||(xnew->xyplot.curve_dash_patterns != xold->xyplot.curve_dash_patterns))) {
		if((c_or_s == SET)&&(xold->xyplot.curve_dash_patterns != NULL)) {
			NhlFree(xold->xyplot.curve_dash_patterns);
		} 
		tmpiptr = (int*) NhlMalloc((unsigned)sizeof(int)*
				xnew->xyplot.num_curves);
		if( tmpiptr== NULL) {
			NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
			return(FATAL);
		}
		bcopy((char*)(xnew->xyplot.curve_dash_patterns),
			(char*)(tmpiptr),
			sizeof(int)*xnew->xyplot.num_curves);
		xnew->xyplot.curve_dash_patterns = tmpiptr;
	}
	if((xnew->xyplot.curve_line_labels != NULL)&&((c_or_s == CREATE)||(xnew->xyplot.curve_line_labels != xold->xyplot.curve_line_labels))) {
		if(xnew->xyplot.curve_line_label_mode != CUSTOM) {
			NhlPError(WARNING,E_UNKNOWN,"%s: xyCurveLineLabel resource set but CurveLineLabelMode is not equal to CUSTOM, lines will not be labeled",error_lead);
			ret = WARNING;
		}
		if((c_or_s == SET)&&(xold->xyplot.curve_line_labels != NULL)) {
			for(i = 0; i < xold->xyplot.num_curves; i++) {
				NhlFree(xold->xyplot.curve_line_labels[i]);
			}
			NhlFree(xold->xyplot.curve_line_labels);
		}
		tmpcptrptr = (char**) NhlMalloc((unsigned)sizeof(char*)*
                	xnew->xyplot.num_curves);
		if( tmpcptrptr== NULL) {
			NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
			return(FATAL);
		}
		for(i = 0; i < xnew->xyplot.num_curves; i++) {
			if(xnew->xyplot.curve_line_labels[i] != NULL) {
				tmpcptrptr[i] = (char*)NhlMalloc((unsigned)
					strlen(xnew->xyplot.curve_line_labels[i])+1);
				if( tmpcptrptr[i] == NULL) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
					return(FATAL);
				}
				strcpy(tmpcptrptr[i],xnew->xyplot.curve_line_labels[i]);
			} else {
				tmpcptrptr[i] = NULL;
			}
		}
		xnew->xyplot.curve_line_labels = tmpcptrptr;
	} else if((xnew->xyplot.curve_line_label_mode == CUSTOM)&&(xnew->xyplot.curve_line_labels == NULL)){
		NhlPError(WARNING,E_UNKNOWN,"%s: CurveLineLabelMode set to CUSTOM but no labels provided, setting CurveLineLabelMode to NONE",error_lead);
		xnew->xyplot.curve_line_label_mode = NONE;
	}
	if((xnew->xyplot.x_irregular_points != NULL)&&((c_or_s == CREATE)||(xnew->xyplot.x_irregular_points != xold->xyplot.x_irregular_points))) {
		if(xnew->xyplot.x_num_irregular_points > 0) {
			if((c_or_s == SET)&&(xold->xyplot.x_irregular_points != NULL)) {
				NhlFree( xold->xyplot.x_irregular_points);
			}
			tmpfptr = (float*)NhlMalloc((unsigned)sizeof(float)*
				xnew->xyplot.x_num_irregular_points);
			if( tmpfptr== NULL) {
				NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
				return(FATAL);
			}
			bcopy((char*)xnew->xyplot.x_irregular_points,(char*)tmpfptr,
			sizeof(float)*xnew->xyplot.x_num_irregular_points);
			xnew->xyplot.x_irregular_points = tmpfptr;
		} else {
			NhlPError(WARNING,E_UNKNOWN,"%s: XIrregularPoints set but XNumIrregularPoints is equal to 0 cannot copy irregular coordinates, if XStyle is IRREGULAR it will be changed to LINEAR",error_lead);
			if(WARNING < ret )
				ret = WARNING;
			if(xnew->xyplot.x_style == IRREGULAR)
				xnew->xyplot.x_style = LINEAR;
			xnew->xyplot.x_irregular_points = NULL;
		}
	}
	if((xnew->xyplot.y_irregular_points != NULL)&&((c_or_s == CREATE)||(xnew->xyplot.y_irregular_points != xold->xyplot.y_irregular_points))) {
		if(xnew->xyplot.y_num_irregular_points > 0) {
			if((c_or_s == SET)&&(xold->xyplot.y_irregular_points != NULL)){
				NhlFree(xold->xyplot.y_irregular_points);
			} 
			tmpfptr = (float*)NhlMalloc((unsigned)sizeof(float)*
					xnew->xyplot.y_num_irregular_points);
			if( tmpfptr== NULL) {
				NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
				return(FATAL);
			}
			bcopy((char*)xnew->xyplot.y_irregular_points,(char*)tmpfptr,
				sizeof(float)*xnew->xyplot.y_num_irregular_points);
			xnew->xyplot.y_irregular_points = tmpfptr;
		} else {
			NhlPError(WARNING,E_UNKNOWN,"%s: YIrregularPoints set but YNumIrregularPoints is equal to 0 cannot copy irregular coordinates, if YStyle is IRREGULAR it will be changed to LINEAR",error_lead);
			if(WARNING < ret )
				ret = WARNING;
			if(xnew->xyplot.y_style == IRREGULAR)
				xnew->xyplot.y_style = LINEAR;
			xnew->xyplot.y_irregular_points = NULL;
		}
	}
	if((xnew->xyplot.x_alternate_coords != NULL)&&((c_or_s == CREATE)||(xnew->xyplot.x_alternate_coords != xold->xyplot.x_alternate_coords))) {
		if(xnew->xyplot.x_num_alternate_coords > 0) {
			if((c_or_s == SET)&&(xold->xyplot.x_alternate_coords != NULL)) {
				NhlFree(xold->xyplot.x_alternate_coords);
			} 
			tmpfptr = (float*)NhlMalloc((unsigned)sizeof(float)*
				xnew->xyplot.x_num_alternate_coords);
			if( tmpfptr== NULL) {
				NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
				return(FATAL);
			}
			bcopy((char*)xnew->xyplot.x_alternate_coords,(char*)tmpfptr,
				sizeof(float)*xnew->xyplot.x_num_alternate_coords);
			xnew->xyplot.x_alternate_coords = tmpfptr;
		} else {
			NhlPError(WARNING,E_UNKNOWN,"%s: XAlternateCoords set but XNumAlternateCoords is equal to 0 cannot copy alternate coordinates, Alternate coordinates will not be used",error_lead);
			if(WARNING < ret )
				ret = WARNING;
			xnew->xyplot.x_alternate = NONE;
			xnew->xyplot.x_alternate_coords = NULL;
			xnew->xyplot.x_original_coords = NULL;
		}
	}
	if((xnew->xyplot.y_alternate_coords != NULL)&&((c_or_s == CREATE)||(xnew->xyplot.y_alternate_coords != xold->xyplot.y_alternate_coords))) {	
		if(xnew->xyplot.y_num_alternate_coords > 0) {
			if((c_or_s == SET)&&(xold->xyplot.y_alternate_coords!=NULL)) {
				NhlFree(xold->xyplot.y_alternate_coords);
			} 
			tmpfptr = (float*)NhlMalloc((unsigned)sizeof(float)
				*xnew->xyplot.y_num_alternate_coords);
			if( tmpfptr== NULL) {
				NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
				return(FATAL);
			}
			bcopy((char*)xnew->xyplot.y_alternate_coords,(char*)tmpfptr,
				sizeof(float)*xnew->xyplot.y_num_alternate_coords);
			xnew->xyplot.y_alternate_coords = tmpfptr;
		} else {
			NhlPError(WARNING,E_UNKNOWN,"%s: YAlternateCoords set but YNumAlternateCoords is equal to 0 cannot copy alternate coordinates, Alternate coordinates will not be used",error_lead);
			if(WARNING < ret )
				ret = WARNING;
			xnew->xyplot.y_alternate = NONE;
			xnew->xyplot.y_alternate_coords = NULL;
			xnew->xyplot.y_original_coords = NULL;
		}
	}
	if((xnew->xyplot.x_original_coords != NULL)&&((c_or_s == CREATE) || (xnew->xyplot.x_original_coords != xold->xyplot.x_original_coords))) {
		if((c_or_s == SET)&&(xold->xyplot.x_original_coords!= NULL)) {
				NhlFree(xold->xyplot.x_original_coords);
		} 
		tmpfptr = (float*)NhlMalloc((unsigned)sizeof(float)
				*xnew->xyplot.x_num_alternate_coords);
		if( tmpfptr== NULL) {
			NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
			return(FATAL);
		}
		bcopy((char*)xnew->xyplot.x_original_coords,(char*)tmpfptr,
			sizeof(float)*xnew->xyplot.x_num_alternate_coords);
		xnew->xyplot.x_original_coords = tmpfptr;
	}
	if((xnew->xyplot.y_original_coords != NULL)&&((c_or_s == CREATE)||(xnew->xyplot.y_original_coords != xold->xyplot.y_original_coords))) {
		if((c_or_s == SET)&&(xold->xyplot.y_original_coords)) {
				NhlFree(xold->xyplot.y_original_coords);
		}
		tmpfptr = (float*)NhlMalloc((unsigned)sizeof(float)
				*xnew->xyplot.y_num_alternate_coords);
		if( tmpfptr== NULL) {
			NhlPError(FATAL,E_UNKNOWN,"%s: Cannot continue because of malloc failure",error_lead);
			return(FATAL);
		}
		bcopy((char*)xnew->xyplot.y_original_coords,(char*)tmpfptr,
			sizeof(float)*xnew->xyplot.y_num_alternate_coords);
		xnew->xyplot.y_original_coords = tmpfptr;
	}

	if(xnew->xyplot.ti_main_string != main) {
		if((c_or_s ==SET)&&(xnew->xyplot.ti_main_string != xold->xyplot.ti_main_string)&&(xold->xyplot.ti_main_string != main)) {
			NhlFree(xold->xyplot.ti_main_string);
		}
		tmpcptr = (char*) NhlMalloc((unsigned)strlen(xnew->xyplot.ti_main_string)+1);
		strcpy(tmpcptr,xnew->xyplot.ti_main_string);
		xnew->xyplot.ti_main_string = tmpcptr;
	}
	if(xnew->xyplot.ti_x_axis_string != x_axis) {
		if((c_or_s ==SET)&&(xnew->xyplot.ti_x_axis_string != xold->xyplot.ti_x_axis_string)&&(xold->xyplot.ti_x_axis_string != main)) {
			NhlFree(xold->xyplot.ti_x_axis_string);
		}
		tmpcptr = (char*) NhlMalloc((unsigned)strlen(xnew->xyplot.ti_x_axis_string)+1);
		strcpy(tmpcptr,xnew->xyplot.ti_x_axis_string);
		xnew->xyplot.ti_x_axis_string = tmpcptr;
	}
	if(xnew->xyplot.ti_y_axis_string != y_axis) {
		if((c_or_s ==SET)&&(xnew->xyplot.ti_y_axis_string != xold->xyplot.ti_y_axis_string)&&(xold->xyplot.ti_y_axis_string != main)) {
			NhlFree(xold->xyplot.ti_y_axis_string);
		}
		tmpcptr = (char*) NhlMalloc((unsigned)strlen(xnew->xyplot.ti_y_axis_string)+1);
		strcpy(tmpcptr,xnew->xyplot.ti_y_axis_string);
		xnew->xyplot.ti_y_axis_string = tmpcptr;
	}
	return(ret);
}

/*
 * Function:	SetMinMax
 *
 * Description:	Since there is no constraint on top > bottom or vice versa
 *		this function sets the *_data_min fields for latter reference.
 *
 * In Args:	xnew	new instance record
 *
 * Out Args:	NONE
 *
 * Return Values:	NONE
 *
 * Side Effects:	NONE
 */
static void SetMinMax
#if __STDC__
(XyPlotLayer xnew)
#else 
(xnew)
	XyPlotLayer xnew;
#endif
{
	int i;

	xnew->xyplot.y_data_min = MIN(xnew->xyplot.y_top,
					xnew->xyplot.y_bottom);
	xnew->xyplot.y_data_max = MAX(xnew->xyplot.y_top,
					xnew->xyplot.y_bottom);
	xnew->xyplot.x_data_min = MIN(xnew->xyplot.x_right,
					xnew->xyplot.x_left);
	xnew->xyplot.x_data_max = MAX(xnew->xyplot.x_right,
					xnew->xyplot.x_left);

	if(xnew->xyplot.x_style == IRREGULAR) {
		xnew->xyplot.x_irr_min = xnew->xyplot.x_irregular_points[0];
		xnew->xyplot.x_irr_max = xnew->xyplot.x_irregular_points[0];
		for(i = 1; i< xnew->xyplot.x_num_irregular_points; i++) {
			xnew->xyplot.x_irr_min = MIN(xnew->xyplot.x_irr_min,
					xnew->xyplot.x_irregular_points[i]);
			xnew->xyplot.x_irr_max = MAX(xnew->xyplot.x_irr_max,
					xnew->xyplot.x_irregular_points[i]);
		}	
	} else {
		xnew->xyplot.x_irr_min = xnew->xyplot.x_data_min;
		xnew->xyplot.x_irr_max = xnew->xyplot.x_data_max;
	}
	if(xnew->xyplot.y_style == IRREGULAR) {
		xnew->xyplot.y_irr_min = xnew->xyplot.y_irregular_points[0];
		xnew->xyplot.y_irr_max = xnew->xyplot.y_irregular_points[0];
		for(i = 1; i< xnew->xyplot.y_num_irregular_points; i++) {
			xnew->xyplot.y_irr_min = MIN(xnew->xyplot.y_irr_min,
					xnew->xyplot.y_irregular_points[i]);
			xnew->xyplot.y_irr_max = MAX(xnew->xyplot.y_irr_max,
					xnew->xyplot.y_irregular_points[i]);
		}	
	} else {
		xnew->xyplot.y_irr_min = xnew->xyplot.y_data_min;
		xnew->xyplot.y_irr_max = xnew->xyplot.y_data_max;
	}
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
 *		happen when either XStyle or YStyle is IRREGULAR and the other
 *		is not. When this happens an IrregularTransObj is created 
 *		and one of the IrregularTranObj is "fooled" into a linear
 *		or log tranformation. This is facilitated for log axis by 
 *		a resource that instructs the IrragularTransObj to take
 *		the logs of the input values and create an approximation of
 *		the logs of the data values.
 *
 * In Args:	xnew	new instance record
 *		xold	old instance record if c_or_s == SET
 *		c_or_s  set to CREATE or SET
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes SetUpTransObjs
#if  __STDC__
(XyPlotLayer xnew, XyPlotLayer xold, int c_or_s)
#else 
(xnew,xold,c_or_s)
	XyPlotLayer xnew;
	XyPlotLayer xold;
	int		c_or_s;
#endif
{
	char buffer[MAXFNAMELEN];
	int xlog = 0, ylog = 0;
	int tmpid;
	float tmpcoords[3];
	char *error_lead;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;
/*
* Now create main transformation object Make sure data left right top and bottom
* are set correctly. These should not be confused with tick mark transformations
*/	
	if(c_or_s == SET) {
		error_lead = "XyPlotInitialize";
	} else {
		error_lead = "XyPlotSetValues";
	}

	sprintf(buffer,"%s",xnew->base.name);
	strcat(buffer,".Trans");
	xlog = 0;
	ylog = 0;
	switch(xnew->xyplot.x_style) {
	case LOG:
		xlog = 1;
		if((xnew->xyplot.x_left <= 0.0)||(xnew->xyplot.x_right<=0.0)){
				NhlPError(FATAL,E_UNKNOWN,"%s: LOG style selected for X-axis but either top or bottom value  is <= 0.0",error_lead);
				return(FATAL);
		}
	case LINEAR:
		switch(xnew->xyplot.y_style){
		case LOG:
			ylog = 1;
			if((xnew->xyplot.y_top<= 0.0)||(xnew->xyplot.y_bottom<=0.0)) {
				NhlPError(FATAL,E_UNKNOWN,"%s: LOG style selected for Y-axis but either top or bottom value  is <= 0.0",error_lead);
				return(FATAL);
			}
		case LINEAR:
			if((c_or_s == CREATE)||((xnew->xyplot.x_style != xold->xyplot.x_style)||(xnew->xyplot.y_style != xold->xyplot.y_style))) {
				if(c_or_s == SET) {
					ret = NhlDestroy(xold->xyplot.thetrans->base.id);
					if(ret <ret1)
						ret1 = ret;
				}
				ret = NhlCreate(&tmpid,buffer,
					logLinTransObjLayerClass,
					xnew->base.id,
					NhlNtrXMinF,xnew->xyplot.x_data_min,
					NhlNtrXMaxF,xnew->xyplot.x_data_max,
					NhlNtrXLog,xlog,
					NhlNtrXReverse, (xnew->xyplot.x_left 
						>xnew->xyplot.x_data_min?1 : 0),
					NhlNtrYMinF,xnew->xyplot.y_data_min,
					NhlNtrYMaxF,xnew->xyplot.y_data_max,
					NhlNtrYLog,ylog,
					NhlNtrYReverse, (xnew->xyplot.y_bottom 
						>xnew->xyplot.y_data_min?1 : 0)
					,NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not create internal tranformation object, cannot continue",error_lead);
					return(FATAL);
				} else if( ret < ret1 )
					ret1 = ret;
			} else {
				tmpid = xnew->xyplot.thetrans->base.id;
				ret = NhlSetValues(tmpid,
					NhlNtrXMinF,xnew->xyplot.x_data_min,
					NhlNtrXMaxF,xnew->xyplot.x_data_max,
					NhlNtrXLog,xlog,
					NhlNtrXReverse, (xnew->xyplot.x_left 
						>xnew->xyplot.x_data_min?1:0) ,
					NhlNtrYMinF,xnew->xyplot.y_data_min,
					NhlNtrYMaxF,xnew->xyplot.y_data_max,
					NhlNtrYLog,ylog,
					NhlNtrYReverse, (xnew->xyplot.y_bottom 
						> xnew->xyplot.y_data_min?1:0)
					,NULL);

				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not set internal tranformation object values, cannot continue",error_lead);
					return(FATAL);
				} else if( ret < ret1 )
					ret1= ret;
			}
			break;
		case IRREGULAR:
			if(!xlog) {
				tmpcoords[0] = xnew->xyplot.x_data_min;
				tmpcoords[2] = xnew->xyplot.x_data_max;
				tmpcoords[1] = (tmpcoords[0]+tmpcoords[2])/2.0;
			} else {
				tmpcoords[0] = xnew->xyplot.x_data_min;
				tmpcoords[2] = xnew->xyplot.x_data_max;
				tmpcoords[1] = (float)pow(10.0,(double)(log10(tmpcoords[0])+log10(tmpcoords[2]))/2.0);
			}
			if((c_or_s == CREATE)||(xold->xyplot.y_irregular_points != xnew->xyplot.y_irregular_points)||(xnew->xyplot.x_data_min < xold->xyplot.x_data_min)||(xnew->xyplot.x_data_max > xold->xyplot.x_data_max)||(xnew->xyplot.y_style != xold->xyplot.y_style)) {
				if(c_or_s == SET) {
					ret = NhlDestroy(xnew->xyplot.thetrans->base.id);
					if(ret <ret1)
						ret1 = ret;
				}
				ret = NhlCreate(&tmpid,buffer,
					irregularType2TransObjLayerClass,
					xnew->base.id,
					NhlNtrXUseLog,xlog,
					NhlNtrXMinF,tmpcoords[0],
					NhlNtrXMaxF,tmpcoords[2],
					NhlNtrXCoordPoints,tmpcoords,
					NhlNtrXNumPoints,3,
					NhlNtrXReverse, (xnew->xyplot.x_left 
						>xnew->xyplot.x_data_min?1:0),
					NhlNtrYMaxF,xnew->xyplot.y_data_max,
					NhlNtrYMinF,xnew->xyplot.y_data_min,
					NhlNtrYCoordPoints,
						xnew->xyplot.y_irregular_points,
					NhlNtrYNumPoints,
					xnew->xyplot.y_num_irregular_points,
					NhlNtrYReverse, ((xnew->xyplot.y_top 
						< xnew->xyplot.y_bottom)?1:0)
					,NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not create internal tranformation object, cannot continue",error_lead);
					return(FATAL);
				} else if( ret < ret1 )
					ret1 = ret;
			} else {
				tmpid = xnew->xyplot.thetrans->base.id;
				ret = NhlSetValues(tmpid,
					NhlNtrXMinF,tmpcoords[0],
					NhlNtrXMaxF,tmpcoords[2],
					NhlNtrYMinF,xnew->xyplot.y_data_min,
					NhlNtrYMaxF,xnew->xyplot.y_data_max,
					NhlNtrXReverse,(xnew->xyplot.x_left
                                                >xnew->xyplot.x_data_min?1:0),
					NhlNtrYReverse,((xnew->xyplot.y_top
                                                < xnew->xyplot.y_bottom)?1:0),
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not set internal tranformation object values, cannot continue",error_lead);
					return(FATAL);
				} else if( ret < ret1 )
					ret1 = ret;
			}
			break;
		case TIME:
		case GEOGRAPHIC:
			NhlPError(FATAL,E_UNKNOWN,"%s:TIME and GEOGRAPHIC styles have not been implemented yet",error_lead);
			return(FATAL);
		default:
			break;
		}
		break;
	case IRREGULAR:
		tmpcoords[0] = xnew->xyplot.y_data_min;
		tmpcoords[2] = xnew->xyplot.y_data_max;
		tmpcoords[1] = (tmpcoords[0] + tmpcoords[2])/2.0;
		switch(xnew->xyplot.y_style){
		case LOG:
			ylog = 1;
			if((xnew->xyplot.y_top<= 0.0)||(xnew->xyplot.y_bottom<=0.0)) {
				NhlPError(FATAL,E_UNKNOWN,"%s: LOG style selected for Y-axis but either top or bottom value  is <= 0.0",error_lead);
				return(FATAL);
			}
/*
* Computes a midpoint between the logs of the values instead of the actual
* values. This combined with the *UseLog resrouces of the irregular objects
* creates an approximation of a log tranformation.
*/
			tmpcoords[0] = tmpcoords[0];
			tmpcoords[2] = tmpcoords[2];
			tmpcoords[1] = (float)pow(10.0,(log10(tmpcoords[0]) + log10(tmpcoords[2]))/2.0);
		case LINEAR:
			if((c_or_s == CREATE)||(xnew->xyplot.x_irregular_points != xold->xyplot.x_irregular_points)||(xnew->xyplot.x_data_min < xold->xyplot.x_data_min)||(xnew->xyplot.x_data_max > xold->xyplot.x_data_max)||(xnew->xyplot.y_data_min < xold->xyplot.y_data_min)||(xnew->xyplot.y_data_max > xold->xyplot.y_data_max)||(xnew->xyplot.x_style != xold->xyplot.y_style)) {
				if(c_or_s == SET) {
					ret = NhlDestroy(xnew->xyplot.thetrans->base.id);
					if(ret < ret1)
						ret1 = ret;
				}
				ret = NhlCreate(&tmpid,buffer,
					irregularType2TransObjLayerClass,
					xnew->base.id,
					NhlNtrXMinF,xnew->xyplot.x_data_min,
					NhlNtrXMaxF,xnew->xyplot.x_data_max,
					NhlNtrXCoordPoints,
						xnew->xyplot.x_irregular_points,
					NhlNtrXNumPoints,
						xnew->xyplot.x_num_irregular_points,
					NhlNtrXReverse, ((xnew->xyplot.x_right 
						< xnew->xyplot.x_left)?1:0) ,
					NhlNtrYUseLog,ylog,
					NhlNtrYMinF,tmpcoords[0],
					NhlNtrYMaxF,tmpcoords[2],
					NhlNtrYCoordPoints,tmpcoords,
					NhlNtrYNumPoints,3,
					NhlNtrYReverse, (xnew->xyplot.y_top
						>xnew->xyplot.y_bottom ?0:1)
					,NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not create internal tranformation object, cannot continue",error_lead);
					return(FATAL);
				} else if( ret < ret1 )
					ret1 = ret;
			} else {
				tmpid = xnew->xyplot.thetrans->base.id;
				ret = NhlSetValues(tmpid,
					NhlNtrXMinF,xnew->xyplot.x_data_min,
					NhlNtrXMaxF,xnew->xyplot.x_data_max,
					NhlNtrYMinF,tmpcoords[0],
					NhlNtrYMaxF,tmpcoords[1],
					NhlNtrXReverse, ((xnew->xyplot.x_right 
						< xnew->xyplot.x_left)?1:0) ,
					NhlNtrYReverse, (xnew->xyplot.y_top
						>xnew->xyplot.y_bottom ?0:1),
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not set internal tranformation object values, cannot continue",error_lead);
					return(FATAL);
				} else if( ret < ret1 )
					ret1 = ret;
			}
			break;
		case IRREGULAR:
			if((c_or_s == CREATE)||(xnew->xyplot.x_irregular_points != xold->xyplot.x_irregular_points)||(xnew->xyplot.x_data_min < xold->xyplot.x_data_min)||(xnew->xyplot.x_data_max > xold->xyplot.x_data_max)||(xnew->xyplot.y_data_min < xold->xyplot.y_data_min)||(xnew->xyplot.y_data_max > xold->xyplot.y_data_max)||(xnew->xyplot.y_irregular_points != xold->xyplot.y_irregular_points)||(xnew->xyplot.y_style != xold->xyplot.y_style)||(xnew->xyplot.x_style != xold->xyplot.x_style)){
				if(c_or_s == SET) {
					ret = NhlDestroy(xnew->xyplot.thetrans->base.id);
					if(ret < ret1)
						ret1 = ret;
				}
				ret = NhlCreate(&tmpid,buffer,
					irregularType2TransObjLayerClass,
					xnew->base.id,
					NhlNtrXMinF,xnew->xyplot.x_data_min,
					NhlNtrXMaxF,xnew->xyplot.x_data_max,
					NhlNtrXCoordPoints,
						xnew->xyplot.x_irregular_points,
					NhlNtrXNumPoints,
						xnew->xyplot.x_num_irregular_points,
					NhlNtrXReverse, ((xnew->xyplot.x_right 
						< xnew->xyplot.x_left)?1:0) ,
					NhlNtrYMinF,xnew->xyplot.y_data_min,
					NhlNtrYMaxF,xnew->xyplot.y_data_max,
					NhlNtrYCoordPoints,
						xnew->xyplot.y_irregular_points,
					NhlNtrYNumPoints,
						xnew->xyplot.y_num_irregular_points,
					NhlNtrYReverse, (xnew->xyplot.y_top
						>xnew->xyplot.y_bottom ?0:1)
					,NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not create internal tranformation object, cannot continue",error_lead);
					return(FATAL);
				} else if( ret < ret1 )
					ret1 = ret;
			} else {
				tmpid = xnew->xyplot.thetrans->base.id;
				ret = NhlSetValues(tmpid,
					NhlNtrXMinF,xnew->xyplot.x_data_min,
					NhlNtrXMaxF,xnew->xyplot.x_data_max,
					NhlNtrYMinF,xnew->xyplot.y_data_min,
					NhlNtrYMaxF,xnew->xyplot.y_data_max,
					NhlNtrXReverse, ((xnew->xyplot.x_right 
						< xnew->xyplot.x_left)?1:0) ,
					NhlNtrYReverse, (xnew->xyplot.y_top
						>xnew->xyplot.y_bottom ?0:1),
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not set internal tranformation object values, cannot continue",error_lead);
					return(FATAL);
				} else if( ret < ret1 )
					ret1 = ret;
			}
			break;
		case TIME:
		case GEOGRAPHIC:
			NhlPError(FATAL,E_UNKNOWN,"%s: TIME and GEOGRAPHIC styles have not been implemented yet",error_lead);
			return(FATAL);
		default:
			break;
		}
		break;
	case GEOGRAPHIC:
	case TIME:
		NhlPError(FATAL,E_UNKNOWN,"%s: TIME and GEOGRAPHIC styles have not been implemented yet",error_lead);
		return(FATAL);
	default:
			break;
	}
	if(tmpid > -1){
		xnew->xyplot.thetrans = _NhlGetLayer(tmpid);
		return(ret1);
	}
	else {
		return(ret1);
	}
}

/*
 * Function:	SetUpTicks
 *
 * Description:	Takes care of setting resources for TickMarks. It is at
 *		this time that the resources, that are blocked by the
 *		_NhlRegisterChildClass call in XyPlotClassInitialize, are set.
 *		_NhlCreateChild is used to create tick marks. _NhlCreateChild
 *		is only called once and SetValues is used at all other times
 *		even when a change of style happens. This function could've
 *		been a lot shorter if there had been an argument list 
 *		interface to Create and SetValues when I built it. The args
 *		list interface would have require only one SetValues and
 *		one Create call. Since only the variable args function
 *		interface was available separate calls for each style
 *		needed to be made. Each call has only slight differences
 *		in resources but require separate calls none the less.
 *		After each create or set a getvalues is done to make sure
 *		that the resources held by the XyPlot object have the same
 *		values as the tick mark child.
 *		
 *		When Alternate axis control is implemented this will be
 *		the function to compute the mapping functions and configure
 *		the tick mark object.
 *
 * In Args:	xnew	new instance record
 *		xold	old instance record if c_or_s == SET
 *		c_or_s  either SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	New objects created or reconfigured.
 */
/*ARGSUSED*/
static NhlErrorTypes SetUpTicks
#if __STDC__
(XyPlotLayer xnew,XyPlotLayer xold,int c_or_s)
#else 
(xnew,xold,c_or_s)
	XyPlotLayer xnew;
	XyPlotLayer xold;
	int	c_or_s;
#endif
{
	char buffer[MAXFNAMELEN];
	int tmpid = -1;
	char *error_lead;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;

	if(c_or_s == CREATE) {
		error_lead = "XyPlotInitialize";
	} else {
		error_lead = "XyPlotSetValues";
	}
/*
* Now deal with creating the children ticks and titles
*/
	tmpid = -1;
	sprintf(buffer,"%s",xnew->base.name);
	strcat(buffer,".Ticks");
	switch(xnew->xyplot.x_alternate) {
	case NONE:
		switch(xnew->xyplot.y_alternate) {
		case NONE:
			if((xnew->xyplot.x_style != IRREGULAR)
			&&(xnew->xyplot.y_style != IRREGULAR))
			{
				if(c_or_s == CREATE) {
				ret = _NhlCreateChild(&tmpid,buffer,tickMarkLayerClass,
					(Layer)xnew,
					NhlNvpXF,xnew->view.x,
					NhlNvpYF,xnew->view.y,
					NhlNvpWidthF,xnew->view.width,
					NhlNvpHeightF,xnew->view.height,
					NhlNtmXBDataLeftF,xnew->xyplot.x_left,
					NhlNtmXBDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataLeftF,xnew->xyplot.x_left,
					NhlNtmYLDataTopF,xnew->xyplot.y_top,
					NhlNtmYLDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYRDataTopF,xnew->xyplot.y_top,
					NhlNtmYRDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYLStyle,xnew->xyplot.y_style, 
					NhlNtmYRStyle,xnew->xyplot.y_style,
					NhlNtmXBStyle,xnew->xyplot.x_style,
					NhlNtmXTStyle,xnew->xyplot.x_style,
					NhlNtmXBMode,xnew->xyplot.tm_x_b_mode,
					NhlNtmXTMode,xnew->xyplot.tm_x_t_mode,
					NhlNtmYLMode,xnew->xyplot.tm_y_l_mode,
					NhlNtmYRMode,xnew->xyplot.tm_y_r_mode,
					NhlNtmXBTickStartF,xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not create internal tick mark object, cannot continue",error_lead);
					return(FATAL);
				} else if(ret < ret1)
					ret1 = ret; 
			
				ret = NhlGetValues(tmpid,
					NhlNtmXBTickStartF,&xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,&xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,&xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,&xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,&xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,&xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,&xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,&xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,&xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,&xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,&xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,&xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,&xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,&xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,&xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,&xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < ret1)
					ret1 = ret;
				} else {
					tmpid = xnew->xyplot.ticks->base.id;
				ret = _NhlSetValuesChild(tmpid,
					(Layer)xnew,
					NhlNvpXF,xnew->view.x,
					NhlNvpYF,xnew->view.y,
					NhlNvpWidthF,xnew->view.width,
					NhlNvpHeightF,xnew->view.height,
					NhlNtmXBDataLeftF,xnew->xyplot.x_left,
					NhlNtmXBDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataLeftF,xnew->xyplot.x_left,
					NhlNtmYLDataTopF,xnew->xyplot.y_top,
					NhlNtmYLDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYRDataTopF,xnew->xyplot.y_top,
					NhlNtmYRDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYLStyle,xnew->xyplot.y_style, 
					NhlNtmYRStyle,xnew->xyplot.y_style,
					NhlNtmXBStyle,xnew->xyplot.x_style,
					NhlNtmXTStyle,xnew->xyplot.x_style,
					NhlNtmXBMode,xnew->xyplot.tm_x_b_mode,
					NhlNtmXTMode,xnew->xyplot.tm_x_t_mode,
					NhlNtmYLMode,xnew->xyplot.tm_y_l_mode,
					NhlNtmYRMode,xnew->xyplot.tm_y_r_mode,
					NhlNtmXBTickStartF,xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,xnew->xyplot.tm_y_l_spacing_type,
					NULL);

				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not set internal tick mark object values, cannot continue",error_lead);
					return(FATAL);
				} else if(ret < ret1)
					ret1 = ret; 

				ret = NhlGetValues(tmpid,
					NhlNtmXBTickStartF,&xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,&xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,&xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,&xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,&xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,&xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,&xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,&xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,&xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,&xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,&xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,&xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,&xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,&xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,&xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,&xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < ret1)
					ret1 = ret;
				}
			} else if((xnew->xyplot.x_style == IRREGULAR)
				&&(xnew->xyplot.y_style != IRREGULAR)) {
				if(c_or_s == CREATE) {
				ret = _NhlCreateChild(&tmpid,buffer,tickMarkLayerClass,
					(Layer)xnew,
					NhlNvpXF,xnew->view.x,
					NhlNvpYF,xnew->view.y,
					NhlNvpWidthF,xnew->view.width,
					NhlNvpHeightF,xnew->view.height,
					NhlNtmXBDataLeftF,xnew->xyplot.x_left,
					NhlNtmXBDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataLeftF,xnew->xyplot.x_left,
					NhlNtmYLDataTopF,xnew->xyplot.y_top,
					NhlNtmYLDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYRDataTopF,xnew->xyplot.y_top,
					NhlNtmYRDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYLStyle,xnew->xyplot.y_style, 
					NhlNtmYRStyle,xnew->xyplot.y_style,
					NhlNtmXBStyle,IRREGULAR,
					NhlNtmXTStyle,IRREGULAR,
					NhlNtmXBIrregularPoints,
						xnew->xyplot.x_irregular_points,
					NhlNtmXTIrregularPoints,
						xnew->xyplot.x_irregular_points,
					NhlNtmXBNumIrregularPoints,
						xnew->xyplot.x_num_irregular_points,
					NhlNtmXTNumIrregularPoints,
						xnew->xyplot.x_num_irregular_points,
					NhlNtmXBMode,xnew->xyplot.tm_x_b_mode,
					NhlNtmXTMode,xnew->xyplot.tm_x_t_mode,
					NhlNtmYLMode,xnew->xyplot.tm_y_l_mode,
					NhlNtmYRMode,xnew->xyplot.tm_y_r_mode,
					NhlNtmXBTickStartF,xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not create internal tick mark object, cannot continue",error_lead);
					return(FATAL);
				} else if(ret < ret1)
					ret1 = ret; 

				ret = NhlGetValues(tmpid,
					NhlNtmXBTickStartF,&xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,&xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,&xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,&xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,&xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,&xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,&xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,&xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,&xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,&xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,&xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,&xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,&xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,&xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,&xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,&xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < ret1)
					ret1 = ret; 
				} else {
					tmpid = xnew->xyplot.ticks->base.id;
				ret = _NhlSetValuesChild(tmpid,
					(Layer)xnew,
					NhlNvpXF,xnew->view.x,
					NhlNvpYF,xnew->view.y,
					NhlNvpWidthF,xnew->view.width,
					NhlNvpHeightF,xnew->view.height,
					NhlNtmXBDataLeftF,xnew->xyplot.x_left,
					NhlNtmXBDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataLeftF,xnew->xyplot.x_left,
					NhlNtmYLDataTopF,xnew->xyplot.y_top,
					NhlNtmYLDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYRDataTopF,xnew->xyplot.y_top,
					NhlNtmYRDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYLStyle,xnew->xyplot.y_style, 
					NhlNtmYRStyle,xnew->xyplot.y_style,
					NhlNtmXBStyle,IRREGULAR,
					NhlNtmXTStyle,IRREGULAR,
					NhlNtmXBIrregularPoints,
						xnew->xyplot.x_irregular_points,
					NhlNtmXTIrregularPoints,
						xnew->xyplot.x_irregular_points,
					NhlNtmXBNumIrregularPoints,
						xnew->xyplot.x_num_irregular_points,
					NhlNtmXTNumIrregularPoints,
						xnew->xyplot.x_num_irregular_points,
					NhlNtmXBMode,xnew->xyplot.tm_x_b_mode,
					NhlNtmXTMode,xnew->xyplot.tm_x_t_mode,
					NhlNtmYLMode,xnew->xyplot.tm_y_l_mode,
					NhlNtmYRMode,xnew->xyplot.tm_y_r_mode,
					NhlNtmXBTickStartF,xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not set internal tick mark object values, cannot continue",error_lead);
					return(FATAL);
				} else if(ret < ret1)
					ret1 = ret; 

				ret = NhlGetValues(tmpid,
					NhlNtmXBTickStartF,&xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,&xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,&xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,&xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,&xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,&xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,&xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,&xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,&xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,&xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,&xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,&xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,&xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,&xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,&xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,&xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < ret1)
					ret1 = ret; 
				}
			} else if((xnew->xyplot.y_style == IRREGULAR)
				&&(xnew->xyplot.x_style != IRREGULAR)) {
				if(c_or_s == CREATE) {
				ret = _NhlCreateChild(&tmpid,buffer,tickMarkLayerClass,
					(Layer)xnew,
					NhlNvpXF,xnew->view.x,
					NhlNvpYF,xnew->view.y,
					NhlNvpWidthF,xnew->view.width,
					NhlNvpHeightF,xnew->view.height,
					NhlNtmXBDataLeftF,xnew->xyplot.x_left,
					NhlNtmXBDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataLeftF,xnew->xyplot.x_left,
					NhlNtmYLDataTopF,xnew->xyplot.y_top,
					NhlNtmYLDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYRDataTopF,xnew->xyplot.y_top,
					NhlNtmYRDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYLStyle,IRREGULAR, 
					NhlNtmYRStyle,IRREGULAR,
					NhlNtmXBStyle,xnew->xyplot.x_style,
					NhlNtmXTStyle,xnew->xyplot.x_style,
					NhlNtmYLNumIrregularPoints,
						xnew->xyplot.y_num_irregular_points,
					NhlNtmYRNumIrregularPoints,
						xnew->xyplot.y_num_irregular_points,
					NhlNtmYLIrregularPoints,
						xnew->xyplot.y_irregular_points,
					NhlNtmYRIrregularPoints,
						xnew->xyplot.y_irregular_points,
					NhlNtmXBMode,xnew->xyplot.tm_x_b_mode,
					NhlNtmXTMode,xnew->xyplot.tm_x_t_mode,
					NhlNtmYLMode,xnew->xyplot.tm_y_l_mode,
					NhlNtmYRMode,xnew->xyplot.tm_y_r_mode,
					NhlNtmXBTickStartF,xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not create internal tick mark object, cannot continue",error_lead);
					return(FATAL);
				} else if(ret < ret1)
					ret1 = ret; 

				ret = NhlGetValues(tmpid,
					NhlNtmXBTickStartF,&xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,&xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,&xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,&xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,&xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,&xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,&xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,&xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,&xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,&xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,&xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,&xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,&xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,&xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,&xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,&xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < ret1)
					ret1 = ret; 
				} else {
					tmpid = xnew->xyplot.ticks->base.id;
				ret = _NhlSetValuesChild(tmpid,
					(Layer)xnew,
					NhlNvpXF,xnew->view.x,
					NhlNvpYF,xnew->view.y,
					NhlNvpWidthF,xnew->view.width,
					NhlNvpHeightF,xnew->view.height,
					NhlNtmXBDataLeftF,xnew->xyplot.x_left,
					NhlNtmXBDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataLeftF,xnew->xyplot.x_left,
					NhlNtmYLDataTopF,xnew->xyplot.y_top,
					NhlNtmYLDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYRDataTopF,xnew->xyplot.y_top,
					NhlNtmYRDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYLStyle,IRREGULAR, 
					NhlNtmYRStyle,IRREGULAR,
					NhlNtmXBStyle,xnew->xyplot.x_style,
					NhlNtmXTStyle,xnew->xyplot.x_style,
					NhlNtmYLNumIrregularPoints,
						xnew->xyplot.y_num_irregular_points,
					NhlNtmYRNumIrregularPoints,
						xnew->xyplot.y_num_irregular_points,
					NhlNtmYLIrregularPoints,
						xnew->xyplot.y_irregular_points,
					NhlNtmYRIrregularPoints,
						xnew->xyplot.y_irregular_points,
					NhlNtmXBMode,xnew->xyplot.tm_x_b_mode,
					NhlNtmXTMode,xnew->xyplot.tm_x_t_mode,
					NhlNtmYLMode,xnew->xyplot.tm_y_l_mode,
					NhlNtmYRMode,xnew->xyplot.tm_y_r_mode,
					NhlNtmXBTickStartF,xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not set internal tick mark object values, cannot continue",error_lead);
					return(FATAL);
				} else if(ret < ret1)
					ret1 = ret; 

				ret = NhlGetValues(tmpid,
					NhlNtmXBTickStartF,&xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,&xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,&xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,&xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,&xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,&xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,&xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,&xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,&xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,&xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,&xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,&xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,&xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,&xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,&xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,&xnew->xyplot.tm_y_l_spacing_type,
					NULL);
					if( ret < ret1 )
						ret1 = ret;
				}
			} else {
				if(c_or_s == CREATE) {
				ret = _NhlCreateChild(&tmpid,buffer,tickMarkLayerClass,
					(Layer)xnew,
					NhlNvpXF,xnew->view.x,
					NhlNvpYF,xnew->view.y,
					NhlNvpWidthF,xnew->view.width,
					NhlNvpHeightF,xnew->view.height,
					NhlNtmXBDataLeftF,xnew->xyplot.x_left,
					NhlNtmXBDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataLeftF,xnew->xyplot.x_left,
					NhlNtmYLDataTopF,xnew->xyplot.y_top,
					NhlNtmYLDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYRDataTopF,xnew->xyplot.y_top,
					NhlNtmYRDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmXBIrregularPoints,xnew->xyplot.x_irregular_points,
					NhlNtmXTIrregularPoints,xnew->xyplot.x_irregular_points,
					NhlNtmXBNumIrregularPoints,xnew->xyplot.x_num_irregular_points,
					NhlNtmXTNumIrregularPoints,xnew->xyplot.x_num_irregular_points,
					NhlNtmYLIrregularPoints,xnew->xyplot.y_irregular_points,
					NhlNtmYRIrregularPoints,xnew->xyplot.y_irregular_points,
					NhlNtmYRNumIrregularPoints,xnew->xyplot.y_num_irregular_points,
					NhlNtmYLNumIrregularPoints,xnew->xyplot.y_num_irregular_points,
					NhlNtmYLStyle,IRREGULAR, 
					NhlNtmYRStyle,IRREGULAR,
					NhlNtmXBStyle,IRREGULAR,
					NhlNtmXTStyle,IRREGULAR,
					NhlNtmXBMode,xnew->xyplot.tm_x_b_mode,
					NhlNtmXTMode,xnew->xyplot.tm_x_t_mode,
					NhlNtmYLMode,xnew->xyplot.tm_y_l_mode,
					NhlNtmYRMode,xnew->xyplot.tm_y_r_mode,
					NhlNtmXBTickStartF,xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not create internal tick mark object, cannot continue",error_lead);
					return(FATAL);
				} else if(ret < ret1)
					ret1 = ret; 

				ret = NhlGetValues(tmpid,
					NhlNtmXBTickStartF,&xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,&xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,&xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,&xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,&xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,&xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,&xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,&xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,&xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,&xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,&xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,&xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,&xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,&xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,&xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,&xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				} else {
					tmpid = xnew->xyplot.ticks->base.id;
				ret =_NhlSetValuesChild(tmpid,
					(Layer)xnew,
					NhlNvpXF,xnew->view.x,
					NhlNvpYF,xnew->view.y,
					NhlNvpWidthF,xnew->view.width,
					NhlNvpHeightF,xnew->view.height,
					NhlNtmXBDataLeftF,xnew->xyplot.x_left,
					NhlNtmXBDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataRightF,xnew->xyplot.x_right,
					NhlNtmXTDataLeftF,xnew->xyplot.x_left,
					NhlNtmYLDataTopF,xnew->xyplot.y_top,
					NhlNtmYLDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmYRDataTopF,xnew->xyplot.y_top,
					NhlNtmYRDataBottomF,xnew->xyplot.y_bottom,
					NhlNtmXBIrregularPoints,xnew->xyplot.x_irregular_points,
					NhlNtmXTIrregularPoints,xnew->xyplot.x_irregular_points,
					NhlNtmXBNumIrregularPoints,xnew->xyplot.x_num_irregular_points,
					NhlNtmXTNumIrregularPoints,xnew->xyplot.x_num_irregular_points,
					NhlNtmYLIrregularPoints,xnew->xyplot.y_irregular_points,
					NhlNtmYRIrregularPoints,xnew->xyplot.y_irregular_points,
					NhlNtmYRNumIrregularPoints,xnew->xyplot.y_num_irregular_points,
					NhlNtmYLNumIrregularPoints,xnew->xyplot.y_num_irregular_points,
					NhlNtmYLStyle,IRREGULAR, 
					NhlNtmYRStyle,IRREGULAR,
					NhlNtmXBStyle,IRREGULAR,
					NhlNtmXTStyle,IRREGULAR,
					NhlNtmXBMode,xnew->xyplot.tm_x_b_mode,
					NhlNtmXTMode,xnew->xyplot.tm_x_t_mode,
					NhlNtmYLMode,xnew->xyplot.tm_y_l_mode,
					NhlNtmYRMode,xnew->xyplot.tm_y_r_mode,
					NhlNtmXBTickStartF,xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < WARNING) {
					NhlPError(FATAL,E_UNKNOWN,"%s: Could not set internal tick mark object values, cannot continue",error_lead);
					return(FATAL);
				} else if(ret < ret1)
					ret1 = ret; 

				ret = NhlGetValues(tmpid,
					NhlNtmXBTickStartF,&xnew->xyplot.tm_x_b_tick_start,
					NhlNtmXBTickEndF,&xnew->xyplot.tm_x_b_tick_end,
					NhlNtmXBTickSpacingF,&xnew->xyplot.tm_x_b_tick_spacing,
					NhlNtmXBSpacingType,&xnew->xyplot.tm_x_b_spacing_type,
					NhlNtmXTTickStartF,&xnew->xyplot.tm_x_t_tick_start,
					NhlNtmXTTickEndF,&xnew->xyplot.tm_x_t_tick_end,
					NhlNtmXTTickSpacingF,&xnew->xyplot.tm_x_t_tick_spacing,
					NhlNtmXTSpacingType,&xnew->xyplot.tm_x_t_spacing_type,
					NhlNtmYRTickStartF,&xnew->xyplot.tm_y_r_tick_start,
					NhlNtmYRTickEndF,&xnew->xyplot.tm_y_r_tick_end,
					NhlNtmYRTickSpacingF,&xnew->xyplot.tm_y_r_tick_spacing,
					NhlNtmYRSpacingType,&xnew->xyplot.tm_y_r_spacing_type,
					NhlNtmYLTickStartF,&xnew->xyplot.tm_y_l_tick_start,
					NhlNtmYLTickEndF,&xnew->xyplot.tm_y_l_tick_end,
					NhlNtmYLTickSpacingF,&xnew->xyplot.tm_y_l_tick_spacing,
					NhlNtmYLSpacingType,&xnew->xyplot.tm_y_l_spacing_type,
					NULL);
				if(ret < ret1)
					ret1 = ret; 
				}
			}
			break;
		case TOPAXIS:
		case BOTTOMAXIS:
		default:
			NhlPError(FATAL,E_UNKNOWN,"%s: Alternate axis' are not implemented yet",error_lead);
			return(FATAL);
		}
		break;
	case LEFTAXIS:
			NhlPError(FATAL,E_UNKNOWN,"%s: Alternate axis' are not implemented yet",error_lead);
			return(FATAL);
/*
			switch(xnew->xyplot.y_alternate) {
                	case NONE:
                	case TOPAXIS:
                	case BOTTOMAXIS:
                	default:
                        	break;
                	}
*/
	case RIGHTAXIS:
			NhlPError(FATAL,E_UNKNOWN,"%s: Alternate axis' are not implemented yet",error_lead);
			return(FATAL);
		/*
			switch(xnew->xyplot.y_alternate) {
                	case NONE:
                	case TOPAXIS:
                	case BOTTOMAXIS:
                	default:
                       		 break;
               		}
		*/
	}
	if(tmpid > -1) {
		xnew->xyplot.ticks = _NhlGetLayer(tmpid);
		return(ret1);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"%s: Couldn't create TickMarks",error_lead);
		return(FATAL);
	}
}

/*
 * Function:	SetUpTitles
 *
 * Description: Sets and Creates Title object. _NhlCreateChild is used to
 *		create the titles.  The title resources *OffsetXF and *OffsetYF
 *		are intercepted by the XyPlot object so adjustments can be
 *		made to make sure that the titles are centered over the
 *		XyPlot's viewport but do not overlap with tick mark labels.
 *		The adjusted values are added to any user supplied values
 *		to obtain the correct location of the title. The resources
 *		for configuring which side of the plot a title goes and in
 *		which position it goes are also intercepted so that the
 *		offset values can be computed.
 *
 *
 * In Args:	xnew 	new instance record
 *		xold	old instance record
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects: Objects created or states changed. 	
 */
/*ARGSUSED*/
static NhlErrorTypes SetUpTitles
#if  __STDC__
(XyPlotLayer xnew, XyPlotLayer xold,int c_or_s) 
#else 
(xnew,xold,c_or_s)
	XyPlotLayer xnew;
	XyPlotLayer xold;
	int	c_or_s;
#endif
{
	int tmpid;
	NhlBoundingBox abox;
	char buffer[MAXFNAMELEN];
	float xtmp,ytmp,widthtmp,heighttmp;
	char *error_lead;
	NhlErrorTypes ret = NOERROR;

	tmpid = -1;
	NhlGetBB(xnew->xyplot.ticks->base.id,&abox);
	xtmp = abox.l;
	ytmp = abox.t;
	widthtmp = abox.r - abox.l;
	heighttmp = abox.t - abox.b;
	
	if(c_or_s == CREATE) {
		error_lead = "XyPlotInitialize";
	} else {
		error_lead = "XyPlotSetValues";
	}

	switch(xnew->xyplot.ti_main_position) {
	case CENTER:
		xnew->xyplot.real_main_offset_x = xnew->xyplot.ti_main_offset_x
			+ ((xnew->view.x + xnew->view.width/2.0)
			- (xtmp + widthtmp/2.0));
		break;
	case LEFT:
		xnew->xyplot.real_main_offset_x = xnew->xyplot.ti_main_offset_x 
			+ (xnew->view.x - xtmp);
		break;
	case RIGHT:
		xnew->xyplot.real_main_offset_x = xnew->xyplot.ti_main_offset_x
			+ ((xnew->view.x + xnew->view.width) 
			- (xtmp + widthtmp));
		break;
	}
	switch(xnew->xyplot.ti_x_axis_position) {
	case CENTER:
		xnew->xyplot.real_x_axis_offset_x = 
			xnew->xyplot.ti_x_axis_offset_x 
			+ ((xnew->view.x + xnew->view.width/2.0)
			- (xtmp + widthtmp/2.0));
		break;
	case LEFT:
		xnew->xyplot.real_x_axis_offset_x = 
			xnew->xyplot.ti_x_axis_offset_x 
			+ (xnew->view.x - xtmp);
		break;
	case RIGHT:
		xnew->xyplot.real_x_axis_offset_x = 
			xnew->xyplot.ti_x_axis_offset_x 
			+ ((xnew->view.x + xnew->view.width) 
			- (xtmp + widthtmp));
		break;
	}
	switch(xnew->xyplot.ti_y_axis_position) {
	case CENTER:
		xnew->xyplot.real_y_axis_offset_y = 
			xnew->xyplot.ti_y_axis_offset_y 
			+ ((xnew->view.y - xnew->view.height/2.0)
			- (ytmp - heighttmp/2.0));
		break;
	case TOP:
		xnew->xyplot.real_y_axis_offset_y = 
			xnew->xyplot.ti_y_axis_offset_y 
			+ (xnew->view.y - ytmp);
		break;
	case BOTTOM:
		xnew->xyplot.real_y_axis_offset_y = 
			xnew->xyplot.ti_y_axis_offset_y 
			+ ((xnew->view.y - xnew->view.height) 
			- (ytmp - heighttmp));
		break;
	}


	if(c_or_s == CREATE) {	
		ret = _NhlCreateChild(&tmpid,buffer,titleLayerClass,(Layer)xnew,
			NhlNvpXF,xtmp,
			NhlNvpYF,ytmp,
			NhlNvpWidthF,widthtmp,
			NhlNvpHeightF,heighttmp,
			NhlNtiMainOffsetXF,xnew->xyplot.real_main_offset_x,
			NhlNtiYAxisOffsetYF,xnew->xyplot.real_y_axis_offset_y,
			NhlNtiXAxisOffsetXF,xnew->xyplot.real_x_axis_offset_x,
			NhlNtiXAxisPosition,xnew->xyplot.ti_x_axis_position,
			NhlNtiYAxisPosition,xnew->xyplot.ti_y_axis_position,
			NhlNtiMainPosition,xnew->xyplot.ti_main_position,
			NhlNtiMainString,xnew->xyplot.ti_main_string,
			NhlNtiXAxisString,xnew->xyplot.ti_x_axis_string,
			NhlNtiYAxisString,xnew->xyplot.ti_y_axis_string,
			NhlNtiMainOn,(xnew->xyplot.titles ? xnew->xyplot.ti_main_on: 0),
			NhlNtiXAxisOn,(xnew->xyplot.titles ? xnew->xyplot.ti_x_axis_on: 0),
			NhlNtiYAxisOn,(xnew->xyplot.titles ? xnew->xyplot.ti_y_axis_on: 0),
			NULL);
	} else {
		tmpid = xnew->xyplot.ttitles->base.id;
		ret = _NhlSetValuesChild(tmpid,
			(Layer)xnew,
			NhlNvpXF,xtmp,
			NhlNvpYF,ytmp,
			NhlNvpWidthF,widthtmp,
			NhlNvpHeightF,heighttmp,
			NhlNtiMainOffsetXF,xnew->xyplot.real_main_offset_x,
			NhlNtiYAxisOffsetYF,xnew->xyplot.real_y_axis_offset_y,
			NhlNtiXAxisOffsetXF,xnew->xyplot.real_x_axis_offset_x,
			NhlNtiXAxisPosition,xnew->xyplot.ti_x_axis_position,
			NhlNtiYAxisPosition,xnew->xyplot.ti_y_axis_position,
			NhlNtiMainPosition,xnew->xyplot.ti_main_position,
			NhlNtiMainString,xnew->xyplot.ti_main_string,
			NhlNtiXAxisString,xnew->xyplot.ti_x_axis_string,
			NhlNtiYAxisString,xnew->xyplot.ti_y_axis_string,
			NhlNtiMainOn,(xnew->xyplot.titles ? xnew->xyplot.ti_main_on: 0),
			NhlNtiXAxisOn,(xnew->xyplot.titles ? xnew->xyplot.ti_x_axis_on: 0),
			NhlNtiYAxisOn,(xnew->xyplot.titles ? xnew->xyplot.ti_y_axis_on: 0),
			NULL);
	}
	if((tmpid > -1)||(ret >= WARNING)) {
		xnew->xyplot.ttitles = _NhlGetLayer(tmpid);
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"%s: Could not create Titles",error_lead);
		return(FATAL);
	} 
}



static NhlErrorTypes ScaleForMove
#if __STDC__
(XyPlotLayer xnew, XyPlotLayer xold, _NhlArgList args, int num_args, int c_or_s)
#else 
(xnew,xold,c_or_s)
	XyPlotLayer xnew;
	XyPlotLayer xold;
	_NhlArgList args;
	int num_args;
	int	c_or_s;
#endif
{
	float deltax,deltay;
	if(c_or_s == CREATE) {
		deltax = xnew->view.width/NHL_DEFAULT_VIEW_WIDTH;
		deltay = xnew->view.height/NHL_DEFAULT_VIEW_HEIGHT;
	} else {
		deltax = xnew->view.width/xold->view.width;
		deltay = xnew->view.height/xold->view.height;
	}

	if((_NhlArgIsSet(args,num_args,NhlNvpHeightF))&&(!_NhlArgIsSet(args,num_args,NhlNxyLineLabelFontHeightF))) {
			xnew->xyplot.line_label_font_height= 
				deltay *
				xnew->xyplot.line_label_font_height;
	}
	if((_NhlArgIsSet(args,num_args,NhlNvpWidthF))&&(!_NhlArgIsSet(args,num_args,NhlNxyDashSegmentLengthF))) {
			xnew->xyplot.dash_segment_length = 
				deltax *
				xnew->xyplot.dash_segment_length;
	}
	return(NOERROR);
}
