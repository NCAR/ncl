/*
 *      $Id: Contour.c,v 1.1 1993-11-20 01:05:46 dbrown Exp $
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
 *	Description:	Source for Contour hlu.
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/ContourP.h>
#include <ncarg/hlu/Converters.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/IrregularType2TransObj.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/CoordArrTableFloatP.h>

typedef enum _CallType{
	DATACHANGE,
	CREATE,
	SET
} _NhlCallType;

/*
 * Resource Default Functions.
 */
/*
 * Functions:	[Comp,Set][XMin,XMax,Top,Bottom]
 *
 * Description:	These functions are used so the Contour object can tell if the
 *		user set the left,right,top,and bottom resources or if it
 *		should use the min/max data values.
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
CompXMin
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
	ContourLayer	contour = (ContourLayer)base;

	contour->contour.comp_x_min_set = False;
	contour->contour.compute_x_min = False;

	return NOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
SetXMin
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
	ContourLayer	contour = (ContourLayer)base;

	contour->contour.x_min_set = False;
	contour->contour.x_min = 1.0;

	return NOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
CompXMax
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
	ContourLayer	contour = (ContourLayer)base;

	contour->contour.comp_x_max_set = False;
	contour->contour.compute_x_max = False;

	return NOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
SetXMax
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
	ContourLayer	contour = (ContourLayer)base;

	contour->contour.x_max_set = False;
	contour->contour.x_max = 2.0;

	return NOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
CompYMax
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
	ContourLayer	contour = (ContourLayer)base;

	contour->contour.comp_y_max_set = False;
	contour->contour.compute_y_max = False;

	return NOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
SetYMax
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
	ContourLayer	contour = (ContourLayer)base;

	contour->contour.y_max_set = False;
	contour->contour.y_max = 2.0;

	return NOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
CompYMin
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
	ContourLayer	contour = (ContourLayer)base;

	contour->contour.comp_y_min_set = False;
	contour->contour.compute_y_min = False;

	return NOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
SetYMin
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
	ContourLayer	contour = (ContourLayer)base;

	contour->contour.y_min_set = False;
	contour->contour.y_min = 1.0;

	return NOERROR;
}

#define	Oset(field)	NhlOffset(CnDataDepLayerRec,cndata.field)
static NhlResource data_resources[] = {
	{NhlNcnColors,NhlCcnColors,NhlTGenArray,sizeof(NhlPointer),
		Oset(colors),NhlTImmediate,(NhlPointer)NULL},
	{NhlNcnColor,NhlCcnColor,NhlTInteger,sizeof(int),
		Oset(color),NhlTImmediate,(NhlPointer)1},

	{NhlNcnDashPatterns,NhlCcnDashPatterns,NhlTGenArray,sizeof(NhlPointer),
		Oset(dash_patterns),NhlTImmediate,(NhlPointer)NULL},
	{NhlNcnDashPattern,NhlCcnDashPattern,NhlTInteger,sizeof(int),
		Oset(dash),NhlTImmediate,(NhlPointer)1},

	{NhlNcnLabelMode,NhlCcnLabelMode,NhlTLineLabelModes,
		sizeof(LineLabelModes),
		Oset(label_mode),NhlTImmediate,(NhlPointer)NOLABELS},
	{NhlNcnExplicitLabels,NhlCcnExplicitLabels,NhlTGenArray,
		sizeof(NhlPointer),
		Oset(labels),NhlTImmediate,(NhlPointer)NULL}
};
#undef Oset

#define	Oset(field)	NhlOffset(ContourLayerRec,contour.field)
static NhlResource resources[] = {
	{NhlNcnCurveData,NhlCcnCurveData,_NhlTDataList,sizeof(NhlGenArray),
		Oset(curve_data),NhlTImmediate,NULL},

	{NhlNcnCurveThicknessF,NhlCcnCurveThicknessF,NhlTFloat,sizeof(float),
		Oset(curve_thickness),NhlTString,"1.0"},

	{NhlNcnXStyle,NhlCcnXStyle,NhlTTickMarkStyles,sizeof(TickMarkStyles),
		Oset(x_style),NhlTImmediate,(NhlPointer)LINEAR},
	{NhlNcnXIrrTensionF,NhlCcnXIrrTensionF,NhlTFloat,sizeof(float),
		Oset(x_tension),NhlTString,"2.0"},
	{NhlNcnYStyle,NhlCcnYStyle,NhlTTickMarkStyles,sizeof(TickMarkStyles),
		Oset(y_style),NhlTImmediate,(NhlPointer)LINEAR},
	{NhlNcnYIrrTensionF,NhlCcnYIrrTensionF,NhlTFloat,sizeof(float),
		Oset(y_tension),NhlTString,"2.0"},

	{NhlNcnXIrregularPoints,NhlCcnXIrregularPoints,NhlTGenArray,
		sizeof(NhlPointer),
		Oset(x_irregular_points),NhlTImmediate,(NhlPointer)NULL},
	{NhlNcnYIrregularPoints,NhlCcnYIrregularPoints,NhlTGenArray,
		sizeof(NhlPointer),
		Oset(y_irregular_points),NhlTImmediate,(NhlPointer)NULL},

	{NhlNcnXReverse,NhlCcnXReverse,NhlTBoolean,sizeof(NhlBoolean),
		Oset(x_reverse),NhlTImmediate,False},
	{NhlNcnYReverse,NhlCcnYReverse,NhlTBoolean,sizeof(NhlBoolean),
		Oset(y_reverse),NhlTImmediate,False},

	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(comp_x_min_set),NhlTImmediate,(NhlPointer)True},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(comp_x_max_set),NhlTImmediate,(NhlPointer)True},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(comp_y_max_set),NhlTImmediate,(NhlPointer)True},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(comp_y_min_set),NhlTImmediate,(NhlPointer)True},

	{NhlNcnComputeXMin,NhlCcnComputeXMin,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_x_min),NhlTProcedure,(NhlPointer)CompXMin},
	{NhlNcnComputeXMax,NhlCcnComputeXMax,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_x_max),NhlTProcedure,(NhlPointer)CompXMax},
	{NhlNcnComputeYMax,NhlCcnComputeYMax,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_y_max),NhlTProcedure,(NhlPointer)CompYMax},
	{NhlNcnComputeYMin,NhlCcnComputeYMin,NhlTBoolean,sizeof(NhlBoolean),
		Oset(compute_y_min),NhlTProcedure,(NhlPointer)CompYMin},

	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(x_min_set),NhlTImmediate,(NhlPointer)True},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(x_max_set),NhlTImmediate,(NhlPointer)True},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(y_max_set),NhlTImmediate,(NhlPointer)True},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(y_min_set),NhlTImmediate,(NhlPointer)True},

	{NhlNcnXMinF,NhlCcnXMinF,NhlTFloat,sizeof(float),
		Oset(x_min),NhlTProcedure,(NhlPointer)SetXMin},
	{NhlNcnXMaxF,NhlCcnXMaxF,NhlTFloat,sizeof(float),
		Oset(x_max),NhlTProcedure,(NhlPointer)SetXMax},
	{NhlNcnYMaxF,NhlCcnYMaxF,NhlTFloat,sizeof(float),
		Oset(y_max),NhlTProcedure,(NhlPointer)SetYMax},
	{NhlNcnYMinF,NhlCcnYMinF,NhlTFloat,sizeof(float),
		Oset(y_min),NhlTProcedure,(NhlPointer)SetYMin},

	{NhlNcnTitles,NhlCcnTitles,NhlTBoolean,sizeof(NhlBoolean),
		Oset(titles),NhlTImmediate,(NhlPointer)True},
	{NhlNcnXAlternate,NhlCcnXAlternate,NhlTAlternatePlace,
		sizeof(AlternatePlace),
		Oset(x_alternate),NhlTImmediate,(NhlPointer)NONE},
	{NhlNcnYAlternate,NhlCcnYAlternate,NhlTAlternatePlace,
		sizeof(AlternatePlace),
		Oset(y_alternate),NhlTImmediate,(NhlPointer)NONE},
	{NhlNcnYAlternateCoords,NhlCcnYAlternateCoords,NhlTGenArray,
		sizeof(NhlPointer),Oset(y_alternate_coords),NhlTImmediate,NULL},
	{NhlNcnXAlternateCoords,NhlCcnXAlternateCoords,NhlTGenArray,
		sizeof(NhlPointer),Oset(x_alternate_coords),NhlTImmediate,NULL},
	{NhlNcnXOriginalCoords,NhlCcnXOriginalCoords,NhlTGenArray,
		sizeof(NhlPointer),Oset(x_original_coords),NhlTImmediate,NULL},
	{NhlNcnYOriginalCoords,NhlCcnYOriginalCoords,NhlTGenArray,
		sizeof(NhlPointer),Oset(y_original_coords),NhlTImmediate,NULL},
	{NhlNcnDashSegmentLengthF,NhlCcnDashSegmentLengthF,NhlTFloat,
		sizeof(float),Oset(dash_segment_length),NhlTString,".15"},
	{NhlNcnLineLabelFontHeightF,NhlCcnLineLabelFontHeightF,NhlTFloat,
		sizeof(float),
		Oset(line_label_font_height),NhlTString,".01"},

/*
* Title resources of special importance are intercepted here
*/
	{NhlNtiMainOffsetXF,NhlCtiMainOffsetYF,NhlTFloat,sizeof(float),
		Oset(ti_main_offset_x),NhlTString,"0.0"},
	{NhlNtiXAxisOffsetXF,NhlCtiXAxisOffsetYF,NhlTFloat,sizeof(float),
		Oset(ti_x_axis_offset_x),NhlTString,"0.0"},
	{NhlNtiYAxisOffsetYF,NhlCtiXAxisOffsetYF,NhlTFloat,sizeof(float),
		Oset(ti_y_axis_offset_y),NhlTString,"0.0"},
	{NhlNtiXAxisPosition,NhlCtiXAxisPosition,NhlTTitlePositions,
		sizeof(TitlePositions),
		Oset(ti_x_axis_position),NhlTImmediate,(NhlPointer)CENTER},
	{NhlNtiYAxisPosition,NhlCtiYAxisPosition,NhlTTitlePositions,
		sizeof(TitlePositions),
		Oset(ti_y_axis_position),NhlTImmediate,(NhlPointer)CENTER},
	{NhlNtiMainPosition,NhlCtiMainPosition,NhlTTitlePositions,
		sizeof(TitlePositions),
		Oset(ti_main_position),NhlTImmediate,(NhlPointer)CENTER},
};
#undef Oset


/* base methods */

static NhlErrorTypes ContourSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes CnDataInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes ContourInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes ContourClassPartInitialize(
#ifdef NhlNeedProto
	LayerClass	lc
#endif
);

static NhlErrorTypes CnDataClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes ContourClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes ContourDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes ContourDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

/* 
* View Methods
*/

static NhlErrorTypes ContourGetBB(
#ifdef NhlNeedProto
        Layer          /* instance */,
        NhlBoundingBox * /*thebox*/
#endif
);

/*
* Transform Methods
*/

static NhlErrorTypes ContourDataToNDC(
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

static NhlErrorTypes ContourNDCToData(
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

static NhlErrorTypes ContourUpdateData(
#ifdef NhlNeedProto
	DataCommLayer	new,
	DataCommLayer	old
#endif
);

static NhlErrorTypes CheckValues(
#ifdef	NhlNeedProto
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
#endif
);

static NhlErrorTypes InternalizePointers(
#ifdef NhlNeedProto
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
#endif
);

static NhlErrorTypes ComputeDataExtents(
#ifdef NhlNeedProto
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
#endif
);

static NhlErrorTypes SetUpTransObjs(
#ifdef NhlNeedProto
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
#endif
);

static NhlErrorTypes SetUpTicks(
#ifdef NhlNeedProto
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
#endif
);
static NhlErrorTypes SetUpTitles(
#ifdef NhlNeedProto
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
#endif
);

static NhlErrorTypes DrawCurves(
#ifdef NhlNeedProto
	ContourLayer	xlayer
#endif
);

CnDataDepLayerClassRec cnDataDepLayerClassRec = {
	/* base_class */
        {
/* class_name			*/	"CnDataDep",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(CnDataDepLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&dataSpecLayerClassRec,

/* layer_resources		*/	data_resources,
/* num_resources		*/	NhlNumber(data_resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	CnDataClassInitialize,
/* layer_initialize		*/	CnDataInitialize,
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
	/* cndatadep_class */
	{
/* foo				*/	0
	}
};

ContourLayerClassRec contourLayerClassRec = {
	/* base_class */
        {
/* class_name                   */      "Contour",
/* nrm_class                    */      NrmNULLQUARK,
/* layer_size                   */      sizeof(ContourLayerRec),
/* class_inited                 */      False,
/* superclass                   */      (LayerClass)&dataCommLayerClassRec,

/* layer_resources              */      resources,
/* num_resources                */      NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize        */      ContourClassPartInitialize,
/* class_initialize             */      ContourClassInitialize,
/* layer_initialize             */      ContourInitialize,
/* layer_set_values             */      ContourSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values             */      NULL,
/* layer_reparent               */      NULL,
/* layer_destroy                */      ContourDestroy,

/* child_resources              */      NULL,

/* layer_draw                   */      ContourDraw,

/* layer_pre_draw               */      NULL,
/* layer_draw_segonly           */      NULL,
/* layer_post_draw              */      NULL,
/* layer_clear                  */      NULL
        },
	/* view_class */
	{
/* segment_workstation		*/	-1,
/* get_bb			*/	ContourGetBB
	},
	/* trans_class */
	{
/* handles_overlays 		*/	True,
/* data_to_ndc			*/	ContourDataToNDC,
/* ndc_to_data			*/	ContourNDCToData,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL
	},
	/* datacomm_class */
	{
/* data_offsets			*/	NULL,
/* update_data			*/	ContourUpdateData
	},
	/* contour_class */
	{
/* foo				*/	NULL
	}
};

LayerClass cnDataDepLayerClass = (LayerClass)&cnDataDepLayerClassRec;
LayerClass contourLayerClass = (LayerClass)&contourLayerClassRec;

static NrmQuark	Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qstring = NrmNULLQUARK;

/*
 * Function:	CnDataClassInitialize
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
CnDataClassInitialize
#if	__STDC__
(
	void
)
#else
()
#endif
{
	Qint = NrmStringToQuark(NhlTInteger);
	Qstring = NrmStringToQuark(NhlTString);

	return NOERROR;
}

/*
 * Function:	ContourClassInitialize
 *
 * Description:	Add type converters for types added to support this class.
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
	NhlConvertArg	altplace[] = {
				{NHLSTRENUM,	NONE,		"none"},
				{NHLSTRENUM,	LEFTAXIS,	"leftaxis"},
				{NHLSTRENUM,	RIGHTAXIS,	"rightaxis"},
				{NHLSTRENUM,	TOPAXIS,	"topaxis"},
				{NHLSTRENUM,	BOTTOMAXIS,	"bottomaxis"}
				};

	NhlConvertArg	lblmode[] = {
				{NHLSTRENUM,	NOLABELS,	"nolabels"},
				{NHLSTRENUM,	LETTERED,	"lettered"},
				{NHLSTRENUM,	CUSTOM,		"custom"}
				};

	NhlRegisterConverter(NhlTString,NhlTAlternatePlace,NhlCvtStringToEnum,
				altplace,NhlNumber(altplace),False,NULL);
	NhlRegisterConverter(NhlTString,NhlTLineLabelModes,NhlCvtStringToEnum,
					lblmode,NhlNumber(lblmode),False,NULL);

	Qfloat = NrmStringToQuark(NhlTFloat);

	return NOERROR;
}

/*
 * Function:	ContourClassPartInitialize
 *
 * Description:	This function initializes fields in the ContourLayerClassPart
 *		that can not be done by static initialization.
 *		Takes care of calling _NhlRegisterChildClass for the title
 *		and the tick mark objects. This needs to be done so resource
 *		forwarding can work. All Title resources are forwarded with
 *		the exception of the Offset*F resources. 
 *		The tick mark object has several resources blocked. These 
 *		resources all have dependencies to the tm**Style resources.
 *		Since these dependencies exist all of the resouces must be
 *		set in the same SetValues or Create calls. Therefore they
 *		are blocked and storage exists, in the Contour Object instance
 *		record, for these resources. The primary reason for blocking
 *		these resouces is the tm**Style resources. These are controled
 *		by the Contour so that both of the x axis and the y axis can
 *		have correct transformation.
 *
 * In Args:	
 *		LayerClass	lc	Layer Class to init
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
	LayerClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	LayerClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes lret = NOERROR;

	/*
	 * Register children objects
	 */
	lret = _NhlRegisterChildClass(lc,titleLayerClass,False,False,
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

	ret = _NhlRegisterChildClass(lc,tickMarkLayerClass,False,False,
			NhlNtmXBDataLeftF,NhlNtmXBDataRightF,NhlNtmXTDataRightF,
			NhlNtmXTDataLeftF,NhlNtmYLDataTopF,NhlNtmYLDataBottomF,
			NhlNtmYRDataTopF,NhlNtmYRDataBottomF,NhlNtmYLStyle, 
			NhlNtmYRStyle, NhlNtmXBStyle, NhlNtmXTStyle,
			NhlNtmXBIrregularPoints, NhlNtmXBNumIrregularPoints,
			NhlNtmXTIrregularPoints, NhlNtmXTNumIrregularPoints,
			NhlNtmYLIrregularPoints, NhlNtmYLNumIrregularPoints,
			NhlNtmYRIrregularPoints, NhlNtmYRNumIrregularPoints,
			NhlNtmXBIrrTensionF,NhlNtmXTIrrTensionF,
			NhlNtmYLIrrTensionF,NhlNtmYRIrrTensionF,
			NULL);

	ret = MIN(ret,lret);

	/*
	 * Register Data Resources
	 */

	lret = _NhlRegisterDataRes((DataCommLayerClass)lc,NhlNcnCurveData,
			cnDataDepLayerClass,coordArrTableFloatLayerClass,NULL);
	return MIN(lret,ret);
}

/*
 * Function:	ContourChanges
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
ContourChanges
#if	__STDC__
(
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
)
#else
(xnew,xold,calledfrom)
	ContourLayer	xnew;
	ContourLayer	xold;
	_NhlCallType	calledfrom;
#endif
{
	NhlErrorTypes	ret1 = NOERROR;
	NhlErrorTypes	ret2 = NOERROR;


	ret1 = InternalizePointers(xnew,xold,calledfrom);
	if(ret1 < WARNING)
		return ret1;

	ret2 = CheckValues(xnew,xold,calledfrom);
	if(ret2 < WARNING)
		return ret2;
	ret1 = MIN(ret1,ret2);

	ret2 = ComputeDataExtents(xnew,xold,calledfrom);
	if(ret2 < WARNING)
		return ret2;
	ret1 = MIN(ret1,ret2);

	if(xnew->contour.data_ranges_set){
		ret2 = SetUpTransObjs(xnew,xold,calledfrom);
		if(ret2 < WARNING)
			return(ret2);
		ret1 = MIN(ret1,ret2);
	}

	ret2 = SetUpTicks(xnew,xold,calledfrom);
	if(ret2 < WARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);

	ret2 = SetUpTitles(xnew,xold,calledfrom);
	if(ret2 < WARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);

	return ret1;

}

/*
 * Function:	CnDataInitialize
 *
 * Description:	Initializes the CnData Dependent class instance.
 *
 * In Args:	
 *		LayerClass	class,
 *		Layer		req,
 *		Layer		new,
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
CnDataInitialize
#if     __STDC__
(
	LayerClass	class,
	Layer		req,
	Layer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        LayerClass      class;
        Layer           req;
        Layer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	CnDataDepLayer	dnew = (CnDataDepLayer)new;
	char		*error_lead = "CnDataInitialize";
	NhlGenArray	gen;
	NhlErrorTypes	ret = NOERROR;

	if(dnew->cndata.colors != NULL){
		gen = dnew->cndata.colors;
		if((gen->typeQ == Qint) && (gen->size == sizeof(int)) &&
						(gen->num_dimensions == 1)){
			dnew->cndata.colors = _NhlCopyGenArray(gen,True);
		}
		else{
			NhlPError(WARNING,E_UNKNOWN,
		"%s:%s must be set with a 1-dim generic int array: ignoring",
					error_lead,NhlNcnColors);

			dnew->cndata.colors = NULL;
			ret = MIN(ret,WARNING);
		}
	}

	if(dnew->cndata.dash_patterns != NULL){
		gen = dnew->cndata.dash_patterns;
		if((gen->typeQ == Qint) && (gen->size == sizeof(int)) &&
						(gen->num_dimensions == 1)){
			dnew->cndata.dash_patterns = _NhlCopyGenArray(gen,True);
		}
		else{
			NhlPError(WARNING,E_UNKNOWN,
		"%s:%s must be set with a 1-dim generic int array: ignoring",
					error_lead,NhlNcnDashPatterns);

			dnew->cndata.dash_patterns = NULL;
			ret = MIN(ret,WARNING);
		}
	}

	if(dnew->cndata.labels != NULL){
		gen = dnew->cndata.labels;
		if((gen->typeQ == Qstring) &&(gen->size == sizeof(NhlString)) &&
						(gen->num_dimensions == 1)){

			dnew->cndata.labels = _NhlCopyGenArray(gen,True);
		}
		else{
			NhlPError(WARNING,E_UNKNOWN,
	"%s:%s must be set with a generic NhlString array: ignoring",
					error_lead,NhlNcnExplicitLabels);

			dnew->cndata.labels = NULL;
			ret = MIN(ret,WARNING);
		}
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
	LayerClass	class,
	Layer		req,
	Layer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        LayerClass      class;
        Layer           req;
        Layer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	ContourLayer	xnew = (ContourLayer)new;

	xnew->contour.ticks = NULL;
	xnew->contour.ttitles = NULL;
	xnew->contour.thetrans = NULL;
	xnew->contour.data_ranges_set = False;
	xnew->contour.check_ranges = True;

	return ContourChanges((ContourLayer)new,NULL,CREATE);
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
static NhlErrorTypes
ContourSetValues
#if  __STDC__
(
	Layer		old,
	Layer		reference,
	Layer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	Layer		old;
	Layer		reference;
	Layer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	ContourLayer xnew = (ContourLayer)new;
	ContourLayer xold = (ContourLayer)old;
	float deltax,deltay;

	/*
	 * POSSIBLE DIV0 Problem???
	 */
	deltax = xnew->view.width/xold->view.width;
	deltay = xnew->view.height/xold->view.height;

	if((xnew->view.width != xold->view.width) &&
		(xnew->contour.line_label_font_height ==
			xold->contour.line_label_font_height)){

		xnew->contour.line_label_font_height =
				deltay * xnew->contour.line_label_font_height;
	}
	if((xnew->view.width != xold->view.width) &&
		(xnew->contour.dash_segment_length ==
			xold->contour.dash_segment_length)){

		xnew->contour.dash_segment_length =
				deltax * xnew->contour.dash_segment_length;
	}

	return ContourChanges(xnew,xold,SET);
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
#if __STDC__
(
	ContourLayer	xlayer
)
#else
(xlayer)
	ContourLayer	xlayer;
#endif
{
	int			i,j;
	int			num_data;
	int			current_letter = 0;
	NhlErrorTypes		ret = NOERROR;
	NhlErrorTypes		ret1 = NOERROR;
	char			buffer[80];
	int			upordownflag = 1;
	CoordArrTableFloatLayer	datal;
	CnDataDepLayer		dataspec;
	_NhlDataNodePtr		*datalist = NULL;
	NhlBoolean		new;

	for(i = 0; i< 80; i++)
		buffer[i] = '\0';

	ret = _NhlActivateWorkstation((Layer)xlayer->base.wkptr);	
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,
"DrawCurves:Could not activate workstation no data curves will be drawn");
		return(FATAL);
	}
	ret1 = MIN(ret,ret1);

	ret = _NhlSetTrans((Layer)xlayer->contour.thetrans,(Layer)xlayer);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,
	"DrawCurves:Could not set transformation no data curves will be drawn");
		return(FATAL);
	}
	ret1 = MIN(ret,ret1);

	num_data = _NhlGetDataInfo(xlayer->contour.curve_data,&datalist);
	if(num_data <= 0){
		xlayer->contour.data_ranges_set = False;
		NhlPError(FATAL,E_UNKNOWN,"ContourDraw:%s resource problem",
							NhlNcnCurveData);
		return FATAL;
	}

	NhlSetValues(xlayer->base.wkptr->base.id,
		NhlNwkLineThicknessF,	xlayer->contour.curve_thickness,
		NhlNwkLineLabelFontHeightF,
					xlayer->contour.line_label_font_height,
		NULL);

	for(i=0;i < num_data;i++){
		float		**yvalues;
		float		**xvalues;
		int		*len_yvalues;
		int		*len_xvalues;
		NhlBoolean	impx;
		NhlBoolean	impy;
		int		*ctable=NULL;
		int		len_ctable=0;
		int		*dashtable=NULL;
		int		len_dashtable=0;
		int		num_curves=0;
		NhlString	*labeltable=NULL;
		int		len_labeltable =0;

		/*
		 * Retrieve Data Information
		 */
		datal=(CoordArrTableFloatLayer)_NhlGetDataSet(datalist[i],&new);
		if(datal == NULL){
			xlayer->contour.data_ranges_set = False;
			NhlPError(FATAL,E_UNKNOWN,"Data Problem???");
			return FATAL;
		}
		if(datal->catfloat.ytable != NULL){
			yvalues = (float**)datal->catfloat.ytable->data;
			len_yvalues = (int*)datal->catfloat.ytable_lens->data;
			num_curves = datal->catfloat.ytable->len_dimensions[0];
			impy = False;
		}
		else{
			yvalues = NULL;
			len_yvalues = NULL;
			impy = True;
		}
		if(datal->catfloat.xtable != NULL){
			xvalues = (float**)datal->catfloat.xtable->data;
			len_xvalues = (int*)datal->catfloat.xtable_lens->data;
			impx = False;
			if(impy)
				num_curves =
				datal->catfloat.xtable->len_dimensions[0];
			else
				num_curves =
					MIN(num_curves,
				datal->catfloat.xtable->len_dimensions[0]);
		}
		else{
			xvalues = NULL;
			len_xvalues = NULL;
			impx = True;
			if(impy){
				NhlPError(WARNING,E_UNKNOWN,
					"Data has implied X and implied Y?");
				continue;
			}
		}

		/*
		 * Retrieve Data Specific information
		 */
		dataspec = (CnDataDepLayer)datalist[i]->dataspec;

		/*
		 * colors
		 */
		if(dataspec->cndata.colors != NULL){
			ctable = (int*)dataspec->cndata.colors->data;
			len_ctable =dataspec->cndata.colors->len_dimensions[0];
		}
		else
			len_ctable = 0;

		/*
		 * dash patterns
		 */
		if(dataspec->cndata.dash_patterns != NULL){
			dashtable = (int*)dataspec->cndata.dash_patterns->data;
			len_dashtable =
				dataspec->cndata.dash_patterns->len_dimensions[0];
		}
		else
			len_dashtable = 0;

		/*
		 * labels
		 */
		if(dataspec->cndata.labels != NULL){
			labeltable = (NhlString*)dataspec->cndata.labels->data;
			len_labeltable =dataspec->cndata.labels->len_dimensions[0];
		}
		else
			len_labeltable = 0;
		
		for(j=0;j < num_curves;j++){
			float		*yvect=NULL;
			float		*xvect=NULL;
			int		dpattern;
			int		color;
			NhlString	label=NULL;
			int		tint;
			int		npts;

			if(!impy && !impx){
				xvect = xvalues[j];
				yvect = yvalues[j];
				npts = MIN(len_yvalues[j],len_xvalues[j]);
			}
			else if(!impx){
				xvect = xvalues[j];
				npts = len_xvalues[j];
			}
			else if(!impy){
				yvect = yvalues[j];
				npts = len_yvalues[j];
			}
			else{
				NhlPError(WARNING,E_UNKNOWN,
					"Data has implied X and implied Y?");
				break;
			}

			/****************
			* Set Color	*
			****************/
			if(j < len_ctable)
				color = ctable[j];
			else
				color = dataspec->cndata.color;

			/****************
			* Set Dash	*
			****************/
			if(j < len_dashtable)
				dpattern = dashtable[j];
			else
				dpattern = dataspec->cndata.dash;

			/****************
			 * Set Label	*
			 ***************/
			/*
			 * clear buffer
			 */
			for(tint = strlen(buffer) - 1; tint >= 0;tint--)
				buffer[tint] = '\0';
			switch(dataspec->cndata.label_mode) {
				case NOLABELS:
					label = NULL;
				break;
				case CUSTOM:	
					if(j < len_labeltable)
						label = labeltable[j];
					else{
						sprintf(buffer,"%s%d",
							dataspec->base.name,
									j+1);
						label = buffer;
					}
				break;
				case LETTERED:
					buffer[0] =
					(char)((int)'A' + current_letter % 26);
					current_letter++;
					buffer[1] = '\0';
					label = buffer;
				break;
			}

			NhlSetValues(xlayer->base.wkptr->base.id,
				NhlNwkLineColor,	color,
				NhlNwkDashPattern,	dpattern,
				NhlNwkLineLabel,	label,
				NULL);

			_NhlSetLineInfo(xlayer->base.wkptr,(Layer)xlayer);

			upordownflag = 1;

			if(impx){
				if(datal->catfloat.missing_y_set){
					float	ymiss=datal->catfloat.missing_y;
					for(tint=0;tint < npts;tint++){
						if(yvect[tint] == ymiss)
							upordownflag = 1;
						else{
							_NhlDataLineTo(
							xlayer->contour.thetrans,
								(Layer)xlayer,
								(float)(tint+1),
								yvect[tint],
								upordownflag);

							upordownflag = 0;
						}
					}
				}
				else{
					for(tint=0;tint < npts;tint++){
						_NhlDataLineTo(
							xlayer->contour.thetrans,
							(Layer)xlayer,
							(float)(tint+1),
							yvect[tint],
							upordownflag);

						upordownflag = 0;
					}
				}
			}
			else if(impy){
				if(datal->catfloat.missing_x_set){
					float	xmiss=datal->catfloat.missing_x;
					for(tint=0;tint < npts;tint++){
						if(xvect[tint] == xmiss)
							upordownflag = 1;
						else{
							_NhlDataLineTo(
							xlayer->contour.thetrans,
								(Layer)xlayer,
								xvect[tint],
								(float)(tint+1),
								upordownflag);

							upordownflag = 0;
						}
					}
				}
				else{
					for(tint=0;tint < npts;tint++){
						_NhlDataLineTo(
							xlayer->contour.thetrans,
							(Layer)xlayer,
							xvect[tint],
							(float)(tint+1),
							upordownflag);

						upordownflag = 0;
					}
				}
			}
			else{
				if(datal->catfloat.missing_x_set &&
					datal->catfloat.missing_y_set){
					float	xmiss=datal->catfloat.missing_x;
					float	ymiss=datal->catfloat.missing_y;

					for(tint=0;tint < npts;tint++){
						if((xvect[tint] == xmiss) ||
							(yvect[tint] == ymiss))
							upordownflag = 1;
						else{
							_NhlDataLineTo(
							xlayer->contour.thetrans,
								(Layer)xlayer,
								xvect[tint],
								yvect[tint],
								upordownflag);

							upordownflag = 0;
						}
					}
				}
				else if(datal->catfloat.missing_x_set){
					float	xmiss=datal->catfloat.missing_x;

					for(tint=0;tint < npts;tint++){
						if(xvect[tint] == xmiss)
							upordownflag = 1;
						else{
							_NhlDataLineTo(
							xlayer->contour.thetrans,
								(Layer)xlayer,
								xvect[tint],
								yvect[tint],
								upordownflag);

							upordownflag = 0;
						}
					}
				}
				else if(datal->catfloat.missing_y_set){
					float	ymiss=datal->catfloat.missing_y;

					for(tint=0;tint < npts;tint++){
						if(yvect[tint] == ymiss)
							upordownflag = 1;
						else{
							_NhlDataLineTo(
							xlayer->contour.thetrans,
								(Layer)xlayer,
								xvect[tint],
								yvect[tint],
								upordownflag);

							upordownflag = 0;
						}
					}
				}
				else{
					for(tint=0;tint < npts;tint++){
						_NhlDataLineTo(
							xlayer->contour.thetrans,
							(Layer)xlayer,
							xvect[tint],
							yvect[tint],
							upordownflag);

						upordownflag = 0;
					}
				}
			}
		}
	}

	/*
	 * This is called here so lastd is called for the last line.
	 */
	_NhlDataLineTo(xlayer->contour.thetrans,(Layer)xlayer,1.0,1.0,1);

	ret = _NhlDeactivateWorkstation(xlayer->base.wkptr);	
	if(ret < ret1)
		ret1 = ret;
	return(ret);
	
}
/*
 * Function:	ContourDraw
 *
 * Description:	Draw method for the Contour object. This function calls 
 *		NhlDraw for the TickMarks and the Titles and then calls 
 *		DrawCurves to set up and call AUTOGRAPH. 
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
(Layer layer)
#else
(layer)
        Layer layer;
#endif
{
	ContourLayer	xlayer = (ContourLayer) layer;
	NhlErrorTypes ret1 = NOERROR;
	NhlErrorTypes ret = NOERROR;

/*
* Should probably have resource for letting user draw curves on
* top of or below Ticks
*/

	if((!xlayer->contour.data_ranges_set) ||
					(xlayer->contour.thetrans == NULL)){
		NhlPError(FATAL,E_UNKNOWN,
				"ContourDraw:Data Must be set before Drawing");
		return FATAL;
	}

	ret = DrawCurves(xlayer);
	if(ret < WARNING)
		return ret;
	ret1 = MIN(ret,ret1);

	if(xlayer->contour.titles && xlayer->contour.ttitles){
		ret = NhlDraw(xlayer->contour.ttitles->base.id);
		if(ret < WARNING)
			return ret;
		ret1 = MIN(ret,ret1);
	}

	if(xlayer->contour.ticks){
		ret = NhlDraw(xlayer->contour.ticks->base.id);
		ret1 = MIN(ret,ret1);
	}

	return ret1;
}


/*
 * Function:	ContourDataToNDC
 *
 * Description: This is the Data to NDC method of the transform class. It
 *		maps data to normalized device coordinates using the Contour
 *		object's TransObj which is referenced through 
 *		xplot->contour.thetrans . The tranformation is set using 
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
static NhlErrorTypes ContourDataToNDC
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
	ContourLayer xplot = (ContourLayer)plot;
	int istrans = 0;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;

	 ret = _NhlSetTrans(xplot->contour.thetrans,plot);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"ContourDataToNDC: A FATAL error occured while setting the tranformation of Contour object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if( ret < ret1 )
		ret1 = ret; 
		

	ret = _NhlDataToWin(xplot->contour.thetrans,plot,x,y,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING){
		NhlPError(FATAL,E_UNKNOWN,"ContourNDCToData: A FATAL error occured while transforming input to window, Contour object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if( ret < ret1)
		ret1 = ret;

	if(!istrans) {
		if(x != xout)
		memcpy((char*)xout,(char*)x,sizeof(float)*n);
		if(y != yout)
		memcpy((char*)yout,(char*)y,sizeof(float)*n);
	}

	istrans = 0;
	ret = _NhlWinToNDC(xplot->contour.thetrans,plot,xout,yout,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"ContourNDCToData: A FATAL error occured while transforming from window to NDC, Contour object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if( ret < ret1)
		ret1 = ret;

	return(ret1);

}
/*
 * Function:	ContourNDCToData
 *
 * Description:	Transform objects NDC to Data method for the Contour. 
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
static NhlErrorTypes ContourNDCToData
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
	ContourLayer xplot = (ContourLayer)plot;
	int istrans = 0;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;

	ret = _NhlSetTrans(xplot->contour.thetrans,plot);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"ContourNDCToData: A FATAL error occured while setting the tranformation of Contour object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if(ret < ret1)
		ret1 = ret;


	ret = _NhlNDCToWin(xplot->contour.thetrans,plot,x,y,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING){
		NhlPError(FATAL,E_UNKNOWN,"ContourNDCToData: A FATAL error occured while transforming input to window, Contour object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if(ret < ret1)
		ret1 = ret;


	if(!istrans) {
		if(x != xout)
		memcpy((char*)xout,(char*)x,sizeof(float)*n);
		if(y != yout)
		memcpy((char*)yout,(char*)y,sizeof(float)*n);
	}


	istrans = 0;
	ret = _NhlWinToData(xplot->contour.thetrans,plot,xout,yout,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"ContourNDCToData: A FATAL error occured while transforming from window to data, Contour object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if(ret < ret1)
		ret1 = ret;


	return(ret1);

}


/*
 * Function:	ContourDestroy
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
static NhlErrorTypes ContourDestroy
#if __STDC__
(Layer inst)
#else
(inst)
Layer inst;
#endif
{
	ContourLayer xinst = (ContourLayer)inst;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;


	if(xinst->contour.ticks != NULL)
		ret = _NhlDestroyChild(xinst->contour.ticks->base.id,inst);

	if(xinst->contour.ttitles != NULL)
		ret1 = _NhlDestroyChild(xinst->contour.ttitles->base.id,inst);
	ret = MIN(ret,ret1);

	if(xinst->contour.thetrans != NULL)
		ret1 = NhlDestroy(xinst->contour.thetrans->base.id);
	ret = MIN(ret,ret1);

	NhlFreeGenArray(xinst->contour.x_irregular_points);
	NhlFreeGenArray(xinst->contour.y_irregular_points);

	NhlFreeGenArray(xinst->contour.x_original_coords);
	NhlFreeGenArray(xinst->contour.x_alternate_coords);
	NhlFreeGenArray(xinst->contour.y_alternate_coords);
	NhlFreeGenArray(xinst->contour.y_original_coords);
	
	return(ret1);
}

/*
 * Function:	ContourGetBB
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
static NhlErrorTypes ContourGetBB
#if  __STDC__
(Layer instance, NhlBoundingBox* thebox)
#else
(instance,thebox)
	Layer instance;
	NhlBoundingBox *thebox;
#endif
{
	ContourLayer xinst = (ContourLayer)instance;
	NhlErrorTypes ret = NOERROR;

	if(xinst->contour.ticks != NULL) {
		ret = _NhlGetBB(xinst->contour.ticks,thebox);
		if(ret < WARNING) 
			return(ret);
	}
	
	if(xinst->contour.ttitles != NULL) {
		return(MIN(ret,_NhlGetBB(xinst->contour.ttitles,thebox)));
	} else {
		return(ret);
	}
}

/*
 * Function:	ContourUpdateData
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
ContourUpdateData
#if	__STDC__
(
	DataCommLayer	new,
	DataCommLayer	old
)
#else
(new,old)
	DataCommLayer	new;
	DataCommLayer	old;
#endif
{
	ContourLayer		xl = (ContourLayer)new;
	ContourLayer		xlold = (ContourLayer)old;
	NhlErrorTypes		ret1=NOERROR,ret2=NOERROR;

	ret2 = ComputeDataExtents(xl,xlold,DATACHANGE);
	if(ret2 < WARNING)
		return ret2;
	ret1 = MIN(ret1,ret2);

	if(xl->contour.data_ranges_set){
		ret2 = SetUpTransObjs(xl,xlold,DATACHANGE);
		if(ret2 < WARNING)
			return(ret2);
		ret1 = MIN(ret1,ret2);
	}

	ret2 = SetUpTicks(xl,xlold,DATACHANGE);
	if(ret2 < WARNING)
		return(ret2);
	ret1 = MIN(ret1,ret2);

	ret2 = SetUpTitles(xl,xlold,DATACHANGE);
	if(ret2 < WARNING)
		return(ret2);
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
#if	__STDC__
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
	NhlErrorTypes	ret = NOERROR;

	if(extent_change && *compute_value){
		*compute_value = False;

		if(compute_change){
			NhlPError(WARNING,E_UNKNOWN,
			"%s:Setting %s to False because %s was specified",
						error_lead,comp_res,extent_res);
			ret = WARNING;
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
 *		ContourLayer	xnew,
 *		ContourLayer	xold,
 *		_NhlCallType	calledfrom
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
static NhlErrorTypes
CheckValues
#if  __STDC__
(
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
)
#else
(xnew,xold,calledfrom)
	ContourLayer	xnew;
	ContourLayer	xold;
	_NhlCallType	calledfrom;
#endif
{
	char*		error_lead;
	NhlErrorTypes	ret = NOERROR, lret = NOERROR;

	if(calledfrom == CREATE)
		error_lead = "ContourInitialize";
	else
		error_lead = "ContourSetValues";

	/*
	 * take care of style resources
	 */
	if((xnew->contour.x_style == IRREGULAR) &&
				(xnew->contour.x_irregular_points == NULL)){
		NhlPError(WARNING,E_UNKNOWN,
		"%s: cannot be IRREGULAR unless %s is set:setting %s to LINEAR",
			NhlNcnXStyle,NhlNcnXIrregularPoints,NhlNcnXStyle);

		xnew->contour.x_style = LINEAR;
		ret = MIN(ret,WARNING);
	}
	if((xnew->contour.y_style == IRREGULAR) &&
				(xnew->contour.y_irregular_points == NULL)){
		NhlPError(WARNING,E_UNKNOWN,
		"%s: cannot be IRREGULAR unless %s is set:setting %s to LINEAR",
			NhlNcnYStyle,NhlNcnYIrregularPoints,NhlNcnYStyle);

		xnew->contour.y_style = LINEAR;
		ret = MIN(ret,WARNING);
	}

	/*
	 * Alternate Coord's are not yet implimented - so make sure x_alternate
	 * and y_alternate are set to NONE.
	 * (Eventually this part should check and make sure the coord arrays
	 * exist if x_alternate and y_alternate are not = to NONE)
	 */
	if(xnew->contour.x_alternate != NONE){
		xnew->contour.x_alternate = NONE;
		NhlPError(WARNING,E_UNKNOWN,
			"%s:%s only supports a value of NONE at this time",
						error_lead,NhlNcnXAlternate);
		ret = MIN(ret,WARNING);
	}
	if(xnew->contour.y_alternate != NONE){
		xnew->contour.y_alternate = NONE;
		NhlPError(WARNING,E_UNKNOWN,
			"%s:%s only supports a value of NONE at this time",
						error_lead,NhlNcnYAlternate);
		ret = MIN(ret,WARNING);
	}

	/*
	 * Check Extents - left right top bottom
	 */

	if(calledfrom == CREATE){
		lret = CheckExtent(xnew->contour.x_min_set,
			xnew->contour.comp_x_min_set,&xnew->contour.compute_x_min,
			NhlNcnComputeXMin,NhlNcnXMinF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent(xnew->contour.x_max_set,
			xnew->contour.comp_x_max_set,&xnew->contour.compute_x_max,
			NhlNcnComputeXMax,NhlNcnXMaxF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent(xnew->contour.y_max_set,
			xnew->contour.comp_y_max_set,&xnew->contour.compute_y_max,
			NhlNcnComputeYMax,NhlNcnYMaxF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent(xnew->contour.y_min_set,
			xnew->contour.comp_y_min_set,&xnew->contour.compute_y_min,
			NhlNcnComputeYMin,NhlNcnYMinF,error_lead);
		ret = MIN(lret,ret);
	}
	else{
		lret = CheckExtent((xold->contour.x_min!=xnew->contour.x_min),
			xnew->contour.compute_x_min,&xnew->contour.compute_x_min,
			NhlNcnComputeXMin,NhlNcnXMinF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent((xold->contour.x_max!=xnew->contour.x_max),
			xnew->contour.compute_x_max,&xnew->contour.compute_x_max,
			NhlNcnComputeXMax,NhlNcnXMaxF,error_lead);
		ret = MIN(lret,ret);

		lret = CheckExtent((xold->contour.y_max!=xnew->contour.y_max),
			xnew->contour.compute_y_max,&xnew->contour.compute_y_max,
			NhlNcnComputeYMax,NhlNcnYMaxF,error_lead);
		ret = MIN(lret,ret);

		lret=CheckExtent((xold->contour.y_min!=xnew->contour.y_min),
			xnew->contour.compute_y_min,&xnew->contour.compute_y_min,
			NhlNcnComputeYMin,NhlNcnYMinF,error_lead);
		ret = MIN(lret,ret);

	}

	if(!xnew->contour.compute_x_min && xnew->contour.x_min_set &&
		(xnew->contour.x_style == LOG) && (xnew->contour.x_min <= 0)){
		NhlPError(WARNING,E_UNKNOWN,
			"%s:%s is LOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNcnXStyle,NhlNcnXMinF,NhlNcnComputeXMin);

		xnew->contour.compute_x_min = True;
		ret = MIN(ret,WARNING);
	}

	if(!xnew->contour.compute_x_max && xnew->contour.x_max_set &&
		(xnew->contour.x_style == LOG) && (xnew->contour.x_max <= 0)){
		NhlPError(WARNING,E_UNKNOWN,
			"%s:%s is LOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNcnXStyle,NhlNcnXMaxF,NhlNcnComputeXMax);

		xnew->contour.compute_x_max = True;
		ret = MIN(ret,WARNING);
	}

	if(!xnew->contour.compute_x_min && xnew->contour.x_min_set &&
		!xnew->contour.compute_x_max && xnew->contour.x_max_set &&
		(xnew->contour.x_max < xnew->contour.x_min)){

		float tfloat;
		NhlPError(WARNING,E_UNKNOWN,"%s:%s is < %s: Swapping",
					error_lead,NhlNcnXMaxF,NhlNcnXMinF);
		tfloat = xnew->contour.x_max;
		xnew->contour.x_max = xnew->contour.x_min;
		xnew->contour.x_min = tfloat;
	}

	if(!xnew->contour.compute_y_min && xnew->contour.y_min_set &&
		(xnew->contour.y_style == LOG) && (xnew->contour.y_min <= 0)){
		NhlPError(WARNING,E_UNKNOWN,
			"%s:%s is LOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNcnYStyle,NhlNcnYMinF,NhlNcnComputeYMin);

		xnew->contour.compute_y_min = True;
		ret = MIN(ret,WARNING);
	}

	if(!xnew->contour.compute_y_max && xnew->contour.y_max_set &&
		(xnew->contour.y_style == LOG) && (xnew->contour.y_max <= 0)){
		NhlPError(WARNING,E_UNKNOWN,
			"%s:%s is LOG:%s can't be <= 0.0:Setting %s to True",
			error_lead,NhlNcnYStyle,NhlNcnYMaxF,NhlNcnComputeYMax);

		xnew->contour.compute_y_max = True;
		ret = MIN(ret,WARNING);
	}

	if(!xnew->contour.compute_y_min && xnew->contour.y_min_set &&
		!xnew->contour.compute_y_max && xnew->contour.y_max_set &&
		(xnew->contour.y_max < xnew->contour.y_min)){

		float tfloat;
		NhlPError(WARNING,E_UNKNOWN,"%s:%s is < %s: Swapping",
					error_lead,NhlNcnYMaxF,NhlNcnYMinF);
		tfloat = xnew->contour.y_max;
		xnew->contour.y_max = xnew->contour.y_min;
		xnew->contour.y_min = tfloat;
	}

	if((calledfrom == SET) &&
		((xold->contour.x_style != xnew->contour.x_style) ||
		(xold->contour.y_style != xnew->contour.y_style))){

		xnew->contour.check_ranges = True;
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
 *		ContourLayer	xnew,
 *		ContourLayer	xold,
 *		_NhlCallType	calledfrom
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
static NhlErrorTypes
InternalizePointers
#if  __STDC__
(
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
)
#else
(xnew,xold,calledfrom)
	ContourLayer	xnew;
	ContourLayer	xold;
	_NhlCallType	calledfrom;
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
	NhlErrorTypes	ret = NOERROR;

	if(calledfrom == SET) {
		error_lead = "ContourSetValues";
	} else {
		error_lead = "ContourInitialize";
	}

	/*
	 * take care of irregular_points
	 */
	if(calledfrom == SET){
		if(xold->contour.x_irregular_points !=
						xnew->contour.x_irregular_points)
			free_x_irreg = True;
		else
			skip_x_irreg_pts = True;
		if(xold->contour.y_irregular_points !=
						xnew->contour.y_irregular_points)
			free_y_irreg = True;
		else
			skip_y_irreg_pts = True;
	}

	if((xnew->contour.x_irregular_points != NULL) && !skip_x_irreg_pts) {
		gen = (NhlGenArray)xnew->contour.x_irregular_points;
		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
		(gen->num_dimensions != 1) || (gen->len_dimensions[0] < 3)){
			NhlPError(WARNING,E_UNKNOWN,
	"%s:%s must be a 1 dim float array with a min of 3 elements: resetting",
					error_lead,NhlNcnXIrregularPoints);

			if(calledfrom == SET)
				xnew->contour.x_irregular_points =
						xold->contour.x_irregular_points;
			else
				xnew->contour.x_irregular_points = NULL;

			free_x_irreg = False;
			ret = MIN(ret,WARNING);
		}
		else{
			float	*tarr;

			xnew->contour.x_irregular_points =
						_NhlCopyGenArray(gen,True);
			if(xnew->contour.x_irregular_points == NULL){
				NhlPError(FATAL,ENOMEM,NULL);
				return FATAL;
			}
			tarr = (float*)gen->data;
			xnew->contour.x_irreg_min =
				MIN(tarr[0],tarr[gen->len_dimensions[0]-1]);
			xnew->contour.x_irreg_max =
				MAX(tarr[0],tarr[gen->len_dimensions[0]-1]);
			xnew->contour.check_ranges = True;
		}
	}
	if(free_x_irreg)
		NhlFreeGenArray(xold->contour.x_irregular_points);

	if((xnew->contour.y_irregular_points != NULL) && !skip_y_irreg_pts){
		gen = (NhlGenArray)xnew->contour.y_irregular_points;
		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
		(gen->num_dimensions != 1) || (gen->len_dimensions[0] < 3)){
			NhlPError(WARNING,E_UNKNOWN,
	"%s:%s must be a 1 dim float array with a min of 3 elements: ignoring",
					error_lead,NhlNcnYIrregularPoints);

			if(calledfrom == SET)
				xnew->contour.y_irregular_points =
						xold->contour.y_irregular_points;
			else
				xnew->contour.y_irregular_points = NULL;

			free_y_irreg = False;
			ret = MIN(ret,WARNING);
		}
		else{
			float	*tarr;

			xnew->contour.y_irregular_points =
						_NhlCopyGenArray(gen,True);
			if(xnew->contour.y_irregular_points == NULL){
				NhlPError(FATAL,ENOMEM,NULL);
				return FATAL;
			}
			tarr = (float*)gen->data;
			xnew->contour.y_irreg_min =
				MIN(tarr[0],tarr[gen->len_dimensions[0]-1]);
			xnew->contour.y_irreg_max =
				MAX(tarr[0],tarr[gen->len_dimensions[0]-1]);
			xnew->contour.check_ranges = True;
		}
	}
	if(free_y_irreg)
		NhlFreeGenArray(xold->contour.y_irregular_points);

	/*
	 * take care of alt coords and orig coords
	 */
	if(calledfrom == SET){
		if(xold->contour.x_alternate_coords !=
						xnew->contour.x_alternate_coords)
			free_x_alt_coord = True;
		else
			skip_x_alt_coord = True;

		if(xold->contour.y_alternate_coords !=
						xnew->contour.y_alternate_coords)
			free_y_alt_coord = True;
		else
			skip_y_alt_coord = True;

		if(xold->contour.x_original_coords !=
						xnew->contour.x_original_coords)
			free_x_orig_coord = True;
		else
			skip_x_orig_coord = True;

		if(xold->contour.y_original_coords !=
						xnew->contour.y_original_coords)
			free_y_orig_coord = True;
		else
			skip_y_orig_coord = True;
	}

	if((xnew->contour.x_alternate_coords != NULL) && !skip_x_alt_coord){
		gen = (NhlGenArray)xnew->contour.x_alternate_coords;
		if((gen->typeQ != Qfloat) || (gen->size != sizeof(float)) ||
						(gen->num_dimensions != 1)){
			NhlPError(WARNING,E_UNKNOWN,
			"%s:%s must be set with a 1-dim float array: ignoring",
					error_lead,NhlNcnXAlternateCoords);

			if(calledfrom == SET)
				xnew->contour.x_alternate_coords =
						xold->contour.x_alternate_coords;
			else
				xnew->contour.x_alternate_coords = NULL;
			free_x_alt_coord = False;

			ret = MIN(ret,WARNING);
		}
		else{
			xnew->contour.x_alternate_coords =
						_NhlCopyGenArray(gen,True);
			if(xnew->contour.x_alternate_coords == NULL){
				NhlPError(FATAL,ENOMEM,NULL);
				return FATAL;
			}
		}
	}
	if(free_x_alt_coord)
		NhlFreeGenArray(xold->contour.x_alternate_coords);

	if((xnew->contour.y_alternate_coords != NULL) && !skip_y_alt_coord){
		gen = (NhlGenArray)xnew->contour.y_alternate_coords;
		if((gen->typeQ == Qfloat) && (gen->size == sizeof(float)) &&
						(gen->num_dimensions == 1)){
			xnew->contour.y_alternate_coords =
						_NhlCopyGenArray(gen,True);
			if(xnew->contour.y_alternate_coords == NULL){
				NhlPError(FATAL,ENOMEM,NULL);
				return FATAL;
			}
		}
		else{
			NhlPError(WARNING,E_UNKNOWN,
			"%s:%s must be set with a 1-dim float array: ignoring",
					error_lead,NhlNcnYAlternateCoords);

			if(calledfrom == SET)
				xnew->contour.y_alternate_coords =
						xold->contour.y_alternate_coords;
			else
				xnew->contour.y_alternate_coords = NULL;
			free_y_alt_coord = False;

			ret = MIN(ret,WARNING);
		}
	}
	if(free_y_alt_coord)
		NhlFreeGenArray(xold->contour.y_alternate_coords);

	if((xnew->contour.x_original_coords != NULL) && !skip_x_orig_coord){
		gen = (NhlGenArray)xnew->contour.x_original_coords;
		if((gen->typeQ == Qfloat) && (gen->size == sizeof(float)) &&
						(gen->num_dimensions == 1)){
			xnew->contour.x_original_coords =
						_NhlCopyGenArray(gen,True);
			if(xnew->contour.x_original_coords == NULL){
				NhlPError(FATAL,ENOMEM,NULL);
				return FATAL;
			}
		}
		else{
			NhlPError(WARNING,E_UNKNOWN,
		"%s:%s must be set with a generic 1-dim float array: ignoring",
					error_lead,NhlNcnXOriginalCoords);

			if(calledfrom == SET)
				xnew->contour.x_original_coords =
						xold->contour.x_original_coords;
			else
				xnew->contour.x_original_coords = NULL;
			free_x_orig_coord = False;

			ret = MIN(ret,WARNING);
		}
	}
	if(free_x_orig_coord)
		NhlFreeGenArray(xold->contour.x_original_coords);

	if((xnew->contour.y_original_coords != NULL) && !skip_y_orig_coord){
		gen = (NhlGenArray)xnew->contour.y_original_coords;
		if((gen->typeQ == Qfloat) && (gen->size == sizeof(float)) &&
						(gen->num_dimensions == 1)){
			xnew->contour.y_original_coords =
						_NhlCopyGenArray(gen,True);
			if(xnew->contour.y_original_coords == NULL){
				NhlPError(FATAL,ENOMEM,NULL);
				return FATAL;
			}
		}
		else{
			NhlPError(WARNING,E_UNKNOWN,
		"%s:%s must be set with a generic float array: ignoring",
					error_lead,NhlNcnYOriginalCoords);

			if(calledfrom == SET)
				xnew->contour.y_original_coords =
						xold->contour.y_original_coords;
			else
				xnew->contour.y_original_coords = NULL;
			free_y_orig_coord = False;

			ret = MIN(ret,WARNING);
		}
	}
	if(free_y_orig_coord)
		NhlFreeGenArray(xold->contour.y_original_coords);

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
#if __STDC__
(
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
)
#else 
(xnew,xold,calledfrom)
	ContourLayer	xnew;
	ContourLayer	xold;
	_NhlCallType	calledfrom;
#endif
{
	_NhlDataNodePtr		*datalist = NULL;
	int			num_data,i;
	NhlBoolean		new;
	CoordArrTableFloatLayer	datal = NULL;
	char			*error_lead;
	NhlErrorTypes		ret = NOERROR;

	if(calledfrom == CREATE){
		error_lead = "ContourInitialize";
		if(xnew->contour.curve_data == NULL){
			xnew->contour.data_ranges_set = False;
			return NOERROR;
		}
	}
	else if(calledfrom == SET){
		error_lead = "ContourSetValues";
	}
	else if(calledfrom == DATACHANGE){
		error_lead = "ContourUpdateData";
	}
	else{
		NhlPError(FATAL,E_UNKNOWN,"BadCall");
		return FATAL;
	}

	if((calledfrom == CREATE) || (calledfrom == DATACHANGE) ||
			(xnew->contour.curve_data != xold->contour.curve_data)){

		xnew->contour.data_ranges_set = True;
		xnew->contour.check_ranges = True;

		num_data = _NhlGetDataInfo(xnew->contour.curve_data,&datalist);
		if(num_data <= 0){
			xnew->contour.data_ranges_set = False;
			return NOERROR;
		}

		/*
		 * Data Conversion Happens Here if anywhere.
		 */
		datal=(CoordArrTableFloatLayer)_NhlGetDataSet(datalist[0],&new);
		if(datal == NULL){
			xnew->contour.data_ranges_set = False;
			NhlPError(FATAL,E_UNKNOWN,"Data Problem???");
			return FATAL;
		}

		xnew->contour.x_data_min = datal->catfloat.min_x;
		xnew->contour.x_data_max = datal->catfloat.max_x;
		xnew->contour.y_data_min = datal->catfloat.min_y;
		xnew->contour.y_data_max = datal->catfloat.max_y;


		for(i=1;i < num_data;i++){
			datal=(CoordArrTableFloatLayer)
					_NhlGetDataSet(datalist[i],&new);
			if(datal == NULL){
				xnew->contour.data_ranges_set = False;
				NhlPError(FATAL,E_UNKNOWN,"Data Problem???");
				return FATAL;
			}
			xnew->contour.x_data_min =
			MIN(xnew->contour.x_data_min,datal->catfloat.min_x);
			xnew->contour.x_data_max =
			MAX(xnew->contour.x_data_max,datal->catfloat.max_x);
			xnew->contour.y_data_min =
			MIN(xnew->contour.y_data_min,datal->catfloat.min_y);
			xnew->contour.y_data_max =
			MAX(xnew->contour.x_data_max,datal->catfloat.max_y);
		}
	}

	if(xnew->contour.check_ranges){

		/*
		 * Set Initial default for left,right,top,bottom
		 * (should only happen if user didn't set it themself, and it
		 * will only be used if the compute resources are False.)
		 *
		 * Also set if compute resources are true.
		 */
		if(!xnew->contour.x_min_set || xnew->contour.compute_x_min){
			xnew->contour.x_min = xnew->contour.x_data_min;
			xnew->contour.x_min_set = True;
		}
		if(!xnew->contour.x_max_set || xnew->contour.compute_x_max){
			xnew->contour.x_max = xnew->contour.x_data_max;
			xnew->contour.x_max_set = True;
		}
		if(!xnew->contour.y_min_set || xnew->contour.compute_y_min){
			xnew->contour.y_min = xnew->contour.y_data_min;
			xnew->contour.y_min_set = True;
		}
		if(!xnew->contour.y_max_set || xnew->contour.compute_y_max){
			xnew->contour.y_max = xnew->contour.y_data_max;
			xnew->contour.y_max_set = True;
		}

		/*
		 * Make sure data extents will work in a LOG trans, if LOG
		 * is specified.
		 */
		if((xnew->contour.x_data_min <= 0.0) &&
						(xnew->contour.x_style == LOG)){
			NhlPError(WARNING,E_UNKNOWN,
	"%s:The Minimuim X value is <= 0.0 LOG invalid:Changing %s to LINEAR",
						error_lead,NhlNcnXStyle);
			ret = MIN(ret,WARNING);
			xnew->contour.x_style = LINEAR;
		}
		if((xnew->contour.y_data_min <= 0.0) &&
						(xnew->contour.y_style == LOG)){
			NhlPError(WARNING,E_UNKNOWN,
	"%s:The Minimuim Y value is <= 0.0 LOG invalid:Changing %s to LINEAR",
						error_lead,NhlNcnYStyle);
			ret = MIN(ret,WARNING);
			xnew->contour.y_style = LINEAR;
		}

		/*
		 * Make sure data extents will work in an IRREGULAR trans,
		 * if IRREGULAR is specifed.
		 */
		if(xnew->contour.x_style == IRREGULAR){
			if(xnew->contour.x_min < xnew->contour.x_irreg_min){

				if(!xnew->contour.compute_x_min){
					NhlPError(WARNING,E_UNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNcnXMinF,
						NhlNcnXIrregularPoints,
						xnew->contour.x_irreg_min);
					ret = MIN(ret,WARNING);
				}
				xnew->contour.x_min = xnew->contour.x_irreg_min;
			}
			if(xnew->contour.x_max > xnew->contour.x_irreg_max){

				if(!xnew->contour.compute_x_max){
					NhlPError(WARNING,E_UNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNcnXMaxF,
						NhlNcnXIrregularPoints,
						xnew->contour.x_irreg_max);
					ret = MIN(ret,WARNING);
				}
				xnew->contour.x_max = xnew->contour.x_irreg_max;
			}
		}
		if(xnew->contour.y_style == IRREGULAR){
			if(xnew->contour.y_min < xnew->contour.y_irreg_min){

				if(!xnew->contour.compute_y_min){
					NhlPError(WARNING,E_UNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNcnYMinF,
						NhlNcnYIrregularPoints,
						xnew->contour.y_irreg_min);
					ret = MIN(ret,WARNING);
				}
				xnew->contour.y_min = xnew->contour.y_irreg_min;
			}
			if(xnew->contour.y_max > xnew->contour.y_irreg_max){

				if(!xnew->contour.compute_y_max){
					NhlPError(WARNING,E_UNKNOWN,
				"%s:%s is not defined by %s: Setting to %f",
						error_lead,NhlNcnYMaxF,
						NhlNcnYIrregularPoints,
						xnew->contour.y_irreg_max);
					ret = MIN(ret,WARNING);
				}
				xnew->contour.y_max = xnew->contour.y_irreg_max;
			}
		}

		xnew->contour.check_ranges = False;
	}

	return ret;
}


/*
 * Function:	SetUpTransObjs
 *
 * Description: Creates, Sets and Destroys the main tranformation object
 *		for the Contour. For log and linear plots the tranformation
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
 *		xold	old instance record if calledfrom == SET
 *		calledfrom  set to CREATE or SET
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes
SetUpTransObjs
#if  __STDC__
(
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
)
#else 
(xnew,xold,calledfrom)
	ContourLayer	xnew;
	ContourLayer	xold;
	_NhlCallType	calledfrom;
#endif
{
	NhlSArg		sargs[30];
	int		nargs = 0;
	char		buffer[MAXRESNAMLEN];
	int		tmpid;
	float		tmpcoords[3];
	char		*error_lead=NULL;
	LayerClass	trans_class = NULL;
	NhlGenArray	gen;
	ContourLayerPart	*newcn = &xnew->contour;
	ContourLayerPart	*oldcn=NULL;

/*
 * Now create main transformation object
 */	
	if(calledfrom == CREATE){
		error_lead = "ContourInitialize";
	}
	else{
		oldcn = &xold->contour;

		if(calledfrom == SET){
			error_lead = "ContourSetValues";
		}
		else if (calledfrom == DATACHANGE){
		/*
		 * If we are coming from UpdateData - The only resources that
		 * could have changed are min and max - if they haven't changed
		 * return immediately.
		 */
			if((newcn->x_min == oldcn->x_min) &&
				(newcn->x_max == oldcn->x_max) &&
				(newcn->y_min == oldcn->y_min) &&
				(newcn->y_max == oldcn->y_max)){
				return NOERROR;
			}
			error_lead = "ContourUpdateData";
		}
	}

	/*
	 * If a new trans object needs to be created, do this.
	 */
	if(	(newcn->thetrans == NULL)
		||
		(calledfrom == CREATE)
		||
		(	(	(newcn->x_style == IRREGULAR) ||
				(newcn->y_style == IRREGULAR)
			) &&
			!oldcn->have_irreg_trans
		)
		||
		(	(newcn->x_style != IRREGULAR) &&
			(newcn->y_style != IRREGULAR) &&
			oldcn->have_irreg_trans
		)
									){

		if(newcn->thetrans != NULL){
			(void)NhlDestroy(newcn->thetrans->base.id);
			newcn->thetrans = NULL;
		}

		sprintf(buffer,"%s",xnew->base.name);
		strcat(buffer,".Trans");

		newcn->fake_x = newcn->fake_y = False;

		if(newcn->y_style == IRREGULAR){

			trans_class = irregularType2TransObjLayerClass;
			newcn->have_irreg_trans = True;

			gen = newcn->y_irregular_points;
			NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								gen->data);
			NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,
							gen->len_dimensions[0]);
			NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,
							newcn->y_tension);

			if(newcn->x_style == IRREGULAR){

				gen = newcn->x_irregular_points;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								gen->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
							gen->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newcn->x_tension);
			}
			else{

				newcn->fake_x = True;
				newcn->fake_x_min = tmpcoords[0] = newcn->x_min;
				newcn->fake_x_max = tmpcoords[2] = newcn->x_max;

				if(newcn->x_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newcn->x_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
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
			if(newcn->x_style == IRREGULAR){

				trans_class = irregularType2TransObjLayerClass;
				newcn->have_irreg_trans = True;

				gen = newcn->x_irregular_points;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								gen->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
							gen->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newcn->x_tension);

				newcn->fake_y = True;
				newcn->fake_y_min = tmpcoords[0] = newcn->y_min;
				newcn->fake_y_max = tmpcoords[2] = newcn->y_max;

				if(newcn->y_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newcn->y_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
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
				trans_class = logLinTransObjLayerClass;
				newcn->have_irreg_trans = False;

				if(newcn->x_style == LOG)
					NhlSetSArg(&sargs[nargs++],NhlNtrXLog,
									True);
				if(newcn->y_style == LOG)
					NhlSetSArg(&sargs[nargs++],NhlNtrYLog,
									True);

			}
		}
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,newcn->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,newcn->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,newcn->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,newcn->y_max);

		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,newcn->x_reverse);
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,newcn->y_reverse);

		(void)NhlALCreate(&tmpid,buffer,trans_class,xnew->base.id,
								sargs,nargs);

		newcn->thetrans = _NhlGetLayer(tmpid);
		if(newcn->thetrans == NULL){
			NhlPError(FATAL,E_UNKNOWN,
				"%s:Unable to continue without transformation",
								error_lead);
			return FATAL;
		}

		return NOERROR;
	}

	/*
	 * SetValues/UpdateData in existing trans object
	 */

	/*
	 * if we are tricking an irreg object into being a log or lin - take
	 * care of setting the transformation.
	 */
	if(newcn->have_irreg_trans){
		if(newcn->fake_x){
			if(newcn->x_style == IRREGULAR){
				newcn->fake_x = False;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
					newcn->x_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
				newcn->x_irregular_points->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newcn->x_tension);
			}
			else if((newcn->x_style != oldcn->x_style) ||
				(newcn->x_min < newcn->fake_x_min) ||
				(newcn->x_max > newcn->fake_x_max)){

				newcn->fake_x_min = tmpcoords[0] = newcn->x_min;
				newcn->fake_x_max = tmpcoords[2] = newcn->x_max;
				if(newcn->x_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newcn->x_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);
			}
		}
		else {
			if(newcn->x_style != IRREGULAR){
				newcn->fake_x = True;
				newcn->fake_x_min = tmpcoords[0] = newcn->x_min;
				newcn->fake_x_max = tmpcoords[2] = newcn->x_max;
				if(newcn->x_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newcn->x_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);

			}
		}
		if(newcn->fake_y){
			if(newcn->y_style == IRREGULAR){
				newcn->fake_y = False;
				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
					newcn->y_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,
				newcn->y_irregular_points->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,
							newcn->y_tension);
			}
			else if((newcn->y_style != oldcn->y_style) ||
				(newcn->y_min < newcn->fake_y_min) ||
				(newcn->y_max > newcn->fake_y_max)){

				newcn->fake_y_min = tmpcoords[0] = newcn->y_min;
				newcn->fake_y_max = tmpcoords[2] = newcn->y_max;
				if(newcn->y_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newcn->y_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);
			}
		}
		else {
			if(newcn->y_style != IRREGULAR){
				newcn->fake_y = True;
				newcn->fake_y_min = tmpcoords[0] = newcn->y_min;
				newcn->fake_y_max = tmpcoords[2] = newcn->y_max;
				if(newcn->y_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newcn->y_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);

			}
		}
	}
		
	if(newcn->x_min != oldcn->x_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,newcn->x_min);
	if(newcn->x_max != oldcn->x_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,newcn->x_max);
	if(newcn->y_min != oldcn->y_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,newcn->y_min);
	if(newcn->y_max != oldcn->y_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,newcn->y_max);

	if(newcn->x_reverse != oldcn->x_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,newcn->x_reverse);
	if(newcn->y_reverse != oldcn->y_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,newcn->y_reverse);

	if(newcn->x_style != oldcn->x_style){
		if(newcn->have_irreg_trans)
			NhlSetSArg(&sargs[nargs++],NhlNtrXUseLog,
						(newcn->x_style == LOG));
		else
			NhlSetSArg(&sargs[nargs++],NhlNtrXLog,
						(newcn->x_style == LOG));
	}

	if(newcn->y_style != oldcn->y_style){
		if(newcn->have_irreg_trans)
			NhlSetSArg(&sargs[nargs++],NhlNtrYUseLog,
						(newcn->y_style == LOG));
		else
			NhlSetSArg(&sargs[nargs++],NhlNtrYLog,
						(newcn->y_style == LOG));
	}

	return NhlALSetValues(newcn->thetrans->base.id,sargs,nargs);
}

/*
 * Function:	SetUpTicks
 *
 * Description:	Takes care of setting resources for TickMarks. It is at
 *		this time that the resources, that are blocked by the
 *		_NhlRegisterChildClass call in ContourClassInitialize, are set.
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
 *		that the resources held by the Contour object have the same
 *		values as the tick mark child.
 *		
 *		When Alternate axis control is implemented this will be
 *		the function to compute the mapping functions and configure
 *		the tick mark object.
 *
 * In Args:	xnew	new instance record
 *		xold	old instance record if calledfrom == SET
 *		calledfrom  either SET or CREATE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	New objects created or reconfigured.
 */
/*ARGSUSED*/
static NhlErrorTypes
SetUpTicks
#if	__STDC__
(
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
)
#else 
(xnew,xold,calledfrom)
	ContourLayer	xnew;
	ContourLayer	xold;
	_NhlCallType	calledfrom;
#endif
{
	char		buffer[MAXRESNAMLEN];
	int		tmpid;
	char		*error_lead;
	NhlSArg		sargs[30];
	int		nargs=0;
	ContourLayerPart	*newcn = &xnew->contour;
	ContourLayerPart	*oldcn = NULL;
	NhlErrorTypes	ret=NOERROR;

	if(calledfrom == CREATE){
		error_lead = "ContourInitialize";
	}
	else{
		oldcn = &xold->contour;

		if(calledfrom == SET){
			error_lead = "ContourSetValues";
		}
		else if(calledfrom == DATACHANGE){
		/*
		 * If we are coming from UpdateData - The only resources that
		 * could have changed are min and max - if they haven't changed
		 * return immediately.
		 */
			if((newcn->x_min == oldcn->x_min) &&
				(newcn->x_max == oldcn->x_max) &&
				(newcn->y_min == oldcn->y_min) &&
				(newcn->y_max == oldcn->y_max)){
				return NOERROR;
			}
			error_lead = "ContourUpdateData";
		}
		else{
			NhlPError(FATAL,E_UNKNOWN,"Bad Call");
			return FATAL;
		}
	}
/*
 * Now deal with creating/changing values in the child ticks
 */
	if(xnew->contour.ticks == NULL){
		/*
		 * Create the tickmark object.
		 */
		strcpy(buffer,xnew->base.name);
		strcat(buffer,".Ticks");

		NhlSetSArg(&sargs[nargs++],NhlNvpXF,xnew->view.x);
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,xnew->view.y);
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,xnew->view.width);
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,xnew->view.height);
		NhlSetSArg(&sargs[nargs++],NhlNtmXBDataLeftF,
				(newcn->x_reverse?newcn->x_max:newcn->x_min));
		NhlSetSArg(&sargs[nargs++],NhlNtmXBDataRightF,
				(!newcn->x_reverse?newcn->x_max:newcn->x_min));
		NhlSetSArg(&sargs[nargs++],NhlNtmXTDataLeftF,
				(newcn->x_reverse?newcn->x_max:newcn->x_min));
		NhlSetSArg(&sargs[nargs++],NhlNtmXTDataRightF,
				(!newcn->x_reverse?newcn->x_max:newcn->x_min));

		NhlSetSArg(&sargs[nargs++],NhlNtmYLDataTopF,
				(!newcn->y_reverse?newcn->y_max:newcn->y_min));
		NhlSetSArg(&sargs[nargs++],NhlNtmYLDataBottomF,
				(newcn->y_reverse?newcn->y_max:newcn->y_min));
		NhlSetSArg(&sargs[nargs++],NhlNtmYRDataTopF,
				(!newcn->y_reverse?newcn->y_max:newcn->y_min));
		NhlSetSArg(&sargs[nargs++],NhlNtmYRDataBottomF,
				(newcn->y_reverse?newcn->y_max:newcn->y_min));
		NhlSetSArg(&sargs[nargs++],NhlNtmXBStyle,newcn->x_style);
		NhlSetSArg(&sargs[nargs++],NhlNtmXTStyle,newcn->x_style);
		NhlSetSArg(&sargs[nargs++],NhlNtmYLStyle,newcn->y_style);
		NhlSetSArg(&sargs[nargs++],NhlNtmYRStyle,newcn->y_style);
		NhlSetSArg(&sargs[nargs++],NhlNtmXBIrrTensionF,
							newcn->x_tension);
		NhlSetSArg(&sargs[nargs++],NhlNtmXTIrrTensionF,
							newcn->x_tension);
		NhlSetSArg(&sargs[nargs++],NhlNtmYLIrrTensionF,
							newcn->y_tension);
		NhlSetSArg(&sargs[nargs++],NhlNtmYRIrrTensionF,
							newcn->y_tension);

		if(newcn->x_irregular_points != NULL){
			NhlSetSArg(&sargs[nargs++],NhlNtmXBIrregularPoints,
					newcn->x_irregular_points->data);
			NhlSetSArg(&sargs[nargs++],NhlNtmXTIrregularPoints,
					newcn->x_irregular_points->data);
			NhlSetSArg(&sargs[nargs++],NhlNtmXBNumIrregularPoints,
				newcn->x_irregular_points->len_dimensions[0]);
			NhlSetSArg(&sargs[nargs++],NhlNtmXTNumIrregularPoints,
				newcn->x_irregular_points->len_dimensions[0]);
		}

		if(newcn->y_irregular_points != NULL){
			NhlSetSArg(&sargs[nargs++],NhlNtmYLIrregularPoints,
					newcn->y_irregular_points->data);
			NhlSetSArg(&sargs[nargs++],NhlNtmYRIrregularPoints,
					newcn->y_irregular_points->data);
			NhlSetSArg(&sargs[nargs++],NhlNtmYLNumIrregularPoints,
				newcn->y_irregular_points->len_dimensions[0]);
			NhlSetSArg(&sargs[nargs++],NhlNtmYRNumIrregularPoints,
				newcn->y_irregular_points->len_dimensions[0]);
		}

		ret = _NhlALCreateChild(&tmpid,buffer,tickMarkLayerClass,
						(Layer)xnew,sargs,nargs);
		newcn->ticks = _NhlGetLayer(tmpid);
		if(newcn->ticks == NULL){
			NhlPError(FATAL,E_UNKNOWN,
			"%s:Unable to Create TickMark Object",error_lead);
			return FATAL;
		}
		return ret;
	}
	else{
		tmpid = xnew->contour.ticks->base.id;

		if(xold->view.x != xnew->view.x)
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,xnew->view.x);
		if(xold->view.y != xnew->view.y)
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,xnew->view.y);
		if(xold->view.width != xnew->view.width)
			NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,
							xnew->view.width);
		if(xold->view.height != xnew->view.height)
			NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,
							xnew->view.height);

		if((oldcn->x_reverse != newcn->x_reverse) ||
			(oldcn->x_min != newcn->x_min) ||
			(oldcn->x_max != newcn->x_max)){

			NhlSetSArg(&sargs[nargs++],NhlNtmXBDataLeftF,
				(newcn->x_reverse?newcn->x_max:newcn->x_min));
			NhlSetSArg(&sargs[nargs++],NhlNtmXBDataRightF,
				(!newcn->x_reverse?newcn->x_max:newcn->x_min));
			NhlSetSArg(&sargs[nargs++],NhlNtmXTDataLeftF,
				(newcn->x_reverse?newcn->x_max:newcn->x_min));
			NhlSetSArg(&sargs[nargs++],NhlNtmXTDataRightF,
				(!newcn->x_reverse?newcn->x_max:newcn->x_min));
		}
		if(oldcn->x_tension != newcn->x_tension) {
			NhlSetSArg(&sargs[nargs++],NhlNtmXBIrrTensionF,
							newcn->x_tension);
			NhlSetSArg(&sargs[nargs++],NhlNtmXTIrrTensionF,
							newcn->x_tension);
		}
		if(oldcn->y_tension != newcn->y_tension) {
			NhlSetSArg(&sargs[nargs++],NhlNtmYLIrrTensionF,
							newcn->y_tension);
			NhlSetSArg(&sargs[nargs++],NhlNtmYRIrrTensionF,
							newcn->y_tension);
		}
		if((oldcn->y_reverse != newcn->y_reverse) ||
			(oldcn->y_min != newcn->y_min) ||
			(oldcn->y_max != newcn->y_max)){

			NhlSetSArg(&sargs[nargs++],NhlNtmYLDataTopF,
				(!newcn->y_reverse?newcn->y_max:newcn->y_min));
			NhlSetSArg(&sargs[nargs++],NhlNtmYLDataBottomF,
				(newcn->y_reverse?newcn->y_max:newcn->y_min));
			NhlSetSArg(&sargs[nargs++],NhlNtmYRDataTopF,
				(!newcn->y_reverse?newcn->y_max:newcn->y_min));
			NhlSetSArg(&sargs[nargs++],NhlNtmYRDataBottomF,
				(newcn->y_reverse?newcn->y_max:newcn->y_min));
		}

		if(oldcn->x_style != newcn->x_style){
			NhlSetSArg(&sargs[nargs++],NhlNtmXBStyle,
								newcn->x_style);
			NhlSetSArg(&sargs[nargs++],NhlNtmXTStyle,
								newcn->x_style);
		}
		if(oldcn->y_style != newcn->y_style){
			NhlSetSArg(&sargs[nargs++],NhlNtmYLStyle,
								newcn->y_style);
			NhlSetSArg(&sargs[nargs++],NhlNtmYRStyle,
								newcn->y_style);
		}

		if(oldcn->x_irregular_points != newcn->x_irregular_points){
			if(newcn->x_irregular_points != NULL){
			NhlSetSArg(&sargs[nargs++],NhlNtmXBIrregularPoints,
					newcn->x_irregular_points->data);
			NhlSetSArg(&sargs[nargs++],NhlNtmXTIrregularPoints,
					newcn->x_irregular_points->data);
			NhlSetSArg(&sargs[nargs++],NhlNtmXBNumIrregularPoints,
				newcn->x_irregular_points->len_dimensions[0]);
			NhlSetSArg(&sargs[nargs++],NhlNtmXTNumIrregularPoints,
				newcn->x_irregular_points->len_dimensions[0]);
			}
			else{
			NhlSetSArg(&sargs[nargs++],NhlNtmXBIrregularPoints,
									NULL);
			NhlSetSArg(&sargs[nargs++],NhlNtmXTIrregularPoints,
									NULL);
			NhlSetSArg(&sargs[nargs++],NhlNtmXBNumIrregularPoints,
									0);
			NhlSetSArg(&sargs[nargs++],NhlNtmXTNumIrregularPoints,
									0);
			}
		}

		if(oldcn->y_irregular_points != newcn->y_irregular_points){
			if(newcn->y_irregular_points != NULL){
			NhlSetSArg(&sargs[nargs++],NhlNtmYLIrregularPoints,
					newcn->y_irregular_points->data);
			NhlSetSArg(&sargs[nargs++],NhlNtmYRIrregularPoints,
					newcn->y_irregular_points->data);
			NhlSetSArg(&sargs[nargs++],NhlNtmYLNumIrregularPoints,
				newcn->y_irregular_points->len_dimensions[0]);
			NhlSetSArg(&sargs[nargs++],NhlNtmYRNumIrregularPoints,
				newcn->y_irregular_points->len_dimensions[0]);
			}
			else{
			NhlSetSArg(&sargs[nargs++],NhlNtmYLIrregularPoints,
									NULL);
			NhlSetSArg(&sargs[nargs++],NhlNtmYRIrregularPoints,
									NULL);
			NhlSetSArg(&sargs[nargs++],NhlNtmYLNumIrregularPoints,
									0);
			NhlSetSArg(&sargs[nargs++],NhlNtmYRNumIrregularPoints,
									0);
			}
		}
		return _NhlALSetValuesChild(tmpid,(Layer)xnew,sargs,nargs);
	}
}

/*
 * Function:	SetUpTitles
 *
 * Description: Sets and Creates Title object. _NhlCreateChild is used to
 *		create the titles.  The title resources *OffsetXF and *OffsetYF
 *		are intercepted by the Contour object so adjustments can be
 *		made to make sure that the titles are centered over the
 *		Contour's viewport but do not overlap with tick mark labels.
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
(
	ContourLayer	xnew,
	ContourLayer	xold,
	_NhlCallType	calledfrom
) 
#else 
(xnew,xold,calledfrom)
	ContourLayer	xnew;
	ContourLayer	xold;
	_NhlCallType	calledfrom;
#endif
{
	ContourLayerPart	*oldcn = NULL;
	ContourLayerPart	*newcn = &xnew->contour;
	int		tmpid = -1;
	NhlBoundingBox	abox;
	char		buffer[MAXFNAMELEN];
	float		xtmp,ytmp,widthtmp,heighttmp;
	char		*error_lead;
	NhlErrorTypes	ret = NOERROR;

	if(calledfrom == CREATE){
		error_lead = "ContourInitialize";
	}
	else{
		oldcn = &xold->contour;

		if(calledfrom == SET){
			error_lead = "ContourSetValues";
		}
		else if(calledfrom == DATACHANGE){
		/*
		 * If we are coming from UpdateData - The only resources that
		 * could have changed are min and max - if they haven't changed
		 * return immediately.
		 */
			if((newcn->x_min == oldcn->x_min) &&
				(newcn->x_max == oldcn->x_max) &&
				(newcn->y_min == oldcn->y_min) &&
				(newcn->y_max == oldcn->y_max)){
				return NOERROR;
			}
			error_lead = "ContourUpdateData";
		}
		else{
			NhlPError(FATAL,E_UNKNOWN,"Bad Call");
			return FATAL;
		}
	}

	if(xnew->contour.ticks != NULL)
		NhlGetBB(xnew->contour.ticks->base.id,&abox);
	else
		NhlGetBB(xnew->base.id,&abox);
	xtmp = abox.l;
	ytmp = abox.t;
	widthtmp = abox.r - abox.l;
	heighttmp = abox.t - abox.b;
	
	switch(xnew->contour.ti_main_position) {
	case CENTER:
		xnew->contour.real_main_offset_x = xnew->contour.ti_main_offset_x
			+ ((xnew->view.x + xnew->view.width/2.0)
			- (xtmp + widthtmp/2.0));
		break;
	case LEFT:
		xnew->contour.real_main_offset_x = xnew->contour.ti_main_offset_x 
			+ (xnew->view.x - xtmp);
		break;
	case RIGHT:
		xnew->contour.real_main_offset_x = xnew->contour.ti_main_offset_x
			+ ((xnew->view.x + xnew->view.width) 
			- (xtmp + widthtmp));
		break;
	}
	switch(xnew->contour.ti_x_axis_position) {
	case CENTER:
		xnew->contour.real_x_axis_offset_x = 
			xnew->contour.ti_x_axis_offset_x 
			+ ((xnew->view.x + xnew->view.width/2.0)
			- (xtmp + widthtmp/2.0));
		break;
	case LEFT:
		xnew->contour.real_x_axis_offset_x = 
			xnew->contour.ti_x_axis_offset_x 
			+ (xnew->view.x - xtmp);
		break;
	case RIGHT:
		xnew->contour.real_x_axis_offset_x = 
			xnew->contour.ti_x_axis_offset_x 
			+ ((xnew->view.x + xnew->view.width) 
			- (xtmp + widthtmp));
		break;
	}
	switch(xnew->contour.ti_y_axis_position) {
	case CENTER:
		xnew->contour.real_y_axis_offset_y = 
			xnew->contour.ti_y_axis_offset_y 
			+ ((xnew->view.y - xnew->view.height/2.0)
			- (ytmp - heighttmp/2.0));
		break;
	case TOP:
		xnew->contour.real_y_axis_offset_y = 
			xnew->contour.ti_y_axis_offset_y 
			+ (xnew->view.y - ytmp);
		break;
	case BOTTOM:
		xnew->contour.real_y_axis_offset_y = 
			xnew->contour.ti_y_axis_offset_y 
			+ ((xnew->view.y - xnew->view.height) 
			- (ytmp - heighttmp));
		break;
	}


	if((calledfrom == CREATE) || (xnew->contour.ttitles == NULL)){	
		strcpy(buffer,xnew->base.name);
		strcat(buffer,".Title");
		ret = _NhlCreateChild(&tmpid,buffer,titleLayerClass,(Layer)xnew,
			NhlNvpXF,xtmp,
			NhlNvpYF,ytmp,
			NhlNvpWidthF,widthtmp,
			NhlNvpHeightF,heighttmp,
			NhlNtiMainOffsetXF,xnew->contour.real_main_offset_x,
			NhlNtiYAxisOffsetYF,xnew->contour.real_y_axis_offset_y,
			NhlNtiXAxisOffsetXF,xnew->contour.real_x_axis_offset_x,
			NhlNtiXAxisPosition,xnew->contour.ti_x_axis_position,
			NhlNtiYAxisPosition,xnew->contour.ti_y_axis_position,
			NhlNtiMainPosition,xnew->contour.ti_main_position,
			NULL);
	} else {
		tmpid = xnew->contour.ttitles->base.id;
		ret = _NhlSetValuesChild(tmpid,
			(Layer)xnew,
			NhlNvpXF,xtmp,
			NhlNvpYF,ytmp,
			NhlNvpWidthF,widthtmp,
			NhlNvpHeightF,heighttmp,
			NhlNtiMainOffsetXF,xnew->contour.real_main_offset_x,
			NhlNtiYAxisOffsetYF,xnew->contour.real_y_axis_offset_y,
			NhlNtiXAxisOffsetXF,xnew->contour.real_x_axis_offset_x,
			NhlNtiXAxisPosition,xnew->contour.ti_x_axis_position,
			NhlNtiYAxisPosition,xnew->contour.ti_y_axis_position,
			NhlNtiMainPosition,xnew->contour.ti_main_position,
			NULL);
	}
	if((tmpid > -1)||(ret >= WARNING)) {
		xnew->contour.ttitles = _NhlGetLayer(tmpid);
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"%s: Could not create Titles",error_lead);
		return(FATAL);
	} 
}
