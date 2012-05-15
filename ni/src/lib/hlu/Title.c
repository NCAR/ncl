/*
 *      $Id: Title.c,v 1.42 2000-08-30 00:38:04 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Title.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Nov 19 10:36:59 MST 1992
 *
 *	Description:	Draws 3 title
 */

#include <ncarg/hlu/TitleP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <math.h>

static char	Main[] = "Main";
static char	XAxis[] = "XAxis";
static char	YAxis[] = "YAxis";

#define Oset(field) NhlOffset(NhlTitleLayerRec,title.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNtiDeltaF, NhlCtiDeltaF, NhlTFloat, sizeof(float),
		 Oset(delta), NhlTString,_NhlUSET( "1.5" ),0,NULL},
	{NhlNtiMainFontColor,NhlCFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),
		Oset(main_font_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{NhlNtiMainFontQuality, NhlCFontQuality, NhlTFontQuality,
		 sizeof(NhlFontQuality),Oset(main_font_quality),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlHIGH ),0,NULL},
	{NhlNtiUseMainAttributes,NhlCtiUseMainAttributes, NhlTBoolean,
		 sizeof(NhlBoolean), Oset(use_main_attributes),NhlTImmediate,
		_NhlUSET(False),0,NULL},
	{NhlNtiMainString, NhlCtiMainString,NhlTString,sizeof(char*),
		 Oset(main_string),NhlTImmediate,_NhlUSET((NhlPointer)Main),
		0,(NhlFreeFunc)NhlFree},
	{NhlNtiMainJust, NhlCTextJustification, NhlTJustification,
		sizeof(NhlJustification),Oset(main_just),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNtiMainFont, NhlCFont, NhlTFont, sizeof(NhlFont),
	 	Oset(main_font),
	 	NhlTImmediate,_NhlUSET((NhlPointer)21),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(main_font_height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtiMainFontHeightF,NhlCFontHeightF,NhlTFloat,sizeof(float),
	 	Oset(main_font_height), 
	 	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtiMainFontAspectF,NhlCFontAspectF,NhlTFloat,sizeof(float),
		 Oset(main_font_aspect), NhlTString,_NhlUSET("1.3125"),0,NULL},
	{NhlNtiMainFontThicknessF,NhlCFontThicknessF,NhlTFloat,
		 sizeof(float), Oset(main_font_thickness),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNtiMainAngleF,NhlCTextAngleF,NhlTFloat, sizeof(float),
		 Oset(main_angle), NhlTString,_NhlUSET("0.00"),0,NULL},
	{NhlNtiMainDirection,NhlCTextDirection,NhlTTextDirection,
		 sizeof(NhlTextDirection),
		 Oset(main_direction), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlACROSS),0,NULL},
	{NhlNtiMainPosition,NhlCtiMainPosition,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(main_position), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlCENTER),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(main_on_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtiMainOn,NhlCtiMainOn,NhlTBoolean, sizeof(NhlBoolean),
		Oset(main_on),
	 	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtiMainSide,NhlCtiMainSide,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(main_side),NhlTImmediate,_NhlUSET((NhlPointer)NhlTOP),
		0,NULL},
	{NhlNtiMainConstantSpacingF, NhlCTextConstantSpacingF,NhlTFloat,
		 sizeof(float), Oset(main_constant_spacing),
		 NhlTString,_NhlUSET("0.0" ), 0,NULL},
	{NhlNtiMainFuncCode, NhlCTextFuncCode, NhlTCharacter,sizeof(char),
		 Oset(main_func_code), NhlTString,_NhlUSET( "~" ),0,NULL},
	{NhlNtiMainOffsetXF, NhlCtiMainOffsetXF, NhlTFloat,sizeof(float),
		 Oset(main_offset_x), NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNtiMainOffsetYF, NhlCtiMainOffsetYF, NhlTFloat,sizeof(float),
		 Oset(main_offset_y), NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNtiXAxisFontColor,NhlCFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(x_axis_font_color), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{NhlNtiXAxisFontQuality, NhlCFontQuality, NhlTFontQuality,
		 sizeof(NhlFontQuality),
		 Oset(x_axis_font_quality), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlHIGH ),0,NULL},
	{NhlNtiXAxisString, NhlCtiXAxisString,NhlTString,sizeof(char*),
		 Oset(x_axis_string),NhlTImmediate,
		 _NhlUSET((NhlPointer)XAxis),0,(NhlFreeFunc)NhlFree},
	{NhlNtiXAxisJust, NhlCTextJustification, NhlTJustification,
		sizeof(NhlJustification),
		Oset(x_axis_just),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlCENTERCENTER),0,NULL},
	{NhlNtiXAxisFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		 Oset(x_axis_font),NhlTImmediate,_NhlUSET((NhlPointer)21 ),
		0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(x_axis_font_height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtiXAxisFontHeightF,NhlCFontHeightF,
		NhlTFloat,sizeof(float),
		Oset(x_axis_font_height),
	 	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtiXAxisFontAspectF,NhlCFontAspectF,NhlTFloat,
		 sizeof(float),
		 Oset(x_axis_font_aspect), NhlTString,_NhlUSET("1.3125"),
		0,NULL},
	{NhlNtiXAxisFontThicknessF,NhlCFontThicknessF,NhlTFloat,
		 sizeof(float),
		 Oset(x_axis_font_thickness), NhlTString,_NhlUSET("1.0"),
		0,NULL},
	{NhlNtiXAxisAngleF,NhlCTextAngleF,NhlTFloat,sizeof(float),
		 Oset(x_axis_angle), NhlTString,_NhlUSET("0.0"),
		0,NULL},
	{NhlNtiXAxisDirection,NhlCTextDirection,NhlTTextDirection,
		 sizeof(NhlTextDirection),Oset(x_axis_direction),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlACROSS), 0,NULL},
	{NhlNtiXAxisPosition,NhlCtiXAxisPosition,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(x_axis_position), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlCENTER),0,NULL},
	{NhlNtiXAxisConstantSpacingF, NhlCTextConstantSpacingF,NhlTFloat,
		 sizeof(float),
		 Oset(x_axis_constant_spacing), NhlTString,_NhlUSET("0.0" ),
		0,NULL},
	{NhlNtiXAxisFuncCode, NhlCTextFuncCode, NhlTCharacter,sizeof(char),
		 Oset(x_axis_func_code), NhlTString,_NhlUSET( "~" ),0,NULL},
	{NhlNtiXAxisOffsetXF, NhlCtiXAxisOffsetXF, NhlTFloat,sizeof(float),
		 Oset(x_axis_offset_x), NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNtiXAxisOffsetYF, NhlCtiXAxisOffsetYF, NhlTFloat,sizeof(float),
		 Oset(x_axis_offset_y), NhlTString,_NhlUSET("0.0"),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(x_axis_on_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtiXAxisOn,NhlCtiXAxisOn,NhlTBoolean, sizeof(NhlBoolean),
		 Oset(x_axis_on), NhlTProcedure,
		 _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtiXAxisSide,NhlCtiXAxisSide,NhlTTitlePositions, 
		 sizeof(NhlTitlePositions),
		 Oset(x_axis_side), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlBOTTOM),0,NULL},
	{NhlNtiYAxisFontColor,NhlCFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(y_axis_font_color), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{NhlNtiYAxisFontQuality, NhlCFontQuality, NhlTFontQuality,
		 sizeof(NhlFontQuality),
		 Oset(y_axis_font_quality), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlHIGH),0,NULL},
	{NhlNtiYAxisString, NhlCtiYAxisString,NhlTString,sizeof(NhlString),
		 Oset(y_axis_string),NhlTImmediate,
		 _NhlUSET((NhlPointer)YAxis),0,(NhlFreeFunc)NhlFree},
	{NhlNtiYAxisJust, NhlCYAxisTextJustification, NhlTJustification,
		sizeof(NhlJustification),Oset(y_axis_just),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlCENTERCENTER ),0,NULL},
	{NhlNtiYAxisFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		 Oset(y_axis_font),NhlTImmediate,
		 _NhlUSET((NhlPointer)21 ),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(y_axis_font_height_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtiYAxisFontHeightF,NhlCFontHeightF,
		 NhlTFloat,sizeof(float),
		 Oset(y_axis_font_height),
	 	NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtiYAxisFontAspectF,NhlCFontAspectF,NhlTFloat,
		 sizeof(float),
		 Oset(y_axis_font_aspect),NhlTString,
	 	 _NhlUSET("1.3125"),0,NULL},
	{NhlNtiYAxisFontThicknessF,NhlCFontThicknessF,NhlTFloat,
		 sizeof(float), Oset(y_axis_font_thickness), NhlTString,
		 _NhlUSET("1.0"),0,NULL},
	{NhlNtiYAxisAngleF,NhlCYAxisTextAngleF,NhlTFloat, sizeof(float),
		 Oset(y_axis_angle), NhlTString,_NhlUSET("90.0"),0,NULL},
	{NhlNtiYAxisDirection,NhlCYAxisTextDirection,NhlTTextDirection,
		 sizeof(NhlTextDirection),
		 Oset(y_axis_direction), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlACROSS),0,NULL},
	{NhlNtiYAxisPosition,NhlCtiYAxisPosition,NhlTTitlePositions,
		 sizeof(NhlTitlePositions),
		 Oset(y_axis_position), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlCENTER),0,NULL},
	{NhlNtiYAxisConstantSpacingF, NhlCTextConstantSpacingF,NhlTFloat,
		 sizeof(float),
		 Oset(y_axis_constant_spacing), NhlTString,_NhlUSET("0.0" ),
		0,NULL},
	{NhlNtiYAxisFuncCode, NhlCTextFuncCode, NhlTCharacter,sizeof(char),
		 Oset(y_axis_func_code), NhlTString,_NhlUSET( "~" ),0,NULL},
	{NhlNtiYAxisOffsetXF, NhlCtiYAxisOffsetXF, NhlTFloat,sizeof(float),
		 Oset(y_axis_offset_x), NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNtiYAxisOffsetYF, NhlCtiYAxisOffsetYF, NhlTFloat,sizeof(float),
		 Oset(y_axis_offset_y), NhlTString,_NhlUSET("0.0"),0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(y_axis_on_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{NhlNtiYAxisOn,NhlCtiYAxisOn,NhlTBoolean, sizeof(NhlBoolean),
		 Oset(y_axis_on), NhlTProcedure,
		 _NhlUSET((NhlPointer)_NhlResUnset),0,NULL},
	{NhlNtiYAxisSide,NhlCtiYAxisSide,NhlTTitlePositions, 
		 sizeof(NhlTitlePositions),
		 Oset(y_axis_side), NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlLEFT),0,NULL}

/* End-documented-resources */

	};
#undef Oset

/*
* Base Methods used
*/
static NhlErrorTypes    TitleSetValues(
#if NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes TitleGetValues(
#if	NhlNeedProto
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
#endif
);

static NhlErrorTypes    TitleInitialize(
#if NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes     TitleDestroy(
#if NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes    TitleClassInitialize();


static NhlErrorTypes TitleGetBB(
#if NhlNeedProto
        NhlLayer          /* instance */,
        NhlBoundingBox * /*thebox*/
#endif
);

static NhlErrorTypes TitleDraw(
#if NhlNeedProto
        NhlLayer           instance
#endif
);



NhlTitleClassRec NhltitleClassRec = {
        {
/* class_name                   */      "titleClass",
/* nrm_class                    */      NrmNULLQUARK,
/* layer_size                   */      sizeof(NhlTitleLayerRec),
/* class_inited                 */      False,
/* superclass                   */      (NhlClass)&NhlviewClassRec,
/* cvt_table			*/	NULL,

/* layer_resources              */      resources,
/* num_resources                */      NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize        */      NULL,
/* class_initialize             */      TitleClassInitialize,
/* layer_initialize             */      TitleInitialize,
/* layer_set_values             */      TitleSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values             */      TitleGetValues,
/* layer_reparent               */      NULL,
/* layer_destroy                */      TitleDestroy,

/* child_resources              */      NULL,

/* layer_draw                   */      TitleDraw,

/* layer_pre_draw               */      NULL,
/* layer_draw_segonly           */      NULL,
/* layer_post_draw              */      NULL,
/* layer_clear                  */      NULL
        },
	{
/* segment_workstation */ -1,
/*get_bb*/	TitleGetBB
	},
	{
	NULL
	}
};

NhlClass NhltitleClass = (NhlClass)&NhltitleClassRec;

/*
 * Function:	nhlftitleclass
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
_NHLCALLF(nhlftitleclass,NHLFTITLECLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhltitleClass;
}

static NrmQuark	Qmain = NrmNULLQUARK;
static NrmQuark	Qxaxis = NrmNULLQUARK;
static NrmQuark	Qyaxis = NrmNULLQUARK;

/*
 * Function:	TitleClassInitialize
 *
 * Description:	
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	NONE
 *
 * Side Effects:	NhlNOERROR
 */
static NhlErrorTypes    TitleClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{
	_NhlEnumVals	titlepos[] = {
		{NhlTOP,	"Top"},
		{NhlBOTTOM,	"Bottom"},
		{NhlLEFT,	"Left"},
		{NhlRIGHT,	"Right"},
		{NhlCENTER,	"Center"}
	};

	_NhlRegisterEnumType(NhlbaseClass,NhlTTitlePositions,titlepos,
							NhlNumber(titlepos));

	Qmain = NrmStringToQuark(NhlNtiMainString);
	Qxaxis = NrmStringToQuark(NhlNtiXAxisString);
	Qyaxis = NrmStringToQuark(NhlNtiYAxisString);

	return(NhlNOERROR);
}

static NhlErrorTypes    ManageTitleTextItems
#if	NhlNeedProto
(
        NhlTitleLayer tnew,
        NhlBoolean    init
        )
#else
(tnew,init)
        NhlTitleLayer tnew,
        NhlBoolean    init
#endif

{
	NhlErrorTypes ret = NhlNOERROR,ret1 = NhlNOERROR;
	float tmpxy,tmpwh,tmpxy1,tmpwh1,main_location;
        NhlBoolean encroaches;
        char *entry;

/*
 * The TextItem is set initially without setting the XOffsetF for the Y
 * Axis Title or the YOffsetF for the X Axis and Main titles. Then the
 * position is retrieved and adjustments are made to ensure that the Title
 * does not intrude into the Title viewport because of text justification,
 * angle, or direction settings. Then the offset is added in unconditionally.
 */
 
        entry = init ? "TitleInitialize" : "TitleSetValues" ;
        
	switch(tnew->title.y_axis_side) {
                case NhlRIGHT:
                        switch(tnew->title.y_axis_position) {
                                case NhlTOP:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                + tnew->title.y_axis_offset_y;
                                        break;
                                case NhlBOTTOM:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                - tnew->view.height
                                                + tnew->title.y_axis_offset_y;
                                        break;
                                default:
					NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Y Axis title can only be positioned on NhlTOP, NhlBOTTOM, or NhlCENTER, defaulting to NhlCENTER",entry);
                                        tnew->title.y_axis_position =
                                                NhlCENTER;
                                        ret = MIN(ret,NhlWARNING);
                                case NhlCENTER:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                - (tnew->view.height/2.0)
                                                +tnew->title.y_axis_offset_y;
                                        break;
                        }
                        tnew->title.y_axis_pos_x =
                                tnew->view.x + tnew->view.width
                                +(tnew->title.delta
                                  *tnew->title.y_axis_font_height);
                        break;
                default:
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Y Axis title can only appear on NhlLEFT or NhlRIGHT side of plot, using NhlLEFT",entry);
                        tnew->title.y_axis_side = NhlLEFT;
                        ret = MIN(ret,NhlWARNING);
                case NhlLEFT:
                        switch(tnew->title.y_axis_position) {
                                case NhlTOP:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                + tnew->title.y_axis_offset_y;
                                        break;
                                case NhlBOTTOM:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                - tnew->view.height
                                                + tnew->title.y_axis_offset_y;
                                        break;
                                default:
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Y Axis title can only be positioned on NhlTOP, NhlBOTTOM, or NhlCENTER, defaulting to NhlCENTER",entry);
                                        tnew->title.y_axis_position =
                                                NhlCENTER;
                                        ret = MIN(ret,NhlWARNING);
                                case NhlCENTER:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                - (tnew->view.height/2.0)
                                                +tnew->title.y_axis_offset_y;
                                        break;
                        }
                        tnew->title.y_axis_pos_x =
                                tnew->view.x
                                -(tnew->title.delta
                                  * tnew->title.y_axis_font_height);
                        break;
        }
        if (init) {
                char buffer[_NhlMAXRESNAMLEN];
/*
 * Dot in name restricts user from specifing resources by name for this object
 * in the resource file.
 */
                strcpy(buffer,tnew->base.name);
                strcat(buffer,".YAxis");
                ret1 = NhlVACreate
                        (&(tnew->title.y_axis_id),
                         buffer,NhltextItemClass,tnew->base.id,
                         NhlNtxFont,tnew->title.y_axis_font,
                         NhlNtxString,tnew->title.y_axis_string,
                         NhlNtxPosXF,tnew->title.y_axis_pos_x,
                         NhlNtxPosYF,tnew->title.y_axis_pos_y,
                         NhlNtxDirection,tnew->title.y_axis_direction,
                         NhlNtxAngleF,tnew->title.y_axis_angle,
                         NhlNtxJust,tnew->title.y_axis_just,
                         NhlNtxFontColor,tnew->title.y_axis_font_color,
                         NhlNtxFontHeightF,tnew->title.y_axis_font_height,
                         NhlNtxFontAspectF,tnew->title.y_axis_font_aspect,
                         NhlNtxConstantSpacingF,
                         tnew->title.y_axis_constant_spacing,
                         NhlNtxFontQuality,tnew->title.y_axis_font_quality,
                         NhlNtxFuncCode,tnew->title.y_axis_func_code,
                         NhlNtxFontThicknessF,
                         tnew->title.y_axis_font_thickness,
                         NULL);
        }
        else {
                ret1 = NhlVASetValues
                        (tnew->title.y_axis_id,
                         NhlNtxFont,tnew->title.y_axis_font,
                         NhlNtxString,tnew->title.y_axis_string,
                         NhlNtxPosXF,tnew->title.y_axis_pos_x,
                         NhlNtxPosYF,tnew->title.y_axis_pos_y,
                         NhlNtxDirection,tnew->title.y_axis_direction,
                         NhlNtxAngleF,tnew->title.y_axis_angle,
                         NhlNtxJust,tnew->title.y_axis_just,
                         NhlNtxFontColor,tnew->title.y_axis_font_color,
                         NhlNtxFontHeightF,tnew->title.y_axis_font_height,
                         NhlNtxFontAspectF,tnew->title.y_axis_font_aspect,
                         NhlNtxConstantSpacingF,
                         tnew->title.y_axis_constant_spacing,
                         NhlNtxFontQuality,tnew->title.y_axis_font_quality,
                         NhlNtxFuncCode,tnew->title.y_axis_func_code,
                         NhlNtxFontThicknessF,
                         tnew->title.y_axis_font_thickness,
                         NULL);
        }
        
        ret = MIN(ret1,ret);
/*
* Now need to check to make sure text does not encroach upon viewport
*/
	NhlVAGetValues(tnew->title.y_axis_id,
                       NhlNvpXF,&tmpxy,
                       NhlNvpWidthF,&tmpwh,NULL);
        
        encroaches = False;
	if(tnew->title.y_axis_side == NhlLEFT) {
		if (tmpxy+tmpwh > tnew->view.x)
			tnew->title.y_axis_pos_x -=
                                (tmpxy+tmpwh) - tnew->view.x;
                tnew->title.y_axis_pos_x += tnew->title.y_axis_offset_x;
                if (tnew->title.y_axis_pos_x + tmpwh > tnew->view.x)
                        encroaches = True;
	}
        else {
		if (tmpxy < tnew->view.x + tnew->view.width)
			tnew->title.y_axis_pos_x +=
                                (tnew->view.x + tnew->view.width) - tmpxy;
                tnew->title.y_axis_pos_x += tnew->title.y_axis_offset_x;
                if (tnew->title.y_axis_pos_x < tnew->view.x + tnew->view.width)
                        encroaches = True;
        }
        NhlVASetValues(tnew->title.y_axis_id,
                       NhlNtxPosXF,tnew->title.y_axis_pos_x,
                       NULL);
        if (encroaches) {
                NhlPError(NhlINFO,NhlEUNKNOWN,
                          "%s: Y Axis title may obscure other plot elements",
                          entry);
                ret = MIN(ret,NhlINFO);
        }
	
	switch(tnew->title.x_axis_side) {
                case NhlTOP:
                        switch(tnew->title.x_axis_position) {
                                case NhlRIGHT:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                +tnew->view.width
                                                +tnew->title.x_axis_offset_x;
                                        break;
                                case NhlLEFT:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                +tnew->title.x_axis_offset_x;
                                        break;
                                default:
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER",entry);
                                        tnew->title.x_axis_position =
                                                NhlCENTER;
					ret = MIN(ret,NhlWARNING);
                                case NhlCENTER:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                + (tnew->view.width/2.0)
                                                +tnew->title.x_axis_offset_x;
                                        break;
                        }
                        tnew->title.x_axis_pos_y = tnew->view.y
                                + (tnew->title.delta
                                   * tnew->title.x_axis_font_height);
                        break;
                default:
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: X Axis title can only appear on NhlTOP or NhlBOTTOM side of plot, using NhlBOTTOM",entry);
                        tnew->title.x_axis_side = NhlBOTTOM;
                        ret = MIN(ret,NhlWARNING);
                case NhlBOTTOM:
                        switch(tnew->title.x_axis_position) {
                                case NhlRIGHT:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                +tnew->view.width
                                                +tnew->title.x_axis_offset_x;
                                        break;
                                case NhlLEFT:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                +tnew->title.x_axis_offset_x;
                                        break;
                                default:
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER",entry);
                                        tnew->title.x_axis_position =
                                                NhlCENTER;
                                        ret = MIN(ret,NhlWARNING);
                                case NhlCENTER:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                + (tnew->view.width/2.0)
                                                +tnew->title.x_axis_offset_x;
                                        break;
                        }
                        tnew->title.x_axis_pos_y = tnew->view.y
                                - tnew->view.height
                                - (tnew->title.delta
                                   * tnew->title.x_axis_font_height);
                        break;
        }
        if (init) {
                char buffer[_NhlMAXRESNAMLEN];
                
                strcpy(buffer,tnew->base.name);
                strcat(buffer,".XAxis");
                ret1 = NhlVACreate
                        (&(tnew->title.x_axis_id),
                         buffer,NhltextItemClass,tnew->base.id,
                         NhlNtxFont,tnew->title.x_axis_font,
                         NhlNtxString,tnew->title.x_axis_string,
                         NhlNtxPosXF,tnew->title.x_axis_pos_x,
                         NhlNtxPosYF,tnew->title.x_axis_pos_y,
                         NhlNtxDirection,tnew->title.x_axis_direction,
                         NhlNtxAngleF,tnew->title.x_axis_angle,
                         NhlNtxJust,tnew->title.x_axis_just,
                         NhlNtxFontColor,tnew->title.x_axis_font_color,
                         NhlNtxFontHeightF,tnew->title.x_axis_font_height,
                         NhlNtxFontAspectF,tnew->title.x_axis_font_aspect,
                         NhlNtxConstantSpacingF,
                         tnew->title.x_axis_constant_spacing,
                         NhlNtxFontQuality,tnew->title.x_axis_font_quality,
                         NhlNtxFuncCode,tnew->title.x_axis_func_code,
                         NhlNtxFontThicknessF,
                         tnew->title.x_axis_font_thickness,
                         NULL);
        }
        else {
                ret1 = NhlVASetValues
                        (tnew->title.x_axis_id,
                         NhlNtxFont,tnew->title.x_axis_font,
                         NhlNtxString,tnew->title.x_axis_string,
                         NhlNtxPosXF,tnew->title.x_axis_pos_x,
                         NhlNtxPosYF,tnew->title.x_axis_pos_y,
                         NhlNtxDirection,tnew->title.x_axis_direction,
                         NhlNtxAngleF,tnew->title.x_axis_angle,
                         NhlNtxJust,tnew->title.x_axis_just,
                         NhlNtxFontColor,tnew->title.x_axis_font_color,
                         NhlNtxFontHeightF,tnew->title.x_axis_font_height,
                         NhlNtxFontAspectF,tnew->title.x_axis_font_aspect,
                         NhlNtxConstantSpacingF,
                         tnew->title.x_axis_constant_spacing,
                         NhlNtxFontQuality,tnew->title.x_axis_font_quality,
                         NhlNtxFuncCode,tnew->title.x_axis_func_code,
                         NhlNtxFontThicknessF,
                         tnew->title.x_axis_font_thickness,
                         NULL);
        }
        ret = MIN(ret1,ret);
/*
* Need to check to make sure xaxis title doesn't encroach on viewport
*/
	NhlVAGetValues(tnew->title.x_axis_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);

	if(tnew->title.x_axis_side == NhlTOP) {
		if(tmpxy - tmpwh < tnew->view.y )
			tnew->title.x_axis_pos_y +=
                                tnew->view.y - (tmpxy - tmpwh );
                tnew->title.x_axis_pos_y += tnew->title.x_axis_offset_y;
                if (tnew->title.x_axis_pos_y - tmpwh < tnew->view.y)
                        encroaches = True;
        }
        else {
		if (tmpxy > tnew->view.y - tnew->view.height)
			tnew->title.x_axis_pos_y -=
                                tmpxy - (tnew->view.y - tnew->view.height);
                tnew->title.x_axis_pos_y += tnew->title.x_axis_offset_y;
                if (tnew->title.x_axis_pos_y >
                    tnew->view.y - tnew->view.height)
                        encroaches = True;
        }
        NhlVASetValues(tnew->title.x_axis_id,
                       NhlNtxPosYF,tnew->title.x_axis_pos_y,
                       NULL);
        if (encroaches) {
                NhlPError(NhlINFO,NhlEUNKNOWN,
                          "%s: X Axis title may obscure other plot elements",
                          entry);
                ret = MIN(ret,NhlINFO);
        }

	switch(tnew->title.main_side) {
                case NhlBOTTOM:
                        main_location = tnew->view.y - tnew->view.height;
                        if((tnew->title.x_axis_side == NhlBOTTOM)
                                &&(tnew->title.x_axis_on)) {
                                NhlVAGetValues(tnew->title.x_axis_id,
                                        NhlNvpYF,&tmpxy,
                                        NhlNvpHeightF,&tmpwh,NULL);
                                main_location = MIN
                                        (tmpxy - tmpwh,main_location);
                        }
                        switch(tnew->title.main_position) {
                                case NhlRIGHT:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                +tnew->view.width
                                                +tnew->title.main_offset_x;
                                        break;
                                case NhlLEFT:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                +tnew->title.main_offset_x;
                                        break;
                                default:
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Main title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER",entry);
                                        tnew->title.main_position = NhlCENTER;
                                        ret = MIN(ret,NhlWARNING);
                                case NhlCENTER:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                + (tnew->view.width/2.0)
                                                +tnew->title.main_offset_x;
                                        break;
                        }
                        tnew->title.main_pos_y = main_location
                                - (tnew->title.delta
                                   * tnew->title.main_font_height);
                        break;
                default:
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Main title can only appear on NhlTOP or NhlBOTTOM side of plot, defaulting to NhlTOP",entry);
                        tnew->title.main_side = NhlTOP;
                        ret = MIN(ret,NhlWARNING);
                case NhlTOP:
                        main_location = tnew->view.y;
                        if((tnew->title.x_axis_side == NhlTOP)
                                &&(tnew->title.x_axis_on)) {
                                NhlVAGetValues(tnew->title.x_axis_id,
                                        NhlNvpYF,&tmpxy,NULL);
                                main_location = MAX(tmpxy,main_location);
                        } 
                        switch(tnew->title.main_position) {
                                case NhlRIGHT:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                +tnew->view.width
                                                +tnew->title.main_offset_x;
                                        break;
                                case NhlLEFT:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                +tnew->title.main_offset_x;
                                        break;
                                default:
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER",entry);
                                        tnew->title.main_position = NhlCENTER;
                                        ret = MIN(ret,NhlWARNING);
                                case NhlCENTER:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                + (tnew->view.width/2.0)
                                                +tnew->title.main_offset_x;
                                        break;
                        }
                        tnew->title.main_pos_y = main_location
                                + (tnew->title.delta
                                   * tnew->title.main_font_height);
                        break;
        }
        if (init) {
                char buffer[_NhlMAXRESNAMLEN];

                strcpy(buffer,tnew->base.name);
                strcat(buffer,".Main");
                ret1 = NhlVACreate
                        (&(tnew->title.main_id),
                         buffer,NhltextItemClass,tnew->base.id,
                         NhlNtxFont,tnew->title.main_font,
                         NhlNtxString,tnew->title.main_string,
                         NhlNtxPosXF,tnew->title.main_pos_x,
                         NhlNtxPosYF,tnew->title.main_pos_y,
                         NhlNtxDirection,tnew->title.main_direction,
                         NhlNtxAngleF,tnew->title.main_angle,
                         NhlNtxJust,tnew->title.main_just,
                         NhlNtxFontColor,tnew->title.main_font_color,
                         NhlNtxFontHeightF,tnew->title.main_font_height,
                         NhlNtxFontAspectF,tnew->title.main_font_aspect,
                         NhlNtxConstantSpacingF,
                         tnew->title.main_constant_spacing,
                         NhlNtxFontQuality,tnew->title.main_font_quality,
                         NhlNtxFuncCode,tnew->title.main_func_code,
                         NhlNtxFontThicknessF,tnew->title.main_font_thickness,
                         NULL);
        }
        else {
                ret1 = NhlVASetValues
                        (tnew->title.main_id,
                         NhlNtxFont,tnew->title.main_font,
                         NhlNtxString,tnew->title.main_string,
                         NhlNtxPosXF,tnew->title.main_pos_x,
                         NhlNtxPosYF,tnew->title.main_pos_y,
                         NhlNtxDirection,tnew->title.main_direction,
                         NhlNtxAngleF,tnew->title.main_angle,
                         NhlNtxJust,tnew->title.main_just,
                         NhlNtxFontColor,tnew->title.main_font_color,
                         NhlNtxFontHeightF,tnew->title.main_font_height,
                         NhlNtxFontAspectF,tnew->title.main_font_aspect,
                         NhlNtxConstantSpacingF,
                         tnew->title.main_constant_spacing,
                         NhlNtxFontQuality,tnew->title.main_font_quality,
                         NhlNtxFuncCode,tnew->title.main_func_code,
                         NhlNtxFontThicknessF,tnew->title.main_font_thickness,
                         NULL);
        }
        
        ret = MIN(ret1,ret);
/*
* Need to check to make sure a) doesnt overlap with xaxis title b) doesn't
* encroach on viewport
*/
	NhlVAGetValues(tnew->title.main_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);
        
	if(tnew->title.x_axis_side == tnew->title.main_side &&
	   tnew->title.x_axis_on) {
		NhlVAGetValues(tnew->title.x_axis_id,
			NhlNvpYF,&tmpxy1,
			NhlNvpHeightF,&tmpwh1,NULL);
                if (tnew->title.main_side == NhlTOP) {
                        tmpxy1 = MIN(tnew->view.y,tmpxy1);
                }
                else {
                        if (tmpxy1 - tmpwh1 >
                            tnew->view.y - tnew->view.height) {
                                tmpxy1 = tnew->view.y;
                                tmpwh1 = tnew->view.height;
                        }
                }
	} else {
		tmpxy1 = tnew->view.y;
		tmpwh1 = tnew->view.height;
	}

	if(tnew->title.main_side == NhlTOP) {
		if((tmpxy - tmpwh) < tmpxy1)
			tnew->title.main_pos_y += tmpxy1 - (tmpxy - tmpwh);
                tnew->title.main_pos_y += tnew->title.main_offset_y;
                if (tnew->title.main_pos_y - tmpwh < tmpxy1)
                        encroaches = True;
        }
        else {
               if(tmpxy > (tmpxy1 - tmpwh1))
                       tnew->title.main_pos_y -= tmpxy - (tmpxy1 - tmpwh1);
               tnew->title.main_pos_y += tnew->title.main_offset_y;
               if (tnew->title.main_pos_y > (tmpxy1 - tmpwh1))
                       encroaches = True;
        }
        NhlVASetValues(tnew->title.main_id,
                       NhlNtxPosYF,tnew->title.main_pos_y,
                       NULL);
        if (encroaches) {
                NhlPError(NhlINFO,NhlEUNKNOWN,
                         "%s: Main Axis title may obscure other plot elements",
                          entry);
                ret = MIN(ret,NhlINFO);
        }

	return(ret);
}

static NhlErrorTypes    CheckAndAdjustAttrs
#if	NhlNeedProto
(
        NhlTitleLayer tnew,
        NhlBoolean    init
        )
#else
(tnew,init)
        NhlTitleLayer tnew,
        NhlBoolean    init
#endif

{
	NhlErrorTypes ret = NhlNOERROR;
        char *entry, *e_text;
 
        entry = init ? "TitleInitialize" : "TitleSetValues" ;

/*
 * use_main_attributes is set then most of the Main text attributes are
 * copied to the  coresponding X and Y axis fields. Direction and angle
 * resources are not copied, because the Y Axis generally needs different
 * values for these.
 */
	if( tnew->title.use_main_attributes ) {
		tnew->title.x_axis_font = tnew->title.y_axis_font =
			tnew->title.main_font;
		tnew->title.y_axis_just = tnew->title.x_axis_just =
			tnew->title.main_just;
		tnew->title.y_axis_font_height =
                        tnew->title.x_axis_font_height =
			tnew->title.main_font_height;
		tnew->title.y_axis_font_aspect =
                        tnew->title.x_axis_font_aspect =
			tnew->title.main_font_aspect;
		tnew->title.y_axis_font_thickness =
			tnew->title.x_axis_font_thickness=
			tnew->title.main_font_thickness;
                tnew->title.y_axis_font_quality =
                        tnew->title.x_axis_font_quality =
                        tnew->title.main_font_quality;
                tnew->title.y_axis_font_color =
                        tnew->title.x_axis_font_color =
                        tnew->title.main_font_color;
		tnew->title.y_axis_constant_spacing = 
			tnew->title.x_axis_constant_spacing =
			tnew->title.main_constant_spacing;
		tnew->title.y_axis_func_code = 
			tnew->title.x_axis_func_code =
			tnew->title.main_func_code;
	}
/*
 * Check constant spacing values
 */

	e_text = 
		"%s: Constant spacing cannot be less than zero, defaulting %s";
	if (tnew->title.main_constant_spacing < 0.0) {
		tnew->title.main_constant_spacing = 0.0;
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry,
			  NhlNtiMainConstantSpacingF);
	}
	if (tnew->title.x_axis_constant_spacing < 0.0) {
		tnew->title.x_axis_constant_spacing = 0.0;
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry,
			  NhlNtiXAxisConstantSpacingF);
	}
	if (tnew->title.y_axis_constant_spacing < 0.0) {
		tnew->title.y_axis_constant_spacing = 0.0;
		ret = MIN(ret,NhlWARNING);
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry,
			  NhlNtiYAxisConstantSpacingF);
	}
        return ret;
        
}

/*
 * Function:	TitleInitialize
 *
 * Description: Initializes the title object. This is complicated because 
 *		titles can be placed centered or at the corners of their 
 *		respective axis as well as on either side. Furthermore the
 *		main title has to be positioned so as not to overlap the 
 *		X axis label which can possibly be on the same side of the
 *		xywidthheight viewport. 
 *
 *		This object places three TextItems arround the x,y,width and
 *		height specified by the the view resources for this object.
 *		It is important to note that the view resources don't define
 *		the viewport for this object.
 *		
 *
 * In Args:	class		objects class pointer
 *		req		requested instance
 *		new		new instance record
 *		args		resource list just in case needed
 *		num_args	number of resources
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	NONE
 */
/*ARGSUSED*/
static NhlErrorTypes    TitleInitialize
#if	NhlNeedProto
(NhlClass class, NhlLayer req,NhlLayer new,_NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        NhlClass	class;
        NhlLayer	req;
        NhlLayer	new;
        _NhlArgList	args;
        int		num_args;
#endif
{
	NhlTitleLayer tnew = (NhlTitleLayer) new;
	NhlTitleLayer treq = (NhlTitleLayer) req;
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;

	tnew->title.new_draw_req = True;
	tnew->title.trans_dat = NULL;
	tnew->title.delta = (float)fabs((double)tnew->title.delta);

	if (!tnew->title.main_font_height_set)
		tnew->title.main_font_height = NhlDEF_TITLE_HEIGHT;
	if (!tnew->title.x_axis_font_height_set)
		tnew->title.x_axis_font_height = NhlDEF_TITLE_HEIGHT;
	if (!tnew->title.y_axis_font_height_set)
		tnew->title.y_axis_font_height = NhlDEF_TITLE_HEIGHT;

	if(tnew->title.main_string != Main){
                tnew->title.main_string = (char*)NhlMalloc((unsigned)
                                strlen(tnew->title.main_string)+1);
                strcpy(tnew->title.main_string,treq->title.main_string);
		if (! tnew->title.main_on_set)
			tnew->title.main_on = True;
	}
	else if (! tnew->title.main_on_set) {
		tnew->title.main_on = False;
	}
	if(tnew->title.x_axis_string != XAxis){
                tnew->title.x_axis_string = (char*)NhlMalloc((unsigned)
                                strlen(tnew->title.x_axis_string)+1);
                strcpy(tnew->title.x_axis_string,treq->title.x_axis_string);
		if (! tnew->title.x_axis_on_set)
			tnew->title.x_axis_on = True;
        }
	else if (! tnew->title.x_axis_on_set) {
		tnew->title.x_axis_on = False;
	}
	if(tnew->title.y_axis_string != YAxis){
                tnew->title.y_axis_string = (char*)NhlMalloc((unsigned)
                                strlen(tnew->title.y_axis_string)+1);
                strcpy(tnew->title.y_axis_string,treq->title.y_axis_string);
		if (! tnew->title.y_axis_on_set)
			tnew->title.y_axis_on = True;
        }
	else if (! tnew->title.y_axis_on_set) {
		tnew->title.y_axis_on = False;
	}

        ret1 = CheckAndAdjustAttrs(tnew,True);
        ret = MIN(ret1,ret);
        
        ret1 = ManageTitleTextItems(tnew,True);
	return(MIN(ret1,ret));
}
		
/*
 * Function:	TitleSetValues
 *
 * Description: Performs setvalues operations on title object. Specifically,
 *		if object is only moved, it scales those text attibutes which
 *		are not set in the current setvalues call. The text attributes
 *		that are scaled are height. The private fields that
 *		hold the possitions of the textitems are transformed using the
 *		children transformation that is available from the view class.
 *		Angles and aspect ratios are not affected.
 *		If an attribute is set then the new value is used instead of
 *		the scaled one. Essentially, this setvalues scales and 
 *		transforms all the values in old and assigns them to new. Then,
 *		it resets the values in new where the requested value is 
 *		different than the old field value.
 *
 * In Args:	old	old instance record
 *		reference	reference instance record
 *		new	new instance record
 *		args	args list just in case needed (unused here)
 *		num_args	number of arguments
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
/*ARGSUSED*/
static NhlErrorTypes    TitleSetValues
#if	NhlNeedProto
(NhlLayer old, NhlLayer reference, NhlLayer new, _NhlArgList args,int num_args)
#else
(old,reference,new,args,num_args)
        NhlLayer	old;
        NhlLayer	reference;
        NhlLayer	new;
        _NhlArgList	args;
        int		num_args;
#endif
{
	NhlTitleLayer tnew = (NhlTitleLayer) new;
	NhlTitleLayer tref = (NhlTitleLayer) reference;
	NhlTitleLayer told = (NhlTitleLayer) old;
	NhlErrorTypes ret = NhlNOERROR,ret1 = NhlNOERROR;
	float deltah;
	float deltaw;
	int		view_args = 0;

	if (tnew->view.use_segments != told->view.use_segments)
		tnew->title.new_draw_req = True;

	if (_NhlArgIsSet(args,num_args,NhlNvpXF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpYF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpWidthF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpHeightF)) view_args++;
	if (_NhlArgIsSet(args,num_args,NhlNvpOn)) view_args++;

	if (num_args > view_args ||
            ! _NhlSegmentSpansArea(tnew->title.trans_dat,
                                   tnew->view.x,
                                   tnew->view.x + tnew->view.width,
                                   tnew->view.y - tnew->view.height,
                                   tnew->view.y))
		tnew->title.new_draw_req = True;

        if (view_args) {

/*
* Since theses values have changed then the view has a transfomration 
* already set that can be used to compute the new location of the text items
*/
		_NhlEvalTrans(tnew->view.trans_children,
			tnew->title.main_pos_x,
			tnew->title.main_pos_y,
			&(tnew->title.main_pos_x),
			&(tnew->title.main_pos_y));
		_NhlEvalTrans(tnew->view.trans_children,
			tnew->title.x_axis_pos_x,
			tnew->title.x_axis_pos_y,
			&(tnew->title.x_axis_pos_x),
			&(tnew->title.x_axis_pos_y));
		_NhlEvalTrans(tnew->view.trans_children,
			tnew->title.y_axis_pos_x,
			tnew->title.y_axis_pos_y,
			&(tnew->title.y_axis_pos_x),
			&(tnew->title.y_axis_pos_y));
		deltaw = tnew->view.width/told->view.width;
		deltah = tnew->view.height/told->view.height;
		tnew->title.y_axis_font_height *= deltah;
		tnew->title.x_axis_font_height *= deltaw;
		tnew->title.main_font_height *= deltaw;

	}
/*
* Now everything is fine if only a move has happened however if other parameters
* have been set then the values must be copied from the tref instance to the
* tnew instance. These parameters are just ones that involve. If the actual
* string has changed for any title then a new location needs to be determined.
* if the main and xaxis title share the same side then any changes to xaxis
* may affect the main.
*/
	if(told->title.main_string != tnew->title.main_string) {
		if(told->title.main_string != Main)
			NhlFree(told->title.main_string);
		tnew->title.main_string = NhlMalloc((unsigned)
				strlen(tref->title.main_string)+1);
		strcpy(tnew->title.main_string,tref->title.main_string);
		if(!tnew->title.main_on &&
				!_NhlArgIsSet(args,num_args,NhlNtiMainOn)){
			tnew->title.main_on = True;
		}
	}
	if(told->title.x_axis_string != tnew->title.x_axis_string) {
		if(told->title.x_axis_string != XAxis)
			NhlFree(told->title.x_axis_string);
		tnew->title.x_axis_string = NhlMalloc((unsigned)
				strlen(tref->title.x_axis_string)+1);
		strcpy(tnew->title.x_axis_string,tref->title.x_axis_string);
		if(!tnew->title.x_axis_on &&
				!_NhlArgIsSet(args,num_args,NhlNtiXAxisOn)){
			tnew->title.x_axis_on = True;
		}
	}
	if(told->title.y_axis_string != tnew->title.y_axis_string) {
		if(told->title.y_axis_string != YAxis)
			NhlFree(told->title.y_axis_string);
		tnew->title.y_axis_string = NhlMalloc((unsigned)
				strlen(tref->title.y_axis_string)+1);
		strcpy(tnew->title.y_axis_string,tref->title.y_axis_string);
		if(!tnew->title.y_axis_on &&
				!_NhlArgIsSet(args,num_args,NhlNtiYAxisOn)){
			tnew->title.y_axis_on = True;
		}
	}
/*
* now determine is height or thickness has changed for each title. If it
* has copy value from the tref instance otherwise proceed
* Use of _NhlArgIsSet is required because height could have been set to same
* value but whole object shrunk anyways.
*/
	if(_NhlArgIsSet(args,num_args,NhlNtiMainFontHeightF)) {
		tnew->title.main_font_height = tref->title.main_font_height;
	}
	if(_NhlArgIsSet(args,num_args,NhlNtiMainFontThicknessF)) {
		tnew->title.main_font_thickness = tref->title.main_font_thickness;
	}
	if(_NhlArgIsSet(args,num_args,NhlNtiXAxisFontHeightF)) {
		tnew->title.x_axis_font_height = tref->title.x_axis_font_height;
	}
	if(_NhlArgIsSet(args,num_args,NhlNtiXAxisFontThicknessF)) {
		tnew->title.x_axis_font_thickness 
			= tref->title.x_axis_font_thickness;
	}
	if(_NhlArgIsSet(args,num_args,NhlNtiYAxisFontHeightF)) {
		tnew->title.y_axis_font_height = tref->title.y_axis_font_height;
	}
	if(_NhlArgIsSet(args,num_args,NhlNtiYAxisFontThicknessF)){
		tnew->title.y_axis_font_thickness 
			= tref->title.y_axis_font_thickness;
	}

	tnew->title.delta = (float)fabs((double)tnew->title.delta);

        ret1 = CheckAndAdjustAttrs(tnew,False);
        ret = MIN(ret1,ret);
        
        ret1 = ManageTitleTextItems(tnew,False);
        
	return(MIN(ret,ret1));
}

/*
 * Function:	TitleGetValues
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
TitleGetValues
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
	char			func[] = "TitleGetValues";
	NhlErrorTypes		ret = NhlNOERROR;
	NhlTitleLayerPart	*tip = &((NhlTitleLayer)l)->title;
	int			i;

	for(i=0;i<nargs;i++){

		if((args[i].quark == Qmain) && tip->main_string){
			*(NhlString*)args[i].value.ptrval =(NhlString)
					NhlMalloc(strlen(tip->main_string)+1);
			if(*(NhlString*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNtiMainString);
				ret = MIN(ret,NhlWARNING);
			}
			strcpy(*(NhlString*)args[i].value.ptrval,
							tip->main_string);
		}
		else if((args[i].quark == Qxaxis) && tip->x_axis_string){
			*(NhlString*)args[i].value.ptrval =(NhlString)
					NhlMalloc(strlen(tip->x_axis_string)+1);
			if(*(NhlString*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNtiXAxisString);
				ret = MIN(ret,NhlWARNING);
			}
			strcpy(*(NhlString*)args[i].value.ptrval,
							tip->x_axis_string);
		}
		else if((args[i].quark == Qyaxis) && tip->y_axis_string){
			*(NhlString*)args[i].value.ptrval =(NhlString)
					NhlMalloc(strlen(tip->y_axis_string)+1);
			if(*(NhlString*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNtiYAxisString);
				ret = MIN(ret,NhlWARNING);
			}
			strcpy(*(NhlString*)args[i].value.ptrval,
							tip->y_axis_string);
		}
	}

	return ret;
}

/*
 * Function:	TitleDestroy
 *
 * Description: Destroys all three TextItems used to create titles and frees
 *		all allocated memory.
 *
 * In Args:	inst	instance record
 *
 * Out Args:	NONE
 *
 * Return Values:	NhlNOERROR	
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes     TitleDestroy
#if	NhlNeedProto
(
        NhlLayer	inst
)
#else
(inst)
	NhlLayer	inst;
#endif
{
	NhlTitleLayer tinst = (NhlTitleLayer) inst;

	if(tinst->title.main_string != Main)
		NhlFree(tinst->title.main_string);
	if(tinst->title.x_axis_string != XAxis)
		NhlFree(tinst->title.x_axis_string);	
	if(tinst->title.y_axis_string != YAxis)
		NhlFree(tinst->title.y_axis_string);
	NhlDestroy(tinst->title.main_id);
	NhlDestroy(tinst->title.x_axis_id);
	NhlDestroy(tinst->title.y_axis_id);

	if (tinst->title.trans_dat != NULL)
		_NhlDeleteViewSegment(inst,tinst->title.trans_dat);

	return(NhlNOERROR);
}

/*
 * Function:	TitleDraw
 *
 * Description:	TitleDraw just calls NhlDraw on the three TextItem children.
 *
 * In Args:	instance 	object instance record
 *
 * Out Args:	NONE
 *
 * Return Values: 	ErrorConditions	
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes TitleDraw
#if	NhlNeedProto
(NhlLayer instance)
#else
(instance)
	NhlLayer	instance;
#endif
{
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *e_text;
	char *entry_name = "TitleDraw";
	NhlTitleLayer tinstance = (NhlTitleLayer) instance;

	if(! tinstance->title.main_on && 
	   ! tinstance->title.x_axis_on &&
	   ! tinstance->title.y_axis_on)
		return ret;

	if (tinstance->view.use_segments && ! tinstance->title.new_draw_req &&
	    tinstance->title.trans_dat &&
	    tinstance->title.trans_dat->id != NgNOT_A_SEGMENT) {
                subret = _NhlActivateWorkstation(tinstance->base.wkptr);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDrawSegment(tinstance->title.trans_dat,
				_NhlWorkstationId(tinstance->base.wkptr));
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlDeactivateWorkstation(tinstance->base.wkptr);
		return MIN(subret,ret);
	}

	NhlVASetValues(instance->base.wkptr->base.id,
		       _NhlNwkReset,	True,
		       NULL);

	tinstance->title.new_draw_req = False;

	if (tinstance->view.use_segments) {
		subret = _NhlActivateWorkstation(tinstance->base.wkptr);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		if (tinstance->title.trans_dat != NULL)
			_NhlDeleteViewSegment(instance, 
					      tinstance->title.trans_dat);
		if ((tinstance->title.trans_dat = 
		     _NhlNewViewSegment(instance)) == NULL) {
			e_text = "%s: error opening segment";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
			return(ret);
		}
		subret = _NhlStartSegment(tinstance->title.trans_dat);
		if ((ret = MIN(subret,ret)) < NhlWARNING)
			return ret;
	}
	if (tinstance->view.use_segments) {
		if(tinstance->title.main_on)
			_NhlSegDraw(_NhlGetLayer(tinstance->title.main_id));
		if(tinstance->title.x_axis_on)
			_NhlSegDraw(_NhlGetLayer(tinstance->title.x_axis_id));
		if(tinstance->title.y_axis_on)
			_NhlSegDraw(_NhlGetLayer(tinstance->title.y_axis_id));
		_NhlEndSegment(tinstance->title.trans_dat);
		_NhlDeactivateWorkstation(tinstance->base.wkptr);
	}
	else {
		if(tinstance->title.main_on)
			NhlDraw(tinstance->title.main_id);
		if(tinstance->title.x_axis_on)
			NhlDraw(tinstance->title.x_axis_id);
		if(tinstance->title.y_axis_on)
			NhlDraw(tinstance->title.y_axis_id);
	}

	return(NhlNOERROR);
}

/*
 * Function:    TitleGetBB
 *
 * Description: Makes sure TextItems outside of view resources are included in
 *              the Bounding Box .
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
static NhlErrorTypes TitleGetBB
#if	NhlNeedProto
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	NhlTitleLayer tinstance = (NhlTitleLayer) instance;
	float x0,y0,width,height;

	if(tinstance->title.main_on) {
		NhlVAGetValues(tinstance->title.main_id,
			       NhlNvpXF,&x0,
			       NhlNvpYF,&y0,
			       NhlNvpWidthF,&width,
			       NhlNvpHeightF, &height, NULL);

		_NhlAddBBInfo(y0,y0-height,x0+width,x0,thebox);
	}
	if(tinstance->title.x_axis_on) {
		NhlVAGetValues(tinstance->title.x_axis_id,
			       NhlNvpXF,&x0,
			       NhlNvpYF,&y0,
			       NhlNvpWidthF,&width,
			       NhlNvpHeightF, &height, NULL);
		
		_NhlAddBBInfo(y0,y0-height,x0+width,x0,thebox);
	}

	if(tinstance->title.y_axis_on) {
		NhlVAGetValues(tinstance->title.y_axis_id,
			       NhlNvpXF,&x0,
			       NhlNvpYF,&y0,
			       NhlNvpWidthF,&width,
			       NhlNvpHeightF, &height, NULL);
		
		_NhlAddBBInfo(y0,y0-height,x0+width,x0,thebox);
	}

	return(NhlNOERROR);
}
