/*
 *      $Id: Title.c,v 1.2 1993-06-03 15:12:10 ethan Exp $
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

#if	__STDC__
#include <stdlib.h>
#endif
#include <stdio.h>
#include <strings.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/TextItem.h>
#include <math.h>
#include <ncarg/hlu/TitleP.h>

#define DEFAULT_STRING "none"

static NhlResource resources[] = {
	{NhlNtiDeltaF, NhlCtiDeltaF, NhlTFloat, sizeof(float),
		NhlOffset(TitleLayerRec,title.delta), NhlTString, "1.5" },
	{NhlNtiMainFontColor,NhlCtiTitleFontColors,NhlTInteger,sizeof(int),
		NhlOffset(TitleLayerRec,title.main_font_color),
		NhlTString,"1"},
	{NhlNtiMainFontQuality, NhlCtiTitleFontQualities, NhlTFQuality,
		sizeof(FontQuality),
		NhlOffset(TitleLayerRec,title.main_font_quality),
		NhlTImmediate,(NhlPointer)HIGH },
	{NhlNtiUseMainAttributes,NhlCtiUseMainAttributes, NhlTInteger,
		sizeof(int),
		NhlOffset(TitleLayerRec,title.use_main_attributes),
		NhlTString,"0" },
	{NhlNtiMainString, NhlCtiMainString,NhlTString,sizeof(char*),
		NhlOffset(TitleLayerRec,title.main_string),NhlTString,DEFAULT_STRING},
	{NhlNtiMainJust, NhlCtiTitleJust, NhlTInteger,sizeof(int),
		NhlOffset(TitleLayerRec,title.main_just),NhlTString,"4" },
	{NhlNtiMainFont, NhlCtiTitleFonts, NhlTInteger, sizeof(int),
		NhlOffset(TitleLayerRec,title.main_font),NhlTString,"0" },
	{NhlNtiMainFontHeightF,NhlCtiTitleFontHeightsF,NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.main_font_height),
		NhlTString,"0.025"},
	{NhlNtiMainFontAspectF,NhlCtiTitleFontAspectsF,NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.main_font_aspect),
		NhlTString,"1.3125"},
	{NhlNtiMainFontThicknessF,NhlCtiTitleFontThicknessF,NhlTFloat,
		sizeof(float),
		NhlOffset(TitleLayerRec,title.main_font_thickness),
		NhlTString,"1.0"},
	{NhlNtiMainAngleF,NhlCtiTitleAnglesF,NhlTFloat,
		sizeof(float),
		NhlOffset(TitleLayerRec,title.main_angle),
		NhlTString,"0.00"},
	{NhlNtiMainDirection,NhlCtiMainDirection,NhlTTextDirection,
		sizeof(TextDirection),
		NhlOffset(TitleLayerRec,title.main_direction),
		NhlTImmediate,(NhlPointer)ACROSS},
	{NhlNtiMainPosition,NhlCtiMainPosition,NhlTTitlePositions,
		sizeof(TitlePositions),
		NhlOffset(TitleLayerRec,title.main_position),
		NhlTImmediate,(NhlPointer)CENTER},
	{NhlNtiMainOn,NhlCtiMainOn,NhlTInteger, sizeof(int),
		NhlOffset(TitleLayerRec,title.main_on),
		NhlTString,"1"},
	{NhlNtiMainSide,NhlCtiMainSide,NhlTTitlePositions, 
		sizeof(TitlePositions),
		NhlOffset(TitleLayerRec,title.main_side),
		NhlTImmediate,(NhlPointer)TOP},
	{NhlNtiMainConstantSpacingF, NhlCtiTitleConstantSpacingsF,NhlTFloat,
		sizeof(float),
		NhlOffset(TitleLayerRec,title.main_constant_spacing),
		NhlTString,"0.0" },
	{NhlNtiMainFuncCode, NhlCtiTitleFuncCodes, NhlTCharacter,sizeof(char),
		NhlOffset(TitleLayerRec,title.main_func_code),
		NhlTString, ":" },
	{NhlNtiMainOffsetXF, NhlCtiMainOffsetXF, NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.main_offset_x),
		NhlTString,"0.0"},
	{NhlNtiMainOffsetYF, NhlCtiMainOffsetYF, NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.main_offset_y),
		NhlTString,"0.0"},
	{NhlNtiXAxisFontColor,NhlCtiTitleFontColors,NhlTInteger,sizeof(int),
		NhlOffset(TitleLayerRec,title.x_axis_font_color),
		NhlTString,"1"},
	{NhlNtiXAxisFontQuality, NhlCtiTitleFontQualities, NhlTFQuality,
		sizeof(FontQuality),
		NhlOffset(TitleLayerRec,title.x_axis_font_quality),
		NhlTImmediate,(NhlPointer)HIGH },
	{NhlNtiXAxisString, NhlCtiXAxisString,NhlTString,sizeof(char*),
		NhlOffset(TitleLayerRec,title.x_axis_string),NhlTString,DEFAULT_STRING },
	{NhlNtiXAxisJust, NhlCtiTitleJust, NhlTInteger,sizeof(int),
		NhlOffset(TitleLayerRec,title.x_axis_just),NhlTString,"4" },
	{NhlNtiXAxisFont, NhlCtiTitleFonts, NhlTInteger, sizeof(int),
		NhlOffset(TitleLayerRec,title.x_axis_font),NhlTString,"0" },
	{NhlNtiXAxisFontHeightF,NhlCtiTitleFontHeightsF,NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.x_axis_font_height),
		NhlTString,"0.025"},
	{NhlNtiXAxisFontAspectF,NhlCtiTitleFontAspectsF,NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.x_axis_font_aspect),
		NhlTString,"1.3125"},
	{NhlNtiXAxisFontThicknessF,NhlCtiTitleFontThicknessF,NhlTFloat,
		sizeof(float),
		NhlOffset(TitleLayerRec,title.x_axis_font_thickness),
		NhlTString,"1.0"},
	{NhlNtiXAxisAngleF,NhlCtiTitleAnglesF,NhlTFloat,
		sizeof(float),
		NhlOffset(TitleLayerRec,title.x_axis_angle),
		NhlTString,"0.0"},
	{NhlNtiXAxisDirection,NhlCtiXAxisDirection,NhlTTextDirection,
		sizeof(TextDirection),
		NhlOffset(TitleLayerRec,title.x_axis_direction),
		NhlTImmediate,(NhlPointer)ACROSS},
	{NhlNtiXAxisPosition,NhlCtiXAxisPosition,NhlTTitlePositions,
		sizeof(TitlePositions),
		NhlOffset(TitleLayerRec,title.x_axis_position),
		NhlTImmediate,(NhlPointer)CENTER},
	{NhlNtiXAxisConstantSpacingF, NhlCtiTitleConstantSpacingsF,NhlTFloat,
		sizeof(float),
		NhlOffset(TitleLayerRec,title.x_axis_constant_spacing),
		NhlTString,"0.0" },
	{NhlNtiXAxisFuncCode, NhlCtiTitleFuncCodes, NhlTCharacter,sizeof(char),
		NhlOffset(TitleLayerRec,title.x_axis_func_code),
		NhlTString, ":" },
	{NhlNtiXAxisOffsetXF, NhlCtiXAxisOffsetXF, NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.x_axis_offset_x),
		NhlTString,"0.0"},
	{NhlNtiXAxisOffsetYF, NhlCtiXAxisOffsetYF, NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.x_axis_offset_y),
		NhlTString,"0.0"},
	{NhlNtiXAxisOn,NhlCtiXAxisOn,NhlTInteger, sizeof(int),
		NhlOffset(TitleLayerRec,title.x_axis_on),
		NhlTString,"1"},
	{NhlNtiXAxisSide,NhlCtiXAxisSide,NhlTTitlePositions, 
		sizeof(TitlePositions),
		NhlOffset(TitleLayerRec,title.x_axis_side),
		NhlTImmediate,(NhlPointer)BOTTOM},
	{NhlNtiYAxisFontColor,NhlCtiTitleFontColors,NhlTInteger,sizeof(int),
		NhlOffset(TitleLayerRec,title.y_axis_font_color),
		NhlTString,"1"},
	{NhlNtiYAxisFontQuality, NhlCtiTitleFontQualities, NhlTFQuality,
		sizeof(FontQuality),
		NhlOffset(TitleLayerRec,title.y_axis_font_quality),
		NhlTImmediate,(NhlPointer)HIGH},
	{NhlNtiYAxisString, NhlCtiYAxisString,NhlTString,sizeof(char*),
		NhlOffset(TitleLayerRec,title.y_axis_string),NhlTString,DEFAULT_STRING},
	{NhlNtiYAxisJust, NhlCtiTitleJust, NhlTInteger,sizeof(int),
		NhlOffset(TitleLayerRec,title.y_axis_just),NhlTString,"4" },
	{NhlNtiYAxisFont, NhlCtiTitleFonts, NhlTInteger, sizeof(int),
		NhlOffset(TitleLayerRec,title.y_axis_font),NhlTString,"0" },
	{NhlNtiYAxisFontHeightF,NhlCtiTitleFontHeightsF,NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.y_axis_font_height),
		NhlTString,"0.025"},
	{NhlNtiYAxisFontAspectF,NhlCtiTitleFontAspectsF,NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.y_axis_font_aspect),
		NhlTString,"1.3125"},
	{NhlNtiYAxisFontThicknessF,NhlCtiTitleFontThicknessF,NhlTFloat,
		sizeof(float),
		NhlOffset(TitleLayerRec,title.y_axis_font_thickness),
		NhlTString,"1.0"},
	{NhlNtiYAxisAngleF,NhlCtiTitleAnglesF,NhlTFloat,
		sizeof(float),
		NhlOffset(TitleLayerRec,title.y_axis_angle),
		NhlTString,"90.0"},
	{NhlNtiYAxisDirection,NhlCtiYAxisDirection,NhlTTextDirection,
		sizeof(TextDirection),
		NhlOffset(TitleLayerRec,title.y_axis_direction),
		NhlTImmediate,(NhlPointer)ACROSS},
	{NhlNtiYAxisPosition,NhlCtiYAxisPosition,NhlTTitlePositions,
		sizeof(TitlePositions),
		NhlOffset(TitleLayerRec,title.y_axis_position),
		NhlTImmediate,(NhlPointer)CENTER},
	{NhlNtiYAxisConstantSpacingF, NhlCtiTitleConstantSpacingsF,NhlTFloat,
		sizeof(float),
		NhlOffset(TitleLayerRec,title.y_axis_constant_spacing),
		NhlTString,"0.0" },
	{NhlNtiYAxisFuncCode, NhlCtiTitleFuncCodes, NhlTCharacter,sizeof(char),
		NhlOffset(TitleLayerRec,title.y_axis_func_code),
		NhlTString, ":" },
	{NhlNtiYAxisOffsetXF, NhlCtiYAxisOffsetXF, NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.y_axis_offset_x),
		NhlTString,"0.0"},
	{NhlNtiYAxisOffsetYF, NhlCtiYAxisOffsetYF, NhlTFloat,sizeof(float),
		NhlOffset(TitleLayerRec,title.y_axis_offset_y),
		NhlTString,"0.0"},
	{NhlNtiYAxisOn,NhlCtiXAxisOn,NhlTInteger, sizeof(int),
		NhlOffset(TitleLayerRec,title.y_axis_on),
		NhlTString,"1"},
	{NhlNtiYAxisSide,NhlCtiYAxisSide,NhlTTitlePositions, 
		sizeof(TitlePositions),
		NhlOffset(TitleLayerRec,title.y_axis_side),
		NhlTImmediate,(NhlPointer)LEFT}
};

/*
* Base Methods used
*/
static NhlErrorTypes    TitleSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);
static NhlErrorTypes    TitleInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes     TitleDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes    TitleClassInitialize();


static NhlErrorTypes TitleGetBB(
#ifdef NhlNeedProto
        Layer          /* instance */,
        NhlBoundingBox * /*thebox*/
#endif
);

static NhlErrorTypes TitleDraw(
#ifdef NhlNeedProto
        Layer           instance
#endif
);



TitleLayerClassRec titleLayerClassRec = {
        {
/* superclass                   */      (LayerClass)&viewLayerClassRec,
/* class_name                   */      "Title",
/* nrm_class                    */      NrmNULLQUARK,
/* layer_size                   */      sizeof(TitleLayerRec),
/* layer_resources              */      resources,
/* num_resources                */      NhlNumber(resources),
/* child_resources              */      NULL,
/* all_resources                */      NULL,
/* class_part_initialize        */      NULL,
/* class_inited                 */      False,
/* class_initialize             */      TitleClassInitialize,
/* layer_initialize             */      TitleInitialize,
/* layer_set_values             */      TitleSetValues,
/* layer_set_values_not         */      NULL,
/* layer_get_values             */      NULL,
/* layer_pre_draw               */      NULL,
/* layer_draw                   */      TitleDraw,
/* layer_draw_segonly           */      NULL,
/* layer_post_draw              */      NULL,
/* layer_clear                  */      NULL,
/* layer_destroy                */      TitleDestroy
        },
	{
/* segment_workstation */ -1,
/*get_bb*/	TitleGetBB
	},
	{
	NULL
	}
};

LayerClass titleLayerClass = (LayerClass)&titleLayerClassRec;

		
	
		
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
#if	__STDC__
(Layer old, Layer reference, Layer new, _NhlArgList args,int num_args)
#else
(old,reference,new,args,num_args)
        Layer		old;
        Layer		reference;
        Layer		new;
        _NhlArgList	args;
        int		num_args;
#endif
{
	TitleLayer tnew = (TitleLayer) new;
	TitleLayer tref = (TitleLayer) reference;
	TitleLayer told = (TitleLayer) old;
	NhlErrorTypes ret = NOERROR,ret1 = NOERROR;
	float tmpxy,tmpwh,tmpxy1,tmpwh1,main_location;
	float deltah;
	float deltaw;

        if((tnew->view.x != told->view.x)
                ||(tnew->view.width != told->view.width)
                ||(tnew->view.y != told->view.y)
                ||(tnew->view.height != told->view.height)){

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
		tnew->title.y_axis_font_thickness *= deltah;
		tnew->title.x_axis_font_height *= deltaw;
		tnew->title.x_axis_font_thickness*= deltaw;
		tnew->title.main_font_height *= deltaw;
		tnew->title.main_font_thickness*= deltaw;

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
		NhlFree(told->title.main_string);
		tnew->title.main_string = NhlMalloc((unsigned)
				strlen(tref->title.main_string)+1);
		strcpy(tnew->title.main_string,tref->title.main_string);
	}
	if(told->title.x_axis_string != tnew->title.x_axis_string) {
		NhlFree(told->title.x_axis_string);
		tnew->title.x_axis_string = NhlMalloc((unsigned)
				strlen(tref->title.x_axis_string)+1);
		strcpy(tnew->title.x_axis_string,tref->title.x_axis_string);
	}
	if(told->title.y_axis_string != tnew->title.y_axis_string) {
		NhlFree(told->title.y_axis_string);
		tnew->title.y_axis_string = NhlMalloc((unsigned)
				strlen(tref->title.y_axis_string)+1);
		strcpy(tnew->title.y_axis_string,tref->title.y_axis_string);
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

        if( tnew->title.use_main_attributes ) {
                tnew->title.x_axis_font = tnew->title.y_axis_font =
                        tnew->title.main_font;
                tnew->title.y_axis_just = tnew->title.x_axis_just =
                        tnew->title.main_just;
                tnew->title.y_axis_font_height =tnew->title.x_axis_font_height =
                        tnew->title.main_font_height;
                tnew->title.y_axis_font_aspect =tnew->title.x_axis_font_aspect =
                        tnew->title.main_font_aspect;
                tnew->title.y_axis_font_thickness =
                        tnew->title.x_axis_font_thickness=
                        tnew->title.main_font_thickness;
                tnew->title.y_axis_angle = tnew->title.x_axis_angle =
                        tnew->title.main_angle;
                tnew->title.y_axis_angle = tnew->title.x_axis_angle =
                        tnew->title.main_angle;
                tnew->title.y_axis_constant_spacing =
                        tnew->title.x_axis_constant_spacing =
                        tnew->title.main_constant_spacing;
                tnew->title.y_axis_func_code =
                        tnew->title.x_axis_func_code =
                        tnew->title.main_func_code;
        }
/*
* Just repeating same spacing computations that were performed in the
* initialize function because its too much of a pain to selectively
* determine which values need to be updated, based on new parameters. 
*/

	switch(tnew->title.y_axis_side) {
                case RIGHT:
                        switch(tnew->title.y_axis_position) {
                                case TOP:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                + tnew->title.y_axis_offset_y;
                                        break;
                                case BOTTOM:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                - tnew->view.height
                                                + tnew->title.y_axis_offset_y;
                                        break;
                                default:
					NhlPError(WARNING,E_UNKNOWN,"TitleSetValues: Y Axis title can only be positioned on TOP, BOTTOM, or CENTER, defaulting to CENTER");
                                        ret = WARNING;
                                case CENTER:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                - (tnew->view.height/2.0)
                                                +tnew->title.y_axis_offset_y;
                                        break;
                        }
                        tnew->title.y_axis_pos_x =
                                tnew->view.x + tnew->view.width
                                +(tnew->title.delta
                                *tnew->title.y_axis_font_height)
                                +tnew->title.y_axis_offset_x;
                        break;
                default:
			NhlPError(WARNING,E_UNKNOWN,"TitleSetValues: Y Axis title can only appear on LEFT or RIGHT side of plot, using LEFT");
                        ret = WARNING;
                case LEFT:
                        switch(tnew->title.y_axis_position) {
                                case TOP:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                + tnew->title.y_axis_offset_y;
                                        break;
                                case BOTTOM:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                - tnew->view.height
                                                + tnew->title.y_axis_offset_y;
                                        break;
                                default:
                                        NhlPError(WARNING,E_UNKNOWN,"TitleSetValues: Y Axis title can only be positioned on TOP, BOTTOM, or CENTER, defaulting to CENTER");
                                        ret = WARNING;
                                case CENTER:
                                        tnew->title.y_axis_pos_y =
                                                tnew->view.y
                                                - (tnew->view.height/2.0)
                                                +tnew->title.y_axis_offset_y;
                                        break;
                        }
                        tnew->title.y_axis_pos_x =
                                tnew->view.x
                                -(tnew->title.delta
                                * tnew->title.y_axis_font_height)
                                +tnew->title.y_axis_offset_x;
                        break;
        }
	ret1 = NhlSetValues(tnew->title.y_axis_id,
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
                NhlNtxConstantSpacingF,tnew->title.y_axis_constant_spacing,
                NhlNtxFontQuality,tnew->title.y_axis_font_quality,
                NhlNtxFuncCode,tnew->title.y_axis_func_code,
                NhlNtxFontThicknessF,tnew->title.y_axis_font_thickness,
                NULL);
/*
* Now need to check to make sure text does not encroach upon viewport
*/
	NhlGetValues(tnew->title.y_axis_id,
		NhlNvpXF,&tmpxy,
		NhlNvpWidthF,&tmpwh,NULL);

	if(tnew->title.y_axis_side == LEFT) {
		if(tmpxy+tmpwh > tnew->view.x){
			tnew->title.y_axis_pos_x -= (tmpxy+tmpwh) 
				- tnew->view.x + (tnew->title.y_axis_font_height*tnew->title.delta);
			NhlSetValues(tnew->title.y_axis_id,
				NhlNtxPosXF,tnew->title.y_axis_pos_x,
				NULL);
		}
	} else {
		if(tmpxy < tnew->view.x + tnew->view.width) {
			tnew->title.y_axis_pos_x += (tnew->view.x 
					+ tnew->view.width)
                                        - tmpxy + (tnew->title.y_axis_font_height*tnew->title.delta);

			NhlSetValues(tnew->title.y_axis_id,
				NhlNtxPosXF, tnew->title.y_axis_pos_x ,
				NULL);
		}
	}

	
	switch(tnew->title.x_axis_side) {
                case TOP:
                        switch(tnew->title.x_axis_position) {
                                case RIGHT:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                +tnew->view.width
                                                +tnew->title.x_axis_offset_x;
                                        break;
                                case LEFT:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                +tnew->title.x_axis_offset_x;
                                        break;
                                default:
                                        NhlPError(WARNING,E_UNKNOWN,"TitleSetValues: X Axis title can only appear on RIGHT, LEFT, or CENTER side, defaulting to CENTER");
					ret= WARNING;
                                case CENTER:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                + (tnew->view.width/2.0)
                                                +tnew->title.x_axis_offset_x;
                                        break;
                        }
                        tnew->title.x_axis_pos_y = tnew->view.y
                                + (tnew->title.delta
                                * tnew->title.x_axis_font_height)
                                + tnew->title.x_axis_offset_y;
                        break;
                default:
                        NhlPError(WARNING,E_UNKNOWN,"TitleSetValues: X Axis title can only appear on TOP or BOTTOM side of plot, using BOTTOM");
                        ret = WARNING;
                case BOTTOM:
                        switch(tnew->title.x_axis_position) {
                                case RIGHT:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                +tnew->view.width
                                                +tnew->title.x_axis_offset_x;
                                        break;
                                case LEFT:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                +tnew->title.x_axis_offset_x;
                                        break;
                                default:
                                        NhlPError(WARNING,E_UNKNOWN,"TitleSetValues: X Axis title can only appear on RIGHT, LEFT, or CENTER side, defaulting to CENTER");
                                        ret = WARNING;
                                case CENTER:
                                        tnew->title.x_axis_pos_x =
                                                tnew->view.x
                                                + (tnew->view.width/2.0)
                                                +tnew->title.x_axis_offset_x;
                                        break;
                        }
                        tnew->title.x_axis_pos_y = tnew->view.y
                                - tnew->view.height
                                - (tnew->title.delta
                                * tnew->title.x_axis_font_height)
                                + tnew->title.x_axis_offset_y;
                        break;
        }
	ret1 = NhlSetValues(tnew->title.x_axis_id,
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
                NhlNtxConstantSpacingF,tnew->title.x_axis_constant_spacing,
                NhlNtxFontQuality,tnew->title.x_axis_font_quality,
                NhlNtxFuncCode,tnew->title.x_axis_func_code,
                NhlNtxFontThicknessF,tnew->title.x_axis_font_thickness,
                NULL);
/*
* Need to check to make sure xaxis title doesn't encroach on viewport
*/
	NhlGetValues(tnew->title.x_axis_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);

	if(tnew->title.x_axis_side == TOP) {
		if(tmpxy - tmpwh < tnew->view.y ){
			tnew->title.x_axis_pos_y += tnew->view.y - (tmpxy - tmpwh ) + (tnew->title.delta*tnew->title.x_axis_font_height);
			NhlSetValues(tnew->title.x_axis_id,
				NhlNtxPosYF,tnew->title.x_axis_pos_y,
				NULL);
		}
	} else {
		if(tmpxy > tnew->view.y - tnew->view.height) {
			tnew->title.x_axis_pos_y -= tmpxy - (tnew->view.y - tnew->view.height)+(tnew->title.delta*tnew->title.x_axis_font_height);

			NhlSetValues(tnew->title.x_axis_id,
				NhlNtxPosYF, tnew->title.x_axis_pos_y ,
				NULL);
		}
	}

	switch(tnew->title.main_side) {
                case BOTTOM:
                        if((tnew->title.x_axis_side == BOTTOM)
                                &&(tnew->title.x_axis_on)) {
                                NhlGetValues(tnew->title.x_axis_id,
                                        NhlNvpYF,&tmpxy,
                                        NhlNvpHeightF,&tmpwh,NULL);
                                main_location = tmpxy - tmpwh;
                        } else {
                                main_location = tnew->view.y
                                        - tnew->view.height;
                        }
                        switch(tnew->title.main_position) {
                                case RIGHT:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                +tnew->view.width
                                                +tnew->title.main_offset_x;
                                        break;
                                case LEFT:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                +tnew->title.main_offset_x;
                                        break;
                                default:
                                        NhlPError(WARNING,E_UNKNOWN,"TitleSetValues: X Axis title can only appear on RIGHT, LEFT, or CENTER side, defaulting to CENTER");
                                        ret = WARNING;
                                case CENTER:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                + (tnew->view.width/2.0)
                                                +tnew->title.main_offset_x;
                                        break;
                        }
                        tnew->title.main_pos_y = main_location
                                - (tnew->title.delta
                                * tnew->title.main_font_height)
                                + tnew->title.main_offset_y;
                        break;
                default:
                        NhlPError(WARNING,E_UNKNOWN,"TitleSetValues: Main title can only appear on TOP or BOTTOM side of plot, defaulting to TOP");
                        ret = WARNING;
                case TOP:
                        if((tnew->title.x_axis_side == TOP)
                                &&(tnew->title.x_axis_on)) {
                                NhlGetValues(tnew->title.x_axis_id,
                                        NhlNvpYF,&tmpxy,NULL);
                                main_location = tmpxy;
                        } else {
                                main_location = tnew->view.y;
                        }
                        switch(tnew->title.main_position) {
                                case RIGHT:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                +tnew->view.width
                                                +tnew->title.main_offset_x;
                                        break;
                                case LEFT:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                +tnew->title.main_offset_x;
                                        break;
                                default:
                                        NhlPError(WARNING,E_UNKNOWN,"TitleSetValues: X Axis title can only appear on RIGHT, LEFT, or CENTER side, defaulting to CENTER");
                                        ret = WARNING;
                                case CENTER:
                                        tnew->title.main_pos_x =
                                                tnew->view.x
                                                + (tnew->view.width/2.0)
                                                +tnew->title.main_offset_x;
                                        break;
                        }
                        tnew->title.main_pos_y = main_location
                                + (tnew->title.delta
                                * tnew->title.main_font_height)
                                + tnew->title.main_offset_y;
                        break;
        }
	ret1 = NhlSetValues(tnew->title.main_id,
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
                NhlNtxConstantSpacingF,tnew->title.main_constant_spacing,
                NhlNtxFontQuality,tnew->title.main_font_quality,
                NhlNtxFuncCode,tnew->title.main_func_code,
                NhlNtxFontThicknessF,tnew->title.main_font_thickness,
                NULL);
/*
* Need to check to make sure a) doesnt overlap with xaxis title b) doesn't
* encroach on viewport
*/
	NhlGetValues(tnew->title.main_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);
	if(tnew->title.x_axis_side == tnew->title.main_side) {
		NhlGetValues(tnew->title.x_axis_id,
			NhlNvpYF,&tmpxy1,
			NhlNvpHeightF,&tmpwh1,NULL);
	} else {
		tmpxy1 = tnew->view.y;
		tmpwh1 = tnew->view.height;
	}

	if(tnew->title.main_side == TOP) {
		if((tmpxy - tmpwh) < tmpxy1) {
			tnew->title.main_pos_y += tmpxy1 - (tmpxy - tmpwh)+ (tnew->title.delta * tnew->title.main_font_height);	
			NhlSetValues(tnew->title.main_id,
				NhlNtxPosYF,tnew->title.main_pos_y,
				NULL);
		} 
	} else {
		if(tmpxy > (tmpxy1 - tmpwh1)) {
			tnew->title.main_pos_y -= tmpxy - (tmpxy1 - tmpwh1)+ (tnew->title.delta * tnew->title.main_font_height);
			NhlSetValues(tnew->title.main_id,
				NhlNtxPosYF,tnew->title.main_pos_y,
				NULL);
		}
	}

	return(MIN(ret,ret1));
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
#if  __STDC__
(LayerClass class, Layer req,Layer new,_NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        LayerClass	class;
        Layer		req;
        Layer		new;
        _NhlArgList	args;
        int		num_args;
#endif
{
	TitleLayer tnew = (TitleLayer) new;
	TitleLayer treq = (TitleLayer) req;
	char buffer[MAXRESNAMLEN];
	NhlErrorTypes ret = NOERROR, ret1 = NOERROR;
	float tmpxy,tmpwh,main_location,tmpxy1,tmpwh1;

	tnew->title.delta = (float)fabs((double)tnew->title.delta);

	if((strcmp(tnew->title.main_string,DEFAULT_STRING)==0)
		&&(tnew->title.main_on)) {
		NhlPError(WARNING,E_UNKNOWN,"TitleIntialize: Main title on but no string provided, turning main title off");
		ret = WARNING;
		tnew->title.main_on = 0;
		tnew->title.main_string = NhlMalloc((unsigned)
					strlen(DEFAULT_STRING)+1);
		strcpy(tnew->title.main_string,DEFAULT_STRING);
	} else {
                tnew->title.main_string = (char*)NhlMalloc((unsigned)
                                strlen(tnew->title.main_string)+1);
                strcpy(tnew->title.main_string,treq->title.main_string);

	}	
	if((strcmp(tnew->title.x_axis_string,DEFAULT_STRING)==0)
		&&(tnew->title.x_axis_on)) {
		NhlPError(WARNING,E_UNKNOWN,"TitleIntialize: XAxis title on but no string provided, turning XAxis title off");
		ret = WARNING;
/*
* need to allocate dummy space so Destroy can just free the string rather
* than have to check if its still the DEFAULT_STRING which is statically
* stored
*/
		tnew->title.x_axis_string = NhlMalloc((unsigned)
					strlen(DEFAULT_STRING)+1);
		strcpy(tnew->title.x_axis_string,DEFAULT_STRING);
		tnew->title.x_axis_on = 0;
	} else {
                tnew->title.x_axis_string = (char*)NhlMalloc((unsigned)
                                strlen(tnew->title.x_axis_string)+1);
                strcpy(tnew->title.x_axis_string,treq->title.x_axis_string);
        }
	if((strcmp(tnew->title.y_axis_string,DEFAULT_STRING)==0) 
		&&(tnew->title.y_axis_on)) {
		NhlPError(WARNING,E_UNKNOWN,"TitleIntialize: YAxis title on but no string provided, turning YAxis title off");
		ret = WARNING;
		tnew->title.y_axis_string = NhlMalloc((unsigned)
					strlen(DEFAULT_STRING)+1);
		strcpy(tnew->title.y_axis_string,DEFAULT_STRING);
		tnew->title.y_axis_on = 0;
	} else {
                tnew->title.y_axis_string = (char*)NhlMalloc((unsigned)
                                strlen(tnew->title.y_axis_string)+1);
                strcpy(tnew->title.y_axis_string,treq->title.y_axis_string);
        }
/*
* use_main_attributes is set then all of the main attributes are copied to the
* coresponding x and y axis fields. This make the creation and SetValues 
* easier to implement plus GetValues is simplified
*/
	if( tnew->title.use_main_attributes ) {
		tnew->title.x_axis_font = tnew->title.y_axis_font =
			tnew->title.main_font;
		tnew->title.y_axis_just = tnew->title.x_axis_just =
			tnew->title.main_just;
		tnew->title.y_axis_font_height =tnew->title.x_axis_font_height =
			tnew->title.main_font_height;
		tnew->title.y_axis_font_aspect =tnew->title.x_axis_font_aspect =
			tnew->title.main_font_aspect;
		tnew->title.y_axis_font_thickness =
			tnew->title.x_axis_font_thickness=
			tnew->title.main_font_thickness;
		tnew->title.y_axis_angle = tnew->title.x_axis_angle =
			tnew->title.main_angle;
		tnew->title.y_axis_angle = tnew->title.x_axis_angle =
			tnew->title.main_angle;
		tnew->title.y_axis_constant_spacing = 
			tnew->title.x_axis_constant_spacing =
			tnew->title.main_constant_spacing;
		tnew->title.y_axis_func_code = 
			tnew->title.x_axis_func_code =
			tnew->title.main_func_code;
	}
/*
* Compute locations of text items based on x,y,width and height for current
* title objects and create them, even if they are turned off.
*/
	switch(tnew->title.y_axis_side) {
		case RIGHT:
			switch(tnew->title.y_axis_position) {
				case TOP:
					tnew->title.y_axis_pos_y = 
						tnew->view.y 
						+ tnew->title.y_axis_offset_y;
					break;
				case BOTTOM:
					tnew->title.y_axis_pos_y =
						tnew->view.y 
						- tnew->view.height
						+ tnew->title.y_axis_offset_y;
					break;
				default:
					NhlPError(WARNING,E_UNKNOWN,"TitleInitialize: Y Axis title can only be positioned on TOP, BOTTOM, or CENTER, defaulting to CENTER");
					ret = WARNING;
				case CENTER:
					tnew->title.y_axis_pos_y =
						tnew->view.y
						- (tnew->view.height/2.0)
						+tnew->title.y_axis_offset_y;
					break;
			}
			tnew->title.y_axis_pos_x =
				tnew->view.x + tnew->view.width
				+(tnew->title.delta 
				*tnew->title.y_axis_font_height)
				+tnew->title.y_axis_offset_x;
			break;
		default: 
			NhlPError(WARNING,E_UNKNOWN,"TitleInitialize: Y Axis title can only appear on LEFT or RIGHT side of plot, using LEFT");
			ret = WARNING;
		case LEFT:
			switch(tnew->title.y_axis_position) {
				case TOP:
					tnew->title.y_axis_pos_y = 
						tnew->view.y 
						+ tnew->title.y_axis_offset_y;
					break;
				case BOTTOM:
					tnew->title.y_axis_pos_y =
						tnew->view.y 
						- tnew->view.height
						+ tnew->title.y_axis_offset_y;
					break;
				default:
					NhlPError(WARNING,E_UNKNOWN,"TitleInitialize: Y Axis title can only be positioned on TOP, BOTTOM, or CENTER, defaulting to CENTER");
					ret = WARNING;
				case CENTER:
					tnew->title.y_axis_pos_y =
						tnew->view.y
						- (tnew->view.height/2.0)
						+tnew->title.y_axis_offset_y;
					break;
			}
			tnew->title.y_axis_pos_x =
				tnew->view.x
				-(tnew->title.delta 
				* tnew->title.y_axis_font_height)
				+tnew->title.y_axis_offset_x;
			break;
	}
	strcpy(buffer,tnew->base.name);
	strcat(buffer,".YAxis");
	ret1 = NhlCreate(&(tnew->title.y_axis_id),
		buffer,textItemLayerClass,
		tnew->base.id,
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
		NhlNtxConstantSpacingF,tnew->title.y_axis_constant_spacing,
		NhlNtxFontQuality,tnew->title.y_axis_font_quality,
		NhlNtxFuncCode,tnew->title.y_axis_func_code,
		NhlNtxFontThicknessF,tnew->title.y_axis_font_thickness,
		NULL);
/*
* Need to check to make sure yaxis title doesn't encroach on viewport
*/
	NhlGetValues(tnew->title.y_axis_id,
		NhlNvpXF,&tmpxy,
		NhlNvpWidthF,&tmpwh,NULL);

	if(tnew->title.y_axis_side == LEFT) {
		if(tmpxy+tmpwh > tnew->view.x){
			tnew->title.y_axis_pos_x -= (tmpxy+tmpwh) 
				- tnew->view.x + (tnew->title.delta * tnew->title.y_axis_font_height);
			NhlSetValues(tnew->title.y_axis_id,
				NhlNtxPosXF,tnew->title.y_axis_pos_x,
				NULL);
		}
	} else {
		if(tmpxy < tnew->view.x + tnew->view.width) {
			tnew->title.y_axis_pos_x += (tnew->view.x 
					+ tnew->view.width)
                                        - tmpxy + (tnew->title.delta * tnew->title.y_axis_font_height);

			NhlSetValues(tnew->title.y_axis_id,
				NhlNtxPosXF, tnew->title.y_axis_pos_x ,
				NULL);
		}
	}

	switch(tnew->title.x_axis_side) {
		case TOP:
			switch(tnew->title.x_axis_position) {
				case RIGHT:
					tnew->title.x_axis_pos_x =
						tnew->view.x 
						+tnew->view.width
						+tnew->title.x_axis_offset_x;
					break;
				case LEFT:
					tnew->title.x_axis_pos_x =
						tnew->view.x
						+tnew->title.x_axis_offset_x;
					break;
				default:
					NhlPError(WARNING,E_UNKNOWN,"TitleInitialize: X Axis title can only appear on RIGHT, LEFT, or CENTER side, defaulting to CENTER");
					ret = WARNING;
				case CENTER:
					tnew->title.x_axis_pos_x =
						tnew->view.x
						+ (tnew->view.width/2.0)
						+tnew->title.x_axis_offset_x;
					break;
			}
			tnew->title.x_axis_pos_y = tnew->view.y 
				+ (tnew->title.delta 	
				* tnew->title.x_axis_font_height) 
				+ tnew->title.x_axis_offset_y;
			break;
		default: 
			NhlPError(WARNING,E_UNKNOWN,"TitleInitialize: X Axis title can only appear on TOP or BOTTOM side of plot, using BOTTOM");
			ret = WARNING;
		case BOTTOM:
			switch(tnew->title.x_axis_position) {
				case RIGHT:
					tnew->title.x_axis_pos_x =
						tnew->view.x 
						+tnew->view.width
						+tnew->title.x_axis_offset_x;
					break;
				case LEFT:
					tnew->title.x_axis_pos_x =
						tnew->view.x
						+tnew->title.x_axis_offset_x;
					break;
				default:
					NhlPError(WARNING,E_UNKNOWN,"TitleInitialize: X Axis title can only appear on RIGHT, LEFT, or CENTER side, defaulting to CENTER");
					ret = WARNING;
				case CENTER:
					tnew->title.x_axis_pos_x =
						tnew->view.x
						+ (tnew->view.width/2.0)
						+tnew->title.x_axis_offset_x;
					break;
			}
			tnew->title.x_axis_pos_y = tnew->view.y 
				- tnew->view.height
				- (tnew->title.delta 	
				* tnew->title.x_axis_font_height) 
				+ tnew->title.x_axis_offset_y;
			break;
	}
	strcpy(buffer,tnew->base.name);
/*
* Dot in name restricts user from specifing resources by name for this object
* in the resource file.
*/
	strcat(buffer,".XAxis");
	ret1 = NhlCreate(&(tnew->title.x_axis_id),
		buffer,textItemLayerClass,
		tnew->base.id,
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
		NhlNtxConstantSpacingF,tnew->title.x_axis_constant_spacing,
		NhlNtxFontQuality,tnew->title.x_axis_font_quality,
		NhlNtxFuncCode,tnew->title.x_axis_func_code,
		NhlNtxFontThicknessF,tnew->title.x_axis_font_thickness,
		NULL);
/*
* Need to check to make sure xaxis title doesn't encroach on viewport
*/
	NhlGetValues(tnew->title.x_axis_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);

	if(tnew->title.x_axis_side == TOP) {
		if(tmpxy - tmpwh < tnew->view.y ){
			tnew->title.x_axis_pos_y += tnew->view.y - (tmpxy - tmpwh ) + (tnew->title.delta * tnew->title.x_axis_font_height);
			NhlSetValues(tnew->title.x_axis_id,
				NhlNtxPosYF,tnew->title.x_axis_pos_y,
				NULL);
		}
	} else {
		if(tmpxy > tnew->view.y - tnew->view.height) {
			tnew->title.x_axis_pos_y -= tmpxy - (tnew->view.y - tnew->view.height)+ (tnew->title.delta * tnew->title.x_axis_font_height);

			NhlSetValues(tnew->title.x_axis_id,
				NhlNtxPosYF, tnew->title.x_axis_pos_y ,
				NULL);
		}
	}
	switch(tnew->title.main_side) {
		case BOTTOM:
			if((tnew->title.x_axis_side == BOTTOM)
				&&(tnew->title.x_axis_on)) {
				NhlGetValues(tnew->title.x_axis_id,
					NhlNvpYF,&tmpxy,
					NhlNvpHeightF,&tmpwh,NULL);
				main_location = tmpxy - tmpwh;
			} else {
				main_location = tnew->view.y 
					- tnew->view.height;
			}
			switch(tnew->title.main_position) {
				case RIGHT:
					tnew->title.main_pos_x =
						tnew->view.x 
						+tnew->view.width
						+tnew->title.main_offset_x;
					break;
				case LEFT:
					tnew->title.main_pos_x =
						tnew->view.x
						+tnew->title.main_offset_x;
					break;
				default:
					NhlPError(WARNING,E_UNKNOWN,"TitleInitialize: X Axis title can only appear on RIGHT, LEFT, or CENTER side, defaulting to CENTER");
					ret = WARNING;
				case CENTER:
					tnew->title.main_pos_x =
						tnew->view.x
						+ (tnew->view.width/2.0)
						+tnew->title.main_offset_x;
					break;
			}
			tnew->title.main_pos_y = main_location 
				- (tnew->title.delta 
				* tnew->title.main_font_height)
				+ tnew->title.main_offset_y;
			break;
		default: 
			NhlPError(WARNING,E_UNKNOWN,"TitleInitialize: Main title can only appear on TOP or BOTTOM side of plot, defaulting to TOP");
			ret = WARNING;
		case TOP:
			if((tnew->title.x_axis_side == TOP)
				&&(tnew->title.x_axis_on)) {
				NhlGetValues(tnew->title.x_axis_id,
					NhlNvpYF,&tmpxy,NULL);
				main_location = tmpxy;
			} else {
				main_location = tnew->view.y;
			}
			switch(tnew->title.main_position) {
				case RIGHT:
					tnew->title.main_pos_x =
						tnew->view.x 
						+tnew->view.width
						+tnew->title.main_offset_x;
					break;
				case LEFT:
					tnew->title.main_pos_x =
						tnew->view.x
						+tnew->title.main_offset_x;
					break;
				default:
					NhlPError(WARNING,E_UNKNOWN,"TitleInitialize: X Axis title can only appear on RIGHT, LEFT, or CENTER side, defaulting to CENTER");
					ret = WARNING;
				case CENTER:
					tnew->title.main_pos_x =
						tnew->view.x
						+ (tnew->view.width/2.0)
						+tnew->title.main_offset_x;
					break;
			}
			tnew->title.main_pos_y = main_location 
				+ (tnew->title.delta 
				* tnew->title.main_font_height)
				+ tnew->title.main_offset_y;
			break;
	}
	strcpy(buffer,tnew->base.name);
/*
* Dot in name restricts user from specifing resources by name for this object
* in the resource file.
*/
	strcat(buffer,".Main");
	ret1 = NhlCreate(&(tnew->title.main_id),
		buffer,textItemLayerClass,
		tnew->base.id,
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
		NhlNtxConstantSpacingF,tnew->title.main_constant_spacing,
		NhlNtxFontQuality,tnew->title.main_font_quality,
		NhlNtxFuncCode,tnew->title.main_func_code,
		NhlNtxFontThicknessF,tnew->title.main_font_thickness,
		NULL);

/*
* Need to check to make sure main title doesn't encroach on viewport
*/
	NhlGetValues(tnew->title.main_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);
	if(tnew->title.x_axis_side == tnew->title.main_side) {
		NhlGetValues(tnew->title.x_axis_id,
			NhlNvpYF,&tmpxy1,
			NhlNvpHeightF,&tmpwh1,NULL);
	} else {
		tmpxy1 = tnew->view.y;
		tmpwh1 = tnew->view.height;
	}

	if(tnew->title.main_side == TOP) {
		if((tmpxy - tmpwh) < tmpxy1) {
			tnew->title.main_pos_y += tmpxy1 - (tmpxy - tmpwh)+ (tnew->title.delta * tnew->title.main_font_height);	
			NhlSetValues(tnew->title.main_id,
				NhlNtxPosYF,tnew->title.main_pos_y,
				NULL);
		} 
	} else {
		if(tmpxy > (tmpxy1 - tmpwh1)) {
			tnew->title.main_pos_y -= tmpxy - (tmpxy1 - tmpwh1)+ (tnew->title.delta * tnew->title.main_font_height);
			NhlSetValues(tnew->title.main_id,
				NhlNtxPosYF,tnew->title.main_pos_y,
				NULL);
		}
	}


	return(MIN(ret1,ret));
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
 * Return Values:	NOERROR	
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes     TitleDestroy
#if	__STDC__
(
        Layer	inst
)
#else
(inst)
	Layer	inst;
#endif
{
	TitleLayer tinst = (TitleLayer) inst;

	NhlFree(tinst->title.main_string);
	NhlFree(tinst->title.x_axis_string);	
	NhlFree(tinst->title.y_axis_string);
	NhlDestroy(tinst->title.main_id);
	NhlDestroy(tinst->title.x_axis_id);
	NhlDestroy(tinst->title.y_axis_id);
	return(NOERROR);
}
static NhlErrorTypes
NhlCvtStringToTitlePositions
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
        TitlePositions tmp;
        NhlErrorTypes ret = NOERROR;

        if(nargs != 0) {
                ret = WARNING;
        }
        if(strncmp((char*)from->addr,"TOP",strlen("TOP"))==0)
                tmp = TOP;
        else if(strncmp((char*)from->addr,"BOTTOM",strlen("BOTTOM"))==0)
                tmp = BOTTOM;
        else if(strncmp((char*)from->addr,"LEFT",strlen("LEFT"))==0)
                tmp = LEFT;
        else if(strncmp((char*)from->addr,"RIGHT",strlen("RIGHT"))==0)
                tmp = RIGHT;
        else if(strncmp((char*)from->addr,"CENTER",strlen("CENTER"))==0)
                tmp = CENTER;
        else {
                NhlPError(WARNING,E_UNKNOWN,"NhlCvtStringToTitlePositions: Could not convert %s to either TOP, BOTTOM, LEFT, RIGHT or CENTER, incorrect string",(char*)from->addr);
                return(WARNING);
        }

        if((to->size >0) && (to->addr != NULL)) {
                /* caller provided space */
                if(to->size < sizeof(TitlePositions)) {
                        to->size = (unsigned int)sizeof(TitlePositions);
                        return(FATAL);
                }
                to->size = (unsigned int)sizeof(TitlePositions);
                *((TitlePositions*)(to->addr)) = tmp;
                return(ret);
        } else {
                static TitlePositions val;
                to->size = (unsigned int)sizeof(TitlePositions);
                val = tmp;
                to->addr = &val;
                return(ret);
        }
}

/*
 * Function:	TitleClassInitialize
 *
 * Description:	Just calls NrmStringToQuark to register new type name
 *		NhlTTitlePositions
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	NONE
 *
 * Side Effects:	NOERROR
 */
static NhlErrorTypes    TitleClassInitialize
#if __STDC__
(void)
#else
()
#endif
{
	NhlRegisterConverter(NhlTString,NhlTTitlePositions,
		NhlCvtStringToTitlePositions,NULL,0,False,NULL);
	return(NOERROR);
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
#if  __STDC__
(Layer instance)
#else
(instance)
	Layer	instance;
#endif
{
	TitleLayer tinstance = (TitleLayer) instance;

	if(tinstance->title.main_on)
		NhlDraw(tinstance->title.main_id);
	if(tinstance->title.x_axis_on)
		NhlDraw(tinstance->title.x_axis_id);
	if(tinstance->title.y_axis_on)
		NhlDraw(tinstance->title.y_axis_id);
	return(NOERROR);
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
#if	__STDC__
(Layer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	Layer instance;
	NhlBoundingBox *thebox;
#endif
{
	TitleLayer tinstance = (TitleLayer) instance;
	float x0,y0,width,height;

	NhlGetValues(tinstance->title.main_id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF, &height, NULL);

	_NhlAddBBInfo(y0,y0-height,x0+width,x0,thebox);

	NhlGetValues(tinstance->title.x_axis_id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF, &height, NULL);

	_NhlAddBBInfo(y0,y0-height,x0+width,x0,thebox);

	NhlGetValues(tinstance->title.y_axis_id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF, &height, NULL);

	_NhlAddBBInfo(y0,y0-height,x0+width,x0,thebox);

	return(NOERROR);
}


