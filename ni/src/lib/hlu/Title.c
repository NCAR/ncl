/*
 *      $Id: Title.c,v 1.5 1994-02-08 20:16:08 boote Exp $
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
#include <ncarg/hlu/Converters.h>
#include <math.h>

static char	Main[] = "Main";
static char	XAxis[] = "XAxis";
static char	YAxis[] = "YAxis";

/*ARGSUSED*/
static NhlErrorTypes
SetMainOn
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
	NhlTitleLayer	tl = (NhlTitleLayer)base;

	tl->title.main_on = !(tl->title.main_string == Main);

	return NhlNOERROR;
}

/*ARGSUSED*/
static NhlErrorTypes
SetXAxisOn
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
	NhlTitleLayer	tl = (NhlTitleLayer)base;

	tl->title.x_axis_on = !(tl->title.x_axis_string == XAxis);

	return NhlNOERROR;
}

/*ARGSUSED*/
static NhlErrorTypes
SetYAxisOn
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
	NhlTitleLayer	tl = (NhlTitleLayer)base;

	tl->title.y_axis_on = !(tl->title.y_axis_string == YAxis);

	return NhlNOERROR;
}

#define Oset(field) NhlOffset(NhlTitleLayerRec,title.field)
static NhlResource resources[] = {
	{NhlNtiDeltaF, NhlCtiDeltaF, NhlTFloat, sizeof(float),
		Oset(delta), NhlTString, "1.5" },
	{NhlNtiMainFontColor,NhlCtiTitleFontColors,NhlTInteger,sizeof(int),
		Oset(main_font_color),NhlTImmediate,(NhlPointer)1},
	{NhlNtiMainFontQuality, NhlCtiTitleFontQualities, NhlTFQuality,
		sizeof(NhlFontQuality),Oset(main_font_quality),
		NhlTImmediate,(NhlPointer)NhlHIGH },
	{NhlNtiUseMainAttributes,NhlCtiUseMainAttributes, NhlTBoolean,
		sizeof(NhlBoolean),
		Oset(use_main_attributes),NhlTImmediate,False},
	{NhlNtiMainString, NhlCtiMainString,NhlTString,sizeof(char*),
		Oset(main_string),NhlTImmediate,(NhlPointer)Main},
	{NhlNtiMainJust, NhlCtiTitleJust, NhlTInteger,sizeof(int),
		Oset(main_just),NhlTImmediate,(NhlPointer)4},
	{NhlNtiMainFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		Oset(main_font),NhlTImmediate,(NhlPointer)0},
	{NhlNtiMainFontHeightF,NhlCtiTitleFontHeightsF,NhlTFloat,sizeof(float),
		Oset(main_font_height), NhlTString,"0.025"},
	{NhlNtiMainFontAspectF,NhlCtiTitleFontAspectsF,NhlTFloat,sizeof(float),
		Oset(main_font_aspect), NhlTString,"1.3125"},
	{NhlNtiMainFontThicknessF,NhlCtiTitleFontThicknessF,NhlTFloat,
		sizeof(float), Oset(main_font_thickness), NhlTString,"1.0"},
	{NhlNtiMainAngleF,NhlCtiTitleAnglesF,NhlTFloat, sizeof(float),
		Oset(main_angle), NhlTString,"0.00"},
	{NhlNtiMainDirection,NhlCtiMainDirection,NhlTTextDirection,
		sizeof(NhlTextDirection),
		Oset(main_direction), NhlTImmediate,(NhlPointer)NhlACROSS},
	{NhlNtiMainPosition,NhlCtiMainPosition,NhlTTitlePositions,
		sizeof(NhlTitlePositions),
		Oset(main_position), NhlTImmediate,(NhlPointer)NhlCENTER},
	{NhlNtiMainOn,NhlCtiMainOn,NhlTBoolean, sizeof(NhlBoolean),
		Oset(main_on),NhlTProcedure,(NhlPointer)SetMainOn},
	{NhlNtiMainSide,NhlCtiMainSide,NhlTTitlePositions,
		sizeof(NhlTitlePositions),
		Oset(main_side),NhlTImmediate,(NhlPointer)NhlTOP},
	{NhlNtiMainConstantSpacingF, NhlCtiTitleConstantSpacingsF,NhlTFloat,
		sizeof(float), Oset(main_constant_spacing),NhlTString,"0.0" },
	{NhlNtiMainFuncCode, NhlCtiTitleFuncCodes, NhlTCharacter,sizeof(char),
		Oset(main_func_code), NhlTString, ":" },
	{NhlNtiMainOffsetXF, NhlCtiMainOffsetXF, NhlTFloat,sizeof(float),
		Oset(main_offset_x), NhlTString,"0.0"},
	{NhlNtiMainOffsetYF, NhlCtiMainOffsetYF, NhlTFloat,sizeof(float),
		Oset(main_offset_y), NhlTString,"0.0"},
	{NhlNtiXAxisFontColor,NhlCtiTitleFontColors,NhlTInteger,sizeof(int),
		Oset(x_axis_font_color), NhlTImmediate,(NhlPointer)1},
	{NhlNtiXAxisFontQuality, NhlCtiTitleFontQualities, NhlTFQuality,
		sizeof(NhlFontQuality),
		Oset(x_axis_font_quality), NhlTImmediate,(NhlPointer)NhlHIGH },
	{NhlNtiXAxisString, NhlCtiXAxisString,NhlTString,sizeof(char*),
		Oset(x_axis_string),NhlTImmediate,(NhlPointer)XAxis},
	{NhlNtiXAxisJust, NhlCtiTitleJust, NhlTInteger,sizeof(int),
		Oset(x_axis_just),NhlTImmediate,(NhlPointer)4 },
	{NhlNtiXAxisFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		Oset(x_axis_font),NhlTImmediate,(NhlPointer)0 },
	{NhlNtiXAxisFontHeightF,NhlCtiTitleFontHeightsF,NhlTFloat,sizeof(float),
		Oset(x_axis_font_height), NhlTString,"0.025"},
	{NhlNtiXAxisFontAspectF,NhlCtiTitleFontAspectsF,NhlTFloat,sizeof(float),
		Oset(x_axis_font_aspect), NhlTString,"1.3125"},
	{NhlNtiXAxisFontThicknessF,NhlCtiTitleFontThicknessF,NhlTFloat,
		sizeof(float),
		Oset(x_axis_font_thickness), NhlTString,"1.0"},
	{NhlNtiXAxisAngleF,NhlCtiTitleAnglesF,NhlTFloat,sizeof(float),
		Oset(x_axis_angle), NhlTString,"0.0"},
	{NhlNtiXAxisDirection,NhlCtiXAxisDirection,NhlTTextDirection,
		sizeof(NhlTextDirection),
		Oset(x_axis_direction), NhlTImmediate,(NhlPointer)NhlACROSS},
	{NhlNtiXAxisPosition,NhlCtiXAxisPosition,NhlTTitlePositions,
		sizeof(NhlTitlePositions),
		Oset(x_axis_position), NhlTImmediate,(NhlPointer)NhlCENTER},
	{NhlNtiXAxisConstantSpacingF, NhlCtiTitleConstantSpacingsF,NhlTFloat,
		sizeof(float),
		Oset(x_axis_constant_spacing), NhlTString,"0.0" },
	{NhlNtiXAxisFuncCode, NhlCtiTitleFuncCodes, NhlTCharacter,sizeof(char),
		Oset(x_axis_func_code), NhlTString, ":" },
	{NhlNtiXAxisOffsetXF, NhlCtiXAxisOffsetXF, NhlTFloat,sizeof(float),
		Oset(x_axis_offset_x), NhlTString,"0.0"},
	{NhlNtiXAxisOffsetYF, NhlCtiXAxisOffsetYF, NhlTFloat,sizeof(float),
		Oset(x_axis_offset_y), NhlTString,"0.0"},
	{NhlNtiXAxisOn,NhlCtiXAxisOn,NhlTBoolean, sizeof(NhlBoolean),
		Oset(x_axis_on), NhlTProcedure,(NhlPointer)SetXAxisOn},
	{NhlNtiXAxisSide,NhlCtiXAxisSide,NhlTTitlePositions, 
		sizeof(NhlTitlePositions),
		Oset(x_axis_side), NhlTImmediate,(NhlPointer)NhlBOTTOM},
	{NhlNtiYAxisFontColor,NhlCtiTitleFontColors,NhlTInteger,sizeof(int),
		Oset(y_axis_font_color), NhlTImmediate,(NhlPointer)1},
	{NhlNtiYAxisFontQuality, NhlCtiTitleFontQualities, NhlTFQuality,
		sizeof(NhlFontQuality),
		Oset(y_axis_font_quality), NhlTImmediate,(NhlPointer)NhlHIGH},
	{NhlNtiYAxisString, NhlCtiYAxisString,NhlTString,sizeof(char*),
		Oset(y_axis_string),NhlTImmediate,(NhlPointer)YAxis},
	{NhlNtiYAxisJust, NhlCtiTitleJust, NhlTInteger,sizeof(int),
		Oset(y_axis_just),NhlTImmediate,(NhlPointer)4 },
	{NhlNtiYAxisFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		Oset(y_axis_font),NhlTImmediate,(NhlPointer)0 },
	{NhlNtiYAxisFontHeightF,NhlCtiTitleFontHeightsF,NhlTFloat,sizeof(float),
		Oset(y_axis_font_height), NhlTString,"0.025"},
	{NhlNtiYAxisFontAspectF,NhlCtiTitleFontAspectsF,NhlTFloat,sizeof(float),
		Oset(y_axis_font_aspect), NhlTString,"1.3125"},
	{NhlNtiYAxisFontThicknessF,NhlCtiTitleFontThicknessF,NhlTFloat,
		sizeof(float), Oset(y_axis_font_thickness), NhlTString,"1.0"},
	{NhlNtiYAxisAngleF,NhlCtiTitleAnglesF,NhlTFloat, sizeof(float),
		Oset(y_axis_angle), NhlTString,"90.0"},
	{NhlNtiYAxisDirection,NhlCtiYAxisDirection,NhlTTextDirection,
		sizeof(NhlTextDirection),
		Oset(y_axis_direction), NhlTImmediate,(NhlPointer)NhlACROSS},
	{NhlNtiYAxisPosition,NhlCtiYAxisPosition,NhlTTitlePositions,
		sizeof(NhlTitlePositions),
		Oset(y_axis_position), NhlTImmediate,(NhlPointer)NhlCENTER},
	{NhlNtiYAxisConstantSpacingF, NhlCtiTitleConstantSpacingsF,NhlTFloat,
		sizeof(float),
		Oset(y_axis_constant_spacing), NhlTString,"0.0" },
	{NhlNtiYAxisFuncCode, NhlCtiTitleFuncCodes, NhlTCharacter,sizeof(char),
		Oset(y_axis_func_code), NhlTString, ":" },
	{NhlNtiYAxisOffsetXF, NhlCtiYAxisOffsetXF, NhlTFloat,sizeof(float),
		Oset(y_axis_offset_x), NhlTString,"0.0"},
	{NhlNtiYAxisOffsetYF, NhlCtiYAxisOffsetYF, NhlTFloat,sizeof(float),
		Oset(y_axis_offset_y), NhlTString,"0.0"},
	{NhlNtiYAxisOn,NhlCtiYAxisOn,NhlTBoolean, sizeof(NhlBoolean),
		Oset(y_axis_on), NhlTProcedure,(NhlPointer)SetYAxisOn},
	{NhlNtiYAxisSide,NhlCtiYAxisSide,NhlTTitlePositions, 
		sizeof(NhlTitlePositions),
		Oset(y_axis_side), NhlTImmediate,(NhlPointer)NhlLEFT}
};
#undef Oset

/*
* Base Methods used
*/
static NhlErrorTypes    TitleSetValues(
#ifdef NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);
static NhlErrorTypes    TitleInitialize(
#ifdef NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes     TitleDestroy(
#ifdef NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes    TitleClassInitialize();


static NhlErrorTypes TitleGetBB(
#ifdef NhlNeedProto
        NhlLayer          /* instance */,
        NhlBoundingBox * /*thebox*/
#endif
);

static NhlErrorTypes TitleDraw(
#ifdef NhlNeedProto
        NhlLayer           instance
#endif
);



NhlTitleLayerClassRec NhltitleLayerClassRec = {
        {
/* class_name                   */      "Title",
/* nrm_class                    */      NrmNULLQUARK,
/* layer_size                   */      sizeof(NhlTitleLayerRec),
/* class_inited                 */      False,
/* superclass                   */      (NhlLayerClass)&NhlviewLayerClassRec,

/* layer_resources              */      resources,
/* num_resources                */      NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize        */      NULL,
/* class_initialize             */      TitleClassInitialize,
/* layer_initialize             */      TitleInitialize,
/* layer_set_values             */      TitleSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values             */      NULL,
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

NhlLayerClass NhltitleLayerClass = (NhlLayerClass)&NhltitleLayerClassRec;

		
	
		
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
		if(told->title.main_string != Main)
			NhlFree(told->title.main_string);
		tnew->title.main_string = NhlMalloc((unsigned)
				strlen(tref->title.main_string)+1);
		strcpy(tnew->title.main_string,tref->title.main_string);
	}
	if(told->title.x_axis_string != tnew->title.x_axis_string) {
		if(told->title.x_axis_string != XAxis)
			NhlFree(told->title.x_axis_string);
		tnew->title.x_axis_string = NhlMalloc((unsigned)
				strlen(tref->title.x_axis_string)+1);
		strcpy(tnew->title.x_axis_string,tref->title.x_axis_string);
	}
	if(told->title.y_axis_string != tnew->title.y_axis_string) {
		if(told->title.y_axis_string != YAxis)
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
					NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleSetValues: Y Axis title can only be positioned on NhlTOP, NhlBOTTOM, or NhlCENTER, defaulting to NhlCENTER");
                                        ret = NhlWARNING;
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
                                *tnew->title.y_axis_font_height)
                                +tnew->title.y_axis_offset_x;
                        break;
                default:
			NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleSetValues: Y Axis title can only appear on NhlLEFT or NhlRIGHT side of plot, using NhlLEFT");
                        ret = NhlWARNING;
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
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleSetValues: Y Axis title can only be positioned on NhlTOP, NhlBOTTOM, or NhlCENTER, defaulting to NhlCENTER");
                                        ret = NhlWARNING;
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
                                * tnew->title.y_axis_font_height)
                                +tnew->title.y_axis_offset_x;
                        break;
        }
	ret1 = NhlVASetValues(tnew->title.y_axis_id,
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
	NhlVAGetValues(tnew->title.y_axis_id,
		NhlNvpXF,&tmpxy,
		NhlNvpWidthF,&tmpwh,NULL);

	if(tnew->title.y_axis_side == NhlLEFT) {
		if(tmpxy+tmpwh > tnew->view.x){
			tnew->title.y_axis_pos_x -= (tmpxy+tmpwh) 
				- tnew->view.x + (tnew->title.y_axis_font_height*tnew->title.delta);
			NhlVASetValues(tnew->title.y_axis_id,
				NhlNtxPosXF,tnew->title.y_axis_pos_x,
				NULL);
		}
	} else {
		if(tmpxy < tnew->view.x + tnew->view.width) {
			tnew->title.y_axis_pos_x += (tnew->view.x 
					+ tnew->view.width)
                                        - tmpxy + (tnew->title.y_axis_font_height*tnew->title.delta);

			NhlVASetValues(tnew->title.y_axis_id,
				NhlNtxPosXF, tnew->title.y_axis_pos_x ,
				NULL);
		}
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
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleSetValues: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER");
					ret= NhlWARNING;
                                case NhlCENTER:
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
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleSetValues: X Axis title can only appear on NhlTOP or NhlBOTTOM side of plot, using NhlBOTTOM");
                        ret = NhlWARNING;
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
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleSetValues: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER");
                                        ret = NhlWARNING;
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
                                * tnew->title.x_axis_font_height)
                                + tnew->title.x_axis_offset_y;
                        break;
        }
	ret1 = NhlVASetValues(tnew->title.x_axis_id,
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
	NhlVAGetValues(tnew->title.x_axis_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);

	if(tnew->title.x_axis_side == NhlTOP) {
		if(tmpxy - tmpwh < tnew->view.y ){
			tnew->title.x_axis_pos_y += tnew->view.y - (tmpxy - tmpwh ) + (tnew->title.delta*tnew->title.x_axis_font_height);
			NhlVASetValues(tnew->title.x_axis_id,
				NhlNtxPosYF,tnew->title.x_axis_pos_y,
				NULL);
		}
	} else {
		if(tmpxy > tnew->view.y - tnew->view.height) {
			tnew->title.x_axis_pos_y -= tmpxy - (tnew->view.y - tnew->view.height)+(tnew->title.delta*tnew->title.x_axis_font_height);

			NhlVASetValues(tnew->title.x_axis_id,
				NhlNtxPosYF, tnew->title.x_axis_pos_y ,
				NULL);
		}
	}

	switch(tnew->title.main_side) {
                case NhlBOTTOM:
                        if((tnew->title.x_axis_side == NhlBOTTOM)
                                &&(tnew->title.x_axis_on)) {
                                NhlVAGetValues(tnew->title.x_axis_id,
                                        NhlNvpYF,&tmpxy,
                                        NhlNvpHeightF,&tmpwh,NULL);
                                main_location = tmpxy - tmpwh;
                        } else {
                                main_location = tnew->view.y
                                        - tnew->view.height;
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
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleSetValues: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER");
                                        ret = NhlWARNING;
                                case NhlCENTER:
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
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleSetValues: Main title can only appear on NhlTOP or NhlBOTTOM side of plot, defaulting to NhlTOP");
                        ret = NhlWARNING;
                case NhlTOP:
                        if((tnew->title.x_axis_side == NhlTOP)
                                &&(tnew->title.x_axis_on)) {
                                NhlVAGetValues(tnew->title.x_axis_id,
                                        NhlNvpYF,&tmpxy,NULL);
                                main_location = tmpxy;
                        } else {
                                main_location = tnew->view.y;
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
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleSetValues: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER");
                                        ret = NhlWARNING;
                                case NhlCENTER:
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
	ret1 = NhlVASetValues(tnew->title.main_id,
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
	NhlVAGetValues(tnew->title.main_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);
	if(tnew->title.x_axis_side == tnew->title.main_side) {
		NhlVAGetValues(tnew->title.x_axis_id,
			NhlNvpYF,&tmpxy1,
			NhlNvpHeightF,&tmpwh1,NULL);
	} else {
		tmpxy1 = tnew->view.y;
		tmpwh1 = tnew->view.height;
	}

	if(tnew->title.main_side == NhlTOP) {
		if((tmpxy - tmpwh) < tmpxy1) {
			tnew->title.main_pos_y += tmpxy1 - (tmpxy - tmpwh)+ (tnew->title.delta * tnew->title.main_font_height);	
			NhlVASetValues(tnew->title.main_id,
				NhlNtxPosYF,tnew->title.main_pos_y,
				NULL);
		} 
	} else {
		if(tmpxy > (tmpxy1 - tmpwh1)) {
			tnew->title.main_pos_y -= tmpxy - (tmpxy1 - tmpwh1)+ (tnew->title.delta * tnew->title.main_font_height);
			NhlVASetValues(tnew->title.main_id,
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
(NhlLayerClass class, NhlLayer req,NhlLayer new,_NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        NhlLayerClass	class;
        NhlLayer	req;
        NhlLayer	new;
        _NhlArgList	args;
        int		num_args;
#endif
{
	NhlTitleLayer tnew = (NhlTitleLayer) new;
	NhlTitleLayer treq = (NhlTitleLayer) req;
	char buffer[_NhlMAXRESNAMLEN];
	NhlErrorTypes ret = NhlNOERROR, ret1 = NhlNOERROR;
	float tmpxy,tmpwh,main_location,tmpxy1,tmpwh1;

	tnew->title.delta = (float)fabs((double)tnew->title.delta);

	if(tnew->title.main_string != Main){
                tnew->title.main_string = (char*)NhlMalloc((unsigned)
                                strlen(tnew->title.main_string)+1);
                strcpy(tnew->title.main_string,treq->title.main_string);

	}	
	if(tnew->title.x_axis_string != XAxis){
                tnew->title.x_axis_string = (char*)NhlMalloc((unsigned)
                                strlen(tnew->title.x_axis_string)+1);
                strcpy(tnew->title.x_axis_string,treq->title.x_axis_string);
        }
	if(tnew->title.y_axis_string != YAxis){
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
					NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleInitialize: Y Axis title can only be positioned on NhlTOP, NhlBOTTOM, or NhlCENTER, defaulting to NhlCENTER");
					ret = NhlWARNING;
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
				*tnew->title.y_axis_font_height)
				+tnew->title.y_axis_offset_x;
			break;
		default: 
			NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleInitialize: Y Axis title can only appear on NhlLEFT or NhlRIGHT side of plot, using NhlLEFT");
			ret = NhlWARNING;
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
					NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleInitialize: Y Axis title can only be positioned on NhlTOP, NhlBOTTOM, or NhlCENTER, defaulting to NhlCENTER");
					ret = NhlWARNING;
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
				* tnew->title.y_axis_font_height)
				+tnew->title.y_axis_offset_x;
			break;
	}
	strcpy(buffer,tnew->base.name);
	strcat(buffer,".YAxis");
	ret1 = NhlVACreate(&(tnew->title.y_axis_id),
		buffer,NhltextItemLayerClass,
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
	NhlVAGetValues(tnew->title.y_axis_id,
		NhlNvpXF,&tmpxy,
		NhlNvpWidthF,&tmpwh,NULL);

	if(tnew->title.y_axis_side == NhlLEFT) {
		if(tmpxy+tmpwh > tnew->view.x){
			tnew->title.y_axis_pos_x -= (tmpxy+tmpwh) 
				- tnew->view.x + (tnew->title.delta * tnew->title.y_axis_font_height);
			NhlVASetValues(tnew->title.y_axis_id,
				NhlNtxPosXF,tnew->title.y_axis_pos_x,
				NULL);
		}
	} else {
		if(tmpxy < tnew->view.x + tnew->view.width) {
			tnew->title.y_axis_pos_x += (tnew->view.x 
					+ tnew->view.width)
                                        - tmpxy + (tnew->title.delta * tnew->title.y_axis_font_height);

			NhlVASetValues(tnew->title.y_axis_id,
				NhlNtxPosXF, tnew->title.y_axis_pos_x ,
				NULL);
		}
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
					NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleInitialize: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER");
					ret = NhlWARNING;
				case NhlCENTER:
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleInitialize: X Axis title can only appear on NhlTOP or NhlBOTTOM side of plot, using NhlBOTTOM");
			ret = NhlWARNING;
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
					NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleInitialize: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER");
					ret = NhlWARNING;
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
	ret1 = NhlVACreate(&(tnew->title.x_axis_id),
		buffer,NhltextItemLayerClass,
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
	NhlVAGetValues(tnew->title.x_axis_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);

	if(tnew->title.x_axis_side == NhlTOP) {
		if(tmpxy - tmpwh < tnew->view.y ){
			tnew->title.x_axis_pos_y += tnew->view.y - (tmpxy - tmpwh ) + (tnew->title.delta * tnew->title.x_axis_font_height);
			NhlVASetValues(tnew->title.x_axis_id,
				NhlNtxPosYF,tnew->title.x_axis_pos_y,
				NULL);
		}
	} else {
		if(tmpxy > tnew->view.y - tnew->view.height) {
			tnew->title.x_axis_pos_y -= tmpxy - (tnew->view.y - tnew->view.height)+ (tnew->title.delta * tnew->title.x_axis_font_height);

			NhlVASetValues(tnew->title.x_axis_id,
				NhlNtxPosYF, tnew->title.x_axis_pos_y ,
				NULL);
		}
	}
	switch(tnew->title.main_side) {
		case NhlBOTTOM:
			if((tnew->title.x_axis_side == NhlBOTTOM)
				&&(tnew->title.x_axis_on)) {
				NhlVAGetValues(tnew->title.x_axis_id,
					NhlNvpYF,&tmpxy,
					NhlNvpHeightF,&tmpwh,NULL);
				main_location = tmpxy - tmpwh;
			} else {
				main_location = tnew->view.y 
					- tnew->view.height;
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
					NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleInitialize: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER");
					ret = NhlWARNING;
				case NhlCENTER:
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleInitialize: Main title can only appear on NhlTOP or NhlBOTTOM side of plot, defaulting to NhlTOP");
			ret = NhlWARNING;
		case NhlTOP:
			if((tnew->title.x_axis_side == NhlTOP)
				&&(tnew->title.x_axis_on)) {
				NhlVAGetValues(tnew->title.x_axis_id,
					NhlNvpYF,&tmpxy,NULL);
				main_location = tmpxy;
			} else {
				main_location = tnew->view.y;
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
					NhlPError(NhlWARNING,NhlEUNKNOWN,"TitleInitialize: X Axis title can only appear on NhlRIGHT, NhlLEFT, or NhlCENTER side, defaulting to NhlCENTER");
					ret = NhlWARNING;
				case NhlCENTER:
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
	ret1 = NhlVACreate(&(tnew->title.main_id),
		buffer,NhltextItemLayerClass,
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
	NhlVAGetValues(tnew->title.main_id,
		NhlNvpYF,&tmpxy,
		NhlNvpHeightF,&tmpwh,NULL);
	if(tnew->title.x_axis_side == tnew->title.main_side) {
		NhlVAGetValues(tnew->title.x_axis_id,
			NhlNvpYF,&tmpxy1,
			NhlNvpHeightF,&tmpwh1,NULL);
	} else {
		tmpxy1 = tnew->view.y;
		tmpwh1 = tnew->view.height;
	}

	if(tnew->title.main_side == NhlTOP) {
		if((tmpxy - tmpwh) < tmpxy1) {
			tnew->title.main_pos_y += tmpxy1 - (tmpxy - tmpwh)+ (tnew->title.delta * tnew->title.main_font_height);	
			NhlVASetValues(tnew->title.main_id,
				NhlNtxPosYF,tnew->title.main_pos_y,
				NULL);
		} 
	} else {
		if(tmpxy > (tmpxy1 - tmpwh1)) {
			tnew->title.main_pos_y -= tmpxy - (tmpxy1 - tmpwh1)+ (tnew->title.delta * tnew->title.main_font_height);
			NhlVASetValues(tnew->title.main_id,
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
 * Return Values:	NhlNOERROR	
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes     TitleDestroy
#if	__STDC__
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
	return(NhlNOERROR);
}

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
#if __STDC__
(void)
#else
()
#endif
{
	NhlConvertArg	titlepos[] = {
				{NhlSTRENUM,	NhlTOP,		"top"},
				{NhlSTRENUM,	NhlBOTTOM,	"bottom"},
				{NhlSTRENUM,	NhlLEFT,	"left"},
				{NhlSTRENUM,	NhlRIGHT,	"right"},
				{NhlSTRENUM,	NhlCENTER,	"center"}
				};

	NhlConvertArg	inttitlepos[] = {
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)NhlTOP},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)NhlBOTTOM},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)NhlLEFT},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)NhlRIGHT},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)NhlCENTER}
				};

	NhlRegisterConverter(NhlTString,NhlTTitlePositions,NhlCvtStringToEnum,
				titlepos,NhlNumber(titlepos),False,NULL);
	NhlRegisterConverter(NhlTInteger,NhlTTitlePositions,NhlCvtIntToEnum,
				inttitlepos,NhlNumber(inttitlepos),False,NULL);
	NhlRegisterConverter(NhlTFloat,NhlTTitlePositions,NhlCvtFloatToEnum,
				inttitlepos,NhlNumber(inttitlepos),False,NULL);

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
#if  __STDC__
(NhlLayer instance)
#else
(instance)
	NhlLayer	instance;
#endif
{
	NhlTitleLayer tinstance = (NhlTitleLayer) instance;

	if(tinstance->title.main_on)
		NhlDraw(tinstance->title.main_id);
	if(tinstance->title.x_axis_on)
		NhlDraw(tinstance->title.x_axis_id);
	if(tinstance->title.y_axis_on)
		NhlDraw(tinstance->title.y_axis_id);
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
#if	__STDC__
(NhlLayer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox *thebox;
#endif
{
	NhlTitleLayer tinstance = (NhlTitleLayer) instance;
	float x0,y0,width,height;

	NhlVAGetValues(tinstance->title.main_id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF, &height, NULL);

	_NhlAddBBInfo(y0,y0-height,x0+width,x0,thebox);

	NhlVAGetValues(tinstance->title.x_axis_id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF, &height, NULL);

	_NhlAddBBInfo(y0,y0-height,x0+width,x0,thebox);

	NhlVAGetValues(tinstance->title.y_axis_id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF, &height, NULL);

	_NhlAddBBInfo(y0,y0-height,x0+width,x0,thebox);

	return(NhlNOERROR);
}
