/*
 *      $Id: TextItem.c,v 1.27 1995-04-07 09:36:01 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TextItem.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 10 17:23:26 MST 1992
 *
 *	Description:	TextItem source for drawing text. 
 *	-------> Text is special. It does not need a transformation and
 *		always draws in NDC coordinates. Therefore, text must 
 *		perform getset's to preserve current transformation.<------
 */

#include <math.h>
#include <ncarg/hlu/TextItemP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/WorkstationI.h>

#define DEFSTRING "NOTHING"
#define DEGTORAD 0.017453293

/*
 * Function:	ResUnset
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
/*ARGSUSED*/
static NhlErrorTypes
ResUnset
#if	NhlNeedProto
(
	NrmName		name,
	NrmClass	cname,
	NhlPointer	base,
	unsigned int	offset
)
#else
(name,cname,base,offset)
	NrmName		name;
	NrmClass	cname;
	NhlPointer	base;
	unsigned int	offset;
#endif
{
	char *cl = (char *)base;
	NhlBoolean *set = (NhlBoolean *)(cl + offset - sizeof(NhlBoolean));

	*set = False;

	return NhlNOERROR;
}

static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNtxString, NhlCtxString, NhlTString, sizeof(char*),
		NhlOffset(NhlTextItemLayerRec,text.string),
		NhlTImmediate,_NhlUSET(DEFSTRING),0,(NhlFreeFunc)NhlFree},
	{ "no.res", "No.res", NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTextItemLayerRec,text.pos_x_set),
			NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtxPosXF, NhlCtxPosXF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec,text.pos_x),
			NhlTProcedure,_NhlUSET((NhlPointer)ResUnset) ,0,NULL},
	{ "no.res", "No.res", NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTextItemLayerRec,text.pos_y_set),
			NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtxPosYF, NhlCtxPosYF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec,text.pos_y),
			NhlTProcedure,_NhlUSET((NhlPointer)ResUnset),0,NULL },
	{ "no.res", "No.res", NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTextItemLayerRec,text.angle_set),
			NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtxAngleF, NhlCtxAngleF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec,text.angle),
		NhlTProcedure,_NhlUSET((NhlPointer)ResUnset),0,NULL },
	{ NhlNtxFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTextItemLayerRec, text.font),
		NhlTImmediate,_NhlUSET(0),0,NULL },
	{ "no.res", "No.res", NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTextItemLayerRec,text.just_set),
			NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtxJust, NhlCtxJust, NhlTJustification, sizeof(int),
		NhlOffset(NhlTextItemLayerRec, text.just),
		NhlTProcedure,_NhlUSET((NhlPointer)ResUnset),0,NULL},
	{ NhlNtxFontQuality, NhlCtxFontQuality, NhlTFontQuality, 
		sizeof(NhlFontQuality),
		NhlOffset(NhlTextItemLayerRec, text.font_quality),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlHIGH),0,NULL},
	{NhlNtxFontColor,NhlCtxFontColor,NhlTColorIndex,sizeof(NhlColorIndex),
		NhlOffset(NhlTextItemLayerRec, text.font_color),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{ "no.res", "No.res", NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTextItemLayerRec,text.font_height_set),
			NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtxFontHeightF, NhlCtxFontHeightF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec, text.font_height),
		NhlTProcedure,_NhlUSET((NhlPointer)ResUnset) ,0,NULL},
	{ "no.res", "No.res", NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTextItemLayerRec,text.font_aspect_set),
			NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtxFontAspectF, NhlCtxFontAspectF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec, text.font_aspect),
		NhlTProcedure, _NhlUSET((NhlPointer)ResUnset),0,NULL },
	{ "no.res", "No.res", NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTextItemLayerRec,text.font_thickness_set),
			NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtxFontThicknessF, NhlCtxFontThicknessF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec, text.font_thickness),
		NhlTProcedure,_NhlUSET((NhlPointer)ResUnset) ,0,NULL},
	{ "no.res", "No.res", NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTextItemLayerRec,text.constant_spacing_set),
			NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtxConstantSpacingF, NhlCtxConstantSpacingF, NhlTFloat, 
		sizeof(float),
		NhlOffset(NhlTextItemLayerRec, text.constant_spacing),
		NhlTProcedure,_NhlUSET((NhlPointer)ResUnset) ,0,NULL},
	{ "no.res", "No.res", NhlTBoolean, sizeof(NhlBoolean),
		NhlOffset(NhlTextItemLayerRec,text.direction_set),
			NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{ NhlNtxDirection, NhlCtxDirection, NhlTTextDirection, 
		sizeof(NhlTextDirection),
		NhlOffset(NhlTextItemLayerRec, text.direction),
		NhlTProcedure,_NhlUSET((NhlPointer)ResUnset),0,NULL},
	{ NhlNtxFuncCode, NhlCtxFuncCode, NhlTCharacter, 
		sizeof(char),
		NhlOffset(NhlTextItemLayerRec, text.func_code),
		NhlTString,_NhlUSET(":"),0,NULL},


	{NhlNtxPerimOn, NhlCtxPerimOn, NhlTBoolean,sizeof(NhlBoolean),
		 NhlOffset(NhlTextItemLayerRec,text.perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNtxPerimColor,NhlCtxPerimColor,NhlTColorIndex,sizeof(NhlColorIndex),
		NhlOffset(NhlTextItemLayerRec,text.perim_color),
		NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNtxPerimThicknessF, NhlCtxPerimThicknessF, NhlTFloat, 
		 sizeof(float), 
		 NhlOffset(NhlTextItemLayerRec,text.perim_thickness),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNtxPerimDashPattern, NhlCtxPerimDashPattern, NhlTDashIndex, 
		 sizeof(NhlDashIndex), 
		 NhlOffset(NhlTextItemLayerRec,text.perim_dash_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL},
	{NhlNtxPerimDashLengthF, NhlCtxPerimDashLengthF, NhlTFloat, 
		 sizeof(float), 
		 NhlOffset(NhlTextItemLayerRec,text.perim_dash_length),
		 NhlTString,_NhlUSET("0.15"),0,NULL},
	{NhlNtxPerimSpaceF, NhlCtxPerimSpaceF, NhlTFloat, 
		 sizeof(float), 
		 NhlOffset(NhlTextItemLayerRec,text.perim_space),
		 NhlTString,_NhlUSET("0.5"),0,NULL},
	{NhlNtxBackgroundFillColor, NhlCtxBackgroundFillColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),
		 NhlOffset(NhlTextItemLayerRec,text.bg_fill_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlTRANSPARENT),0,NULL},

/* End-documented-resources */
};

/*
* Base Methods used
*/

static NhlErrorTypes TextItemSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes TextItemGetValues(
#if	NhlNeedProto
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
#endif
);

static NhlErrorTypes    TextItemInitialize(
#if	NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes	TextItemDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes	TextItemSegDraw(
#if	NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes	TextItemDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes 	TextItemClassInitialize();

/*
* Private functions
*/

static NhlErrorTypes FigureAndSetTextBBInfo(
#if	NhlNeedProto
	NhlTextItemLayer /*tnew;*/
#endif
);

static void RotEval(
#if	NhlNeedProto
float	* /*tmat */,
float	/* x */,
float   /* y */,
float 	* /*xot */,
float   * /*yot */
#endif
);

NhlTextItemLayerClassRec NhltextItemLayerClassRec = {
	{
/* class_name			*/	"textItemLayerClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlTextItemLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlviewLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	TextItemClassInitialize,
/* layer_initialize		*/	TextItemInitialize,
/* layer_set_values		*/	TextItemSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	TextItemGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	TextItemDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	TextItemDraw,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	TextItemSegDraw,
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

NhlLayerClass NhltextItemLayerClass = (NhlLayerClass)&NhltextItemLayerClassRec;

/*
 * Function:	nhlftextitemclass
 *
 * Description:	Fortran ?referencable? function to return layer class.
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
_NHLCALLF(nhlftextitemlayerclass,NHLFTEXTITEMLAYERCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhltextItemLayerClass;
}

static NrmQuark txstrQ = NrmNULLQUARK;

/*
 * Function:	TextItemClassInitialize
 *
 * Description: Just calls StringToQuark to register new types
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error condition
 *
 * Side Effects: 	NONE
 */
static NhlErrorTypes    TextItemClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{
	_NhlEnumVals	fontqlist[] = {
		{NhlHIGH,	"high"},
		{NhlMEDIUM,	"medium"},
		{NhlLOW,	"low"}
	};

	_NhlEnumVals	textdirlist[] = {
		{NhlDOWN,	"down"},
		{NhlACROSS,	"across"}
	};

	_NhlRegisterEnumType(NhlTFontQuality,fontqlist,NhlNumber(fontqlist));
	_NhlRegisterEnumType(NhlTTextDirection,textdirlist,
							NhlNumber(textdirlist));

	txstrQ = NrmStringToQuark(NhlNtxString);

	return(NhlNOERROR);	
}

static NhlErrorTypes
DoPcCalc
#if	NhlNeedFuncProto
(
	NhlTextItemLayer	tnew
)
#else
(tnew)
	NhlTextItemLayer	tnew;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, ret1 = NhlNOERROR;
	float fr,fl,ft,fb,ur,ul,ut,ub;
	int ll;
	char buf[10];
	/*
	 * 21.0 is the default principle height. TextItems will not allow
	 * principle height to be more than this. However it may be less if
	 * aspect ratio 1/font_aspect <= 1.0  (See Plotchar).
	 */
	if( tnew->text.font_aspect <= 0.0 ) {
		tnew->text.font_aspect = 1.3125;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"TextItem: Aspect ratio cannot be zero or negative");
		ret = MIN(ret,NhlWARNING);
	}
	if(tnew->text.font_aspect <= 1.0) {
		tnew->text.real_ph_height = 21.0 * tnew->text.font_aspect;
		tnew->text.real_ph_width = 21.0;
	} else {
		tnew->text.real_ph_width = 21.0 * 1.0/tnew->text.font_aspect;
		tnew->text.real_ph_height = 21.0;
	}
	/*
	 * The 1.125 is used to make up for the font height difference the
	 * 'SA' parameter of plotchar introduces.
	 */
	tnew->text.real_size = 1.0/tnew->text.font_aspect *
					tnew->text.font_height * 1.125;

	/*
	 * Need to determine how big text is so the real posistions of x and y
	 * can be calculated from the angle and the fields pos_x and pos_y.
	 * GKS better be open!
	 */
	c_pcseti("TE",1);
	c_pcsetr("CS",tnew->text.constant_spacing);
	sprintf(buf,"%c",tnew->text.func_code);
	c_pcsetc("FC",buf);
	c_pcsetr("PH",tnew->text.real_ph_height);
	c_pcsetr("PW",tnew->text.real_ph_width);
	c_pcseti("QU",tnew->text.qual);
	if(tnew->text.qual == 2)
		c_pcseti("FN",1);
	else
		c_pcseti("FN",tnew->text.font);
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);


	ret1 = FigureAndSetTextBBInfo(tnew);
	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);

	return MIN(ret,ret1);
}

/*
 * Function:	TextItemInitialize
 *
 * Description:	Performs initilization of TextItem. This involves copying
 *		user data into internal space and then makes calls to 
 *		plotchar to set plotchars space so text extent computations 
 *		can be done.
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: Plotchar state affected
 */
/*ARGSUSED*/
static NhlErrorTypes    TextItemInitialize
#if	NhlNeedProto
( NhlLayerClass class, NhlLayer req, NhlLayer new, _NhlArgList args,int num_args)
#else
(class,req,new,args,num_args)
	NhlLayerClass	class;
	NhlLayer		req;
	NhlLayer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	char			func[] = "TextItemInitialize";
	NhlTextItemLayer	tnew = (NhlTextItemLayer) new;
	char			*tmp;
	NhlErrorTypes		ret=NhlNOERROR,ret1 = NhlNOERROR;
	float			x,y,width,height;
	float			tmpvx0,tmpvx1,tmpvy0,tmpvy1;
	NhlBoolean		do_view_trans = False;

	if(!tnew->text.pos_x_set) tnew->text.pos_x = 0.0;
	if(!tnew->text.pos_y_set) tnew->text.pos_y = 1.0;
	if(!tnew->text.angle_set) tnew->text.angle = 0.0;
	if(!tnew->text.just_set) tnew->text.just = NhlCENTERCENTER;
	if(!tnew->text.font_height_set) tnew->text.font_height = .05;
	if(!tnew->text.font_aspect_set) tnew->text.font_aspect = 1.3125;
	if(!tnew->text.direction_set) tnew->text.direction = NhlACROSS;
	if(!tnew->text.font_thickness_set) tnew->text.font_thickness = 1.0;
	if(!tnew->text.constant_spacing_set) tnew->text.constant_spacing = 0.0;

	if( tnew->text.perim_space < 0.0 ) {
		tnew->text.perim_space = 0.5;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s: Perimeter space cannot be less than zero",func);
		ret = NhlWARNING;
	}

	tnew->text.x_corners = (float*)NhlMalloc((unsigned)4*sizeof(float));
	tnew->text.y_corners = (float*)NhlMalloc((unsigned)4*sizeof(float));
	if(tnew->text.direction == NhlDOWN) {
		sprintf(tnew->text.dirstr,"%cD%c",tnew->text.func_code,
							tnew->text.func_code);
	} else {
		sprintf(tnew->text.dirstr,"%cA%c",tnew->text.func_code,
							tnew->text.func_code);
	}

	if(strcmp(tnew->text.string,DEFSTRING)==0)
		tmp = (char*)tnew->base.name;
	else
		tmp = tnew->text.string;
	tnew->text.string = (char*)NhlMalloc((unsigned)strlen(tmp)+1);
	if(!tnew->text.string){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	strcpy(tnew->text.string,tmp);

	tnew->text.real_string = (char*)NhlMalloc((unsigned)
						strlen(tnew->text.string)+
						strlen(tnew->text.dirstr)+1);
	if(!tnew->text.real_string){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	strcpy(tnew->text.real_string,tnew->text.dirstr);
	strcat(tnew->text.real_string,tnew->text.string);

	switch(tnew->text.font_quality) {
		case NhlHIGH:
			tnew->text.qual = 0;
			break;
		case NhlMEDIUM:
			tnew->text.qual = 1;
			break;
		case NhlLOW:
			tnew->text.qual = 2;
			break;
	}

	/*
	 * If none of the text size attributes are set, but the view ones
	 * are, then we need to transform the text to the view's coordinates.
	 * If any of the text size attributes are set, then the view's
	 * position coordinates are ignored.  To do this, we have to do all
	 * the plotchar calculations, then transform everything, then
	 * redo the plotchar calculations.
	 */
	if(tnew->view.x_set || tnew->view.y_set ||
			tnew->view.width_set || tnew->view.height_set){

		if(tnew->text.pos_x_set || tnew->text.pos_y_set ||
			tnew->text.angle_set || tnew->text.just_set ||
			tnew->text.direction_set ||
			tnew->text.font_height_set ||
			tnew->text.font_aspect_set ||
			tnew->text.font_thickness_set ||
			tnew->text.constant_spacing_set){

			/*
			 * Only report error if this is a user created
			 * textItem.  (It's parent is a workstation.)
			 */
			if(tnew->base.parent == tnew->base.wkptr){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s: Cannot set x,y,width,and height when other text attributes have been specified also, proceding with other text attribute requests",func);
				ret = MIN(NhlWARNING,ret);
			}
		}
		else{
			do_view_trans = True;
			x = tnew->view.x;
			y = tnew->view.y;
			width = tnew->view.width;
			height = tnew->view.height;
		}
	}

	ret1 = DoPcCalc(tnew);

	if(do_view_trans){
		ret = MIN(ret,ret1);

		/*
		 * Need to reset "view" to what the user set it to be.
		 * So we call setvalues on ourself.
		 */
		ret1 = NhlVASetValues(tnew->base.id,
				NhlNvpXF,	x,
				NhlNvpYF,	y,
				NhlNvpWidthF,	width,
				NhlNvpHeightF,	height,
				NULL);
	}

	return(MIN(ret,ret1));
}


/*
 * Function:	TextItemSetValues
 *
 * Description: Performs same operations as TextItemInitialize except if 
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
static NhlErrorTypes TextItemSetValues
#if	NhlNeedProto
(NhlLayer old,NhlLayer reference,NhlLayer new,_NhlArgList args,int num_args)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int	num_args;
#endif
{
	NhlTextItemLayer told = (NhlTextItemLayer) old;
	NhlTextItemLayer tnew = (NhlTextItemLayer) new;
	NhlErrorTypes ret = NhlNOERROR,ret1 = NhlNOERROR;
	float tmpvx0,tmpvx1,tmpvy0,tmpvy1;
	char *tmp;
	int	rstringchange = 0;

	if( tnew->text.perim_space < 0.0 ) {
		tnew->text.perim_space = 0.5;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
	     "TextItemSetValues: Perimeter space cannot be less than zero");
		ret = NhlWARNING;
	}

	if((tnew->view.x != told->view.x)
		||(tnew->view.width != told->view.width)
		||(tnew->view.y != told->view.y)
		||(tnew->view.height != told->view.height)){

		if((tnew->text.pos_x == told->text.pos_x)
		  &&(tnew->text.pos_y == told->text.pos_y)
		  &&(tnew->text.angle == told->text.angle)
		  &&(tnew->text.just == told->text.just)
		  &&(tnew->text.direction == told->text.direction)
		  &&(!_NhlArgIsSet(args,num_args,NhlNtxFontHeightF))
		  &&(tnew->text.font_aspect == told->text.font_aspect)
		  &&(tnew->text.font_thickness == told->text.font_thickness)
		  &&(tnew->text.constant_spacing==told->text.constant_spacing)){
/*
* Only case where x,y,width and height can be set. Need to compute new values
* for font_height, font_thickness, pos_x and pos_y. All other text atts can
* remain the same.
*/
			_NhlEvalTrans(tnew->view.trans_children,
				tnew->text.pos_x,tnew->text.pos_y,
				&tnew->text.pos_x,&tnew->text.pos_y);
			_NhlEvalTrans(tnew->view.trans_children,
				tnew->text.heightvecx[0],
				tnew->text.heightvecy[0],
				&tmpvx0,&tmpvy0);
			_NhlEvalTrans(tnew->view.trans_children,
				tnew->text.heightvecx[1],
				tnew->text.heightvecy[1],
				&tmpvx1,&tmpvy1);
			tnew->text.font_height = (float)sqrt((float)(
				((tmpvx1-tmpvx0)*(tmpvx1-tmpvx0)) 
				+((tmpvy1-tmpvy0)*(tmpvy1-tmpvy0))));
		
		} else if(tnew->base.parent == tnew->base.wkptr){
		  NhlPError(NhlWARNING,NhlEUNKNOWN,"TextItemSetValues: Can not change x,y,width,and height when other text attribute changes have been requested also, preceding with other text attribute requests");
		  ret = MIN(ret,NhlWARNING);
		}
	} 

	if((tnew->text.direction != told->text.direction)||(tnew->text.func_code != told->text.func_code)){
		rstringchange = 1;
		if(tnew->text.direction == NhlDOWN) {
			sprintf(tnew->text.dirstr,"%cD%c",tnew->text.func_code,
							tnew->text.func_code);
		} else {
			sprintf(tnew->text.dirstr,"%cA%c",tnew->text.func_code,
							tnew->text.func_code);
		}
	}

	if(tnew->text.string != told->text.string) 
	{
		rstringchange = 1;
		NhlFree(told->text.string);
		tmp = tnew->text.string;
		tnew->text.string= (char*)NhlMalloc((unsigned)strlen(tmp)+1);
		strcpy(tnew->text.string,tmp);
	}

	if(rstringchange){
		NhlFree(told->text.real_string);
		tnew->text.real_string = (char*)NhlMalloc((unsigned)
						strlen(tnew->text.string)+
						strlen(tnew->text.dirstr)+1);
		strcpy(tnew->text.real_string,tnew->text.dirstr);
		strcat(tnew->text.real_string,tnew->text.string);
	}

	switch(tnew->text.font_quality) {
		case NhlHIGH:
			tnew->text.qual = 0;
			break;
		case NhlMEDIUM:
			tnew->text.qual = 1;
			break;
		case NhlLOW:
			tnew->text.qual = 2;
			break;
	}

	ret1 = DoPcCalc(tnew);

        return(MIN(ret,ret1));
}

/*
 * Function:	TextItemGetValues
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
TextItemGetValues
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
	char			func[] = "TextItemGetValues";
	NhlErrorTypes		ret = NhlNOERROR;
	NhlTextItemLayerPart	*tep = &((NhlTextItemLayer)l)->text;
	int			i;

	for(i=0;i<nargs;i++){

		if((args[i].quark == txstrQ) && tep->string){
			*(NhlString*)args[i].value.ptrval =
				(NhlString)NhlMalloc(strlen(tep->string)+1);
			if(*(NhlString*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNtxString);
				ret = MIN(ret,NhlWARNING);
			}
			strcpy(*(NhlString*)args[i].value.ptrval,tep->string);
		}
	}

	return ret;
}

/*
 * Function:	TextItemDraw
 *
 * Description:  Activates parent workstation, calls plotchar parameter setting
 *		functions and then GKS attributes for linewidth, fill styles,
 *		fill colors and line colors.
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
static NhlErrorTypes    TextItemDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
	NhlLayer 	layer;
#endif
{
	NhlTextItemLayer tlayer = (NhlTextItemLayer) layer;
	float fl,fr,fb,ft,ul,ur,ub,ut;
	int ll;
	NhlErrorTypes ret = NhlNOERROR;
	char buf[10];

	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);

	c_pcseti("TE",0);
	c_pcsetr("CS",tlayer->text.constant_spacing);
	c_pcsetr("PH",tlayer->text.real_ph_height);
	c_pcsetr("PW",tlayer->text.real_ph_width);
	c_pcseti("QU",tlayer->text.qual);
	c_pcseti("FN",tlayer->text.font);
	c_pcseti("OC",_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	c_pcseti("CC",_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	sprintf(buf,"%c",tlayer->text.func_code);
	c_pcsetc("FC",buf);

	_NhlActivateWorkstation(tlayer->base.wkptr);
	if(tlayer->text.qual == 2){
		Gtext_font_prec gtfp;
		gtfp.font = 1;
		gtfp.prec = GPREC_STROKE;
		gset_text_font_prec(&gtfp);
	}
	NhlVASetValues(tlayer->base.wkptr->base.id,
		_NhlNwkReset,	True,
		NULL);

	c_set(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1);

/* first draw the perimeter: it may have a solid background */

	if (tlayer->text.perim_on || 
	    tlayer->text.bg_fill_color > NhlTRANSPARENT) {

		NhlVASetValues(tlayer->base.wkptr->base.id,
			       _NhlNwkDrawEdges,tlayer->text.perim_on,
			       _NhlNwkEdgeDashPattern,
			       tlayer->text.perim_dash_pattern,
			       _NhlNwkEdgeThicknessF,
			       tlayer->text.perim_thickness,
			       _NhlNwkEdgeDashSegLenF,
			       tlayer->text.perim_dash_length,
			       _NhlNwkEdgeColor,tlayer->text.perim_color,
			       _NhlNwkFillColor,tlayer->text.bg_fill_color,
			       _NhlNwkFillIndex,NhlSOLIDFILL,
			       NULL);
			
		_NhlSetFillInfo(tlayer->base.wkptr,layer);
		_NhlWorkstationFill(tlayer->base.wkptr,
				    tlayer->text.xperim,
				    tlayer->text.yperim,5);
			
	}

	gset_linewidth((Gdouble)tlayer->text.font_thickness);
	gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	gset_fill_style_ind(GSTYLE_SOLID);
	gset_marker_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	

	c_plchhq(tlayer->text.real_x_pos,tlayer->text.real_y_pos,
		tlayer->text.real_string,tlayer->text.real_size,
		tlayer->text.angle,tlayer->text.cntr);

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);

	_NhlDeactivateWorkstation(tlayer->base.wkptr);
	return(ret);
}


/*
 * Function:	TextItemSegDraw
 *
 * Description:  Activates parent workstation, calls plotchar parameter setting
 *		functions and then GKS attributes for linewidth, fill styles,
 *		fill colors and line colors.
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
static NhlErrorTypes    TextItemSegDraw
#if	NhlNeedProto
(NhlLayer layer)
#else
(layer)
	NhlLayer 	layer;
#endif
{
	NhlTextItemLayer tlayer = (NhlTextItemLayer) layer;
	float fl,fr,fb,ft,ul,ur,ub,ut;
	int ll;
	NhlErrorTypes ret = NhlNOERROR;
	char buf[10];

	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);

	c_pcseti("TE",0);
	c_pcsetr("CS",tlayer->text.constant_spacing);
	c_pcsetr("PH",tlayer->text.real_ph_height);
	c_pcsetr("PW",tlayer->text.real_ph_width);
	c_pcseti("QU",tlayer->text.qual);
	if(tlayer->text.qual == 2){
		Gtext_font_prec gtfp;
		gtfp.font = 1;
		gtfp.prec = GPREC_STROKE;
		gset_text_font_prec(&gtfp);
	}
	c_pcseti("FN",tlayer->text.font);
	c_pcseti("OC",_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	c_pcseti("CC",_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	sprintf(buf,"%c",tlayer->text.func_code);
	c_pcsetc("FC",buf);

	c_set(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1);
	gset_linewidth((Gdouble)tlayer->text.font_thickness);
	gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	gset_line_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	gset_fill_style_ind(GSTYLE_SOLID);
	gset_marker_colr_ind((Gint)_NhlGetGksCi(tlayer->base.wkptr,tlayer->text.font_color));
	

	c_plchhq(tlayer->text.real_x_pos,tlayer->text.real_y_pos,
		tlayer->text.real_string,tlayer->text.real_size,
		tlayer->text.angle,tlayer->text.cntr);

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);

	return(ret);
}

/*
 * Function:	TextItemDestroy
 *
 * Description: Frees both dynamically allocated strings.
 *
 * In Args:	NhlLayer inst	instance of textitem
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes    TextItemDestroy
#if	NhlNeedProto
(NhlLayer  inst)
#else
(inst)
	NhlLayer	inst;
#endif
{
	NhlTextItemLayer tinst = (NhlTextItemLayer) inst;
	
	NhlFree(tinst->text.string);
	NhlFree(tinst->text.real_string);
	return(NhlNOERROR);
}


/*
 * Function:	RotEval
 *
 * Description: computes the transformation of x and y with the matix tmat
 *		the transformation matrix is a 3x3.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static void RotEval
#if	NhlNeedProto
(float *tmat,float x,float y,float *xot,float *yot)
#else
(tmat,x,y,xot,yot)
	float	*tmat;
	float	x;
	float	y;
	float	*xot;
	float	*yot;
#endif
{
	float tmp;
	*xot = tmat[0] * x + tmat[1] * y + tmat[2];
	*yot = tmat[3] * x + tmat[4] * y + tmat[5];
	tmp =  tmat[6] * x + tmat[7] * y + tmat[8];
	*xot = *xot/tmp;
	*yot = *yot/tmp;
	return;
}

/*
 * Function:	FigureAndSetTextBBInfo
 *
 * Description: Used to compute appropriate information for plotchar based on
 *		resource field values and used to compute final x,y,width and
 *		height information of the text object.  This is made into a
 *		function because both initialize and setvalues need to do the
 *		same thing
 *
 * In Args: 	tnew: is the new instance record for the textitem
 *
 * Out Args:	tnew:
 *
 * Return Values: NONE
 *
 * Side Effects: Lots of fields in tnew are set, Also performs set.
 */
static NhlErrorTypes FigureAndSetTextBBInfo
#if	NhlNeedProto
(NhlTextItemLayer tnew)
#else
(tnew)
	NhlTextItemLayer tnew;
#endif
{
	float tmpdr,tmpdl,tmpdb,tmpdt;
	float xpoints[4],ypoints[4];
	float tmat[3][3];
	float minx,miny,maxx,maxy;
	NhlErrorTypes ret = NhlNOERROR;
	int i;
	float space;
/*
* All points get calculated as if no rotation is going to happen then
* all points are passed through a generic 2D rotational matrix, which gives
* four points from which the x,y,width,and height of this text object can
* be determined. Pre-rotation points for the corners of the text are also 
* determined at this time. 
*                                                                               
*         P1--------------P2                                                    
*          |               |                                                    
*          |               |                                                    
*          |               |                                                    
*         P0              P3                                                    
*/

	c_set(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1);
	switch(tnew->text.just) {
/*
* Middle justification points
*/
		case NhlCENTERLEFT:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.cntr = -1.0;
			} else {
				tnew->text.cntr = 0.0;
			}
			c_plchhq(0.5,0.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y;
			} else {
				tnew->text.real_x_pos = tnew->text.pos_x + tmpdl;
				tnew->text.real_y_pos = tnew->text.pos_y;
			}
			break;
		case NhlCENTERCENTER:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cntr option be correct for the computations
* following to be correct 
*
*/
			tnew->text.real_x_pos = tnew->text.pos_x;
			tnew->text.real_y_pos = tnew->text.pos_y;
			tnew->text.cntr = 0.0;
			c_plchhq(tnew->text.real_x_pos,tnew->text.real_y_pos,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			break;
		case NhlCENTERRIGHT:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.cntr = 1.0;
			} else {
				tnew->text.cntr = 0.0;
			}
			c_plchhq(0.5,0.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y;
			} else {
				tnew->text.real_x_pos = tnew->text.pos_x - tmpdr;
				tnew->text.real_y_pos = tnew->text.pos_y;
			}
			break;
/*
* Top justification points
*/
		case NhlTOPLEFT:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			
			tnew->text.cntr = -1.0;
			c_plchhq(.5,.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y - tmpdt;
			} else {
				tnew->text.real_x_pos = tnew->text.pos_x + tmpdl;
				tnew->text.real_y_pos = tnew->text.pos_y;
			}
/* 
* when on top real_y is below pos_y
*/
			break;
		case NhlTOPCENTER:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.cntr = 0.0;
			} else {
				tnew->text.cntr = -1.0;
			}
			c_plchhq(.5,.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y - tmpdt;
			} else {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y;
			}
			break;
		case NhlTOPRIGHT:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.cntr = 1.0;
			} else {
				tnew->text.cntr = -1.0;
			}
			c_plchhq(0.5,0.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y - tmpdt;
			} else {
				tnew->text.real_x_pos = tnew->text.pos_x - tmpdr;
				tnew->text.real_y_pos = tnew->text.pos_y;
			}
			break;
/*
* Bottom justification points
*/
		case NhlBOTTOMLEFT:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.cntr = -1.0;
			} else {
				tnew->text.cntr = 1.0;
			}
			c_plchhq(.5,.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y + tmpdb;
			} else {
				tnew->text.real_x_pos = tnew->text.pos_x + tmpdl;
				tnew->text.real_y_pos = tnew->text.pos_y;
			}
			break;
		case NhlBOTTOMCENTER:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.cntr = 0.0;
			} else {
				tnew->text.cntr = 1.0;
			}
			c_plchhq(.5,.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y + tmpdb;
			} else {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y;
			}
			break;
		case NhlBOTTOMRIGHT:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			tnew->text.cntr = 1.0;
			c_plchhq(.5,.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			if(tnew->text.direction == NhlACROSS) {
				tnew->text.real_x_pos = tnew->text.pos_x;
				tnew->text.real_y_pos = tnew->text.pos_y + tmpdb;
			} else {
				tnew->text.real_x_pos = tnew->text.pos_x - tmpdr;
				tnew->text.real_y_pos = tnew->text.pos_y;
			}
			break;
		default:
			NhlPError(NhlWARNING,NhlEUNKNOWN,"TextItemInitialize: Incorect justification point, using default");
			ret = NhlWARNING;
			break;
	}

	xpoints[0] = tnew->text.real_x_pos - tmpdl;
	ypoints[0] = tnew->text.real_y_pos - tmpdb;
	tnew->text.heightvecx[0] = xpoints[0];
	tnew->text.heightvecy[0] = ypoints[0];
	tnew->text.heightvecx[1] = xpoints[0];
	tnew->text.heightvecy[1] = ypoints[0] + tnew->text.font_height;

/*
 * If the background is colored or the perimeter outline is on the spacing
 * between the text and the perimeter box must be added to the text bounding
 * box, before performing the rotation transformation.
 */ 
	if (tnew->text.perim_on || 
	    tnew->text.bg_fill_color > NhlTRANSPARENT)
		space = tnew->text.font_height * tnew->text.perim_space;
	else
		space = 0.0;

	xpoints[0] = tnew->text.real_x_pos - tmpdl - space;
	ypoints[0] = tnew->text.real_y_pos - tmpdb - space;
	xpoints[1] = tnew->text.real_x_pos - tmpdl - space;
	ypoints[1] = tnew->text.real_y_pos + tmpdt + space;
	xpoints[2] = tnew->text.real_x_pos + tmpdr + space;
	ypoints[2] = tnew->text.real_y_pos + tmpdt + space;
	xpoints[3] = tnew->text.real_x_pos + tmpdr + space;
	ypoints[3] = tnew->text.real_y_pos - tmpdb - space;
/*
* Formula for orthoganal rotation taken from Foley/VanDamn
*/
	tmat[0][0] = (float)cos((double)(tnew->text.angle*DEGTORAD));
	tmat[1][0] = (float)sin((double)(tnew->text.angle*DEGTORAD));
	tmat[2][0] = 0.0;
	tmat[0][1] = -(float)sin((double)(tnew->text.angle*DEGTORAD));
	tmat[1][1] = (float)cos((double)(tnew->text.angle*DEGTORAD));
	tmat[2][1] = 0.0;
	tmat[0][2] = (tnew->text.pos_x 
			* (1.0-(float)cos((double)(tnew->text.angle*DEGTORAD))))
			+ (tnew->text.pos_y 
			* (float)sin((double)(tnew->text.angle*DEGTORAD)));
	tmat[1][2] = (tnew->text.pos_y 
			* (1.0-(float)cos((double)(tnew->text.angle*DEGTORAD))))
			- (tnew->text.pos_x 
			* (float)sin((double)(tnew->text.angle*DEGTORAD)));
	tmat[2][2] = 1.0;

	

	RotEval(tmat[0],tnew->text.real_x_pos,tnew->text.real_y_pos,
		&tnew->text.real_x_pos,&tnew->text.real_y_pos);	
	RotEval(tmat[0],xpoints[0],ypoints[0],
		&(xpoints[0]),&(ypoints[0]));	
	RotEval(tmat[0],xpoints[1],ypoints[1],
		&(xpoints[1]),&(ypoints[1]));	
	RotEval(tmat[0],xpoints[2],ypoints[2],
		&(xpoints[2]),&(ypoints[2]));	
	RotEval(tmat[0],xpoints[3],ypoints[3],
		&(xpoints[3]),&(ypoints[3]));	
	RotEval(tmat[0],tnew->text.heightvecx[0],tnew->text.heightvecy[0],
		&(tnew->text.heightvecx[0]),&(tnew->text.heightvecy[0]));
	RotEval(tmat[0],tnew->text.heightvecx[1],tnew->text.heightvecy[1],
		&(tnew->text.heightvecx[1]),&(tnew->text.heightvecy[1]));
/*
 * Save the text perimeter points so that the perimeter can be drawn if
 * required.
 */
	memcpy((char*)tnew->text.xperim,(char*)xpoints,4*sizeof(float));
	memcpy((char*)tnew->text.yperim,(char*)ypoints,4*sizeof(float));
	tnew->text.xperim[4] = tnew->text.xperim[0];
	tnew->text.yperim[4] = tnew->text.yperim[0];

/*
* Save height vector so setvalues can compute with it
*/

	minx = 1e10;
	maxx = -1e10;
	miny = 1e10;
	maxy = -1e10;
	for(i= 0; i<4; i++ ) {
		tnew->text.x_corners[i] = xpoints[i];
		tnew->text.y_corners[i] = ypoints[i];
		if(xpoints[i] < minx)
			minx = xpoints[i];
		if(xpoints[i] > maxx)
			maxx = xpoints[i];
		if(ypoints[i] < miny)
			miny = ypoints[i];
		if(ypoints[i] > maxy)
			maxy = ypoints[i];
	}
/*
* ---------> Normally a bad idea to directly set superclass fields. However
* text does not have children or use segments so OK  I still need to configure
* all the private fields so there are no problems. This includes reseting the
* thetrans_children transformation data. <-----------
*/
	_NhlInternalSetView((NhlViewLayer)tnew,minx,maxy,maxx - minx,maxy - miny,1);
/*
* DONE RECONFIGURING VIEW
*/

	return(ret);
}
