/*
 *      $Id: TextItem.c,v 1.5 1994-01-29 00:29:39 boote Exp $
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
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/Converters.h>
#include <ncarg/hlu/TextItemP.h>

#define DEFSTRING "NOTHING"
#define DEGTORAD 0.017453293
static NhlResource resources[] = {
	{ NhlNtxString, NhlCtxString, NhlTString, sizeof(char*),
		NhlOffset(NhlTextItemLayerRec,text.string),
		NhlTImmediate,DEFSTRING},
	{ NhlNtxPosXF, NhlCtxPosXF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec,text.pos_x),
		NhlTString,"0.0" },
	{ NhlNtxPosYF, NhlCtxPosYF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec,text.pos_y),
		NhlTString,"1.0" },
	{ NhlNtxAngleF, NhlCtxAngleF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec,text.angle),
		NhlTString,"0.0" },
	{ NhlNtxFont, NhlCFont, NhlTFont, sizeof(NhlFont),
		NhlOffset(NhlTextItemLayerRec, text.font),
		NhlTImmediate,0 },
	{ NhlNtxJust, NhlCtxJust, NhlTInteger, sizeof(int),
		NhlOffset(NhlTextItemLayerRec, text.just),
		NhlTImmediate,(NhlPointer)4},
	{ NhlNtxFontQuality, NhlCtxFontQuality, NhlTFQuality, 
		sizeof(NhlFontQuality),
		NhlOffset(NhlTextItemLayerRec, text.font_quality),
		NhlTImmediate,(NhlPointer)NhlHIGH},
	{ NhlNtxFontColor, NhlCtxFontColor, NhlTInteger, sizeof(int),
		NhlOffset(NhlTextItemLayerRec, text.font_color),
		NhlTImmediate, (NhlPointer)1},
	{ NhlNtxFontHeightF, NhlCtxFontHeightF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec, text.font_height),
		NhlTString, ".05" },
	{ NhlNtxFontAspectF, NhlCtxFontAspectF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec, text.font_aspect),
		NhlTString, "1.3125" }, /* 21.0/16.0 see plotchar */
	{ NhlNtxFontThicknessF, NhlCtxFontThicknessF, NhlTFloat, sizeof(float),
		NhlOffset(NhlTextItemLayerRec, text.font_thickness),
		NhlTString, "1.0" },
	{ NhlNtxConstantSpacingF, NhlCtxConstantSpacingF, NhlTFloat, 
		sizeof(float),
		NhlOffset(NhlTextItemLayerRec, text.constant_spacing),
		NhlTString, "0.0" },
	{ NhlNtxDirection, NhlCtxDirection, NhlTTextDirection, 
		sizeof(NhlTextDirection),
		NhlOffset(NhlTextItemLayerRec, text.direction),
		NhlTImmediate, (NhlPointer)NhlACROSS},
	{ NhlNtxFuncCode, NhlCtxFuncCode, NhlTCharacter, 
		sizeof(char),
		NhlOffset(NhlTextItemLayerRec, text.func_code),
		NhlTString,":"},
	{ NhlNtxXCorners, NhlCtxXCorners, NhlTFloatPtr,sizeof(float*),
		NhlOffset(NhlTextItemLayerRec, text.x_corners),
		NhlTImmediate, NULL },
	{ NhlNtxYCorners, NhlNtxYCorners, NhlTFloatPtr,sizeof(float*),
		NhlOffset(NhlTextItemLayerRec, text.y_corners),
		NhlTImmediate, NULL }
};

/*
* Base Methods used
*/

static NhlErrorTypes TextItemSetValues(
#ifdef NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes    TextItemInitialize(
#ifdef NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes	TextItemDraw(
#ifdef NhlNeedProto
        NhlLayer   /* layer */
#endif
);

static NhlErrorTypes	TextItemDestroy(
#ifdef NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes 	TextItemClassInitialize();

/*
* Private functions
*/

static NhlErrorTypes FigureAndSetTextBBInfo(
#ifdef NhlNeedProto
	NhlTextItemLayer /*tnew;*/
#endif
);

static void RotEval(
#ifdef NhlNeedProto
float	* /*tmat */,
float	/* x */,
float   /* y */,
float 	* /*xot */,
float   * /*yot */
#endif
);

NhlTextItemLayerClassRec NhltextItemLayerClassRec = {
	{
/* class_name			*/	"TextItem",
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
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	TextItemDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	TextItemDraw,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
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
#if __STDC__
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
	float fl,fr,ft,fb,ul,ur,ut,ub;
	int ll;
	char *tmp,buf[10];
	int	rstringchange = 0;

	if((tnew->view.x != told->view.x)
		||(tnew->view.width != told->view.width)
		||(tnew->view.y != told->view.y)
		||(tnew->view.height != told->view.height)){

		if((tnew->text.string == told->text.string)
		  &&(tnew->text.pos_x == told->text.pos_x)
		  &&(tnew->text.pos_y == told->text.pos_y)
		  &&(tnew->text.angle == told->text.angle)
		  &&(tnew->text.just == told->text.just)
		  &&(tnew->text.direction == told->text.direction)
		  &&(tnew->text.font == told->text.font)
		  &&(!_NhlArgIsSet(args,num_args,NhlNtxFontHeightF))
		  &&(tnew->text.font_aspect == told->text.font_aspect)){
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
		
		} else {
		  NhlPError(NhlWARNING,NhlEUNKNOWN,"TextItemSetValues: Can not change x,y,width,and height when other text attribute changes have been requested also, proceding with other text attribute requests");
		  ret = NhlWARNING;
		}
	} 

	if((tnew->text.direction != told->text.direction)||(tnew->text.func_code != told->text.func_code)){
		rstringchange = 1;
		if((tnew->text.direction == NhlUP)||
					(tnew->text.direction == NhlDOWN)) {
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
/*
* 21.0 is the default principle height. TextItems will not allow principle 
* height to be more than this. However it may be less if aspect ratio 
*  1/font_aspect <= 1.0  (See Plotchar).
*/
	if( tnew->text.font_aspect <= 0.0 ) {
		tnew->text.font_aspect = 1.3125;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"TextItemSetValues: Aspect ratio cannont be zero or negative");
		ret = NhlWARNING;
	}
	if(tnew->text.font_aspect <= 1.0) {
		tnew->text.real_ph_height = 21.0 * tnew->text.font_aspect;
		tnew->text.real_ph_width = 21.0;
	} else {
		tnew->text.real_ph_width = 21.0 * 1.0/tnew->text.font_aspect;
		tnew->text.real_ph_height = 21.0;
	}
	tnew->text.real_size = 1.0/tnew->text.font_aspect * tnew->text.font_height;
/*
* Need to determine how big text is so the real posistions of x and y can
* be calculated from the angle and the fields pos_x and pos_y.
* GKS better be open!
*/
	c_pcseti("TE",1);
	c_pcsetr("CS",tnew->text.constant_spacing);
	sprintf(buf,"%c",tnew->text.func_code);
	c_pcsetc("FC",buf);
	c_pcsetr("PH",tnew->text.real_ph_height);
	c_pcsetr("PW",tnew->text.real_ph_width);
	c_pcseti("QU",tnew->text.qual);
	c_pcseti("FN",tnew->text.font);
        c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
        ret1 = FigureAndSetTextBBInfo(tnew);
        c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
        return(MIN(ret,ret1));
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
#if __STDC__
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
	NhlTextItemLayer	tnew = (NhlTextItemLayer) new;
	char* tmp;
	NhlErrorTypes	ret=NhlNOERROR,ret1 = NhlNOERROR;
	int ll;
	float fr,fl,ft,fb,ur,ul,ut,ub;
	char buf[10];


	tnew->text.x_corners = (float*)NhlMalloc((unsigned)4*sizeof(float));
	tnew->text.y_corners = (float*)NhlMalloc((unsigned)4*sizeof(float));
	if((tnew->text.direction == NhlUP)||(tnew->text.direction == NhlDOWN)) {
		sprintf(tnew->text.dirstr,"%cD%c",tnew->text.func_code,tnew->text.func_code);
	} else {
		sprintf(tnew->text.dirstr,"%cA%c",tnew->text.func_code,tnew->text.func_code);
	}
	if(strcmp(tnew->text.string,DEFSTRING)==0) {
		tnew->text.string = (char*) 
			NhlMalloc((unsigned)strlen(tnew->base.name) +1);
		strcpy(tnew->text.string,tnew->base.name);
	} else {
		tmp = tnew->text.string;
		tnew->text.string= (char*)NhlMalloc((unsigned)strlen(tmp)+1);
		strcpy(tnew->text.string,tmp);
	}

	tnew->text.real_string = (char*)NhlMalloc((unsigned)
						strlen(tnew->text.string)+
						strlen(tnew->text.dirstr)+1);
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
* 21.0 is the default principle height. TextItems will not allow principle 
* height to be more than this. However it may be less if aspect ratio 
*  1/font_aspect <= 1.0  (See Plotchar).
*/
	if( tnew->text.font_aspect <= 0.0 ) {
		tnew->text.font_aspect = 1.3125;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"TextItemSetValues: Aspect ratio cannont be zero or negative");
		ret = NhlWARNING;
	}
	if(tnew->text.font_aspect <= 1.0) {
		tnew->text.real_ph_height = 21.0 * tnew->text.font_aspect;
		tnew->text.real_ph_width = 21.0;
	} else {
		tnew->text.real_ph_width = 21.0 * 1.0/tnew->text.font_aspect;
		tnew->text.real_ph_height = 21.0;
	}
	tnew->text.real_size = 1.0/tnew->text.font_aspect * tnew->text.font_height;
/*
* Need to determine how big text is so the real posistions of x and y can
* be calculated from the angle and the fields pos_x and pos_y.
* GKS better be open!
*/
	c_pcseti("TE",1);
	c_pcsetr("CS",tnew->text.constant_spacing);
	sprintf(buf,"%c",tnew->text.func_code);
	c_pcsetc("FC",buf);
	c_pcsetr("PH",tnew->text.real_ph_height);
	c_pcsetr("PW",tnew->text.real_ph_width);
	c_pcseti("QU",tnew->text.qual);
	c_pcseti("FN",tnew->text.font);
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
	ret1 = FigureAndSetTextBBInfo(tnew);
	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
	return(MIN(ret,ret1));
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
#if  __STDC__
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

	_NhlDeactivateWorkstation(tlayer->base.wkptr);
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
#if  __STDC__
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
#if  __STDC__
(void)
#else
()
#endif
{
	NhlConvertArg	fontqlist[] = {
				{NhlSTRENUM,	NhlHIGH,	"high"},
				{NhlSTRENUM,	NhlMEDIUM,	"medium"},
				{NhlSTRENUM,	NhlLOW,		"low"}
				};

	NhlConvertArg	textdirlist[] = {
				{NhlSTRENUM,	NhlDOWN,	"down"},
				{NhlSTRENUM,	NhlACROSS,	"across"},
				{NhlSTRENUM,	NhlUP,		"up"}
				};

	NhlRegisterConverter(NhlTString,NhlTFQuality,NhlCvtStringToEnum,
				fontqlist,NhlNumber(fontqlist),False,NULL);
	NhlRegisterConverter(NhlTString,NhlTTextDirection,NhlCvtStringToEnum,
				textdirlist,NhlNumber(textdirlist),False,NULL);

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
#if __STDC__
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
#if  __STDC__
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
	
	_NhlActivateWorkstation(tnew->base.wkptr);
	c_set(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1);
	switch(tnew->text.just) {
/*
* Middle justification points
*/
		case 1:
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
		case 4:
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
		case 7:
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
		case 0:
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
		case 3:
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
		case 6:
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
		case 2:
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
		case 5:
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
		case 8:
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
	_NhlDeactivateWorkstation(tnew->base.wkptr);
	xpoints[0] = tnew->text.real_x_pos - tmpdl;
	ypoints[0] = tnew->text.real_y_pos - tmpdb;
	xpoints[1] = tnew->text.real_x_pos - tmpdl;
	ypoints[1] = tnew->text.real_y_pos + tmpdt;
	xpoints[2] = tnew->text.real_x_pos + tmpdr;
	ypoints[2] = tnew->text.real_y_pos + tmpdt;
	xpoints[3] = tnew->text.real_x_pos + tmpdr;
	ypoints[3] = tnew->text.real_y_pos - tmpdb;
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

	

	tnew->text.heightvecx[0] = xpoints[0];
	tnew->text.heightvecy[0] = ypoints[0];
	tnew->text.heightvecx[1] = xpoints[0];
	tnew->text.heightvecy[1] = ypoints[0] + tnew->text.font_height;
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
