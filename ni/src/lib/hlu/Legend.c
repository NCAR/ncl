
/*
 *      $Id: Legend.c,v 1.1 1993-07-27 18:03:04 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Legend.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	Legend source for drawing text. 
 *	-------> Text is special. It does not need a transformation and
 *		always draws in NDC coordinates. Therefore, text must 
 *		perform getset's to preserve current transformation.<------
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/LegendP.h>

/* SUPPRESS 112 */

#define DEFSTRING "NOTHING"
#define DEGTORAD 0.017453293
static NhlResource resources[] = {
	{ NhlNtxString, NhlCtxString, NhlTString, sizeof(char*),
		NhlOffset(LegendLayerRec,text.string),
		NhlTImmediate,DEFSTRING},
	{ NhlNtxPosXF, NhlCtxPosXF, NhlTFloat, sizeof(float),
		NhlOffset(LegendLayerRec,text.pos_x),
		NhlTString,"0.0" },
	{ NhlNtxPosYF, NhlCtxPosYF, NhlTFloat, sizeof(float),
		NhlOffset(LegendLayerRec,text.pos_y),
		NhlTString,"1.0" },
	{ NhlNtxAngleF, NhlCtxAngleF, NhlTFloat, sizeof(float),
		NhlOffset(LegendLayerRec,text.angle),
		NhlTString,"0.0" },
	{ NhlNtxFont, NhlCtxFont, NhlTInteger, sizeof(int),
		NhlOffset(LegendLayerRec, text.font),
		NhlTImmediate,0 },
	{ NhlNtxJust, NhlCtxJust, NhlTInteger, sizeof(int),
		NhlOffset(LegendLayerRec, text.just),
		NhlTImmediate,(NhlPointer)4},
	{ NhlNtxFontQuality, NhlCtxFontQuality, NhlTFQuality, 
		sizeof(FontQuality),
		NhlOffset(LegendLayerRec, text.font_quality),
		NhlTImmediate,(NhlPointer)HIGH},
	{ NhlNtxFontColor, NhlCtxFontColor, NhlTInteger, sizeof(int),
		NhlOffset(LegendLayerRec, text.font_color),
		NhlTImmediate, (NhlPointer)1},
	{ NhlNtxFontHeightF, NhlCtxFontHeightF, NhlTFloat, sizeof(float),
		NhlOffset(LegendLayerRec, text.font_height),
		NhlTString, ".05" },
	{ NhlNtxFontAspectF, NhlCtxFontAspectF, NhlTFloat, sizeof(float),
		NhlOffset(LegendLayerRec, text.font_aspect),
		NhlTString, "1.3125" }, /* 21.0/16.0 see plotchar */
	{ NhlNtxFontThicknessF, NhlCtxFontThicknessF, NhlTFloat, sizeof(float),
		NhlOffset(LegendLayerRec, text.font_thickness),
		NhlTString, "1.0" },
	{ NhlNtxConstantSpacingF, NhlCtxConstantSpacingF, NhlTFloat, 
		sizeof(float),
		NhlOffset(LegendLayerRec, text.constant_spacing),
		NhlTString, "0.0" },
	{ NhlNtxDirection, NhlCtxDirection, NhlTTextDirection, 
		sizeof(TextDirection),
		NhlOffset(LegendLayerRec, text.direction),
		NhlTImmediate, (NhlPointer)ACROSS},
	{ NhlNtxFuncCode, NhlCtxFuncCode, NhlTCharacter, 
		sizeof(char),
		NhlOffset(LegendLayerRec, text.func_code),
		NhlTString,":"},
	{ NhlNtxXCorners, NhlCtxXCorners, NhlTFloatPtr,sizeof(float*),
		NhlOffset(LegendLayerRec, text.x_corners),
		NhlTImmediate, NULL },
	{ NhlNtxYCorners, NhlNtxYCorners, NhlTFloatPtr,sizeof(float*),
		NhlOffset(LegendLayerRec, text.y_corners),
		NhlTImmediate, NULL }
};

/*
* Base Methods used
*/

static NhlErrorTypes LegendSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes    LegendInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes	LegendDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

static NhlErrorTypes	LegendDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes 	LegendClassInitialize();

/*
* Private functions
*/

static NhlErrorTypes FigureAndSetTextBBInfo(
#ifdef NhlNeedProto
	LegendLayer /*tnew;*/
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

LegendLayerClassRec legendLayerClassRec = {
	{
/* superclass			*/	(LayerClass)&viewLayerClassRec,
/* class_name			*/	"Legend",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(LegendLayerRec),
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NULL,
/* class_inited			*/	False,
/* class_initialize		*/	LegendClassInitialize,
/* layer_initialize		*/	LegendInitialize,
/* layer_set_values		*/	LegendSetValues,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	LegendDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	LegendDestroy
	},
	{
/* segment_workstation */ -1,
/* get_bb */		NULL, /* ---------> Do I need one?<---------*/
	},
	{
			NULL
	}
};

LayerClass legendLayerClass = (LayerClass)&legendLayerClassRec;


/*
 * Function:	LegendSetValues
 *
 * Description: Performs same operations as LegendInitialize except if 
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
static NhlErrorTypes LegendSetValues
#if __STDC__
(Layer old,Layer reference,Layer new,_NhlArgList args,int num_args)
#else
(old,reference,new,args,num_args)
	Layer	old;
	Layer	reference;
	Layer	new;
	_NhlArgList	args;
	int	num_args;
#endif
{
	LegendLayer told = (LegendLayer) old;
	LegendLayer tnew = (LegendLayer) new;
	NhlErrorTypes ret = NOERROR,ret1 = NOERROR;
	float tmpvx0,tmpvx1,tmpvy0,tmpvy1,delt;
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
			delt = tnew->text.font_height/told->text.font_height;
		
		} else {
		  NhlPError(WARNING,E_UNKNOWN,"LegendSetValues: Can not change x,y,width,and height when other text attribute changes have been requested also, proceding with other text attribute requests");
		  ret = WARNING;
		}
	} 

	if((tnew->text.direction != told->text.direction)||(tnew->text.func_code != told->text.func_code)){
		rstringchange = 1;
		if((tnew->text.direction == UP)||
					(tnew->text.direction == DOWN)) {
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
		case HIGH:
			tnew->text.qual = 0;
			break;
		case MEDIUM:
			tnew->text.qual = 1;
			break;
		case LOW:
			tnew->text.qual = 2;
			break;
	}
/*
* 21.0 is the default principle height. Legends will not allow principle 
* height to be more than this. However it may be less if aspect ratio 
*  1/font_aspect <= 1.0  (See Plotchar).
*/
	if( tnew->text.font_aspect <= 0.0 ) {
		tnew->text.font_aspect = 1.3125;
		NhlPError(WARNING,E_UNKNOWN,"LegendSetValues: Aspect ratio cannont be zero or negative");
		ret = WARNING;
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
 * Function:	LegendInitialize
 *
 * Description:	Performs initilization of Legend. This involves copying
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
static NhlErrorTypes    LegendInitialize
#if __STDC__
( LayerClass class, Layer req, Layer new, _NhlArgList args,int num_args)
#else
(class,req,new,args,num_args)
	LayerClass	class;
	Layer		req;
	Layer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LegendLayer	tnew = (LegendLayer) new;
	char* tmp;
	NhlErrorTypes	ret=NOERROR,ret1 = NOERROR;
	int ll;
	float fr,fl,ft,fb,ur,ul,ut,ub;
	char buf[10];


	tnew->text.x_corners = (float*)NhlMalloc((unsigned)4*sizeof(float));
	tnew->text.y_corners = (float*)NhlMalloc((unsigned)4*sizeof(float));
	if((tnew->text.direction == UP)||(tnew->text.direction == DOWN)) {
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
		case HIGH:
			tnew->text.qual = 0;
			break;
		case MEDIUM:
			tnew->text.qual = 1;
			break;
		case LOW:
			tnew->text.qual = 2;
			break;
	}
/*
* 21.0 is the default principle height. Legends will not allow principle 
* height to be more than this. However it may be less if aspect ratio 
*  1/font_aspect <= 1.0  (See Plotchar).
*/
	if( tnew->text.font_aspect <= 0.0 ) {
		tnew->text.font_aspect = 1.3125;
		NhlPError(WARNING,E_UNKNOWN,"LegendSetValues: Aspect ratio cannont be zero or negative");
		ret = WARNING;
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
 * Function:	LegendDraw
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
static NhlErrorTypes    LegendDraw
#if  __STDC__
(Layer layer)
#else
(layer)
	Layer 	layer;
#endif
{
	LegendLayer tlayer = (LegendLayer) layer;
	float fl,fr,fb,ft,ul,ur,ub,ut;
	int ll;
	NhlErrorTypes ret = NOERROR;
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
 * Function:	LegendDestroy
 *
 * Description: Frees both dynamically allocated strings.
 *
 * In Args:	Layer inst	instance of legend
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes    LegendDestroy
#if  __STDC__
(Layer  inst)
#else
(inst)
	Layer	inst;
#endif
{
	LegendLayer tinst = (LegendLayer) inst;
	
	NhlFree(tinst->text.string);
	NhlFree(tinst->text.real_string);
	return(NOERROR);
}


/*
 * Function:	NhlCvtStringToFQuality
 *
 * Description:	converter for font quality resources
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
NhlCvtStringToFQuality
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
	FontQuality tmp;
	NhlErrorTypes ret = NOERROR;
	
	if(nargs != 0) {
		ret = WARNING;
	}
	
	if(strncmp((char*)from->addr,"HIGH",strlen("HIGH"))==0) 
		tmp = HIGH;
	else if(strncmp((char*)from->addr,"MEDIUM",strlen("MEDIUM"))==0)
		tmp = MEDIUM;
	else if(strncmp((char*)from->addr,"LOW",strlen("LOW"))==0)
		tmp = LOW;
	else 
		NhlPError(WARNING,E_UNKNOWN,"NhlCvtStringToFQuality: Could not convert %s to either HIGH, MEDIUM or LOW, incorrect string",(char*)from->addr);
	
	if((to->size >0) && (to->addr != NULL)) {
		/* caller provided space */
		if(to->size < sizeof(FontQuality)) {
			to->size = (unsigned int)sizeof(FontQuality);
			return(FATAL);
		}
		to->size = (unsigned int)sizeof(FontQuality);
		*((FontQuality*)(to->addr)) = tmp;
		return(ret);
	} else {
		static FontQuality val;
		to->size = (unsigned int)sizeof(FontQuality);
		val = tmp;
		to->addr = &val;
		return(ret);
	}
}
/*
 * Function:	NhlCvtStringToTextDirection
 *
 * Description:	converter for TextDirectionresources
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
NhlCvtStringToTextDirection
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
	TextDirection tmp;
	NhlErrorTypes ret = NOERROR;
	
	if(nargs != 0) {
		ret = WARNING;
	}
	
	if(strncmp((char*)from->addr,"DOWN",strlen("DOWN"))==0) 
		tmp = DOWN;
	else if(strncmp((char*)from->addr,"UP",strlen("UP"))==0)
		tmp = UP;
	else if(strncmp((char*)from->addr,"ACROSS",strlen("ACROSS"))==0)
		tmp = ACROSS;
	else 
		NhlPError(WARNING,E_UNKNOWN,"NhlCvtStringToTextDirection: Could not convert %s to either UP, DOWN or ACROSS, incorrect string",(char*)from->addr);
	
	if((to->size >0) && (to->addr != NULL)) {
		/* caller provided space */
		if(to->size < sizeof(TextDirection)) {
			to->size = (unsigned int)sizeof(TextDirection);
			return(FATAL);
		}
		to->size = (unsigned int)sizeof(TextDirection);
		*((TextDirection*)(to->addr)) = tmp;
		return(ret);
	} else {
		static TextDirection val;
		to->size = (unsigned int)sizeof(TextDirection);
		val = tmp;
		to->addr = &val;
		return(ret);
	}
}
/*
 * Function:	LegendClassInitialize
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
static NhlErrorTypes    LegendClassInitialize
#if  __STDC__
(void)
#else
()
#endif
{
	(void)NrmStringToQuark(NhlTFQuality);	
	(void)NrmStringToQuark(NhlTTextDirection);
	NhlRegisterConverter(NhlTString,NhlTFQuality,
		NhlCvtStringToFQuality,NULL,0,False,NULL);
	NhlRegisterConverter(NhlTString,NhlTTextDirection,
		NhlCvtStringToTextDirection,NULL,0,False,NULL);
	return(NOERROR);	
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
 * In Args: 	tnew: is the new instance record for the legend
 *
 * Out Args:	tnew:
 *
 * Return Values: NONE
 *
 * Side Effects: Lots of fields in tnew are set, Also performs set.
 */
static NhlErrorTypes FigureAndSetTextBBInfo
#if  __STDC__
(LegendLayer tnew)
#else
(tnew)
	LegendLayer tnew;
#endif
{
	float tmpdr,tmpdl,tmpdb,tmpdt;
	float xpoints[4],ypoints[4];
	float tmat[3][3];
	float minx,miny,maxx,maxy;
	NhlErrorTypes ret = NOERROR;
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
			tnew->text.cntr = -1.0;
			tnew->text.real_x_pos = tnew->text.pos_x;
			tnew->text.real_y_pos = tnew->text.pos_y;
			c_plchhq(tnew->text.real_x_pos,tnew->text.real_y_pos,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
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
			tnew->text.real_x_pos = tnew->text.pos_x;
			tnew->text.real_y_pos = tnew->text.pos_y;
			tnew->text.cntr = 1.0;
			c_plchhq(tnew->text.real_x_pos,tnew->text.real_y_pos,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
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
			tnew->text.real_x_pos = tnew->text.pos_x;
			tnew->text.real_y_pos = tnew->text.pos_y - tmpdt;
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
			tnew->text.cntr = 0.0;
			c_plchhq(.5,.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			tnew->text.real_x_pos = tnew->text.pos_x;
			tnew->text.real_y_pos = tnew->text.pos_y - tmpdt;
			break;
		case 6:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			tnew->text.cntr = 1.0;
			c_plchhq(tnew->text.real_x_pos,tnew->text.real_y_pos,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			tnew->text.real_x_pos = tnew->text.pos_x;
			tnew->text.real_y_pos = tnew->text.pos_y - tmpdt;
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
			tnew->text.cntr = -1.0;
			c_plchhq(.5,.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			tnew->text.real_x_pos = tnew->text.pos_x;
			tnew->text.real_y_pos = tnew->text.pos_y + tmpdb;
			break;
		case 5:
/*
* Just used to determin size so the x and y position parameters can be anything
* however it is important that the cnt option be correct for the computations
* following to be correct 
*
*/
			tnew->text.cntr = 0.0;
			c_plchhq(.5,.5,
				tnew->text.real_string,tnew->text.real_size,
				360.0,tnew->text.cntr);
			c_pcgetr("DL",&tmpdl);
			c_pcgetr("DR",&tmpdr);
			c_pcgetr("DT",&tmpdt);
			c_pcgetr("DB",&tmpdb);
			tnew->text.real_x_pos = tnew->text.pos_x;
			tnew->text.real_y_pos = tnew->text.pos_y + tmpdb;
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
			tnew->text.real_x_pos = tnew->text.pos_x;
			tnew->text.real_y_pos = tnew->text.pos_y + tmpdb;
			break;
		default:
			NhlPError(WARNING,E_UNKNOWN,"LegendInitialize: Incorect justification point, using default");
			ret = WARNING;
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
	_NhlInternalSetView((ViewLayer)tnew,minx,maxy,maxx - minx,maxy - miny,1);
/*
* DONE RECONFIGURING VIEW
*/
	return(ret);
}

