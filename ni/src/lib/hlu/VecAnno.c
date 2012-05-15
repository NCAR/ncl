/*
 *      $Id: VecAnno.c,v 1.14 2005-08-16 23:15:50 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VecAnno.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct  9 19:11:27 MDT 1995
 *
 *	Description:	This object is an annotation for the VectorPlot
 */

#include <ncarg/hlu/VecAnnoP.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <math.h>

/* Resources */
#define Oset(field)	NhlOffset(NhlVecAnnoLayerRec,vecanno.field)
static NhlResource resources[] = {
	{NhlNvaString1On, NhlCvaString1On, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(string1_on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNvaString1, NhlCvaString1, NhlTPointer,
		sizeof(char**),Oset(string1),NhlTImmediate,_NhlUSET(NULL),0,
		(NhlFreeFunc)NhlFree},
	{NhlNvaString2On, NhlCvaString2On, NhlTBoolean,
		 sizeof(NhlBoolean),Oset(string2_on),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNvaString2, NhlCvaString2, NhlTPointer,
		sizeof(char**),Oset(string2),NhlTImmediate,_NhlUSET(NULL),0,
		(NhlFreeFunc)NhlFree},
	{NhlNvaVectorLenF, NhlCvaVectorLenF, NhlTFloat,sizeof(float),
		 Oset(vec_len),NhlTString,_NhlUSET("0.01"),0,NULL},
	{NhlNvaVectorLineColor, NhlCLineColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(vec_line_color),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{NhlNvaVectorFillColor, NhlCFillColor, NhlTColorIndex,
		 sizeof(NhlColorIndex),Oset(vec_fill_color),
		 NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{ NhlNvaArrowLineThicknessF,NhlCLineThicknessF,
		  NhlTFloat,sizeof(float),Oset(ah_line_thickness),NhlTString,
		  _NhlUSET("1.0"),0,NULL},
	{ NhlNvaArrowAngleF,NhlCvaArrowAngleF,
		  NhlTFloat,sizeof(float),Oset(ah_angle),NhlTString,
		  _NhlUSET("0.0"),0,NULL},
	{ NhlNvaArrowSpaceF,NhlCvaArrowSpaceF,
		  NhlTFloat,sizeof(float),Oset(ah_space),NhlTString,
		  _NhlUSET("2.0"),0,NULL},
	{ NhlNvaArrowMinOffsetF,NhlCvaArrowMinOffsetF,
		  NhlTFloat,sizeof(float),Oset(ah_min_offset),NhlTString,
		  _NhlUSET("0.25"),0,NULL},
	{ NhlNvaArrowParams,NhlCvaArrowParams,NhlTPointer,sizeof(NhlPointer),
		  Oset(a_params),NhlTImmediate,
		  _NhlUSET((NhlPointer)0),0,NULL},
	{ NhlNvaDrawParams,NhlCvaDrawParams,NhlTPointer,sizeof(NhlPointer),
		  Oset(d_params),NhlTImmediate,
		  _NhlUSET((NhlPointer)0),0,NULL},

	/*
	 * These resources are actually resources in the TextItem object
	 * that is used by this object.  If there are changes in the
	 * resources of TextItem they may need to be updated as well.
	 */
	{NhlNtxAngleF, NhlCTextAngleF, NhlTFloat,
		sizeof(float),Oset(angle),NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNtxFont, NhlCFont, NhlTInteger,
		sizeof(int),Oset(font),NhlTImmediate,
		 _NhlUSET((NhlPointer)21),0,NULL},
	{NhlNtxJust, NhlCTextJustification, NhlTInteger,
		sizeof(int),Oset(just),NhlTImmediate,
		 _NhlUSET((NhlPointer)4),0,NULL},
	{NhlNtxFontQuality, NhlCFontQuality, NhlTFontQuality,
		sizeof(NhlFontQuality),Oset(font_quality),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlHIGH),0,NULL},
	{NhlNtxFontColor, NhlCFontColor, NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(font_color),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{NhlNtxFontHeightF, NhlCFontHeightF, NhlTFloat,
		sizeof(float),Oset(font_height),NhlTString,
		 _NhlUSET("0.5"),0,NULL},
	{NhlNtxFontAspectF, NhlCFontAspectF, NhlTFloat,
		sizeof(float),Oset(font_aspect),NhlTString,
		 _NhlUSET("1.3125"),0,NULL},
	{NhlNtxFontThicknessF, NhlCFontThicknessF, NhlTFloat,
		sizeof(float),Oset(font_thickness),NhlTString,
		 _NhlUSET("1.0"),0,NULL},
	{NhlNtxFuncCode, NhlCTextFuncCode, NhlTCharacter,
		sizeof(char),Oset(func_code[0]),NhlTString,
		 _NhlUSET("~"),0,NULL},
	{NhlNtxConstantSpacingF, NhlCTextConstantSpacingF, NhlTFloat,
		sizeof(float),Oset(constant_spacing),NhlTString,
		 _NhlUSET("0.0"),0,NULL},
	{NhlNtxDirection, NhlCTextDirection, NhlTTextDirection,
		sizeof(NhlTextDirection),Oset(direction),NhlTImmediate,
		 _NhlUSET((NhlPointer)NhlACROSS),0,NULL},

	{NhlNvaPerimOn, NhlCvaPerimOn, NhlTBoolean,sizeof(NhlBoolean),
		 Oset(perim_on),
		 NhlTImmediate,_NhlUSET((NhlPointer) False),0,NULL},
	{NhlNvaPerimColor,NhlCLineColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),
		 Oset(perim_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlFOREGROUND),0,NULL},
	{NhlNvaPerimThicknessF, NhlCLineThicknessF, NhlTFloat, 
		 sizeof(float), 
		 Oset(perim_thickness),
		 NhlTString,_NhlUSET("1.0"),0,NULL},
	{NhlNvaPerimSpaceF, NhlCvaPerimSpaceF, NhlTFloat, 
		 sizeof(float), 
		 Oset(perim_space),
		 NhlTString,_NhlUSET("0.5"),0,NULL},
	{NhlNvaBackgroundFillColor, NhlCFillBackgroundColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),
		 Oset(bg_fill_color),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlTRANSPARENT),0,NULL},
};
#undef Oset

/* Methode declarations	*/

static NhlErrorTypes VecAnnoClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes VecAnnoInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes VecAnnoSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes VecAnnoDraw(
#if	NhlNeedProto
	NhlLayer	l	/* layer to draw	*/
#endif
);


static NhlErrorTypes VecAnnoDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

/* Class definition	*/

NhlVecAnnoClassRec NhlvecAnnoClassRec = {
	{
/* class_name			*/	"vecAnnoClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlVecAnnoLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlviewClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	VecAnnoClassPartInitialize,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	VecAnnoInitialize,
/* layer_set_values		*/	VecAnnoSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	VecAnnoDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	VecAnnoDraw,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	{
/* segment_workstation		*/	-1,	/* ????	*/
/* get_bb			*/	NULL	/* ????	*/
	},
	{
/* foo				*/	0
	}
};

NhlClass NhlvecAnnoClass = (NhlClass)&NhlvecAnnoClassRec;

/************************************************************************
*									*
*	VecAnno Class Method definitions				*
*									*
************************************************************************/

/*
 * Function:	VecAnnoClassPartInitialize
 *
 * Description:	This function initializes the VecAnno NhlClass record.
 *
 * In Args:
 *		NhlClass	lc	NhlClass being inited
 *
 * Out Args:	none
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
VecAnnoClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* NhlClass being inited	*/
)
#else
(lc)
	NhlClass	lc;	/* NhlClass being inited	*/
#endif
{
	return NhlNOERROR;
}

/*
 * Function:	CalculateGeometry
 *
 * Description:	This function calculates the size of the VecAnno layer
 *		passed to it.
 *
 * In Args:	
 *		NhlVecAnnoLayer	l,		The NhlLayer
 *
 * Out Args:	
 *		float		*x,		x return
 *		float		*y,		y return
 *		float		*width,		width return
 *		float		*height		height return
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	The VecAnno layer's TextItem has changed
 */
static void
CalculateGeometry
#if	NhlNeedProto
(
	NhlVecAnnoLayer	l,		/* The NhlLayer		*/
	NhlVecAnnoLayer ol,		/* the old layer */
	float			*x,		/* x return		*/
	float			*y,		/* y return		*/
	float			*width,		/* width return		*/
	float			*height		/* height return	*/
)
#else
(l,ol,x,y,width,height)
	NhlVecAnnoLayer	l;		/* The NhlLayer		*/
	NhlVecAnnoLayer ol;		/* the old layer */
	float			*x;		/* x return		*/
	float			*y;		/* y return		*/
	float			*width;		/* width return		*/
	float			*height;	/* height return	*/
#endif
{
	NhlVecAnnoLayerPart *vap = (NhlVecAnnoLayerPart *) &l->vecanno;
	NhlVecAnnoLayerPart *ovap;
	float		twidth;
	float		theight;
	float		space;
	float		s1width,s1height;
	float		s2width,s2height;
	float		max_tx_height;
	float		margin;
	float		tx,ty,txe,tye;
        float		ajx, ajy;
	float		xmn,xmx,ymn,ymx;

#define DEGTORAD 0.017453292519943

	if (ol != NULL)
		ovap = (NhlVecAnnoLayerPart *) &ol->vecanno;

	tx = l->view.x;
	ty = l->view.y;
		
	s1width = 0.0;
	s1height = 0.0;
	if (vap->string1_on) {

		NhlVAGetValues(vap->textitem1,
			       NhlNvpWidthF,	&s1width,
			       NhlNvpHeightF,	&s1height,
			       NULL);
	}
	s2width = 0.0;
	s2height = 0.0;
	if (vap->string2_on) {
		NhlVAGetValues(vap->textitem2,
			       NhlNvpWidthF,	&s2width,
			       NhlNvpHeightF,	&s2height,
			       NULL);
	}

	txe = tx + vap->vec_len * cos(DEGTORAD * vap->ah_angle);
	tye = ty + vap->vec_len * sin(DEGTORAD * vap->ah_angle); 

	if (vap->vec_len > 0.0) {
		NGCALLF(vvgetarrowbound,VVGETARROWBOUND)
			(vap->a_params,&tx,&ty,&txe,&tye,&vap->vec_len,
			 &xmn,&ymn,&xmx,&ymx);
		vap->vec.width = xmx - xmn;
		vap->vec.height = ymx - ymn;
	}
	else {
                xmn = xmx = ymn = ymx = 0.0;
		vap->vec.width = 0;
		vap->vec.height = 0;
	}

	max_tx_height = MAX(s1height,s2height);
	margin = vap->font_height * vap->perim_space;

	space = MAX(vap->ah_min_offset * 2.0 * max_tx_height, 
		    vap->ah_space * max_tx_height - vap->vec.height);
	twidth = MAX(vap->vec.width,MAX(s1width,s2width));
	theight = s1height + vap->vec.height + s2height + space;

	vap->ti1.y = ty - margin;
	vap->vec.y = ty - margin - s1height - space / 2.0 ;
	vap->ti2.y = ty - margin - s1height - vap->vec.height - space;

	vap->ti1.x = tx + margin + (twidth - s1width) / 2.0;
	vap->vec.x = tx + margin + (twidth - vap->vec.width) / 2.0;
	vap->ti2.x = tx + margin + (twidth - s2width) / 2.0;

	ajx = vap->vec.x - xmn;
	ajy = vap->vec.y - ymx;

	vap->vxb = tx + ajx;
	vap->vxe = txe + ajx;
	vap->vyb = ty + ajy;
	vap->vye = tye + ajy;

	if (vap->string1_on) {
		vap->ti1.width = s1width;
		vap->ti1.height = s1height;
		if (ol == NULL ||
		    (vap->ti1.x != ovap->ti1.x) ||
		    (vap->ti1.y != ovap->ti1.y) ||
		    (vap->ti1.width != ovap->ti1.width) ||
		    (vap->ti1.height != ovap->ti1.height)) {
			NhlVASetValues(vap->textitem1,
				       NhlNvpXF,	vap->ti1.x,
				       NhlNvpYF,	vap->ti1.y,
				       NhlNvpWidthF,	vap->ti1.width,
				       NhlNvpHeightF,	vap->ti1.height,
				       NULL);
		}
	}
	if (vap->string2_on) {
		vap->ti2.width = s2width;
		vap->ti2.height = s2height;
		if (ol == NULL ||
		    (vap->ti2.x != ovap->ti2.x) ||
		    (vap->ti2.y != ovap->ti2.y) ||
		    (vap->ti2.width != ovap->ti2.width) ||
		    (vap->ti2.height != ovap->ti2.height)) {
			NhlVASetValues(vap->textitem2,
				       NhlNvpXF,	vap->ti2.x,
				       NhlNvpYF,	vap->ti2.y,
				       NhlNvpWidthF,	vap->ti2.width,
				       NhlNvpHeightF,	vap->ti2.height,
				       NULL);
		}
	}

	*x = tx;
	*y = ty;
	*width = twidth + 2.0 * margin;
	*height = theight + 2.0 * margin;

	return;
}

/*
 * Function:	VecAnnoInitialize
 *
 * Description:	This function initializes an instance of a VecAnno layer.
 *
 * In Args:	
 *		NhlClass	lc,	class
 *		NhlLayer		req,	requested
 *		_NhlArgList	args,	args
 *		int		nargs	nargs
 *
 * Out Args:	
 *		NhlLayer		new,	new
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
VecAnnoInitialize
#if	NhlNeedProto
(
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlClass	lc;	/* class	*/
	NhlLayer	req;	/* requested	*/
	NhlLayer	new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	NhlVecAnnoLayer	vanew = (NhlVecAnnoLayer)new;
	NhlVecAnnoLayer	vareq = (NhlVecAnnoLayer)req;
	NhlVecAnnoLayerPart *vap = (NhlVecAnnoLayerPart *) &vanew->vecanno;
	NhlVecAnnoLayerPart *rvap = (NhlVecAnnoLayerPart *) &vareq->vecanno;

	char		name[_NhlMAXRESNAMLEN];
	float		x,y,width,height;
	NhlErrorTypes	ret = NhlNOERROR;
	NhlString	e_text;
	NhlString	entry_name = "VecAnnoInitialize";

	vap->func_code[1] = '\0';

	if (rvap->string1 == NULL) {
		e_text = "%s: vaString1 must not be NULL";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	vap->string1 = (NhlString) NhlMalloc(strlen(rvap->string1) + 1);
	if (vap->string1 == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	strcpy(vap->string1,rvap->string1);
	if (rvap->string2 == NULL) {
		e_text = "%s: vaString2 must not be NULL";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	vap->string2 = (NhlString) NhlMalloc(strlen(rvap->string2) + 1);
	if (vap->string2 == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	strcpy(vap->string2,rvap->string2);

	if (rvap->a_params == NULL) {
		e_text = "%s: vaArrowParams must not be NULL";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	vap->a_params = NhlMalloc(sizeof(_NhlvaArrowParams));
	if (vap->a_params == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	memcpy(vap->a_params,rvap->a_params,sizeof(_NhlvaArrowParams));

	/* give child a name */
	strcpy(name,new->base.name);
	strcat(name,"-String1");

	ret = NhlVACreate(&vap->textitem1,name,NhltextItemClass,new->base.id,
			   NhlNtxString,vap->string1,
			   NhlNtxAngleF,vap->angle,
			   NhlNtxFont,vap->font,
			   NhlNtxJust,vap->just,
			   NhlNtxFontQuality,vap->font_quality,
			   NhlNtxFontHeightF,vap->font_height,
			   NhlNtxFontAspectF,vap->font_aspect,
			   NhlNtxFontThicknessF,vap->font_thickness,
			   NhlNtxFuncCode, vap->func_code[0],
			   NhlNtxConstantSpacingF,vap->constant_spacing,
			   NhlNtxDirection,vap->direction,
			   NhlNtxFontColor,vap->font_color,
			   NULL);

	/* give child a name */
	strcpy(name,new->base.name);
	strcat(name,"-String2");
	ret = NhlVACreate(&vap->textitem2,name,NhltextItemClass,new->base.id,
			   NhlNtxString,vap->string2,
			   NhlNtxAngleF,vap->angle,
			   NhlNtxFont,vap->font,
			   NhlNtxJust,vap->just,
			   NhlNtxFontQuality,vap->font_quality,
			   NhlNtxFontHeightF,vap->font_height,
			   NhlNtxFontAspectF,vap->font_aspect,
			   NhlNtxFontThicknessF,vap->font_thickness,
			   NhlNtxFuncCode, vap->func_code[0],
			   NhlNtxConstantSpacingF,vap->constant_spacing,
			   NhlNtxDirection,vap->direction,
			   NhlNtxFontColor,vap->font_color,
			   NULL);

	/*
	 * Determine size based on font characterestics and text_object
	 */
	CalculateGeometry(vanew,NULL,&x,&y,&width,&height);

	/*
	 * Set the size
	 */
	_NhlInternalSetView((NhlViewLayer)new,x,y,width,height,False);

	return ret;
}

/*
 * Function:	VecAnnoSetValues
 *
 * Description:	This is the SetValues method for the VecAnno object.
 *
 * In Args:	
 *		NhlLayer		old,		old
 *		NhlLayer		req,		requested
 *		_NhlArgList	args,		args to set
 *		int		nargs		nargs
 *
 * Out Args:	
 *		NhlLayer		new,		new
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
VecAnnoSetValues
#if	NhlNeedProto
(
	NhlLayer		old,		/* old		*/
	NhlLayer		req,		/* requested	*/
	NhlLayer		new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
)
#else
(old,req,new,args,nargs)
	NhlLayer		old;		/* old		*/
	NhlLayer		req;		/* requested	*/
	NhlLayer		new;		/* new		*/
	_NhlArgList	args;		/* args to set	*/
	int		nargs;		/* nargs	*/
#endif
{
	NhlBoolean	changed = False, text_changed = False;
	NhlVecAnnoLayer	vaold = (NhlVecAnnoLayer)old;
	NhlVecAnnoLayer	vanew = (NhlVecAnnoLayer)new;
	NhlErrorTypes	ret = NhlNOERROR;
	NhlErrorTypes	lret = NhlNOERROR;
	NhlVecAnnoLayerPart *vap = (NhlVecAnnoLayerPart *) &vanew->vecanno;
	NhlVecAnnoLayerPart *ovap = (NhlVecAnnoLayerPart *) &vaold->vecanno;
	NhlString	e_text;
	NhlString	entry_name = "VecAnnoSetValues";

	/* if the draw params is set nothing else should be */

	if (_NhlArgIsSet(args,nargs,NhlNvaDrawParams)) {
		NhlFree(ovap->d_params);
		ovap->d_params = vap->d_params;
		vap->d_params = NhlMalloc(sizeof(_NhlvaDrawParams));
		if (vap->d_params == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		memcpy(vap->d_params,ovap->d_params,sizeof(_NhlvaDrawParams));
                return NhlNOERROR;
	}

	if (vanew->view.x != vaold->view.x ||
	    vanew->view.y != vaold->view.y ||
	    vanew->view.width != vaold->view.width ||
	    vanew->view.height != vaold->view.height) {
		changed = True;
	}
	if (_NhlArgIsSet(args,nargs,NhlNvaString1)) {
		NhlFree(ovap->string1);
		ovap->string1 = vap->string1;
		vap->string1 = (NhlString) 
			NhlMalloc(strlen(vap->string1) + 1);
		if (vap->string1 == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy(vap->string1,ovap->string1);
		text_changed = True;
	}
	if (_NhlArgIsSet(args,nargs,NhlNvaString2)) {
		NhlFree(ovap->string2);
		ovap->string2 = vap->string2;
		vap->string2 = (NhlString) 
			NhlMalloc(strlen(vap->string2) + 1);
		if (vap->string2 == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		strcpy(vap->string2,ovap->string2);
		text_changed = True;
	}
	if (_NhlArgIsSet(args,nargs,NhlNvaArrowParams)) {
		NhlFree(ovap->a_params);
		ovap->a_params = vap->a_params;
		vap->a_params = NhlMalloc(sizeof(_NhlvaArrowParams));
		if (vap->a_params == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		memcpy(vap->a_params,ovap->a_params,sizeof(_NhlvaArrowParams));
		changed = True;
	}

	if (vap->vec_len != ovap->vec_len)
		changed = True;

	if (text_changed ||
	    (vap->angle != ovap->angle) ||
	    (vap->just != ovap->just) ||
	    (vap->font_quality != ovap->font_quality) ||
	    (vap->font_height != ovap->font_height) ||
	    (vap->font_aspect != ovap->font_aspect) ||
	    (vap->font_thickness != ovap->font_thickness) ||
	    (vap->constant_spacing != ovap->constant_spacing) ||
	    (vap->direction != ovap->direction) ||
	    (vap->font != ovap->font) ) {
		changed = True;

		lret = NhlVASetValues(vap->textitem1,
				      NhlNtxString,vap->string1,
				      NhlNtxAngleF,vap->angle,
				      NhlNtxFont,vap->font,
				      NhlNtxJust,vap->just,
				      NhlNtxFontQuality,vap->font_quality,
				      NhlNtxFontHeightF,vap->font_height,
				      NhlNtxFontAspectF,vap->font_aspect,
				      NhlNtxFontThicknessF,vap->font_thickness,
				      NhlNtxConstantSpacingF,
				      vap->constant_spacing,
				      NhlNtxDirection,vap->direction,
				      NhlNtxFontColor,vap->font_color,
				      NULL);
		if ((ret = MIN(lret,ret)) < NhlWARNING) return ret;
	

		ret = NhlVASetValues(vap->textitem2,
				      NhlNtxString,vap->string2,
				      NhlNtxAngleF,vap->angle,
				      NhlNtxFont,vap->font,
				      NhlNtxJust,vap->just,
				      NhlNtxFontQuality,vap->font_quality,
				      NhlNtxFontHeightF,vap->font_height,
				      NhlNtxFontAspectF,vap->font_aspect,
				      NhlNtxFontThicknessF,vap->font_thickness,
				      NhlNtxConstantSpacingF,
				      vap->constant_spacing,
				      NhlNtxDirection,vap->direction,
				      NhlNtxFontColor,vap->font_color,
				      NULL);
		if ((ret = MIN(lret,ret)) < NhlWARNING) return ret;
	}
		
	/*
	 * Update the geometry of this object
	 */
	if (changed) {
		float x,y,width,height;
		/*
		 * Determine size based on font characterestics and
		 * text_object
		 */
		CalculateGeometry(vanew,vaold,&x,&y,&width,&height);

		/*
		 * Set the size
		 */
		_NhlInternalSetView((NhlViewLayer)new,x,y,width,height,False);
	}
	/*
	 * if just the color has changed then the changed flag was not set and
	 * no setvalues has taken place so we have to do it here
	 */
	else if (vap->font_color != ovap->font_color) {
		lret = NhlVASetValues(vap->textitem1,
				      NhlNtxFontColor,vap->font_color,
				      NULL);
		if ((ret = MIN(lret,ret)) < NhlWARNING) return ret;
	

		ret = NhlVASetValues(vap->textitem2,
				      NhlNtxFontColor,vap->font_color,
				      NULL);
		if ((ret = MIN(lret,ret)) < NhlWARNING) return ret;
	}

	return ret;
}

/*
 * Function:	VecAnnoDraw
 *
 * Description:	This function is called when the VecAnno object should
 *		draw its strings
 *
 * In Args:	
 *		NhlLayer	l	layer to draw
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
VecAnnoDraw
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to draw	*/
)
#else
(l)
	NhlLayer	l;	/* layer to draw	*/
#endif
{
	NhlVecAnnoLayer	val = (NhlVecAnnoLayer)l;
	NhlErrorTypes	ret = NhlNOERROR;
	NhlString	e_text;
	NhlString	entry_name = "VecAnnoDraw";
	NhlVecAnnoLayerPart *vap = (NhlVecAnnoLayerPart *) &val->vecanno;
	float fl,fr,fb,ft,ul,ur,ub,ut;
	int ll;
	float xb,xe,yb,ye,vln;
	float x[5],y[5];
	int line_color;

	x[0] = val->view.x;
	x[1] = val->view.x + val->view.width;
	y[0] = val->view.y - val->view.height;
	y[1] = y[0];
	x[2] = x[1];
	y[2] = val->view.y;
	x[3] = x[0];
	x[4] = x[0];
	y[3] = y[2];
	y[4] = y[0];
       
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
	_NhlActivateWorkstation(l->base.wkptr);

	c_set(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1);

	NhlVASetValues(val->base.wkptr->base.id,
		_NhlNwkReset,	True,
		NULL);


	/* first draw the perimeter: it may have a solid background */
	if (vap->perim_on || vap->bg_fill_color > NhlTRANSPARENT) {
		NhlVASetValues(val->base.wkptr->base.id,
			       _NhlNwkEdgesOn,vap->perim_on,
			       _NhlNwkEdgeDashPattern,NhlSOLIDLINE,
			       _NhlNwkEdgeThicknessF,vap->perim_thickness,
			       _NhlNwkEdgeColor,vap->perim_color,
			       _NhlNwkFillColor,vap->bg_fill_color,
			       _NhlNwkFillIndex,NhlSOLIDFILL,
			       NULL);
	
		_NhlSetFillInfo(l->base.wkptr,l);
		_NhlWorkstationFill(l->base.wkptr,x,y,5);
	}

	if (vap->vec_len > 0.0) {
		line_color = vap->vec_line_color;
		if (vap->a_params->ast_iast == 1) {
			int acm = 0;
			if (vap->vec_fill_color <= NhlTRANSPARENT) {
				if (line_color <= NhlTRANSPARENT)
					line_color = NhlFOREGROUND;
				acm = -1;
			}
			else if (line_color <= NhlTRANSPARENT) {
				acm = -2;
			}
			c_vvseti("ACM",acm);
		}
		else if (line_color <= NhlTRANSPARENT) {
			line_color = NhlFOREGROUND;
		}

		if (line_color > NhlTRANSPARENT)
			gset_line_colr_ind((Gint)
				      _NhlGetGksCi(l->base.wkptr,line_color));
		if (vap->vec_fill_color > NhlTRANSPARENT) 
			gset_fill_colr_ind((Gint)
			      _NhlGetGksCi(l->base.wkptr,vap->vec_fill_color));
		gset_linewidth(vap->ah_line_thickness);
		
		xb = vap->vxb;
		yb = vap->vyb;
		xe = vap->vxe;
		ye = vap->vye;
		vln = vap->vec_len;

		if (vap->d_params == NULL || vap->a_params == NULL) {
			e_text = 
			     "%s: internal parameter values not initialized";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		_NHLCALLF(vvarrowdraw,VVARROWDRAW)(vap->d_params,vap->a_params,
						   &xb,&yb,&xe,&ye,&vln);
	}

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);

	_NhlDeactivateWorkstation(l->base.wkptr);

	if (vap->string1_on)
		NhlDraw(vap->textitem1);
	if (vap->string2_on)
		NhlDraw(vap->textitem2);

	return(ret);
}


/*
 * Function:	VecAnnoDestroy
 *
 * Description:	This is the destroy method for the vecAnno object.
 *		It is responsible for free'ing any memory used by
 *		the object.
 *
 * In Args:	
 *		NhlLayer	l	layer to destroy
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
VecAnnoDestroy
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlVecAnnoLayer	val = (NhlVecAnnoLayer)l;
	NhlVecAnnoLayerPart *vap = (NhlVecAnnoLayerPart *) &val->vecanno;

	if (vap->d_params != NULL)
		NhlFree(vap->d_params);
	if (vap->a_params != NULL)
		NhlFree(vap->a_params);
	NhlFree(vap->string1);
	NhlFree(vap->string2);
	NhlDestroy(vap->textitem1);
	NhlDestroy(vap->textitem2);

	return NhlNOERROR;
}
