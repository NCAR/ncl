/*
 *      $Id: Draw.c,v 1.8 1994-12-16 20:04:11 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Draw.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:33:52 MDT 1992
 *
 *	Description:	This file contains all the functions neccessary for
 *			a given layer instance to draw it's information.
 *			Design documentation is
 *			NhlDOCREF(/design/hlu/Draw.html,here).
 */
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransformP.h>

/*
 * Function:	CallPreDraw
 *
 * Description:	This function is used to call the pre-draw method, it is
 *		a super-to-sub classed chained method that actually
 *		makes the ncarg graphics calls.  This function is
 *		the one that actually traverses the class hierachy
 *		of the object.
 *
 * In Args:	
 *		NhlLayer	l,	object to draw
 *		NhlLayerClass	class	class or super-class of object
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallPreDraw
#if	NhlNeedProto
(
	NhlLayer	l,	/* object to draw	*/
	NhlLayerClass	class	/* class or super-class of object	*/
)
#else
(l,class)
	NhlLayer	l;	/* object to draw	*/
	NhlLayerClass	class;	/* class or super-class of object	*/
#endif
{
	NhlErrorTypes superclassret = NhlNOERROR, localret = NhlNOERROR;

	if(class->base_class.superclass != NULL ) {
		superclassret = CallPreDraw(l,class->base_class.superclass);
		if(superclassret < NhlWARNING)
			return superclassret;
	}

	if(class->base_class.layer_pre_draw != NULL) {
		localret = (*(class->base_class.layer_pre_draw))(l);
	}

	return(MIN(superclassret,localret));
}


/*
 * Function:	CallDraw
 *
 * Description:	This function is used to call the draw method, it is
 *		a super-to-sub classed chained method that actually
 *		makes the ncarg graphics calls.  This function is
 *		the one that actually traverses the class hierachy
 *		of the object.
 *
 * In Args:	
 *		NhlLayer	l,	object to draw
 *		NhlLayerClass	class	class or super-class of object
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallDraw
#if	NhlNeedProto
(
	NhlLayer	l,	/* object to draw	*/
	NhlLayerClass	class	/* class or super-class of object	*/
)
#else
(l,class)
	NhlLayer	l;	/* object to draw	*/
	NhlLayerClass	class;	/* class or super-class of object	*/
#endif
{
	NhlErrorTypes superclassret = NhlNOERROR, localret = NhlNOERROR;

	if(class->base_class.superclass != NULL ) {
		superclassret = CallDraw(l,class->base_class.superclass);
		if(superclassret < NhlWARNING)
			return superclassret;
	}

	if(class->base_class.layer_draw != NULL) {
		localret = (*(class->base_class.layer_draw))(l);
	}

	return(MIN(superclassret,localret));
}

/*
 * Function:	CallPostDraw
 *
 * Description:	This function is used to call the post-draw method, it is
 *		a super-to-sub classed chained method that actually
 *		makes the ncarg graphics calls.  This function is
 *		the one that actually traverses the class hierachy
 *		of the object.
 *
 * In Args:	
 *		NhlLayer	l,	object to draw
 *		NhlLayerClass	class	class or super-class of object
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallPostDraw
#if	NhlNeedProto
(
	NhlLayer	l,	/* object to draw	*/
	NhlLayerClass	class	/* class or super-class of object	*/
)
#else
(l,class)
	NhlLayer	l;	/* object to draw	*/
	NhlLayerClass	class;	/* class or super-class of object	*/
#endif
{
	NhlErrorTypes superclassret = NhlNOERROR, localret = NhlNOERROR;

	if(class->base_class.superclass != NULL ) {
		superclassret = CallPostDraw(l,class->base_class.superclass);
		if(superclassret < NhlWARNING)
			return superclassret;
	}

	if(class->base_class.layer_post_draw != NULL) {
		localret = (*(class->base_class.layer_post_draw))(l);
	}

	return(MIN(superclassret,localret));
}

/*
 * Function:	NhlDraw
 *
 * Description:	This function first checks the overlay status of a plot.
 *		If it is the base of an overlay, it substitutes the 
 *		overlay layer for the plot layer.
 *		If it is part of an overlay other than the overlay base, 
 *		it returns an error.
 *		Then it calls the pre-draw, draw, and post-draw 
 *		methods for the plot.
 *
 * In Args:	
 *		int	id	id of object to draw
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(NhlDraw)
NhlErrorTypes
NhlDraw
#if	NhlNeedProto
(
	int	id	/* id of object to draw	*/
)
#else
(id)
	int id;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlDraw";
	NhlLayer		layer = _NhlGetLayer(id);
	NhlTransformLayerPart	*tfp;
	
	if((layer == NULL) || !_NhlIsBase(layer)){
		e_text = "%s: Invalid plot ID: %d";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,id);
		return(NhlFATAL);
	}

	if (_NhlIsTransform(layer)) {

		tfp = & ((NhlTransformLayer) layer)->trans;

		if (tfp->overlay_status == _tfCurrentOverlayMember) {

			e_text = 
		  "%s: cannot draw overlay member plot, ID %d, independently";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,id);
			return(NhlWARNING);
		}
		else if (tfp->overlay_status == _tfCurrentOverlayBase) {

			layer = tfp->overlay_object;
			if (layer == NULL || ! _NhlIsTransform(layer)) {
				e_text = 
				 "%s: invalid overlay object for plot, ID %d";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,id);
				return(NhlFATAL);
			}
		}
	}

	subret = CallPreDraw(layer,layer->base.layer_class);

	if ((ret = MIN(subret, ret)) < NhlWARNING) {
		e_text = "%s: PreDraw error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	subret = CallDraw(layer,layer->base.layer_class);

	if ((ret = MIN(subret, ret)) < NhlWARNING) {
		e_text = "%s: Draw error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	subret = CallPostDraw(layer,layer->base.layer_class);

	if ((ret = MIN(subret, ret)) < NhlWARNING) {
		e_text = "%s: PostDraw error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
	}

	return ret;

}

/*
 * Function:	nhl_fdraw
 *
 * Description:	Fortran binding function.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	NhlErrorTypes is returned in err_ret
 * Side Effect:	
 */
void
_NHLCALLF(nhl_fdraw,NHL_FDRAW)
#if	NhlNeedProto
(
	int	*id_obj,
	int	*err_ret
)
#else
(id_obj,err_ret)
	int	*id_obj;
	int	*err_ret;
#endif
{
	*err_ret = NhlDraw(*id_obj);

	return;
}

/*
 * Function:	_NhlPreDraw
 *
 * Description:	This global private function calls only the pre-draw method
 *		of an object. 
 *
 * In Args:	
 *		NhlLayer	layer 	layer pointer of the object to pre-draw
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlPreDraw
#if	NhlNeedProto
(
	NhlLayer	layer	/* layer of object to draw	*/
)
#else
(layer)
	NhlLayer	layer;
#endif
{

	if((layer == NULL) || !_NhlIsBase(layer)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "Invalid layer passed to _NhlPreDraw");
		return(NhlFATAL);
	}

	return CallPreDraw(layer,layer->base.layer_class);
}

/*
 * Function:	_NhlDraw
 *
 * Description:	This global private function calls only the actual draw method
 *		of an object - not the pre or post draw methods.
 *
 * In Args:	
 *		NhlLayer	layer 	layer pointer of the object to draw
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlDraw
#if	NhlNeedProto
(
	NhlLayer	layer	/* layer of object to draw	*/
)
#else
(layer)
	NhlLayer	layer;
#endif
{

	if((layer == NULL) || !_NhlIsBase(layer)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "Invalid layer passed to _NhlDraw");
		return(NhlFATAL);
	}

	return CallDraw(layer,layer->base.layer_class);
}


/*
 * Function:	_NhlPostDraw
 *
 * Description:	This global private function calls only the post-draw method
 *		of an object. 
 *
 * In Args:	
 *		NhlLayer	layer 	layer pointer of the object to post-draw
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlPostDraw
#if	NhlNeedProto
(
	NhlLayer	layer	/* layer of object to draw	*/
)
#else
(layer)
	NhlLayer	layer;
#endif
{

	if((layer == NULL) || !_NhlIsBase(layer)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Invalid layer passed to _NhlPostDraw");
		return(NhlFATAL);
	}

	return CallPostDraw(layer,layer->base.layer_class);
}


/*
 * Function:	CallSegDraw
 *
 * Description:	This function is used to call the draw_segonly method, it is
 *		a super-to-sub classed chained method that actually
 *		makes the ncarg graphics calls.  This function is
 *		the one that actually traverses the class hierachy
 *		of the object.
 *
 * In Args:	
 *		NhlLayer	l,	object to draw
 *		NhlLayerClass	class	class or super-class of object
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallSegDraw
#if	NhlNeedProto
(
	NhlLayer	l,	/* object to draw	*/
	NhlLayerClass	class	/* class or super-class of object	*/
)
#else
(l,class)
	NhlLayer	l;	/* object to draw	*/
	NhlLayerClass	class;	/* class or super-class of object	*/
#endif
{
	NhlErrorTypes superclassret = NhlNOERROR, localret = NhlNOERROR;

	if(class->base_class.superclass != NULL ) {
		superclassret = CallSegDraw(l,class->base_class.superclass);
		if(superclassret < NhlWARNING)
			return superclassret;
	}

	if(class->base_class.layer_draw_segonly != NULL) {
		localret = (*(class->base_class.layer_draw_segonly))(l);
	}

	return(MIN(superclassret,localret));
}


/*
 * Function:	_NhlSegDraw
 *
 * Description:	This global private function calls the Segment Drawing
 *		method of an object -- really the method draws into an
 *		open segment started by a parent object.
 *
 * In Args:	
 *		NhlLayer	layer 	layer pointer of the object to draw
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlSegDraw
#if	NhlNeedProto
(
	NhlLayer	layer	/* layer of object to draw	*/
)
#else
(layer)
	NhlLayer	layer;
#endif
{

	if((layer == NULL) || !_NhlIsBase(layer)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "Invalid layer passed to _NhlSegDraw");
		return(NhlFATAL);
	}

	return CallSegDraw(layer,layer->base.layer_class);
}
