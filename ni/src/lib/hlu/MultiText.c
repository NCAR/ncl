/*
 *      $Id: MultiText.c,v 1.1 1993-04-30 17:23:05 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MultiText.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Dec 3 11:44:16 MST 1992
 *
 *	Description:	MultiText is an object that uses an internal TextObject
 *			child to draw numerous text strings in a column or
 *			a row - for use in Tic-Marks.
 */
#include <strings.h>
#include <ncarg/hlu/MultiTextP.h>
#include <ncarg/hlu/TextItem.h>

/* SUPPRESS 112 */

/* Resources */
#define Oset(field)	NhlOffset(MultiTextLayerRec,multitext.field)
static NhlResource resources[] = {
	{NhlNMtextNumStrings, NhlCMtextNumStrings, NhlTInteger,
		sizeof(int),Oset(num_strings),NhlTImmediate,0},
	{NhlNMtextStrings, NhlCMtextStrings, NhlTStringPtr,
		sizeof(char**),Oset(text_strings),NhlTImmediate,NULL},
	{NhlNMtextOrientation, NhlCMtextOrientation, NhlTMTextOrientationType,
		sizeof(NhlMTextOrientatonType),Oset(orientation),NhlTImmediate,
						(NhlPointer)MTEXT_X_CONST},
	{NhlNMtextConstPosF, NhlCMtextConstPosF, NhlTFloat,
		sizeof(float),Oset(const_pos),NhlTString,"-1.0"},
	{NhlNMtextPosArray, NhlCMtextPosArray, NhlTFloatPtr,
		sizeof(float*),Oset(pos_array),NhlTImmediate,NULL},
	/*
	 * These resources are actually resources in the TextItem object
	 * that is used by this object.  If there are changes in the
	 * resources of TextItem they may need to be updated as well.
	 */
	{NhlNtxAngleF, NhlCtxAngleF, NhlTFloat,
		sizeof(float),Oset(angle),NhlTString,"0.0"},
	{NhlNtxFont, NhlCtxFont, NhlTInteger,
		sizeof(int),Oset(font),NhlTImmediate,(NhlPointer)0},
	{NhlNtxJust, NhlCtxJust, NhlTInteger,
		sizeof(int),Oset(just),NhlTImmediate,(NhlPointer)4},
	{NhlNtxFontQuality, NhlCtxFontQuality, NhlTFQuality,
		sizeof(FontQuality),Oset(font_quality),NhlTImmediate,
						(NhlPointer)HIGH},
	{NhlNtxFontHeightF, NhlCtxFontHeightF, NhlTFloat,
		sizeof(float),Oset(font_height),NhlTString,"0.5"},
	{NhlNtxFontAspectF, NhlCtxFontAspectF, NhlTFloat,
		sizeof(float),Oset(font_aspect),NhlTString,"1.3125"},
	{NhlNtxFontThicknessF, NhlCtxFontThicknessF, NhlTFloat,
		sizeof(float),Oset(font_thickness),NhlTString,"1.0"},
	{NhlNtxConstantSpacingF, NhlCtxConstantSpacingF, NhlTFloat,
		sizeof(float),Oset(constant_spacing),NhlTString,"0.0"},
	{NhlNtxDirection, NhlCtxDirection, NhlTTextDirection,
		sizeof(TextDirection),Oset(direction),NhlTImmediate,
						(NhlPointer)ACROSS}
};
#undef Oset

/* Methode declarations	*/

static NhlErrorTypes MultiTextClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes MultiTextInitialize(
#if	NhlNeedProto
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes MultiTextSetValues(
#if	NhlNeedProto
	Layer		old,		/* old		*/
	Layer		req,		/* requested	*/
	Layer		new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes MultiTextDraw(
#if	NhlNeedProto
	Layer	l	/* layer to draw	*/
#endif
);

static NhlErrorTypes MultiTextDestroy(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

/* Class definition	*/

MultiTextLayerClassRec multiTextLayerClassRec = {
	{
/* superclass			*/	(LayerClass)&viewLayerClassRec,
/* class_name			*/	"MultiText",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(MultiTextLayerRec),
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NULL,
/* class_inited			*/	False,
/* class_initialize		*/	MultiTextClassInitialize,
/* layer_initialize		*/	MultiTextInitialize,
/* layer_set_values		*/	MultiTextSetValues,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	MultiTextDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	MultiTextDestroy
	},
	{
/* segment_workstation		*/	-1,	/* ????	*/
/* get_bb			*/	NULL	/* ????	*/
	},
	{
/* foo				*/	0
	}
};

LayerClass multiTextLayerClass = (LayerClass)&multiTextLayerClassRec;

/************************************************************************
* New type converters - needed for new type's defined for this class	*
* Added to the converter table by MultiTextClassInitialize		*
************************************************************************/

/* none needed	*/


/************************************************************************
*									*
*	MultiText Class Method definitions				*
*									*
************************************************************************/

/*
 * Function:	MutliTextClassInitialize
 *
 * Description:	This function initializes the MultiText LayerClass record.
 *
 * In Args:	none
 *
 * Out Args:	none
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
MultiTextClassInitialize
#if	__STDC__
(
	void
)
#else
()
#endif
{
	/*
	 * Register children classes
	 */
	return _NhlRegisterChildClass(multiTextLayerClass,textItemLayerClass,
			True,False,NhlNtxString,NhlNtxPosXF,NhlNtxPosYF,NULL);
}

/*
 * Function:	CalculateGeometry
 *
 * Description:	This function calculates the size of the Multitext layer
 *		passed to it.
 *
 * In Args:	
 *		MultiTextLayer	l,		The Layer
 *
 * Out Args:	
 *		float		*x,		x return
 *		float		*y,		y return
 *		float		*width,		width return
 *		float		*height		height return
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	The Multitext layer's TextItem has changed
 */
static void
CalculateGeometry
#if	__STDC__
(
	MultiTextLayer	l,		/* The Layer		*/
	float		*x,		/* x return		*/
	float		*y,		/* y return		*/
	float		*width,		/* width return		*/
	float		*height		/* height return	*/
)
#else
(l,x,y,width,height)
	MultiTextLayer	l;		/* The Layer		*/
	float		*x;		/* x return		*/
	float		*y;		/* y return		*/
	float		*width;		/* width return		*/
	float		*height;	/* height return	*/
#endif
{
	int		i;
	NhlBoundingBox	box;
	float		tx;
	float		ty;
	float		twidth;
	float		theight;

	box.set = False;

	/*
	 * Determine MultiText size by determining size of all strings
	 */
	for(i=0;i < l->multitext.num_strings;i++){

		if(l->multitext.orientation == MTEXT_X_CONST){
			NhlSetValues(l->multitext.text_object,
				NhlNtxString,l->multitext.text_strings[i],
				NhlNtxPosXF,l->multitext.const_pos,
				NhlNtxPosYF,l->multitext.pos_array[i],
				NULL);
		}
		else{
			NhlSetValues(l->multitext.text_object,
				NhlNtxString,l->multitext.text_strings[i],
				NhlNtxPosXF,l->multitext.pos_array[i],
				NhlNtxPosYF,l->multitext.const_pos,
				NULL);
		}

		/*
		 * retrieve child's geometry
		 */
		NhlGetValues(l->multitext.text_object,
			NhlNvpXF,	&tx,
			NhlNvpYF,	&ty,
			NhlNvpWidthF,	&twidth,
			NhlNvpHeightF,	&theight,
			NULL);

		/*
		 * Merge child with previous box's
		 */
		_NhlAddBBInfo(ty, ty - theight, tx + twidth, tx, &box);
	}

	*x = box.l;
	*y = box.t;
	*width = box.r - box.l;
	*height = box.t - box.b;

	return;
}

/*
 * Function:	MultiTextInitialize
 *
 * Description:	This function initializes an instance of a MultiText layer.
 *
 * In Args:	
 *		LayerClass	lc,	class
 *		Layer		req,	requested
 *		_NhlArgList	args,	args
 *		int		nargs	nargs
 *
 * Out Args:	
 *		Layer		new,	new
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
MultiTextInitialize
#if	__STDC__
(
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	LayerClass	lc;	/* class	*/
	Layer		req;	/* requested	*/
	Layer		new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	MultiTextLayer	mtnew = (MultiTextLayer)new;
	MultiTextLayer	mtreq = (MultiTextLayer)req;
	int		i;
	int		num_strings = mtnew->multitext.num_strings;
	char		**text_strings;
	char		name[128];
	float		x,y,width,height;
	NhlErrorTypes	ret = NOERROR;

	if(num_strings > 0){

		/* Allocate memory for text_strings */

		mtnew->multitext.text_strings = text_strings =
			(char**)NhlMalloc((unsigned)num_strings*sizeof(char*));

		for(i=0;i<num_strings;i++){
			text_strings[i] =
				(char*)NhlMalloc((unsigned)
				strlen(mtreq->multitext.text_strings[i]) + 1);
			strcpy(text_strings[i],
					mtreq->multitext.text_strings[i]);
		}

		/* Allocat memory for Position Array */

		mtnew->multitext.pos_array =
			(float*)NhlMalloc((unsigned)num_strings*sizeof(float));

		for(i=0;i<num_strings;i++)
			mtnew->multitext.pos_array[i] =
						mtreq->multitext.pos_array[i];
	} else {
		mtnew->multitext.text_strings = NULL;
		mtnew->multitext.pos_array = NULL;
	}

	/* give child a name */
	strcpy(name,new->base.name);
	strcat(name,"-TxtItm");

	ret = _NhlCreateChild(&mtnew->multitext.text_object,name,
						textItemLayerClass,new,
		NhlNtxAngleF,		mtnew->multitext.angle,
		NhlNtxFont,		mtnew->multitext.font,
		NhlNtxJust,		mtnew->multitext.just,
		NhlNtxFontQuality,	mtnew->multitext.font_quality,
		NhlNtxFontHeightF,	mtnew->multitext.font_height,
		NhlNtxFontAspectF,	mtnew->multitext.font_aspect,
		NhlNtxFontThicknessF,	mtnew->multitext.font_thickness,
		NhlNtxConstantSpacingF,	mtnew->multitext.constant_spacing,
		NhlNtxDirection,	mtnew->multitext.direction,
		NULL);

	if(num_strings > 0){
		/*
		 * Determine size based on font characterestics and text_object
		 */
		CalculateGeometry(mtnew,&x,&y,&width,&height);

		/*
		 * Set the size
		 */
		_NhlInternalSetView((ViewLayer)new,x,y,width,height,False);

		/*
		 * save the geometry of the first string
		 */
		if(mtnew->multitext.orientation == MTEXT_X_CONST){
			NhlSetValues(mtnew->multitext.text_object,
				NhlNtxString,mtnew->multitext.text_strings[0],
				NhlNtxPosXF,mtnew->multitext.const_pos,
				NhlNtxPosYF,mtnew->multitext.pos_array[0],
				NULL);
		}
		else{
			NhlSetValues(mtnew->multitext.text_object,
				NhlNtxString,mtnew->multitext.text_strings[0],
				NhlNtxPosXF,mtnew->multitext.pos_array[0],
				NhlNtxPosYF,mtnew->multitext.const_pos,
				NULL);
		}

		NhlGetValues(mtnew->multitext.text_object,
			NhlNvpXF,	&mtnew->multitext.text_x,
			NhlNvpYF,	&mtnew->multitext.text_y,
			NhlNvpWidthF,	&mtnew->multitext.text_width,
			NhlNvpHeightF,	&mtnew->multitext.text_height,
			NULL);
	}

	return ret;
}

/*
 * Function:	MultiTextSetValues
 *
 * Description:	This is the SetValues method for the MultiText object.
 *
 * In Args:	
 *		Layer		old,		old
 *		Layer		req,		requested
 *		_NhlArgList	args,		args to set
 *		int		nargs		nargs
 *
 * Out Args:	
 *		Layer		new,		new
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
MultiTextSetValues
#if	__STDC__
(
	Layer		old,		/* old		*/
	Layer		req,		/* requested	*/
	Layer		new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
)
#else
(old,req,new,args,nargs)
	Layer		old;		/* old		*/
	Layer		req;		/* requested	*/
	Layer		new;		/* new		*/
	_NhlArgList	args;		/* args to set	*/
	int		nargs;		/* nargs	*/
#endif
{
	int		i;
	NhlBoolean	changed = False;
	MultiTextLayer	mtold = (MultiTextLayer)old;
	MultiTextLayer	mtreq = (MultiTextLayer)req;
	MultiTextLayer	mtnew = (MultiTextLayer)new;
	NhlErrorTypes	ret = NOERROR;
	NhlErrorTypes	lret = NOERROR;

	if(mtreq->multitext.num_strings != mtold->multitext.num_strings){
		if((mtreq->multitext.text_strings ==
			mtold->multitext.text_strings) ||
				(mtreq->multitext.pos_array ==
						mtold->multitext.pos_array)){
			NhlPError(FATAL,E_UNKNOWN,
				"Multitext SetValues:Changing number of strings w/o changing strings or positions");
			return FATAL;
		}
	}

	/*
	 * if text_strings has changed
	 */
	if(mtreq->multitext.text_strings != mtold->multitext.text_strings){
		changed = True;

		if(mtreq->multitext.num_strings > 0){

			/* Allocate memory for text_strings */

			mtnew->multitext.text_strings =(char**)NhlMalloc(
					(unsigned)mtreq->multitext.num_strings *
								sizeof(char*));
			if(mtnew->multitext.text_strings == NULL){
				NhlPError(FATAL,12,"Unable to SetValues");
				return FATAL;
			}

			for(i=0;i < mtreq->multitext.num_strings;i++){
				mtnew->multitext.text_strings[i] =
					(char*)NhlMalloc((unsigned)strlen(
					mtreq->multitext.text_strings[i]) + 1);
				if(mtnew->multitext.text_strings[i] == NULL){
					NhlPError(FATAL,12,
							"Unable to SetValues");
					return FATAL;
				}
				strcpy(mtnew->multitext.text_strings[i],
					mtreq->multitext.text_strings[i]);
			}
		} else {
			mtnew->multitext.text_strings = NULL;
			mtnew->multitext.num_strings = 0; 
		}
		/* free old memory */
		for(i=0;i < mtold->multitext.num_strings;i++)
			(void)NhlFree(mtold->multitext.text_strings[i]);
		if(mtold->multitext.text_strings != NULL)
			(void)NhlFree(mtold->multitext.text_strings);
		mtold->multitext.text_strings = NULL;
	}

	/*
	 * if pos_array has changed
	 */
	if(mtreq->multitext.pos_array != mtold->multitext.pos_array){
		changed = True;

		if(mtreq->multitext.num_strings > 0){

			/* Allocat memory for Position Array */

			mtnew->multitext.pos_array =(float*)NhlMalloc(
					(unsigned)mtreq->multitext.num_strings *
								sizeof(float));

			if(mtnew->multitext.pos_array == NULL){
				NhlPError(FATAL,12,"Unable to SetValues");
				return FATAL;
			}

			for(i=0;i < mtnew->multitext.num_strings;i++)
				mtnew->multitext.pos_array[i] =
						mtreq->multitext.pos_array[i];
		} else {
			mtnew->multitext.pos_array = NULL;
		}
		/* free old memory */
		if(mtold->multitext.pos_array != NULL)
		(void)NhlFree(mtold->multitext.pos_array);
		mtold->multitext.pos_array = NULL;
	}

	/*
	 * if orientation or const_pos have changed
	 * or if any ViewClass resources have changed
	 */
	if((mtreq->multitext.orientation != mtold->multitext.orientation) ||
		(mtreq->multitext.const_pos != mtold->multitext.const_pos))
		changed = True;

	/*
	 * if any of the TextItem's geo resources have changed
	 */
	if((mtreq->multitext.angle != mtold->multitext.angle) ||
		(mtreq->multitext.font != mtold->multitext.font) ||
		(mtreq->multitext.just != mtold->multitext.just) ||
		(mtreq->multitext.font_quality !=
						mtold->multitext.font_quality)||
		(mtreq->multitext.font_height != mtold->multitext.font_height)||
		(mtreq->multitext.font_aspect != mtold->multitext.font_aspect)||
		(mtreq->multitext.font_thickness !=
					mtold->multitext.font_thickness) ||
		(mtreq->multitext.constant_spacing !=
					mtold->multitext.constant_spacing) ||
		(mtreq->multitext.direction != mtold->multitext.direction)){

		changed = True;

		lret = NhlSetValues(mtnew->multitext.text_object,
			NhlNtxAngleF,		mtnew->multitext.angle,
			NhlNtxFont,		mtnew->multitext.font,
			NhlNtxJust,		mtnew->multitext.just,
			NhlNtxFontQuality,	mtnew->multitext.font_quality,
			NhlNtxFontHeightF,	mtnew->multitext.font_height,
			NhlNtxFontAspectF,	mtnew->multitext.font_aspect,
			NhlNtxFontThicknessF,	mtnew->multitext.font_thickness,
			NhlNtxConstantSpacingF,
					mtnew->multitext.constant_spacing,
			NhlNtxDirection,	mtnew->multitext.direction,
			NULL);
		ret = MIN(ret,lret);
	}

	/*
	 * If nothing has changed - then a change in view class resources
	 * can be handled - ie. view class resource changes can not occur
	 * in the same setvalues call as a change in any other resource
	 * or they will be ignored.
	 * This is because the font resources can change because of a geometry
	 * change.
	 */
	if(!changed &&
		(mtnew->multitext.num_strings > 0) &&
		((mtreq->view.x != mtold->view.x) ||
		(mtreq->view.y != mtold->view.y) ||
		(mtreq->view.width != mtold->view.width) ||
		(mtreq->view.height != mtold->view.height))){

		float x[3], y[3];

		/*
		 * Now this object is changing
		 */
		changed = True;

		/*
		 * translate the bounding box for the first text item
		 */
		_NhlEvalTrans(mtnew->view.trans_children,
			mtnew->multitext.text_x,
			mtnew->multitext.text_y - mtnew->multitext.text_height,
			&x[0],&y[0]);

		_NhlEvalTrans(mtnew->view.trans_children,
			mtnew->multitext.text_x,
			mtnew->multitext.text_y,
			&x[1],&y[1]);

		_NhlEvalTrans(mtnew->view.trans_children,
			mtnew->multitext.text_x + mtnew->multitext.text_width,
			mtnew->multitext.text_y,
			&x[2],&y[2]);

		/*
		 * this setvalues changes the font charactoristics
		 * using the transformed x,y,width,height from the first
		 * text string (the first text string should be current)
		 */
		lret = NhlSetValues(mtnew->multitext.text_object,
			NhlNvpXF,	x[0],
			NhlNvpYF,	y[2],
			NhlNvpWidthF,	x[2] - x[0],
			NhlNvpHeightF,	y[1] - y[0],
			NULL);
		ret = MIN(ret,lret);

		/*
		 * Now translate ConstPos and pos_array
		 */
		mtnew->multitext.pos_array = (float*)NhlMalloc((unsigned)
				mtnew->multitext.num_strings * sizeof(float));
		if(mtnew->multitext.pos_array == NULL){
			NhlPError(FATAL,12,
					"Unable to change size of Multitext");
			return FATAL;
		}

		if(mtnew->multitext.orientation == MTEXT_X_CONST){

			for(i=0;i < mtnew->multitext.num_strings;i++){

				_NhlEvalTrans(mtnew->view.trans_children,
					mtold->multitext.const_pos,
					mtold->multitext.pos_array[i],
					&mtnew->multitext.const_pos,
					&mtnew->multitext.pos_array[i]);
			}
		}
		else{
			for(i=0;i < mtnew->multitext.num_strings;i++){

				_NhlEvalTrans(mtnew->view.trans_children,
					mtold->multitext.pos_array[i],
					mtold->multitext.const_pos,
					&mtnew->multitext.pos_array[i],
					&mtnew->multitext.const_pos);
			}
		}
	}

	/*
	 * Update the geometry of this object
	 */
	if(changed && (mtnew->multitext.num_strings > 0)){
		float x,y,width,height;
		/*
		 * Determine size based on font characterestics and
		 * text_object
		 */
		CalculateGeometry(mtnew,&x,&y,&width,&height);

		/*
		 * Set the size
		 */
		_NhlInternalSetView((ViewLayer)new,x,y,width,height,False);
		/*
		 * save the geometry of the first string
		 */
		if(mtnew->multitext.orientation == MTEXT_X_CONST){
			lret = NhlSetValues(mtnew->multitext.text_object,
				NhlNtxString,mtnew->multitext.text_strings[0],
				NhlNtxPosXF,mtnew->multitext.const_pos,
				NhlNtxPosYF,mtnew->multitext.pos_array[0],
				NULL);
		}
		else{
			lret = NhlSetValues(mtnew->multitext.text_object,
				NhlNtxString,mtnew->multitext.text_strings[0],
				NhlNtxPosXF,mtnew->multitext.pos_array[0],
				NhlNtxPosYF,mtnew->multitext.const_pos,
				NULL);
		}
		ret = MIN(ret,lret);

		lret = NhlGetValues(mtnew->multitext.text_object,
			NhlNvpXF,	&mtnew->multitext.text_x,
			NhlNvpYF,	&mtnew->multitext.text_y,
			NhlNvpWidthF,	&mtnew->multitext.text_width,
			NhlNvpHeightF,	&mtnew->multitext.text_height,
			NULL);
		ret = MIN(ret,lret);
	}

	return ret;
}

/*
 * Function:	MultiTextDraw
 *
 * Description:	This function is called when the MultiText object should
 *		draw it's string's
 *
 * In Args:	
 *		Layer	l	layer to draw
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
MultiTextDraw
#if	__STDC__
(
	Layer	l	/* layer to draw	*/
)
#else
(l)
	Layer	l;	/* layer to draw	*/
#endif
{
	MultiTextLayer	mtl = (MultiTextLayer)l;
	int		i;

	for(i=0;i < mtl->multitext.num_strings;i++){

		if(mtl->multitext.orientation == MTEXT_X_CONST){
			NhlSetValues(mtl->multitext.text_object,
				NhlNtxString,mtl->multitext.text_strings[i],
				NhlNtxPosXF,mtl->multitext.const_pos,
				NhlNtxPosYF,mtl->multitext.pos_array[i],
				NULL);
		}
		else{
			NhlSetValues(mtl->multitext.text_object,
				NhlNtxString,mtl->multitext.text_strings[i],
				NhlNtxPosXF,mtl->multitext.pos_array[i],
				NhlNtxPosYF,mtl->multitext.const_pos,
				NULL);
		}


		NhlDraw(mtl->multitext.text_object);
	}

	return NOERROR;
}

/*
 * Function:	MultiTextDestroy
 *
 * Description:	This is the destroy method for the multitext object.
 *		It is responsible for free'ing any memory used by
 *		the object.
 *
 * In Args:	
 *		Layer	l	layer to destroy
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
MultiTextDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
{
	MultiTextLayer	mtl = (MultiTextLayer)l;

	if(mtl->multitext.text_strings != NULL)
	(void)NhlFree(mtl->multitext.text_strings);
	if(mtl->multitext.pos_array != NULL)
	(void)NhlFree(mtl->multitext.pos_array);

	return NOERROR;
}
