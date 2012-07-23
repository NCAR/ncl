/*
 *      $Id: MultiText.c,v 1.25 2001-12-13 01:57:46 dbrown Exp $
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
#include <ncarg/hlu/MultiTextP.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/ConvertersP.h>
#include <math.h>
#include <ctype.h>

/* Resources */
#define Oset(field)	NhlOffset(NhlMultiTextLayerRec,multitext.field)
static NhlResource resources[] = {
	{NhlNMtextNumStrings, NhlCMtextNumStrings, NhlTInteger,
	 	sizeof(int),Oset(num_strings),NhlTImmediate,{0},
         	_NhlRES_DEFAULT,NULL},
/*
* The free function is probably wrong in this place
*/
	{NhlNMtextStrings, NhlCMtextStrings, NhlTPointer,
	 	sizeof(char**),Oset(text_strings),NhlTImmediate,{NULL},
         	_NhlRES_DEFAULT,(NhlFreeFunc)NhlFree},
	{NhlNMtextOrientation, NhlCMtextOrientation, NhlTMTextOrientationType,
		sizeof(NhlMTextOrientatonType),Oset(orientation),NhlTImmediate,
	 {(NhlPointer)NhlMTEXT_X_CONST},_NhlRES_DEFAULT,NULL},
	{NhlNMtextConstPosF, NhlCMtextConstPosF, NhlTFloat,
	 	sizeof(float),Oset(const_pos),NhlTString,{"-1.0"},
         	_NhlRES_DEFAULT,NULL},
	{NhlNMtextPosArray, NhlCMtextPosArray, NhlTPointer,
	 	sizeof(float*),Oset(pos_array),NhlTImmediate,{NULL},
         	_NhlRES_DEFAULT,(NhlFreeFunc)NhlFree},
	{NhlNMtextMaxLenF, NhlCMtextMaxLenF, NhlTFloat,
	 	sizeof(float),Oset(max_len),NhlTString,{"0.0"},
		 _NhlRES_GONLY,NULL},
	{NhlNMtextAutoStride, NhlCMtextAutoStride, NhlTBoolean,
	 	sizeof(NhlBoolean),Oset(auto_stride),NhlTImmediate,
	 	_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNMtextKeepEndItems, NhlCMtextKeepEndItems, NhlTBoolean,
	 	sizeof(NhlBoolean),Oset(keep_end_items),NhlTImmediate,
	 	_NhlUSET((NhlPointer)False),0,NULL},

	/*
	 * These resources are actually resources in the TextItem object
	 * that is used by this object.  If there are changes in the
	 * resources of TextItem they may need to be updated as well.
	 */
	{NhlNtxAngleF, NhlCTextAngleF, NhlTFloat,
	 	sizeof(float),Oset(angle),NhlTString,{"0.0"},0,NULL},
	{NhlNtxFont, NhlCFont, NhlTInteger,
	 	sizeof(int),Oset(font),NhlTImmediate,{(NhlPointer)21},0,NULL},
	{NhlNtxJust, NhlCTextJustification, NhlTInteger,
		 sizeof(int),Oset(just),NhlTImmediate,{(NhlPointer)4},0,NULL},
	{NhlNtxFontQuality, NhlCFontQuality, NhlTFontQuality,
		sizeof(NhlFontQuality),Oset(font_quality),NhlTImmediate,
	 {(NhlPointer)NhlHIGH},0,NULL},
	{NhlNtxFontHeightF, NhlCFontHeightF, NhlTFloat,
		 sizeof(float),Oset(font_height),NhlTString,{"0.5"},0,NULL},
	{NhlNtxFontAspectF, NhlCFontAspectF, NhlTFloat,
		 sizeof(float),Oset(font_aspect),NhlTString,{"1.3125"},0,NULL},
	{NhlNtxFontThicknessF, NhlCFontThicknessF, NhlTFloat,
		 sizeof(float),Oset(font_thickness),NhlTString,{"1.0"},0,NULL},
	{NhlNtxConstantSpacingF, NhlCTextConstantSpacingF, NhlTFloat,
		 sizeof(float),Oset(constant_spacing),NhlTString,{"0.0"},0,NULL},
	{NhlNtxDirection, NhlCTextDirection, NhlTTextDirection,
		sizeof(NhlTextDirection),Oset(direction),NhlTImmediate,
	 {(NhlPointer)NhlACROSS},0,NULL},
	{NhlNtxFuncCode, NhlCTextFuncCode, NhlTCharacter,
		 sizeof(char),Oset(func_code),NhlTString,{"~"},0,NULL}
};
#undef Oset

/* Method declarations	*/

static NhlErrorTypes MultiTextClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes MultiTextClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes MultiTextInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes MultiTextSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes MultiTextDraw(
#if	NhlNeedProto
	NhlLayer	l	/* layer to draw	*/
#endif
);

static NhlErrorTypes MultiTextSegDraw(
#if	NhlNeedProto
	NhlLayer	l	/* layer to draw	*/
#endif
);

static NhlErrorTypes MultiTextDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

/* Class definition	*/

NhlMultiTextClassRec NhlmultiTextClassRec = {
	{
/* class_name			*/	"multiTextClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlMultiTextLayerRec),
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

/* class_part_initialize	*/	MultiTextClassPartInitialize,
/* class_initialize		*/	MultiTextClassInitialize,
/* layer_initialize		*/	MultiTextInitialize,
/* layer_set_values		*/	MultiTextSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	MultiTextDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	MultiTextDraw,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	MultiTextSegDraw,
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

NhlClass NhlmultiTextClass = (NhlClass)&NhlmultiTextClassRec;

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

static NhlErrorTypes
MultiTextClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlEnumVals	mtotype[] = {
		{NhlMTEXT_X_CONST,	"mtext_x_const"},
		{NhlMTEXT_Y_CONST,	"mtext_y_const"},
		};

	_NhlRegisterEnumType(NhlmultiTextClass,NhlTMTextOrientationType,mtotype,
		NhlNumber(mtotype));

	return NhlNOERROR;
}

/*
 * Function:	MutliTextClassPartInitialize
 *
 * Description:	This function initializes the MultiText NhlClass record.
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
MultiTextClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* NhlClass being inited	*/
)
#else
(lc)
	NhlClass	lc;	/* NhlClass being inited	*/
#endif
{
	/*
	 * Register children classes
	 */
	return _NhlRegisterChildClass(lc,NhltextItemClass,True,False,
				NhlNtxString,NhlNtxPosXF,NhlNtxPosYF,NULL);
}

/*
 * Function:	CalculateGeometry
 *
 * Description:	This function calculates the size of the Multitext layer
 *		passed to it.
 *
 * In Args:	
 *		NhlMultiTextLayer	l,		The NhlLayer
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
#if	NhlNeedProto
(
	NhlMultiTextLayer	l,		/* The NhlLayer		*/
	float			*x,		/* x return		*/
	float			*y,		/* y return		*/
	float			*width,		/* width return		*/
	float			*height		/* height return	*/
)
#else
(l,x,y,width,height)
	NhlMultiTextLayer	l;		/* The NhlLayer		*/
	float			*x;		/* x return		*/
	float			*y;		/* y return		*/
	float			*width;		/* width return		*/
	float			*height;	/* height return	*/
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

		if (l->multitext.auto_stride && ! l->multitext.do_draw[i])
			continue;

		if(l->multitext.orientation == NhlMTEXT_X_CONST){
			NhlVASetValues(l->multitext.text_object,
				NhlNtxString,l->multitext.text_strings[i],
				NhlNtxPosXF,l->multitext.const_pos,
				NhlNtxPosYF,l->multitext.pos_array[i],
				NULL);
		}
		else{
			NhlVASetValues(l->multitext.text_object,
				NhlNtxString,l->multitext.text_strings[i],
				NhlNtxPosXF,l->multitext.pos_array[i],
				NhlNtxPosYF,l->multitext.const_pos,
				NULL);
		}

		/*
		 * retrieve child's geometry
		 */
		NhlVAGetValues(l->multitext.text_object,
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
 * Function:	GetMaxTextLength
 *
 * Description:	This function finds the maximum NDC text length of the  
 *		set of multi-text strings.
 *
 * In Args:	
 *		NhlMultiTextLayer	l,		The NhlLayer
 *
 * Out Args:	
 *		float		*maxlen,	max text length
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	none
 */
static NhlErrorTypes
GetMaxTextLength
#if	NhlNeedProto
(
	NhlMultiTextLayer	l,		/* The NhlLayer		*/
	float			*maxlen		/* max text len		*/
)
#else
(l,maxlen)
	NhlMultiTextLayer	l;		/* The NhlLayer		*/
	float			*maxlen;	/* max text len		*/
#endif
{
	int		i;
	float		twidth,theight;

	
	*maxlen = 0.0;
	l->multitext.max_extent = 0.0;
	

	for(i=0;i < l->multitext.num_strings;i++){

		if(l->multitext.orientation == NhlMTEXT_X_CONST){
			NhlVASetValues(l->multitext.text_object,
				NhlNtxString,l->multitext.text_strings[i],
				NhlNtxPosXF,l->multitext.const_pos,
				NhlNtxAngleF,l->multitext.angle,
				NhlNtxPosYF,l->multitext.pos_array[i],
				NULL);
		}
		else{
			NhlVASetValues(l->multitext.text_object,
				NhlNtxString,l->multitext.text_strings[i],
				NhlNtxPosXF,l->multitext.pos_array[i],
				NhlNtxPosYF,l->multitext.const_pos,
				NhlNtxAngleF,l->multitext.angle,
				NULL);
		}

		/*
		 * retrieve the text object width
		 */
		NhlVAGetValues(l->multitext.text_object,
			       NhlNvpWidthF,	&twidth,
			       NhlNvpHeightF,	&theight,
			       NULL);

		if (l->multitext.direction == NhlDOWN) {
			if (theight > *maxlen)
				*maxlen = theight;
		}
		else {
			if (twidth > *maxlen)
				*maxlen = twidth;
		}

		if (l->multitext.auto_stride) {
			if(l->multitext.orientation == NhlMTEXT_X_CONST)
				l->multitext.extents[i] = theight;
			else
				l->multitext.extents[i] = twidth;

			if (l->multitext.extents[i] > l->multitext.max_extent)
				l->multitext.max_extent = 
					l->multitext.extents[i];
		}	
	}
	return NhlNOERROR;
}

/*
 * Function:	GetActualCharCount
 *
 * Description:	Counts only characters that are not function code directives
 *              
 *
 * In Args:	
 *		NhlMultiTextLayer	l,		The NhlLayer
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	char count
 * Side Effect:	none
 */
static int
GetActualCharCount
#if	NhlNeedProto
(
	NhlString text,
	char	fcode
)
#else
(text, fcode)
NhlString text;
char	fcode;
#endif
{
	char *cp;
	int count = 0;
	NhlBoolean in = False;

	for (cp = text; *cp != '\0'; cp++) {
		if (*cp == fcode) {
			in = in ? False : True;
			continue;
		}
		if (in)
			continue;
		count++;
	}
	return count;

}


/*
 * Function:	GetZeroFraction
 *
 * Description:	returns the fraction of a number's digits that are 0
 *              starting from the left.
 *              
 *
 * In Args:	
 *		NhlMultiTextLayer	l,		The NhlLayer
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	char count
 * Side Effect:	none
 */
static float
GetZeroFraction
#if	NhlNeedProto
(
	NhlString text,
	char	fcode
)
#else
(text, fcode)
NhlString text;
char	fcode;
#endif
{
	char *cp,*lcp;
	int count = 0,zcount = 0;
	NhlBoolean in = False;
	NhlBoolean first = True;

	for (cp = text; *cp != '\0'; cp++) {
		if (*cp == fcode) {
			in = in ? False : True;
			continue;
		}
		if (in)
			continue;
		if (isdigit(*cp)) {
			if (first)
				first = False;
			count++;
			if (*cp != '0')
				zcount = 0;
			else
				zcount++;
		}
		else if (*cp == '.')
			continue;
		else if (! first) {
			lcp = cp - 1;
			break;
		}
	}
	if (count == 0) {
		return 0;
	}
	return zcount / (float)count;

}

/*
 * Function:	SetDrawFlags
 *
 * Description:	If auto_stride is set on, this routine figures out
 *               which text items to eliminate to avoid overlap
 *
 * In Args:	
 *		NhlMultiTextLayer	l,		The NhlLayer
 *
 * Out Args:	
 *		float		*maxlen,	max text length
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	none
 */
static NhlErrorTypes
SetDrawFlags
#if	NhlNeedProto
(
	NhlMultiTextLayer	l		/* The NhlLayer		*/
)
#else
(l,maxlen)
	NhlMultiTextLayer	l;		/* The NhlLayer		*/
#endif
{
	NhlMultiTextLayerPart *mtp = &(l->multitext);
	float sep = mtp->max_extent / 12.0;
	int min_length = 10000;
	int pivot,last;
	int i,stride,minlen_count = 0, maxz_count = 0;
	float zfrac,max_zfrac = 0;
	int minlen_ix = -1, maxzfrac_ix = -1;
	NhlBoolean done;
	NhlErrorTypes ret = NhlNOERROR;
	int istart, iend;

	if (mtp->num_strings < 1)
		return ret;

	sep = MAX(mtp->max_extent / 12.0, l->multitext.font_height / 2.0); 
	if (mtp->keep_end_items) {
		istart = 1;
		iend = mtp->num_strings - 1;
	}
	else {
		istart = 0;
		iend = mtp->num_strings;
	}

	for (i = istart; i < iend; i++) {
		int len;
		if (strchr(mtp->text_strings[i],mtp->func_code))
			len = GetActualCharCount
				(mtp->text_strings[i],mtp->func_code);
		else
			len = strlen(mtp->text_strings[i]);
			
		if (len < min_length) {
			min_length = len;
			minlen_ix = i;
			minlen_count = 1;
		}
		else if (len == min_length)
			minlen_count++;

		zfrac = GetZeroFraction(mtp->text_strings[i],mtp->func_code);
		if (zfrac > 0.0) {
			if (zfrac > max_zfrac) {
				max_zfrac = zfrac;
				maxzfrac_ix = i;
				maxz_count = 1;
			}
			else if (zfrac == max_zfrac) {
				maxz_count++;
			}
		}
		mtp->do_draw[i] = False;
	}
	pivot = istart;
	if (minlen_count && maxz_count) {
		if (maxzfrac_ix > -1 && maxz_count <= minlen_count)
			pivot = maxzfrac_ix;
		else if (minlen_ix > -1)
 			pivot = minlen_ix;
	}
	else if (maxz_count && maxzfrac_ix > -1)
		pivot = maxzfrac_ix;
	else if (minlen_count && minlen_ix > -1)
		pivot = minlen_ix;


	stride = 1;
	done = False;
	while (! done) {
		NhlBoolean redo = False;
		last = pivot;
		for (i = pivot - stride ; i >= istart; i-= stride) {
			if ((0.5 * 
			     (mtp->extents[last] + mtp->extents[i]) + sep) >
			    fabs(mtp->pos_array[last] - mtp->pos_array[i])) {
				stride += 1;
				redo = True;
				break;
			}
			else {
				last = i;
			}
		}
		if (! redo)
			done = True;
	}
	done = False;
	while (! done) {
		NhlBoolean redo = False;
		last = pivot;
		for (i = pivot + stride; i < iend; i+= stride) {
			if ((0.5 * 
			     (mtp->extents[last] + mtp->extents[i]) + sep) >
			    fabs(mtp->pos_array[i] - mtp->pos_array[last])) {
				stride += 1;
				redo = True;
				break;
			}
			else {
				last = i;
			}
		}
		if (! redo)
			done = True;
	}
	mtp->do_draw[pivot] = True;
	for (i = pivot - stride ; i >= istart; i-= stride) {
		mtp->do_draw[i] = True;
	}
	for (i = pivot + stride; i < iend; i+= stride) {
		mtp->do_draw[i] = True;
	}

	if (mtp->keep_end_items) {
		float end_extent = MAX(mtp->extents[0],
				       mtp->extents[mtp->num_strings-1]);

		mtp->do_draw[0] = True;
		for (i = 1; i < mtp->num_strings; i++) {
			if (!mtp->do_draw[i])
				continue;
			if ((0.5 * 
			     (end_extent + mtp->extents[i]) + sep) >
			    fabs(mtp->pos_array[i] - mtp->pos_array[0])) {
				mtp->do_draw[i] = False;
				continue;
			}
			break;
		}
		last = mtp->num_strings-1;
		mtp->do_draw[last] = True;
		for (i = last - 1; i >= 0; i--) {
			if (!mtp->do_draw[i])
				continue;
			if ((0.5 * 
			     (end_extent + mtp->extents[i]) + sep) >
			    fabs(mtp->pos_array[last] - mtp->pos_array[i])) {
				mtp->do_draw[i] = False;
				continue;
			}
			break;
		}
	}
			

			

	return ret;
}
	

/*
 * Function:	MultiTextInitialize
 *
 * Description:	This function initializes an instance of a MultiText layer.
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
MultiTextInitialize
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
	NhlMultiTextLayer	mtnew = (NhlMultiTextLayer)new;
	NhlMultiTextLayer	mtreq = (NhlMultiTextLayer)req;
	int		i;
	int		num_strings = mtnew->multitext.num_strings;
	char		**text_strings;
	char		name[_NhlMAXRESNAMLEN];
	float		x,y,width,height;
	NhlErrorTypes	ret = NhlNOERROR,subret = NhlNOERROR;

	mtnew->multitext.extents = NULL;
	mtnew->multitext.do_draw = NULL;

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

		/* Allocate memory for Position Array */

		mtnew->multitext.pos_array =
			(float*)NhlMalloc((unsigned)num_strings*sizeof(float));

		for(i=0;i<num_strings;i++)
			mtnew->multitext.pos_array[i] =
						mtreq->multitext.pos_array[i];

		if (mtnew->multitext.auto_stride) {
			mtnew->multitext.extents = 
				(float*)NhlMalloc(num_strings*sizeof(float));
			mtnew->multitext.do_draw = 
				(NhlBoolean*)NhlMalloc
				(num_strings*sizeof(NhlBoolean));
		}
	} else {
		mtnew->multitext.text_strings = NULL;
		mtnew->multitext.pos_array = NULL;
	}

	/* give child a name */
	strcpy(name,new->base.name);
	strcat(name,"-TxtItm");

	subret = _NhlVACreateChild(&mtnew->multitext.text_object,name,
						NhltextItemClass,new,
		NhlNtxAngleF,		mtnew->multitext.angle,
		NhlNtxFont,		mtnew->multitext.font,
		NhlNtxJust,		mtnew->multitext.just,
		NhlNtxFontQuality,	mtnew->multitext.font_quality,
		NhlNtxFontHeightF,	mtnew->multitext.font_height,
		NhlNtxFontAspectF,	mtnew->multitext.font_aspect,
		NhlNtxFontThicknessF,	mtnew->multitext.font_thickness,
		NhlNtxConstantSpacingF,	mtnew->multitext.constant_spacing,
		NhlNtxDirection,	mtnew->multitext.direction,
		NhlNtxFuncCode,		mtnew->multitext.func_code,
		NULL);

	ret = MIN(ret,subret);
	if(num_strings > 0){

		/*
		 * Get the maximum text len
		 */

		subret = GetMaxTextLength(mtnew,&mtnew->multitext.max_len);
		ret = MIN(ret,subret);

		/*
		 * if cull overlaps is on set up the draw flag array
		 */

		if (mtnew->multitext.auto_stride) {
			subret = SetDrawFlags(mtnew);
			ret = MIN(ret,subret);
		}

		/*
		 * Determine size based on font characterestics and text_object
		 */
		CalculateGeometry(mtnew,&x,&y,&width,&height);

		/*
		 * Set the size
		 */
		_NhlInternalSetView((NhlViewLayer)new,x,y,width,height,False);
	}

	return ret;
}

/*
 * Function:	MultiTextSetValues
 *
 * Description:	This is the SetValues method for the MultiText object.
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
MultiTextSetValues
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
	int		i;
	NhlBoolean	changed = False;
	NhlBoolean	text_changed = False, attrs_changed = False;
	NhlMultiTextLayer	mtold = (NhlMultiTextLayer)old;
	NhlMultiTextLayer	mtreq = (NhlMultiTextLayer)req;
	NhlMultiTextLayer	mtnew = (NhlMultiTextLayer)new;
	NhlErrorTypes	ret = NhlNOERROR;
	NhlErrorTypes	lret = NhlNOERROR;

	if(mtreq->multitext.num_strings != mtold->multitext.num_strings){
		if((mtreq->multitext.text_strings ==
			mtold->multitext.text_strings) ||
				(mtreq->multitext.pos_array ==
						mtold->multitext.pos_array)){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Multitext SetValues:Changing number of strings w/o changing strings or positions");
			return NhlFATAL;
		}
		if (mtnew->multitext.auto_stride) {
			NhlFree(mtnew->multitext.extents);
			NhlFree(mtnew->multitext.do_draw);
			mtnew->multitext.extents = NULL;
			mtnew->multitext.do_draw = NULL;
		}
		
	}
	if (mtnew->multitext.auto_stride) {
		int num_strings = mtreq->multitext.num_strings;
		if (! mtnew->multitext.extents)
			mtnew->multitext.extents  = 
				(float*)NhlMalloc(num_strings*sizeof(float));
		if (! mtnew->multitext.do_draw)
			mtnew->multitext.do_draw = 
				(NhlBoolean*)NhlMalloc
				(num_strings*sizeof(NhlBoolean));
		if (! (mtnew->multitext.extents &&
		       mtnew->multitext.do_draw)) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
	}

	/*
	 * if text_strings has changed
	 */
	if(mtreq->multitext.text_strings != mtold->multitext.text_strings){
		changed = True;
		text_changed = True;

		if(mtreq->multitext.num_strings > 0){

			/* Allocate memory for text_strings */

			mtnew->multitext.text_strings =(char**)NhlMalloc(
					(unsigned)mtreq->multitext.num_strings *
								sizeof(char*));
			if(mtnew->multitext.text_strings == NULL){
				NhlPError(NhlFATAL,12,"Unable to SetValues");
				return NhlFATAL;
			}

			for(i=0;i < mtreq->multitext.num_strings;i++){
				mtnew->multitext.text_strings[i] =
					(char*)NhlMalloc((unsigned)strlen(
					mtreq->multitext.text_strings[i]) + 1);
				if(mtnew->multitext.text_strings[i] == NULL){
					NhlPError(NhlFATAL,12,
							"Unable to SetValues");
					return NhlFATAL;
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
				NhlPError(NhlFATAL,12,"Unable to SetValues");
				return NhlFATAL;
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
		attrs_changed = True;

		lret = NhlVASetValues(mtnew->multitext.text_object,
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
			NhlNtxFuncCode,		mtnew->multitext.func_code,
			NULL);
		ret = MIN(ret,lret);
	}


	if (text_changed || attrs_changed
	    || mtnew->multitext.auto_stride
	    != mtold->multitext.auto_stride
	    || mtnew->multitext.keep_end_items
	    != mtold->multitext.keep_end_items) {

		/*
		 * Get the maximum text len
		 */
		
		lret = GetMaxTextLength(mtnew,&mtnew->multitext.max_len);
		ret = MIN(ret,lret);

		if (mtnew->multitext.auto_stride) {
			lret = SetDrawFlags(mtnew);
			ret = MIN(ret,lret);
		}

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

		/*
		 * Now this object is changing
		 */
		changed = True;

		/*
		 * Now translate ConstPos and pos_array
		 */
		mtnew->multitext.pos_array = (float*)NhlMalloc((unsigned)
				mtnew->multitext.num_strings * sizeof(float));
		if(mtnew->multitext.pos_array == NULL){
			NhlPError(NhlFATAL,12,
					"Unable to change size of Multitext");
			return NhlFATAL;
		}

		if(mtnew->multitext.orientation == NhlMTEXT_X_CONST){

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
		_NhlInternalSetView((NhlViewLayer)new,x,y,width,height,False);
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
 *		NhlLayer	l	layer to draw
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
MultiTextDraw
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to draw	*/
)
#else
(l)
	NhlLayer	l;	/* layer to draw	*/
#endif
{
	NhlMultiTextLayer	mtl = (NhlMultiTextLayer)l;
	int		i;

	for(i=0;i < mtl->multitext.num_strings;i++){

		if (mtl->multitext.auto_stride 
		    && ! mtl->multitext.do_draw[i])
			continue;
		if(mtl->multitext.orientation == NhlMTEXT_X_CONST){
			NhlVASetValues(mtl->multitext.text_object,
				NhlNtxString,mtl->multitext.text_strings[i],
				NhlNtxPosXF,mtl->multitext.const_pos,
				NhlNtxPosYF,mtl->multitext.pos_array[i],
				NULL);
		}
		else{
			NhlVASetValues(mtl->multitext.text_object,
				NhlNtxString,mtl->multitext.text_strings[i],
				NhlNtxPosXF,mtl->multitext.pos_array[i],
				NhlNtxPosYF,mtl->multitext.const_pos,
				NULL);
		}


		NhlDraw(mtl->multitext.text_object);
	}

	return NhlNOERROR;
}


/*
 * Function:	MultiTextSegDraw
 *
 * Description:	This function is called when the MultiText object should
 *		draw it's string's
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
MultiTextSegDraw
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to draw	*/
)
#else
(l)
	NhlLayer	l;	/* layer to draw	*/
#endif
{
	NhlMultiTextLayer	mtl = (NhlMultiTextLayer)l;
	int		i;

	for(i=0;i < mtl->multitext.num_strings;i++){

		if (mtl->multitext.auto_stride 
		    && ! mtl->multitext.do_draw[i])
			continue;

		if(mtl->multitext.orientation == NhlMTEXT_X_CONST){
			NhlVASetValues(mtl->multitext.text_object,
				NhlNtxString,mtl->multitext.text_strings[i],
				NhlNtxPosXF,mtl->multitext.const_pos,
				NhlNtxPosYF,mtl->multitext.pos_array[i],
				NULL);
		}
		else{
			NhlVASetValues(mtl->multitext.text_object,
				NhlNtxString,mtl->multitext.text_strings[i],
				NhlNtxPosXF,mtl->multitext.pos_array[i],
				NhlNtxPosYF,mtl->multitext.const_pos,
				NULL);
		}


		_NhlSegDraw(_NhlGetLayer(mtl->multitext.text_object));
	}

	return NhlNOERROR;
}

/*
 * Function:	MultiTextDestroy
 *
 * Description:	This is the destroy method for the multitext object.
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
MultiTextDestroy
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	int		i;
	NhlMultiTextLayer	mtl = (NhlMultiTextLayer)l;

	if((mtl->multitext.num_strings > 0) &&
		(mtl->multitext.text_strings != NULL)){

		for(i=0;i < mtl->multitext.num_strings;i++)
			(void)NhlFree(mtl->multitext.text_strings[i]);
		(void)NhlFree(mtl->multitext.text_strings);
	}

	if(mtl->multitext.pos_array != NULL)
		(void)NhlFree(mtl->multitext.pos_array);

	if(mtl->multitext.extents != NULL)
		(void)NhlFree(mtl->multitext.extents);

	if(mtl->multitext.do_draw != NULL)
		(void)NhlFree(mtl->multitext.do_draw);

	return NhlNOERROR;
}
