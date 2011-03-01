/*
 *      $Id: CoordArrays.c,v 1.42 1998-04-16 03:08:35 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArrays.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jan 18 12:37:23 MST 1994
 *
 *	Description:	This class is used to communicate data in the format
 *			of a CoordArray.
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/CoordArraysP.h>
#include <ncarg/hlu/CoordArrTableP.h>
#include <ncarg/hlu/ConvertersP.h>

static	NrmQuark	genQ = NrmNULLQUARK;
static	NrmQuark	floatQ = NrmNULLQUARK;
static	NrmQuark	floatgenQ = NrmNULLQUARK;

static	NrmQuark	xarrQ = NrmNULLQUARK;
static	NrmQuark	yarrQ = NrmNULLQUARK;

static	NrmQuark	xmissQ = NrmNULLQUARK;
static	NrmQuark	ymissQ = NrmNULLQUARK;

static	NrmQuark	xmaxQ = NrmNULLQUARK;
static	NrmQuark	ymaxQ = NrmNULLQUARK;
static	NrmQuark	xminQ = NrmNULLQUARK;
static	NrmQuark	yminQ = NrmNULLQUARK;

typedef enum _NhlcaDType_{
	XDIM,
	YDIM
} _NhlcaDType;

/************************************************************************
*									*
*	CoordArrays Class declarations					*
*									*
************************************************************************/

/*
 * Resource Declarations
 */
/*ARGSUSED*/
static NhlErrorTypes
XCastSet
#if	NhlNeedProto
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
	NhlCoordArraysLayer	carrl = (NhlCoordArraysLayer)base;

	carrl->carr.xcast_set = False;
	carrl->carr.xcast = NhlMULTIPLEVECTORS;

	return NhlNOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
YCastSet
#if	NhlNeedProto
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
	NhlCoordArraysLayer	carrl = (NhlCoordArraysLayer)base;

	carrl->carr.ycast_set = False;
	carrl->carr.ycast = NhlMULTIPLEVECTORS;

	return NhlNOERROR;
}

#define	Oset(field)	NhlOffset(NhlCoordArraysLayerRec,carr.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNcaXArray,NhlCcaXArray,NhlTGenArray,sizeof(NhlGenArray),
		Oset(xarray),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
	{NhlNcaYArray,NhlCcaYArray,NhlTGenArray,sizeof(NhlGenArray),
		Oset(yarray),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
	{"no.res","No.Res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(xcast_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_PRIVATE,NULL},
	{"no.res","No.Res",NhlTBoolean,sizeof(int),
		Oset(ycast_set),NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_PRIVATE,NULL},
	{NhlNcaXCast,NhlCcaCast,NhlTcaCastMode,sizeof(NhlcaCastMode),
		Oset(xcast),NhlTProcedure,_NhlUSET((NhlPointer)XCastSet),0,NULL},
	{NhlNcaYCast,NhlCcaCast,NhlTcaCastMode,sizeof(NhlcaCastMode),
		Oset(ycast),NhlTProcedure,_NhlUSET((NhlPointer)YCastSet),0,NULL},
	{NhlNcaCopyArrays,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		Oset(copy_arrays),NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNcaXMissingV,NhlCdiMissingValue,NhlTVariable,sizeof(NhlGenArray),
		Oset(missing_x),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
	{NhlNcaYMissingV,NhlCdiMissingValue,NhlTVariable,sizeof(NhlGenArray),
		Oset(missing_y),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
	{NhlNcaXMaxV,NhlCcaXMaxV,NhlTVariable,sizeof(NhlGenArray),
		Oset(max_x),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
	{NhlNcaYMaxV,NhlCcaYMaxV,NhlTVariable,sizeof(NhlGenArray),
		Oset(max_y),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
	{NhlNcaXMinV,NhlCcaXMinV,NhlTVariable,sizeof(NhlGenArray),
		Oset(min_x),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
	{NhlNcaYMinV,NhlCcaYMinV,NhlTVariable,sizeof(NhlGenArray),
		Oset(min_y),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},

/* End-documented-resources */

	/*
	 * init private fields
	 */
	{"no.res","No.Res",NhlTPointer,sizeof(NhlPointer),
		Oset(my_xarray),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
         	_NhlRES_PRIVATE,NULL},
	{"no.res","No.Res",NhlTPointer,sizeof(NhlPointer),
		Oset(my_yarray),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
         	_NhlRES_PRIVATE,NULL},
	{"no.res","No.Res",NhlTPointer,sizeof(NhlPointer),
		Oset(xctxt),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
         	_NhlRES_PRIVATE,NULL},
	{"no.res","No.Res",NhlTPointer,sizeof(NhlPointer),
		Oset(yctxt),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
         	_NhlRES_PRIVATE,NULL},
	{"no.res","No.Res",NhlTPointer,sizeof(NhlPointer),
		Oset(my_missing_x),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
         	_NhlRES_PRIVATE,NULL},
	{"no.res","No.Res",NhlTPointer,sizeof(NhlPointer),
		Oset(my_missing_y),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
         	_NhlRES_PRIVATE,NULL},
};
#undef Oset

/* base methods */

static NhlErrorTypes CoordArraysClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes CoordArraysInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArraysSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArraysGetValues(
#if	NhlNeedProto
	NhlLayer	l,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArraysDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

NhlCoordArraysClassRec NhlcoordArraysClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"coordArraysClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCoordArraysLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhldataItemClassRec,
/* cvt_table			*/	NULL,

/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	CoordArraysClassInitialize,
/* layer_initialize		*/	CoordArraysInitialize,
/* layer_set_values		*/	CoordArraysSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	CoordArraysGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	CoordArraysDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	/* NhlDataItemClassPart */
	{
/* foo				*/	0
	},
	/* NhlCoordArraysClassPart */
	{
/* foo				*/	0
	}
};
	
NhlClass NhlcoordArraysClass = (NhlClass)
					&NhlcoordArraysClassRec;

/*
 * Function:	nhlfcoordarraysclass
 *
 * Description:	fortran ref to contour class
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
_NHLCALLF(nhlfcoordarraysclass,NHLFCOORDARRAYSCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlcoordArraysClass;
}

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

/*
 * Function:	CreateFloatTable
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlGenArray
 * Side Effect:	
 */
static NhlErrorTypes
CreateFloatTable
#if	NhlNeedProto
(
	NhlString	cast_res,
	NhlString	other_cast_res,
	NhlString	error_lead,
	NhlGenArray	gen,
	NhlGenArray	other_gen,
	NhlcaCastMode	cast,
	NhlcaCastMode	other_cast,
	NhlGenArray	*tbl,
	NhlGenArray	*tbl_lens
)
#else
(cast_res,other_cast_res,error_lead,gen,other_gen,cast,other_cast,tbl,tbl_lens)
	NhlString	cast_res;
	NhlString	other_cast_res;
	NhlString	error_lead;
	NhlGenArray	gen;
	NhlGenArray	other_gen;
	NhlcaCastMode	cast;
	NhlcaCastMode	other_cast;
	NhlGenArray	*tbl;
	NhlGenArray	*tbl_lens;
#endif
{
	ng_size_t vectors, elements;
	ng_size_t i,j;
	float	**flttable, *fltvect;
	int	*intvect;

	if(!gen){
		*tbl = NULL;
		*tbl_lens = NULL;

		return NhlNOERROR;
	}

	switch(cast){
		case NhlSINGLEVECTOR:
			switch(other_cast){
				case NhlSINGLEVECTOR:
					vectors = 1;
					break;
				case NhlMULTIPLEVECTORS:
					vectors = other_gen->len_dimensions[0];
					break;
				case NhlSPLITVECTORS:
					vectors = other_gen->len_dimensions[1];
					break;
				default:
					NhlPError(NhlFATAL,NhlEUNKNOWN,
							"%s:Invalid %s value",
						error_lead,other_cast_res);
					return NhlFATAL;
			}

			elements = gen->len_dimensions[0];
			break;
		case NhlMULTIPLEVECTORS:
			if(gen->num_dimensions == 1){
				vectors = 1;
				elements = gen->len_dimensions[0];
			}
			else{
				vectors = gen->len_dimensions[0];
				elements = gen->len_dimensions[1];
			}
			break;
		case NhlSPLITVECTORS:
			if(gen->num_dimensions == 1){
				vectors = gen->len_dimensions[0];
				elements = 1;
			}
			else{
				vectors = gen->len_dimensions[1];
				elements = gen->len_dimensions[0];
			}
			break;
		default:
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid %s value",
							error_lead,cast_res);
			return NhlFATAL;
	}

	flttable = NhlConvertMalloc(sizeof(float*)*vectors);
	intvect = NhlConvertMalloc(sizeof(int)*vectors);
	if((flttable == NULL) || (intvect == NULL)){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NhlFATAL;
	}

	/*
	 * gen is a float GenArray since it is from my_{n}array.
	 */
	fltvect = gen->data;

	for(i=0;i < vectors; i++){
		intvect[i] = elements;

		switch(cast){
		case NhlSINGLEVECTOR:
			flttable[i] = fltvect;
			break;
		case NhlMULTIPLEVECTORS:
			flttable[i] = fltvect + (i * elements);
			break;
		case NhlSPLITVECTORS:
			flttable[i] = NhlConvertMalloc(sizeof(float)*elements);
			if(flttable[i] == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
			for(j=0;j<elements;j++)
				*(flttable[i]+j) = *(fltvect+i+(j*elements));
			break;
		}
	}

	*tbl = _NhlConvertCreateGenArray(flttable,NhlTPointer,
					sizeof(NhlPointer),vectors,NULL);
	*tbl_lens = _NhlConvertCreateGenArray(intvect,NhlTInteger,sizeof(int),
								vectors,NULL);

	if((*tbl == NULL) || (*tbl_lens == NULL))
		return NhlFATAL;

	return NhlNOERROR;
}

/*
 * Function:	MyArray
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
MyArray
#if	NhlNeedProto
(
	NhlGenArray		*my_array,
	NhlGenArray		array,
	_NhlConvertContext	*context
)
#else
(my_array,array,context)
	NhlGenArray		*my_array;
	NhlGenArray		array;
	_NhlConvertContext	*context;
#endif
{
	char			func[]="MyArray";
	_NhlConvertContext	tctxt = NULL;
	NhlGenArray		tgen = NULL;
	NrmValue		from,to;
	NhlErrorTypes		ret;

	if(!array)
		*my_array = NULL;
	else if(array->typeQ == floatQ)
		*my_array = array;
	else{
		tctxt = _NhlCreateConvertContext(NULL);
		if(tctxt == NULL){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}

		from.size = sizeof(NhlGenArray);
		from.data.ptrval = array;
		to.size = sizeof(NhlGenArray);
		to.data.ptrval = &tgen;

		ret = _NhlConvertData(tctxt,genQ,floatgenQ,&from,&to);

		if((ret < NhlWARNING) || !tgen){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert from %s to %s",func,
					NhlTGenArray,NhlTFloatGenArray));
			return NhlFATAL;
		}
		*my_array = tgen;
	}

	_NhlFreeConvertContext(*context);
	*context = tctxt;

	return NhlNOERROR;
}

/*
 * Function:	GetMinMax
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
static void
GetMinMax
#if	NhlNeedProto
(
	NhlGenArray	array,
	NhlGenArray	oarray,
	NhlcaCastMode	cast,
	NhlcaCastMode	ocast,
	NhlGenArray	miss,
	float		*min,
	float		*max
)
#else
(array,oarray,cast,ocast,miss,min,max)
	NhlGenArray	array;
	NhlGenArray	oarray;
	NhlcaCastMode	cast;
	NhlcaCastMode	ocast;
	NhlGenArray	miss;
	float		*min;
	float		*max;
#endif
{
	float	mx,mn;
	float	*farr;

	if(array == NULL){
		/*
		 * array is *implied* - use indexes of other dim
		 */
		mn = 1.0;
		if((ocast == NhlMULTIPLEVECTORS) &&
						(oarray->num_dimensions == 2))
			mx = oarray->len_dimensions[1];
		else
			mx = oarray->len_dimensions[0];
	}
	else{
		NhlBoolean	init = False;
		int		i,len;

		mx = mn = 0.0;

		farr = array->data;

		if(cast == NhlSINGLEVECTOR)
			len = array->len_dimensions[0];
		else
			len = array->num_elements;

		for(i=0;i < len;i++){

			if(miss && (farr[i]==*(float*)miss->data))
				continue;

			if(init){
				mx = MAX(farr[i],mx);
				mn = MIN(farr[i],mn);
			}
			else{
				mx = farr[i];
				mn = farr[i];
				init = True;
			}
		}

		if(!init){
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
		"No Valid values in Array, unable to compute Min or Max"));
		}
	}

	*min = mn;
	*max = mx;
	return;
}

/*
 * Function:	FlushObj
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
FlushObj
#if	NhlNeedProto
(
	_NhlcaDType			dim,
	NhlCoordArraysLayerPart		*cap
)
#else
(dim,cap)
	_NhlcaDType			dim;
	NhlCoordArraysLayerPart		*cap;
#endif
{
	char		func[] = "FlushObj";
	float		max,min;
	NrmValue	from,to;
	NhlErrorTypes	ret=NhlNOERROR,lret;

	if(dim == XDIM){

		if(!cap->my_xarray){
			ret = MyArray(&cap->my_xarray,cap->xarray,&cap->xctxt);

			if(ret < NhlWARNING){
				NhlPError(ret,NhlEUNKNOWN,
					"%s:Unable to convert %s to floats",
							func,NhlNcaXArray);
				return ret;
			}
		}
		if(!cap->my_missing_x && cap->missing_x){
			if(cap->missing_x->typeQ == floatQ)
				cap->my_missing_x = cap->missing_x;
			else{
				float	tfloat;

				from.size = sizeof(NhlGenArray);
				from.data.ptrval = cap->missing_x;
				to.size = sizeof(float);
				to.data.ptrval = &tfloat;

				lret = NhlConvertData(NhlDEFAULT_APP,
					NhlTVariable,NhlTFloat,&from,&to);
				if(ret < NhlWARNING)
					return NhlFATAL;
				ret = MIN(ret,lret);

				cap->my_missing_x = _NhlCreateGenArray(&tfloat,
					NhlTFloat,sizeof(float),1,NULL,True);

				if(!cap->my_missing_x)
					return NhlFATAL;
			}
		}

		if(!cap->max_x || !cap->min_x){
			GetMinMax(cap->my_xarray,cap->my_yarray,cap->xcast,
				cap->ycast,cap->my_missing_x,&min,&max);

			if(!cap->max_x)
				cap->max_x =_NhlCreateGenArray(&max,NhlTFloat,
						sizeof(float),1,NULL,True);
			if(!cap->min_x)
				cap->min_x =_NhlCreateGenArray(&min,NhlTFloat,
						sizeof(float),1,NULL,True);

			if(!cap->max_x || !cap->min_x){
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}

		}

		return ret;
	}

	if(dim == YDIM){

		if(!cap->my_yarray){
			lret = MyArray(&cap->my_yarray,cap->yarray,&cap->yctxt);

			if(lret < NhlWARNING){
				NhlPError(ret,NhlEUNKNOWN,
					"%s:Unable to convert %s to floats",
							func,NhlNcaYArray);
				return lret;
			}
			ret = MIN(ret,lret);
		}

		if(!cap->my_missing_y && cap->missing_y){
			if(cap->missing_y->typeQ == floatQ)
				cap->my_missing_y = cap->missing_y;
			else{
				float	tfloat;

				from.size = sizeof(NhlGenArray);
				from.data.ptrval = cap->missing_y;
				to.size = sizeof(float);
				to.data.ptrval = &tfloat;

				lret = NhlConvertData(NhlDEFAULT_APP,
					NhlTVariable,NhlTFloat,&from,&to);
				if(lret < NhlWARNING)
					return NhlFATAL;
				ret = MIN(lret,ret);

				cap->my_missing_y = _NhlCreateGenArray(&tfloat,
					NhlTFloat,sizeof(float),1,NULL,True);

				if(!cap->my_missing_y)
					return NhlFATAL;
			}
		}

		if(!cap->max_y || !cap->min_y){
			GetMinMax(cap->my_yarray,cap->my_xarray,cap->ycast,
				cap->xcast,cap->my_missing_y,&min,&max);

			if(!cap->max_y)
				cap->max_y =_NhlCreateGenArray(&max,NhlTFloat,
						sizeof(float),1,NULL,True);
			if(!cap->min_y)
				cap->min_y =_NhlCreateGenArray(&min,NhlTFloat,
						sizeof(float),1,NULL,True);

			if(!cap->max_y || !cap->min_y){
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}

		}

		return ret;
	}

	return NhlFATAL;
}

/*
 * Function:	CvtCArrToCArrTabFlt
 *
 * Description:	This function is used to convert a Generic CoordArrays
 *		to a CoordArraysFloat object.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CvtCArrToCArrTabFlt
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			num_args
)
#else
(from,to,args,num_args)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			num_args;
#endif
{
	char				func[]="CvtCArrToCArrTabFlt";
	NhlErrorTypes			ret = NhlNOERROR;
	NhlCoordArraysLayer		carrl;
	NhlCoordArraysLayerPart		*cap;
	int				fltid;
	NhlCoordArrTableFloatLayer	fltl;
	NhlCoordArrTableFloatLayerPart	*fltlp;
	NrmValue			fval,tval;

	if(num_args != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Called w/wrong args",func);
		return NhlFATAL;
	}

	carrl = (NhlCoordArraysLayer)_NhlGetLayer(from->data.intval);
	if((carrl == NULL)||
			(carrl->base.layer_class != NhlcoordArraysClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called w/ improper \"from\" object",func);
		return NhlFATAL;
	}

	cap = &carrl->carr;

	ret = NhlALCreate(&fltid,"no.name",NhlcoordArrTableFloatClass,
							carrl->base.id,NULL,0);
	*(int*)to->data.ptrval = fltid;

	fltl = (NhlCoordArrTableFloatLayer)_NhlGetLayer(fltid);

	if((ret < NhlWARNING) || (fltl == NULL)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unable to convert Data",
									func);
		return NhlFATAL;
	}

	fltlp = &fltl->flt;

	ret = FlushObj(XDIM,cap);
	if(ret < NhlWARNING)
		return ret;

	ret = FlushObj(YDIM,cap);
	if(ret < NhlWARNING)
		return ret;

	ret = CreateFloatTable(NhlNcaXCast,NhlNcaYCast,func,cap->my_xarray,
		cap->my_yarray,cap->xcast,cap->ycast,&fltlp->xtable,
							&fltlp->xtable_lens);
	if(ret < NhlWARNING)
		return ret;

	ret = CreateFloatTable(NhlNcaYCast,NhlNcaXCast,func,cap->my_yarray,
		cap->my_xarray,cap->ycast,cap->xcast,&fltlp->ytable,
							&fltlp->ytable_lens);
	if(ret < NhlWARNING)
		return ret;

	if(cap->my_missing_x){
		fltlp->missing_x = *(float*)cap->my_missing_x->data;
		fltlp->missing_x_set = True;
	}
	else
		fltlp->missing_x_set = False;

	if(cap->my_missing_y){
		fltlp->missing_y = *(float*)cap->my_missing_y->data;
		fltlp->missing_y_set = True;
	}
	else
		fltlp->missing_y_set = False;

	if(cap->max_x->typeQ == floatQ)
		fltlp->max_x = *(float *)cap->max_x->data;
	else{
		fval.size = sizeof(NhlGenArray);
		fval.data.ptrval = cap->max_x;
		tval.size = sizeof(float);
		tval.data.ptrval = &fltlp->max_x;
		ret = _NhlReConvertData(genQ,floatQ,&fval,&tval);
		if(ret < NhlINFO){
		NhlPError(ret,NhlEUNKNOWN,"%s:Unable to convert %s to float",
							func,NhlNctXMaxV);
			return ret;
		}
	}

	if(cap->min_x->typeQ == floatQ)
		fltlp->min_x = *(float *)cap->min_x->data;
	else{
		fval.size = sizeof(NhlGenArray);
		fval.data.ptrval = cap->min_x;
		tval.size = sizeof(float);
		tval.data.ptrval = &fltlp->min_x;
		ret = _NhlReConvertData(genQ,floatQ,&fval,&tval);
		if(ret < NhlINFO){
		NhlPError(ret,NhlEUNKNOWN,"%s:Unable to convert %s to float",
							func,NhlNctXMinV);
			return ret;
		}
	}

	if(cap->max_y->typeQ == floatQ)
		fltlp->max_y = *(float *)cap->max_y->data;
	else{
		fval.size = sizeof(NhlGenArray);
		fval.data.ptrval = cap->max_y;
		tval.size = sizeof(float);
		tval.data.ptrval = &fltlp->max_y;
		ret = _NhlReConvertData(genQ,floatQ,&fval,&tval);
		if(ret < NhlINFO){
		NhlPError(ret,NhlEUNKNOWN,"%s:Unable to convert %s to float",
							func,NhlNctYMaxV);
			return ret;
		}
	}

	if(cap->min_y->typeQ == floatQ)
		fltlp->min_y = *(float *)cap->min_y->data;
	else{
		fval.size = sizeof(NhlGenArray);
		fval.data.ptrval = cap->min_y;
		tval.size = sizeof(float);
		tval.data.ptrval = &fltlp->min_y;
		ret = _NhlReConvertData(genQ,floatQ,&fval,&tval);
		if(ret < NhlINFO){
		NhlPError(ret,NhlEUNKNOWN,"%s:Unable to convert %s to float",
							func,NhlNctYMinV);
			return ret;
		 }
	 }
	return ret;
}

/************************************************************************
*									*
*	Methode definitions						*
*									*
************************************************************************/


/*
 * Function:	CoordArraysClassInitialize
 *
 * Description:	This function does one time initialization needed by the
 *		CoordArraysClass.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CoordArraysClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret,lret;
	_NhlEnumVals	cast_mode[] = {
		{NhlSINGLEVECTOR,	"SingleVector"},
		{NhlMULTIPLEVECTORS,	"MultipleVectors"},
		{NhlSPLITVECTORS,	"SplitVectors"}
	};


	floatQ = NrmStringToQuark(NhlTFloat);
	genQ = NrmStringToQuark(NhlTGenArray);
	floatgenQ = NrmStringToQuark(NhlTFloatGenArray);

	xarrQ = NrmStringToQuark(NhlNcaXArray);
	yarrQ = NrmStringToQuark(NhlNcaYArray);

	xmissQ = NrmStringToQuark(NhlNcaXMissingV);
	ymissQ = NrmStringToQuark(NhlNcaYMissingV);

	xmaxQ = NrmStringToQuark(NhlNcaXMaxV);
	ymaxQ = NrmStringToQuark(NhlNcaYMaxV);
	xminQ = NrmStringToQuark(NhlNcaXMinV);
	yminQ = NrmStringToQuark(NhlNcaYMinV);

	lret = _NhlRegisterEnumType(NhlcoordArraysClass,NhlTcaCastMode,
			cast_mode,NhlNumber(cast_mode));

	ret = NhlRegisterConverter(NhlbaseClass,
			NhlcoordArraysClass->base_class.class_name,
			NhlcoordArrTableFloatClass->base_class.class_name,
			CvtCArrToCArrTabFlt,NULL,0,False,NULL);

	return MIN(ret,lret);
}

static NhlErrorTypes
CheckArray
#if	NhlNeedProto
(
	NhlGenArray	arr,
	NhlcaCastMode	*cast,
	NhlBoolean	*cast_set,
	NhlString	arr_res,
	NhlString	cast_res,
	NhlBoolean	*imp
)
#else
(arr,cast,cast_set,arr_res,cast_res,imp)
	NhlGenArray	arr;
	NhlcaCastMode	*cast;
	NhlBoolean	*cast_set;
	NhlString	arr_res;
	NhlString	cast_res;
	NhlBoolean	*imp;
#endif
{
	char		func[]="CheckArray";
	NhlErrorTypes	ret = NhlNOERROR;
	int		num_elements;

	if(arr != NULL){
		if((arr->num_dimensions > 2) ||
					(arr->num_dimensions < 1)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s must be a one or two dimensional array:ignoring",
							func,arr_res);
			
			return NhlFATAL;
		}
	}
	else
		*imp = True;

	if(*cast_set){
		if((*imp) && (*cast != NhlSINGLEVECTOR)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be %d if %s is implied",func,
				cast_res,NhlSINGLEVECTOR,arr_res);
			ret = MIN(ret,NhlWARNING);
			*cast = NhlSINGLEVECTOR;
		}
	}
	else{
		*cast_set = True;
		if((*imp) || (arr->num_dimensions == 1))
			*cast = NhlSINGLEVECTOR;
		else
			*cast = NhlMULTIPLEVECTORS;
	}


	if(!*imp){
		if((arr->num_dimensions == 2) &&
				(*cast == NhlMULTIPLEVECTORS)){
			num_elements = arr->len_dimensions[1];
		}
		else{
			num_elements = arr->len_dimensions[0];
		}

		if(num_elements < 2){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
	"%s:Each vector in the %s array must have at least 2 elements",
							func,arr_res);
			return NhlFATAL;
		}

	}

	return ret;
}

/*
 * Function:	CopyArray
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
CopyArray
#if	NhlNeedProto
(
	NhlGenArray		*arr,
	NhlGenArray		*my_arr,
	NhlBoolean		copy,
	_NhlConvertContext	*ctxt
)
#else
(arr,my_arr,copy,ctxt)
	NhlGenArray		*arr;
	NhlGenArray		*my_arr;
	NhlBoolean		copy;
	_NhlConvertContext	*ctxt;
#endif
{
	NhlErrorTypes	ret;

	if(!*arr){
		*my_arr = NULL;
		return NhlNOERROR;
	}

	if(((*arr)->typeQ != floatQ) && copy){
		ret = MyArray(my_arr,*arr,ctxt);
		if(ret < NhlWARNING)
			return ret;
		*arr = *my_arr;

		return ret;
	}

	*my_arr = *arr = _NhlCopyGenArray(*arr,copy);

	if(*arr == NULL)
		return NhlFATAL;

	return NhlNOERROR;
}

/*
 * Function:	CoordArraysInitialize
 *
 * Description:	This function initializes an instance of a CoordArrays
 *		class object.
 *
 * In Args:	
 *	NhlClass	lc,	class
 *	NhlLayer	req,	requested
 *	NhlLayer	new,	new
 *	_NhlArgList	args,	args
 *	int		nargs	nargs
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CoordArraysInitialize
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
	char			*func = "CoordArraysInitialize";
	NhlCoordArraysLayer	ncarr = (NhlCoordArraysLayer)new;
	NhlCoordArraysLayerPart	*ncap = &ncarr->carr;
	NhlErrorTypes		ret=NhlNOERROR,lret=NhlNOERROR;
	NhlBoolean		impy = False, impx = False;

	ret = CheckArray(ncap->xarray,&ncap->xcast,&ncap->xcast_set,
						NhlNcaXArray,NhlNcaXCast,&impx);
	if(ret < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:%s resource is invalid",func,
								NhlNcaXArray);
		return ret;
	}

	lret = CheckArray(ncap->yarray,&ncap->ycast,&ncap->ycast_set,
						NhlNcaYArray,NhlNcaYCast,&impy);
	if(lret < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:%s resource is invalid",func,
								NhlNcaYArray);
		return lret;
	}
	ret = MIN(ret,lret);

	if(impx && impy){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s:Cannot have Implied X and Y values",func);
		return NhlFATAL;
	}

	lret = CopyArray(&ncap->xarray,&ncap->my_xarray,ncap->copy_arrays,
								&ncap->xctxt);
	if(lret < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:memory???",func);
		return lret;
	}
	ret = MIN(ret,lret);

	lret = CopyArray(&ncap->yarray,&ncap->my_yarray,ncap->copy_arrays,
								&ncap->yctxt);
	if(lret < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:memory???",func);
		return lret;
	}
	ret = MIN(ret,lret);

	if(ncap->missing_x){
		ncap->missing_x = _NhlCopyGenArray(ncap->missing_x,True);
		if(!ncap->missing_x){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
	}
	if(ncap->missing_y){
		ncap->missing_y = _NhlCopyGenArray(ncap->missing_y,True);
		if(!ncap->missing_y){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
	}

	if(ncap->max_x){
		ncap->max_x = _NhlCopyGenArray(ncap->max_x,True);
		if(!ncap->max_x){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		ncap->sticky_max_x = True;
	}
	else
		ncap->sticky_max_x = False;
	if(ncap->max_y){
		ncap->max_y = _NhlCopyGenArray(ncap->max_y,True);
		if(!ncap->max_y){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		ncap->sticky_max_y = True;
	}
	else
		ncap->sticky_max_y = False;
	if(ncap->min_x){
		ncap->min_x = _NhlCopyGenArray(ncap->min_x,True);
		if(!ncap->min_x){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		ncap->sticky_min_x = True;
	}
	else
		ncap->sticky_min_x = False;
	if(ncap->min_y){
		ncap->min_y = _NhlCopyGenArray(ncap->min_y,True);
		if(!ncap->min_y){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		ncap->sticky_min_y = True;
	}
	else
		ncap->sticky_min_y = False;

	return ret;
}

/*
 * Function:	CoordArraysSetValues
 *
 * Description:	...
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CoordArraysSetValues
#if	NhlNeedProto
(
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
)
#else
(old,req,new,args,nargs)
	NhlLayer	old;		/* old		*/
	NhlLayer	req;		/* requested	*/
	NhlLayer	new;		/* new		*/
	_NhlArgList	args;		/* args to set	*/
	int		nargs;		/* nargs	*/
#endif
{
	char			func[] = "CoordArraysSetValues";
	NhlErrorTypes		ret = NhlNOERROR,lret = NhlNOERROR;
	NhlCoordArraysLayer	ncarr = (NhlCoordArraysLayer)new;
	NhlCoordArraysLayer	ocarr = (NhlCoordArraysLayer)old;
	NhlCoordArraysLayerPart	*ncap = &ncarr->carr;
	NhlCoordArraysLayerPart	*ocap = &ocarr->carr;
	NhlBoolean		impx = False, impy = False;
	NhlBoolean		status = False;
	NhlBoolean		update_minmax = False;


	if(ncap->xarray != ocap->xarray){

		ret = CheckArray(ncap->xarray,&ncap->xcast,&ncap->xcast_set,
						NhlNcaXArray,NhlNcaXCast,&impx);

		if(ret < NhlWARNING){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:invalid %s resource: resetting",func,
								NhlNcaXArray);
			ncap->xarray = ocap->xarray;
			ret = NhlWARNING;
		}
	}
	if(ncap->xarray == NULL)
		impx = True;

	if(ncap->yarray != ocap->yarray){

		lret = CheckArray(ncap->yarray,&ncap->ycast,&ncap->ycast_set,
						NhlNcaYArray,NhlNcaYCast,&impy);

		if(lret < NhlWARNING){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:invalid %s resource: resetting",func,
								NhlNcaYArray);
			ncap->yarray = ocap->yarray;
			lret = NhlWARNING;
		}
	}
	ret = MIN(ret,lret);
	if(ncap->yarray == NULL)
		impy = True;

	if(impx && impy){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:Cannot have Implied X and Y values:resetting",func);
		ret = MIN(ret,NhlWARNING);
		ncap->xarray = ocap->xarray;
		ncap->yarray = ocap->yarray;
	}

	if((ncap->xarray != ocap->xarray) ||
		(ncap->xarray && ncap->copy_arrays && !ncap->xarray->my_data)){
		lret = CopyArray(&ncap->xarray,&ncap->my_xarray,
						ncap->copy_arrays,&ncap->xctxt);
		if(lret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:memory??? - resetting %s",func,NhlNcaXArray);
			ncap->xarray = ocap->xarray;
			ret = MIN(ret,lret);
		}
		else{
			status = True;
			if(!ocap->xctxt)
				NhlFreeGenArray(ocap->xarray);
		}
	}
	if((ncap->yarray != ocap->yarray) ||
		(ncap->yarray && ncap->copy_arrays && !ncap->yarray->my_data)){
		lret = CopyArray(&ncap->yarray,&ncap->my_yarray,
						ncap->copy_arrays,&ncap->yctxt);
		if(lret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:memory??? - resetting %s",func,NhlNcaYArray);
			ncap->yarray = ocap->yarray;
			ret = MIN(ret,lret);
		}
		else{
			status = True;
			if(!ocap->yctxt)
				NhlFreeGenArray(ocap->yarray);
		}
	}

	if(ncap->xcast != ocap->xcast)
		status = True;
	if(ncap->ycast != ocap->ycast)
		status = True;

	if(ncap->missing_x != ocap->missing_x){
		ncap->missing_x = _NhlCopyGenArray(ncap->missing_x,True);
		if(!ncap->missing_x){
			NhlPError(NhlWARNING,ENOMEM,"%s:resetting %s",func,
							NhlNcaXMissingV);
			ncap->missing_x = ocap->missing_x;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			status = True;
			ncap->my_missing_x = NULL;
			if(ocap->missing_x == ocap->my_missing_x)
				ocap->my_missing_x = NULL;
			NhlFreeGenArray(ocap->missing_x);
			NhlFreeGenArray(ocap->my_missing_x);
		}
	}
	if(ncap->missing_y != ocap->missing_y){
		ncap->missing_y = _NhlCopyGenArray(ncap->missing_y,True);
		if(!ncap->missing_y){
			NhlPError(NhlWARNING,ENOMEM,"%s:resetting %s",func,
							NhlNcaYMissingV);
			ncap->missing_y = ocap->missing_y;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			status = True;
			ncap->my_missing_y = NULL;
			if(ocap->missing_y == ocap->my_missing_y)
				ocap->my_missing_y = NULL;
			NhlFreeGenArray(ocap->missing_y);
			NhlFreeGenArray(ocap->my_missing_y);
		}
	}

	update_minmax = status;

	if(ncap->max_x != ocap->max_x){
		ncap->max_x = _NhlCopyGenArray(ncap->max_x,True);
		if(!ncap->max_x){
			NhlPError(NhlWARNING,ENOMEM,"%s:resetting %s",func,
								NhlNcaXMaxV);
			ncap->max_x = ocap->max_x;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			NhlFreeGenArray(ocap->max_x);
			ncap->sticky_max_x = True;
			status = True;
		}
	}
	else if(update_minmax && !ncap->sticky_max_x){
		NhlFreeGenArray(ncap->max_x);
		ncap->max_x = NULL;
	}

	if(ncap->max_y != ocap->max_y){
		ncap->max_y = _NhlCopyGenArray(ncap->max_y,True);
		if(!ncap->max_y){
			NhlPError(NhlWARNING,ENOMEM,"%s:resetting %s",func,
								NhlNcaYMaxV);
			ncap->max_y = ocap->max_y;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			NhlFreeGenArray(ocap->max_y);
			ncap->sticky_max_y = True;
			status = True;
		}
	}
	else if(update_minmax && !ncap->sticky_max_y){
		NhlFreeGenArray(ncap->max_y);
		ncap->max_y = NULL;
	}

	if(ncap->min_x != ocap->min_x){
		ncap->min_x = _NhlCopyGenArray(ncap->min_x,True);
		if(!ncap->min_x){
			NhlPError(NhlWARNING,ENOMEM,"%s:resetting %s",func,
								NhlNcaXMinV);
			ncap->min_x = ocap->min_x;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			NhlFreeGenArray(ocap->min_x);
			ncap->sticky_min_x = True;
			status = True;
		}
	}
	else if(update_minmax && !ncap->sticky_min_x){
		NhlFreeGenArray(ncap->min_x);
		ncap->min_x = NULL;
	}

	if(ncap->min_y != ocap->min_y){
		ncap->min_y = _NhlCopyGenArray(ncap->min_y,True);
		if(!ncap->min_y){
			NhlPError(NhlWARNING,ENOMEM,"%s:resetting %s",func,
								NhlNcaYMinV);
			ncap->min_y = ocap->min_y;
			ret = MIN(ret,NhlWARNING);
		}
		else{
			NhlFreeGenArray(ocap->min_y);
			ncap->sticky_min_y = True;
			status = True;
		}
	}
	else if(update_minmax && !ncap->sticky_min_y){
		NhlFreeGenArray(ncap->min_y);
		ncap->min_y = NULL;
	}

	_NhlDataChanged((NhlDataItemLayer)new,status);

	return	ret;
}

/*
 * Function:	CoordArraysGetValues
 *
 * Description:	...
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CoordArraysGetValues
#if	NhlNeedProto
(
	NhlLayer	l,		/* l		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
)
#else
(l,args,nargs)
	NhlLayer	l;		/* l		*/
	_NhlArgList	args;		/* args to set	*/
	int		nargs;		/* nargs	*/
#endif
{
	char			func[] = "CoordArraysGetValues";
	NhlErrorTypes		ret = NhlNOERROR;
	int			i;
	NhlCoordArraysLayer	ca = (NhlCoordArraysLayer)l;
	NhlCoordArraysLayerPart	*cap = &ca->carr;

	for(i=0;i < nargs;i++){

		if((args[i].quark == xarrQ) && cap->xarray){
			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(cap->xarray,True);
			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNcaXArray);
				ret = MIN(ret,NhlWARNING);
			}
		}

		else if((args[i].quark == yarrQ) && cap->yarray){
			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(cap->yarray,True);
			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNcaYArray);
				ret = MIN(ret,NhlWARNING);
			}
		}

		else if((args[i].quark == xmissQ) && cap->missing_x){
			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(cap->missing_x,True);
			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNcaXMissingV);
				ret = MIN(ret,NhlWARNING);
			}
		}

		else if((args[i].quark == ymissQ) && cap->missing_y){
			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(cap->missing_y,True);
			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNcaYMissingV);
				ret = MIN(ret,NhlWARNING);
			}
		}

		else if(args[i].quark == xmaxQ){

			if(!cap->max_x){
				if(FlushObj(XDIM,cap) < NhlWARNING){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						"%s:Unable to determine %s",
							func,NhlNcaXMaxV);
					ret = MIN(ret,NhlWARNING);
					continue;
				}
			}

			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(cap->max_x,True);
			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNcaXMaxV);
				ret = MIN(ret,NhlWARNING);
			}
		}

		else if(args[i].quark == ymaxQ){

			if(!cap->max_y){
				if(FlushObj(YDIM,cap) < NhlWARNING){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						"%s:Unable to determine %s",
							func,NhlNcaYMaxV);
					ret = MIN(ret,NhlWARNING);
					continue;
				}
			}

			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(cap->max_y,True);
			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNcaYMaxV);
				ret = MIN(ret,NhlWARNING);
			}
		}

		else if(args[i].quark == xminQ){

			if(!cap->min_x){
				if(FlushObj(XDIM,cap) < NhlWARNING){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						"%s:Unable to determine %s",
							func,NhlNcaXMinV);
					ret = MIN(ret,NhlWARNING);
					continue;
				}
			}

			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(cap->min_x,True);
			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNcaXMinV);
				ret = MIN(ret,NhlWARNING);
			}
		}

		else if(args[i].quark == yminQ){

			if(!cap->min_y){
				if(FlushObj(YDIM,cap) < NhlWARNING){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						"%s:Unable to determine %s",
							func,NhlNcaYMinV);
					ret = MIN(ret,NhlWARNING);
					continue;
				}
			}

			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(cap->min_y,True);
			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNcaYMinV);
				ret = MIN(ret,NhlWARNING);
			}
		}
	}

	return ret;
}

/*
 * Function:	CoordArraysDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlCoordArraysClass.
 *
 * In Args:	NhlLayer	l
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CoordArraysDestroy
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlCoordArraysLayer	cl = (NhlCoordArraysLayer)l;
	NhlCoordArraysLayerPart	*cap = &cl->carr;

	if(cap->my_xarray != cap->xarray)
		NhlFreeGenArray(cap->xarray);
	if(cap->my_yarray != cap->yarray)
		NhlFreeGenArray(cap->yarray);

	if(cap->xctxt)
		_NhlFreeConvertContext(cap->xctxt);
	else
		NhlFreeGenArray(cap->my_xarray);

	if(cap->yctxt)
		_NhlFreeConvertContext(cap->yctxt);
	else
		NhlFreeGenArray(cap->my_yarray);

	if (cap->my_missing_x == cap->missing_x) 
		cap->my_missing_x = NULL;
	if (cap->my_missing_y == cap->missing_y) 
		cap->my_missing_y = NULL;

	NhlFreeGenArray(cap->my_missing_x);
	NhlFreeGenArray(cap->my_missing_y);

	NhlFreeGenArray(cap->missing_x);
	NhlFreeGenArray(cap->missing_y);

	NhlFreeGenArray(cap->max_x);
	NhlFreeGenArray(cap->max_y);
	NhlFreeGenArray(cap->min_x);
	NhlFreeGenArray(cap->min_y);

	return NhlNOERROR;
}

/************************************************************************
*									*
*	Private API for sub-classes					*
*									*
************************************************************************/

/* none yet */

/************************************************************************
*									*
*	Public API							*
*									*
************************************************************************/

/* none yet */
