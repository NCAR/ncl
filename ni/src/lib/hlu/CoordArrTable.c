/*
 *      $Id: CoordArrTable.c,v 1.33 1997-07-25 21:11:51 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArrTable.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 11:23:48 MDT 1993
 *
 *	Description:	This class is used to communicate data in the format
 *			of CoordArrTable.
 */
#include <stdio.h> 
#include <string.h> 
#include <ncarg/hlu/CoordArrTableP.h> 

/************************************************************************
*									*
*	CoordArrTable Class declarations				*
*									*
************************************************************************/

/*
 * Resource Declarations
 */
#define	Oset(field)	NhlOffset(NhlCoordArrTableLayerRec,cat.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNctXTableType,NhlCctXTableType,NhlTString,sizeof(NhlString),
		 Oset(xtype),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
							(NhlFreeFunc)NhlFree},
	{NhlNctXElementSize,NhlCctXElementSize,NhlTInteger,sizeof(int),
		 Oset(xsize),NhlTImmediate,_NhlUSET((NhlPointer)0),0,(NhlFreeFunc)NULL},
	{NhlNctYTableType,NhlCctYTableType,NhlTString,sizeof(NhlString),
		 Oset(ytype),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
							(NhlFreeFunc)NhlFree},
	{NhlNctYElementSize,NhlCctYElementSize,NhlTInteger,sizeof(int),
		 Oset(ysize),NhlTImmediate,_NhlUSET((NhlPointer)0),0,(NhlFreeFunc)NULL},
	{NhlNctXTable,NhlCctXTable,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(xtable),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
		(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctYTable,NhlCctYTable,NhlTGenArray,sizeof(NhlGenArray),
	 	Oset(ytable),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
	 	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctXTableLengths,NhlCctXTableLengths,NhlTIntegerGenArray,
		sizeof(NhlGenArray),
		 Oset(xtable_lens),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
	 	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctYTableLengths,NhlCctYTableLengths,NhlTIntegerGenArray,
		sizeof(NhlGenArray),
		 Oset(ytable_lens),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
	 	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctCopyTables,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(copy_tables),NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNctXMissingV,NhlCdiMissingValue,NhlTVariable,sizeof(NhlGenArray),
		 Oset(missing_x),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
	 	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctYMissingV,NhlCdiMissingValue,NhlTVariable,sizeof(NhlGenArray),
		 Oset(missing_y),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
	 	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctXMaxV,NhlCctXMaxV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(max_x),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
	 	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctYMaxV,NhlCctYMaxV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(max_y),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
	 	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctXMinV,NhlCctXMinV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(min_x),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
	 	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctYMinV,NhlCctYMinV,NhlTVariable,sizeof(NhlGenArray),
		Oset(min_y),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},

/* End-documented-resources */

	/* use reslist to init private fields */
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(own_x),
		NhlTImmediate,_NhlUSET((NhlPointer)NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(own_y),
		NhlTImmediate,_NhlUSET((NhlPointer)NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(conv_x),
		NhlTImmediate,_NhlUSET((NhlPointer)NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(conv_y),
		NhlTImmediate,_NhlUSET((NhlPointer)NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(own_miss_x),
		NhlTImmediate,_NhlUSET((NhlPointer)NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(own_miss_y),
		NhlTImmediate,_NhlUSET((NhlPointer)NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(sticky_max_x),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(sticky_min_x),
	 	NhlTImmediate,_NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(sticky_max_y),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTPointer,sizeof(NhlPointer),Oset(sticky_min_y),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL}
};
#undef Oset

/* base methods */

static NhlErrorTypes CoordArrTableClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes CoordArrTableInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableGetValues(
#if	NhlNeedProto
	NhlLayer	l,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

NhlCoordArrTableFloatClassRec NhlcoordArrTableFloatClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"coordArrTableFloatClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCoordArrTableFloatLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlobjClassRec,
/* cvt_table			*/	NULL,

/* resources			*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	},
	/* NhlCoordArrTableFloatLayerPart */
	{
/* foo				*/	0
	}
};

NhlCoordArrTableClassRec NhlcoordArrTableClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"coordArrTableClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCoordArrTableLayerRec),
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
/* class_initialize		*/	CoordArrTableClassInitialize,
/* layer_initialize		*/	CoordArrTableInitialize,
/* layer_set_values		*/	CoordArrTableSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	CoordArrTableGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	CoordArrTableDestroy,

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
	/* NhlCoordArrTableClassPart */
	{
/* foo				*/	0
	}
};
	
NhlClass NhlcoordArrTableClass =
				(NhlClass)&NhlcoordArrTableClassRec;
NhlClass NhlcoordArrTableFloatClass = 
			(NhlClass)&NhlcoordArrTableFloatClassRec;

static	NrmQuark	genQ = NrmNULLQUARK;
static	NrmQuark	floatgenQ = NrmNULLQUARK;

static	NrmQuark	byteQ = NrmNULLQUARK;
static	NrmQuark	charQ = NrmNULLQUARK;
static	NrmQuark	doubleQ = NrmNULLQUARK;
static	NrmQuark	floatQ = NrmNULLQUARK;
static	NrmQuark	intQ = NrmNULLQUARK;
static	NrmQuark	longQ = NrmNULLQUARK;
static	NrmQuark	shortQ = NrmNULLQUARK;

static	NrmQuark	xttypeQ = NrmNULLQUARK;
static	NrmQuark	yttypeQ = NrmNULLQUARK;
static	NrmQuark	xtableQ = NrmNULLQUARK;
static	NrmQuark	ytableQ = NrmNULLQUARK;
static	NrmQuark	xtablelensQ = NrmNULLQUARK;
static	NrmQuark	ytablelensQ = NrmNULLQUARK;
static	NrmQuark	missxQ = NrmNULLQUARK;
static	NrmQuark	missyQ = NrmNULLQUARK;
static	NrmQuark	maxxQ = NrmNULLQUARK;
static	NrmQuark	maxyQ = NrmNULLQUARK;
static	NrmQuark	minxQ = NrmNULLQUARK;
static	NrmQuark	minyQ = NrmNULLQUARK;

typedef	enum _NhlDType_{
	XDIM,
	YDIM
} _NhlDType;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

/*
 * Function:	CvtToFltPtrs
 *
 * Description:	Utility function that takes a GenArray of pointers to the
 *		given type.  And converts then to a GenArray of pointers
 *		to floats.
 *
 * In Args:	If context is NULL, then this function is being called
 *		from within a converter function.  So it should use the
 *		memory allocation functions that use that context.  If
 *		context is set, then this function is being called from
 *		Initailize, or SetValues and the memory should be allocated
 *		normally - for direct allocations.  And for conversions,
 *		the context passed in, should be used.  The context will
 *		be holding the memory that is pointed to by the pointers
 *		in the table genarray.  So eventually, the genarray should
 *		be free'd normally - and then the context free'd using
 *		the _NhlFreeConvertContext function.
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
CvtToFltPtrs
#if	NhlNeedProto
(
	NhlGenArray		*to_table,
	NrmQuark		from_type,
	int			from_size,
	NhlGenArray		from_table,
	NhlGenArray		from_table_lens,
	_NhlConvertContext	*context
)
#else
(to_table,from_type,from_size,from_table,from_table_lens,context)
	NhlGenArray		*to_table;
	NrmQuark		from_type;
	int			from_size;
	NhlGenArray		from_table;
	NhlGenArray		from_table_lens;
	_NhlConvertContext	*context;
#endif
{
	char			func[] = "CvtToFltPtrs";
	NrmValue		from,to;
	NhlGenArrayRec		tgen;
	NhlGenArray		new;
	NhlPointer		*from_ptrarr,*to_ptrarr;
	int			*intarr;
	int			i;
	char			from_name[_NhlMAXRESNAMLEN];
	NrmQuark		fromQ;
	NhlErrorTypes		ret;
	_NhlConvertContext	tcontext;

	if(!from_table){
		*to_table = NULL;
		return NhlNOERROR;
	}

	*to_table = _NhlCopyGenArray(from_table,True);
	if(*to_table == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	to_ptrarr = (*to_table)->data;

	strcpy(from_name,NrmQuarkToString(from_type));
	strcat(from_name,NhlTGenArray);
	fromQ = NrmStringToQuark(from_name);

	tgen.num_dimensions = 1;
	tgen.len_dimensions = &tgen.num_elements;
	tgen.typeQ = from_type;
	tgen.size = (unsigned int)from_size;
	tgen.my_data = False;

	from_ptrarr = from_table->data;
	intarr = from_table_lens->data;

	from.size = sizeof(NhlGenArray);
	from.data.ptrval = &tgen;
	to.size = sizeof(NhlGenArray);
	to.data.ptrval = &new;

	tcontext = _NhlCreateConvertContext(NULL);
	if(tcontext == NULL){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}

	for(i=0;i < from_table_lens->num_elements;i++){
		tgen.num_elements = intarr[i];
		tgen.data = from_ptrarr[i];

		if(fromQ == floatgenQ){
			to_ptrarr[i] =
				_NhlCvtCtxtMalloc(tgen.num_elements*tgen.size,
								tcontext);
			if(to_ptrarr[i] == NULL)
				NhlPError(NhlFATAL,ENOMEM,"%s",func);

			memcpy(to_ptrarr[i],tgen.data,
						tgen.num_elements*tgen.size);
		}
		else{
			ret=_NhlConvertData(tcontext,fromQ,floatgenQ,&from,&to);

			if(ret < NhlWARNING){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to convert from %s to %s",
						func,NrmQuarkToString(fromQ),
						NrmQuarkToString(floatgenQ));
				return NhlFATAL;
			}

			to_ptrarr[i] = new->data;
		}
	}

	_NhlFreeConvertContext(*context);
	*context = tcontext;

	return NhlNOERROR;
}

static void
GetMinMax
#if	NhlNeedProto
(
	NhlGenArray	table,
	NhlGenArray	table_lens,
	NhlGenArray	otable_lens,
	NhlGenArray	miss,
	float		*min,
	float		*max
)
#else
(table,table_lens,otable_lens,miss,min,max)
	NhlGenArray	table;
	NhlGenArray	table_lens;
	NhlGenArray	otable_lens;
	NhlGenArray	miss;
	float		*min;
	float		*max;
#endif
{
	int	*lens;
	float	mx,mn;
	int	i,j;
	float	**ftbl,*fvct;

	if(table == NULL){
		/*
		 * table is *implied* - use indexes of other dim's
		 * table_lens as data.
		 */
		mx = mn = 1.0;
		lens = otable_lens->data;
		for(i=0;i < otable_lens->num_elements;i++)
			mx = MAX(mx,lens[i]);
	}
	else{
		NhlBoolean	init = False;

		mx = mn = 0.0;
		ftbl = table->data;
		lens = table_lens->data;

		for(i=0;i < table->num_elements;i++){

			if(ftbl[i]){
				fvct = ftbl[i];

				for(j=0;j < lens[i];j++){
					/*
					 * Skip missing value - if exists
					 */
					if(miss &&
						(fvct[j]==*(float*)miss->data))
						continue;

					if(init){
						mx = MAX(fvct[j],mx);
						mn = MIN(fvct[j],mn);
					}
					else{
						mx = fvct[j];
						mn = fvct[j];
						init = True;
					}
				}
			}
			else{
				if(init){
					mn = MIN(lens[i],mn);
					mx = MAX(lens[i],mx);
				}
				else{
					mn = 1.0;
					mx = MAX(lens[i],mn);
					init = True;
				}
			}
		}
	}

	*min = mn;
	*max = mx;
	return;
}

static NhlErrorTypes
GetOwnMiss
#if	NhlNeedProto
(
	NhlGenArray	*own,
	NhlGenArray	miss
)
#else
(own,miss)
	NhlGenArray	*own;
	NhlGenArray	miss;
#endif
{
	NrmValue	to,from;
	float		value;
	NhlErrorTypes	ret;

	if((miss == NULL) || (miss->typeQ == floatQ)){
		*own = miss;
		return NhlNOERROR;
	}

	from.size = sizeof(NhlGenArray);
	from.data.ptrval = miss;
	to.size = sizeof(float);
	to.data.ptrval = &value;

	ret = NhlConvertData(NhlDEFAULT_APP,NhlTGenArray,NhlTFloat,&from,&to);

	if(ret < NhlWARNING)
		return NhlFATAL;

	*own = _NhlCreateGenArray(&value,NhlTFloat,sizeof(float),1,NULL,True);

	if(*own == NULL)
		return NhlFATAL;

	return MIN(NhlNOERROR,ret);
}

/*
 * Function:	FlushObject
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
FlushObject
#if	NhlNeedProto
(
	_NhlDType			dim,
	NhlCoordArrTableLayerPart	*clp
)
#else
(dim,clp)
	_NhlDType			dim;
	NhlCoordArrTableLayerPart	*clp;
#endif
{
	char		func[] = "FlushObject";
	float		max,min;
	NhlErrorTypes	ret=NhlNOERROR,lret;

	if(dim == XDIM){

		if(!clp->own_x){
			ret = CvtToFltPtrs(&clp->own_x,clp->xtypeQ,clp->xsize,
				clp->xtable,clp->xtable_lens,&clp->conv_x);
			if(ret < NhlWARNING){
				NhlPError(ret,NhlEUNKNOWN,
					"%s:Unable to convert %s to floats",
							func,NhlNctXTable);
				return ret;
			}
		}

		lret = GetOwnMiss(&clp->own_miss_x,clp->missing_x);
		if(lret < NhlWARNING){
			NhlPError(lret,NhlEUNKNOWN,
			"%s:Unable to convert %s to float for %s resource",
				NrmQuarkToString(clp->missing_x->typeQ),
							NhlNctXMissingV);
			return lret;
		}
		ret = MIN(ret,lret);

		if(!clp->max_x || !clp->min_x){
			GetMinMax(clp->own_x,clp->xtable_lens,clp->ytable_lens,
						clp->own_miss_x,&min,&max);

			if(!clp->max_x)
				clp->max_x =_NhlCreateGenArray(&max,NhlTFloat,
						sizeof(float),1,NULL,True);
			if(!clp->min_x)
				clp->min_x =_NhlCreateGenArray(&min,NhlTFloat,
						sizeof(float),1,NULL,True);

			if(!clp->max_x || !clp->min_x){
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}

		}

		return ret;
	}

	if(dim == YDIM){

		if(!clp->own_y){
			ret = CvtToFltPtrs(&clp->own_y,clp->ytypeQ,clp->ysize,
				clp->ytable,clp->ytable_lens,&clp->conv_y);

			if(ret < NhlWARNING){
				NhlPError(ret,NhlEUNKNOWN,
					"%s:Unable to convert %s to floats",
							func,NhlNctYTable);
				return ret;
			}
		}

		lret = GetOwnMiss(&clp->own_miss_y,clp->missing_y);
		if(lret < NhlWARNING){
			NhlPError(lret,NhlEUNKNOWN,
			"%s:Unable to convert %s to float for %s resource",
				NrmQuarkToString(clp->missing_y->typeQ),
							NhlNctYMissingV);
			return lret;
		}
		ret = MIN(ret,lret);

		if(!clp->max_y || !clp->min_y){
			GetMinMax(clp->own_y,clp->ytable_lens,clp->xtable_lens,
						clp->own_miss_y,&min,&max);

			if(!clp->max_y)
				clp->max_y =_NhlCreateGenArray(&max,NhlTFloat,
						sizeof(float),1,NULL,True);
			if(!clp->min_y)
				clp->min_y =_NhlCreateGenArray(&min,NhlTFloat,
						sizeof(float),1,NULL,True);

			if(!clp->max_y || !clp->min_y){
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}

		}

		return ret;
	}

	return NhlFATAL;
}

/*
 * Function:	CvtGenObjToFloatObj
 *
 * Description:	This function is used to convert a Generic CoordArrTable
 *		to a CoordArrTableFloat object.
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
CvtGenObjToFloatObj
#if	NhlNeedProto
(
	NrmValue		*from_val,
	NrmValue		*to_val,
	NhlConvertArgList	args,
	int			num_args
)
#else
(from_val,to_val,args,num_args)
	NrmValue		*from_val;
	NrmValue		*to_val;
	NhlConvertArgList	args;
	int			num_args;
#endif
{
	NhlCoordArrTableLayer		catl = NULL;
	NhlCoordArrTableLayerPart	*lpart;
	int				fltid;
	NhlCoordArrTableFloatLayer	fltl;
	NhlCoordArrTableFloatLayerPart	*fltlpart;
	NrmValue			from,to;
	char				func[] = "CvtGenObjToFloatObj";
	NhlErrorTypes			ret = NhlNOERROR;

	if(num_args != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Called w/wrong args",func);
		return NhlFATAL;
	}

	catl = (NhlCoordArrTableLayer)_NhlGetLayer(from_val->data.intval);
	if((catl==NULL)||(catl->base.layer_class!=NhlcoordArrTableClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called w/ improper \"from\" object",func);
		return NhlFATAL;
	}

	lpart = &catl->cat;

	ret = NhlALCreate(&fltid,"no.name",NhlcoordArrTableFloatClass,
							catl->base.id,NULL,0);

	*(int *)to_val->data.ptrval = fltid;
	fltl = (NhlCoordArrTableFloatLayer)_NhlGetLayer(fltid);

	if((ret < NhlWARNING) || (fltl == NULL))
		return ret;

	fltlpart = &fltl->flt;
	
	ret = FlushObject(XDIM,lpart);
	if(ret < NhlWARNING){
		return ret;
	};
	ret = FlushObject(YDIM,lpart);
	if(ret < NhlWARNING){
		return ret;
	};

	fltlpart->xtable = lpart->own_x;
	fltlpart->ytable = lpart->own_y;
	fltlpart->xtable_lens = lpart->xtable_lens;
	fltlpart->ytable_lens = lpart->ytable_lens;

		
	if(lpart->own_miss_x){
		fltlpart->missing_x = *(float *)lpart->own_miss_x->data;
		fltlpart->missing_x_set = True;
	}
	else
		fltlpart->missing_x_set = False;

	if(lpart->own_miss_y){
		fltlpart->missing_y = *(float *)lpart->own_miss_y->data;
		fltlpart->missing_y_set = True;
	}
	else
		fltlpart->missing_y_set = False;


	if(lpart->max_x->typeQ == floatQ)
		fltlpart->max_x = *(float *)lpart->max_x->data;
	else{
		from.size = sizeof(NhlGenArray);
		from.data.ptrval = lpart->max_x;
		to.size = sizeof(float);
		to.data.ptrval = &fltlpart->max_x;
		ret = _NhlReConvertData(genQ,floatQ,&from,&to);
		if(ret < NhlINFO){
		NhlPError(ret,NhlEUNKNOWN,"%s:Unable to convert %s to float",
							func,NhlNctXMaxV);
			return ret;
		}
	}

	if(lpart->min_x->typeQ == floatQ)
		fltlpart->min_x = *(float *)lpart->min_x->data;
	else{
		from.size = sizeof(NhlGenArray);
		from.data.ptrval = lpart->min_x;
		to.size = sizeof(float);
		to.data.ptrval = &fltlpart->min_x;
		ret = _NhlReConvertData(genQ,floatQ,&from,&to);
		if(ret < NhlINFO){
		NhlPError(ret,NhlEUNKNOWN,"%s:Unable to convert %s to float",
							func,NhlNctXMinV);
			return ret;
		}
	}

	if(lpart->max_y->typeQ == floatQ)
		fltlpart->max_y = *(float *)lpart->max_y->data;
	else{
		from.size = sizeof(NhlGenArray);
		from.data.ptrval = lpart->max_y;
		to.size = sizeof(float);
		to.data.ptrval = &fltlpart->max_y;
		ret = _NhlReConvertData(genQ,floatQ,&from,&to);
		if(ret < NhlINFO){
		NhlPError(ret,NhlEUNKNOWN,"%s:Unable to convert %s to float",
							func,NhlNctYMaxV);
			return ret;
		}
	}

	if(lpart->min_y->typeQ == floatQ)
		fltlpart->min_y = *(float *)lpart->min_y->data;
	else{
		from.size = sizeof(NhlGenArray);
		from.data.ptrval = lpart->min_y;
		to.size = sizeof(float);
		to.data.ptrval = &fltlpart->min_y;
		ret = _NhlReConvertData(genQ,floatQ,&from,&to);
		if(ret < NhlINFO){
		NhlPError(ret,NhlEUNKNOWN,"%s:Unable to convert %s to float",
							func,NhlNctYMinV);
			return ret;
		 }
	 }

	return NhlNOERROR;
}

/************************************************************************
*									*
*	Methode definitions						*
*									*
************************************************************************/


/*
 * Function:	CoordArrTableClassInitialize
 *
 * Description:	This function does one time initialization needed by the
 *		CoordArrTableClass.
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
CoordArrTableClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;

	genQ = NrmStringToQuark(NhlTGenArray);
	floatgenQ = NrmStringToQuark(NhlTFloatGenArray);

	byteQ = NrmStringToQuark(NhlTByte);
	charQ = NrmStringToQuark(NhlTCharacter);
	doubleQ = NrmStringToQuark(NhlTDouble);
	floatQ = NrmStringToQuark(NhlTFloat);
	intQ = NrmStringToQuark(NhlTInteger);
	longQ = NrmStringToQuark(NhlTLong);
	shortQ = NrmStringToQuark(NhlTShort);

	xttypeQ = NrmStringToQuark(NhlNctXTableType);
	yttypeQ = NrmStringToQuark(NhlNctYTableType);
	xtableQ = NrmStringToQuark(NhlNctXTable);
	ytableQ = NrmStringToQuark(NhlNctYTable);
	xtablelensQ = NrmStringToQuark(NhlNctXTableLengths);
	ytablelensQ = NrmStringToQuark(NhlNctYTableLengths);
	missxQ = NrmStringToQuark(NhlNctXMissingV);
	missyQ = NrmStringToQuark(NhlNctYMissingV);
	maxxQ = NrmStringToQuark(NhlNctXMaxV);
	maxyQ = NrmStringToQuark(NhlNctYMaxV);
	minxQ = NrmStringToQuark(NhlNctXMinV);
	minyQ = NrmStringToQuark(NhlNctYMinV);

	ret = NhlRegisterConverter(NhlbaseClass,
			NhlcoordArrTableClass->base_class.class_name,
			NhlcoordArrTableFloatClass->base_class.class_name,
			CvtGenObjToFloatObj,NULL,0,False,NULL);

	return ret;
}

static void
CheckTable
#if	NhlNeedProto
(
	NhlString	func,
	NhlGenArray	table,
	NhlGenArray	table_lens,
	NhlString	table_res,
	NhlString	table_lens_res,
	NhlBoolean	*imp,
	NhlBoolean	*inv
)
#else
(func,table,table_lens,table_res,table_lens_res,imp,inv)
	NhlString	func;
	NhlGenArray	table;
	NhlGenArray	table_lens;
	NhlString	table_res;
	NhlString	table_lens_res;
	NhlBoolean	*imp;
	NhlBoolean	*inv;
#endif
{
	*imp = *inv = False;

	if((table != NULL) && (table_lens != NULL)){
		if((table->num_dimensions != 1) ||
				(table_lens->num_dimensions != 1)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s and %s must one dimensional arrays:ignoring",
						func,table_res,table_lens_res);
			*inv = True;
		}
		else if(table->len_dimensions[0] !=
					table_lens->len_dimensions[0]){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:%s and %s must be arrays of the same length:ignoring",
						func,table_res,table_lens_res);
			*inv = True;
		}
	}
	else if((table == NULL) && (table_lens == NULL)){
		*imp = True;
	}
	else if(table != NULL){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s must be set to set %s: resetting",
						func,table_lens_res,table_res);
		*inv = True;
	}

	return;
}

static NhlErrorTypes
CopyTables
#if	NhlNeedProto
(
	NhlGenArray			*table,
	NhlGenArray			*table_lens,
	NrmQuark			typeQ,
	int				size,
	NhlBoolean			copy,
	NhlGenArray			*own,
	_NhlConvertContext		*context,
	NhlString			func
)
#else
(table,table_lens,typeQ,size,copy,own,context,func)
	NhlGenArray			*table;
	NhlGenArray			*table_lens;
	NrmQuark			typeQ;
	int				size;
	NhlBoolean			copy;
	NhlGenArray			*own;
	_NhlConvertContext		*context;
	NhlString			func;
#endif
{
	NhlErrorTypes	ret;

	if(*table_lens){
		*table_lens = _NhlCopyGenArray(*table_lens,True);
		if(*table_lens == NULL){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
	}

	if(*table && *table_lens){

		*table = _NhlCopyGenArray(*table,copy);
		if(*table == NULL){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}

		if(copy){
			ret = CvtToFltPtrs(own,typeQ,size,*table,*table_lens,
								context);
			if(ret < NhlWARNING){
				NHLPERROR((ret,NhlEUNKNOWN,"%s",func));
				return ret;
			}
		}
		else if(typeQ == floatQ)
			*own = *table;
		else
			*own = NULL;
	}
	else
		*own = NULL;

	return NhlNOERROR;
}

/*
 * Function:	GetTypeSize
 *
 * Description:	Returns the size of some well known types.  This is the
 *		list of types that this object knows the size automatically.
 *		If the user wants to use a new type, then they need to
 *		set the "Size" resources.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	unsigned int
 * Side Effect:	
 */
static unsigned int
GetTypeSize
#if	NhlNeedProto
(
	NrmQuark	typeQ
)
#else
(typeQ)
	NrmQuark	typeQ;
#endif
{
	if(typeQ == floatQ)
		return sizeof(float);
	if(typeQ == intQ)
		return sizeof(int);
	if(typeQ == doubleQ)
		return sizeof(double);
	if((typeQ == charQ)||(typeQ == byteQ))
		return sizeof(char);
	if(typeQ == longQ)
		return sizeof(long);
	if(typeQ == shortQ)
		return sizeof(short);
	return 0;
}

/*
 * Function:	CoordArrTableInitialize
 *
 * Description:	This function initializes an instance of a CoordArrTable
 *		class object.
 *
 * In Args:	
 *	NhlClass	lc,	class
 *	NhlLayer		req,	requested
 *	NhlLayer		new,	new
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
CoordArrTableInitialize
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
	char				func[] = "CoordArrTableInitialize";
	NhlCoordArrTableLayer		ncat = (NhlCoordArrTableLayer)new;
	NhlCoordArrTableLayerPart	*cat = &ncat->cat;
	NhlErrorTypes			ret;
	NhlBoolean			impy, impx;
	NhlBoolean			invx, invy;

	if(cat->xtype == NULL){
		if(cat->xtable != NULL){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s:The %s resource must be specified for the %s resource",
					func,NhlNctXTableType,NhlNctXTable);

			return NhlFATAL;
		}
		cat->xtypeQ = NrmNULLQUARK;
	}
	else{
		cat->xtypeQ = NrmStringToQuark(cat->xtype);
		cat->xtype = NrmQuarkToString(cat->xtypeQ);

		if(cat->xsize == 0){
			cat->xsize = GetTypeSize(cat->xtypeQ);
			if(cat->xsize == 0){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:%s must be set if %s is %s",func,
					NhlNctXElementSize,NhlNctXTableType,
								cat->xtype);
				return NhlFATAL;
			}
		}
	}

	if(cat->ytype == NULL){
		if(cat->ytable != NULL){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s:The %s resource must be specified for the %s resource",
					func,NhlNctYTableType,NhlNctYTable);

			return NhlFATAL;
		}
		cat->ytypeQ = NrmNULLQUARK;
	}
	else{
		cat->ytypeQ = NrmStringToQuark(cat->ytype);
		cat->ytype = NrmQuarkToString(cat->ytypeQ);

		if(cat->ysize == 0){
			cat->ysize = GetTypeSize(cat->ytypeQ);
			if(cat->ysize == 0){
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:%s must be set if %s is %s",func,
					NhlNctYElementSize,NhlNctYTableType,
								cat->ytype);
				return NhlFATAL;
			}
		}
	}

	/*
	 * insure accuracy
	 */
	CheckTable(func,cat->xtable,cat->xtable_lens,NhlNctXTable,
					NhlNctXTableLengths,&impx,&invx);
	if(invx){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:%s and %s are invalid",
					func,NhlNctXTable,NhlNctXTableLengths);
		return NhlFATAL;
	}

	CheckTable(func,cat->ytable,cat->ytable_lens,NhlNctYTable,
					NhlNctYTableLengths,&impy,&invy);
	if(invy){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:%s and %s are invalid",
					func,NhlNctYTable,NhlNctYTableLengths);
		return NhlFATAL;
	}

	if(impx && impy){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Cannot have Implied X and Y values",func);
		return NhlFATAL;
	}
	
	/*
	 * Copy Table & Table_lens
	 */
	ret = CopyTables(&cat->xtable,&cat->xtable_lens,cat->xtypeQ,cat->xsize,
				cat->copy_tables,&cat->own_x,&cat->conv_x,func);
	if(ret != NhlNOERROR){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to copy %s",
							func,NhlNctXTable));
		return NhlFATAL;
	}

	ret = CopyTables(&cat->ytable,&cat->ytable_lens,cat->ytypeQ,cat->ysize,
				cat->copy_tables,&cat->own_y,&cat->conv_y,func);
	if(ret != NhlNOERROR){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to copy %s",
							func,NhlNctYTable));
		return NhlFATAL;
	}

	if(cat->missing_x)
		cat->missing_x = _NhlCopyGenArray(cat->missing_x,True);
	if(cat->missing_y)
		cat->missing_y = _NhlCopyGenArray(cat->missing_y,True);

	if(cat->max_x){
		cat->max_x = _NhlCopyGenArray(cat->max_x,True);
		cat->sticky_max_x = True;
	}
	if(cat->max_y){
		cat->max_y = _NhlCopyGenArray(cat->max_y,True);
		cat->sticky_max_y = True;
	}
	if(cat->min_x){
		cat->min_x = _NhlCopyGenArray(cat->min_x,True);
		cat->sticky_min_x = True;
	}
	if(cat->min_y){
		cat->min_y = _NhlCopyGenArray(cat->min_y,True);
		cat->sticky_min_y = True;
	}

	return NhlNOERROR;
}

/*
 * Function:	CoordArrTableSetValues
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
CoordArrTableSetValues
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
	char				func[] = "CoordArrTableSetValues";
	NhlCoordArrTableLayer		ncat = (NhlCoordArrTableLayer)new;
	NhlCoordArrTableLayerPart	*ncatp = &ncat->cat;
	NhlCoordArrTableLayer		ocat = (NhlCoordArrTableLayer)old;
	NhlCoordArrTableLayerPart	*ocatp = &ocat->cat;
	NhlErrorTypes			ret=NhlNOERROR,lret=NhlNOERROR;
	NhlBoolean			impx,impy;
	NhlBoolean			invx,invy;
	NhlBoolean			status=False;
	NhlBoolean			update_minmax=False;

	if(ncatp->xtype == NULL){
		if(ncatp->xtable != NULL){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s:The %s resource must be specified for the %s resource",func,
						NhlNctXTableType,NhlNctXTable);
			return NhlFATAL;
		}
		ncatp->xtypeQ = NrmNULLQUARK;
	}
	else{
		if(ncatp->xtype != ocatp->xtype){
			if((ncatp->xtable != NULL) &&
					(ncatp->xtable == ocatp->xtable)){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't change %s without changing %s",func,
						NhlNctXTableType,NhlNctXTable);
				ret = MIN(NhlWARNING,ret);
				ncatp->xtype = ocatp->xtype;
			}
			else{
				ncatp->xtypeQ = NrmStringToQuark(ncatp->xtype);
				ncatp->xtype = NrmQuarkToString(ncatp->xtypeQ);
			}
		}
		if((ncatp->xsize != ocatp->xsize) && (ncatp->xtype != NULL) &&
					(ncatp->xtype == ocatp->xtype)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't change %s without changing %s",func,
					NhlNctXElementSize,NhlNctXTableType);
			ret = MIN(NhlWARNING,ret);
			ncatp->xsize = ocatp->xsize;
		}
		if((ncatp->xsize == 0) && (ncatp->xtypeQ != NrmNULLQUARK)){
			ncatp->xsize = GetTypeSize(ncatp->xtypeQ);
			if(ncatp->xsize == 0){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:%s must be set if %s is %s",func,
					NhlNctXElementSize,NhlNctXTableType,
								ncatp->xtype);
				ret = MIN(NhlWARNING,ret);
				ncatp->xtypeQ = ocatp->xtypeQ;
				ncatp->xtable = ocatp->xtable;
				ncatp->xsize = ocatp->xsize;
			}
		}
	}

	if(ncatp->ytype == NULL){
		if(ncatp->ytable != NULL){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s:The %s resource must be specified for the %s resource",func,
						NhlNctYTableType,NhlNctYTable);
			return NhlFATAL;
		}
		ncatp->ytypeQ = NrmNULLQUARK;
	}
	else{
		if(ncatp->ytype != ocatp->ytype){
			if((ncatp->ytable != NULL) &&
					(ncatp->ytable == ocatp->ytable)){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't change %s without changing %s",func,
						NhlNctYTableType,NhlNctYTable);
				ret = MIN(NhlWARNING,ret);
				ncatp->ytype = ocatp->ytype;
			}
			else{
				ncatp->ytypeQ = NrmStringToQuark(ncatp->ytype);
				ncatp->ytype = NrmQuarkToString(ncatp->ytypeQ);
			}
		}
		if((ncatp->ysize != ocatp->ysize) && (ncatp->ytype != NULL) &&
					(ncatp->ytype == ocatp->ytype)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Can't change %s without changing %s",func,
					NhlNctYElementSize,NhlNctYTableType);
			ret = MIN(NhlWARNING,ret);
			ncatp->ysize = ocatp->ysize;
		}
		if((ncatp->ysize == 0) && (ncatp->ytypeQ != NrmNULLQUARK)){
			ncatp->ysize = GetTypeSize(ncatp->ytypeQ);
			if(ncatp->ysize == 0){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:%s must be set if %s is %s",func,
					NhlNctYElementSize,NhlNctYTableType,
								ncatp->ytype);
				ret = MIN(NhlWARNING,ret);
				ncatp->ytypeQ = ocatp->ytypeQ;
				ncatp->ytable = ocatp->ytable;
				ncatp->ysize = ocatp->ysize;
			}
		}
	}

	if((ncatp->xtable != ocatp->xtable) ||
				(ncatp->xtable_lens != ocatp->xtable_lens)){

		CheckTable(func,ncatp->xtable,ncatp->xtable_lens,NhlNctXTable,
					NhlNctXTableLengths,&impx,&invx);

		if(invx){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:invalid X dimension resources:resetting to previous values",
									func);
			ncatp->xtable = ocatp->xtable;
			ncatp->xtable_lens = ocatp->xtable_lens;
			ncatp->xtype = ocatp->xtype;
			ncatp->xtypeQ = ocatp->xtypeQ;
			ncatp->xsize = ocatp->xsize;

			if(ncatp->xtable == NULL)
				impx = True;
		}
	}

	if((ncatp->ytable != ocatp->ytable) ||
				(ncatp->ytable_lens != ocatp->ytable_lens)){

		CheckTable(func,ncatp->ytable,ncatp->ytable_lens,NhlNctYTable,
					NhlNctYTableLengths,&impy,&invy);

		if(invy){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:invalid Y dimension resources:resetting to previous values",
									func);
			ncatp->ytable = ocatp->ytable;
			ncatp->ytable_lens = ocatp->ytable_lens;
			ncatp->ytype = ocatp->ytype;
			ncatp->ytypeQ = ocatp->ytypeQ;
			ncatp->ysize = ocatp->ysize;

			if(ncatp->ytable == NULL)
				impy = True;
		}
	}

	if(impx && impy){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
	"%s:Cannot have implied X and Y values: resetting to previous values",
									func);
		ncatp->xtable = ocatp->xtable;
		ncatp->xtable_lens = ocatp->xtable_lens;
		ncatp->xtype = ocatp->xtype;
		ncatp->xtypeQ = ocatp->xtypeQ;
		ncatp->xsize = ocatp->xsize;

		ncatp->ytable = ocatp->ytable;
		ncatp->ytable_lens = ocatp->ytable_lens;
		ncatp->ytype = ocatp->ytype;
		ncatp->ytypeQ = ocatp->ytypeQ;
		ncatp->ysize = ocatp->ysize;
	}

	/*
	 * If copy_tables is True, but xtable == own_x, then we didn't
	 * actually copy the table before, so we should now.
	 */
	if((ncatp->xtable != ocatp->xtable) ||
		(ncatp->xtable_lens != ocatp->xtable_lens) ||
		(ncatp->copy_tables && (ncatp->xtable == ncatp->own_x))){

		lret = CopyTables(&ncatp->xtable,&ncatp->xtable_lens,
				ncatp->xtypeQ,ncatp->xsize,ncatp->copy_tables,
				&ncatp->own_x,&ncatp->conv_x,func);

		if(lret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to copy %s:resetting",func,
					NhlNctXTable);
			return NhlFATAL;
		}

		ret = MIN(ret,lret);

		status = True;
		if(ocatp->own_x != ocatp->xtable)
			NhlFreeGenArray(ocatp->own_x);
		NhlFreeGenArray(ocatp->xtable);
		NhlFreeGenArray(ocatp->xtable_lens);
	}

	/*
	 * If copy_tables is True, but ytable == own_y, then we didn't
	 * actually copy the table before, so we should now.
	 */
	if((ncatp->ytable != ocatp->ytable) ||
		(ncatp->ytable_lens != ocatp->ytable_lens) ||
		(ncatp->copy_tables && (ncatp->ytable == ncatp->own_y))){

		lret = CopyTables(&ncatp->ytable,&ncatp->ytable_lens,
				ncatp->ytypeQ,ncatp->ysize,ncatp->copy_tables,
				&ncatp->own_y,&ncatp->conv_y,func);

		if(lret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Unable to copy %s:resetting",func,
					NhlNctYTable);
			return NhlFATAL;
		}

		ret = MIN(ret,lret);

		status = True;
		if(ocatp->own_y != ocatp->ytable)
			NhlFreeGenArray(ocatp->own_y);
		NhlFreeGenArray(ocatp->ytable);
		NhlFreeGenArray(ocatp->ytable_lens);
	}

	if(ncatp->missing_x != ocatp->missing_x){
		if(ncatp->missing_x)
			ncatp->missing_x = _NhlCopyGenArray(ncatp->missing_x,
									True);
		status = True;
		ncatp->own_miss_x = NULL;
		if(ocatp->own_miss_x != ocatp->missing_x)
			NhlFreeGenArray(ocatp->own_miss_x);
		NhlFreeGenArray(ocatp->missing_x);
	}

	if(ncatp->missing_y != ocatp->missing_y){
		if(ncatp->missing_y)
			ncatp->missing_y = _NhlCopyGenArray(ncatp->missing_y,
									True);
		status = True;
		ncatp->own_miss_y = NULL;
		if(ocatp->own_miss_y != ocatp->missing_y)
			NhlFreeGenArray(ocatp->own_miss_y);
		NhlFreeGenArray(ocatp->missing_y);
	}

	update_minmax = status;

	if(ncatp->max_x != ocatp->max_x){
		ncatp->max_x = _NhlCopyGenArray(ncatp->max_x,True);
		if(!ncatp->max_x)
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
		status = True;
		NhlFreeGenArray(ocatp->max_x);
	}
	else if(update_minmax && !!ncatp->sticky_max_x){
		NhlFreeGenArray(ncatp->max_x);
		ncatp->max_x = NULL;
	}

	if(ncatp->min_x != ocatp->min_x){
		ncatp->min_x = _NhlCopyGenArray(ncatp->min_x,True);
		if(!ncatp->max_x)
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
		status = True;
		NhlFreeGenArray(ocatp->min_x);
	}
	else if(update_minmax && !!ncatp->sticky_min_x){
		NhlFreeGenArray(ncatp->min_x);
		ncatp->min_x = NULL;
	}

	if(ncatp->max_y != ocatp->max_y){
		ncatp->max_y = _NhlCopyGenArray(ncatp->max_y,True);
		if(!ncatp->max_x)
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
		status = True;
		NhlFreeGenArray(ocatp->max_y);
	}
	else if(update_minmax && !!ncatp->sticky_max_y){
		NhlFreeGenArray(ncatp->max_y);
		ncatp->max_y = NULL;
	}

	if(ncatp->min_y != ocatp->min_y){
		ncatp->min_y = _NhlCopyGenArray(ncatp->min_y,True);
		if(!ncatp->max_x)
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
		status = True;
		NhlFreeGenArray(ocatp->min_y);
	}
	else if(update_minmax && !!ncatp->sticky_min_y){
		NhlFreeGenArray(ncatp->min_y);
		ncatp->min_y = NULL;
	}

	_NhlDataChanged((NhlDataItemLayer)ncat,status);

	return ret;
}

/*
 * Function:	CoordArrTableGetValues
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
CoordArrTableGetValues
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
	char				func[] = "CoordArrTableGetValues";
	NhlCoordArrTableLayer		cl = (NhlCoordArrTableLayer)l;
	NhlCoordArrTableLayerPart	*clp = &cl->cat;
	int				i;
	NhlGenArray			tgen;
	NhlErrorTypes			ret=NhlNOERROR,lret;

	for(i=0;i < nargs; i++){

		if(args[i].quark == xttypeQ){
			*(NhlString*)args[i].value.ptrval =
					NhlMalloc(strlen(clp->xtype) + 1);
			if(*(NhlString*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctXTableType);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
			strcpy(*(NhlString*)args[i].value.ptrval,clp->xtype);
		}

		else if(args[i].quark == yttypeQ){
			*(NhlString*)args[i].value.ptrval =
					NhlMalloc(strlen(clp->ytype) + 1);
			if(*(NhlString*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctXTableType);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
			strcpy(*(NhlString*)args[i].value.ptrval,clp->ytype);
		}

		else if((args[i].quark == xtableQ) && (clp->xtable)){
			NhlPointer	*ttbl,*ftbl;
			int		*lens,j;

			*(NhlGenArray*)args[i].value.ptrval = tgen =
				_NhlCopyGenArray(clp->xtable,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctXTable);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
			else{
				/*
				 * Now copy the vectors
				 */
				lens = clp->xtable_lens->data;
				ftbl = clp->xtable->data;
				ttbl = tgen->data;
				for(j=0;j<clp->xtable->num_elements;j++){
					ttbl[j] = NhlMalloc(clp->xsize*lens[j]);
					if(!ttbl[j]){
						NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctXTable);
				*(NhlGenArray*)args[i].value.ptrval = NULL;
						ret = MIN(NhlWARNING,ret);
						break;
					}
					memcpy(ttbl[j],ftbl[j],
							clp->xsize*lens[j]);
				}
			}
		}

		else if((args[i].quark == ytableQ) && (clp->ytable)){
			NhlPointer	*ttbl,*ftbl;
			int		*lens,j;

			*(NhlGenArray*)args[i].value.ptrval = tgen =
				_NhlCopyGenArray(clp->ytable,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctYTable);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
			else{
				/*
				 * Now copy the vectors
				 */
				lens = clp->ytable_lens->data;
				ftbl = clp->ytable->data;
				ttbl = tgen->data;
				for(j=0;j<clp->ytable->num_elements;j++){
					ttbl[j] = NhlMalloc(clp->ysize*lens[j]);
					if(!ttbl[j]){
						NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctXTable);
				*(NhlGenArray*)args[i].value.ptrval = NULL;
						ret = MIN(NhlWARNING,ret);
						break;
					}
					memcpy(ttbl[j],ftbl[j],
							clp->ysize*lens[j]);
				}
			}
		}

		else if((args[i].quark == xtablelensQ) && (clp->xtable_lens)){

			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(clp->xtable_lens,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
						func,NhlNctXTableLengths);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
		}

		else if((args[i].quark == ytablelensQ) && (clp->ytable_lens)){

			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(clp->ytable_lens,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
						func,NhlNctYTableLengths);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
		}

		else if((args[i].quark == missxQ) && (clp->missing_x)){

			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(clp->missing_x,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctXMissingV);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
		}

		else if((args[i].quark == missyQ) && (clp->missing_y)){

			*(NhlGenArray*)args[i].value.ptrval =
				_NhlCopyGenArray(clp->missing_y,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctYMissingV);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
		}

		else if(args[i].quark == maxxQ){

			if(!clp->max_x){
				lret = FlushObject(XDIM,clp);
				if(lret < NhlWARNING){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						"%s:Unable to determine %s",
							func,NhlNctXMaxV);
					ret = MIN(ret,NhlWARNING);
					continue;
				}
			}

			*(NhlGenArray*)args[i].value.ptrval =
					_NhlCopyGenArray(clp->max_x,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctXMaxV);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
		}

		else if(args[i].quark == minxQ){

			if(!clp->min_x){
				lret = FlushObject(XDIM,clp);
				if(lret < NhlWARNING){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						"%s:Unable to determine %s",
							func,NhlNctXMinV);
					ret = MIN(ret,NhlWARNING);
					continue;
				}
			}

			*(NhlGenArray*)args[i].value.ptrval =
					_NhlCopyGenArray(clp->min_x,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctXMinV);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
		}

		else if(args[i].quark == maxyQ){

			if(!clp->max_y){
				lret = FlushObject(YDIM,clp);
				if(lret < NhlWARNING){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						"%s:Unable to determine %s",
							func,NhlNctYMaxV);
					ret = MIN(ret,NhlWARNING);
					continue;
				}
			}

			*(NhlGenArray*)args[i].value.ptrval =
					_NhlCopyGenArray(clp->max_y,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctYMaxV);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
		}

		else if(args[i].quark == minyQ){

			if(!clp->min_y){
				lret = FlushObject(YDIM,clp);
				if(lret < NhlWARNING){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						"%s:Unable to determine %s",
							func,NhlNctYMaxV);
					ret = MIN(ret,NhlWARNING);
					continue;
				}
			}

			*(NhlGenArray*)args[i].value.ptrval =
					_NhlCopyGenArray(clp->min_y,True);

			if(*(NhlGenArray*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,NhlNctYMinV);
				ret = MIN(NhlWARNING,ret);
				continue;
			}
		}
	}

	return ret;
}

/*
 * Function:	CoordArrTableDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlCoordArrTableClass.
 *
 * In Args:	NhlLayer	l
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CoordArrTableDestroy
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlCoordArrTableLayer		cl = (NhlCoordArrTableLayer)l;
	NhlCoordArrTableLayerPart	*clp = &cl->cat;

	/*
	 * Don't free type_string - it points into Quarks.c's data
	 */

	NhlFreeGenArray(clp->xtable);
	NhlFreeGenArray(clp->ytable);

	NhlFreeGenArray(clp->xtable_lens);
	NhlFreeGenArray(clp->ytable_lens);

	if (clp->missing_x == clp->own_miss_x)
		clp->own_miss_x = NULL;
	if (clp->missing_y == clp->own_miss_y)
		clp->own_miss_y = NULL;

	NhlFreeGenArray(clp->missing_x);
	NhlFreeGenArray(clp->missing_y);

	NhlFreeGenArray(clp->max_x);
	NhlFreeGenArray(clp->max_y);
	NhlFreeGenArray(clp->min_x);
	NhlFreeGenArray(clp->min_y);

	NhlFreeGenArray(clp->own_x);
	NhlFreeGenArray(clp->own_y);

	_NhlFreeConvertContext(clp->conv_x);
	_NhlFreeConvertContext(clp->conv_y);

	NhlFreeGenArray(clp->own_miss_x);
	NhlFreeGenArray(clp->own_miss_y);

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
