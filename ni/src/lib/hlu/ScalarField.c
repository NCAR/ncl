/*
 *      $Id: ScalarField.c,v 1.2 1994-05-05 18:17:08 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ScalarField.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Apr  6 17:53:29 MDT 1994
 *
 *	Description:	This class is used to communicate data in the format
 *			of a CoordArray.
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/ScalarFieldP.h>
#include <ncarg/hlu/CoordArrTableP.h>

/************************************************************************
*									*
*	ScalarField Class declarations					*
*									*
************************************************************************/

/*
 * Resource Declarations
 */


/*
 * Function:	ResourceUnset
 *
 * Description:	This function can be used to determine if a resource has
 *		been set at initialize time either in the Create call or
 *		from a resource data base. In order to use it the Boolean
 *		'.resource_set' variable MUST directly proceed the name
 *		of the resource variable it refers to in the LayerPart
 *		struct. Also a .nores Resource for the resource_set variable
 *		must directly preceed the Resource of interest in the 
 *		Resource initialization list in this module.
 *
 * In Args:	
 *		NrmName		name,
 *		NrmClass	class,
 *		NhlPointer	base,
 *		unsigned int	offset
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

/*ARGSUSED*/
static NhlErrorTypes
ResourceUnset
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
	char *cl = (char *) base;
	NhlBoolean *set = (NhlBoolean *)(cl + offset - sizeof(NhlBoolean));

	*set = False;

	return NhlNOERROR;
}

#define	Oset(field)   NhlOffset(NhlScalarFieldFloatLayerRec,sfieldfloat.field)

static NhlResource fltresources[] = {
	{NhlNsfDataArray,NhlCsfDataArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(d_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL)},
	{NhlNsfXArray,NhlCsfXArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL)},
	{NhlNsfYArray,NhlCsfYArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL)},
	{NhlNsfCopyData,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(copy_arrays),NhlTImmediate,_NhlUSET((NhlPointer)True)},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(data_order_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfDataOrder,NhlCsfDataOrder,NhlTInteger,sizeof(int),
		 Oset(data_order),NhlTProcedure,
		 _NhlUSET((NhlPointer)ResourceUnset)},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(missing_value_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfMissingValueF,NhlCsfMissingValueF,NhlTGenArray,
		 sizeof(NhlGenArray),Oset(missing_value),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset)},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(data_max_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfDataMaxF,NhlCsfDataMaxF,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(data_max),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset)},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(data_min_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfDataMinF,NhlCsfDataMinF,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(data_min),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset)},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(x_max_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfXMaxF,NhlCsfXMaxF,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_max),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset)},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(x_min_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfXMinF,NhlCsfXMinF,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_min),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset)},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(y_max_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfYMaxF,NhlCsfYMaxF,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_max),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset)},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(y_min_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfYMinF,NhlCsfYMinF,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_min),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset)},
};
#undef Oset

#define	Oset(field)	NhlOffset(NhlScalarFieldLayerRec,sfield.field)
static NhlResource resources[] = {
	{NhlNdiType,NhlCdiType,NhlTString,sizeof(NhlString),
		Oset(type_string),NhlTImmediate,_NhlUSET((NhlPointer)NULL)},
	{NhlNsfDataArray,NhlCsfDataArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(d_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL)},
	{NhlNsfXArray,NhlCsfXArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL)},
	{NhlNsfYArray,NhlCsfYArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL)},
	{NhlNsfCopyData,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(copy_arrays),NhlTImmediate,_NhlUSET((NhlPointer)True)},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(data_order_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfDataOrder,NhlCsfDataOrder,NhlTInteger,sizeof(int),
		 Oset(data_order),NhlTProcedure,
		 _NhlUSET((NhlPointer)ResourceUnset)},
	{NhlNsfMissingValueV,NhlCsfMissingValueV,NhlTGenArray,
		 sizeof(NhlGenArray),Oset(missing_value),NhlTImmediate,
		 _NhlUSET(NULL)},
	{NhlNsfDataMinV,NhlCsfDataMinV,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(data_min),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfDataMaxV,NhlCsfDataMaxV,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(data_max),NhlTImmediate,_NhlUSET(NULL)},

	{NhlNsfXMinV,NhlCsfXMinV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(x_min),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfXMaxV,NhlCsfXMaxV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(x_max),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfYMinV,NhlCsfYMinV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(y_min),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfYMaxV,NhlCsfYMaxV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(y_max),NhlTImmediate,_NhlUSET(NULL)},

	{NhlNsfXSubsetMinV,NhlCsfXSubsetMinV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(x_subset_min),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfXSubsetMaxV,NhlCsfXSubsetMaxV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(x_subset_max),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfYSubsetMinV,NhlCsfYSubsetMinV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(y_subset_min),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfYSubsetMaxV,NhlCsfYSubsetMaxV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(y_subset_max),NhlTImmediate,_NhlUSET(NULL)},

	{NhlNsfXIndexMin,NhlCsfXIndexMin,NhlTInteger,sizeof(int),
		Oset(x_index_min),NhlTImmediate,_NhlUSET((NhlPointer)-1)},
	{NhlNsfXIndexMax,NhlCsfXIndexMax,NhlTInteger,sizeof(int),
		Oset(x_index_max),NhlTImmediate,_NhlUSET((NhlPointer)-1)},
	{NhlNsfYIndexMin,NhlCsfYIndexMin,NhlTInteger,sizeof(int),
		Oset(y_index_min),NhlTImmediate,_NhlUSET((NhlPointer)-1)},
	{NhlNsfYIndexMax,NhlCsfYIndexMax,NhlTInteger,sizeof(int),
		Oset(y_index_max),NhlTImmediate,_NhlUSET((NhlPointer)-1)},

	{NhlNsfXStride,NhlCsfXStride,NhlTInteger,sizeof(int),
		Oset(x_stride),NhlTImmediate,_NhlUSET((NhlPointer)1)},
	{NhlNsfYStride,NhlCsfYStride,NhlTInteger,sizeof(int),
		Oset(y_stride),NhlTImmediate,_NhlUSET((NhlPointer)1)}
};
#undef Oset

/* base methods */

static NhlErrorTypes ScalarFieldClassPartInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc	/* lc to init	*/
#endif
);

static NhlErrorTypes ScalarFieldClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes ScalarFieldFloatInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes ScalarFieldInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes ScalarFieldSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes ScalarFieldFloatSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes ScalarFieldDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

static NhlErrorTypes ScalarFieldFloatDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

static NhlErrorTypes    CheckCopyVType(
#if	NhlNeedProto
	NhlGenArray	*ga,
	NhlGenArray	copy_ga,
	NhlString	resource_name,
	NhlString	entry_name
#endif
);

static NhlErrorTypes    CheckCopyGenArray(
#if	NhlNeedProto
	NhlGenArray	*ga,
	NhlGenArray	copy_ga,
	int		num_dim,
	int		*min_len,
	NhlString	resource_name,
	NhlString	entry_name
#endif
);

static void FindMinMax(
#if	NhlNeedProto
	NhlGenArray	ga,
	NhlBoolean	missing_set,
	float		missing_val,
	float		*min,
	float		*max
#endif
);

NhlScalarFieldFloatLayerClassRec NhlscalarFieldFloatLayerClassRec = {
	/* NhlBaseLayerClassPart */
	{
/* class_name			*/	"ScalarFieldFloat",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlScalarFieldFloatLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlobjLayerClassRec,

/* resources			*/	fltresources,
/* num_resources		*/	NhlNumber(fltresources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	ScalarFieldFloatInitialize,
/* layer_set_values		*/	ScalarFieldFloatSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	ScalarFieldFloatDestroy
	},
	/* NhlScalarFieldFloatLayerPart */
	{
/* foo				*/	0
	}
};

NhlScalarFieldLayerClassRec NhlscalarFieldLayerClassRec = {
	/* NhlBaseLayerClassPart */
	{
/* class_name			*/	"ScalarField",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlScalarFieldLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)
						&NhldataItemLayerClassRec,
/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	ScalarFieldClassPartInitialize,
/* class_initialize		*/	ScalarFieldClassInitialize,
/* layer_initialize		*/	ScalarFieldInitialize,
/* layer_set_values		*/	ScalarFieldSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	ScalarFieldDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	/* NhlDataItemLayerClassPart */
	{
/* foo				*/	0
	},
	/* NhlScalarFieldLayerClassPart */
	{
/* foo				*/	0
	}
};
	
NhlLayerClass NhlscalarFieldLayerClass = (NhlLayerClass)
					&NhlscalarFieldLayerClassRec;
NhlLayerClass NhlscalarFieldFloatLayerClass = (NhlLayerClass)
					&NhlscalarFieldFloatLayerClassRec;

static	NrmQuark	Qfloat = NrmNULLQUARK;
static	NrmQuark	Qint = NrmNULLQUARK;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/


/*
 * Function:	ConvertDataToFloatArray
 *
 * Description:	This function converts the incoming Data GenArray of whatever
 *		type into the internal float GenArray type used by the
 *		ScalarFieldFloat object.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private
 * Returns:	NhlGenArray or NULL on error
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlGenArray
ConvertDataToFloatArray
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		ga,
	NhlString		entry_name
)
#else
(sfp,ga,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		ga;
	NhlString		entry_name;
#endif
{
	char *e_text;

	if (ga == NULL) 
		return NULL;
	else if (ga->typeQ == Qfloat)
		return ga;
	else if (ga->typeQ == Qint) {
		int i;
		int *ip;
		float *fp;
		NhlGenArray flt_ga;

		ip = (int *) ga->data;

		if ((fp = (float *) NhlConvertMalloc(ga->num_elements * 
						     sizeof(float))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		for (i = 0; i < ga->num_elements; i++)
			fp[i] = (float) ip[i];
		if ((flt_ga = _NhlCreateGenArray(fp,NhlTFloat,sizeof(float),
						 ga->num_dimensions,
						 ga->len_dimensions,
						 False)) == NULL) {
			e_text = "%s: error creating generic array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		return flt_ga;
	}

	e_text = "%s: no conversion for type %s";
	NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
		  NrmQuarkToString(ga->typeQ));
	return NULL;
}

/*
 * Function:	ConvertToFloatArray
 *
 * Description:	This function converts the incoming GenArray of whatever
 *		type into the internal float GenArray type used by the
 *		ScalarFieldFloat object.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private
 * Returns:	NhlGenArray or NULL on error
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlGenArray
ConvertToFloatArray
#if	__STDC__
(

	NhlGenArray		ga,
	NhlString		entry_name
)
#else
(ga,entry_name)
	NhlGenArray		ga;
	NhlString		entry_name;
#endif
{
	char *e_text;

	if (ga == NULL) 
		return NULL;
	else if (ga->typeQ == Qfloat)
		return ga;
	else if (ga->typeQ == Qint) {
		int i;
		int *ip;
		float *fp;
		NhlGenArray flt_ga;

		ip = (int *) ga->data;

		if ((fp = (float *) NhlConvertMalloc(ga->num_elements * 
						     sizeof(float))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		for (i = 0; i < ga->num_elements; i++)
			fp[i] = (float) ip[i];
		if ((flt_ga = _NhlCreateGenArray(fp,NhlTFloat,sizeof(float),
						 ga->num_dimensions,
						 ga->len_dimensions,
						 False)) == NULL) {
			e_text = "%s: error creating generic array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		return flt_ga;
	}

	e_text = "%s: no conversion for type %s";
	NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
		  NrmQuarkToString(ga->typeQ));
	return NULL;
}

/*
 * Function:	CvtGenSFObjToFloatSFObj
 *
 * Description:	This function is used to convert a Generic ScalarField
 *		to a ScalarFieldFloat object.
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
CvtGenSFObjToFloatSFObj
#if	__STDC__
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
	char			*entry_name="CvtGenSFObjToFloatSFObj";
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlScalarFieldLayer	sfl;
	NhlScalarFieldLayerPart *sfp;
	NhlSArg			sargs[30];
	int			nargs=0;
	NhlGenArray		d_arr = NULL, x_arr = NULL, y_arr = NULL;
	float			fval;
	float			xmin,xmax,ymin,ymax;
	float			xsmin,xsmax,ysmin,ysmax;
	int			ixmin,ixmax,iymin,iymax;
	NhlBoolean		xirr = False, yirr = False;
	NhlBoolean		xrev, yrev;

	if (num_args != 0) {
		e_text = "%s:Called w/wrong args";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	sfl = (NhlScalarFieldLayer)_NhlGetLayer(from->data.intval);
	if ((sfl == NULL) ||
	    (sfl->base.layer_class != NhlscalarFieldLayerClass)){
		e_text = "%s:Called w/ improper \"from\" object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	sfp = (NhlScalarFieldLayerPart *) &sfl->sfield;

	if (sfp->d_arr == NULL || sfp->d_arr->num_dimensions != 2) {
		e_text = "%s: invalid data array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	
	if (sfp->x_arr != NULL) {
		if ((x_arr = ConvertToFloatArray(sfp->x_arr,
						 entry_name)) == NULL)
			return NhlFATAL;

		NhlSetSArg(&sargs[nargs++],NhlNsfXArray,x_arr);
		xirr = True;
	}
		     
	if (sfp->y_arr != NULL) {
		if ((y_arr = ConvertToFloatArray(sfp->y_arr,
						 entry_name)) == NULL)
			return NhlFATAL;

		NhlSetSArg(&sargs[nargs++],NhlNsfYArray,y_arr);
		yirr = True;
	}

	ixmin = (sfp->x_index_min < 0) ? 
		0 : MIN(sfp->x_index_min,sfp->d_arr->len_dimensions[1]);
	ixmax = (sfp->x_index_max < 0) ? 
		sfp->d_arr->len_dimensions[1] : 
			MIN(sfp->x_index_max,sfp->d_arr->len_dimensions[1]); 
	iymin = (sfp->y_index_min < 0) ? 
		0 : MIN(sfp->y_index_min,sfp->d_arr->len_dimensions[1]); 
	iymax = (sfp->y_index_max < 0) ? 
		sfp->d_arr->len_dimensions[0] : 
			MIN(sfp->y_index_max,sfp->d_arr->len_dimensions[1]);

	if (ixmax - ixmin < 2) {
		e_text = 
	       "%s: X index maximum not enough larger than minimum: resetting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(NhlWARNING,ret);
		ixmin = sfp->x_index_min = 0;
		ixmax = sfp->x_index_max = sfp->d_arr->len_dimensions[1];
	}
	if (iymax - iymin < 2) {
		e_text = 
	       "%s: Y index maximum not enough larger than minimum: resetting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(NhlWARNING,ret);
		iymin = sfp->y_index_min = 0;
		iymax = sfp->y_index_max = sfp->d_arr->len_dimensions[0];
	}

 	if (sfp->x_min != NULL)
		xmin = *((float *)sfp->x_min->data);
	else if (sfp->x_subset_min != NULL)
		xmin = *((float *)sfp->x_subset_min->data);
	else 
		xmin = 0.0;

	if (sfp->x_max != NULL)
		xmax = *((float *)sfp->x_max->data);
	else if (sfp->x_subset_max != NULL)
		xmax = *((float *)sfp->x_subset_max->data);
	else 
		xmax = 1.0;

	xrev = (xmin > xmax) ? True : False;

 	if (sfp->x_subset_min != NULL)
		xsmin = MAX(xmin,*((float *)sfp->x_subset_min->data));
	else
		xsmin = xmin;

	if (sfp->x_subset_max != NULL)
		xsmax = MIN(xmax,*((float *)sfp->x_subset_max->data));
	else
		xsmax = xmax;

 	if (sfp->y_min != NULL)
		ymin = *((float *)sfp->y_min->data);
	else if (sfp->y_subset_min != NULL)
		ymin = *((float *)sfp->y_subset_min->data);
	else 
		ymin = 0.0;

	if (sfp->y_max != NULL)
		ymax = *((float *)sfp->y_max->data);
	else if (sfp->y_subset_max != NULL)
		ymax = *((float *)sfp->y_subset_max->data);
	else 
		ymax = 1.0;

	yrev = (ymin > ymax) ? True : False;

 	if (sfp->y_subset_min != NULL)
		ysmin = MAX(ymin,*((float *)sfp->y_subset_min->data));
	else
		ysmin = ymin;
	if (sfp->y_subset_max != NULL)
		ysmax = MIN(ymax,*((float *)sfp->y_subset_max->data));
	else
		ysmax = ymax;
	
	if (xirr) 
		printf("irregular scalar field data not implemented yet\n");
	else {
		float drange;
		int   irange;

		drange = xrev ? xmax - xmin : xmin - xmax;
		irange = ixmax - ixmin;
	}
	if ((d_arr = ConvertDataToFloatArray(sfp,sfp->d_arr,
						  entry_name)) == NULL)
		return NhlFATAL;
		
	else
		NhlSetSArg(&sargs[nargs++],NhlNsfDataArray,d_arr);

		     
	if (sfp->missing_value != NULL) {
		fval = *((float *)sfp->missing_value->data);
		NhlSetSArg(&sargs[nargs++],NhlNsfMissingValueF,fval);
	}
		     
	if (sfp->data_max != NULL) {
		fval = *((float *)sfp->data_max->data);
		NhlSetSArg(&sargs[nargs++],NhlNsfDataMaxF,fval);
	}
		     
	if (sfp->data_min != NULL) {
		fval = *((float *)sfp->data_min->data);
		NhlSetSArg(&sargs[nargs++],NhlNsfDataMinF,fval);
	}
		
	if (sfp->x_max != NULL) {
		fval = *((float *)sfp->x_max->data);
		NhlSetSArg(&sargs[nargs++],NhlNsfXMaxF,fval);
	}

	if (sfp->x_min != NULL) {
		fval = *((float *)sfp->x_min->data);
		NhlSetSArg(&sargs[nargs++],NhlNsfXMinF,fval);
	}
		     
	if (sfp->y_max != NULL) {
		fval = *((float *)sfp->y_max->data);
		NhlSetSArg(&sargs[nargs++],NhlNsfYMaxF,fval);
	}

	if (sfp->y_min != NULL) {
		fval = *((float *)sfp->y_min->data);
		NhlSetSArg(&sargs[nargs++],NhlNsfYMinF,fval);
	}
		
	NhlSetSArg(&sargs[nargs++],NhlNsfCopyData,False);
#if 0
	NhlSetSArg(&sargs[nargs++],NhlNsfDataOrder,sfp->data_order);
#endif

	ret = NhlALCreate(to->data.ptrval,"no.name",
			  NhlscalarFieldFloatLayerClass,
			  sfl->base.id,sargs,nargs);

	if (d_arr != NULL && d_arr != sfp->d_arr)
		NhlFreeGenArray(d_arr);
	if (x_arr != NULL && x_arr != sfp->x_arr)
		NhlFreeGenArray(x_arr);
	if (y_arr != NULL && y_arr != sfp->y_arr)
		NhlFreeGenArray(y_arr);

	return ret;
}

/************************************************************************
*									*
*	Method definitions						*
*									*
************************************************************************/


/*
 * Function:	ScalarFieldClassInitialize
 *
 * Description:	This function does one time initialization needed by the
 *		ScalarFieldClass.
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
ScalarFieldClassInitialize
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;

	Qfloat = NrmStringToQuark(NhlTFloat);
	Qint = NrmStringToQuark(NhlTInteger);

	ret = NhlRegisterConverter(
			NhlscalarFieldLayerClass->base_class.class_name,
			NhlscalarFieldFloatLayerClass->base_class.class_name,
			CvtGenSFObjToFloatSFObj,NULL,0,False,NULL);
	return ret;
}

/*
 * Function:	ScalarFieldClassPartInitialize
 *
 * Description:	This function is used to init the sfield_class part 
 *		of the layer class record of this class and of all sub-classes.
 *
 * In Args:	
 *		NhlLayerClass	lc	pointer to class structure
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
ScalarFieldClassPartInitialize
#if	__STDC__
(
	NhlLayerClass	lc	/* pointer to class structure	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* pointer to class structure	*/
#endif
{
	NhlErrorTypes		ret;

	ret = _NhlRegisterChildClass(lc,NhlscalarFieldFloatLayerClass,
				     True,False,NULL);

	return ret;
}


/*
 * Function:	ScalarFieldInitialize
 *
 * Description:	This function initializes an instance of a ScalarField
 *		class object.
 *
 * In Args:	
 *	NhlLayerClass	lc,	class
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
ScalarFieldInitialize
#if	__STDC__
(
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlLayerClass	lc;	/* class	*/
	NhlLayer	req;	/* requested	*/
	NhlLayer	new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	char			*entry_name = "ScalarFieldInitialize";
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlScalarFieldLayer	sfl = (NhlScalarFieldLayer)new;
	NhlScalarFieldLayerPart	*sfp = &(sfl->sfield);
	NhlGenArray		ga;

	if (sfp->d_arr == NULL) {
		e_text = 
		 "%s:The %s resource must be specified to create a %s object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NhlNsfDataArray,_NhlClassName(lc));
		return NhlFATAL;
	}
	if ((sfp->d_arr = 
	     _NhlCopyGenArray(sfp->d_arr,sfp->copy_arrays)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (sfp->x_arr != NULL && 
	    (sfp->x_arr =
	     _NhlCopyGenArray(sfp->x_arr,sfp->copy_arrays)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (sfp->y_arr != NULL && 
	    (sfp->y_arr =
	     _NhlCopyGenArray(sfp->y_arr,sfp->copy_arrays)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (sfp->missing_value != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->missing_value,
					NhlNsfMissingValueV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->missing_value = ga;
	}

	if (sfp->data_min != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->data_min,
					NhlNsfDataMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->data_min = ga;
	}
	if (sfp->data_max != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->data_max,
					NhlNsfDataMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->data_max = ga;
	}

	if (sfp->x_min != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_min,
					NhlNsfXMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_min = ga;
	}
	if (sfp->x_max != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_max,
					NhlNsfXMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_max = ga;
	}


	if (sfp->y_min != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_min,
					NhlNsfYMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_min = ga;
	}
	if (sfp->y_max != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_max,
					NhlNsfYMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_max = ga;
	}


	if (sfp->x_subset_min != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_subset_min,
					NhlNsfXSubsetMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_subset_min = ga;
	}
	if (sfp->x_subset_max != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_subset_max,
					NhlNsfXSubsetMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_subset_max = ga;
	}


	if (sfp->y_subset_min != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_subset_min,
					NhlNsfYSubsetMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_min = ga;
	}
	if (sfp->y_subset_max != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_subset_max,
					NhlNsfYSubsetMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_max = ga;
	}
	
	return ret;
}

/*
 * Function:	ScalarFieldSetValues
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
ScalarFieldSetValues
#if	__STDC__
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
	char			*entry_name = "ScalarFieldSetValues";
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char 			*e_text;
	NhlScalarFieldLayer	sfl = (NhlScalarFieldLayer)new;
	NhlScalarFieldLayer	osfl = (NhlScalarFieldLayer)old;
	NhlScalarFieldLayerPart	*sfp = &(sfl->sfield);
	NhlScalarFieldLayerPart	*osfp = &(osfl->sfield);
	NhlGenArray		ga;
	NhlBoolean		status = False;

	if (sfp->d_arr != osfp->d_arr) {
		if (sfp->d_arr == NULL) {
			e_text = 
			   "%s:The %s resource cannot be set NULL: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
				  entry_name,NhlNsfDataArray);
			ret = NhlWARNING;
			sfp->d_arr = osfp->d_arr;
		}
		else {
			NhlFreeGenArray(osfp->d_arr);
			if ((ga = 
			     _NhlCopyGenArray(sfp->d_arr,
					      sfp->copy_arrays)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			sfp->d_arr = ga;
			status = True;
		}
	}

	if (sfp->x_arr != osfp->x_arr) {
		NhlFreeGenArray(osfp->x_arr);
		if (sfp->x_arr != NULL && 
		    (ga = _NhlCopyGenArray(sfp->x_arr,
					   sfp->copy_arrays)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		sfp->x_arr = ga;
		status = True;
	}

	if (sfp->y_arr != osfp->y_arr) {
		NhlFreeGenArray(osfp->y_arr);
		if (sfp->y_arr != NULL && 
		    (ga = _NhlCopyGenArray(sfp->y_arr,
					   sfp->copy_arrays)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		sfp->y_arr = ga;
		status = True;
	}

	if (sfp->missing_value != osfp->missing_value) {
		subret = CheckCopyVType(&osfp->missing_value,
					sfp->missing_value,
					NhlNsfMissingValueV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->missing_value = osfp->missing_value;
		status = True;
	}

	if (sfp->data_min != osfp->data_min) {
		subret = CheckCopyVType(&osfp->data_min,sfp->data_min,
					NhlNsfDataMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->data_min = osfp->data_min;
		status = True;
	}
	if (sfp->data_max != osfp->data_max) {
		subret = CheckCopyVType(&osfp->data_max,sfp->data_max,
					NhlNsfDataMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->data_max = osfp->data_max;
		status = True;
	}

	if (sfp->x_min != osfp->x_min) {
		subret = CheckCopyVType(&osfp->x_min,sfp->x_min,
					NhlNsfXMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_min = osfp->x_min;
		status = True;
	}
	if (sfp->x_max != osfp->x_max) {
		subret = CheckCopyVType(&osfp->x_max,sfp->x_max,
					NhlNsfXMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_max = osfp->x_max;
		status = True;
	}

	if (sfp->y_min != osfp->y_min) {
		subret = CheckCopyVType(&osfp->y_min,sfp->y_min,
					NhlNsfYMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_min = osfp->y_min;
		status = True;
	}
	if (sfp->y_max != osfp->y_max) {
		subret = CheckCopyVType(&osfp->y_max,sfp->y_max,
					NhlNsfYMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_max = osfp->y_max;
		status = True;
	}


	if (sfp->x_subset_min != osfp->x_subset_min) {
		subret = CheckCopyVType(&osfp->x_subset_min,sfp->x_subset_min,
					NhlNsfXSubsetMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_subset_min = osfp->x_subset_min;
		status = True;
	}
	if (sfp->x_subset_max != osfp->x_subset_max) {
		subret = CheckCopyVType(&osfp->x_subset_max,sfp->x_subset_max,
					NhlNsfXSubsetMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_subset_max = osfp->x_subset_max;
		status = True;
	}


	if (sfp->y_subset_min != osfp->y_subset_min) {
		subret = CheckCopyVType(&osfp->y_subset_min,sfp->y_subset_min,
					NhlNsfYSubsetMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_min = osfp->y_subset_min;
		status = True;
	}
	if (sfp->y_subset_max != osfp->y_subset_max) {
		subret = CheckCopyVType(&osfp->y_subset_max,sfp->y_subset_max,
					NhlNsfYSubsetMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_max = osfp->y_subset_max;
		status = True;
	}

        _NhlDataChanged((NhlDataItemLayer)new,status);

	return	ret;
}


/*
 * Function:	ScalarFieldDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlScalarFieldLayerClass.
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
ScalarFieldDestroy
#if	__STDC__
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
/*
 *	char			*entry_name = "ScalarFieldDestroy";
 */
	NhlScalarFieldLayer	sfl = (NhlScalarFieldLayer)l;
	NhlScalarFieldLayerPart	*sfp = &(sfl->sfield);

	NhlFreeGenArray(sfp->d_arr);
	NhlFreeGenArray(sfp->x_arr);
	NhlFreeGenArray(sfp->y_arr);
	NhlFreeGenArray(sfp->missing_value);
	NhlFreeGenArray(sfp->data_min);
	NhlFreeGenArray(sfp->data_max);
	NhlFreeGenArray(sfp->x_min);
	NhlFreeGenArray(sfp->x_max);
	NhlFreeGenArray(sfp->y_min);
	NhlFreeGenArray(sfp->y_max);
	NhlFreeGenArray(sfp->x_subset_min);
	NhlFreeGenArray(sfp->x_subset_max);
	NhlFreeGenArray(sfp->y_subset_min);
	NhlFreeGenArray(sfp->y_subset_max);

	return NhlNOERROR;
}


/*
 * Function:    CheckCopyVType
 *
 * Description:	Copies a variable type scalar stored in a GenArray
 *
 * In Args:	
 *		copy_ga 	GenArray to copy
 *		resource_name	name of the GenArray resource 		
 *		entry_name	name of the high level caller of the routine 
 *
 * Out Args:	*ga		copy of the gen array, NULL if error;
 *
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static NhlErrorTypes    CheckCopyVType
#if __STDC__
(
	NhlGenArray	*ga,
	NhlGenArray	copy_ga,
	NhlString	resource_name,
	NhlString	entry_name
)
#else
(ga,copy_ga,resource_name,entry_name)
	NhlGenArray	*ga;
	NhlGenArray	copy_ga;
	NhlString	resource_name;
	NhlString	entry_name;
#endif
{
	char		*e_text;

	if (copy_ga == NULL ||
	    copy_ga->num_elements != 1 || 
	    copy_ga->num_dimensions != 1) {
		e_text = "%s: variable type resource %s invalid";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  resource_name);
		*ga = NULL;
		return NhlFATAL;
	}

	if (copy_ga->typeQ != Qfloat && copy_ga->typeQ != Qint) {
		e_text = "%s: unsupported type for variable type resource %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  resource_name);
		*ga = NULL;
		return NhlFATAL;
	}

	if (*ga != NULL) NhlFreeGenArray(*ga);

	if ((*ga = _NhlCopyGenArray(copy_ga,True)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		*ga = NULL;
		return NhlFATAL;
	}

	return NhlNOERROR;
}

/*
 * Function:	ScalarFieldFloatInitialize
 *
 * Description:	This function initializes an instance of a ScalarFieldFloat
 *		class object.
 *
 * In Args:	
 *	NhlLayerClass	lc,	class
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
ScalarFieldFloatInitialize
#if	__STDC__
(
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlLayerClass	lc;	/* class	*/
	NhlLayer	req;	/* requested	*/
	NhlLayer	new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	char			*entry_name = "ScalarFieldFloatInitialize";
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlScalarFieldFloatLayer	nsf = (NhlScalarFieldFloatLayer) new;
	NhlScalarFieldFloatLayerPart	*nsfp =
			(NhlScalarFieldFloatLayerPart *) &nsf->sfieldfloat;
	int			ldim[2];
	NhlGenArray		ga;
/*
 * Copy the data array, with checking
 */
	ga = NULL;
	ldim[0] = 2; ldim[1] = 2;
	if (nsfp->d_arr == NULL) {
		e_text = "%s: data array resource %s must be set";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNsfDataArray);
		return NhlFATAL;
	}
	else if ((subret = CheckCopyGenArray(&ga,nsfp->d_arr,2,ldim,
					     NhlNsfDataArray,
					     entry_name)) < NhlWARNING)
		return NhlFATAL;
 	else if ((ret = MIN(subret,ret)) <= NhlWARNING) {
		e_text = "%s: invalid data array resource %s: cannot continue";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNsfDataArray);
		return NhlFATAL;
	}
	nsfp->d_arr = ga;

/*
 * Copy the X coordinate array, with checking
 */
	if (nsfp->x_arr != NULL) {
		ga = NULL;
		ldim[0] = 2;
		if ((subret = CheckCopyGenArray(&ga,nsfp->x_arr,1,ldim,
						NhlNsfXArray,
						entry_name)) < NhlWARNING) {
			return NhlFATAL;
		}
		else if ((ret = MIN(subret,ret)) <= NhlWARNING) {
			e_text = "%s: invalid resource %s: resetting to NULL";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNsfXArray);
			nsfp->x_arr = NULL;
		}
		else {
			nsfp->x_arr = ga;
		}
	}

/*
 * Copy the Y coordinate array, with checking
 */
	if (nsfp->y_arr != NULL) {
		ga = NULL;
		ldim[0] = 2;
		if ((subret = CheckCopyGenArray(&ga,nsfp->y_arr,1,ldim,
						NhlNsfYArray,
						entry_name)) < NhlWARNING) {
			return NhlFATAL;
		}
		else if ((ret = MIN(subret,ret)) <= NhlWARNING) {
			e_text = "%s: invalid resource %s: resetting to NULL";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNsfYArray);
			nsfp->x_arr = NULL;
		}
		else {
			nsfp->x_arr = ga;
		}
	}

/*
 * Set the data min/max values 
 * Set the missing value to 0.0 if uninitialized
 */

	if (! nsfp->missing_value_set) 
		nsfp->missing_value = 0.0;
	if (! (nsfp->data_min_set && nsfp->data_max_set)) { 
		float min, max;

		FindMinMax(nsfp->d_arr,nsfp->missing_value_set,
			   nsfp->missing_value,&min,&max);
		if (! nsfp->data_min_set) 
			nsfp->data_min = min;
		if (! nsfp->data_max_set)
			nsfp->data_max = max;
	}
/*
 * The data space defaults to the range 1.0 through array dimension along
 * each coordinate axis.
 */

	if (! nsfp->x_min_set) nsfp->x_min = 1.0;
	if (! nsfp->x_max_set) nsfp->x_max = nsfp->d_arr->len_dimensions[0];
	if (! nsfp->y_min_set) nsfp->y_min = 1.0;
	if (! nsfp->y_max_set) nsfp->y_max = nsfp->d_arr->len_dimensions[1];

	return ret;
}


/*
 * Function:	ScalarFieldFloatSetValues
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
ScalarFieldFloatSetValues
#if	__STDC__
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
	char			*entry_name = "ScalarFieldFloatSetValues";
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlScalarFieldFloatLayer	nsf = (NhlScalarFieldFloatLayer) new;
	NhlScalarFieldFloatLayerPart	*nsfp = 
			(NhlScalarFieldFloatLayerPart *) &nsf->sfieldfloat;
	NhlScalarFieldFloatLayer	osf = (NhlScalarFieldFloatLayer) old;
	NhlScalarFieldFloatLayerPart	*osfp = 
			(NhlScalarFieldFloatLayerPart *) &osf->sfieldfloat;
	int			ldim[2];
	NhlGenArray		ga;
	NhlBoolean		status = False, data_changed = False;

/*
 * If the data array has changed copy it, with checking
 */
	if (nsfp->d_arr != osfp->d_arr) {
		ga = NULL;
		ldim[0] = 2; ldim[1] = 2;
		if (nsfp->d_arr == NULL) {
			e_text = "%s: NULL data array resource %s; resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNsfDataArray);
			ret = NhlWARNING;
			nsfp->d_arr = osfp->d_arr;
		}
		else if ((subret = 
			  CheckCopyGenArray(&ga,nsfp->d_arr,2,ldim,
					    NhlNsfDataArray,
					    entry_name)) < NhlWARNING) {
			return NhlFATAL;
		}
		else if (subret <= NhlWARNING) {
			e_text = 
			    "%s: invalid data array resource %s; resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNsfDataArray);
			ret = MIN(ret,subret);
			nsfp->d_arr = osfp->d_arr;
		}
		else {
			NhlFreeGenArray(osfp->d_arr);
			nsfp->d_arr = ga;
			status = True;
			data_changed = True;
		}
	}

/*
 * If the X coordinate array has changed copy it, with checking
 */

	if (nsfp->x_arr != osfp->x_arr) {
		ga = NULL;
		ldim[0] = 2;
		if (nsfp->x_arr == NULL) {
			e_text = "%s: NULL array resource %s; resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNsfXArray);
			ret = NhlWARNING;
			nsfp->x_arr = osfp->x_arr;
		}
		else if ((subret = 
			  CheckCopyGenArray(&ga,nsfp->x_arr,1,ldim,
					    NhlNsfXArray,
					    entry_name)) < NhlWARNING) {
			return NhlFATAL;
		}
		else if (subret <= NhlWARNING) {
			e_text = 
			    "%s: invalid array resource %s; resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNsfXArray);
			ret = MIN(ret,subret);
			nsfp->x_arr = osfp->x_arr;
		}
		else {
			NhlFreeGenArray(osfp->d_arr);
			nsfp->x_arr = ga;
			status = True;
		}
	}

/*
 * If the Y coordinate array has changed copy it, with checking
 */

	if (nsfp->y_arr != osfp->y_arr) {
		ga = NULL;
		ldim[0] = 2;
		if (nsfp->y_arr == NULL) {
			e_text = "%s: NULL array resource %s; resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNsfYArray);
			ret = NhlWARNING;
			nsfp->y_arr = osfp->y_arr;
		}
		else if ((subret = 
			  CheckCopyGenArray(&ga,nsfp->y_arr,1,ldim,
					    NhlNsfYArray,
					    entry_name)) < NhlWARNING) {
			return NhlFATAL;
		}
		else if (subret <= NhlWARNING) {
			e_text = 
			    "%s: invalid array resource %s; resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNsfYArray);
			ret = MIN(ret,subret);
			nsfp->y_arr = osfp->y_arr;
		}
		else {
			NhlFreeGenArray(osfp->d_arr);
			nsfp->y_arr = ga;
			status = True;
		}
	}

/*
 * Set the data minimum and maximum values
 */
	
	if (_NhlArgIsSet(args,nargs,NhlNsfDataMaxF))
		nsfp->data_max_set = True;
	if (_NhlArgIsSet(args,nargs,NhlNsfDataMinF))
		nsfp->data_min_set = True;
	if (_NhlArgIsSet(args,nargs,NhlNsfMissingValueF))
		nsfp->missing_value_set = True;
	
	if (data_changed && 
	    ! (nsfp->data_min_set && nsfp->data_max_set)) { 
		float min, max;

		FindMinMax(nsfp->d_arr,nsfp->missing_value_set,
			   nsfp->missing_value,&min,&max);
		if (! nsfp->data_min_set) 
			nsfp->data_min = min;
		if (! nsfp->data_max_set)
			nsfp->data_max = max;
	}
	if (nsfp->data_min != osfp->data_min) status = True;
	if (nsfp->data_max != osfp->data_max) status = True;
	if (nsfp->missing_value != osfp->missing_value) status = True;
	if (nsfp->x_min != osfp->x_min) status = True;
	if (nsfp->x_max != osfp->x_max) status = True;
	if (nsfp->y_min != osfp->y_min) status = True;
	if (nsfp->y_max != osfp->y_max) status = True;

	_NhlDataChanged((NhlDataItemLayer) new->base.parent, status);
    
	return ret;

}


/*
 * Function:	ScalarFieldFloatDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlScalarFieldFloatLayerClass.
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
ScalarFieldFloatDestroy
#if	__STDC__
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlScalarFieldFloatLayer	sf = (NhlScalarFieldFloatLayer) l;
	NhlScalarFieldFloatLayerPart	*sfp = 
			(NhlScalarFieldFloatLayerPart *) &sf->sfieldfloat;
	
	if (sfp->d_arr != NULL) {
		NhlFreeGenArray(sfp->d_arr);
	}

	if (sfp->x_arr != NULL) {
		NhlFreeGenArray(sfp->x_arr);
	}

	if (sfp->y_arr != NULL) {
		NhlFreeGenArray(sfp->y_arr);
	}
	return NhlNOERROR;
}

/*
 * Function:    CheckCopyGenArray
 *
 * Description:	Copies a float GenArray with checking
 *
 * In Args:	
 *		copy_ga 	float GenArray to copy
 *		num_dim		number of dimensions needed in GenArray
 *		min_len[]	minimum length of each dimension
 *		resource_name	name of the GenArray resource 		
 *		entry_name	name of the high level caller of the routine 
 *
 * Out Args:	*ga		copy of the gen array, NULL if error;
 *
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static NhlErrorTypes    CheckCopyGenArray
#if __STDC__
(
	NhlGenArray	*ga,
	NhlGenArray	copy_ga,
	int		num_dim,
	int		*min_len,
	NhlString	resource_name,
	NhlString	entry_name
)
#else
(ga,copy_ga,num_dim,min_len,resource_name,entry_name)
	NhlGenArray	*ga;
	NhlGenArray	copy_ga;
	int		num_dim;
	int		*min_len;
	NhlString	resource_name;
	NhlString	entry_name;
#endif
{
	int		i;
	char		*e_text;

/* 
 * If the type is wrong it is a fatal internal error because the type
 * should have been converted to float. Other errors may be user caused 
 * and therefore may be handled non-fatally in a SetValues
 */
	if (copy_ga->typeQ != Qfloat) {
		e_text = "%s: array resource %s has incorrect type";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  resource_name);
		return NhlFATAL;
	}
	if (copy_ga->num_dimensions != num_dim) {
		e_text = 
		  "%s: array resource %s has incorrect number of dimensions";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  resource_name);
		return NhlWARNING;
	}
	for (i = 0; i < num_dim; i++) {
		if (min_len[i] > copy_ga->len_dimensions[i]) { 
			e_text = 
		 "%s: array resource %s has too few elements for dimension %d";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  resource_name,i);
			return NhlWARNING;
		}
	}
	if ((*ga = _NhlCopyGenArray(copy_ga,True)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	return NhlNOERROR;
}

/*
 * Function:    FindMinMax
 *
 * Description:	Finds the maximum and minimum value in the data GenArray
 *
 * In Args:	
 *		ga		the GenArray
 *		missing_set	is the missing value set?
 *		*missing_val	address of the missing value
 *
 * Out Args:	*min		the minimum value
 *		*max		the maximum value
 *
 * Return Values: error status
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static void FindMinMax
#if __STDC__
(
	NhlGenArray	ga,
	NhlBoolean	missing_set,
	float		missing_val,
	float		*min,
	float		*max
)
#else
(ga,missing_set,missing_val,min,max)
	NhlGenArray	ga;
	NhlBoolean	missing_set;
	float		missing_val;
	float		*min;
	float		*max;
#endif
{
	int		ix;
	float		*fp = (float *) ga->data;
		
	*min = BIGNUMBER;
	*max = - BIGNUMBER;
	for (ix = 0; ix < ga->num_elements; ix++,fp++) {
		if (missing_set && *fp == missing_val)
			continue;
		if (*fp < *min) *min = *fp;
		if (*fp > *max) *max = *fp;
	}
	return;
}
