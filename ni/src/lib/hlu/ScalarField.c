/*
 *      $Id: ScalarField.c,v 1.3 1994-05-17 22:26:14 dbrown Exp $
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
#include <math.h>

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

	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		 Oset(missing_value_set),NhlTImmediate,
		 _NhlUSET((NhlPointer)True)},
	{NhlNsfMissingValueF,NhlCsfMissingValueF,NhlTFloat,sizeof(float),
		 Oset(missing_value),
		 NhlTProcedure,_NhlUSET((NhlPointer)ResourceUnset)},

	{NhlNsfDataMinF,NhlCsfDataMinF,NhlTFloat,sizeof(float),
		 Oset(data_min),NhlTString,_NhlUSET("0.0")},
	{NhlNsfDataMaxF,NhlCsfDataMaxF,NhlTFloat,sizeof(float),
		 Oset(data_max),NhlTString,_NhlUSET("1.0")},
	{NhlNsfXCStartF,NhlCsfXCStartF,NhlTFloat,sizeof(float),
		 Oset(x_start),NhlTString,_NhlUSET("0.0")},
	{NhlNsfXCEndF,NhlCsfXCEndF,NhlTFloat,sizeof(float),
		 Oset(x_end),NhlTString,_NhlUSET("1.0")},
	{NhlNsfYCStartF,NhlCsfYCStartF,NhlTFloat,sizeof(float),
		 Oset(y_start),NhlTString,_NhlUSET("0.0")},
	{NhlNsfYCEndF,NhlCsfYCEndF,NhlTFloat,sizeof(float),
		 Oset(y_end),NhlTString,_NhlUSET("1.0")},

	{ NhlNsfDataBegin,NhlCsfDataBegin,NhlTInteger,sizeof(int),
		  Oset(begin),NhlTImmediate,_NhlUSET((NhlPointer)0)},
	{ NhlNsfDataFastDim,NhlCsfDataFastDim,NhlTInteger,sizeof(int),
		  Oset(fast_dim),NhlTImmediate,_NhlUSET((NhlPointer)0)},
	{ NhlNsfDataFastLen,NhlCsfDataFastLen,NhlTInteger,sizeof(int),
		  Oset(fast_len),NhlTImmediate,_NhlUSET((NhlPointer)0)},
	{ NhlNsfDataSlowLen,NhlCsfDataSlowLen,NhlTInteger,sizeof(int),
		  Oset(slow_len),NhlTImmediate,_NhlUSET((NhlPointer)0)},
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
	{NhlNsfSubsetByIndex,NhlCsfSubsetByIndex,
		 NhlTBoolean,sizeof(NhlBoolean),Oset(subset_by_index),
		 NhlTImmediate,_NhlUSET((NhlPointer)False)},
	{NhlNsfCopyData,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(copy_arrays),NhlTImmediate,_NhlUSET((NhlPointer)True)},
	{NhlNsfExchangeDimensions,NhlCsfExchangeDimensions,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(exchange_dimensions),NhlTImmediate,
		 _NhlUSET((NhlPointer)False)},
	{NhlNsfMissingValueV,NhlCsfMissingValueV,NhlTGenArray,
		 sizeof(NhlGenArray),Oset(missing_value),NhlTImmediate,
		 _NhlUSET(NULL)},
	{NhlNsfDataMinV,NhlCsfDataMinV,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(data_min),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfDataMaxV,NhlCsfDataMaxV,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(data_max),NhlTImmediate,_NhlUSET(NULL)},

	{NhlNsfXCStartV,NhlCsfXCStartV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(x_start),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfXCEndV,NhlCsfXCEndV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(x_end),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfYCStartV,NhlCsfYCStartV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(y_start),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfYCEndV,NhlCsfYCEndV,NhlTGenArray,sizeof(NhlGenArray),
		Oset(y_end),NhlTImmediate,_NhlUSET(NULL)},

	{NhlNsfXCStartSubsetV,NhlCsfXCStartSubsetV,
		 NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_subset_start),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfXCEndSubsetV,NhlCsfXCEndSubsetV,
		 NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_subset_end),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfYCStartSubsetV,NhlCsfYCStartSubsetV,
		 NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_subset_start),NhlTImmediate,_NhlUSET(NULL)},
	{NhlNsfYCEndSubsetV,NhlCsfYCEndSubsetV,
		 NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_subset_end),NhlTImmediate,_NhlUSET(NULL)},

	{NhlNsfXCStartIndex,NhlCsfXCStartIndex,NhlTInteger,sizeof(int),
		Oset(x_index_start),NhlTImmediate,_NhlUSET((NhlPointer)-1)},
	{NhlNsfXCEndIndex,NhlCsfXCEndIndex,NhlTInteger,sizeof(int),
		Oset(x_index_end),NhlTImmediate,_NhlUSET((NhlPointer)-1)},
	{NhlNsfYCStartIndex,NhlCsfYCStartIndex,NhlTInteger,sizeof(int),
		Oset(y_index_start),NhlTImmediate,_NhlUSET((NhlPointer)-1)},
	{NhlNsfYCEndIndex,NhlCsfYCEndIndex,NhlTInteger,sizeof(int),
		Oset(y_index_end),NhlTImmediate,_NhlUSET((NhlPointer)-1)},

	{NhlNsfXCStride,NhlCsfXCStride,NhlTInteger,sizeof(int),
		Oset(x_stride),NhlTImmediate,_NhlUSET((NhlPointer)1)},
	{NhlNsfYCStride,NhlCsfYCStride,NhlTInteger,sizeof(int),
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

typedef enum _sfCoord { sfXCOORD, sfYCOORD} sfCoord;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/


/*
 * Function:	Monotonic
 *
 * Description:	This function decides whether an array of floats is in
 *		monotonically ascending or descending order.
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
static NhlBoolean
Monotonic
#if	__STDC__
(
	float		*flts,
	int		count
)
#else
(flts,count)
	float		*flts;
	int		count;
#endif
{
	int i;
	NhlBoolean ascends = True;

/* check ascending */

	for (i = 1; i < count; i++) {
		if (*(flts+i) < *(flts+i-1)) {
			ascends = False;
			break;
		}
	}
	if (ascends)
		return True;

/* check descending */

	for (i = count - 1; i > 0; i--) {
		if (*(flts+i) > *(flts+i-1)) {
			return False;
		}
	}
	return True;
			
}
/*
 * Function:	CoordToFloatArray
 *
 * Description:	This function converts the coordinate arrays used to
 *		specify irregular scalar field grids into float arrays
 *		suitable for use by the Contour object.
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
CoordToFloatArray
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
	NhlGenArray flt_ga;

	if (ga == NULL) 
		return NULL;
	else if (ga->typeQ == Qfloat) {
/*
 * Copy the GenArray but not the data
 */
		if ((flt_ga = _NhlCopyGenArray(ga,False)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
	}
	else if (ga->typeQ == Qint) {
		int i;
		int *ip;
		float *fp;

		ip = (int *) ga->data;

		if ((fp = (float *) NhlMalloc(ga->num_elements * 
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
		ga->my_data = True;
	}
	else {
		e_text = "%s: no conversion for type %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NrmQuarkToString(ga->typeQ));
		return NULL;
	}
	return flt_ga;
	
}

/*
 * Function:	ValidCoordArray
 *
 * Description:	This function checks the coordinate arrays used to
 *		specify irregular scalar field grids. 
 *		It checks to ensure the array is monotonic and that the
 *		number of elements is equal to the corresponding 
 *		dimension of the data array.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private
 * Returns:	True or False
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlBoolean
ValidCoordArray
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		ga,
	sfCoord			ctype,
	NhlString		entry_name
)
#else
(sfp,ga,ctype,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		ga;
	sfCoord			ctype;
	NhlString		entry_name;
#endif
{
	char *e_text;
	int len_dim;
	char *name;

	if (ctype == sfXCOORD) {
		len_dim =  sfp->exchange_dimensions ? 
			sfp->d_arr->len_dimensions[0] :
				sfp->d_arr->len_dimensions[1];
		name = NhlNsfXArray;
	}
	else {
		len_dim =  sfp->exchange_dimensions ? 
			sfp->d_arr->len_dimensions[1] :
				sfp->d_arr->len_dimensions[0];
		name = NhlNsfYArray;
	}

	if (ga->len_dimensions[0] != len_dim) {
		e_text = 
         "%s: irregular coordinate array %s requires %d elements: ignoring %s";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,name,len_dim,name);
		return False;
	}

	if (! Monotonic((float *)ga->data,ga->num_elements)) {
		e_text = 
                "%s: irregular coordinate array %s non-monotonic: ignoring %s";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		return False;
	}
	return True;
	
}


/*
 * Function:	IntDataToFloatArray
 *
 * Description:	This function converts the incoming Data GenArray of integer
 *		type into the internal float GenArray type used by the
 *		ScalarFieldFloat object. New space is always allocated 
 *		for the output data.
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
IntDataToFloatArray
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		ga,
	int			ixstart,
	int			ixend,
	int			iystart,
	int			iyend,
	NhlBoolean		do_minmax,
	NhlBoolean		do_missing,
	float			missing_value,
	float			*dmin,
	float			*dmax,
	NhlString		entry_name
)
#else
(sfp,ga,ixstart,ixend,iystart,iyend,
 do_minmax,do_missing,missing_value,dmin,dmax,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		ga;
	int			ixstart;
	int			ixend;
	int			iystart;
	int			iyend;
	NhlBoolean		do_minmax;
	NhlBoolean		do_missing;
	float			missing_value;
	float			*dmin;
	float			*dmax;
	NhlString		entry_name;
#endif
{
	char *e_text;
	int len_dims[2];
	int i,j;
	float *fp;
	int *ip;
	float tmp;
	NhlGenArray flt_ga;
	int len1;

	if (ga == NULL) 
		return NULL;

	*dmin = BIGNUMBER;
	*dmax = -BIGNUMBER;
	len1 = ga->len_dimensions[1];
	len_dims[1] = (ixend - ixstart + 1) / sfp->x_stride +
		((ixend - ixstart + 1) % sfp->x_stride > 0);
	len_dims[0] = (iyend - iystart + 1) / sfp->y_stride +
		((iyend - iystart + 1) % sfp->y_stride > 0);

	if ((fp = (float *) NhlConvertMalloc(len_dims[1] * len_dims[0] * 
					     sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}

	ip = (int *) ga->data;

	if (do_minmax && do_missing) {
		for (i = 0; i < len_dims[0]; i++) {
			for (j = 0; j < len_dims[1]; j++) {
				tmp = (float) 
					*(ip + 
					  len1*(iystart+i*sfp->y_stride) +
					  ixstart+j*sfp->x_stride);
				if (tmp != missing_value) {
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
				}
				*(fp+(i * len_dims[1] + j)) = tmp;
			}
		}
	}
	else if (do_minmax) {
		for (i = 0; i < len_dims[0]; i++) {
			for (j = 0; j < len_dims[1]; j++) {
				tmp = (float)
					*(ip + 
					  len1*(iystart+i*sfp->y_stride) +
					  ixstart+j*sfp->x_stride);
				if (tmp < *dmin) *dmin = tmp;
				if (tmp > *dmax) *dmax = tmp;
				*(fp+(i * len_dims[1] + j)) = tmp;
			}
		}
	}
	else {
		for (i = 0; i < len_dims[0]; i++) {
			for (j = 0; j < len_dims[1]; j++) {
				*(fp+(i * len_dims[1] + j)) = (float)
					*(ip + 
					  len1*(iystart+i*sfp->y_stride) +
					  ixstart+j*sfp->x_stride);
			}
		}
	}

	if ((flt_ga = _NhlCreateGenArray(fp,NhlTFloat,sizeof(float),
					 2,len_dims,False)) == NULL) {
		e_text = "%s: error creating generic array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	return flt_ga;
}


/*
 * Function:	FloatDataToFloatArray
 *
 * Description:	This function converts the incoming Data GenArray of float
 *		type into the internal float GenArray type used by the
 *		ScalarFieldFloat object. New space for data is allocated 
 *		only if a an x and/or y stride greater than unity is 
 *		specified. (The GenArray wrapper is created in either case.)
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
FloatDataToFloatArray
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		ga,
	int			ixstart,
	int			ixend,
	int			iystart,
	int			iyend,
	NhlBoolean		do_minmax,
	NhlBoolean		do_missing,
	float			missing_value,
	float			*dmin,
	float			*dmax,
	NhlBoolean		*new_data,
	NhlString		entry_name
)
#else
(sfp,ga,ixstart,ixend,iystart,iyend,
 do_minmax,do_missing,missing_value,dmin,dmax,new_data,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		ga;
	int			ixstart;
	int			ixend;
	int			iystart;
	int			iyend;
	NhlBoolean		do_minmax;
	NhlBoolean		do_missing;
	float			missing_value;
	float			*dmin;
	float			*dmax;
	NhlBoolean		*new_data;
	NhlString		entry_name;
#endif
{
	char *e_text;
	NhlGenArray flt_ga;
	int len_dims[2];
	int i,j;
	float *ifp,*fp;
	int len1;
	NhlBoolean copy_req;
	float tmp;

	*new_data = False;
	if (ga == NULL) 
		return NULL;

	*new_data = False;
	*dmin = BIGNUMBER;
	*dmax = -BIGNUMBER;
	len1 = ga->len_dimensions[1];
	len_dims[1] = (ixend - ixstart + 1) / sfp->x_stride +
		((ixend - ixstart + 1) % sfp->x_stride > 0);
	len_dims[0] = (iyend - iystart + 1) / sfp->y_stride +
		((iyend - iystart + 1) % sfp->y_stride > 0);
	copy_req = sfp->x_stride > 1 || sfp->y_stride > 1;

	if (copy_req) {

		*new_data = True;
		ifp = ((float *) ga->data);

		if ((fp = (float *) 
		     NhlConvertMalloc(len_dims[1] * len_dims[0] * 
				      sizeof(float))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		if (do_minmax && do_missing) {
			for (i = 0; i < len_dims[0]; i++) {
				for (j = 0; j < len_dims[1]; j++) {
					tmp = *(ifp + len1 *
						(iystart+i*sfp->y_stride) +
						ixstart+j*sfp->x_stride);
					if (tmp != missing_value) {
						if (tmp < *dmin) *dmin = tmp;
						if (tmp > *dmax) *dmax = tmp;
					}
					*(fp+(i * len_dims[1] + j)) = tmp;
				}
			}
		}
		else if (do_minmax) {
			for (i = 0; i < len_dims[0]; i++) {
				for (j = 0; j < len_dims[1]; j++) {
					tmp = *(ifp + len1 *
						(iystart+i*sfp->y_stride) +
						ixstart+j*sfp->x_stride);
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
					*(fp+(i * len_dims[1] + j)) = tmp;
				}
			}
		}
		else {
			for (i = 0; i < len_dims[0]; i++) {
				for (j = 0; j < len_dims[1]; j++) {
					*(fp+(i * len_dims[1] + j)) = 
					     *(ifp + 
					       len1*(iystart+i*sfp->y_stride) +
					       ixstart+j*sfp->x_stride);
				}
			}
		}
		if ((flt_ga = _NhlCreateGenArray(fp,NhlTFloat,sizeof(float),
						 2,len_dims,False)) == NULL) {
			e_text = "%s: error creating generic array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		return flt_ga;
	}
	else {

		ifp = ((float *) ga->data);

		if (do_minmax && do_missing) {
			for (i = 0; i < len_dims[0]; i++) {
				for (j = 0; j < len_dims[1]; j++) {
					tmp = *(ifp + len1 *
						(iystart+i*sfp->y_stride) +
						ixstart+j*sfp->x_stride);
					if (tmp != missing_value) {
						if (tmp < *dmin) *dmin = tmp;
						if (tmp > *dmax) *dmax = tmp;
					}
				}
			}
		}
		else if (do_minmax) {
			for (i = 0; i < len_dims[0]; i++) {
				for (j = 0; j < len_dims[1]; j++) {
					tmp = *(ifp + len1 *
						(iystart+i*sfp->y_stride) +
						ixstart+j*sfp->x_stride);
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
				}
			}
		}
/*
 * Copy the GenArray but not the data
 */
		if ((flt_ga = _NhlCopyGenArray(ga,False)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		return flt_ga;
	}
}


/*
 * Function:	IntDataToFloatArrayExchDim
 *
 * Description:	This function converts the incoming Data GenArray of integer
 *		type into the internal float GenArray type used by the
 *		ScalarFieldFloat object. The array dimensions are exchanged.
 *		New space is allocated for the output data.
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
IntDataToFloatArrayExchDim
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		ga,
	int			ixstart,
	int			ixend,
	int			iystart,
	int			iyend,
	NhlBoolean		do_minmax,
	NhlBoolean		do_missing,
	float			missing_value,
	float			*dmin,
	float			*dmax,
	NhlString		entry_name
)
#else
(sfp,ga,ixstart,ixend,iystart,iyend,
 do_minmax,do_missing,missing_value,dmin,dmax,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		ga;
	int			ixstart;
	int			ixend;
	int			iystart;
	int			iyend;
	NhlBoolean		do_minmax;
	NhlBoolean		do_missing;
	float			missing_value;
	float			*dmin;
	float			*dmax;
	NhlString		entry_name;
#endif
{
	char *e_text;
	int len_dims[2];
	int i,j;
	float *fp;
	int *ip;
	float tmp;
	NhlGenArray flt_ga;
	int len1;

	if (ga == NULL) 
		return NULL;

	*dmin = BIGNUMBER;
	*dmax = -BIGNUMBER;
	len1 = ga->len_dimensions[1];
/*
 * Assign the dimension length according to the lengths needed by
 * the new output array (0-slow,1-fast). This is , of course, the reverse
 * of the input array.
 */
	len_dims[1] = (ixend - ixstart + 1) / sfp->x_stride +
		((ixend - ixstart + 1) % sfp->x_stride > 0);
	len_dims[0] = (iyend - iystart + 1) / sfp->y_stride +
		((iyend - iystart + 1) % sfp->y_stride > 0);

	if ((fp = (float *) NhlConvertMalloc(len_dims[1] * len_dims[0] * 
					     sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}

	ip = (int *) ga->data;

	if (do_minmax && do_missing) {
		for (i = 0; i < len_dims[1]; i++) {
			for (j = 0; j < len_dims[0]; j++) {
				tmp = (float) 
					*(ip + 
					  len1*(ixstart+i*sfp->x_stride) +
					  iystart+j*sfp->y_stride);
				if (tmp != missing_value) {
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
				}
				*(fp+(j * len_dims[1] + i)) = tmp;
			}
		}
	}
	else if (do_minmax) {
		for (i = 0; i < len_dims[1]; i++) {
			for (j = 0; j < len_dims[0]; j++) {
				tmp = (float)
					*(ip + 
					  len1*(ixstart+i*sfp->x_stride) +
					  iystart+j*sfp->y_stride);
				if (tmp < *dmin) *dmin = tmp;
				if (tmp > *dmax) *dmax = tmp;
				*(fp+(j * len_dims[1] + i)) = tmp;
			}
		}
	}
	else {
		for (i = 0; i < len_dims[1]; i++) {
			for (j = 0; j < len_dims[0]; j++) {
				*(fp+(j * len_dims[1] + i)) = (float)
					*(ip + 
					  len1*(ixstart+i*sfp->x_stride) +
					  iystart+j*sfp->y_stride);
			}
		}
	}

	if ((flt_ga = _NhlCreateGenArray(fp,NhlTFloat,sizeof(float),
					 2,len_dims,False)) == NULL) {
		e_text = "%s: error creating generic array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	return flt_ga;
}


/*
 * Function:	FloatDataToFloatArrayExchDim
 *
 * Description:	This function converts the incoming Data GenArray of float
 *		type into the internal float GenArray type used by the
 *		ScalarFieldFloat object. The array dimensions are exchanged.
 *		New space is always allocated for the output data.
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
FloatDataToFloatArrayExchDim
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		ga,
	int			ixstart,
	int			ixend,
	int			iystart,
	int			iyend,
	NhlBoolean		do_minmax,
	NhlBoolean		do_missing,
	float			missing_value,
	float			*dmin,
	float			*dmax,
	NhlString		entry_name
)
#else
(sfp,ga,ixstart,ixend,iystart,iyend,
 do_minmax,do_missing,missing_value,dmin,dmax,new_data,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		ga;
	int			ixstart;
	int			ixend;
	int			iystart;
	int			iyend;
	NhlBoolean		do_minmax;
	NhlBoolean		do_missing;
	float			missing_value;
	float			*dmin;
	float			*dmax;
	NhlString		entry_name;
#endif
{
	char *e_text;
	NhlGenArray flt_ga;
	int len_dims[2];
	int i,j;
	float *ifp,*fp;
	int len1;
	float tmp;

	if (ga == NULL) 
		return NULL;

	*dmin = BIGNUMBER;
	*dmax = -BIGNUMBER;
	len1 = ga->len_dimensions[1];
/*
 * Assign the dimension length according to the lengths needed by
 * the new output array (0-slow,1-fast). This is , of course, the reverse
 * of the input array.
 */
	len_dims[1] = (ixend - ixstart + 1) / sfp->x_stride +
		((ixend - ixstart + 1) % sfp->x_stride > 0);
	len_dims[0] = (iyend - iystart + 1) / sfp->y_stride +
		((iyend - iystart + 1) % sfp->y_stride > 0);


	ifp = ((float *) ga->data);

	if ((fp = (float *) 
	     NhlConvertMalloc(len_dims[1] * len_dims[0] * 
			      sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if (do_minmax && do_missing) {
		for (i = 0; i < len_dims[1]; i++) {
			for (j = 0; j < len_dims[0]; j++) {
				tmp = *(ifp + 
					len1*(ixstart+i*sfp->x_stride) +
					iystart+j*sfp->y_stride);
				if (tmp != missing_value) {
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
				}
				*(fp+(j * len_dims[1] + i)) = tmp;
			}
		}
	}
	else if (do_minmax) {
		for (i = 0; i < len_dims[1]; i++) {
			for (j = 0; j < len_dims[0]; j++) {
				tmp = *(ifp + 
					len1*(ixstart+i*sfp->x_stride) +
					iystart+j*sfp->y_stride);
				if (tmp < *dmin) *dmin = tmp;
				if (tmp > *dmax) *dmax = tmp;
				*(fp+(j * len_dims[1] + i)) = tmp;
			}
		}
	}
	else {
		for (i = 0; i < len_dims[1]; i++) {
			for (j = 0; j < len_dims[0]; j++) {
				*(fp+(j * len_dims[1] + i)) = 
					*(ifp + 
					  len1*(ixstart+i*sfp->x_stride) +
					  iystart+j*sfp->y_stride);
			}
		}
	}
	if ((flt_ga = _NhlCreateGenArray(fp,NhlTFloat,sizeof(float),
					 2,len_dims,False)) == NULL) {
		e_text = "%s: error creating generic array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	return flt_ga;
}


/*
 * Function:	GetDataBounds
 *
 * Description:	Determines the data bounds of one coordinate axis of the
 *		data array
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
static NhlErrorTypes
GetDataBounds
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	sfCoord			ctype,
	float			*cstart,
	float			*cend,
	NhlString		entry_name
)
#else
(sfp,ctype,cstart,cend,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	sfCoord			ctype;
	float			*cstart;
	float			*cend;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;

	if (ctype == sfXCOORD) {

		if (sfp->x_start != NULL)
			*cstart = *((float *)sfp->x_start->data);
		else if (sfp->x_subset_start != NULL)
			*cstart = *((float *)sfp->x_subset_start->data);
		else 
			*cstart = 0.0;

		if (sfp->x_end != NULL)
			*cend = *((float *)sfp->x_end->data);
		else if (sfp->x_subset_end != NULL)
			*cend = *((float *)sfp->x_subset_end->data);
		else 
			*cend = 1.0;
	}
	else {
		if (sfp->y_start != NULL)
			*cstart = *((float *)sfp->y_start->data);
		else if (sfp->y_subset_start != NULL)
			*cstart = *((float *)sfp->y_subset_start->data);
		else 
			*cstart = 0.0;
		
		if (sfp->y_end != NULL)
			*cend = *((float *)sfp->y_end->data);
		else if (sfp->y_subset_end != NULL)
			*cend = *((float *)sfp->y_subset_end->data);
		else 
			*cend = 1.0;
	}
	return ret;
}

/*
 * Function:	GetIndexBounds
 *
 * Description:	Determines the subset boundary array indexes of one 
 *		coordinate of the data array. (These values may be 
 *		overridden if subset mode is not by index.)
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
static NhlErrorTypes
GetIndexBounds
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	sfCoord			ctype,
	int			*icstart,
	int			*icend,
	NhlString		entry_name
)
#else
(sfp,icstart,icend,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	sfCoord			ctype;
	int			*icstart;
	int			*icend;
	NhlString		entry_name;
#endif
{
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR;
	int			max_index;
			
	if (ctype == sfXCOORD) {

		max_index = sfp->exchange_dimensions ?
			sfp->d_arr->len_dimensions[0] - 1:
			sfp->d_arr->len_dimensions[1] - 1;

		*icstart = (sfp->x_index_start < 0) ? 
			0 : MIN(sfp->x_index_start,max_index);
		if (sfp->x_index_end > max_index) {
			e_text = 
		      "%s: X index end exceeds data boundaries: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(NhlWARNING,ret);
			sfp->x_index_end = max_index;
		}
		*icend = (sfp->x_index_end < 0) ? 
			max_index  :  sfp->x_index_end;

		if (*icend - *icstart < 2) {
			e_text = 
	       "%s: X index end not enough greater than start: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(NhlWARNING,ret);
			*icstart = sfp->x_index_start = 0;
			*icend = sfp->x_index_end = max_index;
		}

	}
	else {

		max_index = sfp->exchange_dimensions ?
			sfp->d_arr->len_dimensions[1] - 1:
			sfp->d_arr->len_dimensions[0] - 1;

		*icstart = (sfp->y_index_start < 0) ? 
			0 : MIN(sfp->y_index_start,max_index); 
		if (sfp->y_index_end > max_index) {
			e_text = 
		      "%s: Y index end exceeds data boundaries: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(NhlWARNING,ret);
			sfp->y_index_end = max_index;
		}
		*icend = (sfp->y_index_end < 0) ? 
			max_index : sfp->y_index_end;

		if (*icend - *icstart < 2) {
			e_text = 
	       "%s: Y index end not enough greater than start: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(NhlWARNING,ret);
			*icstart = sfp->y_index_start = 0;
			*icend = sfp->y_index_end = max_index;
		}
	}

	return ret;

}

/*
 * Function:	GetSubsetBounds
 *
 * Description:	Depending on the value of the NhlNsfSubsetByIndex resource,
 *		determines one coordinate of the data array subset, 
 *		based either on the 
 *		IndexStart/End resources or the SubsetStart/End resources.
 *		In either case the true clipping rectangle is determined
 *		based on the calculated or user assigned array index 
 *		start/end values. The clipping boundaries may not be exactly
 *		what the user asked for due to the truncation involved in
 *		converting from data points to integer array indexes, but
 *		the specified data points are guaranteed to be included.
 *
 * In Args:	sfp
 *		ctype
 *		cstart,cend
 *		entry_name
 * In/Out Args: icstart,icend
 * Out Args:	sxstart,sxend
 *
 * Scope:	private
 * Returns:	NhlGenArray or NULL on error
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
GetSubsetBounds
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	sfCoord			ctype,
	float			cstart,
	float			cend,
	int			*icstart,
	int			*icend,
	float			*scstart,
	float			*scend,
	NhlString		entry_name
)
#else
(sfp,ctype,cstart,cend,icstart,icend,scstart,scend,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	sfCoord			ctype;
	float			cstart;
	float			cend;
	int			*icstart;
	int			*icend;
	float			*scstart;
	float			*scend;
	NhlString		entry_name;
#endif
{
	char		*e_text;
	NhlErrorTypes   ret = NhlNOERROR;
	float		flt_inc;
	NhlBoolean	rev;
	float		drange;
	int		range;
	NhlGenArray	subset_start, subset_end;
	NhlBoolean	nullstart = False, nullend = False;
	char		*c_name;
	

	if (ctype == sfXCOORD) {
		range = sfp->exchange_dimensions ? 
			sfp->d_arr->len_dimensions[0] - 1 :
			sfp->d_arr->len_dimensions[1] - 1;
		subset_start = sfp->x_subset_start;
		subset_end = sfp->x_subset_end;
		c_name = "X coordinate";
	}
	else {
		range = sfp->exchange_dimensions ? 
			sfp->d_arr->len_dimensions[1] - 1 :
			sfp->d_arr->len_dimensions[0] - 1;
		subset_start = sfp->y_subset_start;
		subset_end = sfp->y_subset_end;
		c_name = "Y coordinate";
	}

	if (! sfp->subset_by_index) {

		rev = cstart > cend;
		if (subset_start != NULL)
			*scstart = rev ? 
				MIN(cstart,*((float *)subset_start->data)): 
				MAX(cstart,*((float *)subset_start->data));
		else {
			*scstart = cstart;
			nullstart = True;
		}

		if (subset_end != NULL)
			*scend = rev ?
				MAX(cend,*((float *)subset_end->data)):
				MIN(cend,*((float *)subset_end->data));
		else {
			*scend = cend;
			nullend = True;
		}

		if (rev != (*scstart > *scend)) {
			e_text = 
     "%s: %s  start/end subset order opposed to %s start/end order: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  e_text,entry_name,c_name,c_name);
			ret = MIN(NhlWARNING,ret);
			*scstart = cstart;
			if (! nullstart) 
				*((float *)subset_start->data) = cstart;
			*scend = cend;
			if (! nullstart) 
				*((float *)subset_end->data) = cstart;
		}

/*
 * The index endpoints are chosen to include the subset data endpoints.
 */
		drange = cend - cstart;
		*icstart = MAX(0,floor(((*scstart - cstart) /drange) * range));
		*icend = MIN(range,ceil(((*scend - cstart) / drange) * range));

		if (*icend - *icstart < 2) {
			e_text = 
		        "%s: %s subset data range not large enough: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  c_name,c_name);
			ret = MIN(NhlWARNING,ret);
			*scstart = cstart;
			if (! nullstart) 
				*((float *)subset_start->data) = cstart;
			*scend = cend;
			if (! nullstart) 
				*((float *)subset_end->data) = cstart;
			*icstart = 0;
			*icend = range;
		}
	}

	flt_inc = (cend - cstart) / range;

	*scstart = cstart + flt_inc * *icstart;
	*scend = cstart + flt_inc * *icend;

	return ret;
}


/*
 * Function:	GetSubsetBoundsIrregular
 *
 * Description:	Depending on the value of the NhlNsfSubsetByIndex resource,
 *		determines one coordinate of the data array subset, 
 *		based either on the 
 *		IndexStart/End resources or the SubsetStart/End resources.
 *		In either case the true clipping rectangle is determined
 *		based on the calculated or user assigned array index 
 *		start/end values. The clipping boundaries may not be exactly
 *		what the user asked for due to the truncation involved in
 *		converting from data points to integer array indexes, but
 *		the specified data points are guaranteed to be included.
 *
 * In Args:	sfp
 *		ctype
 *		cstart,cend
 *		entry_name
 * In/Out Args: icstart,icend
 * Out Args:	sxstart,sxend
 *
 * Scope:	private
 * Returns:	NhlGenArray or NULL on error
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
GetSubsetBoundsIrregular
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		c_array,
	sfCoord			ctype,
	float			*cstart,
	float			*cend,
	int			*icstart,
	int			*icend,
	float			*scstart,
	float			*scend,
	NhlString		entry_name
)
#else
(sfp,c_array,ctype,cstart,cend,icstart,icend,scstart,scend,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		c_array;
	sfCoord			ctype;
	float			*cstart;
	float			*cend;
	int			*icstart;
	int			*icend;
	float			*scstart;
	float			*scend;
	NhlString		entry_name;
#endif
{
	char		*e_text;
	NhlErrorTypes   ret = NhlNOERROR;
	NhlBoolean	rev;
	int		i, len;
	NhlGenArray	subset_start, subset_end;
	NhlBoolean	nullstart = False, nullend = False;
	char		*c_name;
	float		*fp, *nfp;

	if (ctype == sfXCOORD) {
		subset_start = sfp->x_subset_start;
		subset_end = sfp->x_subset_end;
		c_name = "X coordinate";
	}
	else {
		subset_start = sfp->y_subset_start;
		subset_end = sfp->y_subset_end;
		c_name = "Y coordinate";
	}

	len = c_array->len_dimensions[0];
	fp = (float *) c_array->data;
	*cstart = fp[0];
	*cend = fp[len-1];

	if (! sfp->subset_by_index) {

		rev = *cstart > *cend;
		if (subset_start != NULL)
			*scstart = rev ? 
				MIN(*cstart,*((float *)subset_start->data)): 
				MAX(*cstart,*((float *)subset_start->data));
		else {
			*scstart = *cstart;
			nullstart = True;
		}

		if (subset_end != NULL)
			*scend = rev ?
				MAX(*cend,*((float *)subset_end->data)):
				MIN(*cend,*((float *)subset_end->data));
		else {
			*scend = *cend;
			nullend = True;
		}

		if (rev != (*scstart > *scend)) {
			e_text = 
      "%s: %s start/end subset order opposed to %s start/end order: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  e_text,entry_name,c_name,c_name);
			ret = MIN(NhlWARNING,ret);
			*scstart = *cstart;
			if (! nullstart) 
				*((float *)subset_start->data) = *cstart;
			*scend = *cend;
			if (! nullstart) 
				*((float *)subset_end->data) = *cstart;
		}

		if (! rev) { 
			for (i = 0; i < len; i++) {
				if (*scstart >= *(fp + i)) {
					*icstart = i;
					break;
				}
			}
			for (i = len - 1; i >= 0; i--) {
				if (*scend <= *(fp + i)) {
					*icend = i;
					break;
				}
			}
		}
		else {
			for (i = 0; i < len; i++) {
				if (*scstart > *(fp + i)) {
					*icstart = MAX(i-1,0);
					break;
				}
			}
			for (i = len - 1; i >= 0; i--) {
				if (*scend < *(fp + i)) {
					*icend = MIN(i+1,len-1);
					break;
				}
			}
		}

		if (*icend - *icstart < 2) {
			e_text = 
		        "%s: %s subset data range not large enough: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  c_name,c_name);
			ret = MIN(NhlWARNING,ret);
			*scstart = *cstart;
			if (! nullstart) 
				*((float *)subset_start->data) = *cstart;
			*scend = *cend;
			if (! nullstart) 
				*((float *)subset_end->data) = *cstart;
			*icstart = 0;
			*icend = len - 1;
		}
	}

	*scstart = fp[*icstart];
	*scend = fp[*icend];

/*
 * If the data is a subset of the complete array, copy the relevant
 * part of the irregular coordinate array to a new array, freeing the 
 * old data if it belongs to the GenArray.
 */
	if (*icstart > 0 || *icend < len) {
		int nlen = *icend - *icstart + 1;
		if ((nfp = (float *)
		     NhlMalloc(nlen * sizeof(float))) == NULL) { 
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		memcpy(nfp,&(fp[*icstart]),nlen * sizeof(float));
	
		if (c_array->my_data)
			NhlFree(c_array->data);
		c_array->data = (void *) nfp;
		c_array->num_elements = c_array->len_dimensions[0] = nlen;
		c_array->my_data = True;
	}

	return ret;
}

/*
 * Function:	GetCoordBounds
 *
 * Description:	
 *
 * In Args:	sfp
 *              ctype
 *		entry_name
 *
 * Out Args:	cstart,cend
 *		icstart,icend
 *		scstart,scend
 *
 * Scope:	private
 * Returns:	NhlGenArray or NULL on error
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
GetCoordBounds
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	sfCoord			ctype,
	float			*cstart,
	float			*cend,
	int			*icstart,
	int			*icend,
	float			*scstart,
	float			*scend,
	NhlString		entry_name
)
#else
(sfp,ctype,cstart,cend,icstart,icend,scstart,scend,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	sfCoord			ctype;
	float			*cstart;
	float			*cend;
	int			*icstart;
	int			*icend;
	float			*scstart;
	float			*scend;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes   ret = NhlNOERROR, subret = NhlNOERROR;

	subret = GetDataBounds(sfp,ctype,cstart,cend,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;

	subret = GetIndexBounds(sfp,ctype,icstart,icend,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;

	subret = GetSubsetBounds(sfp,ctype,*cstart,*cend,
				 icstart,icend,scstart,scend,entry_name);

	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;

	return ret;
}


/*
 * Function:	GetCoordBoundsIrregular
 *
 * Description:	For irregular coordinates, the first and last array elements
 *		define the start and end of the data space; the x/y start/end 
 *		resources are ignored. However, it is still possible to create
 *		subsets of the 	data using either the index or the subset 
 *		start/end resources.
 *		The irregular coordinate array is valid in that it is known
 *		to be monotonic and has the correct number of elements
 *
 * In Args:	sfp
 *              ctype
 *		entry_name
 *
 * Out Args:	cstart,cend
 *		icstart,icend
 *		scstart,scend
 *
 * Scope:	private
 * Returns:	NhlGenArray or NULL on error
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
GetCoordBoundsIrregular
#if	__STDC__
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		c_array,
	sfCoord			ctype,
	float			*cstart,
	float			*cend,
	int			*icstart,
	int			*icend,
	float			*scstart,
	float			*scend,
	NhlString		entry_name
)
#else
(sfp,c_array,ctype,cstart,cend,icstart,icend,scstart,scend,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		c_array;
	sfCoord			ctype;
	float			*cstart;
	float			*cend;
	int			*icstart;
	int			*icend;
	float			*scstart;
	float			*scend;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes   ret = NhlNOERROR, subret = NhlNOERROR;

	subret = GetIndexBounds(sfp,ctype,icstart,icend,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;

	subret = GetSubsetBoundsIrregular(sfp,c_array,ctype,
					  cstart,cend,icstart,icend,
					  scstart,scend,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;
	
	return ret;
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
	float			xstart,xend,ystart,yend;
	float			sxstart,sxend,systart,syend;
	int			ixstart,ixend,iystart,iyend;
	NhlBoolean		xirr = False, yirr = False;
	int			begin,fast_dim,fast_len,slow_len;
	float			missing_value;
	NhlBoolean		do_minmax,do_missing,new_data;
	float			dmin,dmax,tmin,tmax;

/*
 * Check input and retrieve a pointer to the data object
 */
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
	
/*
 * Convert, validate, and set the X and Y irregular coordinate arrays,
 * if defined.
 */
	if (sfp->x_arr != NULL && sfp->x_arr->num_elements > 0) {
		if ((x_arr = CoordToFloatArray(sfp->x_arr,
					       entry_name)) == NULL) {
				return NhlFATAL;
		}
		if (ValidCoordArray(sfp,x_arr,sfXCOORD,entry_name)) {
			NhlSetSArg(&sargs[nargs++],NhlNsfXArray,x_arr);
			xirr = True;
		}
		else {
			NhlFreeGenArray(x_arr);
			x_arr = NULL;
		}
	}
		     
	if (sfp->y_arr != NULL && sfp->y_arr->num_elements > 0) {
		if ((y_arr = CoordToFloatArray(sfp->y_arr,
					       entry_name)) == NULL) {
				return NhlFATAL;
		}
		if (ValidCoordArray(sfp,y_arr,sfYCOORD,entry_name)) {
			NhlSetSArg(&sargs[nargs++],NhlNsfYArray,y_arr);
			yirr = True;
		}
		else {
			NhlFreeGenArray(y_arr);
			y_arr = NULL;
		}
	}

	if (! xirr) {
		subret = GetCoordBounds(sfp,sfXCOORD,&xstart,&xend,
					&ixstart,&ixend,&sxstart,&sxend,
					entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
	}
	else {
		subret = GetCoordBoundsIrregular(sfp,x_arr,sfXCOORD,
						 &xstart,&xend,
						 &ixstart,&ixend,
						 &sxstart,&sxend,
						 entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
	}
	if (! yirr) {
		subret = GetCoordBounds(sfp,sfYCOORD,&ystart,&yend,
					&iystart,&iyend,&systart,&syend,
					entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
	}
	else {
		subret = GetCoordBoundsIrregular(sfp,y_arr,sfYCOORD,
						 &ystart,&yend,
						 &iystart,&iyend,
						 &systart,&syend,
						 entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
	}
/*
 * Set flags to tell the array conversion routines whether to find
 * data max and mins (and if so whether to check for missing values) 
 * during the conversion process.
 */
	if (sfp->missing_value == NULL) {
		do_missing = False;
		missing_value = 0.0;
	}
	else {
		do_missing = True;
		missing_value = *((float *)sfp->missing_value->data);
	}
	do_minmax =  (sfp->data_max == NULL || sfp->data_min == NULL) ?
		True : False;

/*
 * Convert the arrays
 */
	if (sfp->d_arr->typeQ == Qint) {
		if (! sfp->exchange_dimensions) {
			if ((d_arr = 
			     IntDataToFloatArray(sfp,sfp->d_arr,
						 ixstart,ixend,iystart,iyend,
						 do_minmax,do_missing,
						 missing_value,&dmin,&dmax,
						 entry_name)) == NULL) {
				return NhlFATAL;
			}
		}
		else {
			if ((d_arr = 
			  IntDataToFloatArrayExchDim(sfp,sfp->d_arr,
						     ixstart,ixend,
						     iystart,iyend,
						     do_minmax,do_missing,
						     missing_value,&dmin,&dmax,
						     entry_name)) == NULL) {
				return NhlFATAL;
			}
		}
		new_data = True;
	} else if (sfp->d_arr->typeQ == Qfloat) {
		if (! sfp->exchange_dimensions) {
			if ((d_arr = 
			     FloatDataToFloatArray(sfp,sfp->d_arr,
						   ixstart,ixend,iystart,iyend,
						   do_minmax,do_missing,
						   missing_value,&dmin,&dmax,
						   &new_data,
						   entry_name)) == NULL) {
				return NhlFATAL;
			}
		}
		else {
			if ((d_arr = 
			     FloatDataToFloatArrayExchDim(sfp,sfp->d_arr,
						   ixstart,ixend,iystart,iyend,
						   do_minmax,do_missing,
						   missing_value,&dmin,&dmax,
						   entry_name)) == NULL) {
				return NhlFATAL;
			}
			new_data = True;
		}
	}
	else {
		e_text = "%s: no conversion for type %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NrmQuarkToString(sfp->d_arr->typeQ));
		return NULL;
	}

/*
 * If the user passed in a float array and stride values are all unity,
 * then the array is not copied. Set values that will be used to 
 * indicate to the low level routines what portion of the array to use.
 * If a copy was made, the entire array will be utilitized.
 */
	if (! new_data) {
		begin = iystart * d_arr->len_dimensions[1] + ixstart;
		fast_dim = d_arr->len_dimensions[1];
		fast_len = ixend - ixstart + 1;
		slow_len = iyend - iystart + 1;
	}
	else {
		begin = 0;
		fast_dim = d_arr->len_dimensions[1];
		fast_len = d_arr->len_dimensions[1];
		slow_len = d_arr->len_dimensions[0];
	}
/*
 * If the user explicitly sets the data min/max values, make sure the max
 * is greater than the min. If not, WARN and use the actual max and min
 * values. Otherwise pass on without interpretation to the plot object.
 */
	if (sfp->data_min != NULL && sfp->data_max != NULL) {
		tmin = *((float *)sfp->data_min->data);
		tmax = *((float *)sfp->data_max->data);
		if (tmax <= tmin || tmax <= dmin) {
			e_text = "%s: not using %s value: out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NrmQuarkToString(sfp->data_max->typeQ));
		}
		else {
			dmax = tmax;
		}
		if (tmin >= dmax) {
			e_text = "%s: not using %s value: out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NrmQuarkToString(sfp->data_min->typeQ));
		}
		else {
			dmin = tmin;
		}
	}
	else if (sfp->data_max != NULL) {
		tmax = *((float *)sfp->data_max->data);
		if (tmax <= dmin) {
			e_text = "%s: not using %s value: out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NrmQuarkToString(sfp->data_max->typeQ));
		}
		else {
			dmax = tmax;
		}
	}
	else if (sfp->data_min != NULL) {
		tmin = *((float *)sfp->data_min->data);
		if (tmin >= dmax) {
			e_text = "%s: not using %s value: out of range";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NrmQuarkToString(sfp->data_min->typeQ));
		}
		else {
			dmin = tmin;
		}
	}
	
/*
 * Set all remaining resource arguments
 */
	NhlSetSArg(&sargs[nargs++],NhlNsfDataArray,d_arr);
 
	if (do_missing) {
		NhlSetSArg(&sargs[nargs++],NhlNsfMissingValueF,missing_value);
	}
	NhlSetSArg(&sargs[nargs++],NhlNsfDataMinF,dmin);
	NhlSetSArg(&sargs[nargs++],NhlNsfDataMaxF,dmax);
		     
	NhlSetSArg(&sargs[nargs++],NhlNsfXCStartF,sxstart);
	NhlSetSArg(&sargs[nargs++],NhlNsfXCEndF,sxend);
	NhlSetSArg(&sargs[nargs++],NhlNsfYCStartF,systart);
	NhlSetSArg(&sargs[nargs++],NhlNsfYCEndF,syend);

	NhlSetSArg(&sargs[nargs++],NhlNsfDataBegin,begin);
	NhlSetSArg(&sargs[nargs++],NhlNsfDataFastDim,fast_dim);
	NhlSetSArg(&sargs[nargs++],NhlNsfDataFastLen,fast_len);
	NhlSetSArg(&sargs[nargs++],NhlNsfDataSlowLen,slow_len);

/*
 * Create a scalar field float data object
 */
	ret = NhlALCreate(to->data.ptrval,"no.name",
			  NhlscalarFieldFloatLayerClass,
			  sfl->base.id,sargs,nargs);

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

	if (sfp->x_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_start,
					NhlNsfXCStartV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_start = ga;
	}
	if (sfp->x_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_end,
					NhlNsfXCEndV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_end = ga;
	}


	if (sfp->y_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_start,
					NhlNsfYCStartV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_start = ga;
	}
	if (sfp->y_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_end,
					NhlNsfYCEndV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_end = ga;
	}


	if (sfp->x_subset_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_subset_start,
					NhlNsfXCStartSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_subset_start = ga;
	}
	if (sfp->x_subset_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_subset_end,
					NhlNsfXCEndSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_subset_end = ga;
	}


	if (sfp->y_subset_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_subset_start,
					NhlNsfYCStartSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_start = ga;
	}
	if (sfp->y_subset_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_subset_end,
					NhlNsfYCEndSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_end = ga;
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

	if (sfp->x_start != osfp->x_start) {
		subret = CheckCopyVType(&osfp->x_start,sfp->x_start,
					NhlNsfXCStartV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_start = osfp->x_start;
		status = True;
	}
	if (sfp->x_end != osfp->x_end) {
		subret = CheckCopyVType(&osfp->x_end,sfp->x_end,
					NhlNsfXCEndV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_end = osfp->x_end;
		status = True;
	}

	if (sfp->y_start != osfp->y_start) {
		subret = CheckCopyVType(&osfp->y_start,sfp->y_start,
					NhlNsfYCStartV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_start = osfp->y_start;
		status = True;
	}
	if (sfp->y_end != osfp->y_end) {
		subret = CheckCopyVType(&osfp->y_end,sfp->y_end,
					NhlNsfYCEndV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_end = osfp->y_end;
		status = True;
	}


	if (sfp->x_subset_start != osfp->x_subset_start) {
		subret = CheckCopyVType(&osfp->x_subset_start,
					sfp->x_subset_start,
					NhlNsfXCStartSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_subset_start = osfp->x_subset_start;
		status = True;
	}
	if (sfp->x_subset_end != osfp->x_subset_end) {
		subret = CheckCopyVType(&osfp->x_subset_end,sfp->x_subset_end,
					NhlNsfXCEndSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_subset_end = osfp->x_subset_end;
		status = True;
	}


	if (sfp->y_subset_start != osfp->y_subset_start) {
		subret = CheckCopyVType(&osfp->y_subset_start,
					sfp->y_subset_start,
					NhlNsfYCStartSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_start = osfp->y_subset_start;
		status = True;
	}
	if (sfp->y_subset_end != osfp->y_subset_end) {
		subret = CheckCopyVType(&osfp->y_subset_end,sfp->y_subset_end,
					NhlNsfYCEndSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_end = osfp->y_subset_end;
		status = True;
	}

	if (sfp->x_index_start != osfp->x_index_start) 
		status = True;
	if (sfp->x_index_end != osfp->x_index_end)
		status = True;
	if (sfp->y_index_start != osfp->y_index_start)
		status = True;
	if (sfp->y_index_end != osfp->y_index_end)
		status = True;
	if (sfp->x_stride != osfp->x_stride)
		status = True;
	if (sfp->y_stride != osfp->y_stride)
		status = True;

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
	NhlFreeGenArray(sfp->x_start);
	NhlFreeGenArray(sfp->x_end);
	NhlFreeGenArray(sfp->y_start);
	NhlFreeGenArray(sfp->y_end);
	NhlFreeGenArray(sfp->x_subset_start);
	NhlFreeGenArray(sfp->x_subset_end);
	NhlFreeGenArray(sfp->y_subset_start);
	NhlFreeGenArray(sfp->y_subset_end);

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
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	NhlScalarFieldFloatLayer	nsf = (NhlScalarFieldFloatLayer) new;
	NhlScalarFieldFloatLayerPart	*nsfp =
			(NhlScalarFieldFloatLayerPart *) &nsf->sfieldfloat;

	if (nsfp->d_arr == NULL) {
		e_text = "%s: data array resource %s must be set";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNsfDataArray);
		return NhlFATAL;
	}
	if (! nsfp->missing_value_set) 
		nsfp->missing_value = 0.0;

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
	char			*e_text;

/*
 * This function should never be called.
 */

	e_text = "%s: this function should never be called";
	NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
	return NhlFATAL;

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
