/*
 *      $Id: ScalarField.c,v 1.17 1995-11-21 20:18:58 dbrown Exp $
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
#include <math.h>
#include <ncarg/hlu/ScalarFieldP.h>

/************************************************************************
*									*
*	ScalarField Class declarations					*
*									*
************************************************************************/

/*
 * Resource Declarations
 */

#define	Oset(field)	NhlOffset(NhlScalarFieldLayerRec,sfield.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNsfDataArray,NhlCsfDataArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(d_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNsfXArray,NhlCsfXArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNsfYArray,NhlCsfYArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNsfSubsetByIndex,NhlCsfSubsetByIndex,
		 NhlTBoolean,sizeof(NhlBoolean),Oset(subset_by_index),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNsfCopyData,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(copy_arrays),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{NhlNsfExchangeDimensions,NhlCsfExchangeDimensions,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(exchange_dimensions),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),0,NULL},
	{NhlNsfMissingValueV,NhlCsfMissingValueV,NhlTVariable,
		 sizeof(NhlGenArray),Oset(missing_value),NhlTImmediate,
		 _NhlUSET(NULL),0,NULL},
	{NhlNsfDataMinV,NhlCsfDataMinV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(data_min),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNsfDataMaxV,NhlCsfDataMaxV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(data_max),NhlTImmediate,_NhlUSET(NULL),0,NULL},

	{NhlNsfXCStartV,NhlCsfXCStartV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_start),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNsfXCEndV,NhlCsfXCEndV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_end),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNsfYCStartV,NhlCsfYCStartV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_start),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNsfYCEndV,NhlCsfYCEndV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_end),NhlTImmediate,_NhlUSET(NULL),0,NULL},

	{NhlNsfXCStartSubsetV,NhlCsfXCStartSubsetV,
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_subset_start),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNsfXCEndSubsetV,NhlCsfXCEndSubsetV,
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_subset_end),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNsfYCStartSubsetV,NhlCsfYCStartSubsetV,
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_subset_start),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNsfYCEndSubsetV,NhlCsfYCEndSubsetV,
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_subset_end),NhlTImmediate,_NhlUSET(NULL),0,NULL},

	{NhlNsfXCStartIndex,NhlCsfXCStartIndex,NhlTInteger,sizeof(int),
		 Oset(x_index_start),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),0,NULL},
	{NhlNsfXCEndIndex,NhlCsfXCEndIndex,NhlTInteger,sizeof(int),
		 Oset(x_index_end),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),0,NULL},
	{NhlNsfYCStartIndex,NhlCsfYCStartIndex,NhlTInteger,sizeof(int),
		 Oset(y_index_start),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),0,NULL},
	{NhlNsfYCEndIndex,NhlCsfYCEndIndex,NhlTInteger,sizeof(int),
		 Oset(y_index_end),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),0,NULL},

	{NhlNsfXCStride,NhlCsfXCStride,NhlTInteger,sizeof(int),
		Oset(x_stride),NhlTImmediate,_NhlUSET((NhlPointer)1),0,NULL},
	{NhlNsfYCStride,NhlCsfYCStride,NhlTInteger,sizeof(int),
		Oset(y_stride),NhlTImmediate,_NhlUSET((NhlPointer)1),0,NULL},

   	{NhlNsfXCActualStartF,NhlCsfXCActualStartF,NhlTFloat,sizeof(float),
		 Oset(x_actual_start),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_GONLY,NULL},
	{NhlNsfXCActualEndF,NhlCsfXCActualEndF,NhlTFloat,sizeof(float),
		 Oset(x_actual_end),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_GONLY,NULL},
	{NhlNsfYCActualStartF,NhlCsfYCActualStartF,NhlTFloat,sizeof(float),
		 Oset(y_actual_start),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_GONLY,NULL},
	{NhlNsfYCActualEndF,NhlCsfYCActualEndF,NhlTFloat,sizeof(float),
		 Oset(y_actual_end),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_GONLY,NULL}

/* End-documented-resources */

};
#undef Oset

/* base methods */

static NhlErrorTypes ScalarFieldClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc	/* lc to init	*/
#endif
);

static NhlErrorTypes ScalarFieldClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes ScalarFieldInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
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

static NhlErrorTypes    ScalarFieldGetValues(
#if	NhlNeedProto
        NhlLayer	layer,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes ScalarFieldDestroy(
#if	NhlNeedProto
	NhlLayer	layer
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

NhlScalarFieldFloatClassRec NhlscalarFieldFloatClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"scalarFieldFloatClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlScalarFieldFloatLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlobjClassRec,

/* resources			*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	},
	/* NhlScalarFieldFloatLayerPart */
	{
/* foo				*/	0
	}
};

NhlScalarFieldClassRec NhlscalarFieldClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"scalarFieldClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlScalarFieldLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)
						&NhldataItemClassRec,
/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	ScalarFieldClassPartInitialize,
/* class_initialize		*/	ScalarFieldClassInitialize,
/* layer_initialize		*/	ScalarFieldInitialize,
/* layer_set_values		*/	ScalarFieldSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	ScalarFieldGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	ScalarFieldDestroy,

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
	/* NhlScalarFieldClassPart */
	{
/* foo				*/	0
	}
};
	
NhlClass NhlscalarFieldClass = (NhlClass)
					&NhlscalarFieldClassRec;
NhlClass NhlscalarFieldFloatClass = (NhlClass)
					&NhlscalarFieldFloatClassRec;

static	NrmQuark	Qfloat  = NrmNULLQUARK;
static	NrmQuark	Qint  = NrmNULLQUARK;
static	NrmQuark	Qgen_array  = NrmNULLQUARK;
static	NrmQuark	Qd_arr  = NrmNULLQUARK;
static	NrmQuark	Qx_arr  = NrmNULLQUARK;
static	NrmQuark	Qy_arr  = NrmNULLQUARK;
static	NrmQuark	Qmissing_value  = NrmNULLQUARK;
static	NrmQuark	Qdata_min  = NrmNULLQUARK;
static	NrmQuark	Qdata_max  = NrmNULLQUARK;
static	NrmQuark	Qx_start  = NrmNULLQUARK;
static	NrmQuark	Qx_end  = NrmNULLQUARK;
static	NrmQuark	Qy_start  = NrmNULLQUARK;
static	NrmQuark	Qy_end  = NrmNULLQUARK;
static	NrmQuark	Qx_subset_start  = NrmNULLQUARK;
static	NrmQuark	Qx_subset_end  = NrmNULLQUARK;
static	NrmQuark	Qy_subset_start  = NrmNULLQUARK;
static	NrmQuark	Qy_subset_end  = NrmNULLQUARK;
static	NrmQuark	Qx_index_start  = NrmNULLQUARK;
static	NrmQuark	Qx_index_end  = NrmNULLQUARK;
static	NrmQuark	Qy_index_start  = NrmNULLQUARK;
static	NrmQuark	Qy_index_end  = NrmNULLQUARK;
static	NrmQuark	Qx_actual_start  = NrmNULLQUARK;
static	NrmQuark	Qx_actual_end  = NrmNULLQUARK;
static	NrmQuark	Qy_actual_start  = NrmNULLQUARK;
static	NrmQuark	Qy_actual_end  = NrmNULLQUARK;


typedef enum _sfCoord { sfXCOORD, sfYCOORD} sfCoord;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

/*
 * Function:	nhlfscalarfieldclass
 *
 * Description:	fortran ref to scalarfield class
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
_NHLCALLF(nhlfscalarfieldclass,NHLFSCALARFIELDCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlscalarFieldClass;
}


/*
 * Function:	GenToFloatGenArray
 *
 * Description:	This function converts a generic array into a float
 *		generic array. If the array is a float array the return
 *		array pointer value will be identical to the input array 
 *		pointer value.
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
GenToFloatGenArray
#if	NhlNeedProto
(
	NhlGenArray		ga
)
#else
(ga)
	NhlGenArray		ga;
#endif
{
	NhlGenArray flt_ga = NULL;
	NhlErrorTypes subret;
	NrmValue from, to;

	if (ga == NULL) 
		return NULL;

	from.size = sizeof(NhlGenArrayRec);
	from.data.ptrval = ga;
	to.size = sizeof(NhlGenArray);
	to.data.ptrval = &flt_ga;
	subret = NhlReConvertData(NhlTGenArray,NhlTFloatGenArray,&from,&to);
	if (subret < NhlWARNING) {
		return NULL;
	}
	return flt_ga;
}

/*
 * Function:	GetVTypeValue
 *
 * Description:	This function deferences the single data value of a
 *		Variable Type GenArray depending on its type and 
 *		then casts it to float.
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
GetVTypeValue
#if	NhlNeedProto
(
	NhlGenArray	ga,
	float		*fval
)
#else
(ga,fval)
	NhlGenArray	ga;
	float		*fval;
#endif
{
	NhlGenArray lga;

	if ((lga = GenToFloatGenArray(ga)) == NULL) {
		return NhlFATAL;
	}
	*fval = *((float *)lga->data);
	return NhlNOERROR;
}

/*
 * Function:	DataToFloatArray
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
DataToFloatArray
#if	NhlNeedProto
(
 	NhlScalarFieldLayerPart *sfp,
	NhlBoolean		do_minmax,
	NhlBoolean		do_missing,
	float			missing_value,
	float			*dmin,
	float			*dmax,
	NhlBoolean		*new_data,
	NhlString		entry_name
)
#else
(sfp,do_minmax,do_missing,missing_value,dmin,dmax,new_data,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlBoolean		do_minmax;
	NhlBoolean		do_missing;
	float			missing_value;
	float			*dmin;
	float			*dmax;
	NhlBoolean		*new_data;
	NhlString		entry_name;
#endif
{
	char		*e_text;
	NhlGenArray	ga,out_ga;
	NhlBoolean	overwrite_ok = False;
	int		out_len[2];
	int		i,j;
	float		*ifp,*fp;
	int		inlen_1;
	float		tmp;
	int		ixstart = sfp->ix_start;
	int		ixend = sfp->ix_end;
	int		iystart = sfp->iy_start;
	int		iyend = sfp->iy_end;

/*
 * Convert the data array
 */
	if ((ga = GenToFloatGenArray(sfp->d_arr)) == NULL) {
		e_text = "%s: error converting data to float data";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if (ga->num_dimensions != 2 || ga->typeQ != Qfloat) {
		e_text = "%s: internal inconsistency in float data array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if (ga != sfp->d_arr) overwrite_ok = True;

	*new_data = False;
	*dmin = FLT_MAX;
	*dmax = -FLT_MAX;
	inlen_1 = ga->len_dimensions[1];
	out_len[1] = (ixend - ixstart + 1) / sfp->x_stride +
		((ixend - ixstart + 1) % sfp->x_stride > 0);
	out_len[0] = (iyend - iystart + 1) / sfp->y_stride +
		((iyend - iystart + 1) % sfp->y_stride > 0);
	*new_data = sfp->x_stride > 1 || sfp->y_stride > 1;
	out_ga = ga;
	
	if (*new_data) {

		ifp = ((float *) ga->data);

		if (overwrite_ok) 
			fp = ifp;
		else {
			if ((fp = (float *) 
			     NhlConvertMalloc(out_len[1] * out_len[0] * 
					      sizeof(float))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NULL;
			}
		}
		if (do_minmax && do_missing) {
			for (i = 0; i < out_len[0]; i++) {
				for (j = 0; j < out_len[1]; j++) {
					tmp = *(ifp + inlen_1 *
						(iystart+i*sfp->y_stride) +
						ixstart+j*sfp->x_stride);
					if (tmp != missing_value) {
						if (tmp < *dmin) *dmin = tmp;
						if (tmp > *dmax) *dmax = tmp;
					}
					*(fp+(i * out_len[1] + j)) = tmp;
				}
			}
		}
		else if (do_minmax) {
			for (i = 0; i < out_len[0]; i++) {
				for (j = 0; j < out_len[1]; j++) {
					tmp = *(ifp + inlen_1 *
						(iystart+i*sfp->y_stride) +
						ixstart+j*sfp->x_stride);
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
					*(fp+(i * out_len[1] + j)) = tmp;
				}
			}
		}
		else {
			for (i = 0; i < out_len[0]; i++) {
				for (j = 0; j < out_len[1]; j++) {
					*(fp+(i * out_len[1] + j)) = 
					     *(ifp + inlen_1 * 
					       (iystart+i*sfp->y_stride) +
					       ixstart+j*sfp->x_stride);
				}
			}
		}
		if (! overwrite_ok) {
			if ((out_ga = (NhlGenArray) 
			     NhlConvertMalloc(sizeof(NhlGenArrayRec)))
			    == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NULL;
			}
			out_ga->num_dimensions = 2;
			if ((out_ga->len_dimensions = (int *)
			     NhlConvertMalloc(2 * sizeof(int))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NULL;
			}
			out_ga->len_dimensions[0] = out_len[0];
			out_ga->len_dimensions[1] = out_len[1];
			out_ga->num_elements = out_len[0] * out_len[1];
			out_ga->typeQ = Qfloat;
			out_ga->size = sizeof(float);
			out_ga->data = (NhlPointer)fp;
			out_ga->my_data = True;
			return out_ga;
		}
	}
	else {

		ifp = ((float *) ga->data);

		if (do_minmax && do_missing) {
			for (i = 0; i < out_len[0]; i++) {
				for (j = 0; j < out_len[1]; j++) {
					tmp = *(ifp + inlen_1 *
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
			for (i = 0; i < out_len[0]; i++) {
				for (j = 0; j < out_len[1]; j++) {
					tmp = *(ifp + inlen_1 *
						(iystart+i*sfp->y_stride) +
						ixstart+j*sfp->x_stride);
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
				}
			}
		}
		
	}
	return out_ga;
}


/*
 * Function:	DataToFloatArrayExchDim
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
DataToFloatArrayExchDim
#if	NhlNeedProto
(
 	NhlScalarFieldLayerPart *sfp,
	NhlBoolean		do_minmax,
	NhlBoolean		do_missing,
	float			missing_value,
	float			*dmin,
	float			*dmax,
	NhlString		entry_name
)
#else
(sfp,do_minmax,do_missing,missing_value,dmin,dmax,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlBoolean		do_minmax;
	NhlBoolean		do_missing;
	float			missing_value;
	float			*dmin;
	float			*dmax;
	NhlString		entry_name;
#endif
{
	char		*e_text;
	NhlGenArray	ga, out_ga;
	int		out_len[2];
	int		i,j;
	float		*ifp,*fp;
	int		inlen_1;
	float		tmp;
	NhlBoolean	overwrite_ok = False;
	int		ixstart = sfp->ix_start;
	int		ixend = sfp->ix_end;
	int		iystart = sfp->iy_start;
	int		iyend = sfp->iy_end;
	int		x_stride = sfp->x_stride;
	int		y_stride = sfp->y_stride;

/*
 * Convert the data array
 */
	if ((ga = GenToFloatGenArray(sfp->d_arr)) == NULL) {
		e_text = "%s: error converting data to float data";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if (ga->num_dimensions != 2 || ga->typeQ != Qfloat) {
		e_text = "%s: internal inconsistency in float data array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if (ga != sfp->d_arr) overwrite_ok = True;

	*dmin = FLT_MAX;
	*dmax = -FLT_MAX;
	inlen_1 = ga->len_dimensions[1];
/*
 * Assign the dimension length according to the lengths needed by
 * the new output array (0-slow,1-fast). This is , of course, the reverse
 * of the input array.
 */
	out_len[1] = (iyend - iystart + 1) / y_stride +
		((iyend - iystart + 1) % y_stride > 0);
	out_len[0] = (ixend - ixstart + 1) / x_stride +
		((ixend - ixstart + 1) % x_stride > 0);

	ifp = ((float *) ga->data);

/*
 * Eventually if overwrite is ok then the data will be exchanged in place.
 * But for now in this situation make a temporary copy of the 
 * output array, and after the exchange copy it over the original array.
 */

	if (overwrite_ok) {
		if ((fp = (float *) 
		     NhlMalloc(out_len[1] * out_len[0] * 
			       sizeof(float))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
	}
	else {
		if ((fp = (float *) 
		     NhlConvertMalloc(out_len[1] * out_len[0] * 
				      sizeof(float))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
	}
	if (do_minmax && do_missing) {
		for (i = 0; i < out_len[1]; i++) {
			for (j = 0; j < out_len[0]; j++) {
				tmp = *(ifp + 
					inlen_1*(iystart+i*y_stride) +
					ixstart+j*x_stride);
				if (tmp != missing_value) {
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
				}
				*(fp+(j * out_len[1] + i)) = tmp;
			}
		}
	}
	else if (do_minmax) {
		for (i = 0; i < out_len[1]; i++) {
			for (j = 0; j < out_len[0]; j++) {
				tmp = *(ifp + 
					inlen_1*(iystart+i*y_stride) +
					ixstart+j*x_stride);
				if (tmp < *dmin) *dmin = tmp;
				if (tmp > *dmax) *dmax = tmp;
				*(fp+(j * out_len[1] + i)) = tmp;
			}
		}
	}
	else { 
		/* nothing to check so it doesn't matter 
		 * if there are missing values 
		 */
		for (i = 0; i < out_len[1]; i++) {
			for (j = 0; j < out_len[0]; j++) {
				*(fp+(j * out_len[1] + i)) = 
					*(ifp + 
					  inlen_1*(iystart+i*sfp->y_stride) +
					  ixstart+j*x_stride);
			}
		}
	}

	if (overwrite_ok) {
		int num_el = out_len[1] * out_len[0];
		memcpy(ifp,fp, num_el * sizeof(float));
		NhlFree(fp);
		out_ga = ga;
		out_ga->len_dimensions[0] = out_len[0];
		out_ga->len_dimensions[1] = out_len[1];
		out_ga->num_elements = num_el;
	}
	else {
		if ((out_ga = (NhlGenArray) 
		     NhlConvertMalloc(sizeof(NhlGenArrayRec))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name);
			return NULL;
		}
		out_ga->num_dimensions = 2;
		if ((out_ga->len_dimensions = (int *)
		     NhlConvertMalloc(2 * sizeof(int))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name);
			return NULL;
		}
		out_ga->len_dimensions[0] = out_len[0];
		out_ga->len_dimensions[1] = out_len[1];
		out_ga->num_elements = out_len[1] * out_len[0];
		out_ga->typeQ = Qfloat;
		out_ga->size = sizeof(float);
		out_ga->data = (NhlPointer)fp;
		out_ga->my_data = True;
	}
	return out_ga;
}

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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
		len_dim = sfp->d_arr->len_dimensions[1];
		name = NhlNsfXArray;
	}
	else {
		len_dim = sfp->d_arr->len_dimensions[0];
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
#if	NhlNeedProto
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
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;

	if (ctype == sfXCOORD) {

		if (sfp->x_start != NULL) {
			subret = GetVTypeValue(sfp->x_start,cstart);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else if (sfp->x_subset_start != NULL) {
			subret = GetVTypeValue(sfp->x_subset_start,cstart);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else 
			*cstart = 0.0;

		if (sfp->x_end != NULL) {
			subret = GetVTypeValue(sfp->x_end,cend);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else if (sfp->x_subset_end != NULL) {
			subret = GetVTypeValue(sfp->x_subset_end,cend);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else 
			*cend = sfp->d_arr->len_dimensions[1] - 1;
	}
	else {
		if (sfp->y_start != NULL) {
			subret = GetVTypeValue(sfp->y_start,cstart);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else if (sfp->y_subset_start != NULL) {
			subret = GetVTypeValue(sfp->y_subset_start,cstart);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else 
			*cstart = 0.0;
		
		if (sfp->y_end != NULL) {
			subret = GetVTypeValue(sfp->y_end,cend);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else if (sfp->y_subset_end != NULL) {
			subret = GetVTypeValue(sfp->y_subset_end,cend);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else 
			*cend = sfp->d_arr->len_dimensions[0] - 1;
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
#if	NhlNeedProto
(
 	NhlScalarFieldLayerPart *sfp,
	sfCoord			ctype,
	int			*icstart,
	int			*icend,
	NhlString		entry_name
)
#else
(sfp,ctype,icstart,icend,entry_name)
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

		max_index = sfp->d_arr->len_dimensions[1] - 1;

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

		max_index = sfp->d_arr->len_dimensions[0] - 1;

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
#if	NhlNeedProto
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
	NhlErrorTypes   ret = NhlNOERROR, subret = NhlNOERROR;
	float		flt_inc;
	NhlBoolean	rev;
	float		drange;
	int		range;
	NhlGenArray	subset_start, subset_end;
	NhlBoolean	nullstart = False, nullend = False;
	char		*c_name;
	int		stride,rem;

	if (ctype == sfXCOORD) {
		range = sfp->d_arr->len_dimensions[1] - 1;
		subset_start = sfp->x_subset_start;
		subset_end = sfp->x_subset_end;
		stride = sfp->x_stride;
		c_name = "X coordinate";
	}
	else {
		range = sfp->d_arr->len_dimensions[0] - 1;
		subset_start = sfp->y_subset_start;
		subset_end = sfp->y_subset_end;
		stride = sfp->y_stride;
		c_name = "Y coordinate";
	}

	if (! sfp->subset_by_index) {
		float fval;

		rev = cstart > cend;
		if (subset_start != NULL) {
			subret = GetVTypeValue(subset_start,&fval);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				      "%s: error getting variable type value";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return ret;
			}
			*scstart = rev ? MIN(cstart,fval): MAX(cstart,fval);
		}
		else {
			*scstart = cstart;
			nullstart = True;
		}
		if (subset_end != NULL) {
			subret = GetVTypeValue(subset_end,&fval);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return ret;
			}
			*scend = rev ? MAX(cend,fval): MIN(cend,fval);
		}
		else {
			*scend = cend;
			nullend = True;
		}

		if (rev != (*scstart > *scend)) {
			e_text = 
     "%s: %s  start/end subset order opposed to %s start/end order: not using subset resources";
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  e_text,entry_name,c_name,c_name);
			ret = MIN(NhlWARNING,ret);
			*scstart = cstart;
			*scend = cend;
		}

/*
 * The index endpoints are chosen to include the subset data endpoints.
 */
		drange = cend - cstart;
		*icstart = MAX(0,floor(((*scstart - cstart) /drange) * range));
		*icend = MIN(range,ceil(((*scend - cstart) / drange) * range));

		if (*icend - *icstart < 2) {
			e_text = 
		         "%s: %s subset data range not large enough: ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  c_name,c_name);
			ret = MIN(NhlWARNING,ret);
			*scstart = cstart;
			*scend = cend;
			*icstart = 0;
			*icend = range;
		}
	}


/* 
 * if a stride is specified, the index must be a multiple of the stride 
 * value in order to assure that the complete data coordinate range is
 * included. Add to the specified end index if necessary, unless it 
 * would exceed the max index. In this case subtract -- it is not possible
 * to include the complete data range.
 */
	rem = *icend % stride;
	if (rem  > 0) {
		if (*icend + stride - rem <= range)
			*icend += stride - rem;
		else
			*icend -= rem;
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
#if	NhlNeedProto
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		*c_array,
	sfCoord			ctype,
	NhlBoolean		overwrite_ok,
	float			*cstart,
	float			*cend,
	int			*icstart,
	int			*icend,
	float			*scstart,
	float			*scend,
	NhlString		entry_name
)
#else
(sfp,c_array,ctype,overwrite_ok,
 cstart,cend,icstart,icend,scstart,scend,entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		*c_array;
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
	NhlErrorTypes   ret = NhlNOERROR,subret = NhlNOERROR;
	NhlBoolean	rev;
	int		i, len;
	NhlGenArray	subset_start,subset_end,out_ga;
	NhlBoolean	nullstart = False,nullend = False;
	char		*c_name;
	float		*fp, *nfp;
	int		rem,stride;

	if (ctype == sfXCOORD) {
		subset_start = sfp->x_subset_start;
		subset_end = sfp->x_subset_end;
		stride = sfp->x_stride;
		c_name = "X coordinate";
	}
	else {
		subset_start = sfp->y_subset_start;
		subset_end = sfp->y_subset_end;
		stride = sfp->y_stride;
		c_name = "Y coordinate";
	}

	len = (*c_array)->len_dimensions[0];
	fp = (float *) (*c_array)->data;
	*cstart = fp[0];
	*cend = fp[len-1];

	if (! sfp->subset_by_index) {
		float fval;

		rev = *cstart > *cend;
		if (subset_start != NULL) {
			subret = GetVTypeValue(subset_start,&fval);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				      "%s: error getting variable type value";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return ret;
			}
			*scstart = rev ? MIN(*cstart,fval): MAX(*cstart,fval);
		}
		else {
			*scstart = *cstart;
			nullstart = True;
		}

		if (subset_end != NULL) {
			subret = GetVTypeValue(subset_end,&fval);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				      "%s: error getting variable type value";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return ret;
			}
			*scend = rev ? MAX(*cend,fval) : MIN(*cend,fval);
		}
		else {
			*scend = *cend;
			nullend = True;
		}

		if (rev != (*scstart > *scend)) {
			e_text = 
      "%s: %s start/end subset order opposed to %s start/end order: not using subset resources";
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  e_text,entry_name,c_name,c_name);
			ret = MIN(NhlWARNING,ret);
			*scstart = *cstart;
			*scend = *cend;
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
		        "%s: %s subset data range not large enough: ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  c_name,c_name);
			ret = MIN(NhlWARNING,ret);
			*scstart = *cstart;
			*scend = *cend;
			*icstart = 0;
			*icend = len - 1;
		}
	}

/* 
 * if a stride is specified, the end index must be a multiple of the stride 
 * value. Increas the specified end index if necessary, unless it 
 * would exceed the max index. In this case subtract -- it is not possible
 * to include the complete data range.
 */
	rem = *icend % stride;
	if (rem  > 0) {
		if (*icend + stride - rem <= len -1)
			*icend += stride - rem;
		else
			*icend -= rem;
	}
	*scstart = fp[*icstart];
	*scend = fp[*icend];
/*
 * If the data is a subset of the complete array, copy the relevant
 * part of the irregular coordinate array to a new array. The old data
 * space will eventually be freed (I think) by the Converter 
 * memory management routines.
 */
	if (*icstart > 0 || *icend < len - 1) {
		int nlen = *icend - *icstart + 1;
		if (overwrite_ok) {
			for (i = 0; i < nlen; i++) {
				fp[i] = fp[*icstart+i];
			}
			(*c_array)->num_elements = nlen;
		}
		else {
			if ((nfp = (float *)
			     NhlConvertMalloc(nlen * 
					      sizeof(float))) == NULL) { 
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			memcpy(nfp,&(fp[*icstart]),nlen * sizeof(float));
			if ((out_ga = (NhlGenArray) 
			     NhlConvertMalloc(sizeof(NhlGenArrayRec)))
			    == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			out_ga->num_dimensions = 1;
			out_ga->len_dimensions = &out_ga->num_elements;
			out_ga->num_elements = nlen;
			out_ga->typeQ = Qfloat;
			out_ga->size = sizeof(float);
			out_ga->data = (NhlPointer)nfp;
			out_ga->my_data = True;
			*c_array = out_ga;
		}
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
#if	NhlNeedProto
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
#if	NhlNeedProto
(
 	NhlScalarFieldLayerPart *sfp,
	NhlGenArray		*c_array,
	sfCoord			ctype,
	NhlBoolean		overwrite_ok,
	float			*cstart,
	float			*cend,
	int			*icstart,
	int			*icend,
	float			*scstart,
	float			*scend,
	NhlString		entry_name
)
#else
( sfp, c_array, ctype, overwrite_ok, cstart, cend, icstart, icend, scstart, scend, entry_name)
 	NhlScalarFieldLayerPart *sfp;
	NhlGenArray		*c_array;
	sfCoord			ctype;
	NhlBoolean		overwrite_ok;
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

	subret = GetSubsetBoundsIrregular(sfp,c_array,ctype,overwrite_ok,
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
 * 		Note that the ScalarFieldFloat object has no resources 
 *		of its own. Its private fields are set directly by the 
 *		converter.
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
	float			missing_value;
	NhlBoolean		do_minmax,do_missing,new_data,overwrite_ok;
	float			dmin,dmax,tmin,tmax;
	NhlScalarFieldFloatLayer	sffl;
	NhlScalarFieldFloatLayerPart	*sffp;
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
	    (sfl->base.layer_class != NhlscalarFieldClass)){
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
 * Create a scalar field float data object
 */
	subret = NhlALCreate(to->data.ptrval,"no.name",
			     NhlscalarFieldFloatClass,
			     sfl->base.id,sargs,nargs);

	if ((sffl = (NhlScalarFieldFloatLayer)
	     _NhlGetLayer(*((int *)to->data.ptrval))) == NULL) {
		e_text = "%s: error creating scalar field float object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	sffp = &sffl->sfieldfloat;
	sfp->sffloat = sffl;

/*
 * Convert, validate, and set the X and Y irregular coordinate arrays,
 * if defined.
 */
	sffp->x_arr = NULL;
	if (sfp->x_arr != NULL && sfp->x_arr->num_elements > 0) {
		if ((x_arr = GenToFloatGenArray(sfp->x_arr)) == NULL) {
			e_text = "%s: error converting XCoord data to float";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if (ValidCoordArray(sfp,x_arr,sfXCOORD,entry_name)) {
			NhlSetSArg(&sargs[nargs++],NhlNsfXArray,x_arr);
			xirr = True;
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
		overwrite_ok = x_arr != sfp->x_arr;
		subret = GetCoordBoundsIrregular(sfp,&x_arr,sfXCOORD,
						 overwrite_ok,
						 &xstart,&xend,
						 &ixstart,&ixend,
						 &sxstart,&sxend,
						 entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
		sffp->x_arr = x_arr;
	}
	sfp->ix_start = ixstart;
	sfp->ix_end = ixend;
	sfp->x_actual_start = sxstart;
	sfp->x_actual_end = sxend;

	sffp->y_arr = NULL;
	if (sfp->y_arr != NULL && sfp->y_arr->num_elements > 0) {
		if ((y_arr = GenToFloatGenArray(sfp->y_arr)) == NULL) {
			e_text = "%s: error converting YCoord data to float";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if (ValidCoordArray(sfp,y_arr,sfYCOORD,entry_name)) {
			NhlSetSArg(&sargs[nargs++],NhlNsfYArray,y_arr);
			yirr = True;
		}
	}

	if (! yirr) {
		subret = GetCoordBounds(sfp,sfYCOORD,&ystart,&yend,
					&iystart,&iyend,&systart,&syend,
					entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
	}
	else {
		overwrite_ok = y_arr != sfp->y_arr;
		subret = GetCoordBoundsIrregular(sfp,&y_arr,sfYCOORD,
						 overwrite_ok,
						 &ystart,&yend,
						 &iystart,&iyend,
						 &systart,&syend,
						 entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
		sffp->y_arr = y_arr;
	}
	sfp->iy_start = iystart;
	sfp->iy_end = iyend;
	sfp->y_actual_start = systart;
	sfp->y_actual_end = syend;
/*
 * Set flags to tell the array conversion routines whether to find
 * data max and mins (and if so whether to check for missing values) 
 * during the conversion process.
 */
	if (sfp->missing_value == NULL) {
		do_missing = False;
		missing_value = 0.0;
		sffp->missing_value_set = False;
		sffp->missing_value = 0.0;
	}
	else {
		do_missing = True;
		subret = GetVTypeValue(sfp->missing_value, &missing_value);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		sffp->missing_value_set = True;
		sffp->missing_value = missing_value;
	}
	do_minmax =  (sfp->data_max == NULL || sfp->data_min == NULL) ?
		True : False;

	if (! sfp->exchange_dimensions) {
		if ((d_arr = DataToFloatArray(sfp,
					      do_minmax,do_missing,
					      missing_value,&dmin,&dmax,
					      &new_data,
					      entry_name)) == NULL) {
			return NhlFATAL;
		}
		sffp->x_start = sfp->x_actual_start;
		sffp->x_end = sfp->x_actual_end;
		sffp->y_start = sfp->y_actual_start;
		sffp->y_end = sfp->y_actual_end;
	}
	else {
		if ((d_arr = 
		     DataToFloatArrayExchDim(sfp,
					     do_minmax,do_missing,
					     missing_value,&dmin,&dmax,
					     entry_name)) == NULL) {
			return NhlFATAL;
		}
		new_data = True;
		sffp->x_start = sfp->y_actual_start;
		sffp->x_end = sfp->y_actual_end;
		sffp->y_start = sfp->x_actual_start;
		sffp->y_end = sfp->x_actual_end;
	}
	sffp->d_arr = d_arr;

/*
 * If the user passed in a float array and stride values are all unity,
 * then the array is not copied. Set values that will be used to 
 * indicate to the low level routines what portion of the array to use.
 * If a copy was made, the entire array will be utilitized.
 */
	if (! new_data) {
		sffp->begin = iystart * d_arr->len_dimensions[1] + ixstart;
		sffp->fast_dim = d_arr->len_dimensions[1];
		sffp->fast_len = ixend - ixstart + 1;
		sffp->slow_len = iyend - iystart + 1;
	}
	else {
		sffp->begin = 0;
		sffp->fast_dim = d_arr->len_dimensions[1];
		sffp->fast_len = d_arr->len_dimensions[1];
		sffp->slow_len = d_arr->len_dimensions[0];
	}
/*
 * If the user explicitly sets the data min/max values, make sure the max
 * is greater than the min. If not, WARN and exchange. 
 * Otherwise pass on without interpretation to the plot object.
 * Note that if either value has not been set, the actual values will
 * be contained in dmin and dmax.
 */
	tmin = dmin;
	if (sfp->data_min != NULL) {
		subret = GetVTypeValue(sfp->data_min,&tmin);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
	}
	tmax = dmax;
	if (sfp->data_max != NULL) {
		subret = GetVTypeValue(sfp->data_max,&tmax);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
	}
	if (tmax <= tmin) {
		e_text = "%s: %s greater than %s: exchanging";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNsfDataMinV,NhlNsfDataMaxV);
		dmin = tmax;
		dmax = tmin;
	}
	else {
		dmax = tmax;
		dmin = tmin;
	}

	sffp->data_min = dmin;
	sffp->data_max = dmax;

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
#if	NhlNeedProto
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
	Qgen_array = NrmStringToQuark(NhlTGenArray);
	Qd_arr  = NrmStringToQuark(NhlNsfDataArray);
	Qx_arr  = NrmStringToQuark(NhlNsfXArray);
	Qy_arr  = NrmStringToQuark(NhlNsfYArray);
	Qmissing_value  = NrmStringToQuark(NhlNsfMissingValueV);
	Qdata_min  = NrmStringToQuark(NhlNsfDataMinV);
	Qdata_max  = NrmStringToQuark(NhlNsfDataMaxV);
	Qx_start  = NrmStringToQuark(NhlNsfXCStartV);
	Qx_end  = NrmStringToQuark(NhlNsfXCEndV);
	Qy_start  = NrmStringToQuark(NhlNsfYCStartV);
	Qy_end  = NrmStringToQuark(NhlNsfYCEndV);
	Qx_subset_start  = NrmStringToQuark(NhlNsfXCStartSubsetV);
	Qx_subset_end  = NrmStringToQuark(NhlNsfXCEndSubsetV);
	Qy_subset_start  = NrmStringToQuark(NhlNsfYCStartSubsetV);
	Qy_subset_end  = NrmStringToQuark(NhlNsfYCEndSubsetV);
	Qx_index_start  = NrmStringToQuark(NhlNsfXCStartIndex);
	Qx_index_end  = NrmStringToQuark(NhlNsfXCEndIndex);
	Qy_index_start  = NrmStringToQuark(NhlNsfYCStartIndex);
	Qy_index_end  = NrmStringToQuark(NhlNsfYCEndIndex);
	Qx_actual_start  = NrmStringToQuark(NhlNsfXCActualStartF);
	Qx_actual_end  = NrmStringToQuark(NhlNsfXCActualEndF);
	Qy_actual_start  = NrmStringToQuark(NhlNsfYCActualStartF);
	Qy_actual_end  = NrmStringToQuark(NhlNsfYCActualEndF);

	ret = NhlRegisterConverter(
			NhlscalarFieldClass->base_class.class_name,
			NhlscalarFieldFloatClass->base_class.class_name,
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
 *		NhlClass	lc	pointer to class structure
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
ScalarFieldClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class structure	*/
)
#else
(lc)
	NhlClass	lc;	/* pointer to class structure	*/
#endif
{
	NhlErrorTypes		ret;

	ret = _NhlRegisterChildClass(lc,NhlscalarFieldFloatClass,
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
ScalarFieldInitialize
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
	char			*entry_name = "ScalarFieldInitialize";
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlScalarFieldLayer	sfl = (NhlScalarFieldLayer)new;
	NhlScalarFieldLayerPart	*sfp = &(sfl->sfield);
	NhlGenArray		ga;

	sfp->sffloat = NULL;
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
	if (sfp->subset_by_index != osfp->subset_by_index)
		status = True;
	if (sfp->exchange_dimensions != osfp->exchange_dimensions)
		status = True;

        _NhlDataChanged((NhlDataItemLayer)new,status);

	return	ret;
}


/*
 * Function:    CopyData
 *
 * Description: Copies the data of a GenArray
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlPointer CopyData
#if	NhlNeedProto
(
	NhlGenArray ga, 
	NrmQuark resQ
)
#else
(ga,resQ)
	NhlGenArray ga;
	NrmQuark resQ;
#endif
{
	char *e_text, *entry_name = "ScalarFieldGetValues";
	NhlPointer new;

	if ((new = NhlMalloc(ga->num_elements * ga->size)) == NULL) {
		e_text = "%s: dynamic memory allocation error for %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NrmQuarkToString(resQ));
		return NULL;
	}
	memcpy(new,ga->data,ga->num_elements * ga->size);
	
	return new;
}


/*
 * Function:    CreateData
 *
 * Description: Create the data for a VType GenArray given a float value
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlPointer CreateData
#if	NhlNeedProto
(
	float fval, 
	NrmQuark resQ
)
#else
(fval,resQ)
	float fval;
	NrmQuark resQ;
#endif
{
	char *e_text, *entry_name = "ScalarFieldGetValues";
	NhlPointer new;

	if ((new = NhlMalloc(sizeof(float))) == NULL) {
		e_text = "%s: dynamic memory allocation error for %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NrmQuarkToString(resQ));
		return NULL;
	}
	memcpy(new,&fval,sizeof(float));
	
	return new;
}


/*
 * Function:    ForceConvert
 *
 * Description: Explicitly forces the conversion to NhlScalarFieldFloat
 *		when required in order to provide GetValues information
 *		that is not available before the conversion occurs.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlScalarFieldFloatLayer ForceConvert
#if	NhlNeedProto
(
	NhlScalarFieldLayer sfl
)
#else
(sfl)
	NhlScalarFieldLayer sfl;
#endif
{
	char *e_text, *entry_name = "ScalarFieldGetValues";
	int id;
	NhlScalarFieldFloatLayer sffl = NULL;
	NhlErrorTypes ret = NhlNOERROR;
	NrmValue from, to;

	from.size = sizeof(NhlScalarFieldLayerRec);
	from.data.intval = sfl->base.id;
	to.size = sizeof(NhlScalarFieldFloatLayerRec);
	to.data.ptrval = &id;
	ret = NhlConvertData(NhlscalarFieldClass->base_class.class_name,
			NhlscalarFieldFloatClass->base_class.class_name,
			     &from,&to);
	if (ret < NhlWARNING ||
	    (sffl = (NhlScalarFieldFloatLayer) _NhlGetLayer(id)) == NULL) {
		e_text = "%s: error converting NhlScalarFieldLayer";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
	}

	return sffl;
}

/*
 * Function:    ScalarFieldGetValues
 *
 * Description: Retrieves the current setting of one or more ScalarField 
 *      resources.This routine only retrieves resources that require 
 *	special methods that the generic GetValues method cannot handle. 
 *      This includes all resources implemented as GenArrays, including
 *	the variable type scalar resources (VTypes). In general space is 
 *	allocated; the user is responsible for freeing this space. However,
 *	if the CopyArrays resource is False, Data array and the X/Y
 *	coordinate arrays (if they exist) are NOT copied, since the user
 *	is assumed to be keeping a valid copy of them around. 
 *	If the user does a GetValues on the data min or max before a
 *	the conversion to ScalarFieldFloat object has taken place, then
 *	the converter is called explicitly, since most of the converter
 * 	code needs to be executed in order to determine the max and min.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when any of the following resources are retrieved:
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    ScalarFieldGetValues
#if	NhlNeedProto
(NhlLayer layer, _NhlArgList args, int num_args)
#else
(layer,args,num_args)
        NhlLayer        layer;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlScalarFieldLayer sfl = (NhlScalarFieldLayer)layer;
        NhlScalarFieldLayerPart *sfp = &(sfl->sfield);
	NhlScalarFieldFloatLayerPart *sffp = NULL;
	NhlErrorTypes ret = NhlNOERROR;
        NhlGenArray ga;
        char *e_text, *entry_name = "ScalarFieldGetValues";
        int i;
        NrmQuark resQ;
	NrmQuark typeQ = NrmNULLQUARK;
	NhlPointer	data;
	int		dlen[2];
	int		ndim;
	int		size;
	NhlBoolean	nocopy = False, do_genarray;
	float		tmp;
	NhlBoolean	converted;
	int		ival;
	float		fval;
		
	if (sfp->d_arr == NULL) {
		e_text = "%s: internal inconsistency";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	converted = sfp->sffloat != NULL;
	if (converted)
		sffp = &sfp->sffloat->sfieldfloat;

        for( i = 0; i< num_args; i++ ) {
		ga = NULL;
		resQ = args[i].quark;
		do_genarray = False;
                if (resQ == Qd_arr) {
			do_genarray = True;
			ndim = 2;
			dlen[0] = sfp->d_arr->len_dimensions[0];
			dlen[1] = sfp->d_arr->len_dimensions[1];
			if (sfp->copy_arrays) {
				if ((data = CopyData(sfp->d_arr,resQ))
				    == NULL)
					return NhlFATAL;
			}
			else {
				nocopy = True;
				data = sfp->d_arr->data;
			}
			typeQ = sfp->d_arr->typeQ;
			size = sfp->d_arr->size;
                }
                else if (resQ == Qx_arr) {
			do_genarray = True;
			ndim = 1;
			if (sfp->x_arr == NULL) {
				dlen[0] = 0;
				dlen[1] = 0;
				data = NULL;
				typeQ = Qfloat;
				size = 0;
                        }
			else {
				dlen[0] = sfp->x_arr->len_dimensions[0];
				if (sfp->copy_arrays) {
					if ((data = CopyData(sfp->x_arr,resQ))
					    == NULL)
						return NhlFATAL;
				}
				else {
					nocopy = True;
					data = sfp->x_arr->data;
				}
				typeQ = sfp->x_arr->typeQ;
				size = sfp->x_arr->size;
			}
                }
                else if (resQ == Qy_arr) {
			do_genarray = True;
			ndim = 1;
			if (sfp->y_arr == NULL) {
				dlen[0] = 0;
				dlen[1] = 0;
				data = NULL;
				typeQ = Qfloat;
				size = 0;
                        }
			else {
				dlen[0] = sfp->y_arr->len_dimensions[0];
				if (sfp->copy_arrays) {
					if ((data = CopyData(sfp->y_arr,resQ))
					    == NULL)
						return NhlFATAL;
				}
				else {
					nocopy = True;
					data = sfp->y_arr->data;
				}
				typeQ = sfp->y_arr->typeQ;
				size = sfp->y_arr->size;
			}
                }
                else if (resQ == Qmissing_value) {
			do_genarray = True;
			ndim = 1;
			if (sfp->missing_value == NULL) {
				dlen[0] = 0;
				data = NULL;
				typeQ = Qfloat;
				size = 0;
                        }
			else {
				dlen[0] = 
					sfp->missing_value->len_dimensions[0];
				if ((data = CopyData(sfp->missing_value,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->missing_value->typeQ;
				size = sfp->missing_value->size;
			}
                }
                else if (resQ == Qdata_min) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->data_min != NULL) {
				if ((data = CopyData(sfp->data_min,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->data_min->typeQ;
				size = sfp->data_min->size;
			}
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(sffp->data_min,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qdata_max) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->data_max != NULL) {
				if ((data = CopyData(sfp->data_max,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->data_min->typeQ;
				size = sfp->data_min->size;
			}
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(sffp->data_max,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->x_start != NULL) {
				if ((data = CopyData(sfp->x_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_start->typeQ;
				size = sfp->x_start->size;
			}
			else if (sfp->x_subset_start != NULL) {
				if ((data = CopyData(sfp->x_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_subset_start->typeQ;
				size = sfp->x_subset_start->size;
			}
			else {
				tmp = 0.0;
				if ((data = CreateData(tmp,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->x_end != NULL) {
				if ((data = CopyData(sfp->x_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_end->typeQ;
				size = sfp->x_end->size;
			}
			else if (sfp->x_subset_start != NULL) {
				if ((data = CopyData(sfp->x_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_subset_start->typeQ;
				size = sfp->x_subset_start->size;
			}
			else {
				tmp = sfp->d_arr->len_dimensions[1] - 1;
				if ((data = CreateData(tmp,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->y_start != NULL) {
				if ((data = CopyData(sfp->y_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_start->typeQ;
				size = sfp->y_start->size;
			}
			else if (sfp->y_subset_start != NULL) {
				if ((data = CopyData(sfp->y_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_subset_start->typeQ;
				size = sfp->y_subset_start->size;
			}
			else {
				tmp = 0.0;
				if ((data = CreateData(tmp,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->y_end != NULL) {
				if ((data = CopyData(sfp->y_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_end->typeQ;
				size = sfp->y_end->size;
			}
			else if (sfp->y_subset_start != NULL) {
				if ((data = CopyData(sfp->y_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_subset_start->typeQ;
				size = sfp->y_subset_start->size;
			}
			else {
				tmp = sfp->d_arr->len_dimensions[0] - 1;
				if ((data = CreateData(tmp,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_subset_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->x_subset_start != NULL) {
				if ((data = CopyData(sfp->x_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_subset_start->typeQ;
				size = sfp->x_subset_start->size;
			}
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(sffp->x_start,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_subset_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->x_subset_end != NULL) {
				if ((data = CopyData(sfp->x_subset_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_subset_end->typeQ;
				size = sfp->x_subset_end->size;
			}
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(sffp->x_end,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_subset_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->y_subset_start != NULL) {
				if ((data = CopyData(sfp->y_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_subset_start->typeQ;
				size = sfp->y_subset_start->size;
			}
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(sffp->y_start,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_subset_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->y_subset_end != NULL) {
				if ((data = CopyData(sfp->y_subset_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_subset_end->typeQ;
				size = sfp->y_subset_end->size;
			}
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(sffp->y_end,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_index_start) {
			if (sfp->x_index_start > -1)
				ival = sfp->x_index_start;
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				ival = sfp->ix_start;
			}
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qx_index_end) {
			if (sfp->x_index_end > -1)
				ival = sfp->x_index_end;
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				ival = sfp->ix_end;
			}
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_index_start) {
			if (sfp->y_index_start > -1)
				ival = sfp->y_index_start;
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				ival = sfp->iy_start;
			}
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_index_end) {
			if (sfp->y_index_end > -1)
				ival = sfp->y_index_end;
			else {
				if (! converted) {
					sfp->sffloat = ForceConvert(sfl);
					if (sfp->sffloat == NULL)
						return NhlFATAL;
					sffp = &sfp->sffloat->sfieldfloat;
					converted = True;
				}
				ival = sfp->iy_end;
			}
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qx_actual_start) {
			if (! converted) {
				sfp->sffloat = ForceConvert(sfl);
				if (sfp->sffloat == NULL)
					return NhlFATAL;
				sffp = &sfp->sffloat->sfieldfloat;
				converted = True;
			}
			fval = sfp->x_actual_start;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qx_actual_end) {
			if (! converted) {
				sfp->sffloat = ForceConvert(sfl);
				if (sfp->sffloat == NULL)
					return NhlFATAL;
				sffp = &sfp->sffloat->sfieldfloat;
				converted = True;
			}
			fval = sfp->x_actual_end;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_actual_start) {
			if (! converted) {
				sfp->sffloat = ForceConvert(sfl);
				if (sfp->sffloat == NULL)
					return NhlFATAL;
				sffp = &sfp->sffloat->sfieldfloat;
				converted = True;
			}
			fval = sfp->y_actual_start;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_actual_end) {
			if (! converted) {
				sfp->sffloat = ForceConvert(sfl);
				if (sfp->sffloat == NULL)
					return NhlFATAL;
				sffp = &sfp->sffloat->sfieldfloat;
				converted = True;
			}
			fval = sfp->y_actual_end;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }

		if (do_genarray) {
			ga = _NhlCreateGenArray(data,NrmQuarkToString(typeQ),
						size,ndim,dlen,False);

			if (ga == NULL){
				e_text = "%s: error creating %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,NrmQuarkToString(resQ));
				return NhlFATAL;
			}
			ga->my_data = nocopy ? False : True;

			*(NhlGenArray *)args[i].value.ptrval = ga;
			*args[i].type_ret = Qgen_array;
			*args[i].size_ret = sizeof(NhlGenArray);
			*args[i].free_func = (_NhlFreeFunc)NhlFreeGenArray;
		}
        }

        return ret;

}

/*
 * Function:	ScalarFieldDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlScalarFieldClass.
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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

	if (*ga != NULL) NhlFreeGenArray(*ga);

	if ((*ga = _NhlCopyGenArray(copy_ga,True)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		*ga = NULL;
		return NhlFATAL;
	}

	return NhlNOERROR;
}
