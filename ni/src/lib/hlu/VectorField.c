/*
 *      $Id: VectorField.c,v 1.8 1996-09-14 17:07:35 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VectorField.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 28 10:50:34 MDT 1995
 *
 *	Description:	This class is used to communicate data in the format
 *			of a Vector Field.
 */
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ncarg/hlu/VectorFieldP.h>

/************************************************************************
*									*
*	VectorField Class declarations					*
*									*
************************************************************************/

/*
 * Resource Declarations
 */

#define	Oset(field)	NhlOffset(NhlVectorFieldLayerRec,vfield.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNvfDataArray,NhlCvfDataArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(d_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNvfUDataArray,NhlCvfUDataArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(u_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNvfVDataArray,NhlCvfVDataArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(v_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNvfXArray,NhlCvfXArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNvfYArray,NhlCvfYArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNvfPolarData,NhlCvfPolarData,
		 NhlTBoolean,sizeof(NhlBoolean),Oset(polar_data),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNvfSubsetByIndex,NhlCvfSubsetByIndex,
		 NhlTBoolean,sizeof(NhlBoolean),Oset(subset_by_index),
		 NhlTImmediate,_NhlUSET((NhlPointer)False),0,NULL},
	{NhlNvfCopyData,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(copy_arrays),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{NhlNvfExchangeDimensions,NhlCvfExchangeDimensions,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(exchange_dimensions),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),0,NULL},
	{NhlNvfExchangeUVData,NhlCvfExchangeUVData,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(exchange_uv_data),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),0,NULL},
	{NhlNvfSingleMissingValue,NhlCvfSingleMissingValue,
		 NhlTBoolean,sizeof(NhlBoolean),Oset(single_missing),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),0,NULL},
	{NhlNvfMissingUValueV,NhlCvfMissingUValueV,NhlTVariable,
		 sizeof(NhlGenArray),Oset(missing_u_value),NhlTImmediate,
		 _NhlUSET(NULL),0,NULL},
	{NhlNvfMissingVValueV,NhlCvfMissingVValueV,NhlTVariable,
		 sizeof(NhlGenArray),Oset(missing_v_value),NhlTImmediate,
		 _NhlUSET(NULL),0,NULL},
	{NhlNvfMagMinV,NhlCvfMagMinV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(mag_min),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfMagMaxV,NhlCvfMagMaxV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(mag_max),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfUMinV,NhlCvfUMinV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(u_min),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfUMaxV,NhlCvfUMaxV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(u_max),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfVMinV,NhlCvfVMinV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(v_min),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfVMaxV,NhlCvfVMaxV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(v_max),NhlTImmediate,_NhlUSET(NULL),0,NULL},

	{NhlNvfXCStartV,NhlCvfXCStartV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_start),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfXCEndV,NhlCvfXCEndV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_end),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfYCStartV,NhlCvfYCStartV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_start),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfYCEndV,NhlCvfYCEndV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_end),NhlTImmediate,_NhlUSET(NULL),0,NULL},

	{NhlNvfXCStartSubsetV,NhlCvfXCStartSubsetV,
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_subset_start),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfXCEndSubsetV,NhlCvfXCEndSubsetV,
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_subset_end),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfYCStartSubsetV,NhlCvfYCStartSubsetV,
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_subset_start),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNvfYCEndSubsetV,NhlCvfYCEndSubsetV,
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_subset_end),NhlTImmediate,_NhlUSET(NULL),0,NULL},

	{NhlNvfXCStartIndex,NhlCvfXCStartIndex,NhlTInteger,sizeof(int),
		 Oset(x_index_start),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),0,NULL},
	{NhlNvfXCEndIndex,NhlCvfXCEndIndex,NhlTInteger,sizeof(int),
		 Oset(x_index_end),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),0,NULL},
	{NhlNvfYCStartIndex,NhlCvfYCStartIndex,NhlTInteger,sizeof(int),
		 Oset(y_index_start),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),0,NULL},
	{NhlNvfYCEndIndex,NhlCvfYCEndIndex,NhlTInteger,sizeof(int),
		 Oset(y_index_end),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),0,NULL},

	{NhlNvfXCStride,NhlCvfXCStride,NhlTInteger,sizeof(int),
		Oset(x_stride),NhlTImmediate,_NhlUSET((NhlPointer)1),0,NULL},
	{NhlNvfYCStride,NhlCvfYCStride,NhlTInteger,sizeof(int),
		Oset(y_stride),NhlTImmediate,_NhlUSET((NhlPointer)1),0,NULL},

   	{NhlNvfXCActualStartF,NhlCvfXCActualStartF,NhlTFloat,sizeof(float),
		 Oset(x_actual_start),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_GONLY,NULL},
	{NhlNvfXCActualEndF,NhlCvfXCActualEndF,NhlTFloat,sizeof(float),
		 Oset(x_actual_end),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_GONLY,NULL},
	{NhlNvfYCActualStartF,NhlCvfYCActualStartF,NhlTFloat,sizeof(float),
		 Oset(y_actual_start),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_GONLY,NULL},
	{NhlNvfYCActualEndF,NhlCvfYCActualEndF,NhlTFloat,sizeof(float),
		 Oset(y_actual_end),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_GONLY,NULL}

/* End-documented-resources */

};
#undef Oset

/* base methods */

static NhlErrorTypes VectorFieldClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc	/* lc to init	*/
#endif
);

static NhlErrorTypes VectorFieldClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes VectorFieldInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes VectorFieldSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes    VectorFieldGetValues(
#if	NhlNeedProto
        NhlLayer	layer,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes VectorFieldDestroy(
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

NhlVectorFieldFloatClassRec NhlvectorFieldFloatClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"VectorFieldFloatClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlVectorFieldFloatLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlobjClassRec,
/* cvt_table			*/	NULL,

/* resources			*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	},
	/* NhlVectorFieldFloatLayerPart */
	{
/* foo				*/	0
	}
};

NhlVectorFieldClassRec NhlvectorFieldClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"vectorFieldClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlVectorFieldLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)
						&NhldataItemClassRec,
/* cvt_table			*/	NULL,
/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,

/* class_part_initialize	*/	VectorFieldClassPartInitialize,
/* class_initialize		*/	VectorFieldClassInitialize,
/* layer_initialize		*/	VectorFieldInitialize,
/* layer_set_values		*/	VectorFieldSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	VectorFieldGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	VectorFieldDestroy,

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
	/* NhlVectorFieldClassPart */
	{
/* foo				*/	0
	}
};
	
NhlClass NhlvectorFieldClass = (NhlClass)
					&NhlvectorFieldClassRec;
NhlClass NhlvectorFieldFloatClass = (NhlClass)
					&NhlvectorFieldFloatClassRec;

static	NrmQuark	Qfloat  = NrmNULLQUARK;
static	NrmQuark	Qint  = NrmNULLQUARK;
static	NrmQuark	Qgen_array  = NrmNULLQUARK;
static	NrmQuark	Qd_arr  = NrmNULLQUARK;
static	NrmQuark	Qu_arr  = NrmNULLQUARK;
static	NrmQuark	Qv_arr  = NrmNULLQUARK;
static	NrmQuark	Qx_arr  = NrmNULLQUARK;
static	NrmQuark	Qy_arr  = NrmNULLQUARK;
static	NrmQuark	Qmissing_u_value  = NrmNULLQUARK;
static	NrmQuark	Qmissing_v_value  = NrmNULLQUARK;
static	NrmQuark	Qmag_min  = NrmNULLQUARK;
static	NrmQuark	Qmag_max  = NrmNULLQUARK;
static	NrmQuark	Qu_min  = NrmNULLQUARK;
static	NrmQuark	Qu_max  = NrmNULLQUARK;
static	NrmQuark	Qv_min  = NrmNULLQUARK;
static	NrmQuark	Qv_max  = NrmNULLQUARK;
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


typedef enum _vfCoord { vfXCOORD, vfYCOORD} vfCoord;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

/*
 * Function:	nhlfvectorfieldclass
 *
 * Description:	fortran ref to vectorfield class
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
_NHLCALLF(nhlfvectorfieldclass,NHLFVECTORFIELDCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlvectorFieldClass;
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
 * Function:	SplitArray
 *
 * Description:	Splits the vector field data input in the form of a single
 *		3D array into two 2D arrays suitable for use with
 *		VectorPlot or StreamPlot
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
SplitArray
#if	NhlNeedProto
(
	NhlGenArray		ga,
	NhlBoolean		overwrite_ok,
	NhlGenArray		*uga,
	NhlGenArray		*vga,
	NhlString		entry_name
)
#else
(ga,overwrite_ok,uga,vga,entry_name)
	NhlGenArray		ga;
	NhlBoolean		overwrite_ok;
	NhlGenArray		*uga;
	NhlGenArray		*vga;
	NhlString		entry_name;
#endif
{
	char		*e_text;
	int		num_el;
	float		*u, *v;
/* 
 * Split into two arrays. The original array is 2 by N by M, where the
 * slowest dimension has 2 elements.
 * If okay to overwrite, create another GenArray wrapper for the 2nd
 * array, and modify the info in the first array to make it seem like
 * a 2d rather than a 3d array. 
 * Otherwise, create 2 2d arrays.
 * In either case, overwrite_ok will be true on exit.
 */

	num_el = ga->len_dimensions[1] * ga->len_dimensions[2];
	if (overwrite_ok) {
		*vga = (NhlGenArray) NhlConvertMalloc(sizeof(NhlGenArrayRec));
		if (! *vga) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		(*vga)->len_dimensions =
			(int *) NhlConvertMalloc(2 * sizeof(int));
		if (! (*vga)->len_dimensions) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		(*vga)->data = (NhlPointer)(((float *)ga->data) + num_el);
		(*vga)->len_dimensions[0] = ga->len_dimensions[0] =
			ga->len_dimensions[1];
		(*vga)->len_dimensions[1] = ga->len_dimensions[1] =
			ga->len_dimensions[2];
		(*vga)->num_dimensions = ga->num_dimensions = 2;
		(*vga)->num_elements = ga->num_elements = num_el;
		(*vga)->typeQ = ga->typeQ;
		(*vga)->size = ga->size;
		(*vga)->my_data = ga->my_data;
		*uga = ga;
		return NhlNOERROR;
	}
	u = (float *) NhlConvertMalloc(num_el * sizeof(float));
	v = (float *) NhlConvertMalloc(num_el * sizeof(float));
	if (! u || ! v) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	memcpy(u,ga->data,num_el * sizeof(float));
	memcpy(v,((float*)ga->data) + num_el,num_el * sizeof(float));

	*uga = (NhlGenArray) NhlConvertMalloc(sizeof(NhlGenArrayRec));
	*vga = (NhlGenArray) NhlConvertMalloc(sizeof(NhlGenArrayRec));
	if (! *uga || ! *vga) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	(*uga)->len_dimensions = (int *) NhlConvertMalloc(2 * sizeof(int));
	(*vga)->len_dimensions = (int *) NhlConvertMalloc(2 * sizeof(int));
	       
	if (! (*uga)->len_dimensions || ! (*vga)->len_dimensions) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	(*uga)->len_dimensions[0] 
		= (*vga)->len_dimensions[0] = ga->len_dimensions[1];
	(*uga)->len_dimensions[1] 
		= (*vga)->len_dimensions[1] = ga->len_dimensions[2];
	(*uga)->num_elements = (*vga)->num_elements = num_el;
	(*uga)->typeQ = (*vga)->typeQ = Qfloat;
	(*uga)->size = (*vga)->size = sizeof(float);
	(*uga)->my_data = (*vga)->my_data = True;
	(*uga)->data = (NhlPointer) u;
	(*vga)->data = (NhlPointer) v;

	return NhlNOERROR;
}
	
		
/*
 * Function:	GetUVGenArrays
 *
 * Description:	
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
GetUVGenArrays
#if	NhlNeedProto
(
 	NhlVectorFieldLayerPart *vfp,
	NhlGenArray		*uga,
	NhlGenArray		*vga,
	NhlBoolean		*overwrite_ok,
	NhlString		entry_name
)
#else
(vfp,uga,vga,overwrite_ok,entry_name)
 	NhlVectorFieldLayerPart *vfp;
	NhlGenArray		*uga;
	NhlGenArray		*vga;
	NhlBoolean		*overwrite_ok;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;

	*overwrite_ok = False;
	if (vfp->use_d_arr) {
		NhlGenArray ga;
		NhlBoolean  ow_ok;

		if ((ga = GenToFloatGenArray(vfp->d_arr)) == NULL) {
			e_text = "%s: error converting data to float data";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if (ga->num_dimensions != 3 || ga->typeQ != Qfloat) {
			e_text = 
			    "%s: internal inconsistency in float data array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		ow_ok = (ga != vfp->d_arr) ? True : False;
		SplitArray(ga,ow_ok,uga,vga,entry_name);
		*overwrite_ok = True;
		if (vfp->exchange_uv_data) {
			NhlGenArray tmp;
			tmp = *uga;
			*uga = *vga;
			*vga = tmp;
		}

		return ret;
	}
	if ((*uga = GenToFloatGenArray(vfp->u_arr)) == NULL) {
		e_text = "%s: error converting %s data to float";
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  e_text,entry_name,NhlNvfUDataArray);
		return NhlFATAL;
	}
	if ((*uga)->num_dimensions != 2 || (*uga)->typeQ != Qfloat) {
		e_text = "%s: internal inconsistency in %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  e_text,entry_name,NhlNvfUDataArray);
		return NhlFATAL;
	}
	if ((*vga = GenToFloatGenArray(vfp->v_arr)) == NULL) {
		e_text = "%s: error converting %s data to float";
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  e_text,entry_name,NhlNvfVDataArray);
		return NhlFATAL;
	}
	if ((*vga)->num_dimensions != 2 || (*vga)->typeQ != Qfloat) {
		e_text = "%s: internal inconsistency in %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  e_text,entry_name,NhlNvfVDataArray);
		return NhlFATAL;
	}
	if ((*uga) != vfp->u_arr && (*vga) != vfp->v_arr) *overwrite_ok = True;

	if (vfp->exchange_uv_data) {
		NhlGenArray tmp;
		tmp = *uga;
		*uga = *vga;
		*vga = tmp;
	}

	return ret;
	
}

/*
 * Function:	DataToVFField
 *
 * Description:	This function converts the incoming data GenArray(s) 
 *		into the internal float GenArray type used by the
 *		VectorFieldFloat object. New space for data is allocated 
 *		only if a an x and/or y stride greater than unity is 
 *		specified and the data has not already been copied to a
 *		new array by the HLU converter code. 
 *		Fields in the VectorFieldFloat object are filled in.
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
DataToVFField
#if	NhlNeedProto
(
 	NhlVectorFieldLayerPart *vfp,
	NhlvfMissMode		miss_mode,
	float			u_missing,
	float			v_missing,
	NhlString		entry_name
)
#else
(vfp,miss_mode,u_missing,v_missing,entry_name)
 	NhlVectorFieldLayerPart *vfp;
	NhlvfMissMode		miss_mode;
	float			u_missing;
	float			v_missing;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlVectorFieldFloatLayerPart *vffp = &vfp->vffloat->vfieldfloat;
	NhlBoolean	overwrite_ok;
	NhlBoolean	do_mag_minmax,do_uv_minmax;
	int		valid_data_count = 0;
	int		out_len[2];
	int		i,j;
	float		*uf,*vf;
	int		inlen_1;
	int		ixstart = vfp->ix_start;
	int		ixend = vfp->ix_end;
	int		iystart = vfp->iy_start;
	int		iyend = vfp->iy_end;
	NhlGenArray	uga,vga;
	float		*u, *v;
	float		new_data,mag,mag_min,mag_max,u_min,u_max,v_min,v_max;
	float		ut,vt;
	int		ix,ox;
	NhlBoolean	missing;
	int		x_stride, y_stride;

	if ((ret = GetUVGenArrays(vfp,&uga,&vga,
				  &overwrite_ok,entry_name)) < NhlWARNING) {
		return ret;
	}
	
	do_mag_minmax = (vfp->mag_min == NULL || vfp->mag_max == NULL) ?
		True : False;

	do_uv_minmax = (vfp->u_min == NULL || vfp->u_max == NULL ||
		 vfp->v_min == NULL || vfp->v_max == NULL) ?
			 True : False;
	mag_min = u_min = v_min = FLT_MAX;
	mag_max = u_max = v_max = -FLT_MAX;
	inlen_1 = uga->len_dimensions[1];
	x_stride = vfp->x_stride;
	y_stride = vfp->y_stride;
	out_len[1] = (ixend - ixstart + 1) / x_stride +
		((ixend - ixstart + 1) % x_stride > 0);
	out_len[0] = (iyend - iystart + 1) / y_stride +
		((iyend - iystart + 1) % y_stride > 0);
	new_data = x_stride > 1 || y_stride > 1;
	
	u = (float *)uga->data;
	v = (float *)vga->data;
	if (new_data) {
		if (overwrite_ok) {
			uf = u;
			vf = v;
		}
		else {
			if ((uf = (float *) 
			     NhlConvertMalloc(out_len[1] * out_len[0] * 
					      sizeof(float))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			if ((vf = (float *) 
			     NhlConvertMalloc(out_len[1] * out_len[0] * 
					      sizeof(float))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
		}
		for (i = 0; i < out_len[0]; i++) {
			for (j = 0; j < out_len[1]; j++) {
				ix = inlen_1 * (iystart+i*y_stride) +
					ixstart+j*x_stride;
				ut = *(u+ix);
				vt = *(v+ix);
				switch (miss_mode) {
				case vfNONE:
				default:
					missing = False;
					break;
				case vfBOTH:
					missing = (ut == u_missing 
						   || vt == v_missing);
					break;
				case vfUONLY:
					missing = (ut == u_missing);
					break;
				case vfVONLY:
					missing = (vt == v_missing);
					break;
				}
				if (! missing) {
					valid_data_count++;
					if (do_mag_minmax) {
						if (vfp->polar_data)
							mag = ut;
						else 
							mag = ut*ut+vt*vt;
						if (mag < mag_min) 
							mag_min = mag;
						if (mag > mag_max) 
							mag_max = mag;
					}
					if (do_uv_minmax) {
						if (ut < u_min) u_min = ut;
						if (ut > u_max) u_max = ut;
						if (vt < v_min) v_min = vt;
						if (vt > v_max) v_max = vt;
					}
				}
				ox = i * out_len[1] + j;
				*(uf+(ox)) = ut;
				*(vf+(ox)) = vt;
			}
		}
		if (! overwrite_ok) {
			uga = (NhlGenArray) 
				NhlConvertMalloc(sizeof(NhlGenArrayRec));
			vga = (NhlGenArray) 
				NhlConvertMalloc(sizeof(NhlGenArrayRec));
			if (uga == NULL || vga == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}

			uga->num_dimensions = vga->num_dimensions = 2;
			uga->len_dimensions = 
				(int *) NhlConvertMalloc(2 * sizeof(int));
			vga->len_dimensions = 
				(int *) NhlConvertMalloc(2 * sizeof(int));
			if (! uga->len_dimensions || ! vga->len_dimensions) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			uga->len_dimensions[0] 
				= vga->len_dimensions[0] = out_len[0];
			uga->len_dimensions[1] 
				= vga->len_dimensions[1] = out_len[1];
			uga->num_elements = vga->num_elements =
				out_len[0] * out_len[1];
			uga->typeQ = vga->typeQ = Qfloat;
			uga->size = vga->size = sizeof(float);
			uga->my_data = vga->my_data = True;
			uga->data = (NhlPointer) uf;
			vga->data = (NhlPointer) vf;
		}
	}
	else {
		for (i = 0; i < out_len[0]; i++) {
			for (j = 0; j < out_len[1]; j++) {
				ix = inlen_1 * (iystart+i) + ixstart+j;
				ut = *(u+ix);
				vt = *(v+ix);
				switch (miss_mode) {
				case vfNONE:
				default:
					missing = False;
					break;
				case vfBOTH:
					missing = (ut == u_missing 
						   || vt == v_missing);
					break;
				case vfUONLY:
					missing = (ut == u_missing);
					break;
				case vfVONLY:
					missing = (vt == v_missing);
					break;
				}
				if (! missing) {
					valid_data_count++;
					if (do_mag_minmax) {
						if (vfp->polar_data)
							mag = ut;
						else 
							mag = ut*ut+vt*vt;
						if (mag < mag_min) 
							mag_min = mag;
						if (mag > mag_max) 
							mag_max = mag;
					}
					if (do_uv_minmax) {
						if (ut < u_min) u_min = ut;
						if (ut > u_max) u_max = ut;
						if (vt < v_min) v_min = vt;
						if (vt > v_max) v_max = vt;
					}
				}
			}
		}
	}
	vffp->u_arr = uga;
	vffp->v_arr = vga;
	if (valid_data_count == 0) {
		if (miss_mode == vfVONLY) {
			vffp->mag_min = v_missing;
			vffp->mag_max = v_missing;
			vffp->u_min = v_missing;
			vffp->u_max = v_missing;
		}
		else {
			vffp->mag_min = u_missing;
			vffp->mag_max = u_missing;
			vffp->u_min = u_missing;
			vffp->u_max = u_missing;
		}
		if (miss_mode == vfUONLY) {
			vffp->v_min = u_missing;
			vffp->v_max = u_missing;
		}
		else {
			vffp->v_min = v_missing;
			vffp->v_max = v_missing;
		}
	}
	else {
		if (do_mag_minmax) {
			if (vfp->polar_data) {
				if (mag_min < 0.0) {
					e_text = "%s: polar data invalid";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				vffp->mag_min = mag_min;
				vffp->mag_max = mag_max;
			}
			else {
				vffp->mag_min = sqrt(mag_min);
				vffp->mag_max = sqrt(mag_max);
			}
		}
		if (do_uv_minmax) {
			vffp->u_min = u_min;
			vffp->u_max = u_max;
			vffp->v_min = v_min;
			vffp->v_max = v_max;
		}
	}

/*
 * If the user passed in a float array and stride values are all unity,
 * then the array is not copied. Set values that will be used to 
 * indicate to the low level routines what portion of the array to use.
 * If a copy was made, the entire array will be utilitized.
 */
	if (! new_data) {
		vffp->begin = iystart * uga->len_dimensions[1] + ixstart;
		vffp->fast_dim = uga->len_dimensions[1];
		vffp->fast_len = ixend - ixstart + 1;
		vffp->slow_len = iyend - iystart + 1;
	}
	else {
		vffp->begin = 0;
		vffp->fast_dim = uga->len_dimensions[1];
		vffp->fast_len = uga->len_dimensions[1];
		vffp->slow_len = uga->len_dimensions[0];
	}
	vffp->x_start = vfp->x_actual_start;
	vffp->x_end = vfp->x_actual_end;
	vffp->y_start = vfp->y_actual_start;
	vffp->y_end = vfp->y_actual_end;

	return ret;
}


/*
 * Function:	DataToVFFieldExchDim
 *
 * Description:	This function converts the incoming data GenArray(s) 
 *		into the internal float GenArray type used by the
 *		VectorFieldFloat object. The array dimensions are exchanged.
 *		New space is always allocated for the output data.
 *		Fields in the VectorFieldFloat object are filled in.
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
DataToVFFieldExchDim
#if	NhlNeedProto
(
 	NhlVectorFieldLayerPart *vfp,
	NhlvfMissMode		miss_mode,
	float			u_missing,
	float			v_missing,
	NhlString		entry_name
)
#else
(vfp,miss_mode,u_missing,v_missing,entry_name)
 	NhlVectorFieldLayerPart *vfp;
	NhlvfMissMode		miss_mode;
	float			u_missing;
	float			v_missing;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlVectorFieldFloatLayerPart *vffp = &vfp->vffloat->vfieldfloat;
	NhlBoolean	overwrite_ok;
	NhlBoolean	do_mag_minmax,do_uv_minmax;
	int		valid_data_count = 0;
	int		out_len[2];
	int		i,j;
	float		*uf,*vf;
	int		inlen_1;
	int		ixstart = vfp->ix_start;
	int		ixend = vfp->ix_end;
	int		iystart = vfp->iy_start;
	int		iyend = vfp->iy_end;
	NhlGenArray	uga,vga;
	float		*u, *v;
	float		mag,mag_min,mag_max,u_min,u_max,v_min,v_max;
	float		ut,vt;
	int		ix,ox;
	NhlBoolean	missing;
	int		x_stride,y_stride;


	if ((ret = GetUVGenArrays(vfp,&uga,&vga,
				  &overwrite_ok,entry_name)) < NhlWARNING) {
		return ret;
	}
	
	do_mag_minmax = (vfp->mag_min == NULL || vfp->mag_max == NULL) ?
		True : False;

	do_uv_minmax = (vfp->u_min == NULL || vfp->u_max == NULL ||
		 vfp->v_min == NULL || vfp->v_max == NULL) ?
			 True : False;
	mag_min = u_min = v_min = FLT_MAX;
	mag_max = u_max = v_max = -FLT_MAX;
	inlen_1 = uga->len_dimensions[1];
/*
 * Assign the dimension length according to the lengths needed by
 * the new output array (0-slow,1-fast). This is , of course, the reverse
 * of the input array. (Note that the resources XCStartV, etc. still refer
 * to the X axis, and likewise for Y related resources.
 */
	x_stride = vfp->x_stride;
	y_stride = vfp->y_stride;
	out_len[1] = (iyend - iystart + 1) / y_stride +
		((iyend - iystart + 1) % y_stride > 0);
	out_len[0] = (ixend - ixstart + 1) / x_stride +
		((ixend - ixstart + 1) % x_stride > 0);

/*
 * Eventually if overwrite is ok then the data will be exchanged in place.
 * But for now in this situation make a temporary copy of the 
 * output array, and after the exchange copy it over the original array.
 */

	u = (float *)uga->data;
	v = (float *)vga->data;
	if (overwrite_ok) {
		uf = (float *) 
			NhlMalloc(out_len[1] * out_len[0] * sizeof(float));
		vf = (float *) 
			NhlMalloc(out_len[1] * out_len[0] * sizeof(float));
	}
	else {
		uf = (float *) 
		    NhlConvertMalloc(out_len[1] * out_len[0] * sizeof(float));
		vf = (float *) 
		    NhlConvertMalloc(out_len[1] * out_len[0] * sizeof(float));
	}
	if (uf == NULL || vf == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	for (i = 0; i < out_len[1]; i++) {
		for (j = 0; j < out_len[0]; j++) {
			ix = inlen_1 * (iystart + i * y_stride)
				+ ixstart + j * x_stride;
			ut = *(u+ix);
			vt = *(v+ix);
			switch (miss_mode) {
			case vfNONE:
			default:
				missing = False;
				break;
			case vfBOTH:
				missing = (ut == u_missing 
					   || vt == v_missing);
				break;
			case vfUONLY:
				missing = (ut == u_missing);
				break;
			case vfVONLY:
				missing = (vt == v_missing);
				break;
			}
			if (! missing) {
				valid_data_count++;
				if (do_mag_minmax) {
					if (vfp->polar_data)
						mag = ut;
					else 
						mag = ut*ut+vt*vt;
					if (mag < mag_min) mag_min = mag;
					if (mag > mag_max) mag_max = mag;
				}
				if (do_uv_minmax) {
					if (ut < u_min) u_min = ut;
					if (ut > u_max) u_max = ut;
					if (vt < v_min) v_min = vt;
					if (vt > v_max) v_max = vt;
				}
			}
			ox = j * out_len[1] + i;
			*(uf + ox) = ut;
			*(vf + ox) = vt;
		}
	}

	if (overwrite_ok) {
		int num_el = out_len[1] * out_len[0];
		memcpy(u,uf,num_el * sizeof(float));
		memcpy(v,vf,num_el * sizeof(float));
		uga->len_dimensions[0] = vga->len_dimensions[0] = out_len[0];
		uga->len_dimensions[1] = vga->len_dimensions[1] = out_len[1];
		uga->num_elements = vga->num_elements = num_el;
		NhlFree(uf);
		NhlFree(vf);
	}
	else {
		uga = (NhlGenArray) 
			NhlConvertMalloc(sizeof(NhlGenArrayRec));
		vga = (NhlGenArray) 
			NhlConvertMalloc(sizeof(NhlGenArrayRec));
		if (uga == NULL || vga == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name);
			return NhlFATAL;
		}
		uga->num_dimensions = vga->num_dimensions = 2;
		uga->len_dimensions = 
			(int *) NhlConvertMalloc(2 * sizeof(int));
		vga->len_dimensions = 
			(int *) NhlConvertMalloc(2 * sizeof(int));
		if (! uga->len_dimensions || ! vga->len_dimensions) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name);
			return NhlFATAL;
		}
		uga->len_dimensions[0] = vga->len_dimensions[0] = out_len[0];
		uga->len_dimensions[1] = vga->len_dimensions[1] = out_len[1];
		uga->num_elements = 
			vga->num_elements = out_len[0] * out_len[1];
		uga->typeQ = vga->typeQ = Qfloat;
		uga->size = vga->size = sizeof(float);
		uga->my_data = vga->my_data = True;
		uga->data = (NhlPointer) uf;
		vga->data = (NhlPointer) vf;
	}

	vffp->u_arr = uga;
	vffp->v_arr = vga;
	if (valid_data_count == 0) {
		if (miss_mode == vfVONLY) {
			vffp->mag_min = v_missing;
			vffp->mag_max = v_missing;
			vffp->u_min = v_missing;
			vffp->u_max = v_missing;
		}
		else {
			vffp->mag_min = u_missing;
			vffp->mag_max = u_missing;
			vffp->u_min = u_missing;
			vffp->u_max = u_missing;
		}
		if (miss_mode == vfUONLY) {
			vffp->v_min = u_missing;
			vffp->v_max = u_missing;
		}
		else {
			vffp->v_min = v_missing;
			vffp->v_max = v_missing;
		}
	}
	else {
		if (do_mag_minmax) {
			if (vfp->polar_data) {
				if (mag_min < 0.0) {
					e_text = "%s: polar data invalid";
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  e_text,entry_name);
					return NhlFATAL;
				}
				vffp->mag_min = mag_min;
				vffp->mag_max = mag_max;
			}
			else {
				vffp->mag_min = sqrt(mag_min);
				vffp->mag_max = sqrt(mag_max);
			}
		}
		if (do_uv_minmax) {
			vffp->u_min = u_min;
			vffp->u_max = u_max;
			vffp->v_min = v_min;
			vffp->v_max = v_max;
		}
	}
	
	vffp->begin = 0;
	vffp->fast_dim = uga->len_dimensions[1];
	vffp->fast_len = uga->len_dimensions[1];
	vffp->slow_len = uga->len_dimensions[0];

	vffp->x_start = vfp->y_actual_start;
	vffp->x_end = vfp->y_actual_end;
	vffp->y_start = vfp->x_actual_start;
	vffp->y_end = vfp->x_actual_end;

	return ret;
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
 *		specify irregular vector field grids. 
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
 	NhlVectorFieldLayerPart *vfp,
	NhlGenArray		ga,
	vfCoord			ctype,
	NhlString		entry_name
)
#else
(vfp,ga,ctype,entry_name)
 	NhlVectorFieldLayerPart *vfp;
	NhlGenArray		ga;
	vfCoord			ctype;
	NhlString		entry_name;
#endif
{
	char *e_text;
	int len_dim;
	char *name;

	if (ctype == vfXCOORD) {
		len_dim = vfp->len_dims[1];
		name = NhlNvfXArray;
	}
	else {
		len_dim = vfp->len_dims[1];
		name = NhlNvfYArray;
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
 	NhlVectorFieldLayerPart *vfp,
	vfCoord			ctype,
	float			*cstart,
	float			*cend,
	NhlString		entry_name
)
#else
(vfp,ctype,cstart,cend,entry_name)
 	NhlVectorFieldLayerPart *vfp;
	vfCoord			ctype;
	float			*cstart;
	float			*cend;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;

	if (ctype == vfXCOORD) {

		if (vfp->x_start != NULL) {
			subret = GetVTypeValue(vfp->x_start,cstart);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else if (vfp->x_subset_start != NULL) {
			subret = GetVTypeValue(vfp->x_subset_start,cstart);
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

		if (vfp->x_end != NULL) {
			subret = GetVTypeValue(vfp->x_end,cend);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else if (vfp->x_subset_end != NULL) {
			subret = GetVTypeValue(vfp->x_subset_end,cend);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else 
			*cend = vfp->len_dims[1] - 1;
	}
	else {
		if (vfp->y_start != NULL) {
			subret = GetVTypeValue(vfp->y_start,cstart);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else if (vfp->y_subset_start != NULL) {
			subret = GetVTypeValue(vfp->y_subset_start,cstart);
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
		
		if (vfp->y_end != NULL) {
			subret = GetVTypeValue(vfp->y_end,cend);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else if (vfp->y_subset_end != NULL) {
			subret = GetVTypeValue(vfp->y_subset_end,cend);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return ret;
			}
		}
		else 
			*cend = vfp->len_dims[0] - 1;
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
 	NhlVectorFieldLayerPart *vfp,
	vfCoord			ctype,
	int			*icstart,
	int			*icend,
	NhlString		entry_name
)
#else
(vfp,ctype,icstart,icend,entry_name)
 	NhlVectorFieldLayerPart *vfp;
	vfCoord			ctype;
	int			*icstart;
	int			*icend;
	NhlString		entry_name;
#endif
{
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR;
	int			max_index;
			
	if (ctype == vfXCOORD) {

		max_index = vfp->len_dims[1] - 1;

		*icstart = (vfp->x_index_start < 0) ? 
			0 : MIN(vfp->x_index_start,max_index);
		if (vfp->x_index_end > max_index) {
			e_text = 
		      "%s: X index end exceeds data boundaries: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(NhlWARNING,ret);
			vfp->x_index_end = max_index;
		}
		*icend = (vfp->x_index_end < 0) ? 
			max_index  :  vfp->x_index_end;

		if (*icend - *icstart < 2) {
			e_text = 
	       "%s: X index end not enough greater than start: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(NhlWARNING,ret);
			*icstart = vfp->x_index_start = 0;
			*icend = vfp->x_index_end = max_index;
		}
		
	}
	else {

		max_index = vfp->len_dims[0] - 1;

		*icstart = (vfp->y_index_start < 0) ? 
			0 : MIN(vfp->y_index_start,max_index); 
		if (vfp->y_index_end > max_index) {
			e_text = 
		      "%s: Y index end exceeds data boundaries: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(NhlWARNING,ret);
			vfp->y_index_end = max_index;
		}
		*icend = (vfp->y_index_end < 0) ? 
			max_index : vfp->y_index_end;

		if (*icend - *icstart < 2) {
			e_text = 
	       "%s: Y index end not enough greater than start: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(NhlWARNING,ret);
			*icstart = vfp->y_index_start = 0;
			*icend = vfp->y_index_end = max_index;
		}
	}

	return ret;

}

/*
 * Function:	GetSubsetBounds
 *
 * Description:	Depending on the value of the NhlNvfSubsetByIndex resource,
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
 * In Args:	vfp
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
 	NhlVectorFieldLayerPart *vfp,
	vfCoord			ctype,
	float			cstart,
	float			cend,
	int			*icstart,
	int			*icend,
	float			*scstart,
	float			*scend,
	NhlString		entry_name
)
#else
(vfp,ctype,cstart,cend,icstart,icend,scstart,scend,entry_name)
 	NhlVectorFieldLayerPart *vfp;
	vfCoord			ctype;
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
	

	if (ctype == vfXCOORD) {
		range =  vfp->len_dims[1] - 1;
		subset_start = vfp->x_subset_start;
		subset_end = vfp->x_subset_end;
		stride = vfp->x_stride;
		c_name = "X coordinate";
	}
	else {
		range = vfp->len_dims[0] - 1;
		subset_start = vfp->y_subset_start;
		subset_end = vfp->y_subset_end;
		stride = vfp->y_stride;
		c_name = "Y coordinate";
	}

	if (! vfp->subset_by_index) {
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
 * Description:	Depending on the value of the NhlNvfSubsetByIndex resource,
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
 * In Args:	vfp
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
 	NhlVectorFieldLayerPart *vfp,
	NhlGenArray		*c_array,
	vfCoord			ctype,
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
(vfp,c_array,ctype,overwrite_ok,
 cstart,cend,icstart,icend,scstart,scend,entry_name)
 	NhlVectorFieldLayerPart *vfp;
	NhlGenArray		*c_array;
	vfCoord			ctype;
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

	if (ctype == vfXCOORD) {
		subset_start = vfp->x_subset_start;
		subset_end = vfp->x_subset_end;
		stride = vfp->x_stride;
		c_name = "X coordinate";
	}
	else {
		subset_start = vfp->y_subset_start;
		subset_end = vfp->y_subset_end;
		stride = vfp->y_stride;
		c_name = "Y coordinate";
	}

	len = (*c_array)->len_dimensions[0];
	fp = (float *) (*c_array)->data;
	*cstart = fp[0];
	*cend = fp[len-1];

	if (! vfp->subset_by_index) {
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
 * In Args:	vfp
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
 	NhlVectorFieldLayerPart *vfp,
	vfCoord			ctype,
	float			*cstart,
	float			*cend,
	int			*icstart,
	int			*icend,
	float			*scstart,
	float			*scend,
	NhlString		entry_name
)
#else
(vfp,ctype,cstart,cend,icstart,icend,scstart,scend,entry_name)
 	NhlVectorFieldLayerPart *vfp;
	vfCoord			ctype;
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

	subret = GetDataBounds(vfp,ctype,cstart,cend,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;

	subret = GetIndexBounds(vfp,ctype,icstart,icend,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;

	subret = GetSubsetBounds(vfp,ctype,*cstart,*cend,
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
 * In Args:	vfp
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
 	NhlVectorFieldLayerPart *vfp,
	NhlGenArray		*c_array,
	vfCoord			ctype,
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
( vfp, c_array, ctype, overwrite_ok, cstart, cend, icstart, icend, scstart, scend, entry_name)
 	NhlVectorFieldLayerPart *vfp;
	NhlGenArray		*c_array;
	vfCoord			ctype;
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

	subret = GetIndexBounds(vfp,ctype,icstart,icend,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;

	subret = GetSubsetBoundsIrregular(vfp,c_array,ctype,overwrite_ok,
					  cstart,cend,icstart,icend,
					  scstart,scend,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;
	
	return ret;
}

/*
 * Function:	CvtGenVFObjToFloatVFObj
 *
 * Description:	This function is used to convert a Generic VectorField
 *		to a VectorFieldFloat object.
 * 		Note that the VectorFieldFloat object has no resources 
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
CvtGenVFObjToFloatVFObj
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
	char			*entry_name="CvtGenVFObjToFloatVFObj";
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlVectorFieldLayer	vfl;
	NhlVectorFieldLayerPart *vfp;
	NhlSArg			sargs[30];
	int			nargs=0;
	NhlGenArray		x_arr = NULL, y_arr = NULL;
	float			xstart,xend,ystart,yend;
	float			sxstart,sxend,systart,syend;
	int			ixstart,ixend,iystart,iyend;
	NhlBoolean		xirr = False, yirr = False;
	float			u_missing,v_missing;
	NhlBoolean		overwrite_ok;
	float			tmin,tmax;
	NhlvfMissMode		miss_mode;
	NhlBoolean		u_miss_set, v_miss_set;
	NhlVectorFieldFloatLayer	vffl;
	NhlVectorFieldFloatLayerPart	*vffp;
/*
 * Check input and retrieve a pointer to the data object
 */
	if (num_args != 0) {
		e_text = "%s:Called w/wrong args";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	vfl = (NhlVectorFieldLayer)_NhlGetLayer(from->data.intval);
	if ((vfl == NULL) ||
	    (vfl->base.layer_class != NhlvectorFieldClass)){
		e_text = "%s:Called w/ improper \"from\" object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	vfp = (NhlVectorFieldLayerPart *) &vfl->vfield;

	if (vfp->use_d_arr) {
		if (vfp->d_arr == NULL || vfp->d_arr->num_dimensions != 3) {
			e_text = "%s: invalid data array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	else {
		if (vfp->u_arr == NULL || vfp->u_arr->num_dimensions != 2 
		    || vfp->v_arr == NULL || vfp->v_arr->num_dimensions != 2) {
			e_text = "%s: invalid data array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

/*
 * Create a vector field float data object
 */
	subret = NhlALCreate(to->data.ptrval,"no.name",
			     NhlvectorFieldFloatClass,
			     vfl->base.id,sargs,nargs);

	if ((vffl = (NhlVectorFieldFloatLayer)
	     _NhlGetLayer(*((int *)to->data.ptrval))) == NULL) {
		e_text = "%s: error creating vector field float object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	vffp = &vffl->vfieldfloat;
	vfp->x_stride = MAX(1,vfp->x_stride);
	vffp->x_stride = vfp->x_stride;
	vfp->y_stride = MAX(1,vfp->y_stride);
	vffp->y_stride = vfp->y_stride;
	vfp->vffloat  = vffl;
	
/*
 * Convert, validate, and set the X and Y irregular coordinate arrays,
 * if defined.
 */
	vffp->x_arr = NULL;
	if (vfp->x_arr != NULL && vfp->x_arr->num_elements > 0) {
		if ((x_arr = GenToFloatGenArray(vfp->x_arr)) == NULL) {
			e_text = "%s: error converting XCoord data to float";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if (ValidCoordArray(vfp,x_arr,vfXCOORD,entry_name)) {
			NhlSetSArg(&sargs[nargs++],NhlNvfXArray,x_arr);
			xirr = True;
		}
	}

	if (! xirr) {
		subret = GetCoordBounds(vfp,vfXCOORD,&xstart,&xend,
					&ixstart,&ixend,&sxstart,&sxend,
					entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
	}
	else {
		overwrite_ok = x_arr != vfp->x_arr;
		subret = GetCoordBoundsIrregular(vfp,&x_arr,vfXCOORD,
						 overwrite_ok,
						 &xstart,&xend,
						 &ixstart,&ixend,
						 &sxstart,&sxend,
						 entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
		vffp->x_arr = x_arr;
	}
	vfp->ix_start = ixstart;
	vfp->ix_end = ixend;
	vfp->x_actual_start = sxstart;
	vfp->x_actual_end = sxend;

	vffp->y_arr = NULL;
	if (vfp->y_arr != NULL && vfp->y_arr->num_elements > 0) {
		if ((y_arr = GenToFloatGenArray(vfp->y_arr)) == NULL) {
			e_text = "%s: error converting YCoord data to float";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		if (ValidCoordArray(vfp,y_arr,vfYCOORD,entry_name)) {
			NhlSetSArg(&sargs[nargs++],NhlNvfYArray,y_arr);
			yirr = True;
		}
	}

	if (! yirr) {
		subret = GetCoordBounds(vfp,vfYCOORD,&ystart,&yend,
					&iystart,&iyend,&systart,&syend,
					entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
	}
	else {
		overwrite_ok = y_arr != vfp->y_arr;
		subret = GetCoordBoundsIrregular(vfp,&y_arr,vfYCOORD,
						 overwrite_ok,
						 &ystart,&yend,
						 &iystart,&iyend,
						 &systart,&syend,
						 entry_name);
		if ((ret = MIN(ret,subret))  < NhlWARNING) 
			return ret;
		vffp->y_arr = y_arr;
	}
	vfp->iy_start = iystart;
	vfp->iy_end = iyend;
	vfp->y_actual_start = systart;
	vfp->y_actual_end = syend;

	if (vfp->missing_u_value == NULL) {
		u_missing = 0.0;
		u_miss_set = False;
		vffp->u_missing_value = 0.0;
	}
	else {
		subret = GetVTypeValue(vfp->missing_u_value, &u_missing);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		u_miss_set = True;
		vffp->u_missing_value = u_missing;
	}

	if (vfp->missing_v_value == NULL) {
		v_missing = 0.0;
		v_miss_set = False;
		vffp->v_missing_value = 0.0;
	}
	else {
		subret = GetVTypeValue(vfp->missing_v_value, &v_missing);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return ret;
		}
		v_miss_set = True;
		vffp->v_missing_value = v_missing;
	}
	if (vfp->single_missing) {
		if (u_miss_set) {
			vffp->v_missing_value = v_missing = u_missing;
			v_miss_set = True;
		}
		else if (v_miss_set) {
			vffp->u_missing_value = u_missing = v_missing;
			u_miss_set = True;
		}
	}
	if (u_miss_set && v_miss_set)
		miss_mode = vfBOTH;
	else if (u_miss_set)
		miss_mode = vfUONLY;
	else if (v_miss_set)
		miss_mode = vfVONLY;
	else 
		miss_mode = vfNONE;
	vffp->miss_mode = miss_mode;

	if (! vfp->exchange_dimensions) {
		if ((subret = DataToVFField(vfp,miss_mode,
					    u_missing,v_missing,entry_name)) 
		    < NhlWARNING) {
			return NhlFATAL;
		}
	}
	else {
		if ((subret = 
		     DataToVFFieldExchDim(vfp,miss_mode,
					  u_missing,v_missing,entry_name)) 
		    < NhlWARNING) {
			return NhlFATAL;
		}
	}

/*
 * If the user explicitly sets the data min/max values, make sure the max
 * is greater than the min. If not, WARN and exchange. 
 * Otherwise pass on without interpretation to the plot object.
 * Note that if any values have not been set, the actual values should
 * already be entered in the VFFloat data structure.
 */
	tmin = vffp->mag_min;
	if (vfp->mag_min != NULL) {
		subret = GetVTypeValue(vfp->mag_min,&tmin);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,NhlNvfMagMinV);
			return ret;
		}
	}
	tmax = vffp->mag_max;
	if (vfp->mag_max != NULL) {
		subret = GetVTypeValue(vfp->mag_max,&tmax);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,NhlNvfMagMaxV);
			return ret;
		}
	}
	if (tmax < tmin) {
		e_text = "%s: %s greater than %s: exchanging";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNvfMagMinV,NhlNvfMagMaxV);
		vffp->mag_min = tmax;
		vffp->mag_max = tmin;
	}
	else {
		vffp->mag_min = tmin;
		vffp->mag_max = tmax;
	}

	tmin = vffp->u_min;
	if (vfp->u_min != NULL) {
		subret = GetVTypeValue(vfp->u_min,&tmin);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,NhlNvfUMinV);
			return ret;
		}
	}
	tmax = vffp->u_max;
	if (vfp->u_max != NULL) {
		subret = GetVTypeValue(vfp->u_max,&tmax);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,NhlNvfUMaxV);
			return ret;
		}
	}
	if (tmax < tmin) {
		e_text = "%s: %s greater than %s: exchanging";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNvfUMinV,NhlNvfUMaxV);
		vffp->u_min = tmax;
		vffp->u_max = tmin;
	}
	else {
		vffp->u_min = tmin;
		vffp->u_max = tmax;
	}

	tmin = vffp->v_min;
	if (vfp->v_min != NULL) {
		subret = GetVTypeValue(vfp->v_min,&tmin);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,NhlNvfVMinV);
			return ret;
		}
	}
	tmax = vffp->v_max;
	if (vfp->v_max != NULL) {
		subret = GetVTypeValue(vfp->v_max,&tmax);
		if ((ret = MIN(ret,subret)) < NhlWARNING) {
			e_text = "%s: error getting variable type value %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,NhlNvfVMaxV);
			return ret;
		}
	}
	if (tmax < tmin) {
		e_text = "%s: %s greater than %s: exchanging";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNvfVMinV,NhlNvfVMaxV);
		vffp->v_min = tmax;
		vffp->v_max = tmin;
	}
	else {
		vffp->v_min = tmin;
		vffp->v_max = tmax;
	}


	return ret;
}

/************************************************************************
*									*
*	Method definitions						*
*									*
************************************************************************/


/*
 * Function:	VectorFieldClassInitialize
 *
 * Description:	This function does one time initialization needed by the
 *		VectorFieldClass.
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
VectorFieldClassInitialize
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
	Qd_arr  = NrmStringToQuark(NhlNvfDataArray);
	Qu_arr  = NrmStringToQuark(NhlNvfUDataArray);
	Qv_arr  = NrmStringToQuark(NhlNvfVDataArray);
	Qx_arr  = NrmStringToQuark(NhlNvfXArray);
	Qy_arr  = NrmStringToQuark(NhlNvfYArray);
	Qmissing_u_value  = NrmStringToQuark(NhlNvfMissingUValueV);
	Qmissing_v_value  = NrmStringToQuark(NhlNvfMissingVValueV);
	Qmag_min  = NrmStringToQuark(NhlNvfMagMinV);
	Qmag_max  = NrmStringToQuark(NhlNvfMagMaxV);
	Qu_min  = NrmStringToQuark(NhlNvfUMinV);
	Qu_max  = NrmStringToQuark(NhlNvfUMaxV);
	Qv_min  = NrmStringToQuark(NhlNvfVMinV);
	Qv_max  = NrmStringToQuark(NhlNvfVMaxV);
	Qx_start  = NrmStringToQuark(NhlNvfXCStartV);
	Qx_end  = NrmStringToQuark(NhlNvfXCEndV);
	Qy_start  = NrmStringToQuark(NhlNvfYCStartV);
	Qy_end  = NrmStringToQuark(NhlNvfYCEndV);
	Qx_subset_start  = NrmStringToQuark(NhlNvfXCStartSubsetV);
	Qx_subset_end  = NrmStringToQuark(NhlNvfXCEndSubsetV);
	Qy_subset_start  = NrmStringToQuark(NhlNvfYCStartSubsetV);
	Qy_subset_end  = NrmStringToQuark(NhlNvfYCEndSubsetV);
	Qx_index_start  = NrmStringToQuark(NhlNvfXCStartIndex);
	Qx_index_end  = NrmStringToQuark(NhlNvfXCEndIndex);
	Qy_index_start  = NrmStringToQuark(NhlNvfYCStartIndex);
	Qy_index_end  = NrmStringToQuark(NhlNvfYCEndIndex);
	Qx_actual_start  = NrmStringToQuark(NhlNvfXCActualStartF);
	Qx_actual_end  = NrmStringToQuark(NhlNvfXCActualEndF);
	Qy_actual_start  = NrmStringToQuark(NhlNvfYCActualStartF);
	Qy_actual_end  = NrmStringToQuark(NhlNvfYCActualEndF);

	ret = NhlRegisterConverter(NhlbaseClass,
			NhlvectorFieldClass->base_class.class_name,
			NhlvectorFieldFloatClass->base_class.class_name,
			CvtGenVFObjToFloatVFObj,NULL,0,False,NULL);
	return ret;
}

/*
 * Function:	VectorFieldClassPartInitialize
 *
 * Description:	This function is used to init the vfield_class part 
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
VectorFieldClassPartInitialize
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

	ret = _NhlRegisterChildClass(lc,NhlvectorFieldFloatClass,
				     True,False,NULL);

	return ret;
}


/*
 * Function:	VectorFieldInitialize
 *
 * Description:	This function initializes an instance of a VectorField
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
VectorFieldInitialize
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
	char			*entry_name = "VectorFieldInitialize";
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlVectorFieldLayer	vfl = (NhlVectorFieldLayer)new;
	NhlVectorFieldLayerPart	*vfp = &(vfl->vfield);
	NhlGenArray		ga;

	vfp->vffloat = NULL;
	vfp->use_d_arr = False;

	if (! vfp->d_arr &&
	    (! vfp->u_arr || ! vfp->v_arr)) {
		e_text = 
     "%s: either %s or both %s and %s must be specified to create a %s object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NhlNvfDataArray,
			  NhlNvfUDataArray,NhlNvfVDataArray,_NhlClassName(lc));
		return NhlFATAL;
	}

	if (vfp->d_arr) {
		if (vfp->d_arr->num_dimensions != 3) {
			e_text = "%s: invalid number of dimensions in %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
				  entry_name,NhlNvfDataArray);
			return NhlFATAL;
		}
		else if ((vfp->d_arr = 
			  _NhlCopyGenArray(vfp->d_arr,vfp->copy_arrays))
			 == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		vfp->use_d_arr = True;
		vfp->len_dims[0] = vfp->d_arr->len_dimensions[1];
		vfp->len_dims[1] = vfp->d_arr->len_dimensions[2];
	}
	if (! vfp->use_d_arr) {
		if (vfp->u_arr->num_dimensions != 2 || 
		    vfp->v_arr->num_dimensions != 2) {
			e_text = 
			       "%s: invalid number of dimensions in %s or %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNvfUDataArray,NhlNvfVDataArray);
			return NhlFATAL;
		}
		if (vfp->u_arr->len_dimensions[1] !=
		    vfp->v_arr->len_dimensions[1]) {
			e_text = 
			       "%s: dimensions of %s and %s are inconsistent";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNvfUDataArray,NhlNvfVDataArray);
			return NhlFATAL;
		}
		else if ((vfp->u_arr = 
			  _NhlCopyGenArray(vfp->u_arr,vfp->copy_arrays))
			 == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		else if ((vfp->v_arr = 
			  _NhlCopyGenArray(vfp->v_arr,vfp->copy_arrays))
			 == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		vfp->len_dims[1] = vfp->u_arr->len_dimensions[1];
		vfp->len_dims[0] = MIN(vfp->u_arr->len_dimensions[0],
				       vfp->v_arr->len_dimensions[0]);
	}

	if (vfp->x_arr != NULL && 
	    (vfp->x_arr =
	     _NhlCopyGenArray(vfp->x_arr,vfp->copy_arrays)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (vfp->y_arr != NULL && 
	    (vfp->y_arr =
	     _NhlCopyGenArray(vfp->y_arr,vfp->copy_arrays)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (vfp->missing_u_value != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->missing_u_value,
					NhlNvfMissingUValueV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->missing_u_value = ga;
	}
	if (vfp->missing_v_value != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->missing_v_value,
					NhlNvfMissingVValueV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->missing_v_value = ga;
	}

	if (vfp->mag_min != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->mag_min,
					NhlNvfMagMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->mag_min = ga;
	}
	if (vfp->mag_max != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->mag_max,
					NhlNvfMagMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->mag_max = ga;
	}
	if (vfp->u_min != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->u_min,
					NhlNvfUMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->u_min = ga;
	}
	if (vfp->u_max != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->u_max,
					NhlNvfUMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->u_max = ga;
	}
	if (vfp->v_min != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->v_min,
					NhlNvfVMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->v_min = ga;
	}
	if (vfp->v_max != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->v_max,
					NhlNvfVMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->v_max = ga;
	}

	if (vfp->x_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->x_start,
					NhlNvfXCStartV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->x_start = ga;
	}
	if (vfp->x_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->x_end,
					NhlNvfXCEndV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->x_end = ga;
	}


	if (vfp->y_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->y_start,
					NhlNvfYCStartV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->y_start = ga;
	}
	if (vfp->y_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->y_end,
					NhlNvfYCEndV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->y_end = ga;
	}


	if (vfp->x_subset_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->x_subset_start,
					NhlNvfXCStartSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->x_subset_start = ga;
	}
	if (vfp->x_subset_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->x_subset_end,
					NhlNvfXCEndSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->x_subset_end = ga;
	}


	if (vfp->y_subset_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->y_subset_start,
					NhlNvfYCStartSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->y_subset_start = ga;
	}
	if (vfp->y_subset_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,vfp->y_subset_end,
					NhlNvfYCEndSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->y_subset_end = ga;
	}
	
	return ret;
}

/*
 * Function:	VectorFieldSetValues
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
VectorFieldSetValues
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
	char			*entry_name = "VectorFieldSetValues";
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char 			*e_text;
	NhlVectorFieldLayer	vfl = (NhlVectorFieldLayer)new;
	NhlVectorFieldLayer	ovfl = (NhlVectorFieldLayer)old;
	NhlVectorFieldLayerPart	*vfp = &(vfl->vfield);
	NhlVectorFieldLayerPart	*ovfp = &(ovfl->vfield);
	NhlGenArray		ga;
	NhlBoolean		status = False;

	if (vfp->use_d_arr && vfp->d_arr != ovfp->d_arr) {
		if (vfp->d_arr == NULL) {
			e_text = 
			   "%s:The %s resource cannot be set NULL: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
				  entry_name,NhlNvfDataArray);
			ret = NhlWARNING;
			vfp->d_arr = ovfp->d_arr;
		}
		else {
			NhlFreeGenArray(ovfp->d_arr);
			if ((ga = 
			     _NhlCopyGenArray(vfp->d_arr,
					      vfp->copy_arrays)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			vfp->d_arr = ga;
			status = True;
		}
	}
	else if (vfp->u_arr != ovfp->u_arr || vfp->v_arr != ovfp->v_arr) {
		if (vfp->u_arr == NULL) {
			e_text = "%s: %s cannot be set NULL: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNvfUDataArray);
			ret = NhlWARNING;
			vfp->u_arr = ovfp->u_arr;
		}
		else {
			NhlFreeGenArray(ovfp->u_arr);
			if ((ga = 
			     _NhlCopyGenArray(vfp->u_arr,
					      vfp->copy_arrays)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			vfp->u_arr = ga;
			status = True;
		}
		if (vfp->v_arr == NULL) {
			e_text = "%s: %s cannot be set NULL: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNvfVDataArray);
			ret = NhlWARNING;
			vfp->v_arr = ovfp->v_arr;
		}
		else {
			NhlFreeGenArray(ovfp->v_arr);
			if ((ga = 
			     _NhlCopyGenArray(vfp->v_arr,
					      vfp->copy_arrays)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			vfp->v_arr = ga;
			status = True;
		}
	}

	if (vfp->x_arr != ovfp->x_arr) {
		NhlFreeGenArray(ovfp->x_arr);
		if (vfp->x_arr != NULL && 
		    (ga = _NhlCopyGenArray(vfp->x_arr,
					   vfp->copy_arrays)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		vfp->x_arr = ga;
		status = True;
	}

	if (vfp->y_arr != ovfp->y_arr) {
		NhlFreeGenArray(ovfp->y_arr);
		if (vfp->y_arr != NULL && 
		    (ga = _NhlCopyGenArray(vfp->y_arr,
					   vfp->copy_arrays)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		vfp->y_arr = ga;
		status = True;
	}

	if (vfp->missing_u_value != ovfp->missing_u_value) {
		subret = CheckCopyVType(&ovfp->missing_u_value,
					vfp->missing_u_value,
					NhlNvfMissingUValueV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->missing_u_value = ovfp->missing_u_value;
		status = True;
	}
	if (vfp->missing_v_value != ovfp->missing_v_value) {
		subret = CheckCopyVType(&ovfp->missing_v_value,
					vfp->missing_v_value,
					NhlNvfMissingVValueV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->missing_v_value = ovfp->missing_v_value;
		status = True;
	}

	if (vfp->mag_min != ovfp->mag_min) {
		subret = CheckCopyVType(&ovfp->mag_min,vfp->mag_min,
					NhlNvfMagMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->mag_min = ovfp->mag_min;
		status = True;
	}
	if (vfp->mag_max != ovfp->mag_max) {
		subret = CheckCopyVType(&ovfp->mag_max,vfp->mag_max,
					NhlNvfMagMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->mag_max = ovfp->mag_max;
		status = True;
	}

	if (vfp->u_min != ovfp->u_min) {
		subret = CheckCopyVType(&ovfp->u_min,vfp->u_min,
					NhlNvfUMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->u_min = ovfp->u_min;
		status = True;
	}
	if (vfp->u_max != ovfp->u_max) {
		subret = CheckCopyVType(&ovfp->u_max,vfp->u_max,
					NhlNvfUMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->u_max = ovfp->u_max;
		status = True;
	}

	if (vfp->v_min != ovfp->v_min) {
		subret = CheckCopyVType(&ovfp->v_min,vfp->v_min,
					NhlNvfVMinV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->v_min = ovfp->v_min;
		status = True;
	}
	if (vfp->v_max != ovfp->v_max) {
		subret = CheckCopyVType(&ovfp->v_max,vfp->v_max,
					NhlNvfVMaxV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->v_max = ovfp->v_max;
		status = True;
	}

	if (vfp->x_start != ovfp->x_start) {
		subret = CheckCopyVType(&ovfp->x_start,vfp->x_start,
					NhlNvfXCStartV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->x_start = ovfp->x_start;
		status = True;
	}
	if (vfp->x_end != ovfp->x_end) {
		subret = CheckCopyVType(&ovfp->x_end,vfp->x_end,
					NhlNvfXCEndV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->x_end = ovfp->x_end;
		status = True;
	}

	if (vfp->y_start != ovfp->y_start) {
		subret = CheckCopyVType(&ovfp->y_start,vfp->y_start,
					NhlNvfYCStartV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->y_start = ovfp->y_start;
		status = True;
	}
	if (vfp->y_end != ovfp->y_end) {
		subret = CheckCopyVType(&ovfp->y_end,vfp->y_end,
					NhlNvfYCEndV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->y_end = ovfp->y_end;
		status = True;
	}


	if (vfp->x_subset_start != ovfp->x_subset_start) {
		subret = CheckCopyVType(&ovfp->x_subset_start,
					vfp->x_subset_start,
					NhlNvfXCStartSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->x_subset_start = ovfp->x_subset_start;
		status = True;
	}
	if (vfp->x_subset_end != ovfp->x_subset_end) {
		subret = CheckCopyVType(&ovfp->x_subset_end,vfp->x_subset_end,
					NhlNvfXCEndSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->x_subset_end = ovfp->x_subset_end;
		status = True;
	}


	if (vfp->y_subset_start != ovfp->y_subset_start) {
		subret = CheckCopyVType(&ovfp->y_subset_start,
					vfp->y_subset_start,
					NhlNvfYCStartSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->y_subset_start = ovfp->y_subset_start;
		status = True;
	}
	if (vfp->y_subset_end != ovfp->y_subset_end) {
		subret = CheckCopyVType(&ovfp->y_subset_end,vfp->y_subset_end,
					NhlNvfYCEndSubsetV,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		vfp->y_subset_end = ovfp->y_subset_end;
		status = True;
	}

	if (vfp->x_index_start != ovfp->x_index_start) 
		status = True;
	if (vfp->x_index_end != ovfp->x_index_end)
		status = True;
	if (vfp->y_index_start != ovfp->y_index_start)
		status = True;
	if (vfp->y_index_end != ovfp->y_index_end)
		status = True;
	if (vfp->x_stride != ovfp->x_stride)
		status = True;
	if (vfp->y_stride != ovfp->y_stride)
		status = True;
	if (vfp->subset_by_index != ovfp->subset_by_index)
		status = True;
	if (vfp->exchange_dimensions != ovfp->exchange_dimensions)
		status = True;
	if (vfp->exchange_uv_data != ovfp->exchange_uv_data)
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
	char *e_text, *entry_name = "VectorFieldGetValues";
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
	char *e_text, *entry_name = "VectorFieldGetValues";
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
 * Description: Explicitly forces the conversion to NhlVectorFieldFloat
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

static NhlVectorFieldFloatLayer ForceConvert
#if	NhlNeedProto
(
	NhlVectorFieldLayer vfl
)
#else
(vfl)
	NhlVectorFieldLayer vfl;
#endif
{
	char *e_text, *entry_name = "VectorFieldGetValues";
	int id;
	NhlVectorFieldFloatLayer vffl = NULL;
	NhlErrorTypes ret = NhlNOERROR;
	NrmValue from, to;

	from.size = sizeof(NhlVectorFieldLayerRec);
	from.data.intval = vfl->base.id;
	to.size = sizeof(NhlVectorFieldFloatLayerRec);
	to.data.ptrval = &id;
	ret = NhlConvertData(NULL,NhlvectorFieldClass->base_class.class_name,
			NhlvectorFieldFloatClass->base_class.class_name,
			     &from,&to);
	if (ret < NhlWARNING ||
	    (vffl = (NhlVectorFieldFloatLayer) _NhlGetLayer(id)) == NULL) {
		e_text = "%s: error converting NhlVectorFieldLayer";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
	}

	return vffl;
}

/*
 * Function:    VectorFieldGetValues
 *
 * Description: Retrieves the current setting of one or more VectorField 
 *      resources.This routine only retrieves resources that require 
 *	special methods that the generic GetValues method cannot handle. 
 *      This includes all resources implemented as GenArrays, including
 *	the variable type vector resources (VTypes). In general space is 
 *	allocated; the user is responsible for freeing this space. However,
 *	if the CopyArrays resource is False, Data array and the X/Y
 *	coordinate arrays (if they exist) are NOT copied, since the user
 *	is assumed to be keeping a valid copy of them around. 
 *	If the user does a GetValues on the data min or max before a
 *	the conversion to VectorFieldFloat object has taken place, then
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

static NhlErrorTypes    VectorFieldGetValues
#if	NhlNeedProto
(NhlLayer layer, _NhlArgList args, int num_args)
#else
(layer,args,num_args)
        NhlLayer        layer;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlVectorFieldLayer vfl = (NhlVectorFieldLayer)layer;
        NhlVectorFieldLayerPart *vfp = &(vfl->vfield);
	NhlVectorFieldFloatLayerPart *vffp = NULL;
	NhlErrorTypes ret = NhlNOERROR;
        NhlGenArray ga;
        char *e_text, *entry_name = "VectorFieldGetValues";
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

#if 0		
	if (vfp->d_arr == NULL) {
		e_text = "%s: internal inconsistency";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
#endif
	converted = vfp->vffloat != NULL;
	if (converted)
		vffp = &vfp->vffloat->vfieldfloat;

        for( i = 0; i< num_args; i++ ) {
		ga = NULL;
		resQ = args[i].quark;
		do_genarray = False;
                if (resQ == Qd_arr) {
			if (vfp->d_arr != NULL) {
				do_genarray = True;
				ndim = 2;
				dlen[0] = vfp->d_arr->len_dimensions[0];
				dlen[1] = vfp->d_arr->len_dimensions[1];
				if (vfp->copy_arrays) {
					if ((data = CopyData(vfp->d_arr,resQ))
					    == NULL)
						return NhlFATAL;
				}
				else {
					nocopy = True;
					data = vfp->d_arr->data;
				}
				typeQ = vfp->d_arr->typeQ;
				size = vfp->d_arr->size;
			}
                }
                if (resQ == Qu_arr) {
			if (vfp->u_arr != NULL) {
				do_genarray = True;
				ndim = 2;
				dlen[0] = vfp->u_arr->len_dimensions[0];
				dlen[1] = vfp->u_arr->len_dimensions[1];
				if (vfp->copy_arrays) {
					if ((data = CopyData(vfp->u_arr,resQ))
					    == NULL)
						return NhlFATAL;
				}
				else {
					nocopy = True;
					data = vfp->u_arr->data;
				}
				typeQ = vfp->u_arr->typeQ;
				size = vfp->u_arr->size;
			}
                }
                if (resQ == Qv_arr) {
			if (vfp->v_arr != NULL) {
				do_genarray = True;
				ndim = 2;
				dlen[0] = vfp->v_arr->len_dimensions[0];
				dlen[1] = vfp->v_arr->len_dimensions[1];
				if (vfp->copy_arrays) {
					if ((data = CopyData(vfp->v_arr,resQ))
					    == NULL)
						return NhlFATAL;
				}
				else {
					nocopy = True;
					data = vfp->v_arr->data;
				}
				typeQ = vfp->v_arr->typeQ;
				size = vfp->v_arr->size;
			}
                }
                else if (resQ == Qx_arr) {
			do_genarray = True;
			ndim = 1;
			if (vfp->x_arr == NULL) {
				dlen[0] = 0;
				dlen[1] = 0;
				data = NULL;
				typeQ = Qfloat;
				size = 0;
                        }
			else {
				dlen[0] = vfp->x_arr->len_dimensions[0];
				if (vfp->copy_arrays) {
					if ((data = CopyData(vfp->x_arr,resQ))
					    == NULL)
						return NhlFATAL;
				}
				else {
					nocopy = True;
					data = vfp->x_arr->data;
				}
				typeQ = vfp->x_arr->typeQ;
				size = vfp->x_arr->size;
			}
                }
                else if (resQ == Qy_arr) {
			do_genarray = True;
			ndim = 1;
			if (vfp->y_arr == NULL) {
				dlen[0] = 0;
				dlen[1] = 0;
				data = NULL;
				typeQ = Qfloat;
				size = 0;
                        }
			else {
				dlen[0] = vfp->y_arr->len_dimensions[0];
				if (vfp->copy_arrays) {
					if ((data = CopyData(vfp->y_arr,resQ))
					    == NULL)
						return NhlFATAL;
				}
				else {
					nocopy = True;
					data = vfp->y_arr->data;
				}
				typeQ = vfp->y_arr->typeQ;
				size = vfp->y_arr->size;
			}
                }
                else if (resQ == Qmissing_u_value) {
			do_genarray = True;
			ndim = 1;
			if (vfp->missing_u_value == NULL &&
			    vfp->single_missing) {
				dlen[0] = 
				       vfp->missing_v_value->len_dimensions[0];
				if ((data = 
				   CopyData(vfp->missing_v_value,resQ))== NULL)
					return NhlFATAL;
				typeQ = vfp->missing_v_value->typeQ;
				size = vfp->missing_v_value->size;
			}
			else if (vfp->missing_u_value == NULL) {
				dlen[0] = 0;
				data = NULL;
				typeQ = Qfloat;
				size = 0;
                        }
			else {
				dlen[0] = 
				       vfp->missing_u_value->len_dimensions[0];
				if ((data = 
				   CopyData(vfp->missing_u_value,resQ))== NULL)
					return NhlFATAL;
				typeQ = vfp->missing_u_value->typeQ;
				size = vfp->missing_u_value->size;
			}
                }
                else if (resQ == Qmissing_v_value) {
			do_genarray = True;
			ndim = 1;
			if (vfp->missing_v_value == NULL &&
			    vfp->single_missing) {
				dlen[0] = 
				       vfp->missing_u_value->len_dimensions[0];
				if ((data = 
				   CopyData(vfp->missing_u_value,resQ))== NULL)
					return NhlFATAL;
				typeQ = vfp->missing_u_value->typeQ;
				size = vfp->missing_u_value->size;
			}
			else if (vfp->missing_v_value == NULL) {
				dlen[0] = 0;
				data = NULL;
				typeQ = Qfloat;
				size = 0;
                        }
			else {
				dlen[0] = 
				       vfp->missing_v_value->len_dimensions[0];
				if ((data = 
				   CopyData(vfp->missing_v_value,resQ))== NULL)
					return NhlFATAL;
				typeQ = vfp->missing_v_value->typeQ;
				size = vfp->missing_v_value->size;
			}
                }
                else if (resQ == Qmag_min) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->mag_min != NULL) {
				if ((data = CopyData(vfp->mag_min,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->mag_min->typeQ;
				size = vfp->mag_min->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->mag_min,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qmag_max) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->mag_max != NULL) {
				if ((data = CopyData(vfp->mag_max,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->mag_min->typeQ;
				size = vfp->mag_min->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->mag_max,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qu_min) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->u_min != NULL) {
				if ((data = CopyData(vfp->u_min,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->u_min->typeQ;
				size = vfp->u_min->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->u_min,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qu_max) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->u_max != NULL) {
				if ((data = CopyData(vfp->u_max,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->u_min->typeQ;
				size = vfp->u_min->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->u_max,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qv_min) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->v_min != NULL) {
				if ((data = CopyData(vfp->v_min,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->v_min->typeQ;
				size = vfp->v_min->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->v_min,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qv_max) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->v_max != NULL) {
				if ((data = CopyData(vfp->v_max,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->v_min->typeQ;
				size = vfp->v_min->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->v_max,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->x_start != NULL) {
				if ((data = CopyData(vfp->x_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->x_start->typeQ;
				size = vfp->x_start->size;
			}
			else if (vfp->x_subset_start != NULL) {
				if ((data = CopyData(vfp->x_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->x_subset_start->typeQ;
				size = vfp->x_subset_start->size;
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
			if (vfp->x_end != NULL) {
				if ((data = CopyData(vfp->x_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->x_end->typeQ;
				size = vfp->x_end->size;
			}
			else if (vfp->x_subset_start != NULL) {
				if ((data = CopyData(vfp->x_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->x_subset_start->typeQ;
				size = vfp->x_subset_start->size;
			}
			else {
				tmp = vfp->v_arr->len_dimensions[1] - 1;
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
			if (vfp->y_start != NULL) {
				if ((data = CopyData(vfp->y_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->y_start->typeQ;
				size = vfp->y_start->size;
			}
			else if (vfp->y_subset_start != NULL) {
				if ((data = CopyData(vfp->y_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->y_subset_start->typeQ;
				size = vfp->y_subset_start->size;
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
			if (vfp->y_end != NULL) {
				if ((data = CopyData(vfp->y_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->y_end->typeQ;
				size = vfp->y_end->size;
			}
			else if (vfp->y_subset_start != NULL) {
				if ((data = CopyData(vfp->y_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->y_subset_start->typeQ;
				size = vfp->y_subset_start->size;
			}
			else {
				tmp = vfp->v_arr->len_dimensions[0] - 1;
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
			if (vfp->x_subset_start != NULL) {
				if ((data = CopyData(vfp->x_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->x_subset_start->typeQ;
				size = vfp->x_subset_start->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->x_start,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_subset_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->x_subset_end != NULL) {
				if ((data = CopyData(vfp->x_subset_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->x_subset_end->typeQ;
				size = vfp->x_subset_end->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->x_end,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_subset_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->y_subset_start != NULL) {
				if ((data = CopyData(vfp->y_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->y_subset_start->typeQ;
				size = vfp->y_subset_start->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->y_start,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_subset_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (vfp->y_subset_end != NULL) {
				if ((data = CopyData(vfp->y_subset_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = vfp->y_subset_end->typeQ;
				size = vfp->y_subset_end->size;
			}
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				if ((data = 
				     CreateData(vffp->y_end,resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_index_start) {
			if (vfp->x_index_start > -1)
				ival = vfp->x_index_start;
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				ival = vfp->ix_start;
			}
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qx_index_end) {
			if (vfp->x_index_end > -1)
				ival = vfp->x_index_end;
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				ival = vfp->ix_end;
			}
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_index_start) {
			if (vfp->y_index_start > -1)
				ival = vfp->y_index_start;
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				ival = vfp->iy_start;
			}
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_index_end) {
			if (vfp->y_index_end > -1)
				ival = vfp->y_index_end;
			else {
				if (! converted) {
					vfp->vffloat = ForceConvert(vfl);
					if (vfp->vffloat == NULL)
						return NhlFATAL;
					vffp = &vfp->vffloat->vfieldfloat;
					converted = True;
				}
				ival = vfp->iy_end;
			}
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qx_actual_start) {
			if (! converted) {
				vfp->vffloat = ForceConvert(vfl);
				if (vfp->vffloat == NULL)
					return NhlFATAL;
				vffp = &vfp->vffloat->vfieldfloat;
				converted = True;
			}
			fval = vfp->x_actual_start;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qx_actual_end) {
			if (! converted) {
				vfp->vffloat = ForceConvert(vfl);
				if (vfp->vffloat == NULL)
					return NhlFATAL;
				vffp = &vfp->vffloat->vfieldfloat;
				converted = True;
			}
			fval = vfp->x_actual_end;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_actual_start) {
			if (! converted) {
				vfp->vffloat = ForceConvert(vfl);
				if (vfp->vffloat == NULL)
					return NhlFATAL;
				vffp = &vfp->vffloat->vfieldfloat;
				converted = True;
			}
			fval = vfp->y_actual_start;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_actual_end) {
			if (! converted) {
				vfp->vffloat = ForceConvert(vfl);
				if (vfp->vffloat == NULL)
					return NhlFATAL;
				vffp = &vfp->vffloat->vfieldfloat;
				converted = True;
			}
			fval = vfp->y_actual_end;
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
 * Function:	VectorFieldDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlVectorFieldClass.
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
VectorFieldDestroy
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
 *	char			*entry_name = "VectorFieldDestroy";
 */
	NhlVectorFieldLayer	vfl = (NhlVectorFieldLayer)l;
	NhlVectorFieldLayerPart	*vfp = &(vfl->vfield);

	NhlFreeGenArray(vfp->d_arr);
	NhlFreeGenArray(vfp->u_arr);
	NhlFreeGenArray(vfp->v_arr);
	NhlFreeGenArray(vfp->x_arr);
	NhlFreeGenArray(vfp->y_arr);
	NhlFreeGenArray(vfp->missing_u_value);
	NhlFreeGenArray(vfp->missing_v_value);
	NhlFreeGenArray(vfp->mag_min);
	NhlFreeGenArray(vfp->mag_max);
	NhlFreeGenArray(vfp->u_min);
	NhlFreeGenArray(vfp->u_max);
	NhlFreeGenArray(vfp->v_min);
	NhlFreeGenArray(vfp->v_max);
	NhlFreeGenArray(vfp->x_start);
	NhlFreeGenArray(vfp->x_end);
	NhlFreeGenArray(vfp->y_start);
	NhlFreeGenArray(vfp->y_end);
	NhlFreeGenArray(vfp->x_subset_start);
	NhlFreeGenArray(vfp->x_subset_end);
	NhlFreeGenArray(vfp->y_subset_start);
	NhlFreeGenArray(vfp->y_subset_end);

	return NhlNOERROR;
}


/*
 * Function:    CheckCopyVType
 *
 * Description:	Copies a variable type vector stored in a GenArray
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
