/*
 *      $Id: MeshScalarField.c,v 1.8.4.1 2008-03-28 20:37:36 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MeshScalarField.c
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
#include <float.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/MeshScalarFieldP.h>

/************************************************************************
*									*
*	MeshScalarField Class declarations					*
*									*
************************************************************************/

/*
 * Resource Declarations
 */

#define	Oset(field)	NhlOffset(NhlMeshScalarFieldLayerRec,msfield.field)
static NhlResource resources[] = {

/* Begin-documented-resources */
	{NhlNsfDataArray,NhlCsfDataArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(d_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNsfXArray,NhlCsfXArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNsfYArray,NhlCsfYArray,NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_arr),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNsfElementNodes,NhlCsfElementNodes,
	 NhlTIntegerGenArray,sizeof(NhlGenArray),
	 Oset(element_nodes),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNsfFirstNodeIndex,NhlCsfFirstNodeIndex,NhlTInteger,sizeof(int),
	 Oset(first_node_index),NhlTImmediate,_NhlUSET(0),0,NULL},
	{NhlNsfXCellBounds,NhlCsfXCellBounds,NhlTGenArray,sizeof(NhlGenArray),
	 Oset(x_cell_bounds),NhlTImmediate,
	 _NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNsfYCellBounds,NhlCsfYCellBounds,NhlTGenArray,sizeof(NhlGenArray),
	 Oset(y_cell_bounds),NhlTImmediate,
	 _NhlUSET((NhlPointer)NULL),0,NULL},

	{NhlNsfMissingValueV,NhlCsfMissingValueV,NhlTVariable,
		 sizeof(NhlGenArray),Oset(missing_value),NhlTImmediate,
		 _NhlUSET(NULL),0,NULL},

	{NhlNsfGridType,NhlCsfGridType,NhlTdiGridType,sizeof(NhldiGridType),
	 	Oset(grid_type),NhlTImmediate,
	 	_NhlUSET((NhlPointer)NhlSPHERICALGRID),0,NULL},
	{NhlNsfCopyData,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(copy_arrays),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),0,NULL},
	{NhlNsfDataMinV,NhlCsfDataMinV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(data_min),NhlTImmediate,_NhlUSET(NULL),0,NULL},
	{NhlNsfDataMaxV,NhlCsfDataMaxV,NhlTVariable,sizeof(NhlGenArray),
		 Oset(data_max),NhlTImmediate,_NhlUSET(NULL),0,NULL},

   	{NhlNsfXCActualStartF,NhlCsfXCActualStartF,NhlTFloat,sizeof(float),
		 Oset(x_actual_start),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_GONLY,NULL},
	{NhlNsfXCActualEndF,NhlCsfXCActualEndF,NhlTFloat,sizeof(float),
		 Oset(x_actual_end),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_GONLY,NULL},
	{NhlNsfXCElementCount,NhlCsfXCElementCount,NhlTInteger,sizeof(int),
		 Oset(xc_el_count),NhlTImmediate,
         	 _NhlUSET(0),_NhlRES_GONLY,NULL},
	{NhlNsfYCActualStartF,NhlCsfYCActualStartF,NhlTFloat,sizeof(float),
		 Oset(y_actual_start),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_GONLY,NULL},
	{NhlNsfYCActualEndF,NhlCsfYCActualEndF,NhlTFloat,sizeof(float),
		 Oset(y_actual_end),NhlTString,
         	_NhlUSET("1.0"),_NhlRES_GONLY,NULL},
	{NhlNsfYCElementCount,NhlCsfYCElementCount,NhlTInteger,sizeof(int),
		 Oset(yc_el_count),NhlTImmediate,
	 _NhlUSET(0),_NhlRES_GONLY,NULL},
#if 0
	{NhlNsfNodeIndexes,NhlCsfNodeIndexes,
	 NhlTIntegerGenArray,sizeof(NhlGenArray),
	 Oset(node_indexes),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,NULL},
	{NhlNsfExchangeDimensions,NhlCsfExchangeDimensions,NhlTBoolean,
		 sizeof(NhlBoolean),Oset(exchange_dimensions),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),0,NULL},
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

#endif
#undef Oset

/*
 * disable all resources at the superclass level
 */
#define	Oset(field)	NhlOffset(NhlMeshScalarFieldLayerRec,sfield.field)
	{"no.res","No.res",NhlTGenArray,sizeof(NhlGenArray),
		 Oset(d_arr),NhlTImmediate,
	 _NhlUSET((NhlPointer)NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTGenArray,sizeof(NhlGenArray),
		 Oset(x_arr),NhlTImmediate,
	 _NhlUSET((NhlPointer)NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTGenArray,sizeof(NhlGenArray),
		 Oset(y_arr),NhlTImmediate,
	 _NhlUSET((NhlPointer)NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTdiGridType,sizeof(NhldiGridType),
	 	Oset(grid_type),NhlTImmediate,
	 	_NhlUSET((NhlPointer)NhlSPHERICALGRID),_NhlRES_PRIVATE,NULL},
	{"no.res",NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(copy_arrays),NhlTImmediate,
		 _NhlUSET((NhlPointer)True),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTBoolean,
		 sizeof(NhlBoolean),Oset(exchange_dimensions),NhlTImmediate,
		 _NhlUSET((NhlPointer)False),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTVariable,
		 sizeof(NhlGenArray),Oset(missing_value),NhlTImmediate,
		 _NhlUSET(NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTVariable,sizeof(NhlGenArray),
	 Oset(data_min),NhlTImmediate,_NhlUSET(NULL), _NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTVariable,sizeof(NhlGenArray),
	 Oset(data_max),NhlTImmediate,_NhlUSET(NULL),_NhlRES_PRIVATE,NULL},

	{"no.res","No.res",NhlTVariable,sizeof(NhlGenArray),
	 Oset(x_start),NhlTImmediate,_NhlUSET(NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTVariable,sizeof(NhlGenArray),
	 Oset(x_end),NhlTImmediate,_NhlUSET(NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTVariable,sizeof(NhlGenArray),
	 Oset(y_start),NhlTImmediate,_NhlUSET(NULL),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTVariable,sizeof(NhlGenArray),
	 Oset(y_end),NhlTImmediate,_NhlUSET(NULL),_NhlRES_PRIVATE,NULL},

	{"no.res","No.res",
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_subset_start),NhlTImmediate,_NhlUSET(NULL),
	 	_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(x_subset_end),NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_subset_start),NhlTImmediate,_NhlUSET(NULL),
	 	_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",
		 NhlTVariable,sizeof(NhlGenArray),
		 Oset(y_subset_end),NhlTImmediate,_NhlUSET(NULL),
		_NhlRES_PRIVATE,NULL},

	{"no.res","No.res",NhlTInteger,sizeof(int),
		 Oset(x_index_start),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTInteger,sizeof(int),
		 Oset(x_index_end),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTInteger,sizeof(int),
		 Oset(y_index_start),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTInteger,sizeof(int),
		 Oset(y_index_end),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),_NhlRES_PRIVATE,NULL},

	{"no.res","No.res",NhlTInteger,sizeof(int),
		Oset(x_stride),NhlTImmediate,_NhlUSET((NhlPointer)1),
	 	_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTInteger,sizeof(int),
		Oset(y_stride),NhlTImmediate,_NhlUSET((NhlPointer)1),
	 	_NhlRES_PRIVATE,NULL},

   	{"no.res","No.res",NhlTFloat,sizeof(float),
		 Oset(x_actual_start),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTFloat,sizeof(float),
		 Oset(x_actual_end),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTInteger,sizeof(int),
		 Oset(xc_el_count),NhlTImmediate,
         	 _NhlUSET(0),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTFloat,sizeof(float),
		 Oset(y_actual_start),NhlTString,
		 _NhlUSET("0.0"),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTFloat,sizeof(float),
		 Oset(y_actual_end),NhlTString,
         	_NhlUSET("1.0"),_NhlRES_PRIVATE,NULL},
	{"no.res","No.res",NhlTInteger,sizeof(int),
		 Oset(yc_el_count),NhlTImmediate,
         	_NhlUSET(0),_NhlRES_PRIVATE,NULL},

};
#undef Oset

/* base methods */

static NhlErrorTypes MeshScalarFieldClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc	/* lc to init	*/
#endif
);

static NhlErrorTypes MeshScalarFieldClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes MeshScalarFieldInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes MeshScalarFieldSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes    MeshScalarFieldGetValues(
#if	NhlNeedProto
        NhlLayer	layer,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes MeshScalarFieldDestroy(
#if	NhlNeedProto
	NhlLayer	layer
#endif
);

static NhlErrorTypes    CheckCopyVType(
#if	NhlNeedProto
	NhlGenArray	*ga,
	NhlGenArray	copy_ga,
	NhlString	resource_name,
        NhlBoolean	null_ok,
	NhlString	entry_name
#endif
);


NhlMeshScalarFieldClassRec NhlmeshScalarFieldClassRec = {
	/* NhlBaseClassPart */
	{
/* class_name			*/	"meshScalarFieldClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlMeshScalarFieldLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)
						&NhlscalarFieldClassRec,
/* cvt_table			*/	NULL,
/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	MeshScalarFieldClassPartInitialize,
/* class_initialize		*/	MeshScalarFieldClassInitialize,
/* layer_initialize		*/	MeshScalarFieldInitialize,
/* layer_set_values		*/	MeshScalarFieldSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	MeshScalarFieldGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	MeshScalarFieldDestroy,

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
	},
	/* NhlMeshScalarFieldClassPart */
	{
/* foo				*/	0
	}
};
	
NhlClass NhlmeshScalarFieldClass = (NhlClass)
					&NhlmeshScalarFieldClassRec;

static	NrmQuark	Qfloat  = NrmNULLQUARK;
static	NrmQuark	Qint  = NrmNULLQUARK;
static	NrmQuark	Qgen_array  = NrmNULLQUARK;
static	NrmQuark	Qfloat_gen_array  = NrmNULLQUARK;
static	NrmQuark	Qd_arr  = NrmNULLQUARK;
static	NrmQuark	Qx_arr  = NrmNULLQUARK;
static	NrmQuark	Qy_arr  = NrmNULLQUARK;
static	NrmQuark	Qx_cell_bounds  = NrmNULLQUARK;
static	NrmQuark	Qy_cell_bounds  = NrmNULLQUARK;
static  NrmQuark        Qelement_nodes = NrmNULLQUARK;
static  NrmQuark        Qnode_indexes = NrmNULLQUARK;
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
static	NrmQuark	Qxc_el_count  = NrmNULLQUARK;
static	NrmQuark	Qy_actual_start  = NrmNULLQUARK;
static	NrmQuark	Qy_actual_end  = NrmNULLQUARK;
static	NrmQuark	Qyc_el_count  = NrmNULLQUARK;

typedef enum _sfCoord { sfXCOORD, sfYCOORD} sfCoord;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

/*
 * Function:	nhlfmeshScalarfieldclass
 *
 * Description:	fortran ref to meshScalarfield class
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
_NHLCALLF(nhlfmeshScalarfieldclass,NHLFMESHSCALARFIELDCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlmeshScalarFieldClass;
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
 	NhlMeshScalarFieldLayerPart *sfp,
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
 	NhlMeshScalarFieldLayerPart *sfp;
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
	int		out_len;
	int		i;
	float		*ifp,*fp;
	float		tmp;
	int		istart = sfp->istart - sfp->first_node_index;
	int		iend = sfp->iend - sfp->first_node_index;
	int		valid_data_count = 0;

/*
 * Convert the data array
 */
	if ((ga = GenToFloatGenArray(sfp->d_arr)) == NULL) {
		e_text = "%s: error converting data to float data";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if (ga->num_dimensions != 1 || ga->typeQ != Qfloat) {
		e_text = "%s: internal inconsistency in float data array";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if (ga != sfp->d_arr) overwrite_ok = True;

	*new_data = False;
	*dmin = FLT_MAX;
	*dmax = -FLT_MAX;

	out_ga = ga;
	out_len = (iend - istart + 1);
	
	if (*new_data) {

		ifp = ((float *) ga->data);

		if (overwrite_ok) 
			fp = ifp;
		else {
			if ((fp = (float *) 
			     NhlConvertMalloc(out_len * sizeof(float))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NULL;
			}
		}
		if (do_minmax && do_missing) {
			for (i = 0; i < out_len; i++) {
				tmp = *(ifp + istart+i*sfp->istride);
				if (tmp != missing_value) {
					valid_data_count++;
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
				}
				*(fp+i) = tmp;
			}
		}
		else if (do_minmax) {
			valid_data_count = out_len;
			for (i = 0; i < out_len; i++) {
				tmp = *(ifp + istart+i*sfp->istride);
				if (tmp < *dmin) *dmin = tmp;
				if (tmp > *dmax) *dmax = tmp;
				*(fp+i) = tmp;
			}
		}
		else {
			valid_data_count = out_len;
			for (i = 0; i < out_len; i++) {
				tmp = *(ifp + istart+i*sfp->istride);
				*(fp+i) = tmp;
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
			out_ga->num_dimensions = 1;
			out_ga->num_elements = out_len;
			out_ga->len_dimensions = &out_ga->num_elements;
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
			for (i = 0; i < out_len; i++) {
				tmp = *(ifp + istart+i*sfp->istride);
				if (tmp != missing_value) {
					valid_data_count++;
					if (tmp < *dmin) *dmin = tmp;
					if (tmp > *dmax) *dmax = tmp;
				}
			}
		}
		else if (do_minmax) {
			valid_data_count = out_len;
			for (i = 0; i < out_len; i++) {
				tmp = *(ifp + istart+i*sfp->istride);
				if (tmp < *dmin) *dmin = tmp;
				if (tmp > *dmax) *dmax = tmp;
			}
		}
		else {
			*dmin = 0.0;
			*dmax = 1.0;
		}
		
	}
	if (valid_data_count == 0) {
		*dmin = missing_value;
		*dmax = missing_value;
	}
	return out_ga;
}


/*
 * Function:	ValidCoordArray
 *
 * Description:	This function checks the coordinate arrays used to
 *		specify meshScalar field grids. 
 *              First it checks to ensure dimensionality is correct.
 *              All arrays are checked to ensure that corresponding
 *		dimensions have equal numbers of elements.
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
 	NhlMeshScalarFieldLayerPart *sfp,
	NhlGenArray		ga,
	NrmQuark                qname,
	NhlString		entry_name
)
#else
(sfp,ga,name,entry_name)
 	NhlMeshScalarFieldLayerPart *sfp;
	NhlGenArray		ga;
	NrmQuark                qname;
	NhlString		entry_name;
#endif
{
	char *e_text;
	NhlBoolean error = False;


	if (qname == Qx_cell_bounds || qname == Qy_cell_bounds) {
		if (ga->num_dimensions != 2){
			e_text = 
			 "%s: coordinate array %s has invalid dimensionality";
			error = True;
		}
		else if (ga->len_dimensions[0] != sfp->d_el_count) {
			e_text = 
    "%s: first dimension of coordinate array %s requires the same number of elements as the data array";
			error = True;
		}
	}
	else {
		if (ga->num_dimensions != 1) {
			e_text = 
			 "%s: coordinate array %s has invalid dimensionality";
			error = True;
		}
		else if (ga->len_dimensions[0] != sfp->d_el_count) {
			e_text = 
    "%s: coordinate array %s requires the same number of elements as the data array";
			error = True;
		}
	}
	if (error) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NrmQuarkToString(qname));
		return False;
	}

	return True;
	
}


/*
 * Function:	GetDataBounds
 *
 * Description:	determines the max and min coordinate values
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
 	NhlMeshScalarFieldLayerPart *sfp,
	NhlGenArray             xc,
	NhlGenArray             yc,
	float			*xmin,
	float                   *ymin,
	float                   *xmax,
	float                   *ymax,
	NhlString		entry_name
)
#else
(sfp,xc,yc,xmin, ymin, xmax, ymax,entry_name)
 	NhlMeshScalarFieldLayerPart *sfp;
	NhlGenArray             xc;
	NhlGenArray             yc;
	float			*xmin;
	float                   *ymin;
	float                   *xmax;
	float                   *ymax;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	int             i;
	float           *cdata;

	cdata = (float*)xc->data;

	*xmin = *xmax = cdata[0];
	for (i = 0; i < xc->num_elements; i++) {
		if (cdata[i] > *xmax)
			*xmax = cdata[i];
		else if (cdata[i] < *xmin) 
			*xmin = cdata[i];
	}
	cdata = (float*)yc->data;
	*ymin = *ymax = cdata[0];
	for (i = 0; i < yc->num_elements; i++) {
		if (cdata[i] > *ymax)
			*ymax = cdata[i];
		else if (cdata[i] < *ymin) 
			*ymin = cdata[i];
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
 	NhlMeshScalarFieldLayerPart *sfp,
	int			*icstart,
	int			*icend,
	NhlString		entry_name
)
#else
(sfp,ctype,icstart,icend,entry_name)
 	NhlMeshScalarFieldLayerPart *sfp;
	int			*icstart;
	int			*icend;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	int                     i;

	if (sfp->node_indexes) {
		int *indexes = (int *)sfp->node_indexes->data;
		*icstart = *icend = indexes[0];
		for (i = 0; i < sfp->d_el_count; i++) {
			if (indexes[i] > *icend)
				*icend = indexes[i];
			else if (indexes[i] < *icstart)
				*icstart = indexes[i];
		}
	}
	else {
		*icstart = sfp->first_node_index;
		*icend = sfp->first_node_index + sfp->d_el_count - 1;
	}
	return ret;
}
#if 0
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
 	NhlMeshScalarFieldLayerPart *sfp,
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
 	NhlMeshScalarFieldLayerPart *sfp;
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
	NhlGenArray	*subset_start, *subset_end;
	NhlBoolean	nullstart = False, nullend = False;
        NhlBoolean	start_byindex,end_byindex;
	char		*c_name;
	int		stride,rem;

	if (ctype == sfXCOORD) {
		range =  sfp->d_el_count - 1;
		subset_start = &sfp->x_subset_start;
		subset_end = &sfp->x_subset_end;
		stride = sfp->x_stride;
                start_byindex = sfp->xstart_byindex;
                end_byindex = sfp->xend_byindex;
		c_name = "X coordinate";
	}
	else {
		range = sfp->d_el_count - 1;
		subset_start = &sfp->y_subset_start;
		subset_end = &sfp->y_subset_end;
		stride = sfp->y_stride;
                start_byindex = sfp->ystart_byindex;
                end_byindex = sfp->yend_byindex;
		c_name = "Y coordinate";
	}

	if (! sfp->subset_by_index) {
		float fval;

		rev = cstart > cend;
		if (*subset_start != NULL && ! start_byindex) {
			subret = GetVTypeValue(*subset_start,&fval);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				      "%s: error getting variable type value";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return ret;
			}
                        if ((! rev && fval < cstart) ||
                            (rev && fval > cstart)) {
				e_text = 
			      "%s: %s subset start out of range: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
                                          e_text,entry_name,c_name);
                                ret = MIN(NhlWARNING,ret);
                                *scstart = cstart;
                                NhlFreeGenArray(*subset_start);
                                *subset_start = NULL;
                        }
                        else {
                                *scstart = fval;
                        }
		}
		else {
			*scstart = cstart;
			nullstart = True;
		}
		if (*subset_end != NULL && ! end_byindex) {
			subret = GetVTypeValue(*subset_end,&fval);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				e_text = 
				     "%s: error getting variable type value";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return ret;
			}
                        if ((! rev && fval > cend) ||
                            (rev && fval < cend)) {
				e_text = 
			      "%s: %s subset end out of range: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
                                          e_text,entry_name,c_name);
                                ret = MIN(NhlWARNING,ret);
                                *scend = cend;
                                NhlFreeGenArray(*subset_end);
                                *subset_end = NULL;
                        }
                        else {
                                *scend = fval;
                        }
		}
		else {
			*scend = cend;
			nullend = True;
		}

		if (rev != (*scstart > *scend)) {
                        NhlGenArray tmp_ga;
                        float fval;
			e_text = 
"%s: %s start/end subset order opposed to start/end order: reversing subset order";
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  e_text,entry_name,c_name,c_name);
			ret = MIN(NhlWARNING,ret);
                        tmp_ga = *subset_start;
                        *subset_start = *subset_end;
                        *subset_end = tmp_ga;
                        fval = *scstart;
                        *scstart = *scend;
                        *scend = fval;
		}

/*
 * The index endpoints are chosen to include the subset data endpoints.
 */
		drange = cend - cstart;
                if (drange == 0.0) {
			e_text = 
		         "%s: internal error; %s data range is 0.0.";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  c_name,c_name);
                        return NhlFATAL;
                }
                if (! start_byindex)
                        *icstart = MAX(0,floor(((*scstart - cstart) /
                                                drange) * range));
                if (! end_byindex)
                        *icend = MIN(range,ceil(((*scend - cstart) /
                                                 drange) * range));

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
	rem = (*icend - *icstart) % stride;
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
#endif

/*
 * Function:	GetCoordBounds
 *
 * Description:	For irregular coordinates, the first and last array elements
 *		define the start and end of the data space; the x/y start/end 
 *		resources are ignored. However, it is still possible to create
 *		subsets of the 	data using either the index or the subset 
 *		start/end resources.
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
 	NhlMeshScalarFieldLayerPart *sfp,
	NhlGenArray             xc,
	NhlGenArray             yc,
	int			*icstart,
	int			*icend,
	float			*xmin,
	float                   *ymin,
	float                   *xmax,
	float                   *ymax,
	NhlString		entry_name
)
#else
( sfp, xc, yc, icstart, icend, xmin, ymin, xmax, ymax, entry_name)
 	NhlMeshScalarFieldLayerPart *sfp;
	NhlGenArray             xc;
	NhlGenArray             yc;
	int			*icstart;
	int			*icend;
	float			*xmin;
	float                   *ymin;
	float                   *xmax;
	float                   *ymax;
	NhlString		entry_name;
#endif
{
	NhlErrorTypes   ret = NhlNOERROR, subret = NhlNOERROR;

	subret = GetIndexBounds(sfp,icstart,icend,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;

	subret = GetDataBounds(sfp,xc,yc,xmin,ymin,xmax,ymax,entry_name);
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;
	
	return ret;
}

/*
 * Function:	CvtGenSFObjToFloatSFObj
 *
 * Description:	This function is used to convert a Generic MeshScalarField
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
	NhlMeshScalarFieldLayer	sfl;
	NhlMeshScalarFieldLayerPart *sfp;
	NhlSArg			sargs[30];
	int			nargs=0;
	NhlGenArray		d_arr = NULL, x_arr = NULL, y_arr = NULL;
	NhlGenArray             x_cell_bounds = NULL, y_cell_bounds = NULL;
	int                     istart,iend;
	float                   xmin,xmax,ymin,ymax;
	float			missing_value;
	NhlBoolean		do_minmax,do_missing,new_data;
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

	sfl = (NhlMeshScalarFieldLayer)_NhlGetLayer(from->data.intval);
	if ((sfl == NULL) ||
	    (sfl->base.layer_class != NhlmeshScalarFieldClass)){
		e_text = "%s:Called w/ improper \"from\" object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	sfp = (NhlMeshScalarFieldLayerPart *) &sfl->msfield;

	if (sfp->d_arr == NULL || sfp->d_arr->num_dimensions != 1) {
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
 * Convert and validate the X and Y coordinate arrays,
 * if defined.
 */
	sffp->x_arr = NULL;
	if (sfp->x_arr && ((x_arr = GenToFloatGenArray(sfp->x_arr)) == NULL)) {
		e_text = "%s: error converting %s to float";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,NhlNsfXArray);
		return(NhlFATAL);
	}

	sffp->y_arr = NULL;
	if (sfp->y_arr && ((y_arr = GenToFloatGenArray(sfp->y_arr)) == NULL)) {
		e_text = "%s: error converting %s to float; defaulting";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,NhlNsfYArray);
		return(NhlFATAL);
	}

	sffp->x_cell_bounds = NULL;
	if (sfp->x_cell_bounds) {
		if ((x_cell_bounds = 
		     GenToFloatGenArray(sfp->x_cell_bounds)) == NULL) {
			e_text = "%s: error converting %s to float";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,NhlNsfXCellBounds);
			return(NhlFATAL);
		}
	}
	sffp->y_cell_bounds = NULL;
	if (sfp->y_cell_bounds) {
		if ((y_cell_bounds =
		     GenToFloatGenArray(sfp->y_cell_bounds)) == NULL) {
			e_text = "%s: error converting %s to float";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,NhlNsfYCellBounds);
			return(NhlFATAL);
		}
	}
	if (! ((x_arr && y_arr) || (x_cell_bounds && y_cell_bounds))) {
		e_text = "%s: invalid coordinate data";
		NhlPError(NhlFATAL,NhlEUNKNOWN, e_text,entry_name);
		return(NhlFATAL);
	}
	if (x_arr) {
		subret = GetCoordBounds(sfp,x_arr,y_arr,&istart,&iend,
					&xmin,&ymin,&xmax,&ymax,entry_name);
	}
	else {
		subret = GetCoordBounds(sfp,x_cell_bounds,y_cell_bounds,&istart,&iend,
					&xmin,&ymin,&xmax,&ymax,entry_name);
	}
	if ((ret = MIN(ret,subret))  < NhlWARNING) 
		return ret;

	sfp->istart = istart;
	sfp->iend = iend;
	sfp->x_actual_start = xmin;
	sfp->x_actual_end = xmax;
	sfp->y_actual_start = ymin;
	sfp->y_actual_end = ymax;

	sffp->ix_start = sfp->istart;
	sffp->ix_end = sfp->istart;
	sffp->xc_is_bounds = sfp->xc_is_bounds;
	sffp->iy_start = sfp->istart;
	sffp->iy_end = sfp->istart;
	sffp->yc_is_bounds = sfp->yc_is_bounds;

        if (! sfp->subset_by_index) {
                sfp->y_index_start = sfp->istart;
                sfp->y_index_end = sfp->iend;
        }
        if (! sfp->subset_by_index) {
                sfp->x_index_start = sfp->istart;
                sfp->x_index_end = sfp->iend;
        }
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

	if ((d_arr = DataToFloatArray(sfp,do_minmax,do_missing,
				      missing_value,&dmin,&dmax,
				      &new_data,entry_name)) == NULL) {
			return NhlFATAL;
	}
	sffp->x_start = sfp->x_actual_start;
	sffp->x_end = sfp->x_actual_end;
	sffp->y_start = sfp->y_actual_start;
	sffp->y_end = sfp->y_actual_end;
	sffp->ix_start = sfp->istart;
	sffp->ix_end = sfp->iend;
	sffp->xc_is_bounds = sfp->xc_is_bounds;
	sffp->iy_start = sfp->istart;
	sffp->iy_end = sfp->iend;
	sffp->yc_is_bounds = sfp->yc_is_bounds;
	sffp->d_arr = d_arr;
	sffp->x_arr = x_arr;
	sffp->y_arr = y_arr;
	sffp->x_cell_bounds = x_cell_bounds;
	sffp->y_cell_bounds = y_cell_bounds;
	sffp->element_nodes = sfp->element_nodes;


/*
 * If the user passed in a float array and stride values are all unity,
 * then the array is not copied. Set values that will be used to 
 * indicate to the low level routines what portion of the array to use.
 * If a copy was made, the entire array will be utilitized.
 */
	if (! new_data) {
		sffp->begin = istart;
		sffp->fast_dim = d_arr->len_dimensions[0];
		sffp->fast_len = iend - istart + 1;
		sffp->slow_len = 0;
	}
	else {
		sffp->begin = 0;
		sffp->fast_dim = d_arr->len_dimensions[0];
		sffp->fast_len = d_arr->len_dimensions[0];
		sffp->slow_len = 0;
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
	if (tmax < tmin) {
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
	sffp->grid_type = NhlMESHGRID;
	sffp->changed = sfp->changed;
        sfp->up_to_date = True;
        
	return ret;
}

/************************************************************************
*									*
*	Method definitions						*
*									*
************************************************************************/


/*
 * Function:	MeshScalarFieldClassInitialize
 *
 * Description:	This function does one time initialization needed by the
 *		MeshScalarFieldClass.
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
MeshScalarFieldClassInitialize
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
	Qfloat_gen_array = NrmStringToQuark(NhlTFloatGenArray);
	Qd_arr  = NrmStringToQuark(NhlNsfDataArray);
	Qx_arr  = NrmStringToQuark(NhlNsfXArray);
	Qy_arr  = NrmStringToQuark(NhlNsfYArray);
	Qx_cell_bounds  = NrmStringToQuark(NhlNsfXCellBounds);
	Qy_cell_bounds  = NrmStringToQuark(NhlNsfYCellBounds);
	Qelement_nodes = NrmStringToQuark(NhlNsfElementNodes);
	Qnode_indexes = NrmStringToQuark(NhlNsfNodeIndexes);
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
	Qxc_el_count  = NrmStringToQuark(NhlNsfXCElementCount);
	Qy_actual_start  = NrmStringToQuark(NhlNsfYCActualStartF);
	Qy_actual_end  = NrmStringToQuark(NhlNsfYCActualEndF);
	Qyc_el_count  = NrmStringToQuark(NhlNsfYCElementCount);

	ret = NhlRegisterConverter(NhlbaseClass,
			NhlmeshScalarFieldClass->base_class.class_name,
			NhlscalarFieldFloatClass->base_class.class_name,
			CvtGenSFObjToFloatSFObj,NULL,0,False,NULL);
	return ret;
}


/*
 * Function:	MeshScalarFieldClassPartInitialize
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
MeshScalarFieldClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class structure	*/
)
#else
(lc)
	NhlClass	lc;	/* pointer to class structure	*/
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;

	return ret;
}


/*
 * Function:	VTypeValuesEqual
 *
 * Description:	This function does a float compare on two VTypes.
 *
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private
 * Returns:	True if both values are non-null and equal, False otherwise
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlBoolean
VTypeValuesEqual
#if	NhlNeedProto
(
        _NhlConvertContext	context,
        NhlGenArray		vta1,
        NhlGenArray     	vta2
)
#else
(context,vta1,vta2)
	_NhlConvertContext	context;
	NhlGenArray	vta1;
        NhlGenArray     vta2;
#endif
{
        NrmValue from, to;
        float f1,f2;
                
        if (! vta1 || ! vta2)
                return False;
        
        from.size = sizeof(NhlGenArray);
        from.data.ptrval = vta1;
        to.size = sizeof(float);
        to.data.ptrval = &f1;
        
        if (_NhlConvertData(context,Qgen_array,Qfloat,&from,&to) < NhlWARNING)
                return False;
        
        from.data.ptrval = vta2;
        to.data.ptrval = &f2;
        
        if (_NhlConvertData(context,Qgen_array,Qfloat,&from,&to) < NhlWARNING)
                return False;

        if (_NhlCmpFAny(f1,f2,5) == 0.0)
                return True;
        
        return False;
}

/*
 * Function:	MeshScalarFieldInitialize
 *
 * Description:	This function initializes an instance of a MeshScalarField
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
MeshScalarFieldInitialize
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
	char			*entry_name = "MeshScalarFieldInitialize";
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMeshScalarFieldLayer	sfl = (NhlMeshScalarFieldLayer)new;
	NhlMeshScalarFieldLayerPart	*sfp = &(sfl->msfield);
	NhlGenArray		ga;
        _NhlConvertContext	context = NULL;
	

	sfp->changed = 0;
        context = _NhlCreateConvertContext(new);
	sfp->sffloat = NULL;
        sfp->up_to_date = False;
        
        sfp->xstart_byindex = False;
        sfp->xend_byindex = False;
        sfp->ystart_byindex = False;
        sfp->yend_byindex = False;
	sfp->xc_is_bounds = False;
	sfp->yc_is_bounds = False;
	sfp->istride = 1;
/*
 * initialize unsupported resources
 */
	sfp->node_indexes = NULL;
	sfp->exchange_dimensions = False;
	sfp->x_start = sfp->x_end = sfp->y_start = sfp->y_end = NULL;
	sfp->x_subset_start = sfp->x_subset_end = 
		sfp->y_subset_start = sfp->y_subset_end = NULL;
	sfp->x_index_start = sfp->x_index_end = 
		sfp->y_index_start = sfp->y_index_end = -1;
	sfp->x_stride = sfp->y_stride = 1;
	sfp->subset_by_index = False;
/*
 * end unsupported resource initialization
 */

        
	if (sfp->d_arr == NULL) {
		e_text = 
		 "%s:The %s resource must be specified to create a %s object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NhlNsfDataArray,_NhlClassName(lc));
		return NhlFATAL;
	}
	else if (sfp->d_arr->num_dimensions != 1) {
		e_text = 
		 "%s:The %s resource must have only 1 dimension to create a %s object";
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
	sfp->changed |= _NhlsfDARR_CHANGED;
	sfp->d_el_count = sfp->d_arr->len_dimensions[0];

	if (sfp->d_el_count < 2) {
		e_text = "%s: Insufficient number of elements in %s",
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNsfDataArray);
		return NhlFATAL;
	}
        
        if (sfp->x_arr) {
                NrmValue from, to;
                NhlGenArray fltga;
                
                from.size = sizeof(NhlGenArray);
                from.data.ptrval = sfp->x_arr;
                to.size = sizeof(NhlGenArray);
                to.data.ptrval = &fltga;
                subret = _NhlConvertData(context,Qgen_array,
                                         Qfloat_gen_array,&from,&to);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        sfp->x_arr = NULL;
                else if (! ValidCoordArray
			 (sfp,fltga,Qx_arr,entry_name)) {
                        sfp->x_arr = NULL;
                }
                else {
                        if ((sfp->x_arr = _NhlCopyGenArray
                             (fltga,sfp->copy_arrays)) == NULL) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,
                                          NhlEUNKNOWN,e_text,entry_name);
                                return NhlFATAL;
                        }
			sfp->changed |= _NhlsfXARR_CHANGED;
			sfp->xc_el_count = sfp->d_el_count;
                }
	}
	if (sfp->x_arr == NULL && sfp->x_cell_bounds == NULL) {
		e_text = 
		 "%s:The %s or %s resource must be valid to create a %s object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NhlNsfXArray,NhlNsfXCellBounds,_NhlClassName(lc));
		return NhlFATAL;
	}

        if (sfp->y_arr) {
                NrmValue from, to;
                NhlGenArray fltga;
                
                from.size = sizeof(NhlGenArray);
                from.data.ptrval = sfp->y_arr;
                to.size = sizeof(NhlGenArray);
                to.data.ptrval = &fltga;
                subret = _NhlConvertData(context,Qgen_array,
                                         Qfloat_gen_array,&from,&to);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        sfp->y_arr = NULL;
                else if (! ValidCoordArray
			 (sfp,fltga,Qy_arr,entry_name)) {
                        sfp->y_arr = NULL;
                }
                else {
                        if ((sfp->y_arr = _NhlCopyGenArray
                             (fltga,sfp->copy_arrays)) == NULL) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,
                                          NhlEUNKNOWN,e_text,entry_name);
                                return NhlFATAL;
                        }
			sfp->changed |= _NhlsfYARR_CHANGED;
			sfp->yc_el_count = sfp->d_el_count;
                }
	}
	if (sfp->y_arr == NULL && sfp->y_cell_bounds == NULL) {
		e_text = 
		 "%s:The %s or %s resource must be valid to create a %s object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NhlNsfYArray,NhlNsfYCellBounds,_NhlClassName(lc));
		return NhlFATAL;
	}

#if 0
	if (sfp->element_nodes == NULL && 
		! (sfp->x_cell_bounds && sfp->y_cell_bounds)) {
		e_text = 
	 "%s:Either %s or %s and %s  must be specified to create a %s object";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NhlNsfElementNodes,
			  NhlNsfXCellBounds, NhlNsfYCellBounds,_NhlClassName(lc));
		return NhlFATAL;
	}
#endif
	if (sfp->element_nodes) {
		if (sfp->element_nodes->num_dimensions != 2) {
			e_text = 
	 "%s:The %s resource must have 2 dimensions to create a %s object";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNsfElementNodes,_NhlClassName(lc));
			return NhlFATAL;
		}
		if ((sfp->element_nodes = 
		     _NhlCopyGenArray(sfp->element_nodes,
				      sfp->copy_arrays)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	if (sfp->x_cell_bounds) {
                NrmValue from, to;
                NhlGenArray fltga;
                
                from.size = sizeof(NhlGenArray);
                from.data.ptrval = sfp->x_cell_bounds;
                to.size = sizeof(NhlGenArray);
                to.data.ptrval = &fltga;
                subret = _NhlConvertData(context,Qgen_array,
                                         Qfloat_gen_array,&from,&to);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        sfp->x_cell_bounds = NULL;
                else if (! ValidCoordArray
			 (sfp,fltga,Qx_cell_bounds,entry_name)) {
                        sfp->x_cell_bounds = NULL;
                }
                else {
                        if ((sfp->x_cell_bounds = _NhlCopyGenArray
                             (fltga,sfp->copy_arrays)) == NULL) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,
                                          NhlEUNKNOWN,e_text,entry_name);
                                return NhlFATAL;
                        }
			sfp->changed |= _NhlsfXARR_CHANGED;
                }
	}
	if (sfp->y_cell_bounds) {
                NrmValue from, to;
                NhlGenArray fltga;
                
                from.size = sizeof(NhlGenArray);
                from.data.ptrval = sfp->y_cell_bounds;
                to.size = sizeof(NhlGenArray);
                to.data.ptrval = &fltga;
                subret = _NhlConvertData(context,Qgen_array,
                                         Qfloat_gen_array,&from,&to);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        sfp->y_cell_bounds = NULL;
                else if (! ValidCoordArray
			 (sfp,fltga,Qy_cell_bounds,entry_name)) {
                        sfp->y_cell_bounds = NULL;
                }
                else {
                        if ((sfp->y_cell_bounds = _NhlCopyGenArray
                             (fltga,sfp->copy_arrays)) == NULL) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,
                                          NhlEUNKNOWN,e_text,entry_name);
                                return NhlFATAL;
                        }
			sfp->changed |= _NhlsfYARR_CHANGED;
                }
	}
	if ((sfp->x_cell_bounds && ! sfp->y_cell_bounds) ||
	    (sfp->y_cell_bounds && ! sfp->x_cell_bounds)) {
		e_text = 
       "%s:If either %s or %s is specified, both must be specified and valid";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NhlNsfXCellBounds, NhlNsfYCellBounds);
		return NhlFATAL;
	}

        if (sfp->node_indexes) {
		if ((sfp->node_indexes = 
		     _NhlCopyGenArray(
			     sfp->node_indexes,sfp->copy_arrays)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	if (sfp->missing_value != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->missing_value,
					NhlNsfMissingValueV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->missing_value = ga;
	}

	if (sfp->data_min != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->data_min,
					NhlNsfDataMinV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->data_min = ga;
	}
	if (sfp->data_max != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->data_max,
					NhlNsfDataMaxV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->data_max = ga;
	}

        if (VTypeValuesEqual(context,sfp->x_start,sfp->x_end)) {
                e_text = "%s: %s and %s values cannot be equal, defaulting";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          NhlNsfXCStartV,NhlNsfXCEndV);
                ret = MIN(ret,NhlWARNING);
                sfp->x_start = NULL;
                sfp->x_end = NULL;
        }
	if (sfp->x_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_start,
					NhlNsfXCStartV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_start = ga;
	}
	if (sfp->x_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->x_end,
					NhlNsfXCEndV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_end = ga;
	}


        if (VTypeValuesEqual(context,sfp->y_start,sfp->y_end)) {
                e_text = "%s: %s and %s values cannot be equal, defaulting";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          NhlNsfYCStartV,NhlNsfYCEndV);
                ret = MIN(ret,NhlWARNING);
                sfp->y_start = NULL;
                sfp->y_end = NULL;
        }
	if (sfp->y_start != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_start,
					NhlNsfYCStartV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_start = ga;
	}
	if (sfp->y_end != NULL) {
		ga = NULL;
		subret = CheckCopyVType(&ga,sfp->y_end,
					NhlNsfYCEndV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_end = ga;
	}
        
        if (VTypeValuesEqual(context,sfp->x_subset_start,sfp->x_subset_end)) {
                e_text = "%s: %s and %s values cannot be equal, defaulting";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          NhlNsfXCStartSubsetV,NhlNsfXCEndSubsetV);
                ret = MIN(ret,NhlWARNING);
                sfp->x_subset_start = NULL;
                sfp->x_subset_end = NULL;
        }
	if (sfp->x_subset_start != NULL) {
                if (sfp->subset_by_index)
                        sfp->x_subset_start = NULL;
                else {
                        ga = NULL;
                        subret = CheckCopyVType
                                (&ga,sfp->x_subset_start,
                                 NhlNsfXCStartSubsetV,True,entry_name);
                        if ((ret = MIN(ret,subret)) < NhlWARNING)
                                return ret;
                        sfp->x_subset_start = ga;
                }
	}
        if (! sfp->x_subset_start)
                sfp->xstart_byindex = True;
                
	if (sfp->x_subset_end != NULL) {
                if (sfp->subset_by_index)
                        sfp->x_subset_end = NULL;
                else {
                        ga = NULL;
                        subret = CheckCopyVType
                                (&ga,sfp->x_subset_end,
                                 NhlNsfXCEndSubsetV,True,entry_name);
                        if ((ret = MIN(ret,subret)) < NhlWARNING)
                                return ret;
                        sfp->x_subset_end = ga;
                }
	}
        if (! sfp->x_subset_end)
                sfp->xend_byindex = True;


        if (VTypeValuesEqual(context,sfp->y_subset_start,sfp->y_subset_end)) {
                e_text = "%s: %s and %s values cannot be equal, defaulting";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          NhlNsfYCStartV,NhlNsfYCEndV);
                ret = MIN(ret,NhlWARNING);
                sfp->y_subset_start = NULL;
                sfp->y_subset_end = NULL;
        }
	if (sfp->y_subset_start != NULL) {
                if (sfp->subset_by_index)
                        sfp->y_subset_start = NULL;
                else {
                        ga = NULL;
                        subret = CheckCopyVType
                                (&ga,sfp->y_subset_start,
                                 NhlNsfYCStartSubsetV,True,entry_name);
                        if ((ret = MIN(ret,subret)) < NhlWARNING)
                                return ret;
                        sfp->y_subset_start = ga;
                }
	}
        if (! sfp->y_subset_start)
                sfp->ystart_byindex = True;
                
	if (sfp->y_subset_end != NULL) {
                if (sfp->subset_by_index)
                        sfp->y_subset_end = NULL;
                else {
                        ga = NULL;
                        subret = CheckCopyVType
                                (&ga,sfp->y_subset_end,
                                 NhlNsfYCEndSubsetV,True,entry_name);
                        if ((ret = MIN(ret,subret)) < NhlWARNING)
                                return ret;
                        sfp->y_subset_end = ga;
                }
	}
        if (! sfp->y_subset_end)
                sfp->yend_byindex = True;
	
        _NhlFreeConvertContext(context);
	return ret;
}

/*
 * Function:	MeshScalarFieldSetValues
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
MeshScalarFieldSetValues
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
	char			*entry_name = "MeshScalarFieldSetValues";
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char 			*e_text;
	NhlMeshScalarFieldLayer	sfl = (NhlMeshScalarFieldLayer)new;
	NhlMeshScalarFieldLayer	osfl = (NhlMeshScalarFieldLayer)old;
	NhlMeshScalarFieldLayerPart	*sfp = &(sfl->msfield);
	NhlMeshScalarFieldLayerPart	*osfp = &(osfl->msfield);
	NhlGenArray		ga;
	NhlBoolean		status = False;
        _NhlConvertContext	context = NULL;
        NhlBoolean		x_arr_changed = False, y_arr_changed = False;
	NhlBoolean		dim_changed = False;
        NhlBoolean		x_start_changed = False, x_end_changed = False;
        NhlBoolean		y_start_changed = False, y_end_changed = False;

/*
 * The changed bit field records changes to the X and Y coordinate array
 * as passed to the MeshScalarFieldFloat object. Changes to the array itself
 * count, but also subsection and stride changes.
 */
	sfp->changed = 0;
        context = _NhlCreateConvertContext(new);
	if (sfp->d_arr != osfp->d_arr) {
		if (sfp->d_arr == NULL || sfp->d_arr->num_dimensions != 1 ||
			sfp->d_arr->len_dimensions[0] < 3) {
			e_text = 
			   "%s: %s is missing or has more than 1 dimension: restoring previous value";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
				  entry_name,NhlNsfDataArray);
			ret = NhlWARNING;
			sfp->d_arr = osfp->d_arr;
		}
		else {
			if ((ga = 
			     _NhlCopyGenArray(sfp->d_arr,
					      sfp->copy_arrays)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			sfp->d_arr = ga;
			NhlFreeGenArray(osfp->d_arr);
			status = True;
			if (sfp->d_arr->len_dimensions[0] != sfp->d_el_count)
				dim_changed = True;
			sfp->d_el_count = sfp->d_arr->len_dimensions[0];
			sfp->changed |= _NhlsfDARR_CHANGED;
		}
	}


	if (!sfp->x_arr && ! sfp->x_cell_bounds) {
		e_text = "%s: invalid %s value: restoring previous value";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNsfXArray);
		ret = NhlWARNING;
		sfp->x_arr = osfp->x_arr;
        }
        else if (sfp->x_arr != osfp->x_arr) {
                NrmValue from, to;
                NhlGenArray fltga;
                
                from.size = sizeof(NhlGenArray);
                from.data.ptrval = sfp->x_arr;
                to.size = sizeof(NhlGenArray);
                to.data.ptrval = &fltga;
                subret = _NhlConvertData(context,Qgen_array,
                                         Qfloat_gen_array,&from,&to);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        sfp->x_arr = osfp->x_arr;
                else if (! ValidCoordArray(
				 sfp,fltga,Qx_arr,entry_name)) {
                        sfp->x_arr = osfp->x_arr;
                }
                else {
		        if (dim_changed || 
			    sfp->x_arr->size != osfp->x_arr->size ||
			    sfp->x_arr->typeQ != osfp->x_arr->typeQ ||
			    memcmp(sfp->x_arr->data,osfp->x_arr->data,
				   sfp->x_arr->size 
				   * sfp->x_arr->num_elements) )
				x_arr_changed = True;

                        if ((sfp->x_arr = _NhlCopyGenArray
                             (fltga,sfp->copy_arrays)) == NULL) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,
                                          NhlEUNKNOWN,e_text,entry_name);
                                return NhlFATAL;
                        }
                        NhlFreeGenArray(osfp->x_arr);
			sfp->changed |= _NhlsfXARR_CHANGED;
                        status = True;
			sfp->xc_el_count = sfp->d_el_count;
                }
	}

	if (!sfp->y_arr && !sfp->y_cell_bounds) {
		e_text = "%s: invalid %s value: restoring previous value";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNsfYArray);
		ret = NhlWARNING;
		sfp->y_arr = osfp->y_arr;
        }
        else if (sfp->y_arr != osfp->y_arr) {
                NrmValue from, to;
                NhlGenArray fltga;
                
                from.size = sizeof(NhlGenArray);
                from.data.ptrval = sfp->y_arr;
                to.size = sizeof(NhlGenArray);
                to.data.ptrval = &fltga;
                subret = _NhlConvertData(context,Qgen_array,
                                         Qfloat_gen_array,&from,&to);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        sfp->y_arr = osfp->y_arr;
                else if (! ValidCoordArray
			 (sfp,fltga,Qy_arr,entry_name)) {
                        sfp->y_arr = osfp->y_arr;
                }
                else {
		        if (dim_changed || 
			    sfp->y_arr->size != osfp->y_arr->size ||
			    sfp->y_arr->typeQ != osfp->y_arr->typeQ ||
			    memcmp(sfp->y_arr->data,osfp->y_arr->data,
				   sfp->y_arr->size 
				   * sfp->y_arr->num_elements) )
				y_arr_changed = True;

                        if ((sfp->y_arr = _NhlCopyGenArray
                             (fltga,sfp->copy_arrays)) == NULL) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,
                                          NhlEUNKNOWN,e_text,entry_name);
                                return NhlFATAL;
                        }
                        NhlFreeGenArray(osfp->y_arr);
			sfp->changed |= _NhlsfYARR_CHANGED;
                        status = True;
			sfp->yc_el_count = sfp->d_el_count;
                }
	}

/*
 * If dimension lengths have changed then subsetting returns to default
 */
	if (dim_changed) {
		if (! _NhlArgIsSet(args,nargs,NhlNsfXCStartIndex))
			sfp->x_index_start = -1;
		if (! _NhlArgIsSet(args,nargs,NhlNsfXCEndIndex))
			sfp->x_index_end = -1;
		if (! _NhlArgIsSet(args,nargs,NhlNsfYCStartIndex))
			sfp->y_index_start = -1;
		if (! _NhlArgIsSet(args,nargs,NhlNsfYCEndIndex))
			sfp->y_index_end = -1;
	}

	if (sfp->element_nodes && sfp->element_nodes->num_dimensions != 2) {
		e_text = 
			"%s: invalid %s value: restoring previous value";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNsfElementNodes);
		ret = NhlWARNING;
		sfp->element_nodes = osfp->element_nodes;
	}
	if (sfp->element_nodes != osfp->element_nodes) {
		if (sfp->element_nodes) {
			if ((ga = _NhlCopyGenArray(
				     sfp->element_nodes,
				     sfp->copy_arrays)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			sfp->element_nodes = ga;
		}
		NhlFreeGenArray(osfp->element_nodes);
		status = True;
	}

        if (sfp->node_indexes != osfp->node_indexes) {
		if (sfp->node_indexes) {
			if ((ga = _NhlCopyGenArray(
				     sfp->node_indexes,
				     sfp->copy_arrays)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return NhlFATAL;
			}
			sfp->node_indexes = ga;
		}
		NhlFreeGenArray(osfp->node_indexes);
		status = True;
	}

	if ((sfp->x_cell_bounds && ! sfp->y_cell_bounds) ||
	    (sfp->y_cell_bounds && ! sfp->x_cell_bounds)) {
		sfp->x_cell_bounds = osfp->x_cell_bounds;
		sfp->y_cell_bounds = osfp->y_cell_bounds;
		e_text = 
      "%s:If either %s or %s is specified, both must be specified and valid: restoring previous values";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
			  entry_name,NhlNsfXCellBounds, NhlNsfYCellBounds);
		return NhlWARNING;
	}
	if (! sfp->x_cell_bounds) {
		if (osfp->x_cell_bounds) {
			NhlFreeGenArray(osfp->x_cell_bounds);
			status = True;
		}
	}
        else if (sfp->x_cell_bounds && 
		 sfp->x_cell_bounds != osfp->x_cell_bounds) {
                NrmValue from, to;
                NhlGenArray fltga;
                
                from.size = sizeof(NhlGenArray);
                from.data.ptrval = sfp->x_cell_bounds;
                to.size = sizeof(NhlGenArray);
                to.data.ptrval = &fltga;
                subret = _NhlConvertData(context,Qgen_array,
                                         Qfloat_gen_array,&from,&to);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        sfp->x_cell_bounds = osfp->x_cell_bounds;
                else if (! ValidCoordArray(
				 sfp,fltga,Qx_cell_bounds,entry_name)) {
                        sfp->x_cell_bounds = osfp->x_cell_bounds;
                }
                else {
		        if (dim_changed || 
			    ! osfp->x_cell_bounds ||
			    sfp->x_cell_bounds->size != 
			    osfp->x_cell_bounds->size ||
			    sfp->x_cell_bounds->typeQ != 
			    osfp->x_cell_bounds->typeQ ||
			    memcmp(sfp->x_cell_bounds->data,
				   osfp->x_cell_bounds->data,
				   sfp->x_cell_bounds->size 
				   * sfp->x_cell_bounds->num_elements) )
				x_arr_changed = True;

                        if ((sfp->x_cell_bounds = _NhlCopyGenArray
                             (fltga,sfp->copy_arrays)) == NULL) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,
                                          NhlEUNKNOWN,e_text,entry_name);
                                return NhlFATAL;
                        }
                        NhlFreeGenArray(osfp->x_cell_bounds);
			sfp->changed |= _NhlsfXARR_CHANGED;
                        status = True;
                }
	}

	if (! sfp->y_cell_bounds) {
		if (osfp->y_cell_bounds) {
			NhlFreeGenArray(osfp->y_cell_bounds);
			status = True;
		}
	}
        if (sfp->y_cell_bounds && sfp->y_cell_bounds != osfp->y_cell_bounds) {
                NrmValue from, to;
                NhlGenArray fltga;
                
                from.size = sizeof(NhlGenArray);
                from.data.ptrval = sfp->y_cell_bounds;
                to.size = sizeof(NhlGenArray);
                to.data.ptrval = &fltga;
                subret = _NhlConvertData(context,Qgen_array,
                                         Qfloat_gen_array,&from,&to);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        sfp->y_cell_bounds = osfp->y_cell_bounds;
                else if (! ValidCoordArray(
				 sfp,fltga,Qy_cell_bounds,entry_name)) {
                        sfp->y_cell_bounds = osfp->y_cell_bounds;
                }
                else {
		        if (dim_changed || 
			    ! osfp->y_cell_bounds ||
			    sfp->y_cell_bounds->size != 
			    osfp->y_cell_bounds->size ||
			    sfp->y_cell_bounds->typeQ != 
			    osfp->y_cell_bounds->typeQ ||
			    memcmp(sfp->y_cell_bounds->data,
				   osfp->y_cell_bounds->data,
				   sfp->y_cell_bounds->size 
				   * sfp->y_cell_bounds->num_elements) )
				y_arr_changed = True;

                        if ((sfp->y_cell_bounds = _NhlCopyGenArray
                             (fltga,sfp->copy_arrays)) == NULL) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,
                                          NhlEUNKNOWN,e_text,entry_name);
                                return NhlFATAL;
                        }
                        NhlFreeGenArray(osfp->y_cell_bounds);
			sfp->changed |= _NhlsfYARR_CHANGED;
                        status = True;
                }
	}

	if (sfp->missing_value != osfp->missing_value) {
		subret = CheckCopyVType(&osfp->missing_value,
					sfp->missing_value,
					NhlNsfMissingValueV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->missing_value = osfp->missing_value;
		status = True;
	}

	if (sfp->data_min != osfp->data_min) {
		subret = CheckCopyVType(&osfp->data_min,sfp->data_min,
					NhlNsfDataMinV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->data_min = osfp->data_min;
		status = True;
	}
	if (sfp->data_max != osfp->data_max) {
		subret = CheckCopyVType(&osfp->data_max,sfp->data_max,
					NhlNsfDataMaxV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->data_max = osfp->data_max;
		status = True;
	}

        if (VTypeValuesEqual(context,sfp->x_start,sfp->x_end)) {
                e_text =
             "%s: %s and %s values cannot be equal, restoring previous values";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          NhlNsfXCStartV,NhlNsfXCEndV);
                ret = MIN(ret,NhlWARNING);
                sfp->x_start = osfp->x_start;
                sfp->x_end = osfp->x_end;
        }
	if (sfp->x_start != osfp->x_start) {
		subret = CheckCopyVType(&osfp->x_start,sfp->x_start,
					NhlNsfXCStartV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_start = osfp->x_start;
                x_start_changed = True;
		status = True;
	}
        else if (x_arr_changed && sfp->x_start) {
               	NhlFreeGenArray(osfp->x_start);
                sfp->x_start = NULL;
        }
	if (sfp->x_end != osfp->x_end) {
		subret = CheckCopyVType(&osfp->x_end,sfp->x_end,
					NhlNsfXCEndV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_end = osfp->x_end;
                x_end_changed = True;
		status = True;
	}
        else if (x_arr_changed && sfp->x_end) {
               	NhlFreeGenArray(osfp->x_end);
                sfp->x_end = NULL;
        }

        if (VTypeValuesEqual(context,sfp->y_start,sfp->y_end)) {
                e_text =
             "%s: %s and %s values cannot be equal, restoring previous values";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          NhlNsfYCStartV,NhlNsfYCEndV);
                ret = MIN(ret,NhlWARNING);
                sfp->y_start = osfp->y_start;
                sfp->y_end = osfp->y_end;
        }
	if (sfp->y_start != osfp->y_start) {
		subret = CheckCopyVType(&osfp->y_start,sfp->y_start,
					NhlNsfYCStartV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_start = osfp->y_start;
                y_start_changed = True;
		status = True;
	}
        else if (y_arr_changed && sfp->y_start) {
               	NhlFreeGenArray(osfp->y_start);
                sfp->y_start = NULL;
        }
	if (sfp->y_end != osfp->y_end) {
		subret = CheckCopyVType(&osfp->y_end,sfp->y_end,
					NhlNsfYCEndV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_end = osfp->y_end;
                y_end_changed = True;
		status = True;
	}
        else if (y_arr_changed && sfp->y_end) {
               	NhlFreeGenArray(osfp->y_end);
                sfp->y_end = NULL;
        }

        if (VTypeValuesEqual(context,sfp->x_subset_start,sfp->x_subset_end)) {
                e_text =
             "%s: %s and %s values cannot be equal, restoring previous values";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          NhlNsfXCStartSubsetV,NhlNsfXCEndSubsetV);
                ret = MIN(ret,NhlWARNING);
                sfp->x_subset_start = osfp->x_subset_start;
                sfp->x_subset_end = osfp->y_subset_end;
        }
	if (sfp->x_subset_start != osfp->x_subset_start) {
		subret = CheckCopyVType(&osfp->x_subset_start,
					sfp->x_subset_start,
					NhlNsfXCStartSubsetV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->changed |= _NhlsfXARR_CHANGED;
		sfp->x_subset_start = osfp->x_subset_start;
		status = True;
	}
        else if (x_arr_changed || sfp->subset_by_index ||
                 x_start_changed || x_end_changed ||
                 sfp->x_index_start != osfp->x_index_start) {
                NhlFreeGenArray(osfp->x_subset_start);
                sfp->x_subset_start = NULL;
        }
        sfp->xstart_byindex = sfp->x_subset_start ? False : True;
        
	if (sfp->x_subset_end != osfp->x_subset_end) {
		subret = CheckCopyVType(&osfp->x_subset_end,sfp->x_subset_end,
					NhlNsfXCEndSubsetV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->x_subset_end = osfp->x_subset_end;
		sfp->changed |= _NhlsfXARR_CHANGED;
		status = True;
	}
        else if (x_arr_changed || sfp->subset_by_index ||
                 x_start_changed || x_end_changed ||
                 sfp->x_index_end != osfp->x_index_end) {
               	NhlFreeGenArray(osfp->x_subset_end);
                sfp->x_subset_end = NULL;
        }
        sfp->xend_byindex = sfp->x_subset_end ? False : True;


        if (VTypeValuesEqual(context,sfp->y_subset_start,sfp->y_subset_end)) {
                e_text =
             "%s: %s and %s values cannot be equal, restoring previous values";
                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          NhlNsfYCStartSubsetV,NhlNsfYCEndSubsetV);
                ret = MIN(ret,NhlWARNING);
                sfp->y_subset_start = osfp->y_subset_start;
                sfp->y_subset_end = osfp->y_subset_end;
        }
	if (sfp->y_subset_start != osfp->y_subset_start) {
		subret = CheckCopyVType(&osfp->y_subset_start,
					sfp->y_subset_start,
					NhlNsfYCStartSubsetV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_start = osfp->y_subset_start;
		sfp->changed |= _NhlsfYARR_CHANGED;
		status = True;
	}
        else if (y_arr_changed || sfp->subset_by_index ||
                 y_start_changed || y_end_changed ||
                 sfp->y_index_start != osfp->y_index_start) {
               	NhlFreeGenArray(osfp->y_subset_start);
                sfp->y_subset_start = NULL;
        }
        sfp->ystart_byindex = sfp->y_subset_start ? False : True;

        
	if (sfp->y_subset_end != osfp->y_subset_end) {
		subret = CheckCopyVType(&osfp->y_subset_end,sfp->y_subset_end,
					NhlNsfYCEndSubsetV,True,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING)
		    return ret;
		sfp->y_subset_end = osfp->y_subset_end;
		sfp->changed |= _NhlsfYARR_CHANGED;
		status = True;
	}
        else if (y_arr_changed || sfp->subset_by_index ||
                 y_start_changed || y_end_changed ||
                 sfp->y_index_end != osfp->y_index_end) {
               	NhlFreeGenArray(osfp->y_subset_end);
                sfp->y_subset_end = NULL;
        }
        sfp->yend_byindex = sfp->y_subset_end ? False : True;

	if (sfp->x_index_start != osfp->x_index_start) {
		if (sfp->xstart_byindex)
			sfp->changed |= _NhlsfXARR_CHANGED;
		status = True;
	}
	if (sfp->x_index_end != osfp->x_index_end) {
		if (sfp->xend_byindex)
			sfp->changed |= _NhlsfXARR_CHANGED;
		status = True;
	}

	if (sfp->y_index_start != osfp->y_index_start) {
		if (sfp->ystart_byindex)
			sfp->changed |= _NhlsfYARR_CHANGED;
		status = True;
	}
	if (sfp->y_index_end != osfp->y_index_end) {
		if (sfp->yend_byindex)
			sfp->changed |= _NhlsfYARR_CHANGED;
		status = True;
	}

	if (sfp->x_stride != osfp->x_stride) {
		sfp->changed |= _NhlsfXARR_CHANGED;
		status = True;
	}
	if (sfp->y_stride != osfp->y_stride) {
		sfp->changed |= _NhlsfYARR_CHANGED;
		status = True;
	}
	if (sfp->exchange_dimensions != osfp->exchange_dimensions) {
		sfp->changed |= _NhlsfXARR_CHANGED;
		sfp->changed |= _NhlsfYARR_CHANGED;
		status = True;
	}
	if (sfp->subset_by_index != osfp->subset_by_index)
		status = True;

	if (sfp->changed & _NhlsfXARR_CHANGED ||
	     sfp->changed & _NhlsfYARR_CHANGED) {
		sfp->changed |= _NhlsfXARR_CHANGED;
		sfp->changed |= _NhlsfYARR_CHANGED;
	}
                
        _NhlDataChanged((NhlDataItemLayer)new,status);

        if (status)
                sfp->up_to_date = False;

        _NhlFreeConvertContext(context);

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
	char *e_text, *entry_name = "MeshScalarFieldGetValues";
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
 * Function:    CreateVData
 *
 * Description: Create the data for a VType GenArray given any type value
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlPointer CreateVData
#if	NhlNeedProto
(
	NhlPointer value,
	int size,
	NrmQuark resQ
)
#else
(value,size,resQ)
	NhlPointer value;
	int   size;
	NrmQuark resQ;
#endif
{
	char *e_text, *entry_name = "MeshScalarFieldGetValues";
	NhlPointer new;

	if ((new = NhlMalloc(size)) == NULL) {
		e_text = "%s: dynamic memory allocation error for %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,NrmQuarkToString(resQ));
		return NULL;
	}
	memcpy(new,value,size);
	
	return new;
}


/*
 * Function:    ForceConvert
 *
 * Description: Explicitly forces the conversion to NhlMeshScalarFieldFloat
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

static NhlErrorTypes ForceConvert
#if	NhlNeedProto
(
	NhlMeshScalarFieldLayer sfl
)
#else
(sfl)
	NhlMeshScalarFieldLayer sfl;
#endif
{
	char *e_text, *entry_name = "MeshScalarFieldGetValues";
        NhlMeshScalarFieldLayerPart *sfp = &(sfl->msfield);
	int id;
	NhlScalarFieldFloatLayer sffl = NULL;
	NhlErrorTypes ret = NhlNOERROR;
	NrmValue from, to;

	from.size = sizeof(NhlMeshScalarFieldLayerRec);
	from.data.intval = sfl->base.id;
	to.size = sizeof(NhlScalarFieldFloatLayerRec);
	to.data.ptrval = &id;
	ret = NhlConvertData(NhlDEFAULT_APP,
			NhlmeshScalarFieldClass->base_class.class_name,
			NhlscalarFieldFloatClass->base_class.class_name,
			     &from,&to);
	if (ret < NhlWARNING ||
	    (sffl = (NhlScalarFieldFloatLayer) _NhlGetLayer(id)) == NULL) {
		e_text = "%s: error converting NhlMeshScalarFieldLayer";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
                return ret;
	}
        sfp->sffloat = sffl;
        
	return ret;
}

/*
 * Function:    MeshScalarFieldGetValues
 *
 * Description: Retrieves the current setting of one or more MeshScalarField 
 *      resources.This routine only retrieves resources that require 
 *	special methods that the generic GetValues method cannot handle. 
 *      This includes all resources implemented as GenArrays, including
 *	the variable type meshScalar resources (VTypes). In general space is 
 *	allocated; the user is responsible for freeing this space. However,
 *	if the CopyArrays resource is False, Data array and the X/Y
 *	coordinate arrays (if they exist) are NOT copied, since the user
 *	is assumed to be keeping a valid copy of them around. 
 *	If the user does a GetValues on the data min or max before a
 *	the conversion to MeshScalarFieldFloat object has taken place, then
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

static NhlErrorTypes    MeshScalarFieldGetValues
#if	NhlNeedProto
(NhlLayer layer, _NhlArgList args, int num_args)
#else
(layer,args,num_args)
        NhlLayer        layer;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlMeshScalarFieldLayer sfl = (NhlMeshScalarFieldLayer)layer;
        NhlMeshScalarFieldLayerPart *sfp = &(sfl->msfield);
	NhlScalarFieldFloatLayerPart *sffp = NULL;
	NhlErrorTypes subret = NhlNOERROR,ret = NhlNOERROR;
        NhlGenArray ga;
        char *e_text, *entry_name = "MeshScalarFieldGetValues";
        int i;
        NrmQuark resQ;
	NrmQuark typeQ = NrmNULLQUARK;
	NhlPointer	data;
	ng_size_t       dlen[2];
	int		ndim;
	int		size;
	NhlBoolean	nocopy = False, do_genarray;
	int		ival;
	float		fval;
		
	if (sfp->d_arr == NULL) {
		e_text = "%s: internal inconsistency";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (! (sfp->sffloat && sfp->up_to_date)) {
		subret = ForceConvert(sfl);
                if ((ret = MIN(subret,ret))  < NhlWARNING)
                        return ret;
        }
        sffp = &sfp->sffloat->sfieldfloat;

        for( i = 0; i< num_args; i++ ) {
		ga = NULL;
		resQ = args[i].quark;
		do_genarray = False;
                if (resQ == Qd_arr) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = sfp->d_arr->len_dimensions[0];
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
			dlen[0] = sfp->x_arr->len_dimensions[0];
                        if (sfp->copy_arrays) {
                                if ((data = CopyData(sfp->x_arr,resQ)) == NULL)
                                        return NhlFATAL;
                        }
                        else {
                                nocopy = True;
                                data = sfp->x_arr->data;
                        }
                        typeQ = sfp->x_arr->typeQ;
                        size = sfp->x_arr->size;
                }
                else if (resQ == Qy_arr) {
                        do_genarray = True;
			ndim = 1;
			dlen[0] = sfp->y_arr->len_dimensions[0];
                        if (sfp->copy_arrays) {
                                if ((data = CopyData(sfp->y_arr,resQ)) == NULL)
                                        return NhlFATAL;
                        }
                        else {
                                nocopy = True;
                                data = sfp->y_arr->data;
                        }
                        typeQ = sfp->y_arr->typeQ;
                        size = sfp->y_arr->size;
                }
                else if (resQ == Qx_cell_bounds && sfp->x_cell_bounds) {
                        do_genarray = True;
			ndim = 2;
			dlen[0] = sfp->x_cell_bounds->len_dimensions[0];
			dlen[1] = sfp->x_cell_bounds->len_dimensions[1];
                        if (sfp->copy_arrays) {
                                if ((data = CopyData
				     (sfp->x_cell_bounds,resQ)) == NULL)
                                        return NhlFATAL;
                        }
                        else {
                                nocopy = True;
                                data = sfp->y_arr->data;
                        }
                        typeQ = sfp->x_cell_bounds->typeQ;
                        size = sfp->x_cell_bounds->size;
                }
		else if (resQ == Qx_cell_bounds) {
			*(NhlGenArray *)args[i].value.ptrval = NULL;
			*args[i].type_ret = Qgen_array;
			*args[i].size_ret = sizeof(NhlGenArray);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_cell_bounds && sfp->y_cell_bounds) {
                        do_genarray = True;
			ndim = 2;
			dlen[0] = sfp->y_cell_bounds->len_dimensions[0];
			dlen[1] = sfp->y_cell_bounds->len_dimensions[1];
                        if (sfp->copy_arrays) {
                                if ((data = CopyData
				     (sfp->y_cell_bounds,resQ)) == NULL)
                                        return NhlFATAL;
                        }
                        else {
                                nocopy = True;
                                data = sfp->y_arr->data;
                        }
                        typeQ = sfp->y_cell_bounds->typeQ;
                        size = sfp->y_cell_bounds->size;
                }
		else if (resQ == Qy_cell_bounds) {
			*(NhlGenArray *)args[i].value.ptrval = NULL;
			*args[i].type_ret = Qgen_array;
			*args[i].size_ret = sizeof(NhlGenArray);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qelement_nodes && sfp->element_nodes) {
                        do_genarray = True;
			ndim = 2;
			dlen[0] = sfp->element_nodes->len_dimensions[0];
			dlen[1] = sfp->element_nodes->len_dimensions[1];
                        if (sfp->copy_arrays) {
                                if ((data = CopyData
				     (sfp->element_nodes,resQ)) == NULL)
                                        return NhlFATAL;
                        }
                        else {
                                nocopy = True;
                                data = sfp->y_arr->data;
                        }
                        typeQ = sfp->element_nodes->typeQ;
                        size = sfp->element_nodes->size;
                }
		else if (resQ == Qelement_nodes) {
			*(NhlGenArray *)args[i].value.ptrval = NULL;
			*args[i].type_ret = Qgen_array;
			*args[i].size_ret = sizeof(NhlGenArray);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qnode_indexes && sfp->node_indexes) {
                        do_genarray = True;
			ndim = 1;
			dlen[0] = sfp->node_indexes->len_dimensions[0];
                        if (sfp->copy_arrays) {
                                if ((data = 
				     CopyData(sfp->node_indexes,resQ)) == NULL)
                                        return NhlFATAL;
                        }
                        else {
                                nocopy = True;
                                data = sfp->node_indexes->data;
                        }
                        typeQ = sfp->node_indexes->typeQ;
                        size = sfp->node_indexes->size;
                }
                else if (resQ == Qnode_indexes) {
			int j;
                        do_genarray = True;
			ndim = 1;
			dlen[0] = sfp->d_el_count;
			data = NhlMalloc(dlen[0] * sizeof(int));
			for (j = 0; j < sfp->d_el_count; j++) {
				((int *)data)[j] = j + sfp->first_node_index;
			}
                        typeQ = Qint;
                        size = sizeof(int);
                }
                else if (resQ == Qmissing_value && sfp->missing_value) {
                        do_genarray = True;
                        ndim = 1;
                        dlen[0] = sfp->missing_value->len_dimensions[0];
                        if ((data = CopyData(sfp->missing_value,resQ)) == NULL)
                                return NhlFATAL;
                        typeQ = sfp->missing_value->typeQ;
                        size = sfp->missing_value->size;
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
				if ((data = 
				     CreateVData((NhlPointer)&sffp->data_min,
						 sizeof(float),resQ)) == NULL)
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
				typeQ = sfp->data_max->typeQ;
				size = sfp->data_max->size;
			}
			else {
				if ((data = 
				     CreateVData((NhlPointer)&sffp->data_max,
						 sizeof(float),resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
#if 0
                else if (resQ == Qx_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->x_arr) {
				data = CreateVData
				     ((NhlPointer)((char *)sfp->x_arr->data +
				       (sfp->xc_start_el * sfp->x_arr->size)),
				      sfp->x_arr->size,resQ);
				if (!data)
					return NhlFATAL;
				typeQ = sfp->x_arr->typeQ;
				size = sfp->x_arr->size;
			}
			else if (sfp->x_start != NULL) {
				if ((data = CopyData(sfp->x_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_start->typeQ;
				size = sfp->x_start->size;
			}
			else {
				tmp = 0.0;
				if ((data = CreateVData((NhlPointer)&tmp,
						 sizeof(float),resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->x_arr) {
				data = CreateVData
				  ((NhlPointer)((char *)sfp->x_arr->data + 
				   (sfp->xc_end_el * sfp->x_arr->size)),
				   sfp->x_arr->size,resQ);
				if (!data)
					return NhlFATAL;
				typeQ = sfp->x_arr->typeQ;
				size = sfp->x_arr->size;
			}
			else if (sfp->x_end != NULL) {
				if ((data = CopyData(sfp->x_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_end->typeQ;
				size = sfp->x_end->size;
			}
			else {
				tmp = (float)sfp->xc_el_count - 1;
				if ((data = CreateVData((NhlPointer)&tmp,
						 sizeof(float),resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->y_arr) {
				data = CreateVData
				  ((NhlPointer)((char *)sfp->y_arr->data + 
				   (sfp->yc_start_el * sfp->y_arr->size)),
				   sfp->y_arr->size,resQ);
				if (!data)
					return NhlFATAL;
				typeQ = sfp->y_arr->typeQ;
				size = sfp->y_arr->size;
			}
			else if (sfp->y_start != NULL) {
				if ((data = CopyData(sfp->y_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_start->typeQ;
				size = sfp->y_start->size;
			}
			else {
				tmp = 0.0;
				if ((data = CreateVData((NhlPointer)&tmp,
						 sizeof(float),resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (sfp->y_arr) {
				data = CreateVData
				  ((NhlPointer)((char *)sfp->y_arr->data + 
				   (sfp->yc_end_el * sfp->y_arr->size)),
				   sfp->y_arr->size,resQ);
				if (!data)
					return NhlFATAL;
				typeQ = sfp->y_arr->typeQ;
				size = sfp->y_arr->size;
				
			}
			else if (sfp->y_end != NULL) {
				if ((data = CopyData(sfp->y_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_end->typeQ;
				size = sfp->y_end->size;
			}
			else {
				tmp = sfp->yc_el_count - 1;
				if ((data = CreateVData((NhlPointer)&tmp,
						 sizeof(float),resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_subset_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (! sfp->subset_by_index &&
			    sfp->x_subset_start != NULL) {
				if ((data = CopyData(sfp->x_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_subset_start->typeQ;
				size = sfp->x_subset_start->size;
			}
			else {
				if ((data = 
				     CreateVData((NhlPointer)
						 &sfp->x_actual_start,
						 sizeof(float),resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_subset_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (! sfp->subset_by_index &&
			    sfp->x_subset_end != NULL) {
				if ((data = CopyData(sfp->x_subset_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->x_subset_end->typeQ;
				size = sfp->x_subset_end->size;
			}
			else {
				if ((data = 
				     CreateVData((NhlPointer)
						 &sfp->x_actual_end,
						 sizeof(float),resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_subset_start) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (! sfp->subset_by_index &&
			    sfp->y_subset_start != NULL) {
				if ((data = CopyData(sfp->y_subset_start,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_subset_start->typeQ;
				size = sfp->y_subset_start->size;
			}
			else {
				if ((data = 
				     CreateVData((NhlPointer)
						 &sfp->y_actual_start,
						 sizeof(float),resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qy_subset_end) {
			do_genarray = True;
			ndim = 1;
			dlen[0] = 1;
			if (! sfp->subset_by_index &&
			    sfp->y_subset_end != NULL) {
				if ((data = CopyData(sfp->y_subset_end,resQ))
				    == NULL)
					return NhlFATAL;
				typeQ = sfp->y_subset_end->typeQ;
				size = sfp->y_subset_end->size;
			}
			else {
				if ((data = 
				     CreateVData((NhlPointer)
						 &sfp->y_actual_end,
						 sizeof(float),resQ)) == NULL)
					return NhlFATAL;
				typeQ = Qfloat;
				size = sizeof(float);
			}
                }
                else if (resQ == Qx_index_start) {
			if (sfp->x_index_start > -1)
				ival = sfp->x_index_start;
			else {
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
				ival = sfp->iy_end;
			}
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
#endif
                else if (resQ == Qx_actual_start) {
			fval = sfp->x_actual_start;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qx_actual_end) {
			fval = sfp->x_actual_end;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qxc_el_count) {
                        ival = sfp->xc_el_count;
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_actual_start) {
			fval = sfp->y_actual_start;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qy_actual_end) {
			fval = sfp->y_actual_end;
			*(float*)args[i].value.ptrval = fval;
			*args[i].type_ret = Qfloat;
			*args[i].size_ret = sizeof(float);
			*args[i].free_func = NULL;
                }
                else if (resQ == Qyc_el_count) {
                        ival = sfp->yc_el_count;
			*(int*)args[i].value.ptrval = ival;
			*args[i].type_ret = Qint;
			*args[i].size_ret = sizeof(int);
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
 * Function:	MeshScalarFieldDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlMeshScalarFieldClass.
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
MeshScalarFieldDestroy
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
 *	char			*entry_name = "MeshScalarFieldDestroy";
 */
	NhlMeshScalarFieldLayer	sfl = (NhlMeshScalarFieldLayer)l;
	NhlMeshScalarFieldLayerPart	*sfp = &(sfl->msfield);

	NhlFreeGenArray(sfp->d_arr);
	NhlFreeGenArray(sfp->x_arr);
	NhlFreeGenArray(sfp->y_arr);
	if (sfp->element_nodes)
		NhlFreeGenArray(sfp->element_nodes);
	if (sfp->node_indexes)
		NhlFreeGenArray(sfp->node_indexes);
	if (sfp->x_cell_bounds)
		NhlFreeGenArray(sfp->x_cell_bounds);
	if (sfp->y_cell_bounds)
		NhlFreeGenArray(sfp->y_cell_bounds);
	NhlFreeGenArray(sfp->missing_value);
	NhlFreeGenArray(sfp->data_min);
	NhlFreeGenArray(sfp->data_max);
#if 0
	NhlFreeGenArray(sfp->x_start);
	NhlFreeGenArray(sfp->x_end);
	NhlFreeGenArray(sfp->y_start);
	NhlFreeGenArray(sfp->y_end);
	NhlFreeGenArray(sfp->x_subset_start);
	NhlFreeGenArray(sfp->x_subset_end);
	NhlFreeGenArray(sfp->y_subset_start);
	NhlFreeGenArray(sfp->y_subset_end);
#endif

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
        NhlBoolean	null_ok,
	NhlString	entry_name
)
#else
(ga,copy_ga,resource_name,null_ok,entry_name)
	NhlGenArray	*ga;
	NhlGenArray	copy_ga;
	NhlString	resource_name;
        NhlBoolean	null_ok;
	NhlString	entry_name;
#endif
{
	char		*e_text;
        NhlGenArray	tga;

/*
 * if null_ok and the copy genarray is Null, sets the return genarray to
 * null, after freeing it if necessary. If not null_ok and the copy genarray
 * is null a warning is issued and the return genarray is returned unaltered.
 */
        if (null_ok && copy_ga == NULL) {
                if (*ga != NULL) NhlFreeGenArray(*ga);
                *ga = NULL;
                return NhlNOERROR;
        }
        else if (copy_ga == NULL ||
                 copy_ga->num_elements != 1 || 
                 copy_ga->num_dimensions != 1) {
		e_text = "%s: variable type resource %s invalid";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
                          resource_name);
		return NhlWARNING;
	}

        tga = *ga;
	if ((*ga = _NhlCopyGenArray(copy_ga,True)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		*ga = NULL;
		return NhlFATAL;
	}
	if (tga != NULL) NhlFreeGenArray(tga);

	return NhlNOERROR;
}

