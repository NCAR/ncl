/*
 *      $Id: CoordArrays.c,v 1.8 1994-02-01 18:22:26 boote Exp $
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

/************************************************************************
*									*
*	CoordArrays Class declarations					*
*									*
************************************************************************/

/*
 * Resource Declarations
 */

/*
 * Function:	Resource Default Procedures
 *
 * Description:	These proc's are used to determine if the user/program
 *		set the min,max,missing resources or if the object
 *		should compute them.
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
MissingXSet
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
	NhlLayer	l = (NhlLayer)base;

	if(l->base.layer_class == NhlcoordArraysFloatLayerClass){
		NhlCoordArraysFloatLayer	carrl =
						(NhlCoordArraysFloatLayer)l;

		carrl->carrfloat.missing_x_set = False;
		carrl->carrfloat.missing_x = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArraysIntLayerClass){
		NhlCoordArraysIntLayer	carrl = (NhlCoordArraysIntLayer)l;

		carrl->carrint.missing_x_set = False;
		carrl->carrint.missing_x = 0;
	}
	else
		return NhlFATAL;

	return NhlNOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
MissingYSet
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
	NhlLayer	l = (NhlLayer)base;

	if(l->base.layer_class == NhlcoordArraysFloatLayerClass){
		NhlCoordArraysFloatLayer	carrl = (NhlCoordArraysFloatLayer)l;

		carrl->carrfloat.missing_y_set = False;
		carrl->carrfloat.missing_y = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArraysIntLayerClass){
		NhlCoordArraysIntLayer	carrl = (NhlCoordArraysIntLayer)l;

		carrl->carrint.missing_y_set = False;
		carrl->carrint.missing_y = 0;
	}
	else
		return NhlFATAL;

	return NhlNOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
MaxXSet
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
	NhlLayer	l = (NhlLayer)base;

	if(l->base.layer_class == NhlcoordArraysFloatLayerClass){
		NhlCoordArraysFloatLayer	carrl = (NhlCoordArraysFloatLayer)l;

		carrl->carrfloat.max_x_set = False;
		carrl->carrfloat.max_x = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArraysIntLayerClass){
		NhlCoordArraysIntLayer	carrl = (NhlCoordArraysIntLayer)l;

		carrl->carrint.max_x_set = False;
		carrl->carrint.max_x = 0;
	}
	else
		return NhlFATAL;

	return NhlNOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
MaxYSet
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
	NhlLayer	l = (NhlLayer)base;

	if(l->base.layer_class == NhlcoordArraysFloatLayerClass){
		NhlCoordArraysFloatLayer	carrl = (NhlCoordArraysFloatLayer)l;

		carrl->carrfloat.max_y_set = False;
		carrl->carrfloat.max_y = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArraysIntLayerClass){
		NhlCoordArraysIntLayer	carrl = (NhlCoordArraysIntLayer)l;

		carrl->carrint.max_y_set = False;
		carrl->carrint.max_y = 0;
	}
	else
		return NhlFATAL;

	return NhlNOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
MinXSet
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
	NhlLayer	l = (NhlLayer)base;

	if(l->base.layer_class == NhlcoordArraysFloatLayerClass){
		NhlCoordArraysFloatLayer	carrl = (NhlCoordArraysFloatLayer)l;

		carrl->carrfloat.min_x_set = False;
		carrl->carrfloat.min_x = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArraysIntLayerClass){
		NhlCoordArraysIntLayer	carrl = (NhlCoordArraysIntLayer)l;

		carrl->carrint.min_x_set = False;
		carrl->carrint.min_x = 0;
	}
	else
		return NhlFATAL;

	return NhlNOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
MinYSet
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
	NhlLayer	l = (NhlLayer)base;

	if(l->base.layer_class == NhlcoordArraysFloatLayerClass){
		NhlCoordArraysFloatLayer	carrl = (NhlCoordArraysFloatLayer)l;

		carrl->carrfloat.min_y_set = False;
		carrl->carrfloat.min_y = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArraysIntLayerClass){
		NhlCoordArraysIntLayer	carrl = (NhlCoordArraysIntLayer)l;

		carrl->carrint.min_y_set = False;
		carrl->carrint.min_y = 0;
	}
	else
		return NhlFATAL;

	return NhlNOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
XCastSet
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
	NhlLayer	l = (NhlLayer)base;

	if(l->base.layer_class == NhlcoordArraysFloatLayerClass){
		NhlCoordArraysFloatLayer	carrl = (NhlCoordArraysFloatLayer)l;

		carrl->carrfloat.x_cast_set = False;
		carrl->carrfloat.x_cast = 2;
	}
	else if(l->base.layer_class == NhlcoordArraysIntLayerClass){
		NhlCoordArraysIntLayer	carrl = (NhlCoordArraysIntLayer)l;

		carrl->carrint.x_cast_set = False;
		carrl->carrint.x_cast = 2;
	}
	else
		return NhlFATAL;

	return NhlNOERROR;
}
/*ARGSUSED*/
static NhlErrorTypes
YCastSet
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
	NhlLayer	l = (NhlLayer)base;

	if(l->base.layer_class == NhlcoordArraysFloatLayerClass){
		NhlCoordArraysFloatLayer	carrl = (NhlCoordArraysFloatLayer)l;

		carrl->carrfloat.y_cast_set = False;
		carrl->carrfloat.y_cast = 2;
	}
	else if(l->base.layer_class == NhlcoordArraysIntLayerClass){
		NhlCoordArraysIntLayer	carrl = (NhlCoordArraysIntLayer)l;

		carrl->carrint.y_cast_set = False;
		carrl->carrint.y_cast = 2;
	}
	else
		return NhlFATAL;

	return NhlNOERROR;
}

#define	Oset(field)	NhlOffset(NhlCoordArraysFloatLayerRec,carrfloat.field)
static NhlResource fltresources[] = {
	{NhlNcaXArray,NhlCcaXArray,NhlTGenArray,sizeof(NhlGenArray),
		Oset(xarray),NhlTImmediate,(NhlPointer)NULL},
	{NhlNcaYArray,NhlCcaYArray,NhlTGenArray,sizeof(NhlGenArray),
		Oset(yarray),NhlTImmediate,(NhlPointer)NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(x_cast_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaXCast,NhlCcaXCast,NhlTInteger,sizeof(int),
		Oset(x_cast),NhlTProcedure,(NhlPointer)XCastSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(y_cast_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaYCast,NhlCcaYCast,NhlTInteger,sizeof(int),
		Oset(y_cast),NhlTProcedure,(NhlPointer)YCastSet},
	{NhlNcaCopyArrays,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		Oset(copy_arrays),NhlTImmediate,(NhlPointer)True},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaXMissingF,NhlCdiMissingValue,NhlTFloat,sizeof(float),
		Oset(missing_x),NhlTProcedure,(NhlPointer)MissingXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaYMissingF,NhlCdiMissingValue,NhlTFloat,sizeof(float),
		Oset(missing_y),NhlTProcedure,(NhlPointer)MissingYSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaXMaxF,NhlCcaXMaxF,NhlTFloat,sizeof(float),
		Oset(max_x),NhlTProcedure,(NhlPointer)MaxXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaYMaxF,NhlCcaYMaxF,NhlTFloat,sizeof(float),
		Oset(max_y),NhlTProcedure,(NhlPointer)MaxYSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaXMinF,NhlCcaXMinF,NhlTFloat,sizeof(float),
		Oset(min_x),NhlTProcedure,(NhlPointer)MinXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaYMinF,NhlCcaYMinF,NhlTFloat,sizeof(float),
		Oset(min_y),NhlTProcedure,(NhlPointer)MinYSet}
};
#undef Oset

#define	Oset(field)	NhlOffset(NhlCoordArraysIntLayerRec,carrint.field)
static NhlResource intresources[] = {
	{NhlNcaXArray,NhlCcaXArray,NhlTGenArray,sizeof(NhlGenArray),
		Oset(xarray),NhlTImmediate,(NhlPointer)NULL},
	{NhlNcaYArray,NhlCcaYArray,NhlTGenArray,sizeof(NhlGenArray),
		Oset(yarray),NhlTImmediate,(NhlPointer)NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(x_cast_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaXCast,NhlCcaXCast,NhlTInteger,sizeof(int),
		Oset(x_cast),NhlTProcedure,(NhlPointer)XCastSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(y_cast_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaYCast,NhlCcaYCast,NhlTInteger,sizeof(int),
		Oset(y_cast),NhlTProcedure,(NhlPointer)YCastSet},
	{NhlNcaCopyArrays,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		Oset(copy_arrays),NhlTImmediate,(NhlPointer)True},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaXMissing,NhlCdiMissingValue,NhlTFloat,sizeof(float),
		Oset(missing_x),NhlTProcedure,(NhlPointer)MissingXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaYMissing,NhlCdiMissingValue,NhlTFloat,sizeof(float),
		Oset(missing_y),NhlTProcedure,(NhlPointer)MissingYSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaXMax,NhlCcaXMax,NhlTFloat,sizeof(float),
		Oset(max_x),NhlTProcedure,(NhlPointer)MaxXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaYMax,NhlCcaYMax,NhlTFloat,sizeof(float),
		Oset(max_y),NhlTProcedure,(NhlPointer)MaxYSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaXMin,NhlCcaXMin,NhlTFloat,sizeof(float),
		Oset(min_x),NhlTProcedure,(NhlPointer)MinXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNcaYMin,NhlCcaYMin,NhlTFloat,sizeof(float),
		Oset(min_y),NhlTProcedure,(NhlPointer)MinYSet}
};
#undef Oset

#define	Oset(field)	NhlOffset(NhlCoordArraysLayerRec,carr.field)
static NhlResource resources[] = {
	{NhlNdiType,NhlCdiType,NhlTString,sizeof(NhlString),
		Oset(type_string),NhlTImmediate,(NhlPointer)NULL}
};
#undef Oset

/* base methods */

static NhlErrorTypes CoordArraysClassPartInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc	/* lc to init	*/
#endif
);

static NhlErrorTypes CoordArraysClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes CoordArraysFloatInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArraysIntInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArraysInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
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

static NhlErrorTypes CoordArraysFloatSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArraysIntSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArraysDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

static NhlErrorTypes CoordArraysFloatDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

static NhlErrorTypes CoordArraysIntDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

NhlCoordArraysFloatLayerClassRec NhlcoordArraysFloatLayerClassRec = {
	/* NhlBaseLayerClassPart */
	{
/* class_name			*/	"CoordArraysFloat",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCoordArraysFloatLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlobjLayerClassRec,

/* resources			*/	fltresources,
/* num_resources		*/	NhlNumber(fltresources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	CoordArraysFloatInitialize,
/* layer_set_values		*/	CoordArraysFloatSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	CoordArraysFloatDestroy
	},
	/* NhlCoordArraysFloatLayerPart */
	{
/* foo				*/	0
	}
};

NhlCoordArraysIntLayerClassRec NhlcoordArraysIntLayerClassRec = {
	/* NhlBaseLayerClassPart */
	{
/* class_name			*/	"CoordArraysInt",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCoordArraysIntLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlobjLayerClassRec,

/* resources			*/	intresources,
/* num_resources		*/	NhlNumber(intresources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	CoordArraysIntInitialize,
/* layer_set_values		*/	CoordArraysIntSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	CoordArraysIntDestroy
	},
	/* NhlCoordArraysIntLayerPart */
	{
/* foo				*/	0
	}
};

NhlCoordArraysLayerClassRec NhlcoordArraysLayerClassRec = {
	/* NhlBaseLayerClassPart */
	{
/* class_name			*/	"CoordArrays",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCoordArraysLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhldataItemLayerClassRec,

/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	CoordArraysClassPartInitialize,
/* class_initialize		*/	CoordArraysClassInitialize,
/* layer_initialize		*/	CoordArraysInitialize,
/* layer_set_values		*/	CoordArraysSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	CoordArraysDestroy,

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
	/* NhlCoordArraysLayerClassPart */
	{
/* foo				*/	0
	}
};
	
NhlLayerClass NhlcoordArraysLayerClass = (NhlLayerClass)
					&NhlcoordArraysLayerClassRec;
NhlLayerClass NhlcoordArraysFloatLayerClass = (NhlLayerClass)
					&NhlcoordArraysFloatLayerClassRec;
NhlLayerClass NhlcoordArraysIntLayerClass = (NhlLayerClass)
					&NhlcoordArraysIntLayerClassRec;

static	NrmQuark	floatQ = NrmNULLQUARK;
static	NrmQuark	intQ = NrmNULLQUARK;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

/*
 * Function:	CreateTableFlt
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
static void
CreateTableFlt
#if	__STDC__
(
	NhlString	cast_res,
	NhlString	other_cast_res,
	NhlString	error_lead,
	NhlGenArray	gen,
	NhlGenArray	other_gen,
	int		cast,
	int		other_cast,
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
	int		cast;
	int		other_cast;
	NhlGenArray	*tbl;
	NhlGenArray	*tbl_lens;
#endif
{
	int	vectors, elements;
	int	i,j;
	float	**flttable, *fltvect;
	int	*intvect;

	switch(cast){
		case 1:
			switch(other_cast){
				case 1:
					vectors = 1;
					break;
				case 2:
					vectors = other_gen->len_dimensions[0];
					break;
				case 3:
					vectors = other_gen->len_dimensions[1];
					break;
				default:
					NhlPError(NhlFATAL,NhlEUNKNOWN,
							"%s:Invalid %s value",
						error_lead,other_cast_res);
					return;
			}

			elements = gen->len_dimensions[0];
			break;
		case 2:
			if(gen->num_dimensions == 1){
				vectors = 1;
				elements = gen->len_dimensions[0];
			}
			else{
				vectors = gen->len_dimensions[0];
				elements = gen->len_dimensions[1];
			}
			break;
		case 3:
			vectors = gen->len_dimensions[1];
			elements = gen->len_dimensions[0];
			break;
		default:
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid %s value",
							error_lead,cast_res);
			return;
	}

	flttable = NhlConvertMalloc(sizeof(float*)*vectors);
	intvect = NhlConvertMalloc(sizeof(int)*vectors);
	if((flttable == NULL) || (intvect == NULL)){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return;
	}
	fltvect = gen->data;
	for(i=0;i < vectors; i++){
		intvect[i] = elements;

		switch(cast){
		case 1:
			flttable[i] = fltvect;
			break;
		case 2:
			flttable[i] = fltvect + (i * elements);
			break;
		case 3:
			flttable[i] = NhlConvertMalloc(sizeof(float)*elements);
			if(flttable[i] == NULL){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return;
			}
			for(j=0;j<elements;j++)
				*(flttable[i]+j) = *(fltvect+i+(j*elements));
			break;
		}
	}

	*tbl = NhlCreateGenArray(flttable,NhlTPointer,sizeof(NhlPointer),1,
								&vectors);
	*tbl_lens = NhlCreateGenArray(intvect,NhlTInteger,sizeof(int),1,
								&vectors);
	return;
}

/*
 * Function:	CreateTableInt
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
static void
CreateTableInt
#if	__STDC__
(
	NhlString	cast_res,
	NhlString	other_cast_res,
	NhlString	error_lead,
	NhlGenArray	gen,
	NhlGenArray	other_gen,
	int		cast,
	int		other_cast,
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
	int		cast;
	int		other_cast;
	NhlGenArray	*tbl;
	NhlGenArray	*tbl_lens;
#endif
{
	int	vectors, elements;
	int	i,j;
	float	**flttable;
	int	*intarr;
	int	*intvect;

	switch(cast){
		case 1:
			switch(other_cast){
				case 1:
					vectors = 1;
					break;
				case 2:
					vectors = other_gen->len_dimensions[0];
					break;
				case 3:
					vectors = other_gen->len_dimensions[1];
					break;
				default:
					NhlPError(NhlFATAL,NhlEUNKNOWN,
							"%s:Invalid %s value",
						error_lead,other_cast_res);
					return;
			}

			elements = gen->len_dimensions[0];
			break;
		case 2:
			if(gen->num_dimensions == 1){
				vectors = 1;
				elements = gen->len_dimensions[0];
			}
			else{
				vectors = gen->len_dimensions[0];
				elements = gen->len_dimensions[1];
			}
			break;
		case 3:
			vectors = gen->len_dimensions[1];
			elements = gen->len_dimensions[0];
			break;
		default:
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid %s value",
							error_lead,cast_res);
			return;
	}

	flttable = NhlConvertMalloc(sizeof(float*)*vectors);
	intvect = NhlConvertMalloc(sizeof(int)*vectors);
	if((flttable == NULL) || (intvect == NULL)){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return;
	}
	intarr = gen->data;
	for(i=0;i < vectors; i++){
		flttable[i] = NhlConvertMalloc(sizeof(float)*elements);
		if(flttable[i] == NULL){
			NhlPError(NhlFATAL,ENOMEM,NULL);
			return;
		}
		intvect[i] = elements;
		switch(cast){
			case 1:
				for(j=0;j<elements;j++)
					*(flttable[i]+j) = (float)*(intarr+j);
				break;
			case 2:
				for(j=0;j<elements;j++)
					*(flttable[i]+j) =
						(float)*(intarr+(i*elements)+j);
				break;
			case 3:
				for(j=0;j<elements;j++)
					*(flttable[i]+j) =
						(float)*(intarr+i+(j*elements));
				break;
			default:
				return;
		}
	}

	*tbl = NhlCreateGenArray(flttable,NhlTPointer,sizeof(NhlPointer),1,
								&vectors);
	*tbl_lens = NhlCreateGenArray(intvect,NhlTInteger,sizeof(int),1,
								&vectors);
	return;
}

/*
 * Function:	CvtCArraysObjToFloatObj
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
CvtCArraysObjToFloatObj
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
	char			*error_lead="CvtCArraysObjToFloatObj";
	NhlErrorTypes		ret = NhlNOERROR;
	NhlCoordArraysLayer	carrl = NULL;
	NhlSArg			sargs[30];
	int			nargs=0;
	NhlGenArray		xtbl = NULL, ytbl = NULL;
	NhlGenArray		xtbl_lens = NULL, ytbl_lens = NULL;

	if(num_args != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Called w/wrong args",
								error_lead);
		return NhlFATAL;
	}

	carrl = (NhlCoordArraysLayer)_NhlGetLayer(*(int*)(from->addr));
	if((carrl == NULL)||
			(carrl->base.layer_class != NhlcoordArraysLayerClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called w/ improper \"from\" object",error_lead);
		return NhlFATAL;
	}

	if(carrl->carr.type == floatQ){
		NhlCoordArraysFloatLayer	l =
				(NhlCoordArraysFloatLayer)carrl->carr.child;
		NhlCoordArraysFloatLayerPart	*child =
				(NhlCoordArraysFloatLayerPart*)&l->carrfloat;

		NhlSetSArg(&sargs[nargs++],NhlNctXMaxF,child->max_x);
		NhlSetSArg(&sargs[nargs++],NhlNctYMaxF,child->max_y);
		NhlSetSArg(&sargs[nargs++],NhlNctXMinF,child->min_x);
		NhlSetSArg(&sargs[nargs++],NhlNctYMinF,child->min_y);

		if(child->missing_x_set)
			NhlSetSArg(&sargs[nargs++],NhlNctXMissingF,
							child->missing_x);
		if(child->missing_y_set)
			NhlSetSArg(&sargs[nargs++],NhlNctYMissingF,
							child->missing_y);

		if(child->xarray != NULL){
			CreateTableFlt(NhlNcaXCast,NhlNcaYCast,error_lead,
				child->xarray,child->yarray,child->x_cast,
						child->y_cast,&xtbl,&xtbl_lens);
			if((xtbl == NULL) || (xtbl_lens == NULL)){
				return NhlFATAL;
			}
			NhlSetSArg(&sargs[nargs++],NhlNctXTable,xtbl);
			NhlSetSArg(&sargs[nargs++],NhlNctXTableLengths,
								xtbl_lens);
		}
		if(child->yarray != NULL){
			CreateTableFlt(NhlNcaYCast,NhlNcaXCast,error_lead,
				child->yarray,child->xarray,child->y_cast,
						child->x_cast,&ytbl,&ytbl_lens);
			if((ytbl == NULL) || (ytbl_lens == NULL)){
				return NhlFATAL;
			}
			NhlSetSArg(&sargs[nargs++],NhlNctYTable,ytbl);
			NhlSetSArg(&sargs[nargs++],NhlNctYTableLengths,
								ytbl_lens);
		}
	}
	else if(carrl->carr.type == intQ){
		NhlCoordArraysIntLayer	l =
				(NhlCoordArraysIntLayer)carrl->carr.child;
		NhlCoordArraysIntLayerPart	*child =
				(NhlCoordArraysIntLayerPart*)&l->carrint;

		NhlSetSArg(&sargs[nargs++],NhlNctXMaxF,child->max_x);
		NhlSetSArg(&sargs[nargs++],NhlNctYMaxF,child->max_y);
		NhlSetSArg(&sargs[nargs++],NhlNctXMinF,child->min_x);
		NhlSetSArg(&sargs[nargs++],NhlNctYMinF,child->min_y);

		if(child->missing_x_set)
			NhlSetSArg(&sargs[nargs++],NhlNctXMissingF,
							child->missing_x);
		if(child->missing_y_set)
			NhlSetSArg(&sargs[nargs++],NhlNctYMissingF,
							child->missing_y);

		if(child->xarray != NULL){
			CreateTableInt(NhlNcaXCast,NhlNcaYCast,error_lead,
				child->xarray,child->yarray,child->x_cast,
						child->y_cast,&xtbl,&xtbl_lens);
			if((xtbl == NULL) || (xtbl_lens == NULL)){
				return NhlFATAL;
			}
			NhlSetSArg(&sargs[nargs++],NhlNctXTable,xtbl);
			NhlSetSArg(&sargs[nargs++],NhlNctXTableLengths,
								xtbl_lens);
		}
		if(child->yarray != NULL){
			CreateTableInt(NhlNcaYCast,NhlNcaXCast,error_lead,
				child->yarray,child->xarray,child->y_cast,
						child->x_cast,&ytbl,&ytbl_lens);
			if((ytbl == NULL) || (ytbl_lens == NULL)){
				return NhlFATAL;
			}
			NhlSetSArg(&sargs[nargs++],NhlNctYTable,ytbl);
			NhlSetSArg(&sargs[nargs++],NhlNctYTableLengths,
								ytbl_lens);
		}
	}
	else{
		return NhlFATAL;
	}


	NhlSetSArg(&sargs[nargs++],NhlNctCopyTables,False);

	ret = NhlALCreate((int*)to->addr,"no.name",
		NhlcoordArrTableFloatLayerClass,carrl->base.id,sargs,nargs);

	NhlFreeGenArray(xtbl);
	NhlFreeGenArray(xtbl_lens);
	NhlFreeGenArray(ytbl);
	NhlFreeGenArray(ytbl_lens);

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
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;

	floatQ = NrmStringToQuark(NhlTFloat);
	intQ = NrmStringToQuark(NhlTInteger);

	ret = NhlRegisterConverter(
			NhlcoordArraysLayerClass->base_class.class_name,
			NhlcoordArrTableFloatLayerClass->base_class.class_name,
			CvtCArraysObjToFloatObj,NULL,0,False,NULL);

	return ret;
}

/*
 * Function:	CoordArraysClassPartInitialize
 *
 * Description:	This function is used to init the carr_class part of the layer
 *		class record of this class and of all sub-classes.
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
CoordArraysClassPartInitialize
#if	__STDC__
(
	NhlLayerClass	lc	/* pointer to class structure	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* pointer to class structure	*/
#endif
{
	NhlErrorTypes		ret, lret;

	ret = _NhlRegisterChildClass(lc,NhlcoordArraysFloatLayerClass,True,
								False,NULL);
	lret = _NhlRegisterChildClass(lc,NhlcoordArraysIntLayerClass,True,
								False,NULL);
	return MIN(lret,ret);
}

#define	COPY_ARRAY(type,dim)\
{									\
	if(ncarr->carr##type.dim##array != NULL){			\
		ncarr->carr##type.dim##array =				\
			_NhlCopyGenArray(ncarr->carr##type.dim##array,	\
					ncarr->carr##type.copy_arrays);	\
									\
		if(ncarr->carr##type.dim##array == NULL){		\
			NhlPError(NhlFATAL,ENOMEM,NULL);		\
			return NhlFATAL;				\
		}							\
	}								\
}

#define CHECK_ARRAY(TYPE,dim,DIM)\
{									\
	NhlGenArray		gen;					\
	NhlCoordArraysLayer	cal = (NhlCoordArraysLayer)ncarr->base.parent;\
									\
	if(ncarr->carr##TYPE.dim##array != NULL){			\
		gen = ncarr->carr##TYPE.dim##array;			\
		if((gen->num_dimensions > 2) ||				\
				(gen->num_dimensions < 1) ||		\
				(cal->carr.type != gen->typeQ)){	\
			NhlPError(NhlWARNING,NhlEUNKNOWN,			\
		"%s:%s must be a one or two dimensional %s array:ignoring",\
				error_lead,NhlNca##DIM##Array,#TYPE);	\
			inv##dim = True;				\
		}							\
	}								\
	else								\
		imp##dim = True;					\
}

#define FREE_ARRAY(type,dim,pre)\
{									\
	NhlFreeGenArray(pre##carr->carr##type.dim##array);		\
}

#define	CHECK_MINMAX(type,dim,otherdim)\
{									\
	if(!ncarr->carr##type.max_##dim##_set ||			\
				!ncarr->carr##type.min_##dim##_set){	\
		NhlBoolean	initminmax = False;			\
		type		*vals,max=(type)0,min=(type)0;		\
									\
		if(ncarr->carr##type.dim##array != NULL){		\
			int i, num;					\
									\
			vals=(type*)ncarr->carr##type.dim##array->data;	\
									\
			if(ncarr->carr##type.dim##_cast == 1)		\
				num =					\
			ncarr->carr##type.dim##array->len_dimensions[0];\
			else						\
				num =					\
			ncarr->carr##type.dim##array->num_elements;	\
									\
			for(i=0;i < num;i++,vals++){			\
			if((ncarr->carr##type.missing_##dim##_set) &&	\
			(*vals == ncarr->carr##type.missing_##dim))	\
							continue;	\
									\
						if(initminmax){		\
						max = MAX(*vals,max);	\
						min = MIN(*vals,min);	\
						}			\
						else{			\
						max = *vals;		\
						min = *vals;		\
						initminmax=True;	\
						}			\
			}						\
		}							\
		else{							\
			min = (type)1.0;				\
			if((ncarr->carr##type.otherdim##_cast == 2) &&	\
		(ncarr->carr##type.otherdim##array->num_dimensions == 2))\
max = (type)ncarr->carr##type.otherdim##array->len_dimensions[1];	\
			else						\
max = (type)ncarr->carr##type.otherdim##array->len_dimensions[0];	\
		}							\
									\
		if(!ncarr->carr##type.max_##dim##_set)			\
			ncarr->carr##type.max_##dim = max;		\
		if(!ncarr->carr##type.min_##dim##_set)			\
			ncarr->carr##type.min_##dim = min;		\
	}								\
}

#define	CHECK_CAST(type,dim,DIM)\
{									\
	if(ncarr->carr##type.dim##_cast_set){				\
		if((imp##dim) && (ncarr->carr##type.dim##_cast != 1)){	\
			NhlPError(NhlWARNING,NhlEUNKNOWN,		\
				"%s:%s must be one if %s is implied",	\
					error_lead,NhlNca##DIM##Cast,	\
						NhlNca##DIM##Array);	\
			ret = MIN(ret,NhlWARNING);			\
			ncarr->carr##type.dim##_cast = 1;		\
		}							\
	}								\
	else{								\
		ncarr->carr##type.dim##_cast_set = True;		\
		if((imp##dim) ||					\
		(ncarr->carr##type.dim##array->num_dimensions == 1))	\
			ncarr->carr##type.dim##_cast = 1;		\
		else							\
			ncarr->carr##type.dim##_cast = 2;		\
	}								\
}

#define	INIT_ARRAY(type,dim,DIM)\
{									\
	NhlBoolean	inv##dim = False;				\
	int		num_elements;					\
									\
	CHECK_ARRAY(type,dim,DIM)					\
									\
	if(inv##dim){							\
		NhlPError(NhlFATAL,NhlEUNKNOWN,				\
		"%s:Resources specifying %s dimension are invalid",	\
						error_lead,#DIM);	\
		return NhlFATAL;					\
	}								\
									\
	CHECK_CAST(type,dim,DIM)					\
									\
									\
	if(!imp##dim){							\
		if((ncarr->carr##type.dim##array->num_dimensions == 2) &&\
				(ncarr->carr##type.dim##_cast == 2)){	\
			num_elements =					\
			ncarr->carr##type.dim##array->len_dimensions[1];\
		}							\
		else{							\
			num_elements =					\
			ncarr->carr##type.dim##array->len_dimensions[0];\
		}							\
									\
		if(num_elements < 2){					\
			NhlPError(NhlFATAL,NhlEUNKNOWN,			\
	"%s:Each vector in the %s array must have at least 2 elements",	\
					error_lead,NhlNca##DIM##Array);	\
			return NhlFATAL;				\
		}							\
		COPY_ARRAY(type,dim)					\
	}								\
	else{								\
		ncarr->carr##type.dim##array = NULL;			\
	}								\
}



#define INIT_FUNC(name,type)\
{									\
	char			*error_lead = #name "Initialize";	\
	Nhl##name##Layer	ncarr = (Nhl##name##Layer)new;		\
	NhlBoolean		impy = False, impx = False;		\
	NhlErrorTypes		ret = NhlNOERROR;			\
									\
	/*								\
	 * insure accuracy, and copy Array				\
	 */								\
	INIT_ARRAY(type,y,Y)						\
	INIT_ARRAY(type,x,X)						\
									\
	if(impx && impy){						\
		NhlPError(NhlFATAL,NhlEUNKNOWN,				\
		"%s:Cannot have Implied X and Y values",error_lead);	\
		return NhlFATAL;					\
	}								\
									\
	/*								\
	 * Set Max's and Min's						\
	 */								\
	CHECK_MINMAX(type,x,y)						\
	CHECK_MINMAX(type,y,x)						\
									\
	return ret;							\
}

/*
 * Function:	CoordArraysFloatInitialize
 *
 * Description:	This function initializes an instance of a CoordArraysFloat
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
CoordArraysFloatInitialize
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
INIT_FUNC(CoordArraysFloat,float)

/*
 * Function:	CoordArraysIntInitialize
 *
 * Description:	This function initializes an instance of a CoordArraysInt
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
CoordArraysIntInitialize
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
INIT_FUNC(CoordArraysInt,int)

/*
 * Function:	CoordArraysInitialize
 *
 * Description:	This function initializes an instance of a CoordArrays
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
CoordArraysInitialize
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
	char			*error_lead = "CoordArraysInitialize";
	NhlCoordArraysLayer	ncarr = (NhlCoordArraysLayer)new;
	NhlLayerClass		child_class=NULL;
	int			tchild;
	char			name[_NhlMAXRESNAMLEN];
	NhlErrorTypes		ret;

	if(ncarr->carr.type_string == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s:The %s resource must be specified to create a %s object",
				error_lead,NhlNdiType,_NhlClassName(lc));

		return NhlFATAL;
	}

	ncarr->carr.type = NrmStringToQuark(ncarr->carr.type_string);
	/*
	 * Point to perminate memory in Quarks.c
	 */
	ncarr->carr.type_string = NrmQuarkToString(ncarr->carr.type);

	/*
	 * Create the correct child...
	 */
	strcpy(name,ncarr->base.name);
	if(ncarr->carr.type == floatQ){
		child_class = NhlcoordArraysFloatLayerClass;
		strcat(name,".FLT");
	}
	else if(ncarr->carr.type == intQ){
		child_class = NhlcoordArraysIntLayerClass;
		strcat(name,".INT");
	}

	ret = _NhlCreateChild(&tchild,name,child_class,new,NULL);

	ncarr->carr.child = _NhlGetLayer(tchild);

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
	char			*error_lead = "CoordArraysSetValues";
	NhlErrorTypes		ret = NhlNOERROR;
	NhlCoordArraysLayer	ncarr = (NhlCoordArraysLayer)new;
	NhlCoordArraysLayer	ocarr = (NhlCoordArraysLayer)old;

	if(ncarr->carr.type_string != ocarr->carr.type_string){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is setable only at create time - ignoring!",
							error_lead,NhlNdiType);
		ncarr->carr.type_string = ocarr->carr.type_string;
		ret = NhlWARNING;
	}

	return	ret;
}

#define	SET_ARRAY(type,dim,DIM)\
{									\
	/* only do stuff if one of the fields changed */		\
	if(ncarr->carr##type.dim##array !=				\
					ocarr->carr##type.dim##array){	\
									\
		NhlBoolean	inv##dim = False;			\
									\
		CHECK_ARRAY(type,dim,DIM)				\
									\
		if(inv##dim){						\
			NhlPError(NhlWARNING,NhlEUNKNOWN,		\
				"%s:invalid %s dimension: resetting %s",\
				error_lead,#DIM,NhlNca##DIM##Array);	\
									\
			ncarr->carr##type.dim##array =			\
					ocarr->carr##type.dim##array;	\
									\
			if(ncarr->carr##type.dim##array == NULL)	\
				imp##dim = True;			\
		}							\
	}								\
}

#define	FINISH_ARRAY(type,dim,DIM)\
{									\
	if(ncarr->carr##type.dim##array!=ocarr->carr##type.dim##array){	\
		status = True;						\
		COPY_ARRAY(type,dim)					\
		FREE_ARRAY(type,dim,o)					\
	}								\
									\
	/*								\
	 * if copy_arrays is True, but my_data is False -		\
	 * need to copy the array.					\
	 */								\
	if((ncarr->carr##type.copy_arrays) &&				\
			!ncarr->carr##type.dim##array->my_data){	\
		NhlGenArray	tgen = ncarr->carr##type.dim##array;	\
									\
		status = True;						\
		COPY_ARRAY(type,dim)					\
		NhlFreeGenArray(tgen);					\
	}								\
}

#define	SETVAL_FUNC(name,type)\
{									\
	char			*error_lead = #name "SetValues";	\
	Nhl##name##Layer	ncarr = (Nhl##name##Layer)new;		\
	Nhl##name##Layer	ocarr = (Nhl##name##Layer)old;		\
	NhlBoolean		impx = False, impy = False;		\
	NhlBoolean		status = False;				\
	NhlErrorTypes		ret = NhlNOERROR;			\
									\
	SET_ARRAY(type,x,X)						\
	SET_ARRAY(type,x,X)						\
									\
	if(impx && impy){						\
		NhlPError(NhlWARNING,NhlEUNKNOWN,			\
		"%s:Cannot have Implied X and Y values:resetting",	\
							error_lead);	\
		ret = MIN(ret,NhlWARNING);				\
		ncarr->carr##type.xarray = ocarr->carr##type.xarray;	\
		ncarr->carr##type.yarray = ocarr->carr##type.yarray;	\
	}								\
									\
	FINISH_ARRAY(type,y,Y)						\
	FINISH_ARRAY(type,x,X)						\
									\
	CHECK_CAST(type,y,Y)						\
	CHECK_CAST(type,x,X)						\
									\
	if(ncarr->carr##type.x_cast != ocarr->carr##type.x_cast)	\
		status = True;						\
	if(ncarr->carr##type.y_cast != ocarr->carr##type.y_cast)	\
		status = True;						\
									\
	if(ncarr->carr##type.missing_x != ocarr->carr##type.missing_x){	\
		ncarr->carr##type.missing_x_set = True;			\
		status = True;						\
	}								\
	if(ncarr->carr##type.missing_y != ocarr->carr##type.missing_y){	\
		ncarr->carr##type.missing_y_set = True;			\
		status = True;						\
	}								\
									\
	if(ncarr->carr##type.max_x != ocarr->carr##type.max_x)		\
		ncarr->carr##type.max_x_set = True;			\
	if(ncarr->carr##type.min_x != ocarr->carr##type.min_x)		\
		ncarr->carr##type.min_x_set = True;			\
	if(ncarr->carr##type.max_y != ocarr->carr##type.max_y)		\
		ncarr->carr##type.max_y_set = True;			\
	if(ncarr->carr##type.min_y != ocarr->carr##type.min_y)		\
		ncarr->carr##type.min_y_set = True;			\
									\
	/*								\
	 * Set Max's and Min's						\
	 */								\
	CHECK_MINMAX(type,x,y)						\
	CHECK_MINMAX(type,y,x)						\
									\
	if(ncarr->carr##type.max_x != ocarr->carr##type.max_x)		\
		status = True;						\
	if(ncarr->carr##type.min_x != ocarr->carr##type.min_x)		\
		status = True;						\
	if(ncarr->carr##type.max_y != ocarr->carr##type.max_y)		\
		status = True;						\
	if(ncarr->carr##type.min_y != ocarr->carr##type.min_y)		\
		status = True;						\
									\
	_NhlDataChanged((NhlDataItemLayer)new->base.parent,status);	\
									\
	return	ret;							\
}

/*
 * Function:	CoordArraysFloatSetValues
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
CoordArraysFloatSetValues
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
SETVAL_FUNC(CoordArraysFloat,float)

/*
 * Function:	CoordArraysIntSetValues
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
CoordArraysIntSetValues
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
SETVAL_FUNC(CoordArraysInt,int)

/*
 * Function:	CoordArraysDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlCoordArraysLayerClass.
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
CoordArraysDestroy
#if	__STDC__
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlCoordArraysLayer	carrl = (NhlCoordArraysLayer)l;

	/*
	 * Don't free type_string - it points into Quarks.c's data
	 */

	if(carrl->carr.child != NULL)
		return _NhlDestroyChild(carrl->carr.child->base.id,l);

	return NhlNOERROR;
}

#define DESTROY_FUNC(name,type)\
{									\
	Nhl##name##Layer	ncarr = (Nhl##name##Layer)l;		\
									\
	FREE_ARRAY(type,x,n)						\
	FREE_ARRAY(type,y,n)						\
									\
	return NhlNOERROR;						\
}		

/*
 * Function:	CoordArraysFloatDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlCoordArraysFloatLayerClass.
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
CoordArraysFloatDestroy
#if	__STDC__
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
DESTROY_FUNC(CoordArraysFloat,float)

/*
 * Function:	CoordArraysIntDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlCoordArraysIntLayerClass.
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
CoordArraysIntDestroy
#if	__STDC__
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
DESTROY_FUNC(CoordArraysInt,int)

#undef	DESTROY_FUNC

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
