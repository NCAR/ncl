/*
 *      $Id: CoordArrTable.c,v 1.6 1994-01-14 23:36:02 boote Exp $
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
	Layer	l = (Layer)base;

	if(l->base.layer_class == coordArrTableFloatLayerClass){
		CoordArrTableFloatLayer	catl = (CoordArrTableFloatLayer)l;

		catl->catfloat.missing_x_set = False;
		catl->catfloat.missing_x = 0.0;
	}
	else if(l->base.layer_class == coordArrTableIntLayerClass){
		CoordArrTableIntLayer	catl = (CoordArrTableIntLayer)l;

		catl->catint.missing_x_set = False;
		catl->catint.missing_x = 0;
	}
	else
		return FATAL;

	return NOERROR;
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
	Layer	l = (Layer)base;

	if(l->base.layer_class == coordArrTableFloatLayerClass){
		CoordArrTableFloatLayer	catl = (CoordArrTableFloatLayer)l;

		catl->catfloat.missing_y_set = False;
		catl->catfloat.missing_y = 0.0;
	}
	else if(l->base.layer_class == coordArrTableIntLayerClass){
		CoordArrTableIntLayer	catl = (CoordArrTableIntLayer)l;

		catl->catint.missing_y_set = False;
		catl->catint.missing_y = 0;
	}
	else
		return FATAL;

	return NOERROR;
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
	Layer	l = (Layer)base;

	if(l->base.layer_class == coordArrTableFloatLayerClass){
		CoordArrTableFloatLayer	catl = (CoordArrTableFloatLayer)l;

		catl->catfloat.max_x_set = False;
		catl->catfloat.max_x = 0.0;
	}
	else if(l->base.layer_class == coordArrTableIntLayerClass){
		CoordArrTableIntLayer	catl = (CoordArrTableIntLayer)l;

		catl->catint.max_x_set = False;
		catl->catint.max_x = 0;
	}
	else
		return FATAL;

	return NOERROR;
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
	Layer	l = (Layer)base;

	if(l->base.layer_class == coordArrTableFloatLayerClass){
		CoordArrTableFloatLayer	catl = (CoordArrTableFloatLayer)l;

		catl->catfloat.max_y_set = False;
		catl->catfloat.max_y = 0.0;
	}
	else if(l->base.layer_class == coordArrTableIntLayerClass){
		CoordArrTableIntLayer	catl = (CoordArrTableIntLayer)l;

		catl->catint.max_y_set = False;
		catl->catint.max_y = 0;
	}
	else
		return FATAL;

	return NOERROR;
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
	Layer	l = (Layer)base;

	if(l->base.layer_class == coordArrTableFloatLayerClass){
		CoordArrTableFloatLayer	catl = (CoordArrTableFloatLayer)l;

		catl->catfloat.min_x_set = False;
		catl->catfloat.min_x = 0.0;
	}
	else if(l->base.layer_class == coordArrTableIntLayerClass){
		CoordArrTableIntLayer	catl = (CoordArrTableIntLayer)l;

		catl->catint.min_x_set = False;
		catl->catint.min_x = 0;
	}
	else
		return FATAL;

	return NOERROR;
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
	Layer	l = (Layer)base;

	if(l->base.layer_class == coordArrTableFloatLayerClass){
		CoordArrTableFloatLayer	catl = (CoordArrTableFloatLayer)l;

		catl->catfloat.min_y_set = False;
		catl->catfloat.min_y = 0.0;
	}
	else if(l->base.layer_class == coordArrTableIntLayerClass){
		CoordArrTableIntLayer	catl = (CoordArrTableIntLayer)l;

		catl->catint.min_y_set = False;
		catl->catint.min_y = 0;
	}
	else
		return FATAL;

	return NOERROR;
}

#define	Oset(field)	NhlOffset(CoordArrTableFloatLayerRec,catfloat.field)
static NhlResource fltresources[] = {
	{NhlNctXTable,NhlCctXTable,NhlTGenArray,sizeof(NhlGenArray),
		Oset(xtable),NhlTImmediate,(NhlPointer)NULL},
	{NhlNctYTable,NhlCctYTable,NhlTGenArray,sizeof(NhlGenArray),
		Oset(ytable),NhlTImmediate,(NhlPointer)NULL},
	{NhlNctXTableLengths,NhlCctXTableLengths,NhlTGenArray,
		sizeof(NhlGenArray),
		Oset(xtable_lens),NhlTImmediate,(NhlPointer)NULL},
	{NhlNctYTableLengths,NhlCctYTableLengths,NhlTGenArray,
		sizeof(NhlGenArray),
		Oset(ytable_lens),NhlTImmediate,(NhlPointer)NULL},
	{NhlNctCopyTables,NhlCctCopyTables,NhlTBoolean,sizeof(NhlBoolean),
		Oset(copy_tables),NhlTImmediate,(NhlPointer)True},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctXMissingF,NhlCctXMissingF,NhlTFloat,sizeof(float),
		Oset(missing_x),NhlTProcedure,(NhlPointer)MissingXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctYMissingF,NhlCctYMissingF,NhlTFloat,sizeof(float),
		Oset(missing_y),NhlTProcedure,(NhlPointer)MissingYSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctXMaxF,NhlCctXMaxF,NhlTFloat,sizeof(float),
		Oset(max_x),NhlTProcedure,(NhlPointer)MaxXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctYMaxF,NhlCctYMaxF,NhlTFloat,sizeof(float),
		Oset(max_y),NhlTProcedure,(NhlPointer)MaxYSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctXMinF,NhlCctXMinF,NhlTFloat,sizeof(float),
		Oset(min_x),NhlTProcedure,(NhlPointer)MinXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctYMinF,NhlCctYMinF,NhlTFloat,sizeof(float),
		Oset(min_y),NhlTProcedure,(NhlPointer)MinYSet},
	/* use reslist to init private "own" fields */
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(own_x),NhlTImmediate,(NhlPointer)False},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(own_y),NhlTImmediate,(NhlPointer)False}
};
#undef Oset

#define	Oset(field)	NhlOffset(CoordArrTableIntLayerRec,catint.field)
static NhlResource intresources[] = {
	{NhlNctXTable,NhlCctXTable,NhlTGenArray,sizeof(NhlGenArray),
		Oset(xtable),NhlTImmediate,(NhlPointer)NULL},
	{NhlNctYTable,NhlCctYTable,NhlTGenArray,sizeof(NhlGenArray),
		Oset(ytable),NhlTImmediate,(NhlPointer)NULL},
	{NhlNctXTableLengths,NhlCctXTableLengths,NhlTGenArray,
		sizeof(NhlGenArray),
		Oset(xtable_lens),NhlTImmediate,(NhlPointer)NULL},
	{NhlNctYTableLengths,NhlCctYTableLengths,NhlTGenArray,
		sizeof(NhlGenArray),
		Oset(ytable_lens),NhlTImmediate,(NhlPointer)NULL},
	{NhlNctCopyTables,NhlCctCopyTables,NhlTBoolean,sizeof(NhlBoolean),
		Oset(copy_tables),NhlTImmediate,(NhlPointer)True},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctXMissing,NhlCctXMissing,NhlTFloat,sizeof(float),
		Oset(missing_x),NhlTProcedure,(NhlPointer)MissingXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctYMissing,NhlCctYMissing,NhlTFloat,sizeof(float),
		Oset(missing_y),NhlTProcedure,(NhlPointer)MissingYSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctXMax,NhlCctXMax,NhlTFloat,sizeof(float),
		Oset(max_x),NhlTProcedure,(NhlPointer)MaxXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctYMax,NhlCctYMax,NhlTFloat,sizeof(float),
		Oset(max_y),NhlTProcedure,(NhlPointer)MaxYSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_x_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctXMin,NhlCctXMin,NhlTFloat,sizeof(float),
		Oset(min_x),NhlTProcedure,(NhlPointer)MinXSet},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_y_set),NhlTImmediate,(NhlPointer)True},
	{NhlNctYMin,NhlCctYMin,NhlTFloat,sizeof(float),
		Oset(min_y),NhlTProcedure,(NhlPointer)MinYSet},
	/* use reslist to init private "own" fields */
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(own_x),NhlTImmediate,(NhlPointer)False},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(own_y),NhlTImmediate,(NhlPointer)False}
};
#undef Oset

#define	Oset(field)	NhlOffset(CoordArrTableLayerRec,cat.field)
static NhlResource resources[] = {
	{NhlNdiType,NhlCdiType,NhlTString,sizeof(NhlString),
		Oset(type_string),NhlTImmediate,(NhlPointer)NULL}
};
#undef Oset

/* base methods */

static NhlErrorTypes CoordArrTableClassPartInitialize(
#if	NhlNeedProto
	LayerClass	lc	/* lc to init	*/
#endif
);

static NhlErrorTypes CoordArrTableClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes CoordArrTableFloatInitialize(
#if	NhlNeedProto
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableIntInitialize(
#if	NhlNeedProto
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableInitialize(
#if	NhlNeedProto
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableSetValues(
#if	NhlNeedProto
	Layer		old,		/* old		*/
	Layer		req,		/* requested	*/
	Layer		new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableFloatSetValues(
#if	NhlNeedProto
	Layer		old,		/* old		*/
	Layer		req,		/* requested	*/
	Layer		new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableIntSetValues(
#if	NhlNeedProto
	Layer		old,		/* old		*/
	Layer		req,		/* requested	*/
	Layer		new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableDestroy(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

static NhlErrorTypes CoordArrTableFloatDestroy(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

static NhlErrorTypes CoordArrTableIntDestroy(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

CoordArrTableFloatLayerClassRec coordArrTableFloatLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* class_name			*/	"CoordArrTableFloat",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(CoordArrTableFloatLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&objLayerClassRec,

/* resources			*/	fltresources,
/* num_resources		*/	NhlNumber(fltresources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	CoordArrTableFloatInitialize,
/* layer_set_values		*/	CoordArrTableFloatSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	CoordArrTableFloatDestroy
	},
	/* CoordArrTableFloatLayerPart */
	{
/* foo				*/	0
	}
};

CoordArrTableIntLayerClassRec coordArrTableIntLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* class_name			*/	"CoordArrTableInt",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(CoordArrTableIntLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&objLayerClassRec,

/* resources			*/	intresources,
/* num_resources		*/	NhlNumber(intresources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	CoordArrTableIntInitialize,
/* layer_set_values		*/	CoordArrTableIntSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	CoordArrTableIntDestroy
	},
	/* CoordArrTableIntLayerPart */
	{
/* foo				*/	0
	}
};

CoordArrTableLayerClassRec coordArrTableLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* class_name			*/	"CoordArrTable",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(CoordArrTableLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&dataItemLayerClassRec,

/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	CoordArrTableClassPartInitialize,
/* class_initialize		*/	CoordArrTableClassInitialize,
/* layer_initialize		*/	CoordArrTableInitialize,
/* layer_set_values		*/	CoordArrTableSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	CoordArrTableDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	/* DataItemLayerClassPart */
	{
/* foo				*/	0
	},
	/* CoordArrTableLayerClassPart */
	{
/* foo				*/	0
	}
};
	
LayerClass coordArrTableLayerClass = (LayerClass)&coordArrTableLayerClassRec;
LayerClass coordArrTableFloatLayerClass = (LayerClass)&coordArrTableFloatLayerClassRec;
LayerClass coordArrTableIntLayerClass = (LayerClass)&coordArrTableIntLayerClassRec;

static	NrmQuark	floatQ = NrmNULLQUARK;
static	NrmQuark	intQ = NrmNULLQUARK;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

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
	CoordArrTableLayer	catl = NULL;
	NhlSArg			sargs[30];
	int			nargs=0,i,j,*lens;
	float			**flttable,*fltvect;
	NhlGenArray		xtbl = NULL, ytbl = NULL;
	NhlErrorTypes		ret = NOERROR;

	if(num_args != 0){
		NhlPError(FATAL,E_UNKNOWN,
				"CvtGenObjToFloatObj:Called w/wrong args");
		return FATAL;
	}

	catl = (CoordArrTableLayer)_NhlGetLayer(*(int*)(from->addr));
	if((catl == NULL)||(catl->base.layer_class != coordArrTableLayerClass)){
		NhlPError(FATAL,E_UNKNOWN,
		"CvtGenObjToFloatObj:Called w/ improper \"from\" object");
		return FATAL;
	}

	if(catl->cat.type == floatQ){
		CoordArrTableFloatLayer	l =
				(CoordArrTableFloatLayer)catl->cat.child;
		CoordArrTableFloatLayerPart	*child =
				(CoordArrTableFloatLayerPart*)&l->catfloat;

		NhlSetSArg(&sargs[nargs++],NhlNctXTable,child->xtable);
		NhlSetSArg(&sargs[nargs++],NhlNctYTable,child->ytable);
		NhlSetSArg(&sargs[nargs++],NhlNctXTableLengths,
							child->xtable_lens);
		NhlSetSArg(&sargs[nargs++],NhlNctYTableLengths,
							child->ytable_lens);
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
	}
	else if(catl->cat.type == intQ){
		int	*intvect,**inttable;
		CoordArrTableIntLayer	l =
					(CoordArrTableIntLayer)catl->cat.child;
		CoordArrTableIntLayerPart	*child =
					(CoordArrTableIntLayerPart*)&l->catint;

		NhlSetSArg(&sargs[nargs++],NhlNctXTableLengths,
							child->xtable_lens);
		NhlSetSArg(&sargs[nargs++],NhlNctYTableLengths,
							child->ytable_lens);
		NhlSetSArg(&sargs[nargs++],NhlNctXMaxF,(float)child->max_x);
		NhlSetSArg(&sargs[nargs++],NhlNctYMaxF,(float)child->max_y);
		NhlSetSArg(&sargs[nargs++],NhlNctXMinF,(float)child->min_x);
		NhlSetSArg(&sargs[nargs++],NhlNctYMinF,(float)child->min_y);
		if(child->missing_x_set)
			NhlSetSArg(&sargs[nargs++],NhlNctXMissingF,
						(float)child->missing_x);
		if(child->missing_y_set)
			NhlSetSArg(&sargs[nargs++],NhlNctYMissingF,
						(float)child->missing_y);
		/*
		 * I must copy the vectors here, and use the convertMalloc
		 * function to do it, that way the memory is around as long
		 * as the data conversion is needed.
		 */
		if(child->xtable != NULL){
			xtbl = _NhlCopyGenArray(child->xtable,True);
			if(xtbl == NULL)
				return FATAL;
			inttable = (int**)xtbl->data;
			flttable = (float**)xtbl->data;
			lens = (int*)child->xtable_lens->data;
			for(i=0;i < xtbl->len_dimensions[0];i++){
				if(inttable[i] != NULL){
					intvect = inttable[i];
					fltvect = NhlConvertMalloc
							(sizeof(float)*lens[i]);
					if(fltvect == NULL)
						return FATAL;
					for(j=0;j < lens[i];j++){
						fltvect[j] = (float)intvect[j];
					}
					flttable[i] = fltvect;
				}
				else
					flttable[i] = NULL;
			}
			NhlSetSArg(&sargs[nargs++],NhlNctXTable,xtbl);
		}
		if(child->ytable != NULL){
			ytbl = _NhlCopyGenArray(child->ytable,True);
			if(ytbl == NULL)
				return FATAL;
			inttable = (int**)ytbl->data;
			flttable = (float**)ytbl->data;
			lens = (int*)child->ytable_lens->data;
			for(i=0;i < ytbl->len_dimensions[0];i++){
				if(inttable[i] != NULL){
					intvect = inttable[i];
					fltvect = NhlConvertMalloc
							(sizeof(float)*lens[i]);
					if(fltvect == NULL)
						return FATAL;
					for(j=0;j < lens[i];j++){
						fltvect[j] = (float)intvect[j];
					}
					flttable[i] = fltvect;
				}
				else
					flttable[i] = NULL;
			}
			NhlSetSArg(&sargs[nargs++],NhlNctYTable,ytbl);
		}
	}
	else{
		return FATAL;
	}

	NhlSetSArg(&sargs[nargs++],NhlNctCopyTables,False);

	ret = NhlALCreate((int*)to->addr,"no.name",
			coordArrTableFloatLayerClass,catl->base.id,sargs,nargs);

	NhlFreeGenArray(xtbl);
	NhlFreeGenArray(ytbl);

	return ret;
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
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret = NOERROR;

	floatQ = NrmStringToQuark(NhlTFloat);
	intQ = NrmStringToQuark(NhlTInteger);

	ret = NhlRegisterConverter(
			coordArrTableLayerClass->base_class.class_name,
			coordArrTableFloatLayerClass->base_class.class_name,
			CvtGenObjToFloatObj,NULL,0,False,NULL);

	return ret;
}

/*
 * Function:	CoordArrTableClassPartInitialize
 *
 * Description:	This function is used to init the cat_class part of the layer
 *		class record of this class and of all sub-classes.
 *
 * In Args:	
 *		LayerClass	lc	pointer to class structure
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CoordArrTableClassPartInitialize
#if	__STDC__
(
	LayerClass	lc	/* pointer to class structure	*/
)
#else
(lc)
	LayerClass	lc;	/* pointer to class structure	*/
#endif
{
	NhlErrorTypes		ret, lret;

	ret = _NhlRegisterChildClass(lc,coordArrTableFloatLayerClass,True,
								False,NULL);
	lret = _NhlRegisterChildClass(lc,coordArrTableIntLayerClass,True,
								False,NULL);
	return MIN(lret,ret);
}

#define	COPY_TABLE_LEN(type,dim)\
{									\
	if(ncat->cat##type.dim##table_lens != NULL){			\
	ncat->cat##type.dim##table_lens =				\
		_NhlCopyGenArray(ncat->cat##type.dim##table_lens,True);	\
	if(ncat->cat##type.dim##table_lens == NULL){			\
		NhlPError(FATAL,ENOMEM,NULL);				\
		return FATAL;						\
	}								\
	}								\
}

#define	COPY_TABLE(type,dim)\
{									\
	if(ncat->cat##type.dim##table != NULL){				\
	ncat->cat##type.dim##table =					\
		_NhlCopyGenArray(ncat->cat##type.dim##table,True);	\
	if(ncat->cat##type.dim##table == NULL){				\
		NhlPError(FATAL,ENOMEM,NULL);				\
		return FATAL;						\
	}								\
	if(ncat->cat##type.copy_tables){				\
		type	**vals,*ovect,*nvect;				\
		int	*lens,i;					\
									\
		vals = ncat->cat##type.dim##table->data;		\
		lens = ncat->cat##type.dim##table_lens->data;		\
									\
		for(i=0;i < ncat->cat##type.dim##table->num_elements;	\
								i++){	\
			ovect = vals[i];				\
			if(ovect == NULL)				\
				continue;				\
									\
			nvect = NhlMalloc(sizeof(type)*lens[i]);	\
			if(nvect == NULL){				\
				NhlPError(FATAL,ENOMEM,NULL);		\
				return FATAL;				\
			}						\
			memcpy(nvect,ovect,sizeof(type)*lens[i]);	\
			vals[i] = nvect;				\
		}							\
									\
		ncat->cat##type.own_##dim = True;			\
	}								\
	else								\
		ncat->cat##type.own_##dim = False;			\
	}								\
}

#define CHECK_TABLES(type,dim,DIM)\
{									\
	NhlGenArray	gen, gen2;					\
									\
	if((ncat->cat##type.dim##table != NULL) &&			\
			(ncat->cat##type.dim##table_lens != NULL)){	\
		gen = ncat->cat##type.dim##table;			\
		gen2 = ncat->cat##type.dim##table_lens;			\
		if((gen->num_dimensions != 1) ||			\
					(gen2->num_dimensions != 1)){	\
			NhlPError(WARNING,E_UNKNOWN,			\
		"%s:%s and %s must one dimensional arrays:ignoring",	\
					error_lead,NhlNct##DIM##Table,	\
					NhlNct##DIM##TableLengths);	\
			imp##dim = True;				\
		}							\
		else if(gen->len_dimensions[0]!=gen2->len_dimensions[0]){\
			NhlPError(WARNING,E_UNKNOWN,			\
	"%s:%s and %s must be arrays of the same length:ignoring",	\
					error_lead,NhlNct##DIM##Table,	\
					NhlNct##DIM##TableLengths);	\
			imp##dim = True;				\
		}							\
	}								\
	else if((ncat->cat##type.dim##table != NULL) ||			\
			(ncat->cat##type.dim##table_lens != NULL)){	\
		NhlPError(WARNING,E_UNKNOWN,				\
		"%s:%s and %s must be set together:resetting both",	\
					error_lead,NhlNct##DIM##Table,	\
					NhlNct##DIM##TableLengths);	\
		imp##dim = True;					\
	}								\
	else								\
		imp##dim = True;					\
}

#define	FREE_TABLE_LEN(type,dim,pre)\
	NhlFreeGenArray(pre##cat->cat##type.dim##table_lens);

#define FREE_TABLE(type,dim,pre)\
{									\
	if((pre##cat->cat##type.dim##table != NULL) &&			\
					pre##cat->cat##type.own_##dim){	\
		type	**vals;						\
		int	i;						\
									\
		vals = pre##cat->cat##type.dim##table->data;		\
									\
		for(i=0;i<pre##cat->cat##type.dim##table->num_elements;	\
								i++)	\
			NhlFree(vals[i]);				\
	}								\
	NhlFreeGenArray(pre##cat->cat##type.dim##table);		\
}

#define FREE_TABLES(type,dim,pre)\
{									\
	FREE_TABLE(type,dim,pre)					\
	FREE_TABLE_LEN(type,dim,pre)					\
}

#define	CHECK_MINMAX(type,dim,otherdim)\
{									\
	if(!ncat->cat##type.max_##dim##_set ||				\
				!ncat->cat##type.min_##dim##_set){	\
		NhlBoolean	initminmax = False;			\
		int		*lens,i,j;				\
		type		**vals,max=(type)0,min=(type)0;		\
									\
		if(ncat->cat##type.dim##table != NULL){			\
									\
			vals=(type**)ncat->cat##type.dim##table->data;	\
									\
			lens = (int*)ncat->cat##type.dim##table_lens->data;\
			for(i=0;					\
			i<ncat->cat##type.dim##table->len_dimensions[0];\
								i++){	\
				type	*vect;				\
									\
				if(vals[i] != NULL){			\
					vect = vals[i];			\
					for(j=0;j < lens[i];j++){	\
			if((ncat->cat##type.missing_##dim##_set) &&	\
			(vect[j] == ncat->cat##type.missing_##dim))	\
							continue;	\
									\
						if(initminmax){		\
						max = MAX(vect[j],max);	\
						min = MIN(vect[j],min);	\
						}			\
						else{			\
						max = vect[j];		\
						min = vect[j];		\
						initminmax=True;	\
						}			\
					}				\
				}					\
				else{					\
					if(initminmax){			\
					max = MAX(max,lens[i]);		\
					min = MIN(min,(type)1.0);	\
					}				\
					else{				\
					max = lens[i];			\
					min = (type)1.0;		\
					initminmax=True;		\
					}				\
				}					\
			}						\
									\
		}							\
		else{							\
			max = min = (type)1.0;				\
			lens =						\
			(int*)ncat->cat##type.otherdim##table_lens->data;\
			for(i=0;					\
		i<ncat->cat##type.otherdim##table_lens->len_dimensions[0];\
								i++)	\
				max = MAX(max,lens[i]);			\
		}							\
									\
		if(!ncat->cat##type.max_##dim##_set)			\
			ncat->cat##type.max_##dim = max;		\
		if(!ncat->cat##type.min_##dim##_set)			\
			ncat->cat##type.min_##dim = min;		\
	}								\
}

#define	INIT_TABLES(type,dim,DIM)\
{									\
	CHECK_TABLES(type,dim,DIM)					\
									\
	if(!imp##dim){							\
		COPY_TABLE_LEN(type,dim)				\
		COPY_TABLE(type,dim)					\
	}								\
	else{								\
		ncat->cat##type.dim##table = NULL;			\
		ncat->cat##type.dim##table_lens = NULL;			\
	}								\
}

#define INIT_FUNC(name,type)\
{									\
	char		*error_lead = #name "Initialize";		\
	name##Layer	ncat = (name##Layer)new;			\
	NhlBoolean	impy = False, impx = False;			\
									\
	/*								\
	 * insure accuracy, and copy Table & Table_lens			\
	 */								\
	INIT_TABLES(type,y,Y)						\
	INIT_TABLES(type,x,X)						\
									\
	if(impx && impy){						\
		NhlPError(FATAL,E_UNKNOWN,				\
		"%s:Cannot have Implied X and Y values",error_lead);	\
		return FATAL;						\
	}								\
									\
	/*								\
	 * Set Max's and Min's						\
	 */								\
	CHECK_MINMAX(type,x,y)						\
	CHECK_MINMAX(type,y,x)						\
									\
	return NOERROR;							\
}		

/*
 * Function:	CoordArrTableFloatInitialize
 *
 * Description:	This function initializes an instance of a CoordArrTableFloat
 *		class object.
 *
 * In Args:	
 *	LayerClass	lc,	class
 *	Layer		req,	requested
 *	Layer		new,	new
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
CoordArrTableFloatInitialize
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
INIT_FUNC(CoordArrTableFloat,float)

/*
 * Function:	CoordArrTableIntInitialize
 *
 * Description:	This function initializes an instance of a CoordArrTableInt
 *		class object.
 *
 * In Args:	
 *	LayerClass	lc,	class
 *	Layer		req,	requested
 *	Layer		new,	new
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
CoordArrTableIntInitialize
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
INIT_FUNC(CoordArrTableInt,int)

#undef INIT_FUNC

/*
 * Function:	CoordArrTableInitialize
 *
 * Description:	This function initializes an instance of a CoordArrTable
 *		class object.
 *
 * In Args:	
 *	LayerClass	lc,	class
 *	Layer		req,	requested
 *	Layer		new,	new
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
	char			*error_lead = "CoordArrTableInitialize";
	CoordArrTableLayer	ncat = (CoordArrTableLayer)new;
	LayerClass		child_class=NULL;
	int			tchild;
	char			name[MAXRESNAMLEN];
	NhlErrorTypes		ret;

	if(ncat->cat.type_string == NULL){
		NhlPError(FATAL,E_UNKNOWN,
		"%s:The %s resource must be specified to create a %s object",
				error_lead,NhlNdiType,_NhlClassName(lc));

		return FATAL;
	}

	ncat->cat.type = NrmStringToQuark(ncat->cat.type_string);
	/*
	 * Point to perminate memory in Quarks.c
	 */
	ncat->cat.type_string = NrmQuarkToString(ncat->cat.type);

	/*
	 * Create the correct child...
	 */
	strcpy(name,ncat->base.name);
	if(ncat->cat.type == floatQ){
		child_class = coordArrTableFloatLayerClass;
		strcat(name,".FLT");
	}
	else if(ncat->cat.type == intQ){
		child_class = coordArrTableIntLayerClass;
		strcat(name,".INT");
	}

	ret = _NhlCreateChild(&tchild,name,child_class,new,NULL);

	ncat->cat.child = _NhlGetLayer(tchild);

	return ret;
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
	char			*error_lead = "CoordArrTableSetValues";
	NhlErrorTypes		ret = NOERROR;
	CoordArrTableLayer	ncat = (CoordArrTableLayer)new;
	CoordArrTableLayer	ocat = (CoordArrTableLayer)old;

	if(ncat->cat.type_string != ocat->cat.type_string){
		NhlPError(INFO,E_UNKNOWN,
			"%s:%s is setable only at create time - ignoring!",
							error_lead,NhlNdiType);
		ncat->cat.type_string = ocat->cat.type_string;
		ret = INFO;
	}

	return	ret;
}

#define	SET_TABLES(type,dim,DIM)\
{									\
	NhlBoolean	imp##dim = False;				\
									\
	/* only do stuff if one of the fields changed */		\
	if((ncat->cat##type.dim##table !=				\
					ocat->cat##type.dim##table) ||	\
		(ncat->cat##type.dim##table_lens !=			\
				ocat->cat##type.dim##table_lens)){	\
									\
		CHECK_TABLES(type,dim,DIM)				\
									\
		/* if imp##dim then bad values - reset */		\
		if(imp##dim){						\
			ncat->cat##type.dim##table =			\
					ocat->cat##type.dim##table;	\
			ncat->cat##type.dim##table_lens =		\
					ocat->cat##type.dim##table_lens;\
		}							\
		else{							\
			if(ncat->cat##type.dim##table_lens !=		\
					ocat->cat##type.dim##table_lens){\
				COPY_TABLE_LEN(type,dim)		\
				FREE_TABLE_LEN(type,dim,o)		\
			}						\
			if(ncat->cat##type.dim##table !=		\
					ocat->cat##type.dim##table){	\
				COPY_TABLE(type,dim)			\
				FREE_TABLE(type,dim,o)			\
			}						\
		}							\
	}								\
									\
	/*								\
	 * if copy_tables is True, but own##dim is False -		\
	 * need to copy the vectors.					\
	 */								\
	if((ncat->cat##type.copy_tables)&&!ncat->cat##type.own_##dim){	\
		NhlGenArray	tgen = ncat->cat##type.dim##table;	\
									\
		COPY_TABLE(type,dim)					\
		NhlFreeGenArray(tgen);					\
	}								\
}

#define	SETVAL_FUNC(name,type)\
{									\
	char		*error_lead = #name "SetValues";		\
	name##Layer	ncat = (name##Layer)new;			\
	name##Layer	ocat = (name##Layer)old;			\
									\
	SET_TABLES(type,x,X)						\
	SET_TABLES(type,x,X)						\
									\
	/*								\
	 * Set Max's and Min's						\
	 */								\
	CHECK_MINMAX(type,x,y)						\
	CHECK_MINMAX(type,y,x)						\
									\
	return	NOERROR;						\
}

/*
 * Function:	CoordArrTableFloatSetValues
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
CoordArrTableFloatSetValues
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
SETVAL_FUNC(CoordArrTableFloat,float)

/*
 * Function:	CoordArrTableIntSetValues
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
CoordArrTableIntSetValues
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
SETVAL_FUNC(CoordArrTableInt,int)

/*
 * Function:	CoordArrTableDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the CoordArrTableLayerClass.
 *
 * In Args:	Layer	l
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CoordArrTableDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
{
	CoordArrTableLayer	catl = (CoordArrTableLayer)l;

	/*
	 * Don't free type_string - it points into Quarks.c's data
	 */

	if(catl->cat.child != NULL)
		return _NhlDestroyChild(catl->cat.child->base.id,l);

	return NOERROR;
}

#define DESTROY_FUNC(name,type)\
{									\
	name##Layer	ncat = (name##Layer)l;				\
									\
	FREE_TABLES(type,x,n)						\
	FREE_TABLES(type,y,n)						\
									\
	return NOERROR;							\
}		

/*
 * Function:	CoordArrTableFloatDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the CoordArrTableFloatLayerClass.
 *
 * In Args:	Layer	l
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CoordArrTableFloatDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
DESTROY_FUNC(CoordArrTableFloat,float)

/*
 * Function:	CoordArrTableIntDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the CoordArrTableIntLayerClass.
 *
 * In Args:	Layer	l
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CoordArrTableIntDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
DESTROY_FUNC(CoordArrTableInt,int)

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
