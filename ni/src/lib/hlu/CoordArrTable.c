/*
 *      $Id: CoordArrTable.c,v 1.14 1994-05-12 23:50:44 boote Exp $
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
	NhlLayer	l = (NhlLayer)base;

	if(l->base.layer_class == NhlcoordArrTableFloatLayerClass){
		NhlCoordArrTableFloatLayer	catl =
						(NhlCoordArrTableFloatLayer)l;

		catl->catfloat.missing_x_set = False;
		catl->catfloat.missing_x = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArrTableIntLayerClass){
		NhlCoordArrTableIntLayer	catl = (NhlCoordArrTableIntLayer)l;

		catl->catint.missing_x_set = False;
		catl->catint.missing_x = 0;
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

	if(l->base.layer_class == NhlcoordArrTableFloatLayerClass){
		NhlCoordArrTableFloatLayer	catl = (NhlCoordArrTableFloatLayer)l;

		catl->catfloat.missing_y_set = False;
		catl->catfloat.missing_y = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArrTableIntLayerClass){
		NhlCoordArrTableIntLayer	catl = (NhlCoordArrTableIntLayer)l;

		catl->catint.missing_y_set = False;
		catl->catint.missing_y = 0;
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

	if(l->base.layer_class == NhlcoordArrTableFloatLayerClass){
		NhlCoordArrTableFloatLayer	catl = (NhlCoordArrTableFloatLayer)l;

		catl->catfloat.max_x_set = False;
		catl->catfloat.max_x = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArrTableIntLayerClass){
		NhlCoordArrTableIntLayer	catl = (NhlCoordArrTableIntLayer)l;

		catl->catint.max_x_set = False;
		catl->catint.max_x = 0;
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

	if(l->base.layer_class == NhlcoordArrTableFloatLayerClass){
		NhlCoordArrTableFloatLayer	catl = (NhlCoordArrTableFloatLayer)l;

		catl->catfloat.max_y_set = False;
		catl->catfloat.max_y = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArrTableIntLayerClass){
		NhlCoordArrTableIntLayer	catl = (NhlCoordArrTableIntLayer)l;

		catl->catint.max_y_set = False;
		catl->catint.max_y = 0;
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

	if(l->base.layer_class == NhlcoordArrTableFloatLayerClass){
		NhlCoordArrTableFloatLayer	catl = (NhlCoordArrTableFloatLayer)l;

		catl->catfloat.min_x_set = False;
		catl->catfloat.min_x = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArrTableIntLayerClass){
		NhlCoordArrTableIntLayer	catl = (NhlCoordArrTableIntLayer)l;

		catl->catint.min_x_set = False;
		catl->catint.min_x = 0;
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

	if(l->base.layer_class == NhlcoordArrTableFloatLayerClass){
		NhlCoordArrTableFloatLayer	catl = (NhlCoordArrTableFloatLayer)l;

		catl->catfloat.min_y_set = False;
		catl->catfloat.min_y = 0.0;
	}
	else if(l->base.layer_class == NhlcoordArrTableIntLayerClass){
		NhlCoordArrTableIntLayer	catl = (NhlCoordArrTableIntLayer)l;

		catl->catint.min_y_set = False;
		catl->catint.min_y = 0;
	}
	else
		return NhlFATAL;

	return NhlNOERROR;
}

#define	Oset(field)	NhlOffset(NhlCoordArrTableFloatLayerRec,catfloat.field)
static NhlResource fltresources[] = {
	{NhlNctXTable,NhlCctXTable,NhlTGenArray,sizeof(NhlGenArray),
		Oset(xtable),NhlTImmediate,(NhlPointer)NULL,0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctYTable,NhlCctYTable,NhlTGenArray,sizeof(NhlGenArray),
		Oset(ytable),NhlTImmediate,(NhlPointer)NULL,0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctXTableLengths,NhlCctXTableLengths,NhlTGenArray,
		sizeof(NhlGenArray),
		Oset(xtable_lens),NhlTImmediate,(NhlPointer)NULL,0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctYTableLengths,NhlCctYTableLengths,NhlTGenArray,
		sizeof(NhlGenArray),
		Oset(ytable_lens),NhlTImmediate,(NhlPointer)NULL,0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctCopyTables,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		Oset(copy_tables),NhlTImmediate,(NhlPointer)True,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_x_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctXMissingF,NhlCdiMissingValue,NhlTFloat,sizeof(float),
		Oset(missing_x),NhlTProcedure,(NhlPointer)MissingXSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_y_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctYMissingF,NhlCdiMissingValue,NhlTFloat,sizeof(float),
		Oset(missing_y),NhlTProcedure,(NhlPointer)MissingYSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_x_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctXMaxF,NhlCctXMaxF,NhlTFloat,sizeof(float),
		Oset(max_x),NhlTProcedure,(NhlPointer)MaxXSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_y_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctYMaxF,NhlCctYMaxF,NhlTFloat,sizeof(float),
		Oset(max_y),NhlTProcedure,(NhlPointer)MaxYSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_x_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctXMinF,NhlCctXMinF,NhlTFloat,sizeof(float),
		Oset(min_x),NhlTProcedure,(NhlPointer)MinXSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_y_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctYMinF,NhlCctYMinF,NhlTFloat,sizeof(float),
		Oset(min_y),NhlTProcedure,(NhlPointer)MinYSet,0,NULL},
	/* use reslist to init private "own" fields */
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(own_x),NhlTImmediate,(NhlPointer)False,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(own_y),NhlTImmediate,(NhlPointer)False,0,NULL}
};
#undef Oset

#define	Oset(field)	NhlOffset(NhlCoordArrTableIntLayerRec,catint.field)
static NhlResource intresources[] = {
	{NhlNctXTable,NhlCctXTable,NhlTGenArray,sizeof(NhlGenArray),
		Oset(xtable),NhlTImmediate,(NhlPointer)NULL,0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctYTable,NhlCctYTable,NhlTGenArray,sizeof(NhlGenArray),
		Oset(ytable),NhlTImmediate,(NhlPointer)NULL,0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctXTableLengths,NhlCctXTableLengths,NhlTGenArray,
		sizeof(NhlGenArray),
		Oset(xtable_lens),NhlTImmediate,(NhlPointer)NULL,0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctYTableLengths,NhlCctYTableLengths,NhlTGenArray,
		sizeof(NhlGenArray),
		Oset(ytable_lens),NhlTImmediate,(NhlPointer)NULL,0,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNctCopyTables,NhlCdiCopyData,NhlTBoolean,sizeof(NhlBoolean),
		Oset(copy_tables),NhlTImmediate,(NhlPointer)True,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_x_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctXMissing,NhlCdiMissingValue,NhlTFloat,sizeof(float),
		Oset(missing_x),NhlTProcedure,(NhlPointer)MissingXSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(missing_y_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctYMissing,NhlCdiMissingValue,NhlTFloat,sizeof(float),
		Oset(missing_y),NhlTProcedure,(NhlPointer)MissingYSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_x_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctXMax,NhlCctXMax,NhlTFloat,sizeof(float),
		Oset(max_x),NhlTProcedure,(NhlPointer)MaxXSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(max_y_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctYMax,NhlCctYMax,NhlTFloat,sizeof(float),
		Oset(max_y),NhlTProcedure,(NhlPointer)MaxYSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_x_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctXMin,NhlCctXMin,NhlTFloat,sizeof(float),
		Oset(min_x),NhlTProcedure,(NhlPointer)MinXSet,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(min_y_set),NhlTImmediate,(NhlPointer)True,0,NULL},
	{NhlNctYMin,NhlCctYMin,NhlTFloat,sizeof(float),
		Oset(min_y),NhlTProcedure,(NhlPointer)MinYSet,0,NULL},
	/* use reslist to init private "own" fields */
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(own_x),NhlTImmediate,(NhlPointer)False,0,NULL},
	{"no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
		Oset(own_y),NhlTImmediate,(NhlPointer)False,0,NULL}
};
#undef Oset

#define	Oset(field)	NhlOffset(NhlCoordArrTableLayerRec,cat.field)
static NhlResource resources[] = {
	{NhlNdiType,NhlCdiType,NhlTString,sizeof(NhlString),
		Oset(type_string),NhlTImmediate,(NhlPointer)NULL,0,(NhlFreeFunc)NhlFree}
};
#undef Oset

/* base methods */

static NhlErrorTypes CoordArrTableClassPartInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc	/* lc to init	*/
#endif
);

static NhlErrorTypes CoordArrTableClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes CoordArrTableFloatInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableIntInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
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

static NhlErrorTypes CoordArrTableFloatSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableIntSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes CoordArrTableDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

static NhlErrorTypes CoordArrTableFloatDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

static NhlErrorTypes CoordArrTableIntDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

NhlCoordArrTableFloatLayerClassRec NhlcoordArrTableFloatLayerClassRec = {
	/* NhlBaseLayerClassPart */
	{
/* class_name			*/	"CoordArrTableFloat",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCoordArrTableFloatLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlobjLayerClassRec,

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
	/* NhlCoordArrTableFloatLayerPart */
	{
/* foo				*/	0
	}
};

NhlCoordArrTableIntLayerClassRec NhlcoordArrTableIntLayerClassRec = {
	/* NhlBaseLayerClassPart */
	{
/* class_name			*/	"CoordArrTableInt",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCoordArrTableIntLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlobjLayerClassRec,

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
	/* NhlCoordArrTableIntLayerPart */
	{
/* foo				*/	0
	}
};

NhlCoordArrTableLayerClassRec NhlcoordArrTableLayerClassRec = {
	/* NhlBaseLayerClassPart */
	{
/* class_name			*/	"CoordArrTable",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlCoordArrTableLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhldataItemLayerClassRec,

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
	/* NhlDataItemLayerClassPart */
	{
/* foo				*/	0
	},
	/* NhlCoordArrTableLayerClassPart */
	{
/* foo				*/	0
	}
};
	
NhlLayerClass NhlcoordArrTableLayerClass =
				(NhlLayerClass)&NhlcoordArrTableLayerClassRec;
NhlLayerClass NhlcoordArrTableFloatLayerClass = 
			(NhlLayerClass)&NhlcoordArrTableFloatLayerClassRec;
NhlLayerClass NhlcoordArrTableIntLayerClass =
			(NhlLayerClass)&NhlcoordArrTableIntLayerClassRec;

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
	NhlCoordArrTableLayer	catl = NULL;
	NhlSArg			sargs[30];
	int			nargs=0,i,j,*lens;
	float			**flttable,*fltvect;
	NhlGenArray		xtbl = NULL, ytbl = NULL;
	NhlErrorTypes		ret = NhlNOERROR;

	if(num_args != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"CvtGenObjToFloatObj:Called w/wrong args");
		return NhlFATAL;
	}

	catl = (NhlCoordArrTableLayer)_NhlGetLayer(from->data.intval);
	if((catl == NULL)||(catl->base.layer_class != NhlcoordArrTableLayerClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"CvtGenObjToFloatObj:Called w/ improper \"from\" object");
		return NhlFATAL;
	}

	if(catl->cat.type == floatQ){
		NhlCoordArrTableFloatLayer	l =
				(NhlCoordArrTableFloatLayer)catl->cat.child;
		NhlCoordArrTableFloatLayerPart	*child =
				(NhlCoordArrTableFloatLayerPart*)&l->catfloat;

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
		NhlCoordArrTableIntLayer	l =
				(NhlCoordArrTableIntLayer)catl->cat.child;
		NhlCoordArrTableIntLayerPart	*child =
				(NhlCoordArrTableIntLayerPart*)&l->catint;

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
				return NhlFATAL;
			inttable = (int**)xtbl->data;
			flttable = (float**)xtbl->data;
			lens = (int*)child->xtable_lens->data;
			for(i=0;i < xtbl->len_dimensions[0];i++){
				if(inttable[i] != NULL){
					intvect = inttable[i];
					fltvect = NhlConvertMalloc
							(sizeof(float)*lens[i]);
					if(fltvect == NULL)
						return NhlFATAL;
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
				return NhlFATAL;
			inttable = (int**)ytbl->data;
			flttable = (float**)ytbl->data;
			lens = (int*)child->ytable_lens->data;
			for(i=0;i < ytbl->len_dimensions[0];i++){
				if(inttable[i] != NULL){
					intvect = inttable[i];
					fltvect = NhlConvertMalloc
							(sizeof(float)*lens[i]);
					if(fltvect == NULL)
						return NhlFATAL;
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
		return NhlFATAL;
	}

	NhlSetSArg(&sargs[nargs++],NhlNctCopyTables,False);

	ret = NhlALCreate(to->data.ptrval,"no.name",
		NhlcoordArrTableFloatLayerClass,catl->base.id,sargs,nargs);

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
	NhlErrorTypes	ret = NhlNOERROR;

	floatQ = NrmStringToQuark(NhlTFloat);
	intQ = NrmStringToQuark(NhlTInteger);

	ret = NhlRegisterConverter(
			NhlcoordArrTableLayerClass->base_class.class_name,
			NhlcoordArrTableFloatLayerClass->base_class.class_name,
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
 *		NhlLayerClass	lc	pointer to class structure
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
	NhlLayerClass	lc	/* pointer to class structure	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* pointer to class structure	*/
#endif
{
	NhlErrorTypes		ret, lret;

	ret = _NhlRegisterChildClass(lc,NhlcoordArrTableFloatLayerClass,True,
								False,NULL);
	lret = _NhlRegisterChildClass(lc,NhlcoordArrTableIntLayerClass,True,
								False,NULL);
	return MIN(lret,ret);
}

#define	COPY_TABLE_LEN(type,dim)\
{									\
	if(ncat->cat##type.dim##table_lens != NULL){			\
	ncat->cat##type.dim##table_lens =				\
	_NhlCopyGenArray((NhlGenArray)ncat->cat##type.dim##table_lens,	\
						(NhlBoolean)True);	\
	if(ncat->cat##type.dim##table_lens == NULL){			\
		NhlPError(NhlFATAL,ENOMEM,NULL);			\
		return NhlFATAL;					\
	}								\
	}								\
}

#define	COPY_TABLE(type,dim)\
{									\
	if(ncat->cat##type.dim##table != NULL){				\
	ncat->cat##type.dim##table =					\
	_NhlCopyGenArray((NhlGenArray)ncat->cat##type.dim##table,	\
						(NhlBoolean)True);	\
	if(ncat->cat##type.dim##table == NULL){				\
		NhlPError(NhlFATAL,ENOMEM,NULL);			\
		return NhlFATAL;					\
	}								\
	if(ncat->cat##type.copy_tables){				\
		type	**vals##dim,*ovect##dim,*nvect##dim;		\
		int	*lens##dim,i##dim;				\
									\
		vals##dim = ncat->cat##type.dim##table->data;		\
		lens##dim = ncat->cat##type.dim##table_lens->data;	\
									\
		for(i##dim=0;						\
			i##dim<ncat->cat##type.dim##table->num_elements;\
							i##dim++){	\
			ovect##dim = vals##dim[i##dim];			\
			if(ovect##dim == NULL)				\
				continue;				\
									\
			nvect##dim = NhlMalloc(sizeof(type)*		\
						lens##dim[i##dim]);	\
			if(nvect##dim == NULL){				\
				NhlPError(NhlFATAL,ENOMEM,NULL);	\
				return NhlFATAL;			\
			}						\
			memcpy((char*)nvect##dim,(char*)ovect##dim,	\
				sizeof(type)*lens##dim[i##dim]);	\
			vals##dim[i##dim] = nvect##dim;			\
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
	NhlGenArray	gen##DIM, gen2##DIM;				\
									\
	if((ncat->cat##type.dim##table != NULL) &&			\
			(ncat->cat##type.dim##table_lens != NULL)){	\
		gen##DIM = ncat->cat##type.dim##table;			\
		gen2##DIM = ncat->cat##type.dim##table_lens;		\
		if((gen##DIM->num_dimensions != 1) ||			\
				(gen2##DIM->num_dimensions != 1)){	\
			NhlPError(NhlWARNING,NhlEUNKNOWN,		\
		"%s:%s and %s must one dimensional arrays:ignoring",	\
					error_lead,NhlNct##DIM##Table,	\
					NhlNct##DIM##TableLengths);	\
			inv##dim = True;				\
		}							\
		else if(gen##DIM->len_dimensions[0] !=			\
					gen2##DIM->len_dimensions[0]){	\
			NhlPError(NhlWARNING,NhlEUNKNOWN,		\
	"%s:%s and %s must be arrays of the same length:ignoring",	\
					error_lead,NhlNct##DIM##Table,	\
					NhlNct##DIM##TableLengths);	\
			inv##dim = True;				\
		}							\
	}								\
	else if((ncat->cat##type.dim##table != NULL) ||			\
			(ncat->cat##type.dim##table_lens != NULL)){	\
		NhlPError(NhlWARNING,NhlEUNKNOWN,			\
		"%s:%s and %s must be set together:resetting both",	\
					error_lead,NhlNct##DIM##Table,	\
					NhlNct##DIM##TableLengths);	\
		inv##dim = True;					\
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
		type	**fvals##dim;					\
		int	fi##dim;					\
									\
		fvals##dim = pre##cat->cat##type.dim##table->data;	\
									\
		for(fi##dim=0;						\
		fi##dim<pre##cat->cat##type.dim##table->num_elements;	\
							fi##dim++)	\
			NhlFree(fvals##dim[fi##dim]);			\
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
		NhlBoolean	init##dim = False;			\
		int		*mlens##dim,mi##dim,mj##dim;		\
		type		**mv##dim;				\
		type		mx##dim=(type)0,mn##dim=(type)0;	\
									\
		if(ncat->cat##type.dim##table != NULL){			\
									\
			mv##dim=(type**)ncat->cat##type.dim##table->data;\
									\
			mlens##dim =					\
			(int*)ncat->cat##type.dim##table_lens->data;	\
									\
			for(mi##dim=0;					\
		mi##dim<ncat->cat##type.dim##table->len_dimensions[0];	\
							mi##dim++){	\
				type	*mvec##dim;			\
									\
				if(mv##dim[mi##dim] != NULL){		\
					mvec##dim = mv##dim[mi##dim];	\
					for(mj##dim=0;			\
					mj##dim < mlens##dim[mi##dim];	\
							mj##dim++){	\
			if((ncat->cat##type.missing_##dim##_set) &&	\
			(mvec##dim[mj##dim]==ncat->cat##type.missing_##dim))\
							continue;	\
									\
						if(init##dim){		\
			mx##dim = MAX(mvec##dim[mj##dim],mx##dim);	\
			mn##dim = MIN(mvec##dim[mj##dim],mn##dim);	\
						}			\
						else{			\
			mx##dim = mvec##dim[mj##dim];			\
			mn##dim = mvec##dim[mj##dim];			\
							init##dim=True;	\
						}			\
					}				\
				}					\
				else{					\
					if(init##dim){			\
			mx##dim = MAX(mx##dim,mlens##dim[mi##dim]);	\
			mn##dim = MIN(mn##dim,(type)1.0);		\
					}				\
					else{				\
					mx##dim = mlens##dim[mi##dim];	\
					mn##dim = (type)1.0;		\
					init##dim=True;			\
					}				\
				}					\
			}						\
									\
		}							\
		else{							\
			mx##dim = mn##dim = (type)1.0;			\
			mlens##dim =					\
			(int*)ncat->cat##type.otherdim##table_lens->data;\
			for(mi##dim=0;					\
	mi##dim<ncat->cat##type.otherdim##table_lens->len_dimensions[0];\
							mi##dim++)	\
				mx##dim = MAX(mx##dim,mlens##dim[mi##dim]);\
		}							\
									\
		if(!ncat->cat##type.max_##dim##_set)			\
			ncat->cat##type.max_##dim = mx##dim;		\
		if(!ncat->cat##type.min_##dim##_set)			\
			ncat->cat##type.min_##dim = mn##dim;		\
	}								\
}

#define	INIT_TABLES(type,dim,DIM)\
{									\
	NhlBoolean	inv##dim = False;				\
									\
	CHECK_TABLES(type,dim,DIM)					\
									\
	if(inv##dim){							\
		NhlPError(NhlFATAL,NhlEUNKNOWN,				\
		"%s:Resources specifying %s dimension are invalid",	\
						error_lead,#DIM);	\
		return NhlFATAL;					\
	}								\
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
	char			*error_lead = #name "Initialize";	\
	Nhl##name##Layer	ncat = (Nhl##name##Layer)new;		\
	NhlBoolean		impy = False, impx = False;		\
									\
	/*								\
	 * insure accuracy, and copy Table & Table_lens			\
	 */								\
	INIT_TABLES(type,y,Y)						\
	INIT_TABLES(type,x,X)						\
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
	return NhlNOERROR;						\
}		

/*
 * Function:	CoordArrTableFloatInitialize
 *
 * Description:	This function initializes an instance of a CoordArrTableFloat
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
CoordArrTableFloatInitialize
#if	__STDC__
(
	NhlLayerClass	lc,	/* class	*/
	NhlLayer		req,	/* requested	*/
	NhlLayer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlLayerClass	lc;	/* class	*/
	NhlLayer		req;	/* requested	*/
	NhlLayer		new;	/* new		*/
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
CoordArrTableIntInitialize
#if	__STDC__
(
	NhlLayerClass	lc,	/* class	*/
	NhlLayer		req,	/* requested	*/
	NhlLayer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlLayerClass	lc;	/* class	*/
	NhlLayer		req;	/* requested	*/
	NhlLayer		new;	/* new		*/
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
CoordArrTableInitialize
#if	__STDC__
(
	NhlLayerClass	lc,	/* class	*/
	NhlLayer		req,	/* requested	*/
	NhlLayer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlLayerClass	lc;	/* class	*/
	NhlLayer		req;	/* requested	*/
	NhlLayer		new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	char			*error_lead = "CoordArrTableInitialize";
	NhlCoordArrTableLayer	ncat = (NhlCoordArrTableLayer)new;
	NhlLayerClass		child_class=NULL;
	int			tchild;
	char			name[_NhlMAXRESNAMLEN];
	NhlErrorTypes		ret;

	if(ncat->cat.type_string == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s:The %s resource must be specified to create a %s object",
				error_lead,NhlNdiType,_NhlClassName(lc));

		return NhlFATAL;
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
		child_class = NhlcoordArrTableFloatLayerClass;
		strcat(name,".FLT");
	}
	else if(ncat->cat.type == intQ){
		child_class = NhlcoordArrTableIntLayerClass;
		strcat(name,".INT");
	}

	ret = _NhlVACreateChild(&tchild,name,child_class,new,NULL);

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
	char			*error_lead = "CoordArrTableSetValues";
	NhlErrorTypes		ret = NhlNOERROR;
	NhlCoordArrTableLayer	ncat = (NhlCoordArrTableLayer)new;
	NhlCoordArrTableLayer	ocat = (NhlCoordArrTableLayer)old;

	if(ncat->cat.type_string != ocat->cat.type_string){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is setable only at create time - ignoring!",
							error_lead,NhlNdiType);
		ncat->cat.type_string = ocat->cat.type_string;
		ret = NhlWARNING;
	}

	return	ret;
}

#define	SET_TABLES(type,dim,DIM)\
{									\
	/* only do stuff if one of the fields changed */		\
	if((ncat->cat##type.dim##table !=				\
					ocat->cat##type.dim##table) ||	\
		(ncat->cat##type.dim##table_lens !=			\
				ocat->cat##type.dim##table_lens)){	\
									\
		NhlBoolean	inv##dim = False;			\
									\
		CHECK_TABLES(type,dim,DIM)				\
									\
		if(inv##dim){						\
			NhlPError(NhlWARNING,NhlEUNKNOWN,		\
			"%s:invalid %s dimension: resetting %s and %s",	\
				error_lead,#DIM,NhlNct##DIM##Table,	\
					NhlNct##DIM##TableLengths);	\
			ncat->cat##type.dim##table =			\
					ocat->cat##type.dim##table;	\
			ncat->cat##type.dim##table_lens =		\
					ocat->cat##type.dim##table_lens;\
									\
			if(ncat->cat##type.dim##table == NULL)		\
				imp##dim = True;			\
		}							\
	}								\
}

#define	FINISH_TABLES(type,dim,DIM)\
{									\
	if(ncat->cat##type.dim##table_lens !=				\
				ocat->cat##type.dim##table_lens){	\
		status = True;						\
		COPY_TABLE_LEN(type,dim)				\
		FREE_TABLE_LEN(type,dim,o)				\
	}								\
									\
	/*								\
	 * if copy_tables is True, but own##dim is False -		\
	 * need to copy the vectors also.				\
	 */								\
	if((ncat->cat##type.dim##table!=ocat->cat##type.dim##table) ||	\
	((ncat->cat##type.copy_tables)&&!ncat->cat##type.own_##dim)){	\
		status = True;						\
		COPY_TABLE(type,dim)					\
		FREE_TABLE(type,dim,o)					\
	}								\
									\
}

#define	SETVAL_FUNC(name,type)\
{									\
	char			*error_lead = #name "SetValues";	\
	Nhl##name##Layer	ncat = (Nhl##name##Layer)new;		\
	Nhl##name##Layer	ocat = (Nhl##name##Layer)old;		\
	NhlBoolean		impx = False, impy = False;		\
	NhlBoolean		status = False;				\
	NhlErrorTypes		ret = NhlNOERROR;			\
									\
	SET_TABLES(type,y,Y)						\
	SET_TABLES(type,x,X)						\
									\
	if(impx && impy){						\
		NhlPError(NhlWARNING,NhlEUNKNOWN,			\
		"%s:Cannot have Implied X and Y values:resetting",	\
							error_lead);	\
		ret = MIN(ret,NhlWARNING);				\
		ncat->cat##type.xtable = ocat->cat##type.xtable;	\
		ncat->cat##type.ytable = ocat->cat##type.ytable;	\
		ncat->cat##type.xtable_lens=ocat->cat##type.xtable_lens;\
		ncat->cat##type.ytable_lens=ocat->cat##type.ytable_lens;\
									\
	}								\
									\
	FINISH_TABLES(type,y,Y)						\
	FINISH_TABLES(type,x,X)						\
									\
	if(ncat->cat##type.missing_x != ocat->cat##type.missing_x){	\
		ncat->cat##type.missing_x_set = True;			\
		status = True;						\
	}								\
	if(ncat->cat##type.missing_y != ocat->cat##type.missing_y){	\
		ncat->cat##type.missing_y_set = True;			\
		status = True;						\
	}								\
									\
	if(ncat->cat##type.max_x != ocat->cat##type.max_x)		\
		ncat->cat##type.max_x_set = True;			\
	if(ncat->cat##type.min_x != ocat->cat##type.min_x)		\
		ncat->cat##type.min_x_set = True;			\
	if(ncat->cat##type.max_y != ocat->cat##type.max_y)		\
		ncat->cat##type.max_y_set = True;			\
	if(ncat->cat##type.min_y != ocat->cat##type.min_y)		\
		ncat->cat##type.min_y_set = True;			\
									\
	/*								\
	 * Set Max's and Min's						\
	 */								\
	CHECK_MINMAX(type,x,y)						\
	CHECK_MINMAX(type,y,x)						\
									\
	if(ncat->cat##type.max_x != ocat->cat##type.max_x)		\
		status = True;						\
	if(ncat->cat##type.min_x != ocat->cat##type.min_x)		\
		status = True;						\
	if(ncat->cat##type.max_y != ocat->cat##type.max_y)		\
		status = True;						\
	if(ncat->cat##type.min_y != ocat->cat##type.min_y)		\
		status = True;						\
	/*								\
	 * Notify superclass that info has changed - This allows	\
	 * the cached data to be marked as out-of-date.			\
	 */								\
	_NhlDataChanged((NhlDataItemLayer)new->base.parent,status);	\
									\
									\
	return	ret;							\
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
SETVAL_FUNC(CoordArrTableInt,int)

/*
 * Function:	CoordArrTableDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlCoordArrTableLayerClass.
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
#if	__STDC__
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	NhlCoordArrTableLayer	catl = (NhlCoordArrTableLayer)l;

	/*
	 * Don't free type_string - it points into Quarks.c's data
	 */

	if(catl->cat.child != NULL)
		return _NhlDestroyChild(catl->cat.child->base.id,l);

	return NhlNOERROR;
}

#define DESTROY_FUNC(name,type)\
{									\
	Nhl##name##Layer	ncat = (Nhl##name##Layer)l;		\
									\
	FREE_TABLES(type,x,n)						\
	FREE_TABLES(type,y,n)						\
									\
	return NhlNOERROR;						\
}		

/*
 * Function:	CoordArrTableFloatDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlCoordArrTableFloatLayerClass.
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
CoordArrTableFloatDestroy
#if	__STDC__
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
DESTROY_FUNC(CoordArrTableFloat,float)

/*
 * Function:	CoordArrTableIntDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the NhlCoordArrTableIntLayerClass.
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
CoordArrTableIntDestroy
#if	__STDC__
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
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
