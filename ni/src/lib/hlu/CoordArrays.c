/*
 *      $Id: CoordArrays.c,v 1.1 1993-09-15 22:10:30 boote Exp $
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
 *	Date:		Wed Jul 28 11:23:48 MDT 1993
 *
 *	Description:	This class is used to communicate data in the format
 *			of CoordArrays.
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/CoordArraysP.h>

/************************************************************************
*									*
*	CoordArrays Class declarations					*
*									*
************************************************************************/

#define	Oset(field)	NhlOffset(CoordArraysFloatLayerRec,carraysfloat.field)
static NhlResource fltresources[] = {
	{"notyet","notyet",NhlTInteger,sizeof(int),
		Oset(foo),NhlTImmediate,(NhlPointer)0}
};
#undef Oset

#define	Oset(field)	NhlOffset(CoordArraysIntLayerRec,carraysint.field)
static NhlResource intresources[] = {
	{"notyet","notyet",NhlTInteger,sizeof(int),
		Oset(foo),NhlTImmediate,(NhlPointer)0}
};
#undef Oset

#define	Oset(field)	NhlOffset(CoordArraysLayerRec,carrays.field)
static NhlResource resources[] = {
	{"notyet","notyet",NhlTInteger,sizeof(int),
		Oset(foo),NhlTImmediate,(NhlPointer)0}
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
	LayerClass	lc,	/* class	*/
	Layer		req,	/* requested	*/
	Layer		new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes CoordArraysDestroy(
#if	NhlNeedProto
	Layer	l	/* layer to destroy	*/
#endif
);

CoordArraysFloatLayerClassRec coordArraysFloatLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* class_name			*/	"CoordArraysFloat",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(CoordArraysFloatLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&objLayerClassRec,

/* resources			*/	fltresources,
/* num_resources		*/	NhlNumber(fltresources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	},
	/* CoordArraysFloatLayerPart */
	{
/* foo				*/	NULL
	}
};

CoordArraysIntLayerClassRec coordArraysIntLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* class_name			*/	"CoordArraysInt",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(CoordArraysIntLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&objLayerClassRec,

/* resources			*/	intresources,
/* num_resources		*/	NhlNumber(intresources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
	},
	/* CoordArraysIntLayerPart */
	{
/* foo				*/	NULL
	}
};

CoordArraysLayerClassRec coordArraysLayerClassRec = {
	/* BaseLayerClassPart */
	{
/* class_name			*/	"CoordArrays",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(CoordArraysLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&dataItemLayerClassRec,

/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	CoordArraysClassInitialize,
/* layer_initialize		*/	CoordArraysInitialize,
/* layer_set_values		*/	NULL,
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
	/* DataItemLayerClassPart */
	{
/* foo				*/	NULL
	},
	/* CoordArraysLayerClassPart */
	{
/* foo				*/	NULL
	}
};
	
LayerClass coordArraysLayerClass = (LayerClass)&coordArraysLayerClassRec;
LayerClass coordArraysFloatLayerClass = (LayerClass)&coordArraysFloatLayerClassRec;
LayerClass coordArraysIntLayerClass = (LayerClass)&coordArraysIntLayerClassRec;

/************************************************************************
*	New type converters - added to converter table by		*
*	ClassInitialize							*
************************************************************************/

/* none yet */


/************************************************************************
*									*
*	Methode definitions						*
*									*
************************************************************************/

static	NrmQuark	floatQ = NrmNULLQUARK;
static	NrmQuark	intQ = NrmNULLQUARK;

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
	NhlErrorTypes	ret=NOERROR, lret=NOERROR;

	floatQ = NrmStringToQuark(NhlTFloat);
	intQ = NrmStringToQuark(NhlTInteger);
	ret = _NhlInitializeLayerClass(coordArraysIntLayerClass);
	lret = _NhlInitializeLayerClass(coordArraysFloatLayerClass);

	return MIN(ret,lret);
}

/*
 * Function:	CoordArraysInitialize
 *
 * Description:	This function initializes an instance of a CoordArrays
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
CoordArraysInitialize
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
#ifdef	NOTYET
	CoordArraysLayer	lcl = (CoordArraysLayer)new;
	NhlGenArray		gen = lcl->carrays.array;
	NhlGenArray		ngen;
	int			*len = NULL;
	LayerClass		childlc = NULL;
	NhlErrorTypes		ret = NOERROR;

	if(gen == NULL){
		NhlPError(FATAL,E_UNKNOWN,
			"The %s resource must be set to create a %s object",
					NhlNUserData,_NhlClassName(lc));
		return FATAL;
	}

	if((gen->typeQ != floatQ) && (gen->typeQ != intQ)){
		NhlPError(FATAL,E_UNKNOWN,
"The %s object does not support %s type data in the %s resource at this time",
		_NhlClassName(lc),NrmNameToString(gen->typeQ),NhlNUserData);
		return FATAL;
	}

	if((gen->num_dimensions != 2) || (gen->len_dimensions[0] != 2) ||
						(gen->len_dimensions[1] < 1)){
		NhlPError(FATAL,E_UNKNOWN,
		"%s does not support dimensionality array in %s resource",
						_NhlClassName(lc),NhlNUserData);
		return FATAL;
	}

	ngen = NhlMalloc(sizeof(NhlGenArrayRec));
	len = NhlMalloc(sizeof(int) * 2);
	if((ngen == NULL) || (len == NULL)){
		NhlPError(FATAL,ENOMEM,NULL);
		(void)NhlFree(ngen);
		(void)NhlFree(len);
		return FATAL;
	}

	ngen->num_dimensions = 2;
	memcpy(len,gen->len_dimensions,sizeof(int) * 2);
	ngen->len_dimensions = len;
	ngen->typeQ = gen->typeQ;
	ngen->data = gen->data;

	lcl->carrays.array = ngen;


	if(ngen->typeQ == floatQ){
		childlc = coordArraysFloatLayerClass;
	}
	else if(ngen->typeQ == intQ){
		childlc = coordArraysIntLayerClass;
	}

	ret =NhlCreate(&lcl->carrays.child,lcl->base.name,childlc,lcl->base.id,
					_NhlNcvNumPoints,	len[1],
					_NhlNcvArray,		ngen->data,
					NULL);

	if(ret < WARNING){
		NhlPError(FATAL,E_UNKNOWN,"Unable to create %s child in %s",
				childlc->base_class.class_name,lcl->base.name);
		(void)NhlFree(len);
		(void)NhlFree(ngen);
		return ret;
	}

	lcl->carrays.type_child = childlc->base_class.nrm_class;
		
#endif	/* NOTYET */
	return NOERROR;
}

/*
 * Function:	CoordArraysDestroy
 *
 * Description:	This function free's any memory that has been allocated
 *		on behalf of this instance of the CoordArraysLayerClass.
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
CoordArraysDestroy
#if	__STDC__
(
	Layer	l	/* layer to destroy	*/
)
#else
(l)
	Layer	l;	/* layer to destroy	*/
#endif
{
	CoordArraysLayer	lcl = (CoordArraysLayer)l;

	return NhlDestroy(lcl->carrays.child);
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
