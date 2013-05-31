/*
 *      $Id: Primitive.c,v 1.1 2000-06-28 19:03:59 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Primitive.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 21 17:04:23 MDT 2000
 *
 *	Description:	The Primitive class is the specification for
 *                      an individual graphics primitive
 */

#include <ncarg/hlu/PrimitiveP.h>
#include <ncarg/hlu/ConvertersP.h>

#define Oset(field)     NhlOffset(NhlPrimitiveLayerRec,primitive.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNprXArray,NhlCprXArray,NhlTFloatGenArray,sizeof(NhlGenArray),
	 Oset(x_arr),NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
	 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNprYArray,NhlCprYArray,NhlTFloatGenArray,sizeof(NhlGenArray),
	 Oset(y_arr),NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
	 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNprPolyType,NhlCprPolyType,NhlTPolyType,sizeof(NhlPolyType),
	 Oset(poly_type),NhlTImmediate,
	 _NhlUSET((NhlPointer)NhlPOLYLINE),0,NULL},
	{NhlNprGraphicStyle,NhlCprGraphicStyle,NhlTObjId,sizeof(int),
	 Oset(graphic_style),NhlTImmediate,
	 _NhlUSET((NhlPointer)NhlNULLOBJID),_NhlRES_NORACCESS,NULL},

};

/*
* Base Methods used
*/

static NhlErrorTypes PrimitiveSetValues(
#if	NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes    PrimitiveInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes	PrimitiveDestroy(
#if	NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes    PrimitiveGetValues(
#if	NhlNeedProto
	NhlLayer,       /* l */
	_NhlArgList,    /* args */
	int             /* num_args */
#endif
);

static NhlErrorTypes 	PrimitiveClassInitialize();

NhlPrimitiveClassRec NhlprimitiveClassRec = {
	{
/* class_name			*/	"primitiveClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlPrimitiveLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlobjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	PrimitiveClassInitialize,
/* layer_initialize		*/	PrimitiveInitialize,
/* layer_set_values		*/	PrimitiveSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	PrimitiveGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	PrimitiveDestroy
	},
	{
					NULL
	}
};

NhlClass NhlprimitiveClass = 
			(NhlClass)&NhlprimitiveClassRec;

static NrmQuark Qxarray;
static NrmQuark Qyarray;

/*
 * Function:	nhlfprimitiveclass
 *
 * Description:	Fortran ?referencable? function to return layer class.
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
_NHLCALLF(nhlfprimitiveclass,NHLFPRIMITIVECLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlprimitiveClass;
}


/*
 * Function:	PrimitiveInitialize
 *
 * Description:	
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: Plotchar state affected
 */
/*ARGSUSED*/
static NhlErrorTypes
PrimitiveInitialize
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
	NhlClass	class;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlPrimitiveLayer	prnew = (NhlPrimitiveLayer) new;
	NhlPrimitiveLayerPart	*prp = &prnew->primitive;

	NhlErrorTypes		ret=NhlNOERROR;

	if (prp->x_arr != NULL)
		prp->x_arr = _NhlCopyGenArray(prp->x_arr,True);
		
	if (prp->y_arr != NULL)
		prp->y_arr = _NhlCopyGenArray(prp->y_arr,True);


	return ret;
}

/*
 * Function:	PrimitiveSetValues
 *
 * Description: 
 *
 * In Args:	Stprdard SetValues args
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: GKS and plotchar state changes.
 */
/*ARGSUSED*/
static NhlErrorTypes 
PrimitiveSetValues
#if	NhlNeedProto
(
	NhlLayer	old,
	NhlLayer	reference,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlPrimitiveLayer 	prnew = (NhlPrimitiveLayer) new;
	NhlPrimitiveLayerPart	*prp = &prnew->primitive;
	NhlPrimitiveLayer 	prold = (NhlPrimitiveLayer) old;
	NhlPrimitiveLayerPart	*oprp = &prold->primitive;
	NhlGenArray		gen;
	NhlErrorTypes 		ret = NhlNOERROR;

	if (prp->x_arr != oprp->x_arr){
		gen = prp->x_arr;
		prp->x_arr = _NhlCopyGenArray(gen,True);
		if(gen && ! prp->x_arr){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		else{
			NhlFreeGenArray(oprp->x_arr);
		}
	}

	if (prp->y_arr != oprp->y_arr){
		gen = prp->y_arr;
		prp->y_arr = _NhlCopyGenArray(gen,True);
		if(gen && ! prp->y_arr){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		else{
			NhlFreeGenArray(oprp->y_arr);
		}
	}


	return ret;
}

/*
 * Function:	PrimitiveDestroy
 *
 * Description: 
 *
 * In Args:	NhlLayer inst	instance of Primitive
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes
PrimitiveDestroy
#if	NhlNeedProto
(
	NhlLayer	layer)
#else
(layer)
	NhlLayer	layer;
#endif
{
	NhlPrimitiveLayer 	prl = (NhlPrimitiveLayer) layer;
	NhlPrimitiveLayerPart	*prp = &prl->primitive;
	NhlErrorTypes		ret = NhlNOERROR;

	if (prp->x_arr) 
		NhlFreeGenArray(prp->x_arr);

	if (prp->y_arr) 
		NhlFreeGenArray(prp->y_arr);


	return ret;
}



/*
 * Function:	PrimitiveClassInitialize
 *
 * Description: Just calls StringToQuark to register new types
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	Error condition
 *
 * Side Effects: 	NONE
 */
static NhlErrorTypes    PrimitiveClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{
        _NhlEnumVals   polytypelist[] = {
        {NhlPOLYLINE,	"Polyline"},
        {NhlPOLYGON,	"Polygon"},
        {NhlPOLYMARKER,	"Polymarker"},
        };

	_NhlRegisterEnumType(NhlprimitiveClass,NhlTPolyType,
			polytypelist,NhlNumber(polytypelist));

	Qxarray = NrmStringToQuark(NhlNprXArray);
	Qyarray = NrmStringToQuark(NhlNprYArray);

	return(NhlNOERROR);	
}


/*
 * Function:    PrimitiveGetValues
 *
 * Description: Retrieves the current setting of one or more Primitive
 *      resources.
 *      This routine only retrieves resources that require special methods
 *      that the generic GetValues method cannot handle. For now this means
 *      all the GenArray resources. Note that space is allocated; the user
 *      is responsible for freeing this space.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when any of the following resources are retrieved:
 *              NhlNprXArray
 *              NhlNprYArray
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    PrimitiveGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlPrimitiveLayer prl = (NhlPrimitiveLayer)l;
        NhlPrimitiveLayerPart *prp = &(prl->primitive);
        NhlGenArray ga;
        char *e_text;
        int i;

        for( i = 0; i< num_args; i++ ) {

                ga = NULL;
                if(args[i].quark == Qxarray) {
                        ga = prp->x_arr;
                }
                else if(args[i].quark == Qyarray) {
                        ga = prp->y_arr;
                }
                if (ga != NULL) {
                        if ((ga = _NhlCopyGenArray(ga,True)) == NULL) {
                                e_text = "%s: error copying %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          "PrimitiveGetValues",
					  NrmQuarkToString(args[i].quark));
                                return NhlFATAL;
                        }
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
			continue;
                }
        }

        return(NhlNOERROR);
}
