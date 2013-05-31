/*
 *      $Id: GraphicStyle.c,v 1.4 1997-02-24 22:12:24 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		GraphicStyle.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 20 18:10:29 MST 1996
 *
 *	Description:	The GraphicStyle class controls the attributes of 
 *                      drawing primitives
 */

#include <math.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/GraphicStyleP.h>

#define Oset(field)     NhlOffset(NhlGraphicStyleLayerRec,graphicstyle.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNgsClipOn,NhlCgsClipOn,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(clip_on),NhlTImmediate,
		 _NhlUSET((NhlPointer) True),0,NULL},
	{NhlNgsColors,NhlCgsColors,NhlTColorIndexGenArray,sizeof(NhlGenArray),
	 Oset(colors),NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
	 (NhlFreeFunc)NhlFreeGenArray},
	{NhlNgsSegments,NhlCgsSegments,NhlTIntegerGenArray,sizeof(NhlGenArray),
	 Oset(segments),NhlTImmediate,_NhlUSET((NhlPointer) NULL),0,
	 (NhlFreeFunc)NhlFreeGenArray},

/* End-documented-resources */

};

/*
* Base Methods used
*/


static NhlErrorTypes GraphicStyleClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes    GraphicStyleInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes GraphicStyleSetValues(
#if	NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int		num_args
#endif
);

static NhlErrorTypes	GraphicStyleDestroy(
#if	NhlNeedProto
        NhlLayer	layer
#endif
);

static NhlErrorTypes    GraphicStyleGetValues(
#if	NhlNeedProto
	NhlLayer,       /* l */
	_NhlArgList,    /* args */
	int             /* num_args */
#endif
);


NhlGraphicStyleClassRec NhlgraphicStyleClassRec = {
	{
/* class_name			*/	"graphicStyleClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlGraphicStyleLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlstyleClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	GraphicStyleClassPartInitialize,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	GraphicStyleInitialize,
/* layer_set_values		*/	GraphicStyleSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	GraphicStyleGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	GraphicStyleDestroy,

/* child_resources		*/	NULL,
/* layer_draw			*/      NULL,
/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL
	},
	{
					NULL
	},
	{
					NULL
	}
};

NhlClass NhlgraphicStyleClass = 
			(NhlClass)&NhlgraphicStyleClassRec;

static NrmQuark Qcolors;
static NrmQuark Qsegments;

/*
 * Function:	nhlfgraphicStyleclass
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
_NHLCALLF(nhlfgraphicstyleclass,NHLFGRAPHICSTYLECLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlgraphicStyleClass;
}


/*
 * Function:	GraphicStyleClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		NhlGraphicStyleClassPart that cannot be initialized statically.
 *              Registers child classes.
 *
 * In Args:	
 *		NhlClass	lc	NhlLayer Class to init
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

static NhlErrorTypes
GraphicStyleClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	NhlClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;
	char		*entry_name = "GraphicStyleClassPartInitialize";

	Qcolors = NrmStringToQuark(NhlNgsColors);
	Qsegments = NrmStringToQuark(NhlNgsSegments);

/*
 * Register children objects
 */
	subret = _NhlRegisterChildClass(lc,NhllineStyleClass,
					True,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhllineStyleClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlfillStyleClass,
					True,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlfillStyleClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhlmarkerStyleClass,
					True,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhlmarkerStyleClass");
		return(NhlFATAL);
	}

	subret = _NhlRegisterChildClass(lc,NhltextStyleClass,
					True,False,NULL);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error registering %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  "NhltextStyleClass");
		return(NhlFATAL);
	}

	return ret;
}

/*
 * Function:	GraphicStyleInitialize
 *
 * Description:	
 *
 * In Args:	Standard initialize parameters
 *
 * Out Args:	NONE
 *
 * Return Values: Error condition
 *
 * Side Effects: 
 */
/*ARGSUSED*/
static NhlErrorTypes
GraphicStyleInitialize
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
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlGraphicStyleLayer	gsnew = (NhlGraphicStyleLayer) new;
	NhlGraphicStyleLayerPart	*gsp = &(gsnew->graphicstyle);
        char                    buffer[_NhlMAXRESNAMLEN];

	if (gsp->colors != NULL)
		gsp->colors = _NhlCopyGenArray(gsp->colors,True);
	if (gsp->segments != NULL)
		gsp->segments = _NhlCopyGenArray(gsp->segments,True);

/*
 * all the individual style objects get the same name as the parent
 */
	sprintf(buffer,"%s",gsnew->base.name);

	subret = _NhlALCreateChild(&gsp->line_style_id,buffer,
				   NhllineStyleClass,
				   (NhlLayer) gsnew, NULL,0);

	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = _NhlALCreateChild(&gsp->fill_style_id,buffer,
				   NhlfillStyleClass,
				   (NhlLayer) gsnew, NULL,0);

	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = _NhlALCreateChild(&gsp->marker_style_id,buffer,
				   NhlmarkerStyleClass,
				   (NhlLayer) gsnew, NULL,0);

	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	subret = _NhlALCreateChild(&gsp->text_style_id,buffer,
				   NhltextStyleClass,
				   (NhlLayer) gsnew, NULL,0);

	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	return ret;
}

/*
 * Function:	GraphicStyleSetValues
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
GraphicStyleSetValues
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
	NhlGraphicStyleLayer 	gsnew = (NhlGraphicStyleLayer) new;
	NhlGraphicStyleLayerPart	*gsp = &gsnew->graphicstyle;
	NhlGraphicStyleLayer 	gsold = (NhlGraphicStyleLayer) old;
	NhlGraphicStyleLayerPart	*ogsp = &gsold->graphicstyle;
	NhlGenArray		gen;
	NhlErrorTypes 		ret = NhlNOERROR;

	if (gsp->colors != ogsp->colors){
		gen = gsp->colors;
		gsp->colors = _NhlCopyGenArray(gen,True);
		if(gen && ! gsp->colors){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		else{
			NhlFreeGenArray(ogsp->colors);
		}
	}
	if (gsp->segments != ogsp->segments){
		gen = gsp->segments;
		gsp->segments = _NhlCopyGenArray(gen,True);
		if(gen && ! gsp->segments){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		else{
			NhlFreeGenArray(ogsp->segments);
		}
	}

	return ret;
}

/*
 * Function:	GraphicStyleDestroy
 *
 * Description: 
 *
 * In Args:	NhlLayer inst	instance of GraphicStyle
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */
static NhlErrorTypes
GraphicStyleDestroy
#if	NhlNeedProto
(
	NhlLayer	layer)
#else
(layer)
	NhlLayer	layer;
#endif
{
	NhlGraphicStyleLayer 	gs = (NhlGraphicStyleLayer) layer;
	NhlGraphicStyleLayerPart	*gsp = &(gs->graphicstyle);
	NhlErrorTypes		ret = NhlNOERROR;

	if (gsp->colors) 
		NhlFreeGenArray(gsp->colors);
	if (gsp->segments) 
		NhlFreeGenArray(gsp->segments);

	(void) _NhlDestroyChild(gsp->line_style_id,layer);
	(void) _NhlDestroyChild(gsp->fill_style_id,layer);
	(void) _NhlDestroyChild(gsp->marker_style_id,layer);
	(void) _NhlDestroyChild(gsp->text_style_id,layer);

	return ret;
}

/*
 * Function:    GraphicStyleGetValues
 *
 * Description: Retrieves the current setting of one or more GraphicStyle
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
 *              NhlNgsSegments
 *              NhlNgsColors
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    GraphicStyleGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlGraphicStyleLayer gsl = (NhlGraphicStyleLayer)l;
        NhlGraphicStyleLayerPart *gsp = &(gsl->graphicstyle);
        NhlGenArray ga;
        char *e_text;
        int i;

        for( i = 0; i< num_args; i++ ) {

                ga = NULL;
                if(args[i].quark == Qcolors) {
                        ga = gsp->colors;
                }
                if(args[i].quark == Qsegments) {
                        ga = gsp->segments;
                }
                if (ga != NULL) {
                        if ((ga = _NhlCopyGenArray(ga,True)) == NULL) {
                                e_text = "%s: error copying %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          "GraphicStyleGetValues",
					  NrmQuarkToString(args[i].quark));
                                return NhlFATAL;
                        }
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
			continue;
                }
        }

        return(NhlNOERROR);
}

