/*
 *      $Id: NcgmWorkstation.c,v 1.1 1993-04-30 17:23:16 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		NcgmWorkstation.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 15 10:00:09 MDT 1992
 *
 *	Description:	Responsible for managing the NCGM workstation element
 */

#include <stdio.h>
#include <strings.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NcgmWorkstationP.h>

#define DEFAULT_META_NAME "gmeta"

static NhlResource resources[] = {
	{ NhlNwkMetaName, NhlCwkMetaName, NhlTString, sizeof(char*),
	NhlOffset(NcgmWorkstationLayerRec,ncgm.meta_name),NhlTString,DEFAULT_META_NAME }
};

/*
* NcgmWorkstation base_class method declarations
*/

static NhlErrorTypes NcgmWorkstationInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,        /* args */
        int             /* num_args */
#endif
);


static NhlErrorTypes NcgmWorkstationClassPartInitialize(
#ifdef NhlNeedProto
        LayerClass      /* lc */
#endif
);

static NhlErrorTypes NcgmWorkstationDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes NcgmWorkstationSetValues(
#ifdef NhlNeedProto
        Layer,		/* old */
        Layer,		/* reference */
        Layer,		/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

/*
* NcgmWorkstation work_class method declarations
*/

static NhlErrorTypes NcgmWorkstationOpen(
#ifdef NhlNeedProto
	Layer /* instance */
#endif
);

/*
* A pointer to this is assigned to the cgm_inited field of this
* class. this pointer is then propagated down the subclass heirarchy
* with the class part initialize function.
*/
static NcgmStatus ncgm_is_initialized = UNINITED;
static int default_conid = NCGM_DEFAULT_CONID;

NcgmWorkstationLayerClassRec ncgmWorkstationLayerClassRec = {
        {
/* superclass			*/	(LayerClass)&workstationLayerClassRec,
/* class_name			*/	"NcgmWorkstation",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NcgmWorkstationLayerRec),
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NcgmWorkstationClassPartInitialize,
/* class_inited			*/	False,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NcgmWorkstationInitialize,
/* layer_set_values		*/	NcgmWorkstationSetValues,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	NcgmWorkstationDestroy
        },
        {
/* open_work		*/	NcgmWorkstationOpen,
/* close_work		*/	NULL,
/* activate_work	*/	NULL,
/* deactivate_work	*/	NULL,
/* update_work		*/	NhlInheritUpdate,
/* clear_work		*/	NhlInheritClear
	},
	{
/* cgm_inited	*/	&ncgm_is_initialized
	}
};

LayerClass ncgmWorkstationLayerClass = (LayerClass)&ncgmWorkstationLayerClassRec;



/*
 * Function:	NcgmWorkstationInitialize
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes NcgmWorkstationInitialize
#if __STDC__
(LayerClass class, Layer req, Layer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        LayerClass class;
        Layer req;
        Layer new;
        _NhlArgList args;
        int num_args; 
#endif
{
	NcgmWorkstationLayerClass wclass = (NcgmWorkstationLayerClass)class;
	NcgmWorkstationLayer	wnew = (NcgmWorkstationLayer) new;
	NcgmWorkstationLayer	wreq = (NcgmWorkstationLayer) req;



	/* SUPPRESS 112 */
	if(strcmp(wreq->ncgm.meta_name,DEFAULT_META_NAME) != 0) {
		/* SUPPRESS 112 */
		wnew->ncgm.meta_name =
			(char*)NhlMalloc(strlen(wreq->ncgm.meta_name) + 1);
		/* SUPPRESS 112 */
		strcpy(wnew->ncgm.meta_name,wreq->ncgm.meta_name);
	}
	if(*(wclass->ncgm_class.cgm_inited) == UNINITED){
		wnew->work.gkswkstype = NCGM_WORKSTATION_TYPE;
		wnew->work.gkswksconid = default_conid++;
		*(wclass->ncgm_class.cgm_inited) = INITED;
		return(NOERROR);
	} else if( *(wclass->ncgm_class.cgm_inited) == INITED) {
		NhlPError(FATAL,E_UNKNOWN,"NcgmWorkstationInitialize: Only one NCGM workstation is allowed by NCAR Graphics");
		return(FATAL);
	} 
}


/*
 * Function:	NcgmWorkstationClassPartInitialize
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes NcgmWorkstationClassPartInitialize
#if	__STDC__
(LayerClass lc)
#else
(lc)
	LayerClass lc;
#endif
{
	NcgmWorkstationLayerClass wlc = (NcgmWorkstationLayerClass)lc;
	LayerClass	sc = wlc->base_class.superclass;

	if(sc != (LayerClass)&workstationLayerClassRec) {
		wlc->ncgm_class.cgm_inited = 
			((NcgmWorkstationLayerClass)sc)->ncgm_class.cgm_inited;
	}
	return(NOERROR);
}

/*
 * Function:    NcgmWorkstationDestroy
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes NcgmWorkstationDestroy
#if     __STDC__
(Layer inst)
#else
(inst)
        Layer inst;
#endif
{
	NcgmWorkstationLayer winst = (NcgmWorkstationLayer)inst;

	if(strcmp(winst->ncgm.meta_name,DEFAULT_META_NAME) != 0)
		NhlFree(winst->ncgm.meta_name);
	return(NOERROR);
}


/*
 * Function:	NcgmWorkstationSetValues
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes NcgmWorkstationSetValues
#if __STDC__
(Layer old, Layer reference, Layer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
        Layer old;
        Layer reference;
        Layer new; 
        _NhlArgList args;
        int num_args;
#endif
{
	NcgmWorkstationLayer wold = (NcgmWorkstationLayer) old;
	NcgmWorkstationLayer wnew = (NcgmWorkstationLayer) new;

	if(wnew->ncgm.meta_name != wold->ncgm.meta_name ) {
		NhlPError(WARNING,E_UNKNOWN,"NcgmWorkstationSetValues: metafile name cannot change after initialization");
		return(WARNING);
	}
	return(NOERROR);
}

/*
 * Function:	NcgmWorkstationOpen
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes NcgmWorkstationOpen
#if __STDC__
(Layer instance)
#else
(instance)
	Layer instance;
#endif
{
	Gescape_in_data indat;
	Gescape_out_data *outdat;
	NcgmWorkstationLayer winstance = (NcgmWorkstationLayer) instance;

	indat.escape_r1.size = strlen(winstance->ncgm.meta_name) + 1;
	indat.escape_r1.data = (void*)winstance->ncgm.meta_name;
	
	gescape(-1391,&indat,NULL,&outdat);

	return(NOERROR);
}
