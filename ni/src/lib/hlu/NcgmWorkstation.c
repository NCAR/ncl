/*
 *      $Id: NcgmWorkstation.c,v 1.19 1995-12-19 20:39:19 boote Exp $
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
#include <string.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NcgmWorkstationP.h>

#define DEFAULT_META_NAME	"gmeta"

static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNwkMetaName, NhlCwkMetaName, NhlTString, sizeof(char*),
	NhlOffset(NhlNcgmWorkstationLayerRec,ncgm.meta_name),NhlTString,
		NULL,0,(NhlFreeFunc)NhlFree }

/* End-documented-resources */

};

/*
* NcgmWorkstation base_class method declarations
*/

static NhlErrorTypes NcgmWorkstationInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,        /* args */
        int             /* num_args */
#endif
);


static NhlErrorTypes NcgmWorkstationClassPartInitialize(
#if	NhlNeedProto
        NhlClass      /* lc */
#endif
);

static NhlErrorTypes NcgmWorkstationDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes NcgmWorkstationSetValues(
#if	NhlNeedProto
        NhlLayer,		/* old */
        NhlLayer,		/* reference */
        NhlLayer,		/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

static NhlErrorTypes NcgmWorkstationGetValues(
#if	NhlNeedProto
	NhlLayer, /*l */
	_NhlArgList, /* args */
	int	/*nargs*/
#endif
);

/*
* NcgmWorkstation work_class method declarations
*/

static NhlErrorTypes NcgmWorkstationOpen(
#if	NhlNeedProto
	NhlLayer
#endif
);

/*
* A pointer to this is assigned to the cgm_inited field of this
* class. this pointer is then propagated down the subclass heirarchy
* with the class part initialize function.
*/
static _NhlNcgmStatus ncgm_is_initialized = _NhlUNINITED;

NhlNcgmWorkstationClassRec NhlncgmWorkstationClassRec = {
        {
/* class_name			*/	"ncgmWorkstationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlNcgmWorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlworkstationClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NcgmWorkstationClassPartInitialize,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NcgmWorkstationInitialize,
/* layer_set_values		*/	NcgmWorkstationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NcgmWorkstationDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
        },
        {
/* def_background	*/	{0.0,0.0,0.0},
/* open_work		*/	NcgmWorkstationOpen,
/* close_work		*/	NhlInheritClose,
/* activate_work	*/	NhlInheritActivate,
/* deactivate_work	*/	NhlInheritDeactivate,
/* alloc_colors		*/	NhlInheritAllocateColors,
/* update_work		*/	NhlInheritUpdate,
/* clear_work		*/	NhlInheritClear,
/* lineto_work		*/	NhlInheritLineTo,
/* fill_work		*/	NhlInheritFill,
/* marker_work		*/	NhlInheritMarker
	},
	{
/* cgm_inited	*/	&ncgm_is_initialized
	}
};

NhlClass NhlncgmWorkstationClass = (NhlClass)&NhlncgmWorkstationClassRec;

/*
 * Function:	nhlfncgmworkstationclass
 *
 * Description:	fortran ref to this class
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
_NHLCALLF(nhlfncgmworkstationclass,NHLFNCGMWORKSTATIONCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlncgmWorkstationClass;
}



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
#if	NhlNeedProto
(NhlClass class, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        NhlClass class;
        NhlLayer req;
        NhlLayer new;
        _NhlArgList args;
        int num_args; 
#endif
{
	char				func[] = "NcgmWorkstationInitialize";
	int				default_conid = NCGM_DEFAULT_CONID;
	NhlNcgmWorkstationClass	wclass=
					(NhlNcgmWorkstationClass)class;
	NhlNcgmWorkstationLayer		wnew = (NhlNcgmWorkstationLayer)new;
	NhlNcgmWorkstationLayerPart	*np = &wnew->ncgm;
	char				*tfname = NULL;
	NhlErrorTypes			ret = NhlNOERROR;

	if(np->meta_name){
		tfname = (char*)_NGResolvePath(np->meta_name);
		if(!tfname){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:Unable to resolve path name for \"%s\", defaulting %s",
				func,np->meta_name,NhlNwkMetaName);
			ret = NhlWARNING;
		}
	}

	if(!tfname){
		tfname = DEFAULT_META_NAME;
	}

	np->meta_name = (char*)NhlMalloc(strlen(tfname) + 1);
	strcpy(np->meta_name,tfname);

	while(1){
		int	opn, ierr;
		_NHLCALLF(nhl_finqunit,NHL_FINQUNIT)(&default_conid,&opn,&ierr);
		if(!opn)
			break;
		default_conid++;
	}

	if(*(wclass->ncgm_class.cgm_inited) == _NhlUNINITED){
		wnew->work.gkswkstype = NCGM_WORKSTATION_TYPE;
		wnew->work.gkswksconid = default_conid;
		*(wclass->ncgm_class.cgm_inited) = _NhlINITED;
		return ret;
	}
	else{
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Only one NCGMWorkstation is allowed",func);
		return NhlFATAL;
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
#if	NhlNeedProto
(NhlClass lc)
#else
(lc)
	NhlClass lc;
#endif
{
	NhlNcgmWorkstationClass wlc = (NhlNcgmWorkstationClass)lc;
	NhlClass	sc = wlc->base_class.superclass;

	if(sc != (NhlClass)&NhlworkstationClassRec) {
		wlc->ncgm_class.cgm_inited = 
			((NhlNcgmWorkstationClass)sc)->ncgm_class.cgm_inited;
	}
	return(NhlNOERROR);
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
#if	NhlNeedProto
(NhlLayer inst)
#else
(inst)
        NhlLayer inst;
#endif
{
	NhlNcgmWorkstationLayer winst = (NhlNcgmWorkstationLayer)inst;
	NhlNcgmWorkstationClass wclass = (NhlNcgmWorkstationClass)inst->base.layer_class;

	NhlFree(winst->ncgm.meta_name);
	*(wclass->ncgm_class.cgm_inited) = _NhlUNINITED;
	return(NhlNOERROR);
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
#if	NhlNeedProto
(NhlLayer old, NhlLayer reference, NhlLayer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
        NhlLayer old;
        NhlLayer reference;
        NhlLayer new; 
        _NhlArgList args;
        int num_args;
#endif
{
	NhlNcgmWorkstationLayer wold = (NhlNcgmWorkstationLayer) old;
	NhlNcgmWorkstationLayer wnew = (NhlNcgmWorkstationLayer) new;

	if(wnew->ncgm.meta_name != wold->ncgm.meta_name ) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"NcgmWorkstationSetValues: metafile name cannot change after initialization");
		wnew->ncgm.meta_name = wold->ncgm.meta_name;
		return(NhlWARNING);
	}
	return(NhlNOERROR);
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
static NhlErrorTypes
NcgmWorkstationOpen
#if	NhlNeedProto
(
	NhlLayer	instance
)
#else
(instance)
	NhlLayer	instance;
#endif
{
	Gescape_in_data indat;
	Gescape_out_data *outdat;
	NhlNcgmWorkstationLayer winstance = (NhlNcgmWorkstationLayer) instance;

	indat.escape_r1.size = strlen(winstance->ncgm.meta_name) + 1;
	indat.escape_r1.data = (void*)winstance->ncgm.meta_name;
	
	gescape(-1391,&indat,NULL,&outdat);

	return (*NhlworkstationClassRec.work_class.open_work)(instance);
}
static NhlErrorTypes NcgmWorkstationGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int nargs)
#else
(l, args, nargs)
NhlLayer l;
_NhlArgList args;
int nargs;
#endif
{
	int i = 0;
	NrmQuark QMetaName = NrmStringToQuark(NhlNwkMetaName);
	NhlNcgmWorkstationLayerPart * ncwk = & ((NhlNcgmWorkstationLayer)l)->ncgm;
	char *e_text;

	for(i = 0; i < nargs; i++) {
		if(args[i].quark == QMetaName) {
			if(ncwk->meta_name != NULL) {
				*((NhlString*)(args[i].value.ptrval)) = NhlMalloc(strlen(ncwk->meta_name)+1);
				if(*((NhlString*)(args[i].value.ptrval)) != NULL) {
					strcpy(*((NhlString*)(args[i].value.ptrval)),ncwk->meta_name);
				} else {
					e_text = "%s: error copying String";
					NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						"NcgmWorkStationGetValues");
					return NhlFATAL;
				}
			} else {
				*((NhlString*)(args[i].value.ptrval)) = NULL;
			}
		}
	}
	return(NhlNOERROR); 
}
