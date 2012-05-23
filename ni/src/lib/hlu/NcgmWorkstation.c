/*
 *      $Id: NcgmWorkstation.c,v 1.40 2003-11-25 22:41:18 dbrown Exp $
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

#define DEBUG_NCGM 0
#define DEFAULT_META_NAME	"gmeta"

static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNwkMetaName, NhlCwkMetaName, NhlTString, sizeof(char*),
          NhlOffset(NhlNcgmWorkstationLayerRec,ncgm.meta_name),NhlTString,
          {NULL},0,(NhlFreeFunc)NhlFree },
	{NhlNwkVSWidthDevUnits,NhlCwkVSWidthDevUnits,NhlTInteger,sizeof(int),
          NhlOffset(NhlNcgmWorkstationLayerRec,work.vswidth_dev_units),
          NhlTImmediate,_NhlUSET((NhlPointer)32767),_NhlRES_GONLY,NULL}

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

static NhlErrorTypes NcgmWorkstationClose(
#if	NhlNeedProto
	NhlLayer
#endif
);


static NhlErrorTypes NcgmWorkstationActivate(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);

static NhlErrorTypes NcgmWorkstationDeactivate(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);


static NhlErrorTypes NcgmWorkstationAllocateColors(
#if	NhlNeedProto
	NhlWorkstationLayer	wl,
	NhlPrivateColor		*new,
	NhlPrivateColor		*old
#endif
);

static NhlErrorTypes NcgmWorkstationUpdate(
#if	NhlNeedProto
	NhlLayer	l	/* instance	*/
#endif
);

static NhlErrorTypes NcgmWorkstationClear(
#if	NhlNeedProto
	NhlLayer	l	/* instance	*/
#endif
);

static void NcgmWorkstationNotify(
#if	NhlNeedProto
	NhlLayer	l,	/* instance	*/
        int		action
#endif
);


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
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NcgmWorkstationClassPartInitialize,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NcgmWorkstationInitialize,
/* layer_set_values		*/	NcgmWorkstationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NcgmWorkstationGetValues,
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
/* current_wks_count	*/	NhlInheritCurrentWksCount,                
/* wks_hlu_ids		*/	NhlInheritGksWksRecs,
/* hlu_wks_flag		*/	NhlInheritHluWksFlag,
/* def_background	*/	{1.0,1.0,1.0},
/* rgb_dbm		*/	NULL,
/* pal			*/	NhlInheritPalette,
/* open_work		*/	NcgmWorkstationOpen,
/* close_work		*/	NcgmWorkstationClose,
/* activate_work	*/	NcgmWorkstationActivate,
/* deactivate_work	*/	NcgmWorkstationDeactivate,
/* alloc_colors		*/	NcgmWorkstationAllocateColors,
/* update_work		*/	NcgmWorkstationUpdate,
/* clear_work		*/	NcgmWorkstationClear,
/* lineto_work		*/	NhlInheritLineTo,
/* fill_work		*/	NhlInheritFill,
/* marker_work		*/	NhlInheritMarker,
/* notify_work		*/	NcgmWorkstationNotify,
/* update_drawbb        */      NULL
	},
	{
/* current_ncgm_wkid	*/	NhlNULLOBJID
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
	if (strlen(tfname) > _NhlMAXLLUPATHLEN) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: Filepath %s exceeds maximum length of %d", func,
			  tfname,_NhlMAXLLUPATHLEN);
		return NhlFATAL;
	}

        if (wclass->ncgm_class.last_base_meta_name &&
            ! strcmp(tfname,wclass->ncgm_class.last_base_meta_name)) {
                char suffix[8];
                np->suffix = ++wclass->ncgm_class.base_meta_name_count;
                
                sprintf(suffix,"%d",np->suffix);
                np->meta_name = (char*)NhlMalloc
                        (strlen(tfname) + strlen(suffix) + 1);
                sprintf(np->meta_name,"%s%s",tfname,suffix);
        }
        else {
                np->suffix = 0;
                np->meta_name = (char*)NhlMalloc(strlen(tfname) + 1);
                strcpy(np->meta_name,tfname);
                wclass->ncgm_class.base_meta_name_count = 0;
                if (wclass->ncgm_class.last_base_meta_name)
                        NhlFree(wclass->ncgm_class.last_base_meta_name);
                wclass->ncgm_class.last_base_meta_name =
                        (char*)NhlMalloc(strlen(tfname) + 1);
                strcpy(wclass->ncgm_class.last_base_meta_name,tfname);
        }

	while(1){
		int	opn, ierr;
		_NHLCALLF(nhlpfinqunit,NHLPFINQUNIT)
			(&default_conid,&opn,&ierr);
		if(!opn)
			break;
		default_conid++;
	}

	wnew->work.gkswkstype = NCGM_WORKSTATION_TYPE;
	wnew->work.gkswksconid = default_conid;

	np->opened = False;

	return ret;
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

	wlc->ncgm_class.current_ncgm_wkid = NhlNULLOBJID;
        wlc->ncgm_class.last_base_meta_name = NULL;
        wlc->ncgm_class.base_meta_name_count = 0;

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

            /* unreserve the name associated with the workstation being
               destroyed */
        
        if (wclass->ncgm_class.last_base_meta_name &&
            ! strncmp(winst->ncgm.meta_name,
                      wclass->ncgm_class.last_base_meta_name,
                      strlen(wclass->ncgm_class.last_base_meta_name))) {
                
                if (wclass->ncgm_class.base_meta_name_count == 0) {
                        NhlFree(wclass->ncgm_class.last_base_meta_name);
                        wclass->ncgm_class.last_base_meta_name = NULL;
                }
                
                if (winst->ncgm.suffix == 
                    wclass->ncgm_class.base_meta_name_count &&
                    wclass->ncgm_class.base_meta_name_count > 0) {
                        wclass->ncgm_class.base_meta_name_count--;
                }
        }
	NhlFree(winst->ncgm.meta_name);

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

static NhlErrorTypes
TempClose
#if NhlNeedProto
(
	NhlLayer	l,
	NhlString	func
)
#else
(l,func)
	NhlLayer	l;
	NhlString	func;
#endif
{
	NhlNcgmWorkstationLayer		wl = (NhlNcgmWorkstationLayer)l;
	NhlNcgmWorkstationClass wlc = 
		(NhlNcgmWorkstationClass)l->base.layer_class;

#if DEBUG_NCGM
	fprintf(stderr,"calling ngmftc: hlu_id %d; gkswkid %d; metafile %s\n",
                l->base.id,wl->work.gkswksid,wl->ncgm.meta_name);
#endif
	c_ngmftc(wl->work.gkswksid);
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;
        wlc->ncgm_class.current_ncgm_wkid = NhlNULLOBJID;

	return NhlNOERROR;
}

static NhlErrorTypes
UpdateGKSState
#if NhlNeedProto
(
	NhlLayer	l,
	NhlString	func
)
#else
(l,func)
	NhlLayer	l;
	NhlString	func;
#endif
{
	NhlNcgmWorkstationLayer	wl = (NhlNcgmWorkstationLayer)l;
	NhlNcgmWorkstationClass wlc = 
		(NhlNcgmWorkstationClass)l->base.layer_class;
	NhlNcgmWorkstationLayerPart	*np = &wl->ncgm;
	NhlErrorTypes subret = NhlNOERROR,retcode= NhlNOERROR;
	NhlBoolean new = False;
	int i;
	Gcolr_rep ctab[_NhlMAX_COLOR_MAP];

	if (wlc->ncgm_class.current_ncgm_wkid == NhlNULLOBJID) {
		new = True;
	}
	else if (wlc->ncgm_class.current_ncgm_wkid != l->base.id) {
	        subret = TempClose
                        (_NhlGetLayer(wlc->ncgm_class.current_ncgm_wkid),func);
		if ((retcode = MIN(retcode,subret)) < NhlWARNING)
			return retcode;
		new = True;
	}
	if (new) {
		if (np->new_frame) {
			np->new_frame = False;
		}
                                
#if DEBUG_NCGM
	fprintf(stderr,"calling ngreop: hlu_id %d;  gkswkid %d; metafile %s\n",
                l->base.id,wl->work.gkswksid,np->meta_name);
#endif
		for( i = 0; i < wl->work.color_map_len; i++) {
			ctab[i].rgb.red = wl->work.private_color_map[i].red;
			ctab[i].rgb.green = wl->work.private_color_map[i].green;
			ctab[i].rgb.blue = wl->work.private_color_map[i].blue;
		}
		c_ngreop(wl->work.gkswksid,wl->work.gkswksconid,1,
			 np->meta_name,1,
			 np->gks_iat,np->gks_rat,wl->work.color_map_len,0,ctab);
		if(_NhlLLErrCheckPrnt(NhlFATAL,func))
			return NhlFATAL;
		wlc->ncgm_class.current_ncgm_wkid = l->base.id;
	}

	return retcode;
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
	char			func[] = "NcgmWorkstationOpen";
	NhlNcgmWorkstationLayer winstance = (NhlNcgmWorkstationLayer) instance;
	NhlNcgmWorkstationClass wlc = 
		(NhlNcgmWorkstationClass)instance->base.layer_class;
	NhlErrorTypes subret = NhlNOERROR,retcode= NhlNOERROR;

	indat.escape_r1.size = strlen(winstance->ncgm.meta_name) + 1;
	indat.escape_r1.data = (void*)winstance->ncgm.meta_name;
	
	gescape(-1391,&indat,NULL,&outdat);

	if (wlc->ncgm_class.current_ncgm_wkid != instance->base.id &&
	    wlc->ncgm_class.current_ncgm_wkid != NhlNULLOBJID) {
	        subret = TempClose
                        (_NhlGetLayer(wlc->ncgm_class.current_ncgm_wkid),func);
		if ((retcode = MIN(retcode,subret)) < NhlWARNING)
			return retcode;
	}
	wlc->ncgm_class.current_ncgm_wkid = instance->base.id;
	winstance->ncgm.started = False;
	winstance->ncgm.new_frame = True;
	winstance->ncgm.update_colors = False;
	winstance->ncgm.opened = True;

	subret = (*NhlworkstationClassRec.work_class.open_work)(instance);
        if (subret < NhlWARNING) {
                winstance->ncgm.opened = False;
                return NhlFATAL;
        }
        
        
#if DEBUG_NCGM
	fprintf(stderr,"opened metafile %s\n",winstance->ncgm.meta_name);
#endif

/*
 * initialize the GKS state variables
 */
	c_ngsrat(2,winstance->ncgm.gks_iat,winstance->ncgm.gks_rat);

	
	return MIN(subret,retcode);
	
}
/*
 * Function:	NcgmWorkstationActivate
 *
 * Description:	WorkstationActivate activates the workdstation associated with
 *		this instance. If the workstation hasn't been initialize, which
 *		is next to impossible since NhlOpenWork is called from create,
 *		an error status is returned. Other wise the workstation is
 *		activated.
 *
 * In Args:	Takes just the instance
 *
 * Out Args:	Changes fields of the instance
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes
NcgmWorkstationActivate
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlNcgmWorkstationLayer	wl = (NhlNcgmWorkstationLayer)l;
	char			func[] = "NcgmWorkstationActivate";
	NhlNcgmWorkstationLayerPart	*np = &wl->ncgm;
	NhlErrorTypes subret = NhlNOERROR,retcode= NhlNOERROR;

	if (! np->opened) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: attempt to activate unopened workstation",func);
		return NhlFATAL;
	}

	subret = UpdateGKSState(l,func);
	if ((retcode = MIN(retcode,subret)) < NhlWARNING)
		return retcode;

	subret = (*NhlworkstationClassRec.work_class.activate_work)(l);
	if ((retcode = MIN(retcode,subret)) < NhlWARNING)
		return retcode;

	if (np->update_colors) {
		wl->work.cmap_changed = True;
		_NhlAllocateColors((NhlWorkstationLayer)l);
	}

	return retcode;
}

/*
 * Function:	NcgmWorkstationDeactivate
 *
 * Description:	Deactivates workstation. if not open NhlFATAL error if not
 *		active informational message.
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
NcgmWorkstationDeactivate
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	char			func[] = "NcgmWorkstationDeactivate";
	NhlNcgmWorkstationLayer	nl = (NhlNcgmWorkstationLayer)l;
	NhlNcgmWorkstationLayerPart	*np = &nl->ncgm;
	NhlErrorTypes subret = NhlNOERROR,retcode= NhlNOERROR;

 	if (! np->opened) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		       "%s: attempt to deactivate unopened workstation",func);
		return NhlFATAL;
	}

#if DEBUG_NCGM
	fprintf(stderr,"calling ngsrat for %s\n",np->meta_name);
#endif

        c_ngsrat(2,np->gks_iat,np->gks_rat);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func))
		retcode = NhlWARNING;
	subret = (*NhlworkstationClassRec.work_class.deactivate_work)(l);
	return MIN(subret,retcode);
}

/*
 * Function:	NcgmWorkstationClose
 *
 * Description:	Called before workstation destroy. This like Open is an "up-
 *		chained" method it is intended to allow subclasses to do things
 *		before the actual close. If workstation is not open or is 
 *		currently active an error message is provided.
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
NcgmWorkstationClose
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlNcgmWorkstationLayer	wl = (NhlNcgmWorkstationLayer)l;
	char			func[] = "NcgmWorkstationClose";
	NhlNcgmWorkstationClass wlc = 
		(NhlNcgmWorkstationClass)l->base.layer_class;
	NhlNcgmWorkstationLayerPart	*np = &wl->ncgm;
	NhlErrorTypes subret = NhlNOERROR,retcode = NhlNOERROR;

 	if (! np->opened) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		       "%s: attempt to close unopened workstation",func);
		return NhlFATAL;
	}

	subret = UpdateGKSState(l,func);
	if ((retcode = MIN(retcode,subret)) < NhlWARNING)
		return retcode;
		
#if DEBUG_NCGM
	fprintf(stderr,"closing metafile %s\n",np->meta_name);
#endif
	subret = (*NhlworkstationClassRec.work_class.close_work)(l);
	if ((retcode = MIN(retcode,subret)) < NhlWARNING)
		return retcode;
	np->opened = False;
	wlc->ncgm_class.current_ncgm_wkid = NhlNULLOBJID;

	return retcode;
}


/*
 * Function:	NcgmWorkstationAllocateColors
 *
 * Description:	This function is used to clear the workstation
 *
 * In Args:	
 *		NhlLayer	l	workstation layer to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
NcgmWorkstationAllocateColors
#if	NhlNeedProto
(
	NhlWorkstationLayer	workl,
	NhlPrivateColor		*new,
	NhlPrivateColor		*old
)
#else
(workl,new,old)
	NhlWorkstationLayer	workl;
	NhlPrivateColor		*new;
	NhlPrivateColor		*old;
#endif
{
	NhlNcgmWorkstationLayer	wl = (NhlNcgmWorkstationLayer)workl;
	char			func[] = "NcgmWorkstationAllocateColors";
	NhlNcgmWorkstationLayerPart	*np = &wl->ncgm;
	NhlErrorTypes subret = NhlNOERROR,retcode = NhlNOERROR;

 	if (! np->opened) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		    "%s: attempt to allocate colors for unopened workstation",
			  func);
		return NhlFATAL;
	}

	subret = UpdateGKSState((NhlLayer)workl,func);
	if ((retcode = MIN(retcode,subret)) < NhlWARNING)
		return retcode;

#if DEBUG_NCGM
	fprintf(stderr,"calling allocate colors\n");
#endif

/*
 * force a color map to be written at the beginning of each 
 * frame
 */
	if (np->update_colors) {
		int		i;

		for (i = 0; i < _NhlMAX_COLOR_MAP; i++) {
			if (new[i].cstat == _NhlCOLSET)
				new[i].cstat = _NhlCOLCHANGE;
		}
		np->update_colors = False;
	}
	subret = (*NhlworkstationClassRec.work_class.alloc_colors)
								(workl,new,old);
	retcode = MIN(retcode,subret);

	return retcode;
}

/*
 * Function:	NcgmWorkstationUpdate
 *
 * Description:	This function is used to clear the workstation
 *
 * In Args:	
 *		NhlLayer	l	workstation layer to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
NcgmWorkstationUpdate
#if	NhlNeedProto
(
	NhlLayer	l	/* workstation layer to update	*/
)
#else
(l)
	NhlLayer	l;	/* workstation layer to update	*/
#endif
{
	NhlNcgmWorkstationLayer	wl = (NhlNcgmWorkstationLayer)l;
	char			func[] = "NcgmWorkstationUpdate";
	NhlNcgmWorkstationLayerPart	*np = &wl->ncgm;
	NhlErrorTypes subret = NhlNOERROR,retcode = NhlNOERROR;

 	if (! np->opened) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		       "%s: attempt to update unopened workstation",func);
		return NhlFATAL;
	}


	subret = UpdateGKSState(l,func);
	if ((retcode = MIN(retcode,subret)) < NhlWARNING)
		return retcode;

#if DEBUG_NCGM
	fprintf(stderr,"calling update workstation\n");
#endif

	subret = (*NhlworkstationClassRec.work_class.update_work)(l);
	retcode = MIN(retcode,subret);

/*
 * close the ncgm after the initial update performed by the WorkstationOpen
 * call to avoid empty frames in certain situations. 
 */
	if (! np->started) {
                subret = TempClose(l,func);
		retcode = MIN(retcode,subret);
		np->new_frame = False;
		np->started = True;
	}

	return retcode;
}

/*
 * Function:	NcgmWorkstationClear
 *
 * Description:	This function is used to clear the workstation
 *
 * In Args:	
 *		NhlLayer	l	workstation layer to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
NcgmWorkstationClear
#if	NhlNeedProto
(
	NhlLayer	l	/* workstation layer to update	*/
)
#else
(l)
	NhlLayer	l;	/* workstation layer to update	*/
#endif
{
	NhlNcgmWorkstationLayer	wl = (NhlNcgmWorkstationLayer)l;
	char			func[] = "NcgmWorkstationClear";
	NhlNcgmWorkstationLayerPart	*np = &wl->ncgm;
	NhlErrorTypes subret = NhlNOERROR,retcode = NhlNOERROR;

 	if (! np->opened) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		       "%s: attempt to clear unopened workstation",func);
		return NhlFATAL;
	}

	subret = UpdateGKSState(l,func);
	if ((retcode = MIN(retcode,subret)) < NhlWARNING)
		return retcode;

#if DEBUG_NCGM
	fprintf(stderr,"calling gclrwk\n");
#endif

	subret = (*NhlworkstationClassRec.work_class.clear_work)(l);
	if ((retcode = MIN(retcode,subret)) < NhlWARNING)
		return retcode;

/*
 * always closs the ncgm on a clear to avoid empty frames
 */
	subret = TempClose(l,func);
	retcode = MIN(retcode,subret);
	np->new_frame = True;
	np->update_colors = True;

	return retcode;
}

/*
 * Function:	NcgmWorkstationNotify
 *
 * Description:	This function is used to notify a workstation about
 *              operations performed at the LLU level
 *
 *
 * In Args:	
 *		NhlLayer	l	workstation layer to update
 *              int             action  the operation performed
 *               _NhlwkLLUActivate   0
 *		 _NhlwkLLUDeactivate 1
 *		 _NhlwkLLUClear      2
 *		 _NhlwkLLUClose      3
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

static void NcgmWorkstationNotify(
#if	NhlNeedProto
	NhlLayer	l,	/* instance	*/
        int		action
#endif
        )
{
	NhlNcgmWorkstationLayer	wl = (NhlNcgmWorkstationLayer)l;
	char			func[] = "NcgmWorkstationNotify";
	Gctrl_flag cofl = GFLAG_ALWAYS;

        switch (action) {
            case _NhlwkLLUActivate:
                    UpdateGKSState(l,func);
                    _NHLCALLF(gzacwk,GZACWK)(&wl->work.gkswksid);
                    wl->work.cleared = False;
                    break;
            case _NhlwkLLUDeactivate:
                    UpdateGKSState(l,func);
                    _NHLCALLF(gzdawk,GZDAWK)(&wl->work.gkswksid);
                    TempClose(l,func);
                    break;
            case _NhlwkLLUClear:
                    UpdateGKSState(l,func);
                    _NHLCALLF(gzclrwk,GZCLRWK)(&wl->work.gkswksid,&cofl);
                    if (! wksisact(wl->work.gkswksid))
                            TempClose(l,func); 
                    wl->work.cleared = True;
                    break;
            case _NhlwkLLUClose:
                    NcgmWorkstationClose(l);
                    break;
        }
        return;
}

                    
                    


