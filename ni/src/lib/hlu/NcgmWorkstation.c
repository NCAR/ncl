/*
 *      $Id: NcgmWorkstation.c,v 1.31 1998-02-27 23:16:01 dbrown Exp $
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
          NULL,0,(NhlFreeFunc)NhlFree },
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
	NhlLayer	l
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
/* current_wks_count	*/	NhlInheritCurrentWksCount,                
/* def_background	*/	{0.0,0.0,0.0},
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
/* marker_work		*/	NhlInheritMarker
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

	np->meta_name = (char*)NhlMalloc(strlen(tfname) + 1);
	strcpy(np->meta_name,tfname);

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
	NhlClass	sc = wlc->base_class.superclass;

	wlc->ncgm_class.current_ncgm_wkid = NhlNULLOBJID;

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

#if DEBUG_NCGM
	fprintf(stderr,"calling ngmftc for %s\n",wl->ncgm.meta_name);
#endif
	c_ngmftc(wl->work.gkswksid);
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;

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
	int i = 3;

	if (wlc->ncgm_class.current_ncgm_wkid == NhlNULLOBJID) {
		new = True;
	}
	else if (wlc->ncgm_class.current_ncgm_wkid != l->base.id) {
	        subret = TempClose(
			      _NhlGetLayer(wlc->ncgm_class.current_ncgm_wkid),
				   func);
		if ((retcode = MIN(retcode,subret)) < NhlWARNING)
			return retcode;
		new = True;
	}
	if (new) {
		int action = 1;
		while(wksisopn(i)) {
			i++;
		}
		wl->work.gkswksid = i;
		if (np->new_frame) {
			action = 2;
			np->new_frame = False;
		}
                                
#if DEBUG_NCGM
	fprintf(stderr,"calling ngreop iop %d for %s\n",2,np->meta_name);
#endif
		c_ngreop(wl->work.gkswksid,wl->work.gkswksconid,1,
			 np->meta_name,2,
			 np->gks_iat,np->gks_rat,0,0,NULL);
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
	static int first = True;

	indat.escape_r1.size = strlen(winstance->ncgm.meta_name) + 1;
	indat.escape_r1.data = (void*)winstance->ncgm.meta_name;
	
	gescape(-1391,&indat,NULL,&outdat);

	if (wlc->ncgm_class.current_ncgm_wkid != instance->base.id &&
	    wlc->ncgm_class.current_ncgm_wkid != NhlNULLOBJID) {
	        subret = TempClose(
			      _NhlGetLayer(wlc->ncgm_class.current_ncgm_wkid),
				   func);
		if ((retcode = MIN(retcode,subret)) < NhlWARNING)
			return retcode;
	}
	wlc->ncgm_class.current_ncgm_wkid = instance->base.id;
	winstance->ncgm.opened = True;
	winstance->ncgm.started = False;
	winstance->ncgm.new_frame = True;
	winstance->ncgm.update_colors = False;

	subret = (*NhlworkstationClassRec.work_class.open_work)(instance);
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
	NhlNcgmWorkstationClass wlc = 
		(NhlNcgmWorkstationClass)l->base.layer_class;
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
		_NhlAllocateColors(l);
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
	NhlLayer	l	/* workstation layer to update	*/
)
#else
(l)
	NhlLayer	l;	/* workstation layer to update	*/
#endif
{
	NhlNcgmWorkstationLayer	wl = (NhlNcgmWorkstationLayer)l;
	char			func[] = "NcgmWorkstationAllocateColors";
	NhlNcgmWorkstationClass wlc = 
		(NhlNcgmWorkstationClass)l->base.layer_class;
	NhlNcgmWorkstationLayerPart	*np = &wl->ncgm;
	NhlErrorTypes subret = NhlNOERROR,retcode = NhlNOERROR;

 	if (! np->opened) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		    "%s: attempt to allocate colors for unopened workstation",
			  func);
		return NhlFATAL;
	}

	subret = UpdateGKSState(l,func);
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
		NhlPrivateColor	*pcmap;
		int		i;

		pcmap = wl->work.private_color_map;
		for (i = 0; i < _NhlMAX_COLOR_MAP; i++) {
			if (pcmap[i].cstat == _NhlCOLSET)
				pcmap[i].cstat = _NhlCOLCHANGE;
		}
		np->update_colors = False;
		wl->work.cmap_changed = True;
	}
	subret = (*NhlworkstationClassRec.work_class.alloc_colors)(l);
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
	NhlNcgmWorkstationClass wlc = 
		(NhlNcgmWorkstationClass)l->base.layer_class;
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
                subret = TempClose(_NhlGetLayer
				   (wlc->ncgm_class.current_ncgm_wkid),func);
		retcode = MIN(retcode,subret);
		wlc->ncgm_class.current_ncgm_wkid = NhlNULLOBJID;
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
	NhlNcgmWorkstationClass wlc = 
		(NhlNcgmWorkstationClass)l->base.layer_class;
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
	subret = TempClose(_NhlGetLayer(wlc->ncgm_class.current_ncgm_wkid),
			   func);
	retcode = MIN(retcode,subret);
	wlc->ncgm_class.current_ncgm_wkid = NhlNULLOBJID;
	np->new_frame = True;
	np->update_colors = True;

	return retcode;
}



