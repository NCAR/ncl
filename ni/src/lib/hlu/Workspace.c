/*
 *      $Id: Workspace.c,v 1.7 1994-05-17 22:26:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Workspace.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Mar  9 12:59:39 MST 1994
 *
 *	Description:	Manages various kinds of workspaces used by the
 *			low level utilities, including scratch int and
 *			float workspaces, areamaps, and potentially 
 *			segments.
 *			Allows workspaces to be given various levels of
 *			persistence. Contains wrapper functions for all
 *			NCARG low-level routines that use workspaces; these
 *			routines automatically recover from calls to the
 *			Fortran layer with insufficient workspace sizes.
 *			The user is allowed to define a threshold overall
 *			workspace size, that if exceeded causes the 
 *			workspace object to begin dumping currently idle
 *			workspaces either by simple deallocation or by 
 *			writing to disk if the workspace has disk level 
 *			persistence. 
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/WorkspaceP.h>
#include <ncarg/hlu/Converters.h>

/* Method declarations	*/

static NhlErrorTypes WorkspaceClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes WorkspaceInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes WorkspaceSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes    WorkspaceGetValues(
#ifdef NhlNeedProto
        NhlLayer,       /* l */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes WorkspaceDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

/* private typedef */


/* private static function declarations */

static NhlWorkspaceRec	*wsFindNode(
#ifdef	NhlNeedProto
	int		ws_id,
	char		*entry_name
#endif
);

static void		wsDeleteNode(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);


static NhlErrorTypes	ReduceCurrentAlloc(
#ifdef	NhlNeedProto
	int		over,
	char		*entry_name
#endif
);

static NhlErrorTypes	GetScratchSpace(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	GetCorePreservedSpace(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	GetDiskPreservedSpace(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	EnlargeWorkspace(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	TrimWorkspace(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes AddToIdleList(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	NhlwsIdleRec	**head,
	NhlwsIdleRec	**tail,
	char		*entry_name
#endif
);

static NhlErrorTypes RemoveFromIdleList(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	NhlwsIdleRec	**head,
	NhlwsIdleRec	**tail,
	char		*entry_name
#endif
);

static NhlwsIdleRec	*GetIdleRec(
#ifdef	NhlNeedProto
	void
#endif
);

static void PutBackIdleRec(
#ifdef	NhlNeedProto
	NhlwsIdleRec	*idlep
#endif
);

static NhlErrorTypes	RestoreFromFile(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	SaveToFile(
#ifdef	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

/* Resources */

#define Oset(field)	NhlOffset(NhlWorkspaceLayerRec,workspace.field)
static NhlResource resources[] = {
	{NhlNwsMaximumSize,NhlCwsMaximumSize,NhlTLong,
		 sizeof(long),Oset(maximum_size),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlwsDEF_MAXIMUM),0,NULL},
	{NhlNwsThresholdSize,NhlCwsThresholdSize,NhlTLong,
		 sizeof(long),Oset(threshold_size),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlwsDEF_THRESHOLD),0,NULL},
	{NhlNwsCurrentSize,NhlCwsCurrentSize,NhlTLong,
		 sizeof(long),Oset(current_size),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),0,NULL}
};
#undef Oset
	
/* Class definition	*/

NhlWorkspaceLayerClassRec NhlworkspaceLayerClassRec = {
	/* BaseClassPart */
	{
/* class_name			*/	"Workspace",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlWorkspaceLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlobjLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	WorkspaceClassInitialize,
/* layer_initialize		*/	WorkspaceInitialize,
/* layer_set_values		*/	WorkspaceSetValues,
/* layer_set_values_hook	*/	WorkspaceSetValues,
/* layer_get_values		*/	WorkspaceGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	WorkspaceDestroy
	},
	
	/* WorkspaceClassPart */
	{
		
/* num_ws_instances		*/	0
	}		
		
};

NhlLayerClass NhlworkspaceLayerClass = (NhlLayerClass)
				&NhlworkspaceLayerClassRec;

/* private static data */

static int			WSInited = False;
static NhlWorkspaceLayer	WSLayer = NULL;
static NhlWorkspaceLayerPart	*WSp = NULL;


static NhlwsIdleRec	*ScratchHead = NULL;
static NhlwsIdleRec	*CoreHead = NULL;
static NhlwsIdleRec	*DiskHead = NULL;
static NhlwsIdleRec	*ScratchTail = NULL;
static NhlwsIdleRec	*CoreTail = NULL;
static NhlwsIdleRec	*DiskTail = NULL;
static NhlwsIdleRec	**IdleRecPool = NULL;
static int		NextIdleRec;
static int		IdleRecCount = 0;

static NrmQuark Qlabel_area_map = NrmNULLQUARK;
static NrmQuark Qezmap_area_map = NrmNULLQUARK;
static NrmQuark Qfill_area_map = NrmNULLQUARK;
static NrmQuark Qother_area_map = NrmNULLQUARK;
static NrmQuark Qsegment_data = NrmNULLQUARK;
static NrmQuark	Qconpack_int = NrmNULLQUARK;
static NrmQuark	Qconpack_float = NrmNULLQUARK;
static NrmQuark Qfloat = NrmNULLQUARK;
static NrmQuark Qint = NrmNULLQUARK;
static NrmQuark Qcurrent_size = NrmNULLQUARK;

/*
 * Function:	WorkspaceClassInitialize
 *
 * Description:	This function initializes the Workspace Class structure
 *
 * In Args:	none
 *
 * Out Args:	none
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkspaceClassInitialize
#if	__STDC__
(
	void
)
#else
()
#endif
{
	Qcurrent_size = NrmStringToQuark(NhlNwsCurrentSize);
	return NhlNOERROR;
}

/*
 * Function:	WorkspaceInitialize
 *
 * Description:	This function initializes an instance of an Workspace class
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
WorkspaceInitialize
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
	NhlErrorTypes		ret = NhlNOERROR;
        char			*e_text;
        char    	        *entry_name = "WorkspaceInitialize";
	NhlWorkspaceLayerClass	wsc = (NhlWorkspaceLayerClass)lc;
	NhlWorkspaceLayer	wsnew = (NhlWorkspaceLayer)new;
	NhlWorkspaceLayerPart	*wsp = &(wsnew->workspace);

	if (wsc->workspace_class.num_ws_instances > 0){
		e_text = 
		     "%s: only one instance of WorkspaceClass is supported"; 
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	wsc->workspace_class.num_ws_instances = 1;


	if (wsp->maximum_size < NhlwsMIN_MAXIMUM) {
		e_text = "%s: maximum size must be at least %d, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlwsMIN_MAXIMUM);
		wsp->maximum_size = NhlwsMIN_MAXIMUM;
	}

	if (wsp->threshold_size < NhlwsMIN_THRESHOLD) {
		e_text = "%s: threshold size must be at least %d, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlwsMIN_THRESHOLD);
		wsp->threshold_size = NhlwsMIN_THRESHOLD;
	}

	if (wsp->threshold_size > wsp->maximum_size) {
		e_text = 
		 "%s: threshold size cannot exceed maximum size: currently %d";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,wsp->maximum_size);
		wsp->threshold_size = wsp->maximum_size;
	}

	if (wsp->current_size != 0) {
		e_text = 
		"%s: attempt to set read-only resource %s ignored, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNwsCurrentSize);
		wsp->current_size = 0;
	}

	wsp->total_size = 0;
	wsp->last_ws_id = 0;
	wsp->ws_list = NULL;

	return(ret);
}

/*
 * Function:	WorkspaceSetValues
 *
 * Description:
 *
 * In Args:	
 *		NhlLayer	old,		old
 *		NhlLayer	req,		requested
 *		_NhlArgList	args,		args to set
 *		int		nargs		nargs
 *
 * Out Args:	
 *		NhlLayer	new,		new
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
WorkspaceSetValues
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
	NhlLayer	old;		/* old		*/
	NhlLayer	req;		/* requested	*/
	NhlLayer	new;		/* new		*/
	_NhlArgList	args;		/* args to set	*/
	int		nargs;		/* nargs	*/
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
        char			*e_text;
        char    	        *entry_name = "WorkspaceSetValues";
	NhlWorkspaceLayer	wsnew = (NhlWorkspaceLayer) new;
	NhlWorkspaceLayer	wsold = (NhlWorkspaceLayer) old;
	NhlWorkspaceLayerPart	*wsp = &(wsnew->workspace);

	if (wsp->threshold_size < NhlwsMIN_THRESHOLD) {
		e_text = "%s: threshold size must be at least %d, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlwsMIN_THRESHOLD);
		wsp->threshold_size = NhlwsMIN_THRESHOLD;
	}

	if (wsp->maximum_size < NhlwsMIN_MAXIMUM) {
		e_text = "%s: maximum size must be at least %d, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlwsMIN_MAXIMUM);
		wsp->maximum_size = NhlwsMIN_MAXIMUM;
	}

	if (wsp->threshold_size > wsp->maximum_size) {
		e_text = 
		 "%s: threshold size cannot exceed maximum size: currently %d";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,wsp->maximum_size);
		wsp->threshold_size = wsp->maximum_size;
	}

	if (wsp->current_size != wsold->workspace.current_size) {
		e_text = 
		"%s: attempt to set read-only resource %s ignored, defaulting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNwsCurrentSize);
		wsp->current_size = wsp->total_size;
	}
	return ret;
}

/*
 * Function:    WorkspaceGetValues
 *
 * Description: Retrieves the current size of all Workspaces managed
 *		by the Workspace object. 
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    WorkspaceGetValues
#if __STDC__
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     num_args;
#endif
{
        NhlWorkspaceLayer wsl = (NhlWorkspaceLayer) l;
        NhlWorkspaceLayerPart *wsp = &(wsl->workspace);
	int i;

        for (i = 0; i < num_args; i++) {

                if(args[i].quark == Qcurrent_size) 
			*((long *)(args[i].value.lngval)) = 
				wsp->current_size = wsp->total_size;
	}
#if 1 /* for debugging: */
	{
		NhlWorkspaceRec *wsrp;
		int		count = 0;
		for (wsrp = WSp->ws_list; wsrp != NULL; wsrp = wsrp->next) {
			if (wsrp->ws_ptr)
				count += wsrp->cur_size;
		}
		printf("the actual value is %d\n", count);
	}
#endif

	return NhlNOERROR;
}
/*
 * Function:	WorkspaceDestroy
 *
 * Description:	Destroys the Workspace object
 *		-- removes all members of the Workspace Record list
 *
 * In Args:	NhlLayer	l	The layer to destroy
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkspaceDestroy
#if	__STDC__
(
	NhlLayer l	/* layer to ready for destruction	*/
)
#else
(l)
	NhlLayer l;	/* layer to ready for destruction	*/
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlWorkspaceLayer	wsl = (NhlWorkspaceLayer) l;
	NhlWorkspaceLayerPart	*wsp = &(wsl->workspace);
	NhlWorkspaceRec		*wsrp, *twsrp;
	

	for (wsrp = wsp->ws_list; wsrp != NULL; wsrp = twsrp) {
		twsrp = wsrp->next;
		NhlFree(wsrp);
	}
	wsp->ws_list = NULL;
	return ret;
}


/*
 *	Private API for Workspace management
 */

/*
 * Function:	_NhlInitWorkspace
 *
 * Description:	This function initializes the Workspace object for the hlu
 *		library.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	
 * Side Effect:	
 */
void _NhlInitWorkspace
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
        char            *e_text;
        char            *entry_name = "_NhlInitWorkspace";
	int		pid;

	if (WSInited)
		return;

	ret = NhlVACreate(&pid,"workspace",
			  NhlworkspaceLayerClass,NhlNOPARENT,NULL);
	if (ret < NhlWARNING){
		e_text = "%s: error creating Workspace object";
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
		return;
	}

	WSLayer = (NhlWorkspaceLayer)_NhlGetLayer(pid);
	WSp = (NhlWorkspaceLayerPart *) &(WSLayer->workspace);
	WSInited = True;
	Qlabel_area_map = NrmStringToQuark(NhlTLabelAreaMap);
	Qezmap_area_map = NrmStringToQuark(NhlTEzmapAreaMap);
	Qfill_area_map = NrmStringToQuark(NhlTFillAreaMap);
	Qother_area_map = NrmStringToQuark(NhlTOtherAreaMap);
	Qsegment_data = NrmStringToQuark(NhlTSegmentData);
	Qconpack_float = NrmStringToQuark(NhlTConpackFloat);
	Qconpack_int = NrmStringToQuark(NhlTConpackInt);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qint = NrmStringToQuark(NhlTInteger);

	return;
}

/*
 * Function:	_NhlCloseWorkspace
 *
 * Description:	Destroys the Workspace layer. The static IdleRec list data
 *		is cleaned up, then freed and the Destroy method for the
 *		Workspace called
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	void
 * Side Effect:	
 */
void
_NhlCloseWorkspace
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlwsIdleRec	*idlep;
	int		i;
/*
 * Put all idle records back into the idle rec pool, then free it.
 */
	for (idlep = ScratchHead; idlep != NULL; idlep = idlep->next)
		PutBackIdleRec(idlep);
	for (idlep = CoreHead; idlep != NULL; idlep = idlep->next)
		PutBackIdleRec(idlep);
	for (idlep = DiskHead; idlep != NULL; idlep = idlep->next)
		PutBackIdleRec(idlep);
	for (i = 0; i < IdleRecCount; i++) {
		NhlFree(IdleRecPool[i]);
	}
	NhlFree(IdleRecPool);

	NhlDestroy(WSLayer->base.id);
	WSLayer = NULL;
	WSp = NULL;
	WSInited = False;
}

/*
 *	Public API for Workspace management
 */

/*
 * Function:	NhlGetWorkspaceObjectID
 *
 * Description:	This function returns the pid associated with the 
 *		workspaceLayer object, so that a user can set Workspace
 *		resources.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	The PID associated with workspaceLayer - Or NhlErrorTypes
 *		if workspaceLayer is unavailable.
 * Side Effect:	
 */
int
NhlGetWorkspaceObjectID
#if	__STDC__
(
	void
)
#else
()
#endif
{
        char            *e_text;
        char            *entry_name = "NhlWorkspaceGetID";

	if (WSInited && WSLayer != NULL)
		return WSLayer->base.id;

	e_text = "%s: Workspace object not found";
	NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
	return NhlFATAL;
}

/*
 * Function:	nhl_fgetworkspaceobjectid
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhl_fgetworkspaceobjectid,NHL_FGETWORKSPACEOBJECTID)
#if	NhlNeedProto
(
	int	*id
)
#else
(id)
	int	*id;
#endif
{
	*id = NhlGetWorkspaceObjectID();

	return;
}

/*
 *	Friend API for Workspace management
 */


/*
 * Function:	_NhlNewWorkspace
 *
 * Description:	Allocates a new Workspace record, and returns its id
 *		to the caller.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	A new Workspace ID or an NhlErrorType if unsuccessful
 * Side Effect:	
 */
int _NhlNewWorkspace
#if	__STDC__
(
	NhlString	type,
	NhlPersistence	persistence,			    
	int		req_size
)
#else
(type,persistence,req_size)
	NhlString	type;
	NhlPersistence	persistence;			    
	int		req_size;
	
#endif
{
        char            *e_text;
        char            *entry_name = "_NhlNewWorkspace";
	NhlWorkspaceRec	*wsrp;

	if (! WSInited || WSp == NULL) {
		e_text = "%s: Workspace object in invalid state";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return (int) NhlFATAL;
	}
	if ((wsrp = (NhlWorkspaceRec *) 
	     NhlMalloc(sizeof(NhlWorkspaceRec))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return (int) NhlFATAL;
	}
	wsrp->next = WSp->ws_list;
	WSp->ws_list = wsrp;
	wsrp->ws_id = ++(WSp->last_ws_id);
	wsrp->type = NrmStringToQuark(type);
	wsrp->persistence = persistence;
	wsrp->ws_data = NULL;
	wsrp->ws_ptr = NULL;
	wsrp->cur_size = req_size;
	wsrp->in_use = False;
	wsrp->data_intact = False;
	wsrp->tmp_fp = NULL;

	return wsrp->ws_id;

}

/*
 * Function:	_NhlFreeWorkspace
 *
 * Description:	Deallocates a Workspace Record.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	void
 * Side Effect:	
 */
void _NhlFreeWorkspace
#if	__STDC__
(
	int		ws_id
)
#else
(ws_id)
	int		ws_id;
#endif
{
        char            *e_text;
        char            *entry_name = "_NhlFreeWorkspace";
	NhlWorkspaceRec	*wsrp;

	if (! WSInited || WSp == NULL) {
		e_text = "%s: Workspace object in invalid state";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return;
	}
	if ((wsrp = wsFindNode(ws_id,entry_name)) == NULL) {
		e_text = "%s: Workspace id not found";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return;
	}
	if (wsrp->ws_ptr != NULL) {
		WSp->total_size -= wsrp->cur_size;
		NhlFree(wsrp->ws_ptr);
	}

	if (! wsrp->in_use) {
		switch (wsrp->persistence) {
		case NhlwsNONE:
			RemoveFromIdleList(wsrp,&ScratchHead,
					   &ScratchTail,entry_name);
			break;
		case NhlwsCORE:
			RemoveFromIdleList(wsrp,&CoreHead,
					   &CoreTail,entry_name);
			break;
		case NhlwsDISK:
			RemoveFromIdleList(wsrp,&DiskHead,
					   &DiskTail,entry_name);
			break;
		default:
			e_text = "%s: invalid persistence type";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			break;
		}
	}
	wsDeleteNode(wsrp,entry_name);
	return;
}

/*
 * Function:	wsFindNode
 *
 * Description: Finds the Workspace Record associated with a spcific
 *		Workspace id.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlWorkspaceRec	* (NULL if error)
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlWorkspaceRec	*wsFindNode
#if	__STDC__
(
	int		ws_id,
	char		*entry_name
)
#else
(ws_id,entry_name)
	int		ws_id;
	char 		*entry_name;
#endif
{
	NhlWorkspaceRec	*wsrp;

	for (wsrp = WSp->ws_list; wsrp != NULL; wsrp = wsrp->next) {
		if (ws_id == wsrp->ws_id) 
			return wsrp;
	}
	return NULL;
	
}

/*
 * Function:	wsDeleteNode
 *
 * Description: Removes a Workspace node from the Workspace List and
 *		frees its memory space.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void wsDeleteNode
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
)
#else
(wsrp,entry_name)
	NhlWorkspaceRec	*wsrp;
	char 		*entry_name;
#endif
{
        char            *e_text;
	NhlWorkspaceRec	*lwsrp = wsrp;

	if (wsrp == WSp->ws_list) {
		WSp->ws_list = wsrp->next;
		NhlFree(wsrp);
		return;
	}
	for (lwsrp = WSp->ws_list; lwsrp->next != NULL; lwsrp = lwsrp->next) {
		if (wsrp == lwsrp->next) {
			lwsrp->next = wsrp->next;
			NhlFree(wsrp);
			return;
		}
	}
	e_text = "%s: internal error in wsDeleteNode";
	NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
	return;
	
}

/*
 * Function:	_NhlUseWorkspace
 *
 * Description:	Prepares a Workspace for use, and returns a pointer to
 *		the portion of a Workspace Record visible to a plot.
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlWorkspace *, or NULL if unsuccessful.
 * Side Effect:	
 */
NhlWorkspace *_NhlUseWorkspace
#if	__STDC__
(
	int		ws_id
)
#else
(ws_id)
	int		ws_id;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
        char            *e_text;
        char            *entry_name = "_NhlUseWorkspace";
	NhlWorkspaceRec	*wsrp;
	int		over_threshold;

	if (! WSInited || WSp == NULL) {
		e_text = "%s: Workspace object in invalid state";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if ((wsrp = wsFindNode(ws_id,entry_name)) == NULL) {
		e_text = "%s: Workspace id not found";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}

	wsrp->in_use = True;
	over_threshold = WSp->total_size - WSp->threshold_size;
	if (wsrp->ws_ptr == NULL)
		over_threshold += wsrp->cur_size;
	if (over_threshold > 0) {
		ret = ReduceCurrentAlloc(over_threshold,entry_name);
		if (ret < NhlWARNING) return NULL;
	}
/*
 * If the required size of the workspace plus the current total allocation
 * is greater than the set maximum size, bail out with a fatal error.
 */
	if (WSp->total_size + wsrp->cur_size > WSp->maximum_size) {
		wsrp->in_use = False;
		e_text =
		"%s: Allocation of workspace would exceed maximum total size";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}

	switch (wsrp->persistence) {
	case NhlwsNONE:
		ret = GetScratchSpace(wsrp,entry_name);
		if (ret < NhlWARNING) return NULL;
		ret = RemoveFromIdleList(wsrp,
					 &ScratchHead,&ScratchTail,entry_name);
		if (ret < NhlWARNING) return NULL;
		break;
	case NhlwsCORE:
		ret = GetCorePreservedSpace(wsrp,entry_name);
		if (ret < NhlWARNING) return NULL;
		ret = RemoveFromIdleList(wsrp,
					 &CoreHead,&CoreTail,entry_name);
		if (ret < NhlWARNING) return NULL;
		break;
	case NhlwsDISK:
		ret = GetDiskPreservedSpace(wsrp,entry_name);
		if (ret < NhlWARNING) return NULL;
		ret = RemoveFromIdleList(wsrp,
					 &DiskHead,&DiskTail,entry_name);
		if (ret < NhlWARNING) return NULL;
		break;
	default:
		e_text = "%s: invalid persistence type";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	return (NhlWorkspace *) wsrp;
	
}


/*
 * Function:	ReduceCurrentAlloc
 * Description: Attempts to reduce the total allocation below the 
 *		threshold value, by deallocating Workspaces currently on
 *		one of the Idle Lists. The 3 lists are searched in the
 *		order: Scratch (workspaces that have no permanent data 
 *		needing preservation), Disk (workspaces whose data are
 *		written to temporary files for retrieval at next use), and
 *		Core (workspaces preserved if possible in memory). 
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes ReduceCurrentAlloc
#if	__STDC__
(
	int		over,
	char		*entry_name
)
#else
(over,entry_name)
	int		over;
	char 		*entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlwsIdleRec		*idlep;
	NhlWorkspaceRec		*wsrp;

	idlep = ScratchTail;
	while (over > 0 && idlep != NULL) {
		wsrp = idlep->wsrp;
		if (wsrp->ws_ptr != NULL) {
			NhlFree(wsrp->ws_ptr);
			wsrp->ws_ptr = NULL;
			over -= wsrp->cur_size;
			WSp->total_size -= wsrp->cur_size;
			wsrp->data_intact = False;
		}
		idlep = idlep->prev;
	}
			
	idlep = DiskTail;
	while (over > 0 && idlep != NULL) {
		wsrp = idlep->wsrp;
		if (wsrp->ws_ptr != NULL) {
			if ((ret = SaveToFile(wsrp,entry_name)) < NhlWARNING)
				return ret;
			NhlFree(wsrp->ws_ptr);
			wsrp->ws_ptr = NULL;
			over -= wsrp->cur_size;
			WSp->total_size -= wsrp->cur_size;
		}
		idlep = idlep->prev;
	}

	idlep = CoreTail;
	while (over > 0 && idlep != NULL) {
		wsrp = idlep->wsrp;
		if (wsrp->ws_ptr != NULL) {
			NhlFree(wsrp->ws_ptr);
			wsrp->ws_ptr = NULL;
			over -= wsrp->cur_size;
			WSp->total_size -= wsrp->cur_size;
			wsrp->data_intact = False;
		}
		idlep = idlep->prev;
	}

	return NhlNOERROR;
}

/*
 * Function:	SaveToFile
 *
 * Description: Saves disk-persistent Workspaces to a temporary file.
 *		The file is opened using tmpfile, which immediately 
 *		unlinks the files so that it never appears as a named
 *		file in the file system. If the file is opened rewind
 *		is called before the write.
 *		
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes	SaveToFile
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
)
#else
(wsrp,entry_name)
	NhlWorkspaceRec	*wsrp;
	char 		*entry_name;
#endif
{
        char            *e_text;
	int		count;

	if (wsrp->tmp_fp == NULL) {
		if ((wsrp->tmp_fp = _NhlTmpFile()) == NULL) {
			e_text ="%s: workspace tmpfile creation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
/*
 * if there is nothing to save don't bother 
 */
	else if (! wsrp->data_intact) {
		if (fclose(wsrp->tmp_fp) == EOF) {
			e_text ="%s: error closing workspace tmpfile";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		wsrp->tmp_fp = NULL;
		return NhlNOERROR;
	}
/*
 * otherwise rewind the file
 */
	else {
		rewind(wsrp->tmp_fp);
	}
	if ((count = fwrite(wsrp->ws_ptr,1,wsrp->cur_size,
			     wsrp->tmp_fp)) != wsrp->cur_size) { 
		e_text ="%s: error writing workspace to tmpfile";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	return NhlNOERROR;
}


/*
 * Function:	RestoreFromFile
 *
 * Description: Restores the Workspace data saved in a temporary file.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes	RestoreFromFile
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
)
#else
(wsrp,entry_name)
	NhlWorkspaceRec	*wsrp;
	char 		*entry_name;
#endif
{
        char            	*e_text;
	int			count;

	rewind(wsrp->tmp_fp);
	if ((count = fread(wsrp->ws_ptr,1,wsrp->cur_size,
			     wsrp->tmp_fp)) != wsrp->cur_size) { 
		e_text ="%s: error restoring workspace from tmpfile";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	return NhlNOERROR;
}

/*
 * Function:	GetScratchSpace
 *
 * Description: Allocates Workspace of persistence type Scratch, if needed.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes	GetScratchSpace
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
)
#else
(wsrp,entry_name)
	NhlWorkspaceRec	*wsrp;
	char 		*entry_name;
#endif
{
	char *e_text;

	if (wsrp->ws_ptr == NULL) {
		if ((wsrp->ws_ptr = (NhlPointer) 
		     NhlMalloc(wsrp->cur_size)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		wsrp->data_intact = False;
		WSp->total_size += wsrp->cur_size;
	}
	return NhlNOERROR;
}


/*
 * Function:	GetCorePreservedSpace
 *
 * Description: Allocates Workspace of persistence type Core, if needed.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes	GetCorePreservedSpace
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
)
#else
(wsrp,entry_name)
	NhlWorkspaceRec	*wsrp;
	char 		*entry_name;
#endif
{
	char *e_text;

	if (wsrp->ws_ptr == NULL) {
		if ((wsrp->ws_ptr = (NhlPointer) 
		     NhlMalloc(wsrp->cur_size)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		wsrp->data_intact = False;
		WSp->total_size += wsrp->cur_size;

	}

	return NhlNOERROR;
}


/*
 * Function:	GetDiskPreservedSpace
 *
 * Description: Allocates Workspace of persistence type Disk, if needed.
 *		Then if the data is stored in a temporary file, it is
 *		restored.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes	GetDiskPreservedSpace
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
)
#else
(wsrp,entry_name)
	NhlWorkspaceRec	*wsrp;
	char 		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;

	if (wsrp->ws_ptr == NULL) {
		if ((wsrp->ws_ptr = (NhlPointer) 
		     NhlMalloc(wsrp->cur_size)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		WSp->total_size += wsrp->cur_size;
		wsrp->data_intact = False;
	}
	if (wsrp->tmp_fp != NULL && ! wsrp->data_intact) {
		if ((ret = RestoreFromFile(wsrp,entry_name)) < NhlWARNING) 
			return ret;
		wsrp->data_intact = True;
	}

	return ret;
}

/*
 * Function:	_NhlIdleWorkspace
 *
 * Description: Signals to the Workspace object that current use of the
 *		Workspace is completed; the Workspace object is free to
 *		give the space to another workspace, saving the information
 *		to a file if necessary.
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlIdleWorkspace
#if	__STDC__
(
	NhlWorkspace	*ws
)
#else
(ws)
	NhlWorkspace	*ws;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
        char            *e_text;
        char            *entry_name = "_NhlIdleWorkspace";
	NhlWorkspaceRec	*wsrp = (NhlWorkspaceRec *) ws;
	NhlWorkspaceRec *lwsrp;

	for (lwsrp = WSp->ws_list; lwsrp != NULL; lwsrp = lwsrp->next) {
		if (wsrp == lwsrp) {
			break;
		}
	}
	if (wsrp == NULL) {
		e_text = "%s: Invalid Workspace pointer";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	ret = TrimWorkspace(wsrp, entry_name);
	if (ret < NhlWARNING) return ret;

	wsrp->in_use = False;
	wsrp->data_intact = True;

	switch (wsrp->persistence) {
	case NhlwsNONE:
		ret = AddToIdleList(wsrp,&ScratchHead,&ScratchTail,entry_name);
		if (ret < NhlWARNING) return ret;
		break;
	case NhlwsCORE:
		ret = AddToIdleList(wsrp,&CoreHead,&CoreTail,entry_name);
		if (ret < NhlWARNING) return ret;
		break;
	case NhlwsDISK:
		ret = AddToIdleList(wsrp,&DiskHead,&DiskTail,entry_name);
		if (ret < NhlWARNING) return ret;
		break;
	default:
		e_text = "%s: invalid persistence type";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return ret;
	}
	return ret;
	
}

/*
 * Function:	AddToIdleList
 *
 * Description: Adds an idled Workspace Record to one of the idle lists. 
 *		Note that the appearance of a Workspace Record on an idle
 *		list is in addition to its appearance on the main Workspace
 *		Record list.
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes AddToIdleList
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	NhlwsIdleRec	**head,
	NhlwsIdleRec	**tail,
	char		*entry_name
)
#else
(wsrp,head,tail,entry_name)
	NhlWorkspaceRec	*wsrp;
	NhlwsIdleRec	**head;
	NhlwsIdleRec	**tail;
	char		*entry_name;
#endif
{
	char		*e_text;
	NhlwsIdleRec	*idlep;

	if ((idlep = GetIdleRec()) == NULL) {
		e_text = "%s: memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	idlep->wsrp = wsrp;
	idlep->next = *head;
	idlep->prev = NULL;

	if (*head != NULL) (*head)->prev = idlep;
	*head = idlep;
	if (*tail == NULL) *tail = idlep;
	
	return NhlNOERROR;
}


/*
 * Function:	RemoveFromIdleList
 *
 * Description: Removes a Worspace Record from its Idle List
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes RemoveFromIdleList
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	NhlwsIdleRec	**head,
	NhlwsIdleRec	**tail,
	char		*entry_name
)
#else
(wsrp,head,tail,entry_name)
	NhlWorkspaceRec	*wsrp;
	NhlwsIdleRec	**head;
	NhlwsIdleRec	**tail;
	char		*entry_name;
#endif
{
	NhlwsIdleRec	*idlep;

	for (idlep = *head; idlep != NULL; idlep = idlep->next) {
		if (wsrp == idlep->wsrp)
			break;
	}
	if (idlep == NULL)
		return NhlNOERROR;

	if (idlep->next != NULL)
		idlep->next->prev = idlep->prev;
	else
		*tail = idlep->prev;

	if (idlep->prev != NULL)
		idlep->prev->next = idlep->next;
	else
		*head = idlep->next;

	PutBackIdleRec(idlep);

	return NhlNOERROR;
}

/*
 * Function:	GetIdleRec
 *
 * Description: Gets an idle record from the idle record pointer array (where
 *		unused idle records are stored). 
 *
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlwsIdleRec * or NULL
 * Side Effect:	
 */
static NhlwsIdleRec	*GetIdleRec
#if	__STDC__
(
	void
)
#else
()
#endif
{
	int i;

	if (IdleRecPool == NULL)  {
		IdleRecPool = (NhlwsIdleRec **) 
			NhlMalloc(NhlwsIDLE_REC_ALLOC * 
				  sizeof(NhlwsIdleRec *));
		if (IdleRecPool == NULL) return NULL;
		for (i = 0; i < NhlwsIDLE_REC_ALLOC; i++) {
			IdleRecPool[i] = (NhlwsIdleRec *) 
				NhlMalloc(NhlwsIDLE_REC_ALLOC *
					  sizeof(NhlwsIdleRec));
			if (IdleRecPool[i] == NULL) return NULL;
		}
		IdleRecCount = NhlwsIDLE_REC_ALLOC;
		NextIdleRec = IdleRecCount - 1;
	}
	else if (NextIdleRec < 0) {
		IdleRecPool = (NhlwsIdleRec **) 
			NhlRealloc(IdleRecPool, sizeof(NhlwsIdleRec *) *
				   (IdleRecCount + NhlwsIDLE_REC_ALLOC));
		if (IdleRecPool == NULL) return NULL;
		for (i = 0; i < NhlwsIDLE_REC_ALLOC; i++) {
			IdleRecPool[i] = (NhlwsIdleRec *) 
				NhlMalloc(NhlwsIDLE_REC_ALLOC *
					  sizeof(NhlwsIdleRec));
			if (IdleRecPool[i] == NULL) return NULL;
		}
		IdleRecCount += NhlwsIDLE_REC_ALLOC;
		NextIdleRec = NhlwsIDLE_REC_ALLOC - 1;
	}

	return (IdleRecPool[NextIdleRec--]);

}

/*
 * Function:	PutBackIdleRec
 *
 * Description: Puts an Idle Record back into the Idle Record pointer
 *		array. This happens whenever a Workspace changes state
 *		from 'idle' to 'in_use'.
 *		
 * In Args:	NhlwsIdleRec *
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void PutBackIdleRec
#if	__STDC__
(
	NhlwsIdleRec	*idlep
)
#else
(idlep)
	NhlwsIdleRec	*idlep;
#endif
{
	IdleRecPool[++NextIdleRec] = idlep;
	return;
}


/*
 * Function:	TrimWorkspace
 *
 * Description: Cuts the size down to no more than some value times the 
 *              last size needed for a complete draw. This function
 *		is called only when the Workspace is idled.
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes	TrimWorkspace
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
)
#else
(wsrp,entry_name)
	NhlWorkspaceRec	*wsrp;
	char 		*entry_name;
#endif
{
	char *e_text;
	int size_used, cur_size, new_size, type_size;
	int *iws;
	NhlBoolean reduce_size = False;
	float reduce_fac = 3.0;

	if (wsrp->type == Qlabel_area_map || 
	    wsrp->type == Qezmap_area_map ||
	    wsrp->type == Qfill_area_map ||
	    wsrp->type == Qother_area_map) {
 		type_size = sizeof(int);
		cur_size = wsrp->cur_size / type_size;
		iws = (int *) wsrp->ws_ptr;
		size_used = cur_size - (iws[5] - iws[4] - 1);

		if ((new_size = (int) (size_used * reduce_fac)) < cur_size) {
			c_armvam(wsrp->ws_ptr,wsrp->ws_ptr,new_size);
			reduce_size = True;
		}
				
	}
	else if (wsrp->type == Qconpack_float) {
 		type_size = sizeof(float);
		cur_size = wsrp->cur_size / type_size;
		c_cpgeti("RWU",&size_used);
		if ((new_size = size_used * reduce_fac) < cur_size)
			reduce_size = True;
	}
	else if (wsrp->type == Qconpack_int) {
 		type_size = sizeof(int);
		cur_size = wsrp->cur_size / type_size;
		c_cpgeti("IWU",&size_used);
		if ((new_size = size_used * reduce_fac) < cur_size)
			reduce_size = True;
	}
	else {
		return NhlNOERROR;
	}

	if (reduce_size) {
		cur_size = MAX(256,new_size * type_size);
		if ((wsrp->ws_ptr = 
		     NhlRealloc(wsrp->ws_ptr,cur_size)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
				  entry_name);
			return NhlFATAL;
		}
		WSp->total_size -= (wsrp->cur_size - cur_size);
		wsrp->cur_size = cur_size;
	}

	return NhlNOERROR;
}

/*
 * Function:	EnlargeWorkspace
 *
 * Description: Enlarges a Workspace that one of the lower level routines
 *		found too small. For now the current workspace size is 
 *		simply doubled.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes	EnlargeWorkspace
#if	__STDC__
(
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
)
#else
(wsrp,entry_name)
	NhlWorkspaceRec	*wsrp;
	char 		*entry_name;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	char *e_text;
	int over_threshold;
	int nsize;

	/* Double the current size */

	over_threshold = 
		WSp->total_size + wsrp->cur_size - WSp->threshold_size;
	if (over_threshold > 0) {
		ret = ReduceCurrentAlloc(over_threshold,entry_name);
		if (ret < NhlWARNING) return NhlFATAL;
	}
	
/*
 * If additional workspace size plus the current total allocation
 * is greater than the current maximum size, bail out with a fatal error.
 */
	if (WSp->total_size + wsrp->cur_size > WSp->maximum_size) {
		e_text =
	       "%s: Reallocation of workspace would exceed maximum total size";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if ((wsrp->ws_ptr = (NhlPointer) 
		     NhlRealloc(wsrp->ws_ptr,2*wsrp->cur_size)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	WSp->total_size += wsrp->cur_size;
	wsrp->cur_size *= 2;
	if (wsrp->type == Qlabel_area_map || 
	    wsrp->type == Qezmap_area_map ||
	    wsrp->type == Qfill_area_map ||
	    wsrp->type == Qother_area_map) {
		nsize = wsrp->cur_size / sizeof(int);
		c_armvam(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
	}
/*
 * Waiting for Conpack workspace move routines
 */
	else if (wsrp->type == Qconpack_float) {
		int nsize = wsrp->cur_size/sizeof(float);
		c_cpmvrw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
	}
	else if (wsrp->type == Qconpack_int) {
		int nsize = wsrp->cur_size/sizeof(int);
		c_cpmviw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
	}


	return NhlNOERROR;
}

/*
 * Wrapper functions for low-level NCAR Graphics calls. 
 * A plot object must be a Workspace "friend" to use these.
 */


/*
 * Function:	_NhlArinam
 *
 * Description: Areas routine ARINAM 
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlArinam
#if	__STDC__
(
	NhlWorkspace	*amap_ws,
	char		*entry_name
)
#else
(amap_ws,entry_name)
	NhlWorkspace	*amap_ws;
	char		*entry_name;
#endif
{
	char		*e_text;
	NhlWorkspaceRec	*wsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	char		*e_msg;
	int		err_num;

	c_entsr(&save_mode,1);

	c_arinam(wsrp->ws_ptr,(wsrp->cur_size/sizeof(int)));

	if (c_nerro(&err_num) != 0) {
		e_msg = c_semess(0);
		c_errof();
		e_text = "%s: %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,e_msg);
		return NhlFATAL;
	}
	
	c_retsr(save_mode);

	return NhlNOERROR;
}

/*
 * Function:	_NhlArpram
 *
 * Description: Areas routine ARPRAM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlArpram
#if	__STDC__
(
	NhlWorkspace	*amap_ws,
	int		flag1,
	int		flag2,
	int		flag3,
	char		*entry_name
)
#else
(amap_ws,flag1,flag2,flag3,entry_name)
	NhlWorkspace	*amap_ws;
	int		flag1;
	int		flag2;
	int		flag3;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*wsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg, *cmp_msg = "AREA-MAP ARRAY OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {

		c_arpram(wsrp->ws_ptr,flag1,flag2,flag3);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
				printf("resizing ws old %d", wsrp->cur_size);
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", wsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}


/*
 * Function:	_NhlAredam
 *
 * Description: Areas routine AREDAM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlAredam
#if	__STDC__
(
	NhlWorkspace	*amap_ws,
	float		*x,
	float		*y,
	int		npoints,
	int		group_id,
	int		left_id,
	int		right_id,
	char		*entry_name
)
#else
(amap_ws,x,y,npoints,group_id,left_id,right_id,entry_name)
	NhlWorkspace	*amap_ws;
	float		*x;
	float		*y;
	int		npoints;
	int		group_id;
	int		left_id;
	int		right_id;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*wsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg, *cmp_msg = "AREA-MAP ARRAY OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_aredam(wsrp->ws_ptr,x,y,npoints,group_id,left_id,right_id);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
				printf("resizing ws old %d", wsrp->cur_size);
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", wsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}

/*
 * Function:	_NhlArscam
 *
 * Description: Areas routine ARSCAM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlArscam
#if	__STDC__
(
	NhlWorkspace	*amap_ws,
	int		(*apr)(float *xcs, 
			       float *ycs, 
			       int *ncs, 
			       int *iai, 
			       int *iag, 
			       int *nai),
	char		*entry_name
)
#else
(amap_ws,apr,entry_name)
	NhlWorkspace	*amap_ws;
	int		(*apr)();
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*wsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg, *cmp_msg = "AREA-MAP ARRAY OVERFLOW";
	int		err_num;
	float		x[NhlwsMAX_GKS_POINTS], y[NhlwsMAX_GKS_POINTS];
	int		group_ids[NhlwsMAX_AREA_GROUPS];
	int		area_ids[NhlwsMAX_AREA_GROUPS];

	c_entsr(&save_mode,1);

	do {
		c_arscam(wsrp->ws_ptr,x,y,NhlwsMAX_GKS_POINTS,
			 group_ids,area_ids,NhlwsMAX_AREA_GROUPS,apr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
				printf("resizing ws old %d", wsrp->cur_size);
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", wsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}


/*
 * Function:	_NhlArdbpa
 *
 * Description: Areas routine ARDBPA
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlArdbpa
#if	__STDC__
(
	NhlWorkspace	*amap_ws,
	int		igi,
	char		*label,
	char		*entry_name
)
#else
(amap_ws,igi,label,entry_name)
	NhlWorkspace	*amap_ws;
	int		igi;
	char		*label;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*wsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg, *cmp_msg = "AREA-MAP ARRAY OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_ardbpa(wsrp->ws_ptr,igi,label);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
				printf("resizing ws old %d", wsrp->cur_size);
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", wsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}

/*
 * Function:	_NhlDumpAreaMap
 *
 * Description: Write the areamap to a file for debugging purposes
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlDumpAreaMap
#if	__STDC__
(
 	NhlWorkspace	*amap_ws,
	char		*entry_name
)
#else
(amap_ws,entry_name)
 	NhlWorkspace	*amap_ws;
	char		*entry_name;
#endif
{
	char		*e_text;
	NhlWorkspaceRec	*awsrp = (NhlWorkspaceRec *) amap_ws;
	char		*prefix = "NhlAreaMap";
	char		suffix[10];
	static		count = 0;
	char		buffer[20];
	FILE		*fp;
	int		i;
	int		*ip = (int *)awsrp->ws_ptr;

	strcpy(buffer,prefix);
	sprintf(suffix,"%c%-d",'.',count++);
	strcat(buffer,suffix);

	if ((fp = fopen(buffer,"w")) == NULL) {
		e_text = "%s: error opening %s for areamap dump";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,buffer);
		return NhlFATAL;
	}
	printf("Dumping areamap to file %s\n",buffer);
/*
 * areamap array element index is written using Fortran indexing, followed
 * by the value of the areamap element, 1 element per line.
 */
	for (i=0;i<ip[4];i++) {
		fprintf(fp,"%10d%10d\n",i+1,ip[i]);
	}
	for (i=ip[5]-1;i<ip[0];i++) {
		fprintf(fp,"%10d%10d\n",i+1,ip[i]);
	}

	return NhlNOERROR;
}

/*
 * Function:	_NhlCpback
 *
 * Description: Conpack routine CPBACK
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCpback
#if	__STDC__
(
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
)
#else
(zdat,flt_ws,int_ws,entry_name)
	float		*zdat;
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*int_ws;
	char		*entry_name;
#endif
{
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *iwsrp = (NhlWorkspaceRec *) int_ws;
	int		save_mode;
	char		*e_msg;
	int		err_num;

	c_entsr(&save_mode,1);

	c_cpback(zdat,fwsrp->ws_ptr,iwsrp->ws_ptr);

	if (c_nerro(&err_num) != 0) {
		e_msg = c_semess(0);
		c_errof();
		e_text = "%s: %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,e_msg);
		return NhlFATAL;
	}
	
	c_retsr(save_mode);
	
	return NhlNOERROR;
}

/*
 * Function:	_NhlCpclam
 *
 * Description: Conpack routine CPCLAM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCpclam
#if	__STDC__
(
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
)
#else
(zdat,flt_ws,int_ws,amap_ws,entry_name)
	float		*zdat;
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*int_ws;
 	NhlWorkspace	*amap_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*awsrp = (NhlWorkspaceRec *) amap_ws;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *iwsrp = (NhlWorkspaceRec *) int_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "AREA-MAP ARRAY OVERFLOW";
	char		*cmp_msg2 = "REAL WORKSPACE OVERFLOW";
	char		*cmp_msg3 = "INTEGER WORKSPACE OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_cpclam(zdat,fwsrp->ws_ptr,iwsrp->ws_ptr,awsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
				printf("resizing amap old %d",awsrp->cur_size);
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", awsrp->cur_size);
			}
			else if (strstr(e_msg,cmp_msg2)) {
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", fwsrp->cur_size);
			}
			else if (strstr(e_msg,cmp_msg3)) {
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", iwsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}


/*
 * Function:	_NhlCpcldm
 *
 * Description: Conpack routine CPCLDM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCpcldm
#if	__STDC__
(
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	int		(*rtpl)(float *xcs, 
				float *ycs,
				int *ncs,
				int *iai,
				int *iag,
				int *nai),
	char		*entry_name
)
#else
(zdat,flt_ws,int_ws,amap_ws,rtpl,entry_name)
	float		*zdat;
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*int_ws;
 	NhlWorkspace	*amap_ws;
	int		(*rtpl)();
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*awsrp = (NhlWorkspaceRec *) amap_ws;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *iwsrp = (NhlWorkspaceRec *) int_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "AREA-MAP ARRAY OVERFLOW";
	char		*cmp_msg2 = "REAL WORKSPACE OVERFLOW";
	char		*cmp_msg3 = "INTEGER WORKSPACE OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_cpcldm(zdat,fwsrp->ws_ptr,iwsrp->ws_ptr,awsrp->ws_ptr,rtpl);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
				printf("resizing amap old %d",awsrp->cur_size);
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", awsrp->cur_size);
			}
			else if (strstr(e_msg,cmp_msg2)) {
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", fwsrp->cur_size);
			}
			else if (strstr(e_msg,cmp_msg3)) {
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", iwsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}


/*
 * Function:	_NhlCpcldr
 *
 * Description: Conpack routine CPCLDR
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCpcldr
#if	__STDC__
(
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
)
#else
(zdat,flt_ws,int_ws,amap_ws,entry_name)
	float		*zdat;
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*int_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *iwsrp = (NhlWorkspaceRec *) int_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "REAL WORKSPACE OVERFLOW";
	char		*cmp_msg2 = "INTEGER WORKSPACE OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_cpcldr(zdat,fwsrp->ws_ptr,iwsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", fwsrp->cur_size);
			}
			else if (strstr(e_msg,cmp_msg2)) {
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", iwsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}


/*
 * Function:	_NhlCplbam
 *
 * Description: Conpack routine CPLBAM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCplbam
#if	__STDC__
(
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
)
#else
(zdat,flt_ws,int_ws,amap_ws,entry_name)
	float		*zdat;
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*int_ws;
 	NhlWorkspace	*amap_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*awsrp = (NhlWorkspaceRec *) amap_ws;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *iwsrp = (NhlWorkspaceRec *) int_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "AREA-MAP ARRAY OVERFLOW";
	char		*cmp_msg2 = "REAL WORKSPACE OVERFLOW";
	char		*cmp_msg3 = "INTEGER WORKSPACE OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_cplbam(zdat,fwsrp->ws_ptr,iwsrp->ws_ptr,awsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1) != NULL) {
				printf("resizing amap old %d",awsrp->cur_size);
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", awsrp->cur_size);
			}
			else if (strstr(e_msg,cmp_msg2) != NULL) {
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", fwsrp->cur_size);
			}
			else if (strstr(e_msg,cmp_msg3) != NULL) {
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", iwsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}


/*
 * Function:	_NhlCplbdr
 *
 * Description: Conpack routine CPLBDR
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCplbdr
#if	__STDC__
(
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
)
#else
(zdat,flt_ws,int_ws,amap_ws,entry_name)
	float		*zdat;
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*int_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *iwsrp = (NhlWorkspaceRec *) int_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "REAL WORKSPACE OVERFLOW";
	char		*cmp_msg2 = "INTEGER WORKSPACE OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_cplbdr(zdat,fwsrp->ws_ptr,iwsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", fwsrp->cur_size);
			}
			else if (strstr(e_msg,cmp_msg2)) {
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", iwsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}

/*
 * Function:	_NhlCprect
 *
 * Description: Conpack routine CPRECT
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCprect
#if	__STDC__
(
	float		*zdat,
	int		kzdt,
	int		mzdt,
	int		nzdt,				
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
)
#else
(zdat,kzdt,mzdt,nzdt,flt_ws,int_ws,entry_name)
	float		*zdat;
	int		kzdt;
	int		mzdt;
	int		nzdt;				
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*int_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *iwsrp = (NhlWorkspaceRec *) int_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "REAL WORKSPACE OVERFLOW";
	char		*cmp_msg2 = "INTEGER WORKSPACE OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_cprect(zdat,kzdt,mzdt,nzdt,
			 fwsrp->ws_ptr,(fwsrp->cur_size/sizeof(float)),
			 iwsrp->ws_ptr,(iwsrp->cur_size/sizeof(int)));

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", fwsrp->cur_size);
			}
			else if (strstr(e_msg,cmp_msg2)) {
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", iwsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}

/*
 * Function:	_NhlMapbla
 *
 * Description: Ezmap routine MAPBLA
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlMapbla
#if	__STDC__
(
 	NhlWorkspace	*amap_ws,
	char		*entry_name
)
#else
(amap_ws,entry_name)
 	NhlWorkspace	*amap_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*wsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg, *cmp_msg = "AREA-MAP ARRAY OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_mapbla(wsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
				printf("resizing ws old %d", wsrp->cur_size);
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", wsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}


/*
 * Function:	_NhlMapita
 *
 * Description: Ezmap routine MAPITA
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlMapita
#if	__STDC__
(
	NhlWorkspace	*amap_ws,
	float		x,
	float		y,
	int		up_or_down,
	int		group_id,
	int		left_id,
	int		right_id,
	char		*entry_name
)
#else
(amap_ws,x,y,up_or_down,group_id,left_id,right_id,entry_name)
	NhlWorkspace	*amap_ws;
	float		x;
	float		y;
	int		up_or_down;
	int		group_id;
	int		left_id;
	int		right_id;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*wsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg, *cmp_msg = "AREA-MAP ARRAY OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_mapita(x,y,up_or_down,wsrp->ws_ptr,
			 group_id,left_id,right_id);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
				printf("resizing ws old %d", wsrp->cur_size);
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", wsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}

/*
 * Function:	_NhlMapiqa
 *
 * Description: Ezmap routine MAPIQA
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlMapiqa
#if	__STDC__
(
	NhlWorkspace	*amap_ws,
	int		group_id,
	int		left_id,
	int		right_id,
	char		*entry_name
)
#else
(amap_ws,group_id,left_id,right_id,entry_name)
	NhlWorkspace	*amap_ws;
	int		group_id;
	int		left_id;
	int		right_id;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec	*wsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg, *cmp_msg = "AREA-MAP ARRAY OVERFLOW";
	int		err_num;

	c_entsr(&save_mode,1);

	do {
		c_mapiqa(wsrp->ws_ptr,group_id,left_id,right_id);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
				printf("resizing ws old %d", wsrp->cur_size);
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
				printf(" new %d\n", wsrp->cur_size);
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}
