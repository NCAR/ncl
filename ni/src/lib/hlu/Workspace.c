/*
 *      $Id: Workspace.c,v 1.47 2006-07-14 17:24:32 dbrown Exp $
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
#include <math.h>
#include <ncarg/hlu/WorkspaceP.h>
#include <ncarg/hlu/TransObjI.h>
#include <ncarg/hlu/ContourPlotI.h>

/* Method declarations	*/

static NhlErrorTypes WorkspaceClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes WorkspaceInitialize(
#if	NhlNeedProto
	NhlClass	lc,	/* class	*/
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
#if	NhlNeedProto
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
#if	NhlNeedProto
	int		ws_id,
	char		*entry_name
#endif
);

static void		wsDeleteNode(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);


static NhlErrorTypes	ReduceCurrentAlloc(
#if	NhlNeedProto
	int		over,
	char		*entry_name
#endif
);

static NhlErrorTypes	GetScratchSpace(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	GetCorePreservedSpace(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	GetDiskPreservedSpace(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	EnlargeWorkspace(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	TrimWorkspace(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	ChangeWorkspaceSize(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	int		amount,
	char		*entry_name
#endif
);

static NhlErrorTypes AddToIdleList(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	NhlwsIdleRec	**head,
	NhlwsIdleRec	**tail,
	char		*entry_name
#endif
);

static NhlErrorTypes RemoveFromIdleList(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	NhlwsIdleRec	**head,
	NhlwsIdleRec	**tail,
	char		*entry_name
#endif
);

static NhlwsIdleRec	*GetIdleRec(
#if	NhlNeedProto
	void
#endif
);

static void PutBackIdleRec(
#if	NhlNeedProto
	NhlwsIdleRec	*idlep
#endif
);

static NhlErrorTypes	RestoreFromFile(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

static NhlErrorTypes	SaveToFile(
#if	NhlNeedProto
	NhlWorkspaceRec	*wsrp,
	char		*entry_name
#endif
);

/* Resources */

#define Oset(field)	NhlOffset(NhlWorkspaceLayerRec,workspace.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNwsMaximumSize,NhlCwsMaximumSize,NhlTLong,
		 sizeof(long),Oset(maximum_size),
		 NhlTImmediate,_NhlUSET((NhlPointer) NhlwsDEF_MAXIMUM),0,NULL},
	{NhlNwsThresholdSize,NhlCwsThresholdSize,NhlTLong,
		 sizeof(long),Oset(threshold_size),
		 NhlTImmediate,
		 _NhlUSET((NhlPointer) NhlwsDEF_THRESHOLD),0,NULL},
	{NhlNwsCurrentSize,NhlCwsCurrentSize,NhlTLong,
		 sizeof(long),Oset(current_size),
		 NhlTImmediate,_NhlUSET((NhlPointer) 0),_NhlRES_GONLY,NULL}

/* End-documented-resources */

};
#undef Oset
	
/* Class definition	*/

NhlWorkspaceClassRec NhlworkspaceClassRec = {
	/* BaseClassPart */
	{
/* class_name			*/	"workspaceClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlWorkspaceLayerRec),
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

NhlClass NhlworkspaceClass = (NhlClass)
				&NhlworkspaceClassRec;

/* private static data */

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
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	Qcurrent_size = NrmStringToQuark(NhlNwsCurrentSize);
	Qfloat = NrmStringToQuark(NhlTFloat);
	Qint = NrmStringToQuark(NhlTInteger);
	return NhlNOERROR;
}

/*
 * Function:	WorkspaceInitialize
 *
 * Description:	This function initializes an instance of an Workspace class
 *
 * In Args:	
 *	NhlClass	lc,	class
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
#if	NhlNeedProto
(
	NhlClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlClass	lc;	/* class	*/
	NhlLayer	req;	/* requested	*/
	NhlLayer	new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
        char			*e_text;
        char    	        *entry_name = "WorkspaceInitialize";
	NhlWorkspaceClass	wsc = (NhlWorkspaceClass)lc;
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

	WSLayer = wsnew;
	WSp = (NhlWorkspaceLayerPart *) &(WSLayer->workspace);

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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if DEBUG_WS /* for debugging: */
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
#if	NhlNeedProto
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
	NhlwsIdleRec		*idlep;
	int			i;
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

	for (wsrp = wsp->ws_list; wsrp != NULL; wsrp = twsrp) {
		twsrp = wsrp->next;
		if (wsrp->ws_ptr != NULL)
			NhlFree(wsrp->ws_ptr);
		NhlFree(wsrp);
	}
	wsp->ws_list = NULL;

	WSLayer = NULL;
	WSp = NULL;

	return ret;
}

/*
 *	Public API for Workspace management
 */

/*
 * Function:	NhlGetWorkspaceObjectId
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
NhlGetWorkspaceObjectId
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
        char            *e_text;
        char            *entry_name = "NhlWorkspaceGetId";

	if (WSLayer != NULL)
		return WSLayer->base.id;

	e_text = "%s: Workspace object not found";
	NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
	return NhlFATAL;
}

/*
 * Function:	nhlpfgetworkspaceobjectid
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
void _NHLCALLF(nhlpfgetworkspaceobjectid,NHLPFGETWORKSPACEOBJECTID)
#if	NhlNeedProto
(
	int	*id
)
#else
(id)
	int	*id;
#endif
{
	*id = NhlGetWorkspaceObjectId();

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
#if	NhlNeedProto
(
	NhlwsType	type,
	NhlPersistence	persistence,			    
	int		req_size
)
#else
(type,persistence,req_size)
	NhlwsType	type;
	NhlPersistence	persistence;			    
	int		req_size;
	
#endif
{
        char            *e_text;
        char            *entry_name = "_NhlNewWorkspace";
	NhlWorkspaceRec	*wsrp;

	if (WSp == NULL) {
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
	wsrp->type = type;
	wsrp->persistence = persistence;
	wsrp->ws_ptr = NULL;
	wsrp->cur_size = wsrp->req_size = req_size;
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
#if	NhlNeedProto
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

	if (WSp == NULL) {
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
			if (wsrp->tmp_fp != NULL &&
			    fclose(wsrp->tmp_fp) == EOF) {
				e_text ="%s: error closing workspace tmpfile";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return;
			}
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

extern NhlBoolean _NhlWorkspaceDataIntact
#if	NhlNeedProto
(
	int		ws_id
)
#else
(ws_id)
	int		ws_id;
#endif
{
        char            *e_text;
        char            *entry_name = "_NhlWorkspaceDataIntact";
	NhlWorkspaceRec	*wsrp;

	if (WSp == NULL) {
		e_text = "%s: Workspace object in invalid state";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return False;
	}
	if ((wsrp = wsFindNode(ws_id,entry_name)) == NULL) {
		e_text = "%s: Workspace id not found";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return False;
	}
	return (wsrp->data_intact);
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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

	if (WSp == NULL) {
		e_text = "%s: Workspace object in invalid state";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
	if ((wsrp = wsFindNode(ws_id,entry_name)) == NULL) {
		e_text = "%s: Workspace id not found";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}

	if (wsrp->in_use) {
		e_text = "%s: Workspace is currently in use";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}
        switch (wsrp->persistence) {
        case NhlwsNONE:
                ret = RemoveFromIdleList(wsrp,
                                         &ScratchHead,&ScratchTail,entry_name);
                if (ret < NhlWARNING) return NULL;
                break;
        case NhlwsCORE:
                ret = RemoveFromIdleList(wsrp,
                                         &CoreHead,&CoreTail,entry_name);
                if (ret < NhlWARNING) return NULL;
                break;
        case NhlwsDISK:
                ret = RemoveFromIdleList(wsrp,
                                         &DiskHead,&DiskTail,entry_name);
                if (ret < NhlWARNING) return NULL;
                break;
        default:
                e_text = "%s: invalid persistence type";
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
	if (wsrp->ws_ptr == NULL && 
	    WSp->total_size + wsrp->cur_size > WSp->maximum_size) {
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
		break;
	case NhlwsCORE:
		ret = GetCorePreservedSpace(wsrp,entry_name);
		if (ret < NhlWARNING) return NULL;
		break;
	case NhlwsDISK:
		ret = GetDiskPreservedSpace(wsrp,entry_name);
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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
#if	NhlNeedProto
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

	if (wsrp->type == NhlwsAREAMAP) {
		reduce_fac = 1.2;
 		type_size = sizeof(int);
		cur_size = wsrp->cur_size / type_size;
		iws = (int *) wsrp->ws_ptr;
		size_used = cur_size - (iws[5] - iws[4] - 1);

		if ((new_size = (int) (size_used * reduce_fac)) < cur_size) {
			c_armvam(wsrp->ws_ptr,wsrp->ws_ptr,new_size);
			reduce_size = True;
		}
				
	}
	else if (wsrp->type == NhlwsCNFLOAT) {
 		type_size = sizeof(float);
		cur_size = wsrp->cur_size / type_size;
		c_cpgeti("RWU",&size_used);
		if ((new_size = size_used * reduce_fac) < cur_size)
			reduce_size = True;
	}
	else if (wsrp->type == NhlwsCNINT) {
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
 * Function:	ChangeWorkspaceSize
 *
 * Description: Changes a Workspace size by a specific amount.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes	ChangeWorkspaceSize
#if	NhlNeedProto
(
	NhlWorkspaceRec	*wsrp,
	int		amount,
	char		*entry_name
)
#else
(wsrp,amount,entry_name)
	NhlWorkspaceRec	*wsrp;
	int		amount;
	char 		*entry_name;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	char *e_text;
	int over_threshold;
	int nsize;

	over_threshold = 
		WSp->total_size + amount - WSp->threshold_size;
	if (over_threshold > 0) {
		ret = ReduceCurrentAlloc(over_threshold,entry_name);
		if (ret < NhlWARNING) return NhlFATAL;
	}
	
/*
 * If additional workspace size plus the current total allocation
 * is greater than the current maximum size, bail out with a fatal error.
 */
	if (WSp->total_size + amount > WSp->maximum_size) {
		e_text =
	       "%s: Workspace reallocation would exceed maximum size %d";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,WSp->maximum_size);
		return NhlFATAL;
	}

#if 0
	printf("changing workspace size from %d to %d\n",
	       wsrp->cur_size, wsrp->cur_size + amount);
#endif	
	if ((wsrp->ws_ptr = (NhlPointer) 
		     NhlRealloc(wsrp->ws_ptr,
				wsrp->cur_size + amount)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	WSp->total_size += amount;
	wsrp->cur_size += amount;
	if (wsrp->data_intact) {
		if (wsrp->type == NhlwsAREAMAP) {
			nsize = wsrp->cur_size / sizeof(int);
			c_armvam(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
		}
		else if (wsrp->type == NhlwsCNFLOAT) {
			int nsize = wsrp->cur_size/sizeof(float);
			c_cpmvrw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
		}
		else if (wsrp->type == NhlwsCNINT) {
			int nsize = wsrp->cur_size/sizeof(int);
			c_cpmviw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
		}
		else if (wsrp->type == NhlwsCTFLOAT) {
			int nsize = wsrp->cur_size/sizeof(float);
			c_ctmvrw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
		}
		else if (wsrp->type == NhlwsCTINT) {
			int nsize = wsrp->cur_size/sizeof(int);
			c_ctmviw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
		}
	}

#if DEBUG_WS
	printf("Workspace: total size: %d\n", WSp->total_size);

#endif
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
#if	NhlNeedProto
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
	       "%s: Workspace reallocation would exceed maximum size %d";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
			  entry_name,WSp->maximum_size);
		return NhlFATAL;
	}

	if ((wsrp->ws_ptr = (NhlPointer) 
		     NhlRealloc(wsrp->ws_ptr,2*wsrp->cur_size)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	WSp->total_size += wsrp->cur_size;
#if 0
	printf("doubling current workspace size %d\n",wsrp->cur_size);
#endif
	wsrp->cur_size *= 2;
	if (wsrp->type == NhlwsAREAMAP) {
		nsize = wsrp->cur_size / sizeof(int);
		c_armvam(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
	}
	else if (wsrp->type == NhlwsCNFLOAT) {
		int nsize = wsrp->cur_size/sizeof(float);
		c_cpmvrw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
	}
	else if (wsrp->type == NhlwsCNINT) {
		int nsize = wsrp->cur_size/sizeof(int);
		c_cpmviw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
	}
	else if (wsrp->type == NhlwsCTFLOAT) {
		int nsize = wsrp->cur_size/sizeof(float);
		c_ctmvrw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
	}
	else if (wsrp->type == NhlwsCTINT) {
		int nsize = wsrp->cur_size/sizeof(int);
		c_ctmviw(wsrp->ws_ptr,wsrp->ws_ptr,nsize);
	}

#if DEBUG_WS
	printf("Workspace: total size: %d\n", WSp->total_size);

#endif
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
#if	NhlNeedProto
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
	char		*e_msg;
	int		err_num;

	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	wsrp->data_intact = False;
	if (wsrp->cur_size != wsrp->req_size) {
		int amount = wsrp->req_size - wsrp->cur_size;
		ret = ChangeWorkspaceSize(wsrp,amount,entry_name);
		if (ret < NhlWARNING) return ret;
	}
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

	return ret;
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
#if	NhlNeedProto
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

	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}

	c_entsr(&save_mode,1);

	do {

		/* if flag1 is set to 999 call the hlu version of the ARPRAM; otherwise
		   call the version in libncarg/areas; they are almost identical */
		if (flag1 == 999) {
			_NHLCALLF(hluarpram,HLUARPRAM)(wsrp->ws_ptr,&flag1,&flag2,&flag3);
		}
		else {
			c_arpram(wsrp->ws_ptr,flag1,flag2,flag3);
		}

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
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
#if	NhlNeedProto
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

	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}

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
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
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
#if	NhlNeedProto
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
	float		flx,frx,fby,fuy,wlx,wrx,wby,wuy; int ll;


	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);

	do {
		c_arscam(wsrp->ws_ptr,x,y,NhlwsMAX_GKS_POINTS,
			 group_ids,area_ids,NhlwsMAX_AREA_GROUPS,apr);

#if DEBUG_WS
                printf("areamap size %d, size used %d\n",
                       ((int*)wsrp->ws_ptr)[0],((int*)wsrp->ws_ptr)[0] -
                       (((int*)wsrp->ws_ptr)[5] - ((int*)wsrp->ws_ptr)[4]));
#endif
                
		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
	
	c_set(flx,frx,fby,fuy,wlx,wrx,wby,wuy,ll);

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
#if	NhlNeedProto
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

	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
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
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
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
#if	NhlNeedProto
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
	static		int count = 0;
	char		buffer[20];
	FILE		*fp;
	int		i;
	int		*ip;

	if (! (awsrp && awsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	ip = (int *)awsrp->ws_ptr;
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

	if (fclose(fp) == EOF) {
		e_text ="%s: error areamap dump file %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,buffer);
		return NhlFATAL;
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
#if	NhlNeedProto
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
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
#if	NhlNeedProto
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr &&
		awsrp && awsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
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
#if DEBUG_WS
				printf("resizing amap old %d",awsrp->cur_size);
#endif
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", awsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg3)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
#if	NhlNeedProto
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr &&
		awsrp && awsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
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
#if DEBUG_WS
				printf("resizing amap old %d",awsrp->cur_size);
#endif
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", awsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg3)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
#if	NhlNeedProto
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
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
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
 * Function:	_NhlCpcltr
 *
 * Description: Conpack routine CPCLTR
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCpcltr
(
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	float           clevel,
	int             *flag,
	float           **xloc,
	float           **yloc,
	int             *npoints,
	char		*entry_name
)
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
	int             irw1, irw2;

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		/* 
		 *  actually use cptrcl instead of cpcltr: cpcltr calls set everytime which can be
		 * a huge unnecessary performance hit. It then just calls cptrcl, which unfortunately 
		 * does not have a c interface function.
		 */
		_NHLCALLF(cptrcl,CPTRCL)(zdat,fwsrp->ws_ptr,iwsrp->ws_ptr,&clevel,flag,&irw1,&irw2,npoints);

		/*
		c_cpcltr(zdat,fwsrp->ws_ptr,iwsrp->ws_ptr,clevel,flag,&irw1,&irw2,npoints);
		*/

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
	*xloc = &(((float*)fwsrp->ws_ptr)[irw1]);
	*yloc = &(((float*)fwsrp->ws_ptr)[irw2]);

	return NhlNOERROR;
}

/*
 * Function:	_NhlCtcltr
 *
 * Description: Conpack routine CTCLTR
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCtcltr
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	float           clevel,
	int             *flag,
	float           **xloc,
	float           **yloc,
	int             *npoints,
	char		*entry_name
)
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
	int             irw1, irw2;

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		/* 
		 * actually use cttrcl instead of ctcltr: cpcltr calls set everytime which can be
		 * a huge unnecessary performance hit. It then just calls cttrcl, which unfortunately 
		 * does not have a c interface function.
		 */
		_NHLCALLF(cttrcl,CTTRCL)(rpnt,iedg,itri,fwsrp->ws_ptr,iwsrp->ws_ptr,&clevel,flag,&irw1,&irw2,npoints);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
	*xloc = &(((float*)fwsrp->ws_ptr)[irw1]);
	*yloc = &(((float*)fwsrp->ws_ptr)[irw2]);

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
#if	NhlNeedProto
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr &&
		awsrp && awsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
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
#if DEBUG_WS
				printf("resizing amap old %d",awsrp->cur_size);
#endif
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", awsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2) != NULL) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg3) != NULL) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
#if	NhlNeedProto
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
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
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
 * Function:	_NhlCpcica
 *
 * Description: Conpack routine CPCICA
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCpcica
#if	NhlNeedProto
(
	float		*zdat,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	NhlWorkspace	*cell_ws,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	float		min_cell_size,
	NhlBoolean	smooth,
	NhlBoolean      use_mesh_fill,
	char		*entry_name
)
#else
(zdat,flt_ws,int_ws,cell_ws,ica1,icam,ican,
 xcpf,ycpf,xcqf,ycqf,min_cell_size,smooth,entry_name)
	float		*zdat;
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*int_ws;
	NhlWorkspace	*cell_ws;
	int		ica1;
	int		icam;
	int		ican;
	float		xcpf;
	float		ycpf;
	float		xcqf;
	float		ycqf;
	float		min_cell_size;
	NhlBoolean	smooth;
	NhlBoolean      use_mesh_fill,
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *iwsrp = (NhlWorkspaceRec *) int_ws;
	NhlWorkspaceRec *cwsrp = (NhlWorkspaceRec *) cell_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "REAL WORKSPACE OVERFLOW";
	char		*cmp_msg2 = "INTEGER WORKSPACE OVERFLOW";
	int		err_num;
	float		fl,fr,fb,ft,wl,wr,wb,wt;
	int		ll;
	int		startx = 1,starty = 1;
	int		countx,county;
	float 		lxcpf,lycpf,lxcqf,lycqf;
	float		xstep,ystep;
	float		lx = -999.,rx = -999.,by = -999. ,ty = -999.;
	float		xedge,yedge;
	int		xedge_ix,yedge_ix;
	int		count;

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr &&
		cwsrp && cwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	if (cwsrp->cur_size != icam * ican * sizeof(int)) {
		int amount = (icam * ican) * sizeof(int) - cwsrp->cur_size;
		ret = ChangeWorkspaceSize(cwsrp,amount,entry_name);
		if (ret < NhlWARNING) return ret;
	}

	lxcpf = smooth ? MAX(0.0,xcpf) : xcpf;
	lycpf = smooth ? MAX(0.0,ycpf) : ycpf;
	lxcqf = smooth ? MIN(1.0,xcqf) : xcqf;
	lycqf = smooth ? MIN(1.0,ycqf) : ycqf;

	if (use_mesh_fill) {
		ret = _NhlMeshFill(zdat,cwsrp->ws_ptr,ica1,icam,ican,
				     lxcpf,lycpf,lxcqf,lycqf,entry_name);
		if (ret < NhlWARNING) 
			return NhlFATAL;
	}
	else if (! smooth) {
		ret = _NhlRasterFill(zdat,cwsrp->ws_ptr,ica1,icam,ican,
				     lxcpf,lycpf,lxcqf,lycqf,entry_name);
		if (ret < NhlWARNING) 
			return NhlFATAL;
	}
	else do {
		NGCALLF(cpcica,CPCICA)
			(zdat,fwsrp->ws_ptr,iwsrp->ws_ptr,cwsrp->ws_ptr,
			 &ica1,&icam,&ican,&lxcpf,&lycpf,&lxcqf,&lycqf);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);

/*
 * Since all the transformations have been applied during the creation of
 * the cell array, it must be placed into the frame using an identity
 * transformation.
 */
	c_getset(&fl,&fr,&fb,&ft,&wl,&wr,&wb,&wt,&ll);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	c_set(fl,fr,fb,ft,fl,fr,fb,ft,1);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	
	countx = icam;
	county = ican;
	if (! smooth) {
		float rem;
		xstep = (lxcqf-lxcpf) / icam;
		ystep = (lycqf-lycpf) / ican;
		if (lxcpf < 0) {
			count = -lxcpf / xstep;
			rem = -lxcpf - count * xstep;
			if (rem > 0.0)
				count += 1;
			startx += count;
			countx -= count;
			lx = xstep - rem;
			lxcpf += count * xstep;
		}
		if (lxcqf > 1.0) {
			count = (lxcqf - 1.0) / xstep;
			rem = (lxcqf - 1.0) - count * xstep;
			if (rem > 0.0)
				count += 1;
			countx -= count;
			rx = 1.0 - (xstep -rem);
			lxcqf -= count * xstep;
		}
		if (lycpf < 0) {
			count = -lycpf / ystep;
			rem = -lycpf - count * ystep;
			if (rem > 0.0)
				count += 1;
			starty += count;
			county -= count;
			by = ystep -rem;
			lycpf += count * ystep;
		}
		if (lycqf > 1.0) {
			count = (lycqf - 1.0) / ystep;
			rem = (lycqf - 1.0) - count * ystep;
			if (rem > 0.0)
				count += 1;
			county -= count;
			ty = 1.0 - (ystep - rem);
			lycqf -= count * ystep;
		}
	}
	if (countx > 0 && county > 0) {
		_NHLCALLF(gca,GCA)(&lxcpf,&lycpf,&lxcqf,&lycqf,&icam,&ican,
				   &startx,&starty,&countx,&county,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}

	/* 
	 * Fill around the edges if necessary -- the min_cell_size check
	 * is required to prevent 0-sized arrays in device space, which
	 * can cause core dumps, at least using the xWorkstation.
	 */

	count = 1;
	if (lx > min_cell_size && county > 0) {
		xedge = 0.0;
		xedge_ix = MAX(1,startx-1);
		_NHLCALLF(gca,GCA)(&xedge,&lycpf,&lx,&lycqf,&icam,&ican,
				   &xedge_ix,&starty,&count,&county,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (by > min_cell_size && countx > 0) {
		yedge = 0.0;
		yedge_ix = MAX(1,starty-1);
		_NHLCALLF(gca,GCA)(&lxcpf,&yedge,&lxcqf,&by,&icam,&ican,
				   &startx,&yedge_ix,&countx,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (rx > min_cell_size && county > 0) {
		xedge = 1.0;
		xedge_ix = startx + countx;
		_NHLCALLF(gca,GCA)(&rx,&lycpf,&xedge,&lycqf,&icam,&ican,
				   &xedge_ix,&starty,&count,&county,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (ty > min_cell_size && countx > 0) {
		yedge = 1.0;
		yedge_ix = starty + county;
		_NHLCALLF(gca,GCA)(&lxcpf,&ty,&lxcqf,&yedge,&icam,&ican,
				   &startx,&yedge_ix,&countx,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	/* now do possible single cells at the corners */

	if (lx > min_cell_size && by > min_cell_size) {
		xedge = 0.0;
		xedge_ix = MAX(1,startx - 1);
		yedge = 0.0;
		yedge_ix = MAX(1,starty -1);
		_NHLCALLF(gca,GCA)(&xedge,&yedge,&lx,&by,&icam,&ican,
				   &xedge_ix,&yedge_ix,&count,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (rx > min_cell_size && by > min_cell_size) {
		xedge = 1.0;
		xedge_ix = startx + countx;
		yedge = 0.0;
		yedge_ix = MAX(1,starty -1);
		_NHLCALLF(gca,GCA)(&rx,&yedge,&xedge,&by,&icam,&ican,
				   &xedge_ix,&yedge_ix,&count,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (lx > min_cell_size && ty > min_cell_size) {
		xedge = 0.0;
		xedge_ix = MAX(1,startx - 1);
		yedge = 1.0;
		yedge_ix = starty + county;
		_NHLCALLF(gca,GCA)(&xedge,&ty,&lx,&yedge,&icam,&ican,
				   &xedge_ix,&yedge_ix,&count,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (rx > min_cell_size && ty > min_cell_size) {
		xedge = 1.0;
		xedge_ix = startx + countx;
		yedge = 1.0;
		yedge_ix = starty + county;
		_NHLCALLF(gca,GCA)(&rx,&ty,&xedge,&yedge,&icam,&ican,
				   &xedge_ix,&yedge_ix,&count,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}


	c_set(fl,fr,fb,ft,wl,wr,wb,wt,ll);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

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
#if	NhlNeedProto
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	if (iwsrp->cur_size < mzdt * nzdt * sizeof(int)) {
		int amount = (mzdt * nzdt) * sizeof(int) - iwsrp->cur_size;
		ret = ChangeWorkspaceSize(iwsrp,amount,entry_name);
		if (ret < NhlWARNING) return ret;
	}

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
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
 * Function:	_NhlCtmesh
 *
 * Description: Conpackt routine CTMESH
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCtmesh
#if	NhlNeedProto
(
	float		*rpnt,
	int		npnt,
	int		lopn,
	int             *iedg,
	int             nedg,
	int             loen,
	int             *itri,
	int             ntri,
	int             lotn,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
)
#else
(rpnt,npnt,lopn,iedg,nedg,loen,itri,ntri,lotn,flt_ws,int_ws,entry_name)
	float		*rpnt;
	int		npnt;
	int		lopn;
	int             *iedg;
	int             nedg;
	int             loen;
	int             *itri;
	int             ntri;
	int             lotn;
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_ctmesh(rpnt,npnt,lopn,
			 iedg,nedg,loen,
			 itri,ntri,lotn, 
			 fwsrp->ws_ptr,(fwsrp->cur_size/sizeof(float)),
			 iwsrp->ws_ptr,(iwsrp->cur_size/sizeof(int)));

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
 * Function:	_NhlHLUCtmesh
 *
 * Description: modified version of Conpackt routine CTMESH that 
 *   is optimized for threads.
 *
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlHLUCtmesh
#if	NhlNeedProto
(
	float		*rpnt,
	int		npnt,
	int		lopn,
	int             *iedg,
	int             nedg,
	int             loen,
	int             *itri,
	int             ntri,
	int             lotn,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
)
#else
(rpnt,npnt,lopn,iedg,nedg,loen,itri,ntri,lotn,flt_ws,int_ws,entry_name)
	float		*rpnt;
	int		npnt;
	int		lopn;
	int             *iedg;
	int             nedg;
	int             loen;
	int             *itri;
	int             ntri;
	int             lotn;
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		int fw_size, iw_size;
		fw_size = fwsrp->cur_size/sizeof(float);
		iw_size = iwsrp->cur_size/sizeof(int);
		NGCALLF(hluctmesh,HLUCTMESH)(rpnt,&npnt,&lopn,
					     iedg,&nedg,&loen,
					     itri,&ntri,&lotn, 
					     fwsrp->ws_ptr,&fw_size,
					     iwsrp->ws_ptr,&iw_size);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
 * Function:	_NhlCtcldr
 *
 * Description: Conpackt routine CTCLDR
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCtcldr
#if	NhlNeedProto
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
)
#else
(rpnt,iedg,itri,flt_ws,int_ws,entry_name)
	float		*rpnt;
	int             *iedg;
	int             *itri;
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_ctcldr(rpnt,iedg,itri,fwsrp->ws_ptr,iwsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
 * Function:	_NhlCtlbdr
 *
 * Description: Conpackt routine CTLBDR
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCtlbdr
#if	NhlNeedProto
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	char		*entry_name
)
#else
(rpnt,iedg,itri,flt_ws,int_ws,entry_name)
	float		*rpnt;
	int             *iedg;
	int             *itri;
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);


	do {
		c_ctlbdr(rpnt,iedg,itri,fwsrp->ws_ptr,iwsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
 * Function:	_NhlCtcica
 *
 * Description: Conpackt routine CTCICA
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCtcica
#if	NhlNeedProto
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
	NhlWorkspace	*cell_ws,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	float		min_cell_size,
	NhlBoolean	smooth,
	int             fill_op,
	void            *info,
	char		*entry_name
)
#else
(rpnt,iedg,itri,flt_ws,int_ws,cell_ws,ica1,icam,ican,
 xcpf,ycpf,xcqf,ycqf,min_cell_size,smooth,fill_op,entry_name)
	float		*rpnt;
	int             *iedg;
	int             *itri;
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*int_ws;
	NhlWorkspace	*cell_ws;
	int		ica1;
	int		icam;
	int		ican;
	float		xcpf;
	float		ycpf;
	float		xcqf;
	float		ycqf;
	float		min_cell_size;
	NhlBoolean	smooth;
	int             fill_op;
	void            *info;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *iwsrp = (NhlWorkspaceRec *) int_ws;
	NhlWorkspaceRec *cwsrp = (NhlWorkspaceRec *) cell_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "REAL WORKSPACE OVERFLOW";
	char		*cmp_msg2 = "INTEGER WORKSPACE OVERFLOW";
	int		err_num;
	float		fl,fr,fb,ft,wl,wr,wb,wt;
	int		ll;
	int		startx = 1,starty = 1;
	int		countx,county;
	float 		lxcpf,lycpf,lxcqf,lycqf;
	float		xstep,ystep;
	float		lx = -999.,rx = -999.,by = -999. ,ty = -999.;
	float		xedge,yedge;
	int		xedge_ix,yedge_ix;
	int		count;

	c_entsr(&save_mode,1);

	if (fill_op == 3) 
	  goto GCA_ONLY;

	if (fill_op == 1  || ! smooth) {
		if (! (cwsrp && cwsrp->ws_ptr)) {
			e_text = "%s: invalid workspace";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}
	else {
		if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr &&
		       cwsrp && cwsrp->ws_ptr)) { 
			e_text = "%s: invalid workspace";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
	}

	if (cwsrp->cur_size != icam * ican * sizeof(int)) {
		int amount = (icam * ican) * sizeof(int) - cwsrp->cur_size;
		ret = ChangeWorkspaceSize(cwsrp,amount,entry_name);
		if (ret < NhlWARNING) return ret;
	}

	lxcpf = smooth ? MAX(0.0,xcpf) : xcpf;
	lycpf = smooth ? MAX(0.0,ycpf) : ycpf;
	lxcqf = smooth ? MIN(1.0,xcqf) : xcqf;
	lycqf = smooth ? MIN(1.0,ycqf) : ycqf;

	if (fill_op == 1) {
		ret = _NhlUnstructuredMeshFill(cwsrp->ws_ptr,
					       ica1,icam,ican,
					       lxcpf,lycpf,lxcqf,lycqf,
					       min_cell_size,
					       smooth,
					       entry_name);
		if (ret < NhlWARNING) 
			return NhlFATAL;
	}
	/*else if (! smooth) {   */
	else {  /* the C routine handles both smooth and unsmooth raster fill now */
		ret = _NhlTriMeshRasterFill
			(rpnt,iedg,itri,cwsrp->ws_ptr,
			 ica1,icam,ican,lxcpf,lycpf,lxcqf,lycqf,info,entry_name);
		if (ret < NhlWARNING) 
			return NhlFATAL;
		if (fill_op == 2)
		  return ret;
	}
#if 0
	else do {
		int yes = 1;
		if (yes) {
		 NGCALLF(ctcica,CTCICA)
			 (rpnt,iedg,itri,fwsrp->ws_ptr,iwsrp->ws_ptr,
			 cwsrp->ws_ptr,
			 &ica1,&icam,&ican,&lxcpf,&lycpf,&lxcqf,&lycqf);
		}
		else {
			ret = _NhlTriMeshRasterFill
			(rpnt,iedg,itri,cwsrp->ws_ptr,
			 ica1,icam,ican,lxcpf,lycpf,lxcqf,lycqf,info,entry_name);
			if (ret < NhlWARNING) 
				return NhlFATAL;
		}
		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
		}
	} while (! done);
#endif

 GCA_ONLY:
/*
 * Since all the transformations have been applied during the creation of
 * the cell array, it must be placed into the frame using an identity
 * transformation.
 */
	lxcpf = smooth ? MAX(0.0,xcpf) : xcpf;
	lycpf = smooth ? MAX(0.0,ycpf) : ycpf;
	lxcqf = smooth ? MIN(1.0,xcqf) : xcqf;
	lycqf = smooth ? MIN(1.0,ycqf) : ycqf;

	c_getset(&fl,&fr,&fb,&ft,&wl,&wr,&wb,&wt,&ll);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	c_set(fl,fr,fb,ft,fl,fr,fb,ft,1);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	

	countx = icam;
	county = ican;
	if (! smooth) {
		float rem;
		xstep = (lxcqf-lxcpf) / icam;
		ystep = (lycqf-lycpf) / ican;
		if (lxcpf < 0) {
			count = -lxcpf / xstep;
			rem = -lxcpf - count * xstep;
			if (rem > 0.0)
				count += 1;
			startx += count;
			countx -= count;
			lx = xstep - rem;
			lxcpf += count * xstep;
		}
		if (lxcqf > 1.0) {
			count = (lxcqf - 1.0) / xstep;
			rem = (lxcqf - 1.0) - count * xstep;
			if (rem > 0.0)
				count += 1;
			countx -= count;
			rx = 1.0 - (xstep -rem);
			lxcqf -= count * xstep;
		}
		if (lycpf < 0) {
			count = -lycpf / ystep;
			rem = -lycpf - count * ystep;
			if (rem > 0.0)
				count += 1;
			starty += count;
			county -= count;
			by = ystep -rem;
			lycpf += count * ystep;
		}
		if (lycqf > 1.0) {
			count = (lycqf - 1.0) / ystep;
			rem = (lycqf - 1.0) - count * ystep;
			if (rem > 0.0)
				count += 1;
			county -= count;
			ty = 1.0 - (ystep - rem);
			lycqf -= count * ystep;
		}
	}
	if (countx > 0 && county > 0) {
		_NHLCALLF(gca,GCA)(&lxcpf,&lycpf,&lxcqf,&lycqf,&icam,&ican,
				   &startx,&starty,&countx,&county,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}

	/* 
	 * Fill around the edges if necessary -- the min_cell_size check
	 * is required to prevent 0-sized arrays in device space, which
	 * can cause core dumps, at least using the xWorkstation.
	 */

	count = 1;
	if (lx > min_cell_size && county > 0) {
		xedge = 0.0;
		xedge_ix = MAX(1,startx-1);
		_NHLCALLF(gca,GCA)(&xedge,&lycpf,&lx,&lycqf,&icam,&ican,
				   &xedge_ix,&starty,&count,&county,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (by > min_cell_size && countx > 0) {
		yedge = 0.0;
		yedge_ix = MAX(1,starty-1);
		_NHLCALLF(gca,GCA)(&lxcpf,&yedge,&lxcqf,&by,&icam,&ican,
				   &startx,&yedge_ix,&countx,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (rx > min_cell_size && county > 0) {
		xedge = 1.0;
		xedge_ix = startx + countx;
		_NHLCALLF(gca,GCA)(&rx,&lycpf,&xedge,&lycqf,&icam,&ican,
				   &xedge_ix,&starty,&count,&county,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (ty > min_cell_size && countx > 0) {
		yedge = 1.0;
		yedge_ix = starty + county;
		_NHLCALLF(gca,GCA)(&lxcpf,&ty,&lxcqf,&yedge,&icam,&ican,
				   &startx,&yedge_ix,&countx,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	/* now do possible single cells at the corners */

	if (lx > min_cell_size && by > min_cell_size) {
		xedge = 0.0;
		xedge_ix = MAX(1,startx - 1);
		yedge = 0.0;
		yedge_ix = MAX(1,starty -1);
		_NHLCALLF(gca,GCA)(&xedge,&yedge,&lx,&by,&icam,&ican,
				   &xedge_ix,&yedge_ix,&count,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (rx > min_cell_size && by > min_cell_size) {
		xedge = 1.0;
		xedge_ix = startx + countx;
		yedge = 0.0;
		yedge_ix = MAX(1,starty -1);
		_NHLCALLF(gca,GCA)(&rx,&yedge,&xedge,&by,&icam,&ican,
				   &xedge_ix,&yedge_ix,&count,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (lx > min_cell_size && ty > min_cell_size) {
		xedge = 0.0;
		xedge_ix = MAX(1,startx - 1);
		yedge = 1.0;
		yedge_ix = starty + county;
		_NHLCALLF(gca,GCA)(&xedge,&ty,&lx,&yedge,&icam,&ican,
				   &xedge_ix,&yedge_ix,&count,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (rx > min_cell_size && ty > min_cell_size) {
		xedge = 1.0;
		xedge_ix = startx + countx;
		yedge = 1.0;
		yedge_ix = starty + county;
		_NHLCALLF(gca,GCA)(&rx,&ty,&xedge,&yedge,&icam,&ican,
				   &xedge_ix,&yedge_ix,&count,&count,
				   cwsrp->ws_ptr);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}



	c_set(fl,fr,fb,ft,wl,wr,wb,wt,ll);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	c_retsr(save_mode);

	return NhlNOERROR;
}


/*
 * Function:	_NhlCtclam
 *
 * Description: Conpack routine CTCLAM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCtclam
#if	NhlNeedProto
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
)
#else
(rpnt,iedg,itri,flt_ws,int_ws,amap_ws,entry_name)
	float		*rpnt;
	int             *iedg;
	int             *itri;
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr &&
		awsrp && awsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_ctclam(rpnt,iedg,itri,
			 fwsrp->ws_ptr,iwsrp->ws_ptr,awsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing amap old %d",awsrp->cur_size);
#endif
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", awsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg3)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
 * Function:	_NhlCtcldm
 *
 * Description: Conpack routine CTCLDM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCtcldm
#if	NhlNeedProto
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
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
(rpnt,iedg,itri,flt_ws,int_ws,amap_ws,rtpl,entry_name)
	float		*rpnt;
	int             *iedg;
	int             *itri;
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr &&
		awsrp && awsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_ctcldm(rpnt,iedg,itri,
			 fwsrp->ws_ptr,iwsrp->ws_ptr,awsrp->ws_ptr,rtpl);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing amap old %d",awsrp->cur_size);
#endif
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", awsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg3)) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
 * Function:	_NhlCtlbam
 *
 * Description: Conpack routine CTLBAM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlCtlbam
#if	NhlNeedProto
(
	float		*rpnt,
	int             *iedg,
	int             *itri,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*int_ws,
 	NhlWorkspace	*amap_ws,
	char		*entry_name
)
#else
(rpnt,iedg,itriflt_ws,int_ws,amap_ws,entry_name)
	float		*rpnt;
	int             *iedg;
	int             *itri;
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

	if (! (fwsrp && fwsrp->ws_ptr && iwsrp && iwsrp->ws_ptr &&
		awsrp && awsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_ctlbam(rpnt,iedg,itri,
			 fwsrp->ws_ptr,iwsrp->ws_ptr,awsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1) != NULL) {
#if DEBUG_WS
				printf("resizing amap old %d",awsrp->cur_size);
#endif
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", awsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2) != NULL) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg3) != NULL) {
#if DEBUG_WS
				printf("resizing int_ws old %d",
				       iwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(iwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", iwsrp->cur_size);
#endif
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
#if	NhlNeedProto
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
	float 		flx,frx,fby,fuy,wlx,wrx,wby,wuy; 
	int 		ll;

	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);
	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);

	do {
		c_mapbla(wsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
			c_mapint();
			if (c_nerro(&err_num) != 0) {
				e_msg = c_semess(0);
				c_errof();
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
			c_set(flx,frx,fby,fuy,wlx,wrx,wby,wuy,ll);
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
#if	NhlNeedProto
(
	NhlWorkspace	*amap_ws,
	float		y,
	float		x,
	int		up_or_down,
	int		group_id,
	int		left_id,
	int		right_id,
	char		*entry_name
)
#else
(amap_ws,y,x,up_or_down,group_id,left_id,right_id,entry_name)
	NhlWorkspace	*amap_ws;
	float		y;
	float		x;
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


	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_mapita(y,x,up_or_down,wsrp->ws_ptr,
			 group_id,left_id,right_id);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
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
#if	NhlNeedProto
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

	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
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
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
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
 * Function:	_NhlMapblm
 *
 * Description: Ezmap routine MAPBLM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlMapblm
#if	NhlNeedProto
(
	NhlWorkspace	*amap_ws,
	int		(*ulpr)(float *xcra, 
			       float *ycra, 
			       int *mcra, 
			       int *iaai, 
			       int *iagi, 
			       int *nogi),
	char		*entry_name
)
#else
(amap_ws,ulpr,entry_name)
	NhlWorkspace	*amap_ws;
	int		(*ulpr)();
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


	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_mapblm(wsrp->ws_ptr,x,y,NhlwsMAX_GKS_POINTS,
			 group_ids,area_ids,NhlwsMAX_AREA_GROUPS,ulpr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
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
 * Function:	_NhlMapgrm
 *
 * Description: Ezmap routine MAPGRM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlMapgrm
#if	NhlNeedProto
(
	NhlWorkspace	*amap_ws,
	int		(*ulpr)(float *xcra, 
			       float *ycra, 
			       int *mcra, 
			       int *iaai, 
			       int *iagi, 
			       int *nogi),
	char		*entry_name
)
#else
(amap_ws,ulpr,entry_name)
	NhlWorkspace	*amap_ws;
	int		(*ulpr)();
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

	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_mapgrm(wsrp->ws_ptr,x,y,NhlwsMAX_GKS_POINTS,
			 group_ids,area_ids,NhlwsMAX_AREA_GROUPS,ulpr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
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
 * Function:	_NhlMplnam
 *
 * Description: Ezmapb routine MPLNAM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlMplnam
#if	NhlNeedProto
(
 	NhlWorkspace	*amap_ws,
        NhlString	map_data_filename,
        int		level,
	char		*entry_name
)
#else
(amap_ws,map_data_filename,level,entry_name)
 	NhlWorkspace	*amap_ws;
        NhlString	map_data_filename;
        int		level;
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
	float 		flx,frx,fby,fuy,wlx,wrx,wby,wuy; 
	int 		ll;
        int		len;
        NGstring 	map_data_filename_f;

	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
        len = NGSTRLEN(map_data_filename);
        map_data_filename_f = NGCstrToFstr(map_data_filename,len);

	c_entsr(&save_mode,1);
	c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);

	do {
                NGCALLF(mplnam,MPLNAM)
                        (map_data_filename_f,&level,wsrp->ws_ptr,len);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
			}
			else {
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
			c_mapint();
			if (c_nerro(&err_num) != 0) {
				e_msg = c_semess(0);
				c_errof();
				e_text = "%s: %s";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name,e_msg);
				return NhlFATAL;
			}
			c_set(flx,frx,fby,fuy,wlx,wrx,wby,wuy,ll);
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}

/*
 * Function:	_NhlMplndm
 *
 * Description: Ezmapb routine MPLNDM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlMplndm
#if	NhlNeedProto
(
	NhlWorkspace	*amap_ws,
        NhlString	map_data_filename,
        int		level,
	int		(*ulpr)(float *xcra, 
			       float *ycra, 
			       int *mcra, 
			       int *iaai, 
			       int *iagi, 
			       int *nogi),
	char		*entry_name
)
#else
(amap_ws,map_data_filename,level,ulpr,entry_name)
	NhlWorkspace	*amap_ws;
        NhlString	map_data_filename;
        int		level;
	int		(*ulpr)();
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
        int		len,npoints,ngroups;
        NGstring 	map_data_filename_f;

	if (! (wsrp && wsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
        npoints = NhlwsMAX_GKS_POINTS;
        ngroups = NhlwsMAX_AREA_GROUPS;
        len = NGSTRLEN(map_data_filename);
        map_data_filename_f = NGCstrToFstr(map_data_filename,len);
        
	c_entsr(&save_mode,1);

	do {
                NGCALLF(mplndm,MPLNDM)
                        (map_data_filename_f,&level,
                         wsrp->ws_ptr,x,y,&npoints,
			 group_ids,area_ids,&ngroups,ulpr,len);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg)) {
#if DEBUG_WS
				printf("resizing ws old %d", wsrp->cur_size);
#endif
				ret = EnlargeWorkspace(wsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", wsrp->cur_size);
#endif
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
 * Function:	_NhlMdrgol
 *
 * Description: Ezmap routine mdrgol
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlMdrgol
#if	NhlNeedProto
(
	int		irgl,
	NhlWorkspace	*flt_ws,
	char		*entry_name
)
#else
(irgl,flt_ws,entry_name)
	int		irgl;
	NhlWorkspace	*flt_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "REAL WORKSPACE OVERFLOW";
	int		err_num;

	if (! (fwsrp && fwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_mdrgol(irgl,fwsrp->ws_ptr,(fwsrp->cur_size/sizeof(float)));

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
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
 * Function:	_NhlMdrgsf
 *
 * Description: Ezmap routine mdrgsf
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlMdrgsf
#if	NhlNeedProto
(
	int		irgl,
	NhlWorkspace	*flt_ws,
	NhlWorkspace	*amap_ws,
	char		*entry_name
)
#else
(irgl,flt_ws,amap_ws,entry_name)
	int		irgl;
	NhlWorkspace	*flt_ws;
	NhlWorkspace	*amap_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec	*awsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "AREA-MAP ARRAY OVERFLOW";
	char		*cmp_msg2 = "REAL WORKSPACE OVERFLOW";
	int		err_num;

	if (! (fwsrp && fwsrp->ws_ptr && awsrp && awsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	c_entsr(&save_mode,1);

	do {
		c_mdrgsf(irgl,fwsrp->ws_ptr,(fwsrp->cur_size/sizeof(float)),
			 awsrp->ws_ptr,(awsrp->cur_size/sizeof(float)));

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1) != NULL) {
#if DEBUG_WS
				printf("resizing amap old %d",awsrp->cur_size);
#endif
				ret = EnlargeWorkspace(awsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", awsrp->cur_size);
#endif
			}
			else if (strstr(e_msg,cmp_msg2) != NULL) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
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
 * Function:	_NhlVvinit
 *
 * Description: Vectors routine VVINIT
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlVvinit
#if	NhlNeedProto
(
	float		*u,
	int		lu,
	float		*v,
	int		lv,
	float		*p,
	int		lp,
	int		m,
	int		n,				
	NhlWorkspace	*flt_ws,
	char		*entry_name
)
#else
(u,lu,v,lv,p,lp,m,n,flt_ws,entry_name)
	float		*u;
	int		lu;
	float		*v;
	int		lv;
	float		*p;
	int		lp;
	int		m;
	int		n;				
	NhlWorkspace	*flt_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	int		req_size;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "REAL WORKSPACE OVERFLOW";
	int		err_num;

	req_size = 2 * m * n * sizeof(float);
	if (fwsrp && fwsrp->cur_size != req_size) {
		int amount = req_size - fwsrp->cur_size;
		ret = ChangeWorkspaceSize(fwsrp,amount,entry_name);
		if (ret < NhlWARNING) return ret;
	}

	c_entsr(&save_mode,1);
	do {
		if (fwsrp) {
			c_vvinit(u,lu,v,lv,p,lp,m,n,fwsrp->ws_ptr,
				 (fwsrp->cur_size/sizeof(float)));
		}
		else {
			c_vvinit(u,lu,v,lv,p,lp,m,n,NULL,0);
		}

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
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
 * Function:	_NhlVvectr
 *
 * Description: Vectors routine VVECTR
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlVvectr
#if	NhlNeedProto
(
	float		*u,
	float		*v,
	float		*p,
        NhlWorkspace	*amap_ws,
	int 		(*vvudmv)(float *xcs, 
				  float *ycs,
				  int *ncs,
				  int *iai,
				  int *iag,
				  int *nai),
	NhlWorkspace	*flt_ws,
	char		*entry_name
)
#else
(u,v,p,amap_ws,vvudmv,flt_ws,entry_name)
	float		*u;
	float		*v;
	float		*p;
        NhlWorkspace	*amap_ws;
	int 		(*vvudmv)();
	NhlWorkspace	*flt_ws;
	char		*entry_name;
#endif
{
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *awsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	int		err_num;
	void		*awp,*fwp;

	awp = awsrp ? awsrp->ws_ptr : NULL;
	fwp = fwsrp ? fwsrp->ws_ptr : NULL;
	c_entsr(&save_mode,1);
	do {
		c_vvectr(u,v,p,awp,vvudmv,fwp);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			e_text = "%s: %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,e_msg);
			return NhlFATAL;
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}


/*
 * Function:	_NhlStinit
 *
 * Description: Streamlines routine STINIT
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlStinit
#if	NhlNeedProto
(
	float		*u,
	int		lu,
	float		*v,
	int		lv,
	float		*p,
	int		lp,
	int		m,
	int		n,				
	NhlWorkspace	*flt_ws,
	char		*entry_name
)
#else
(u,lu,v,lv,p,lp,m,n,flt_ws,entry_name)
	float		*u;
	int		lu;
	float		*v;
	int		lv;
	float		*p;
	int		lp;
	int		m;
	int		n;				
	NhlWorkspace	*flt_ws;
	char		*entry_name;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	char		*cmp_msg1 = "REAL WORKSPACE OVERFLOW";
	int		err_num;
	int		req_size;

	if (! (fwsrp && fwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	req_size = 5 * m * n * sizeof(float);
	if (fwsrp->cur_size != req_size) {
		int amount = req_size - fwsrp->cur_size;
		ret = ChangeWorkspaceSize(fwsrp,amount,entry_name);
		if (ret < NhlWARNING) return ret;
	}

	c_entsr(&save_mode,1);

	do {
		c_stinit(u,lu,v,lv,p,lp,m,n,
			 fwsrp->ws_ptr,(fwsrp->cur_size/sizeof(float)));

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			if (strstr(e_msg,cmp_msg1)) {
#if DEBUG_WS
				printf("resizing flt_ws old %d",
				       fwsrp->cur_size);
#endif
				ret = EnlargeWorkspace(fwsrp,entry_name);
				if (ret < NhlWARNING) return ret;
#if DEBUG_WS
				printf(" new %d\n", fwsrp->cur_size);
#endif
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
 * Function:	_NhlStream
 *
 * Description: Streamlines routine STREAM
 *		
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Friend
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes _NhlStream
#if	NhlNeedProto
(
	float		*u,
	float		*v,
	float		*p,
        NhlWorkspace	*amap_ws,
	int 		(*stumsl)(float *xcs, 
				  float *ycs,
				  int *ncs,
				  int *iai,
				  int *iag,
				  int *nai),
	NhlWorkspace	*flt_ws,
	char		*entry_name
)
#else
(u,v,p,amap_ws,stumsl,flt_ws,entry_name)
	float		*u;
	float		*v;
	float		*p;
        NhlWorkspace	*amap_ws;
	int 		(*stumsl)();
	NhlWorkspace	*flt_ws;
	char		*entry_name;
#endif
{
	char		*e_text;
	NhlWorkspaceRec *fwsrp = (NhlWorkspaceRec *) flt_ws;
	NhlWorkspaceRec *awsrp = (NhlWorkspaceRec *) amap_ws;
	int		save_mode;
	NhlBoolean	done = False;
	char		*e_msg;
	int		err_num;
	void		*awp;

	if (! (fwsrp && fwsrp->ws_ptr)) { 
		e_text = "%s: invalid workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,e_text,entry_name);
		return NhlFATAL;
	}
	awp = awsrp ? awsrp->ws_ptr : NULL;

	c_entsr(&save_mode,1);

	do {
		c_stream(u,v,p,awp,stumsl,fwsrp->ws_ptr);

		if (c_nerro(&err_num) == 0) {
			done = True;
		}
		else {
			e_msg = c_semess(0);
			c_errof();
			e_text = "%s: %s";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name,e_msg);
			return NhlFATAL;
		}
	} while (! done);
	
	c_retsr(save_mode);

	return NhlNOERROR;
}
