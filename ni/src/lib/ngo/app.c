/*
 *      $Id: app.c,v 1.5 1997-02-27 20:25:43 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		app.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 29 15:55:07 MDT 1996
 *
 *	Description:	
 */
#include <ncarg/ngo/appP.h>
#include <ncarg/hlu/AppI.h>
#include <ncarg/ngo/ncledit.h>

#define	Oset(field)	NhlOffset(NgAppMgrRec,app.field)
static NhlResource resources[] = {

	{NgNappName,NgCappName,NhlTString,sizeof(NhlString),
		Oset(app_name),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CGONLY,(NhlFreeFunc)NhlFree},
	{NgNappClass,NgCappClass,NhlTString,sizeof(NhlString),
		Oset(app_class),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CGONLY,(NhlFreeFunc)NhlFree},
	{NgNappNclState,NgCappNclState,NhlTInteger,sizeof(int),Oset(nclstate),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlDEFAULT_APP),
		_NhlRES_SGONLY,(NhlFreeFunc)NULL},

};
#undef	Oset

static NhlErrorTypes AppMgrClassPartInitialize(
	NhlClass	lc
);

static NhlErrorTypes AppMgrClassInitialize(
	void
);

static NhlErrorTypes AppMgrInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes AppMgrDestroy(
	NhlLayer	l
);

static void AppError(
	NgAppMgr	app
);

NgAppMgrClassRec NgappMgrClassRec = {
	{
/* class_name			*/	"appMgrClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NgAppMgrRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlobjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize	*/	AppMgrClassPartInitialize,
/* class_initialize		*/	AppMgrClassInitialize,
/* layer_initialize		*/	AppMgrInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	AppMgrDestroy,

	},
	{
/* num_mgrs			*/	0,
/* run_proc			*/	AppError,
/* dev_wproc			*/	(NgDevWorkProc)AppError,
	}
};

NhlClass NgappMgrClass = (NhlClass)&NgappMgrClassRec;

/*
 * Function:	AppMgrClassPartInitialize
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
static NhlErrorTypes
AppMgrClassPartInitialize
(
	NhlClass	lc
)
{
	return NhlNOERROR;
}

/*
 * Function:	AppMgrClassInitialize
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
static NhlErrorTypes
AppMgrClassInitialize
(
	void
)
{

	return NhlNOERROR;
}

/*
 * Function:	AppMgrInitialize
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
static NhlErrorTypes
AppMgrInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char		func[] = "AppMgrInitialize";
	NgAppMgrClass	ac = (NgAppMgrClass)lc;
	NgAppMgrPart	*app = &((NgAppMgr)new)->app;
	NhlClass	amc = new->base.layer_class;

	if(amc == NgappMgrClass){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"Cannot create an AppMgr class!!"));
		return NhlFATAL;
	}

	if(ac->app_class.num_mgrs){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:Only one AppMgr can be created",func));
		return NhlFATAL;
	}

	if(!app->app_name || !app->app_class){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"%s:A Required resource not set",func));
		return NhlFATAL;
	}

	app->wp = NULL;
	app->go = NULL;
	app->ncleditors = NULL;
	app->active = NULL;

	ac->app_class.num_mgrs++;

	_NhlAppSetDefGuiData((NhlPointer)new->base.id);
	NhlVASetValues(new->base.appobj->base.id,
		_NhlNguiData,	new->base.id,
		NULL);

	return NhlNOERROR;
}

static void
FreeGOList
(
	_NgAppGOList	gol
)
{
	int	i,j;

	if(!gol)
		return;

	FreeGOList(gol->next);

	i=0;
	while(i < _NgGOLISTSIZE && gol->num){
		if(gol->go[i] != NhlDEFAULT_APP){
			NhlDestroy(gol->go[i]);
			gol->num--;
		}
		i++;
	}

	NhlFree(gol);

	return;
}

/*
 * Function:	AppMgrDestroy
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
static NhlErrorTypes
AppMgrDestroy
(
	NhlLayer	l
)
{
	NgAppMgr	app = (NgAppMgr)l;
	NgAppMgrPart	*ap = &app->app;
	NgAppMgrClass	ac = (NgAppMgrClass)NgappMgrClass;
	int		i;
	_NgWorkProc	wk1,wk2;
	_NgAppFStack	afs1,afs2;

	wk1 = ap->wp;
	while(wk1){
		wk2 = wk1;
		wk1 = wk1->next;
		NhlFree(wk2);
	}

	FreeGOList(ap->go);
	FreeGOList(ap->ncleditors);

	afs1 = ap->active;
	while(afs1){
		afs2 = afs1;
		afs1 = afs2->next;
		NhlFree(afs2);
	}

	return NhlNOERROR;
}

/*
 * Function:	AppError
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
static void
AppError
(
	NgAppMgr	app
)
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,
			"AppError:AppMgr subclass \"%s\" messed up.",
			_NhlClassName(app->base.layer_class));
	return;
}

/************************************************************************
 *									*
 *				Exported API				*
 *									*
 ***********************************************************************/

/*
 * Function:	NgAppRun
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
void
NgAppRun
(
	int	appid
)
{
	char		func[] = "NgAppRun";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	NgAppMgrClass	ac;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	ac = (NgAppMgrClass)app->base.layer_class;

	if(!ac->app_class.run_proc){
		AppError(app);
		return;
	}

	(*(ac->app_class.run_proc))(app);

	return;
}

/*
 * Function:	NgAppQuit
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
void
NgAppQuit
(
	int	appid
)
{
	char		func[] = "NgAppQuit";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	NgAppMgrClass	ac;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	ac = (NgAppMgrClass)app->base.layer_class;
/*
 * TODO:	call a quitConfirm callback - iff cbdata comes back true,
 *		then call quitNow callback. If that returns, exit.
 */
	exit(0);

	return;
}

static NhlBoolean
DoWork
(
	NgAppMgr	app
)
{
	NhlBoolean	done;
	_NgWorkProc	tmp = app->app.wp;

	done = (*tmp->proc)(tmp->cdata);

	if(done){
		app->app.wp = tmp->next;
		NhlFree(tmp);
	}

	return !app->app.wp;
}


/*
 * Function:	NgAddWorkProc
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
void
NgAddWorkProc
(
	int		appid,
	NgWorkProc	work_proc,
	NhlPointer	cdata
)
{
	char		func[] = "NgAddWorkProc";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	NgAppMgrClass	ac;
	_NgWorkProc	wp,*tmp;
	NhlBoolean	pending;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	ac = (NgAppMgrClass)app->base.layer_class;

	if(!ac->app_class.dev_wproc){
		AppError(app);
		return;
	}

	pending = (app->app.wp && True);
	wp = NhlMalloc(sizeof(_NgWorkProcRec));
	if(!wp){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	wp->proc = work_proc;
	wp->cdata = cdata;
	wp->next = NULL;

	for(tmp = &app->app.wp;*tmp;tmp = &(*tmp)->next);
	*tmp = wp;

	/*
	 * Only install a work proc at the device level, if there
	 * isn't one already installed.
	 */
	if(!pending)
		(*(ac->app_class.dev_wproc))(app,DoWork);

	return;
}

/*
 * Function:	NgAddPriorityWorkProc
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
void
NgAddPriorityWorkProc
(
	int		appid,
	NgWorkProc	work_proc,
	NhlPointer	cdata
)
{
	char		func[] = "NgAddPriorityWorkProc";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	NgAppMgrClass	ac;
	_NgWorkProc	wp;
	NhlBoolean	pending;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	ac = (NgAppMgrClass)app->base.layer_class;

	if(!ac->app_class.dev_wproc){
		AppError(app);
		return;
	}

	pending = (app->app.wp && True);
	wp = NhlMalloc(sizeof(_NgWorkProcRec));
	if(!wp){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	wp->proc = work_proc;
	wp->cdata = cdata;
	wp->next = app->app.wp;
	app->app.wp = wp;

	/*
	 * Only install a work proc at the device level, if there
	 * isn't one already installed.
	 */
	if(!pending)
		(*(ac->app_class.dev_wproc))(app,DoWork);

	return;
}

/*
 * Function:	NgRemoveWorkProc
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
void
NgRemoveWorkProc
(
	int		appid,
	NgWorkProc	work_proc,
	NhlPointer	cdata
)
{
	char		func[] = "NgRemoveWorkProc";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgWorkProc	tmp,*wp;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	wp = &app->app.wp;

	while(*wp){
		if(((*wp)->proc == work_proc) &&
			((*wp)->cdata == cdata)){

			tmp = *wp;
			*wp = (*wp)->next;
			NhlFree(tmp);

			return;
		}

		wp = &(*wp)->next;
	}

	NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			"%s:Unable to remove proc",func));
	return;
}

#define	NgCBWPCALLING	(0x01)
#define	NgCBWPCLEANOUT	(0x02)
#define	NgCBWPDESTROY	(0x04)

typedef struct _NgCBWP_WPLRec _NgCBWP_WPLRec, *_NgCBWP_WPL;

struct NgCBWPRec{
	int		appmgrid;
	NgCBWPCopyFunc	copy_func;
	NgCBWPFreeFunc	free_func;
	_NhlCBFunc	cb_func;
	NhlArgVal	udata;

	int		state;
	int		co_id;
	_NhlCB		cb;
	_NhlCB		ldestroycb;
	_NhlCB		adestroycb;
	_NgCBWP_WPL	wp_list;
};

struct _NgCBWP_WPLRec{
	NgCBWP		cbwp;
	NhlArgVal	cbdata;
	_NgCBWP_WPL	next;
};

static void
_NgCBWPCleanout(
	NgCBWP		cbwp
);

/*
 * This function actually executes the cb_func that was added with the
 * NgCBWPAdd call - it frees the cbdata after calling the cb_func.
 */
static NhlBoolean
_NgCBWPWorkProc
(
	NhlPointer	cdata
)
{
	_NgCBWP_WPL	wpnode = (_NgCBWP_WPL)cdata;
	_NgCBWP_WPL	*wpptr;
	NgCBWP		cbwp = wpnode->cbwp;

	if(cbwp->state & NgCBWPCALLING)
		return False;

	cbwp->state = NgCBWPCALLING;
	(*cbwp->cb_func)(wpnode->cbdata,cbwp->udata);
	if(cbwp->free_func)
		(*cbwp->free_func)(wpnode->cbdata);

	wpptr = &cbwp->wp_list;
	while(*wpptr){
		if(*wpptr == wpnode){
			*wpptr = wpnode->next;
			break;
		}
		wpptr = &(*wpptr)->next;
	}

	NhlFree(wpnode);

	cbwp->state &= ~NgCBWPCALLING;
	/*
	 * If this CBWP list was marked for destroy during the call of
	 * the function - then free the whole thing now.
	 */
	if(cbwp->state & NgCBWPDESTROY)
		NgCBWPDestroy(cbwp);
	else if(cbwp->state & NgCBWPCLEANOUT)
		_NgCBWPCleanout(cbwp);

	return True;
}

/*
 * This is the function that is actually called by the NhlLayer's callback
 * list.  It just copies the cbdata and adds a work proc that will call the
 * users routine with the copy of the cbdata.
 */
static void
_NgCBWPCallback
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NhlArgVal	lcbdata;
	_NgCBWP_WPL	wpnode;
	NgCBWP		cbwp = (NgCBWP)udata.ptrval;

	if(cbwp->state & (NgCBWPDESTROY | NgCBWPCLEANOUT))
		return;

	NhlINITVAR(lcbdata);
	if(cbwp->copy_func){
		if(!(*cbwp->copy_func)(cbdata,&lcbdata))
			return;
	}
	else
		lcbdata = cbdata;

	wpnode = NhlMalloc(sizeof(_NgCBWP_WPLRec));
	if(!wpnode){
		if(cbwp->free_func)
			(*cbwp->free_func)(lcbdata);
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	wpnode->cbwp = cbwp;
	wpnode->cbdata = lcbdata;
	wpnode->next = cbwp->wp_list;
	cbwp->wp_list = wpnode;
	NgAddWorkProc(cbwp->appmgrid,_NgCBWPWorkProc,(NhlPointer)wpnode);

	return;
}

static void
_NgCBWPCleanout
(
	NgCBWP		cbwp
)
{
	_NgCBWP_WPL	node;

	if(cbwp->state & NgCBWPCALLING){
		cbwp->state |= NgCBWPCLEANOUT;
		return;
	}

	_NhlCBDelete(cbwp->cb);
	_NhlCBDelete(cbwp->ldestroycb);
	_NhlCBDelete(cbwp->adestroycb);
	cbwp->cb = cbwp->ldestroycb = cbwp->adestroycb = NULL;

	if((cbwp->co_id == NhlDEFAULT_APP) || (cbwp->co_id == cbwp->appmgrid)){
		while(cbwp->wp_list){
			node = cbwp->wp_list;
			cbwp->wp_list = node->next;
			if(cbwp->free_func)
				(*cbwp->free_func)(node->cbdata);
			NgRemoveWorkProc(cbwp->appmgrid,_NgCBWPWorkProc,
							(NhlPointer)node);
			NhlFree(node);
		}
	}

	return;
}

/*
 * This function is added as Destroy callback to the appmgr and to the
 * NhlLayer that the original callback list was installed to.  It removes
 * the callback, and if the object being destroyed is the appmgr, then
 * it also free's the _NgCBWP_WPL list since the work proc's won't be
 * executed anyway.
 */
static void
_NgCBWPDeleteCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgCBWP		cbwp = (NgCBWP)udata.ptrval;
	NhlLayer	l = (NhlLayer)cbdata.ptrval;

	if(l->base.id != cbwp->appmgrid)
		cbwp->co_id = l->base.id;

	_NgCBWPCleanout(cbwp);

	return;
}

/*
 * Function:	NgCBWPAdd
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
extern NgCBWP
NgCBWPAdd
(
	int		appmgrid,
	NgCBWPCopyFunc	copy_func,
	NgCBWPFreeFunc	free_func,
	NhlLayer	l,
	NhlString	cbname,
	NhlArgVal	sel,
	_NhlCBFunc	cb_func,
	NhlArgVal	udata
)
{
	char		func[] = "NgCBWPAdd";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appmgrid);
	NhlArgVal	ludata,lsel;
	NgCBWP		cbwp;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	cbwp = NhlMalloc(sizeof(NgCBWPRec));
	if(!cbwp){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	cbwp->state = 0;
	cbwp->co_id = NhlDEFAULT_APP;
	NhlINITVAR(ludata);
	NhlINITVAR(lsel);
	ludata.ptrval = cbwp;
	cbwp->cb = _NhlAddObjCallback(l,cbname,sel,_NgCBWPCallback,ludata);
	if(!cbwp->cb){
		NhlFree(cbwp);
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to add CBWP",func));
		return NULL;
	}

	cbwp->ldestroycb = _NhlAddObjCallback(l,_NhlCBobjDestroy,
						lsel,_NgCBWPDeleteCB,ludata);
	cbwp->adestroycb = _NhlAddObjCallback((NhlLayer)app,_NhlCBobjDestroy,
						lsel,_NgCBWPDeleteCB,ludata);

	cbwp->appmgrid = appmgrid;
	cbwp->copy_func = copy_func;
	cbwp->free_func = free_func;
	cbwp->cb_func = cb_func;
	cbwp->udata = udata;

	cbwp->wp_list = NULL;

	return cbwp;
}

/*
 * Function:	NgCBWPDestroy
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
extern void
NgCBWPDestroy
(
	NgCBWP	cbwp
)
{
	_NgCBWP_WPL	node;

	if(!cbwp)
		return;

	if(cbwp->state & NgCBWPCALLING){
		cbwp->state |= NgCBWPDESTROY;
		return;
	}

	cbwp->co_id = NhlDEFAULT_APP;
	_NgCBWPCleanout(cbwp);

	NhlFree(cbwp);

	return;
}

/*
 * Function:	NgAppAddGO
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
void
NgAppAddGO
(
	int	appid,
	int	goid
)
{
	char		func[] = "NgAppAddGO";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppGOList	*go;
	int		i;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	go = &app->app.go;
	while(*go && (*go)->num >= _NgGOLISTSIZE)
		go = &(*go)->next;

	if(!*go){
		*go = NhlMalloc(sizeof(_NgAppGOListRec));
		if(!*go){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		memset((*go)->go,NhlDEFAULT_APP,sizeof(int)*_NgGOLISTSIZE);
		(*go)->num = 0;
		(*go)->next = NULL;
	}

	for(i=0;i<_NgGOLISTSIZE;i++){
		if((*go)->go[i] == NhlDEFAULT_APP){
			(*go)->go[i] = goid;
			(*go)->num++;
			/*
			 * If there is a current grab, then this needs
			 * to be added in the disabled state.
			 */
			if(app->app.active)
				NgGOSensitive(goid,False);
			return;
		}
	}

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to add goid???",func));
	return;
}

/*
 * Function:	NgAppRemoveGO
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
void
NgAppRemoveGO
(
	int	appid,
	int	goid
)
{
	char		func[] = "NgAppRemoveGO";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppGOList	go;
	int		i;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	go = app->app.go;

	while(go){
		if(go->num){
			for(i=0;i<_NgGOLISTSIZE;i++){
				if(go->go[i] == goid){
					go->go[i] = NhlDEFAULT_APP;
					go->num--;
					NgAppReleaseFocus(appid,goid);
					return;
				}
			}
		}
		go = go->next;
	}

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to find goid %d",func,goid));
	return;
}

/*
 * Function:	NgAppAddNclEditor
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
void
NgAppAddNclEditor
(
	int	appid,
	int	goid
)
{
	char		func[] = "NgAppAddNclEditor";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppGOList	*go;
	int		i;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	go = &app->app.ncleditors;
	while(*go && (*go)->num >= _NgGOLISTSIZE)
		go = &(*go)->next;

	if(!*go){
		*go = NhlMalloc(sizeof(_NgAppGOListRec));
		if(!*go){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		memset((*go)->go,NhlDEFAULT_APP,sizeof(int)*_NgGOLISTSIZE);
		(*go)->num = 0;
		(*go)->next = NULL;
	}

	for(i=0;i<_NgGOLISTSIZE;i++){
		if((*go)->go[i] == NhlDEFAULT_APP){
			(*go)->go[i] = goid;
			(*go)->num++;
			return;
		}
	}

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to add NclEditor???",func));
	return;
}

/*
 * Function:	NgAppRemoveNclEditor
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
void
NgAppRemoveNclEditor
(
	int	appid,
	int	goid
)
{
	char		func[] = "NgAppRemoveNclEditor";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppGOList	go;
	int		i;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	go = app->app.ncleditors;

	while(go){
		if(go->num){
			for(i=0;i<_NgGOLISTSIZE;i++){
				if(go->go[i] == goid){
					go->go[i] = NhlDEFAULT_APP;
					go->num--;
					return;
				}
			}
		}
		go = go->next;
	}

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to find NclEditor %d",func,
									goid));
	return;
}

/*
 * Function:	NgAppGetNclEditor
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
int
NgAppGetNclEditor
(
	int		appid,
	NhlBoolean	new
)
{
	char		func[] = "NgAppGetNclEditor";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppGOList	go;
	int		i;
	int		ne;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	go = app->app.ncleditors;

	while(go && !new){
		if(go->num){
			for(i=0;i<_NgGOLISTSIZE;i++){
				if(go->go[i] != NhlDEFAULT_APP)
					return go->go[i];
			}
		}
		go = go->next;
	}

	NhlVACreate(&ne,"ncledit",NgnclEditClass,app->base.appobj->base.id,
		NULL);

	return ne;
}

/*
 * Function:	NgAppGrabFocus
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
void
NgAppGrabFocus
(
	int	appid,
	int	goid
)
{
	char		func[] = "NgAppGrabFocus";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppFStack	fs;
	_NgAppGOList	go;
	int		i;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	fs = NhlMalloc(sizeof(_NgAppFStackRec));
	if(!fs){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	fs->active = goid;
	fs->next = app->app.active;
	app->app.active = fs;

	go = app->app.go;
	while(go){
		if(go->num){
			for(i=0;i<_NgGOLISTSIZE;i++){
				if(go->go[i] == NhlDEFAULT_APP) continue;
				NgGOSensitive(go->go[i],(go->go[i] == goid));
			}
		}
		go = go->next;
	}

	return;
}

/*
 * Function:	NgAppReleaseFocus
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
void
NgAppReleaseFocus
(
	int	appid,
	int	goid
)
{
	char		func[] = "NgAppReleaseFocus";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppFStack	*fsp,fs=NULL;
	_NgAppGOList	go;
	int		i;
	NhlBoolean	enable = False;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	fsp = &app->app.active;
	while(*fsp){
		if((*fsp)->active == goid){
			fs = *fsp;
			*fsp = (*fsp)->next;
			break;
		}
	}

	if(!fs) return;
	NhlFree(fs);

	if(app->app.active)
		goid = app->app.active->active;
	else
		enable = True;

	go = app->app.go;
	while(go){
		for(i=0;i<_NgGOLISTSIZE;i++){
			if(go->go[i] == NhlDEFAULT_APP) continue;
			NgGOSensitive(go->go[i],
					(enable || (go->go[i] == goid)));
		}
		go = go->next;
	}
}
