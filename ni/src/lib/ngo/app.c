/*
 *      $Id: app.c,v 1.1 1996-10-10 18:55:17 boote Exp $
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

#define	Oset(field)	NhlOffset(NgAppMgrRec,app.field)
static NhlResource resources[] = {

	{NgNappName,NgCappName,NhlTString,sizeof(NhlString),
		Oset(app_name),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CGONLY,(NhlFreeFunc)NhlFree},
	{NgNappClass,NgCappClass,NhlTString,sizeof(NhlString),
		Oset(app_class),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CGONLY,(NhlFreeFunc)NhlFree}

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

NgAppMgr	appMgr = NULL;

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
	app->active = NULL;

	ac->app_class.num_mgrs++;
	appMgr = (NgAppMgr)new;

	return NhlNOERROR;
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

	wk1 = ap->wp;
	while(wk1){
		wk2 = wk1;
		wk1 = wk1->next;
		NhlFree(wk2);
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
	NgAppMgrClass	ac;
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

/*
 * Function:	NgAppGetID
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
NgAppGetID
(
	void
)
{
	if(!appMgr){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can't find NgAppMgr???"));
		return NhlFATAL;
	}

	return appMgr->base.id;
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
