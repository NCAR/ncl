/*
 *      $Id: go.c,v 1.2 1996-10-16 16:21:18 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		go.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 19 13:29:08 MDT 1996
 *
 *	Description:	
 */
#include <ncarg/ngo/goP.h>
#include <ncarg/ngo/xapp.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/MenuShell.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>

#ifdef	DEBUG
#include <X11/Xmu/Editres.h>
#endif

#define	Oset(field)	NhlOffset(NgGORec,go.field)
static NhlResource resources[] = {
	{NgNgoTitle,NgCgoTitle,NhlTString,sizeof(NhlString),
		Oset(title),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_RCONLY,NULL},
};
#undef	Oset

static NhlErrorTypes GOClassPartInitialize(
	NhlClass	lc
);

static NhlErrorTypes GOInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes GODestroy(
	NhlLayer	l
);

static NhlBoolean GOCreateWin(
	NgGO		go
);

static NhlBoolean GOCreateWinHook(
	NgGO		go
);

NgGOClassRec NggOClassRec = {
	{
/* class_name			*/	"gOClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NgGOClassRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlobjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,

/* class_part_initialize	*/	GOClassPartInitialize,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	GOInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	GODestroy,

	},
	{
/* dialog		*/	NULL,
/* toplevel		*/	NULL,
/* manager		*/	NULL,

/* top_win_chain	*/	True,
/* create_win		*/	GOCreateWin,		/* D chained	*/
/* create_win_hook	*/	GOCreateWinHook,	/* U chained	*/

	}
};

NhlClass NggOClass = (NhlClass)&NggOClassRec;

/*
 * Function:	GOClassPartInitialize
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
GOClassPartInitialize
(
	NhlClass	lc
)
{
	NgGOClass	gc = (NgGOClass)lc;
	NgGOClass	sc = (NgGOClass)lc->base_class.superclass;
	NhlErrorTypes	ret = NhlNOERROR;

	gc->go_class.dialog = transientShellWidgetClass;
	gc->go_class.toplevel = applicationShellWidgetClass;
	gc->go_class.manager = xmFormWidgetClass;

	if(lc == NggOClass)
		return ret;

	if(!gc->go_class.create_win)
		gc->go_class.create_win = sc->go_class.create_win;
	if(!gc->go_class.create_win_hook)
		gc->go_class.create_win_hook = sc->go_class.create_win_hook;

	return ret;
}

static void
closeWindow
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	char	func[] = "closeWindow";
	int	goid = NhlDEFAULT_APP;

	if(!w){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid Widget",func));
		return;
	}

	goid = NgGOWidgetToGoId(w);
	NgGOPopdown(goid);
}

static void
quitApplication
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	char	func[] = "quitApplication";
	int	goid = NhlDEFAULT_APP;
	NgGO	go;

	if(!w){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid Widget",func));
		return;
	}

	goid = NgGOWidgetToGoId(w);
	go = (NgGO)_NhlGetLayer(goid);
	if(!go){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:can't find NgGO",func));
		return;
	}
	NgAppQuit(go->go.appmgr);
}

/*
 * Global actions for all parts of the application
 */
static XtActionsRec go_act[] = {
	{"closeWindow", closeWindow,},
	{"quitApplication", quitApplication,},
};

/*
 * Function:	GOInitialize
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
GOInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	static NhlBoolean	init = True;
	char			func[] = "GOInitialize";
	NgGOClass		gc = (NgGOClass)lc;
	NgGOPart		*go = &((NgGO)new)->go;
	NgGOPart		*rgo = &((NgGO)req)->go;
	NgGO			pgo;
	NhlArgVal		dummy,udata;
	NhlErrorTypes		ret = NhlNOERROR,lret;

	go->subshell = _NhlIsClass(new->base.parent,NggOClass);

	if(NhlIsClass((int)new->base.gui_data,NgappMgrClass))
		go->appmgr = (int)new->base.gui_data;
	else{
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid appmgr",func));
		return NhlFATAL;
	}

	if(go->title){
		go->title = NhlMalloc(sizeof(char)*(strlen(go->title)+1));
		if(!go->title){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		strcpy(go->title,rgo->title);
	}

	go->sensitive = True;
	go->x_sensitive = True;
	go->i_sensitive = True;

#ifdef	DEBUG
	memset(&dummy,0,sizeof(NhlArgVal));
	memset(&udata,0,sizeof(NhlArgVal));
#endif
	udata.ptrval = new;
	go->appdestroy_cb = _NhlAddObjCallback(_NhlGetLayer(go->appmgr),
				_NhlCBobjDestroy,dummy,NgDestroyMeCB,udata);

	lret = NhlVAGetValues(go->appmgr,
		NgNxappExport,	&go->x,
		NULL);
	if(lret < NhlWARNING)
		return lret;
	ret = MIN(lret,ret);

	if(init){
		init = False;
		XtAppAddActions(go->x->app_con,go_act,NhlNumber(go_act));
	}

	go->iowin = None;

	go->pup = True;
	go->pshell = NULL;

	go->up = False;
	go->shell = NULL;

	NgAppAddGO(go->appmgr,new->base.id);

	return NhlNOERROR;
}

static void
DestroyFunc
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NhlDestroy((int)udata);
}

/*
 * Function:	GODestroy
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
GODestroy
(
	NhlLayer	l
)
{
	NgGOPart	*go = &((NgGO)l)->go;

	NgAppRemoveGO(go->appmgr,l->base.id);

	_NhlCBDelete(go->appdestroy_cb);
	if(go->shell){
		XtRemoveCallback(go->shell,XmNdestroyCallback,DestroyFunc,
							(XtPointer)l->base.id);
		XtDestroyWidget(go->shell);
	}
	if(go->iowin != None)
		XDestroyWindow(go->x->dpy,go->iowin);

	return NhlNOERROR;
}

static void
Enable
(
	NgGO	go
)
{
	NgGOPart	*gp = &go->go;

	if(gp->iowin == None)
		return;

	XUnmapWindow(gp->x->dpy,gp->iowin);

	return;
}

static void
Disable
(
	NgGO	go
)
{
	char		func[] = "Disable";
	NgGOPart	*gp = &go->go;
	NgXAppExport	x = gp->x;

	if(!gp->shell || !XtIsRealized(gp->shell))
		return;

	if(gp->iowin == None){
		XSetWindowAttributes	att;
		unsigned long		vmask;

		vmask = CWDontPropagate | CWCursor | CWEventMask;
		att.do_not_propagate_mask = (KeyPressMask | KeyReleaseMask |
				ButtonPressMask | ButtonReleaseMask |
				PointerMotionMask | ButtonMotionMask);
		att.event_mask = (KeyPressMask | KeyReleaseMask |
				ButtonPressMask | ButtonReleaseMask |
				PointerMotionMask | ButtonMotionMask);
		att.cursor = x->wait;
		gp->iowin = XCreateWindow(x->dpy,XtWindow(gp->shell),
				0,0,WidthOfScreen(XtScreen(gp->shell)),
				HeightOfScreen(XtScreen(gp->shell)),
				0,CopyFromParent,InputOnly,CopyFromParent,
				vmask,&att);
		if(gp->iowin == None){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Can't create iowin",
								func));
			return;
		}
	}

	XMapRaised(x->dpy,gp->iowin);
	XFlush(x->dpy);

	return;
}

static void
PPopup
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	NgGO	go = (NgGO)udata;

	if(event->type != MapNotify)
		return;

	go->go.pup = True;
	if(!go->go.up)
		return;

	XtPopup(go->go.shell,XtGrabNone);

	if(!go->go.sensitive)
		Disable(go);

	return;
}

static void
PPopdown
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgGO	go = (NgGO)udata;

	go->go.pup = False;
	if(!go->go.up)
		return;

	XtPopdown(go->go.shell);
}

/*
 * GOClass methods
 */
static NhlBoolean
GOCreateWin
(
	NgGO	go
)
{
	char			func[]="GOCreateWin";
	NgGOPart		*pp;
	NgGOPart		*gp = &go->go;
	NgGOClass		gc = (NgGOClass)go->base.layer_class;
	NgXAppExport		x = gp->x;
	XWindowAttributes	att;
	char			mgrname[_NhlMAXRESNAMLEN];
	XtResource		xtres[] = {
		{NgNglobalTranslations,NgCglobalTranslations,
			XtRTranslationTable,sizeof(XtTranslations),
			XtOffset(NgGO,go.global_trans),XtRImmediate,NULL},
		};

	if(gp->subshell){
		pp = &((NgGO)go->base.parent)->go;
		if(!pp->shell){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:parent GO has invalid shell!",func));
			return False;
		}
		gp->pshell = pp->shell;
		gp->shell = XtVaCreatePopupShell(go->base.name,
						gc->go_class.dialog,pp->shell,
			XmNdeleteResponse,	XmDO_NOTHING,
			XmNautoUnmanage,	False,
			XmNallowShellResize,	True,
			NULL);

		XtAddEventHandler(pp->shell,SubstructureNotifyMask,
						False,PPopup,go);
		XtAddCallback(pp->shell,XmNpopdownCallback,PPopdown,go);
		gp->pup = XtIsRealized(pp->shell) &&
			XGetWindowAttributes(x->dpy,XtWindow(pp->shell),&att) &&
						(att.map_state == IsViewable);
	}
	else{
		gp->shell = XtVaAppCreateShell(go->base.name,"NgNGO",
					gc->go_class.toplevel,x->dpy,
			XmNdeleteResponse,	XmDO_NOTHING,
			XmNautoUnmanage,	False,
			XmNallowShellResize,	True,
			XmNuserData,		go->base.id,
			NULL);
	}
	XmAddWMProtocolCallback(gp->shell,x->wm_delete_window,_NgGOPopdownCB,
							(XtPointer)go->base.id);
	XtAddCallback(gp->shell,XmNdestroyCallback,DestroyFunc,
							(XtPointer)go->base.id);
#ifdef	DEBUG
	/*
	 * If debug, turn on editres
	 */
	XtAddEventHandler(gp->shell,(EventMask)0,True,
			(XtEventHandler)_XEditResCheckMessages,NULL);
#endif

	/*
	 * Retrieve application defined resources for this shell.
	 */
	XtGetApplicationResources(gp->shell,go,xtres,XtNumber(xtres),NULL,0);

	/*
	 * I set both BulletinBoard and RowColumn and resources since Xt
	 * doesn't complain if you try to set resources that don't exist.
	 */
	sprintf(mgrname,"%sMGR",go->base.name);
	gp->manager = XtVaCreateManagedWidget(mgrname,gc->go_class.manager,
								gp->shell,
		XmNuserData,		go->base.id,
		/* bulletinBoard Resources	*/
		XmNautoUnmanage,	False,
		XmNresizePolicy,	XmRESIZE_ANY,
		/* RowColumn Resources		*/
		XmNresizeHeight,	True,
		XmNresizeWidth,		True,
		NULL);

	return True;
}

static void
InstallTranslations
(
	Widget		w,
	NgGOPart	*gp
)
{
	if(!w)
		return;

	/*
	 * Install global translations
	 */
	if(gp->global_trans)
		XtOverrideTranslations(w,gp->global_trans);

	/*
	 * Install text translations
	 */
	if(XmIsText(w) || XmIsTextField(w)){
		;
	}

	if(XtIsComposite(w)){
		WidgetList	list;
		Cardinal	nlist=0;
		int		i;

		XtVaGetValues(w,
			XmNchildren,	&list,
			XmNnumChildren,	&nlist,
			NULL);

		for(i=0;i<nlist;i++)
			InstallTranslations(list[i],gp);
	}

	return;
}

static NhlBoolean
GOCreateWinHook
(
	NgGO	go
)
{
	char			func[]="GOCreateWinHook";
	NgGOPart		*gp = &go->go;
	NgGOClass		gc = (NgGOClass)go->base.layer_class;

	InstallTranslations(gp->shell,gp);

	return;
}
/*
 * Static funcs that are not methods
 */
static NhlBoolean
CallCreateWin
(
	NgGO		go,
	NgGOClass	gc
)
{
	NgGOClass	sc = (NgGOClass)gc->base_class.superclass;
	NhlBoolean	ret=True;

	if(!gc->go_class.top_win_chain)
		ret = CallCreateWin(go,sc);

	if(!ret)
		return;

	if(gc->go_class.create_win)
		return (*gc->go_class.create_win)(go);

	return ret;
}

static NhlBoolean
CallCreateWinHook
(
	NgGO		go,
	NgGOClass	gc
)
{
	NgGOClass	sc = (NgGOClass)gc->base_class.superclass;
	NhlBoolean	ret = True;

	if(gc->go_class.create_win_hook)
		ret = (*gc->go_class.create_win_hook)(go);

	if(ret && !gc->go_class.top_win_chain)
		return CallCreateWinHook(go,sc);

	return ret;
}

/*
 * Private API for sub-classes
 */

void
_NgIGOSensitive
(
	NgGO		go,
	NhlBoolean	sensitive
)
{
	NgGOPart	*gp = &go->go;
	NgGOClass	gc = (NgGOClass)go->base.layer_class;

	if(sensitive){
		if(gp->x_sensitive && !gp->sensitive){
			Enable(go);
			gp->sensitive = True;
		}
	
		gp->i_sensitive = True;
	}
	else{
		if(gp->sensitive)
			Disable(go);
	
		gp->sensitive = False;
		gp->i_sensitive = False;
	}

	return;
}

void
_NgGOPopupCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgGOPopup((int)udata);

	return;
}

void
_NgGOPopdownCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgGOPopdown((int)udata);

	return;
}

void
_NgGODefActionCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	char			func[]="_NgGODefActionCB";
	XmAnyCallbackStruct	*xmcb = (XmAnyCallbackStruct*)cbdata;
	NhlString		*params = (NhlString*)udata;
	int			i=0;

	if(!w){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:No Widget!",func));
		return;
	}

	if(params){
		while(*params++)
			i++;
		params = (NhlString*)udata;
	}

	XtCallActionProc(w,XtName(w),xmcb->event,params,i);
}
/*
 * Public API
 */
void
NgGOCreateWindow
(
	int	goid
)
{
	char		func[] = "NgGOCreateWindow";
	NgGO		go = (NgGO)_NhlGetLayer(goid);
	NgGOClass	gc;

	if(!go || !_NhlIsClass((NhlLayer)go,NggOClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid goid %d",
								func,goid));
		return;
	}

	if(go->go.shell)
		return;
	gc = (NgGOClass)go->base.layer_class;

	if(CallCreateWin(go,gc))
		CallCreateWinHook(go,gc);

	return;
}

/*
 * Function:	NgGOSensitive
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
NgGOSensitive
(
	int		goid,
	NhlBoolean	sensitive
)
{
	char		func[] = "NgGOSensitive";
	NgGO		go = (NgGO)_NhlGetLayer(goid);
	NgGOPart	*gp;
	NgGOClass	gc;

	if(!go || !_NhlIsClass((NhlLayer)go,NggOClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid goid %d",
								func,goid));
		return;
	}
	gp = &go->go;
	gc = (NgGOClass)go->base.layer_class;

	if(sensitive){
		if(gp->i_sensitive && !gp->sensitive){
			Enable(go);
			gp->sensitive = True;
		}
	
		gp->x_sensitive = True;
	}
	else{
		if(gp->sensitive)
			Disable(go);
	
		gp->sensitive = False;
		gp->x_sensitive = False;
	}

	return;
}

void
NgGOPopup
(
	int		goid
)
{
	char		func[] = "NgGOPopup";
	NgGO		go = (NgGO)_NhlGetLayer(goid);
	NgGOPart	*gp;
	NgGOClass	gc;

	if(!go || !_NhlIsClass((NhlLayer)go,NggOClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid goid %d",
								func,goid));
		return;
	}
	gp = &go->go;


	if(!gp->shell)
		NgGOCreateWindow(go->base.id);

	gp->up = True;
	if(!gp->pup)
		return;

	XtPopup(gp->shell,XtGrabNone);

	if(!go->go.sensitive)
		Disable(go);

	return;
}

void
NgGOPopdown
(
	int		goid
)
{
	char		func[] = "NgGOPopdown";
	NgGO		go = (NgGO)_NhlGetLayer(goid);
	NgGOPart	*gp;

	if(!go || !_NhlIsClass((NhlLayer)go,NggOClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid goid %d",
								func,goid));
		return;
	}
	gp = &go->go;

	gp->up = False;

	if(!gp->pup)
		return;

	if(gp->shell)
		XtPopdown(gp->shell);

	return;
}

int
NgGOWidgetToGoId
(
	Widget		w
)
{
	char		func[]="NgGOWidgetToGoId";
	Widget		shell=w;
	WidgetList	list;
	Cardinal	nlist;
	Widget		manager;
	int		i;
	int		goid=NhlDEFAULT_APP;

	/*
	 * Find toplevel shell that is not a menushell
	 */
	while(shell && (!XtIsShell(shell) || XmIsMenuShell(shell)))
		shell = XtParent(shell);

	if(!shell){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"%s:can't find top shell from %s",func,XtName(w)));
		return goid;
	}

	XtVaGetValues(shell,
		XmNchildren,	&list,
		XmNnumChildren,	&nlist,
		NULL);
	for(i=0;i<nlist;i++){
		if(XmIsManager(list[i])){
			XtVaGetValues(list[i],
				XmNuserData,	&goid,
				NULL);
			if(NhlIsClass(goid,NggOClass))
				break;
		}
		goid = NhlDEFAULT_APP;
	}

	return goid;
}
