/*
 *      $Id: go.c,v 1.5 1997-02-27 20:25:43 boote Exp $
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
#include <ncarg/ngo/nclstate.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/MenuShell.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushBG.h>

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
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

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

/*
 * Function:	addFile
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
addFile
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	char		func[] = "addFile";
	int		goid = NhlDEFAULT_APP;
	int		appmgr = NhlDEFAULT_APP;
	int		addfile = NhlDEFAULT_APP;

	goid = NgGOWidgetToGoId(w);
	if(goid == NhlDEFAULT_APP){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid Widget",func));
		return;
	}
	NhlVAGetValues(goid,
		_NhlNguiData,	&appmgr,
		NULL);

	if((*num_params == 1) || (*num_params > 2)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:wrong number of params",func));
		return;
	}
	else if(*num_params == 2){
		int		nclstate = NhlDEFAULT_APP;
		struct stat	buf;
		char		line[512];

		if((strlen(params[0])+strlen(params[1])) > (sizeof(line) - 19)){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:parameters too long:%s:%s",
				func,params[0],params[1]));
			return;
		}

		NhlVAGetValues(appmgr,
			NgNappNclState,	&nclstate,
			NULL);
		if(!NhlIsClass(nclstate,NgnclStateClass)){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:invalid nclstate obj",func));
			return;
		}

		/*
		 * Check to see if file exists and is text...
		 */
		if(stat(params[1],&buf) != 0){
			NHLPERROR((NhlFATAL,errno,"%s:unable to access file %s",
							func,params[0]));
			return;
		}

		/*
		 * Submit it to nclstate.
		 */
		sprintf(line,"%s = addfile(\"%s\",\"r\")\n",
							params[0],params[1]);
		(void)NgNclSubmitBlock(nclstate,line);

		return;
	}

	/*
	 * Popup addfile selection box.
	 */
	NhlVAGetValues(appmgr,
		NgNxappAddFile,	&addfile,
		NULL);
	/*
	 *TODO: move addfile window to center of goid window.
	 */
	NgGOPopup(addfile);

	return;
}

/*
 * Function:	loadScript
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
loadScript
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	char		func[] = "loadScript";
	int		goid = NhlDEFAULT_APP;
	int		appmgr = NhlDEFAULT_APP;
	int		load = NhlDEFAULT_APP;

	goid = NgGOWidgetToGoId(w);
	if(goid == NhlDEFAULT_APP){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid Widget",func));
		return;
	}

	NhlVAGetValues(goid,
		_NhlNguiData,	&appmgr,
		NULL);

	if(*num_params > 1){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:wrong number of params",func));
		return;
	}
	else if(*num_params == 1){
		int		nclstate = NhlDEFAULT_APP;
		struct stat	buf;
		char		line[512];

		if(strlen(params[0]) > (sizeof(line) - 9)){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:parameter too long:%s",func,params[0]));
			return;
		}
		NhlVAGetValues(appmgr,
			NgNappNclState,	&nclstate,
			NULL);

		if(!NhlIsClass(nclstate,NgnclStateClass)){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:invalid nclstate obj",func));
			return;
		}

		/*
		 * Check to see if file exists and is text...
		 */

		if(stat(params[0],&buf) != 0){
			NHLPERROR((NhlFATAL,errno,"%s:unable to access file %s",
							func,params[0]));
			return;
		}
		/*
		 * Submit it to nclstate.
		 */
		sprintf(line,"load \"%s\"\n",params[0]);
		(void)NgNclSubmitBlock(nclstate,line);

		return;
	}

	/*
	 * Popup addfile selection box.
	 */
	NhlVAGetValues(appmgr,
		NgNxappLoadFile,	&load,
		NULL);
	/*
	 *TODO: move load window to center of goid window.
	 */
	NgGOPopup(load);

	return;
}

/*
 * Function:	nclWindow
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
nclWindow
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	char		func[] = "nclWindow";
	int		goid = NhlDEFAULT_APP;
	int		appmgr = NhlDEFAULT_APP;
	int		ne = NhlDEFAULT_APP;
	NhlBoolean	new = False;

	goid = NgGOWidgetToGoId(w);
	if(goid == NhlDEFAULT_APP){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid Widget",func));
		return;
	}

	NhlVAGetValues(goid,
		_NhlNguiData,	&appmgr,
		NULL);

	if(*num_params > 1){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:wrong number of params",func));
		return;
	}
	else if((*num_params == 1) && !strcmp(params[0],"new")){
		new = True;
	}

	ne = NgAppGetNclEditor(appmgr,new);

	NgGOPopup(ne);

	return;
}

/*
 * Global actions for all parts of the application
 */
static XtActionsRec go_act[] = {
	{"quitApplication", quitApplication,},
	{"closeWindow", closeWindow,},
	{"addFile", addFile,},
	{"loadScript", loadScript,},
	{"nclWindow", nclWindow,},
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

	NhlINITVAR(dummy);
	NhlINITVAR(udata);
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
		XtAppAddActions(go->x->app,go_act,NhlNumber(go_act));
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
			XmNmappedWhenManaged,	False,
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
			XmNmappedWhenManaged,	False,
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
	 * I set both BulletinBoard and RowColumn resources since Xt
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
	if(gp->global_trans && XtIsWidget(w))
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

	return True;
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
		return ret;

	if(gc->go_class.create_win)
		ret = (*gc->go_class.create_win)(go);

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

	if(!gc->go_class.top_win_chain)
		ret = CallCreateWinHook(go,sc);

	if(!ret)
		return ret;

	if(gc->go_class.create_win_hook)
		ret = (*gc->go_class.create_win_hook)(go);

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

Widget
_NgGOCreateMenubar
(
	NgGO	go,
	Widget	manager
)
{
	Widget	m;
	Widget	menubar,menush,fmenu,emenu;
	Widget	vmenu,omenu,wmenu,hmenu;
	Widget	file,edit,view,options,window,help;
	Widget	addfile,load,close,quit;
	Widget	ncledit;

	if(manager)
		m = manager;
	else
		m = go->go.manager;

	menubar =XtVaCreateManagedWidget("menubar",xmRowColumnWidgetClass,m,
		XmNrowColumnType,	XmMENU_BAR,
		NULL);

	menush = XtVaCreatePopupShell("menush",xmMenuShellWidgetClass,
								go->go.shell,
		XmNwidth,		5,
		XmNheight,		5,
		XmNallowShellResize,	True,
		XtNoverrideRedirect,	True,
		NULL);
	fmenu = XtVaCreateWidget("fmenu",xmRowColumnWidgetClass,menush,
		XmNrowColumnType,	XmMENU_PULLDOWN,
		NULL);

	emenu = XtVaCreateWidget("emenu",xmRowColumnWidgetClass,menush,
		XmNrowColumnType,	XmMENU_PULLDOWN,
		NULL);

	vmenu = XtVaCreateWidget("vmenu",xmRowColumnWidgetClass,menush,
		XmNrowColumnType,	XmMENU_PULLDOWN,
		NULL);

	omenu = XtVaCreateWidget("omenu",xmRowColumnWidgetClass,menush,
		XmNrowColumnType,	XmMENU_PULLDOWN,
		NULL);

	wmenu = XtVaCreateWidget("wmenu",xmRowColumnWidgetClass,menush,
		XmNrowColumnType,	XmMENU_PULLDOWN,
		NULL);

	hmenu = XtVaCreateWidget("hmenu",xmRowColumnWidgetClass,menush,
		XmNrowColumnType,	XmMENU_PULLDOWN,
		NULL);

	file = XtVaCreateManagedWidget("file",xmCascadeButtonGadgetClass,
									menubar,
		XmNsubMenuId,	fmenu,
		NULL);

	edit = XtVaCreateManagedWidget("edit",xmCascadeButtonGadgetClass,
									menubar,
		XmNsubMenuId,	emenu,
		NULL);

	view = XtVaCreateManagedWidget("view",xmCascadeButtonGadgetClass,
									menubar,
		XmNsubMenuId,	vmenu,
		NULL);

	options = XtVaCreateManagedWidget("options",xmCascadeButtonGadgetClass,
									menubar,
		XmNsubMenuId,	omenu,
		NULL);

	window = XtVaCreateManagedWidget("window",xmCascadeButtonGadgetClass,
									menubar,
		XmNsubMenuId,	wmenu,
		NULL);

	help = XtVaCreateManagedWidget("help",xmCascadeButtonGadgetClass,
									menubar,
		XmNsubMenuId,	hmenu,
		NULL);

	XtVaSetValues(menubar,
		XmNmenuHelpWidget,	help,
		NULL);

	addfile = XtVaCreateManagedWidget("addFile",
					xmPushButtonGadgetClass,fmenu,
		NULL);
	XtAddCallback(addfile,XmNactivateCallback,_NgGODefActionCB,NULL);

	load = XtVaCreateManagedWidget("loadScript",
					xmPushButtonGadgetClass,fmenu,
		NULL);
	XtAddCallback(load,XmNactivateCallback,_NgGODefActionCB,NULL);

	close = XtVaCreateManagedWidget("closeWindow",
					xmPushButtonGadgetClass,fmenu,
		NULL);
	XtAddCallback(close,XmNactivateCallback,_NgGODefActionCB,NULL);

	quit = XtVaCreateManagedWidget("quitApplication",
					xmPushButtonGadgetClass,fmenu,
		NULL);
	XtAddCallback(quit,XmNactivateCallback,_NgGODefActionCB,NULL);

	ncledit = XtVaCreateManagedWidget("nclWindow",
					xmPushButtonGadgetClass,wmenu,
		NULL);
	XtAddCallback(ncledit,XmNactivateCallback,_NgGODefActionCB,NULL);

	XtManageChild(fmenu);
	XtManageChild(emenu);
	XtManageChild(vmenu);
	XtManageChild(omenu);
	XtManageChild(wmenu);
	XtManageChild(hmenu);

	return menubar;
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
			goid = NhlDEFAULT_APP;
		}
	}

	return goid;
}
