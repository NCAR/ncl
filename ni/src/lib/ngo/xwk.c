/*
 *      $Id: xwk.c,v 1.2 1997-06-04 18:08:38 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xwk.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Feb 14 11:20:40 MST 1997
 *
 *	Description:	
 */
#include <ncarg/ngo/xwkP.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushBG.h>
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>

#define	Oset(field)	NhlOffset(NgXWkRec,xwk.field)
static NhlResource resources[] = {
	{NgNxwkWork,NgCxwkWork,NhlTPointer,sizeof(NhlPointer),
		Oset(xwork),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CONLY,NULL},
	{NgNxwkSize,NgCxwkSize,NhlTInteger,sizeof(int),
		Oset(size),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_DEFAULT,NULL},
};
#undef	Oset

static NhlErrorTypes XWkClassInitialize(
	void
);

static NhlErrorTypes XWkInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes XWkDestroy(
	NhlLayer	l
);

static NhlBoolean XWkCreateWin(
	NgGO	go
);

static NhlBoolean XWkCreateWinHook(
	NgGO	go
);

NgXWkClassRec NgxWkClassRec = {
	{
/* class_name		*/	"xWkClass",
/* nrm_class		*/	NrmNULLQUARK,
/* layer_size		*/	sizeof(NgXWkRec),
/* class_inited		*/	False,
/* superclass		*/	(NhlClass)&NggOClassRec,
/* cvt_table		*/	NULL,

/* layer_resources	*/	resources,
/* num_resources	*/	NhlNumber(resources),
/* all_resources	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize*/	NULL,
/* class_initialize	*/	XWkClassInitialize,
/* layer_initialize	*/	XWkInitialize,
/* layer_set_values	*/	NULL,
/* layer_set_values_hook*/	NULL,
/* layer_get_values	*/	NULL,
/* layer_reparent	*/	NULL,
/* layer_destroy	*/	XWkDestroy,

	},
	{
/* dialog		*/	NULL,
/* toplevel		*/	NULL,
/* manager		*/	NULL,

/* top_win_chain	*/	False,
/* create_win		*/	XWkCreateWin,
/* create_win_hook	*/	XWkCreateWinHook,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgxWkClass = (NhlClass)&NgxWkClassRec;

/*
 * Function:	NgXWorkPreOpenCB
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
NgXWorkPreOpenCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	char			func[]="NgXWorkPreOpenCB";
	NhlXWorkstationLayer	wk = (NhlXWorkstationLayer)cbdata.ptrval;
	NgXWk			xwk;
	NgXAppExport		x;
	int			xwkid,appmgr;
	XtInputMask		mask;
	Dimension		w,h;

	/*
	 * Eventually, set up a *controlling* window, that doesn't actually
	 * show the graphics if the user set the window_id.
	 */
	if(wk->xwork.window_id_set)
		return;

	/*
	 * TODO - retrieve "size" from workstation -
	 *	(workstation doesn't currently support size)
	 */
	appmgr = (int)wk->base.gui_data;
	NhlVACreate(&xwkid,"xwork",NgxWkClass,NhlGetParentId(appmgr),
		NgNxwkWork,	wk,
		NULL);
	xwk = (NgXWk)_NhlGetLayer(xwkid);
	if(!xwk){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		"%s:Unable to create GUI control window for workstation \"%s\"",							func,wk->base.name));
		return;
	}
	x = xwk->go.x;

	/*
	 * Pop up xwk, and get the workstation window.
	 * - lock out rest of app so we can enter mini-event loop until
	 *   workstation window is actually mapped.
	 */
	NgAppGrabFocus(appmgr,xwkid);
	NgGOPopup(xwkid);
	/*
	 * This will process all X Events until xwk is mapped, and
	 * there are no outstanding X Events. (All those events should
	 * be associated with the xwk window since Focus has been locked
	 * there.)
	 *
	 * ** Don't call XtAppProcessEvent unless there is an actual	**
	 * ** X Event to be processed or we could end up calling work	**
	 * ** procs, timers, or alternate inputs which could be big	**
	 * ** trouble!							**
	 */
	for(mask=XtAppPending(x->app);
			!xwk->xwk.mapped || (XtIMXEvent&mask);
						mask=XtAppPending(x->app)){
		if(mask & XtIMXEvent)
			XtAppProcessEvent(x->app,XtIMXEvent);
	}

	NgAppReleaseFocus(appmgr,xwkid);

	XtVaGetValues(xwk->go.shell,
		XmNwidth,	&w,
		XmNheight,	&h,
		NULL);

	return;
}

/*
 * Function:	XWkClassInitialize
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
XWkClassInitialize
(
	void
)
{
	NhlArgVal	sel,udata;

	NhlINITVAR(sel);
	NhlINITVAR(udata);
	_NhlAddClassCallback(NhlxWorkstationClass,_NhlCBworkPreOpen,sel,
							NgXWorkPreOpenCB,udata);
	return NhlNOERROR;
}

/*
 * This function is called after the xwork is destroyed.
 */
static void
WorkDestroyCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgXWk		xwk = (NgXWk)udata.ptrval;

	xwk->xwk.xwork = NULL;
	NhlDestroy(xwk->base.id);

	return;
}

/*
 * Function:	XWkInitialize
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
XWkInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char			func[] = "XWkInitialize";
	NgXWk			xwk = (NgXWk)new;
	NgXWkPart		*np = &((NgXWk)new)->xwk;
	NgXWkPart		*rp = &((NgXWk)req)->xwk;
	NhlArgVal		sel,udata;

	if(!_NhlIsClass((NhlLayer)np->xwork,NhlxWorkstationClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid XWorkstation",
									func));
		return NhlFATAL;
	}
	/*
	 * initialize state vars...
	 */
	np->mapped = False;

	/*
	 * Make sure this object is destroyed if the workstation is
	 * destroyed.
	 */
	NhlINITVAR(sel);
	NhlINITVAR(udata);
	udata.ptrval = new;
	np->xwork_destroycb = NgCBWPAdd(xwk->go.appmgr,NULL,NULL,
		(NhlLayer)np->xwork,_NhlCBobjDestroy,sel,WorkDestroyCB,udata);

	/*
	 * Create a "color" context for this window, with the "xappmgr"
	 * one as the parent.
	 * call gescape with the NGESC_CNATIVE(NGC_XALLOCCOLOR) to setup
	 * gks to use the "color" context.
	 */

	/*
	 * Set size of graphics window - should eventually retrieve
	 * this from (xwork).
	 */
	if(np->size){
		np->grw = np->grh = (Dimension)np->size;
	}
	else{
		np->size = np->grw = np->grh = 500;
	}

	return NhlNOERROR;
}

static NhlErrorTypes
XWkDestroy
(
	NhlLayer	l
)
{
	NgXWk		xwk = (NgXWk)l;
	NgXWkPart	*xp = &((NgXWk)l)->xwk;

	NgCBWPDestroy(xp->xwork_destroycb);

	if(xp->xwork){
/*
 * TODO: When "ref_strings" are available from ncl - submit a block to
 *	destroy "xwork" instead of calling this.
 */
		NhlDestroy(xp->xwork->base.id);
	}

	return NhlNOERROR;
}

static void
MapGraphicsEH
(
	Widget		widget,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	NgXWk	xwk = (NgXWk)udata;

	if(event->type != MapNotify)
		return;

	XtRemoveEventHandler(widget,StructureNotifyMask,False,
							MapGraphicsEH,udata);

	xwk->xwk.mapped = True;

	return;
}

static void
XWkSetMainSize
(
	NgXWk	xwk
)
{
	NgXWkPart	*xp = &xwk->xwk;
	Widget		grVP;
	Dimension	wSW,hSW,wVP,hVP,w,h;

	XtVaSetValues(xwk->go.shell,
		XmNmaxWidth,	WidthOfScreen(XtScreen(xwk->go.shell)),
		XmNmaxHeight,	HeightOfScreen(XtScreen(xwk->go.shell)),
		NULL);

	XtVaSetValues(xp->graphics,
		XmNwidth,	xp->grw,
		XmNheight,	xp->grh,
		NULL);

	XtVaGetValues(xp->graphicsSW,
		XmNwidth,	&wSW,
		XmNheight,	&hSW,
		XmNclipWindow,	&grVP,
		NULL);

	XtVaGetValues(grVP,
		XmNwidth,	&wVP,
		XmNheight,	&hVP,
		NULL);

	XtVaSetValues(xp->graphicsSW,
		XmNwidth,	(wSW + xp->grw - wVP),
		XmNheight,	(hSW + xp->grh - hVP),
		NULL);

	XtVaGetValues(xwk->go.shell,
		XmNwidth,	&w,
		XmNheight,	&h,
		NULL);

	XtVaSetValues(xwk->go.shell,
		XmNmaxWidth,	MIN(w,WidthOfScreen(XtScreen(xwk->go.shell))),
		XmNmaxHeight,	MIN(h,HeightOfScreen(XtScreen(xwk->go.shell))),
		NULL);

	return;
}

static NhlBoolean
XWkCreateWin
(
	NgGO	go
)
{
	char			func[]="XWkCreateWin";
	NgXWk			xwk = (NgXWk)go;
	NgXWkPart		*xp = &xwk->xwk;
	NgXAppExport		x = go->go.x;
	Widget			manager;
	Widget			menubar;

	manager = XtVaCreateManagedWidget("mgr",xmFormWidgetClass,
								go->go.manager,
		NULL);

	menubar = _NgGOCreateMenubar(go,manager);

#if	NOT
	Widget			menush,fmenu,emenu;
	Widget			vmenu,omenu,wmenu,hmenu;
	Widget			file,edit,view,options,window,help;
	Widget			addfile,load,close,quit;
	Widget			ncledit,browse;

	menubar =XtVaCreateManagedWidget("menubar",xmRowColumnWidgetClass,
									manager,
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
        
	browse = XtVaCreateManagedWidget("browseWindow",
					xmPushButtonGadgetClass,wmenu,
		NULL);
	XtAddCallback(browse,XmNactivateCallback,_NgGODefActionCB,NULL);

	XtManageChild(fmenu);
	XtManageChild(emenu);
	XtManageChild(vmenu);
	XtManageChild(omenu);
	XtManageChild(wmenu);
	XtManageChild(hmenu);
#endif

	xp->graphicsSW = XtVaCreateManagedWidget("xworkSW",
					xmScrolledWindowWidgetClass,manager,
		XmNscrollingPolicy,		XmAUTOMATIC,
		XmNscrollBarDisplayPolicy,	XmSTATIC,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			menubar,
		NULL);

	xp->graphics = XtVaCreateManagedWidget("graphics",
					xmDrawingAreaWidgetClass,xp->graphicsSW,
		NULL);

	XtAddEventHandler(xp->graphics,StructureNotifyMask,False,MapGraphicsEH,
								(XtPointer)xwk);

	return True;
}


static NhlBoolean
XWkCreateWinHook
(
	NgGO	go
)
{
	char			func[]="XWkCreateWinHook";
	NgXWk			xwk = (NgXWk)go;
	NgXWkPart		*xp = &xwk->xwk;
	NgXAppExport		x = go->go.x;
	XSetWindowAttributes	xswa;
	Dimension		dim;

	/*
	 * Geometry hacks follow...
	 */
	XtRealizeWidget(go->go.shell);

	xswa.backing_store = WhenMapped;
	XChangeWindowAttributes(x->dpy,XtWindow(xp->graphics),
							CWBackingStore,&xswa);

	XWkSetMainSize(xwk);

	/*
	 * Set the workstation X window now that we have it.
	 */
	xp->xwork->xwork.window_id =
			xp->xwork->work.gkswksconid = XtWindow(xp->graphics);
	xp->xwork->work.gkswkstype = 7;
	xp->xwork->xwork.pause = False;

	return True;
}
