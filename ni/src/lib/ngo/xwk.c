/*
 *      $Id: xwk.c,v 1.9 1998-10-19 20:25:55 boote Exp $
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
#include <ncarg/gksP.h>
#include <ncarg/ngo/xwkP.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/colormap.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <ncarg/ngo/CascadeBG.h>
#include <Xm/PushBG.h>
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>

#include <Xcb/xcbP.h>

#define	Oset(field)	NhlOffset(NgXWkRec,xwk.field)
static NhlResource resources[] = {
	{NgNxwkWork,NgCxwkWork,NhlTPointer,sizeof(NhlPointer),
		Oset(xwork),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CONLY,NULL},
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
/* close		*/	_NgGOInheritClose,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgxWkClass = (NhlClass)&NgxWkClassRec;

void NgXWorkPopup
(
	int appmgr,
	int xwkid
)
{
	NgXWk			xwk;
	XtInputMask		mask;
	NgXAppExport		x;

	xwk = (NgXWk)_NhlGetLayer(xwkid);
	if(!xwk){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s: Error getting X window layer"));
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
#if	NOT
	for(mask=XtAppPending(x->app);
			!xwk->xwk.mapped || (XtIMXEvent&mask);
						mask=XtAppPending(x->app)){
		if(mask & XtIMXEvent)
			XtAppProcessEvent(x->app,XtIMXEvent);
	}
	/* 
	 * on my linux box at least it does not seem to be sufficient to 
	 * wait for all the event processing -- the draws can still occur
	 * before the window is ready to receive them. The following seems
	 * to take care of the problem. Not sure if the syncs are really
	 * necessary. The sleep is necessary.
	 *
	 * - hopefully the loop below fixes the problem without the sleep...
	 *
	 * It looks like the problem is that mapped was never being set to
	 * false - so calling Popup on the XWks was not a "flushed" event, so
	 * the event loop above probably would have still worked, but the one
	 * below is better.. (not as effecient, but completely "flushed".)
	 * 	- jeff
	 */
	XSync(x->dpy,False);
	sleep(1);
	XSync(x->dpy,False);
#endif
	mask = XtAppPending(x->app);
	while(!xwk->xwk.mapped || (XtIMXEvent&mask)){
		if(mask&XtIMXEvent)
			XtAppProcessEvent(x->app,XtIMXEvent);
		mask = XtAppPending(x->app);
		/*
		 * If there are no events - then call XSync to flush all
		 * the events.  Then go back and process them all.  This should
		 * make sure that we have processed all possible events before
		 * leaving this loop.
		 */
		if(!(mask&XtIMXEvent)){
			XSync(x->dpy,False);
			mask = XtAppPending(x->app);
		}
	}

	NgAppReleaseFocus(appmgr,xwkid);
}

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
	int			xwkid,appmgr,selected_work;

	/*
	 * Eventually, set up a *controlling* window, that doesn't actually
	 * show the graphics if the user set the window_id.
	 */
	if(wk->xwork.window_id_set)
		return;

	appmgr = (int)wk->base.gui_data;
	NhlVACreate(&xwkid,"xwork",NgxWkClass,NhlGetParentId(appmgr),
		NgNxwkWork,	wk,
		NULL);
	xwk = (NgXWk)_NhlGetLayer(xwkid);
	if(!xwk){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
	   "%s:Unable to create GUI control window for workstation \"%s\"",
							func,wk->base.name));
		return;
	}
	wk->base.gui_data2 = (NhlPointer) xwkid;

	selected_work = NgAppGetSelectedWork(appmgr,False,NULL);

	/* 
	 * if selected_work is -1 it means that a workstation has never
	 * been created. The first one gets created unmapped.
	 */

	if (selected_work < 0) {
		NgGOCreateUnmapped(xwkid);
		return;
	}

	/* otherwise pop up the workstation immediately */

	NgXWorkPopup(appmgr,xwkid);

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
 * Function:	colorMapEditor
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
colorMapEditor
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	char		func[] = "colorMapEditor";
	NgXWk		xwk;
	int		xwkid = NhlDEFAULT_APP;

	xwkid = NgGOWidgetToGoId(w);
	if(xwkid == NhlDEFAULT_APP){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid Widget",func));
		return;
	}

	if(!NhlIsClass(xwkid,NgxWkClass)){
		return;
	}

	xwk = (NgXWk)_NhlGetLayer(xwkid);

	if(!xwk->xwk.cmap_editor){
		NhlVACreate(&xwk->xwk.cmap_editor,"colorMapEditor",
							NgcolorMapClass,xwkid,
			NgNcmWork,	xwk->xwk.xwork->base.id,
			NULL);
	}

	NgGOPopup(xwk->xwk.cmap_editor);

	return;
}

static XtActionsRec actions[] = {
	{"colorMapEditor",	colorMapEditor,},
};

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
	static int		init = True;
	char			func[] = "XWkInitialize";
	NgXWk			xwk = (NgXWk)new;
	NgXWkPart		*np = &((NgXWk)new)->xwk;
	NgXWkPart		*rp = &((NgXWk)req)->xwk;
	NhlArgVal		sel,udata;
	int			nclstate=NhlDEFAULT_APP;

	if(!_NhlIsClass((NhlLayer)np->xwork,NhlxWorkstationClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid XWorkstation",
									func));
		return NhlFATAL;
	}

	if(init){
		init = False;
		XtAppAddActions(xwk->go.x->app,actions,NhlNumber(actions));
	}

	/*
	 * initialize state vars...
	 */
	np->mapped = False;
	np->graphics = NULL;

	/*
	 * Make sure this object is destroyed if the workstation is
	 * destroyed.
	 */
	NhlINITVAR(sel);
	NhlINITVAR(udata);
	udata.ptrval = new;
	np->xwork_destroycb = NgCBWPAdd(xwk->go.appmgr,NULL,NULL,
		(NhlLayer)np->xwork,_NhlCBobjDestroy,sel,WorkDestroyCB,udata);

	NhlVAGetValues(xwk->go.appmgr,
		NgNappNclState,	&nclstate,
		NULL);
	np->appdestroycb = _NhlAddObjCallback(_NhlGetLayer(xwk->go.appmgr),
				_NhlCBobjDestroy,sel,NgDestroyMeCB,udata);
	np->nsdestroycb = _NhlAddObjCallback(_NhlGetLayer(nclstate),
				_NhlCBobjDestroy,sel,NgDestroyMeCB,udata);

	np->my_broker = False;
	np->xcb = NULL;

	/*
	 * Set size of graphics window - should eventually retrieve
	 * this from (xwork).
	 */
	if(np->xwork->xwork.xwinconfig.width > 0)
		np->grw = np->xwork->xwork.xwinconfig.width;
	else
		np->grw = 500;
	if(np->xwork->xwork.xwinconfig.height > 0)
		np->grh = np->xwork->xwork.xwinconfig.height;
	else
		np->grh = 500;

	np->size = NULL;

	if(!xwk->go.xm_title){
		xwk->go.xm_title = NgXAppCreateXmString(xwk->go.appmgr,
					xwk->xwk.xwork->xwork.xwinconfig.title);
	}

	xwk->xwk.cmap_editor = NhlNULLOBJID;

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

	if((event->type != MapNotify) && (event->type != UnmapNotify))
		return;

	xwk->xwk.mapped = (event->type == MapNotify);

	return;
}

static NhlErrorTypes
XWkDestroy
(
	NhlLayer	l
)
{
	char		func[]="XWkDestroy";
	NgXWk		xwk = (NgXWk)l;
	NgXWkPart	*xp = &((NgXWk)l)->xwk;

	NgCBWPDestroy(xp->xwork_destroycb);
	_NhlCBDelete(xp->appdestroycb);
	_NhlCBDelete(xp->nsdestroycb);

	if(xp->graphics)
		XtRemoveEventHandler(xp->graphics,StructureNotifyMask,False,
							MapGraphicsEH,xwk);
#if 0
	NgAppRemoveGO(xwk->go.appmgr,xwk->base.id);
#endif
	if(xp->xwork){
		int	nclstate=NhlDEFAULT_APP;
		char	*ref;
		char	cmdbuff[1024];

		NhlVAGetValues(xwk->go.appmgr,
			NgNappNclState,	&nclstate,
			NULL);
		if(!NhlIsClass(nclstate,NgnclStateClass)){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s:Invalid nclstate object %d",func,nclstate));
			return NhlFATAL;
		}

 		ref = NgNclGetHLURef(nclstate,xp->xwork->base.id);
		if(ref){
			sprintf(cmdbuff,"destroy(%s)\n",ref);
			(void)NgNclSubmitBlock(nclstate,cmdbuff);
		}
		else
			NhlDestroy(xp->xwork->base.id);
	}

	_NhlCBDelete(xp->broker_destroyCB);
	if(xp->my_broker)
		XcbDestroy(xp->xcb);

	return NhlNOERROR;
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

static void
BrokerDestroy
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgXWk		xwk = (NgXWk)udata.ptrval;
	NgXWkPart	*xp = &xwk->xwk;

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		"%s:Xcb destroyed before X Workstation!",xwk->base.name));

	xp->my_broker = False;
	NhlDestroy(xwk->base.id);

	return;
}

static void
CFault
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	Colormap	cmap = cbdata.ulngval;
	NgXWk		xwk = (NgXWk)udata.ptrval;
	NgXWkPart	*xp = &xwk->xwk;

	XtVaSetValues(xp->graphics,
		XmNcolormap,	cmap,
		NULL);

	if(cmap != XcbGetColormap(xp->pxcb))
		XtSetWMColormapWindows(xwk->go.shell,&xp->graphics,1);

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
	XcbAttrRec		xcbattr;
	unsigned long		mask = 0;
	XcbMode			cmode;
	Widget			w;

	_NgGOSetTitle(go,xwk->xwk.xwork->xwork.xwinconfig.title,
				xwk->xwk.xwork->xwork.xwinconfig.icon_title);

	_NgGOCreateMenubar(go);

	XtVaSetValues(xwk->go.edit,
		XmNsensitive,	True,
		NULL);
	
	w = XtVaCreateManagedWidget("colorMapEditor",xmPushButtonGadgetClass,
		xwk->go.emenu,NULL);
	XtAddCallback(w,XmNactivateCallback,_NgGODefActionCB,NULL);

	xp->graphicsSW = XtVaCreateManagedWidget("xworkSW",
				xmScrolledWindowWidgetClass,go->go.manager,
		XmNscrollingPolicy,		XmAUTOMATIC,
		XmNscrollBarDisplayPolicy,	XmSTATIC,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			go->go.menubar,
		NULL);

	xp->pxcb = xwk->go.xcb;
	XtVaGetValues(go->go.shell,
		XcbNcolorMode,		&cmode,
		NULL);

	/*
	 * This breaks the data abstraction, but I don't have time to
	 * add all the layers in between right now.  And I probably never
	 * will...
	 * (Changing min_ncols in the shell's Xcb should make it so that
	 * the graphics window only has a different colormap then the
	 * rest of the window in the MOST extreme cases, since gks can
	 * only allocate 256 colors...)
	 */
	if(xp->pxcb)
		xp->pxcb->min_ncols = 256;

	mask |= XcbMODE;
	xcbattr.mode = (cmode == XcbSHAREDCMAP)?XcbSHAREDCMAP:XcbMIXEDCMAP;

	mask |= XcbPARENT;
	xcbattr.parent = xp->pxcb;

	xp->xcb = XcbCreate(x->dpy,&xcbattr,mask);
	if(!xp->xcb)
		xp->xcb = xp->pxcb;
	else
		xp->my_broker = True;

	xp->graphics = XtVaCreateManagedWidget("graphics",
					xmDrawingAreaWidgetClass,xp->graphicsSW,
		XmNbottomShadowColor,	0,
		XmNhighlightColor,	0,
		XmNtopShadowColor,	0,
		XmNbackground,		0,
		XmNborderColor,		0,
		XmNdepth,		XcbGetDepth(xp->xcb),
		XmNcolormap,		XcbGetColormap(xp->xcb),
		NULL);

	XtAddEventHandler(xp->graphics,StructureNotifyMask,False,MapGraphicsEH,
								(XtPointer)xwk);

	if(xp->xcb){
		NhlArgVal	sel,udata;

		NhlINITVAR(sel);
		NhlINITVAR(udata);
		udata.ptrval = xwk;
		xp->broker_destroyCB = _NhlCBAdd(xp->xcb->destroyCBL,sel,
							BrokerDestroy,udata);
		xp->broker_cfaultCB = _NhlCBAdd(xp->xcb->cfaultCBL,sel,CFault,
									udata);
	}

	return True;
}

static void
XwkAllocColor
(
	void	*cref,
	void	*color_def
)
{
	Xcb	xcb = (Xcb)cref;
	XColor	*xcol = (XColor*)color_def;

	XcbAllocROColor(xcb,xcol);

	return;
}

static void
XwkFreeColors
(
	void		*cref,
	unsigned long	*pixels,
	int		npixels
)
{
	Xcb	xcb = (Xcb)cref;

	XcbFreeColors(xcb,pixels,npixels);

	return;
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
	_NGCXAllocColor		xalloccolor;
	Gescape_in_data		gesc_in;

	/*
	 * Geometry hacks follow...
	 */
	XtRealizeWidget(go->go.shell);

	XWkSetMainSize(xwk);

	/*
	 * Only until pixmap backup's are working in gks...
	 */
	xswa.backing_store = WhenMapped;
	XChangeWindowAttributes(x->dpy,XtWindow(xp->graphics),
							CWBackingStore,&xswa);

	if(XcbGetColormap(xp->xcb) != XcbGetColormap(xp->pxcb))
		XtSetWMColormapWindows(xwk->go.shell,&xp->graphics,1);

	/*
	 * Set the workstation X window now that we have it.
	 */
	xp->xwork->xwork.window_id =
			xp->xwork->work.gkswksconid = XtWindow(xp->graphics);
	xp->xwork->work.gkswkstype = 7;
	xp->xwork->xwork.pause = False;

	/*
	 * Send the escape element to get gks to allocate colors from
	 * the broker.
	 */
	gesc_in.escape_r1.data = &xalloccolor;
	gesc_in.escape_r1.size = 0;
	xalloccolor.type = NGC_XALLOCCOLOR;
	xalloccolor.work_id = -1;
	xalloccolor.xalloc_color = XwkAllocColor;
	xalloccolor.xfree_colors = XwkFreeColors;
	xalloccolor.cref = xp->xcb;
	gescape(NGESC_CNATIVE,&gesc_in,NULL,NULL);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,"XWkCreateWinHook");

	return True;
}
