/*
 *      $Id: xwk.c,v 1.5 1997-09-04 17:05:46 boote Exp $
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

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeBG.h>
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

	return NhlNOERROR;
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
	int		nclstate=NhlDEFAULT_APP;

	NhlVAGetValues(xwk->go.appmgr,
		NgNappNclState,	&nclstate,
		NULL);
	if(!NhlIsClass(nclstate,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid nclstate object %d",
								func,nclstate));
		return NhlFATAL;
	}

	NgCBWPDestroy(xp->xwork_destroycb);

	if(xp->xwork){
		char	*ref;
		char	cmdbuff[1024];

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

	_NgGOSetTitle(go,xwk->xwk.xwork->xwork.xwinconfig.title,
				xwk->xwk.xwork->xwork.xwinconfig.icon_title);

	_NgGOCreateMenubar(go);

	xp->graphicsSW = XtVaCreateManagedWidget("xworkSW",
				xmScrolledWindowWidgetClass,go->go.manager,
		XmNscrollingPolicy,		XmAUTOMATIC,
		XmNscrollBarDisplayPolicy,	XmSTATIC,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			go->go.menubar,
		NULL);

	XtVaGetValues(go->go.shell,
		XcbNcolorMode,		&cmode,
		XcbNcolorBroker,	&xp->pxcb,
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
