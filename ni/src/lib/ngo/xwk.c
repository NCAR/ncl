/*
 *      $Id: xwk.c,v 1.26 1999-11-04 17:36:54 dbrown Exp $
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
#include <ncarg/ngo/xinteractP.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>
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
	{NgNxwkSelectedView,NgCxwkSelectedView,NhlTPointer,
	 sizeof(NhlPointer),Oset(selected_view_id),NhlTImmediate,
	 _NhlUSET((NhlPointer)NhlNULLOBJID),_NhlRES_GONLY,NULL},
	{NgNxwkDrawSelectedViewOnly,NgCxwkDrawSelectedViewOnly,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(draw_single_view),NhlTImmediate,
	 _NhlUSET((NhlPointer)False),_NhlRES_NOSACCESS,NULL},
	{NgNxwkAutoUpdate,NgCxwkAutoUpdate,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(auto_refresh),NhlTImmediate,
	 _NhlUSET((NhlPointer)True),_NhlRES_NOSACCESS,NULL},
	{NgNxwkRaiseOnDraw,NgCxwkRaiseOnDraw,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(raise_on_draw),NhlTImmediate,
	 _NhlUSET((NhlPointer)False),_NhlRES_GONLY,NULL}
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
static NhlBoolean
GetBrowser
(
	int		goid,
	NhlPointer	udata
)
{
	int	*browse = (int*)udata;

	if(NhlIsClass(goid,NgbrowseClass)){
		*browse = goid;
		return False;
	}

	return True;
}
static void
ColorWPCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgXWk		xwk = (NgXWk)udata.ptrval;
	int		browse = NhlDEFAULT_APP;
	NgXWkPart	*xp;
 	NgHluData 	hdata;
	NgWksObj	wkobj;

	xp = &xwk->xwk;
	xp->color_cb_pending = False;
	hdata = (NgHluData) xp->xwork->base.gui_data2;
	wkobj = hdata ? (NgWksObj) hdata->gdata : NULL;

	if (wkobj) {
		wkobj->colormap_cb_pending = False;
	}

	if (xp->ignore_color_cb)
		return;

	if (!xp->mapped) {
		return;
	}
	
	NgAppEnumerateGO(xwk->go.appmgr,GetBrowser,&browse);

	if (browse != NhlDEFAULT_APP) {
		xp->ignore_color_cb = True;
		NgUpdatePages(browse,True,xp->xwork->base.id);
		xp->ignore_color_cb = False;
	}

}

static void
ColorCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgXWk		xwk = (NgXWk)udata.ptrval;
	NhlXPixel	bg,fg;
	XGCValues       gcv;
	NgXWkPart	*xp;
	NgXAppExport	x;

#if DEBUG_XWK
	fprintf(stderr,"in color callback\n");
#endif

	xp = &xwk->xwk;
	x = xwk->go.x;
	NhlGetXPixel(xp->xwork->base.id,0,&bg);
	NhlGetXPixel(xp->xwork->base.id,1,&fg);
	gcv.foreground = bg ^ fg;
	gcv.background = bg;

	if (! xp->xor_gc) {
		gcv.function = GXxor;
		gcv.line_width = 0;
		xp->xor_gc = XCreateGC
			(x->dpy,XtWindow(xp->graphics),
			 (GCLineWidth|GCBackground|GCForeground|GCFunction),
			 &gcv);
		XtAddEventHandler(xp->graphics,ButtonPressMask,False,
				  (XtEventHandler)_NgSelectionEH,
				  (XtPointer)xwk);
	}
	else {
		xp->select_rect_vis = False;
		XChangeGC(xwk->go.x->dpy,xp->xor_gc,
			  (GCBackground|GCForeground),&gcv);
	}
	
	return;

}

void NgXWorkPopup
(
	int appmgr,
	int xwkid
)
{
	NgXWk			xwk;
	NgXWkPart		*xp;
	XtInputMask		mask;
	NgXAppExport		x;

	xwk = (NgXWk)_NhlGetLayer(xwkid);

	if(!xwk){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s: Error getting X window layer"));
		return;
	}
	xp = &xwk->xwk;
	x = xwk->go.x;
	/*
	 * Pop up xwk, and get the workstation window.
	 * - lock out rest of app so we can enter mini-event loop until
	 *   workstation window is actually mapped.
	 */
	NgAppGrabFocus(appmgr,xwkid);
	NgGOPopup(xwkid);
#if DEBUG_XWK
	fprintf(stderr,"mapped = %d\n",xp->mapped);
#endif

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

	mask = XtAppPending(x->app);
	while(!xp->mapped || (XtIMXEvent&mask)){
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

static NhlBoolean TestColorWPCB(
        NhlArgVal       cbdata,
        NhlArgVal       udata,
        NhlArgVal       *ret
)
{

	NgXWk		xwk = (NgXWk)udata.ptrval;
	NgXWkPart	*xp;
 	NgHluData 	hdata;
	NgWksObj	wkobj;
	NhlGenArray	colormap;


	xp = &xwk->xwk;
	NhlVAGetValues(xp->xwork->base.id,
		       NhlNwkColorMap,&colormap,
		       NULL);

	if (xp->ignore_color_cb || xp->color_cb_pending) {
		if (xp->last_colormap)
			NhlFreeGenArray(xp->last_colormap);
		xp->last_colormap = colormap;
		return False;
	}

	if (xp->last_colormap) {
		if (colormap->num_elements == xp->last_colormap->num_elements 
		    && ! memcmp(colormap->data,xp->last_colormap->data,
				colormap->num_elements * sizeof(float))) {
			NhlFreeGenArray(colormap);
			return False;
		}
		else {
			NhlFreeGenArray(xp->last_colormap);
		}
	}
	xp->last_colormap = colormap;

	xp->color_cb_pending = True;
	hdata = (NgHluData) xp->xwork->base.gui_data2;
	wkobj = hdata ? (NgWksObj) hdata->gdata : NULL;

	if (wkobj) {
		wkobj->colormap_cb_pending = True;
	}
	return True;
}

static void
SetUpWorkColorCBs
(
	int xwkid
)
{
	NgXWk			xwk;
	NgXWkPart		*xp;

	xwk = (NgXWk)_NhlGetLayer(xwkid);

	if(!xwk){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s: Error getting X window layer"));
		return;
	}
	xp = &xwk->xwk;

	if (! xp->xor_gc) {
		NhlArgVal		sel,udata;

		NhlINITVAR(sel);
		NhlINITVAR(udata);
		udata.ptrval = xwk;
		sel.lngval = 0;
		_NhlAddObjCallback
			((NhlLayer)xp->xwork,
			 _NhlCBworkColorIndexChange,sel,ColorCB,udata);
		NgCBWPAdd(xwk->go.appmgr,TestColorWPCB,NULL,
			  (NhlLayer)xp->xwork,
			  _NhlCBworkColorMapChange,sel,ColorWPCB,udata);
		sel.lngval = 1;
		_NhlAddObjCallback
			((NhlLayer)xp->xwork,
			 _NhlCBworkColorIndexChange,sel,ColorCB,udata);
	}
	return;
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
	NgWksObj 		wko;
	NgHluData		hdata;

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
	/* 
	 * For most objects the ViewTree (in mwin.c for now) allocates the 
	 * Obj info rec and assigns it to the gui_data2 pointer. But the
	 * draw routine may need to know the NgXwk object id before the
	 * workstation gets put into the ViewTree. So the NgXwk creates it.
	 * It will still be freed by the ViewTree. If the xwk is a preview,
	 * then it is set during the NgCreatePreviewGraphic routine (graphic.c)
	 */
	if (wk->base.gui_data2)
		hdata = (NgHluData) wk->base.gui_data2;
	else
		hdata = NgGetHluData();
	if (hdata->gdata)
		wko = (NgWksObj) hdata->gdata;
	else 
		wko = NhlMalloc(sizeof(NgWksObjRec));

	if(! (wko && hdata)){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}	
	/* only fill in the wrapper id at this point */
	wko->wks_wrap_id = xwkid;
	wko->auto_refresh = True;
	wko->colormap_cb_pending = False;
	hdata->gdata = (NhlPointer) wko;
	
	wk->base.gui_data2 = (NhlPointer) hdata;

	selected_work = NgAppGetSelectedWork(appmgr,False,NULL);

	/* 
	 * if selected_work is -1 it means that a workstation has never
	 * been created. The first one gets created unmapped.
	 */

	if (selected_work < 0 || hdata->preview) {
		NgGOCreateUnmapped(xwkid);
		SetUpWorkColorCBs(xwkid);
		return;
	}

	/* otherwise pop up the workstation immediately */

	NgXWorkPopup(appmgr,xwkid);
	SetUpWorkColorCBs(xwkid);

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
			    NgNcmWork,xwk->xwk.xwork->base.id,
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
	np->xor_gc = NULL;
	np->graphics = NULL;
	np->ignore_color_cb = False;
	np->color_cb_pending = False;
	np->last_colormap = NULL;
	memset(np->xor_box,0,sizeof(XPoint)*5);

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
		xwk->go.xm_title = NgXAppCreateXmString
			(xwk->go.appmgr,
			 xwk->xwk.xwork->xwork.xwinconfig.title);
	}

	xwk->xwk.cmap_editor = NhlNULLOBJID;

	xwk->xwk.lastp.x = xwk->xwk.lastp.y = (Position) -1;
	xwk->xwk.selected_view_id = NhlNULLOBJID;
	xwk->xwk.views = NULL;
	xwk->xwk.view_count = 0;
	xwk->xwk.view_alloc_count = 0;
	xwk->xwk.select_rect_vis = False;
	xwk->xwk.manipulate_eh_active = False;
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

	if ((event->type != MapNotify) && (event->type != UnmapNotify))
		return;

	if (event->type == MapNotify && event->xmap.window != XtWindow(widget))
		return;
	else if (event->xunmap.window != XtWindow(widget))
		return;

	if (event->type == MapNotify)
		xwk->xwk.mapped = True; /* once true always true */
#if DEBUG_XWK
	fprintf(stderr,"EH mapped = %d\n",xwk->xwk.mapped);
#endif

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

	if(xwk->go.shell)
		XtRemoveEventHandler(xwk->go.shell,StructureNotifyMask,False,
				     (XtEventHandler)_NgSelectionEH,xwk);

	if(xp->xwork){
		int	nclstate=NhlDEFAULT_APP;
		char	*ref;
		char	cmdbuff[1024];
		NgHluData hdata = (NgHluData) xp->xwork->base.gui_data2;
		NgWksObj	wkobj;

		wkobj = hdata ? (NgWksObj) hdata->gdata : NULL;

		if (wkobj) {
			/* turn off auto refresh */
			wkobj->auto_refresh = False;
		}

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

	if (xp->xor_gc) {
		XFreeGC(xwk->go.x->dpy,xp->xor_gc);
		if (xp->graphics) 
			XtRemoveEventHandler(xp->graphics,StructureNotifyMask,
					     False,MapGraphicsEH,xwk);
	}
	if (xp->views)
		NhlFree(xp->views);

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
	Dimension	wSW,hSW,w,h;

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
#if 0
	XtVaGetValues(grVP,
		XmNwidth,	&wVP,
		XmNheight,	&hVP,
		NULL);
	XtVaSetValues(xp->graphicsSW,
		XmNwidth,	(wSW + xp->grw - wVP),
		XmNheight,	(hSW + xp->grh - hVP),
		NULL);
#endif
	XtVaSetValues(xp->graphicsSW,
		XmNwidth,	(xp->grw +4),
		XmNheight,	(xp->grh +4),
		NULL);

	XtVaGetValues(xwk->go.shell,
		XmNwidth,	&w,
		XmNheight,	&h,
		NULL);

#if 0	
	XtVaSetValues(xwk->go.shell,
		XmNmaxWidth,	MIN(w,WidthOfScreen(XtScreen(xwk->go.shell))),
		XmNmaxHeight,	MIN(h,HeightOfScreen(XtScreen(xwk->go.shell))),
		NULL);
#endif
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

void
DrawSingleViewOptionCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmToggleButtonCallbackStruct	*xmcb = 
		(XmToggleButtonCallbackStruct*)cbdata;
	NgXWk			xwk = (NgXWk)udata;

#if DEBUG_XWK
	fprintf(stderr,"draw single view %s\n", xmcb->set ? "on" : "off");
#endif
	xwk->xwk.draw_single_view = xmcb->set;

	return;
}


void
RaiseOnDrawOptionCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmToggleButtonCallbackStruct	*xmcb = 
		(XmToggleButtonCallbackStruct*)cbdata;
	NgXWk			xwk = (NgXWk)udata;

#if DEBUG_XWK
	fprintf(stderr,"raise on draw %s\n", xmcb->set ? "on" : "off");
#endif
	xwk->xwk.raise_on_draw = xmcb->set;

	return;
}

void
AutoRefreshOptionCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmToggleButtonCallbackStruct	*xmcb = 
		(XmToggleButtonCallbackStruct*)cbdata;
	NgXWk			xwk = (NgXWk)udata;
	NgHluData		hdata = xwk->xwk.xwork->base.gui_data2;
	NgWksObj		wksobj;

#if DEBUG_XWK
	fprintf(stderr,"auto refresh %s\n", xmcb->set ? "on" : "off");
#endif
	xwk->xwk.auto_refresh = xmcb->set;

	wksobj = hdata ? (NgWksObj) hdata->gdata : NULL;
	if (wksobj)
		wksobj->auto_refresh = xwk->xwk.auto_refresh;

	return;
}

void
ExchangeForeBackCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgXWk			xwk = (NgXWk)udata;
	NhlGenArray		back_gen,fore_gen;

#if DEBUG_XWK
	fprintf(stderr,"exchanging foreground and background colors\n");
#endif
	NhlVAGetValues(xwk->xwk.xwork->base.id,
		       NhlNwkBackgroundColor,&back_gen,
		       NhlNwkForegroundColor,&fore_gen,
		       NULL);
	NhlVASetValues(xwk->xwk.xwork->base.id,
		       NhlNwkBackgroundColor,fore_gen,
		       NhlNwkForegroundColor,back_gen,
		       NULL);
		       
	NhlFreeGenArray(fore_gen);
	NhlFreeGenArray(back_gen);

	return;
}

static NhlBoolean
XWkCreateWin
(
	NgGO	go
)
{
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

	w = XtVaCreateManagedWidget("exchangeForeBackGround",
				    xmPushButtonGadgetClass,
				    xwk->go.emenu,NULL);
	XtAddCallback(w,XmNactivateCallback,ExchangeForeBackCB,xwk);


	XtVaSetValues(xwk->go.options,
		XmNsensitive,	True,
		NULL);
	
	w = XtVaCreateManagedWidget
		("autoRefreshOption",xmToggleButtonGadgetClass,
		 xwk->go.omenu,
		 XmNset,xp->auto_refresh,
		 NULL);
	XtAddCallback(w,XmNvalueChangedCallback,AutoRefreshOptionCB,xwk);


	w = XtVaCreateManagedWidget
		("drawSingleViewOption",xmToggleButtonGadgetClass,
		 xwk->go.omenu,
		 XmNset,xp->draw_single_view,
		 NULL);
	XtAddCallback(w,XmNvalueChangedCallback,DrawSingleViewOptionCB,xwk);

	XtVaSetValues(xwk->go.view,
		XmNsensitive,	True,
		NULL);
	w = XtVaCreateManagedWidget
		("raiseOnDrawOption",xmToggleButtonGadgetClass,
		 xwk->go.omenu,
		 XmNset,xp->raise_on_draw,
		 NULL);
	XtAddCallback(w,XmNvalueChangedCallback,RaiseOnDrawOptionCB,xwk);

	XtVaSetValues(xwk->go.view,
		XmNsensitive,	True,
		NULL);
	
	w = XtVaCreateManagedWidget
		("clearAllViews",xmPushButtonGadgetClass,
		 xwk->go.vmenu,NULL);
	XtAddCallback(w,XmNactivateCallback,_NgClearAllViewsCB,xwk);

	w = XtVaCreateManagedWidget
		("drawAllViews",xmPushButtonGadgetClass,
		 xwk->go.vmenu,NULL);
	XtAddCallback(w,XmNactivateCallback,_NgDrawAllViewsCB,xwk);

	xp->graphicsSW = XtVaCreateManagedWidget("xworkSW",
				xmScrolledWindowWidgetClass,go->go.manager,
		XmNscrollingPolicy,		XmAPPLICATION_DEFINED,
#if 0
		XmNscrollBarDisplayPolicy,	XmAS_NEEDED,
                XmNvisualPolicy,		XmCONSTANT,
#endif
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
	
	xp->graphics = XtVaCreateManagedWidget
		("graphics",
		 xmDrawingAreaWidgetClass,xp->graphicsSW,
		 XmNbottomShadowColor,	0,
		 XmNhighlightColor,	0,
		 XmNtopShadowColor,	0,
		 XmNbackground,		0,
		 XmNborderColor,	0,
		 XmNdepth,		XcbGetDepth(xp->xcb),
		 XmNcolormap,		XcbGetColormap(xp->xcb),
		 NULL);

	XtAddEventHandler(go->go.shell,StructureNotifyMask,
			  False,MapGraphicsEH,(XtPointer)xwk);

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

static void
ChangeSizeEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
        NgGO go		= (NgGO) udata;
	NgXWk		xwk = (NgXWk)go;
	NgXWkPart	*xp = &xwk->xwk;
	Widget		grVP;
	Dimension	grw,grh,wSW,hSW;
	static Dimension new_wh,w_off,h_off;
	static NhlBoolean user_generated = True,redo = False;
	static NhlBoolean first = True;
	NgWksState wks_state;

#if DEBUG_XWK
	fprintf(stderr,"in xwk ChangeSizeEH\n");
#endif

	if(event->type != ConfigureNotify)
		return;
	if (user_generated && redo) {
		redo = False;
		return;
	}
	if (! user_generated) {
		user_generated = True;

		XtMapWidget(xp->graphicsSW);
		XSync(go->go.x->dpy,False);

		NhlVAGetValues(go->go.appmgr,
			       NgNappWksState,&wks_state,
			       NULL);

		XtVaGetValues(xp->graphics,
			      XmNwidth,&xp->grw,
			      XmNheight,&xp->grh,
			      NULL);
		NgUpdateTransformation(wks_state);
		if (xp->draw_single_view) {
			NgDrawXwkView(xwk->base.id,xp->selected_view_id,True);
		}
		else {
			_NgDrawAllViewsCB(w,(XtPointer)xwk,NULL);
		}

		return;
	}
#if DEBUG_XWK
	fprintf(stderr,"EH height %d width %d\n",
		event->xconfigure.height,
		event->xconfigure.width);
#endif


	XtVaGetValues(xp->graphics,
		      XmNwidth,		&grw,
		      XmNheight,	&grh,
		      NULL);

	XtVaGetValues(xp->graphicsSW,
		      XmNwidth,		&wSW,
		      XmNheight,	&hSW,
		      XmNclipWindow,	&grVP,
		      NULL);
#if DEBUG_XWK
	fprintf(stderr,"wSW %d hSW %d\n",wSW,hSW);
#endif

	if (first) {
		first = False;
		w_off = wSW - grw;
		h_off = hSW - grh;
	}

	new_wh = MIN(wSW - w_off,hSW - h_off);

	if (new_wh != grw || new_wh != grh) {

		XtSetMappedWhenManaged(xp->graphicsSW,False);
		XtUnmapWidget(xp->graphicsSW);

		user_generated = False;
		XtVaSetValues(xp->graphics,
			      XmNwidth,		new_wh,
			      XmNheight,	new_wh,
			      NULL);

		XtVaSetValues(xp->graphicsSW,
			      XmNwidth,(new_wh + w_off),
			      XmNheight,(new_wh + h_off),
			      NULL);

		XtVaGetValues(xp->graphicsSW,
			      XmNwidth,&wSW,
			      XmNheight,&hSW,
			      NULL);

		if (wSW - w_off != hSW - h_off) {
			new_wh = MAX(wSW-w_off,hSW-h_off);
			redo = True;

			XtVaSetValues(xp->graphics,
				      XmNwidth,		new_wh,
				      XmNheight,	new_wh,
				      NULL);

			XtVaSetValues(xp->graphicsSW,
				      XmNwidth,(new_wh + w_off),
				      XmNheight,(new_wh + h_off),
				      NULL);
		}

		return;

	}

	NhlVAGetValues(go->go.appmgr,
		       NgNappWksState,&wks_state,
		       NULL);

	XtVaGetValues(xp->graphics,
		      XmNwidth,&xp->grw,
		      XmNheight,&xp->grh,
		      NULL);
	NgUpdateTransformation(wks_state);
	if (xp->draw_single_view) {
		NgDrawXwkView(xwk->base.id,xp->selected_view_id,True);
	}
	else {
		_NgDrawAllViewsCB(w,(XtPointer)xwk,NULL);
	}

	return;
}

static NhlBoolean
XWkCreateWinHook
(
	NgGO	go
)
{
	NgXWk			xwk = (NgXWk)go;
	NgXWkPart		*xp = &xwk->xwk;
	NgXAppExport		x = go->go.x;
	XSetWindowAttributes	xswa;
	_NGCXAllocColor		xalloccolor;
	Gescape_in_data		gesc_in;

	/*
	 * Geometry hacks follow...
	 */
	XtRealizeWidget(go->go.shell);

	XWkSetMainSize(xwk);

        XtAddEventHandler(go->go.manager,StructureNotifyMask,
                          False,ChangeSizeEH,(XtPointer)go);

	/*
	 * Only until pixmap backup's are working in gks...
	 */
	xswa.backing_store = Always;
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
