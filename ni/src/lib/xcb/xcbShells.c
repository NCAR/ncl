/*
 *      $Id: xcbShells.c,v 1.1 1997-06-11 20:49:23 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		appS.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Apr 18 12:55:58 MDT 1997
 *
 *	Description:	
 */
#include <X11/Intrinsic.h>
#include <Xcb/appSP.h>

#define XcbShellIndex	(-1)
#define XcbOverrideShellIndex		(XmOverrideShellIndex + 1)
#define XcbTransientShellIndex		(XmTransientShellIndex + 1)

static XmPartResource raw_resources[] = {
	/*
	 * Over-ride default values of these resources. (Move them to
	 * the Xcb Class so SetValues can insure that the user doesn't
	 * try and set them - they are now Create/GetValues only resources.
	 */
	{"rm.Depth","Rm.Depth",XtRInt,sizeof(int),
		XmPartOffset(Core,depth), XtRImmediate, (XtPointer)0},
	{"rm.Colormap","Rm.Colormap",XtRColormap,sizeof(Colormap),
		XmPartOffset(Core,colormap),XtRImmediate, (XtPointer)None},
	{"rm.Visual","Rm.Visual",XtRVisual,sizeof(Visual*),
		XmPartOffset(Shell,visual),XtRImmediate,(XtPointer)NULL},
	{XtNdepth,XtCDepth,XtRInt,sizeof(int),
		XmPartOffset(XcbShell,depth), XtRImmediate, (XtPointer)0},
	{XtNcolormap,XtCColormap,XtRColormap,sizeof(Colormap),
		XmPartOffset(XcbShell,colormap),XtRImmediate, (XtPointer)None},
	{XtNvisual,XtCVisual,XtRVisual,sizeof(Visual*),
		XmPartOffset(XcbShell,visual),XtRImmediate,(XtPointer)NULL},

	/*
	 * Change types of "Pixel" resources, so that converters don't
	 * allocate any colors for shell widgets. - They will get allocated
	 * during the "Initialize" method, instead of during the conversion
	 * stage.
	 */
	{"rm.Background","Rm.Background",XtRPixel,sizeof(Pixel),
		XmPartOffset(Core,background_pixel),XtRImmediate,(XtPointer)0},
	{"rm.Border","Rm.Border",XtRPixel,sizeof(Pixel),
		XmPartOffset(Core,border_pixel),XtRImmediate,(XtPointer)0},
	{"back.String","Back.String",XtRString,sizeof(String),
		XmPartOffset(XcbShell,background_string),XtRImmediate,
		(XtPointer)NULL},
	{XtNbackground,XtCBackground,XcbRPixel,sizeof(Pixel),
		XmPartOffset(XcbShell,background_pixel),
		XtRString,(XtPointer)"XtDefaultBackground"},
	{"border.String","Border.String",XtRString,sizeof(String),
		XmPartOffset(XcbShell,border_string),XtRImmediate,
		(XtPointer)NULL},
	{XtNborderColor,XtCBorderColor,XcbRPixel,sizeof(Pixel),
		XmPartOffset(XcbShell,border_pixel),
		XtRString,(XtPointer)"XtDefaultForeground"},

	/*
	 * New resources...
	 */
	{XcbNcolorBroker,XcbCcolorBroker,XtRPointer,sizeof(XtPointer),
		XmPartOffset(XcbShell,color_broker),XtRImmediate,
		(XtPointer)NULL},
	{XcbNparentBroker,XcbCparentBroker,XtRPointer,sizeof(XtPointer),
		XmPartOffset(XcbShell,parent_broker),XtRImmediate,
		(XtPointer)NULL},
	{XcbNcolorMode,XcbCcolorMode,XcbRColorMode,sizeof(XcbMode),
		XmPartOffset(XcbShell,color_mode),XtRImmediate,
		(XtPointer)XcbMIXEDCMAP},
	{XcbNmaxColorCells,XcbCmaxColorCells,XtRInt,sizeof(int),
		XmPartOffset(XcbShell,max_color_cells),XtRImmediate,
		(XtPointer)0},
	{XcbNminColorCells,XcbCminColorCells,XtRInt,sizeof(int),
		XmPartOffset(XcbShell,min_color_cells),XtRImmediate,
		(XtPointer)0},
	{XcbNredLevels,XcbCredLevels,XtRInt,sizeof(int),
		XmPartOffset(XcbShell,red_levels),XtRImmediate,(XtPointer)0},
	{XcbNgreenLevels,XcbCgreenLevels,XtRInt,sizeof(int),
		XmPartOffset(XcbShell,green_levels),XtRImmediate,(XtPointer)0},
	{XcbNblueLevels,XcbCblueLevels,XtRInt,sizeof(int),
		XmPartOffset(XcbShell,blue_levels),XtRImmediate,(XtPointer)0},
	{XcbNrgbError,XcbCrgbError,XtRInt,sizeof(int),
		XmPartOffset(XcbShell,rgb_error),XtRImmediate,(XtPointer)-1},
	{"no.res","No.res",XtRBool,sizeof(Boolean),
		XmPartOffset(XcbShell,my_broker),XtRImmediate,(XtPointer)False}
};

static void
_XcbInitializeConverters(
	void
);


static void
XcbShellClassInitialize(
	Cardinal		oset_index,
	WidgetClass		cptr,
	XcbShellClassPart	*pptr,
	XmPartResource		*classlist
)
{
	int		nres = XtNumber(raw_resources);
	int		i;
	Cardinal	offset;
	Cardinal	level;

	_XcbInitializeConverters();

	/*
	 * Copy raw_resources to resouces, converting -1 offset indexes
	 * to correct index. Then set "resource"/size in class recored,
	 * then call XmResolvePartOffsets.
	 */
	for(i=0;i<nres;i++){
		classlist[i] = raw_resources[i];
		if((classlist[i].resource_offset | XmOFFSETMASK) == -1){
			offset = classlist[i].resource_offset & XmOFFSETMASK;
			classlist[i].resource_offset =
				(oset_index << XmOFFSETBITS) + offset;
		}
	}
	cptr->core_class.resources = (XtResource *)classlist;
	cptr->core_class.num_resources = nres;

	XmResolvePartOffsets(cptr,&pptr->oset);
}

static void
BrokerDestroy
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	XcbShellPart	*xp = (XcbShellPart*)udata.ptrval;

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Xcb destroyed before Shell!",
						XtName(xp->self)));
	/*
	 * Set my_broker to false so Destroy method doesn't try to destroy
	 * it. (That would be re-entrant...)
	 */
	xp->my_broker = False;
	XtDestroyWidget(xp->self);

	return;
}

static void
ChangeCmap
(
	Widget		w,
	Colormap	cmap
)
{
	register int	i;
	CompositePart	*cwp;

	if(XcbIsXcbShell(w))
		return;

	if(XtIsShell(w))
		XtVaSetValues(w,
			XmNcolormap,	cmap,
			NULL);

	if(XtIsComposite(w)){
		cwp = &((CompositeWidget)w)->composite;
		for(i=0;i<cwp->num_children;i++)
			ChangeCmap(cwp->children[i],cmap);
	}

	if(XtIsWidget(w))
		for(i=0;i<w->core.num_popups;i++)
			ChangeCmap(w->core.popup_list[i],cmap);

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
	XcbShellPart	*xp = (XcbShellPart*)udata.ptrval;
	CompositePart	*cwp;
	register int	i;

	xp->colormap = xp->self->core.colormap = cmap;

	if(XtIsRealized(xp->self))
		XSetWindowColormap(XtDisplay(xp->self),XtWindow(xp->self),cmap);

	if(XtIsComposite(xp->self)){
		cwp = &((CompositeWidget)xp->self)->composite;
		for(i=0;i<cwp->num_children;i++)
			ChangeCmap(cwp->children[i],cmap);
	}

	if(XtIsWidget(xp->self))
		for(i=0;i<xp->self->core.num_popups;i++)
			ChangeCmap(xp->self->core.popup_list[i],cmap);

	return;
}

static void
XcbShellInitialize(
	CorePart	*cp,
	ShellPart	*sp,
	XcbShellPart	*xp
)
{
	Xcb		xcb = xp->color_broker;
	Xcb		parent;
	XcbAttrRec	xcbattr;
	unsigned long	mask = 0;
	XColor		nc;
	NhlArgVal	sel,udata;

	if(xcb){
		if(xcb->scr != XScreenNumberOfScreen(cp->screen))
			xcb = NULL;
	}

	if(!xcb){
		mask |= XcbMODE;
		xcbattr.mode = xp->color_mode;

		mask |= XcbSCREEN;
		xcbattr.scr = XScreenNumberOfScreen(cp->screen);

		if(xp->visual){
			mask |= XcbVIS;
			xcbattr.vis = xp->visual;

			if(xp->colormap){
				mask |= XcbCMAP;
				xcbattr.cmap = xp->colormap;
			}

		}

		if(xp->parent_broker){
			xcbattr.parent = xp->parent_broker;
		}
		else if(cp->parent){
			xcbattr.parent = xp->parent_broker =
					XcbGetXcbFromWidget(cp->parent);
		}

		if(xp->visual && xcbattr.parent &&
			(XVisualIDFromVisual(xp->visual) !=
			XVisualIDFromVisual(XcbGetVisual(xcbattr.parent))))
				xcbattr.parent = NULL;
		if(xcbattr.parent)
			mask |= XcbPARENT;

		if(xp->max_color_cells){
			mask |= XcbMAXNCOLS;
			xcbattr.max_ncols = xp->max_color_cells;
		}

		if(xp->min_color_cells){
			mask |= XcbMINNCOLS;
			xcbattr.min_ncols = xp->min_color_cells;
		}

		mask |= XcbRGBLEVELS;
		xcbattr.rlevels = xp->red_levels;
		xcbattr.glevels = xp->green_levels;
		xcbattr.blevels = xp->blue_levels;

		if(xp->rgb_error != -1){
			mask |= XcbRGBERR;
			xcbattr.rgberr = xp->rgb_error;
		}

		xcb = XcbCreate(XtDisplay(cp->self),&xcbattr,mask);

		if(!xcb){
			/*
			 * HOW TO FAIL???
			 */
			NHLPERROR((NhlFATAL,NhlNOERROR,
			"Unable to initialize Xcb color for Shell!!!!"));
		}
		else
			xp->my_broker = True;
	}
	xp->color_broker = xcb;
	xp->self = cp->self;

	if(!xcb)
		return;

	NhlINITVAR(sel);
	NhlINITVAR(udata);
	udata.ptrval = xp;

	xp->broker_destroyCB = _NhlCBAdd(xcb->destroyCBL,sel,BrokerDestroy,
									udata);
	xp->broker_cfaultCB = _NhlCBAdd(xcb->cfaultCBL,sel,CFault,udata);

	cp->colormap = xp->colormap = xcb->cmap;
	cp->depth = xp->depth = xcb->visinfo->depth;
	sp->visual = xp->visual = xcb->vis;

	if(xp->background_string &&
		XcbAllocNamedColor(xcb,xp->background_string,&nc))
		xp->background_pixel = nc.pixel;
	if(xp->border_string &&
		XcbAllocNamedColor(xcb,xp->border_string,&nc))
		xp->border_pixel = nc.pixel;

	cp->border_pixel = xp->border_pixel;
	cp->background_pixel = xp->background_pixel;

	return;
}

static void
XcbShellDestroy(
	XcbShellPart	*xp
)
{

	if(xp->background_string)
		XcbFreeColor(xp->color_broker,xp->background_pixel);
	if(xp->border_string)
		XcbFreeColor(xp->color_broker,xp->border_pixel);

	_NhlCBDelete(xp->broker_destroyCB);
	if(xp->my_broker)
		XcbDestroy(xp->color_broker);

	return;
}

static Boolean
XcbShellSetValues(
	XcbShellPart	*op,
	XcbShellPart	*np
)
{
	XtAppContext			app =
					XtWidgetToApplicationContext(np->self);
	Boolean				redisplay = False;
	Mask				win_mask = 0;
	XSetWindowAttributes		attr;

	if(op->depth != np->depth){
		XtAppWarningMsg(app,"invalidDepth","setValues","XcbError",
					"Can't change widget depth",
					(String*)NULL,(Cardinal*)NULL);
		np->depth = op->depth;
	}

	if(op->colormap != np->colormap){
		XtAppWarningMsg(app,"invalidColormap","setValues","XcbError",
				"Colormap can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->colormap = op->colormap;
	}

	if(op->visual != np->visual){
		XtAppWarningMsg(app,"invalidVisual","setValues","XcbError",
				"Visual can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->visual = op->visual;
	}

	if(op->background_pixel != np->background_pixel){
		if(op->background_string){
			XcbFreeColor(op->color_broker,op->background_pixel);
			np->background_string = NULL;
		}
		if(XtIsRealized(np->self)){
			attr.background_pixel = np->background_pixel;
			win_mask |= CWBackPixel;
			redisplay = True;
		}
	}

	if(op->border_pixel != np->border_pixel){
		if(op->border_string){
			XcbFreeColor(op->color_broker,op->border_pixel);
			np->border_string = NULL;
		}
		if(XtIsRealized(np->self)){
			attr.border_pixel = np->border_pixel;
			win_mask |= CWBorderPixel;
			redisplay = True;
		}
	}

	if(win_mask != 0)
		XChangeWindowAttributes(XtDisplay(np->self),XtWindow(np->self),
								win_mask,&attr);

	if(op->color_broker != np->color_broker){
		XtAppWarningMsg(app,"invalidXcb","setValues","XcbError",
				"ColorBroker can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->color_broker = op->color_broker;
	}

	if(op->parent_broker != np->parent_broker){
		XtAppWarningMsg(app,"invalidXcb","setValues","XcbError",
			"ParentBroker can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->parent_broker = op->parent_broker;
	}

	if(op->color_mode != np->color_mode){
		XtAppWarningMsg(app,"invalidColorMode","setValues","XcbError",
			"ColorMode can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->color_mode = op->color_mode;
	}

	if(op->max_color_cells != np->max_color_cells){
		XtAppWarningMsg(app,"invalidMaxColors","setValues","XcbError",
			"MaxColorCells can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->max_color_cells = op->max_color_cells;
	}

	if(op->min_color_cells != np->min_color_cells){
		XtAppWarningMsg(app,"invalidMaxColors","setValues","XcbError",
			"MinColorCells can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->min_color_cells = op->min_color_cells;
	}

	if(op->red_levels != np->red_levels){
		XtAppWarningMsg(app,"invalidRedLevels","setValues","XcbError",
			"RedLevels can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->red_levels = op->red_levels;
	}

	if(op->green_levels != np->green_levels){
		XtAppWarningMsg(app,"invalidGreenLevels","setValues","XcbError",
			"GreenLevels can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->green_levels = op->green_levels;
	}

	if(op->blue_levels != np->blue_levels){
		XtAppWarningMsg(app,"invalidBlueLevels","setValues","XcbError",
			"BlueLevels can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->blue_levels = op->blue_levels;
	}

	if(op->rgb_error != np->rgb_error){
		XtAppWarningMsg(app,"invalidRGBError","setValues","XcbError",
			"RGBError can only be set at initialization",
				(String*)NULL,(Cardinal*)NULL);
		np->rgb_error = op->rgb_error;
	}

	return redisplay;
}

/************************************************************************
*	XcbApplicationShellClassWidget stuff				*
************************************************************************/

static void
XcbApplicationShellClassInitialize(
	void
);

static void
XcbApplicationShellInitialize(
	Widget		req,
	Widget		new,
	ArgList		args,
	Cardinal	*nargs
);

static void
XcbApplicationShellDestroy(
	Widget	wid
);

static Boolean
XcbApplicationShellSetValues(
	Widget		old,
	Widget		req,
	Widget		new,
	ArgList		args,
	Cardinal	*nargs
);

XcbApplicationShellClassRec xcbApplicationShellClassRec = {
	/* Core */
{
/* superclass		*/	(WidgetClass)&applicationShellClassRec,
/* class_name		*/	"XcbApplicationShell",
/* widget_size		*/	sizeof(XcbShellPart),
/* class_initialize	*/	XcbApplicationShellClassInitialize,
/* class_part_initialize*/	NULL,
/* class_inited		*/	FALSE,
/* initialize		*/	XcbApplicationShellInitialize,
/* initialize_hook	*/	NULL,
/* realize		*/	XtInheritRealize,
/* actions		*/	NULL,
/* num_actions		*/	0,
/* resources		*/	NULL,
/* num_resources	*/	0,
/* xrm_class		*/	NULLQUARK,
/* compress_motion	*/	FALSE,
/* compress_exposure	*/	TRUE,
/* compress_enterleave	*/	FALSE,
/* visible_interest	*/	FALSE,
/* destroy		*/	XcbApplicationShellDestroy,
/* resize		*/	XtInheritResize,
/* expose		*/	NULL,
/* set_values		*/	XcbApplicationShellSetValues,
/* set_values_hook	*/	NULL,
/* set_values_almost	*/	XtInheritSetValuesAlmost,
/* get_values_hook	*/	NULL,
/* accept_focus		*/	NULL,
/* version		*/	XtVersionDontCheck,
/* callback_offsets	*/	NULL,
/* tm_table		*/	XtInheritTranslations,
/* query_geometry	*/	NULL,
/* display_accelerator	*/	NULL,
/* extension		*/	NULL
},
	/* composite */
{
/* geometry_manager	*/	XtInheritGeometryManager,
/* change_managed	*/	XtInheritChangeManaged,
/* insert_child		*/	XtInheritInsertChild,
/* delete_child		*/	XtInheritDeleteChild,
/* extension		*/	NULL
},
	/* shell */
{
/* extension		*/	NULL
},
	/* wm */
{
/* extension		*/	NULL
},
	/* vendor */
{
/* extension		*/	NULL
},
	/* topLevel */
{
/* extension		*/	NULL
},
	/* application */
{
/* extension		*/	NULL
},
	/* xcb */
{
/* oset			*/	0
}
};

WidgetClass xcbApplicationShellWidgetClass = (WidgetClass)&xcbApplicationShellClassRec;

static XmPartResource	app_resources[XtNumber(raw_resources)];

static void
XcbApplicationShellClassInitialize(
	void
)
{
	XcbShellClassInitialize(XcbApplicationShellIndex,
		xcbApplicationShellWidgetClass,
		&xcbApplicationShellClassRec.xcb_application_shell_class,
		app_resources);

	return;
}

static void
XcbApplicationShellInitialize(
	Widget		req,
	Widget		new,
	ArgList		args,
	Cardinal	*nargs
)
{
	XcbApplicationShellClass	cl =
			(XcbApplicationShellClass)new->core.widget_class;
	XmOffsetPtr	oset = cl->xcb_application_shell_class.oset;

	CorePart	*cp = (CorePart*)((char*)new + oset[CoreIndex]);
	ShellPart	*sp = (ShellPart*)((char*)new + oset[ShellIndex]);
	XcbShellPart	*xp = (XcbShellPart*)((char*)new +
						oset[XcbApplicationShellIndex]);
	XcbShellInitialize(cp,sp,xp);

	return;
}

static void
XcbApplicationShellDestroy(
	Widget	w
)
{
	XcbApplicationShellClass	cl =
			(XcbApplicationShellClass)w->core.widget_class;
	XmOffsetPtr	oset = cl->xcb_application_shell_class.oset;
	XcbShellPart	*xp = (XcbShellPart*)((char*)w +
						oset[XcbApplicationShellIndex]);

	XcbShellDestroy(xp);
}

static Boolean
XcbApplicationShellSetValues(
	Widget		old,
	Widget		req,
	Widget		new,
	ArgList		args,
	Cardinal	*nargs
)
{
	XcbApplicationShellClass	cl =
			(XcbApplicationShellClass)new->core.widget_class;
	XmOffsetPtr	oset = cl->xcb_application_shell_class.oset;
	XcbShellPart	*np = (XcbShellPart*)((char*)new +
						oset[XcbApplicationShellIndex]);
	XcbShellPart	*op = (XcbShellPart*)((char*)old +
						oset[XcbApplicationShellIndex]);

	return XcbShellSetValues(op,np);
}

/*
 * Public API
 */

Boolean
XcbIsXcbShell(
	Widget	w
)
{
	return (w->core.widget_class == xcbApplicationShellWidgetClass);
}

Xcb
XcbGetXcbFromWidget(
	Widget	w
)
{
	WidgetClass	cl = w->core.widget_class;
	XmOffsetPtr	oset;
	XcbShellPart	*xp;

	if(!XtIsShell(w))
		return XcbGetXcbFromWidget(w->core.parent);

	if(cl == xcbApplicationShellWidgetClass){
		oset = xcbApplicationShellClassRec.xcb_application_shell_class.oset;
		xp = (XcbShellPart*)((char*)w+oset[XcbApplicationShellIndex]);
		return xp->color_broker;
	}
	else if(w->core.parent)
		return XcbGetXcbFromWidget(w->core.parent);

	return NULL;
}

#define	done(type, value) \
	{							\
	    if (toVal->addr != NULL) {				\
		if (toVal->size < sizeof(type)) {		\
		    toVal->size = sizeof(type);			\
		    return False;				\
		}						\
		*(type*)(toVal->addr) = (value);		\
	    }							\
	    else {						\
		static type static_val;				\
		static_val = (value);				\
		toVal->addr = (XPointer)&static_val;		\
	    }							\
	    toVal->size = sizeof(type);				\
	    return True;					\
	}

static XtConvertArgRec xcbColorConvertArgs[] = {
	{XtWidgetBaseOffset,(XtPointer)XtOffset(Widget,core.self),
								sizeof(Widget)}
};

static Boolean
XcbCvtStringToPixel(
    Display*	dpy,
    XrmValuePtr	args,
    Cardinal	*num_args,
    XrmValuePtr	fromVal,
    XrmValuePtr	toVal,
    XtPointer	*closure_ret
)
{
	String		str = (String)fromVal->addr;
	XtAppContext	app = XtDisplayToApplicationContext(dpy);
	XColor		screenColor;
	XColor		exactColor;
	Widget		w;
	Xcb		xcb;
	Status		status;
	String		msg,params[1];
	Cardinal	num_params=1;
	Colormap	cmap;

	if (*num_args != 1) {
		XtAppWarningMsg(app,"wrongParameters","XcbCvtStringToPixel",
			"XcbError",
			"String to pixel conversion needs widget argument",
			(String *)NULL, (Cardinal *)NULL);
		return False;
	}

	w = *((Widget *) args[0].addr);
	xcb = XcbGetXcbFromWidget(w);

	if(strcasecmp(str,XtDefaultBackground) == 0){
		if(!xcb){
			*closure_ret = False;
			done(Pixel,WhitePixelOfScreen(w->core.screen));
		}
		else
			str = "white";
	}
	else if(strcasecmp(str,XtDefaultForeground) == 0){
		if(!xcb){
			*closure_ret = False;
			done(Pixel,BlackPixelOfScreen(w->core.screen));
		}
		else
			str = "black";
	}

	if(xcb)
		cmap = XcbGetColormap(xcb);
	else
		cmap = w->core.colormap;

	params[0] = str;
	if(!XParseColor(dpy,cmap,str,&exactColor)){
		msg = "Color name \"%s\" is not defined";
		XtAppWarningMsg(app,"badValue","cvtStringToPixel",
				"XcbError", msg, params, &num_params);
		*closure_ret = False;
		return False;
	}

	screenColor = exactColor;
	if(!xcb){
		if(!XAllocColor(dpy,w->core.colormap,&screenColor)){
			msg = "Cannot allocate colormap entry for \"%s\"";
			XtAppWarningMsg(app,"noColormap","cvtStringToPixel",
				"XcbError", msg, params, &num_params);
			*closure_ret = False;
			return False;
		}
	}
	else
		XcbAllocROColor(xcb,&screenColor);

	*closure_ret == (char*)True;
	done(Pixel,screenColor.pixel);
}

/* ARGSUSED */
static void XcbFreePixel(app, toVal, closure, args, num_args)
    XtAppContext app;
    XrmValuePtr	toVal;
    XtPointer	closure;
    XrmValuePtr	args;
    Cardinal	*num_args;
{
	Widget	w;
	Xcb	xcb;

	if(*num_args != 1){
		XtAppWarningMsg(app,"wrongParameters","XcbFreePixel","XcbColor",
			"Freeing a pixel requires widget argument",
			(String*)NULL,(Cardinal*)NULL);
		return;
	}

	if(!closure)
		return;

	w = *((Widget *) args[0].addr);
	xcb = XcbGetXcbFromWidget(w);

	if(!xcb)
		XFreeColors(XtDisplay(w),w->core.colormap,
			(unsigned long*)toVal->addr,1,(unsigned long)0);
	else
		XcbFreeColor(xcb,*(unsigned long*)toVal->addr);
}

static Boolean
XcbCvtColorToXcbPixel(
	Display		*dpy,
	XrmValuePtr	args,
	Cardinal	*num_args,
	XrmValuePtr	fromVal,
	XrmValuePtr	toVal,
	XtPointer	*closure_ret
)
{
	if(*num_args != 0)
		XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
			"wrongParameters","cvtXColorToPixel","XcbError",
			"Color to Pixel conversion takes 0 args",
			(String*)NULL,(Cardinal*)NULL);
	done(Pixel,((XColor*)fromVal->addr)->pixel);
}

static Boolean
XcbCvtIntToXcbPixel(
	Display		*dpy,
	XrmValuePtr	args,
	Cardinal	*num_args,
	XrmValuePtr	fromVal,
	XrmValuePtr	toVal,
	XtPointer	*closure_ret
)
{
	if(*num_args != 0)
		XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
			"wrongParameters","cvtXColorToPixel","XcbError",
			"Color to Pixel conversion takes 0 args",
			(String*)NULL,(Cardinal*)NULL);
	done(Pixel,*(int*)fromVal->addr);
}

static Boolean
XcbCvtStringToXcbPixel(
	Display		*dpy,
	XrmValuePtr	args,
	Cardinal	*num_args,
	XrmValuePtr	fromVal,
	XrmValuePtr	toVal,
	XtPointer	*closure_ret
)
{
	XtAppContext	app = XtDisplayToApplicationContext(dpy);
	String		*str = (String*)((char*)toVal->addr - sizeof(String));;

	if(*num_args != 0){
		XtAppWarningMsg(app,"wrongParameters","cvtStringToXcbPixel",
			"XcbError","String to XcbPixel needs no args",
			(String*)NULL,(Cardinal*)NULL);
		return False;
	}

	*str = (String)fromVal->addr;
	*closure_ret = (char*)False;
	done(Pixel,0);
}

/*ARGSUSED*/
static Boolean XcbCvtStringToVisual(
	Display		*dpy,
	XrmValuePtr	args,
	Cardinal	*num_args,
	XrmValuePtr	fromVal,
	XrmValuePtr	toVal,
	XtPointer	*closure_ret
)
{
	String		str = (String)fromVal->addr;
	char		*endptr;
	int		vc;
	XVisualInfo	vinfo;
	XVisualInfo	vtmpl;
	Visual		*def_vis;
	Visual		*xcb_vis=NULL;
	Widget		w;
	Screen		*scr;
	int		depth;
	Xcb		xcb=NULL;

	if(*num_args != 1){
		XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
			"wrongParameters","xcbCvtStringToVisual",
			"XcbError",
		"String to Visual conversion needs widget argument",
			(String *) NULL, (Cardinal *)NULL);
		return False;
	}

	w = *(Widget*)args[0].addr;
	scr = w->core.screen;
	if(w->core.widget_class==xcbApplicationShellWidgetClass){
		XmOffsetPtr	oset;
		XcbShellPart	*xp;

		oset = xcbApplicationShellClassRec.xcb_application_shell_class.oset;
		xp = (XcbShellPart*)((char*)w+oset[XcbApplicationShellIndex]);
		depth = xp->depth;
	}
	else
		depth = w->core.depth;

	def_vis = DefaultVisualOfScreen(scr);

	if (strcasecmp(str,"StaticGray") == 0)		vc = StaticGray;
	else if (strcasecmp(str,"StaticColor") == 0)	vc = StaticColor;
	else if (strcasecmp(str,"TrueColor") == 0)	vc = TrueColor;
	else if (strcasecmp(str,"GrayScale") == 0)	vc = GrayScale;
	else if (strcasecmp(str,"PseudoColor") == 0)	vc = PseudoColor;
	else if (strcasecmp(str,"DirectColor") == 0)	vc = DirectColor;
	else if (isdigit((int)*str) &&
		(vc = (int)strtol(str,&endptr,0)) &&
		(str != endptr))			;
	else if (strcasecmp(str,"DefaultVisual") == 0){
		if((DefaultDepth(dpy,XScreenNumberOfScreen(scr)) == depth) ||
			(w->core.widget_class==xcbApplicationShellWidgetClass)){
			done(Visual*,def_vis);
		}
		else{
			XtDisplayStringConversionWarning(dpy,str,
							"Doesn't match depth");
			return False;
		}
	}
	else if ((strcasecmp(str,"XcbVisual") == 0) &&
		((w->core.widget_class==xcbApplicationShellWidgetClass) ||
			(w->core.parent != NULL) &&
			(xcb = XcbGetXcbFromWidget(w->core.parent)))){
		if(xcb)
			xcb_vis = XcbGetVisual(xcb);
		done(Visual*,xcb_vis);
	}
	else{
		XtDisplayStringConversionWarning(dpy,str,"Visual class name");
		return False;
	}

	if (XMatchVisualInfo(dpy,XScreenNumberOfScreen(scr),depth,vc,&vinfo)){
		done( Visual*, vinfo.visual );
	}
	else {
		String params[2];
		Cardinal num_params = 2;
		params[0] = str;
		params[1] = DisplayString(dpy);
		XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
			"conversionError","stringToVisual","XcbError",
			"Cannot find Visual of class %s for display %s",
			params,&num_params );
		return False;
	}
}

/*ARGSUSED*/
static Boolean XcbCvtStringToXcbColorMode(
	Display		*dpy,
	XrmValuePtr	args,
	Cardinal	*num_args,
	XrmValuePtr	fromVal,
	XrmValuePtr	toVal,
	XtPointer	*closure_ret
)
{
	String		str = (String)fromVal->addr;
	XcbMode		mode;

	if(*num_args != 0){
		XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
			"wrongParameters","XcbCvtStringToXcbColorMode",
			"XcbError",
		"String to XcbColorMode conversion needs no arguments",
			(String *) NULL, (Cardinal *)NULL);
		return False;
	}

	if (strcasecmp(str,"sharedcmap") == 0)		mode = XcbSHAREDCMAP;
	else if (strcasecmp(str,"mixedcmap") == 0)	mode = XcbMIXEDCMAP;
	else if (strcasecmp(str,"privatecmap") == 0)	mode = XcbPRIVATECMAP;
	else{
		XtDisplayStringConversionWarning(dpy,str,"XcbColorMode name");
		return False;
	}

	done( XcbMode, mode );
}

static void
_XcbInitializeConverters(
	void
)
{
	static Boolean	init = FALSE;

	if(init)
		return;

	init = TRUE;

	/*
	 * MyPixel is a type that should not ever actually allocate a
	 * color - it should just save the string.  If it is specified with
	 * an "int" or a "Color", then it should do the same thing the
	 * ToPixel converters would do.
	 * These are only used in the xcb shells to over-ride the background
	 * and border resources in the shell.
	 */
	XtSetTypeConverter(XtRColor,XcbRPixel,XcbCvtColorToXcbPixel,NULL,0,
						XtCacheNone,(XtDestructor)NULL);
	XtSetTypeConverter(XtRInt,XcbRPixel,XcbCvtIntToXcbPixel,NULL,0,
						XtCacheNone,(XtDestructor)NULL);
	XtSetTypeConverter(XcbRPixel,XtRColor,XtCvtIntToColor,
			(XtConvertArgList)colorConvertArgs,2,
			XtCacheByDisplay,(XtDestructor)NULL);
	XtSetTypeConverter(XtRString,XcbRPixel,XcbCvtStringToXcbPixel,
			NULL,0,XtCacheNone,(XtDestructor)NULL);

	XtSetTypeConverter(XtRString,XtRPixel,XcbCvtStringToPixel,
		xcbColorConvertArgs,XtNumber(xcbColorConvertArgs),
		XtCacheByDisplay|XtCacheRefCount,XcbFreePixel);

#if	(XmVERSION >= 2)
	XtSetTypeConverter(XtRString,XmRSelectColor,XcbCvtStringToPixel,
		xcbColorConvertArgs,XtNumber(xcbColorConvertArgs),
		XtCacheByDisplay|XtCacheRefCount,XcbFreePixel);
#endif

	XtSetTypeConverter(XtRString,XtRVisual,XcbCvtStringToVisual,
		xcbColorConvertArgs,XtNumber(xcbColorConvertArgs),
		XtCacheByDisplay,NULL);

	XtSetTypeConverter(XtRString,XcbRColorMode,XcbCvtStringToXcbColorMode,
		NULL,0,XtCacheAll,(XtDestructor)NULL);
}
