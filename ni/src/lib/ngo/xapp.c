/*
 *      $Id: xapp.c,v 1.2 1996-11-24 22:27:36 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xapp.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 29 15:55:07 MDT 1996
 *
 *	Description:	
 */
#include <ncarg/ngo/xappP.h>
#include <ncarg/hlu/AppI.h>

#include <X11/cursorfont.h>
#include <Xm/Xm.h>

#define	Oset(field)	NhlOffset(NgXAppMgrRec,xapp.field)
static NhlResource resources[] = {
	{NgNxappDpy,NgCxappDpy,NhlTPointer,sizeof(NhlPointer),
		Oset(x.dpy),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CGONLY,NULL},
	{NgNxappContext,NgCxappContext,NhlTPointer,sizeof(NhlPointer),
		Oset(x.app_con),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CGONLY,NULL},
	{NgNxappArgc,NgCxappArgc,NhlTInteger,sizeof(int),
		Oset(argc),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_CONLY,NULL},
	{NgNxappArgv,NgCxappArgv,NhlTPointer,sizeof(NhlPointer),
		Oset(argv),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CONLY,NULL},
	{NgNxappExport,NgCxappExport,NhlTPointer,sizeof(NhlPointer),
		Oset(x),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_GONLY,NULL},
};
#undef	Oset

static NhlErrorTypes XAppMgrClassPartInitialize(
	NhlClass	lc
);

static NhlErrorTypes XAppMgrClassInitialize(
	void
);

static NhlErrorTypes XAppMgrInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes XAppMgrGetValues(
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes XAppMgrDestroy(
	NhlLayer	l
);

static void XAppRun(
	NgAppMgr	app
);

static void XAppDevWProc(
	NgAppMgr		app,
	NgWorkProcHandler	wp
);

NgXAppMgrClassRec NgxappMgrClassRec = {
	{
/* class_name			*/	"xappMgrClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NgXAppMgrRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NgappMgrClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,

/* class_part_initialize	*/	XAppMgrClassPartInitialize,
/* class_initialize		*/	XAppMgrClassInitialize,
/* layer_initialize		*/	XAppMgrInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	XAppMgrGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	XAppMgrDestroy,

	},
	{
/* num_mgrs			*/	0,
/* run_proc			*/	XAppRun,
/* dev_wproc			*/	XAppDevWProc,
	},
	{
/* foo				*/	0
	}
};

NhlClass NgxappMgrClass = (NhlClass)&NgxappMgrClassRec;

/*
 * Function:	XAppMgrClassPartInitialize
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
XAppMgrClassPartInitialize
(
	NhlClass	lc
)
{
	/*
	 * Subclasses of this class are not valid!
	 */
	if(lc != (NhlClass)&NgxappMgrClassRec)
		return NhlFATAL;

	return NhlNOERROR;
}

/*
 * Function:	XAppMgrClassInitialize
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
XAppMgrClassInitialize
(
	void
)
{
	XtToolkitInitialize();

	return NhlNOERROR;
}

/*
 * Function:	LoadXres
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
NhlBoolean
LoadXres
(
	NrmDatabase		*db,
	NrmBindingList		bindings,
	NrmQuarkList		quarks,
	NrmRepresentation	*type,
	NrmValue		*value,
	NhlPointer		closure
)
{
	XrmDatabase		xdb = (XrmDatabase)closure;
	XrmBinding		stack_xbindings[100];
	XrmQuark		stack_xquarks[100];
	XrmBindingList		xbindings = stack_xbindings;
	XrmQuarkList		xquarks = stack_xquarks;
	XrmRepresentation	xtype;
	XrmValue		xvalue;

	while(*quarks){
		switch(*bindings){
			case NrmBindLoosely:
				*xbindings = XrmBindLoosely;
			break;
			case NrmBindTightly:
				*xbindings = XrmBindTightly;
			break;
			default:
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Bad Binding"));
		}
		*xquarks = XrmStringToQuark(NrmQuarkToString(*quarks));
		quarks++;bindings++;
		xquarks++;xbindings++;
	}
	*xquarks = 0;
	xtype = XrmStringToQuark(NrmQuarkToString(*type));
	xvalue.size = value->size;
	xvalue.addr = value->data.ptrval;

	XrmQPutResource(&xdb,stack_xbindings,stack_xquarks,xtype,&xvalue);

	return False;
}

/*
 * Function:	LoadXresFromNres
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
LoadXresFromNres
(
	NrmDatabase	db,
	Display		*dpy
)
{
	NrmQuark	none = NrmNULLQUARK;

	NrmEnumerateDatabase(db,&none,&none,NrmEnumAllLevels,LoadXres,
		XtScreenDatabase(DefaultScreenOfDisplay(dpy)));

	return;
}

/*
 * Function:	XAppMgrInitialize
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
XAppMgrInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char		func[] = "XAppMgrInitialize";
	NgXAppMgrClass	ac = (NgXAppMgrClass)lc;
	NgXAppMgrPart	*xapp = &((NgXAppMgr)new)->xapp;
	NgAppMgrPart	*app = &((NgXAppMgr)new)->app;

	if(!xapp->x.app_con){
		if(xapp->x.dpy)
			xapp->x.app_con = XtDisplayToApplicationContext(
								xapp->x.dpy);
		else
			xapp->x.app_con = XtCreateApplicationContext();
	}
	if(!xapp->x.app_con){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to get Xt Application Context",func);
		return NhlFATAL;
	}

	if(!xapp->x.dpy){
		xapp->x.dpy = XtOpenDisplay(xapp->x.app_con,NULL,
				app->app_name,app->app_class,
				NULL,0,&xapp->argc,xapp->argv);
	}

	/*
	 * These are only valid during Create, so null them out now.
	 */
	xapp->argc = NULL;
	xapp->argv = NULL;

	if(!xapp->x.dpy){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to open display %s",
							getenv("DISPLAY"));
		return NhlFATAL;
	}

	xapp->x.wm_delete_window = XmInternAtom(xapp->x.dpy,"WM_DELETE_WINDOW",
									False);
	xapp->x.wait = XCreateFontCursor(xapp->x.dpy,XC_watch);

	LoadXresFromNres(_NhlGetResDB(new),xapp->x.dpy);

	return NhlNOERROR;
}

/*
 * Function:	XAppMgrGetValues
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
XAppMgrGetValues
(
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
)
{
	static NrmQuark	qexp = NrmNULLQUARK;
	NgXAppMgrPart	*xapp = &((NgXAppMgr)l)->xapp;
	int		i;

	if(!qexp)
		qexp = NrmStringToQuark(NgNxappExport);

	for(i=0;i<nargs;i++){
		if(args[i].quark == qexp)
			*(NgXAppExport*)args[i].value.ptrval = &xapp->x;
	}

	return NhlNOERROR;
}

/*
 * Function:	XAppMgrDestroy
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
XAppMgrDestroy
(
	NhlLayer	l
)
{
/*
 * TODO!!!
 */
	return NhlNOERROR;
}

/*
 * Function:	NgXAppRun
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
XAppRun
(
	NgAppMgr	app
)
{
	NgXAppMgr	xapp = (NgXAppMgr)app;

	XtAppMainLoop(xapp->xapp.x.app_con);
}

typedef struct XAppWorkType{
	NgAppMgr		app;
	NgWorkProcHandler	wp;
} *XAppWorkType;

/*
 * Function:	XWorkProcs
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
static Boolean		/* "X" type */
XWorkProcs
(
	XtPointer	client_data
)
{
	XAppWorkType	xwk = (XAppWorkType)client_data;
	NhlBoolean	done;

	done = (*xwk->wp)(xwk->app);

	if(done)
		NhlFree(xwk);

	return done;
}

/*
 * Function:	XAppDevWProc
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
XAppDevWProc(
	NgAppMgr		app,
	NgWorkProcHandler	wp
)
{
	NgXAppMgrPart	*xapp = &((NgXAppMgr)app)->xapp;
	XAppWorkType	xwk = NhlMalloc(sizeof(struct XAppWorkType));

	if(!xwk){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	xwk->app = app;
	xwk->wp = wp;

	(void)XtAppAddWorkProc(xapp->x.app_con,XWorkProcs,xwk);

	return;
}

XmString
NgXAppCreateXmString
(
	int	xappid,
	char	*str
)
{
	char		func[] = "NgXAppCreateXmString";
	NgXAppMgr	xapp = (NgXAppMgr)_NhlGetLayer(xappid);

	if(!xapp || !_NhlIsClass((NhlLayer)xapp,NgxappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid xappid",func));
		return NULL;
	}

	/*
	 * If it is not a multiline text string it is faster to call
	 * XmStringCreate.
	 */
	if(strchr(str,'\n'))
		return XmStringCreateLtoR(str,XmFONTLIST_DEFAULT_TAG);
	return XmStringCreate(str,XmFONTLIST_DEFAULT_TAG);
}

void
NgXAppFreeXmString
(
	int		xappid,
	XmString	xmstr
)
{
	char		func[] = "NgXAppFreeXmString";
	NgXAppMgr	xapp = (NgXAppMgr)_NhlGetLayer(xappid);

	if(!xapp || !_NhlIsClass((NhlLayer)xapp,NgxappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid xappid",func));
		return;
	}

	XmStringFree(xmstr);
}
