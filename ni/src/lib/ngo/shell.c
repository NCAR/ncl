/*
 *      $Id: shell.c,v 1.4 2000-03-21 02:35:52 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  199r			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		shapego.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Aug 31 14:25:53 MDT 1999
 *
 *	Description:	
 */
#include <math.h>

#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/shellP.h>
#include <Xcb/xcbP.h>

#include <Xm/Xm.h>
#include <Xm/MessageB.h>


#define	Oset(field)	NhlOffset(NgShellRec,shell.field)
static NhlResource resources[] = {
	{NgNshContentFunc,NgCshContentFunc,NhlTPointer,sizeof(NhlPointer),
	 Oset(cfunc),NhlTImmediate,_NhlUSET(NULL),
	 _NhlRES_CONLY,NULL},      
	{NgNshContentFuncData,NgCshContentFuncData,NhlTPointer,
	 sizeof(NhlPointer),Oset(cfunc_data),NhlTImmediate,_NhlUSET(NULL),
	 _NhlRES_CONLY,NULL},      
	{NgNshDoFocusGrab,NgCshDoFocusGrab,NhlTBoolean,
	 sizeof(NhlBoolean),Oset(do_focus_grab),NhlTImmediate,
	 _NhlUSET((NhlPointer)False),_NhlRES_CONLY,NULL},      
};
#undef	Oset

static NhlErrorTypes ShellInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes ShellDestroy(
	NhlLayer	l
);

static NhlBoolean ShellCreateWin(
	NgGO	go
);

static NhlBoolean ShellCreateWinHook(
	NgGO	go
);

static NhlErrorTypes ShellClassPartInitialize(
	NhlClass	lc
);

NgShellClassRec NgshellClassRec = {
	{
/* class_name		*/	"shellClass",
/* nrm_class		*/	NrmNULLQUARK,
/* layer_size		*/	sizeof(NgShellRec),
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

/* class_part_initialize*/	ShellClassPartInitialize,
/* class_initialize	*/	NULL,
/* layer_initialize	*/	ShellInitialize,
/* layer_set_values	*/	NULL,
/* layer_set_values_hook*/	NULL,
/* layer_get_values	*/	NULL,
/* layer_reparent	*/	NULL,
/* layer_destroy	*/	ShellDestroy,

	},
	{
/* dialog		*/	NULL,
/* toplevel		*/	NULL,
/* manager		*/	NULL,
/* top_win_chain	*/	False,
/* create_win		*/	ShellCreateWin,
/* create_win_hook	*/	ShellCreateWinHook,
/* close		*/	_NgGOInheritClose,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgshellClass = (NhlClass)&NgshellClassRec;

static NhlErrorTypes
ShellClassPartInitialize
(
	NhlClass	lc
)
{
	NgShellClass	shell = (NgShellClass)lc;

	/*
	 * Override go class definition of manager class.
	 */

	shell->go_class.dialog = topLevelShellWidgetClass;
	shell->go_class.dialog = xcbApplicationShellWidgetClass;
	shell->go_class.manager = xmMessageBoxWidgetClass;

	return NhlNOERROR;
}

static NhlErrorTypes
ShellInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	NgShellPart		*np = &((NgShell)new)->shell;

	np->focus = False;

	return NhlNOERROR;
}

static NhlErrorTypes
ShellDestroy
(
	NhlLayer	l
)
{

	return NhlNOERROR;
}

static void
Grab
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	NgShell	sh = (NgShell)udata;

	if(event->type != MapNotify)
		return;
	if (event->xmap.window != XtWindow(sh->go.shell))
		return;
	if (! sh->shell.do_focus_grab)
		return;
	if (sh->shell.focus)
		return;

	NgAppGrabFocus(sh->go.appmgr,sh->base.id);
	sh->shell.focus = True;

	return;
}

static void
Release
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgShell	sh = (NgShell)udata;

	if(!sh->shell.focus)
		return;
	NgAppReleaseFocus(sh->go.appmgr,sh->base.id);
	sh->shell.focus = False;

	return;
}

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

static NhlBoolean
ShellCreateWin
(
	NgGO	go
)
{
	NgShellPart	*np = &((NgShell)go)->shell;

/*
 * Set up template dialog manager
 */
        XtVaSetValues(go->go.manager,
                      XmNdialogType,XmDIALOG_TEMPLATE,
                      NULL);
	XtAddCallback(go->go.manager,
                      XmNcancelCallback,_NgGOPopdownCB,(XtPointer)go->base.id);
 	XtUnmanageChild(XmMessageBoxGetChild(go->go.manager,
                                             XmDIALOG_HELP_BUTTON));
 	XtUnmanageChild(XmMessageBoxGetChild(go->go.manager,
                                             XmDIALOG_SYMBOL_LABEL));

	XtAddEventHandler(go->go.shell,StructureNotifyMask,False,Grab,go);
	XtAddCallback(go->go.shell,XmNpopdownCallback,Release,go);

	if (np->cfunc) 
		(*np->cfunc)(go->base.id,np->cfunc_data);

	return True;
}

static NhlBoolean
ShellCreateWinHook
(
	NgGO	go
)
{

	return True;
}
