/*
 *      $Id: load.c,v 1.4 1997-09-17 16:41:08 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		load.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct 14 16:32:09 MDT 1996
 *
 *	Description:	
 */
#include <ncarg/ngo/loadP.h>
#include <ncarg/ngo/nclstate.h>

#include <Xm/Xm.h>
#include <Xm/SelectioB.h>
#include <Xm/FileSB.h>

#define	Oset(field)	NhlOffset(NgLoadRec,load.field)
static NhlResource resources[] = {
	{"no.res","no.res",NhlTInteger,sizeof(int),
		Oset(foo),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_NOACCESS,NULL},
};
#undef	Oset

static NhlErrorTypes LoadClassPartInitialize(
	NhlClass	lc
);

static NhlErrorTypes LoadInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlBoolean LoadCreateWin(
	NgGO	go
);

NgLoadClassRec NgloadClassRec = {
	{
/* class_name		*/	"loadClass",
/* nrm_class		*/	NrmNULLQUARK,
/* layer_size		*/	sizeof(NgLoadRec),
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

/* class_part_initialize*/	LoadClassPartInitialize,
/* class_initialize	*/	NULL,
/* layer_initialize	*/	LoadInitialize,
/* layer_set_values	*/	NULL,
/* layer_set_values_hook*/	NULL,
/* layer_get_values	*/	NULL,
/* layer_reparent	*/	NULL,
/* layer_destroy	*/	NULL,

	},
	{
/* dialog		*/	NULL,
/* toplevel		*/	NULL,
/* manager		*/	NULL,

/* top_win_chain	*/	False,
/* create_win		*/	LoadCreateWin,
/* create_win_hook	*/	NULL,
/* create_win_hook	*/	_NgGOInheritClose,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgloadClass = (NhlClass)&NgloadClassRec;

static NhlErrorTypes
LoadClassPartInitialize
(
	NhlClass	lc
)
{
	NgLoadClass	load = (NgLoadClass)lc;

	/*
	 * Over-ride go class definition of manager class.
	 */
	load->go_class.manager = xmFileSelectionBoxWidgetClass;

	return NhlNOERROR;
}

static NhlErrorTypes
LoadInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char		func[] = "LoadInitialize";
	NgLoad		ncl = (NgLoad)new;
	NgLoadPart	*np = &((NgLoad)new)->load;
	NgLoadPart	*rp = &((NgLoad)req)->load;


	return NhlNOERROR;
}

static void
LoadScriptOkCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmFileSelectionBoxCallbackStruct	*cbs =
				(XmFileSelectionBoxCallbackStruct*)cbdata;
	char	func[] = "LoadScriptOkCB";
	NgLoad	l = (NgLoad)udata;
	int	nsid = NhlDEFAULT_APP;
	char	line[512];
	char	*dirstr;

	if(!XmStringGetLtoR(cbs->value,XmFONTLIST_DEFAULT_TAG,&dirstr)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Can't get filename",func));
		return;
	}
	if(strlen(dirstr) > (sizeof(line) - 9)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:filename too long:%s",func,
								dirstr));
		XtFree(dirstr);
		return;
	}
	sprintf(line,"load \"%s\"\n",dirstr);
	XtFree(dirstr);

	NgGOPopdown(l->base.id);

	NhlVAGetValues(l->go.appmgr,
		NgNappNclState,	&nsid,
		NULL);

	if(!NhlIsClass(nsid,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid nclstate id",func));
		return;
	}
	(void)NgNclSubmitLine(nsid,line,True);

	return;
}

static NhlBoolean
LoadCreateWin
(
	NgGO	go
)
{
	char		func[]="LoadCreateWin";
	NgLoadPart	*np = &((NgLoad)go)->load;

	XtVaSetValues(go->go.manager,
		XmNresizePolicy,	XmRESIZE_GROW,
		NULL);

	XtUnmanageChild(XmSelectionBoxGetChild(go->go.manager,
							XmDIALOG_HELP_BUTTON));
	XtAddCallback(go->go.manager,XmNokCallback,LoadScriptOkCB,
								(XtPointer)go);
	XtAddCallback(go->go.manager,XmNcancelCallback,_NgGOPopdownCB,
							(XtPointer)go->base.id);

	return True;
}
