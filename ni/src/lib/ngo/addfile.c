/*
 *      $Id: addfile.c,v 1.1 1996-11-24 22:27:33 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		addfile.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct 14 16:32:09 MDT 1996
 *
 *	Description:	
 */
#include <ncarg/ngo/addfileP.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/xutil.h>

#include <Xm/Xm.h>
#include <Xm/MenuShell.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>
#include <Xm/SelectioB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>

#define	Oset(field)	NhlOffset(NgAddFileRec,addfile.field)
static NhlResource resources[] = {
	{"no.res","no.res",NhlTInteger,sizeof(int),
		Oset(foo),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_NOACCESS,NULL},
};
#undef	Oset

static NhlErrorTypes AddFileClassPartInitialize(
	NhlClass	lc
);

static NhlErrorTypes AddFileInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlBoolean AddFileCreateWin(
	NgGO	go
);

NgAddFileClassRec NgaddFileClassRec = {
	{
/* class_name		*/	"addFileClass",
/* nrm_class		*/	NrmNULLQUARK,
/* layer_size		*/	sizeof(NgAddFileRec),
/* class_inited		*/	False,
/* superclass		*/	(NhlClass)&NggOClassRec,
/* cvt_table		*/	NULL,

/* layer_resources	*/	resources,
/* num_resources	*/	NhlNumber(resources),
/* all_resources	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,

/* class_part_initialize*/	AddFileClassPartInitialize,
/* class_initialize	*/	NULL,
/* layer_initialize	*/	AddFileInitialize,
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
/* create_win		*/	AddFileCreateWin,
/* create_win_hook	*/	NULL,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgaddFileClass = (NhlClass)&NgaddFileClassRec;

static NhlErrorTypes
AddFileClassPartInitialize
(
	NhlClass	lc
)
{
	NgAddFileClass	addfile = (NgAddFileClass)lc;

	/*
	 * Over-ride go class definition of manager class.
	 */
	addfile->go_class.manager = xmFileSelectionBoxWidgetClass;

	return NhlNOERROR;
}

static NhlErrorTypes
AddFileInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char		func[] = "AddFileInitialize";
	NgAddFile		ncl = (NgAddFile)new;
	NgAddFilePart	*np = &((NgAddFile)new)->addfile;
	NgAddFilePart	*rp = &((NgAddFile)req)->addfile;

	np->vname = np->fname = np->optmenu = NULL;

	return NhlNOERROR;
}

static void
AddFileScriptOkCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmFileSelectionBoxCallbackStruct	*cbs =
				(XmFileSelectionBoxCallbackStruct*)cbdata;
	char		func[] = "AddFileScriptOkCB";
	NgAddFile	l = (NgAddFile)udata;
	NgAddFilePart	*np = &l->addfile;
	int		nsid = NhlDEFAULT_APP;
	char		line[512];
	char		*dirstr,*vname;
	Widget		rtype;
	char		*rw = "r";

	if(!XmStringGetLtoR(cbs->value,XmFONTLIST_DEFAULT_TAG,&dirstr)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Can't get filename",func));
		return;
	}
	vname = XmTextFieldGetString(l->addfile.vname);
	if(!vname || (vname[0] == '\0')){
		XtFree(vname);
		XtFree(dirstr);
		return;
	}

	/*
	 * Make sure dirstr and vname aren't so long they blow out the
	 * stack memory.
	 */
	if((strlen(dirstr)+strlen(vname)) > (sizeof(line) - 19)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"%s:variable name and filename too long:%s:%s",func,
								vname,dirstr));
		XtFree(vname);
		XtFree(dirstr);
		return;
	}

	XtVaGetValues(np->optmenu,
		XmNmenuHistory,	&rtype,
		NULL);

	if(rtype)
		rw = XtName(rtype);

	sprintf(line,"%s = addfile(\"%s\",\"%s\")\n",vname,dirstr,rw);
	XtFree(vname);
	XtFree(dirstr);

	NgGOPopdown(l->base.id);

	NhlVAGetValues(l->go.appmgr,
		NgNappNclState,	&nsid,
		NULL);

	if(!NhlIsClass(nsid,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid nclstate id",func));
		return;
	}
	(void)NgNclSubmitBlock(nsid,line);

	return;
}

static void
AddFileSelectFileCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmListCallbackStruct	*cbs = (XmListCallbackStruct*)cbdata;
	NgAddFile		l = (NgAddFile)udata;
	NgAddFilePart		*np = &l->addfile;
	char			bname[_NhlMAXFNAMELEN];
	char			cmd[_NhlMAXFNAMELEN];
	char			*item = NULL;
	char			*ptr,*ptr2;
	XmString		cmdstr;
	size_t			len;

	XmStringGetLtoR(cbs->item,XmFONTLIST_DEFAULT_TAG,&item);

	if(item)
		ptr = strrchr(item,'/');
	if(ptr)
		ptr++;
	if(ptr){
		/* Prefix numeric filenames with "FVAR" */
		if(!isalpha(*ptr))
			strcpy(bname,"FVAR");
		else
			bname[0]='\0';
		ptr2 = &bname[strlen(bname)];
		len = strcspn(ptr,".");
		strncat(ptr2,ptr,len);
		ptr2[len]='\0';
	}
	else
		bname[0] = '\0';
	XmTextFieldSetString(np->vname,bname);

	sprintf(cmd," = addfile(\"%s\",\"",ptr);
	cmdstr = NgXAppCreateXmString(l->go.appmgr,cmd);
	XtVaSetValues(np->fname,
		XmNlabelString,	cmdstr,
		NULL);
	NgXAppFreeXmString(l->go.appmgr,cmdstr);
	XtFree(item);
		
	return;
}

static NhlBoolean
AddFileCreateWin
(
	NgGO	go
)
{
	char		func[]="AddFileCreateWin";
	NgAddFilePart	*np = &((NgAddFile)go)->addfile;
	Widget		form,menu,menush;
	XmString	dirmask;

	XtVaSetValues(go->go.manager,
		XmNresizePolicy,	XmRESIZE_GROW,
		XmNfileSearchProc,	NgXFileSearchProc,
		NULL);

	XtVaGetValues(go->go.manager,
		XmNdirMask,	&dirmask,
		NULL);
	XmFileSelectionDoSearch(go->go.manager,dirmask);
	XmStringFree(dirmask);

	XtAddCallback(XmFileSelectionBoxGetChild(go->go.manager,XmDIALOG_LIST),
		XmNbrowseSelectionCallback,AddFileSelectFileCB,(XtPointer)go);

	XtUnmanageChild(XmFileSelectionBoxGetChild(go->go.manager,
						XmDIALOG_SELECTION_LABEL));
	XtUnmanageChild(XmFileSelectionBoxGetChild(go->go.manager,
						XmDIALOG_TEXT));
	XtUnmanageChild(XmFileSelectionBoxGetChild(go->go.manager,
							XmDIALOG_HELP_BUTTON));
	form = XtVaCreateManagedWidget("vnameform",xmFormWidgetClass,
								go->go.manager,
		NULL);

	np->vname = XtVaCreateManagedWidget("vname",xmTextFieldWidgetClass,form,
		NULL);

	np->fname = XtVaCreateManagedWidget("midtxt",xmLabelWidgetClass,form,
		XmNleftWidget,		np->vname,
		XmNleftAttachment,	XmATTACH_WIDGET,
		NULL);

	menush = XtVaCreatePopupShell("rwMenush",xmMenuShellWidgetClass,form,
		XmNwidth,	5,
		XmNheight,	5,
		XmNallowShellResize,	True,
		XmNoverrideRedirect,	True,
		NULL);

	menu = XtVaCreateWidget("rwMenu",xmRowColumnWidgetClass,menush,
		XmNrowColumnType,	XmMENU_PULLDOWN,
		NULL);

	XtVaCreateManagedWidget("r",xmPushButtonGadgetClass,menu,
		XmNalignment,	XmALIGNMENT_CENTER,
		NULL);
	XtVaCreateManagedWidget("w",xmPushButtonGadgetClass,menu,
		XmNalignment,	XmALIGNMENT_CENTER,
		NULL);

	np->optmenu = XtVaCreateManagedWidget("rwoptMenu",
						xmRowColumnWidgetClass,form,
		XmNleftWidget,		np->fname,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNspacing,		0,
		XmNrowColumnType,	XmMENU_OPTION,
		XmNsubMenuId,		menu,
		NULL);

	menu = XmOptionLabelGadget(np->optmenu);
	if(menu)
		XtUnmanageChild(menu);

	XtVaCreateManagedWidget("endtxt",xmLabelWidgetClass,form,
		XmNleftWidget,		np->optmenu,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNalignment,		XmALIGNMENT_BEGINNING,
		NULL);

	XtAddCallback(go->go.manager,XmNokCallback,AddFileScriptOkCB,
								(XtPointer)go);
	XtAddCallback(go->go.manager,XmNcancelCallback,_NgGOPopdownCB,
							(XtPointer)go->base.id);

	return True;
}
