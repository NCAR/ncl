/*
 *      $Id: ncledit.c,v 1.1 1996-10-10 18:55:22 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ncledit.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 19 13:29:08 MDT 1996
 *
 *	Description:	This class isn't actually used yet.  It is a place
 *			holder so that as much common functionality can
 *			be pulled up from xncledit as possible - when
 *			we are actually forced to port to another arch.
 */
#include <ncarg/ngo/ncleditP.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/xutil.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include <Xm/MenuShell.h>

static char RSTSTR[] = "You must finish entering block, or press reset";
static char ERRSTR[] = "Error!";

#define	Oset(field)	NhlOffset(NgNclEditRec,ncledit.field)
static NhlResource resources[] = {
	{NgNNclState,NgCNclState,NhlTInteger,sizeof(int),
		Oset(nsid),NhlTImmediate,_NhlUSET((NhlPointer)NhlDEFAULT_APP),
		_NhlRES_CONLY,NULL},
	{NgNneResetWarning,NgCneResetWarning,NhlTString,sizeof(NhlString),
		Oset(rmsg),NhlTImmediate,_NhlUSET((NhlPointer)RSTSTR),
		_NhlRES_RONLY,NULL},
	{NgNneErrorString,NgCneErrorString,NhlTString,sizeof(NhlString),
		Oset(emsg),NhlTImmediate,_NhlUSET((NhlPointer)ERRSTR),
		_NhlRES_RONLY,NULL},
};
#undef	Oset

static NhlErrorTypes NEInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes NEDestroy(
	NhlLayer	l
);

static NhlBoolean NECreateWin(
	NgGO	go
);

NgNclEditClassRec NgnclEditClassRec = {
	{
/* class_name		*/	"nclEditClass",
/* nrm_class		*/	NrmNULLQUARK,
/* layer_size		*/	sizeof(NgNclEditRec),
/* class_inited		*/	False,
/* superclass		*/	(NhlClass)&NggOClassRec,
/* cvt_table		*/	NULL,

/* layer_resources	*/	resources,
/* num_resources	*/	NhlNumber(resources),
/* all_resources	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,

/* class_part_initialize*/	NULL,
/* class_initialize	*/	NULL,
/* layer_initialize	*/	NEInitialize,
/* layer_set_values	*/	NULL,
/* layer_set_values_hook*/	NULL,
/* layer_get_values	*/	NULL,
/* layer_reparent	*/	NULL,
/* layer_destroy	*/	NEDestroy,

	},
	{
/* dialog		*/	NULL,
/* toplevel		*/	NULL,
/* manager		*/	NULL,

/* top_win_chain	*/	False,
/* create_win		*/	NECreateWin,
/* create_win_hook	*/	NULL,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgnclEditClass = (NhlClass)&NgnclEditClassRec;

static NhlErrorTypes
NEInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char		func[] = "NEInitialize";
	NgNclEdit	ncl = (NgNclEdit)new;
	NgNclEditPart	*np = &((NgNclEdit)new)->ncledit;
	NgNclEditPart	*rp = &((NgNclEdit)req)->ncledit;

	if(!NhlIsClass(np->nsid,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid nclstate resource",
									func));
		return NhlFATAL;
	}

	np->rstr = NgXAppCreateXmString(ncl->go.appmgr,np->rmsg);
	np->rmsg = NULL;
	np->estr = NgXAppCreateXmString(ncl->go.appmgr,np->emsg);
	np->emsg = NULL;
	np->nstr = NgXAppCreateXmString(ncl->go.appmgr,"");
	np->cstr = np->nstr;

	np->edit = False;
	np->my_cmd = False;
	np->mem = NULL;
	np->more_cmds = NULL;
	np->prompt_pos = np->submit_pos = np->reset_pos = 0;
	np->line = 0;
	np->my_focus = False;

	return NhlNOERROR;
}

static NhlErrorTypes
NEDestroy
(
	NhlLayer	l
)
{
	NgNclEdit	ncl = (NgNclEdit)l;
	NgNclEditPart	*np = &((NgNclEdit)l)->ncledit;

	/*
	 * Don't destroy widgets!  NgGO takes care of that.
	 */
	NgXAppFreeXmString(ncl->go.appmgr,np->rstr);
	np->rstr = NULL;
	NgXAppFreeXmString(ncl->go.appmgr,np->estr);
	np->estr = NULL;
	NgXAppFreeXmString(ncl->go.appmgr,np->nstr);
	np->nstr = NULL;
	np->cstr = NULL;

	if(np->mem){
		NhlFree(np->mem);
		np->mem = NULL;
		np->more_cmds = NULL;
	}

	return NhlNOERROR;
}

static void Popup
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmProcessTraversal((Widget)udata,XmTRAVERSE_CURRENT);
}

static void
CheckInput
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgNclEdit	ncl = (NgNclEdit)udata;
	NgNclEditPart	*np = &ncl->ncledit;
	XmTextVerifyPtr	ver = (XmTextVerifyPtr)cbdata;
	char		*nl,*ptr;
	int		oset;

	if(np->edit)
		return;

	if(ver->reason != XmCR_MODIFYING_TEXT_VALUE)
		return;

	/*
	 * Don't allow things before submit_pos to be modified.
	 */
	if(ver->startPos < np->submit_pos){
		if(ver->endPos < np->submit_pos){
			/*
			 * The insert cursor is completely before the
			 * submit position.  Just take the text being
			 * typed or pasted and put it at the end.
			 */
			ver->startPos = ver->endPos = XmTextGetLastPosition(w);
		}
		else{
			/*
			 * The endPos was in the valid edit area, so just
			 * move the startPos up to submit position.
			 * If start == end and the text has 0 length,
			 * then the user is trying to backspace over the
			 * prompt, so I disallow that.
			 */
			ver->startPos = np->submit_pos;
			if(ver->startPos == ver->endPos &&
							ver->text->length == 0){
				ver->doit = False;
				return;
			}
		}
	}

	if(ver->text->length <= 1)
		return;

	/*
	 * Find first unescaped newline.
	 */
	nl = strchr(ver->text->ptr,'\n');
	while(nl && nl != ver->text->ptr && *(nl - 1) == '\\')
		nl = strchr(++nl,'\n');

	if(!nl)
		return;

	np->mem = NhlMalloc(sizeof(char)*(ver->text->length + 2));
	if(!np->mem){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		ver->doit = False;
		return;
	}
	/*
	 * take first line of input and send it through
	 */
	oset = nl - ver->text->ptr + 1;
	strncpy(np->mem,ver->text->ptr,oset);
	np->mem[oset] = '\0';
	ver->text->ptr = np->mem;
	ver->text->length = oset;

	/*
	 * take remaining input and put it in ncledit structure for processing
	 * during activate() action.
	 */
	nl++;oset++;
	strcpy(&np->mem[oset],nl);
	np->more_cmds = &np->mem[oset];

	return;
}

static void
ActivateCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	char		func[] = "ActivateCB";
	NgNclEdit	ncl = (NgNclEdit)udata;
	NgNclEditPart	*np = &ncl->ncledit;
	char		*cmd_stack[128];
	char		*cmd_buff;
	char		*nl;
	char		*cmd;
	int		len;

	if(np->edit)
		return;

	len = XmTextGetLastPosition(w) - np->submit_pos;
	if(len < 1) return;

	cmd_buff = _NgStackAlloc(len + 1,cmd_stack);
	if(XmTextGetSubstring(w,np->submit_pos,len,len+1,cmd_buff)
							!= XmCOPY_SUCCEEDED){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to retrive string.",
									func));
		goto FREE_MEM;
	}

	/*
	 * Find first unescaped newline.
	 */
	nl = strchr(cmd_buff,'\n');
	while(nl && nl != cmd_buff && *(nl - 1) == '\\')
		nl = strchr(++nl,'\n');

	if(!nl){
		if(np->mem){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
						"%s:NclEdit BAD STATE!",func));
		}
		goto FREE_MEM;
	}

	if(nl != &cmd_buff[len-1]){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:NclEdit BAD STATE!",func));
		goto FREE_MEM;
	}

	np->edit = True;
	np->my_cmd = True;

	if(!NgNclSubmitLine(np->nsid,cmd_buff,False))
		goto FREE_MEM;

	np->my_cmd = False;
	cmd = np->more_cmds;

	while(cmd && *cmd){
		/*
		 * If there is a terminating newline for cmd, then submit
		 * it, otherwise, just paste it on the end of submittext.
		 */
		nl = strchr(cmd,'\n');
		while(nl && nl > cmd && *(nl-1) == '\\')
			nl = strchr(++nl,'\n');

		if(!nl){
			XmTextInsert(w,XmTextGetLastPosition(w),cmd);
			break;
		}

		if(!NgNclSubmitLine(np->nsid,cmd,False))
			break;

		cmd = nl + 1;
	}


FREE_MEM:
	np->edit = False;
	np->my_cmd = False;
	_NgStackFree(cmd_buff,cmd_stack);
	if(np->mem) NhlFree(np->mem);
	np->mem = NULL;
	np->more_cmds = NULL;

	return;
}


static void
SubmitCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgNclEdit	ncl = (NgNclEdit)udata.ptrval;
	NgNclEditPart	*np = &ncl->ncledit;
	NhlBoolean	edit;

	if(np->my_cmd) return;
	edit = np->edit;
	np->edit = True;

	XmTextReplace(np->text,np->submit_pos,XmTextGetLastPosition(np->text),
								cbdata.strval);
	np->edit = edit;

	return;
}

static void
OutputCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgNclEdit	ncl = (NgNclEdit)udata.ptrval;
	NgNclEditPart	*np = &ncl->ncledit;
	NhlBoolean	edit;

	edit = np->edit;
	np->edit = True;

	XmTextInsert(np->text,XmTextGetLastPosition(np->text),cbdata.strval);
	np->edit = edit;

	return;
}

static void
ErrOutputCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgNclEdit	ncl = (NgNclEdit)udata.ptrval;
	NgNclEditPart	*np = &ncl->ncledit;
	NhlBoolean	edit;
	char		buffer[NhlERRMAXMSGLEN+1];
	int		len;

	NhlErrSPrintMsg(buffer,(NhlErrMsg*)cbdata.ptrval);
	len = strlen(buffer);
	if(len < 1)
		return;
	if(buffer[len-1] != '\n')
		strcat(buffer,"\n");

	edit = np->edit;
	np->edit = True;
	XmTextInsert(np->text,XmTextGetLastPosition(np->text),buffer);
	np->edit = edit;

	return;
}

static void
PromptCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	char			func[] = "PromptCB";
	NgNclEdit		ncl = (NgNclEdit)udata.ptrval;
	NgNclEditPart		*np = &ncl->ncledit;
	NhlBoolean		edit;
	XmTextPosition		pos = XmTextGetLastPosition(np->text);
	char			buff[20];
	NgNclPromptCBData	prompt = (NgNclPromptCBData)cbdata.ptrval;
	XmString		msg;

	edit = np->edit;
	np->edit = True;

	if(XmTextGetSubstring(np->text,pos-1,1,2,buff) != XmCOPY_SUCCEEDED){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Can't get text.",func));
	}
	else if(*buff != '\n')
		XmTextInsert(np->text,pos++,"\n");

	np->prompt_pos = pos;
	if(prompt->istate){
		np->reset_pos = pos;
		if(np->my_focus){
			XtSetSensitive(np->reset,False);
			NgAppReleaseFocus(ncl->go.appmgr,ncl->base.id);
			np->my_focus = False;
		}
		np->cstr = np->nstr;
	}
	else{
		if(edit && !np->my_focus){
			XtSetSensitive(np->reset,True);
			NgAppGrabFocus(ncl->go.appmgr,ncl->base.id);
			np->my_focus = True;
			np->cstr = np->rstr;
		}
	}

	msg = np->cstr;

	if(prompt->err){
		XBell(ncl->go.x->dpy,0);
		NgGOPopup(ncl->base.id);
		msg = np->estr;
	}

	XtVaSetValues(np->ilabel,
		XmNlabelString,	msg,
		NULL);

	sprintf(buff,"ncl %d> ",prompt->line);
	XmTextInsert(np->text,pos,buff);
	np->submit_pos = pos + strlen(buff);
	np->line = prompt->line;

	np->edit = edit;

	return;
}

static void
ResetCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgNclEdit		ncl = (NgNclEdit)udata.ptrval;
	NgNclEditPart		*np = &ncl->ncledit;

	XmTextSetHighlight(np->text,np->reset_pos,np->prompt_pos,
						XmHIGHLIGHT_SECONDARY_SELECTED);
	np->reset_pos = np->prompt_pos;
	if(np->my_focus){
		XtSetSensitive(np->reset,False);
		NgAppReleaseFocus(ncl->go.appmgr,ncl->base.id);
		np->cstr = np->nstr;
		XtVaSetValues(np->ilabel,
			XmNlabelString,	np->cstr,
			NULL);
		np->my_focus = False;
	}

	return;
}

static void ResetButtonCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgNclReset((int)udata);
}

static NhlBoolean
NECreateWin
(
	NgGO	go
)
{
	char		func[]="NECreateWin";
	NgNclEditPart	*np = &((NgNclEdit)go)->ncledit;
	Widget		menubar,menush,fmenu,fmenush,emenu,emenush;
	Widget		cmenu,cmenush;
	Widget		hmenu,hmenush;
	Widget		file,edit,config,help;
	Widget		close,quit;
	Widget		pane,sform,sform1;
	Widget		slabel;
	Widget		hoframe,holabel;
	Widget		vframe,vlabel;
	Widget		fframe,flabel;
	Widget		fuframe,fulabel;
	Widget		oform,olabel;
	Widget		iform,scroll;
	Dimension	width;
	Arg		args[10];
	int		nargs;
	NhlLayer	nstate;
	NhlArgVal	dummy,udata;
	char		buff[20];

	menubar =XtVaCreateManagedWidget("menubar",xmRowColumnWidgetClass,
								go->go.manager,
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

	cmenu = XtVaCreateWidget("cmenu",xmRowColumnWidgetClass,menush,
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

	config = XtVaCreateManagedWidget("config",xmCascadeButtonGadgetClass,
									menubar,
		XmNsubMenuId,	cmenu,
		NULL);

	help = XtVaCreateManagedWidget("help",xmCascadeButtonGadgetClass,
									menubar,
		XmNsubMenuId,	hmenu,
		NULL);

	XtVaSetValues(menubar,
		XmNmenuHelpWidget,	help,
		NULL);

	close = XtVaCreateManagedWidget("closeWindow",
					xmPushButtonGadgetClass,fmenu,
		NULL);
	XtAddCallback(close,XmNactivateCallback,_NgGODefActionCB,NULL);

	quit = XtVaCreateManagedWidget("quitApplication",
					xmPushButtonGadgetClass,fmenu,
		NULL);
	XtAddCallback(quit,XmNactivateCallback,_NgGODefActionCB,NULL);

	XtManageChild(fmenu);
	XtManageChild(emenu);
	XtManageChild(cmenu);
	XtManageChild(hmenu);

	pane = XtVaCreateManagedWidget("pane",xmPanedWindowWidgetClass,
								go->go.manager,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		menubar,
		NULL);

	sform = XtVaCreateManagedWidget("sform",xmFormWidgetClass,pane,
		NULL);

	slabel = XtVaCreateManagedWidget("slabel",xmLabelWidgetClass,sform,
		NULL);

	hoframe = XtVaCreateManagedWidget("hoframe",xmFrameWidgetClass,sform,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		slabel,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	25,
		NULL);

	holabel = XtVaCreateManagedWidget("holabel",xmLabelWidgetClass,hoframe,
		XmNchildType,			XmFRAME_TITLE_CHILD,
		NULL);

	nargs = 0;
	np->holist = XmCreateScrolledList(hoframe,"holist",args,nargs);
	XtManageChild(np->holist);

	(void)NgXListManage(np->nsid,np->holist,NgNclHLUVAR,NULL,NULL);

	vframe = XtVaCreateManagedWidget("vframe",xmFrameWidgetClass,sform,
		XmNtopWidget,		slabel,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNleftAttachment,	XmATTACH_POSITION,
		XmNleftPosition,	25,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	50,
		NULL);

	vlabel = XtVaCreateManagedWidget("vlabel",xmLabelWidgetClass,vframe,
		XmNchildType,			XmFRAME_TITLE_CHILD,
		NULL);

	np->vlist = XmCreateScrolledList(vframe,"vlist",NULL,0);
	XtManageChild(np->vlist);

	(void)NgXListManage(np->nsid,np->vlist,NgNclVAR,NULL,NULL);

	fframe = XtVaCreateManagedWidget("fframe",xmFrameWidgetClass,sform,
		XmNtopWidget,		slabel,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNleftAttachment,	XmATTACH_POSITION,
		XmNleftPosition,	50,
		XmNrightAttachment,	XmATTACH_POSITION,
		XmNrightPosition,	75,
		NULL);

	flabel = XtVaCreateManagedWidget("flabel",xmLabelWidgetClass,fframe,
		XmNchildType,			XmFRAME_TITLE_CHILD,
		NULL);

	np->flist = XmCreateScrolledList(fframe,"flist",NULL,0);
	XtManageChild(np->flist);

	(void)NgXListManage(np->nsid,np->flist,NgNclFILEVAR,NULL,NULL);

	fuframe = XtVaCreateManagedWidget("fuframe",xmFrameWidgetClass,sform,
		XmNtopWidget,		slabel,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNleftAttachment,	XmATTACH_POSITION,
		XmNleftPosition,	75,
		NULL);

	fulabel = XtVaCreateManagedWidget("fulabel",xmLabelWidgetClass,fuframe,
		XmNchildType,			XmFRAME_TITLE_CHILD,
		NULL);

	nargs = 0;
	np->fulist = XmCreateScrolledList(fuframe,"fulist",NULL,0);
	XtManageChild(np->fulist);

	(void)NgXListManage(np->nsid,np->fulist,NgNclFUNC,NULL,NULL);

	iform = XtVaCreateManagedWidget("iform",xmFormWidgetClass,pane,
		NULL);

	np->reset = XtVaCreateManagedWidget("reset",xmPushButtonWidgetClass,
									iform,
		NULL);
	XtAddCallback(np->reset,XmNactivateCallback,ResetButtonCB,
							(XtPointer)np->nsid);

	np->ilabel = XtVaCreateManagedWidget("ilabel",xmLabelWidgetClass,iform,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		np->reset,
		XmNtopAttachment,	XmATTACH_OPPOSITE_WIDGET,
		XmNtopWidget,		np->reset,
		NULL);

	scroll = XtVaCreateManagedWidget("scroll",xmScrolledWindowWidgetClass,
									iform,
		XmNbottomAttachment,	XmATTACH_WIDGET,
		XmNbottomWidget,	np->reset,
		NULL);

	np->text = XtVaCreateManagedWidget("nclcmd",xmTextWidgetClass,scroll,
		XmNeditMode,	XmMULTI_LINE_EDIT,
		NULL);
	XtAddCallback(np->text,XmNmodifyVerifyCallback,CheckInput,
								(XtPointer)go);
	XtAddCallback(np->text,XmNactivateCallback,ActivateCB,(XtPointer)go);

	XtAddCallback(go->go.shell,XmNpopupCallback,Popup,(XtPointer)np->text);

#ifdef	DEBUG
	memset(&dummy,0,sizeof(NhlArgVal));
	memset(&udata,0,sizeof(NhlArgVal));
#endif
	udata.ptrval = go;

	nstate = _NhlGetLayer(np->nsid);
	np->submitcb = _NhlAddObjCallback(nstate,NgCBnsSubmit,dummy,SubmitCB,
									udata);
	np->promptcb = _NhlAddObjCallback(nstate,NgCBnsPrompt,dummy,PromptCB,
									udata);
	np->resetcb = _NhlAddObjCallback(nstate,NgCBnsReset,dummy,ResetCB,
									udata);
	np->outputcb = _NhlAddObjCallback(nstate,NgCBnsOutput,dummy,OutputCB,
									udata);
	np->erroutputcb = _NhlAddObjCallback(nstate,NgCBnsErrOutput,dummy,
							ErrOutputCB,udata);

	np->line = NgNclCurrentLine(np->nsid);
	sprintf(buff,"ncl %d> ",np->line);
	np->edit = True;
	XmTextReplace(np->text,0,XmTextGetLastPosition(np->text),buff);
	np->edit = False;
	np->reset_pos = 0;
	np->submit_pos = np->reset_pos + strlen(buff);

	return True;
}
