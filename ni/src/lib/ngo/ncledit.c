/*
 *      $Id: ncledit.c,v 1.7 1997-06-04 18:08:29 dbrown Exp $
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
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

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
	char			func[] = "NEInitialize";
	NgNclEdit		ncl = (NgNclEdit)new;
	NgNclEditPart		*np = &((NgNclEdit)new)->ncledit;
	NgNclEditPart		*rp = &((NgNclEdit)req)->ncledit;

	np->nsid = NhlDEFAULT_APP;
	NhlVAGetValues(ncl->go.appmgr,
		NgNappNclState,	&np->nsid,
		NULL);
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
	np->print = False;
	np->do_output = False;
	np->more_cmds = NULL;
	np->submit_pos = np->reset_pos = 0;
	np->line = 0;
	np->my_focus = False;

	np->prompt_win = None;
	np->high_gc = NULL;
	np->high_drawn = False;

	NgAppAddNclEditor(ncl->go.appmgr,new->base.id);

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

	NgAppRemoveNclEditor(ncl->go.appmgr,l->base.id);

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

	if(np->more_cmds){
		NhlFree(np->more_cmds);
	}

	if(np->prompt_win != None)
		XDestroyWindow(ncl->go.x->dpy,np->prompt_win);

	if(np->high_gc != None)
		XFreeGC(ncl->go.x->dpy,np->high_gc);

	return NhlNOERROR;
}

static void
TextFocusCB
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
	int		oset,len;
	int		max,ssize;

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
	XmTextShowPosition(w,ver->startPos);
	XmTextShowPosition(np->prompt_text,
					XmTextGetLastPosition(np->prompt_text));
	/*
	 * value of scrollbar doesn't get updated properly, so we need to use
	 * the max value and slider size to tell the prompt_text the correct
	 * slider value for the bottom of the window - which should be where
	 * it is after the ShowPosition call from above.
	 */
	XtVaGetValues(np->vsbar,
		XmNmaximum,	&max,
		XmNsliderSize,	&ssize,
		NULL);
	XtVaSetValues(np->prompt_text,
		XmNuserData,	max - ssize,
		NULL);

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

	/*
	 * take first line of input and send it through
	 */
	oset = nl - ver->text->ptr + 1;
	ptr = XtMalloc(sizeof(char)*(oset + 1));
	if(!ptr){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		ver->doit = False;
		return;
	}
	strncpy(ptr,ver->text->ptr,oset);
	ptr[oset] = '\0';
	ver->text->ptr = ptr;
	len = ver->text->length - oset;
	ver->text->length = oset;

	/*
	 * take remaining input and put it in ncledit structure for processing
	 * during activate() action.
	 */
	np->more_cmds = NhlMalloc(sizeof(char)*(len + 1));
	if(!np->more_cmds){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}
	nl++;
	strcpy(np->more_cmds,nl);

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
	np->edit = True;

	len = XmTextGetLastPosition(w) - np->submit_pos;
	if(len < 1){
		np->edit = False;
		return;
	}

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
		if(np->more_cmds){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
						"%s:NclEdit BAD STATE!",func));
		}
		goto FREE_MEM;
	}

	if(nl != &cmd_buff[len-1]){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:NclEdit BAD STATE!",func));
		goto FREE_MEM;
	}

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
	_NgStackFree(cmd_buff,cmd_stack);
	np->edit = False;
	np->my_cmd = False;
	if(np->more_cmds) NhlFree(np->more_cmds);
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
	int		val;

	if(!np->my_cmd){
		edit = np->edit;
		np->edit = True;

		XmTextReplace(np->text,np->submit_pos,
				XmTextGetLastPosition(np->text),cbdata.strval);
		np->edit = edit;
	}

	np->do_output = True;
	XmTextInsert(np->prompt_text,XmTextGetLastPosition(np->prompt_text),
									"\n");
	XtVaGetValues(np->vsbar,
		XmNvalue,	&val,
		NULL);
	XtVaSetValues(np->prompt_text,
		XmNuserData,	val,
		NULL);

	return;
}

static void
UpdateDisplay
(
	Display	*dpy
)
{
	XEvent	ev;

	XSync(dpy,False);

	while(XCheckMaskEvent(dpy,ExposureMask,&ev))
		XtDispatchEvent(&ev);

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
	char		*nl;
	int		num=0;
	XmTextPosition	tp;
	NhlBoolean	edit;

	if(!np->do_output)
		return;

	edit = np->edit;
	np->edit = True;

	np->print = True;
	XmTextInsert(np->text,XmTextGetLastPosition(np->text),cbdata.strval);
	nl = strchr(cbdata.strval,'\n');
	while(nl){
		num++;
		nl = strchr(++nl,'\n');
	}
	if(num)
		tp = XmTextGetLastPosition(np->prompt_text);
	while(num){
		XmTextInsert(np->prompt_text,tp++,"\n");
		num--;
	}
	XtVaGetValues(np->vsbar,
		XmNvalue,	&num,
		NULL);
	XtVaSetValues(np->prompt_text,
		XmNuserData,	num,
		NULL);
	np->edit = edit;

	UpdateDisplay(ncl->go.x->dpy);

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
	XmTextPosition	tp;
	char		buffer[NhlERRMAXMSGLEN+1];
	char		*nl;
	int		len;

	if(!np->do_output)
		return;

	NhlErrSPrintMsg(buffer,(NhlErrMsg*)cbdata.ptrval);
	len = strlen(buffer);
	if(len < 1)
		return;
	if(buffer[len-1] != '\n')
		strcat(buffer,"\n");

	edit = np->edit;
	np->edit = True;
	XmTextInsert(np->text,XmTextGetLastPosition(np->text),buffer);
	len=0;
	nl = strchr(buffer,'\n');
	while(nl){
		len++;
		nl = strchr(++nl,'\n');
	}
	if(len)
		tp = XmTextGetLastPosition(np->prompt_text);
	while(len){
		XmTextInsert(np->prompt_text,tp++,"\n");
		len--;
	}
	XtVaGetValues(np->vsbar,
		XmNvalue,	&len,
		NULL);
	XtVaSetValues(np->prompt_text,
		XmNuserData,	len,
		NULL);
	np->edit = edit;

	UpdateDisplay(ncl->go.x->dpy);

	return;
}

static void
DrawHighlight
(
	NgNclEdit	ncl
)
{
	NgNclEditPart	*np = &ncl->ncledit;
	int		x,y;
	unsigned int	width,height;
	XPoint		pt[3];

	if(!np->high_drawn)
		return;

	x = np->left_offset-1;
	y = np->top_offset-1;
	width = np->prompt_width+np->text_width+2;
	height = np->text_height+2;
	XDrawRectangle(ncl->go.x->dpy,XtWindow(np->iform),np->high_gc,
			x,y,width,height);
	pt[0].x = pt[2].y = 0;
	pt[0].y = pt[1].y = np->text_height+1;
	pt[1].x = pt[2].x = np->text_width+1;
	XDrawLines(ncl->go.x->dpy,XtWindow(np->scroll),np->high_gc,pt,3,
							CoordModeOrigin);
}

static void
EraseHighlight
(
	NgNclEdit	ncl
)
{
	NgNclEditPart	*np = &ncl->ncledit;
	int		x,y;
	unsigned int	width,height;

	if(!np->high_drawn)
		return;

	XClearArea(ncl->go.x->dpy,XtWindow(np->iform),0,0,0,0,False);
	XClearArea(ncl->go.x->dpy,XtWindow(np->scroll),0,0,0,0,False);
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
	XmTextPosition		ppos = XmTextGetLastPosition(np->prompt_text);
	char			buff[20];
	NgNclPromptCBData	prompt = (NgNclPromptCBData)cbdata.ptrval;
	XmString		msg;
	int			len;
	Dimension		font_width;

	edit = np->edit;
	np->edit = True;

	if(XmTextGetSubstring(np->text,pos-2,2,3,buff) != XmCOPY_SUCCEEDED){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Can't get text.",func));
	}
	else if(np->print){
		/*
		 * If NclOutput was called put an extra blank line in before
		 * prompt.
		 */
		np->print = False;
		if(buff[1] != '\n'){
			XmTextInsert(np->text,pos,"\n\n");
			pos += 2;
			XmTextInsert(np->prompt_text,ppos,"\n\n");
			ppos += 2;
		}
		else if(buff[0] != '\n'){
			XmTextInsert(np->text,pos++,"\n");
			XmTextInsert(np->prompt_text,ppos++,"\n");
		}
	}
	else if(buff[1] != '\n'){
		XmTextInsert(np->text,pos++,"\n");
		XmTextInsert(np->prompt_text,ppos++,"\n");
	}

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

	sprintf(buff,"ncl %d>",prompt->line);
	msg = NgXAppCreateXmString(ncl->go.appmgr,buff);
	font_width = XmStringWidth(np->prompt_font,msg);
	NgXAppFreeXmString(ncl->go.appmgr,msg);
	if(font_width+np->shadow_thickness+(2*np->prompt_margin) >
							np->prompt_width){
		EraseHighlight(ncl);
		XtVaSetValues(np->prompt_text,
			XmNwidth,font_width+(2*np->shadow_thickness)+
							(2*np->prompt_margin),
			NULL);
		np->prompt_width = font_width+(2*np->shadow_thickness)+
							(2*np->prompt_margin);
		DrawHighlight(ncl);
	}

	XmTextInsert(np->prompt_text,ppos,buff);
	np->submit_pos = pos;
	np->line = prompt->line;

	XtVaGetValues(np->vsbar,
		XmNvalue,	&len,
		NULL);
	XtVaSetValues(np->prompt_text,
		XmNuserData,	len,
		NULL);

	np->edit = edit;
	np->do_output = False;

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

	XmTextSetHighlight(np->text,np->reset_pos,np->submit_pos,
						XmHIGHLIGHT_SECONDARY_SELECTED);
	np->reset_pos = np->submit_pos;
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

static void
MapPromptEH
(
	Widget		widget,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	char			func[] = "MapPromptEH";
	NgNclEdit		ncl = (NgNclEdit)udata;
	NgNclEditPart		*np = &ncl->ncledit;
	NgGOPart		*gp = &ncl->go;
	NgXAppExport		x = ncl->go.x;
	XSetWindowAttributes	att;
	unsigned long		vmask;
	XGCValues		gcvalues;

	if(event->type != MapNotify)
		return;

	XtRemoveEventHandler(np->prompt_text,StructureNotifyMask,False,
						MapPromptEH,udata);

	XtVaGetValues(np->prompt_text,
		XmNwidth,	&np->prompt_width,
		XmNleftOffset,	&np->left_offset,
		XmNtopOffset,	&np->top_offset,
		NULL);

	XtVaGetValues(np->text,
		XmNheight,		&np->text_height,
		XmNwidth,		&np->text_width,
		XmNshadowThickness,	&np->shadow_thickness,
		XmNhighlightColor,	&np->high_color,
		NULL);

	XtVaGetValues(np->iform,
		XmNbackground,		&np->back_color,
		NULL);

	gcvalues.function = GXcopy;
	gcvalues.foreground = np->high_color;
	gcvalues.line_width = 2;
	np->high_gc = XCreateGC(x->dpy,XtWindow(np->iform),
			(GCFunction|GCLineWidth|GCForeground),&gcvalues);

	vmask = CWDontPropagate | CWEventMask;
	att.do_not_propagate_mask = (KeyPressMask | KeyReleaseMask |
				ButtonPressMask | ButtonReleaseMask |
				PointerMotionMask | ButtonMotionMask);
	att.event_mask = (KeyPressMask | KeyReleaseMask |
				ButtonPressMask | ButtonReleaseMask |
				PointerMotionMask | ButtonMotionMask);
	np->prompt_win = XCreateWindow(x->dpy,XtWindow(np->prompt_text),
				0,0,WidthOfScreen(XtScreen(np->prompt_text)),
				HeightOfScreen(XtScreen(np->prompt_text)),
				0,CopyFromParent,InputOnly,CopyFromParent,
				vmask,&att);
	if(np->prompt_win == None){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Can't create iowin",func));
		return;
	}

	XMapRaised(x->dpy,np->prompt_win);

	return;
}

static void
PromptScrollCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmScrollBarCallbackStruct	*cbs =
					(XmScrollBarCallbackStruct *)cbdata;
	Widget		prompt = (Widget)udata;
	int		cur_top,lines;

	XtVaGetValues(prompt,
		XmNuserData,	&cur_top,
		NULL);

	lines = cbs->value - cur_top;
	XtVaSetValues(prompt,
		XmNuserData,	cbs->value,
		NULL);

	XmTextScroll(prompt,lines);

	return;
}

static void
ExposeFocusEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	NgNclEdit	ncl = (NgNclEdit)udata;

	DrawHighlight(ncl);
}

static void
GainFocusCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgNclEdit	ncl = (NgNclEdit)udata;
	NgNclEditPart	*np = &ncl->ncledit;

	np->high_drawn = True;
	DrawHighlight(ncl);
	XtAddEventHandler(np->iform,ExposureMask,False,
					ExposeFocusEH,(XtPointer)ncl);
}

static void
LoseFocusCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgNclEdit	ncl = (NgNclEdit)udata;
	NgNclEditPart	*np = &ncl->ncledit;
	int		x,y;
	unsigned int	width,height;

	XtRemoveEventHandler(np->iform,ExposureMask,False,
					ExposeFocusEH,(XtPointer)ncl);

	EraseHighlight(ncl);
	np->high_drawn = False;
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
	NgNclEdit	ncl = (NgNclEdit)udata;
	NgNclEditPart	*np = &ncl->ncledit;
	int		val;
	NhlBoolean	draw = np->high_drawn;

	if(event->type != ConfigureNotify)
		return;

	EraseHighlight(ncl);
	np->high_drawn = False;

	XtVaGetValues(w,
		XmNheight,	&np->text_height,
		XmNwidth,	&np->text_width,
		NULL);
	XtVaGetValues(ncl->ncledit.vsbar,
		XmNvalue,	&val,
		NULL);
	XtVaSetValues(ncl->ncledit.prompt_text,
		XmNheight,	np->text_height,
		XmNuserData,	val,
		NULL);

	np->high_drawn = draw;
	DrawHighlight(ncl);

	return;
}

static NhlBoolean
NECreateWin
(
	NgGO	go
)
{
	char		func[]="NECreateWin";
	NgNclEditPart	*np = &((NgNclEdit)go)->ncledit;
	Widget		menubar,menush,fmenu,emenu;
	Widget		vmenu,omenu,wmenu,hmenu;
	Widget		file,edit,view,options,window,help;
	Widget		addfile,load,close,quit,browse;
	Widget		pane,sform,sform1;
	Widget		slabel;
	Widget		hoframe,holabel;
	Widget		vframe,vlabel;
	Widget		fframe,flabel;
	Widget		fuframe,fulabel;
	Widget		oform,olabel;
	Widget		iform,scroll;
	Widget		hsbar;
	Dimension	width,height;
	Arg		args[10];
	int		nargs;
	NhlLayer	nstate;
	NhlArgVal	dummy,udata;
	char		buff[20];
	XmString	msg;

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
		XmNrightAttachment,	XmATTACH_POSITION,
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
		XmNrightAttachment,	XmATTACH_POSITION,
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
		NULL);

	fulabel = XtVaCreateManagedWidget("fulabel",xmLabelWidgetClass,fuframe,
		XmNchildType,			XmFRAME_TITLE_CHILD,
		NULL);

	nargs = 0;
	np->fulist = XmCreateScrolledList(fuframe,"fulist",NULL,0);
	XtManageChild(np->fulist);

	(void)NgXListManage(np->nsid,np->fulist,NgNclFUNC,NULL,NULL);

	np->iform = iform = XtVaCreateManagedWidget("iform",xmFormWidgetClass,
									pane,
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

	np->prompt_text = XtVaCreateManagedWidget("nclprompt",xmTextWidgetClass,
									iform,
		XmNeditMode,		XmMULTI_LINE_EDIT,
		XmNuserData,		0,
		NULL);
	XtVaGetValues(np->prompt_text,
		XmNfontList,		&np->prompt_font,
		XmNmarginWidth,		&np->prompt_margin,
		XmNshadowThickness,	&np->shadow_thickness,
		NULL);

	XtAddEventHandler(np->prompt_text,StructureNotifyMask,False,
						MapPromptEH,(XtPointer)go);

	np->scroll = scroll = XtVaCreateManagedWidget("scroll",
					xmScrolledWindowWidgetClass,iform,
		XmNbottomAttachment,	XmATTACH_WIDGET,
		XmNbottomWidget,	np->reset,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		np->prompt_text,
		NULL);

	np->text = XtVaCreateManagedWidget("nclcmd",xmTextWidgetClass,scroll,
		XmNeditMode,	XmMULTI_LINE_EDIT,
		NULL);
	XtAddCallback(np->prompt_text,XmNfocusCallback,TextFocusCB,
							(XtPointer)np->text);
	XtAddCallback(np->text,XmNmodifyVerifyCallback,CheckInput,
								(XtPointer)go);
	XtAddCallback(np->text,XmNactivateCallback,ActivateCB,(XtPointer)go);
	XtAddCallback(go->go.shell,XmNpopupCallback,TextFocusCB,
							(XtPointer)np->text);
	XtAddCallback(np->text,XmNfocusCallback,GainFocusCB,(XtPointer)go);
	XtAddCallback(np->text,XmNlosingFocusCallback,LoseFocusCB,
								(XtPointer)go);

	XtAddEventHandler(np->text,StructureNotifyMask,False,ChangeSizeEH,
								(XtPointer)go);
	XtVaGetValues(scroll,
		XmNverticalScrollBar,	&np->vsbar,
		XmNhorizontalScrollBar,	&hsbar,
		NULL);

#if	NOT
	XtVaGetValues(hsbar,
		XmNwidth,	&width,
		XmNheight,	&height,
		NULL);

	XtVaSetValues(hsbar,
		XmNshadowThickness,	2,
		XmNwidth,		width+4,
		XmNheight,		height+4,
		NULL);

	XtVaGetValues(np->vsbar,
		XmNwidth,	&width,
		XmNheight,	&height,
		NULL);

	XtVaSetValues(np->vsbar,
		XmNshadowThickness,	2,
		XmNwidth,		width+4,
		XmNheight,		height+4,
		NULL);
#endif

	XtAddCallback(np->vsbar,XmNvalueChangedCallback,PromptScrollCB,
							np->prompt_text);
	XtAddCallback(np->vsbar,XmNincrementCallback,PromptScrollCB,
							np->prompt_text);
	XtAddCallback(np->vsbar,XmNdecrementCallback,PromptScrollCB,
							np->prompt_text);
	XtAddCallback(np->vsbar,XmNpageIncrementCallback,PromptScrollCB,
							np->prompt_text);
	XtAddCallback(np->vsbar,XmNpageDecrementCallback,PromptScrollCB,
							np->prompt_text);
	XtAddCallback(np->vsbar,XmNtoTopCallback,PromptScrollCB,
							np->prompt_text);
	XtAddCallback(np->vsbar,XmNtoBottomCallback,PromptScrollCB,
							np->prompt_text);
	XtAddCallback(np->vsbar,XmNdragCallback,PromptScrollCB,np->prompt_text);

	NhlINITVAR(dummy);
	NhlINITVAR(udata);
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

	np->edit = False;
	np->line = NgNclCurrentLine(np->nsid);

	sprintf(buff,"ncl %d>",np->line);
	msg = NgXAppCreateXmString(go->go.appmgr,buff);
	width = XmStringWidth(np->prompt_font,msg);
	NgXAppFreeXmString(go->go.appmgr,msg);
	XtVaSetValues(np->prompt_text,
		XmNwidth,width+(2*np->shadow_thickness)+(2*np->prompt_margin),
		NULL);
	XmTextReplace(np->prompt_text,0,XmTextGetLastPosition(np->prompt_text),
									buff);
	np->submit_pos = np->reset_pos = 0;

	return True;
}
