/*
 *      $Id: addfile.c,v 1.19 1998-03-23 22:48:40 dbrown Exp $
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
 *	Authors:	Jeff W. Boote, David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct 14 16:32:09 MDT 1996
 *
 *	Description:	
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

#include <ncarg/ngo/addfileP.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/xutil.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/MenuShell.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/SelectioB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/TextF.h>
#include  <Xm/MessageB.h>
#include  <Xm/LabelG.h>
#include  <Xm/Frame.h>

static  NrmQuark QPreviewFile = NrmNULLQUARK;
static Pixel Background;
#define ADDFILESTRING "Add file. Enter file reference variable name:"
#define FILEADDEDSTRING "File added. File reference variable name is:"
#define NOFILESTRING "No recognized data files match path filter."
static XmString  AddFileXmString;
static XmString FileAddedXmString;
static XmString NoFileXmString;
static XmString NoMatchString = NULL;
static char Buffer[512];

#define MIN_VLIST_WIDTH 70

#define	Oset(field)	NhlOffset(NgAddFileRec,addfile.field)
static NhlResource resources[] = {
	{"no.res","no.res",NhlTInteger,sizeof(int),
		Oset(foo),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_NOACCESS,NULL},
};
#undef	Oset

static void ApplyAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static void FilterTextAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static void FilterAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static void OpenDataFileAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static void SelectFileAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static void ListUpOrDownAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static void InfoPopdownAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static void InfoPopupAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static void CheckInfoPopupAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static void VcrToggleAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static XtActionsRec addfileactions[] = {
	{ "ApplyAction", ApplyAction },
	{ "OpenDataFileAction", OpenDataFileAction },
        { "FilterAction", FilterAction },
        { "FilterTextAction", FilterTextAction },
	{ "SelectFileAction", SelectFileAction },
	{ "ListUpOrDownAction", ListUpOrDownAction },
	{ "InfoPopupAction", InfoPopupAction },
	{ "InfoPopdownAction", InfoPopdownAction },
	{ "CheckInfoPopupAction", CheckInfoPopupAction },
	{ "VcrToggleAction", VcrToggleAction },
};

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
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

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
/* close		*/	_NgGOInheritClose,
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

	addfile->go_class.manager = xmMessageBoxWidgetClass;

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

	np->vname = np->rw_optmenu = NULL;
        np->dl = NULL;
        np->finfo = NULL;
        np->dim_rec = NULL;
        np->attr_rec = NULL;
        np->mapped = False;
        np->user_dir_width = 0;
        np->list_timer_set = False;
        np->popped_up = False;
        np->adjust_event = False;
        np->shell_height = 0;
        np->vlist_empty = True;
	np->vlist_resize_width = MIN_VLIST_WIDTH;
	np->readable = np->writable = False;
	np->cleared = True;

	return NhlNOERROR;
}

static NhlBoolean RecognizedType
(
	char *filespec,
        NhlBoolean *writable
)
{
	char *f_extens[] = { "cdf", "nc", "hdf", "hd", "grb", "ccm" };
	char *dot_loc;
        int i;

        *writable = False;
	if ((dot_loc = strrchr(filespec,'.')) != NULL) {
		dot_loc++;
		for (i=0; i < NhlNumber(f_extens); i++) {
			if (! strcmp(dot_loc,f_extens[i])) {
                                if (i < 4)
                                        *writable = True;
				return True;
			}
		}
	}
        return False;
}

static void
AddFileScript
(
        NgGO		go
)
{
	char		func[] = "AddFileScript";
	NgAddFile	l = (NgAddFile)go;
	NgAddFilePart	*np = &l->addfile;
	char		*vname,*vname1;
	Widget		rtype;
	char		*rw = "r";
        NhlBoolean	writable;
        
        if (! RecognizedType(np->dirspec,&writable)) {
                if (!writable)
                        np->writable = False;
                return;
        }
        if (!writable)
                np->writable = False;
            
	vname = XmTextFieldGetString(l->addfile.vname);
	if(!vname || (vname[0] == '\0')){
		XtFree(vname);
		return;
	}
        vname1 = NgNclGetSymName(np->nsid,vname,False);

	/*
	 * Make sure dirstr and vname aren't so long they blow out the
	 * stack memory.
	 */
	if((strlen(np->dirspec)+strlen(vname1)) > (sizeof(Buffer) - 19)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"%s:variable name and filename too long:%s:%s",func,
                           vname1,np->dirspec));
		XtFree(vname);
		return;
	}

	XtVaGetValues(np->rw_optmenu,
		XmNmenuHistory,	&rtype,
		NULL);
	if(rtype && rtype == np->write_label)
		rw = "w";

	rw = "r";
	sprintf(Buffer,"%s = addfile(\"%s\",\"%s\")\n",vname1,np->dirspec,rw);

	(void)NgNclSubmitBlock(np->nsid,Buffer);

        XtFree(vname);
	return;
}

static void AdjustSize
(
 	NgGO	go
)
{
	NgAddFile	l = (NgAddFile)go;
	NgAddFilePart	*np = &l->addfile;
        int max,ssize,inc,pinc,off,oldoff;
	Widget wid,dirlabel,hscroll;
        XmFontList      fontlist;
        XmString xmlabel;
        Dimension w, cwidth,add,vwidth,fwidth,formwidth;
        Dimension formheight,h;
	int		vis_items, h_add = 0;
        
	np->adjust_event = False;
        if (! np->mapped)
                return;
        XtVaGetValues(go->go.manager,
                      XmNheight,&h,
                      XmNwidth,&w,
                      NULL);

#if	DEBUG_ADDFILE
        fprintf(stderr,"shell height %d width %d\n",h,w);
#endif
        
        if (np->shell_height <= 0)
                np->shell_height = h;
        else {
                h_add = h - np->shell_height;
                np->shell_height = h;
        }
/*
 * Force the directory box to be at least as wide as the directory label
 */
	XtVaSetValues(np->fselect_box,
		      XmNleftAttachment,XmATTACH_NONE,
		      XmNbottomAttachment,XmATTACH_NONE,
		      NULL);
        dirlabel = XmFileSelectionBoxGetChild(np->fselect_box,
                                              XmDIALOG_DIR_LIST_LABEL);
        XtVaGetValues(dirlabel,
                      XmNfontList,&fontlist,
                      XmNlabelString,&xmlabel,
                      NULL);
        
        w = XmStringWidth(fontlist,xmlabel);
        XmStringFree(xmlabel);
        XtVaGetValues(XtParent(np->dirlist),
                      XmNhorizontalScrollBar,&hscroll,
                      XmNwidth,&cwidth,
                      NULL);
		      
        cwidth = MAX(MAX(cwidth,w+10),np->user_dir_width);
	XtVaSetValues(XtParent(np->dirlist),
		      XmNwidth,cwidth,
		      NULL);
#if 0
        XtVaSetValues(XtParent(np->dirlist),
                      XmNwidth,cwidth,
                      NULL);
        XtVaGetValues(XtParent(np->dirlist),
                      XmNwidth,&cwidth,
                      NULL);
#endif
        XtVaGetValues(XtParent(np->vlist),
                      XmNwidth,&vwidth,
                      NULL);
        XtVaGetValues(XtParent(np->filelist),
                      XmNwidth,&fwidth,
                      NULL);
        cwidth = cwidth+vwidth+fwidth+40;
        XtVaGetValues(np->listform,
                      XmNwidth,&formwidth,
                      XmNheight,&formheight,
                      NULL);
        if (formwidth < cwidth - 1 || formwidth > cwidth + 1) {
                np->adjust_event = True;
#if	DEBUG_ADDFILE
                fprintf(stderr,"width %d adjusted width %d\n",
                        formwidth,cwidth);
#endif
                XtVaSetValues(np->listform,
                              XmNwidth,cwidth,
                              NULL);
        }
        if (h_add > 1 || h_add < -1) {
#if	DEBUG_ADDFILE
                fprintf(stderr,"height %d adjusted height %d\n",formheight,
                       formheight + h_add);
#endif
                formheight += h_add;
                XtVaSetValues(np->listform,
                              XmNheight,formheight,
                              NULL);
        }

	XtVaGetValues(np->vlist,
		      XmNvisibleItemCount,&vis_items,
		      NULL);
	XtVaSetValues(np->dirlist,
		      XmNvisibleItemCount,vis_items,
		      NULL);
	XtVaSetValues(np->filelist,
		      XmNvisibleItemCount,vis_items,
		      NULL);
	XtVaSetValues(np->fselect_box,
		      XmNleftAttachment,XmATTACH_FORM,
		      XmNbottomAttachment,XmATTACH_FORM,
		      NULL);
        XtVaGetValues(np->info_optmenu,
                      XmNleftOffset,&oldoff,
                      XmNwidth,&cwidth,
                      NULL);

        off = -cwidth/2;
        if (off < oldoff-1 || off > oldoff + 1) {
                XtVaSetValues(np->info_optmenu,
                              XmNleftPosition,50,
                              XmNleftOffset,off,
                              NULL);
        }

        XtVaGetValues(XtParent(np->dirlist),
                      XmNhorizontalScrollBar,&hscroll,
                      XmNwidth,&cwidth,
                      NULL);
        XtVaGetValues(hscroll,
                      XmNmaximum,&max,
		      XmNsliderSize,&ssize,
		      XmNincrement,&inc,
		      XmNpageIncrement,&pinc,
                      NULL);
	XmScrollBarSetValues(hscroll,max-ssize,ssize,inc,pinc,True);


	return;
}

static void SetApplyForm
(
        NgGO	go
)
{
	NgAddFile		l = (NgAddFile)go;
	NgAddFilePart		*np = &l->addfile;
	char			bname[_NhlMAXFNAMELEN];
	char			*item = NULL;
	char			*ptr,*ptr2,*ptr3;
	size_t			len;
        Dimension		width1,width2;
        char			*vname;
        NclApiDataList          *dlist,*dl;
        NclApiFileInfoRec	*finfo;
        NhlBoolean		added = False;
        NrmQuark		symbol;
        static NhlBoolean	first = True;
        XmString		xmstring;

        if (np->mapped && first) {
                first = False;
                XtVaGetValues(np->filelist,
                              XmNbackground,&Background,
                              NULL);
        }
        item = np->dirspec;
        if (item) {
                dlist = NclGetFileList();
                for (dl = dlist; dl != NULL; dl = dl->next) {
                        finfo = dl->u.file;
			if (finfo->name == QPreviewFile)
				continue;
                        if (!strcmp(item,NrmQuarkToString(finfo->path))) {
                                added = True;
                                symbol = finfo->name;
                                break;
                        }
                }
                NclFreeDataList(dlist);
        }

        if (added) {
                XtVaSetValues(np->vname,
                              XmNbackground,Background,
                              XmNeditable,False,
                              NULL);
                XmTextFieldSetString(np->vname,NrmQuarkToString(symbol));
                if (!np->vname_added || np->cleared) {
                        np->vname_added = True;
                        XtVaSetValues(np->vname_label,
                                      XmNlabelString,FileAddedXmString,
                                      NULL);
                }
                XtSetSensitive(np->ok,False);
                XtSetSensitive(np->apply,False);
		np->cleared = False;
                return;
        }

        if (np->vname_added || np->cleared) {
                np->vname_added = False;
                XtVaSetValues(np->vname_label,
                              XmNlabelString,AddFileXmString,
                              NULL);
                XtVaSetValues(np->vname,
                              XmNeditable,True,
                              XtVaTypedArg,XmNbackground,
                              XmRString,"lightsalmon",12,
                              NULL);
                XtSetSensitive(np->ok,True);
                XtSetSensitive(np->apply,True);
		np->cleared = False;
        }
        
	if(item)
		ptr = strrchr(item,'/');
	if(ptr)
		ptr++;
	if(ptr){
		/* Prefix numeric filenames with "F" */
		if(!isalpha(*ptr))
			strcpy(bname,"F");
		else
			bname[0]='\0';
		ptr2 = &bname[strlen(bname)];
		ptr3 = strrchr(item,'.');
                len = ptr3 - ptr;
		strncat(ptr2,ptr,len);
		ptr2[len]='\0';
                while (*ptr2 != '\0') {
                        if (! isalnum(*ptr2))
                                *ptr2 = '_';
                        ptr2++;
                }
	}
	else
		bname[0] = '\0';

        vname = NgNclGetSymName(np->nsid,bname,False);
	XmTextFieldSetString(np->vname,vname);
                
	return;
        
}

static void
ApplyButtonCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
        NgGO		go = (NgGO)udata;
	NgAddFile	l = (NgAddFile)go;
	NgAddFilePart	*np = &l->addfile;

#if	DEBUG_ADDFILE
	fprintf(stderr,"ApplyButtonCB(IN)\n");
#endif
	if (np->readable && ! np->vname_added) {
		AddFileScript(go);
        	SetApplyForm(go);
	}
        
        return;
}

static void
OkButtonCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
        NgGO		go = (NgGO)udata;
	NgAddFile	l = (NgAddFile)go;
	NgAddFilePart	*np = &l->addfile;
        
#if	DEBUG_ADDFILE
	fprintf(stderr,"OkButtonCB(IN)\n");
#endif
	if (np->readable && ! np->vname_added) {
		AddFileScript(go);
        	SetApplyForm(go);
	}
	NgGOPopdown(l->base.id);
}

static void
CreateDimInfoPopup
(
        NgAddFile	l,
	NgafDimInfoRec	*dip
)
{
        Widget frame, form;
        
        dip->popup =
                XtVaCreatePopupShell("DimInfo",overrideShellWidgetClass,
                                     l->go.manager,
                                     XmNallowShellResize,True,
                                     NULL);
        dip->frame =
                XtVaCreateManagedWidget("frame",
                                        xmFrameWidgetClass,dip->popup,
                                        NULL);
        form =
                XtVaCreateManagedWidget("form",
                                        xmFormWidgetClass,dip->frame,
                                        NULL);

        dip->grid = NgCreateDimInfoGrid
                (form,QPreviewFile,dip->vinfo,True,True);
        
        return;
        
}

static void
DoDimInfoPopup
(
        NgAddFile	l,
	NgafDimInfoRec	*dip,
	NhlBoolean	high
)
{
	NgAddFilePart		*np = &l->addfile;
        XmString		xmtext;
        Position		x,y,root_x,root_y;
        Dimension		w,h,p_w,p_h,root_w,root_h;
        char			*grid_text;
        int			i;
        NhlBoolean		new = False;

        XmListPosToBounds(np->vlist,dip->pos,&x,&y,&w,&h);
        XtTranslateCoords(np->vlist,(Position) 0,(Position) 0,
                          &root_x,&root_y);
        
        if (! dip->popup) {
                CreateDimInfoPopup(l,dip);
                new = True;
        }
        else {
                NgUpdateDimInfoGrid(dip->grid,QPreviewFile,dip->vinfo);
        }
        root_w = WidthOfScreen(XtScreen(dip->popup));
        root_h = HeightOfScreen(XtScreen(dip->popup));
        
            /* hack: realize widget so that its size can be calculated */
        if (new) {
                XtVaSetValues (dip->popup,
                               XmNx,root_x+x,
                               XmNy,root_y+y,
                               NULL);
                XtPopup(dip->popup,XtGrabNone);
                XtPopdown(dip->popup);
        }
        
        XtVaGetValues(dip->frame,
                      XmNwidth,&p_w,
                      XmNheight,&p_h,
                      NULL);
        
	x = MAX(0,MIN(root_w-p_w,root_x+x));
	y = high ? 
		MAX(0,MIN(root_h-p_h,root_y+y-p_h)) :
			MAX(0,MIN(root_h-p_h,root_y+y+h));

        XtVaSetValues (dip->popup,
                       XmNx,x,
                       XmNy,y,
                       NULL);

        XtPopup(dip->popup,XtGrabNone);
        dip->up = True;
        
        return;
}

static void
CreateAttrPopup
(
        NgAddFile	l,
	NgafAttrInfoRec	*aip,
        NclApiDataList	*dlist
)
{
        Widget frame, form;
        
        aip->popup =
                XtVaCreatePopupShell("AttrInfo",overrideShellWidgetClass,
                                     l->go.manager,
                                     XmNallowShellResize,True,
                                     NULL);
        aip->frame =
                XtVaCreateManagedWidget("frame",
                                        xmFrameWidgetClass,aip->popup,
                                        NULL);
        form =
                XtVaCreateManagedWidget("form",
                                        xmFormWidgetClass,aip->frame,
                                        NULL);

        aip->grid = NgCreateAttrInfoGrid(form,QPreviewFile,dlist);
        
        return;
        
}

static void
DoAttrPopup
(
        NgAddFile	l,
	NgafAttrInfoRec	*aip,
	NhlBoolean	high
)
{
	NgAddFilePart		*np = &l->addfile;
        XmString		xmtext;
        Position		x,y,root_x,root_y;
        Dimension		w,h,p_w,p_h,root_w,root_h;
        char			*grid_text;
        int			i;
        NhlBoolean		new = False;
        NclApiDataList		*dlist;
        
        
        XmListPosToBounds(aip->list,aip->pos,&x,&y,&w,&h);
        XtTranslateCoords(aip->list,(Position) 0,(Position) 0,
                          &root_x,&root_y);

/*
 * If aip->dl is NULL then use the file dlist for global atts
 */
        dlist = aip->dl ? aip->dl : np->dl;
        
        if (! aip->popup) {
                CreateAttrPopup(l,aip,dlist);
                new = True;
        }
        else {
                NgUpdateAttrInfoGrid(aip->grid,QPreviewFile,dlist);
        }
        root_w = WidthOfScreen(XtScreen(aip->popup));
        root_h = HeightOfScreen(XtScreen(aip->popup));
        
            /* hack: realize widget so that its size can be calculated */
        if (new) {
                XtVaSetValues (aip->popup,
                               XmNx,root_x+x,
                               XmNy,root_y+y+h,
                               NULL);
                XtPopup(aip->popup,XtGrabNone);
                XtPopdown(aip->popup);
        }
        
        XtVaGetValues(aip->frame,
                      XmNwidth,&p_w,
                      XmNheight,&p_h,
                      NULL);

	x = MAX(0,MIN(root_w-p_w,root_x+x));
	y = high ? 
		MAX(0,MIN(root_h-p_h,root_y+y-p_h)) :
			MAX(0,MIN(root_h-p_h,root_y+y+h));
        XtVaSetValues (aip->popup,
                       XmNx,x,
                       XmNy,y,
                       NULL);

        XtPopup(aip->popup,XtGrabNone);
        aip->up = True;
        
        return;
}
static void ClearVarList
(
	NgGO	go
)
{
	NgAddFile		l = (NgAddFile)go;
	NgAddFilePart		*np = &l->addfile;
        XmFontList      	fontlist;
        Dimension 		w, cwidth;

        np->finfo = NULL;
        np->file_changed = False;
        if (np->vlist_empty) {
                XmListDeselectAllItems(np->vlist);
                return;
	}
        
        XmListDeleteAllItems(np->vlist);
        XmListAddItem(np->vlist,NoMatchString,0);
        np->vlist_empty = True;
#if 0        
        XtVaGetValues(np->vlist,
                      XmNwidth,&cwidth,
                      XmNfontList,&fontlist,
                      NULL);
        
        w = XmStringWidth(fontlist,NoMatchString);

        w = MAX(MIN_VLIST_WIDTH,w+10);
        if (w != cwidth) {
                XtVaSetValues(np->vlist,
                              XmNwidth,MAX(MIN_VLIST_WIDTH,w+10),
                              NULL);
                AdjustSize(go);
        }
#endif        
        return;
}

static void OpenForPreview
(
	NgGO	go
)
{

	NgAddFile		l = (NgAddFile)go;
	NgAddFilePart		*np = &l->addfile;
	XmString dirspec;
	static NhlBoolean first = True;
	NclApiDataList *vlist,*dl;
        char func[] = "OpenForPreview";
        NhlBoolean	writable;

#if	DEBUG_ADDFILE
	fprintf(stderr,"OpenForPreview(IN)\n");
#endif

	if (! np->readable) {
                ClearVarList(go);
		return;
	}
	if (! RecognizedType(np->dirspec,&writable)) {
#if	DEBUG_ADDFILE
                fprintf(stderr,"file format not recognized\n");
#endif
                ClearVarList(go);
		return;
	}
        
	if (first) {
		QPreviewFile = NrmStringToQuark("_NgPreviewFile"); 
		first = False;
	}
	else {
		sprintf(Buffer,"delete(_NgPreviewFile)\n");
                (void)NgNclSubmitBlock(np->nsid,Buffer);
	}
	
	sprintf(Buffer,"_NgPreviewFile = addfile(\"%s\",\"r\")\n",np->dirspec);

        (void)NgNclSubmitBlock(np->nsid,Buffer);

	dl = NclGetFileInfo(QPreviewFile);
	if (!dl) {
#if	DEBUG_ADDFILE
                fprintf(stderr,"error reading file\n");
#endif
                ClearVarList(go);
		return;
	}
	if (np->dl != NULL)
		NclFreeDataList(np->dl);
	np->dl = dl;
	np->finfo = np->dl->u.file;

#if	DEBUG_ADDFILE
	fprintf(stderr,"n_dims %d, n_atts %d, n_vars %d\n",
	       np->finfo->n_dims,
	       np->finfo->n_atts,np->finfo->n_vars);
#endif

	return;
}

static void
DoInfoPopup
(
	NgAddFile	l,
        int		type,
	NhlBoolean	high
)
{
	NgAddFilePart		*np = &l->addfile;
        String			item, sval = NULL;
        NclApiDataList		*dl;
        XmStringTable		xmstringtable;
        int			*pos, count;
        NrmQuark		qvar;
	NgafAttrInfoRec		*aip;
	NgafDimInfoRec		*dip;
        int			top,vis_count;
        

        if (type == GLOBAL_ATTRS_POPUP && ! np->finfo) {
		OpenForPreview((NgGO)l);
	}
        if (np->finfo == NULL) {
                ClearVarList((NgGO)l);
                return;
        }
	switch (type) {
	case DIM_INFO_POPUP:
	case VAR_ATTRS_POPUP:
                np->cur_list = np->vlist;
                if (np->vlist_empty) {
                        XmListDeselectAllItems(np->vlist);
                        return;
                }
		XtVaGetValues(np->vlist,
			      XmNselectedItems,&xmstringtable,
			      NULL);
		if (!xmstringtable || !xmstringtable[0]) {
			return;
		}
		if (!XmListGetSelectedPos(np->vlist,&pos,&count))
			return;
                
		XmStringGetLtoR(xmstringtable[0],XmFONTLIST_DEFAULT_TAG,&sval);
		item = sval;
#if	DEBUG_ADDFILE
                fprintf(stderr,"var is %s\n", item);
#endif
		break;
	case GLOBAL_ATTRS_POPUP:
                np->cur_list = np->filelist;
		if (!XmListGetSelectedPos(np->filelist,&pos,&count))
			return;
		item = strrchr(np->dirspec,'/');
		if (!item)
			item = np->dirspec;
		if (!item)
			return;
		break;
	}
        XtVaGetValues(np->cur_list,
                      XmNtopItemPosition,&top,
                      XmNvisibleItemCount,&vis_count,
                      NULL);

        if (pos[0] < top)
                XmListSetPos(np->cur_list,pos[0]);
        else if (pos[0] > top + vis_count - 1)
                XmListSetBottomPos(np->cur_list,pos[0]);
                      
        switch (type) {
	case DIM_INFO_POPUP:
                dip = np->dim_rec;
                if (! dip) {
                        dip = NhlMalloc(sizeof(NgafDimInfoRec));
                        np->dim_rec = dip;
                        dip->popup = NULL;
                        dip->dl = NULL;
                }
		else if (dip->up) {
			XtPopdown(dip->popup);
		}
		dip->up = False;
        
                dip->qvar = NrmStringToQuark(item);
                if (dip->dl) {
                        NclFreeDataList(dip->dl);
                }
                dip->dl = NclGetFileVarInfo(QPreviewFile,dip->qvar);
                dip->vinfo = dip->dl->u.var;
                dip->pos = pos[0];

                DoDimInfoPopup(l,dip,high);
		break;
	case VAR_ATTRS_POPUP:
                aip = np->attr_rec;
                if (! aip) {
                        aip = NhlMalloc(sizeof(NgafAttrInfoRec));
                        np->attr_rec = aip;
                        aip->popup = NULL;
                        aip->dl = NULL;
                }
		else if (aip->up) {
			XtPopdown(aip->popup);
		}
		aip->up = False;
        
                aip->qvar = NrmStringToQuark(item);
                if (aip->dl) {
                        NclFreeDataList(aip->dl);
                }
                aip->dl = NclGetFileVarInfo(QPreviewFile,aip->qvar);
                aip->pos = pos[0];
		aip->list = np->vlist;

                DoAttrPopup(l,aip,high);
		break;
	case GLOBAL_ATTRS_POPUP:  /* global file attributes */
                aip = np->attr_rec;
                if (! aip) {
                        aip = NhlMalloc(sizeof(NgafAttrInfoRec));
                        np->attr_rec = aip;
                        aip->popup = NULL;
                        aip->dl = NULL;
                }
		else if (aip->up) {
			XtPopdown(aip->popup);
		}
		aip->up = False;
        
                aip->qvar = NrmStringToQuark(item);
                if (aip->dl) {
                        NclFreeDataList(aip->dl);
                }
                aip->dl = NULL;
                aip->pos = pos[0];
		aip->list = np->filelist;

                DoAttrPopup(l,aip,high);
		break;
        }
        
	if (sval)
		XtFree(sval);
        XtFree((char*)pos);
        return;
}


static void
CheckInfoPopup
(
	NgAddFile	l,
        int		type
)
{
	NgAddFilePart		*np = &l->addfile;
        NgafDimInfoRec		*dip;
        int			*pos, count;
        

        switch (type) {
	case DIM_INFO_POPUP:
		if (!XmListGetSelectedPos(np->vlist,&pos,&count))
			return;
                if (np->dim_rec) {
                        if (np->dim_rec->pos != pos[0])
                                DoInfoPopup(l,type,False);
                }
		break;
	case VAR_ATTRS_POPUP:
		if (!XmListGetSelectedPos(np->vlist,&pos,&count))
			return;
                if (np->attr_rec) {
                        if (np->attr_rec->pos != pos[0])
                                DoInfoPopup(l,type,True);
                }
		break;
	case GLOBAL_ATTRS_POPUP:
		if (!XmListGetSelectedPos(np->filelist,&pos,&count))
			return;
                if (np->attr_rec) {
                        if (np->attr_rec->pos != pos[0])
                                DoInfoPopup(l,type,True);
                }
		break;
        }
                
        XtFree((char*)pos);
        return;
}

static NhlBoolean ShowVarList
(
	NgGO	go
)
{

	NgAddFile		l = (NgAddFile)go;
	NgAddFilePart		*np = &l->addfile;
	NclApiDataList *vlist,*dl,*vl,*cl;
	int i;
	NhlBoolean recognized = False;
	XmString xmstring;
	String *clist;
        char func[] = "ShowVarList";
        XmFontList      fontlist;
        Dimension maxw = 0, cwidth;
        NgOrderData *vodata;
        NhlBoolean reverse;
                
#if	DEBUG_ADDFILE
	fprintf(stderr,"ShowVarList(IN)\n");
#endif

        if (np->vlist_empty) {
                OpenForPreview(go);
        }
        if (!np->finfo)
                return False;
        
	vodata = NhlMalloc(np->finfo->n_vars * sizeof(NgOrderData));
	vlist = vl = NclGetFileVarsList(QPreviewFile);
	for (i=0; vl != NULL; vl = vl->next,i++) {
		int j;
		NclApiVarInfoRec *vinfo;
		
		vinfo = vl->u.var;
		
		vodata[i].name = NrmQuarkToString(vinfo->name);
                vodata[i].n_dims = vinfo->n_dims;
		for (j = 0,vodata[i].size = 1; j < vinfo->n_dims; j++) {
                        vodata[i].size *= vinfo->dim_info[j].dim_size;
                }
                
	}
        XtVaGetValues(np->vlist,
                      XmNfontList,&fontlist,
                      XmNwidth, &cwidth,
                      NULL);

        reverse = np->var_sort_mode == NgNO_SORT ? True : False;
        NgSortOrderDataList
                (vodata,np->finfo->n_vars,np->var_sort_mode,reverse);
	XmListDeleteAllItems(np->vlist);
        
        for (i = 0; i < np->finfo->n_vars; i++) {
                Dimension w;
                
		xmstring =  XmStringCreateLocalized(vodata[i].name);
                w = XmStringWidth(fontlist,xmstring);
                if (w > maxw) maxw = w;
		XmListAddItem(np->vlist,xmstring,0);
		XmStringFree(xmstring);
	}
        np->vlist_resize_width = MAX(MIN_VLIST_WIDTH,maxw+10);
        if (cwidth < np->vlist_resize_width) {
                XtVaSetValues(np->vlist,
                              XmNwidth,np->vlist_resize_width,
                              NULL);
                AdjustSize(go);
        }
	NclFreeDataList(vlist);
        NhlFree(vodata);
        np->vlist_empty = False;
        XmListSelectPos(np->vlist,1,True);
        
	return True;
}

static void VarSortOptionsCB 
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgGO go = (NgGO)data;
	NgAddFile		l = (NgAddFile)go;
	NgAddFilePart		*np = &l->addfile;
        XtPointer		option;

#if	DEBUG_ADDFILE
	fprintf(stderr,"VarSortOptionsCB(IN)\n");
#endif
        
	XtVaGetValues(w,
		XmNuserData,&option,
		NULL);
        
        if (np->var_sort_mode != (NgSortMode)option) {
                np->var_sort_mode = (NgSortMode)option;
                if (!np->vlist_empty)
                        ShowVarList(go);
        }
        
	return;
}

static void SelectTextCB 
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgGO go = (NgGO)data;
	NgAddFile		l = (NgAddFile)go;
	NgAddFilePart		*np = &l->addfile;

#if	DEBUG_ADDFILE
	fprintf(stderr,"SelectTextCB(IN)\n");
#endif
	ShowVarList(go);
	return;
}

static void SelectButtonCB 
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgGO	go = (NgGO)data;
	
#if	DEBUG_ADDFILE
	fprintf(stderr,"SelectButtonCB(IN)\n");
#endif
	ShowVarList(go);

	return;
}

static void ClearApplyForm
(
        NgGO	go
)
{
	NgAddFile		l = (NgAddFile)go;
	NgAddFilePart		*np = &l->addfile;
	char			bname[_NhlMAXFNAMELEN];
	char			cmd[_NhlMAXFNAMELEN];
	char			*item = NULL;
	char			*ptr,*ptr2,*ptr3;
	XmString		cmdstr;
	size_t			len;
	char			*vname;

	XtVaSetValues(np->vname,
		      XmNeditable,True,
		      NULL);
	vname = XmTextFieldGetString(l->addfile.vname);
	XmTextFieldSetSelection(l->addfile.vname,0,strlen(vname),CurrentTime);
	XmTextFieldRemove(l->addfile.vname);
	XtFree(vname);
	XtSetSensitive(np->ok,False);
	XtSetSensitive(np->apply,False);
	XtVaSetValues(np->vname_label,
		      XmNlabelString,NoFileXmString,
		      NULL);
	XtVaSetValues(np->vname,
		      XmNbackground,Background,
		      XmNeditable,False,
		      NULL);

	np->cleared = True;
	return;
        
}

static void SetSelectText
(
 	NgGO	go
)
{
	NgAddFile	l = (NgAddFile)go;
	NgAddFilePart	*np = &l->addfile;
	char	*selecttext,*tstring = NULL;
	int	len;
	XmString dirspec;
	XmTextPosition pos;
	struct stat statbuf;
	XmString xmtmp;
	time_t cur_time;
        NhlBoolean writable;


	XtVaGetValues(np->fselect_box,
                      XmNdirSpec,&dirspec,
                      NULL);
                
	XmStringGetLtoR(dirspec,XmFONTLIST_DEFAULT_TAG,&selecttext);
	XmStringFree(dirspec);

        if (strcmp(np->dirspec,selecttext)) {
                np->file_changed = True;
        }
	else {
		return;
	}
	XtFree(np->dirspec);
	np->dirspec = selecttext;
	
	stat(selecttext,&statbuf);
	if (S_ISREG(statbuf.st_mode)) {
		sprintf(Buffer,"%ld",(long)statbuf.st_size);
		if (((getuid() == statbuf.st_uid) && 
		    (statbuf.st_mode & S_IRUSR)) ||
		    ((getgid() == statbuf.st_gid) &&
		     (statbuf.st_mode & S_IRGRP)) ||
		    (statbuf.st_mode & S_IROTH))
			np->readable = True;
		else
			np->readable = False;
		if (((getuid() == statbuf.st_uid) && 
		    (statbuf.st_mode & S_IWUSR)) ||
		    ((getgid() == statbuf.st_gid) &&
		     (statbuf.st_mode & S_IWGRP)) ||
		    (statbuf.st_mode & S_IWOTH))
			np->writable = True;
		else
			np->writable = False;
	}
	else {
		np->readable = False;
		np->writable = False;
		sprintf(Buffer,"           ");
	}
	if (! RecognizedType(np->dirspec,&writable)) {
                np->readable = False;
	}

        XtVaSetValues(np->rw_optmenu,
                      XmNmenuHistory,np->read_label,
                      NULL);

        if (!writable || !np->writable) {
                np->writable = False;
                XtSetSensitive(np->write_label,False);
        }
        else {
                XtSetSensitive(np->write_label,True);
        }

        if (!np->readable) {
                XtSetSensitive(np->rw_optmenu,False);
        }
        else {
                XtSetSensitive(np->rw_optmenu,True);
        }
        
	xmtmp = XmStringCreateLocalized(Buffer);
	XtVaSetValues(np->fsize_label,
                      XmNlabelString,xmtmp,
                      NULL);
	XmStringFree(xmtmp);

	if (S_ISREG(statbuf.st_mode)) {
		char *cp;
		struct tm *ts = localtime(&statbuf.st_mtime);
		sprintf(Buffer,"%s",asctime(ts));
		cp = strrchr(Buffer,'\n');
		if (cp)
		  *cp = '\0';
	}
	else
		sprintf(Buffer,"           ");
	xmtmp = XmStringCreateLocalized(Buffer);
	XtVaSetValues(np->fdate_label,
                      XmNlabelString,xmtmp,
                      NULL);
	XmStringFree(xmtmp);

	if (!np->readable) {
                ClearApplyForm(go);
        }
        else {
                SetApplyForm(go);
        }
        
	return;
}

static void Filter
(
 	NgGO	go
)
{
	NgAddFile	l = (NgAddFile)go;
	NgAddFilePart	*np = &l->addfile;
	char	*filtertext,*tstring = NULL;
	int	len;
        Dimension w, cwidth;

	if (! np->mapped)
        	return;
        
#if	DEBUG_ADDFILE
	fprintf(stderr,"Filter(IN)\n");
#endif

	XtVaGetValues(np->filtertext,
                      XmNvalue,&filtertext,
                      NULL);
	len = strlen(filtertext);
	if (filtertext[len-1] == '/') {
		tstring = NhlMalloc(len+2);
		strcpy(tstring,filtertext);
		strcat(tstring,"*");
		XtFree(filtertext);
		filtertext = tstring;
	}
	XmStringFree(np->dirmask);
	np->dirmask =  XmStringCreateLocalized(filtertext);
/*
 * Temporarily unattach the left side of the file selection box so it
 * will resize itself appropriately for the length of the file names in
 * the list. When reattached the directory box will resize to fit the
 * remaining space.
 */


	XtVaSetValues(np->fselect_box,
                    XmNleftAttachment,XmATTACH_NONE,
                    NULL);
        
	XmFileSelectionDoSearch(np->fselect_box,np->dirmask);
        AdjustSize(go);
        

	if (tstring == NULL) {
		XtFree(filtertext);
	}
	else {
		XtVaSetValues(np->filtertext,
			      XmNvalue,filtertext,
			      XmNcursorPosition,strlen(filtertext),
			      NULL);
		NhlFree(filtertext);
	}

        XmListSelectPos(np->filelist,1,True);

	SetSelectText(go);
        ClearVarList(go);

	return;
}
static void FilterButtonCB 
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgGO	go = (NgGO)data;

#if	DEBUG_ADDFILE
	fprintf(stderr,"FilterButtonCB(IN)\n");
#endif
	Filter(go);
	return;
}


static void FilterTextCB 
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgGO	go = (NgGO)data;

#if	DEBUG_ADDFILE
	fprintf(stderr,"FilterTextCB(IN)\n");
#endif
	Filter(go);
	return;
}

static void CancelCB 
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgAddFile	l = (NgAddFile)data;

#if	DEBUG_ADDFILE
	fprintf(stderr,"CancelCB(IN)\n");
#endif
	NgGOPopdown(l->base.id);

	return;
}

static void ListUpOrDown
(
	Widget		list,
        int		type
        )
{
	int     count,vis_count,top,position;
	int	*pos_list,pos_count;

	XtVaGetValues(list,
                      XmNitemCount, &count,
                      XmNtopItemPosition, &top,
                      XmNvisibleItemCount, &vis_count,
                      NULL);
        
	if (count == 0) {
		return;
	}

	if (! XmListGetSelectedPos(list,&pos_list,&pos_count)) {
		position = 0;
	}
	else {
		if (pos_count > 1) {
#if	DEBUG_ADDFILE
                        fprintf(stderr,"Error: more than one item selected\n");
#endif
			return;
		}
		position = pos_list[0];
	}
	if (position == 0) {
	        XmListSelectPos(list,++position,True) ;
        } 
	else {
		switch(type) {
		case 0:
			if (position > 1) {
				XmListDeselectPos(list,position);
				XmListSelectPos(list,--position,True);
			}
			break;
		case 1:
			if (position < count) {
				XmListDeselectPos(list,position);
				XmListSelectPos(list,++position,True);
			}
			break;
		case 2:
			XmListDeselectPos(list,position);
			position = 1;
			XmListSelectPos(list,position,True);
			break;
		case 3:
			XmListDeselectPos(list,position);
			position = count;
			XmListSelectPos(list,position,True);
			break;
		}
	}
	if (top > position)
		XmListSetPos(list,position);
	else if ((top + vis_count) <= position)
		XmListSetBottomPos(list,position);

        XtFree((char*)pos_list);
        
        return;
}
static void ListTimeoutCB
(
	XtPointer	data,
        XtIntervalId	*timer
        )
{
	NgGO		go = (NgGO) data;
	NgAddFile	l = (NgAddFile)(go);
	NgAddFilePart	*np = &l->addfile;
	NgVcrControl	vcrp = np->vcrp;
        
#if	DEBUG_ADDFILE
	fprintf(stderr,"ListTimeoutCB(IN)\n");
#endif
        if (! np->list_forward) {
                ListUpOrDown(np->cur_list,0);
        }
        else {
                ListUpOrDown(np->cur_list,1);
        }

        np->list_timer_id = XtAppAddTimeOut(go->go.x->app,
                                            100,ListTimeoutCB,go);
}

static void UpdatePopupTypeCB 
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgGO	go = (NgGO)data;
	NgAddFile	l = (NgAddFile)go;
	NgAddFilePart	*np = &l->addfile;
        int		type;

#if	DEBUG_ADDFILE
	fprintf(stderr,"UpdatePopupTypeCB(IN)\n");
#endif
        
	XtVaGetValues(w,
		XmNuserData,&type,
		NULL);
        if (type == DIM_INFO_POPUP || type == VAR_ATTRS_POPUP) {
                np->cur_list = np->vlist;
                if (np->readable && np->vlist_empty) {
                        ShowVarList(go);
                }
        }
        else {
                np->cur_list = np->filelist;
        }

        if (type != np->cur_popup_type) {
		if (np->attr_rec && np->attr_rec->up) {
			XtPopdown(np->attr_rec->popup);
			np->attr_rec->up = False;
		}
		if (np->dim_rec && np->dim_rec->up) {
			XtPopdown(np->dim_rec->popup);
			np->dim_rec->up = False;
		}
                XtVaSetValues(np->vcrp->start_stop,
                              XmNshadowType,XmSHADOW_OUT,NULL);
                np->popped_up = False;
	}
        np->cur_popup_type = type;
        return;
}

static void VcrArmCB 
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) data;
	NgAddFile	l = (NgAddFile)(go);
	NgAddFilePart	*np = &l->addfile;
	NgVcrControl	vcrp = np->vcrp;
        
#if	DEBUG_ADDFILE
	fprintf(stderr,"VcrArmCB(IN)\n");
#endif
        if (np->attr_rec && np->attr_rec->up) {
                XtPopdown(np->attr_rec->popup);
                np->attr_rec->up = False;
        }
        if (np->dim_rec && np->dim_rec->up) {
                XtPopdown(np->dim_rec->popup);
                np->dim_rec->up = False;
        }
	if (np->cur_popup_type != GLOBAL_ATTRS_POPUP 
	    && np->vlist_empty && np->readable) {
		ShowVarList(go);
	}
        
        if (w == vcrp->reverse) {
                ListUpOrDown(np->cur_list,0);
                np->list_forward = False;
        }
        else if (w == vcrp->forward) {
                ListUpOrDown(np->cur_list,1);
                np->list_forward = True;
        }

        np->list_timer_set = True;
        np->list_timer_id = XtAppAddTimeOut(go->go.x->app,
                                            250,ListTimeoutCB,go);
                                            
	return;
}

static void VcrCB 
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) data;
	NgAddFile	l = (NgAddFile)(go);
	NgAddFilePart	*np = &l->addfile;
	NgVcrControl	vcrp = np->vcrp;
        int 		type;

        
#if	DEBUG_ADDFILE
	fprintf(stderr,"VcrCB(IN)\n");
#endif
        if (np->list_timer_set) {
                XtRemoveTimeOut(np->list_timer_id);
                np->list_timer_set = False;
        }
        
	if (np->cur_popup_type != GLOBAL_ATTRS_POPUP 
	    && np->vlist_empty && np->readable) {
		ShowVarList(go);
	}
        
        if (w == vcrp->begin) {
                ListUpOrDown(np->cur_list,2);
        }
        else if (w == vcrp->reverse) {
                ;
        }
        else if (w == vcrp->start_stop) {
                ;
        }
        else if (w == vcrp->forward) {
                ;
        }
        else if (w == vcrp->end) {
                ListUpOrDown(np->cur_list,3);
        }
        
	SetSelectText(go);
        if (np->file_changed) {
                ClearVarList(go);
        }

        if (!np->readable) {
                if (np->popped_up) {
                        if (np->cur_popup_type != DIM_INFO_POPUP)
                                XtPopdown(np->attr_rec->popup);
                        else
                                XtPopdown(np->dim_rec->popup);
                }
                XtVaSetValues(vcrp->start_stop,
                              XmNshadowType,XmSHADOW_OUT,NULL);
                np->popped_up = False;
                return;
        }
        
        DoInfoPopup(l,np->cur_popup_type,True);

        XtVaSetValues(vcrp->start_stop,
                      XmNshadowType,XmSHADOW_IN,NULL);
        np->popped_up = True;
        
	return;
}

static void
LeaveVcrControlEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	NgGO		go = (NgGO) udata;
	NgAddFile	l = (NgAddFile)(go);
	NgAddFilePart	*np = &l->addfile;
	NgVcrControl	vcrp = np->vcrp;
        XCrossingEvent	*xcev = &event->xcrossing;
        Dimension	w_w,w_h;
        Widget		popup;

        if (np->cur_popup_type < DIM_INFO_POPUP)
                return;
        if (np->cur_popup_type != DIM_INFO_POPUP) {
                if (!np->attr_rec)
                        return;
                if (!np->attr_rec->up)
                        return;
                popup = np->attr_rec->popup;
        }
        else {
                if (!np->dim_rec)
                        return;
                if (!np->dim_rec->up)
                        return;
                popup = np->dim_rec->popup;
        }

        XtVaGetValues(w,
                      XmNwidth,&w_w,
                      XmNheight,&w_h,
                      NULL);
#if	DEBUG_ADDFILE
	fprintf(stderr,"LeaveVcrControl event %d %d %d %d %d\n",event->type,
               xcev->x,w_w,xcev->y,w_h);
#endif

        if (xcev->x < 0 || xcev->x >= w_w || xcev->y < 0 || xcev->y >= w_h) {
                XtPopdown(popup);
#if	DEBUG_ADDFILE
                fprintf(stderr,"ending showing attr info\n");
#endif
		if (np->cur_popup_type == DIM_INFO_POPUP)
			np->dim_rec->up = False;
		else
			np->attr_rec->up = False;
                XtVaSetValues(vcrp->start_stop,
                              XmNshadowType,XmSHADOW_OUT,NULL);
                np->popped_up = False;
        }

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
        NgAddFile	l = (NgAddFile)go;
        NgAddFilePart	*np = &l->addfile;

#if	DEBUG_ADDFILE
        fprintf(stderr,"EH height %d width %d\n",
                event->xconfigure.height,
                event->xconfigure.width);
#endif
        
        if (np->adjust_event) {
#if	DEBUG_ADDFILE
                fprintf(stderr,"ConfigureNotify generated by AdjustSize\n");
#endif
                np->adjust_event = False;
        }
	else if(event->type == ConfigureNotify) {
#if	DEBUG_ADDFILE
                fprintf(stderr,"user-generated ConfigureNotify\n");
#endif
		XtUnmapWidget(np->workareaform);
                XtVaGetValues(XtParent(np->dirlist),
                              XmNwidth,&np->user_dir_width,
                              NULL);
                XtVaSetValues(np->vlist,
                              XmNwidth,np->vlist_resize_width,
                              NULL);
                AdjustSize(go);
		XtMapWidget(np->workareaform);
        }
        else if (event->type == MapNotify) {
		String dirmask_text;
                np->mapped = True;
		XmStringGetLtoR
			(np->dirmask,XmFONTLIST_DEFAULT_TAG,&dirmask_text);
		XtVaSetValues(np->filtertext,
			      XmNvalue,dirmask_text,
			      XmNcursorPosition,strlen(dirmask_text),
			      NULL);
		XtFree(dirmask_text);
		Filter(go);
                AdjustSize(go);
                if ( !np->lists_mapped) {
                        XtMapWidget(np->workareaform);
                        np->lists_mapped = True;
                }
#if	DEBUG_ADDFILE
                fprintf(stderr,"MapNotify\n");
#endif

                return;
        }

	return;
}

static void
FileRefDeleteCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgGO		go = (NgGO) udata.ptrval;
        NgNclAny	node = (NgNclAny)cbdata.ptrval;
	NgAddFilePart	*np = &((NgAddFile)go)->addfile;
        char		*vname;
        XmString	xmstring;
        
#if	DEBUG_ADDFILE
        fprintf(stderr,"in fileref delete callback\n");
#endif

        XtVaGetValues(np->vname,
                      XmNvalue,&vname,
                      NULL);
        if (strcmp(vname,node->name))
                return;

        np->vname_added = False;
        XtVaSetValues(np->vname_label,
                      XmNlabelString,AddFileXmString,
                      NULL);
        XtVaSetValues(np->vname,
                      XmNeditable,True,
                      XtVaTypedArg,XmNbackground,
                      XmRString,"lightsalmon",12,
                      NULL);
        XtSetSensitive(np->ok,True);
        XtSetSensitive(np->apply,True);
        
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
	Widget		applyform,menu,menush,optmenu;
	Widget		filterbutton,varinfobutton;
        Widget		varform,filterform,form,label;
        Widget		varlabel,filterlabel,sizelabel,datelabel;
        Widget		frame,frame1,sep,pb;
	XmString	dirspec,xmtmp;
        String		dirmask_text;
        int		n;
	Arg		args[32];
        time_t		tim;
        NgVcrControl	vcrp;
        Dimension	width1,width2;
        int		pos;
	NhlArgVal	sel,user_data;
        XtTranslations	translations;
	char		*cp;
	static NhlBoolean first = True;
        
        char		vlist_trans[] =
	"Button3<Motion>:        ListButtonMotion() \
                                CheckInfoPopupAction(1) \n\
        <Btn1Down>:             ListBeginSelect() \
                                InfoPopupAction(0) \n\
        ~s ~c ~m ~a <Btn1Up>:   ListEndSelect() \
                                InfoPopdownAction(0) \n\
        Button1<Motion>:        ListButtonMotion() \
                                CheckInfoPopupAction(0) \n\
        <Btn3Down>:             ListBeginSelect() \
                                InfoPopupAction(1) \n\
        <Btn3Up>:               ListEndSelect() \
				InfoPopdownAction(1)";
        char		dirlist_trans[] =	
        "<Key>Return:            FilterAction() \n\
        <Select>:               FilterTextAction() \n\
        <Btn1Up>(2+):           ListBeginSelect() \
                                ListEndSelect() \
                                FilterAction() \n\
        <Btn1Down>,<Btn1Up>:    ListBeginSelect() \
                                ListEndSelect() \
                                FilterTextAction() \n\
        ~s ~c ~m ~a <Btn1Up>:   ListEndSelect() \
				FilterTextAction() \n\
        <Key>osfUp:             ListPrevItem() \
                                FilterTextAction() \n\
        <Key>osfDown:           ListNextItem() \
                                FilterTextAction() \n\
        <Key>osfPageUp:         ListPrevPage() \
                                FilterTextAction() \n\
        <Key>osfPageDown:       ListNextPage() \
				FilterTextAction()";
        char		filelist_trans[] =
        "Button1<Motion>:       ListBeginSelect() \n\
	<Btn1Down>:             ListBeginSelect() \
                                ListEndSelect() \
                                SelectFileAction() \n\
        ~s ~c ~m ~a <Btn1Up>:   ListEndSelect() \
                                SelectFileAction() \n\
        <Btn1Up>(2+):           ListEndSelect() \
                                OpenDataFileAction() \n\
        <Key>Return:            OpenDataFileAction() \n\
        <Select>:               SelectFileAction() \n\
        <Key>osfUp:             ListPrevItem() \
                                SelectFileAction() \n\
        <Key>osfDown:           ListNextItem() \
                                SelectFileAction() \n\
        <Key>osfPageUp:         ListPrevPage() \
                                SelectFileAction() \n\
        <Key>osfPageDown:       ListNextPage() \
                                SelectFileAction() \n\
        <Btn3Down>:             ListBeginSelect() \
                                ListEndSelect() \
                                SelectFileAction() \
                                InfoPopupAction(2) \n\
        <Btn3Up>:               ListEndSelect() \
				InfoPopdownAction(2)" ;

	if (first) {
		first = False;
		AddFileXmString = 
			NgXAppCreateXmString(go->go.appmgr,ADDFILESTRING);
		FileAddedXmString = 
		  	NgXAppCreateXmString(go->go.appmgr,FILEADDEDSTRING);
		NoFileXmString = 
		  	NgXAppCreateXmString(go->go.appmgr,NOFILESTRING);
	}
	XtAppAddActions(go->go.x->app,
                        addfileactions,NhlNumber(addfileactions));
	NhlVAGetValues(go->go.appmgr,
		NgNappNclState,	&np->nsid,
		NULL);

	if(!NhlIsClass(np->nsid,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                  "%s:invalid nclstate id, can't initialize addfile dialog",
                           func));
		return NhlFATAL;
	}

/*
 * Set up template dialog manager
 */
        XtVaSetValues(go->go.manager,
                      XmNdialogType,XmDIALOG_TEMPLATE,
                      NULL);
	XtAddCallback(go->go.manager,
                      XmNcancelCallback,_NgGOPopdownCB,(XtPointer)go->base.id);
	XtAddCallback(go->go.manager,
                      XmNokCallback,OkButtonCB,(XtPointer)go);
        np->ok = XmMessageBoxGetChild(go->go.manager,XmDIALOG_OK_BUTTON);
        
 	XtUnmanageChild(XmMessageBoxGetChild(go->go.manager,
                                             XmDIALOG_HELP_BUTTON));
 	XtUnmanageChild(XmMessageBoxGetChild(go->go.manager,
                                             XmDIALOG_SYMBOL_LABEL));
        XtAddEventHandler(go->go.manager,StructureNotifyMask,
                          False,ChangeSizeEH,(XtPointer)go);

        np->apply =
                XtVaCreateManagedWidget("Apply",
                                        xmPushButtonGadgetClass,go->go.manager,
                                        NULL);
	XtAddCallback(np->apply,XmNactivateCallback,ApplyButtonCB,go);
        
        filterbutton =
                XtVaCreateManagedWidget("Filter",
                                        xmPushButtonGadgetClass,go->go.manager,
                                        NULL);
	XtAddCallback(filterbutton,XmNactivateCallback,FilterButtonCB,go);

        varinfobutton =
                XtVaCreateManagedWidget("VarList",
                                        xmPushButtonGadgetClass,go->go.manager,
                                        NULL);
	XtAddCallback(varinfobutton,XmNactivateCallback,SelectTextCB,go);
        
        np->workareaform =
                XtVaCreateWidget("workareaform",
                                 xmFormWidgetClass,go->go.manager,
                                 NULL);
        XtSetMappedWhenManaged(np->workareaform,False);
        XtManageChild(np->workareaform);

/* bottom line */

	applyform =
                XtVaCreateManagedWidget("applyform",xmFormWidgetClass,
					np->workareaform,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        NULL);

            /* the read/write option menu */
	menush =
                XtVaCreatePopupShell("rwMenush",xmMenuShellWidgetClass,
                                     applyform,
                                     XmNwidth,5,
                                     XmNheight,5,
                                     XmNallowShellResize,True,
                                     XmNoverrideRedirect,True,
                                     NULL);

	menu =
                XtVaCreateWidget("rwMenu",xmRowColumnWidgetClass,menush,
                                 XmNrowColumnType,XmMENU_PULLDOWN,
                                 NULL);

	np->read_label =
                XtVaCreateManagedWidget("read-only",
                                        xmPushButtonGadgetClass,menu,
                                        XmNalignment,	XmALIGNMENT_CENTER,
                                        NULL);
	np->write_label =
                XtVaCreateManagedWidget("read/write",
                                        xmPushButtonGadgetClass,menu,
                                        XmNalignment,	XmALIGNMENT_CENTER,
                                        NULL);
	np->rw_optmenu =
                XtVaCreateManagedWidget("rwoptMenu",
                                        xmRowColumnWidgetClass,applyform,
                                        XmNspacing,0,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        XmNleftAttachment,XmATTACH_NONE,
                                        XmNrightAttachment,XmATTACH_FORM,
                                        XmNrowColumnType,XmMENU_OPTION,
                                        XmNsubMenuId,menu,
                                        NULL);

	menu = XmOptionLabelGadget(np->rw_optmenu);
	if(menu)
		XtUnmanageChild(menu);

	np->vname =
                XtVaCreateManagedWidget("vname",
                                        xmTextFieldWidgetClass,applyform,
					XmNtopAttachment, XmATTACH_NONE,
                                        XmNrightAttachment,XmATTACH_WIDGET,
                                        XmNrightWidget,np->rw_optmenu,
                                        XmNresizeWidth,False,
                                        XmNuserData,go,
                                        NULL);
	XtVaSetValues(np->vname,
		      XtVaTypedArg,XmNbackground,
		      XmRString,"lightsalmon",12,
		      NULL);

	np->vname_label =
                XtVaCreateManagedWidget("vname_label",
					xmLabelWidgetClass,applyform,
                                        XmNrightAttachment,XmATTACH_NONE,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,np->vname,
                                        XmNlabelString,AddFileXmString,
                                        NULL);
        np->vname_added = False;

	sep = 
                XtVaCreateManagedWidget("sep",
                                        xmSeparatorGadgetClass,
                                        np->workareaform,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,applyform,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        NULL);
   
	xmtmp = XmStringCreateLocalized("Date");
	datelabel = 
                XtVaCreateManagedWidget("datelabel",
                                        xmLabelGadgetClass,np->workareaform,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,sep,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        XmNrightAttachment,XmATTACH_NONE,
                                        XmNlabelString,xmtmp,
                                        NULL);
	XmStringFree(xmtmp);

	frame1 = 
                XtVaCreateManagedWidget("dateframe",
                                        xmFrameWidgetClass,np->workareaform,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,sep,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        XmNleftWidget,datelabel,
                                        XmNleftAttachment,XmATTACH_WIDGET,
                                        XmNrightAttachment,XmATTACH_NONE,
                                        NULL);
	tim = time(NULL);
	sprintf(Buffer,"%s",ctime(&tim));
	cp = strrchr(Buffer,'\n');
	if (cp)
		*cp = '\0';
	strcat(Buffer,"00");
	xmtmp = XmStringCreateLocalized(Buffer);
        np->fdate_label =
                XtVaCreateManagedWidget("filedate",
                                        xmLabelGadgetClass,frame1,
                                        XmNlabelString,xmtmp,
                                        NULL);
	XmStringFree(xmtmp);

	xmtmp = XmStringCreateLocalized("     ");
	XtVaSetValues(np->fdate_label,
                      XmNrecomputeSize,False,
                      XmNlabelString,xmtmp,
                      NULL);
	XmStringFree(xmtmp);
        
	xmtmp = XmStringCreateLocalized("Size");
	sizelabel = 
                XtVaCreateManagedWidget("sizelabel",
                                        xmLabelGadgetClass,np->workareaform,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,frame1,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        XmNrightAttachment,XmATTACH_NONE,
                                        XmNlabelString,xmtmp,
                                        NULL);
	XmStringFree(xmtmp);

	frame = 
                XtVaCreateManagedWidget("sizeframe",
                                        xmFrameWidgetClass,np->workareaform,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,frame1,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        XmNleftWidget,sizelabel,
                                        XmNleftAttachment,XmATTACH_WIDGET,
                                        XmNrightAttachment,XmATTACH_NONE,
                                        NULL);

	xmtmp = XmStringCreateLocalized("000000000");
	np->fsize_label =
                XtVaCreateManagedWidget("filesize",
                                        xmLabelGadgetClass,frame,
                                        XmNlabelString,xmtmp,
                                        NULL);
	XmStringFree(xmtmp);

	xmtmp = XmStringCreateLocalized("     ");
	XtVaSetValues(np->fsize_label,
                      XmNrecomputeSize,False,
                      XmNlabelString,xmtmp,
                      NULL);
	XmStringFree(xmtmp);

            /* var sort options option menu */
	menush =
                XtVaCreatePopupShell("varSortMenuSh",xmMenuShellWidgetClass,
                                     np->workareaform,
                                     XmNwidth,5,
                                     XmNheight,5,
                                     XmNallowShellResize,True,
                                     XmNoverrideRedirect,True,
                                     NULL);

	menu =
                XtVaCreateWidget("varSortMenu",xmRowColumnWidgetClass,menush,
                                 XmNrowColumnType,XmMENU_PULLDOWN,
                                 NULL);


        pb = XtVaCreateManagedWidget("Ascii",
                                     xmPushButtonGadgetClass,menu,
				     XmNalignment,	XmALIGNMENT_CENTER,
				     XmNuserData,(XtPointer)NgASCII_SORT,
				     XmNmarginHeight,0,
				     NULL);
        XtAddCallback(pb,XmNactivateCallback,VarSortOptionsCB,go);

        pb = XtVaCreateManagedWidget("Dims",
				     xmPushButtonGadgetClass,menu,
				     XmNalignment,	XmALIGNMENT_CENTER,
				     XmNuserData,(XtPointer)NgDIM_SORT,
				     XmNmarginHeight,0,
				     NULL);
        XtAddCallback(pb,XmNactivateCallback,VarSortOptionsCB,go);

        pb = XtVaCreateManagedWidget("Size",
				     xmPushButtonGadgetClass,menu,
				     XmNalignment,	XmALIGNMENT_CENTER,
				     XmNuserData,(XtPointer)NgSIZE_SORT,
				     XmNmarginHeight,0,
				     NULL);
        XtAddCallback(pb,XmNactivateCallback,VarSortOptionsCB,go);

        pb = XtVaCreateManagedWidget("No",
				     xmPushButtonGadgetClass,menu,
				     XmNalignment,	XmALIGNMENT_CENTER,
				     XmNuserData,(XtPointer)NgNO_SORT,
				     XmNmarginHeight,0,
				     NULL);
        XtAddCallback(pb,XmNactivateCallback,VarSortOptionsCB,go);
        
	optmenu =
                XtVaCreateManagedWidget("varSortMenu",
                                        xmRowColumnWidgetClass,
                                        np->workareaform,
                                        XmNleftAttachment,XmATTACH_WIDGET,
                                        XmNleftWidget,frame,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,frame1,
                                        XmNbottomOffset,4,
                                        XmNrightAttachment,XmATTACH_NONE,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        XmNspacing,0,
                                        XmNrowColumnType,XmMENU_OPTION,
                                        XmNsubMenuId,menu,
                                        NULL);
        
        
	xmtmp = XmStringCreateLocalized("Var Sort");
	menu = XmOptionLabelGadget(optmenu);
        XtVaSetValues(menu,
                      XmNlabelString,xmtmp,
                      NULL);
	XmStringFree(xmtmp);
        np->var_sort_mode = NgASCII_SORT;

	np->info_frame = 
                XtVaCreateManagedWidget("vcrframe",
                                        xmFrameWidgetClass,np->workareaform,
                                        XmNleftAttachment,XmATTACH_WIDGET,
                                        XmNleftWidget,optmenu,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,sep,
                                        XmNbottomOffset,10,
                                        XmNleftOffset,5,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        XmNshadowType,XmSHADOW_ETCHED_OUT,
                                        NULL);
	form = 
                XtVaCreateManagedWidget("form",
                                        xmFormWidgetClass,np->info_frame,
                                        NULL);

            /* info popup type option menu */
	menush =
                XtVaCreatePopupShell("infoMenuSh",xmMenuShellWidgetClass,
                                     form,
                                     XmNwidth,5,
                                     XmNheight,5,
                                     XmNallowShellResize,True,
                                     XmNoverrideRedirect,True,
                                     NULL);

	menu =
                XtVaCreateWidget("infoMenu",xmRowColumnWidgetClass,menush,
                                 XmNrowColumnType,XmMENU_PULLDOWN,
                                 NULL);


        pb = XtVaCreateManagedWidget("Global Attrs",
				    xmPushButtonGadgetClass,menu,
				     XmNalignment,	XmALIGNMENT_CENTER,
				     XmNuserData,GLOBAL_ATTRS_POPUP,
				     XmNmarginHeight,0,
				     NULL);
        XtAddCallback(pb,XmNactivateCallback,UpdatePopupTypeCB,go);

        pb = XtVaCreateManagedWidget("Var Attrs",
				     xmPushButtonGadgetClass,menu,
				     XmNalignment,	XmALIGNMENT_CENTER,
				     XmNuserData,VAR_ATTRS_POPUP,
				     XmNmarginHeight,0,
				     NULL);
        XtAddCallback(pb,XmNactivateCallback,UpdatePopupTypeCB,go);

        pb = XtVaCreateManagedWidget("Var Dim Info",
				     xmPushButtonGadgetClass,menu,
				     XmNalignment,	XmALIGNMENT_CENTER,
				     XmNmarginHeight,0,
				     XmNuserData,DIM_INFO_POPUP,
				     NULL);
        XtAddCallback(pb,XmNactivateCallback,UpdatePopupTypeCB,go);
        
	np->info_optmenu =
                XtVaCreateManagedWidget("infopopupMenu",
                                        xmRowColumnWidgetClass,form,
                                        XmNleftAttachment,XmATTACH_POSITION,
                                        XmNleftOffset,0,
                                        XmNrightAttachment,XmATTACH_NONE,
                                        XmNbottomAttachment,XmATTACH_NONE,
                                        XmNrowColumnType,XmMENU_OPTION,
                                        XmNspacing,0,
                                        XmNsubMenuId,menu,
                                        NULL);
        
        
	xmtmp = XmStringCreateLocalized("View");
	menu = XmOptionLabelGadget(np->info_optmenu);
        XtVaSetValues(menu,
                      XmNlabelString,xmtmp,
                      NULL);
	XmStringFree(xmtmp);
        
        np->vcrp = vcrp = 
 		NgCreateVcrControl(go,form,20,True,
				   True,False,True,True,True,False,True);
        
        XtVaSetValues(vcrp->form,
                      XmNtopAttachment,XmATTACH_WIDGET,
                      XmNtopWidget,np->info_optmenu,
                      NULL);
        
        XtAddCallback(vcrp->begin,XmNactivateCallback,VcrCB,go);
        XtAddCallback(vcrp->reverse,XmNactivateCallback,VcrCB,go);
        XtAddCallback(vcrp->reverse,XmNarmCallback,VcrArmCB,go);
        XtAddCallback(vcrp->forward,XmNactivateCallback,VcrCB,go);
        XtAddCallback(vcrp->forward,XmNarmCallback,VcrArmCB,go);
        XtAddCallback(vcrp->end,XmNactivateCallback,VcrCB,go);
        XtAddEventHandler(np->info_frame,LeaveWindowMask,
                          False,LeaveVcrControlEH,(XtPointer)go);
        XtVaSetValues(vcrp->start_stop,
                      XmNuserData,go,
                      XmNpushButtonEnabled,False,
                      XmNshadowType,XmSHADOW_OUT,
                      NULL);
        

/*
 * The file selection box and the variable display list are contained in
 * the listform. The variable display list is in its own form -- the varform.
 */

        np->listform =
                XtVaCreateManagedWidget("listform",
                                        xmFormWidgetClass,np->workareaform,
                                        XmNresizePolicy,XmRESIZE_NONE,
                                        XmNtopAttachment,XmATTACH_NONE,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,np->fsize_label,
                                        NULL);

        varform =
                XtVaCreateManagedWidget("varform",
                                        xmFormWidgetClass,np->listform,
                                        XmNleftAttachment,XmATTACH_NONE,
                                        XmNleftWidget,np->fselect_box,
                                        NULL);
        varlabel =
                XtVaCreateManagedWidget("varlabel",
                                        xmLabelWidgetClass,varform,
                                        XmNleftPosition,0,
                                        XmNleftAttachment,XmATTACH_POSITION,
                                        XmNrightAttachment,XmATTACH_NONE,
                                        XmNbottomAttachment,XmATTACH_NONE,
                                        NULL);

	n = 0;
	XtSetArg(args[n],XmNtopWidget,varlabel); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNselectionPolicy,XmBROWSE_SELECT); n++;
	XtSetArg(args[n],XmNlistSizePolicy,XmCONSTANT); n++;
	XtSetArg(args[n],XmNwidth,np->vlist_resize_width); n++;
	XtSetArg(args[n],XmNscrollBarDisplayPolicy,XmSTATIC); n++;
	XtSetArg(args[n],XmNnavigationType,XmSTICKY_TAB_GROUP); n++;
	XtSetArg(args[n],XmNautomaticSelection,True); n++;
	XtSetArg(args[n],XmNuserData,go); n++;
        
        np->vlist = XmCreateScrolledList(varform,"VarList",args,n);
        XtManageChild(np->vlist);
        np->lists_mapped = False;

        translations = XtParseTranslationTable(vlist_trans);
        XtOverrideTranslations(np->vlist,translations);

	np->fselect_box = 
                XtVaCreateManagedWidget("addfile_fsb",
                                        xmFileSelectionBoxWidgetClass,
                                        np->listform,
                                        XmNrightAttachment,XmATTACH_WIDGET,
                                        XmNrightWidget,varform,
                                        XmNuserData,go,
                                        XmNfileSearchProc,NgXFileSearchProc,
                                        NULL);
        
 	XtUnmanageChild(XmFileSelectionBoxGetChild(np->fselect_box,
						   XmDIALOG_HELP_BUTTON));
	XtUnmanageChild(XmFileSelectionBoxGetChild(np->fselect_box,
						   XmDIALOG_OK_BUTTON));
	XtUnmanageChild(XmFileSelectionBoxGetChild(np->fselect_box,
						   XmDIALOG_APPLY_BUTTON));
	XtUnmanageChild(XmFileSelectionBoxGetChild(np->fselect_box,
						   XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild(XmFileSelectionBoxGetChild(np->fselect_box,
						   XmDIALOG_SEPARATOR));
	XtUnmanageChild(XmFileSelectionBoxGetChild(np->fselect_box,
						   XmDIALOG_FILTER_LABEL));
	XtUnmanageChild(XmFileSelectionBoxGetChild(np->fselect_box,
						   XmDIALOG_FILTER_TEXT));
	XtUnmanageChild(XmFileSelectionBoxGetChild(np->fselect_box,
						   XmDIALOG_SELECTION_LABEL));
	XtUnmanageChild(XmFileSelectionBoxGetChild(np->fselect_box,
						   XmDIALOG_TEXT));
	/* get the wid's of the dirlist and filelist and set the 
	 * np as user data
	 */

	np->filelist = XmFileSelectionBoxGetChild(np->fselect_box,
                                                  XmDIALOG_LIST);

        XtVaSetValues(XtParent(np->filelist),
		      XmNscrollBarDisplayPolicy,XmSTATIC,
                      NULL);
        
        translations = XtParseTranslationTable(filelist_trans);
        XtOverrideTranslations(np->filelist,translations);
	np->dirlist = XmFileSelectionBoxGetChild(np->fselect_box,
                                                 XmDIALOG_DIR_LIST);
        translations = XtParseTranslationTable(dirlist_trans);
        XtOverrideTranslations(np->dirlist,translations);
#if 0        
	_XtDisplayTranslations(np->filelist,NULL,NULL,NULL);
#endif
	XtVaSetValues(np->dirlist,
                      XmNuserData,go,
                      NULL);
	XtVaSetValues(np->filelist,
                      XmNuserData,go,
                      NULL);
	XtVaGetValues(np->fselect_box,
                      XmNdirMask,&np->dirmask,
                      XmNdirSpec,&dirspec,
                      XmNpattern,&np->pattern,
                      XmNdirectory,&np->dir,
                      XmNnoMatchString,&NoMatchString,
                      NULL);
	np->cur_list = np->filelist;
	np->cur_popup_type = GLOBAL_ATTRS_POPUP;
        
        XmListAddItem(np->vlist,NoMatchString,0);
	XmStringGetLtoR(dirspec,XmFONTLIST_DEFAULT_TAG,&np->dirspec);
/*
 * The filter label and text area are contained in the filterform
 */
	filterform = 
                XtVaCreateManagedWidget("filterform",
                                        xmFormWidgetClass,np->workareaform,
                                        XmNbottomAttachment,XmATTACH_WIDGET,
                                        XmNbottomWidget,np->listform,
                                        NULL);
	filterlabel = 
                XtVaCreateManagedWidget("Filter",
                                        xmLabelGadgetClass,filterform,
                                        XmNrightAttachment,XmATTACH_NONE,
                                        NULL);

        np->filtertext = 
                XtVaCreateManagedWidget("FilterText",
                                        xmTextFieldWidgetClass,filterform,
                                        XmNleftAttachment,XmATTACH_WIDGET,
                                        XmNleftWidget,filterlabel,
                                        XmNresizeWidth,False,
					XmNvalue,"filler",
					XmNcursorPosition,strlen("filler"),
                                        XmNuserData,go,
                                        NULL);
	XmStringGetLtoR(np->dirmask,XmFONTLIST_DEFAULT_TAG,&dirmask_text);
	XtVaSetValues(np->filtertext,
		      XtVaTypedArg,XmNbackground,
		      XmRString,"lightsalmon",12,
#if 0
		      XmNvalue,dirmask_text,
		      XmNcursorPosition,strlen(dirmask_text),
#endif
		      NULL);
	XtAddCallback(np->filtertext,XmNactivateCallback,
		      FilterTextCB,go);
	XtFree(dirmask_text);
        
	NhlINITVAR(sel);
	NhlINITVAR(user_data);
	user_data.ptrval = go;
        sel.lngval = NgNclCBDELETE_FILEVAR;
	_NhlAddObjCallback(_NhlGetLayer(np->nsid),
                           NgCBnsObject,sel,FileRefDeleteCB,user_data);
        Filter(go);
        ClearVarList(go);
#if 0        
        {
/* begin debug */
        int	max,ssize,inc,pinc;
        Widget  hscroll;
                
        XtVaGetValues(XtParent(np->dirlist),
                      XmNhorizontalScrollBar,&hscroll,
                      NULL);
        XtVaGetValues(hscroll,
                      XmNmaximum,&max,
		      XmNsliderSize,&ssize,
		      XmNincrement,&inc,
		      XmNpageIncrement,&pinc,
                      NULL);
        
#if	DEBUG_ADDFILE
        fprintf(stderr,"dirlist max %d, ssize %d, inc %d, pageinc %d\n",
               max,ssize,inc,pinc);
#endif
        XtVaSetValues(hscroll,
		      XmNpageIncrement,40,
                      NULL);
        XtVaGetValues(XtParent(np->filelist),
                      XmNhorizontalScrollBar,&hscroll,
                      NULL);
        XtVaGetValues(hscroll,
                      XmNmaximum,&max,
		      XmNsliderSize,&ssize,
		      XmNincrement,&inc,
		      XmNpageIncrement,&pinc,
                      NULL);
#if	DEBUG_ADDFILE
        fprintf(stderr,"filelist max %d, ssize %d, inc %d, pageinc %d\n",
               max,ssize,inc,pinc);
#endif
        XtVaGetValues(XtParent(np->vlist),
                      XmNhorizontalScrollBar,&hscroll,
                      NULL);
        XtVaGetValues(hscroll,
                      XmNmaximum,&max,
		      XmNsliderSize,&ssize,
		      XmNincrement,&inc,
		      XmNpageIncrement,&pinc,
                      NULL);
        
#if	DEBUG_ADDFILE
        fprintf(stderr,"vlist max %d, ssize %d, inc %d, pageinc %d\n",
               max,ssize,inc,pinc);
#endif
/* end debug */
        }
        XSync(go->go.x->dpy,False);
#endif        
	return True;
}

static void ApplyAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;
        NgAddFile	l;
        NgAddFilePart	*np;
        
#if	DEBUG_ADDFILE
	fprintf(stderr,"ApplyAction(IN)\n");
#endif
	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);
	if (! go)
		return;

	l = (NgAddFile)go;
	np = &l->addfile;

	if (np->readable && ! np->vname_added) {
		AddFileScript(go);
        	SetApplyForm(go);
	}
        
        return;
}

static void FilterTextAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;
	XmString xmdirmask;
	char    *dirmask;
        NgAddFile	l;
        NgAddFilePart	*np;
        
#if	DEBUG_ADDFILE
	fprintf(stderr,"FilterTextAction(IN)\n");
#endif
	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);
	l = (NgAddFile)go;
	np = &l->addfile;

	XtVaGetValues(np->fselect_box,
                      XmNdirMask,(void*)&xmdirmask,
                      NULL);
	XmStringGetLtoR(xmdirmask,XmSTRING_DEFAULT_CHARSET,&dirmask);

	XtVaSetValues(np->filtertext,
		      XmNvalue,dirmask,
		      XmNcursorPosition,strlen(dirmask),
		      NULL);
        
	XmStringFree(xmdirmask);
	XtFree(dirmask);
        
	return;
}

static void FilterAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;

#if	DEBUG_ADDFILE
	fprintf(stderr,"FilterAction(IN)\n");
#endif

	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);
	Filter(go);

	return;
}


static void SelectFileAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;
	NgAddFile		l; 
	NgAddFilePart		*np;

#if	DEBUG_ADDFILE
	fprintf(stderr,"SelectFileAction(IN)\n");
#endif
	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);
        l = (NgAddFile)go;
        np = &l->addfile;

	SetSelectText(go);
        if (np->file_changed)
                ClearVarList(go);

	return;
}


static void OpenDataFileAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;
	NgAddFile		l; 
	NgAddFilePart		*np;

#if	DEBUG_ADDFILE
	fprintf(stderr,"OpenDataFileAction(IN)\n");
#endif
	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);

        l = (NgAddFile)go;
        np = &l->addfile;
	SetSelectText( go);
	ShowVarList(go);

	return;
}

static void ListUpOrDownAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;
        NgAddFile	l;
        NgAddFilePart	*np;
	int     pval;
	Widget  list;


#if	DEBUG_ADDFILE
	fprintf(stderr,"ListUpOrDownAction(IN)\n");
#endif
	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);
	l = (NgAddFile)go;
	np = &l->addfile;

	pval = atoi(params[0]);
        
	if (w == np->filtertext)
		list = np->dirlist;
	else if (w == np->vname)
		list = np->filelist;
	else {
#if	DEBUG_ADDFILE
                fprintf(stderr,"invalid widget\n");
#endif
                return;
	}

        ListUpOrDown(list,pval);

	if (w == np->filtertext)
		FilterTextAction(np->dirlist,xev,params,0);
	else
		SelectFileAction(np->filelist,xev,params,0);

	return;
}

static void InfoPopupAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;
        NgAddFile	l;
        int	type;

#if	DEBUG_ADDFILE
	fprintf(stderr,"InfoPopupAction(IN)\n");
#endif
	type = atoi(params[0]);

	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);
        
	l = (NgAddFile)go;

	if (type == DIM_INFO_POPUP)
		DoInfoPopup(l,type,False);
	else
		DoInfoPopup(l,type,True);
        
	return;
}


static void CheckInfoPopupAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;
        NgAddFile	l;
        int	type;
        XMotionEvent *mev = (XMotionEvent *)xev;

#if	DEBUG_ADDFILE
	fprintf(stderr,"CheckInfoPopupAction(IN)\n");
#endif
	type = atoi(params[0]);

	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);
        
	l = (NgAddFile)go;

	if (mev->state & Button1MotionMask)
		CheckInfoPopup(l,DIM_INFO_POPUP);
	if (mev->state & Button3MotionMask)
		CheckInfoPopup(l,VAR_ATTRS_POPUP);
        
	return;
}

static void InfoPopdownAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;
        NgAddFile	l;
        NgAddFilePart	*np;
        int	type;
        XButtonEvent *bev = (XButtonEvent *)xev;


#if	DEBUG_ADDFILE
	fprintf(stderr,"InfoPopdownAction(IN)\n");
#endif
        
	type = atoi(params[0]);
	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);
        
	l = (NgAddFile)go;
	np = &l->addfile;

#if	DEBUG_ADDFILE
	fprintf(stderr,"button %d mask %d\n",bev->button,bev->state);
#endif

        if (bev->button == 1) {
                if (np->dim_rec && np->dim_rec->up) {
                        XtPopdown(np->dim_rec->popup);
#if	DEBUG_ADDFILE
                        fprintf(stderr,"ending showing var dim info\n");
#endif
                        np->dim_rec->up = False;
                }
        }
        else if (bev->button == 3) {
                if (np->attr_rec && np->attr_rec->up) {
                        XtPopdown(np->attr_rec->popup);
#if	DEBUG_ADDFILE
                        fprintf(stderr,"ending showing attr info\n");
#endif
                        np->attr_rec->up = False;
                }
        }
        
	return;
}
        
static void VcrToggleAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
        )
{
	NgGO	go;
        NgAddFile	l;
        NgAddFilePart	*np;
        Widget		popup = NULL;

	XtVaGetValues(w,
                      XmNuserData,(void*)&go,
                      NULL);
        
	l = (NgAddFile)go;
	np = &l->addfile;
        
#if	DEBUG_ADDFILE
	fprintf(stderr,"VcrToggleAction(IN)\n");
#endif
        if (np->popped_up) {
                if (np->cur_popup_type != DIM_INFO_POPUP && np->attr_rec) {
                        popup = np->attr_rec->popup;
                        np->attr_rec->up = False;
                }
                else if (np->dim_rec) {
                        popup = np->dim_rec->popup;
                        np->dim_rec->up = False;
                }
                if (popup)
                        XtPopdown(popup);
                np->popped_up = False;
                XtVaSetValues(w,
                              XmNpushButtonEnabled,False,
                              XmNshadowType,XmSHADOW_OUT,NULL);
#if	DEBUG_ADDFILE
                fprintf(stderr,"ending showing attr info\n");
#endif
        }
        else {
                if (! np->readable)
                        return;
		if (np->cur_popup_type != GLOBAL_ATTRS_POPUP
                    && np->vlist_empty) {
			ShowVarList(go);
		}
                DoInfoPopup(l,np->cur_popup_type,True);
                np->popped_up = True;
                XtVaSetValues(w,
                              XmNshadowType,XmSHADOW_IN,NULL);
        }
        
        return;
}

