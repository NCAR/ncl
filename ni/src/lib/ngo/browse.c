/*
 *      $Id: browse.c,v 1.10 1997-09-04 17:05:40 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		browse.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Mar  4 12:38:45 MST 1997
 *
 *	Description:	
 */
#include <ncarg/ngo/browseP.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/filepageP.h>
#include <ncarg/ngo/varpageP.h>
#include <ncarg/ngo/hlupageP.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Frame.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/DrawnB.h>
#include <XmL/Folder.h>

static void TabFocusAction(
#if	NhlNeedProto
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
#endif
);

static XtActionsRec browseactions[] = {
	{ "TabFocusAction", TabFocusAction }
};

static NhlErrorTypes BrowseInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes BrowseDestroy(
	NhlLayer	l
);

static NhlBoolean BrowseCreateWin(
	NgGO	go
);

static brPage *
UpdatePanes(
	NgGO		go,
        brPageType	type,
        NrmQuark	qvar,
        NrmQuark	qfile,
	NhlBoolean	delete
);

NgBrowseClassRec NgbrowseClassRec = {
	{
/* class_name		*/	"browseClass",
/* nrm_class		*/	NrmNULLQUARK,
/* layer_size		*/	sizeof(NgBrowseRec),
/* class_inited		*/	False,
/* superclass		*/	(NhlClass)&NggOClassRec,
/* cvt_table		*/	NULL,

/* layer_resources	*/	NULL,
/* num_resources	*/	0,
/* all_resources	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize*/	NULL,
/* class_initialize	*/	NULL,
/* layer_initialize	*/	BrowseInitialize,
/* layer_set_values	*/	NULL,
/* layer_set_values_hook*/	NULL,
/* layer_get_values	*/	NULL,
/* layer_reparent	*/	NULL,
/* layer_destroy	*/	BrowseDestroy,

	},
	{
/* dialog		*/	NULL,
/* toplevel		*/	NULL,
/* manager		*/	NULL,

/* top_win_chain	*/	False,
/* create_win		*/	BrowseCreateWin,
/* create_win_hook	*/	NULL,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgbrowseClass = (NhlClass)&NgbrowseClassRec;

static NgPageId CurrentPageId = 0;

static NhlErrorTypes
BrowseInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char			func[] = "BrowseInitialize";
	NgBrowse		browse = (NgBrowse)new;
	NgBrowsePart		*np = &((NgBrowse)new)->browse;
	NgBrowsePart		*rp = &((NgBrowse)req)->browse;

	np->nsid = NhlDEFAULT_APP;
	NhlVAGetValues(browse->go.appmgr,
		NgNappNclState,	&np->nsid,
		NULL);
	if(!NhlIsClass(np->nsid,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid nclstate resource",
									func));
		return NhlFATAL;
	}

        np->vmenus = NULL;
        
	return NhlNOERROR;
}

static NhlErrorTypes
BrowseDestroy
(
	NhlLayer	l
)
{
	NgBrowse	browse = (NgBrowse)l;
	NgBrowsePart	*np = &((NgBrowse)l)->browse;

	NgAppRemoveGO(browse->go.appmgr,l->base.id);

	return NhlNOERROR;
}

#if	NOT
static Widget
CreateMenuBar
(
	NgGO	go
)
{
	NgBrowsePart	*np = &((NgBrowse)go)->browse;
	Widget		menubar,menush,fmenu,emenu;
	Widget		vmenu,omenu,wmenu,hmenu;
	Widget		file,edit,view,options,window,help;
	Widget		addfile,load,close,quit,ncledit;
        
	menubar = XtVaCreateManagedWidget
                ("menubar",xmRowColumnWidgetClass,
                 go->go.manager,
                 XmNrowColumnType,	XmMENU_BAR,
                 NULL);

	menush = XtVaCreatePopupShell
                ("menush",xmMenuShellWidgetClass,
                 go->go.shell,
                 XmNwidth,		5,
                 XmNheight,		5,
                 XmNallowShellResize,	True,
                 XtNoverrideRedirect,	True,
                 NULL);
	fmenu = XtVaCreateWidget
                ("fmenu",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);

	emenu = XtVaCreateWidget
                ("emenu",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);

	vmenu = XtVaCreateWidget
                ("vmenu",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);

	omenu = XtVaCreateWidget
                ("omenu",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);

	wmenu = XtVaCreateWidget
                ("wmenu",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);

	hmenu = XtVaCreateWidget
                ("hmenu",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);

	file = XtVaCreateManagedWidget
                ("file",xmCascadeButtonGadgetClass,
                 menubar,
                 XmNsubMenuId,	fmenu,
                 NULL);

	edit = XtVaCreateManagedWidget
                ("edit",xmCascadeButtonGadgetClass,
                 menubar,
                 XmNsubMenuId,	emenu,
                 NULL);

	view = XtVaCreateManagedWidget
                ("view",xmCascadeButtonGadgetClass,
                 menubar,
                 XmNsubMenuId,	vmenu,
                 NULL);

	options = XtVaCreateManagedWidget
                ("options",xmCascadeButtonGadgetClass,
                 menubar,
                 XmNsubMenuId,	omenu,
                 NULL);

	window = XtVaCreateManagedWidget
                ("window",xmCascadeButtonGadgetClass,
                 menubar,
                 XmNsubMenuId,	wmenu,
                 NULL);

	help = XtVaCreateManagedWidget
                ("help",xmCascadeButtonGadgetClass,
                 menubar,
                 XmNsubMenuId,	hmenu,
                 NULL);

	XtVaSetValues
                (menubar,
                 XmNmenuHelpWidget,	help,
                 NULL);

	addfile = XtVaCreateManagedWidget
                ("addFile",xmPushButtonGadgetClass,
                 fmenu,
                 NULL);
	XtAddCallback(addfile,XmNactivateCallback,_NgGODefActionCB,NULL);

	load = XtVaCreateManagedWidget
                ("loadScript",xmPushButtonGadgetClass,
                 fmenu,
                 NULL);
	XtAddCallback(load,XmNactivateCallback,_NgGODefActionCB,NULL);

	close = XtVaCreateManagedWidget
                ("closeWindow",xmPushButtonGadgetClass,
                 fmenu,
                 NULL);
	XtAddCallback(close,XmNactivateCallback,_NgGODefActionCB,NULL);

	quit = XtVaCreateManagedWidget
                ("quitApplication",xmPushButtonGadgetClass,
                 fmenu,
                 NULL);
	XtAddCallback(quit,XmNactivateCallback,_NgGODefActionCB,NULL);
        
	ncledit = XtVaCreateManagedWidget
                ("nclWindow",xmPushButtonGadgetClass,
                 wmenu,
                 NULL);
	XtAddCallback(ncledit,XmNactivateCallback,_NgGODefActionCB,NULL);

	XtManageChild(fmenu);
	XtManageChild(emenu);
	XtManageChild(vmenu);
	XtManageChild(omenu);
	XtManageChild(wmenu);
	XtManageChild(hmenu);

        return menubar;
        
}
#endif

/*
 * Sets the folder size to the greater of the available space
 * in the clip window and the size of the page. It returns the size of
 * the available space for page elements that should be sized to fit.
 */

extern void
NgSetFolderSize
(
	brPane	*pane,
        Dimension page_width,
        Dimension page_height,
        Dimension *avail_width,
        Dimension *avail_height
        )
{
	int			left_off,top_off,right_off,bottom_off;
	Dimension		tab_height;
        Dimension		sw_width,sw_height;
        Dimension		vscroll_width = 23, hscroll_height = 23;
        Dimension		folder_border = 2,clip_border = 2;
        Dimension		req_width,folder_width, folder_height;
        enum SB_Status { Yes, No, Maybe } h_scroll_status, v_scroll_status;
        
	XtVaGetValues(pane->folder,
		      XmNtabBarHeight,&tab_height,
		      XmNtopOffset,&top_off,
		      XmNleftOffset,&left_off,
		      XmNrightOffset,&right_off,
		      XmNbottomOffset,&bottom_off,
		      NULL);
        
        XtVaGetValues(pane->scroller,
                      XmNwidth, &sw_width,
                      XmNheight, &sw_height,
                      NULL);
        *avail_width = sw_width - left_off - right_off
                - 2 * folder_border - 2 * clip_border;
        *avail_height = sw_height - top_off - bottom_off
                - 2 * folder_border - 2 * clip_border - tab_height;

        req_width = MAX(page_width,pane->max_tab_xtnt);
        if (req_width > *avail_width)
                h_scroll_status = Yes;
        else if (req_width < *avail_width - vscroll_width)
                h_scroll_status = No;
        else
                h_scroll_status = Maybe;
        
        if (page_height > *avail_height)
                v_scroll_status = Yes;
        else if (page_height < *avail_height - hscroll_height)
                v_scroll_status = No;
        else
                v_scroll_status = Maybe;

        if (v_scroll_status == Maybe && h_scroll_status == Maybe)
                v_scroll_status = h_scroll_status = No;
        else if (v_scroll_status == Maybe)
                v_scroll_status = h_scroll_status;
        else if (h_scroll_status == Maybe)
                h_scroll_status = v_scroll_status;

        if (v_scroll_status == Yes)
                *avail_width -= vscroll_width;
        if (h_scroll_status == Yes)
                *avail_height -= hscroll_height;

        folder_width = MAX(*avail_width + 2 * folder_border,
                           req_width + 2 * folder_border);
        folder_height = MAX(*avail_height + 2 * folder_border + tab_height,
                            page_height + 2 * folder_border + tab_height + 2);
        
        XtVaSetValues(pane->folder,
                      XmNwidth,folder_width,
                      XmNheight,folder_height,
                      NULL);
        
        return;
}

static char *PageString
(
        brPane		*pane,
        brPage		*page
        )
{
        static char string[512];
        
        switch (page->type) {
            case _brREGVAR:
                    strcpy(string,NrmQuarkToString(page->qvar));
                    break;
            case _brFILEREF:
                    strcpy(string,NrmQuarkToString(page->qfile));
                    break;
            case _brFILEVAR:
                    strcpy(string,NrmQuarkToString(page->qfile));
                    strcat(string,"->");
                    strcat(string,NrmQuarkToString(page->qvar));
                    break;
            case _brHLUVAR:
                    strcpy(string,NrmQuarkToString(page->qvar));
                    break;
        }

        return string;
        
}
static void SetTabFocus
(
        NgGO	go,
        brPane	*pane,
        brPage	*page
        )
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        XFocusChangeEvent xev;
        int		pos;
        
        for (pos = 0; pos < pane->pagecount; pos++) {
                brPage *tpage = XmLArrayGet(pane->pagelist,pos);
                if (tpage && tpage == page)
                        break;
        }
        
	if (pos == pane->pagecount) {
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
                fprintf(stderr,"page not found\n");
#endif
                return;
	}

#if	DEBUG_DATABROWSER & DEBUG_FOLDER
        fprintf(stderr,"focus: %s tab #%d\n",PageString(pane,page),pos);
#endif
        XtSetKeyboardFocus(go->go.manager,page->tab->tab);
        xev.type = FocusIn;
        xev.send_event = False;
        xev.display = XtDisplay(page->tab->tab);
        xev.window = XtWindow(page->tab->tab);
        xev.mode = NotifyNormal;
        xev.detail = NotifyDetailNone;
        XtCallActionProc(page->tab->tab,
                         "XmLFolderPrimFocusIn",(XEvent*)&xev,NULL,0);
        
        pcp->focus_pane = pane;
        pcp->focus_pos = pos;
        
        return;
}

static void ActiveTabCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPane		*pane = (brPane *)udata;
	NgGO		go = pane->go;
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        brPage		*page;
        int		ix = 0;
        char		*string;
        
        XmLFolderCallbackStruct *cbs = (XmLFolderCallbackStruct *)cb_data;

        
#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"ActiveTabCB(IN)\n");
#endif
        
	if (cbs->pos >= pane->pagecount) {
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
                fprintf(stderr,"tab position out of range\n");
#endif
                return;
	}
        page = XmLArrayGet(pane->pagelist,cbs->pos);
        
        string = PageString(pane,page);
        
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
        fprintf(stderr,"tab # %d activated for %s\n",cbs->pos,string);
#endif
        pane->active_pos = cbs->pos;

#if	DEBUG_DATABROWSER & DEBUG_FOLDER
        fprintf(stderr,"setting keyboard focus to %s\n",
                       PageString(pane,page));
#endif

        SetTabFocus(go,pane,page);
	(*page->pdata->adjust_page_geo)(page);
        
        return;
        
}

static void
TabFocusEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
        brPane		*pane = (brPane *)udata;
	NgGO		go = pane->go;
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        brPage		*page;
        int		pos;
        char		*string;
        

	switch (event->type) {
	case FocusOut:
		return;
	case FocusIn:
		break;
	}

#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"TabFocusEH(IN)\n");
#endif
        for (pos = 0; pos < pane->pagecount; pos++) {
                page = XmLArrayGet(pane->pagelist,pos);
                if (page && page->tab && w == page->tab->tab)
                        break;
        }
        
	if (pos == pane->pagecount) {
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
                fprintf(stderr,"tab position out of range\n");
#endif
                return;
	}

        string = PageString(pane,page);
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
        fprintf(stderr,"focus: %s tab #%d\n",string,pos);
#endif

        pcp->focus_pane = pane;
        pcp->focus_pos = pos;
        
	return;
}

static void
CreateFolder
(
	NgGO		go,
        brPane	 	*pane
)
{
	int left,right;

        pane->folder = XtVaCreateManagedWidget
                ("Folder",xmlFolderWidgetClass,
                 pane->form,
                 XmNresizePolicy,XmRESIZE_NONE,
                 XmNresizable,True,
                 XmNbottomAttachment,XmATTACH_NONE,
                 XmNrightAttachment,XmATTACH_NONE,
		 XmNheight,10,
		 XmNwidth,10,
                 NULL);
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
        fprintf(stderr,"folder %x pane %x\n", pane->folder,pane);
#endif

        XtAddCallback(pane->folder,XmNactivateCallback,ActiveTabCB,pane);

	XtVaCreateManagedWidget("BogusWidget",
				xmLabelGadgetClass,pane->folder,
				NULL);
	pane->has_folder = True;

	return;
}
static void
DestroyPageDataList
(
	NgGO		go,
        brPageDataList	pdata
)
{
	brPageData	*pd,*next;

	for (pd = pdata; pd != NULL ; ) {
		if (pd->dl)
			NclFreeDataList(pd->dl);
		(*pd->destroy_page)(pd->type_rec);
		XtDestroyWidget(pd->form);
		next = pd->next;
		NhlFree(pd);
		pd = next;
	}
	return;
}

static void
DestroyFolder
(
	NgGO		go,
        brPane	 	*pane
)
{
	int i;

	DestroyPageDataList(go,pane->var_pages);
	DestroyPageDataList(go,pane->fileref_pages);
	DestroyPageDataList(go,pane->hlu_pages);
	for (i = 0; i < pane->tabcount; i++) {
		brTab *tab = XmLArrayGet(pane->tablist,i);
		XtDestroyWidget(tab->tab);
		NhlFree(tab);
	}
	XmLArrayFree(pane->tablist);
	for (i = 0; i < pane->pagecount; i++) {
		brPage *page = XmLArrayGet(pane->pagelist,i);
		NhlFree(page);
	}
	XmLArrayFree(pane->pagelist);

	XtDestroyWidget(pane->folder);
	pane->has_folder = False;
        pane->tabcount = 0;
        pane->tablist = NULL;
	pane->pagelist = NULL;
        pane->last_pagecount = pane->pagecount = 0;
	pane->fileref_pages = NULL;
	pane->var_pages = NULL;
	pane->hlu_pages = NULL;
        pane->active_pos = 0;
}

static void
InitPane
(
	NgGO		go,
        Widget		parent,
        brPane	 	*pane
)
{
        XtActionList actions;
        Cardinal num_actions;
        
        pane->go = go;
        pane->managed = False;
        pane->topform = XtVaCreateWidget
                ("topform", xmFormWidgetClass,parent,
		 XmNallowResize,	True,
                 NULL);
        pane->scroller = XtVaCreateManagedWidget
                ("scroller", xmScrolledWindowWidgetClass,pane->topform,
                 XmNscrollBarDisplayPolicy,	XmAS_NEEDED,
                 XmNscrollingPolicy,		XmAUTOMATIC,
                 NULL);
        pane->form = XtVaCreateManagedWidget
                ("form", xmFormWidgetClass,pane->scroller,
                 NULL);

        pane->has_folder = False;

        pane->tabcount = 0;
        pane->tablist = NULL;
        pane->max_tab_xtnt = 0;
	pane->pagelist = NULL;
        pane->last_pagecount = pane->pagecount = 0;
	pane->fileref_pages = NULL;
	pane->var_pages = NULL;
	pane->hlu_pages = NULL;
        pane->active_pos = 0;

        return;
}

static void
MapFirstPaneEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	Widget		form = (Widget)udata;
	Dimension	height;

	if(event->type != MapNotify)
		return;

	XtRemoveEventHandler(w,StructureNotifyMask,False,MapFirstPaneEH,NULL);
	XtManageChild(form);

	return;
}

static brPane *
AddPane
(
	NgGO	go
)
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        NhlString	e_text;
        brPaneControl	*pcp = &np->pane_ctrl;
        brPane		*pane;
        int		i;
        Dimension	height,theight = 0;

/*
 * Temporarily fix the shell widget size,
 * but only if the shell has been realized
 */
        if (pcp->current_count > 0) {
                XtVaSetValues(go->go.manager,
                              XmNresizePolicy,XmRESIZE_NONE,
                              NULL);
        }
        
        for (i=0; i < pcp->current_count; i++) {
                XtVaGetValues(pcp->panes[i]->topform,
                              XmNheight,&height,
                              NULL);
                theight += height;
        }

        if (pcp->current_count < pcp->alloc_count) {
                pane = pcp->panes[pcp->current_count];
        }
        else {
                pcp->panes[pcp->current_count] = NhlMalloc(sizeof(brPane));
                if (!pcp->panes[pcp->current_count]) {
                        e_text = "%s: dynamic memory allocation error";
                        NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                }
                pane = pcp->panes[pcp->current_count];
                pcp->alloc_count++;
                InitPane(go,np->paned_window,pane);
        }
        pcp->current_count++;
        
        if(XtIsRealized(np->paned_window))
        	XtManageChild(pane->topform);
	else{
		XtAddEventHandler(np->paned_window,StructureNotifyMask,
			False,MapFirstPaneEH,pane->topform);
	}
        pane->managed = True;

        height = theight / pcp->current_count;
	if(height > 0){
                for (i=0; i < pcp->current_count; i++) {
                        XtVaSetValues(pcp->panes[i]->topform,
                                      XmNpaneMaximum,height,
                                      XmNpaneMinimum,height,
                                      XmNheight,height,
                                      NULL);
                }
        }
        
        pcp->current_ix = pcp->current_count - 1;

        for (i=0; i < pcp->current_count; i++) {
                XtVaSetValues(pcp->panes[i]->topform,
                              XmNpaneMaximum,1000,
                              XmNpaneMinimum,1,
                              NULL);
        }

        XtVaSetValues(go->go.manager,
                      XmNresizePolicy,XmRESIZE_ANY,
                      NULL);
        return pane;
}

static void
RemovePane
(
	NgGO	go
)
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        NhlString	e_text;
        brPaneControl	*pcp = &np->pane_ctrl;
        brPane		*pane;
        int		i;
        Dimension	height,theight = 0;

        if (pcp->current_count < 2)
                return;

        XtVaSetValues(go->go.manager,
                      XmNresizePolicy,XmRESIZE_NONE,
                      NULL);
        for (i=0; i < pcp->current_count; i++) {
                XtVaGetValues(pcp->panes[i]->topform,
                              XmNheight,&height,
                              NULL);
                theight += height;
        }
        
        pane = pcp->panes[pcp->current_count-1];
        XtUnmanageChild(pane->topform);
        pane->managed = False;

        pcp->current_count--;
        if (pcp->current_ix >= pcp->current_count-1)
                pcp->current_ix = 0;
        
        height = theight / pcp->current_count;

        if (height > 0) {
                for (i=0; i < pcp->current_count; i++) {
                        XtVaSetValues(pcp->panes[i]->topform,
                                      XmNpaneMaximum,height,
                                      XmNpaneMinimum,height,
                                      XmNheight,height,
                                      NULL);
                }
        }

        for (i=0; i < pcp->current_count; i++) {
                XtVaSetValues(pcp->panes[i]->topform,
                              XmNpaneMaximum,1000,
                              XmNpaneMinimum,1,
                              NULL);
        }

        XtVaSetValues(go->go.manager,
                      XmNresizePolicy,XmRESIZE_ANY,
                      NULL);
        return;
}

static void
NextPane
(
	NgGO	go
        )
{
        
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;

        pcp->current_ix = (pcp->current_ix+1) % pcp->current_count;
        return;
        
}

static brPane *
CurrentPane
(
	NgGO	go
)
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;

        return pcp->panes[pcp->current_ix];
        
}

static int
PaneNumber
(
	NgGO	go,
	brPane	*pane
)
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
	int		i;

	for (i=0; i < pcp->alloc_count; i++) {
		if (pane == pcp->panes[i]) {
			return i;
		}
	}

        return -1;
        
}


static void
NewTab
(
	NgGO		go,
        brPane		*pane
)
{
	NgBrowse		browse = (NgBrowse)go;
	NgBrowsePart		*np = &browse->browse;
        NhlString		e_text;
        brTab			*tp,*tlp;
        int			pos = 0;

	if (!(tp = NhlMalloc(sizeof(brTab)))) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
	}
	tp->tab =  XtVaCreateWidget
		("tab",xmDrawnButtonWidgetClass,pane->folder,
                 XmNuserData,pane,
		 NULL);
        XtAddEventHandler(tp->tab,FocusChangeMask,
                          False,TabFocusEH,(XtPointer)pane);
        
        tp->managed = False;

        if (! pane->tablist) {
                pane->tablist = XmLArrayNew(0,0);
        }
        else {
                pos = pane->tabcount;
        }
        XmLArrayAdd(pane->tablist,pos,1);
        XmLArraySet(pane->tablist,pos,tp);
        pane->tabcount++;
}

typedef enum _TabUpdateMode 
{
        _ADD,
        _REMOVE,
        _SHUFFLE
} TabUpdateMode;
        
static void
UpdateTabs
(
	NgGO		go,
        brPane		*pane,
        int		pos,
        TabUpdateMode	mode
)
{
	NgBrowse		browse = (NgBrowse)go;
	NgBrowsePart		*np = &browse->browse;
        brPaneControl		*pcp = &np->pane_ctrl;
        NhlString		e_text;
	char			name[512];
	XmString		xmname;
        brPage			*page;
        NrmQuark		qfile = NrmNULLQUARK;
        int			count = 0;
        Dimension		x,width,tab_x,tab_width,form_width;
        brTab			*tab;
        int			i;

        if (mode == _REMOVE) {
                if (pane->active_pos == pane->remove_pos) {
                        if (pane->active_pos < pane->pagecount) {
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
				fprintf(stderr,
					"setting tab %d active for pane %d\n",
					pane->pagecount,PaneNumber(go,pane));
#endif
                                XmLFolderSetActiveTab(
                                        pane->folder,pane->pagecount,True);
                        }
                        else {
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
				fprintf(stderr,
					"setting tab %d active for pane %d\n",
					pane->pagecount-1,PaneNumber(go,pane));
#endif
                                XmLFolderSetActiveTab(
                                        pane->folder,pane->pagecount-1,True);
                        }
                }
                else {
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
		        fprintf(stderr,
				"setting tab %d active for pane %d\n",
				pane->pagecount,PaneNumber(go,pane));
#endif
                        XmLFolderSetActiveTab(
                                pane->folder,pane->pagecount,True);
                }
                        
        }
        
        while (pane->tabcount < pane->pagecount) {
                NewTab(go,pane);
        }
        
        for (i = 0; i < pane->pagecount; i++) {
                tab = XmLArrayGet(pane->tablist,i);
                page = XmLArrayGet(pane->pagelist,i);
                
                switch (page->type) {
                    case _brREGVAR:
                            strcpy(name,NrmQuarkToString(page->qvar));
                            break;
                    case _brFILEREF:
                            qfile = page->qfile;
                            strcpy(name,NrmQuarkToString(page->qfile));
                            break;
                    case _brHLUVAR:
                            strcpy(name,NrmQuarkToString(page->qvar));
                            break;
                    case _brFILEVAR:
                            if (page->qfile == qfile) {
                                    strcpy(name,"->");
                            }
                            else {
                                    qfile = page->qfile;
                                    strcpy(name,NrmQuarkToString(page->qfile));
                                    strcat(name,"->");
                            }
                            strcat(name,NrmQuarkToString(page->qvar));
                            break;
                }
                xmname = NgXAppCreateXmString(go->go.appmgr,name);
		XtVaSetValues(tab->tab,
			      XmNlabelString,xmname,
                              XmNtabManagedName,NULL,
			      XmNtabManagedWidget,page->pdata->form,
			      NULL);
		NgXAppFreeXmString(go->go.appmgr,xmname);

                if (! tab->managed) {
                        XtManageChild(tab->tab);
                        tab->managed = True;
                }
                page->tab = tab;
	}

        for (i = pane->pagecount; i < pane->tabcount; i++) {
                tab = XmLArrayGet(pane->tablist,i);
                XtVaSetValues(tab->tab,
                              XmNtabManagedName,"BogusWidget",
                              NULL);
        }
        if (mode == _REMOVE) {
                if (pane->active_pos == pane->remove_pos)
                        pos = 0;
                else if (pane->active_pos < pane->remove_pos)
                        pos = MAX(0,pane->active_pos);
                else
                        pos = MAX(0,pane->active_pos-1);
        }
        else if (pane->pagecount > 1) {
                int tmppos = (pos == 0) ? pane->pagecount-1 : 0;
                
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
                fprintf(stderr,"setting tab %d active for pane %d\n",
                        pane->pagecount-1,PaneNumber(go,pane));
#endif
                XmLFolderSetActiveTab(pane->folder,tmppos,True);
        }
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
	fprintf(stderr,
		"setting tab %d active for pane %d\n",
		pos,PaneNumber(go,pane));
#endif
        XmLFolderSetActiveTab(pane->folder,pos,True);
        pane->active_pos = pos;
        
        for (i = pane->pagecount; i < pane->tabcount; i++) {
                tab = XmLArrayGet(pane->tablist,i);
                XtUnmanageChild(tab->tab);
                tab->managed = False;
        }
/*
 * find the maximum tab x positions
 */
        tab = XmLArrayGet(pane->tablist,pane->pagecount-1);

        XtVaGetValues(tab->tab,
                      XmNwidth,&tab_width,
                      XmNx,&tab_x,
                      NULL);
        
        pane->max_tab_xtnt = tab_x + tab_width;
        
	return;
}


static int
InsertPage
(
        brPane		*pane,
        brPage		*page
        )
{
        int startpos = -1,endpos;
        int i,pos = 0;
        brPage *pp;
        
        if (! pane->pagelist) {
                pane->pagelist = XmLArrayNew(0,0);
                XmLArrayAdd(pane->pagelist,pos,1);
		pane->pagecount++;
                XmLArraySet(pane->pagelist,pos,page);
                return pos;
        }

        switch (page->type) {
            case _brREGVAR:
                    XmLArrayAdd(pane->pagelist,pos,1);
		    pane->pagecount++;
                    XmLArraySet(pane->pagelist,pos,page);
                    break;
            case _brHLUVAR:
                    XmLArrayAdd(pane->pagelist,pos,1);
		    pane->pagecount++;
                    XmLArraySet(pane->pagelist,pos,page);
                    break;
            case _brFILEREF:
                    XmLArrayAdd(pane->pagelist,pos,1);
		    pane->pagecount++;
                    XmLArraySet(pane->pagelist,pos,page);
                    for (i = 1; i < pane->pagecount; i++) {
                            pp = XmLArrayGet(pane->pagelist,i);
                            if (page->qfile == pp->qfile) {
                                    if (startpos == -1) {
                                            startpos = endpos = i;
                                            continue;
                                    }
                                    endpos = i;
                            }
                            else if (startpos != -1) {
                                    break;
                            }
                    }
                    if (startpos > 1)
                            XmLArrayMove(pane->pagelist,
                                         1,startpos,endpos-startpos+1);
                    break;
            case _brFILEVAR:
                    for (i = 0; i < pane->pagecount; i++) {
                            pp = XmLArrayGet(pane->pagelist,i);
                            if (page->qfile == pp->qfile) {
                                    if (startpos == -1) {
                                            if (pp->type == _brFILEREF)
                                                    pos = 1;
                                            startpos = endpos = i;
                                            continue;
                                    }
                                    endpos = i;
                            }
                            else if (startpos != -1) {
                                    break;
                            }
                    }
                    if (startpos > 0)
                            XmLArrayMove(pane->pagelist,
                                         0,startpos,endpos-startpos+1);
                    XmLArrayAdd(pane->pagelist,pos,1);
		    pane->pagecount++;
                    XmLArraySet(pane->pagelist,pos,page);
        }


        return pos;
}

static void
DeleteVarPage
(
	brPage	*page
)
{
        NrmQuark	qvar,qfile;
	NgGO		go;

	go = (NgGO) page->go;
        qvar = page->qvar;
        qfile = page->qfile;

        switch (page->type) {
            case _brREGVAR:
                    UpdatePanes(go,_brREGVAR,qvar,NULL,True);
                    break;
            case _brFILEREF:
                    UpdatePanes(go,_brFILEREF,NULL,qfile,True);
                    break;
            case _brFILEVAR:
                    UpdatePanes(go,_brFILEVAR,qvar,qfile,True);
                    break;
            case _brHLUVAR:
                    UpdatePanes(go,_brHLUVAR,qvar,NULL,True);
                    break;
        }
}

static void
VarDeleteCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgGO go = (NgGO) udata.ptrval;
        NgNclAny node = (NgNclAny)cbdata.ptrval;

        printf("deleting %s\n", node->name);
        UpdatePanes(go,_brREGVAR,NrmStringToQuark(node->name),NULL,True);
                
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
	NgBrowse	browse = (NgBrowse)go;
        NgNclAny	node = (NgNclAny)cbdata.ptrval;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        brPane		*pane;
        brPage		*page;
        int		i,j;
        NrmQuark	qfile;

        printf("deleting %s\n", node->name);
        qfile = NrmStringToQuark(node->name);
         
        for (i = 0; i < pcp->alloc_count; i++) {
                pane = pcp->panes[i];
                for (j = 0; j < pane->pagecount; ) {
                        page = XmLArrayGet(pane->pagelist,j);
                        if (page->qfile == qfile) {
                                if (page->type == _brFILEREF)
                                        UpdatePanes(go,_brFILEREF,
                                                    NULL,qfile,True);
                                else if (page->type == _brFILEVAR)
                                        UpdatePanes(go,_brFILEVAR,
                                                    page->qvar,qfile,True);
                                continue;
                        }
                        j++;
                }
        }

	return;
}

static void
HluVarDeleteCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgGO go = (NgGO) udata.ptrval;
        NgNclAny node = (NgNclAny)cbdata.ptrval;

        printf("deleting %s\n", node->name);
        
        UpdatePanes(go,_brHLUVAR,NrmStringToQuark(node->name),NULL,True);
        
	return;
}

static brPage *AddPage
(
	NgGO		go,
        brPane		*pane,
        brPageType	type,
        NrmQuark	qvar,
        NrmQuark	qfile,
        brPage		*copy_page
        )
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        char		*e_text;
        brPage		*page;
        int		pos;
        Dimension	tab_height;
        Dimension	w,h;
        
        page = NhlMalloc(sizeof(brPage));
        if (!page) {
                e_text = "%s: dynamic memory allocation error";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                return;
        }
        page->go = go;
        page->type = type;
        page->qvar = qvar;
        page->qfile = qfile;
        page->tab = NULL;
        
        if (copy_page) /* assumes the 'copy_page' will be deleted */
                page->id = copy_page->id;
        else
                page->id = ++CurrentPageId;
        
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
        fprintf(stderr,"AddPage: %x -- pane %d %s\n",
               page,PaneNumber(go,pane),PageString(pane,page));
#endif
        
        if (! pane->has_folder)
		CreateFolder(go,pane);

        XtVaGetValues(XtParent(pane->form),
                      XmNheight,&h,
                      XmNwidth,&w,
                      NULL);
#if DEBUG_DATABROWSER
        fprintf(stderr,"clip window w,h: %d,%d\n",w,h);
#endif

        switch (type) {
        case _brREGVAR:
		page->pdata = NgGetVarPage(go,pane,page,copy_page);
		break;
        case _brFILEVAR:
		page->pdata = NgGetVarPage(go,pane,page,copy_page);
                break;
        case _brFILEREF:
		page->pdata = NgGetFileRefPage(go,pane,page,copy_page);
                break;
        case _brHLUVAR:
		page->pdata = NgGetHluPage(go,pane,page,copy_page);
                break;
        }
        if (!page->pdata) {
                e_text = "%s: dynamic memory allocation error";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                return NULL;
        }
        page->pdata->pane = pane;
	
        pos = InsertPage(pane,page);
	
        UpdateTabs(go,pane,pos,_ADD);
        
	(*page->pdata->adjust_page_geo)(page);

        XmScrollVisible(pane->scroller,page->tab->tab,9,9);
        
	(*page->pdata->adjust_page_geo)(page);

        return page;
}

static brPage *
RemovePage
(
	NgGO		go,
        brPane		*pane,
        int		pos
        )
{
        brPage	*page = XmLArrayGet(pane->pagelist,pos);

        XmLArrayDel(pane->pagelist,pos,1);
        pane->pagecount--;

        return page;
}

static void DeletePage(
	NgGO		go,
        brPane		*pane,
        int		pos
        )
{
	brPage *page = RemovePage(go,pane,pos);

	if (pane->pagecount == 0) {
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
                fprintf(stderr,"DeletePage: %x -- pane %d %s\n",
                       page,PaneNumber(go,pane),PageString(pane,page));
#endif
                page->pdata->in_use = False;
                NclFreeDataList(page->pdata->dl);
                page->pdata->dl = NULL;
                NhlFree(page);
		DestroyFolder(go,pane);
		return;
	}

	UpdateTabs(go,pane,-1,_REMOVE);

#if	DEBUG_DATABROWSER & DEBUG_FOLDER
        fprintf(stderr,"DeletePage: %x -- pane %d %s\n",
               page,PaneNumber(go,pane),PageString(pane,page));
#endif
        if (page->pdata->deactivate_page)
                (*page->pdata->deactivate_page)(page);
        page->pdata->in_use = False;
        NclFreeDataList(page->pdata->dl);
        page->pdata->dl = NULL;

        NhlFree(page);
        
        return;
}
        
        
static void
ShufflePage
(
	NgGO		go,
        brPane		*pane,
        int		pos
        )
{
        brPage	*page;
        int	newpos;
        
        page = RemovePage(go,pane,pos);

        newpos = InsertPage(pane,page);

        UpdateTabs(go,pane,newpos,_SHUFFLE);
        
        XmScrollVisible(pane->scroller,page->tab->tab,9,9);

	(*page->pdata->adjust_page_geo)(page);

        return;
}

static brPage *
UpdatePanes
(
	NgGO		go,
        brPageType	type,
        NrmQuark	qvar,
        NrmQuark	qfile,
	NhlBoolean	delete
)
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        NhlBoolean	page_found = False;
        int 		i,j,pos = -1;
        brPane		*delete_pane = NULL;
        brPage		*page = NULL,*copy_page = NULL;

        for (i = 0; i < pcp->alloc_count; i++) {
                brPane	*this_pane = pcp->panes[i];

                for (j = 0; j < this_pane->pagecount; j++) {
                        page = XmLArrayGet(this_pane->pagelist,j);
                        this_pane->remove_pos = -1;
			if (page->qvar == qvar && page->qfile == qfile) {
                                if (! delete && this_pane == CurrentPane(go)) {
                                        page_found = True;
                                        ShufflePage(go,this_pane,j);
                                }
                                else {
                                        this_pane->remove_pos = j;
                                        delete_pane = this_pane;
                                        copy_page = page;
                                }
                                break;
                        }
                }
        }
	if (! delete) {
		if (! page_found) {
                	page = AddPage
                                (go,CurrentPane(go),type,qvar,qfile,copy_page);
        	}
                if (delete_pane)
                        DeletePage(go,delete_pane,delete_pane->remove_pos);
                SetTabFocus(go,CurrentPane(go),page);
        	NextPane(go);
	}
        else if (delete_pane)
                DeletePage(go,delete_pane,delete_pane->remove_pos);
        
        return page;
}

typedef struct _timer_data 
{
        NgGO go;
        NrmQuark qvar;
        brPageType type;
} timer_data;

static void BrowseTimeoutCB 
(
	XtPointer	data,
        XtIntervalId	*timer
        )
{
        timer_data	*tdata = (timer_data *)data;
	NgGO		go = (NgGO) tdata->go;
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
	NrmQuark	qvar = tdata->qvar;

        switch (tdata->type) {
            case _brREGVAR:
                    UpdatePanes(go,_brREGVAR,qvar,NULL,False);
                    break;
            case _brFILEREF:
                    UpdatePanes(go,_brFILEREF,NULL,qvar,False);
                    break;
            case _brFILEVAR:
                    UpdatePanes(go,_brFILEVAR,
                                qvar,np->vmenus->qfile,False);
                    break;
            case _brHLUVAR:
                    UpdatePanes(go,_brHLUVAR,qvar,NULL,False);
                    break;
        }
        return;

}
        
static void BrowseHluCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) udata;
	NgBrowse	browse = (NgBrowse)udata;
	NgBrowsePart	*np = &browse->browse;
	NrmQuark	qvar;
        brPage		*page;
        brPane		*pane;
        static timer_data tdata;
        
#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"BrowseVarCB(IN)\n");
#endif
	XtVaGetValues(w,
		      XmNuserData,&qvar,
		      NULL);

#if	DEBUG_DATABROWSER & DEBUG_FOLDER
	fprintf(stderr,"browsing var %s\n", NrmQuarkToString(qvar));
#endif

        tdata.go = go;
        tdata.qvar = qvar;
        tdata.type = _brHLUVAR;
        
        XtAppAddTimeOut(go->go.x->app,50,BrowseTimeoutCB,&tdata);
        
	return;
}
        
static void BrowseVarCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) udata;
	NgBrowse	browse = (NgBrowse)udata;
	NgBrowsePart	*np = &browse->browse;
	NrmQuark	qvar;
        brPage		*page;
        brPane		*pane;
        static timer_data tdata;
        
#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"BrowseVarCB(IN)\n");
#endif
	XtVaGetValues(w,
		      XmNuserData,&qvar,
		      NULL);

#if	DEBUG_DATABROWSER & DEBUG_FOLDER
	fprintf(stderr,"browsing var %s\n", NrmQuarkToString(qvar));
#endif

        tdata.go = go;
        tdata.qvar = qvar;
        tdata.type = _brREGVAR;
        
        XtAppAddTimeOut(go->go.x->app,50,BrowseTimeoutCB,&tdata);
        
	return;
}

static void BrowseFileCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) udata;
	NgBrowse	browse = (NgBrowse)udata;
	NgBrowsePart	*np = &browse->browse;
	NrmQuark	qfile;
        brPage		*page;
        brPane		*pane;
        static timer_data tdata;

#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"BrowseFileCB(IN)\n");
#endif
	XtVaGetValues(w,
		      XmNuserData,&qfile,
		      NULL);

#if	DEBUG_DATABROWSER & DEBUG_FOLDER
	fprintf(stderr,"browsing file %s\n", NrmQuarkToString(qfile));
#endif
        
        tdata.go = go;
        tdata.qvar = qfile;
        tdata.type = _brFILEREF;
        
        XtAppAddTimeOut(go->go.x->app,50,BrowseTimeoutCB,&tdata);

	return;
}

static void BrowseFileVarCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) udata;
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
	NrmQuark	qvar;
        brPage		*page;
        brPane		*pane;
        static timer_data tdata;

#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"BrowseFileVarCB(IN)\n");
#endif
	XtVaGetValues(w,
		      XmNuserData,&qvar,
		      NULL);
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
	fprintf(stderr,"browsing filevar %s in file %s\n",
               NrmQuarkToString(qvar),
               NrmQuarkToString(np->vmenus->qfile));
#endif

        tdata.go = go;
        tdata.qvar = qvar;
        tdata.type = _brFILEVAR;
        
        XtAppAddTimeOut(go->go.x->app,50,BrowseTimeoutCB,&tdata);
        
	return;
}

static void BrowseHluVarCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) udata;
	NgBrowse	browse = (NgBrowse)udata;
	NgBrowsePart	*np = &browse->browse;
	NrmQuark	qvar;
        brPage		*page;
        brPane		*pane;
        static timer_data tdata;

#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"BrowseFileCB(IN)\n");
#endif
	XtVaGetValues(w,
		      XmNuserData,&qvar,
		      NULL);

#if	DEBUG_DATABROWSER & DEBUG_FOLDER
	fprintf(stderr,"browsing file %s\n", NrmQuarkToString(qfile));
#endif
        
        tdata.go = go;
        tdata.qvar = qvar;
        tdata.type = _brHLUVAR;
        
        XtAppAddTimeOut(go->go.x->app,50,BrowseTimeoutCB,&tdata);

	return;
}

static NgVarMenus
CreateVarMenus
(
	NgGO	go,
        Widget	parent
)
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        Widget		menubar,label;

        label = XtVaCreateManagedWidget
                ("Browse:",xmLabelGadgetClass,
                 parent,
                 XmNrightAttachment,	XmATTACH_NONE,
                 NULL);
        
	menubar = XtVaCreateManagedWidget
                ("menubar",xmRowColumnWidgetClass,
                 parent,
                 XmNrowColumnType,	XmMENU_BAR,
                 XmNleftAttachment,	XmATTACH_WIDGET,
                 XmNleftWidget,		label,
                 XmNleftOffset,		15,
                 XmNrightAttachment,	XmATTACH_NONE,
                 NULL);
        return NgCreateVarMenus(browse->go.appmgr,
                                np->nsid,menubar,
				BrowseHluCB,
                                BrowseVarCB,
                                BrowseFileCB,
                                BrowseFileVarCB,
                                browse);
}
static void
AdjustPaneGeometry
(
	NgGO	go
	)
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
	int i;

#if DEBUG_DATABROWSER
	fprintf(stderr,"in adjust pane geometry\n");
#endif

	for (i = 0; i < pcp->current_count; i++) {
		brPane	*pane = pcp->panes[i];
		brPage	*page;


		if (! pane->has_folder || ! pane->pagelist)
			continue;
		page = XmLArrayGet(pane->pagelist,pane->active_pos);
		if (!page) {
#if DEBUG_DATABROWSER
			fprintf(stderr,"error retrieving active page\n");
#endif
			return;
		}
		(*page->pdata->adjust_page_geo)(page);
	}
	return;
}

static void PaneCtrlCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) udata;
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        char		buf[10];
        
#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"PaneCtrlCB(IN)\n");
#endif
        
        if (w == pcp->vcr->reverse) {
                RemovePane(go);
        }
        else if (w == pcp->vcr->forward) {
                AddPane(go);
        }

        sprintf(buf,"%d",pcp->current_count);
        XtVaSetValues(pcp->text,
                      XmNvalue,buf,
                      NULL);

#if	DEBUG_DATABROWSER & DEBUG_FOLDER
	fprintf(stderr,"in_use %d current_ix %d alloc %d\n",
                pcp->current_count,pcp->current_ix,pcp->alloc_count);
#endif
        AdjustPaneGeometry(go);
        
	return;
}

static void DeleteSelectionCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) udata;
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        brPage		*page;
        brPane		*pane;
        NrmQuark	qvar,qfile;
        int		i;
        
#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"CycleSelectionCB(IN)\n");
#endif
        if (! pcp->focus_pane || pcp->focus_pos < 0
            || pcp->focus_pos >= pcp->focus_pane->pagecount)
                return;
        page = XmLArrayGet(pcp->focus_pane->pagelist,pcp->focus_pos);

	DeleteVarPage(page);
        
        return;
}

static void CycleSelectionCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgGO		go = (NgGO) udata;
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        brPage		*page;
        brPane		*pane;
        NrmQuark	qvar,qfile;
        int		i;
        
#if	DEBUG_DATABROWSER & DEBUG_ENTRY
	fprintf(stderr,"CycleSelectionCB(IN)\n");
#endif
        if (! pcp->focus_pane || pcp->focus_pos < 0
            || pcp->focus_pos >= pcp->focus_pane->pagecount)
                return;
        page = XmLArrayGet(pcp->focus_pane->pagelist,pcp->focus_pos);
        qvar = page->qvar;
        qfile = page->qfile;

        for (i = 0; i < pcp->current_count; i++) {
                if (pcp->panes[i] == pcp->focus_pane) {
                        pcp->current_ix = i;
                        break;
                }
        }
        NextPane(go);

        switch (page->type) {
            case _brREGVAR:
                    UpdatePanes(go,_brREGVAR,qvar,NULL,False);
                    break;
            case _brFILEREF:
                    UpdatePanes(go,_brFILEREF,NULL,qfile,False);
                    break;
            case _brFILEVAR:
                    UpdatePanes(go,_brFILEVAR,qvar,qfile,False);
                    break;
            case _brHLUVAR:
                    UpdatePanes(go,_brHLUVAR,qvar,qfile,False);
                    break;
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
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;

#if	DEBUG_DATABROWSER & DEBUG_FOLDER
        fprintf(stderr,"EH height %d width %d\n",
                event->xconfigure.height,
                event->xconfigure.width);
#endif
        
	if(event->type == ConfigureNotify) {
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
                fprintf(stderr,"ConfigureNotify\n");
#endif
		if (! np->mapped)
			return;
        }
        else if (event->type == MapNotify) {
                np->mapped = True;
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
                fprintf(stderr,"MapNotify\n");
#endif
        }
	else {
		return;
	}

	AdjustPaneGeometry(go);

	return;
}

static void
SetupPaneControl
(
	NgGO	go,
        Widget	parent
)
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        Widget		label,pb;
        brPaneControl	*pcp = &np->pane_ctrl;
        Pixel		foreground,background;

        label = XtVaCreateManagedWidget
                ("Panes:",xmLabelGadgetClass,
                 parent,
                 XmNrightAttachment,	XmATTACH_NONE,
                 NULL);
        
        pcp->alloc_count = 0;
        pcp->current_count = 0;
        pcp->current_ix = 0;
        pcp->focus_pane = NULL;
        pcp->focus_pos = 0;

        pcp->text = XtVaCreateManagedWidget
                ("PaneCount",xmTextFieldWidgetClass,
                 parent,
                 XmNleftAttachment,	XmATTACH_WIDGET,
                 XmNleftWidget,		label,
                 XmNleftOffset,		15,
                 XmNrightAttachment,	XmATTACH_NONE,
                 XmNvalue,		"1",
                 XmNcolumns,		2,
                 XmNeditable,		False,
                 XmNresizeWidth,	True,
                 XmNtopOffset,		5,
                 XmNbottomOffset,	5,
                 NULL);
        
        pcp->vcr = NgCreateVcrControl
                (go,parent,20,False,False,False,True,False,True,False,False);
        
        XtVaSetValues
                (pcp->vcr->form,
                 XmNleftAttachment,	XmATTACH_WIDGET,
                 XmNleftWidget,		pcp->text,
                 XmNrightAttachment,	XmATTACH_NONE,
                 NULL);
        
        XtAddCallback(pcp->vcr->reverse,XmNactivateCallback,PaneCtrlCB,go);
        XtAddCallback(pcp->vcr->forward,XmNactivateCallback,PaneCtrlCB,go);

        label = XtVaCreateManagedWidget
                ("Page:",xmLabelGadgetClass,
                 parent,
                 XmNleftAttachment,	XmATTACH_WIDGET,
                 XmNleftWidget,		pcp->vcr->form,
                 XmNrightAttachment,	XmATTACH_NONE,
                 NULL);

        pb = XtVaCreateManagedWidget
                ("Cycle",xmPushButtonWidgetClass,
                 parent,
                 XmNleftOffset,		15,
                 XmNleftAttachment,	XmATTACH_WIDGET,
                 XmNleftWidget,		label,
                 XmNrightAttachment,	XmATTACH_NONE,
                 XmNtopOffset,		5,
                 XmNbottomOffset,	5,
                 NULL);
        XtAddCallback(pb,XmNactivateCallback,CycleSelectionCB,go);


        pb = XtVaCreateManagedWidget
                ("Delete",xmPushButtonWidgetClass,
                 parent,
                 XmNleftOffset,		15,
                 XmNleftAttachment,	XmATTACH_WIDGET,
                 XmNleftWidget,		pb,
                 XmNrightAttachment,	XmATTACH_NONE,
                 XmNtopOffset,		5,
                 XmNbottomOffset,	5,
                 NULL);
        XtAddCallback(pb,XmNactivateCallback,DeleteSelectionCB,go);

#if 0        
        XtVaGetValues(pb,
                      XmNforeground,&foreground,
                      XmNbackground,&background,
                      NULL);
        
        XtVaSetValues(pb,
                      XmNforeground,background,
                      XmNbackground,foreground,
                      NULL);
#endif        
        return;
}

static NhlBoolean
BrowseCreateWin
(
	NgGO	go
)
{
	char		func[]="BrowseCreateWin";
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        Widget		datamenubar;
	Widget		form,sep;
        brPane		*pane;
        XmString	xmstring;
	NhlArgVal	sel,user_data;
	NhlLayer	ncl = _NhlGetLayer(np->nsid);
                
	np->mapped = False;
	XtAppAddActions(go->go.x->app,
                        browseactions,NhlNumber(browseactions));

        XtAddEventHandler(go->go.manager,StructureNotifyMask,
                          False,ChangeSizeEH,(XtPointer)go);

	_NgGOSetTitle(go,"Browse:",NULL);
	_NgGOCreateMenubar(go);
        form = XtVaCreateManagedWidget
                ("form",xmFormWidgetClass,
                 go->go.manager,
                 XmNtopAttachment,	XmATTACH_WIDGET,
                 XmNtopWidget,		go->go.menubar,
                 XmNbottomAttachment,	XmATTACH_NONE,
                 NULL);
        np->vmenus = CreateVarMenus(go,form);

        form = XtVaCreateManagedWidget
                ("form",xmFormWidgetClass,
                 go->go.manager,
                 XmNtopAttachment,	XmATTACH_WIDGET,
                 XmNtopWidget,		np->vmenus,
                 XmNbottomAttachment,	XmATTACH_NONE,
                 NULL);
        SetupPaneControl(go,form);
        
        sep = XtVaCreateManagedWidget
                ("sep",xmSeparatorGadgetClass,
                 go->go.manager,
                 XmNtopAttachment,	XmATTACH_WIDGET,
                 XmNtopWidget,	form,
                 XmNbottomAttachment,	XmATTACH_NONE,
                 NULL);

	np->paned_window = XtVaCreateManagedWidget
                ("paned_window",xmPanedWindowWidgetClass,
                 go->go.manager,
                 XmNallowResize,	True,
                 XmNtopAttachment,	XmATTACH_WIDGET,
                 XmNtopWidget,		sep,
                 NULL);


        pane = AddPane(go);
        
	NhlINITVAR(sel);
	NhlINITVAR(user_data);
	user_data.ptrval = go;
        sel.lngval = NgNclCBDELETE_VAR;
	_NhlAddObjCallback(ncl,NgCBnsObject,sel,VarDeleteCB,user_data);
        sel.lngval = NgNclCBDELETE_FILEVAR;
	_NhlAddObjCallback(ncl,NgCBnsObject,sel,FileRefDeleteCB,user_data);
        sel.lngval = NgNclCBDELETE_HLUVAR;
	_NhlAddObjCallback(ncl,NgCBnsObject,sel,HluVarDeleteCB,user_data);
	return True;
}

extern NgPageId NgOpenPage(
        int		goid,
        brPageType	type,
        NrmQuark	*qname,
        int		qcount
        )
{
        NgGO		go = (NgGO)_NhlGetLayer(goid);
        brPage		*page;
        
        switch (type) {
            case _brREGVAR:
                    if (qcount < 1 || qname[0] == NrmNULLQUARK)
                            return NULL;
                    page = UpdatePanes(go,_brREGVAR,qname[0],NULL,False);
                    break;
            case _brFILEREF:
                    if (qcount < 1 || qname[0] == NrmNULLQUARK)
                            return NULL;
                    page = UpdatePanes(go,_brFILEREF,NULL,qname[0],False);
                    break;
            case _brFILEVAR:
                    if (qcount < 2
                        || qname[0] == NrmNULLQUARK
                        || qname[1] == NrmNULLQUARK)
                            return NULL;
                    page = UpdatePanes(go,_brFILEVAR,qname[0],qname[1],False);
                    break;
            case _brHLUVAR:
                    if (qcount < 1 || qname[0] == NrmNULLQUARK)
                            return NULL;
                    page = UpdatePanes(go,_brHLUVAR,qname[0],NULL,False);
                    break;
        }
        return page->id;

}

static brPage *GetPageReference
(
        NgGO	go,
        NgPageId	id
        )
{
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        brPane		*pane;
        brPage		*page;
        int		i,j;
         
        for (i = 0; i < pcp->alloc_count; i++) {
                pane = pcp->panes[i];
                for (j = 0; j < pane->pagecount; j++) {
                        page = XmLArrayGet(pane->pagelist,j);
                        if (page->id == id)
                                return page;
                }
        }
        return NULL;
}

extern void NgPageOutputNotify(
        int		goid,
        NgPageId	page_id,
        brPageType	output_page_type,
        NhlPointer	output_data
        )
{
        NgGO		go = (NgGO)_NhlGetLayer(goid);
        brPage		*page = GetPageReference(go,page_id);

        if (! page)
                return;
        if (! page->pdata->page_input_notify)
                return;
        
        (*page->pdata->page_input_notify)(page,output_page_type,output_data);

        return;
}

        
extern NhlPointer NgPageData(
        int		goid,
        NgPageId	page_id
        )
{
        NgGO		go = (NgGO)_NhlGetLayer(goid);
        brPage		*page = GetPageReference(go,page_id);

        if (! page)
                return NULL;
        if (! page->pdata->public_page_data)
                return NULL;

        return ((*page->pdata->public_page_data)(page));

}

extern NhlErrorTypes NgUpdatePage
(
        int		goid,
        NgPageId	page_id
        )        
{
        NgGO		go = (NgGO)_NhlGetLayer(goid);
        brPage		*page = GetPageReference(go,page_id);

        if (! page)
                return NhlFATAL;
        if (! page->pdata->page_input_notify)
                return NhlFATAL;

        return ((*page->pdata->update_page)(page));

}

extern int
NgGetPageId
(
        int		goid,
        NrmQuark	qsym1,
        NrmQuark	qsym2
        )
{
        NgGO		go = (NgGO)_NhlGetLayer(goid);
	NgBrowse	browse = (NgBrowse)go;
	NgBrowsePart	*np = &browse->browse;
        brPaneControl	*pcp = &np->pane_ctrl;
        brPane		*pane;
        brPage		*page;
        int		i,j;
         
        for (i = 0; i < pcp->alloc_count; i++) {
                pane = pcp->panes[i];
                for (j = 0; j < pane->pagecount; j++) {
                        page = XmLArrayGet(pane->pagelist,j);
                        if (qsym2 > NrmNULLQUARK) 
                            if (page->qvar == qsym1 && page->qfile == qsym2)
                                    return page->id;
                        else if (page->qvar == qsym1)
                                return page->id;
                }
        }
        return NgNoPage;
}


static void TabFocusAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgGO	go;
        
#if	DEBUG_DATABROWSER & DEBUG_FOLDER
	fprintf(stderr,"TabFocusAction(IN)\n");
#endif
        
	return;
}
