/*
 *      $Id: createmenu.c,v 1.1 1997-10-03 20:07:55 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		createmenu.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 15 13:49:25 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/createmenuP.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/browse.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushBG.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/TextF.h>
#include  <Xm/Form.h>
#include  <Xm/LabelG.h>

#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/DataItem.h>
#include <ncarg/hlu/ResourcesP.h>

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

static void CreateCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	CreateMenuRec	*priv = (CreateMenuRec	*)udata;
	NgCreateMenu	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
        NrmQuark	qname;
        char		*vartext;
        NgPageId	page_id;
        NgHluPage	*hlu_page;
        int		browse_id = NhlDEFAULT_APP;
        XmAnyCallbackStruct	*xmcb = (XmAnyCallbackStruct*)cb_data;
        NgGO		browse;
        char		buf[256];
        NhlString 	varname;
        
        NgAppEnumerateGO(priv->go->go.appmgr,GetBrowser,&browse_id);
        browse = (NgGO) _NhlGetLayer(browse_id);
        if (!browse) {
                XtCallActionProc(w,"browseWindow",xmcb->event,NULL,0);
                NgAppEnumerateGO(priv->go->go.appmgr,GetBrowser,&browse_id);
                browse = (NgGO) _NhlGetLayer(browse_id);
        }
        
        printf("in create cb\n");
        XtVaGetValues(priv->dialog_text,
                      XmNvalue,&vartext,
                      NULL);
                
        varname = NgNclGetSymName(priv->go->go.nclstate,vartext,False);
                
/* create the NCL graphic variable using this name now
 * in order that it won't be "stolen" before the hlu
 * object actually gets created.
 */
                
        sprintf(buf,"%s = new(1,graphic)\n",varname);
        (void)NgNclSubmitBlock(priv->go->go.nclstate,buf);

        qname = NrmStringToQuark(vartext);
        page_id = NgOpenPage(browse_id,_brHLUVAR,&qname,1);
        if (page_id <= NgNoPage) {
                printf("unable to open hlu page\n");
                return;
        }
        hlu_page = (NgHluPage *)NgPageData(browse_id,page_id);
        if (! hlu_page) {
                printf("unable to get public page data\n");
                return;
        }
        hlu_page->class_name = priv->create_class->base_class.class_name;
        hlu_page->data_info = NULL;
                
        if (NgUpdatePage(browse_id,page_id) < NhlWARNING) {
                printf("error updating hlu page\n");
                return;
        }
        if (! browse->go.up)
                NgGOPopup(browse_id);
        
        NgPageOutputNotify(browse_id,page_id,_brNULL,NULL);
        
        return;
}
static void GetClassPrefix
(
        NhlClass	class,
        NhlString	prefix
        )
{
        int i;

        if (!strcmp(class->base_class.class_name,"logLinPlotClass")) {
                sprintf(prefix,"ll");
                return;
        }
        else if (!strcmp(class->base_class.class_name,"irregularPlotClass")) {
                sprintf(prefix,"ir");
                return;
        }
        else if (!strcmp(class->base_class.class_name,
                         "ncgmWorkstationClass")) {
                sprintf(prefix,"ncgmWk");
                return;
        }
        else if (!strcmp(class->base_class.class_name,"psWorkstationClass")) {
                sprintf(prefix,"psWk");
                return;
        }
        else if (!strcmp(class->base_class.class_name,"xWorkstationClass")) {
                sprintf(prefix,"xWk");
                return;
        }
        if (class->base_class.class_inited) {
                for (i = 0; i < class->base_class.num_resources; i++) {
                        NhlString resstring;
                        NrmResource *res = (NrmResource *)
                                &class->base_class.resources[i];
                        if (res->res_info & _NhlRES_INTERCEPTED ||
                            res->res_info & _NhlRES_PRIVATE ||
                            res->nhlclass != class)
                                continue;
                        resstring = NrmQuarkToString(res->nrm_name);
                        prefix[0] = resstring[0];
                        prefix[1] = resstring[1];
                        if (isupper(resstring[2])) {
                                prefix[2] = '\0';
                                return;
                        }
                        prefix[2] = resstring[2];
                        prefix[3] = '\0';
                        return;
                }
        }
        else {
                for (i = 0; i < class->base_class.num_resources; i++) {
                        NhlResource *res = &class->base_class.resources[i];
                        if (res->res_info & _NhlRES_INTERCEPTED ||
                            res->res_info & _NhlRES_PRIVATE)
                                continue;
                        prefix[0] = res->resource_name[0];
                        prefix[1] = res->resource_name[1];
                        if (isupper(res->resource_name[2])) {
                                prefix[2] = '\0';
                                return;
                        }
                        prefix[2] = res->resource_name[2];
                        prefix[3] = '\0';
                        return;
                }
        }
        return;
}

static void CreateHluDialog
(
        CreateMenuRec	*priv,
        NhlClass	class
       )
{
	Arg	args[50];
	int	nargs;
	NgCreateMenu	*pub = &priv->public;
	char    buf[128] = "",prefix[8];
        char    *cp;
        XmString xmname;
        Widget  form,label,help;

        priv->create_class = class;
        
        sprintf(buf,"Create %s",class->base_class.class_name);
        cp = strstr(buf,"Class"); /* remove the word class */
        *cp = '\0';
        
        xmname = NgXAppCreateXmString
                (priv->go->go.appmgr,buf);
        
        GetClassPrefix(class,prefix);
        sprintf(buf,"%s_obj",prefix);

        nargs = 0;
	XtSetArg(args[nargs],XmNdialogTitle,xmname);nargs++;
        if (! priv->create_dialog) {
                priv->create_dialog = XmCreateMessageDialog
                        (priv->parent,"CreateDialog",args,nargs);
                help = XmMessageBoxGetChild
                        (priv->create_dialog,XmDIALOG_HELP_BUTTON);
                XtUnmanageChild(help);
		XtAddCallback(priv->create_dialog,
			      XmNokCallback,CreateCB,priv);
		form = XtVaCreateManagedWidget
                        ("form",xmFormWidgetClass,
                         priv->create_dialog,
                         NULL);
                label = XtVaCreateManagedWidget
                        ("Name",xmLabelGadgetClass,
                         form,
                         XmNrightAttachment,XmATTACH_NONE,
                         NULL);
                priv->dialog_text = XtVaCreateManagedWidget
                        ("dialog",xmTextFieldWidgetClass,
                         form,
                         XmNleftAttachment,XmATTACH_WIDGET,
                         XmNleftWidget,label,
                         XmNvalue,
                         NgNclGetSymName(priv->go->go.nclstate,buf,True),
                         XmNresizeWidth,True,
                         NULL);
        }
	else {
		XtSetValues(priv->create_dialog,args,nargs);
                XtVaSetValues(priv->dialog_text,
                              XmNvalue,
                              NgNclGetSymName
                              (priv->go->go.nclstate,buf,True),
                              NULL);
	}
	XmStringFree(xmname);
        XtManageChild(priv->create_dialog);
        
        return;
        
}

static void CreateHluDialogCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	CreateMenuRec	*priv = (CreateMenuRec	*)udata;
	NgCreateMenu	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
        NhlClass	class;

        printf("in plot create cb\n");

        XtVaGetValues(w,
                      XmNuserData,&class,
                      NULL);
        CreateHluDialog(priv,class);
        return;
        
}

static void WorkstationMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	CreateMenuRec	*priv = (CreateMenuRec	*)udata;
	NgCreateMenu	*pub = &priv->public;
        NgMenuRec	*wks = &priv->wks;
        int		i,count,wks_count = 0;
        NhlClass	*classes,wks_classes[20];

        printf("in workstation menu cb %s\n",NhlName(priv->go->go.nclstate));

        NhlVAGetValues(priv->go->go.nclstate,
                       NgNnsHluClassCount,&count,
                       NgNnsHluClasses,&classes,
                       NULL);
        
        for (i = 0; i < count; i++) {
                if (NhlClassIsSubclass(classes[i],NhlworkstationClass)) {
                        wks_classes[wks_count] = classes[i];
                        wks_count++;
                }
        }
        if (wks_count > wks->count) {
                wks->buttons = NhlRealloc
                        (wks->buttons,wks_count * sizeof(Widget));
                for (i = 0; i < wks_count; i++) {
                        wks->buttons[i] = XtVaCreateManagedWidget
                                (wks_classes[i]->base_class.class_name,
                                 xmPushButtonGadgetClass,
                                 wks->menu,
                                 XmNuserData,wks_classes[i],
                                 NULL);
                        XtAddCallback(wks->buttons[i],
                                      XmNactivateCallback,CreateHluDialogCB,
                                      priv);
                }
                wks->count = wks_count;
        }
        
        return;
}
static void PlotMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	CreateMenuRec	*priv = (CreateMenuRec	*)udata;
	NgCreateMenu	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
        int		i,count,plot_count = 0;
        NhlClass	*classes,plot_classes[20];

        printf("in plot menu cb\n");

        NhlVAGetValues(priv->go->go.nclstate,
                       NgNnsHluClassCount,&count,
                       NgNnsHluClasses,&classes,
                       NULL);
        
        for (i = 0; i < count; i++) {
                if (NhlClassIsSubclass(classes[i],NhltransformClass)) {
                        plot_classes[plot_count] = classes[i];
                        plot_count++;
                }
        }
        if (plot_count > plot->count) {
                plot->buttons = NhlRealloc
                        (plot->buttons,plot_count * sizeof(Widget));
                for (i = 0; i < plot_count; i++) {
                        plot->buttons[i] = XtVaCreateManagedWidget
                                (plot_classes[i]->base_class.class_name,
                                 xmPushButtonGadgetClass,
                                 plot->menu,
                                 XmNuserData,plot_classes[i],
                                 NULL);
                        XtAddCallback(plot->buttons[i],
                                      XmNactivateCallback,CreateHluDialogCB,
                                      priv);
                }
                plot->count = plot_count;
        }

        return;
}

static void AnnoMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	CreateMenuRec	*priv = (CreateMenuRec	*)udata;
	NgCreateMenu	*pub = &priv->public;
        NgMenuRec	*anno = &priv->anno;
        int		i,count,anno_count = 0;
        NhlClass	*classes,anno_classes[20];

        printf("in anno menu cb\n");

        NhlVAGetValues(priv->go->go.nclstate,
                       NgNnsHluClassCount,&count,
                       NgNnsHluClasses,&classes,
                       NULL);
        
        for (i = 0; i < count; i++) {
                if (NhlClassIsSubclass(classes[i],NhlviewClass)&&
                    !NhlClassIsSubclass(classes[i],NhltransformClass)) {
                        anno_classes[anno_count] = classes[i];
                        anno_count++;
                }
        }
        if (anno_count > anno->count) {
                anno->buttons = NhlRealloc
                        (anno->buttons,anno_count * sizeof(Widget));
                for (i = 0; i < anno_count; i++) {
                        anno->buttons[i] = XtVaCreateManagedWidget
                                (anno_classes[i]->base_class.class_name,
                                 xmPushButtonGadgetClass,
                                 anno->menu,
                                 XmNuserData,anno_classes[i],
                                 NULL);
                        XtAddCallback(anno->buttons[i],
                                      XmNactivateCallback,CreateHluDialogCB,
                                      priv);
                }
                anno->count = anno_count;
        }

        return;
}

static void OtherMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	CreateMenuRec	*priv = (CreateMenuRec	*)udata;
	NgCreateMenu	*pub = &priv->public;
        NgMenuRec	*other = &priv->other;
        int		i,count,other_count = 0;
        NhlClass	*classes,other_classes[20];

        printf("in other menu cb\n");

        NhlVAGetValues(priv->go->go.nclstate,
                       NgNnsHluClassCount,&count,
                       NgNnsHluClasses,&classes,
                       NULL);
        
        for (i = 0; i < count; i++) {
                if (!NhlClassIsSubclass(classes[i],NhlviewClass)&&
                    !NhlClassIsSubclass(classes[i],NhlworkstationClass) &&
                    !NhlClassIsSubclass(classes[i],NhldataItemClass)) {
                        other_classes[other_count] = classes[i];
                        other_count++;
                }
        }
        if (other_count > other->count) {
                other->buttons = NhlRealloc
                        (other->buttons,other_count * sizeof(Widget));
                for (i = 0; i < other_count; i++) {
                        other->buttons[i] = XtVaCreateManagedWidget
                                (other_classes[i]->base_class.class_name,
                                 xmPushButtonGadgetClass,
                                 other->menu,
                                 XmNuserData,other_classes[i],
                                 NULL);
                        XtAddCallback(other->buttons[i],
                                      XmNactivateCallback,CreateHluDialogCB,
                                      priv);
                }
                other->count = other_count;
        }

        return;
}

NhlErrorTypes NgUpdateCreateMenu
(
        NgCreateMenu		*create_menu
        )
{
	NgCreateMenu	*pub = create_menu;
	CreateMenuRec	*priv = (CreateMenuRec	*)pub;
}

NgCreateMenu *
NgCreateCreateMenu
(
        int            	goid,
        Widget		parent
)
{
	CreateMenuRec	*priv;
	NgCreateMenu	*pub;
        Widget		menush;
        NgGO		go = (NgGO)_NhlGetLayer(goid);
        
        if (!go)
                return NULL;
        
        priv = NhlMalloc(sizeof(CreateMenuRec));
        priv->go = go;
        priv->create_dialog = NULL;
        priv->parent = parent;
	pub = &priv->public;
        
        
        priv->plot.count = priv->var.count = priv->data.count =
                priv->wks.count = priv->anno.count = priv->other.count = 0;
        priv->plot.buttons = priv->var.buttons = priv->data.buttons =
                priv->wks.buttons = priv->anno.buttons =
                priv->other.buttons = NULL;

	menush = XtVaCreatePopupShell
                ("override_sh",xmMenuShellWidgetClass,
                 parent,
                 XmNwidth,		5,
                 XmNheight,		5,
                 XmNallowShellResize,	True,
                 XtNoverrideRedirect,	True,
                 NULL);
        
        priv->wks.menu =  XtVaCreateWidget
                ("Workstation",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);
	XtAddCallback(priv->wks.menu,
		      XmNmapCallback,WorkstationMenuCB,priv);
	pub->wks_mbutton = 
                XtVaCreateManagedWidget
                ("Workstation",xmCascadeButtonGadgetClass,
                 parent,
                 XmNsubMenuId,priv->wks.menu,
                 NULL);

        priv->plot.menu =  XtVaCreateWidget
                ("Plot",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);
	XtAddCallback(priv->plot.menu,
		      XmNmapCallback,PlotMenuCB,priv);
	pub->plot_mbutton = 
                XtVaCreateManagedWidget
                ("Plot",xmCascadeButtonGadgetClass,
                 parent,
                 XmNsubMenuId,priv->plot.menu,
                 NULL);

        priv->anno.menu =  XtVaCreateWidget
                ("Annotation",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);
	XtAddCallback(priv->anno.menu,
		      XmNmapCallback,AnnoMenuCB,priv);
	pub->anno_mbutton = 
                XtVaCreateManagedWidget
                ("Annotation",xmCascadeButtonGadgetClass,
                 parent,
                 XmNsubMenuId,priv->anno.menu,
                 NULL);

        priv->other.menu =  XtVaCreateWidget
                ("Other Hlu",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);
	XtAddCallback(priv->other.menu,
		      XmNmapCallback,OtherMenuCB,priv);
	pub->other_mbutton = 
                XtVaCreateManagedWidget
                ("Other Hlu",xmCascadeButtonGadgetClass,
                 parent,
                 XmNsubMenuId,priv->other.menu,
                 NULL);

        return pub;
        
}

void NgDestroyCreateMenu
(
        NgCreateMenu		*create_menu
        )
{
	NgCreateMenu	*pub = create_menu;
	CreateMenuRec	*priv = (CreateMenuRec	*)pub;

        if (priv->wks.count)
                NhlFree(priv->wks.buttons);
        if (priv->plot.count)
                NhlFree(priv->plot.buttons);
        if (priv->anno.count)
                NhlFree(priv->anno.buttons);
        if (priv->other.count)
                NhlFree(priv->other.buttons);
        
        NhlFree(priv);

        return;
}
