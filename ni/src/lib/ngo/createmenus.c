/*
 *      $Id: createmenus.c,v 1.2 1997-06-20 21:48:23 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		createmenus.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun  6 17:26:33 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/createmenusP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>
#include <ncarg/ngo/browse.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/TextF.h>
#include  <Xm/Form.h>
#include  <Xm/LabelG.h>

static _cmDataReceptor PlotTypes[] = {
	{NgLineContour, ngLINECONTOUR,"cn_obj","contourPlotClass"},
	{NgFillContour, ngFILLCONTOUR,"cn_obj","contourPlotClass"},
	{NgRasterContour, ngRASTERCONTOUR,"cn_obj","contourPlotClass"},
	{NgStreamline, ngSTREAMLINE,"st_obj","streamlinePlotClass"},
	{NgLineVector, ngLINEVECTOR,"vc_obj","vectorPlotClass"},
	{NgFillVector, ngFILLVECTOR,"vc_obj","vectorPlotClass"},
	{NgLineXy, ngLINEXY,"xy_obj","xyPlotClass"},
	{NgScatterXy, ngSCATTERXY,"xy_obj","xyPlotClass"}
};

static _cmDataReceptor DataItemTypes[] = {
	{NgCoordArray, ngCOORDARRAY,"ca_obj","coordArraysClass"},
	{NgScalarField, ngSCALARFIELD,"sf_obj","scalarFieldClass"},
	{NgVectorField, ngVECTORFIELD,"vc_obj","vectorFieldClass"},
	{NgNclVariable, ngNCLVARIABLE,"ncl_var","none"}
};

static void OutputNotify
(
        CreateMenusRec	*priv,
        NgPageId		page
        )
{
	NgCreateMenus	*pub = &priv->public;
        int ndims = pub->vinfo->n_dims;
        int i,size = ndims * sizeof(long);
        brPageType ptype =
                pub->qsymbol > NrmNULLQUARK ? _brFILEVAR : _brREGVAR;

        if (! priv->output) {
                priv->output = NhlMalloc(sizeof(NgVarPageOutput));
                priv->output->ndims = 0;
                priv->output->start =
                        priv->output->finish = priv->output->stride = NULL;
        }
        if (ndims > priv->output->ndims) {
                priv->output->ndims = ndims;
                priv->output->start = NhlRealloc(priv->output->start,size);
                priv->output->finish = NhlRealloc(priv->output->finish,size);
                priv->output->stride = NhlRealloc(priv->output->stride,size);
        }
        memcpy(priv->output->start,pub->start,size);
        memcpy(priv->output->finish,pub->finish,size);
        memcpy(priv->output->stride,pub->stride,size);
        priv->output->qfile = pub->qsymbol;
        priv->output->qvar = pub->vinfo->name;
        
        if (page > NgNoPage) {
                NgPageOutputNotify(priv->go->base.id,page,ptype,priv->output);
                return;
        }
        for (i = 0; i < priv->pagecount; i++) {
                NgPageOutputNotify
                        (priv->go->base.id,
                         priv->page_ids[i],ptype,priv->output);
        }
        return;
}

static void CopyShapedVar
(
        CreateMenusRec	*priv,
        NhlString	varname
        )
{
        char buf[256];
	NgCreateMenus	*pub = &priv->public;
        int i;

        if (pub->qsymbol)
                sprintf(buf,"%s = %s->%s(",NgNclGetSymName(varname,False),
                        NrmQuarkToString(pub->qsymbol),
                        NrmQuarkToString(pub->vinfo->name));
        else
                sprintf(buf,"%s = %s(",NgNclGetSymName(varname,False),
                        NrmQuarkToString(pub->qsymbol),
                        NrmQuarkToString(pub->vinfo->name));
        for (i = 0; i < pub->vinfo->n_dims; i++) 
		sprintf(&buf[strlen(buf)],"%d:%d:%d,",pub->start[i],
			pub->finish[i],pub->stride[i]);
        
        	/* backup 1 to remove last comma */
	sprintf(&buf[strlen(buf)-1],")\n");

	(void)NgNclSubmitBlock(priv->nsid,buf);

        return;
}

static void CreateCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	CreateMenusRec	*priv = (CreateMenusRec	*)udata;
	NgCreateMenus	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
        _cmDataReceptor	*type;
        NrmQuark	qname;
        char		*vartext;
        NgPageId		*page;
        
        printf("in create cb\n");
        XtVaGetValues(w,
                      XmNuserData,&type,
                      NULL);
        XtVaGetValues(priv->dialog_text,
                      XmNvalue,&vartext,
                      NULL);
            /* need to qualify text string, and warn user if it's already
               a symbol */
        if (type->type == ngNCLVARIABLE) {
                CopyShapedVar(priv,vartext);
                return;
        }
        else {
                char buf[256];
                NhlString varname = NgNclGetSymName(vartext,False);
                
                    /* create the NCL graphic variable using this name now
                       in order that it won't be "stolen" before the hlu
                       object actually gets created */
                
                sprintf(buf,"%s = new(1,graphic)\n",varname);
                (void)NgNclSubmitBlock(priv->nsid,buf);

                qname = NrmStringToQuark(vartext);
                priv->page_ids = NhlRealloc
                        (priv->page_ids,sizeof(NgPageId)*(++priv->pagecount));
                
                priv->page_ids[priv->pagecount-1] =
                        NgOpenPage(priv->go->base.id,
                                   _brHLUVAR,&qname,1);
                OutputNotify(priv,priv->page_ids[priv->pagecount-1]);
        }
                
        return;
}

static void CreateDialog
(
       CreateMenusRec	*priv,
       _cmDataReceptor 	*type
       )
{
	Arg	args[50];
	int	nargs;
	NgCreateMenus	*pub = &priv->public;
	char    buf[128] = "";
        XmString xmname;
        Widget  form,label,help;
        
        if (type->type == ngNCLVARIABLE)
                sprintf(buf,"Create Ncl Variable",type->name);
        else
                sprintf(buf,"Create %sPlot",type->name);
        
        xmname = NgXAppCreateXmString(priv->go->go.appmgr,buf);
        printf("%s\n",buf);

        nargs = 0;
	XtSetArg(args[nargs],XmNdialogTitle,xmname);nargs++;
	XtSetArg(args[nargs],XmNuserData,type);nargs++;
        if (! priv->create_dialog) {
                priv->create_dialog = XmCreateMessageDialog
                        (pub->menubar,"CreateDialog",args,nargs);
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
                         XmNvalue,NgNclGetSymName(type->def_name,True),
                         XmNresizeWidth,True,
                         NULL);
        }
	else {
		XtSetValues(priv->create_dialog,args,nargs);
                XtVaSetValues(priv->dialog_text,
                              XmNvalue,NgNclGetSymName(type->def_name,True),
                              NULL);
	}
	XmStringFree(xmname);
        XtManageChild(priv->create_dialog);
        
        return;
        
}

static void CreateDialogCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	CreateMenusRec	*priv = (CreateMenusRec	*)udata;
	NgCreateMenus	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
        _cmDataReceptor	*type;

        printf("in plot create cb\n");

        XtVaGetValues(w,
                      XmNuserData,&type,
                      NULL);
        CreateDialog(priv,type);
        return;
        
}

static void PlotMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	CreateMenusRec	*priv = (CreateMenusRec	*)udata;
	NgCreateMenus	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
        int		i;

        printf("in plot menu cb\n");

        if (plot->count == 0) {
                plot->count = NhlNumber(PlotTypes);
                plot->buttons = NhlMalloc(plot->count * sizeof(Widget));
                for (i = 0; i < plot->count; i++) {
                        plot->buttons[i] = XtVaCreateManagedWidget
                                (PlotTypes[i].name,xmCascadeButtonGadgetClass,
                                 plot->menu,
                                 XmNuserData,&PlotTypes[i],
                                 NULL);
                        XtAddCallback(plot->buttons[i],
                                      XmNactivateCallback,CreateDialogCB,
                                      priv);
                }
        }
        return;
}
NhlErrorTypes NgUpdateCreateMenus
(
        NgCreateMenus		*create_menus
        )
{
	NgCreateMenus	*pub = create_menus;
	CreateMenusRec	*priv = (CreateMenusRec	*)pub;
}

NgCreateMenus *
NgCreateCreateMenus
(
        NgGO            go,
        Widget		parent
)
{
	CreateMenusRec	*priv;
	NgCreateMenus	*pub;
        Widget		menush;

        priv = NhlMalloc(sizeof(CreateMenusRec));
        priv->go = go;
        priv->create_dialog = NULL;
	pub = &priv->public;
        
	NhlVAGetValues(priv->go->go.appmgr,
		NgNappNclState,	&priv->nsid,
		NULL);
        
        priv->pagecount = 0;
        priv->page_ids = NULL;
        priv->output = NULL;
        priv->plot.count = priv->var.count = priv->data.count = 0;
        priv->plot.alloced = priv->var.alloced = priv->data.alloced = 0;
        
        pub->menubar =  XtVaCreateManagedWidget
                ("CreateMenu",xmRowColumnWidgetClass,
                 parent,
                 XmNrowColumnType,      XmMENU_BAR,
                 NULL);

	menush = XtVaCreatePopupShell
                ("override_sh",xmMenuShellWidgetClass,
                 pub->menubar,
                 XmNwidth,		5,
                 XmNheight,		5,
                 XmNallowShellResize,	True,
                 XtNoverrideRedirect,	True,
                 NULL);
        
        priv->plot.menu =  XtVaCreateWidget
                ("Plot",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
		 XmNuserData,	&DataItemTypes[3],
                 NULL);
	XtAddCallback(priv->plot.menu,
		      XmNmapCallback,PlotMenuCB,priv);

	pub->plot_mbutton = 
                XtVaCreateManagedWidget
                ("Plot",xmCascadeButtonGadgetClass,
                 pub->menubar,
                 XmNsubMenuId,	priv->plot.menu,
                 NULL);

        priv->var.menu = XtVaCreateWidget
                ("Variable",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 XmNuserData,&DataItemTypes[3],
                 NULL);
	XtAddCallback(priv->var.menu,
		      XmNmapCallback,CreateDialogCB,priv);

	pub->var_mbutton = 
                XtVaCreateManagedWidget
                ("Variable",xmCascadeButtonGadgetClass,
                 pub->menubar,
                 XmNuserData,&DataItemTypes[3],
                 NULL);
        XtAddCallback(pub->var_mbutton,
                      XmNactivateCallback,CreateDialogCB,
                      priv);
        
	XtManageChild(priv->plot.menu);
	XtManageChild(priv->var.menu);

        return pub;
        
}

void NgDestroyCreateMenus
(
        NgCreateMenus		*create_menus
        )
{
	NgCreateMenus	*pub = create_menus;
	CreateMenusRec	*priv = (CreateMenusRec	*)pub;

        if (priv->plot.count)
                NhlFree(priv->plot.buttons);
        if (priv->output) {
                NhlFree(priv->output->start);
                NhlFree(priv->output->finish);
                NhlFree(priv->output->stride);
                NhlFree(priv->output);
        }
        if (priv->page_ids)
                NhlFree(priv->page_ids);
        
        NhlFree(priv);

        return;
}
