/*
 *      $Id: varpage.c,v 1.1 1997-06-04 18:08:35 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		varpage.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon May  5 17:07:45 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/varpageP.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/ToggleBG.h>
#include <Xm/PushBG.h>
        
static void
AdjustVarPageGeometry
(
        NhlPointer	data
)
{
	brPage	*page = (brPage *) data;
	brPageData	*pdp = page->pdata;
	brVarPageRec	*rec = (brVarPageRec *)pdp->type_rec;
	Dimension		w,h,y,twidth,theight;
        Dimension		avail_width,avail_height;

	XtVaGetValues(rec->vartree->tree,
		      XmNwidth,&w,
		      NULL);
	twidth = w;
	XtVaGetValues(rec->diminfogrid->grid,
		      XmNwidth,&w,
		      NULL);
	twidth = MAX(w,twidth);
        if (rec->shaper_managed) {
                XtVaGetValues(rec->shaper->frame,
                              XmNwidth,&w,
                              NULL);
                twidth = MAX(w,twidth);
        }

        if (rec->datagrid_managed) {
		NgUpdateDataGrid(rec->datagrid,
				 page->qfile,pdp->dl->u.var);
                XtVaGetValues(rec->datagrid->grid,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
        }
        else
                XtVaGetValues(rec->datagrid_toggle,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
                
	theight = y + h;
        NgSetFolderSize(page->pdata->pane,
                      twidth,theight,&avail_width,&avail_height);
        
	if (rec->shaper && rec->shaper->datagrid) {
                XtVaSetValues(rec->shaper->datagrid->grid,
                              XmNwidth,avail_width -
                              rec->shaper->datagrid->sub_width -
                              rec->shaper->sub_width,
                              NULL);
        }
	if (rec->datagrid) {
                XtVaSetValues(rec->datagrid->grid,
                              XmNwidth,avail_width,
                              NULL);
	}
	if (rec->datagrid_managed) {
		NgUpdateDataGrid(rec->datagrid,
				 page->qfile,pdp->dl->u.var);
                XtVaSetValues(rec->datagrid->grid,
                              XmNwidth,avail_width - rec->datagrid->sub_width,
                              NULL);
	}
	
	return;
}

static void UpdateDataCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPage		*page = (brPage *)udata;
	brPageData	*pdp = page->pdata;
	brVarPageRec *rec = (brVarPageRec *) pdp->type_rec;

        if (rec->shaper && rec->shaper->new_shape) {
                rec->datagrid->start = rec->start;
                rec->datagrid->finish = rec->finish;
                rec->datagrid->stride = rec->stride;
		NgUpdateDataGrid(rec->datagrid,page->qfile,pdp->dl->u.var);
                rec->new_data = False;
		rec->shaper->new_shape = False;
        }
        (*pdp->adjust_page_geo)(page);
        return;
}


static void DataGridToggleCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPage		*page = (brPage *)udata;
	brPageData	*pdp = page->pdata;
	brVarPageRec *rec = (brVarPageRec *) pdp->type_rec;
        Widget top;

        printf("DataGridToggleCB(IN)\n");

        top = rec->shaper_managed ? rec->shaper->frame : rec->shaper_toggle;
        if (! rec->datagrid) {
                rec->datagrid = NgCreateDataGrid
                        (page->go,pdp->form,
                         page->qfile,pdp->dl->u.var,False,False);
                rec->datagrid->start = rec->start;
                rec->datagrid->finish = rec->finish;
                rec->datagrid->stride = rec->stride;
		NgUpdateDataGrid(rec->datagrid,page->qfile,pdp->dl->u.var);
                XtVaSetValues(rec->datagrid->grid,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNtopOffset,2,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->datagrid_toggle,
                              NULL);
                rec->new_data = False;
                rec->datagrid_managed = True;
        }
        else if (rec->new_data) {
                rec->datagrid->start = rec->start;
                rec->datagrid->finish = rec->finish;
                rec->datagrid->stride = rec->stride;
		NgUpdateDataGrid(rec->datagrid,page->qfile,pdp->dl->u.var);
                rec->new_data = False;
                if (rec->shaper)
                        rec->shaper->new_shape = False;
                if (! rec->datagrid_managed) {
                        XtManageChild(rec->datagrid->grid);
                        rec->datagrid_managed = True;
                }
        }
        else if (rec->datagrid_managed) {
                XtUnmanageChild(rec->datagrid->grid);
                rec->datagrid_managed = False;
        }
        else {
                XtManageChild(rec->datagrid->grid);
                rec->datagrid_managed = True;
        }
        
        (*pdp->adjust_page_geo)(page);
        
        return;
        
}

static void ShaperToggleCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPage		*page = (brPage *)udata;
	brPageData	*pdp = page->pdata;
	brVarPageRec *rec = (brVarPageRec *) pdp->type_rec;
                
        printf("ShaperToggleCB(IN)\n");

        if (! rec->shaper) {
                rec->shaper = NhlMalloc(sizeof(NgShaper));
                rec->shaper->go = page->go;
                rec->shaper->parent = pdp->form;
                rec->shaper->shaper = NULL;
                rec->shaper->pdata = NULL;
                rec->shaper->apply = NULL;
                rec->shaper->start = rec->start;
                rec->shaper->finish = rec->finish;
                rec->shaper->stride = rec->stride;
                rec->shaper->new_data = True;
                rec->shaper->restore = False;
                rec->shaper->vinfo = pdp->dl->u.var;
                rec->shaper->qfile = page->qfile;
                rec->shaper->geo_notify = AdjustVarPageGeometry;
                rec->shaper->geo_data = (NhlPointer) page;
                
                NgDoShaper(rec->shaper);
                XtVaSetValues(rec->shaper->frame,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNtopOffset,2,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->shaper_toggle,
                              NULL);
                rec->shaper_managed = True;
                rec->new_shape = False;
                XtVaSetValues(rec->datagrid_toggle,
                              XmNtopWidget,rec->shaper->frame,
                              NULL);
        }
        else if (rec->new_shape) {
                rec->shaper->start = rec->start;
                rec->shaper->finish = rec->finish;
                rec->shaper->stride = rec->stride;
                rec->shaper->new_data = True;
                rec->shaper->restore = False;
                rec->shaper->vinfo = pdp->dl->u.var;
                rec->shaper->qfile = page->qfile;
                rec->shaper->geo_data = (NhlPointer) page;
                NgDoShaper(rec->shaper);
                rec->new_shape = False;
                if (! rec->shaper_managed) {
                        XtManageChild(rec->shaper->frame);
                        rec->shaper_managed = True;
                        XtVaSetValues(rec->datagrid_toggle,
                                      XmNtopWidget,rec->shaper->frame,
                                      NULL);
                }
        }
        else if (rec->shaper_managed) {
                XtUnmanageChild(rec->shaper->frame);
                rec->shaper_managed = False;
                XtVaSetValues(rec->datagrid_toggle,
                              XmNtopWidget,rec->shaper_toggle,
                              NULL);
        }
        else {
                XtManageChild(rec->shaper->frame);
                rec->shaper_managed = True;
                XtVaSetValues(rec->datagrid_toggle,
                              XmNtopWidget,rec->shaper->frame,
                              NULL);
        }
        
        (*pdp->adjust_page_geo)(page);
        
        return;
        
}

static void DestroyVarPage
(
	NhlPointer data
)
{
	brVarPageRec	*rec = (brVarPageRec	*)data;

        NgDestroyVarTree(rec->vartree);
        NgDestroyDimInfoGrid(rec->diminfogrid);
        if (rec->datagrid) {
                NgDestroyDataGrid(rec->datagrid);
        }
        if (rec->shaper) {
                NgDestroyShaper(rec->shaper);
        }
        if (rec->start)
                NhlFree(rec->start);
        if (rec->finish)
                NhlFree(rec->finish);
        if (rec->stride)
                NhlFree(rec->stride);
        
        NhlFree(data);
	return;
}
static void
DeactivateVarPage
(
	brPage	*page
)
{
	brVarPageRec *rec = (brVarPageRec *)page->pdata->type_rec;
           
        if (rec->shaper) {
                NgDeactivateShaper(rec->shaper);
        }
        if (rec->shaper_managed) {
                XtUnmanageChild(rec->shaper->frame);
                rec->shaper_managed = False;
                XtVaSetValues(rec->shaper_toggle,
                              XmNset,False,
                              NULL);
                XtVaSetValues(rec->datagrid_toggle,
                              XmNtopWidget,rec->shaper_toggle,
                              NULL);
        }
        rec->new_shape = True;
        XtRemoveCallback(rec->shaper_toggle,
                         XmNvalueChangedCallback,ShaperToggleCB,page);
        
        if (rec->datagrid) {
                NgDeactivateDataGrid(rec->datagrid);
        }
        if (rec->datagrid_managed) {
                XtUnmanageChild(rec->datagrid->grid);
                rec->datagrid_managed = False;
        }
        rec->new_data = True;
        XtRemoveCallback(rec->datagrid_toggle,
                         XmNvalueChangedCallback,DataGridToggleCB,page);
        return;
}

void InitializeDimInfo
(
        brVarPageRec		*rec,
        NclApiDataList		*dl
        )
{
        NclApiVarInfoRec	*vinfo = dl->u.var;
        int i;
        
        if (rec->start)
                NhlFree(rec->start);
        rec->start = NhlMalloc(sizeof(long)*vinfo->n_dims);
        if (rec->finish)
                NhlFree(rec->finish);
        rec->finish = NhlMalloc(sizeof(long)*vinfo->n_dims);
        if (rec->stride)
                NhlFree(rec->stride);
        rec->stride = NhlMalloc(sizeof(long)*vinfo->n_dims);

        for (i = 0; i<vinfo->n_dims; i++) {
                rec->start[i] = 0;
                rec->finish[i] = 0;
                rec->stride[i] = 1;
        }
        rec->finish[vinfo->n_dims-1] = 
		vinfo->dim_info[vinfo->n_dims-1].dim_size - 1;
        if (vinfo->n_dims > 1) {
                rec->finish[vinfo->n_dims-2] = 
			vinfo->dim_info[vinfo->n_dims-2].dim_size - 1;
        }
        
        return;
}


extern brPageData *
NgGetVarPage
(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page
)
{
	NgBrowse		browse = (NgBrowse)go;
	NgBrowsePart		*np = &browse->browse;
        NhlString		e_text;
	brPageData		*pdp;
	brVarPageRec		*rec,*copy_rec = NULL;
        Dimension		h1,h2;
        Widget			sep;
	NgVarTree		*copy_vartree;

	if (copy_page) {
		copy_rec = (brVarPageRec *) copy_page->pdata->type_rec;
		copy_vartree = copy_rec->vartree;
	}
	for (pdp = pane->var_pages; pdp != NULL; pdp = pdp->next) {
		if (!pdp->in_use)
		  break;
	}
	if (pdp) {
                if (pdp->dl)
                        NclFreeDataList(pdp->dl);
                if (page->type == _brFILEVAR)
                        pdp->dl = NclGetFileVarInfo(page->qfile,page->qvar);
                else
                        pdp->dl = NclGetVarInfo(page->qvar);
                if (!pdp->dl) {
                        e_text =
                           "%s: error getting file var information for %s->%s";
                        NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go),
                                  NrmQuarkToString(page->qfile),
                                  NrmQuarkToString(page->qvar));
                        
                }
		rec = (brVarPageRec *) pdp->type_rec;
                InitializeDimInfo(rec,pdp->dl);
		if (copy_page)
			NgDupVarTree(go,pdp->form,page->qfile,page->qvar,
                                     pdp->dl,rec->vartree,copy_vartree);
		else
			NgUpdateVarTree(rec->vartree,
                                        page->qfile,page->qvar,pdp->dl);
		NgUpdateDimInfoGrid(rec->diminfogrid,
				    page->qfile,pdp->dl->u.var);
                if (copy_rec && copy_rec->datagrid_managed) {
                        rec->datagrid->start = rec->start;
                        rec->datagrid->finish = rec->finish;
                        rec->datagrid->stride = rec->stride;
                        NgUpdateDataGrid(rec->datagrid,
                                         page->qfile,pdp->dl->u.var);
                        XtManageChild(rec->datagrid->grid);
                        rec->datagrid_managed = True;
                        rec->new_data = False;
                        XtVaSetValues(rec->datagrid_toggle,
                                      XmNset,True,
                                      NULL);
                }
                XtAddCallback(rec->datagrid_toggle,
                              XmNvalueChangedCallback,DataGridToggleCB,page);
                XtAddCallback(rec->shaper_toggle,
                              XmNvalueChangedCallback,ShaperToggleCB,page);
                rec->vartree->geo_notify = AdjustVarPageGeometry;
                rec->vartree->geo_data = (NhlPointer) page;
		pdp->in_use = True;
		return pdp;
	}
	if (!(pdp = NhlMalloc(sizeof(brPageData)))) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
	}
	rec = (brVarPageRec*) NhlMalloc(sizeof(brVarPageRec));
	pdp->next = pane->var_pages;
	pane->var_pages = pdp;

	pdp->type_rec = (void *) rec;
	if (! pdp->type_rec) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
	}

	pdp->form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,pane->folder,
                 XmNy,28,
                 XmNx,2,
                 NULL);
	
	if (page->type == _brFILEVAR)
                pdp->dl = NclGetFileVarInfo(page->qfile,page->qvar);
        else
                pdp->dl = NclGetVarInfo(page->qvar);
        if (!pdp->dl) {
                e_text = "%s: error getting file var information for %s->%s";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go),
                          NrmQuarkToString(page->qfile),
                          NrmQuarkToString(page->qvar));
        }
	rec->start = NULL;
	rec->finish = NULL;
	rec->stride = NULL;
        InitializeDimInfo(rec,pdp->dl);
        
	rec->diminfogrid = NgCreateDimInfoGrid
		(pdp->form,page->qfile,pdp->dl->u.var,True,False);
        XtVaSetValues(rec->diminfogrid->grid,
                      XmNrightAttachment,XmATTACH_NONE,
                      XmNbottomAttachment,XmATTACH_NONE,
                      NULL);
        
	if (copy_page)
		rec->vartree = 
			NgDupVarTree(go,pdp->form,
                                      page->qfile,page->qvar,pdp->dl,
				      NULL,copy_vartree);
	else
		rec->vartree = 
	        	NgCreateVarTree(go,pdp->form,
                                        page->qfile,page->qvar,pdp->dl);
        
        XtVaSetValues(rec->vartree->tree,
                      XmNrightAttachment,XmATTACH_NONE,
                      XmNbottomAttachment,XmATTACH_NONE,
                      XmNtopOffset,8,
                      XmNtopAttachment,XmATTACH_WIDGET,
                      XmNtopWidget,rec->diminfogrid->grid,
                      NULL);
        rec->vartree->geo_notify = AdjustVarPageGeometry;
        rec->vartree->geo_data = (NhlPointer) page;
        
        rec->shaper_toggle = XtVaCreateManagedWidget
                ("Shape",xmToggleButtonGadgetClass,pdp->form,
                 XmNbottomAttachment,XmATTACH_NONE,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNtopOffset,4,
                 XmNtopAttachment,XmATTACH_WIDGET,
                 XmNtopWidget,rec->vartree->tree,
                 NULL);
        XtAddCallback(rec->shaper_toggle,
                      XmNvalueChangedCallback,ShaperToggleCB,page);
        rec->shaper = NULL;
	rec->shaper_managed = False;

        rec->datagrid_toggle = XtVaCreateManagedWidget
                ("Data",xmToggleButtonGadgetClass,pdp->form,
                 XmNbottomAttachment,XmATTACH_NONE,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNtopOffset,4,
                 XmNtopAttachment,XmATTACH_WIDGET,
                 XmNtopWidget,rec->shaper_toggle,
                 NULL);
        XtAddCallback(rec->datagrid_toggle,
                      XmNvalueChangedCallback,DataGridToggleCB,page);
        
        if (copy_rec && copy_rec->datagrid_managed) {

                rec->datagrid = NgCreateDataGrid
                        (page->go,pdp->form,
                         page->qfile,pdp->dl->u.var,False,False);
                rec->datagrid->start = rec->start;
                rec->datagrid->finish = rec->finish;
                rec->datagrid->stride = rec->stride;
		NgUpdateDataGrid(rec->datagrid,page->qfile,pdp->dl->u.var);
                XtVaSetValues(rec->datagrid->grid,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNtopOffset,2,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->datagrid_toggle,
                              NULL);
                rec->new_data = False;
                rec->datagrid_managed = True;
                XtVaSetValues(rec->datagrid_toggle,
                              XmNset,True,
                              NULL);
        }
        else {
                rec->datagrid = NULL;
                rec->datagrid_managed = False;
                rec->new_data = True;
        }
	pdp->in_use = True;
	pdp->destroy_page = DestroyVarPage;
	pdp->adjust_page_geo = AdjustVarPageGeometry;
	pdp->deactivate_page = DeactivateVarPage;

	return pdp;
}
