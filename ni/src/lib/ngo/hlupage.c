/*
 *      $Id: hlupage.c,v 1.2 1997-06-23 21:06:24 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hlupage.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jun  9 21:02:27 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/hlupageP.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleBG.h>

static void CreateUpdateCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPage		*page = (brPage *)udata;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;

        printf("in CreateUpdateCB\n");
        
        return;
}

static void AutoUpdateCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPage		*page = (brPage *)udata;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;

        printf("in AutoUpdateCB\n");
        return;
}

static void HluPageInputNotify (
        brPage *page,
        brPageType output_page_type,
 	NhlPointer output_data
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
        NgVarPageOutput	*var_data = (NgVarPageOutput *)output_data;
                
        printf("in hlu page input notify\n");

        switch (output_page_type) {
            case _brREGVAR:
            case _brFILEVAR:
                    rec->var_data[var_data->data_ix] = var_data;
                    NgUpdateDataSinkGrid
                            (rec->data_sink_grid,page->qvar,pub->data_info);
                    break;
            default:
                    printf("page type not supported for input\n");
        }
                    
        return;
}

static NhlPointer PublicHluPageData (
        brPage *page
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
        
        printf("in public hlu page data\n");
        
        return (NhlPointer) pub;
}

static void
DeactivateHluPage
(
	brPage	*page
)
{
	brHluPageRec *rec = (brHluPageRec *)page->pdata->type_rec;

        XtRemoveCallback(rec->create_update,
                         XmNactivateCallback,CreateUpdateCB,page);
        XtRemoveCallback(rec->auto_update,
                         XmNvalueChangedCallback,AutoUpdateCB,page);

        rec->activated = False;
}

static void DestroyHluPage
(
	NhlPointer data
)
{
	brHluPageRec	*hlu_rec = (brHluPageRec	*)data;

        NgDestroyDataSinkGrid(hlu_rec->data_sink_grid);
        
        NhlFree(data);
        
	return;
}

static void
AdjustHluPageGeometry
(
        NhlPointer	data
)
{
	brPage	*page = (brPage *) data;
        brHluPageRec	*rec;
	Dimension		w,h,y,twidth,theight;
        Dimension		avail_width,avail_height;
        
	rec = (brHluPageRec *)page->pdata->type_rec;
        
	twidth = 0;
	theight = 0;

        if (rec->data_sink_grid) {
                XtVaGetValues(rec->data_sink_grid->grid,
                              XmNwidth,&w,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
        }
        
	twidth = MAX(w,twidth);
        theight = y + h;
        NgSetFolderSize(page->pdata->pane,
                        twidth,theight,&avail_width,&avail_height);
	
	return;
}
        
static NhlErrorTypes UpdateHluPage
(
        brPage *page
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
        
        printf("in updata hlu page\n");

        printf("%s\n",pub->class_name);

        if (pub->data_info->n_dataitems > rec->var_data_count) {
                int i;
                
                rec->var_data = NhlRealloc
                        (rec->var_data,pub->data_info->n_dataitems *
                         sizeof(NgVarPageOutput *));
                for (i = rec->var_data_count;
                     i < pub->data_info->n_dataitems; i++)
                        rec->var_data[i] = NULL;
        }
        rec->var_data_count = pub->data_info->n_dataitems;
        
        if (! rec->data_sink_grid) {
                rec->data_sink_grid = NgCreateDataSinkGrid
                        (pdp->form,page->qvar,pub->data_info);
                XtVaSetValues(rec->data_sink_grid->grid,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNrightAttachment,XmATTACH_NONE,
                              NULL);
        }
        rec->data_sink_grid->dataitems = rec->var_data;
        NgUpdateDataSinkGrid(rec->data_sink_grid,page->qvar,pub->data_info);

        
        if (! rec->create_update) {
                rec->create_update = XtVaCreateManagedWidget
                        ("Create/Update",xmPushButtonGadgetClass,
                         pdp->form,
                         XmNtopAttachment,XmATTACH_WIDGET,
                         XmNtopWidget,rec->data_sink_grid->grid,
                         XmNrightAttachment,XmATTACH_NONE,
                         XmNbottomAttachment,XmATTACH_NONE,
                         NULL);
        }
        if (! rec->auto_update) {
                rec->auto_update = XtVaCreateManagedWidget
                        ("Auto Update",xmToggleButtonGadgetClass,
                         pdp->form,
                         XmNtopAttachment,XmATTACH_WIDGET,
                         XmNtopWidget,rec->data_sink_grid->grid,
                         XmNleftAttachment,XmATTACH_WIDGET,
                         XmNleftWidget,rec->create_update,
                         XmNrightAttachment,XmATTACH_NONE,
                         XmNbottomAttachment,XmATTACH_NONE,
                         NULL);
        }
        if (! rec->activated) {
                rec->activated = True;
                XtAddCallback(rec->create_update,
                              XmNactivateCallback,CreateUpdateCB,page);
                XtAddCallback(rec->auto_update,
                              XmNvalueChangedCallback,AutoUpdateCB,page);
        }
                 
        return NhlNOERROR;

}

static brPageData *
NewHluPage
(
  	NgGO		go,
        brPane		*pane
        )
{
	brPageData	*pdp;
	brHluPageRec	*rec;
        NhlString	e_text;
        
	if (!(pdp = NhlMalloc(sizeof(brPageData)))) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                return NULL;
	}
        pdp->dl = NULL;
	pdp->next = pane->hlu_pages;
	pane->hlu_pages = pdp;

	rec = (brHluPageRec*) NhlMalloc(sizeof(brHluPageRec));
	if (! rec) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                NhlFree(pdp);
                return NULL;
	}
        pdp->type_rec = (NhlPointer) rec;
        rec->activated = False;
        rec->data_sink_grid = NULL;
        rec->create_update = NULL;
        rec->auto_update = NULL;
        rec->var_data_count = 0;
        rec->var_data = NULL;
        
	pdp->form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,pane->folder,
                 XmNy,28,
                 XmNx,2,
                 NULL);
        
	pdp->destroy_page = DestroyHluPage;
	pdp->adjust_page_geo = AdjustHluPageGeometry;
	pdp->deactivate_page = DeactivateHluPage;
	pdp->page_output_notify = NULL;
        pdp->page_input_notify = HluPageInputNotify;
        pdp->public_page_data = PublicHluPageData;
        pdp->update_page = UpdateHluPage;
        
        return pdp;
}

extern brPageData *
NgGetHluPage
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
	brHluPageRec		*rec;
        NrmQuark		*qhlus;
        int			count;
        NhlBoolean		is_hlu = False;
        int			i;

        qhlus = NclGetHLUVarSymNames(&count);
        for (i = 0; i < count; i++) {
                if (qhlus[i] == page->qvar) {
                        is_hlu = True;
                        break;
                }
        }
        NclFree(qhlus);

	if (copy_page) {
		brHluPageRec *copy_rec = 
			 (brHluPageRec *) copy_page->pdata->type_rec;
	}
	for (pdp = pane->hlu_pages; pdp != NULL; pdp = pdp->next) {
		if (!pdp->in_use)
		  break;
	}
        if (! pdp)
                pdp = NewHluPage(go,pane);
        if (! pdp)
                return NULL;
        
	pdp->in_use = True;
        
	return pdp;
}
