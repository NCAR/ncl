/*
 *      $Id: filepage.c,v 1.5 1998-01-08 01:19:24 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		filepage.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon May  5 17:07:45 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/filepageP.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>

static void DestroyFileRefPage
(
	NhlPointer data
)
{
	brFileRefPageRec	*fr_rec = (brFileRefPageRec	*)data;

        NgDestroyFileTree(fr_rec->filetree);
        NhlFree(data);
        
	return;
}

static void
AdjustFileRefPageGeometry
(
        NhlPointer	data
)
{
	brPage	*page = (brPage *) data;
        brFileRefPageRec	*rec;
	Dimension		w,h,y,twidth,theight;
        Dimension		avail_width,avail_height;

	rec = (brFileRefPageRec *)page->pdata->type_rec;

	XtVaGetValues(rec->filetree->tree,
		      XmNwidth,&w,
		      XmNy,&y,
		      XmNheight,&h,
		      NULL);
        
	twidth = w;
	theight = y + h;

        NgSetFolderSize(page->pdata->pane,
                        twidth,theight,&avail_width,&avail_height);
	
	return;
}
static brPageData *
NewFileRefPage
(
  	NgGO		go,
        brPane		*pane
        )
{
	brPageData		*pdp;
	brFileRefPageRec	*rec;
        NhlString		e_text;
        
	if (!(pdp = NhlMalloc(sizeof(brPageData)))) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                return NULL;
	}
        pdp->dl = NULL;
	pdp->next = pane->fileref_pages;
	pane->fileref_pages = pdp;

	rec = (brFileRefPageRec*) NhlMalloc(sizeof(brFileRefPageRec));
	if (! rec) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                NhlFree(pdp);
                return NULL;
	}
        rec->filetree = NULL;
        pdp->type_rec = (NhlPointer) rec;
        
	pdp->form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,pane->folder,
                 XmNy,28,
                 XmNx,2,
                 NULL);
	pdp->destroy_page = DestroyFileRefPage;
	pdp->adjust_page_geo = AdjustFileRefPageGeometry;
	pdp->deactivate_page = NULL;
	pdp->page_output_notify = NULL;
        pdp->page_input_notify = NULL;
        pdp->public_page_data = NULL;
        pdp->update_page = NULL;
        pdp->page_focus_notify = NULL;
        
        return pdp;
}

        
extern brPageData *
NgGetFileRefPage
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
	brFileRefPageRec	*rec;
	NgFileTree		*copy_filetree;

	if (copy_page) {
		brFileRefPageRec *copy_rec = 
			 (brFileRefPageRec *) copy_page->pdata->type_rec;
		copy_filetree = copy_rec->filetree;
	}
	for (pdp = pane->fileref_pages; pdp != NULL; pdp = pdp->next) {
		if (!pdp->in_use)
		  break;
	}
        if (! pdp)
                pdp = NewFileRefPage(go,pane);
        if (! pdp)
                return NULL;
        page->pdata = pdp;
        
        if (pdp->dl)
                NclFreeDataList(pdp->dl);
        pdp->dl = NclGetFileInfo(page->qfile);
        if (!pdp->dl) {
                e_text = "%s: error getting file information for %s";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go),
                          NrmQuarkToString(page->qfile));
        }
        rec = (brFileRefPageRec *) pdp->type_rec;

        if (! rec->filetree) {
                if (copy_page)
                        rec->filetree = NgDupFileTree
                                (go,pdp->form,page->qfile,pdp->dl,
                                 NULL,copy_filetree);
                else
                        rec->filetree = NgCreateFileTree
                                (go,pdp->form,page->qfile,pdp->dl);
                
                XtVaSetValues(rec->filetree->tree,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNbottomAttachment,XmATTACH_NONE,
                              NULL);
                rec->filetree->geo_notify = AdjustFileRefPageGeometry;
        }
        else if (copy_page)
                NgDupFileTree(go,pdp->form,page->qfile,pdp->dl,
                              rec->filetree,copy_filetree);
        else
                NgUpdateFileTree(rec->filetree,page->qfile,pdp->dl);
        
        
        pdp->in_use = True;
        rec->filetree->geo_data = (NhlPointer) page;
        return pdp;

}
