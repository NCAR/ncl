/*
 *      $Id: htmlpage.c,v 1.3 1999-05-22 00:36:19 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		htmlpage.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon May  5 17:07:45 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/htmlpageP.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>

static void DestroyHtmlPage
(
	NhlPointer data
)
{
	brHtmlPageRec	*rec = (brHtmlPageRec	*)data;

/* 
 * Don't need to do anything about the htmlview because it is destroyed
 * in the browser
 */

        NhlFree(rec);
        
	return;
}

static void
AdjustHtmlPageGeometry
(
        NhlPointer	data
)
{
	brPage	*page = (brPage *) data;
        brHtmlPageRec	*rec;
	Dimension		w,h,y,twidth,theight;
        Dimension		avail_width,avail_height;

	rec = (brHtmlPageRec *)page->pdata->type_rec;

	NgGetHtmlViewSize(rec->go->base.id,rec->page_id,rec->id,
                          0,True,0,&rec->height,&rec->width);


        NgSetFolderSize(page->pdata->pane,
                        rec->width,rec->height,&avail_width,&avail_height);

	return;
}

static void GetHtmlView
(
	brPage *page
)
{
        brPageData	*pdp = page->pdata;
	brHtmlPageRec	*rec = (brHtmlPageRec	*)pdp->type_rec;

	rec->id = NgGetHtmlView
                (rec->go->base.id,rec->page_id,_hbGENERIC,
                 rec->id,rec->qurl,NrmNULLQUARK,
                 pdp->form,0,0);

	NgGetHtmlViewSize(rec->go->base.id,rec->page_id,rec->id,
                          0,False,0,&rec->height,&rec->width);

	NgShowHtmlView(rec->go->base.id,rec->page_id,rec->id);

	return;
}

static void HtmlPageFocusNotify (
        brPage *page,
        NhlBoolean in
        )
{
        brPageData	*pdp = page->pdata;
	brHtmlPageRec	*rec = (brHtmlPageRec	*)pdp->type_rec;
        
        if (in) {
                GetHtmlView(page);
		AdjustHtmlPageGeometry((NhlPointer)page);
	}
        return;
}

static brPageData *
NewHtmlPage
(
  	NgGO		go,
        brPane		*pane,
	brPage		*page
        )
{
	brPageData	*pdp;
	brHtmlPageRec	*rec;
        NhlString	e_text;
	
        
	if (!(pdp = NhlMalloc(sizeof(brPageData)))) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                return NULL;
	}
        pdp->dl = NULL;
	pdp->next = pane->html_pages;
	pane->html_pages = pdp;

	rec = (brHtmlPageRec*) NhlMalloc(sizeof(brHtmlPageRec));
	if (! rec) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                NhlFree(pdp);
                return NULL;
	}

	rec->go = go;
	rec->page_id = page->id;
	rec->id = NgNoHtmlView;
	rec->height = rec->width = 0;
	rec->qurl = page->qvar;

        pdp->type_rec = (NhlPointer) rec;
        
	pdp->form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,pane->folder,
                 XmNy,28,
                 XmNx,2,
                 NULL);
	pdp->destroy_page = DestroyHtmlPage;
	pdp->adjust_page_geo = AdjustHtmlPageGeometry;
	pdp->deactivate_page = NULL;
        pdp->public_page_data = NULL;
        pdp->update_page = NULL;
        pdp->reset_page = NULL;
        pdp->page_focus_notify = HtmlPageFocusNotify;
        pdp->page_message_notify = NULL;

        return pdp;
}

        
extern brPageData *
NgGetHtmlPage
(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page,
	NgPageSaveState save_state
)
{
	NgBrowse		browse = (NgBrowse)go;
	NgBrowsePart		*np = &browse->browse;
        NhlString		e_text;
	brPageData		*pdp;
	brHtmlPageRec		*rec,*copy_rec;
	NhlBoolean		new = False;
	

	if (copy_page) {
		copy_rec = (brHtmlPageRec *) copy_page->pdata->type_rec;
	}
	for (pdp = pane->html_pages; pdp != NULL; pdp = pdp->next) {
		if (!pdp->in_use)
		  break;
	}
        if (! pdp) {
                pdp = NewHtmlPage(go,pane,page);
		new = True;
		if (! pdp)
			return NULL;
		rec = (brHtmlPageRec *) pdp->type_rec;
	}
	else {
		rec = (brHtmlPageRec *) pdp->type_rec;
		rec->page_id = page->id;
		rec->id = NgNoHtmlView;
		rec->height = rec->width = 0;
		rec->qurl = page->qvar;
	}
        page->pdata = pdp;

	if (copy_page)
		rec->qurl = copy_rec->qurl;
        

        GetHtmlView(page);

        pdp->in_use = True;
        if (new)
		_NgGOWidgetTranslations(go,pdp->form);

        return pdp;

}

extern void 
NgRefreshHtmlPage
(
	brPage		*page
)
{
        brPageData	*pdp = page->pdata;
	brHtmlPageRec	*rec = (brHtmlPageRec	*)pdp->type_rec;
	
	NgRefreshHtmlView(page->go->base.id,page->id,rec->id);
}

extern void 
NgHtmlPageContentNotify
(
	int		go_id,
	int 		page_id,
	NrmQuark	qurl
)
{
	brPage *page = _NgGetPageRef(go_id,page_id);
        brPageData	*pdp;
	brHtmlPageRec	*rec; 

	if (! page)
		return;
	pdp = page->pdata;
	rec  = (brHtmlPageRec	*)pdp->type_rec;
	
	rec->qurl = qurl;

	return;
}
	









