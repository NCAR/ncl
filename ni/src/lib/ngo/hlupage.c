/*
 *      $Id: hlupage.c,v 1.1 1997-06-20 16:35:32 dbrown Exp $
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

static void HluPageInputNotify (
        brPage *page,
        brPageType output_page_type,
 	NhlPointer output_data
        )
{
        printf("in hlu page input notify\n");
        return;
}

static void DestroyHluPage
(
	NhlPointer data
)
{
	brHluPageRec	*fr_rec = (brHluPageRec	*)data;

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

        NgSetFolderSize(page->pdata->pane,
                        twidth,theight,&avail_width,&avail_height);
	
	return;
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
        
	pdp->form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,pane->folder,
                 XmNy,28,
                 XmNx,2,
                 NULL);
        
	pdp->destroy_page = DestroyHluPage;
	pdp->adjust_page_geo = AdjustHluPageGeometry;
	pdp->deactivate_page = NULL;
	pdp->page_output_notify = NULL;
        pdp->page_input_notify = HluPageInputNotify;
        pdp->public_page_data = NULL;
        pdp->update_page = NULL;
        
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
