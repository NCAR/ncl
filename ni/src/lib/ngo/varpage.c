/*
 *      $Id: varpage.c,v 1.13 1999-05-22 00:36:27 dbrown Exp $
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
#include <Xm/LabelG.h>

static void FreeDataLinks(
	XmLArray datalinks
	)
{
	int i,count;
	NgPageReply reply;

	if (! datalinks)
		return;
	count = XmLArrayGetCount(datalinks);
	for (i = 0; i < count; i++) {
		reply = (NgPageReply) XmLArrayGet(datalinks,i);
		NhlFree(reply);
	}
	XmLArrayFree(datalinks);
}

static XmLArray CopyDataLinks
(
	XmLArray datalinks
	)
{
	XmLArray 	copy_dl;
	NgPageReply 	link,copy_link;
	int 		i,count;

	count = XmLArrayGetCount(datalinks);
	
	if (! count)
		return NULL;
	copy_dl = XmLArrayNew(0,0);

	for (i = 0; i < count; i++) {
		copy_link = NhlMalloc(sizeof(NgPageReplyRec));
		if (! link) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NULL;
		}
		link = XmLArrayGet(datalinks,i);
		copy_link->req = link->req;
		copy_link->id = link->id;
		copy_link->qfile = link->qfile;
		copy_link->qvar = link->qvar;
		XmLArrayAdd(copy_dl,0,1);
		XmLArraySet(copy_dl,0,copy_link);
	}
	return copy_dl;
}

	
static NhlBoolean UpdateDataLink
(
	brVarPageRec		*rec,
	NhlBoolean		on,
	NgPageMessageType	message_type,
	NgPageId		page_id,
	NrmQuark		qfile,
	NrmQuark		qvar
)

{
	int		i,count,pos;
	NgPageReply 	link;
	NhlBoolean	new = False;

	if (! rec->datalinks) {
		rec->datalinks = XmLArrayNew(0,0);
		if (! rec->datalinks) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
	}

	pos = count = XmLArrayGetCount(rec->datalinks);

	for (i = 0; i < count; i++) {
		link = XmLArrayGet(rec->datalinks,i);
		if (link->id == page_id &&
		    link->req == message_type) {
			pos = i;
			break;
		}
	}
	if (pos == count) {
		if (! on) 
			return True;
		pos = 0;
		link = NhlMalloc(sizeof(NgPageReplyRec));
		if (! link) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		new = True;
	}
	else if (! on) {
		XmLArrayDel(rec->datalinks,pos,1);
		NhlFree(link);
		return True;
	}

	link->req = message_type;
	link->id = page_id;
	link->qfile = qfile;
	link->qvar = qvar;
	
	if (new) {
		XmLArrayAdd(rec->datalinks,0,1);
		XmLArraySet(rec->datalinks,0,link);
	}

	return True;
}

static NhlErrorTypes GetDataLinkReqMessage
(
        brPage *page,
	NgPageMessage   message
        )
{
        brPageData	*pdp = page->pdata;
	brVarPageRec	*rec = (brVarPageRec	*)pdp->type_rec;
	brDataLinkReq	dlink;
	NgPageMessageType mesg_type = _NgNOMESSAGE;
	brPage		*frpage;

	dlink = (brDataLinkReq) message->message;
	if (message->mtype != _NgVARDATALINK_REQ) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "invalid message type for DataLine"));
		return NhlFATAL;
	}
	mesg_type = _NgVARDATA;

	frpage = _NgGetPageRef(rec->go->base.id,message->from_id);

	if (frpage) {
		UpdateDataLink
			(rec,dlink->on,mesg_type,frpage->id,
			 frpage->qfile,frpage->qvar);
	}
	if (message->message_free)
		(*message->message_free)(message->message);
	
	return NhlNOERROR;
}

static void NotifyDataLinks
(
	brPage	*page
)
{
	brPageData	*pdp = page->pdata;
	brVarPageRec	*rec = (brVarPageRec *)pdp->type_rec;
	NgPageReply 	link;
	int		i, count;

	if (! rec->datalinks)
		return;

	count = XmLArrayGetCount(rec->datalinks);

	for (i = 0; i < count; i++) {
		link = (NgPageReply) XmLArrayGet(rec->datalinks,i);
		/* a VarPage should only have type _NgVARDATA links */
		if (link->req != _NgVARDATA) 
			continue; 

		/* now we only post to _brHLUVAR pages, when there are
		 * other types as well, this will have to be updated.
		 */
		NgPostPageMessage
			(rec->go->base.id,page->id,_NgNOMESSAGE,_brHLUVAR,
			 link->qfile,link->qvar,_NgVARDATA,
			 rec->vdata,True,NULL,True);
	}
}
			
static NhlErrorTypes VarPageMessageNotify
(
        brPage		*page
        )
{
	brPageData	*pdp = page->pdata;
	brVarPageRec	*rec = (brVarPageRec *)pdp->type_rec;
	NgPageMessage   *messages;
	int 		i,count;
	NhlErrorTypes	ret = NhlNOERROR,subret = NhlNOERROR;

#if DEBUG_VARPAGE
	fprintf(stderr,"in VarPageMessageNotify\n");
#endif

	count = NgRetrievePageMessages
		(page->go->base.id,
		 page->qfile ? _brFILEVAR : _brREGVAR,
		 page->qfile,page->qvar,&messages);

	for (i = count - 1; i >= 0; i--) {
		switch (messages[i]->mtype) {  
		case _NgVARDATALINK_REQ:
			subret = GetDataLinkReqMessage(page,messages[i]);
			ret = MIN(ret,subret);
			break;
		default:
			break;
		}
	}

	if (count)
		NgDeletePageMessages(page->go->base.id,count,messages,False);

	return ret;
}

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
				 page->qfile,pdp->dl->u.var,
                                 rec->start,rec->finish,rec->stride);
                XtVaGetValues(rec->datagrid->grid,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
        }
        else
                XtVaGetValues(rec->data_ctrl_form,
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
				 page->qfile,pdp->dl->u.var,
                                 rec->start,rec->finish,rec->stride);
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
		NgUpdateDataGrid(rec->datagrid,page->qfile,pdp->dl->u.var,
                                 rec->start,rec->finish,rec->stride);
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
        
#if DEBUG_VARPAGE
        fprintf(stderr,"DataGridToggleCB(IN)\n");
#endif
        top = rec->shaper_managed ? rec->shaper->frame : rec->shaper_toggle;
        if (! rec->datagrid) {
                rec->datagrid = NgCreateDataGrid
                        (page->go,pdp->form,
                         page->qfile,pdp->dl->u.var,False,False);
		NgUpdateDataGrid(rec->datagrid,page->qfile,pdp->dl->u.var,
                                 rec->start,rec->finish,rec->stride);
                XtVaSetValues(rec->datagrid->grid,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNtopOffset,2,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->data_ctrl_form,
                              NULL);
                rec->new_data = False;
                rec->datagrid_managed = True;
		_NgGOWidgetTranslations(page->go,rec->datagrid->grid);
        }
        else if (rec->new_data) {
		NgUpdateDataGrid(rec->datagrid,page->qfile,pdp->dl->u.var,
                                 rec->start,rec->finish,rec->stride);
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

        if (rec->datagrid_managed) {
                Position  x,y;
                Dimension width,height;
                XRectangle rect;
                XtVaGetValues(rec->datagrid->grid,
                              XmNwidth,&width,
                              XmNheight,&height,
                              XmNx,&x,
                              XmNy,&y,
                              NULL);
                rect.x = x;
                rect.y = y;
                rect.width = width;
                rect.height = height;
                
                NgPageSetVisible
                        (rec->go->base.id,rec->page_id,pdp->form,&rect);
        }
        return;
        
}

static void
UpdateShaper
(
	brPage		*page
)
{
	brPageData	*pdp = page->pdata;
	brVarPageRec *rec = (brVarPageRec *) pdp->type_rec;

        if (! rec->shaper) {
                rec->shaper = NhlMalloc(sizeof(NgShaper));
                rec->shaper->go = page->go;
                rec->shaper->parent = pdp->form;
                rec->shaper->shaper = NULL;
                rec->shaper->apply = NULL;
                rec->shaper->start = rec->start;
                rec->shaper->finish = rec->finish;
                rec->shaper->stride = rec->stride;
                rec->shaper->new_data = True;
                rec->shaper->restore = False;
                rec->shaper->vinfo = pdp->dl->u.var;
                rec->shaper->qfile = page->qfile;
                rec->shaper->geo_notify = AdjustVarPageGeometry;
                rec->shaper->pdata = NULL; /* prevent pointer function calls */
                rec->shaper->tgl_coord = -1;
                
                NgDoShaper(rec->shaper);
		rec->shaper->pdata = (NhlPointer) page;
                XtVaSetValues(rec->shaper->frame,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNtopOffset,2,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->shaper_toggle,
                              NULL);
                rec->shaper_managed = True;
                rec->new_shape = False;
                XtVaSetValues(rec->data_ctrl_form,
                              XmNtopWidget,rec->shaper->frame,
                              NULL);
		_NgGOWidgetTranslations(page->go,rec->shaper->frame);
        }
        else if (rec->new_shape) {
                rec->shaper->start = rec->start;
                rec->shaper->finish = rec->finish;
                rec->shaper->stride = rec->stride;
                rec->shaper->new_data = True;
                rec->shaper->restore = False;
                rec->shaper->vinfo = pdp->dl->u.var;
                rec->shaper->qfile = page->qfile;
                rec->shaper->pdata = NULL;/* prevent pointer function calls */
                NgDoShaper(rec->shaper);
                rec->shaper->pdata = (NhlPointer) page;
                rec->new_shape = False;
                if (! rec->shaper_managed) {
                        XtManageChild(rec->shaper->frame);
                        rec->shaper_managed = True;
                        XtVaSetValues(rec->data_ctrl_form,
                                      XmNtopWidget,rec->shaper->frame,
                                      NULL);
                }
        }
        else if (rec->shaper_managed) {
                XtUnmanageChild(rec->shaper->frame);
                rec->shaper_managed = False;
                XtVaSetValues(rec->data_ctrl_form,
                              XmNtopWidget,rec->shaper_toggle,
                              NULL);
        }
        else {
                XtManageChild(rec->shaper->frame);
                rec->shaper_managed = True;
                XtVaSetValues(rec->data_ctrl_form,
                              XmNtopWidget,rec->shaper->frame,
                              NULL);
        }
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
                
#if DEBUG_VARPAGE
        fprintf(stderr,"ShaperToggleCB(IN)\n");
#endif
	UpdateShaper(page);
        
        (*pdp->adjust_page_geo)(page);
        
        if (rec->shaper_managed) {
                Position  x,y;
                Dimension width,height;
                XRectangle rect;
                XtVaGetValues(rec->shaper->frame,
                              XmNwidth,&width,
                              XmNheight,&height,
                              XmNx,&x,
                              XmNy,&y,
                              NULL);
                rect.x = x;
                rect.y = y;
                rect.width = width;
                rect.height = height;
                XtVaGetValues(rec->plotspecmenu->menubar,
                              XmNwidth,&width,
                              XmNheight,&height,
                              XmNx,&x,
                              XmNy,&y,
                              NULL);
                rect.height = y - rect.y + height;
                
                NgPageSetVisible
                        (rec->go->base.id,rec->page_id,pdp->form,&rect);
        }
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
        NgDestroyPlotSpecMenu(rec->plotspecmenu);
        
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

        NgFreeVarData(rec->vdata);
	if (rec->datalinks)
		FreeDataLinks(rec->datalinks);

        NhlFree(data);
	
	return;
}
static void 
FreeVarSaveState
(
	brVarSaveState vs_state
)
{
	FreeDataLinks(vs_state->datalinks);
	if (vs_state->start)
		NhlFree(vs_state->start);
	if (vs_state->finish)
		NhlFree(vs_state->finish);
	if (vs_state->stride)
		NhlFree(vs_state->stride);

	NhlFree(vs_state);

	return;
}

static void
SaveVarState
(
	brPage	*page
)
{
	brVarPageRec *rec = (brVarPageRec *)page->pdata->type_rec;
	brVarSaveState vs_state = NhlMalloc(sizeof(brVarSaveStateRec));

	if (! vs_state) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}
	vs_state->vdata = rec->vdata;
	vs_state->start = rec->start;
	vs_state->finish = rec->finish;
	vs_state->stride = rec->stride;
	vs_state->datalinks = rec->datalinks;
	vs_state->shaper_managed = rec->shaper_managed;

	NgSavePageState(rec->go->base.id,page->id,page->qfile,page->qvar,
			(NhlPointer)vs_state,(NhlFreeFunc) FreeVarSaveState);

/*
 * Make sure this varpage doesn't try to do anything with any of this 
 * allocated storage, after storing it into the save structure.
 */
	rec->vdata = NULL;
	rec->datalinks = NULL;
	rec->start = NULL;
	rec->finish = NULL;
	rec->stride = NULL;

	return;
}

static void
DeactivateVarPage
(
	brPage	*page
)
{
	brVarPageRec *rec = (brVarPageRec *)page->pdata->type_rec;

	SaveVarState(page);

        if (rec->shaper) {
                NgDeactivateShaper(rec->shaper);
        }
        if (rec->shaper_managed) {
                XtUnmanageChild(rec->shaper->frame);
                rec->shaper_managed = False;
                XtVaSetValues(rec->shaper_toggle,
                              XmNset,False,
                              NULL);
                XtVaSetValues(rec->data_ctrl_form,
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
                XtVaSetValues(rec->datagrid_toggle,
                              XmNset,False,
                              NULL);
        }
        rec->new_data = True;
        XtRemoveCallback(rec->datagrid_toggle,
                         XmNvalueChangedCallback,DataGridToggleCB,page);
        
        return;
}

NhlBoolean InitializeDimInfo
(
	brPage		*page,
        brVarPageRec	*rec,
        brVarPageRec	*copy_rec,
        NclApiDataList	*dl,
        NclApiDataList	*copy_dl
        )
{
        NclApiVarInfoRec	*vinfo = dl->u.var;
        NhlBoolean		do_copy;
        int i;

        do_copy = copy_rec && vinfo->n_dims == copy_dl->u.var->n_dims ?
                True : False;

        if (rec->start)
                NhlFree(rec->start);
        rec->start = NhlMalloc(sizeof(long)*vinfo->n_dims);
        if (rec->finish)
                NhlFree(rec->finish);
        rec->finish = NhlMalloc(sizeof(long)*vinfo->n_dims);
        if (rec->stride)
                NhlFree(rec->stride);
        rec->stride = NhlMalloc(sizeof(long)*vinfo->n_dims);
	if ( ! rec->vdata)
		rec->vdata = NgNewVarData();
	if (! (rec->start && rec->finish && rec->stride && rec->vdata)) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return False;
	}

        if (do_copy) {
                memcpy(rec->start,copy_rec->start,
                       sizeof(long)*vinfo->n_dims);
                memcpy(rec->finish,copy_rec->finish,
                       sizeof(long)*vinfo->n_dims);
                memcpy(rec->stride,copy_rec->stride,
                       sizeof(long)*vinfo->n_dims);
		return NgCopyVarData(rec->vdata,copy_rec->vdata);
        }
        for (i = 0; i<vinfo->n_dims; i++) {
                rec->start[i] = 0;
                rec->finish[i] = 0;
                rec->stride[i] = 1;
        }
/*
 * initial shape of any variable is a slice along the two fastest dims,
 * unless there's only 1 dim.
 */
        rec->finish[vinfo->n_dims-1] = 
		vinfo->dim_info[vinfo->n_dims-1].dim_size - 1;
        if (vinfo->n_dims > 1) {
                rec->finish[vinfo->n_dims-2] = 
			vinfo->dim_info[vinfo->n_dims-2].dim_size - 1;
        }

	return NgSetVarData(dl,rec->vdata,page->qfile,dl->u.var->name,
			    NrmNULLQUARK,vinfo->n_dims,
			    rec->start,rec->finish,rec->stride,_NgSHAPED_VAR);

}

static brPageData *
NewVarPage
(
  	NgGO		go,
        brPane		*pane
        )
{
	brPageData	*pdp;
	brVarPageRec	*rec;
        NhlString	e_text;
        
	if (!(pdp = NhlMalloc(sizeof(brPageData)))) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                return NULL;
	}
        pdp->dl = NULL;
	pdp->next = pane->var_pages;
	pane->var_pages = pdp;

	rec = (brVarPageRec*) NhlMalloc(sizeof(brVarPageRec));
	if (! rec) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                NhlFree(pdp);
                return NULL;
	}
        rec->diminfogrid = NULL;
        rec->vartree = NULL;
        rec->new_shape = True;
        rec->shaper = NULL;
        rec->shaper_toggle = NULL;
        rec->shaper_managed = False;
        rec->datagrid = NULL;
        rec->datagrid_toggle = NULL;
        rec->datagrid_managed = False;
        rec->new_data = True;
        rec->data_ctrl_form = NULL;
        rec->datagrid_toggle = NULL;
        rec->plotspecmenu = NULL;
        rec->start = NULL;
        rec->finish = NULL;
        rec->stride = NULL;
        rec->go = go;
	rec->vdata = NULL;
	rec->datalinks = NULL;
        
        pdp->type_rec = (NhlPointer) rec;
        pdp->pane = pane;
        
	pdp->form = XtVaCreateManagedWidget
		("VarPage",xmFormWidgetClass,pane->folder,
                 XmNy,28,
                 XmNx,2,
                 NULL);
        
	pdp->destroy_page = DestroyVarPage;
	pdp->adjust_page_geo = AdjustVarPageGeometry;
	pdp->deactivate_page = DeactivateVarPage;
	pdp->page_message_notify = VarPageMessageNotify;
        pdp->public_page_data = NULL;
        pdp->update_page = NULL;
        pdp->reset_page = NULL;
        pdp->page_focus_notify = NULL;
        
        return pdp;
}

static void 
RestoreVarState(
	brPage		*page,
	NhlBoolean	*do_shaper,
	NgPageSaveState save_state
)
{
	brPageData	*pdp = page->pdata;
	brVarPageRec	*rec = (brVarPageRec *)pdp->type_rec;
	brVarSaveState vs_state = (brVarSaveState) save_state->page_state;

	rec->datalinks = vs_state->datalinks;
	rec->vdata = vs_state->vdata;
	rec->start = vs_state->start;
	rec->finish = vs_state->finish;
	rec->stride = vs_state->stride;

	*do_shaper = vs_state->shaper_managed ? True : False;
	NhlFree(vs_state);
	NhlFree(save_state);

	return;
}

extern brPageData *
NgGetVarPage
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
	brVarPageRec		*rec,*copy_rec = NULL;
        Dimension		h1,h2;
        Widget			sep;
	NgVarTree		*copy_vartree;
        Widget			label,top_widget;
	NhlBoolean		do_shaper = False;
	NhlBoolean		new = False;

	if (copy_page) {
		copy_rec = (brVarPageRec *) copy_page->pdata->type_rec;
		copy_vartree = copy_rec->vartree;
	}
	for (pdp = pane->var_pages; pdp != NULL; pdp = pdp->next) {
		if (!pdp->in_use)
		  break;
	}
        if (! pdp) {
                pdp = NewVarPage(go,pane);
		new = True;
	}
        if (! pdp)
                return NULL;
        page->pdata = pdp;

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
        rec->page_id = page->id;
	if (save_state) {
		RestoreVarState(page,&do_shaper,save_state);
	}
        else if (!InitializeDimInfo(page,rec,copy_rec,pdp->dl,
				    copy_page ? copy_page->pdata->dl : NULL)) {
		return NULL;
	}

/* Data Links */

	if (copy_rec && copy_rec->datalinks) {
		if (rec->datalinks) {
			FreeDataLinks(rec->datalinks);
		}
		rec->datalinks = CopyDataLinks(copy_rec->datalinks);
	}
	
/* DimInfoGrid */
        
        if (! rec->diminfogrid) {
                rec->diminfogrid = NgCreateDimInfoGrid
                        (pdp->form,page->qfile,pdp->dl->u.var,True,False);
                XtVaSetValues(rec->diminfogrid->grid,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNbottomAttachment,XmATTACH_NONE,
                              NULL);
        }
        else {
		NgUpdateDimInfoGrid(rec->diminfogrid,
				    page->qfile,pdp->dl->u.var);
        }
        
/* VarTree */
        
        if (!rec->vartree) {
		if (copy_page)
			rec->vartree = NgDupVarTree
                                (go,pdp->form,page->qfile,page->qvar,
                                 pdp->dl,NULL,copy_vartree);
                else 
                        rec->vartree = NgCreateVarTree
                                (go,pdp->form,page->qfile,page->qvar,pdp->dl);
                XtVaSetValues(rec->vartree->tree,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNtopOffset,8,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->diminfogrid->grid,
                              NULL);
                rec->vartree->geo_notify = AdjustVarPageGeometry;
        }
        else if (copy_page)
                NgDupVarTree(go,pdp->form,page->qfile,page->qvar,
                             pdp->dl,rec->vartree,copy_vartree);
        else
                NgUpdateVarTree(rec->vartree,page->qfile,page->qvar,pdp->dl);
        
        rec->vartree->geo_data = (NhlPointer) page;


/* Shaper */
        
        if (! rec->shaper_toggle) {
                rec->shaper_toggle = XtVaCreateManagedWidget
                        ("ShaperTgl",xmToggleButtonGadgetClass,pdp->form,
                         XmNbottomAttachment,XmATTACH_NONE,
                         XmNrightAttachment,XmATTACH_NONE,
                         XmNtopOffset,4,
                         XmNtopAttachment,XmATTACH_WIDGET,
                         XmNtopWidget,rec->vartree->tree,
                         NULL);
        }
        if (copy_rec && copy_rec->shaper_managed) {
                Boolean state;
                if (! rec->shaper) {
                        rec->shaper = NhlMalloc(sizeof(NgShaper));
                        rec->shaper->go = page->go;
                        rec->shaper->parent = pdp->form;
                        rec->shaper->shaper = NULL;
                        rec->shaper->apply = NULL;
                        rec->shaper->geo_notify = AdjustVarPageGeometry;
                }
                else {
                        XtManageChild(rec->shaper->frame);
                }
                rec->shaper->start = rec->start;
                rec->shaper->finish = rec->finish;
                rec->shaper->stride = rec->stride;
                rec->shaper->new_data = True;
                rec->shaper->restore = False;
                rec->shaper->vinfo = pdp->dl->u.var;
                rec->shaper->qfile = page->qfile;
                rec->shaper->tgl_coord = copy_rec->shaper->tgl_coord;
                rec->shaper->pdata = NULL; /* prevent function pointer calls */
                
                NgDoShaper(rec->shaper);
                XtVaSetValues(rec->shaper->frame,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNtopOffset,2,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->shaper_toggle,
                              NULL);
                rec->shaper_managed = True;
                rec->shaper->shapeinfogrid->edit_row =
                        copy_rec->shaper->shapeinfogrid->edit_row;
                NgSetShapeInfoGridSetFocusCell(rec->shaper->shapeinfogrid);
                state = XmToggleButtonGetState
                        (copy_rec->shaper->datagrid_toggle);
                XmToggleButtonSetState
                        (rec->shaper->datagrid_toggle,state,True);
                if (state) {
                        state = XmToggleButtonGetState
                                (copy_rec->shaper->all_selected_tgl);
                        XmToggleButtonSetState
                                (rec->shaper->all_selected_tgl,state,True);
                }
                state = XmToggleButtonGetState(copy_rec->shaper->indexes_tgl);
                XmToggleButtonSetState(rec->shaper->indexes_tgl,state,True);
                state = XmToggleButtonGetState
                        (copy_rec->shaper->synchro_step_tgl);
                XmToggleButtonSetState
                        (rec->shaper->synchro_step_tgl,state,True);
                rec->new_shape = False;
                rec->shaper->pdata = (NhlPointer) page;
                XtVaSetValues(rec->shaper_toggle,
                              XmNset,True,
                              NULL);
        }
	else if (do_shaper) {
		UpdateShaper(page);
	}
		

/* Data Control and Output */        
        
        top_widget = rec->shaper_managed ?
                rec->shaper->frame : rec->shaper_toggle;
        if (rec->data_ctrl_form) {
                XtVaSetValues(rec->data_ctrl_form,
                              XmNtopWidget,top_widget,
                              NULL);
        }
        else {
                rec->data_ctrl_form = XtVaCreateManagedWidget
                        ("form",xmFormWidgetClass,pdp->form,
                         XmNbottomAttachment,XmATTACH_NONE,
                         XmNrightAttachment,XmATTACH_NONE,
                         XmNtopOffset,4,
                         XmNtopAttachment,XmATTACH_WIDGET,
                         XmNtopWidget,top_widget,
                         NULL);

                rec->datagrid_toggle = XtVaCreateManagedWidget
                        ("DatagridTgl",xmToggleButtonGadgetClass,
			 rec->data_ctrl_form,
                         XmNrightAttachment,XmATTACH_NONE,
                         NULL);

                label =  XtVaCreateManagedWidget
                        ("OutputMenuLbl",xmLabelGadgetClass,
                         rec->data_ctrl_form,
                         XmNrightAttachment,XmATTACH_NONE,
                         XmNtopOffset,3,
                         XmNleftOffset,15,
                         XmNleftAttachment,XmATTACH_WIDGET,
                         XmNleftWidget,rec->datagrid_toggle,
                         NULL);
                rec->plotspecmenu =
                        NgCreatePlotSpecMenu(page->go,rec->data_ctrl_form);
                XtVaSetValues(rec->plotspecmenu->menubar,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNtopOffset,4,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->shaper_toggle,
                              XmNheight,25,
                              XmNleftOffset,10,
                              XmNleftAttachment,XmATTACH_WIDGET,
                              XmNleftWidget,label,
                              NULL);
        }
	rec->plotspecmenu->page_id = page->id;
	rec->plotspecmenu->vdata = rec->vdata;

/* Data Grid */
        
        if (copy_rec && copy_rec->datagrid_managed) {
                int start_col,start_row;
                if (! rec->datagrid) {
                        rec->datagrid = NgCreateDataGrid
                                (page->go,pdp->form,
                                 page->qfile,pdp->dl->u.var,False,False);
                        XtVaSetValues(rec->datagrid->grid,
                                      XmNbottomAttachment,XmATTACH_NONE,
                                      XmNrightAttachment,XmATTACH_NONE,
                                      XmNtopOffset,2,
                                      XmNtopAttachment,XmATTACH_WIDGET,
                                      XmNtopWidget,rec->data_ctrl_form,
                                      NULL);
                }
                NgUpdateDataGrid(rec->datagrid,
                                 page->qfile,pdp->dl->u.var,
                                 rec->start,rec->finish,rec->stride);
                XtManageChild(rec->datagrid->grid);
                XtVaGetValues(copy_rec->datagrid->grid,
                              XmNscrollColumn,&start_col,
                              XmNscrollRow,&start_row,
                              NULL);
                XtVaSetValues(rec->datagrid->grid,
                              XmNscrollColumn,start_col,
                              XmNscrollRow,start_row,
                              NULL);
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

        pdp->in_use = True;
        AdjustVarPageGeometry((NhlPointer)page);

        if (new)
		_NgGOWidgetTranslations(go,pdp->form);

	return pdp;
}

extern void NgVarPageDataUpdate
(
        NhlPointer 	pdata
        )
{
	brPage	*page = (brPage *) pdata;
	brPageData	*pdp = page->pdata;
	brVarPageRec	*rec = (brVarPageRec *)pdp->type_rec;
        int ndims = pdp->dl->u.var->n_dims;
        int i,size = ndims * sizeof(long);
        brPageType ptype = page->qfile > NrmNULLQUARK ? _brFILEVAR : _brREGVAR;
	NhlBoolean notify_req = False;

	if (! NgSetVarData(pdp->dl,rec->vdata,page->qfile,
			   pdp->dl->u.var->name,NrmNULLQUARK,
			   ndims,rec->start,rec->finish,rec->stride,
			   _NgSHAPED_VAR))
		return;

	NotifyDataLinks(page);

        return;
}
