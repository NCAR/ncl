/*
 *      $Id: plottree.c,v 1.3 1999-11-19 02:10:09 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plottree.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 24 14:37:14 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/plottreeP.h>
#include <ncarg/ngo/browseP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/ngo/hlupage.h>
#include <ncarg/ngo/plotapp.h>
#include <ncarg/hlu/View.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include  <ncarg/ngo/Tree.h>
#include <float.h>

static Dimension Row_Height;

static Pixmap Check_Pixmap,No_Check_Pixmap;

static void Button3ObjectAction(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static XtActionsRec plottreeactions[] = {
	{ "Button3ObjectAction", Button3ObjectAction }
};

static void AdjustTextWidget
(
        NgPlotTreeRec *ptp,
	int row,
	int col
        )
{
	NgPlotTree *pub = &ptp->public;
        Widget parent = pub->tree;
        Position x,y,tx=0,ty=0;
        int i;

        for (i=0; i < 4; i++) {
                XtVaGetValues(parent,
                              XmNx,&x,
                              XmNy,&y,
                              NULL);
                tx += x;
                ty += y;
                parent = XtParent(parent);
        }
        x = MAX(0,-tx);
        y = MAX(0,-ty);
        
        XtMoveWidget(ptp->text,x,y);

        return;
}

static void MakeRowsVisible
(
        NgPlotTreeRec	*ptp,
        int top_row,
        int bottom_row
        )         
{
	NgPlotTree *pub = &ptp->public;
	XRectangle	rect;
        
	XmLGridRowColumnToXY
                (pub->tree,
                 XmCONTENT,top_row,XmCONTENT,0,False,&rect);
        rect.height = (bottom_row - top_row + 1) * Row_Height;
        
        NgPageSetVisible(
                ptp->go->base.id,ptp->page_id,pub->tree,&rect);
        return;
}

static void ExpandPlotInfo
(
        NgPlotTreeRec *ptp,
        ptNodeData	*ndata,
        int pos
        )
{
	NgPlotTree *pub = &ptp->public;
        XmLTreeRowDefinition *rowdefs;
	NgDataProfile	dprof;
	NhlString 	wkname;
        int rowcount,i;
        char buf[256];
        
#if	DEBUG_PLOTTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandPlotInfo(IN)\n");
#endif
        
        rowcount = 2;
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));

        for (i = 0; i < rowcount; i++) {
                switch (i) {
                    case 0:
                            sprintf(buf,"Plot Style");
                            break;
                    case 1:
                            sprintf(buf,"Display Window");
                            break;
                }
                rowdefs[i].level = ndata->type / 10 + 1;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
        }
        
        XmLTreeAddRows(pub->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
        dprof = ptp->data_profile;
        for (i = 0; i < rowcount; i++) {
                NhlBoolean do_string = True;
                        
                XmStringFree(rowdefs[i].string);
                switch (i) {
                    case 0:
			    if (dprof)
				    sprintf(buf,"%s",
					    NgPlotAppName(ptp->go->base.id,
							  dprof->qpstyle));
			    else
				    sprintf(buf,"No plot style name");
                            break;
                    case 1:
			    wkname = NgNclGetHLURef
				    (ptp->go->go.nclstate,ptp->wk_id);
			    if (wkname) 
				    sprintf(buf,"%s",wkname);
			    else
				    sprintf(buf,"No display defined");
                            break;
                }
                if (do_string) {
                        XmLGridSetStringsPos(pub->tree,
                                             XmCONTENT,pos+i,XmCONTENT,1,buf);
                        ptp->c2_width = MAX(ptp->c2_width,strlen(buf));
                }
        }
        XtVaSetValues(pub->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ptp->c2_width,
                      NULL);
        NhlFree(rowdefs);


        return;
}

static void ToggleObjectCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgPlotTreeRec *ptp = (NgPlotTreeRec *) udata;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        XmLGridColumn	col;
        ptNodeData	*ndata;
	Pixmap		pixmap;
	ptCompData 	cdata;
        
        cbs = (XmLGridCallbackStruct *)cb_data;
	if (cbs->column != 2)
		return;

        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        col = XmLGridGetColumn(w,XmCONTENT,2);

        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNcolumnPtr,col,
		      XmNcellPixmap,&pixmap,
                      NULL);

	XtVaGetValues(ptp->public.tree,
                      XmNrowPtr,row,
		      XmNrowUserData,&ndata,
		      NULL);

	if (! (ndata && ndata->info)) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}
	cdata = (ptCompData) ndata->info;

	if (pixmap == Check_Pixmap) {
		XtVaSetValues(w,
			      XmNrow,cbs->row,
			      XmNcolumn,2,
			      XmNcellPixmap,No_Check_Pixmap,
			      NULL);
		cdata->on = False;
		cdata->modified = ! cdata->modified;
	}
	else {
		XtVaSetValues(w,
			      XmNrow,cbs->row,
			      XmNcolumn,2,
			      XmNcellPixmap,Check_Pixmap,
			      NULL);
		cdata->on = True;
		cdata->modified = ! cdata->modified;
	}
	return;
}
#if 0
static void
SetValCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgPlotTreeRec *ptp = (NgPlotTreeRec *)udata.ptrval;
	_NhlValueSetCBData vsdata = (_NhlValueSetCBData) cbdata.ptrval;
	ptNodeData *base_objndata,*ndata;
	int base_objndata_count;
	NhlLayer l;
	NrmQuark qvname;
	NhlBoolean on;
	ptCompData cdata;
	int i;
	int row_base;

#if 1
        fprintf(stderr,"in plottree setval cb\n");
#endif
	return;

	if (! ptp)
		return;
	l = _NhlGetLayer(vsdata->id);

	if (! l)
		return;
	qvname = NrmStringToQuark(l->base.name);

/*
 * this is very simplistic and will have to be modified if this tree type
 * becomes more complicated.
 */
	base_objndata = ptp->plot.subdata[1].subdata;
	base_objndata_count = ptp->plot.subdata[1].subcount;
	if (!base_objndata_count)
		return;
	if (! ptp->plot.subdata[0].expanded) 
		row_base = 2;
	else
		row_base = 2 + ptp->plot.subdata[0].subcount;
/*
 *
 */

	for (i = 0; i < base_objndata_count; i++) {
		ndata = &base_objndata[i];
		if (! ndata->info) 
			continue;
		cdata = (ptCompData) ndata->info;
		if (cdata->qname == qvname)
			break;
	}
        if (i == base_objndata_count)
		return;
	
	NhlVAGetValues(l->base.id,
		       NhlNvpOn,&on,
		       NULL);
	if (cdata->pt_cb)
		cdata->pt_cb = False;
	else {
		cdata->usr_on = on;
	}

	XtVaSetValues(ptp->public.tree,
		      XmNcolumn,2,
		      XmNrow,row_base+i,
		      XmNcellPixmap,on ? Check_Pixmap : No_Check_Pixmap,
		      NULL);
	return;
}
#endif
static void ExpandComponentInfo
(
        NgPlotTreeRec *ptp,
        ptNodeData	*ndata,
        int pos
        )
{
	NgPlotTree *pub = &ptp->public;
        XmLTreeRowDefinition *rowdefs;
	NgDataProfile	dprof = ptp->data_profile;
        int rowcount,i;
        char buf[256];
	NhlArgVal sel,user_data;

        
#if	DEBUG_PLOTTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandPlotInfo(IN)\n");
#endif
        
	if (! dprof || ! dprof->obj_count)
		return;
        rowcount = dprof->obj_count;
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(rowcount * sizeof(ptNodeData));

        for (i = 0; i < rowcount; i++) {
		ptCompData cdata = NhlMalloc(sizeof(ptCompDataRec));

		cdata->qname = dprof->qobjects[i];
		cdata->on = True;
		cdata->usr_on = -1;
		cdata->modified = False;
		cdata->pt_cb = False;

		sprintf(buf,"%s",NrmQuarkToString(dprof->qobjects[i]));
                rowdefs[i].level = ndata->type / 10 + 1;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].info = (NhlPointer) cdata;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].type = _ptPCompObj;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
        }
        
        XmLTreeAddRows(pub->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
	NhlINITVAR(sel);
	NhlINITVAR(user_data);
	sel.lngval = NrmStringToQuark(NhlNvpOn);
	user_data.ptrval = ptp;
        for (i = 0; i < rowcount; i++) {
                NhlBoolean do_string = True;
		char *cp;
                        
                XmStringFree(rowdefs[i].string);
		sprintf(buf,"%s",dprof->obj_classes[i]->base_class.class_name);
		cp = strstr(buf,"Class");
		*cp = '\0';
		
		buf[0] = toupper(buf[0]);
                if (do_string) {
                        XmLGridSetStringsPos(pub->tree,
                                             XmCONTENT,pos+i,XmCONTENT,1,buf);
                        ptp->c2_width = MAX(ptp->c2_width,strlen(buf));
                }
                XtVaSetValues(ptp->public.tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
		if (NhlClassIsSubclass(dprof->obj_classes[i],NhlviewClass)) {
			XtVaSetValues(ptp->public.tree,
				      XmNcolumn,2,
				      XmNrow,pos+i,
				      XmNcellPixmap,Check_Pixmap,
				      XmNcellBackground,
				      ptp->go->go.edit_field_pixel,
				      NULL);
#if 0
			if (pub->hlu_ids) {
				NhlLayer l = _NhlGetLayer(pub->hlu_ids[i]);
				if (! l) 
					continue;
				ptp->sv_cbs[i] = _NhlAddObjCallback
					(l,_NhlCBobjValueSet,
					 sel,SetValCB,user_data); 
			}				
#endif
		}

        }
        XtVaSetValues(pub->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ptp->c2_width,
                      NULL);
        NhlFree(rowdefs);


        return;
}

static NhlString
DataItemValue
(
	NgDataItem ditem
)
{
	static char buffer[4096];
	NgVarData vdata;
	int i;

	vdata = ditem->vdata;

	if (!vdata) {
		sprintf(buffer,"<null>");
	}
	else if (vdata->set_state == _NgUNKNOWN_DATA) {
		sprintf(buffer,"<unknown %d-d data>",vdata->ndims);
		if (vdata->ndims)
			sprintf(&buffer[strlen(buffer)-1],": size (");
		for (i=0; i< vdata->ndims; i++) {
			sprintf(&buffer[strlen(buffer)],"%ld,",
				vdata->finish[i]+1);
		}
		if (vdata->ndims) {
			/* back up 1 to remove final comma */
			sprintf(&buffer[strlen(buffer)-1],")>");
		}
	}
	else if (vdata->set_state == _NgEXPRESSION || 
		 vdata->set_state == _NgUSER_EXPRESSION ||
		 vdata->set_state == _NgBOGUS_EXPRESSION) {
		if (! strlen(vdata->expr_val))
			sprintf(buffer,"<null>");
		else
			sprintf(buffer,vdata->expr_val);
	}
	else if (! vdata->qvar) {
		sprintf(buffer,"<null>");
	}
	else {
		if (vdata->qfile && vdata->qvar && vdata->qcoord)
			sprintf(buffer,"%s->%s&%s(",
				NrmQuarkToString(vdata->qfile),
				NrmQuarkToString(vdata->qvar),
				NrmQuarkToString(vdata->qcoord));
		else if (vdata->qfile && vdata->qvar)
			sprintf(buffer,"%s->%s(",
				NrmQuarkToString(vdata->qfile),
				NrmQuarkToString(vdata->qvar));
		else if (vdata->qvar && vdata->qcoord)
			sprintf(buffer,"%s&%s(",
				NrmQuarkToString(vdata->qvar),
				NrmQuarkToString(vdata->qcoord));
		else
			sprintf(buffer,"%s(",
				NrmQuarkToString(vdata->qvar));

		for (i=0; i< vdata->ndims; i++) {
			if ((vdata->finish[i] - vdata->start[i])
			    /vdata->stride[i] == 0)
				sprintf(&buffer[strlen(buffer)],
					"%ld,",vdata->start[i]);
			else if (vdata->stride[i] == 1)
				sprintf(&buffer[strlen(buffer)],"%ld:%ld,",
					vdata->start[i],vdata->finish[i]);
			else
				sprintf(&buffer[strlen(buffer)],"%ld:%ld:%ld,",
					vdata->start[i],
					vdata->finish[i],vdata->stride[i]);
		}
		/* back up 1 to remove final comma */
		buffer[strlen(buffer)-1] = ')';
	}
	return buffer;
}

static void ExpandLinkResources
(
        NgPlotTreeRec *ptp,
        ptNodeData	*ndata,
        int pos
        )
{
	NgPlotTree *pub = &ptp->public;
        XmLTreeRowDefinition *rowdefs;
	NgDataProfile	dprof = ptp->data_profile;
        int rowcount,i,ic;
        char buf[256];
        
#if	DEBUG_PLOTTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandPlotInfo(IN)\n");
#endif
        
	if (! dprof || ! ptp->ditem_vis_count)
		return;
        rowcount = ptp->ditem_vis_count;
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));

        for (i = 0,ic = 0; i < dprof->n_dataitems; i++) {
		if (! dprof->ditems[i]->vis)
			continue;
		sprintf(buf,"%s",dprof->ditems[i]->name);
                rowdefs[ic].level = ndata->type / 10 + 1;
                rowdefs[ic].expands = False;
                rowdefs[ic].isExpanded = False;
                rowdefs[ic].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[ic].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[ic].string = XmStringCreateLocalized(buf);
		ic++;
        }
        XmLTreeAddRows(pub->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
        for (i = 0,ic = 0; i < dprof->n_dataitems; i++) {
                NhlBoolean do_string = True;
		char *cp;
		if (! dprof->ditems[i]->vis)
			continue;
                XmStringFree(rowdefs[ic].string);
		cp = DataItemValue(dprof->ditems[i]);
                if (do_string) {
                        XmLGridSetStringsPos(pub->tree,
                                             XmCONTENT,pos+ic,XmCONTENT,1,cp);
                        ptp->c2_width = MAX(ptp->c2_width,strlen(cp));
                }
		ic++;
        }
        XtVaSetValues(pub->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ptp->c2_width,
                      NULL);
        NhlFree(rowdefs);


        return;
}

static void ExpandTree 
(
        NgPlotTreeRec	*ptp,
        ptNodeData	*ndata,
        int		row
        )
{
	NgPlotTree *pub = &ptp->public;
        
        if (! ptp->expand_called) {
                short		cw,ch;
                XmFontList      fontlist;
                Dimension 	h,rh;
                int		nrows;
                XmLGridRow	grid_row;

                grid_row = XmLGridGetRow(pub->tree,XmCONTENT,row);
                XtVaSetValues(pub->tree,
                              XmNrowSizePolicy,XmCONSTANT,
                              NULL);
                XtVaGetValues(pub->tree,
                              XmNrowPtr,grid_row,
                              XmNfontList,&fontlist,
                              XmNrows,&nrows,
                              XmNrowHeight,&rh,
                              XmNheight,&h,
                              NULL);

                XtVaSetValues(pub->tree,
                              XmNrowSizePolicy,XmVARIABLE,
                              NULL);
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                Row_Height = MAX(rh,h/nrows);
                Row_Height = 20;
                ptp->expand_called = True;
        }

        switch (ndata->type) {
	case _ptPInfo:
		ExpandPlotInfo(ptp,ndata,row+1);
		break;
	case _ptPComp:
		XtVaSetValues(pub->tree,
			      XmNrow,row,
			      XmNcolumn,2,
			      XmNcellType,XmSTRING_CELL,
			      NULL);
		XmLGridSetStringsPos(pub->tree,XmCONTENT,row,XmCONTENT,2,"On");
		ExpandComponentInfo(ptp,ndata,row+1);
		XtAddCallback(ptp->public.tree,XmNselectCallback,
			      ToggleObjectCB,ptp);
		break;
	case _ptPLink:
		ExpandLinkResources(ptp,ndata,row+1);
		break;
        }
        return;
}
        
        
static int FindRowChange
(
        NgPlotTreeRec	*ptp,
        ptNodeData	*ndata
        )
{
        int i;
        int rows = 0;

        if (! ndata->subdata)
                return ndata->subcount;

        for (i = 0; i < ndata->subcount; i++) {
                rows += 1;
                if (ndata->subdata[i].expanded) {
                        rows += FindRowChange(ptp,&ndata->subdata[i]);
                }
        }
        return rows;
}

static void ExpandCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgPlotTreeRec *ptp = (NgPlotTreeRec *) udata;
	NgPlotTree *pub = &ptp->public;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        ptNodeData	*ndata;
        int		row_change;
        
        cbs = (XmLGridCallbackStruct *)cb_data;
        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNrowUserData,&ndata,
                      NULL);
        
        if (ndata->subcount > 0) {
                ndata->expanded = True;
		if (ndata->type == _ptPComp) {
			XmLGridSetStringsPos
				(pub->tree,XmCONTENT,
				 cbs->row,XmCONTENT,2,"On");
		}
                row_change = FindRowChange(ptp,ndata);
                if (pub->geo_notify && pub->geo_data)
                        (*pub->geo_notify)(pub->geo_data);
                MakeRowsVisible(ptp,cbs->row,cbs->row + row_change);
                return;
        }
        
        ExpandTree(ptp,ndata,cbs->row);
        row_change = FindRowChange(ptp,ndata);
        
        if (pub->geo_notify && pub->geo_data)
                (*pub->geo_notify)(pub->geo_data);
        MakeRowsVisible(ptp,cbs->row,cbs->row + row_change);
        AdjustTextWidget(ptp,cbs->row,0);
        
        return;
}

static void CollapseCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgPlotTreeRec *ptp = (NgPlotTreeRec *) udata;
	NgPlotTree *pub = &ptp->public;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        ptNodeData	*ndata;

        cbs = (XmLGridCallbackStruct *)cb_data;
        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNrowUserData,&ndata,
                      NULL);

        ndata->expanded = False;
        
	if (ndata->type == _ptPComp)
		XmLGridSetStringsPos
			(pub->tree,XmCONTENT,cbs->row,XmCONTENT,2,"  ");
        if (pub->geo_notify && pub->geo_data)
                (*pub->geo_notify)(pub->geo_data);
}

static void FocusCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgPlotTreeRec *ptp = (NgPlotTreeRec *) udata;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        
        if (cb->reason == XmCR_CELL_FOCUS_IN) {
                MakeRowsVisible(ptp,cb->row,cb->row);
                AdjustTextWidget(ptp,cb->row,0);
        }
        
        return;
}

static void FreeSubNodes
(
        ptNodeData	*ndata
        )
{
        int i;

        if (! ndata->subdata)
                return;
        for (i = 0; i < ndata->subcount; i++) {
                FreeSubNodes(&ndata->subdata[i]);
        }
	if (ndata->subdata->info)
		NhlFree(ndata->subdata->info);
        NhlFree(ndata->subdata);
        return;
}

static int ExpandNodeDataList
(
        NgPlotTreeRec	*ptp,
        ptNodeData	*to_ndata,
        ptNodeData	*from_ndata,
        int		node_count,
        int		row
        )
{
	NgPlotTree *pub = &ptp->public;
        int i;

        for (i = 0; i < node_count; i++) {
		if (from_ndata[i].expanded) {
			XtVaSetValues(pub->tree,
				      XmNrow,row,
				      XmNrowIsExpanded,True,
				      NULL);
		}
		if (from_ndata[i].subcount)
			ExpandTree(ptp,&to_ndata[i],row);
		to_ndata[i].expanded = from_ndata[i].expanded;
		row++;
                if (from_ndata[i].subdata)
                        row = ExpandNodeDataList
                                (ptp,to_ndata[i].subdata,
                                 from_ndata[i].subdata,
                                 from_ndata[i].subcount,row);
                else
                        row += to_ndata[i].subcount;
        }
        
        return row;
}

/*
 * Copies the state of an existing plottree to another plottree. If
 * to_plot_tree is NULL, a new plottree is created. 
 */

NgPlotTree *NgDupPlotTree
(
        NgGO			go,
        Widget			parent,
	int			wk_id,
        NrmQuark		qname,
	NgDataProfile		data_profile,
	NgPlotTree		*to_plot_tree,
        NgPlotTree		*from_plot_tree
        )
{
        NgPlotTreeRec *fromptp,*toptp;

        fromptp = (NgPlotTreeRec *) from_plot_tree;
        if (!fromptp)
                return NULL;

	if (to_plot_tree) {
		NgUpdatePlotTree(to_plot_tree,wk_id,qname,data_profile);
		toptp = (NgPlotTreeRec *) to_plot_tree;
	}
	else
		toptp = (NgPlotTreeRec *) 
			NgCreatePlotTree(go,parent,wk_id,qname,data_profile);
        
        if (!fromptp->expand_called)
                return (NgPlotTree *) toptp;

        ExpandNodeDataList
                (toptp,toptp->plot.subdata,
                 fromptp->plot.subdata,fromptp->plot.subcount,0);

	return (NgPlotTree *) toptp;
        
}

NhlErrorTypes NgUpdatePlotTree
(
        NgPlotTree		*plot_tree,
	int			wk_id,
        NrmQuark		qname,
	NgDataProfile		data_profile
        )
{
        NgPlotTreeRec *ptp;
	NgPlotTree *pub;
        int	i;
        int	tcount;
        XmLTreeRowDefinition *rowdefs;
        char	buf[256];
        Dimension width = 0;
        ptNodeData	*ndata;
	NgDataProfile	dprof;
        
        ptp = (NgPlotTreeRec *) plot_tree;
        if (!ptp) return NhlFATAL;
	pub = plot_tree;

        XmLGridDeleteAllRows(pub->tree,XmCONTENT);

        pub->geo_notify = NULL;
        pub->geo_data = NULL;
	pub->hlu_ids = NULL;
	ptp->wk_id = wk_id;
        ptp->qname = qname;
	ptp->data_profile = data_profile;
        ptp->page_id = NgGetPageId(ptp->go->base.id,ptp->qname,NrmNULLQUARK);
	ptp->ditem_vis_count = 0;
	ptp->sv_cbs = NULL;


        ndata = &ptp->plot;
        ndata->parent = NULL;
        ndata->type = _ptTop;
        ndata->info = NULL;
        ndata->expanded = True;

        if (ndata->subdata)
                FreeSubNodes(ndata);

        tcount = 2;
        rowdefs = NhlMalloc(tcount * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(tcount * sizeof(ptNodeData));
        ndata->subcount = tcount;

        for (i = 0; i < tcount; i++) {
                NhlBoolean expands;
                
                switch (i) {
                    case 0:
			    sprintf(buf,"Plot");
                            expands = True;
                            break;
                    case 1:
                            sprintf(buf,"Components");
                            expands = True;
                            break;
                    case 2:
                            sprintf(buf,"Link Resources");
                            expands = True;
                            break;
                }
                rowdefs[i].level = 1;
                rowdefs[i].expands = expands;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
        }
        XmLTreeAddRows(pub->tree,rowdefs,tcount,0);

	dprof = ptp->data_profile;
        for (i = 0; i < tcount; i++) {
                switch (i) {
                    case 0:
                            sprintf(buf,"%s",NrmQuarkToString(ptp->qname));
                            ndata->subdata[i].type = _ptPInfo;
                            break;
                    case 1:
			    if (dprof) 
				    sprintf(buf,"%d",dprof->obj_count);
			    else {
				    sprintf(buf,"0");
			    }
                            ndata->subdata[i].type = _ptPComp;
                            break;
                    case 2:
			    if (dprof) {
				    int i;
				    for (i = 0; i < dprof->n_dataitems; i++) {
					    if (dprof->ditems[i]->vis)
						    ptp->ditem_vis_count++;
				    }
				    sprintf(buf,"%d",ptp->ditem_vis_count);
			    }
			    else {
				    sprintf(buf,"0");
			    }
                            ndata->subdata[i].type = _ptPLink;
                            break;
                }
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].info = NULL;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
                
                width = MAX(width,strlen(buf));
                XtVaSetValues(pub->tree,
                              XmNrow,i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                XmLGridSetStringsPos(pub->tree,XmCONTENT,i,XmCONTENT,1,buf);
                XmStringFree(rowdefs[i].string);
        }
        ptp->c2_width = MAX(width,14);
        ptp->expand_called = False;
        NhlFree(rowdefs);
	if (! ptp->created) {
		XtVaSetValues(pub->tree,
			      XmNcolumn,1,
			      XmNcolumnWidth,ptp->c2_width,
			      NULL);
		ptp->created = True;
	}
	else {
		XtVaSetValues(pub->tree,
			      XmNimmediateDraw,False,
			      XmNcolumn,1,
			      XmNcolumnWidth,ptp->c2_width,
			      NULL);
	}
#if 0
	if (dprof) {
		ptp->sv_cbs = NhlMalloc(dprof->obj_count * sizeof(_NhlCB));
		memset(ptp->sv_cbs,0,dprof->obj_count * sizeof(_NhlCB));
	}
#endif
        return NhlNOERROR;
}

NgPlotTree *NgCreatePlotTree
(
        NgGO			go,
        Widget			parent,
	int			wk_id,
        NrmQuark 		qname,
	NgDataProfile		data_profile
        )
{
        NhlErrorTypes ret;
        NgPlotTreeRec *ptp;
	NgPlotTree *pub;
        static NhlBoolean first = True;
 
	XtAppAddActions(go->go.x->app,
                        plottreeactions,NhlNumber(plottreeactions));
         if (first) {
 		NgBrowse browse = (NgBrowse) go;

		Check_Pixmap = browse->browse.pixmaps.check;
		No_Check_Pixmap = browse->browse.pixmaps.no_check;
                first = False;
        }
        
        ptp = NhlMalloc(sizeof(NgPlotTreeRec));
        if (!ptp) return NULL;
	pub = &ptp->public;

        pub->geo_notify = NULL;
        pub->geo_data = NULL;
        ptp->created = False;
        ptp->qname = qname;
	ptp->wk_id = wk_id;
	ptp->data_profile = data_profile;
        ptp->go = go;
        ptp->plot.subdata = NULL;
        ptp->plot.subcount = 0;
        
        pub->tree = XtVaCreateManagedWidget
		("PlotTree",
		 xmlTreeWidgetClass,parent,
		 XmNverticalSizePolicy,XmVARIABLE,
		 XmNhorizontalSizePolicy,XmVARIABLE,
		 XmNcolumns, 3,
		 XmNimmediateDraw,True,
		 XmNuserData,ptp,
		 XmNselectionPolicy,XmSELECT_NONE,
		 NULL);

        XtVaSetValues(pub->tree,
                      XmNcellDefaults,True,
                      XmNcellRightBorderType,XmBORDER_NONE,
                      XmNcellTopBorderType,XmBORDER_NONE,
                      XmNcellBottomBorderType,XmBORDER_NONE,
                      XmNcellAlignment,XmALIGNMENT_LEFT,
                      XmNcellMarginLeft,10,
                      NULL);

        XtVaSetValues(pub->tree,
                      XmNcellLeftBorderType,XmBORDER_NONE,
		      XmNcellDefaults,True,
		      XmNcolumn,2,
                      XmNcellMarginLeft,3,
		      XmNcellType,XmPIXMAP_CELL,
		      XmNcolumnWidth,3,
		      NULL);
        
        XtAddCallback(pub->tree,XmNexpandCallback,ExpandCB,ptp);
        XtAddCallback(pub->tree,XmNcollapseCallback,CollapseCB,ptp);
        XtVaGetValues(pub->tree,
                      XmNtextWidget,&ptp->text,
                      NULL);
        
        ret = NgUpdatePlotTree((NgPlotTree*) ptp,wk_id,qname,data_profile);

/*
 * wait until rows are initialized before adding focus callback; otherwise
 * we get core dumps.
 */
        XtAddCallback(pub->tree,XmNcellFocusCallback,FocusCB,ptp);
        if (ret < NhlWARNING) {
                NhlFree(ptp);
                return NULL;
        }
        return (NgPlotTree *) ptp;
}

void NgDestroyPlotTree
(
        NgPlotTree		*plot_tree
        )
{
        NgPlotTreeRec *ptp;
        
        ptp = (NgPlotTreeRec *) plot_tree;
        if (!ptp) return;

        FreeSubNodes(&ptp->plot);

        NhlFree(ptp);
        
        return;
}

static void Button3ObjectAction(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
        )
{
        int		x,y;
        unsigned char	rowtype, coltype;
        int		row,col;
        XmLGridRow	rowptr;
        ptNodeData	*ndata;
        NgPlotTreeRec	*ptp;
        
        
#if	DEBUG_FILETREE
	fprintf(stderr,"Button3ObjectAction(IN)\n");
#endif
        x = xev->xbutton.x;
        y = xev->xbutton.y;
        
        if (XmLGridXYToRowColumn(w,x,y,&rowtype,&row,&coltype,&col) < 0)
                return;

        rowptr = XmLGridGetRow(w,XmCONTENT,row);
        XtVaGetValues(w,
                      XmNrowPtr,rowptr,
                      XmNrowUserData,&ndata,
                      XmNuserData,&ptp,
                      NULL);
	if (! ndata)
		return;
        if (ndata->type == _ptPCompObj) {
		NrmQuark qnames[2];
		char buf[256];
		NclExtValueRec	*val = NULL;
		NhlBoolean preview = False;
		ptCompData cdata = (ptCompData) ndata->info;

#if DEBUG_FILETREE
                fprintf(stderr,"file var %s\n",NrmQuarkToString(cdata->qname));
#endif
		sprintf(buf,"%s_%s",NrmQuarkToString(ptp->qname),
			NrmQuarkToString(cdata->qname));
                qnames[0] = NrmStringToQuark(buf);
                qnames[1] = ptp->qname;


		val = NclReadVar(qnames[0],NULL,NULL,NULL);
		if (! val) {
			 NHLPERROR((NhlFATAL,ENOMEM,NULL));
			 return;
		}
		if (*(int*)val->value == val->missing.intval) 
			preview = True;
		NclFreeExtValue(val);
		
		/*
		 * can't do preview configuration right now
		 */
		if (preview)
			return;

                NgOpenPage(ptp->go->base.id,_brHLUVAR,qnames,2,NULL);

#if 0		
		/*
		 * find the obj class
		 */
		dprof = ptp->data_profile;

		for (i = 0; i < dprof->obj_count; i++) {
			if (cdata->qname == dprof->qobjects[i]) {
				obj_class = dprof->obj_classes[i];
				break;
			}
		}

		hlu_create_rec.obj_id = NhlNULLOBJID;
		hlu_create_rec.app_id = NhlNULLOBJID;
		hlu_create_rec.class_name =  obj_class->base_class.class_name;
		hlu_create_rec.plot_style = NrmQuarkToString(dprof->qpstyle);
		hlu_create_rec.plot_style_dir = NULL;
		hlu_create_rec.has_input_data = False;
		hlu_create_rec.state = _hluNOTCREATED;
		hlu_create_rec.dprof = NULL;

		NgPostPageMessage(ptp->go->base.id,ptp->page_id,
				  _NgNOMESSAGE,_brHLUVAR,NrmNULLQUARK,
				  qnames[0],_NgHLUOBJCREATE,
				  (NhlPointer)&hlu_create_rec,True,
				  NULL,True);
#endif		
        }
        return;
}

	
int NgPlotTreeAddResList
(
        int		nclstate,
        NhlPointer	res_data,
        int		block_id
        )
{
        NgPlotTreeRec *ptp;
        NhlString res_name;
        NhlString value;
        NhlBoolean quote;
	ptNodeData *base_objndata,*ndata;
	int base_objndata_count;
	NgPlotTreeResData rdata;
	ptCompData cdata;
	int i,row_base;
	NhlBoolean cur_on;

#if DEBUG_PLOTTREE
	fprintf(stderr,"in res tree add res list\n");
#endif

        rdata = (NgPlotTreeResData) res_data;
        if (!rdata) 
		return 0;
	ptp  = (NgPlotTreeRec *) rdata->plot_tree;

	base_objndata = ptp->plot.subdata[1].subdata;
	base_objndata_count = ptp->plot.subdata[1].subcount;
	if (!base_objndata_count)
		return 0;
	if (! ptp->plot.subdata[0].expanded) 
		row_base = 2;
	else
		row_base = 2 + ptp->plot.subdata[0].subcount;

	for (i = 0; i < base_objndata_count; i++) {
		ndata = &base_objndata[i];
		if (! ndata->info) 
			continue;
		cdata = (ptCompData) ndata->info;
		if (cdata->qname == rdata->qname)
			break;
	}
	if (i == base_objndata_count)
		return 0;
	if (ptp->public.first_vpon) {
		if (! cdata->modified)
			return 0;
	}
	else {
		NhlVAGetValues(ptp->public.hlu_ids[i],
			       NhlNvpOn,&cur_on,
			       NULL);
		if (cur_on == cdata->on)
			return 0;

		cdata->on = MIN(cdata->on,cur_on);
	}
			       
	res_name = "vpOn";
	value = cdata->on ? "True" : "False";
	quote = True;

        NgNclVisBlockAddResList(nclstate,block_id,1,
                                &res_name,&value,&quote);

	cdata->modified = False;

	XtVaSetValues(ptp->public.tree,
		      XmNcolumn,2,
		      XmNrow,row_base+i,
		      XmNcellPixmap,cdata->on ? Check_Pixmap : No_Check_Pixmap,
		      NULL);
	
        return 1;
}
