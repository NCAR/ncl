/*
 *      $Id: functree.c,v 1.2 1999-12-24 01:29:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		functree.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Nov 12 10:13:12 MST 1999
 *
 *	Description:	
 */

#include <ncarg/ngo/functreeP.h>
#include <ncarg/ngo/browseP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/ngo/hlupage.h>
#include <ncarg/ngo/plotapp.h>
#include <ncarg/hlu/View.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include  <ncarg/ngo/Tree.h>
#include <float.h>

static Dimension Row_Height;
static Pixel Foreground,Background;


#if 0
static void Button3ObjectAction(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static XtActionsRec functreeactions[] = {
	{ "Button3ObjectAction", Button3ObjectAction }
};
#endif
static void AdjustTextWidget
(
        NgFuncTreeRec *ftp,
	int row,
	int col
        )
{
	NgFuncTree *pub = &ftp->public;
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
        
        XtMoveWidget(ftp->text,x,y);

        return;
}

static void MakeRowsVisible
(
        NgFuncTreeRec	*ftp,
        int top_row,
        int bottom_row
        )         
{
	NgFuncTree *pub = &ftp->public;
	XRectangle	rect;
        
	rect.x = rect.y = rect.width = 0;
	XmLGridRowColumnToXY
                (pub->tree,
                 XmCONTENT,top_row,XmCONTENT,0,False,&rect);
        rect.height = (bottom_row - top_row + 1) * Row_Height;
#if 0        
        NgPageSetVisible(
                ftp->go->base.id,ftp->page_id,pub->tree,&rect);
#endif
        return;
}

static void ExpandFuncInfo
(
        NgFuncTreeRec *ftp,
        ftNodeData	*ndata,
        int pos
        )
{
	NgFuncTree *pub = &ftp->public;
        XmLTreeRowDefinition *rowdefs;
	NgDataProfile	dprof;
	NgDataItem	ditem = NULL;
	NgResInfo	rinfo = NULL;
	NhlString 	wkname;
        int rowcount,i;
        char buf[256];
        
#if	DEBUG_FUNCTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandFuncInfo(IN)\n");
#endif
        
        rowcount = 1;
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(rowcount * sizeof(ftNodeData));

        for (i = 0; i < rowcount; i++) {
                switch (i) {
                    case 0:
                            sprintf(buf,"Function Name");
                            break;
                }
                rowdefs[i].level = ndata->type / 10 + 1;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].info = NULL;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].type = _ftPInfoDatum;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
        }
        
        XmLTreeAddRows(pub->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
        dprof = ftp->data_profile;
	if (dprof) {
		ditem = dprof->ditems[ftp->data_ix];
		rinfo = ditem->res_info;
	}
        for (i = 0; i < rowcount; i++) {
                NhlBoolean do_string = True;
                        
                XmStringFree(rowdefs[i].string);
                switch (i) {
                    case 0:
			    if (rinfo)
				    sprintf(buf,"%s",
					    NrmQuarkToString(rinfo->qsym));
			    else
				    sprintf(buf,"%s","<Unknown>");
                            break;
                }
                if (do_string) {
                        XmLGridSetStringsPos(pub->tree,
                                             XmCONTENT,pos+i,XmCONTENT,1,buf);
                        ftp->c2_width = MAX(ftp->c2_width,strlen(buf));
                }
        }
        XtVaSetValues(pub->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,(int) (ftp->c2_width *
 		      ftp->go->go.x->avg_font_width_mult),
                      NULL);
        NhlFree(rowdefs);


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
	NgFuncTreeRec *ftp = (NgFuncTreeRec *)udata.ptrval;
	_NhlValueSetCBData vsdata = (_NhlValueSetCBData) cbdata.ptrval;
	ftNodeData *base_objndata,*ndata;
	int base_objndata_count;
	NhlLayer l;
	NrmQuark qvname;
	NhlBoolean on;
	ftCompData cdata;
	int i;
	int row_base;

#if DEBUG_FUNCTREE
        fprintf(stderr,"in functree setval cb\n");
#endif
	return;

	if (! ftp)
		return;
	l = _NhlGetLayer(vsdata->id);

	if (! l)
		return;
	qvname = NrmStringToQuark(l->base.name);

/*
 * this is very simplistic and will have to be modified if this tree type
 * becomes more complicated.
 */
	base_objndata = ftp->func.subdata[1].subdata;
	base_objndata_count = ftp->func.subdata[1].subcount;
	if (!base_objndata_count)
		return;
	if (! ftp->func.subdata[0].expanded) 
		row_base = 2;
	else
		row_base = 2 + ftp->func.subdata[0].subcount;
/*
 *
 */

	for (i = 0; i < base_objndata_count; i++) {
		ndata = &base_objndata[i];
		if (! ndata->info) 
			continue;
		cdata = (ftCompData) ndata->info;
		if (cdata->qname == qvname)
			break;
	}
        if (i == base_objndata_count)
		return;
	
	NhlVAGetValues(l->base.id,
		       NhlNvpOn,&on,
		       NULL);
	if (cdata->ft_cb)
		cdata->ft_cb = False;
	else {
		cdata->usr_on = on;
	}
	return;
}
#endif
static void ExpandComponentInfo
(
        NgFuncTreeRec *ftp,
        ftNodeData	*ndata,
        int pos
        )
{
	NgFuncTree *pub = &ftp->public;
        XmLTreeRowDefinition *rowdefs;
        int rowcount,i;
        char buf[256];
	NhlArgVal sel,user_data;
	NgDataProfile	dprof;
	NgDataItem	ditem = NULL;
	NgResInfo	rinfo = NULL;

        
#if	DEBUG_FUNCTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandComponentInfo(IN)\n");
#endif
        
        dprof = ftp->data_profile;
	if (dprof) {
		ditem = dprof->ditems[ftp->data_ix];
		rinfo = ditem->res_info;
	}
	if (! (rinfo && rinfo->argcount))
		return;
        rowcount = rinfo->argcount;
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(rowcount * sizeof(ftNodeData));

        for (i = 0; i < rowcount; i++) {
		NgArgInfo arginfo = &rinfo->args[i];
		ftCompData cdata = NhlMalloc(sizeof(ftCompDataRec));

		cdata->ditem = ditem;
		cdata->argix = i;
		cdata->modified = False;
		cdata->ft_cb = False;
		cdata->new_value = NULL;

		sprintf(buf,"%s",NrmQuarkToString(arginfo->qargname));
                rowdefs[i].level = ndata->type / 10 + 1;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].info = (NhlPointer) cdata;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].type = _ftPCompObj;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
        }
        
        XmLTreeAddRows(pub->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
	NhlINITVAR(sel);
	NhlINITVAR(user_data);
	sel.lngval = NrmStringToQuark(NhlNvpOn);
	user_data.ptrval = ftp;
        for (i = 0; i < rowcount; i++) {
		NgArgInfo arginfo = &rinfo->args[i];
                NhlBoolean do_string = True;
		char *cp;
                        
                XmStringFree(rowdefs[i].string);
		sprintf(buf,"%s",arginfo->sval);
		
                if (do_string) {
                        XmLGridSetStringsPos(pub->tree,
                                             XmCONTENT,pos+i,XmCONTENT,1,buf);
                        ftp->c2_width = MAX(ftp->c2_width,strlen(buf));
                }
                XtVaSetValues(ftp->public.tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
		if (ftp->edit_enabled) {
			XtVaSetValues(ftp->public.tree,
				      XmNcolumn,1,
				      XmNrow,pos+i,
				      XmNcellEditable,True,
				      XmNcellBackground,
				      ftp->go->go.edit_field_pixel,
				      NULL);
		}
		else {
			XtVaSetValues(ftp->public.tree,
				      XmNcolumn,1,
				      XmNrow,pos+i,
				      XmNcellEditable,False,
				      XmNcellBackground,
				      Background,
				      NULL);
		}
        }
        XtVaSetValues(pub->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,(int)(ftp->c2_width *
 		      ftp->go->go.x->avg_font_width_mult),
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
			sprintf(&buffer[strlen(buffer)],"%d,",
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
					"%d,",vdata->start[i]);
			else if (vdata->stride[i] == 1)
				sprintf(&buffer[strlen(buffer)],"%d:%d,",
					vdata->start[i],vdata->finish[i]);
			else
				sprintf(&buffer[strlen(buffer)],"%d:%d:%d,",
					vdata->start[i],
					vdata->finish[i],vdata->stride[i]);
		}
		/* back up 1 to remove final comma */
		buffer[strlen(buffer)-1] = ')';
	}
	return buffer;
}

static void ExpandTree 
(
        NgFuncTreeRec	*ftp,
        ftNodeData	*ndata,
        int		row
        )
{
	NgFuncTree *pub = &ftp->public;
        
        if (! ftp->expand_called) {
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
                ftp->expand_called = True;
        }

        switch (ndata->type) {
	case _ftPInfo:
		ExpandFuncInfo(ftp,ndata,row+1);
		break;
	case _ftPComp:
		ExpandComponentInfo(ftp,ndata,row+1);
		break;
        }
        return;
}
        
        
static int FindRowChange
(
        NgFuncTreeRec	*ftp,
        ftNodeData	*ndata
        )
{
        int i;
        int rows = 0;

        if (! ndata->subdata)
                return ndata->subcount;

        for (i = 0; i < ndata->subcount; i++) {
                rows += 1;
                if (ndata->subdata[i].expanded) {
                        rows += FindRowChange(ftp,&ndata->subdata[i]);
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
	NgFuncTreeRec *ftp = (NgFuncTreeRec *) udata;
	NgFuncTree *pub = &ftp->public;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        ftNodeData	*ndata;
        int		row_change;
        
        cbs = (XmLGridCallbackStruct *)cb_data;
        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNrowUserData,&ndata,
                      NULL);
        
        if (ndata->subcount > 0) {
                ndata->expanded = True;
		if (ndata->type == _ftPComp) {
			XmLGridSetStringsPos
				(pub->tree,XmCONTENT,
				 cbs->row,XmCONTENT,2,"On");
		}
                row_change = FindRowChange(ftp,ndata);
                if (pub->geo_notify && pub->geo_data)
                        (*pub->geo_notify)(pub->geo_data);
                MakeRowsVisible(ftp,cbs->row,cbs->row + row_change);
                return;
        }
        
        ExpandTree(ftp,ndata,cbs->row);
        row_change = FindRowChange(ftp,ndata);
        
        if (pub->geo_notify && pub->geo_data)
                (*pub->geo_notify)(pub->geo_data);
        MakeRowsVisible(ftp,cbs->row,cbs->row + row_change);
        AdjustTextWidget(ftp,cbs->row,0);
        
        return;
}

static void CollapseCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgFuncTreeRec *ftp = (NgFuncTreeRec *) udata;
	NgFuncTree *pub = &ftp->public;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        ftNodeData	*ndata;

        cbs = (XmLGridCallbackStruct *)cb_data;
        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNrowUserData,&ndata,
                      NULL);

        ndata->expanded = False;
        
	if (ndata->type == _ftPComp)
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
	NgFuncTreeRec *ftp = (NgFuncTreeRec *) udata;
        NgFuncTree *pub = &ftp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;
	Boolean		editable;
        
        if (! cb->reason == XmCR_CELL_FOCUS_IN)
		return;
#if DEBUG_FUNCTREE
	printf("in functree focus in cb\n");
#endif
	MakeRowsVisible(ftp,cb->row,cb->row);
	AdjustTextWidget(ftp,cb->row,0);

	if (! cb->column == 1)
		return;

	if (ftp->edit_row == cb->row)
		return;

        rowptr = XmLGridGetRow(pub->tree,XmCONTENT,cb->row);
        colptr = XmLGridGetColumn(pub->tree,XmCONTENT,cb->column);
		
	XtVaGetValues(pub->tree,
		      XmNcolumnPtr,colptr,
		      XmNrowPtr,rowptr,
		      XmNcellEditable,&editable,
		      NULL);

	if (ftp->edit_row > -1) {
		XtVaSetValues(pub->tree,
			      XmNrow,ftp->edit_row,
			      XmNcolumn,cb->column,
			      XmNcellBackground,ftp->go->go.edit_field_pixel,
			      NULL);
	}
	if (! editable) {
		ftp->edit_row = -1;
	}
	else {
		XtVaSetValues(pub->tree,
			      XmNrow,cb->row,
			      XmNcolumn,cb->column,
			      XmNcellBackground,ftp->go->go.select_pixel,
			      NULL);
		ftp->edit_row = cb->row;
	}
        
        return;
}

static void FreeSubNodes
(
        ftNodeData	*ndata
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
        NgFuncTreeRec	*ftp,
        ftNodeData	*to_ndata,
        ftNodeData	*from_ndata,
        int		node_count,
        int		row
        )
{
	NgFuncTree *pub = &ftp->public;
        int i;

        for (i = 0; i < node_count; i++) {
		if (from_ndata[i].expanded) {
			XtVaSetValues(pub->tree,
				      XmNrow,row,
				      XmNrowIsExpanded,True,
				      NULL);
		}
		if (from_ndata[i].subcount)
			ExpandTree(ftp,&to_ndata[i],row);
		to_ndata[i].expanded = from_ndata[i].expanded;
		row++;
                if (from_ndata[i].subdata)
                        row = ExpandNodeDataList
                                (ftp,to_ndata[i].subdata,
                                 from_ndata[i].subdata,
                                 from_ndata[i].subcount,row);
                else
                        row += to_ndata[i].subcount;
        }
        
        return row;
}

/*
 * Copies the state of an existing functree to another functree. If
 * to_func_tree is NULL, a new functree is created. 
 */

NgFuncTree *NgDupFuncTree
(
        NgGO			go,
        Widget			parent,
	NrmQuark		qname,
	int			data_ix,
	NgDataProfile		data_profile,
	NgFuncTree		*to_func_tree,
        NgFuncTree		*from_func_tree
        )
{
        NgFuncTreeRec *fromftp,*toftp;

        fromftp = (NgFuncTreeRec *) from_func_tree;
        if (!fromftp)
                return NULL;

	if (to_func_tree) {
		NgUpdateFuncTree
			(to_func_tree,qname,data_ix,
			 data_profile,fromftp->edit_enabled);
		toftp = (NgFuncTreeRec *) to_func_tree;
	}
	else {
		toftp = (NgFuncTreeRec *) 
			NgCreateFuncTree
			(go,parent,qname,data_ix,
			 data_profile,fromftp->edit_enabled);
	}
        
        if (!fromftp->expand_called)
                return (NgFuncTree *) toftp;

        ExpandNodeDataList
                (toftp,toftp->func.subdata,
                 fromftp->func.subdata,fromftp->func.subcount,0);

	return (NgFuncTree *) toftp;
        
}

static void SetExpressionMode
(
	NgFuncTreeRec 	*ftp,
	ftNodeData	*ndata,
	NgDataItem	ditem,
	NgResInfo	rinfo
)
{
	NgFuncTree *pub = &ftp->public;
        int	tcount;
	int i;
        char	buf[256];
        XmLTreeRowDefinition *rowdefs;
        Dimension width = 0;

        tcount = 2;
        rowdefs = NhlMalloc(tcount * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(tcount * sizeof(ftNodeData));
        ndata->subcount = tcount;

        for (i = 0; i < tcount; i++) {
                NhlBoolean expands;
                
                switch (i) {
                    case 0:
			    sprintf(buf,"Value Type");
                            expands = False;
                            break;
                    case 1:
                            sprintf(buf,"Value");
                            expands = False;
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
	
        for (i = 0; i < tcount; i++) {
		ftCompData cdata;
                switch (i) {
                    case 0:
                            sprintf(buf,"%s","Expression");
                            ndata->subdata[i].type = _ftPInfo;
			    ndata->subdata[i].info = NULL;
                            break;
                    case 1:
			    cdata = NhlMalloc(sizeof(ftCompDataRec));
			    cdata->ditem = ditem;
			    cdata->argix = -1;
			    cdata->modified = False;
			    cdata->ft_cb = False;
			    cdata->new_value = NULL;

			    if (ditem->vdata->expr_val)
				    sprintf(buf,"%s",ditem->vdata->expr_val);
			    else
				    sprintf(buf,"%s","<unknown value>");

                            ndata->subdata[i].type = _ftPComp;
			    ndata->subdata[i].info = cdata;
			    if (ftp->edit_enabled) {
				    XtVaSetValues(pub->tree,
						  XmNcolumn,1,
						  XmNrow,i,
						  XmNcellEditable,True,
						  XmNcellBackground,
						  ftp->go->go.edit_field_pixel,
						  NULL);
			    }
			    else {
				    XtVaSetValues(pub->tree,
						  XmNcolumn,1,
						  XmNrow,i,
						  XmNcellEditable,False,
						  XmNcellBackground,
						  Background,
						  NULL);
			    }
                            break;
                }
                ndata->subdata[i].parent = ndata;
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
	
        ftp->c2_width = MAX(width,14);
        ftp->expand_called = False;
        NhlFree(rowdefs);
}

static void SetVarMode
(

	NgFuncTreeRec 	*ftp,
	ftNodeData	*ndata,
	NgDataItem	ditem,
	NgResInfo	rinfo
)
{
	return;
}
static void SetFuncMode
(
	NgFuncTreeRec 	*ftp,
	ftNodeData	*ndata,
	NgDataItem	ditem,
	NgResInfo	rinfo
)
{
	NgFuncTree *pub = &ftp->public;
        int	tcount;
        XmLTreeRowDefinition *rowdefs;
	int i;
        char	buf[256];
        Dimension width = 0;
	XmLGridCallbackStruct cbs;

        tcount = 2;
        rowdefs = NhlMalloc(tcount * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(tcount * sizeof(ftNodeData));
        ndata->subcount = tcount;

        for (i = 0; i < tcount; i++) {
                NhlBoolean expands;
		NhlBoolean expanded;
                
                switch (i) {
                    case 0:
			    sprintf(buf,"Value Type");
                            expands = True;
			    expanded = False;
                            break;
                    case 1:
                            sprintf(buf,"Parameters");
			    expanded = True;
                            expands = True;
                            break;
		}
                rowdefs[i].level = 1;
                rowdefs[i].expands = expands;
                rowdefs[i].isExpanded = expanded;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
        }
        XmLTreeAddRows(pub->tree,rowdefs,tcount,0);
	
        for (i = 0; i < tcount; i++) {
                switch (i) {
                    case 0:
                            sprintf(buf,"%s","Function");
                            ndata->subdata[i].type = _ftPInfo;
                            break;
                    case 1:
			    if (rinfo)
				    sprintf(buf,"%d",rinfo->argcount);
			    else
				    sprintf(buf,"0");

                            ndata->subdata[i].type = _ftPComp;
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
	
        ftp->c2_width = MAX(width,14);
        ftp->expand_called = False;
        NhlFree(rowdefs);

	cbs.row = 1;
	ExpandCB(pub->tree,ftp,&cbs);
	
}

static void SelectCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgFuncTreeRec *ftp = (NgFuncTreeRec *) udata;
        NgFuncTree *pub = &ftp->public;
        XmLGridCallbackStruct *cb;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;
	Boolean		editable;

#if DEBUG_FUNCTREE
	fprintf(stderr,"in select callback\n");
#endif

        cb = (XmLGridCallbackStruct *)cb_data;
        rowptr = XmLGridGetRow(pub->tree,XmCONTENT,cb->row);
        colptr = XmLGridGetColumn(pub->tree,XmCONTENT,cb->column);

	if (! cb->column == 1)
		return;
		
	XtVaGetValues(pub->tree,
		      XmNcolumnPtr,colptr,
		      XmNrowPtr,rowptr,
		      XmNcellEditable,&editable,
		      NULL);

	if (ftp->edit_row != cb->row) {
		if (ftp->edit_row > -1) {
			XtVaSetValues(pub->tree,
				      XmNrow,ftp->edit_row,
				      XmNcolumn,cb->column,
				      XmNcellBackground,
				      ftp->go->go.edit_field_pixel,
				      NULL);
		}
		if (! editable) {
			ftp->edit_row = -1;
		}
		else {
			XtVaSetValues(pub->tree,
				      XmNrow,cb->row,
				      XmNcolumn,cb->column,
				      XmNcellBackground,
				      ftp->go->go.select_pixel,
				      NULL);
			ftp->edit_row = cb->row;
		}
	}

	if (editable && ! ftp->manual_edit_started) {
		XmLGridEditBegin(pub->tree,True,cb->row,cb->column);
	}

}
        

static void EditCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgFuncTreeRec *ftp = (NgFuncTreeRec *) udata;
        NgFuncTree *pub = &ftp->public;
        XmLGridCallbackStruct *cb;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;
        ftNodeData	*ndata;
        NhlBoolean	update = True;

        cb = (XmLGridCallbackStruct *)cb_data;
        rowptr = XmLGridGetRow(pub->tree,XmCONTENT,cb->row);
        colptr = XmLGridGetColumn(pub->tree,XmCONTENT,cb->column);
        
        switch (cb->reason) {
	    char	*cur_string;
            case XmCR_EDIT_INSERT:
#if DEBUG_FUNCTREE        
                    fprintf(stderr,"edit insert\n");
#endif
                    XtVaSetValues(ftp->text,
                                  XmNborderWidth,2,
                                  XmNcursorPositionVisible,True,
				  XmNbackground,ftp->go->go.select_pixel,
                                  NULL);
		    if (! ftp->text_dropped) {
			    if (ftp->selected_row_xmstr)
				    XmStringFree(ftp->selected_row_xmstr);
			    XtVaGetValues
				    (pub->tree,
				     XmNcolumnPtr,colptr,
				     XmNrowPtr,rowptr,
				     XmNcellString,&ftp->selected_row_xmstr,
				     NULL);
			    XmTextSetInsertionPosition(ftp->text,0);
		    }
		    else {
			    ftp->text_dropped = False;
			    XmStringGetLtoR(ftp->selected_row_xmstr,
					    XmFONTLIST_DEFAULT_TAG,
					    &cur_string);
			    XmTextInsert(ftp->text,0,cur_string);
			    XmTextSetInsertionPosition(ftp->text,
						       strlen(cur_string));
			    XtFree(cur_string);
		    }
		    ftp->manual_edit_started = True;
                    ftp->edit_row = cb->row;
		    XtSetKeyboardFocus(ftp->go->go.manager,ftp->text);
                    
                    return;
            case XmCR_EDIT_BEGIN:
#if DEBUG_FUNCTREE        
                    fprintf(stderr,"edit begin\n");
#endif
		    if (ftp->selected_row_xmstr)
			    XmStringFree(ftp->selected_row_xmstr);
		    XtVaGetValues
			    (pub->tree,
			     XmNcolumnPtr,colptr,
			     XmNrowPtr,rowptr,
			     XmNcellString,&ftp->selected_row_xmstr,
			     NULL);
		    XmStringGetLtoR(ftp->selected_row_xmstr,
				    XmFONTLIST_DEFAULT_TAG,&cur_string);
                    XtVaSetValues(ftp->text,
				  XmNvalue,cur_string,
                                  XmNcursorPosition,0,
                                  XmNborderWidth,2,
                                  XmNcursorPositionVisible,True,
				  XmNbackground,ftp->go->go.select_pixel,
                                  NULL);
                    ftp->edit_row = cb->row;
                    ftp->manual_edit_started = True;
		    XtFree(cur_string);
		    XtSetKeyboardFocus(ftp->go->go.manager,ftp->text);
                    return;
            case XmCR_EDIT_CANCEL:
#if DEBUG_FUNCTREE
                    fprintf(stderr,"edit cancel\n");
#endif
                    update = False;
		    ftp->manual_edit_started = False;
                    break;
            case XmCR_EDIT_COMPLETE:
#if DEBUG_FUNCTREE
                    fprintf(stderr,"edit complete\n");
#endif
		    ftp->manual_edit_started = False;
                    break;
        }
	/*
	 * only get to here on cancel or complete
	 */
	ftp->edit_row = -1;
        if (update) {
                ftCompData comp;
                char *new_string,*cur_string = NULL;
                
                XtVaGetValues(w,
                              XmNrowPtr,rowptr,
                              XmNrowUserData,&ndata,
                              NULL);
		if (ftp->selected_row_xmstr) {
			XmStringGetLtoR(ftp->selected_row_xmstr,
					XmFONTLIST_DEFAULT_TAG,&cur_string);
		}
                new_string = XmTextGetString(ftp->text);
		if (!cur_string || strcmp(new_string,cur_string)) {
                	comp = (ftCompData)ndata->info;
			if (comp->new_value)
				NhlFree(comp->new_value);
			comp->new_value = NhlMalloc(strlen(new_string)+1);
			strcpy(comp->new_value,new_string);
		}
		if (cur_string)
			XtFree(cur_string);
		XtFree(new_string);
        }
        XtVaSetValues(pub->tree,
                      XmNcolumn,1,
                      XmNrow,cb->row,
		      XmNcellBackground,ftp->go->go.edit_field_pixel,
                      NULL);
        XtVaSetValues(ftp->text,
#if 0
                      XmNvalue,ftp->selected_row_xmstr,
#endif
		      XmNbackground,ftp->go->go.edit_field_pixel,
                      NULL);
	XtSetKeyboardFocus(ftp->go->go.manager,ftp->text);
#if DEBUG_FUNCTREE
	fprintf(stderr,"leaving functree edit\n");
#endif
        return;
        
}

NhlErrorTypes NgUpdateFuncTree
(
        NgFuncTree		*func_tree,
	NrmQuark		qname,
	int			data_ix,
	NgDataProfile		data_profile,
	NhlBoolean		edit_enabled
        )
{
        NgFuncTreeRec *ftp;
	NgFuncTree *pub;
        int	i;
	NgDataProfile	dprof;
	NgDataItem	ditem = NULL;
	NgResInfo	rinfo = NULL;
	NclApiFuncInfoRec *finfo = NULL;
	ftNodeData	*ndata;
	
        
        ftp = (NgFuncTreeRec *) func_tree;
        if (!ftp) return NhlFATAL;
	pub = func_tree;


        pub->geo_notify = NULL;
        pub->geo_data = NULL;
	ftp->qname = qname;
        ftp->data_ix = data_ix;
	ftp->data_profile = data_profile;
	ftp->edit_enabled = edit_enabled;

	ftp->arg_count = 0;
	ftp->sv_cbs = NULL;
	ftp->edit_row = -1;
	if (ftp->created) {
		XtRemoveCallback(pub->tree,XmNcellFocusCallback,FocusCB,ftp);
	}
        XmLGridDeleteAllRows(pub->tree,XmCONTENT);


        ndata = &ftp->func;
        ndata->parent = NULL;
        ndata->type = _ftTop;
        ndata->info = NULL;
        ndata->expanded = True;

        if (ndata->subdata)
                FreeSubNodes(ndata);

	dprof = ftp->data_profile;
	if (dprof) {
		ditem = dprof->ditems[data_ix];
		rinfo = ditem->res_info;
	}
	if (! rinfo) {
		SetExpressionMode(ftp,ndata,ditem,rinfo);
	}
	else {
		switch (rinfo->valtype) {
		default:
			SetExpressionMode(ftp,ndata,ditem,rinfo);
			break;
#if 0
			SetVarMode(ftp,ndata,ditem,rinfo);
			break;
#endif
		case _NgFUNC:
			SetFuncMode(ftp,ndata,ditem,rinfo);
			break;
		}
	}
	if (! ftp->created) {
		XtVaSetValues(pub->tree,
			      XmNcolumn,1,
			      XmNcolumnWidth,(int)(ftp->c2_width *
			      ftp->go->go.x->avg_font_width_mult),
			      NULL);
		ftp->created = True;
	}
	else {
		XtVaSetValues(pub->tree,
			      XmNimmediateDraw,False,
			      XmNcolumn,1,
			      XmNcolumnWidth,(int)(ftp->c2_width *
			      ftp->go->go.x->avg_font_width_mult),
			      NULL);
	}
	
/*
 * wait until rows are initialized before adding focus callback; otherwise
 * we get core dumps.
 */
        XtAddCallback(pub->tree,XmNcellFocusCallback,FocusCB,ftp);
        return NhlNOERROR;
	
}
	

	
NgFuncTree *NgCreateFuncTree
(
        NgGO			go,
        Widget			parent,
	NrmQuark		qname,
	int			data_ix,
	NgDataProfile		data_profile,
	NhlBoolean		edit_enabled
        )
{
        NhlErrorTypes ret;
        NgFuncTreeRec *ftp;
	NgFuncTree *pub;
        static NhlBoolean first = True;

#if 0 
	XtAppAddActions(go->go.x->app,
                        functreeactions,NhlNumber(functreeactions));
#endif
        
	if (first) {
                XtVaGetValues(go->go.shell,
                              XmNforeground,&Foreground,
                              XmNbackground,&Background,
                              NULL);
		first = False;
	}		
        ftp = NhlMalloc(sizeof(NgFuncTreeRec));
        if (!ftp) return NULL;
	pub = &ftp->public;

        pub->geo_notify = NULL;
        pub->geo_data = NULL;
        ftp->created = False;
        ftp->qname = qname;
	ftp->data_profile = data_profile;
        ftp->go = go;
	ftp->text_dropped = False;
	ftp->selected_row_xmstr = NULL;
	ftp->manual_edit_started = False;
	ftp->edit_row = -1;
        ftp->func.subdata = NULL;
        ftp->func.subcount = 0;
	ftp->ignore_focus_cb = False;
	ftp->focus_cb = NULL;
	ftp->edit_enabled = edit_enabled;
        
        pub->tree = XtVaCreateManagedWidget
		("FuncTree",
		 xmlTreeWidgetClass,parent,
		 XmNverticalSizePolicy,XmVARIABLE,
		 XmNhorizontalSizePolicy,XmVARIABLE,
		 XmNcolumns, 2,
		 XmNimmediateDraw,True,
		 XmNuserData,ftp,
		 XmNselectionPolicy,XmSELECT_NONE,
#if 0
		 XmNdebugLevel,3,
#endif
		 NULL);

        XtVaSetValues(pub->tree,
                      XmNcellDefaults,True,
                      XmNcellRightBorderType,XmBORDER_NONE,
                      XmNcellTopBorderType,XmBORDER_NONE,
                      XmNcellBottomBorderType,XmBORDER_NONE,
                      XmNcellAlignment,XmALIGNMENT_LEFT,
                      XmNcellMarginLeft,10,
                      NULL);

        
        XtAddCallback(pub->tree,XmNexpandCallback,ExpandCB,ftp);
        XtAddCallback(pub->tree,XmNcollapseCallback,CollapseCB,ftp);
        XtAddCallback(pub->tree,XmNeditCallback,EditCB,ftp);
        XtAddCallback(pub->tree,XmNselectCallback,SelectCB,ftp);
        XtVaGetValues(pub->tree,
                      XmNtextWidget,&ftp->text,
                      NULL);
        
        ret = NgUpdateFuncTree
		((NgFuncTree*) ftp,qname,data_ix,
		 data_profile,ftp->edit_enabled);

        if (ret < NhlWARNING) {
                NhlFree(ftp);
                return NULL;
        }
        return (NgFuncTree *) ftp;
}

void NgDestroyFuncTree
(
        NgFuncTree		*func_tree
        )
{
        NgFuncTreeRec *ftp;
        
        ftp = (NgFuncTreeRec *) func_tree;
        if (!ftp) return;

        FreeSubNodes(&ftp->func);

        NhlFree(ftp);
        
        return;
}
	

ftNodeData *FindNodeOfType
(
	ftNodeData 	*start_node,
	_ftNodeType	type
)
{
	int i;
	ftNodeData 	*node = start_node;
	ftNodeData	*nnode;
/*
 * look downwards in subdata first;
 * otherwise look at the next node by traversing up to the parent
 */
	if (! node)
		return NULL;
	if (node->type == type)
		return node;

	for (i = 0; i < node->subcount; i++) {
		nnode = FindNodeOfType(&node->subdata[i],type);
		if (nnode)
			return nnode;
	}
	return NULL;
}

NhlString NgGetFuncTreeValue
(
	NgFuncTree		*func_tree,
	int			*data_ix,
	NhlBoolean		*is_new_value
        )
{
	NgFuncTreeRec		*ftp = (NgFuncTreeRec	*)func_tree;
	NhlString value;
	NgDataProfile	dprof;
	NgDataItem	ditem = NULL;
	NgResInfo	rinfo = NULL;
	ftNodeData	*ndata,*pdata;
	ftCompData	cdata;
	int		size,i;

	*is_new_value = False;
	*data_ix = ftp->data_ix;

#if	DEBUG_FUNCTREE & DEBUG_ENTRY
	fprintf(stderr,"NgGetFuncTreeValue(IN)\n");
#endif
        
	XmLGridEditComplete(func_tree->tree);
        dprof = ftp->data_profile;
	if (dprof) {
		ditem = dprof->ditems[ftp->data_ix];
		rinfo = ditem->res_info;
	}
	if (! rinfo)
		return NULL;

	if (rinfo->valtype != _NgFUNC) {
		ndata = FindNodeOfType(&ftp->func,_ftPComp);

		if (! ndata && ndata->info) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"internal error"));
			return NULL;
		}
		cdata = (ftCompData) ndata->info;
		
		if (cdata->new_value) {
			value = NhlMalloc(strlen(cdata->new_value)+1);
			strcpy(value,cdata->new_value);
			*is_new_value = True;
			return value;
		}
		else if (ditem->vdata->expr_val) {
			value = NhlMalloc(strlen(ditem->vdata->expr_val)+1);
			strcpy(value,ditem->vdata->expr_val);
			return value;
		}
		else
			return NULL;
	}
	
	size = strlen(NrmQuarkToString(rinfo->qsym)) + 
		rinfo->argcount + 2;
	for (i = 0; i < rinfo->argcount; i++) {
		size += strlen(rinfo->args[i].sval);
	}
	value = NhlMalloc(size);

	sprintf(value,"%s(",NrmQuarkToString(rinfo->qsym));

	pdata = FindNodeOfType(&ftp->func,_ftPComp);
	if (! (pdata->subcount == rinfo->argcount)) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"internal error"));
		return NULL;
	}

	for (i = 0; i < rinfo->argcount; i++) {
		ndata = &pdata->subdata[i];
		if (! ndata->info) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"internal error"));
			return NULL;
		}
		cdata = (ftCompData) ndata->info;
		
		if (! cdata->new_value) {
			sprintf(&value[strlen(value)],
				"%s,",rinfo->args[i].sval);
			continue;
		}
		if (strlen(cdata->new_value) > strlen(rinfo->args[i].sval)) {
			int inc = strlen(cdata->new_value) -
				strlen(rinfo->args[i].sval);
			size += inc;
			value = NhlRealloc(value,size);
		}
		sprintf(&value[strlen(value)],"%s,",cdata->new_value);
		NhlFree(rinfo->args[i].sval);
		rinfo->args[i].sval = cdata->new_value;
		rinfo->args[i].modified = True;
		cdata->new_value = NULL;
		*is_new_value = True;
	}
	if (rinfo->argcount)
		/* write over last comma */
		sprintf(&value[strlen(value)-1],")");
	else 
		sprintf(&value[strlen(value)],")");

	return value;
}
	

#if 0
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
        ftNodeData	*ndata;
        NgFuncTreeRec	*ftp;
        
        
#if	DEBUG_FUNCTREE
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
                      XmNuserData,&ftp,
                      NULL);
	if (! ndata)
		return;
        if (ndata->type == _ftPCompObj) {
		NrmQuark qnames[2];
		char buf[256];
		NclExtValueRec	*val = NULL;
		NhlBoolean preview = False;
		ftCompData cdata = (ftCompData) ndata->info;

#if DEBUG_FUNCTREE
                fprintf(stderr,"file var %s\n",NrmQuarkToString(cdata->qname));
#endif
		sprintf(buf,"%s_%s",NrmQuarkToString(ftp->qname),
			NrmQuarkToString(cdata->qname));
                qnames[0] = NrmStringToQuark(buf);
                qnames[1] = ftp->qname;


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

                NgOpenPage(ftp->go->base.id,_brHLUVAR,qnames,2,NULL);

#if 0		
		/*
		 * find the obj class
		 */
		dprof = ftp->data_profile;

		for (i = 0; i < dprof->obj_count; i++) {
			if (cdata->qname == dprof->qobjects[i]) {
				obj_class = dprof->obj_classes[i];
				break;
			}
		}

		hlu_create_rec.obj_id = NhlNULLOBJID;
		hlu_create_rec.app_id = NhlNULLOBJID;
		hlu_create_rec.class_name =  obj_class->base_class.class_name;
		hlu_create_rec.func_style = NrmQuarkToString(dprof->qpstyle);
		hlu_create_rec.func_style_dir = NULL;
		hlu_create_rec.has_input_data = False;
		hlu_create_rec.state = _hluNOTCREATED;
		hlu_create_rec.dprof = NULL;

		NgPostPageMessage(ftp->go->base.id,ftp->page_id,
				  _NgNOMESSAGE,_brHLUVAR,NrmNULLQUARK,
				  qnames[0],_NgHLUOBJCREATE,
				  (NhlPointer)&hlu_create_rec,True,
				  NULL,True);
#endif		
        }
        return;
}
#endif
