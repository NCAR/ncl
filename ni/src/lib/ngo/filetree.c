/*
 *      $Id: filetree.c,v 1.8 1998-03-23 22:48:41 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		filetree.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 11 11:53:31 MST 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/filetreeP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/ngo/browse.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include  <ncarg/ngo/Tree.h>
#include <float.h>

static NrmQuark Qlong_name;
static Dimension Row_Height;
static Dimension Char_Height;

static void Button3Action(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static XtActionsRec filetreeactions[] = {
	{ "Button3Action", Button3Action }
};

static void AdjustTextWidget
(
        NgFileTreeRec *ftp,
	int row,
	int col
        )
{
        Widget parent = ftp->tree;
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
        NgFileTreeRec	*ftp,
        int top_row,
        int bottom_row
        )         
{
	XRectangle	rect;
        
	XmLGridRowColumnToXY
                (ftp->tree,
                 XmCONTENT,top_row,XmCONTENT,0,False,&rect);
        rect.height = (bottom_row - top_row + 1) * Row_Height;
        
        NgPageSetVisible(
                ftp->go->base.id,ftp->page_id,ftp->tree,&rect);
        return;
}

static void ExpandFileInfo
(
        NgFileTreeRec *ftp,
        ftNodeData	*ndata,
        int pos
        )
{
        NclApiFileInfoRec *finfo;
        XmLTreeRowDefinition *rowdefs;
        int rowcount,i;
        char buf[256];
        
#if	DEBUG_FILETREE
	fprintf(stderr,"ExpandFileInfo(IN)\n");
#endif
        
        finfo = ftp->dlist->u.file;
        rowcount = 2;
        
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
        
        for (i = 0; i < rowcount; i++) {
                switch (i) {
                    case 0:
                            sprintf(buf,"Path");
                            break;
                    case 1:
                            sprintf(buf,"Status");
                            break;
                }
                rowdefs[i].level = ndata->type / 10 + 1;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
        }
        
        XmLTreeAddRows(ftp->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
        for (i = 0; i < rowcount; i++) {
                XmStringFree(rowdefs[i].string);
                switch (i) {
                    case 0:
                            sprintf(buf,"%s",NrmQuarkToString(finfo->path));
                            break;
                    case 1:
                            sprintf(buf,"%s",
                                    finfo->wr_status > 0 ?
                                    "read-only" : "read-write");
                            break;
                }
                XmLGridSetStringsPos(ftp->tree,
                                     XmCONTENT,pos+i,XmCONTENT,1,buf);
                ftp->c2_width = MAX(ftp->c2_width,strlen(buf));
        }
        XtVaSetValues(ftp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ftp->c2_width,
                      NULL);
        NhlFree(rowdefs);

        return;
}
static void
FixFloat
(
	char		*buf
        )
{
        char *cp,tcp;
                            
        if (!strchr(buf,'.')) {
                strcat(buf,".0");
                if ((cp=strchr(buf,'e'))!= NULL) {
                        char *tcp;
                        for (tcp = &buf[strlen(buf)-1];
                             tcp > cp+1;tcp--) {
                                *tcp = *(tcp-2);
                        }
                        *cp = '.';
                        *(cp+1) = '0';
                }
        }
}

static void ExpandAttr
(
        NgFileTreeRec	*ftp,
        ftNodeData	*ndata,
        int		pos
        )
{
	NclExtValueRec *val = NULL;
        int	i,trows = 0;
        char buf[256];
        XmString xmempty,xmlabel;
        XmLTreeRowDefinition *rowdefs;
        
#if	DEBUG_FILETREE
	fprintf(stderr,"ExpandAttr(IN)\n");
#endif

        switch (ndata->type) {
            default:
                    fprintf(stderr,"internal expand callback error\n");
                    return;
            case _ftLAttr:
                    val = NclReadFileAtt(ftp->qfileref,ndata->qname);
                    break;
            case _ftLDAttr:
            case _ftLVAttr:
            case _ftLVDAttr:
                    val = NclReadFileVarAtt
                            (ftp->qfileref,ndata->parent->qname,ndata->qname);
                    break;
        }
        if (!val) return;

        XtVaSetValues(ftp->tree,
                      XmNlayoutFrozen,True,
                      NULL);

        rowdefs = NhlMalloc(2 * sizeof(XmLTreeRowDefinition));
        for (i = 0; i < 2; i++) {
                switch (i) {
                    case 0:
                            strcpy(buf,"Type");
                            break;
                    case 1:
                            strcpy(buf,"Elements");
                            break;
                }
                rowdefs[i].level = ndata->type / 10 + 1;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = NgXAppCreateXmString
                        (ftp->go->go.appmgr,buf);
        }
        XmLTreeAddRows(ftp->tree,rowdefs,2,pos);
        for (i = 0; i < 2; i++) {
                XmStringFree(rowdefs[i].string);
                switch (i) {
                    case 0:
                            strcpy(buf,NgTypeString(val->type));
                            break;
                    case 1:
                            sprintf(buf,"%d",val->totalelements);
                            break;
                }
                XmLGridSetStringsPos(ftp->tree,
                                     XmCONTENT,pos+i,XmCONTENT,1,buf);
                ftp->c2_width = MAX(ftp->c2_width,strlen(buf));
        }
        NhlFree(rowdefs);
	trows += 2;
        pos += 2;
        
        xmempty = NgXAppCreateXmString(ftp->go->go.appmgr," ");
        for (i = 0; i < val->totalelements; i++) {
                char *attvalue,*cp,*lastcp;
                int ncols,nlcount = 1,rows,size,j,len;
                XmString xmattval;
                
                attvalue = NgTypedValueToString(val,i,False,&len);
                cp = lastcp = attvalue;
                while ((cp = strchr(cp,'\n')) != NULL) {
                        ftp->c2_width = MAX(ftp->c2_width,cp-lastcp);
                        nlcount++;
                        cp++;
                        lastcp = cp;
                }
                rows = nlcount < 4 ? nlcount : (int)(nlcount*3.0)/4.0+0.5;
                trows += rows;
                rowdefs = NhlMalloc(rows * sizeof(XmLTreeRowDefinition));
                
                if (val->totalelements > 1)
                        sprintf(buf,"Value(%d)",i);
                else 
                        sprintf(buf,"Value");
                xmlabel = NgXAppCreateXmString(ftp->go->go.appmgr,buf);
                for (j = 0; j < rows; j++) {
                        rowdefs[j].level = ndata->type / 10 + 1;
                        rowdefs[j].expands = False;
                        rowdefs[j].isExpanded = False;
                        rowdefs[j].pixmap = XmUNSPECIFIED_PIXMAP;
                        rowdefs[j].pixmask = XmUNSPECIFIED_PIXMAP;
                        rowdefs[j].string = j == 0 ? xmlabel : xmempty;
                }
                XmLTreeAddRows(ftp->tree,rowdefs,rows,pos);
                NgXAppFreeXmString(ftp->go->go.appmgr,xmlabel);
                NhlFree(rowdefs);
                
                XtVaSetValues(ftp->tree,
                              XmNrow,pos,
                              XmNcolumn,0,
                              XmNcellRowSpan,rows-1,
                              NULL);
                
                xmattval = NgXAppCreateXmString(ftp->go->go.appmgr,attvalue);
                XtVaSetValues(ftp->tree,
                              XmNrow,pos,
                              XmNcolumn,1,
                              XmNcellRowSpan,rows-1,
                              XmNcellString,xmattval,
                              NULL);
                NgXAppFreeXmString(ftp->go->go.appmgr,xmattval);
                pos += rows;
                
        }
        NgXAppFreeXmString(ftp->go->go.appmgr,xmempty);
        ndata->expanded = True;
        ndata->subcount = trows;

        XtVaSetValues(ftp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ftp->c2_width,
                      NULL);
        XtVaSetValues(ftp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        
        NclFreeExtValue(val);

        return;
        
}

static void DoSingleLineAttrVal
(
        NgFileTreeRec	*ftp,
        ftNodeData	*ndata,
        int		pos
        )
{
	NclExtValueRec *val = NULL;
        int	i,trows = 0;
        char	buf[256];
        char	*bufp;
        
#if	DEBUG_FILETREE
	fprintf(stderr,"ExpandAttr(IN)\n");
#endif

        switch (ndata->type) {
            default:
                    fprintf(stderr,"internal expand callback error\n");
                    return;
            case _ftLAttr:
                    val = NclReadFileAtt(ftp->qfileref,ndata->qname);
                    break;
            case _ftLDAttr:
            case _ftLVAttr:
            case _ftLVDAttr:
                    val = NclReadFileVarAtt
                            (ftp->qfileref,ndata->parent->qname,ndata->qname);
                    break;
        }
        if (!val) return;

        buf[0] = '\0';
        for (i = 0,bufp = buf; i < val->totalelements; i++) {
                XmLTreeRowDefinition *rowdefs;
                char *attvalue,*cp,*nextp;
                int ncols,nlcount = 1,rows,vlen;
                
                attvalue = NgTypedValueToString(val,i,False,&vlen);
                
                for (cp = attvalue; *cp != '\0';cp++) {
                        if (! isspace(*cp))
                                break;
                }
                if (*cp == '\0')
                        continue;
                if (cp > attvalue) {
                        strncpy(bufp,"<>",
                                buf+sizeof(buf)-bufp-1);
                }
                nextp = &bufp[strlen(bufp)];
                strncat(bufp,cp,buf+sizeof(buf)-bufp-1);
                for (cp = nextp; *cp != '\0';cp++) {
                        if (*cp == '\n') {
                                while (isspace(*cp))
                                        cp--;
                                strncpy(cp+1,">>>",
                                        buf+sizeof(buf)-cp-1);
                                break;
                        }
                }
                
                rows = nlcount < 4 ? nlcount : (int)(nlcount*3.0)/4.0+0.5;
                trows += rows;
                
                if (i < val->totalelements-1)
                        strncat(bufp,", ",sizeof(buf)-strlen(buf)-1);
                if (strlen(buf) >= sizeof(buf) -1)
                        break;
                bufp = &buf[strlen(buf)];
                
        }
        XmLGridSetStringsPos(ftp->tree,XmCONTENT,pos,XmCONTENT,1,buf);
        ftp->c2_width = MAX(ftp->c2_width,strlen(buf));
        
        NclFreeExtValue(val);
        
        return;
        
}
static void ExpandAttrList
(
        NgFileTreeRec *ftp,
        ftNodeData	*ndata,
        int pos
        )
{
        NclApiFileInfoRec *finfo;
        NclApiVarInfoRec *vinfo;
        NclApiDataList	*dlist = NULL;
        XmLTreeRowDefinition *rowdefs;
        int nattrs,i;
        char buf[256];
        NrmQuark *ql;
        _ftNodeType subtype;
        
#if	DEBUG_FILETREE
	fprintf(stderr,"ExpandAttrList(IN)\n");
#endif

        switch (ndata->type) {
            default:
                    fprintf(stderr,"internal expand callback error\n");
                    return;
            case _ftAttr:
                    finfo = ftp->dlist->u.file;
                    nattrs = finfo->n_atts;
                    ql = finfo->attnames;
                    subtype = _ftLAttr;
                    break;
            case _ftDAttr:
                    dlist = NclGetFileVarInfo(ftp->qfileref,ndata->qname);
                    if (!dlist) return;
                    vinfo = dlist->u.var;
                    nattrs = vinfo->n_atts;
                    ql = vinfo->attnames;
                    subtype = _ftLDAttr;
                    break;
            case _ftVAttr:
                    dlist = NclGetFileVarInfo(ftp->qfileref,ndata->qname);
                    if (!dlist) return;
                    vinfo = dlist->u.var;
                    nattrs = vinfo->n_atts;
                    ql = vinfo->attnames;
                    subtype = _ftLVAttr;
                    break;
            case _ftVDAttr:
                    dlist = NclGetFileVarCoordInfo
                            (ftp->qfileref,ndata->parent->qname,ndata->qname);
                    if (!dlist) return;
                    vinfo = dlist->u.var;
                    nattrs = vinfo->n_atts;
                    ql = vinfo->attnames;
                    subtype = _ftLVDAttr;
                    break;
        }
        NgSortQuarkList(ql,nattrs,False);
        rowdefs = NhlMalloc(nattrs * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(nattrs * sizeof(ftNodeData));

        for (i = 0; i < nattrs; i++) {
                sprintf(buf,"%s",NrmQuarkToString(ql[i]));
                rowdefs[i].level = subtype / 10;
                rowdefs[i].expands = True;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].qname = ql[i];
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].type = subtype;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
        }
        
        XmLTreeAddRows(ftp->tree,rowdefs,nattrs,pos);
        ndata->expanded = True;
        ndata->subcount = nattrs;
        

        XtVaSetValues(ftp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        for (i = 0; i < nattrs; i++) {
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(ftp->tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                DoSingleLineAttrVal(ftp,&ndata->subdata[i],pos+i);
                
        }
        XtVaSetValues(ftp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        NhlFree(rowdefs);
        if (dlist)
              NclFreeDataList(dlist);  

        XtVaSetValues(ftp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ftp->c2_width,
                      NULL);
        return;
}

static void ExpandDim
(
        NgFileTreeRec *ftp,
        ftNodeData	*ndata,
        int pos
        )
{
        NclApiFileInfoRec *finfo;
        NclApiVarInfoRec *vinfo = NULL;
        NclApiDataList	*dlist = NULL;
        NclExtValueRec *val = NULL;
        XmLTreeRowDefinition *rowdefs;
        int rowcount,i;
        char buf[256];
        NrmQuark *ql;
        _ftNodeType subtype;
        long    stride;
        NhlBoolean is_coord_var = False;
        int size = 0,dim_ix;
        
#if	DEBUG_FILETREE
	fprintf(stderr,"ExpandDim(IN)\n");
#endif
        
        finfo = ftp->dlist->u.file;
        for (i=0; i < finfo->n_vars; i++) {
                if (ndata->qname == finfo->var_names[i]) {
                        is_coord_var = True;
                        break;
                }
        }
        for (i=0; i < finfo->n_dims; i++) {
                if (ndata->qname == finfo->dim_info[i].dim_quark) {
                        dim_ix = i;
                        break;
                }
        }
        rowcount = 3;
        if (is_coord_var) {
                dlist = NclGetFileVarInfo(ftp->qfileref,ndata->qname);
                if (!dlist)
                        return;
                vinfo = dlist->u.var;
                rowcount = 4;
        }
        
        ndata->subdata = NhlMalloc(rowcount * sizeof(ftNodeData));
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
        for (i = 0; i < rowcount; i++) {
                NhlBoolean expand = False;
                
                switch (i) {
                    case 0:
                            sprintf(buf,"Type");
                            subtype = ndata->type == _ftLDim ?
                                    _ftDInfo : _ftVDInfo;
                            
                            break;
                            
                    case 1:
                            sprintf(buf,"Elements");
                            subtype = ndata->type == _ftLDim ?
                                    _ftDInfo : _ftVDInfo;
                            
                            break;
                    case 2:
                            sprintf(buf,"Range");
                            subtype = ndata->type == _ftLDim ?
                                    _ftDInfo : _ftVDInfo;
                            
                            break;
                    case 3:
                            sprintf(buf,"Attributes");
                            subtype = ndata->type == _ftLDim ?
                                    _ftDAttr : _ftVDAttr;
                            expand = vinfo->n_atts > 0 ? True : False;
                            break;
                }
                rowdefs[i].level = subtype / 10;
                rowdefs[i].expands = expand;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].qname = ndata->qname;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].type = subtype;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
        }
        
        XmLTreeAddRows(ftp->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
        if (vinfo) {
                stride = MAX(1,vinfo->dim_info[0].dim_size - 1);
                val = NclReadFileVarCoord
                        (ftp->qfileref,ndata->qname,vinfo->coordnames[0],
                         NULL,NULL,&stride);
        }
        
         for (i = 0; i < rowcount; i++) {
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(ftp->tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                switch (i) {
                    case 0:
                            if (! vinfo)
                                    sprintf(buf,"index");
                            else
                                    sprintf(buf,"%s",
                                            NgTypeString(vinfo->data_type));
                            break;
                    case 1:
                            sprintf(buf,"%d",finfo->dim_info[dim_ix].dim_size);
                            break;
                    case 2:
                            if (! vinfo) {
                                    if (finfo->dim_info[dim_ix].dim_size < 2) {
                                            sprintf(buf,"[0]");
                                    }
                                    else {
                                            sprintf(buf,"[0, %d]",
                                           finfo->dim_info[dim_ix].dim_size-1);
                                    }
                            }
                            else {
                                    int len;
                                    char *tbuf,*sval;
                                    sval = NgTypedValueToString
                                            (val,0,True,&len);
                                    sprintf(buf,"[%s",sval);
                                    tbuf = buf + len + 1;
                                    if (vinfo->dim_info[0].dim_size > 1) {
                                            sval = NgTypedValueToString
                                                    (val,1,True,&len);
                                            sprintf(tbuf,", %s]",sval);
                                    }
                                    else {
                                            sprintf(tbuf,"]");
                                    }
                            }
                            break;
                    case 3:
                            sprintf(buf,"%d",vinfo->n_atts);
                            break;
                }
                XmLGridSetStringsPos(ftp->tree,
                                     XmCONTENT,pos+i,XmCONTENT,1,buf);
                ftp->c2_width = MAX(ftp->c2_width,strlen(buf));
        }
        XtVaSetValues(ftp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ftp->c2_width,
                      NULL);

        NhlFree(rowdefs);
        if (dlist)
                NclFreeDataList(dlist);
        if (val)
                NclFreeExtValue(val);

        return;
}

static char *GetLongName(
        NrmQuark qfile,
        NrmQuark qvar
        )
{
        NclApiVarInfoRec *vinfo;
        NclApiDataList	*dlist = NULL;
        int i;
        char *sval = NULL;
        NclExtValueRec *val;
        
        dlist = NclGetFileVarInfo(qfile,qvar);
        if (!dlist)
                return NULL;
        vinfo = dlist->u.var;

	for (i = 0; i < vinfo->n_atts; i++) {
		if (vinfo->attnames[i] == Qlong_name) {
                        int len;
                        
                        val = NclReadFileVarAtt(qfile,qvar,
                                                vinfo->attnames[i]);
                        sval = NgTypedValueToString(val,0,False,&len);
                        
                        if (val->constant != 0)
                                NclFree(val->value);
                        NclFreeExtValue(val);
                        break;
                }
        }
        NclFreeDataList(dlist);

        return sval;
}

static int dimcomp
(
 	const void *p1,
	const void *p2
)
{
	const NclDimRec drec1 = *(NclDimRec *) p1;
	const NclDimRec drec2 = *(NclDimRec *) p2;
	int ret;

	ret =  strcmp(NrmQuarkToString(drec1.dim_quark),
                      NrmQuarkToString(drec2.dim_quark));
        
	return ret;
}
        
static void ExpandDimList
(
        NgFileTreeRec *ftp,
        ftNodeData	*ndata,
        int pos
        )
{
        NclApiFileInfoRec *finfo;
        NclApiVarInfoRec *vinfo;
        NclApiDataList	*dlist = NULL;
        XmLTreeRowDefinition *rowdefs;
        int ndims,i;
        char buf[256];
        NclDimRec *drec;
        _ftNodeType subtype;
        NhlBoolean is_coord_var[256];
        
#if	DEBUG_FILETREE
	fprintf(stderr,"ExpandDimList(IN)\n");
#endif

        switch (ndata->type) {
            default:
                    fprintf(stderr,"internal expand callback error\n");
                    return;
            case _ftDim:
                    finfo = ftp->dlist->u.file;
                    ndims = finfo->n_dims;
                    drec = finfo->dim_info;
                    qsort(drec,finfo->n_dims,sizeof(NclDimRec),dimcomp);
                    subtype = _ftLDim;
                    break;
            case _ftVDim:
                    dlist = NclGetFileVarInfo(ftp->qfileref,ndata->qname);
                    if (!dlist) return;
                    vinfo = dlist->u.var;
                    ndims = vinfo->n_dims;
                    drec = vinfo->dim_info;
                    subtype = _ftLVDim;
                    break;
        }
        rowdefs = NhlMalloc(ndims * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(ndims * sizeof(ftNodeData));

        XtVaSetValues(ftp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        for (i = 0; i < ndims; i++) {
                is_coord_var[i] = False;
                if (ndata->type == _ftVDim) {
                        if (vinfo->coordnames[i] != -1)
                                is_coord_var[i] = True;
                }
                else {
                        int j;
                        for (j = 0; j < finfo->n_vars; j++) {
                                if (drec[i].dim_quark == finfo->var_names[j]) {
                                        is_coord_var[i] = True;
                                        break;
                                }
                        }
                }
                if (is_coord_var[i]) {
                        rowdefs[i].expands = True;
                        ndata->subdata[i].type = subtype;
                }
                else {
                        rowdefs[i].expands = True;
                        ndata->subdata[i].type = subtype;
                }
                sprintf(buf,"%s",NrmQuarkToString(drec[i].dim_quark));
                rowdefs[i].level = subtype / 10;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].qname = drec[i].dim_quark;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
        }
        
        XmLTreeAddRows(ftp->tree,rowdefs,ndims,pos);
        ndata->expanded = True;
        ndata->subcount = ndims;
        
        for (i = 0; i < ndims; i++) {
                char *long_name = NULL;
                if (is_coord_var[i]) {
                        long_name = GetLongName
                                (ftp->qfileref,drec[i].dim_quark);
                }
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(ftp->tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                if (long_name) {
                        sprintf(buf,"%s",long_name);
                        XmLGridSetStringsPos
                                (ftp->tree,XmCONTENT,pos+i,XmCONTENT,1,buf);
                        ftp->c2_width = MAX(ftp->c2_width,strlen(buf));
                }
        }
        XtVaSetValues(ftp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ftp->c2_width,
                      NULL);
        XtVaSetValues(ftp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        
        NhlFree(rowdefs);
        if (dlist)
              NclFreeDataList(dlist);  


        return;
}

static void ExpandVar
(
        NgFileTreeRec *ftp,
        ftNodeData	*ndata,
        int pos
        )
{
        NclApiFileInfoRec *finfo;
        NclApiVarInfoRec *vinfo;
        NclApiDataList	*dlist = NULL;
        XmLTreeRowDefinition *rowdefs;
        int rowcount,i;
        char buf[256];
        NrmQuark *ql;
        _ftNodeType subtype;
        
#if	DEBUG_FILETREE
	fprintf(stderr,"ExpandVar(IN)\n");
#endif
        
        rowcount = 4;
        
        dlist = NclGetFileVarInfo(ftp->qfileref,ndata->qname);
        if (!dlist)
                return;
        vinfo = dlist->u.var;
        
        ndata->subdata = NhlMalloc(rowcount * sizeof(ftNodeData));
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
        for (i = 0; i < rowcount; i++) {
                NhlBoolean expands = True;
                switch (i) {
                    case 0:
                            sprintf(buf,"Type");
                            subtype = _ftVInfo;
                            expands = False;
                            break;
                    case 1:
                            sprintf(buf,"Elements");
                            subtype = _ftVInfo;
                            expands = False;
                            break;
                            
                    case 2:
                            sprintf(buf,"Dimensions");
                            subtype = _ftVDim;
                            break;
                    case 3:
                            sprintf(buf,"Attributes");
                            expands = vinfo->n_atts > 0 ? True : False;
                            subtype = _ftVAttr;
                            break;
                }
                rowdefs[i].level = subtype / 10;
                rowdefs[i].expands = expands;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].qname = ndata->qname;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].type = subtype;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
        }
        
        XmLTreeAddRows(ftp->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
        for (i = 0; i < rowcount; i++) {
                int j,size = 1;
                
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(ftp->tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                switch (i) {
                    case 0:
                            sprintf(buf,"%s",NgTypeString(vinfo->data_type));
                            break;
                    case 1:
                            for (j = 0; j < vinfo->n_dims; j++)
                                    size *= vinfo->dim_info[j].dim_size;
                            sprintf(buf,"%d",size);
                            break;
                    case 2:
                            sprintf(buf,"%d",vinfo->n_dims);
                            break;
                    case 3:
                            sprintf(buf,"%d",vinfo->n_atts);
                            break;
                }
                XmLGridSetStringsPos(ftp->tree,
                                     XmCONTENT,pos+i,XmCONTENT,1,buf);
                ftp->c2_width = MAX(ftp->c2_width,strlen(buf));
        }
        XtVaSetValues(ftp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ftp->c2_width,
                      NULL);

        NhlFree(rowdefs);
        if (dlist)
              NclFreeDataList(dlist);  


        return;
}

static void ExpandVarList
(
        NgFileTreeRec *ftp,
        ftNodeData	*ndata,
        int pos
        )
{
        NclApiFileInfoRec *finfo;
        XmLTreeRowDefinition *rowdefs;
        int nvars,i;
        char buf[256];
        NrmQuark *ql;
        _ftNodeType subtype;
        
#if	DEBUG_FILETREE
	fprintf(stderr,"ExpandVarList(IN)\n");
#endif

        finfo = ftp->dlist->u.file;
        nvars = finfo->n_vars;
        ql = finfo->var_names;
        NgSortQuarkList(ql,nvars,False);
        subtype = _ftLVar;
        
        rowdefs = NhlMalloc(nvars * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(nvars * sizeof(ftNodeData));

        XtVaSetValues(ftp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        for (i = 0; i < nvars; i++) {
                sprintf(buf,"%s",NrmQuarkToString(ql[i]));
                rowdefs[i].level = subtype / 10;
                rowdefs[i].expands = True;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].qname = ql[i];
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].type = subtype;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
        }
        
        XmLTreeAddRows(ftp->tree,rowdefs,nvars,pos);
        ndata->expanded = True;
        ndata->subcount = nvars;
        
        for (i = 0; i < nvars; i++) {
                char *long_name = GetLongName(ftp->qfileref,ql[i]);
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(ftp->tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                if (long_name) {
                        sprintf(buf,"%s",long_name);
                        XmLGridSetStringsPos
                                (ftp->tree,XmCONTENT,pos+i,XmCONTENT,1,buf);
                        ftp->c2_width = MAX(ftp->c2_width,strlen(buf));
                }
                
        }
        XtVaSetValues(ftp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,ftp->c2_width,
                      NULL);
        XtVaSetValues(ftp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        NhlFree(rowdefs);


        return;
}
static void ExpandTree 
(
        NgFileTreeRec	*ftp,
        ftNodeData	*ndata,
        int		row
        )
{
        
        if (! ftp->expand_called) {
                short		cw,ch;
                XmFontList      fontlist;
                Dimension 	h,rh;
                int		nrows;
                XmLGridRow	grid_row;

                grid_row = XmLGridGetRow(ftp->tree,XmCONTENT,row);
                XtVaSetValues(ftp->tree,
                              XmNrowSizePolicy,XmCONSTANT,
                              NULL);
                XtVaGetValues(ftp->tree,
                              XmNrowPtr,grid_row,
                              XmNfontList,&fontlist,
                              XmNrows,&nrows,
                              XmNrowHeight,&rh,
                              XmNheight,&h,
                              NULL);

                XtVaSetValues(ftp->tree,
                              XmNrowSizePolicy,XmVARIABLE,
                              NULL);
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                Char_Height = ch;
                Row_Height = MAX(rh,h/nrows);
                Row_Height = 20;
                ftp->expand_called = True;
        }

        switch (ndata->type) {
            case _ftInfo:
                    ExpandFileInfo(ftp,ndata,row+1);
                    break;
            case _ftDInfo:
            case _ftVDInfo:
                    break;
            case _ftVInfo:
                    break;
            case _ftAttr:
            case _ftDAttr:
            case _ftVAttr:
            case _ftVDAttr:
                    ExpandAttrList(ftp,ndata,row+1);
                    break;
            case _ftDim:
            case _ftVDim:
                    ExpandDimList(ftp,ndata,row+1);
                    break;
            case _ftVar:
                    ExpandVarList(ftp,ndata,row+1);
                    break;

            case _ftLAttr:
            case _ftLDAttr:
            case _ftLVAttr:
            case _ftLVDAttr:
                    ExpandAttr(ftp,ndata,row+1);
                    break;
                    
            case _ftLDim:
            case _ftLVDim:
                    ExpandDim(ftp,ndata,row+1);
                    break;
                    
            case _ftLVar:
                    ExpandVar(ftp,ndata,row+1);
                    break;

        }
        return;
}
        
static int FindRowChange
(
        NgFileTreeRec	*ftp,
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
	NgFileTreeRec *ftp = (NgFileTreeRec *) udata;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        ftNodeData	*ndata;
        int		pos,row_change;
        
        cbs = (XmLGridCallbackStruct *)cb_data;
        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNrowUserData,&ndata,
                      NULL);
        
        if (ndata->subcount > 0) {
                ndata->expanded = True;
                row_change = FindRowChange(ftp,ndata);
                if (ftp->geo_notify && ftp->geo_data)
                        (*ftp->geo_notify)(ftp->geo_data);
                MakeRowsVisible(ftp,cbs->row,cbs->row + row_change);
                return;
        }
        
        XDefineCursor(ftp->go->go.x->dpy,
                      XtWindow(ftp->go->go.manager),ftp->go->go.x->wait);
        XSync(ftp->go->go.x->dpy,False);
        ExpandTree(ftp,ndata,cbs->row);
        XUndefineCursor(ftp->go->go.x->dpy,XtWindow(ftp->go->go.manager));

        row_change = FindRowChange(ftp,ndata);
        if (ftp->geo_notify && ftp->geo_data)
                (*ftp->geo_notify)(ftp->geo_data);
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
	NgFileTreeRec *ftp = (NgFileTreeRec *) udata;
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

        if (ftp->geo_notify && ftp->geo_data)
                (*ftp->geo_notify)(ftp->geo_data);
        
}

static void FocusCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgFileTreeRec *ftp = (NgFileTreeRec *) udata;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        
        if (cb->reason == XmCR_CELL_FOCUS_IN) {
                MakeRowsVisible(ftp,cb->row,cb->row);
                AdjustTextWidget(ftp,cb->row,0);
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
        NhlFree(ndata->subdata);
        return;
}

static int ExpandNodeDataList
(
        NgFileTreeRec	*ftp,
        ftNodeData	*to_ndata,
        ftNodeData	*from_ndata,
        int		node_count,
        int		row
        )
{
        int i;

        for (i = 0; i < node_count; i++) {
#if 0
		if (! from_ndata[i].expanded) {
			row++; 
                        continue;
		}
#endif
		if (from_ndata[i].expanded) {
			XtVaSetValues(ftp->tree,
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
 * Copies the state of an existing filetree to another filetree. If
 * to_file_tree is NULL, a new filetree is created. 
 */

NgFileTree *NgDupFileTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiDataList		*dlist,
	NgFileTree		*to_file_tree,
        NgFileTree		*from_file_tree
        )
{
        NgFileTreeRec *fromftp,*toftp;

        fromftp = (NgFileTreeRec *) from_file_tree;
        if (!fromftp)
                return NULL;

	if (to_file_tree) {
		NgUpdateFileTree(to_file_tree,qfileref,dlist);
		toftp = (NgFileTreeRec *) to_file_tree;
	}
	else
		toftp = (NgFileTreeRec *) 
			NgCreateFileTree
				(go,parent,qfileref,dlist);
        
        if (!fromftp->expand_called)
                return (NgFileTree *) toftp;

        ExpandNodeDataList
                (toftp,toftp->file.subdata,
                 fromftp->file.subdata,fromftp->file.subcount,0);

	return (NgFileTree *) toftp;
        
}

NhlErrorTypes NgUpdateFileTree
(
        NgFileTree		*file_tree,
        NrmQuark		qfileref,
        NclApiDataList		*dlist
        )
{
        NhlErrorTypes ret;
        NgFileTreeRec *ftp;
        int	i,nrows,nvisrows;
        static Dimension height;
        NhlBoolean first = True;
        NclApiFileInfoRec *finfo;
        int	nattrs,nvars,ndims,tcount;
        XmLTreeRowDefinition *rowdefs;
        char	buf[256];
        Dimension width = 0;
        ftNodeData	*ndata;
        
        ftp = (NgFileTreeRec *) file_tree;
        if (!ftp) return NhlFATAL;

        XmLGridDeleteAllRows(ftp->tree,XmCONTENT);
        
        ftp->geo_data = NULL;
        ftp->qfileref = qfileref;
        ftp->dlist = dlist;
        ftp->page_id = NgGetPageId
                (ftp->go->base.id,NrmNULLQUARK,ftp->qfileref);

        finfo = dlist->u.file;
        nattrs = finfo->n_atts;
        nvars = finfo->n_vars;
        ndims = finfo->n_dims;
        ndata = &ftp->file;
        ndata->parent = NULL;
        ndata->type = _ftTop;
        ndata->qname = qfileref;
        ndata->expanded = True;

        if (ndata->subdata)
                FreeSubNodes(ndata);

        tcount = 4;
        rowdefs = NhlMalloc(tcount * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(tcount * sizeof(ftNodeData));
        ndata->subcount = tcount;
        
        for (i = 0; i < tcount; i++) {
                NhlBoolean expands;
                
                switch (i) {
                    case 0:
                            sprintf(buf,"File Info");
                            expands = True;
                            break;
                    case 1:
                            sprintf(buf,"Global Attributes");
                            expands = nattrs > 0 ? True : False;
                            break;
                    case 2:
                            sprintf(buf,"Dimensions");
                            expands = True;
                            break;
                    case 3:
                            sprintf(buf,"File Variables");
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
        XmLTreeAddRows(ftp->tree,rowdefs,tcount,0);

        for (i = 0; i < tcount; i++) {
                XtPointer udata;
                
                switch (i) {
                    case 0:
                            sprintf(buf,"%s",NrmQuarkToString(ftp->qfileref));
                            ndata->subdata[i].type = _ftInfo;
                            break;
                    case 1:
                            sprintf(buf,"%d",nattrs);
                            ndata->subdata[i].type = _ftAttr;
                            break;
                    case 2:
                            sprintf(buf,"%d",ndims);
                            ndata->subdata[i].type = _ftDim;
                            break;
                    case 3:
                            sprintf(buf,"%d",nvars);
                            ndata->subdata[i].type = _ftVar;
                            break;
                }
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].qname = NrmNULLQUARK;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
                
                width = MAX(width,strlen(buf));
                XtVaSetValues(ftp->tree,
                              XmNrow,i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                XmLGridSetStringsPos(ftp->tree,XmCONTENT,i,XmCONTENT,1,buf);
                XmStringFree(rowdefs[i].string);
        }
        ftp->c2_width = width;
        XtVaSetValues(ftp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,width,
                      NULL);
        ftp->created = True;
        ftp->expand_called = False;
        NhlFree(rowdefs);
        
        return NhlNOERROR;
}


NgFileTree *NgCreateFileTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiDataList		*dlist
        )
{
        NhlErrorTypes ret;
        NgFileTreeRec *ftp;
        static NhlBoolean first = True;
        
	XtAppAddActions(go->go.x->app,
                        filetreeactions,NhlNumber(filetreeactions));
 
        if (first) {
                Qlong_name = NrmStringToQuark("long_name");
                first = False;
        }
        
        ftp = NhlMalloc(sizeof(NgFileTreeRec));
        if (!ftp) return NULL;

        ftp->geo_notify = NULL;
        ftp->geo_data = NULL;
        ftp->created = False;
        ftp->qfileref = NrmNULLQUARK;
        ftp->go = go;
        ftp->file.subdata = NULL;
        ftp->file.subcount = 0;
        
        ftp->tree = XtVaCreateManagedWidget("FileTree",
                                            xmlTreeWidgetClass,parent,
                                            XmNverticalSizePolicy,XmVARIABLE,
                                            XmNhorizontalSizePolicy,XmVARIABLE,
                                            XmNcolumns, 2,
                                            XmNuserData,ftp,
                                            NULL);
        XtVaSetValues(ftp->tree,
                      XmNcellDefaults,True,
                      XmNcellRightBorderType,XmBORDER_NONE,
                      XmNcellTopBorderType,XmBORDER_NONE,
                      XmNcellBottomBorderType,XmBORDER_NONE,
                      XmNcellAlignment,XmALIGNMENT_LEFT,
                      XmNcellMarginLeft,10,
                      NULL);
        
        XtAddCallback(ftp->tree,XmNexpandCallback,ExpandCB,ftp);
        XtAddCallback(ftp->tree,XmNcollapseCallback,CollapseCB,ftp);
        XtVaGetValues(ftp->tree,
                      XmNtextWidget,&ftp->text,
                      NULL);
        
        ret = NgUpdateFileTree((NgFileTree*) ftp,qfileref,dlist);
/*
 * wait until rows are initialized before adding focus callback; otherwise
 * we get core dumps.
 */
        XtAddCallback(ftp->tree,XmNcellFocusCallback,FocusCB,ftp);

        if (ret < NhlWARNING) {
                NhlFree(ftp);
                return NULL;
        }
        return (NgFileTree *) ftp;
}

void NgDestroyFileTree
(
        NgFileTree		*file_tree
        )
{
        NgFileTreeRec *ftp;
        
        ftp = (NgFileTreeRec *) file_tree;
        if (!ftp) return;

        FreeSubNodes(&ftp->file);

        NhlFree(ftp);
        
        return;
}

static void Button3Action(
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
        NgFileTreeRec	*ftp;
        
        
#if	DEBUG_FILETREE
	fprintf(stderr,"Button3Action(IN)\n");
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
        if (ndata->type == _ftLVar) {
                NrmQuark qnames[2];
#if DEBUG_FILETREE
                fprintf(stderr,"file var %s\n",NrmQuarkToString(ndata->qname));
#endif
                qnames[0] = ndata->qname;
                qnames[1] = ftp->qfileref;
                NgOpenPage(ftp->go->base.id,_brFILEVAR,qnames,2);
        }
        return;
}

               
