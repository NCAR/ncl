/*
 *      $Id: vartree.c,v 1.10 1998-12-16 23:51:43 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		vartree.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 24 14:37:14 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/vartreeP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>
#include <ncarg/ngo/stringutil.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include  <ncarg/ngo/Tree.h>
#include <float.h>

static NrmQuark Qlong_name;
static Dimension Row_Height;
static Dimension Char_Height;
static NhlString Unnamed = "<unnamed>";


static void AdjustTextWidget
(
        NgVarTreeRec *vtp,
	int row,
	int col
        )
{
        Widget parent = vtp->tree;
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
        
        XtMoveWidget(vtp->text,x,y);

        return;
}

static void MakeRowsVisible
(
        NgVarTreeRec	*vtp,
        int top_row,
        int bottom_row
        )         
{
	XRectangle	rect;
        
	XmLGridRowColumnToXY
                (vtp->tree,
                 XmCONTENT,top_row,XmCONTENT,0,False,&rect);
        rect.height = (bottom_row - top_row + 1) * Row_Height;
        
        NgPageSetVisible(
                vtp->go->base.id,vtp->page_id,vtp->tree,&rect);
        return;
}

static char *GetLongName(
        NrmQuark qfile,
        NrmQuark qvar,
        NrmQuark qdim
        )
{
        NclApiVarInfoRec *vinfo;
        NclApiDataList	*dlist = NULL;
        int i,len;
        char *sval = NULL;
        NclExtValueRec *val;

        if (qfile != NrmNULLQUARK) {
                if (qdim != NrmNULLQUARK)
                        dlist = NclGetFileVarCoordInfo(qfile,qvar,qdim);
                else
                        dlist = NclGetFileVarInfo(qfile,qvar);
        }
        else {
                if (qdim != NrmNULLQUARK)
                        dlist = NclGetVarCoordInfo(qvar,qdim);
                else 
                        dlist = NclGetVarInfo(qvar);
        }
        
        if (!dlist)
                return NULL;
        vinfo = dlist->u.var;

	for (i = 0; i < vinfo->n_atts; i++) {
		if (vinfo->attnames[i] == Qlong_name) {
                        if (qfile != NrmNULLQUARK) {
                                if (qdim != NrmNULLQUARK)
                                        val = NclReadFileVarAtt
                                                (qfile,qdim,
                                                 vinfo->attnames[i]);
                                else
                                        val = NclReadFileVarAtt
                                                (qfile,qvar,
                                                 vinfo->attnames[i]);
                        }
                        else {
                                if (qdim != NrmNULLQUARK)
                                        val = NclReadVarCoordAtt
                                                (qvar,qdim,vinfo->attnames[i]);
                                else
                                        val = NclReadVarAtt
                                                (qvar,vinfo->attnames[i]);
                        }
                        sval = NgTypedValueToString(val,0,False,&len);
                        break;
                }
        }
        NclFreeDataList(dlist);

        return sval;
}

static void ExpandVarInfo
(
        NgVarTreeRec *vtp,
        vtNodeData	*ndata,
        int pos
        )
{
        NclApiVarInfoRec *vinfo;
        NclApiFileInfoRec *finfo;
        NclApiDataList	*dlist = NULL;
        XmLTreeRowDefinition *rowdefs;
        int rowcount,i;
        char buf[256];
        
#if	DEBUG_VARTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandFileInfo(IN)\n");
#endif
        vinfo = vtp->dlist->u.var;
        
        rowcount = 4;
        if (vtp->qfileref != NrmNULLQUARK) {
                dlist = NclGetFileInfo(vtp->qfileref);
                if (! dlist)
                        return;
                finfo = dlist->u.file;
                rowcount = 6;
        }
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));

        for (i = 0; i < rowcount; i++) {
                switch (i) {
                    case 0:
                            sprintf(buf,"Description");
                            break;
                    case 1:
                            sprintf(buf,"Type");
                            break;
                    case 2:
                            sprintf(buf,"Dimensionality");
                            break;
                    case 3:
                            sprintf(buf,"Elements");
                            break;
                    case 4:
                            sprintf(buf,"File Ref Var");
                            break;
                    case 5:
                            sprintf(buf,"Path");
                            break;
                }
                rowdefs[i].level = ndata->type / 10 + 1;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
        }
        
        XmLTreeAddRows(vtp->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
        for (i = 0; i < rowcount; i++) {
                char *sval;
                int j,size = 1,dims = 0;
                NhlBoolean do_string = True;
                        
                XmStringFree(rowdefs[i].string);
                switch (i) {
                    case 0:
                            sval = GetLongName(vtp->qfileref,
                                               vtp->qvar,NrmNULLQUARK);
                            if (sval) {
                                    sprintf(buf,"%s",sval);
                            }
                            else {
                                    do_string = False;
                            }
                            break;
                    case 1:
                            sprintf(buf,"%s",NgTypeString(vinfo->data_type));
                            break;
                    case 2:
                            for (j = 0; j < vinfo->n_dims; j++)
                                    if (vinfo->dim_info[j].dim_size > 1)
                                            dims += 1;
                            sprintf(buf,"%d",dims);
                            break;
                    case 3:
                            for (j = 0; j < vinfo->n_dims; j++)
                                    size *= vinfo->dim_info[j].dim_size;
                            sprintf(buf,"%d",size);
                            break;
                    case 4:
                            sprintf(buf,"%s",NrmQuarkToString(vtp->qfileref));
                            break;
                    case 5:
                            sprintf(buf,"%s",NrmQuarkToString(finfo->path));
                            break;
                }
                if (do_string) {
                        XmLGridSetStringsPos(vtp->tree,
                                             XmCONTENT,pos+i,XmCONTENT,1,buf);
                        vtp->c2_width = MAX(vtp->c2_width,strlen(buf));
                }
        }
        XtVaSetValues(vtp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,vtp->c2_width,
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
        NgVarTreeRec	*vtp,
        vtNodeData	*ndata,
        int		pos
        )
{
	NclExtValueRec *val = NULL;
        int	i,trows = 0;
        char buf[256];
        XmString xmempty,xmlabel;
        XmLTreeRowDefinition *rowdefs;
        
#if	DEBUG_VARTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandAttr(IN)\n");
#endif

        switch (ndata->type) {
            default:
                    fprintf(stderr,"internal expand callback error\n");
                    return;
            case _vtLVAttr:
                    if (vtp->qfileref != NrmNULLQUARK)
                            val = NclReadFileVarAtt
                                    (vtp->qfileref,
                                     ndata->parent->qname,ndata->qname);
                    else
                            val = NclReadVarAtt
                                    (ndata->parent->qname,ndata->qname);
                    break;
            case _vtLVDAttr:
                    if (vtp->qfileref != NrmNULLQUARK)
                            val = NclReadFileVarAtt
                                    (vtp->qfileref,
                                     ndata->parent->qname,ndata->qname);
                    else
                            val = NclReadVarCoordAtt
                                    (vtp->qvar,
                                     ndata->parent->qname,ndata->qname);
                    break;
        }
        if (!val) return;

        XtVaSetValues(vtp->tree,
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
                        (vtp->go->go.appmgr,buf);
        }
        XmLTreeAddRows(vtp->tree,rowdefs,2,pos);
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
                XmLGridSetStringsPos(vtp->tree,
                                     XmCONTENT,pos+i,XmCONTENT,1,buf);
                vtp->c2_width = MAX(vtp->c2_width,strlen(buf));
        }
        NhlFree(rowdefs);
	trows += 2;
        pos += 2;
        
        xmempty = NgXAppCreateXmString(vtp->go->go.appmgr," ");
        for (i = 0; i < val->totalelements; i++) {
                char *attvalue,*cp,*lastcp;
                int len,ncols,nlcount = 1,rows,size,j;
                XmString xmattval;

                attvalue = NgTypedValueToString(val,i,False,&len);
                
                cp = lastcp = attvalue;
                while ((cp = strchr(cp,'\n')) != NULL) {
                        vtp->c2_width = MAX(vtp->c2_width,cp-lastcp);
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
                xmlabel = NgXAppCreateXmString(vtp->go->go.appmgr,buf);
                for (j = 0; j < rows; j++) {
                        rowdefs[j].level = ndata->type / 10 + 1;
                        rowdefs[j].expands = False;
                        rowdefs[j].isExpanded = False;
                        rowdefs[j].pixmap = XmUNSPECIFIED_PIXMAP;
                        rowdefs[j].pixmask = XmUNSPECIFIED_PIXMAP;
                        rowdefs[j].string = j == 0 ? xmlabel : xmempty;
                }
                XmLTreeAddRows(vtp->tree,rowdefs,rows,pos);
                NgXAppFreeXmString(vtp->go->go.appmgr,xmlabel);
                NhlFree(rowdefs);
                
                XtVaSetValues(vtp->tree,
                              XmNrow,pos,
                              XmNcolumn,0,
                              XmNcellRowSpan,rows-1,
                              NULL);
                
                xmattval = NgXAppCreateXmString(vtp->go->go.appmgr,attvalue);
                XtVaSetValues(vtp->tree,
                              XmNrow,pos,
                              XmNcolumn,1,
                              XmNcellRowSpan,rows-1,
                              XmNcellString,xmattval,
                              NULL);
                NgXAppFreeXmString(vtp->go->go.appmgr,xmattval);
                pos += rows;
                
        }
        NgXAppFreeXmString(vtp->go->go.appmgr,xmempty);
        ndata->expanded = True;
        ndata->subcount = trows;

        XtVaSetValues(vtp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,vtp->c2_width,
                      NULL);
        XtVaSetValues(vtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);

        
        NclFreeExtValue(val);

        return;
        
}

static void DoSingleLineAttrVal
(
        NgVarTreeRec	*vtp,
        vtNodeData	*ndata,
        int		pos
        )
{
	NclExtValueRec *val = NULL;
        int	i,trows = 0;
        char	buf[256];
        char	*bufp;

        switch (ndata->type) {
            default:
                    fprintf(stderr,"internal expand callback error\n");
                    return;
            case _vtLVAttr:
                    if (vtp->qfileref != NrmNULLQUARK)
                            val = NclReadFileVarAtt
                                    (vtp->qfileref,
                                     ndata->parent->qname,ndata->qname);
                    else
                            val = NclReadVarAtt
                                    (ndata->parent->qname,ndata->qname);
                    break;
            case _vtLVDAttr:
                    if (vtp->qfileref != NrmNULLQUARK)
                            val = NclReadFileVarAtt
                                    (vtp->qfileref,
                                     ndata->parent->qname,ndata->qname);
                    else
                            val = NclReadVarCoordAtt
                                    (vtp->qvar,
                                     ndata->parent->qname,ndata->qname);
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
        XmLGridSetStringsPos(vtp->tree,XmCONTENT,pos,XmCONTENT,1,buf);
        vtp->c2_width = MAX(vtp->c2_width,strlen(buf));
        
        NclFreeExtValue(val);
        
        return;
        
}
static void ExpandAttrList
(
        NgVarTreeRec *vtp,
        vtNodeData	*ndata,
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
        _vtNodeType subtype;
        
#if	DEBUG_VARTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandAttrList(IN)\n");
#endif

        switch (ndata->type) {
            default:
                    fprintf(stderr,"internal expand callback error\n");
                    return;
            case _vtVAttr:
                    vinfo = vtp->dlist->u.var;
                    nattrs = vinfo->n_atts;
                    ql = vinfo->attnames;
                    subtype = _vtLVAttr;
                    break;
            case _vtVDAttr:
                    if (vtp->qfileref != NrmNULLQUARK)
                            dlist = NclGetFileVarCoordInfo
                                    (vtp->qfileref,vtp->qvar,ndata->qname);
                    else
                            dlist = NclGetVarCoordInfo
                                    (vtp->qvar,ndata->qname);
                    if (!dlist) return;
                    vinfo = dlist->u.var;
                    nattrs = vinfo->n_atts;
                    ql = vinfo->attnames;
                    subtype = _vtLVDAttr;
                    break;
        }
        NgSortQuarkList(ql,nattrs,False);
        rowdefs = NhlMalloc(nattrs * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(nattrs * sizeof(vtNodeData));

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
        
        XmLTreeAddRows(vtp->tree,rowdefs,nattrs,pos);
        ndata->expanded = True;
        ndata->subcount = nattrs;
        

        XtVaSetValues(vtp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        for (i = 0; i < nattrs; i++) {
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(vtp->tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                DoSingleLineAttrVal(vtp,&ndata->subdata[i],pos+i);
                
        }
        XtVaSetValues(vtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        NhlFree(rowdefs);
        if (dlist)
              NclFreeDataList(dlist);  


        XtVaSetValues(vtp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,vtp->c2_width,
                      NULL);
        return;
}

static void ExpandDim
(
        NgVarTreeRec *vtp,
        vtNodeData	*ndata,
        int pos
        )
{
        NclApiVarInfoRec *vinfo,*cvinfo = NULL;
        NclApiDataList	*dlist = NULL;
        NclExtValueRec *val = NULL;
        XmLTreeRowDefinition *rowdefs;
        int rowcount,i;
        char buf[256];
        NrmQuark *ql;
        _vtNodeType subtype;
        long    stride;
        NhlBoolean is_coord_var = False;
        int size = 0,dim_ix;
        
#if	DEBUG_VARTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandDim(IN)\n");
#endif
        
        vinfo = vtp->dlist->u.var;

        for (i=0; i < vinfo->n_dims; i++) {
                if (ndata->qname == vinfo->dim_info[i].dim_quark) {
                        if (vinfo->coordnames[i] != -1)
                                is_coord_var = True;
                        dim_ix = i;
                        break;
                }
        }
        rowcount = 3;
        if (is_coord_var) {
                if (vtp->qfileref != NrmNULLQUARK)
                        dlist = NclGetFileVarCoordInfo
                                (vtp->qfileref,vtp->qvar,ndata->qname);
                else
                        dlist = NclGetVarCoordInfo(vtp->qvar,ndata->qname);
                if (! dlist)
                        return;
                cvinfo = dlist->u.var;
                rowcount = 4;
        }
        
        ndata->subdata = NhlMalloc(rowcount * sizeof(vtNodeData));
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
        for (i = 0; i < rowcount; i++) {
                NhlBoolean expand = False;
                
                switch (i) {
                    case 0:
                            sprintf(buf,"Type");
                            subtype = _vtVDInfo;
                            break;
                            
                    case 1:
                            sprintf(buf,"Elements");
                            subtype = _vtVDInfo;
                            break;
                    case 2:
                            sprintf(buf,"Range");
                            subtype = _vtVDInfo;
                            break;
                    case 3:
                            sprintf(buf,"Attributes");
                            subtype = _vtVDAttr;
                            expand = cvinfo->n_atts > 0 ? True : False;
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
        
        XmLTreeAddRows(vtp->tree,rowdefs,rowcount,pos);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        
        if (cvinfo) {
                stride = MAX(1,cvinfo->dim_info[0].dim_size - 1);
                if (vtp->qfileref != NrmNULLQUARK)
                        val = NclReadFileVarCoord
                                (vtp->qfileref,vtp->qvar,ndata->qname,
                                 NULL,NULL,&stride);
                else
                        val = NclReadVarCoord
                                (vtp->qvar,ndata->qname,NULL,NULL,&stride);
        }
        
        for (i = 0; i < rowcount; i++) {
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(vtp->tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                switch (i) {
                    case 0:
                            if (! cvinfo)
                                    sprintf(buf,"index");
                            else
                                    sprintf(buf,"%s",
                                            NgTypeString(cvinfo->data_type));
                            break;
                    case 1:
                            sprintf(buf,"%d",vinfo->dim_info[dim_ix].dim_size);
                            break;
                    case 2:
                            if (! cvinfo) {
                                    if (vinfo->dim_info[dim_ix].dim_size < 2) {
                                            sprintf(buf,"[0]");
                                    }
                                    else {
                                            sprintf(buf,"[0, %d]",
                                           vinfo->dim_info[dim_ix].dim_size-1);
                                    }
                            }
                            else {
                                    int len;
                                    char *tbuf,*sval;
                                    sval = NgTypedValueToString
                                            (val,0,True,&len);
                                    sprintf(buf,"[%s",sval);
                                    tbuf = buf + len + 1;
                                    if (cvinfo->dim_info[0].dim_size > 1) {
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
                            sprintf(buf,"%d",cvinfo->n_atts);
                            break;
                }
                XmLGridSetStringsPos(vtp->tree,
                                     XmCONTENT,pos+i,XmCONTENT,1,buf);
                vtp->c2_width = MAX(vtp->c2_width,strlen(buf));
        }
        XtVaSetValues(vtp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,vtp->c2_width,
                      NULL);

        NhlFree(rowdefs);
        if (dlist)
                NclFreeDataList(dlist);
        if (val)
                NclFreeExtValue(val);


        return;
}
        
static void ExpandDimList
(
        NgVarTreeRec *vtp,
        vtNodeData	*ndata,
        int pos
        )
{
        NclApiFileInfoRec *finfo;
        NclApiVarInfoRec *vinfo;
        XmLTreeRowDefinition *rowdefs;
        int ndims,i;
        char buf[256];
        NclDimRec *drec;
        _vtNodeType subtype;
        NhlBoolean is_coord_var[256];
        
#if	DEBUG_VARTREE & DEBUG_ENTRY
	fprintf(stderr,"ExpandDimList(IN)\n");
#endif

        switch (ndata->type) {
            default:
                    fprintf(stderr,"internal expand callback error\n");
                    return;
            case _vtVDim:
                    vinfo = vtp->dlist->u.var;
                    ndims = vinfo->n_dims;
                    drec = vinfo->dim_info;
                    subtype = _vtLVDim;
                    break;
        }
        rowdefs = NhlMalloc(ndims * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(ndims * sizeof(vtNodeData));

        XtVaSetValues(vtp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        for (i = 0; i < ndims; i++) {
                is_coord_var[i] = False;
                if (ndata->type == _vtVDim) {
                        if (vinfo->coordnames[i] != -1)
                                is_coord_var[i] = True;
                }
                if (is_coord_var[i]) {
                        rowdefs[i].expands = True;
                        ndata->subdata[i].type = subtype;
                }
                else {
                        rowdefs[i].expands = True;
                        ndata->subdata[i].type = subtype;
                }
                sprintf(buf,"%s",
                        (drec[i].dim_quark <= NrmNULLQUARK ?
                         Unnamed : NrmQuarkToString(drec[i].dim_quark)));
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
        
        XmLTreeAddRows(vtp->tree,rowdefs,ndims,pos);
        ndata->expanded = True;
        ndata->subcount = ndims;
        
        for (i = 0; i < ndims; i++) {
                char *long_name = NULL;
                if (is_coord_var[i]) {
                        long_name = GetLongName
                                (vtp->qfileref,vtp->qvar,drec[i].dim_quark);
                }
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(vtp->tree,
                              XmNrow,pos+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                if (long_name) {
                        sprintf(buf,"%s",long_name);
                        XmLGridSetStringsPos
                                (vtp->tree,XmCONTENT,pos+i,XmCONTENT,1,buf);
                        vtp->c2_width = MAX(vtp->c2_width,strlen(buf));
                }
        }
        XtVaSetValues(vtp->tree,
                      XmNcolumn,1,
                      XmNcolumnWidth,vtp->c2_width,
                      NULL);
        XtVaSetValues(vtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        
        NhlFree(rowdefs);

        return;
}

static void ExpandTree 
(
        NgVarTreeRec	*vtp,
        vtNodeData	*ndata,
        int		row
        )
{
        
        if (! vtp->expand_called) {
                short		cw,ch;
                XmFontList      fontlist;
                Dimension 	h,rh;
                int		nrows;
                XmLGridRow	grid_row;

                grid_row = XmLGridGetRow(vtp->tree,XmCONTENT,row);
                XtVaSetValues(vtp->tree,
                              XmNrowSizePolicy,XmCONSTANT,
                              NULL);
                XtVaGetValues(vtp->tree,
                              XmNrowPtr,grid_row,
                              XmNfontList,&fontlist,
                              XmNrows,&nrows,
                              XmNrowHeight,&rh,
                              XmNheight,&h,
                              NULL);

                XtVaSetValues(vtp->tree,
                              XmNrowSizePolicy,XmVARIABLE,
                              NULL);
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                Char_Height = ch;
                Row_Height = MAX(rh,h/nrows);
                Row_Height = 20;
                vtp->expand_called = True;
        }

        switch (ndata->type) {
            case _vtVInfo:
                    ExpandVarInfo(vtp,ndata,row+1);
                    break;
            case _vtVAttr:
            case _vtVDAttr:
                    ExpandAttrList(vtp,ndata,row+1);
                    break;
            case _vtVDim:
                    ExpandDimList(vtp,ndata,row+1);
                    break;
            case _vtLVAttr:
            case _vtLVDAttr:
                    ExpandAttr(vtp,ndata,row+1);
                    break;
            case _vtLVDim:
                    ExpandDim(vtp,ndata,row+1);
                    break;
        }
        return;
}
        
        
static int FindRowChange
(
        NgVarTreeRec	*vtp,
        vtNodeData	*ndata
        )
{
        int i;
        int rows = 0;

        if (! ndata->subdata)
                return ndata->subcount;

        for (i = 0; i < ndata->subcount; i++) {
                rows += 1;
                if (ndata->subdata[i].expanded) {
                        rows += FindRowChange(vtp,&ndata->subdata[i]);
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
	NgVarTreeRec *vtp = (NgVarTreeRec *) udata;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        vtNodeData	*ndata;
        int		pos,row_change;
        
        cbs = (XmLGridCallbackStruct *)cb_data;
        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNrowUserData,&ndata,
                      NULL);
        
        if (ndata->subcount > 0) {
                ndata->expanded = True;
                row_change = FindRowChange(vtp,ndata);
                if (vtp->geo_notify && vtp->geo_data)
                        (*vtp->geo_notify)(vtp->geo_data);
                MakeRowsVisible(vtp,cbs->row,cbs->row + row_change);
                return;
        }
        
        ExpandTree(vtp,ndata,cbs->row);
        row_change = FindRowChange(vtp,ndata);
        
        if (vtp->geo_notify && vtp->geo_data)
                (*vtp->geo_notify)(vtp->geo_data);
        MakeRowsVisible(vtp,cbs->row,cbs->row + row_change);
        AdjustTextWidget(vtp,cbs->row,0);
        
        return;
}

static void CollapseCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgVarTreeRec *vtp = (NgVarTreeRec *) udata;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        vtNodeData	*ndata;

        cbs = (XmLGridCallbackStruct *)cb_data;
        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNrowUserData,&ndata,
                      NULL);

        ndata->expanded = False;
        
        if (vtp->geo_notify && vtp->geo_data)
                (*vtp->geo_notify)(vtp->geo_data);
}

static void FocusCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgVarTreeRec *vtp = (NgVarTreeRec *) udata;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        
        if (cb->reason == XmCR_CELL_FOCUS_IN) {
                MakeRowsVisible(vtp,cb->row,cb->row);
                AdjustTextWidget(vtp,cb->row,0);
        }
        
        return;
}

static void FreeSubNodes
(
        vtNodeData	*ndata
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
        NgVarTreeRec	*vtp,
        vtNodeData	*to_ndata,
        vtNodeData	*from_ndata,
        int		node_count,
        int		row
        )
{
        int i;

        for (i = 0; i < node_count; i++) {
		if (from_ndata[i].expanded) {
			XtVaSetValues(vtp->tree,
				      XmNrow,row,
				      XmNrowIsExpanded,True,
				      NULL);
		}
		if (from_ndata[i].subcount)
			ExpandTree(vtp,&to_ndata[i],row);
		to_ndata[i].expanded = from_ndata[i].expanded;
		row++;
                if (from_ndata[i].subdata)
                        row = ExpandNodeDataList
                                (vtp,to_ndata[i].subdata,
                                 from_ndata[i].subdata,
                                 from_ndata[i].subcount,row);
                else
                        row += to_ndata[i].subcount;
        }
        
        return row;
}

/*
 * Copies the state of an existing vartree to another vartree. If
 * to_var_tree is NULL, a new vartree is created. 
 */

NgVarTree *NgDupVarTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark 		qfileref,
        NrmQuark		qvar,
        NclApiDataList		*dlist,
	NgVarTree		*to_var_tree,
        NgVarTree		*from_var_tree
        )
{
        NgVarTreeRec *fromvtp,*tovtp;

        fromvtp = (NgVarTreeRec *) from_var_tree;
        if (!fromvtp)
                return NULL;

	if (to_var_tree) {
		NgUpdateVarTree(to_var_tree,qfileref,qvar,dlist);
		tovtp = (NgVarTreeRec *) to_var_tree;
	}
	else
		tovtp = (NgVarTreeRec *) 
			NgCreateVarTree
				(go,parent,qfileref,qvar,dlist);
        
        if (!fromvtp->expand_called)
                return (NgVarTree *) tovtp;

        ExpandNodeDataList
                (tovtp,tovtp->var.subdata,
                 fromvtp->var.subdata,fromvtp->var.subcount,0);

	return (NgVarTree *) tovtp;
        
}

NhlErrorTypes NgUpdateVarTree
(
        NgVarTree		*var_tree,
        NrmQuark		qfileref,
        NrmQuark		qvar,
        NclApiDataList		*dlist
        )
{
        NhlErrorTypes ret;
        NgVarTreeRec *vtp;
        int	i,nrows,nvisrows;
        static Dimension height;
        NhlBoolean first = True;
        NclApiVarInfoRec *vinfo;
        int	nattrs,ndims,tcount;
        XmLTreeRowDefinition *rowdefs;
        char	buf[256];
        Dimension width = 0;
        vtNodeData	*ndata;
        
        vtp = (NgVarTreeRec *) var_tree;
        if (!vtp) return NhlFATAL;

        XmLGridDeleteAllRows(vtp->tree,XmCONTENT);

        vtp->geo_notify = NULL;
        vtp->geo_data = NULL;
        vtp->qfileref = qfileref;
        vtp->qvar = qvar;
        vtp->dlist = dlist;
        vtp->page_id = NgGetPageId
                (vtp->go->base.id,vtp->qvar,vtp->qfileref);

        vinfo = dlist->u.var;
        nattrs = vinfo->n_atts;
        ndims = vinfo->n_dims;
        ndata = &vtp->var;
        ndata->parent = NULL;
        ndata->type = _vtTop;
        ndata->qname = qvar;
        ndata->expanded = True;

        if (ndata->subdata)
                FreeSubNodes(ndata);

        tcount = 3;
        rowdefs = NhlMalloc(tcount * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(tcount * sizeof(vtNodeData));
        ndata->subcount = tcount;
        
        for (i = 0; i < tcount; i++) {
                NhlBoolean expands;
                
                switch (i) {
                    case 0:
                            if (vtp->qfileref)
                                    sprintf(buf,"File Variable");
                            else 
                                    sprintf(buf,"Variable");
                            expands = True;
                            break;
                    case 1:
                            sprintf(buf,"Dimensions");
                            expands = True;
                            break;
                    case 2:
                            sprintf(buf,"Attributes");
                            expands = nattrs > 0 ? True : False;
                            break;
                }
                rowdefs[i].level = 1;
                rowdefs[i].expands = expands;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
        }
        XmLTreeAddRows(vtp->tree,rowdefs,tcount,0);

        for (i = 0; i < tcount; i++) {
                XtPointer udata;
                
                switch (i) {
                    case 0:
                            sprintf(buf,"%s",NrmQuarkToString(vtp->qvar));
                            ndata->subdata[i].type = _vtVInfo;
                            break;
                    case 1:
                            sprintf(buf,"%d",ndims);
                            ndata->subdata[i].type = _vtVDim;
                            break;
                    case 2:
                            sprintf(buf,"%d",nattrs);
                            ndata->subdata[i].type = _vtVAttr;
                            break;
                }
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].qname = qvar;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
                
                width = MAX(width,strlen(buf));
                XtVaSetValues(vtp->tree,
                              XmNrow,i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                XmLGridSetStringsPos(vtp->tree,XmCONTENT,i,XmCONTENT,1,buf);
                XmStringFree(rowdefs[i].string);
        }
        vtp->c2_width = MAX(width,14);
        vtp->expand_called = False;
        NhlFree(rowdefs);
	if (! vtp->created) {
		XtVaSetValues(vtp->tree,
			      XmNcolumn,1,
			      XmNcolumnWidth,vtp->c2_width,
			      NULL);
		vtp->created = True;
	}
	else {
		XtVaSetValues(vtp->tree,
			      XmNimmediateDraw,False,
			      XmNcolumn,1,
			      XmNcolumnWidth,vtp->c2_width,
			      NULL);
	}
        return NhlNOERROR;
}

NgVarTree *NgCreateVarTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark 		qfileref,
        NrmQuark		qvar,
        NclApiDataList		*dlist
        )
{
        NhlErrorTypes ret;
        NgVarTreeRec *vtp;
        static NhlBoolean first = True;
 
        if (first) {
                Qlong_name = NrmStringToQuark("long_name");
                first = False;
        }
        
        vtp = NhlMalloc(sizeof(NgVarTreeRec));
        if (!vtp) return NULL;

        vtp->geo_notify = NULL;
        vtp->geo_data = NULL;
        vtp->created = False;
        vtp->qfileref = qfileref;
        vtp->qvar = qvar;
        vtp->go = go;
        vtp->var.subdata = NULL;
        vtp->var.subcount = 0;
        
        vtp->tree = XtVaCreateManagedWidget("VarTree",
                                            xmlTreeWidgetClass,parent,
                                            XmNverticalSizePolicy,XmVARIABLE,
                                            XmNhorizontalSizePolicy,XmVARIABLE,
                                            XmNcolumns, 2,
                                            XmNimmediateDraw,True,
                                            NULL);
        XtVaSetValues(vtp->tree,
                      XmNcellDefaults,True,
                      XmNcellRightBorderType,XmBORDER_NONE,
                      XmNcellTopBorderType,XmBORDER_NONE,
                      XmNcellBottomBorderType,XmBORDER_NONE,
                      XmNcellAlignment,XmALIGNMENT_LEFT,
                      XmNcellMarginLeft,10,
                      NULL);
        
        XtAddCallback(vtp->tree,XmNexpandCallback,ExpandCB,vtp);
        XtAddCallback(vtp->tree,XmNcollapseCallback,CollapseCB,vtp);
        XtVaGetValues(vtp->tree,
                      XmNtextWidget,&vtp->text,
                      NULL);
        
        ret = NgUpdateVarTree((NgVarTree*) vtp,qfileref,qvar,dlist);

/*
 * wait until rows are initialized before adding focus callback; otherwise
 * we get core dumps.
 */
        XtAddCallback(vtp->tree,XmNcellFocusCallback,FocusCB,vtp);
        if (ret < NhlWARNING) {
                NhlFree(vtp);
                return NULL;
        }
        return (NgVarTree *) vtp;
}

void NgDestroyVarTree
(
        NgVarTree		*var_tree
        )
{
        NgVarTreeRec *vtp;
        
        vtp = (NgVarTreeRec *) var_tree;
        if (!vtp) return;

        FreeSubNodes(&vtp->var);

        NhlFree(vtp);
        
        return;
}
