/*
 *      $Id: attrinfogrid.c,v 1.1 1997-03-04 00:04:40 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		attrinfogrid.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 11 11:53:31 MST 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/attrinfogridP.h>
#include <ncarg/ngo/xutil.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include  <XmL/Grid.h>
#include <float.h>

static char *Buffer;
static int  Buflen;
static int  Max_Width;

static char *
ColumnWidths
(
	NgAttrInfoGridRec *aip
)
{
	int	i;
        char	sizestr[10];
        int	twidth = 0;
        
        Buffer[0] = '\0';
	for (i=0; i < 2; i++) {
                int width = aip->cwidths[i];
                if (width + twidth > Max_Width)
                        width = Max_Width - twidth;
                twidth += width;
                sprintf(sizestr,"%dc ",width);
		strcat(Buffer,sizestr);
	}
        Buffer[strlen(Buffer)-1] = '\0';
#if DEBUG_ATTR_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}


static char *
TitleText
(
	NgAttrInfoGridRec	*aip,
        int			attr_count
)
{
        NclApiDataList *dlist = aip->dlist;
        char file_string[] = "%s Global Attributes";
	char no_file_string[] = "%s: No Global Attributes";
        char var_string[] = "%s Attributes";
	char no_var_string[] = "%s: No Attributes";
        char *name, *yes, *no;
        
        if (dlist->kind == FILE_LIST) {
                NclApiFileInfoRec *finfo;
		char *path;
                
                finfo = dlist->u.file;
                path = NrmQuarkToString(finfo->path);
		name = strrchr(path,'/')+1;
		if (!name)
			name = path;
		yes = file_string;
		no = no_file_string;
        }
        else if (dlist->kind == FILEVAR_LIST ||
                 dlist->kind == VARIABLE_LIST) {
                NclApiVarInfoRec *vinfo;
                
                vinfo = dlist->u.var;
                name = NrmQuarkToString(vinfo->name);
		yes = var_string;
		no = no_var_string;
        }

        if (attr_count > 0) {
                sprintf(Buffer,yes,name);
        }
        else {
                sprintf(Buffer,no,name);
                aip->cwidths[0] = MAX(aip->cwidths[0],strlen(Buffer)/2);
                aip->cwidths[1] = MAX(aip->cwidths[1],strlen(Buffer));
        }

        
#if DEBUG_ATTR_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        

        return Buffer;
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

static char *
AttrText
(
	NgAttrInfoGridRec	*aip,
        int			attnum
)
{
        NclApiDataList *dlist = aip->dlist;
	NclExtValueRec *val = NULL;
        char *attname,*attvalue;
        int cwidth0,cwidth1,i,len;
        char *cp;

        Buffer[0] = '\0';
        if (dlist->kind == FILE_LIST) {
                NclApiFileInfoRec *finfo;
                
                finfo = dlist->u.file;
                attname = NrmQuarkToString(finfo->attnames[attnum]);
                val = NclReadFileAtt(aip->qfileref,finfo->attnames[attnum]);
        }
        else if (dlist->kind == VARIABLE_LIST) {
                NclApiVarInfoRec *vinfo;
                
                vinfo = dlist->u.var;
                attname = NrmQuarkToString(vinfo->attnames[attnum]);
                if (vinfo->type == FILEVAR)
                        val = NclReadFileVarAtt(aip->qfileref,vinfo->name,
                                                vinfo->attnames[attnum]);
                else
                        val = NclReadVarAtt(vinfo->name,
                                            vinfo->attnames[attnum]);
        }
        if (!val) {
                return Buffer;
        }
        
        sprintf(Buffer,"%s|",attname);
        cwidth0 = strlen(Buffer);
        aip->cwidths[0] = MAX(aip->cwidths[0],cwidth0-1);

        for (i = 0; i < val->totalelements; i++) {
                char tbuf[256];
                char *vstart = (char *) val->value + i * val->elem_size;
                
                attvalue = NULL;
                switch (val->type) {
                    case NCLAPI_float:
                            sprintf(tbuf,"%.*g",FLT_DIG,*(float*)vstart);
                            FixFloat(tbuf);
                            break;
                    case NCLAPI_double:
                            sprintf(tbuf,"%.*g",DBL_DIG,*(double*)vstart);
                            FixFloat(tbuf);
                            break;
                    case NCLAPI_byte:
                            sprintf(tbuf,"0x%x",*(double*)vstart);
                            break;
                    default:
                            attvalue = NclTypeToString(vstart,val->type);
                            strncpy(tbuf,attvalue,sizeof(tbuf)-1);
                            break;
                }
                strncat(Buffer,tbuf,Buflen-strlen(Buffer)-1);
                if (i < val->totalelements-1)
                        strncat(Buffer,", ",Buflen-strlen(Buffer)-1);
                if (attvalue)
                        NclFree(attvalue);
        }
        len = strlen(Buffer);
        if (strlen(Buffer) > MAX_LINE_LENGTH - 1) {
                for (i = 0; i < MAX_LINE_LENGTH - 3; i++) {
                        if (isspace(Buffer[i])) {
                                i++;
                                cp = &Buffer[i];
                                while(isspace(*cp) && cp < Buffer + len)
                                        cp++;
                                if (Buffer+len-cp > 0)
                                        memcpy(&Buffer[i],cp,Buffer+len-cp);
                        }
                }
                strcpy(&Buffer[MAX_LINE_LENGTH - 3],">>");
        }
        cwidth1 = strlen(Buffer)-cwidth0+2;
        aip->cwidths[1] = MAX(aip->cwidths[1],cwidth1);
        if (cwidth0 + cwidth1 > Max_Width) {
                aip->too_long[attnum] = 1;
        }

        for (i=0; i<strlen(Buffer); i++) {
                if (Buffer[i] == '\n')
                        Buffer[i] = ' ';
        }

        
#if DEBUG_ATTR_INFO_GRID      
        fprintf(stderr,"FLT_DIG %d, DBL_DIG %d\n",FLT_DIG,DBL_DIG);
        fprintf(stderr,"%s\n",Buffer);
#endif        
	if (val->constant != 0)
		NclFree(val->value);
        NclFreeExtValue(val);
        
        return Buffer;
}

NhlErrorTypes NgUpdateAttrInfoGrid
(
        NgAttrInfoGrid		attr_info_grid,
        NrmQuark		qfileref,
        NclApiDataList		*dlist
        )
{
        NhlErrorTypes ret;
        NgAttrInfoGridRec *aip;
        int	nattrs,i;
        int	max_width,root_w;
        short	cw,ch;
        XmFontList      fontlist;
        
        aip = (NgAttrInfoGridRec *) attr_info_grid;
        if (!aip) return NhlFATAL;
        
        XtVaGetValues(aip->grid,
                      XmNfontList,&fontlist,
                      NULL);

        XmLFontListGetDimensions(fontlist,&cw,&ch,True);
        root_w = WidthOfScreen(XtScreen(aip->grid));
        Max_Width = root_w / cw - cw;

        aip->qfileref = qfileref;
        aip->dlist = dlist;

        if (dlist->kind == FILE_LIST) {
                NclApiFileInfoRec *finfo;
                finfo = dlist->u.file;
                nattrs = finfo->n_atts;
        }
        else {
                NclApiVarInfoRec *vinfo;
                vinfo = dlist->u.var;
                nattrs = vinfo->n_atts;
        }
        if (nattrs > aip->c_alloc) {
                aip->too_long = NhlRealloc(aip->too_long,
                                           nattrs * sizeof(short));
                aip->last_too_long = NhlRealloc(aip->last_too_long,
                                                nattrs * sizeof(short));
                memset((char *)(aip->last_too_long+aip->c_alloc),
                                0,(nattrs - aip->c_alloc) * sizeof(short));
                aip->c_alloc = nattrs;
        }
        memset((char *)aip->too_long,0,nattrs * sizeof(short));

        XtVaSetValues(aip->grid,
                      XmNrows,nattrs + 1,
                      XmNvisibleRows,nattrs + 1,
                      NULL);
        XtVaSetValues(aip->grid,
                      XmNrow,0,
                      XmNcolumn,0,
                      XmNcellColumnSpan,2,
                      NULL);

        for (i = 0; i < 2; i++)
                aip->cwidths[i] = 0;
        
        XmLGridSetStringsPos(aip->grid,XmCONTENT,0,XmCONTENT,0,
                             TitleText(aip,nattrs));
        for (i = 0; i < nattrs; i++) {
                XmLGridSetStringsPos(aip->grid,XmCONTENT,i+1,XmCONTENT,0,
                                     AttrText(aip,i));
                if (aip->too_long[i] != aip->last_too_long[i]) {
                        int align = aip->too_long[i] ?
                                XmALIGNMENT_LEFT : XmALIGNMENT_CENTER;
                        XtVaSetValues(aip->grid,
                                      XmNrow,i+1,
                                      XmNcolumn,1,
                                      XmNcellAlignment,align,
                                      NULL);
                }
        }
        
        XtVaSetValues(aip->grid,
                      XmNsimpleWidths,ColumnWidths(aip),
                      NULL);
        XmLGridSelectRow(aip->grid,0,False);

        memcpy(aip->last_too_long,aip->too_long,nattrs*sizeof(short));
        
        return NhlNOERROR;
}


NgAttrInfoGrid NgCreateAttrInfoGrid
(
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiDataList		*dlist
        )
{
        NhlErrorTypes ret;
        NgAttrInfoGridRec *aip;
        static NhlBoolean first = True;

        if (first) {
                Buffer = NhlMalloc(BUFINC);
                Buflen = BUFINC;
                first = False;
        }
        
        aip = NhlMalloc(sizeof(NgAttrInfoGridRec));
        if (!aip) return NULL;

        aip->too_long = NhlMalloc(16 * sizeof(short));
        aip->last_too_long = NhlMalloc(16 * sizeof(short));
        memset(aip->too_long,0,16 * sizeof(short));
        memset(aip->last_too_long,0,16 * sizeof(short));
        aip->c_alloc = 16;
        
        aip->grid = XtVaCreateManagedWidget("AttrInfoGrid",
                                            xmlGridWidgetClass,parent,
                                            XmNcolumns,2,
                                            XmNhorizontalSizePolicy,XmVARIABLE,
                                            NULL);
        
        ret = NgUpdateAttrInfoGrid((NgAttrInfoGrid) aip,qfileref,dlist);

        if (ret < NhlWARNING) {
                NhlFree(aip);
                return NULL;
        }
        return (NgAttrInfoGrid) aip;
}

        
