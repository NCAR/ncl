/*
 *      $Id: diminfogrid.c,v 1.6 1998-12-16 23:51:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		diminfogrid.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 11 11:53:31 MST 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/diminfogridP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/stringutil.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include  <ncarg/ngo/Grid.h>

static char *Buffer;
static int  Buflen;
static NrmQuark Qlong_name;
static Dimension Row_Height;
static NhlString Unnamed = "<unnamed>";

static char *
ColumnWidths
(
	NgDimInfoGridRec *dip
)
{
	int	i;
        NclApiVarInfoRec *vinfo = dip->vinfo;
        char	sizestr[10];

        Buffer[0] = '\0';
	for (i=0; i <= vinfo->n_dims; i++) {
                sprintf(sizestr,"%dc ",dip->cwidths[i]);
                if (strlen(sizestr) + strlen(Buffer) + 1> Buflen) {
                        Buffer = NhlRealloc(Buffer,Buflen+BUFINC);
                }
		strcat(Buffer,sizestr);
	}
        Buffer[strlen(Buffer)-1] = '\0';
#if DEBUG_DIM_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}


static char *
VarTypeText
(
	NgDimInfoGridRec *dip
)
{
	int	i;
        NclApiVarInfoRec *vinfo = dip->vinfo;
	NclExtValueRec *val;
        char *cp,*sval = NULL;
        int cwidth,twidth,len;

        sprintf(Buffer,"%dD %s|",vinfo->n_dims,NgTypeString(vinfo->data_type));
        dip->cwidths[0] = MAX(dip->cwidths[0],strlen(Buffer));

	for (i = 0; i < vinfo->n_atts; i++)
		if (vinfo->attnames[i] == Qlong_name)
			break;
	if (i == vinfo->n_atts) {
                cp = NrmQuarkToString(vinfo->name);
        }
        else {
                if (dip->qfileref) {
                        val = NclReadFileVarAtt(dip->qfileref,vinfo->name,
                                                vinfo->attnames[i]);
                }
                else {
                        val = NclReadVarAtt(vinfo->name,vinfo->attnames[i]);
                }
                cp = sval = NgTypedValueToString(val,0,False,&len);
                if (val->constant != 0)
                        NclFree(val->value);
                NclFreeExtValue(val);
        }

        while (strlen(cp) + strlen(Buffer) + 1> Buflen) {
                Buffer = NhlRealloc(Buffer,Buflen+BUFINC);
        }
        strcat(Buffer,cp);
        
#if DEBUG_DIM_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        

	for (i=1,twidth=0; i <= vinfo->n_dims; i++) {
                twidth += dip->cwidths[i];
	}
        cwidth = strlen(cp);
        dip->cwidths[1] = MAX(dip->cwidths[1],
                             dip->cwidths[1]+cwidth-twidth);
        
        return Buffer;
}

static char *
DimNamesText
(
	NgDimInfoGridRec *dip
)
{
	int	i;
        NclApiVarInfoRec *vinfo = dip->vinfo;
        char *cp;

	strcpy(Buffer,"Dimensions");
        dip->cwidths[0] = MAX(dip->cwidths[0],strlen(Buffer));
        
	for (i=0; i < vinfo->n_dims; i++) {
                int clen;
                if (vinfo->dim_info[i].dim_quark <= NrmNULLQUARK)
                        cp = Unnamed;
                else
                        cp = NrmQuarkToString(vinfo->dim_info[i].dim_quark);
                clen = strlen(cp);
                while (clen + strlen(Buffer) + 2> Buflen) {
                        Buffer = NhlRealloc(Buffer,Buflen+BUFINC);
                }
                strcat(Buffer,"|");
		strcat(Buffer,cp);
                dip->cwidths[i+1] = MAX(dip->cwidths[i+1],clen);
	}
#if DEBUG_DIM_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}

static char *
ElementsText
(
	NgDimInfoGridRec *dip
)
{
	int	i;
	char	dimstr[20];
        NclApiVarInfoRec *vinfo = dip->vinfo;

	strcpy(Buffer,"Elements");
        dip->cwidths[0] = MAX(dip->cwidths[0],strlen(Buffer));
        
	for (i=0; i < vinfo->n_dims; i++) {
                strcat(Buffer,"|");
		sprintf(dimstr,"%d",vinfo->dim_info[i].dim_size);
		strcat(Buffer,dimstr);
                dip->cwidths[i+1] = MAX(dip->cwidths[i+1],strlen(dimstr));
	}
#if DEBUG_DIM_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}

static char *
RangeText
(
	NgDimInfoGridRec *dip
)
{
	Arg	args[50];
	int	nargs = 0;
	int	i;
	char	substr[32];
	NclExtValueRec *val;
        NclApiVarInfoRec *vinfo = dip->vinfo;
        int	len;
        

        strcpy(Buffer,"Range");
        dip->cwidths[0] = MAX(dip->cwidths[0],strlen(Buffer));
        
	for (i=0; i < vinfo->n_dims; i++) {
                len = strlen(Buffer)+1; /* add 1 for the | character */
		if (vinfo->coordnames[i] == -1) {
                        if (vinfo->dim_info[i].dim_size > 1) {
                                sprintf(substr,"|[0, %d]",
                                        vinfo->dim_info[i].dim_size-1);
                        }
                        else {
                                sprintf(substr,"|[0]");
                        }
                        while (strlen(substr) + strlen(Buffer) + 2> Buflen) {
                                Buffer = NhlRealloc(Buffer,Buflen+BUFINC);
                        }
			strcat(Buffer,substr);
		}
		else {
                        char    *vstart,*sval;
                        int size,len;
                        long    stride;
                        
			stride = MAX(1,vinfo->dim_info[i].dim_size - 1);
                        if (dip->qfileref) {
                                val = NclReadFileVarCoord(dip->qfileref,
                                                          vinfo->name,
                                                          vinfo->coordnames[i],
                                                          NULL,NULL,&stride);
                        }
                        else {
                                val = NclReadVarCoord(vinfo->name,
                                                      vinfo->coordnames[i],
                                                      NULL,NULL,&stride);
                        }
                        sval = NgTypedValueToString(val,0,True,&len);
                        strcat(Buffer,"|[");
			strcat(Buffer,sval);
                        size = len;
                        if (vinfo->dim_info[i].dim_size > 1) {
                                sval = NgTypedValueToString(val,1,True,&len);
                                size += strlen(sval);
                        }
                        while (size + strlen(Buffer) + 6> Buflen) {
                                Buffer = NhlRealloc(Buffer,Buflen+BUFINC);
                        }
                        if (vinfo->dim_info[i].dim_size > 1) {
                                strcat(Buffer,", ");
                                strcat(Buffer,sval);
                        }
			strcat(Buffer,"]");
			if (val->constant != 0)
				NclFree(val->value);
			NclFreeExtValue(val);
		}
                dip->cwidths[i+1] = MAX(dip->cwidths[i+1],
                                        strlen(Buffer)-len);
	}
#if DEBUG_DIM_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
	       
}

NhlErrorTypes NgUpdateDimInfoGrid
(
        NgDimInfoGrid		*dim_info_grid,
        NrmQuark		qfileref,
        NclApiVarInfoRec	*vinfo
        )
{
        NhlErrorTypes ret;
        NgDimInfoGridRec *dip;
        int	i,nrows,nvisrows;
        static Dimension height;
        NhlBoolean first = True;
        
        dip = (NgDimInfoGridRec *) dim_info_grid;
        if (!dip) return NhlFATAL;

        dip->qfileref = qfileref;
        dip->vinfo = vinfo;

        
        if (first) {
                short		cw,ch;
                XmFontList      fontlist;
                
                XtVaGetValues(dip->grid,
                              XmNfontList,&fontlist,
                              XmNrows,&nrows,
                              XmNvisibleRows,&nvisrows,
                              XmNheight,&height,
                              NULL);
                
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                Row_Height = ch + 2;
                dip->height = 6+Row_Height*(nrows)+2*(nrows-1);
                first = False;
        }
        
#if DEBUG_DIM_INFO_GRID

        fprintf(stderr,"diminfo rows %d vis rows %d height %d\n",
               nrows, nvisrows,height);
#endif
        
        nrows = dip->headline_on ? 4 : 3;
        XtVaSetValues(dip->grid,
                      XmNrows,nrows,
                      XmNcolumns,dip->vinfo->n_dims + 1,
                      NULL);
        if (dip->headline_on) {
                XtVaSetValues(dip->grid,
                              XmNrow,0,
                              XmNcolumn,1,
                              XmNcellColumnSpan,dip->vinfo->n_dims,
                              NULL);
        }
        
        for (i = 0; i <= dip->vinfo->n_dims; i++)
                dip->cwidths[i] = 0;
        
        XmLGridSetStringsPos(dip->grid,XmCONTENT,nrows-3,XmCONTENT,0,
                             DimNamesText(dip));
        XmLGridSetStringsPos(dip->grid,XmCONTENT,nrows-2,XmCONTENT,0,
                             ElementsText(dip));
        XmLGridSetStringsPos(dip->grid,XmCONTENT,nrows-1,XmCONTENT,0,
                             RangeText(dip));
        if (dip->headline_on) {
                XmLGridSetStringsPos(dip->grid,XmCONTENT,0,XmCONTENT,0,
                                     VarTypeText(dip));
        }
        
	if (dip->creating) {
		dip->creating = False;
		XtVaSetValues(dip->grid,
			      XmNsimpleWidths,ColumnWidths(dip),
			      NULL);
	}
	else {
		XtVaSetValues(dip->grid,
			      XmNsimpleWidths,ColumnWidths(dip),
			      XmNimmediateDraw,False,
			      NULL);
	}

        
        return NhlNOERROR;
}


NgDimInfoGrid *NgCreateDimInfoGrid
(
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiVarInfoRec	*vinfo,
        NhlBoolean		headline_on,
        NhlBoolean		highlight_on
        )
{
        NhlErrorTypes ret;
        NgDimInfoGridRec *dip;
        static NhlBoolean first = True;
        int nrows;
        unsigned char sel_policy;
 
        if (first) {
                Qlong_name = NrmStringToQuark("long_name");
                Buffer = NhlMalloc(BUFINC);
                Buflen = BUFINC;
                first = False;
        }
        
        dip = NhlMalloc(sizeof(NgDimInfoGridRec));
        if (!dip) return NULL;
	dip->creating = True;

        nrows = headline_on ? 4 : 3;
        sel_policy = highlight_on ? XmSELECT_BROWSE_ROW : XmSELECT_NONE;
        
        dip->grid = XtVaCreateManagedWidget("DimInfoGrid",
                                            xmlGridWidgetClass,parent,
                                            XmNrows,nrows,
                                            XmNselectionPolicy,sel_policy,
                                            XmNverticalSizePolicy,XmVARIABLE,
                                            XmNhorizontalSizePolicy,XmVARIABLE,
                                            XmNimmediateDraw,True,
                                            NULL);
        dip->headline_on = headline_on;
        dip->highlight_on = highlight_on;
        
        ret = NgUpdateDimInfoGrid((NgDimInfoGrid*) dip,qfileref,vinfo);

        if (ret < NhlWARNING) {
                NhlFree(dip);
                return NULL;
        }
        return (NgDimInfoGrid *) dip;
}

        
void NgDestroyDimInfoGrid
(
        NgDimInfoGrid		*dim_info_grid
        )
{
        NgDimInfoGridRec *dip;
        
        dip = (NgDimInfoGridRec *) dim_info_grid;
        if (!dip) return;

        NhlFree(dip);
        
        return;
}
