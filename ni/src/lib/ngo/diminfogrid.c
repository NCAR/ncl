/*
 *      $Id: diminfogrid.c,v 1.1 1997-03-04 02:53:50 dbrown Exp $
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

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include  <XmL/Grid.h>

static char *Buffer;
static int  Buflen;
static NrmQuark Qlong_name;

static char *
TypeString
#ifdef    NhlNeedProto
(
	int type
)
#else
(type)
	int type;
#endif
{
	switch (type) {
	case NCLAPI_none:
	default:
		return("none");
	case NCLAPI_short:
		return("short");
	case NCLAPI_int:
		return("int");
	case NCLAPI_long:
		return("long");
	case NCLAPI_float:
		return("float");
	case NCLAPI_double:
		return("double");
	case NCLAPI_char:
		return("char");
	case NCLAPI_byte:
		return("byte");
	case NCLAPI_string:
		return("string");
#if 0
	case NCLAPI_numeric:
		return("numeric");
#endif
	case NCLAPI_logical:
		return("logical");
	case NCLAPI_obj:
		return("obj");
	}
}

static char *
RemoveZeros
#if	NhlNeedProto
(
	char		*fstr

)
#else
(fstr)
	char		*fstr;
#endif
{
	char *cp = fstr + strlen(fstr) - 1;

	/* assumes there must be a decimal point */

	if (*cp != '0' || cp - fstr < 4)
		return fstr;
	while (*(cp-1) == '0') {
		*cp = '\0';
		cp--;
	}
	return fstr;
}

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
        int cwidth,twidth;

        sprintf(Buffer,"%dD %s|",vinfo->n_dims,TypeString(vinfo->data_type));
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
                cp = sval = NclTypeToString(val->value,val->type);
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
        if (sval)
                NclFree(sval);
        
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
                                sprintf(substr,"|[0,%d]",
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
                        char    *vstart,*sval1,*sval2;
                        int size;
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
			vstart = (char *) val->value;
			sval1 = NclTypeToString(vstart,val->type);
			sval1 = RemoveZeros(sval1);
                        size = strlen(sval1);
                        if (vinfo->dim_info[i].dim_size > 1) {
                                sval2 = NclTypeToString(vstart
                                                        + val->elem_size,
                                                        val->type);
                                sval2 = RemoveZeros(sval2);
                                size += strlen(sval2);
                        }
                        while (size + strlen(Buffer) + 6> Buflen) {
                                Buffer = NhlRealloc(Buffer,Buflen+BUFINC);
                        }
                        strcat(Buffer,"|[");
			strcat(Buffer,sval1);
			NclFree(sval1);
                        if (vinfo->dim_info[i].dim_size > 1) {
                                strcat(Buffer,", ");
                                strcat(Buffer,sval2);
                                NclFree(sval2);
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
        NgDimInfoGrid		dim_info_grid,
        NrmQuark		qfileref,
        NclApiVarInfoRec	*vinfo
        )
{
        NhlErrorTypes ret;
        NgDimInfoGridRec *dip;
        int	i;
        
        dip = (NgDimInfoGridRec *) dim_info_grid;
        if (!dip) return NhlFATAL;

        dip->qfileref = qfileref;
        dip->vinfo = vinfo;
        
        XtVaSetValues(dip->grid,
                      XmNcolumns,dip->vinfo->n_dims + 1,
                      NULL);
        XtVaSetValues(dip->grid,
                      XmNrow,0,
                      XmNcolumn,1,
                      XmNcellColumnSpan,dip->vinfo->n_dims,
                      NULL);

        for (i = 0; i <= dip->vinfo->n_dims; i++)
                dip->cwidths[i] = 0;
        
        XmLGridSetStringsPos(dip->grid,XmCONTENT,1,XmCONTENT,0,
                             DimNamesText(dip));
        XmLGridSetStringsPos(dip->grid,XmCONTENT,2,XmCONTENT,0,
                             ElementsText(dip));
        XmLGridSetStringsPos(dip->grid,XmCONTENT,3,XmCONTENT,0,
                             RangeText(dip));
        XmLGridSetStringsPos(dip->grid,XmCONTENT,0,XmCONTENT,0,
                             VarTypeText(dip));
        XtVaSetValues(dip->grid,
                      XmNsimpleWidths,ColumnWidths(dip),
                      NULL);
        
        return NhlNOERROR;
}


NgDimInfoGrid NgCreateDimInfoGrid
(
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiVarInfoRec	*vinfo
        )
{
        NhlErrorTypes ret;
        NgDimInfoGridRec *dip;
        static NhlBoolean first = True;

        if (first) {
                Qlong_name = NrmStringToQuark("long_name");
                Buffer = NhlMalloc(BUFINC);
                Buflen = BUFINC;
                first = False;
        }
        
        dip = NhlMalloc(sizeof(NgDimInfoGridRec));
        if (!dip) return NULL;
        
        dip->grid = XtVaCreateManagedWidget("DimInfoGrid",
                                            xmlGridWidgetClass,parent,
                                            XmNrows,4,
                                            XmNvisibleRows,4,
                                            XmNhorizontalSizePolicy,XmVARIABLE,
                                            NULL);
        
        ret = NgUpdateDimInfoGrid((NgDimInfoGrid) dip,qfileref,vinfo);

        if (ret < NhlWARNING) {
                NhlFree(dip);
                return NULL;
        }
        return (NgDimInfoGrid) dip;
}

        
