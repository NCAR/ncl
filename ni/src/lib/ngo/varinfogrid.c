/*
 *      $Id: varinfogrid.c,v 1.1 1997-03-04 00:04:41 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		varinfogrid.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 11 11:53:31 MST 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/varinfogridP.h>
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
	NgVarInfoGridRec *vip
)
{
	int	i;
        NclApiVarInfoRec *vinfo = vip->vinfo;
        char	sizestr[10];

        Buffer[0] = '\0';
	for (i=0; i <= vinfo->n_dims; i++) {
                sprintf(sizestr,"%dc ",vip->cwidths[i]);
                if (strlen(sizestr) + strlen(Buffer) + 1> Buflen) {
                        Buffer = NhlRealloc(Buffer,Buflen+BUFINC);
                }
		strcat(Buffer,sizestr);
	}
        Buffer[strlen(Buffer)-1] = '\0';
#if DEBUG_VAR_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}


static char *
VarTypeText
(
	NgVarInfoGridRec *vip
)
{
	int	i;
        NclApiVarInfoRec *vinfo = vip->vinfo;
	NclExtValueRec *val;
        char *cp,*sval = NULL;
        int cwidth,twidth;

        sprintf(Buffer,"%dD %s|",vinfo->n_dims,TypeString(vinfo->data_type));
        vip->cwidths[0] = MAX(vip->cwidths[0],strlen(Buffer));

	for (i = 0; i < vinfo->n_atts; i++)
		if (vinfo->attnames[i] == Qlong_name)
			break;
	if (i == vinfo->n_atts) {
                cp = NrmQuarkToString(vinfo->name);
        }
        else {
                if (vip->qfileref) {
                        val = NclReadFileVarAtt(vip->qfileref,vinfo->name,
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
        
#if DEBUG_VAR_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        

	for (i=1,twidth=0; i <= vinfo->n_dims; i++) {
                twidth += vip->cwidths[i];
	}
        cwidth = strlen(cp);
        vip->cwidths[1] = MAX(vip->cwidths[1],
                             vip->cwidths[1]+cwidth-twidth);
        if (sval)
                NclFree(sval);
        
        return Buffer;
}

static char *
DimNamesText
(
	NgVarInfoGridRec *vip
)
{
	int	i;
        NclApiVarInfoRec *vinfo = vip->vinfo;
        char *cp;

	strcpy(Buffer,"Dimensions");
        vip->cwidths[0] = MAX(vip->cwidths[0],strlen(Buffer));
        
	for (i=0; i < vinfo->n_dims; i++) {
                int clen;
                cp = NrmQuarkToString(vinfo->dim_info[i].dim_quark);
                clen = strlen(cp);
                while (clen + strlen(Buffer) + 2> Buflen) {
                        Buffer = NhlRealloc(Buffer,Buflen+BUFINC);
                }
                strcat(Buffer,"|");
		strcat(Buffer,cp);
                vip->cwidths[i+1] = MAX(vip->cwidths[i+1],clen);
	}
#if DEBUG_VAR_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}

static char *
ElementsText
(
	NgVarInfoGridRec *vip
)
{
	int	i;
	char	dimstr[20];
        NclApiVarInfoRec *vinfo = vip->vinfo;

	strcpy(Buffer,"Elements");
        vip->cwidths[0] = MAX(vip->cwidths[0],strlen(Buffer));
        
	for (i=0; i < vinfo->n_dims; i++) {
                strcat(Buffer,"|");
		sprintf(dimstr,"%d",vinfo->dim_info[i].dim_size);
		strcat(Buffer,dimstr);
                vip->cwidths[i+1] = MAX(vip->cwidths[i+1],strlen(dimstr));
	}
#if DEBUG_VAR_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}

static char *
RangeText
(
	NgVarInfoGridRec *vip
)
{
	Arg	args[50];
	int	nargs = 0;
	int	i;
	char	substr[32];
	NclExtValueRec *val;
        NclApiVarInfoRec *vinfo = vip->vinfo;
        int	len;
        

        strcpy(Buffer,"Range");
        vip->cwidths[0] = MAX(vip->cwidths[0],strlen(Buffer));
        
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
                        if (vip->qfileref) {
                                val = NclReadFileVarCoord(vip->qfileref,
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
                vip->cwidths[i+1] = MAX(vip->cwidths[i+1],
                                        strlen(Buffer)-len);
	}
#if DEBUG_VAR_INFO_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
	       
}

NhlErrorTypes NgUpdateVarInfoGrid
(
        NgVarInfoGrid		var_info_grid,
        NrmQuark		qfileref,
        NclApiVarInfoRec	*vinfo
        )
{
        NhlErrorTypes ret;
        NgVarInfoGridRec *vip;
        int	i;
        
        vip = (NgVarInfoGridRec *) var_info_grid;
        if (!vip) return NhlFATAL;

        vip->qfileref = qfileref;
        vip->vinfo = vinfo;
        
        XtVaSetValues(vip->grid,
                      XmNcolumns,vip->vinfo->n_dims + 1,
                      NULL);
        XtVaSetValues(vip->grid,
                      XmNrow,0,
                      XmNcolumn,1,
                      XmNcellColumnSpan,vip->vinfo->n_dims,
                      NULL);

        for (i = 0; i <= vip->vinfo->n_dims; i++)
                vip->cwidths[i] = 0;
        
        XmLGridSetStringsPos(vip->grid,XmCONTENT,1,XmCONTENT,0,
                             DimNamesText(vip));
        XmLGridSetStringsPos(vip->grid,XmCONTENT,2,XmCONTENT,0,
                             ElementsText(vip));
        XmLGridSetStringsPos(vip->grid,XmCONTENT,3,XmCONTENT,0,
                             RangeText(vip));
        XmLGridSetStringsPos(vip->grid,XmCONTENT,0,XmCONTENT,0,
                             VarTypeText(vip));
        XtVaSetValues(vip->grid,
                      XmNsimpleWidths,ColumnWidths(vip),
                      NULL);
        
        return NhlNOERROR;
}


NgVarInfoGrid NgCreateVarInfoGrid
(
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiVarInfoRec	*vinfo
        )
{
        NhlErrorTypes ret;
        NgVarInfoGridRec *vip;
        static NhlBoolean first = True;

        if (first) {
                Qlong_name = NrmStringToQuark("long_name");
                Buffer = NhlMalloc(BUFINC);
                Buflen = BUFINC;
                first = False;
        }
        
        vip = NhlMalloc(sizeof(NgVarInfoGridRec));
        if (!vip) return NULL;
        
        vip->grid = XtVaCreateManagedWidget("VarInfoGrid",
                                            xmlGridWidgetClass,parent,
                                            XmNrows,4,
                                            XmNvisibleRows,4,
                                            XmNhorizontalSizePolicy,XmVARIABLE,
                                            NULL);
        
        ret = NgUpdateVarInfoGrid((NgVarInfoGrid) vip,qfileref,vinfo);

        if (ret < NhlWARNING) {
                NhlFree(vip);
                return NULL;
        }
        return (NgVarInfoGrid) vip;
}

        
