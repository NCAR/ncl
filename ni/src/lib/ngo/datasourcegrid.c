/*
 *      $Id: datasourcegrid.c,v 1.1 1998-12-16 23:51:33 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datasourcegrid.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sun Jun 22 14:31:22 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/datasourcegridP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/stringutil.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include  <ncarg/ngo/Grid.h>
#include <float.h>

static char *Buffer;
static int  Buflen;
static int  Max_Width;
static Dimension  Row_Height;

static char *
ColumnWidths
(
	NgDataSourceGridRec *dsp
)
{
	int	i;
        char	sizestr[10];
        int	twidth = 0;
        
        Buffer[0] = '\0';
	for (i=0; i < 2; i++) {
                int width = dsp->cwidths[i];
                if (width + twidth > Max_Width)
                        width = Max_Width - twidth;
                twidth += width;
                sprintf(sizestr,"%dc ",width);
		strcat(Buffer,sizestr);
	}
        Buffer[strlen(Buffer)-1] = '\0';
#if DEBUG_DATA_SOURCE_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}

static char *
TitleText
(
	NgDataSourceGridRec	*dsp
)
{
        NgDataSourceGrid data_source_grid = dsp->public;
        int len;
        
        
        sprintf(Buffer,"%s|",dsp->data_profile->class_name);
        len = dsp->cwidths[0] = strlen(Buffer);
        
        sprintf(&Buffer[len],"%s","Data Variables");
        dsp->cwidths[1] = strlen(Buffer) - len;
        
#if DEBUG_DATA_SOURCE_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        

        return Buffer;
}

static char *
DataText
(
	NgDataSourceGridRec	*dsp,
        int			dataix
)
{
        int cwidth0,cwidth1,len;
        char buf[128];
        NgDataSourceGrid *pub = &dsp->public;
        
        sprintf(Buffer,"%s|",dsp->data_profile->data_names[dataix]);
        cwidth0 = strlen(Buffer);
        dsp->cwidths[0] = MAX(dsp->cwidths[0],cwidth0-1);
        if (! pub->dataitems)
                return Buffer;
        
        if (pub->dataitems[dataix]) {
                int i;
                NgVarDataRec *ditem = pub->dataitems[dataix];

                if (ditem->qfile && ditem->qvar)
                        sprintf(&Buffer[cwidth0],"%s->%s(",
                                NrmQuarkToString(ditem->qfile),
                                NrmQuarkToString(ditem->qvar));
                else if (ditem->qvar)
                        sprintf(&Buffer[cwidth0],"%s(",
                                NrmQuarkToString(ditem->qvar));
		else {
			sprintf(&Buffer[cwidth0],"null");
			dsp->cwidths[1] = MAX(dsp->cwidths[1],
					      strlen(Buffer)-cwidth0-1);
			return Buffer;
		}
                for (i=0; i< ditem->ndims; i++) {
                        if ((ditem->finish[i] - ditem->start[i])
                            /ditem->stride[i] == 0) {
                                sprintf(&Buffer[strlen(Buffer)],"%d,",
                                        ditem->start[i]);
                        }
                        else {
                                sprintf(&Buffer[strlen(Buffer)],"%d:%d:%d,",
                                        ditem->start[i],
                                        ditem->finish[i],ditem->stride[i]);
                        }
                }
                    /* back up 1 to remove final comma */
                Buffer[strlen(Buffer)-1] = ')';
                dsp->cwidths[1] = MAX(dsp->cwidths[1],
                                      strlen(Buffer)-cwidth0-1);
        }
        
        return Buffer;
}

NhlErrorTypes NgUpdateDataSourceGrid
(
        NgDataSourceGrid		*data_source_grid,
        NrmQuark		qname,
        NgDataProfileRec		*data_profile_rec
       )
{
        NhlErrorTypes ret;
        NgDataSourceGridRec *dsp;
        int	nattrs,i;
        Dimension	height;
        NhlBoolean	first = True;
        
        
        dsp = (NgDataSourceGridRec *) data_source_grid;
        if (!dsp) return NhlFATAL;
        if (first) {
                int		root_w;
                short		cw,ch;
                XmFontList      fontlist;
                
                XtVaGetValues(data_source_grid->grid,
                              XmNfontList,&fontlist,
                              NULL);
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                root_w = WidthOfScreen(XtScreen(data_source_grid->grid));
                Max_Width = root_w / cw - cw;
                Row_Height = ch + 2;
                first = False;
        }
        dsp->data_profile = data_profile_rec;
        dsp->qname = qname;
        XtVaSetValues(data_source_grid->grid,
                      XmNrows,data_profile_rec->n_dataitems,
                      NULL);

        for (i = 0; i < 2; i++)
                dsp->cwidths[i] = 0;
        
        XmLGridSetStringsPos(data_source_grid->grid,XmHEADING,0,XmCONTENT,0,
                             TitleText(dsp));
        for (i = 0; i < data_profile_rec->n_dataitems; i++) {
                XmLGridSetStringsPos
                        (data_source_grid->grid,XmCONTENT,i,XmCONTENT,0,
                         DataText(dsp,i));
        }
        XtVaSetValues(data_source_grid->grid,
                      XmNsimpleWidths,ColumnWidths(dsp),
                      NULL);
        
        XmLGridSelectRow(data_source_grid->grid,0,False);

	if (! dsp->created) {
		dsp->created = True;
		XtVaSetValues(data_source_grid->grid,
			      XmNimmediateDraw,False,
			      NULL);
	}

        return NhlNOERROR;
}

NgDataSourceGrid *NgCreateDataSourceGrid
(
        Widget			parent,
        NrmQuark		qname,
        NgDataProfileRec		*data_profile_rec
        )
{
        NhlErrorTypes ret;
        NgDataSourceGridRec *dsp;
        NgDataSourceGrid *data_source_grid;
        int nattrs;
        static NhlBoolean first = True;

        if (first) {
                Buffer = NhlMalloc(BUFINC);
                Buflen = BUFINC;
                first = False;
        }
        
        dsp = NhlMalloc(sizeof(NgDataSourceGridRec));
        if (!dsp) return NULL;
        data_source_grid = &dsp->public;
        dsp->data_profile = data_profile_rec;
        dsp->qname = qname;
	dsp->created = False;
        data_source_grid->dataitems = NULL;
        
        data_source_grid->grid = XtVaCreateManagedWidget
                ("DataSourceGrid",
                 xmlGridWidgetClass,parent,
                 XmNverticalSizePolicy,XmVARIABLE,
                 XmNhorizontalSizePolicy,XmVARIABLE,
                 XmNcolumns,2,
                 XmNrows,0,
		 XmNimmediateDraw,True,
                 NULL);
        XmLGridAddRows(data_source_grid->grid,XmHEADING,0,1);
        
        return data_source_grid;
}

        
void NgDestroyDataSourceGrid
(
        NgDataSourceGrid		*data_source_grid
        )
{
        NgDataSourceGridRec *dsp;
        
        dsp = (NgDataSourceGridRec *) data_source_grid;
        if (!dsp) return;

        NhlFree(dsp);
        
        return;
}

        
