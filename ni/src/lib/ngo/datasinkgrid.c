/*
 *      $Id: datasinkgrid.c,v 1.3 1997-06-27 07:20:16 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datasinkgrid.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sun Jun 22 14:31:22 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/datasinkgridP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/stringutil.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include  <XmL/Grid.h>
#include <float.h>

static char *Buffer;
static int  Buflen;
static int  Max_Width;
static Dimension  Row_Height;

static char *
ColumnWidths
(
	NgDataSinkGridRec *dsp
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
#if DEBUG_DATA_SINK_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}

static char *
TitleText
(
	NgDataSinkGridRec	*dsp
)
{
        NgDataSinkGrid data_sink_grid = dsp->public;
        int len;
        
        
        sprintf(Buffer,"%s|",dsp->data_sink->class_name);
        len = dsp->cwidths[0] = strlen(Buffer);
        
        sprintf(&Buffer[len],"%s","Data Variables");
        dsp->cwidths[1] = strlen(Buffer) - len;
        
#if DEBUG_DATA_SINK_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        

        return Buffer;
}

static char *
DataText
(
	NgDataSinkGridRec	*dsp,
        int			dataix
)
{
        int cwidth0,cwidth1,len;
        char buf[128];
        NgDataSinkGrid *pub = &dsp->public;
        
        sprintf(Buffer,"%s|",dsp->data_sink->data_names[dataix]);
        cwidth0 = strlen(Buffer);
        dsp->cwidths[0] = MAX(dsp->cwidths[0],cwidth0-1);
        if (! pub->dataitems)
                return Buffer;
        
        if (pub->dataitems[dataix]) {
                int i;
                NgVarPageOutput *ditem = pub->dataitems[dataix];

                if (ditem->qfile)
                        sprintf(&Buffer[cwidth0],"%s->%s(",
                                NrmQuarkToString(ditem->qfile),
                                NrmQuarkToString(ditem->qvar));
                else
                        sprintf(&Buffer[cwidth0],"%s",ditem->qvar);
                for (i=0; i< ditem->ndims; i++) {
                        sprintf(&Buffer[strlen(Buffer)],"%d:%d:%d,",
                                ditem->start[i],
                                ditem->finish[i],ditem->stride[i]);
                }
                Buffer[strlen(Buffer)-1] = ')';
                dsp->cwidths[1] = MAX(dsp->cwidths[1],
                                      strlen(Buffer)-cwidth0-1);
        }
        
        return Buffer;
}

NhlErrorTypes NgUpdateDataSinkGrid
(
        NgDataSinkGrid		*data_sink_grid,
        NrmQuark		qname,
        NgDataSinkRec		*data_sink_rec
       )
{
        NhlErrorTypes ret;
        NgDataSinkGridRec *dsp;
        int	nattrs,i;
        Dimension	height;
        NhlBoolean	first = True;
        
        
        dsp = (NgDataSinkGridRec *) data_sink_grid;
        if (!dsp) return NhlFATAL;
        if (first) {
                int		root_w;
                short		cw,ch;
                XmFontList      fontlist;
                
                XtVaGetValues(data_sink_grid->grid,
                              XmNfontList,&fontlist,
                              NULL);
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                root_w = WidthOfScreen(XtScreen(data_sink_grid->grid));
                Max_Width = root_w / cw - cw;
                Row_Height = ch + 2;
                first = False;
        }
        dsp->data_sink = data_sink_rec;
        dsp->qname = qname;
        XtVaSetValues(data_sink_grid->grid,
                      XmNrows,data_sink_rec->n_dataitems,
                      NULL);

        for (i = 0; i < 2; i++)
                dsp->cwidths[i] = 0;
        
        XmLGridSetStringsPos(data_sink_grid->grid,XmHEADING,0,XmCONTENT,0,
                             TitleText(dsp));
        for (i = 0; i < data_sink_rec->n_dataitems; i++) {
                XmLGridSetStringsPos
                        (data_sink_grid->grid,XmCONTENT,i,XmCONTENT,0,
                         DataText(dsp,i));
        }
        XtVaSetValues(data_sink_grid->grid,
                      XmNsimpleWidths,ColumnWidths(dsp),
                      NULL);
        
        XmLGridSelectRow(data_sink_grid->grid,0,False);

        return NhlNOERROR;
}

NgDataSinkGrid *NgCreateDataSinkGrid
(
        Widget			parent,
        NrmQuark		qname,
        NgDataSinkRec		*data_sink_rec
        )
{
        NhlErrorTypes ret;
        NgDataSinkGridRec *dsp;
        NgDataSinkGrid *data_sink_grid;
        int nattrs;
        static NhlBoolean first = True;

        if (first) {
                Buffer = NhlMalloc(BUFINC);
                Buflen = BUFINC;
                first = False;
        }
        
        dsp = NhlMalloc(sizeof(NgDataSinkGridRec));
        if (!dsp) return NULL;
        data_sink_grid = &dsp->public;
        dsp->data_sink = data_sink_rec;
        dsp->qname = qname;
        data_sink_grid->dataitems = NULL;
        
        data_sink_grid->grid = XtVaCreateManagedWidget
                ("DataSinkGrid",
                 xmlGridWidgetClass,parent,
                 XmNverticalSizePolicy,XmVARIABLE,
                 XmNhorizontalSizePolicy,XmVARIABLE,
                 XmNcolumns,2,
                 XmNrows,2,
                 NULL);
        XmLGridAddRows(data_sink_grid->grid,XmHEADING,0,1);
        
        return data_sink_grid;
}

        
void NgDestroyDataSinkGrid
(
        NgDataSinkGrid		*data_sink_grid
        )
{
        NgDataSinkGridRec *dsp;
        
        dsp = (NgDataSinkGridRec *) data_sink_grid;
        if (!dsp) return;

        NhlFree(dsp);
        
        return;
}

        
