/*
 *      $Id: datagrid.c,v 1.5 1997-10-03 20:07:57 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datagrid.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 14:22:58 MST 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/datagridP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/stringutil.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include  <ncarg/ngo/Grid.h>

static char *Buffer = NULL;
static int  Buflen;
static NrmQuark Qlong_name;
static Dimension Row_Height,Width_Const,Width_Mult,Width_Char;

static void GridTraverseAction(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static XtActionsRec myact[] = {
        { "GridTraverseAction", GridTraverseAction },
};

static void GridPosToArrayPos
(
        NgDataGridRec *dgrp,
        int	grid_col,
        int	grid_row,
        int	last_dim
        )
{
        NgDataGrid *dgp = &dgrp->datagrid;
        int i,j,ndims = last_dim + 1;
        long size,trow;

        dgrp->array_pos[ndims-1] = dgrp->array_rev[ndims-1] ?
                dgrp->start[ndims-1] - grid_col * dgrp->stride[ndims-1] :
                dgrp->start[ndims-1] + grid_col * dgrp->stride[ndims-1];
        if (ndims < 2)
                return;

        trow = grid_row;
        
        for (i=ndims-2; i > -1; i--) {
                dgrp->array_pos[i] = 
                        (dgrp->start[i] +
                         ((dgrp->array_rev[i] ? -trow : trow)
                          %dgrp->array_size[i]) * dgrp->stride[i])
                        % dgrp->vinfo->dim_info[i].dim_size;
                trow /= dgrp->array_size[i];
                if (trow == 0) {
                        for (j = 0; j < i; j++)
                                dgrp->array_pos[j] = dgrp->start[j];
                        break;
                }
        }

#if DEBUG_DATA_GRID
        for (i=0;i<ndims;i++)
                fprintf(stderr,"%ld, ",dgrp->array_pos[i]);
        fprintf(stderr,"\n");
#endif
        
        return;
}

static NclExtValueRec *ReadMoreData
(
        NgDataGridRec *dgrp,
        int	col,
        int	row,
        int	rows_to_go,
        int	*last_row_read
        )
{
        int i;
        NclExtValueRec *val = NULL;
        NgDataGrid *dgp = &dgrp->datagrid;

        GridPosToArrayPos(dgrp,col,row,dgrp->col_dim);
        for (i = 0; i <= dgrp->col_dim; i++) {
                dgrp->istart[i] = dgrp->array_pos[i];
                dgrp->ifinish[i] = dgrp->array_pos[i];
        }
        if (dgrp->array_rev[dgrp->col_dim]) {
                dgrp->ifinish[dgrp->col_dim] =
                        MAX(dgrp->finish[dgrp->col_dim],
                            dgrp->istart[dgrp->col_dim] -
                            dgrp->stride[dgrp->col_dim] * dgrp->ncols-1);
        }
        else {
                dgrp->ifinish[dgrp->col_dim] =
                        MIN(dgrp->finish[dgrp->col_dim],
                            dgrp->istart[dgrp->col_dim] +
                            dgrp->stride[dgrp->col_dim] * dgrp->ncols-1);
        }
        if (dgrp->row_dim > -1) {
                if (dgrp->array_rev[dgrp->row_dim]) {
                        dgrp->ifinish[dgrp->row_dim] =
                        MAX(dgrp->finish[dgrp->row_dim],
                            dgrp->istart[dgrp->row_dim] -
                            dgrp->stride[dgrp->row_dim] * rows_to_go);
                }
                else {
                        dgrp->ifinish[dgrp->row_dim] =
                        MIN(dgrp->finish[dgrp->row_dim],
                            dgrp->istart[dgrp->row_dim] +
                            dgrp->stride[dgrp->row_dim] * rows_to_go);
                }
        }
	
        switch (dgrp->vinfo->type) {
            case FILEVAR:
                    val = NclReadFileVar
                            (dgrp->qsymbol,dgrp->vinfo->name,
                             dgrp->istart,dgrp->ifinish,dgrp->stride);
                    break;
            case COORD:
                    val = NclReadVarCoord
                            (dgrp->qsymbol,dgrp->vinfo->name,
                             dgrp->istart,dgrp->ifinish,dgrp->stride);
                    break;
            case NORMAL:
                    val = NclReadVar
                            (dgrp->vinfo->name,
                             dgrp->istart,dgrp->ifinish,dgrp->stride);
                    break;
            default:
                    fprintf(stderr,"invalid var type\n");
        }

        if (dgrp->row_dim > -1)
                *last_row_read = row +
                        abs(dgrp->ifinish[dgrp->row_dim]
                            - dgrp->istart[dgrp->row_dim])
                        / dgrp->stride[dgrp->row_dim];
        else
                *last_row_read = row;
        
        return val;
}

static void FillVisibleGrid
(
        NgDataGridRec *dgrp
        )
{
        NgDataGrid *dgp = &dgrp->datagrid;
        NclExtValueRec *val = NULL;
        int i,j;
        int ndims = dgrp->col_dim + 1;
        int end_col, end_row;
        Dimension save_cell_width,cell_width,width,head_width=0;
        int vix,last_row_read;
        int start_col,start_row,ncols,nrows;
        XmLGridColumn colptr;
        XmLGridRow rowptr;
        
        XtVaGetValues(dgp->grid,
                      XmNscrollColumn,&start_col,
                      XmNscrollRow,&start_row,
                      XmNvisibleRows,&nrows,
                      XmNwidth,&width,
                      NULL);
        colptr = XmLGridGetColumn(dgp->grid,XmCONTENT,0);
        rowptr = XmLGridGetRow(dgp->grid,XmCONTENT,0);
        XtVaGetValues(dgp->grid,
                      XmNcolumnPtr,colptr,
                      XmNrowPtr,rowptr,
                      XmNcolumnWidth,&save_cell_width,
                      NULL);
        nrows--;

	cell_width = dgrp->cell_widths[dgrp->head_cols];
	switch (dgrp->vinfo->data_type) {
	case NCLAPI_char:
		cell_width = MAX(1,cell_width);
		break;
	case NCLAPI_byte:
		cell_width = MAX(2,cell_width);
		break;
	case NCLAPI_short:
	case NCLAPI_int:
	case NCLAPI_long:
		cell_width = MAX(4,cell_width);
		break;
	default:
		cell_width = MAX(4,cell_width);
		break;
	}
	for (i = 0; i < dgrp->head_cols; i++) {
		head_width += (dgrp->cell_widths[i] * Width_Char +
			       Width_Mult);
	}
	
	dgrp->ncols = 2 + (width - Width_Const - head_width) /
	  (cell_width * Width_Char + Width_Mult);
        dgrp->ncols = MIN(dgrp->ncols,
			  dgrp->array_size[dgrp->col_dim] - start_col);

        for (i = ndims; i < dgrp->vinfo->n_dims; i++) {
                dgrp->istart[i] = dgrp->start[i];
                dgrp->ifinish[i] = dgrp->finish[i];
        }
        
#if DEBUG_DATA_GRID
        fprintf(stderr,"topleft row %d column %d rows,cols %d %d\n",
                start_row,start_col,nrows,dgrp->ncols);
#endif
	
        last_row_read = start_row - 1;

	cell_width;
        for (i = 0; i < nrows; i++) {
                int total_len = 0;
                
                if (start_row + i > last_row_read) {
                        if (val) {
                                if (val->constant != 0)
                                        NclFree(val->value);
                                NclFreeExtValue(val);
                        }
                        val = ReadMoreData(dgrp,start_col,start_row+i,
                                           nrows - i,&last_row_read);
                        if (!val) {
                                fprintf(stderr,"error reading var\n");
                                return;
                        }
                        vix = 0;
                }
                strcpy(Buffer,"");
                for (j = 0; j < dgrp->ncols; j++) {
                        char *sval;
                        int len;

                        sval = NgTypedValueToString(val,vix,False,&len);
                        
                        total_len += (len + 1);
                        if (total_len > Buflen-2) {
                                Buflen *= 2;
                                Buffer = NhlRealloc(Buffer,Buflen);
                        }
                        strcat(Buffer,sval);
                        strcat(Buffer,"|");
                        cell_width = MAX(cell_width,len);
                        vix++;
                        if (vix >= val->totalelements)
                                break;
                }
                XmLGridSetStringsPos(dgp->grid,XmCONTENT,start_row+i,
                                     XmCONTENT,start_col,Buffer);
        }

        if (save_cell_width < cell_width ||
            save_cell_width > cell_width + 1) {
                XtVaSetValues(dgp->grid,
                              XmNcolumnWidth,cell_width,
                              NULL);
        }
        
	if (val->constant != 0)
		NclFree(val->value);
        NclFreeExtValue(val);
#if 0
        dgp->sub_width = 0;
        for (i = start_col + dgrp->ncols-1; i >= start_col+2; i--) {
                XRectangle clipped, unclipped;
                if (XmLGridColumnIsVisible(dgp->grid,i)) {
                        Dimension width;
                        XmLGridRowColumnToXY(dgp->grid,XmCONTENT,start_row,
                                             XmCONTENT,i,True,&clipped);
                        XmLGridRowColumnToXY(dgp->grid,XmCONTENT,start_row,
                                             XmCONTENT,i,False,&unclipped);
                        printf("clipped %d unclipped %d\n",
                               clipped.width,unclipped.width);
                        printf("column %d is visible\n",i);
                        dgp->sub_width = clipped.width;
                        width =  clipped.width < unclipped.width ?
                                clipped.x : clipped.x + clipped.width;
                        XtVaSetValues(dgp->grid,
                                      XmNwidth,width,
                                      NULL);
                        break;
                }
        }
#endif
        return;
}


static void GridPositionCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgDataGridRec *dgrp = (NgDataGridRec *)udata;
        NgDataGrid *dgp = &dgrp->datagrid;

        if (! dgrp->vinfo)
                return;

        XtVaSetValues(dgp->grid,
                      XmNlayoutFrozen,True,
                      NULL);

        FillVisibleGrid(dgrp);

        XtVaSetValues(dgp->grid,
                      XmNlayoutFrozen,False,
                      NULL);

        return;
}

static void SetIndexes
(
        NgDataGridRec *dgrp
        )
{
        NgDataGrid *dgp = &dgrp->datagrid;
        int i,len,total_len = 0;
        char tbuf[32];
        int sign = dgrp->start[0] > dgrp->finish[0] ? -1 : 1;
        Dimension width = 0;
        int cols;

        cols = 1 + abs(dgrp->finish[0]-dgrp->start[0]) / abs(dgrp->stride[0]);
        
        XtVaSetValues(dgp->grid,
                      XmNrows,1,
                      XmNcolumns,cols,
                      XmNvisibleRows,1,
                      NULL);
        
        strcpy(Buffer,"");
        for (i = sign * dgrp->start[0];
             i <= sign * dgrp->finish[0];
             i += dgrp->stride[0]) {
                sprintf(tbuf,"%d|",sign * i);
                len = strlen(tbuf);
                total_len += len;
                if (total_len > Buflen-2) {
                        Buflen *= 2;
                        Buffer = NhlRealloc(Buffer,Buflen);
                }
                strcat(Buffer,tbuf);
                width = MAX(width,len);
        }
        XmLGridSetStringsPos(dgp->grid,XmCONTENT,0,XmCONTENT,0,Buffer);
        
        XtVaSetValues(dgp->grid,
#if 0                      
                      XmNscrollColumn,0,
#endif                      
                      XmNcolumnWidth,width,
                      NULL);
        return;
}

static void SetData
(
        NgDataGridRec *dgrp
        )
{
        NgDataGrid *dgp = &dgrp->datagrid;
        int i,j,k;
        int ndims;
        NclExtValueRec *val;
        char *sval;
        int vix;
        Dimension width = 0,total_len = 0;
        int col_ix,cols = 0,head_cols = 0,rows = 1;
        int head_col_count;
        NhlBoolean rev;

        ndims = dgrp->vinfo->n_dims;
        if (!dgrp->array_size) {
                dgrp->array_size = NhlMalloc(ndims * sizeof(int));
        }
        if (!dgrp->istart) {
                dgrp->istart = NhlMalloc(ndims * sizeof(long));
        }
        if (!dgrp->ifinish) {
                dgrp->ifinish = NhlMalloc(ndims * sizeof(long));
        }
        if (!dgrp->array_pos) {
                dgrp->array_pos = NhlMalloc(ndims * sizeof(long));
        }
        if (!dgrp->array_rev) {
                dgrp->array_rev = NhlMalloc(ndims * sizeof(NhlBoolean));
        }
	rows = 1;
        dgrp->row_dim = -1;
        dgrp->col_dim = -1;

        for (i = ndims-1; i > -1; i--) {
		dgrp->array_size[i] =
			1 + abs(dgrp->finish[i] - dgrp->start[i]) /
				abs(dgrp->stride[i]);
		dgrp->array_rev[i] = 
			(dgrp->start[i] > dgrp->finish[i]) ? True : False;
                if (cols == 0) {
                        if (dgrp->array_size[i] <= 1)
                                continue;
                        cols = dgrp->array_size[i];
                        dgrp->col_dim = i;
                }
                else {
                        head_cols++;
                        rows *= dgrp->array_size[i];
                        if (dgrp->row_dim == -1)
				  dgrp->row_dim = i;
                }
        }
        if (dgrp->col_dim == -1) {
                dgrp->col_dim = ndims - 1;
                cols = dgrp->array_size[dgrp->col_dim];
        }
                
/*
 * Eliminate header columns for "outside" dimensions w/ only 1 element
 */	 
        for (i = 0; i < ndims; i++) {
		if (dgrp->array_size[i] > 1)
			break;
		head_cols--;
	}
        head_cols = MAX(0,MIN(MAX_HEADERS,head_cols));
	dgrp->cell_widths = NhlMalloc(sizeof(short) *(head_cols + 1));
	for (i = 0; i <= head_cols; i++)
		dgrp->cell_widths[i] = 1;
        dgrp->max_col = cols - 1;
        dgrp->max_row = rows - 1;
        XtVaSetValues(dgp->grid,
                      XmNrows,rows,
                      XmNcolumns,cols,
                      XmNvisibleRows,MIN(rows+1,10),
                      NULL);
/*
 * HEADING row
 */
              
        strcpy(Buffer,"");
        total_len = 0;
        if (dgrp->vinfo->coordnames[dgrp->col_dim] == -1 ||
            dgrp->vinfo->name == dgrp->vinfo->coordnames[dgrp->col_dim]) {
                    /* no coordinate variable associated with this dimension
                       or this variable is a coordinate variable */
                int sign = dgrp->array_rev[dgrp->col_dim] ? -1 : 1;
                
                for (i = sign * dgrp->start[dgrp->col_dim];
                     i <= sign * dgrp->finish[dgrp->col_dim];
                     i += dgrp->stride[dgrp->col_dim]) {
                        char tbuf[32];
                        int len;
                        sprintf(tbuf,"%d|",sign * i);
                        len = strlen(tbuf);
                        total_len += len;
                        if (total_len > Buflen-2) {
                                Buflen *= 2;
                                Buffer = NhlRealloc(Buffer,Buflen);
                        }
                        strcat(Buffer,tbuf);
                        width = MAX(width,len);
                }
        }
        else {
                val = NULL;
                switch (dgrp->vinfo->type) {
                    case FILEVAR:
                            val = NclReadFileVarCoord
                                    (dgrp->qsymbol,dgrp->vinfo->name,
                                     dgrp->vinfo->coordnames[dgrp->col_dim],
                                     &dgrp->start[dgrp->col_dim],
                                     &dgrp->finish[dgrp->col_dim],
                                     &dgrp->stride[dgrp->col_dim]);
                            break;
                    case NORMAL:
                            val = NclReadVarCoord
                                    (dgrp->vinfo->name,
                                     dgrp->vinfo->coordnames[dgrp->col_dim],
                                     &dgrp->start[dgrp->col_dim],
                                     &dgrp->finish[dgrp->col_dim],
                                     &dgrp->stride[dgrp->col_dim]);
                            break;
                    default:
                            fprintf(stderr,"invalid var type\n");
                }
                if (!val) {
                        fprintf(stderr,"error reading var\n");
                        return;
                }
                for (i = 0; i < cols; i++) {
                        int len;

                        sval = NgTypedValueToString(val,i,False,&len);
                        
                        total_len += (len + 1);
                        if (total_len > Buflen-2) {
                                Buflen *= 2;
                                Buffer = NhlRealloc(Buffer,Buflen);
                        }
                        strcat(Buffer,sval);
                        strcat(Buffer,"|");
                        width = MAX(width,len);
                }
                if (val) {
                        if (val->constant != 0)
                                NclFree(val->value);
                        NclFreeExtValue(val);
                }
        }
        XmLGridSetStringsPos(dgp->grid,XmHEADING,0,XmCONTENT,0,Buffer);
	dgrp->cell_widths[head_cols] = width;
		
/*
 * HEADING columns
 */
        if (head_cols > dgrp->head_cols) {
                XmLGridAddColumns(dgp->grid,XmHEADING,0,
                                  head_cols - dgrp->head_cols);

        }
        else if (dgrp->head_cols > head_cols) {
                XmLGridDeleteColumns(dgp->grid,XmHEADING,0,
                                  dgrp->head_cols - head_cols);
        }
        dgrp->head_cols = head_cols;
        
	col_ix = head_cols - 1;
	for (i = dgrp->row_dim; i > -1 && col_ix > -1; i--,col_ix--) {
                val = NULL;
                width = 0;
                strcpy(Buffer,"");
                total_len = 0;
                if (dgrp->vinfo->coordnames[i] == -1) {
                        for (j = 0; j < rows; j++) {
                                char tbuf[32];
                                int len;
				GridPosToArrayPos(dgrp,0,j,dgrp->col_dim);
                                sprintf(tbuf,"%d\n",dgrp->array_pos[i]);
                                len = strlen(tbuf);
                                total_len += len;
                                if (total_len > Buflen-2) {
                                        Buflen *= 2;
                                        Buffer = NhlRealloc(Buffer,Buflen);
                                }
                                strcat(Buffer,tbuf);
                                width = MAX(width,len);
                        }
		}
		else {
			int last_pos = -1;
                        switch (dgrp->vinfo->type) {
                            case FILEVAR:
                                    val = NclReadFileVarCoord
                                            (dgrp->qsymbol,dgrp->vinfo->name,
                                             dgrp->vinfo->coordnames[i],
                                             &dgrp->start[i],
                                             &dgrp->finish[i],
                                             &dgrp->stride[i]);
                                    break;
                            case NORMAL:
                                    val = NclReadVarCoord
                                            (dgrp->vinfo->name,
                                             dgrp->vinfo->coordnames[i],
                                             &dgrp->start[i],
                                             &dgrp->finish[i],
                                             &dgrp->stride[i]);
                                    break;
                            default:
                                    fprintf(stderr,"invalid var type\n");
                        }
                        if (!val) {
                                fprintf(stderr,"error reading var\n");
                                return;
                        }
			vix = 0;
                        for (j = 0; j < rows; j++) {
                                int len;
				GridPosToArrayPos(dgrp,0,j,dgrp->col_dim);
				
				if (dgrp->array_pos[i] != last_pos) {
					last_pos = dgrp->array_pos[i];
					sval = NgTypedValueToString
                                                (val,vix,False,&len);
					vix = (vix+1) % dgrp->array_size[i];
				}
                                total_len += (len + 1);
                                if (total_len > Buflen-2) {
                                        Buflen *= 2;
                                        Buffer = NhlRealloc(Buffer,Buflen);
                                }
                                strcat(Buffer,sval);
                                strcat(Buffer,"\n");
                                width = MAX(width,len);
                        }
                        if (val) {
                                if (val->constant != 0)
                                        NclFree(val->value);
                                NclFreeExtValue(val);
                        }
                }
                XmLGridSetStringsPos(dgp->grid,
                                     XmCONTENT,0,XmHEADING,col_ix,Buffer);
                XtVaSetValues(dgp->grid,
                              XmNcolumn,col_ix,
                              XmNcolumnType,XmHEADING,
                              XmNcolumnWidth,width,
                              NULL);
		dgrp->cell_widths[col_ix] = width;
        }
#if 0
        XtVaSetValues(dgp->grid,
                      XmNscrollColumn,0,
                      XmNscrollRow,0,
                      NULL);
#endif        
        FillVisibleGrid(dgrp);
        return;
		
}

NhlErrorTypes NgUpdateDataGrid
(
        NgDataGrid		*data_grid,
        NrmQuark		qsymbol,
        NclApiVarInfoRec	*vinfo,
	long			*start,
	long			*finish,
	long			*stride
        )
{
        static NhlBoolean first = True;
        NhlErrorTypes ret;
        NgDataGridRec *dgrp;
        NgDataGrid *dgp;
	int ndims;
        
        dgrp = (NgDataGridRec *) data_grid;
        if (!dgrp) return NhlFATAL;
        dgp = &dgrp->datagrid;

	if (!vinfo) /* non-coordinate dimension */
		ndims = 1;
	else
		ndims = vinfo->n_dims;

	if (dgrp->reinit > 2
            && dgrp->qsymbol == qsymbol && dgrp->vinfo == vinfo) {
		if (! memcmp(dgrp->start,start,ndims *sizeof(long)) &&
		    ! memcmp(dgrp->finish,finish,ndims *sizeof(long)) &&
		    ! memcmp(dgrp->stride,stride,ndims *sizeof(long))) {
			return NhlNOERROR;
		}
	}
	else if (!dgrp->start ||
		 (ndims > dgrp->dims_alloced) ||
		 (vinfo && !dgrp->vinfo)) {
		dgrp->start = NhlRealloc(dgrp->start,ndims * sizeof(long));
		dgrp->finish = NhlRealloc(dgrp->finish,ndims * sizeof(long));
		dgrp->stride = NhlRealloc(dgrp->stride,ndims * sizeof(long));
                dgrp->dims_alloced = ndims;
	}
	memcpy(dgrp->start,start,ndims *sizeof(long));
	memcpy(dgrp->finish,finish,ndims *sizeof(long));
	memcpy(dgrp->stride,stride,ndims *sizeof(long));

        if (dgrp->istart) {
                NhlFree(dgrp->istart);
                dgrp->istart = NULL;
        }
        if (dgrp->ifinish) {
                NhlFree(dgrp->ifinish);
                dgrp->ifinish = NULL;
        }
        if (dgrp->array_pos) {
                NhlFree(dgrp->array_pos);
                dgrp->array_pos = NULL;
        }
        if (dgrp->array_size) {
                NhlFree(dgrp->array_size);
                dgrp->array_size = NULL;
        }
        if (dgrp->array_rev) {
                NhlFree(dgrp->array_rev);
                dgrp->array_rev = NULL;
        }
        if (dgrp->cell_widths) {
                NhlFree(dgrp->cell_widths);
                dgrp->cell_widths = NULL;
        }
        
        dgrp->reinit++;
        if (first) {
                short		cw,ch;
                XmFontList      fontlist;
                int		nrows,nvisrows;
		Dimension	ml=0,mr=0,shadow;
		Dimension	height,width;
                
                XtVaGetValues(dgp->grid,
                              XmNfontList,&fontlist,
                              XmNrows,&nrows,
                              XmNvisibleRows,&nvisrows,
                              XmNheight,&height,
                              XmNwidth,&width,
			      XmNshadowThickness,&shadow,
                              NULL);

                
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
		Width_Char = cw;
		Width_Const = shadow * 2;
		Width_Mult = 4 + ml + mr;
                Row_Height = ch + 2;
                first = False;
        }
        
        dgrp->qsymbol = qsymbol;
        dgrp->vinfo = vinfo;
        dgp->sub_width = 0;

        XDefineCursor(dgrp->go->go.x->dpy,
                      XtWindow(dgrp->go->go.manager),dgrp->go->go.x->wait);
        XSync(dgrp->go->go.x->dpy,False);
        XtVaSetValues(dgp->grid,
                      XmNlayoutFrozen,True,
                      NULL);
        if (!dgrp->vinfo) {
                if (dgrp->head_rows) {
                        XmLGridDeleteRows
                                (dgp->grid,XmHEADING,0,dgrp->head_rows);
                        dgrp->head_rows = 0;
                }
                if (dgrp->head_cols) {
                        XmLGridDeleteColumns(dgp->grid,XmHEADING,0,
                                             dgrp->head_cols);
                        dgrp->head_cols = 0;
                }
                SetIndexes(dgrp);
        }
        else {
                if (! dgrp->head_rows) {
                        XmLGridAddRows(dgp->grid,XmHEADING,0,1);
                        dgrp->head_rows = 1;
                }
                SetData(dgrp);
        }
        XtVaSetValues(dgp->grid,
                      XmNlayoutFrozen,False,
                      NULL);

        XUndefineCursor(dgrp->go->go.x->dpy,XtWindow(dgrp->go->go.manager));
        
        return NhlNOERROR;
}


NgDataGrid *NgCreateDataGrid
(
        NgGO                    go,
        Widget			parent,
        NrmQuark 		qsymbol,
        NclApiVarInfoRec	*vinfo,
        NhlBoolean		headline_on,
        NhlBoolean		highlight_on
        )
{
        NhlErrorTypes ret;
        NgDataGridRec *dgrp;
        static NhlBoolean first = True;
        int i,nrows;
        unsigned char sel_policy;
        NgDataGrid *dgp;
        Widget hsb,vsb;
 
        if (first) {
		XtAppAddActions(go->go.x->app,myact,NhlNumber(myact));
                Qlong_name = NrmStringToQuark("long_name");
                Buffer = NhlMalloc(BUFINC);
                Buflen = BUFINC;
                first = False;
        }
        
        dgrp = NhlMalloc(sizeof(NgDataGridRec));
        if (!dgrp) return NULL;
        dgp = &dgrp->datagrid;
        dgrp->go = go;
	dgrp->vinfo = NULL;
        dgrp->dims_alloced = 0;

        nrows = headline_on ? 4 : 3;
        sel_policy = highlight_on ? XmSELECT_BROWSE_ROW : XmSELECT_NONE;

        dgrp->start = NULL;
        dgrp->finish = NULL;
        dgrp->stride = NULL;
        dgp->sub_width = 0;
        dgrp->istart = NULL;
        dgrp->ifinish = NULL;
        dgrp->array_pos = NULL;
        dgrp->array_size = NULL;
        dgrp->array_rev = NULL;
	dgrp->cell_widths = NULL;
        dgrp->reinit = 0;
        dgp->grid = XtVaCreateManagedWidget("DataGrid",
                                            xmlGridWidgetClass,parent,
                                            XmNrows,1,
                                            XmNcolumns,1,
                                            XmNselectionPolicy,sel_policy,
                                            XmNverticalSizePolicy,XmCONSTANT,
                                            XmNhorizontalSizePolicy,XmCONSTANT,
                                            XmNhsbDisplayPolicy,XmSTATIC,
                                            XmNvsbDisplayPolicy,XmAS_NEEDED,
					    XmNuserData,dgrp,
                                            NULL);
        XtAddCallback(dgp->grid,XmNscrollCallback,GridPositionCB,dgrp);

        dgrp->head_rows = 0;
        dgrp->head_cols = 0;
        dgrp->parent = parent;

        XtVaGetValues(dgp->grid,
                      XmNhorizontalScrollBar,&hsb,
                      XmNverticalScrollBar,&vsb,
                      NULL);

        return (NgDataGrid *) dgrp;
}
void NgDeactivateDataGrid
(
        NgDataGrid		*data_grid
        )
{
        NgDataGridRec *dgrp;
        NgDataGrid *dgp;
        
        dgrp = (NgDataGridRec *) data_grid;
        if (!dgrp) return;
        dgp = &dgrp->datagrid;

        XtVaSetValues(dgp->grid,
                      XmNscrollColumn,0,
                      XmNscrollRow,0,
                      NULL);
        
        XtVaSetValues(dgp->grid,
                      XmNrows,0,
                      XmNcolumns,0,
                      NULL);
        
        if (dgrp->istart) {
                NhlFree(dgrp->istart);
                dgrp->istart = NULL;
        }
        if (dgrp->ifinish) {
                NhlFree(dgrp->ifinish);
                dgrp->ifinish = NULL;
        }
        if (dgrp->array_pos) {
                NhlFree(dgrp->array_pos);
                dgrp->array_pos = NULL;
        }
        if (dgrp->array_size) {
                NhlFree(dgrp->array_size);
                dgrp->array_size = NULL;
        }
        if (dgrp->array_rev) {
                NhlFree(dgrp->array_rev);
                dgrp->array_rev = NULL;
        }
        if (dgrp->cell_widths) {
                NhlFree(dgrp->cell_widths);
                dgrp->cell_widths = NULL;
        }
        dgrp->reinit = 1;
        
        return;
}

        
void NgDestroyDataGrid
(
        NgDataGrid		*data_grid
        )
{
        NgDataGridRec *dgrp;
        NgDataGrid *dgp;
        
        dgrp = (NgDataGridRec *) data_grid;
        if (!dgrp) return;
        dgp = &dgrp->datagrid;

        NgDeactivateDataGrid(data_grid);

        if (dgrp->start)
                NhlFree(dgrp->start);
        if (dgrp->finish)
                NhlFree(dgrp->finish);
        if (dgrp->stride)
                NhlFree(dgrp->stride);
        NhlFree(dgrp);
        
        return;
}

static void
GridTraverseAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{

        NgDataGridRec *dgrp;
#if DEBUG_DATA_GRID
	fprintf(stderr,"in grid traverse action\n");
#endif

	XtVaGetValues(w,
		      XmNuserData,&dgrp,
		      NULL);

	return;
}
