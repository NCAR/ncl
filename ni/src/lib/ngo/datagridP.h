/*
 *      $Id: datagridP.h,v 1.3 1997-06-20 21:48:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datagridP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Apr 25 14:44:43 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_DATAGRIDP_H_
#define	_NG_DATAGRIDP_H_

#include <ncarg/ngo/goP.h>
#include <ncarg/ngo/datagrid.h>
 
#define DEBUG_DATA_GRID 0
#define BUFINC 4096
#define MAX_HEADERS 10

/*
 * Any field in the public structure NgDataGrid, defined in
 * datagrid.h, must appear in the same order at the beginning
 * of the NgDataGridRec definition. 
 */

typedef struct _NgDataGridRec 
{
        NgDataGrid		datagrid;
        NgGO			go;
        Widget			parent;
        NrmQuark		qsymbol; /* fileref if filevar,
                                            NULL if regvar,
                                            var if var coord */
 	NclApiVarInfoRec	*vinfo;  /* copy of pointer in shaper, could
                                            be freed between any update call */
        int			dims_alloced;
        int			head_rows;
        int			head_cols;
        int			col_dim;
        int			row_dim;
        int			max_col;
        int			max_row;
	int			ncols;
        long			*start;
        long			*finish;
        long			*stride;
        long			*istart;
        long			*ifinish;
        long			*array_pos;
        int			*array_size;
        NhlBoolean		*array_rev;
	short			*cell_widths;
        int			cheight;
        int			reinit;
} NgDataGridRec;

#endif	/* _NG_DATAGRIDP_H_ */
