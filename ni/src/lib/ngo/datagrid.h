/*
 *      $Id: datagrid.h,v 1.3 2000-03-21 02:35:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datagrid.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Apr 25 14:44:43 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_DATAGRID_H
#define	_NG_DATAGRID_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/ncl.h>

/*
 * Public api
 */

typedef struct _NgDataGrid 
{
        Widget		grid;
        Dimension	sub_width;
} NgDataGrid;

/*
 * qsymbol: fileref if vinfo represents a filevar
 *	    NULL    if vinfo represents a regvar
 *          regvar  if vinfo represents a regvar coord var
 */
NgDataGrid *NgCreateDataGrid
(
        int			goid,
        Widget			parent,
        NrmQuark 		qsymbol,
        NclApiVarInfoRec	*vinfo,
        NhlBoolean		headline_on,
        NhlBoolean		highlight_on
        );

NhlErrorTypes NgUpdateDataGrid
(
        NgDataGrid		*data_grid,
        NrmQuark		qsymbol,
        NclApiVarInfoRec	*vinfo,
	long			*start,
	long			*finish,
	long			*stride
        );

void NgDeactivateDataGrid
(
        NgDataGrid		*data_grid
        );

void NgDestroyDataGrid
(
        NgDataGrid		*data_grid
        );
        

#endif	/* _NG_DATAGRID_H */
