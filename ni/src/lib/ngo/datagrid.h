/*
 *      $Id: datagrid.h,v 1.1 1997-06-04 18:08:24 dbrown Exp $
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

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif

/*
 * Public api
 */

typedef struct _NgDataGrid 
{
        Widget		grid;
        long		*start;
        long		*finish;
        long		*stride;
        Dimension	sub_width;
} NgDataGrid;

/*
 * qsymbol: fileref if vinfo represents a filevar
 *	    NULL    if vinfo represents a regvar
 *          regvar  if vinfo represents a regvar coord var
 */
NgDataGrid *NgCreateDataGrid
(
        NgGO                    go,
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
        NclApiVarInfoRec	*vinfo
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
