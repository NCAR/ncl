/*
 *      $Id: datavargrid.h,v 1.3 2000-05-16 01:59:21 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datavargrid.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jul 27 14:04:18 MDT 1999
 *
 *	Description:	
 */
#ifndef	_NG_DATAVARGRID_H
#define	_NG_DATAVARGRID_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/browse.h>

/*
 * Public api
 */

typedef struct _NgDataVarGrid
{
        Widget			grid;
        NhlBoolean		headline_on;
        Dimension		height;
	int			plotdata_count;
        NgPlotData		plotdata;
	int			var_string_count;
	NhlString		*var_strings;
} NgDataVarGrid;


NgDataVarGrid *NgCreateDataVarGrid
(
	int			go_id,
        Widget			parent,
        NrmQuark		qname,
	int			count,
        NgPlotData		plotdata
        );

NhlErrorTypes NgUpdateDataVarGrid
(
        NgDataVarGrid		*datavar_grid,
        NrmQuark		qname,
	int			count,
        NgPlotData		plotdata
        );

void NgDestroyDataVarGrid
(
        NgDataVarGrid		*datavar_grid
        );

void NgDeactivateDataVarGrid
(
        NgDataVarGrid		*datavar_grid
        );
        

#endif	/* _NG_DATAVARGRID_H */
