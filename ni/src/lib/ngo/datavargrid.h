/*
 *      $Id: datavargrid.h,v 1.1 1999-07-30 03:20:50 dbrown Exp $
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

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif

#include <ncarg/ngo/dataprofile.h>

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
} NgDataVarGrid;


NgDataVarGrid *NgCreateDataVarGrid
(
	NgGO			go,
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
