/*
 *      $Id: datasinkgrid.h,v 1.3 1997-10-03 20:07:59 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datasinkgrid.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sun Jun 22 14:31:22 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_DATASINKGRID_H
#define	_NG_DATASINKGRID_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/plotspecmenu.h>
#include <ncarg/ngo/varpage.h>

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

typedef struct _NgDataSinkGrid
{
        Widget			grid;
        NgVarPageOutput		**dataitems;
        NhlBoolean		headline_on;
        Dimension		height;
} NgDataSinkGrid;


NgDataSinkGrid *NgCreateDataSinkGrid
(
        Widget			parent,
        NrmQuark		qname,
        NgDataSinkRec		*data_sink_rec
        );

NhlErrorTypes NgUpdateDataSinkGrid
(
        NgDataSinkGrid		*data_sink_grid,
        NrmQuark		qname,
        NgDataSinkRec		*data_sink_rec
        );

void NgDestroyDataSinkGrid
(
        NgDataSinkGrid		*data_sink_grid
        );
        

#endif	/* _NG_DATASINKGRID_H */
