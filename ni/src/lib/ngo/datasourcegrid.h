/*
 *      $Id: datasourcegrid.h,v 1.2 1999-01-11 19:36:24 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datasourcegrid.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sun Jun 22 14:31:22 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_DATASOURCEGRID_H
#define	_NG_DATASOURCEGRID_H

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
#if 0
typedef struct _NgOLDDataProfileRec
{
	NgClassType	type;
        NhlString	def_name;
        NhlString	class_name;
	NhlClass	class;
        int		n_dataitems;
	int		master_data_ix;
        int		n_datadims[8];
        NhlString	data_names[8];
        NrmQuark	data_resnames[8];
	NrmQuark	qfiles[8];
	NrmQuark	qvars[8];
	int		n_coords;
        int		coord_ix[8];
} NgOLDDataProfileRec, *NgOLDDataProfile;
#endif

typedef struct _NgDataSourceGrid
{
        Widget			grid;
        NhlBoolean		headline_on;
        Dimension		height;
} NgDataSourceGrid;


NgDataSourceGrid *NgCreateDataSourceGrid
(
	NgGO			go,
        Widget			parent,
        NrmQuark		qname,
        NgDataProfile		data_profile
        );

NhlErrorTypes NgUpdateDataSourceGrid
(
        NgDataSourceGrid	*data_source_grid,
        NrmQuark		qname,
        NgDataProfile		data_profile
        );

void NgDestroyDataSourceGrid
(
        NgDataSourceGrid		*data_source_grid
        );

void NgDeactivateDataSourceGrid
(
        NgDataSourceGrid		*data_source_grid
        );
        

#endif	/* _NG_DATASOURCEGRID_H */
