/*
 *      $Id: datasourcegrid.h,v 1.1 1998-12-16 23:51:33 dbrown Exp $
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

/*
 * Public api
 */

typedef enum {
	_NgNONGRAPHIC,
	_NgCONTOURPLOT,
	_NgSTREAMLINEPLOT,
	_NgVECTORPLOT,
	_NgXYPLOT,
	_NgCOORDARRAY,
	_NgSCALARFIELD,
	_NgVECTORFIELD
} NgClassType;
        
typedef struct _NgDataProfileRec
{
	NgClassType	type;
        NhlString	name;
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
} NgDataProfileRec, *NgDataProfile;

typedef struct _NgDataSourceGrid
{
        Widget			grid;
        NgVarDataRec		**dataitems;
        NhlBoolean		headline_on;
        Dimension		height;
} NgDataSourceGrid;


NgDataSourceGrid *NgCreateDataSourceGrid
(
        Widget			parent,
        NrmQuark		qname,
        NgDataProfileRec	*data_profile_rec
        );

NhlErrorTypes NgUpdateDataSourceGrid
(
        NgDataSourceGrid		*data_source_grid,
        NrmQuark		qname,
        NgDataProfileRec	*data_profile_rec
        );

void NgDestroyDataSourceGrid
(
        NgDataSourceGrid		*data_source_grid
        );
        

#endif	/* _NG_DATASOURCEGRID_H */
