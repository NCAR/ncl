/*
 *      $Id: diminfogrid.h,v 1.1 1997-03-04 02:53:51 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		diminfogrid.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 13:59:32 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_DIMINFOGRID_H
#define	_NG_DIMINFOGRID_H

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

typedef NhlPointer NgDimInfoGrid;

NgDimInfoGrid NgCreateDimInfoGrid
(
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiVarInfoRec	*vinfo
        );

NhlErrorTypes NgUpdateDimInfoGrid
(
        NgDimInfoGrid		dim_info_grid,
        NrmQuark		qfileref,
        NclApiVarInfoRec	*vinfo
        );
        

#endif	/* _NG_DIMINFOGRID_H */
