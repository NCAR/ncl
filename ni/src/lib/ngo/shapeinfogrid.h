/*
 *      $Id: shapeinfogrid.h,v 1.2 1997-06-06 03:14:54 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		shapeinfogrid.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu May 22 19:15:36 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_SHAPEINFOGRID_H
#define	_NG_SHAPEINFOGRID_H

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

typedef void (*NgShapeNotify)(
#if     NhlNeedProto
        NhlPointer data
#endif
);

typedef void (*NgDimSelectNotify)(
#if     NhlNeedProto
        NhlPointer data
#endif
);

typedef struct _NgShapeInfoGrid 
{
        Widget		grid;
        long		*start;
        long		*finish;
        long		*stride;
        Boolean		index_mode;
        Boolean		headline_on;
        Boolean		highlight_on;
        Dimension	height;
        int		selected_dim;
        NhlBoolean	synchro_step;
        NgShapeNotify	shape_notify;
        NgDimSelectNotify dim_select_notify;
        NhlPointer	notify_data;
} NgShapeInfoGrid;
                
NgShapeInfoGrid *NgCreateShapeInfoGrid
(
        NgGO                    go,
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiVarInfoRec	*vinfo,
        NhlBoolean		headline_on,
        NhlBoolean		highlight_on
        );

NhlErrorTypes NgUpdateShapeInfoGrid
(
        NgShapeInfoGrid		*shape_info_grid,
        NrmQuark		qfileref,
        NclApiVarInfoRec	*vinfo
        );

void NgDestroyShapeInfoGrid
(
        NgShapeInfoGrid		*shape_info_grid
        );
        
void NgDeactivateShapeInfoGrid
(
        NgShapeInfoGrid		*shape_info_grid
        );

#define NG_INCREMENT 0
#define NG_DECREMENT 1
#define NG_MAX_VAL   2
#define NG_MIN_VAL   3
#define NG_MATCH_VAL 4
#define NG_STRIDE_INC 5
#define NG_STRIDE_DEC 6

NhlErrorTypes NgShapeInfoGridEditFocusCell
(
        NgShapeInfoGrid		*shape_info_grid,
        unsigned char		how,
        Boolean			synchro_mode_update
        );

NhlErrorTypes NgShapeInfoGridEditFocusCellComplete
(
        NgShapeInfoGrid		*shape_info_grid
        );

NhlErrorTypes NgShapeInfoGridSynchroStepMode
(
        NgShapeInfoGrid		*shape_info_grid,
        NhlBoolean		on
        );


#endif	/* _NG_SHAPEINFOGRID_H */
