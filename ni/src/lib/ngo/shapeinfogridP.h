/*
 *      $Id: shapeinfogridP.h,v 1.1 1997-06-04 18:08:31 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		shapeinfogridP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu May 22 19:15:36 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_SHAPEINFOGRIDP_H_
#define	_NG_SHAPEINFOGRIDP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/shapeinfogrid.h>

 
#define DEBUG_SHAPE_INFO_GRID 0
#define BUFINC 256
#define START_ROW 0
#define FINISH_ROW 1
#define STRIDE_ROW 2
#define DIM_NAMES_ROW 0
#define SELECTED_ROW 1
/*
 * Any field in the public structure NgShapeInfoGrid, defined in
 * shapeinfogrid.h, must appear in the same order at the beginning
 * of the NgShapeInfoGridRec definition. 
 */

typedef struct _NgShapeInfoGridRec 
{
            /* public fields - exported as NgShapeInfoGrid */
        
        NgShapeInfoGrid		shapeinfogrid;
        
            /* private fields */
        NgGO                    go;
        NrmQuark		qfileref;
 	NclApiVarInfoRec	*vinfo;
        int			cwidths[32];
        int			shape;
        int			cur_size;
        int			total_size;
        int			coords_alloced;
        float			*start_coords;
        float			*finish_coords;
        Boolean			*float_types;
        int			edit_row;     
} NgShapeInfoGridRec;

#endif	/* _NG_SHAPEINFOGRIDP_H_ */
