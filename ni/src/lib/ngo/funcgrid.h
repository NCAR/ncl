/*
 *      $Id: funcgrid.h,v 1.3 2000-03-21 02:35:41 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		funcgrid.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Dec  6 14:01:33 MST 1999
 *
 *	Description:	
 */
#ifndef	_NG_FUNCGRID_H
#define	_NG_FUNCGRID_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/dataprofile.h>

/*
 * Public api
 */

typedef struct _NgFuncGrid
{
        Widget			grid;
} NgFuncGrid;


NgFuncGrid *NgCreateFuncGrid
(
	int			go_id,
        Widget			parent,
        NrmQuark		qname,
        NgDataProfile		data_profile
        );

NhlErrorTypes NgUpdateFuncGrid
(
        NgFuncGrid		*func_grid,
        NrmQuark		qname,
        NgDataProfile		data_profile
        );

NhlErrorTypes NgSynchronizeFuncGridState
(
        NgFuncGrid	*func_grid
);

void NgDestroyFuncGrid
(
        NgFuncGrid		*func_grid
        );

void NgDeactivateFuncGrid
(
        NgFuncGrid		*func_grid
        );
        

#endif	/* _NG_FUNCGRID_H */
