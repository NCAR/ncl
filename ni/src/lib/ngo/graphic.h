/*
 *      $Id: graphic.h,v 1.3 1999-01-11 19:36:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		graphic.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 10 18:38:01 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_GRAPHIC_H
#define	_NG_GRAPHIC_H

#include <ncarg/ngo/go.h>

typedef void (*NgSetResProc)
(
        int nclstate,
        NhlPointer res_proc_data,
        int block_id
        );

typedef void (*NgPreviewResProc)
(
        int srlist_id,
        NhlPointer res_proc_data
        );

extern
NhlErrorTypes NgCreatePreviewGraphic
(
	int		goid,
        int		*hlu_id,
	NhlString	ncl_graphic,
	NhlString	ncl_parent,
	NhlString	classname,
        int		pres_proc_count,
        NgPreviewResProc *pres_procs,
        NhlPointer	*pres_proc_data
        );

extern
NhlErrorTypes NgCreatePreviewGraphic
(
	int		goid,
        int		*hlu_id,
	NhlString	ncl_graphic,
	NhlString	ncl_parent,
	NhlString	classname,
        int		pres_proc_count,
        NgPreviewResProc *pres_procs,
        NhlPointer	*pres_proc_data
        );

extern
NhlErrorTypes NgDestroyPreviewGraphic
(
	int		goid,
        int		hlu_id
        );

extern
NhlErrorTypes NgCreateGraphic
(
	int		goid,
        int		*hlu_id,
	NhlString	ncl_graphic,
	NhlString	ncl_parent,
	NhlString	classname,
        int		res_proc_count,
        NgSetResProc	*res_procs,
        NhlPointer	*res_proc_data
        );

extern
NhlErrorTypes NgUpdateGraphic
(
	int		goid,
	NhlString	ncl_graphic,
        int		res_proc_count,
        NgSetResProc	*res_procs,
        NhlPointer	*res_proc_data
        );

extern
NhlErrorTypes NgDestroyGraphic
(
	int		goid,
	NhlString	ncl_graphic
        );

extern
NhlErrorTypes NgDrawGraphic
(
	int		goid,
	NhlString	ncl_graphic,
        NhlBoolean	clear
        );

/*
 * a wrapper for NgDrawGraphic that takes a view id instead of a string ref
 */

extern
NhlErrorTypes NgDrawView
(
	int		goid,
	int		view_id,
        NhlBoolean	clear
        );


#endif	/* _NG_GRAPHIC_H */

