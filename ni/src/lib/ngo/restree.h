/*
 *      $Id: restree.h,v 1.5 1999-05-22 00:36:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		restree.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 28 13:18:34 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_RESTREE_H
#define	_NG_RESTREE_H

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
typedef void (*rtGeoNotifyFunc) (
        NhlPointer data
        );

typedef struct _NgResTree 
{
        Widget		tree;
        NhlBoolean	preview_instance;
        Widget		h_scroll;
        Widget		v_scroll;
        rtGeoNotifyFunc	geo_notify;
        NhlPointer	geo_data;
} NgResTree;

extern int NgResTreeAddResList
(
        int		nclstate,
        NhlPointer	res_tree,
        int		block_id
        );

void NgResTreePreviewResList
(
        int		setrl_id,
        NhlPointer	res_tree
        );

extern NhlErrorTypes NgResTreeResUpdateComplete
(
        NgResTree	*res_tree,
        int		hlu_id,
        NhlBoolean	update_all
        );

extern NhlErrorTypes NgResTreeInstallSetValCB
(
        NgResTree	*res_tree,
        int		hlu_id,
        NhlBoolean	install
        );

extern NgResTree *NgCreateResTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark		qhlu,
        NhlClass		class,
        int			hlu_id
        );

extern NhlErrorTypes NgUpdateResTree
(
        NgResTree		*res_tree,
        NrmQuark		qhlu,
        NhlClass		class,
        int			hlu_id
        );

extern NgResTree *NgDupResTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark		qhlu,
        NhlClass		class,
        int			hlu_id,
	NgResTree		*to_res_tree,
        NgResTree		*from_res_tree
        );


extern void NgDestroyResTree
(
        NgResTree		*res_tree
        );

extern void NgRestoreResTreeOverlays
(
        NgResTree		*res_tree
        );

NhlString NgResTreeGetSetValue
(
        NgResTree	*res_tree,
	NrmQuark	resq
        );

#endif	/* _NG_RESTREE_H */
