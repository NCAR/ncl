/*
 *      $Id: plottree.h,v 1.3 2000-03-21 02:35:47 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plottree.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 24 14:37:14 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_PLOTTREE_H
#define	_NG_PLOTTREE_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/dataprofile.h>

/*
 * Public api
 */

typedef void (*ptGeoNotifyFunc) (
        NhlPointer data
        );

typedef struct _NgPlotTree 
{
        Widget		tree;
        ptGeoNotifyFunc	geo_notify;
        NhlPointer	geo_data;
	int		*hlu_ids;	/* read-only pointer into plotpage */
	NhlBoolean	first_vpon;
} NgPlotTree;
                
extern NgPlotTree *NgCreatePlotTree
(
	int			go_id,
        Widget			parent,
	int			wk_id,
        NrmQuark 		qname,
	NgDataProfile		data_profile
        );

extern NhlErrorTypes NgUpdatePlotTree
(
        NgPlotTree		*plot_tree,
	int			wk_id,
        NrmQuark		qname,
	NgDataProfile		data_profile
        );

extern NgPlotTree *NgDupPlotTree
(
	int			go_id,
        Widget			parent,
	int			wk_id,
        NrmQuark		qname,
	NgDataProfile		data_profile,
	NgPlotTree		*to_plot_tree,
        NgPlotTree		*from_plot_tree
        );

extern void NgDestroyPlotTree
(
        NgPlotTree		*plot_tree
        );
        

extern int NgPlotTreeAddResList
(
        int		nclstate,
        NhlPointer	res_data,
        int		block_id
        );

typedef struct _NgPlotTreeResDataRec
{
	NgPlotTree	*plot_tree;
	NrmQuark	qname;
}NgPlotTreeResDataRec, *NgPlotTreeResData;
	
		

#endif	/* _NG_PLOTTREE_H */
