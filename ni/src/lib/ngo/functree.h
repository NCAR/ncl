/*
 *      $Id: functree.h,v 1.3 2000-05-16 01:59:23 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		functree.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Nov 12 10:13:12 MST 1999
 *
 *	Description:	
 */
#ifndef	_NG_FUNCTREE_H
#define	_NG_FUNCTREE_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/ncl.h>
#include <ncarg/ngo/browse.h>

/*
 * Public api
 */

typedef void (*ftGeoNotifyFunc) (
        NhlPointer data
        );

typedef struct _NgFuncTree 
{
        Widget		tree;
        ftGeoNotifyFunc	geo_notify;
        NhlPointer	geo_data;
} NgFuncTree;
                
extern NgFuncTree *NgCreateFuncTree
(
        int			go_id,
        Widget			parent,
	NrmQuark		qname,
	int			data_ix,
	NgDataProfile		data_profile,
	NhlBoolean		edit_enabled
        );

extern NhlErrorTypes NgUpdateFuncTree
(
        NgFuncTree		*func_tree,
	NrmQuark		qname,
	int			data_ix,
	NgDataProfile		data_profile,
	NhlBoolean		edit_enabled
        );

extern NgFuncTree *NgDupFuncTree
(
	int			go_id,
        Widget			parent,
	NrmQuark		qname,
        int			data_ix,
	NgDataProfile		data_profile,
	NgFuncTree		*to_func_tree,
        NgFuncTree		*from_func_tree
        );

extern void NgDestroyFuncTree
(
        NgFuncTree		*func_tree
        );
        

extern NhlString NgGetFuncTreeValue
(
	NgFuncTree		*func_tree,
	int			*data_ix,
	NhlBoolean		*is_new_value
        );

		

#endif	/* _NG_FUNCTREE_H */
