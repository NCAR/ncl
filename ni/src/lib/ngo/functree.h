/*
 *      $Id: functree.h,v 1.1 1999-12-07 19:08:43 dbrown Exp $
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
#include <ncarg/ngo/dataprofile.h>

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
        NgGO			go,
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
        NgGO			go,
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
