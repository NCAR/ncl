/*
 *      $Id: filetree.h,v 1.1 1997-06-04 18:08:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		filetree.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 13:59:32 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_FILETREE_H
#define	_NG_FILETREE_H

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

typedef void (*ftGeoNotifyFunc) (
        NhlPointer data
        );

typedef struct _NgFileTree 
{
        Widget		tree;
        ftGeoNotifyFunc	geo_notify;
        NhlPointer	geo_data;
} NgFileTree;
                
NgFileTree *NgCreateFileTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiDataList		*dlist
        );

NhlErrorTypes NgUpdateFileTree
(
        NgFileTree		*file_tree,
        NrmQuark		qfileref,
        NclApiDataList		*dlist
        );

NgFileTree *NgDupFileTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark 		qfileref,
        NclApiDataList		*dlist,
	NgFileTree		*to_file_tree,
        NgFileTree		*from_file_tree
        );

void NgDestroyFileTree
(
        NgFileTree		*file_tree
        );
        

#endif	/* _NG_FILETREE_H */
