/*
 *      $Id: filetree.h,v 1.2 2000-03-21 02:35:40 dbrown Exp $
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
#include <ncarg/ngo/ncl.h>

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
	int			go_id,
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
	int			go_id,
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
