/*
 *      $Id: restreeP.h,v 1.1 1997-08-21 16:33:05 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
**************************************************************f**********/
/*
 *	File:		restreeP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 28 13:18:34 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_RESTREEP_H_
#define	_NG_RESTREEP_H_

#include <ncarg/ngo/goP.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/ResourcesP.h>

#include <ncarg/ngo/restree.h>

 
#define DEBUG_ENTRY 1
#define DEBUG_TREE 1 << 1
#define DEBUG_RESTREE DEBUG_ENTRY | DEBUG_TREE

/* defines for each node of the file tree
 * -- not including the leaf nodes.
 * 10's digit represents level in the tree hierarchy
 */
#define _rtTop		0

#define _rtLevel1	10
#define _rtLevel2	20
#define _rtLevel3	30
#define _rtLevel4	40


#define _rtClassGroup   01
#define _rtClass	02
#define _rtResGroup	03
#define _rtRes		04

#define _rtSuperClassGroup 0
#define _rtChildClassGroup 1

typedef char _rtNodeType;

typedef char _rtBool;

typedef struct _rtResData
{
        NrmResource	*res;
        NhlClass	real_class;
        _rtBool		vis;
        int		row;
        NhlString	value;
} rtResData;

typedef struct _rtSetValNode 
{
        struct _rtSetValNode	*next;
        int			row;
        rtResData		*res_data;
} rtSetValNode;
        
typedef struct _rtNodeData 
{
        struct _rtNodeData	*parent;
        void			*info;
        _rtNodeType		type;
        _rtBool			expanded;
        unsigned short		subcount;
	struct _rtNodeData  	*subdata;
} rtNodeData;

typedef struct _NgResTreeRec 
{
            /* public data */
        NgResTree		restree;
        
            /* private fields */
        NgGO			go;
        int			nclstate;
        NrmQuark		qhlu;
        NhlClass		class;
        int			super_class_count;
        int			class_count;
        NhlClass		*classes;
        NhlBoolean		*instantiated;
        int			*top_res_counts;
        int			qnames_count;
        NrmNameList		qnames;
        int			res_data_count;
        int			res_data_alloc_count;
        rtResData		*res_data;
        int			hlu_id;
        Dimension		c2_width;
        NhlBoolean		created;
        NhlBoolean		expand_called;
        rtNodeData		top;
        int			edit_row;
	Boolean			manual_edit_started;
        rtSetValNode		*set_val_list;
        Boolean			scroll_cbs_installed;
        Widget			text;
	Widget			enum_ed;
	Widget			enum_menu;
	Widget			*enum_buttons;
	int			enum_buttons_alloced;
 	int			enum_buttons_used;
} NgResTreeRec;

#endif	/* _NG_RESTREEP_H_ */


