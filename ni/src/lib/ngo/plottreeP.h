/*
 *      $Id: plottreeP.h,v 1.2 1999-10-13 17:15:51 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plottreeP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 24 14:37:14 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_PLOTTREEP_H_
#define	_NG_PLOTTREEP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/plottree.h>

 
#define DEBUG_ENTRY 1
#define DEBUG_TREE 1 << 1
#define DEBUG_PLOTTREE 0

/* defines for each node of the file tree
 * -- not including the leaf nodes.
 * 10's digit represents level in the tree hierarchy
 */
#define _ptTop		0

#define _ptPInfo	10
#define _ptPComp	11
#define _ptPData	12
#define _ptPLink	13

#define _ptPCompObj	20

typedef char _ptNodeType;
typedef char _ptBool;


typedef struct _ptNodeData 
{
        struct _ptNodeData *parent;
        void		*info;
        _ptNodeType	type;
        _ptBool		expanded;
        unsigned short	subcount;
	struct _ptNodeData  *subdata;
} ptNodeData;
        
typedef struct _ptCompDataRec
{
        NrmQuark	qname;
	NhlBoolean	on;
	int		usr_on;
	NhlBoolean	pt_cb;
	NhlBoolean	modified;
} ptCompDataRec, *ptCompData;

/*
 * Any field in the public structure NgPlotTree, defined in
 * plottree.h, must appear in the same order at the beginning
 * of the NgPlotTreeRec definition. 
 */

typedef struct _NgPlotTreeRec 
{
	NgPlotTree		public;
        
            /* private fields */
        NgGO			go;
	int			wk_id;
        NgPageId		page_id;
        Widget			text;
        NrmQuark		qname;
	NgDataProfile		data_profile;
	int			ditem_vis_count;
        Dimension		c2_width;
        NhlBoolean		created;
        NhlBoolean		expand_called;
        ptNodeData		plot;
	_NhlCB			*sv_cbs;
} NgPlotTreeRec;

#endif	/* _NG_PLOTTREEP_H_ */




