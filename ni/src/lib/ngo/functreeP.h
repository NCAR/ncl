/*
 *      $Id: functreeP.h,v 1.1 1999-12-07 19:08:44 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		functreeP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Nov 12 10:13:12 MST 1999
 *
 *	Description:	
 */
#ifndef	_NG_FUNCTREEP_H_
#define	_NG_FUNCTREEP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/functree.h>

 
#define DEBUG_ENTRY 1
#define DEBUG_TREE 1 << 1
#define DEBUG_FUNCTREE 0

/* defines for each node of the file tree
 * -- not including the leaf nodes.
 * 10's digit represents level in the tree hierarchy
 */
#define _ftTop		0

#define _ftPInfo	10
#define _ftPComp	11
#define _ftPData	12
#define _ftPLink	13

#define _ftPCompObj	20
#define _ftPInfoDatum	21

typedef char _ftNodeType;
typedef char _ftBool;


typedef struct _ftNodeData 
{
        struct _ftNodeData *parent;
        void		*info;
        _ftNodeType	type;
        _ftBool		expanded;
        unsigned short	subcount;
	struct _ftNodeData *subdata;
} ftNodeData;
        
typedef struct _ftCompDataRec
{
	NgDataItem	ditem;
	int		argix;
	NhlBoolean	ft_cb;
	NhlBoolean	modified;
	char		*new_value;  /* allocated by Xm - use XtFree */
} ftCompDataRec, *ftCompData;


typedef struct _NgFuncTreeRec 
{
	NgFuncTree		public;
        
            /* private fields */
        NgGO			go;
        NgPageId		page_id;
        Widget			text;
	NrmQuark		qname;
	int			data_ix;
	NgDataProfile		data_profile;
	int			arg_count;
        Dimension		c2_width;
        NhlBoolean		created;
        NhlBoolean		expand_called;
        ftNodeData		func;
	_NhlCB			*sv_cbs;
	NhlBoolean		text_dropped;		/* not enabled yet */
	XmString		selected_row_xmstr;
	NhlBoolean		manual_edit_started;
	int			edit_row;
	NhlBoolean		ignore_focus_cb;
	XtCallbackProc		focus_cb;
	NhlBoolean		edit_enabled;
} NgFuncTreeRec;

#endif	/* _NG_FUNCTREEP_H_ */




