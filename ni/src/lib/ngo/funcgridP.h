/*
 *      $Id: funcgridP.h,v 1.1 1999-12-07 19:08:42 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		funcgridP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Dec  6 14:01:33 MST 1999
 *
 *	Description:	
 */
#ifndef	_NG_FUNCGRIDP_H_
#define	_NG_FUNCGRIDP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/funcgrid.h>
#include <ncarg/ngo/functree.h>

 
#define DEBUG_FUNC_GRID 0
#define BUFINC 256
#define MAX_LINE_LENGTH 81


/*
 * Any field in the public structure NgFuncGrid, defined in
 * funcgrid.h, must appear in the same order at the beginning
 * of the NgFuncGridRec definition. 
 */

typedef struct _NgFuncGridRec 
{
        NgFuncGrid		public;
        
            /* private fields */
	NgGO			go;
	Widget			parent;
        NrmQuark		qname;
        NgDataProfile		data_profile;
 	NclApiDataList		*dlist;        
        int			cwidths[4];
        int			c_alloc;
	NhlBoolean		created;
	NhlBoolean		in_edit;
	int			selected_row;
	int			vis_row_count;
	XmString		edit_save_string;
	Widget			text;
	NhlBoolean		text_dropped;
	int			func_tool_id;
	int			data_ix;
	NgFuncTree		*func_tree;
	Widget			restore_tgl;
	Widget			enable_tgl;
	NgVarDataSetState	*orig_states;
	NgVarDataSetState	*last_states;
} NgFuncGridRec;


#endif	/* _NG_FUNCGRIDP_H_ */
