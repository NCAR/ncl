/*
 *      $Id: datavargridP.h,v 1.2 1999-09-11 01:06:14 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datavargridP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jul 27 14:04:18 MDT 1999
 *
 *	Description:	
 */
#ifndef	_NG_DATAVARGRIDP_H_
#define	_NG_DATAVARGRIDP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/datavargrid.h>
#include <ncarg/ngo/shaper.h>

 
#define DEBUG_DATA_VAR_GRID 0
#define BUFINC 256
#define MAX_LINE_LENGTH 81


/*
 * Any field in the public structure NgDataVarGrid, defined in
 * datavargrid.h, must appear in the same order at the beginning
 * of the NgDataVarGridRec definition. 
 */

typedef struct _NgDataVarGridRec 
{
        NgDataVarGrid		public;
        
            /* private fields */
	NgGO			go;
	Widget			parent;
        NrmQuark		qname;
 	NclApiDataList		*dlist;        
        int			cwidths[2];
	NhlBoolean		created;
	NhlBoolean		in_edit;
	int			selected_row;
	XmString		edit_save_string;
	Widget			text;
	NhlBoolean		text_dropped;
	int			shape_tool_id;
	int			data_ix;
	NgShaper		*shaper;
} NgDataVarGridRec;


#endif	/* _NG_DATAVARGRIDP_H_ */
