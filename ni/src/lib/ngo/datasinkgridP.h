/*
 *      $Id: datasinkgridP.h,v 1.1 1997-06-23 21:08:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datasinkgridP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sun Jun 22 14:31:22 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_DATASINKGRIDP_H_
#define	_NG_DATASINKGRIDP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/datasinkgrid.h>

 
#define DEBUG_DATA_SINK_GRID 0
#define BUFINC 256
#define MAX_LINE_LENGTH 81


/*
 * Any field in the public structure NgDataSinkGrid, defined in
 * datasinkgrid.h, must appear in the same order at the beginning
 * of the NgDataSinkGridRec definition. 
 */

typedef struct _NgDataSinkGridRec 
{
        NgDataSinkGrid		public;
        
            /* private fields */
        NrmQuark		qname;
        NgDataSinkRec		*data_sink;
 	NclApiDataList		*dlist;        
        int			cwidths[2];
        short			*too_long;
        short			*last_too_long;
        int			c_alloc;
} NgDataSinkGridRec;


#endif	/* _NG_DATASINKGRIDP_H_ */
