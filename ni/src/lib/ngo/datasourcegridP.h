/*
 *      $Id: datasourcegridP.h,v 1.1 1998-12-16 23:51:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datasourcegridP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sun Jun 22 14:31:22 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_DATASOURCEGRIDP_H_
#define	_NG_DATASOURCEGRIDP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/datasourcegrid.h>

 
#define DEBUG_DATA_SOURCE_GRID 0
#define BUFINC 256
#define MAX_LINE_LENGTH 81


/*
 * Any field in the public structure NgDataSourceGrid, defined in
 * datasourcegrid.h, must appear in the same order at the beginning
 * of the NgDataSourceGridRec definition. 
 */

typedef struct _NgDataSourceGridRec 
{
        NgDataSourceGrid		public;
        
            /* private fields */
        NrmQuark		qname;
        NgDataProfileRec	*data_profile;
 	NclApiDataList		*dlist;        
        int			cwidths[2];
        int			c_alloc;
	NhlBoolean		created;
} NgDataSourceGridRec;


#endif	/* _NG_DATASOURCEGRIDP_H_ */
