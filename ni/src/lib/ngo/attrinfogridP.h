/*
 *      $Id: attrinfogridP.h,v 1.2 1997-06-04 18:08:21 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		attrinfogridP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 14:22:58 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_ATTRINFOGRIDP_H_
#define	_NG_ATTRINFOGRIDP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/attrinfogrid.h>

 
#define DEBUG_ATTR_INFO_GRID 0
#define BUFINC 256
#define MAX_LINE_LENGTH 81


/*
 * Any field in the public structure NgAttrInfoGrid, defined in
 * attrinfogrid.h, must appear in the same order at the beginning
 * of the NgAttrInfoGridRec definition. 
 */

typedef struct _NgAttrInfoGridRec 
{
            /* public fields - exported as NgAttrInfoGrid */
        Widget			grid;
        NhlBoolean		headline_on;
        Dimension		height;
        
            /* private fields */
        NrmQuark		qfileref;
 	NclApiDataList		*dlist;
        int			cwidths[2];
        short			*too_long;
        short			*last_too_long;
        int			c_alloc;
} NgAttrInfoGridRec;


#endif	/* _NG_ATTRINFOGRIDP_H_ */
