/*
 *      $Id: attrinfogridP.h,v 1.1 1997-03-04 00:04:41 dbrown Exp $
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

typedef struct _NgAttrInfoGridRec 
{
        NrmQuark		qfileref;
 	NclApiDataList		*dlist;
        Widget			grid;
        int			cwidths[2];
        short			*too_long;
        short			*last_too_long;
        int			c_alloc;
} NgAttrInfoGridRec;


#endif	/* _NG_ATTRINFOGRIDP_H_ */
