/*
 *      $Id: diminfogridP.h,v 1.1 1997-03-04 02:53:51 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		diminfogridP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 14:22:58 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_DIMINFOGRIDP_H_
#define	_NG_DIMINFOGRIDP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/diminfogrid.h>

 
#define DEBUG_DIM_INFO_GRID 0
#define BUFINC 256


typedef struct _NgDimInfoGridRec 
{
        NrmQuark		qfileref;
 	NclApiVarInfoRec	*vinfo;
        Widget			grid;
        int			cwidths[32];
} NgDimInfoGridRec;

#endif	/* _NG_DIMINFOGRIDP_H_ */
