/*
 *      $Id: varinfogridP.h,v 1.1 1997-03-04 00:04:42 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		varinfogridP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 14:22:58 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_VARINFOGRIDP_H_
#define	_NG_VARINFOGRIDP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/varinfogrid.h>

 
#define DEBUG_VAR_INFO_GRID 0
#define BUFINC 256


typedef struct _NgVarInfoGridRec 
{
        NrmQuark		qfileref;
 	NclApiVarInfoRec	*vinfo;
        Widget			grid;
        int			cwidths[32];
} NgVarInfoGridRec;

#endif	/* _NG_VARINFOGRIDP_H_ */
