#ifndef _NNcgmWorkstation_h
/*
 *      $Id: NcgmWorkstation.h,v 1.1 1993-04-30 17:23:21 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		NcgmWorkstation.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 14 17:03:36 MDT 1992
 *
 *	Description:	Public header for NcgmWorkstation class
 */
#define	_NNcgmWorkstation_h

#include <ncarg/hlu/Workstation.h>

#define	NhlNwkMetaName	"wkMetaName"
#define NhlCwkMetaName	"WkMetaName"

extern LayerClass ncgmWorkstationLayerClass;

typedef struct _NcgmWorkstationLayerRec *NcgmWorkstationLayer;
typedef struct _NcgmWorkstationLayerClassRec *NcgmWorkstationLayerClass;


#endif /* _NNcgmWorkstation_h */
