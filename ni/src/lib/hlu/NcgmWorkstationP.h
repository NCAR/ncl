/*
 *      $Id: NcgmWorkstationP.h,v 1.2 1993-10-19 17:51:58 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CgmWorkstationP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 14 17:03:13 MDT 1992
 *
 *	Description:	Private header file for CgmWorkstation class
 */
#ifndef _NCgmWorkstation_h
#define _NCgmWorkstation_h
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/hlu/NcgmWorkstation.h>


typedef enum { INITED, INITEDALMOST, UNINITED } NcgmStatus;

#define NCGM_DEFAULT_CONID 7
#define NCGM_WORKSTATION_TYPE 1

typedef struct _NcgmWorkstationLayerPart {
	/* User setable resource fields */

	char 	*meta_name;

	/* Private internal fields */
	
	/* Export Values */
	/* Import Values */
} NcgmWorkstationLayerPart;

typedef struct _NcgmWorkstationLayerRec {
	BaseLayerPart	base;
	WorkstationLayerPart	work;
	NcgmWorkstationLayerPart	ncgm;
} NcgmWorkstationLayerRec;

typedef struct _NcgmWorkstationLayerClassPart {
	NcgmStatus *cgm_inited;
} NcgmWorkstationLayerClassPart;

typedef struct _NcgmWorkstationLayerClassRec {
	BaseLayerClassPart	base_class;
	WorkstationLayerClassPart	work_class;
	NcgmWorkstationLayerClassPart	ncgm_class;
} NcgmWorkstationLayerClassRec;

extern NcgmWorkstationLayerClassRec ncgmWorkstationLayerClassRec;

#endif /* _NCgmWorkstation_h */
