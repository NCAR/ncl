/*
 *      $Id: NcgmWorkstationP.h,v 1.3 1994-01-27 21:25:13 boote Exp $
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


typedef enum { _NhlINITED, _NhlINITEDALMOST, _NhlUNINITED } _NhlNcgmStatus;

#define NCGM_DEFAULT_CONID 7
#define NCGM_WORKSTATION_TYPE 1

typedef struct _NhlNcgmWorkstationLayerPart {
	/* User setable resource fields */

	char 	*meta_name;

	/* Private internal fields */
	
	/* Export Values */
	/* Import Values */
} NhlNcgmWorkstationLayerPart;

typedef struct _NhlNcgmWorkstationLayerRec {
	NhlBaseLayerPart	base;
	NhlWorkstationLayerPart	work;
	NhlNcgmWorkstationLayerPart	ncgm;
} NhlNcgmWorkstationLayerRec;

typedef struct _NhlNcgmWorkstationLayerClassPart {
	_NhlNcgmStatus *cgm_inited;
} NhlNcgmWorkstationLayerClassPart;

typedef struct _NhlNcgmWorkstationLayerClassRec {
	NhlBaseLayerClassPart			base_class;
	NhlWorkstationLayerClassPart		work_class;
	NhlNcgmWorkstationLayerClassPart	ncgm_class;
} NhlNcgmWorkstationLayerClassRec;

typedef struct _NhlNcgmWorkstationLayerRec *NhlNcgmWorkstationLayer;
typedef struct _NhlNcgmWorkstationLayerClassRec *NhlNcgmWorkstationLayerClass;

extern NhlNcgmWorkstationLayerClassRec NhlncgmWorkstationLayerClassRec;

#endif /* _NCgmWorkstation_h */
