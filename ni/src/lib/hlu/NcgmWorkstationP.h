/*
 *      $Id: NcgmWorkstationP.h,v 1.8 1998-03-11 18:35:47 dbrown Exp $
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


#define NCGM_DEFAULT_CONID 7
#define NCGM_WORKSTATION_TYPE 1

typedef struct _NhlNcgmWorkstationLayerPart {
	/* User settable resource fields */

	char 		*meta_name;

	/* Private internal fields */

        int        suffix;
	NhlBoolean opened;
	NhlBoolean started;
	NhlBoolean new_frame;
	NhlBoolean update_colors;
	int	gks_iat[14];
	float	gks_rat[7];
	
	/* Export Values */

	/* Import Values */
} NhlNcgmWorkstationLayerPart;

typedef struct _NhlNcgmWorkstationLayerRec {
	NhlBaseLayerPart	base;
	NhlWorkstationLayerPart	work;
	NhlNcgmWorkstationLayerPart	ncgm;
} NhlNcgmWorkstationLayerRec;

typedef struct _NhlNcgmWorkstationClassPart {
	int	current_ncgm_wkid;
        char    *last_base_meta_name;
        int     base_meta_name_count;
} NhlNcgmWorkstationClassPart;

typedef struct _NhlNcgmWorkstationClassRec {
	NhlBaseClassPart			base_class;
	NhlWorkstationClassPart		work_class;
	NhlNcgmWorkstationClassPart	ncgm_class;
} NhlNcgmWorkstationClassRec;

typedef struct _NhlNcgmWorkstationLayerRec *NhlNcgmWorkstationLayer;
typedef struct _NhlNcgmWorkstationClassRec *NhlNcgmWorkstationClass;

extern NhlNcgmWorkstationClassRec NhlncgmWorkstationClassRec;

#endif /* _NCgmWorkstation_h */
