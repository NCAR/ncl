/*
 *      $Id: PSWorkstationP.h,v 1.2 1995-04-07 10:43:20 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		PSWorkstationP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Mar 24 00:40:46 MST 1995
 *
 *	Description:	
 */
#ifndef _NPSWorkstationP_h
#define _NPSWorkstationP_h
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/hlu/PSWorkstation.h>

#define	MAX_OPEN_PS	(15)
/*
 * PS workstation type identifiers start at 20.
 */
#define	PSBASE		(20)

typedef struct _NhlPSWorkstationLayerPart {
	/* User setable resource fields */

	NhlPSFormat		format;
	NhlVisualType		visual;
	NhlWorkOrientation	orientation;

	NhlString		filename;

	int			resolution;

	int			lower_x;
	int			lower_y;
	int			upper_x;
	int			upper_y;

	NhlBoolean		full_background;

	/* Private internal fields */
	
} NhlPSWorkstationLayerPart;

typedef struct _NhlPSWorkstationLayerRec {
	NhlBaseLayerPart		base;
	NhlWorkstationLayerPart		work;
	NhlPSWorkstationLayerPart	ps;
} NhlPSWorkstationLayerRec;

typedef struct _NhlPSWorkstationClassPart {
	int	*num_current;
} NhlPSWorkstationClassPart;

typedef struct _NhlPSWorkstationClassRec {
	NhlBaseClassPart			base_class;
	NhlWorkstationClassPart		work_class;
	NhlPSWorkstationClassPart		ps_class;
} NhlPSWorkstationClassRec;

typedef struct _NhlPSWorkstationLayerRec *NhlPSWorkstationLayer;
typedef struct _NhlPSWorkstationClassRec *NhlPSWorkstationClass;

extern NhlPSWorkstationClassRec NhlpsWorkstationClassRec;

#endif /* _NPSWorkstationP_h */
