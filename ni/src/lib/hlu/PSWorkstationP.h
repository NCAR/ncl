/*
 *      $Id: PSWorkstationP.h,v 1.1 1995-03-24 11:27:35 boote Exp $
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

typedef struct _NhlPSWorkstationLayerClassPart {
	int	*num_current;
} NhlPSWorkstationLayerClassPart;

typedef struct _NhlPSWorkstationLayerClassRec {
	NhlBaseLayerClassPart			base_class;
	NhlWorkstationLayerClassPart		work_class;
	NhlPSWorkstationLayerClassPart		ps_class;
} NhlPSWorkstationLayerClassRec;

typedef struct _NhlPSWorkstationLayerRec *NhlPSWorkstationLayer;
typedef struct _NhlPSWorkstationLayerClassRec *NhlPSWorkstationLayerClass;

extern NhlPSWorkstationLayerClassRec NhlpsWorkstationLayerClassRec;

#endif /* _NPSWorkstationP_h */
