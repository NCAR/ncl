/*
 *      $Id: PSWorkstationP.h,v 1.9 2010-02-09 23:12:44 brownrig Exp $
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

#define	PSBASE		(20)
/*
 * PS workstation type identifiers start at 20.
 *
 * PS workstation type will be one of the following.
 * This value is set in PSWorkstationInitialize().
 *
 *  |  wktype  |  visual  |  format  |  orientation  |
 *  ==================================================
 *  |    20    |  color   |   ps     |    portrait   |
 *  |    21    |  color   |   eps    |    portrait   |
 *  |    22    |  color   |   epsi   |    portrait   |
 *  |    23    |  mono    |   ps     |    portrait   |
 *  |    24    |  mono    |   eps    |    portrait   |
 *  |    25    |  mono    |   epsi   |    portrait   |
 *  |    26    |  color   |   ps     |    landscape  |
 *  |    27    |  color   |   eps    |    landscape  |
 *  |    28    |  color   |   epsi   |    landscape  |
 *  |    29    |  mono    |   ps     |    landscape  |
 *  |    30    |  mono    |   eps    |    landscape  |
 *  |    31    |  mono    |   epsi   |    landscape  |
 */

typedef struct _NhlPSWorkstationLayerPart {
	/* User setable resource fields */

	NhlPSFormat		format;
	NhlVisualType		visual;
	NhlWorkOrientation	orientation;
	NhlColorModel		color_model;

	NhlString		filename;

	int			resolution;

	NhlString   paper_size;  /* standard paper name, e.g., "legal", "A2", etc. */
    float       page_width;  /*  inches  */
    float       page_height; /*    "     */
	int			lower_x;
	int			lower_y;
	int			upper_x;
	int			upper_y;

	NhlBoolean		full_background;
	NhlBoolean		suppress_background;
	NhlBoolean		suppress_bbinfo;

	/* Private internal fields */

	NhlBoolean		dev_bounds_updated;

	NhlBoundingBox          bbox;
} NhlPSWorkstationLayerPart;

typedef struct _NhlPSWorkstationLayerRec {
	NhlBaseLayerPart		base;
	NhlWorkstationLayerPart		work;
	NhlPSWorkstationLayerPart	ps;
} NhlPSWorkstationLayerRec;

typedef struct _NhlPSWorkstationClassPart {
	int	foo;
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
