/*
 *      $Id: CoordArrays.h,v 1.1 1993-09-15 22:10:34 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArrays.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 11:34:00 MDT 1993
 *
 *	Description:	Public declarations for CoordArrays object.
 */
#ifndef _NCoordArrays_h
#define _NCoordArrays_h
#include <ncarg/hlu/DataItem.h>

/*
 * New Resource Names
 */

#define	NhlNcaXData	"caXData"
#define	NhlCcaXData	"CaXData"
#define	NhlNcaYData	"caYData"
#define	NhlCcaYData	"CaYData"

typedef struct _CoordArraysLayerClassRec *CoordArraysLayerClass;
typedef struct _CoordArraysLayerRec *CoordArraysLayer;

extern LayerClass coordArraysLayerClass;

#endif /*_NCoordArrays_h */
