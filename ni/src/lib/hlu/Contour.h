/*
 *      $Id: Contour.h,v 1.4 1994-01-27 21:21:34 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Contour.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for Contour class.
 */

#ifndef _NContour_h
#define _NContour_h

#include <ncarg/hlu/Overlay.h>

/*
 * Contour instance resources
 */

#define NhlNcnOutOfRangeValF	"cnOutOfRangeValF"

/*
 * Contour class resources
 */


#define NhlCcnOutOfRangeValF	"CnOutOfRangeValF"

extern NhlLayerClass			NhlcontourLayerClass;

#endif /*_NContour_h */
