/*
 *      $Id: ImageWorkstation.h,v 1.1 2004-03-20 00:16:24 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ImageWorkstation.h
 *
 *	Author:		DavidBrown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Mar 17 12:28:04 MST 2004	
 *
 *	Description:	Public header for ImageWorkstation class
 */
#ifndef _NImageWorkstation_h
#define	_NImageWorkstation_h

#include <ncarg/hlu/XWorkstation.h>


/*
 * See: Workstation.h and XWorkstation.h for resources shared with 
 * XWorkstation
 */

#define	NhlNwkImageFormat       "wkImageFormat"
#define	NhlCwkImageFormat	"WkImageFormat"

#define	NhlNwkImageFileName	"wkImageFileName"
#define	NhlCwkImageFileName	"WkImageFileName"

#define	NhlTImageFormat	"ImageFormat"
typedef enum _NhlImageFormat{
	NhlXWD = 0,
	NhlPNG = 1
} NhlImageFormat;

extern NhlClass NhlimageWorkstationClass;

#endif /* _NImageWorkstation_h */
