/*
 *      $Id: PSWorkstation.h,v 1.6 2003-02-27 18:14:35 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		PSWorkstation.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Mar 24 00:33:23 MST 1995
 *
 *	Description:	
 */
#ifndef _NPSWorkstation_h
#define	_NPSWorkstation_h

#include <ncarg/hlu/Workstation.h>

/*
 * See: Workstation.h for common/shared resources with PDF Workstation
 */
#define	NhlNwkPSFormat		"wkPSFormat"
#define	NhlCwkPSFormat		"WkPSFormat"

#define	NhlNwkPSFileName	"wkPSFileName"
#define	NhlCwkPSFileName	"WkPSFileName"

#define	NhlNwkPSResolution	"wkPSResolution"
#define	NhlCwkPSResolution	"WkPSResolution"

/*
 * New Types.
 */
#define	NhlTPSFormat	"PSFormat"
typedef enum _NhlPSFormat{
	NhlPS = 0,
	NhlEPS = 1,
	NhlEPSI = 2
} NhlPSFormat;


extern NhlClass NhlpsWorkstationClass;

#endif /* _NPSWorkstation_h */
