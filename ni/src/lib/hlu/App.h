/*
 *      $Id: App.h,v 1.2 1994-10-04 01:02:02 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		App.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jul 29 12:32:39 MDT 1994
 *
 *	Description:	
 */
#ifndef _NApp_h
#define _NApp_h

#include <ncarg/hlu/Base.h>

#define	NhlNappUsrDir	"appUsrDir"
#define	NhlCappUsrDir	"AppUsrDir"
#define	NhlNappSysDir	"appSysDir"
#define	NhlCappSysDir	"AppSysDir"

#define NhlNappFileSuffix	"appFileSuffix"
#define NhlCappFileSuffix	"AppFileSuffix"

extern NhlLayerClass NhlappLayerClass;

#endif  /* _NApp_h */
