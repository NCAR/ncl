/*
 *      $Id: App.h,v 1.3 1995-02-17 10:22:53 boote Exp $
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

#define	NhlNappUsrDir		"appUsrDir"
#define	NhlCappUsrDir		"AppUsrDir"
#define	NhlNappSysDir		"appSysDir"
#define	NhlCappSysDir		"AppSysDir"
#define NhlNappFileSuffix	"appFileSuffix"
#define NhlCappFileSuffix	"AppFileSuffix"
#define NhlNappDefaultParent	"appDefaultParent"
#define NhlCappDefaultParent	"AppDefaultParent"

extern NhlLayerClass NhlappLayerClass;

extern int NhlAppGetDefaultParentID(
#if	NhlNeedProto
	void
#endif
);

#endif  /* _NApp_h */
