/*
 *      $Id: Workspace.h,v 1.5 1995-04-07 10:44:20 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Workspace.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Mar  9 12:40:57 MST 1994
 *
 *	Description:	Public header for Workspace class.
 */

#ifndef _NWORKSPACE_h
#define _NWORKSPACE_h

#include <ncarg/hlu/Base.h>

#define NhlNwsMaximumSize	"wsMaximumSize"
#define NhlNwsThresholdSize	"wsThresholdSize"
#define NhlNwsCurrentSize	"wsCurrentSize"

#define NhlCwsMaximumSize	"WsMaximumSize"
#define NhlCwsThresholdSize	"WsThresholdSize"
#define NhlCwsCurrentSize	"WsCurrentSize"

extern NhlClass NhlworkspaceClass;

extern int NhlGetWorkspaceObjectID(
#if	NhlNeedProto
	void
#endif
);

#endif /*_NWORKSPACE_h */
