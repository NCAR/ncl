/*
 *      $Id: OverlayI.h,v 1.4 1994-04-29 21:31:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		OverlayI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jan 26 18:25:49 MST 1994
 *
 *	Description:	
 */
#ifndef	_NOVERLAYI_H
#define	_NOVERLAYI_H

#include <ncarg/hlu/ViewI.h>
#include <ncarg/hlu/Overlay.h>

/* 
 * Resource used by the base Overlay to tell its members that
 * the overlay transform has changed.
 */
   
#define NhlNovUpdateReq		".ovUpdateReq"
#define NhlCovUpdateReq		".OvUpdateReq"

/* 
 * Convenience function that performs the basic management of an
 * overlay for a plot object. Designed to be called from ...Initialize
 * or ...SetValues.
 */

extern NhlErrorTypes _NhlManageOverlay(
#ifdef NhlNeedProto
	NhlLayer	*overlay_object,
	NhlLayer	lnew,
	NhlLayer	lold,
	NhlBoolean	init,
	NhlSArgList	sargs,
	int		nargs,
	char		*entry_name				   
#endif
);

extern void _NhlovCpMapXY(
#ifdef NhlNeedProto
	float *xin,
	float *yin,
	float* xout,
	float* yout
#endif
);

extern void _NhlovCpInvMapXY(
#ifdef NhlNeedProto
	float *xin,
	float *yin,
	float* xout,
	float* yout
#endif
);

#endif	/* _NOVERLAYI_H */



