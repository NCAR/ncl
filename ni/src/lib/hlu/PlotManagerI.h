/*
 *      $Id: PlotManagerI.h,v 1.3 1998-04-16 03:08:58 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		PlotManagerI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jan 26 18:25:49 MST 1994
 *
 *	Description:	
 */
#ifndef	_NPLOTMANAGERI_H
#define	_NPLOTMANAGERI_H

#include <ncarg/hlu/PlotManager.h>

/*
 * structure used to define the attributes of an annotation 
 * object. If a plot wishes to have an annotation managed by the PlotManager
 * Manager it needs to handle resources for these fields.
 */

typedef struct	_NhlAnnotationRec {
	int			id;
	NhlBoolean		on;
	int			zone;
	NhlPosition		side;
	NhlJustification	just;
	float			para_pos;
	float			ortho_pos;
} NhlAnnotationRec;

/* 
 * Resource used by the base PlotManager to tell its members that
 * the overlay transform has changed.
 */
   
#define NhlNpmUpdateReq		".pmUpdateReq"
#define NhlCpmUpdateReq		".PmUpdateReq"


/* 
 * Resource used by member plots to alert the base overlay when an
 * external annotation is modified in a way that affects its view.
 */
   
#define NhlNpmUpdateAnnoReq	".pmUpdateAnnoReq"
#define NhlCpmUpdateAnnoReq	".PmUpdateAnnoReq"

/* 
 * Convenience function that performs the basic management of an
 * overlay for a plot object. Designed to be called from ...Initialize
 * ...SetValues, or ...UpdateData.
 */

typedef enum _NhlCalledFrom_{
	_NhlSETVALUES = 0,
	_NhlCREATE = 1,
	_NhlUPDATEDATA = 2
} _NhlCalledFrom;


extern NhlErrorTypes _NhlManageOverlay(
#if	NhlNeedProto
	NhlLayer	*overlay_object,
	NhlLayer	lnew,
	NhlLayer	lold,
	_NhlCalledFrom	method,
	NhlSArgList	sargs,
	int		nargs,
	char		*entry_name				   
#endif
);

#endif	/* _NPLOTMANAGERI_H */



