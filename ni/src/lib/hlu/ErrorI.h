/*
 *      $Id: ErrorI.h,v 1.4 1996-09-14 17:06:11 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ErrorI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug  1 17:52:39 MDT 1994
 *
 *	Description:	
 */
#ifndef	_NErrorI_h
#define	_NErrorI_h

#include <ncarg/hlu/Error.h>

#define	_NhlNerrMode	"err.Mode"
#define	_NhlCerrMode	_NhlClangMode

/*
 * constants used for the gks error handler (gerhnd)
 */
#define	_NhlGKSERRNUM	(-9998)
#define	_NhlGKSERRMSG	"GKS ERROR REPORTED FROM libhlu.a(Error.o)"
#define	_NhlGKSMAXMSGLEN	(128)

/*
 * Callback name
 */
#define	_NhlCBerrPError	"CBerrPError"

#endif	/* _NErrorI_h */
