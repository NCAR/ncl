/*
 *      $Id: ErrorI.h,v 1.2 1994-09-06 21:51:28 boote Exp $
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
 * useful macro for reporting and clearing the error state of llncarg.
 */
#define	_NhlErrRepClear(func,lib)\
{									\
	int	_dum;							\
	if(c_nerro(&dum)){						\
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,"%s:%s Error:%s",	\
					#func,#lib,c_semess(0)));	\
		c_errof();						\
	}								\
}
#endif	/* _NErrorI_h */
