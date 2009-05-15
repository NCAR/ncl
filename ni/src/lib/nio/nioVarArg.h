/*
 *      $Id: nioVarArg.h,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VarArg.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 3 17:14:31 MDT 1992
 *
 *	Description:	This file contains all the macro defs and prototypes
 *			to support varargs for the hlu library.
 */
#ifndef	_VARARG_H_
#define	_VARARG_H_


#include "niohluP.h"

#if	NhlNeedProto
#include <stdarg.h>
#define VA_START(ap,last)       va_start(ap,last) 
#else
#include <varargs.h>
#define VA_START(ap,last)       va_start(ap) 
#endif

extern int _NhlVarToSetArgList(
#if	NhlNeedProto
	va_list		ap,		/* vararg list			*/ 
	_NhlArgList	args		/* pointer to return arglist in	*/ 
#endif
);

#endif	/* _VARARG_H_ */
