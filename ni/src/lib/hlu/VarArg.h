/*
 *      $Id: VarArg.h,v 1.2 1993-05-27 19:11:31 ethan Exp $
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

#ifdef	__STDC__
#include <stdarg.h>
#define VA_START(ap,last)       va_start(ap,last) 
#else
#include <varargs.h>
#define VA_START(ap,last)       va_start(ap) 
#endif

extern int _NhlCountSetVarList(
#ifdef	NhlNeedProto
	va_list	/* var arg list to be counted	*/
#endif
);

extern int _NhlCountGetVarList(
#ifdef	NhlNeedProto
	va_list	/* var arg list to be counted	*/
#endif
);

extern void _NhlVarToSetArgList(
#ifdef	NhlNeedProto
	va_list         ap,             /* vararg list  */ 
	_NhlArgList     *args,          /* pointer to return arglist in */ 
	int             num_vargs       /* number of arg pairs in ap    */ 
#endif
);

extern void _NhlVarToGetArgList(
#ifdef	NhlNeedProto
	va_list         ap,             /* vararg list  */ 
	_NhlArgList     *args,          /* pointer to return arglist in */ 
	int             num_vargs       /* number of arg pairs in ap    */ 
#endif
);

#endif	/* _VARARG_H_ */
