/*
 *      $Id: nio_path.h,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	File:		nio_path.h
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 2 14:21:17 MDT 1992
 *
 *	Description:	
 */

#ifndef	_nio_path_
#define	_nio_path_


#ifndef	PATH_MAX
#ifdef	_POSIX_PATH_MAX
#define	PATH_MAX	_POSIX_PATH_MAX
#else
#define	PATH_MAX	1024
#endif
#endif	/* PATH_MAX	*/

#define	PREFIX			"NCARG_"

#ifndef	_NGPATHDELIMITER
#define	_NGPATHDELIMITER	"/"
#endif	/* _NGPATHDELIMITER */

extern const char *get_ncarg_path_err(
#ifdef	NeedFuncProto
	void
#endif
);


#ifdef	__STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif	/*	__STDC__	*/


#endif
