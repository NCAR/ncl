
/*
 *      $Id: ncarg_path.h,v 1.1 1992-09-09 17:38:17 clyne Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ncarg_path.h
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 2 14:21:17 MDT 1992
 *
 *	Description:	
 */

#ifndef	_ncarg_path_
#define	_ncarg_path_

#ifndef	PATH_MAX
#ifdef	_POSIX_PATH_MAX
#define	PATH_MAX	_POSIX_PATH_MAX
#else
#define	PATH_MAX	1024
#endif
#endif	/* PATH_MAX	*/


#define	DEFAULT_TMP	"/tmp"
#define	PREFIX		"NCARG_"
#define	ROOT_ENV	"NCARG_ROOT"

#endif
