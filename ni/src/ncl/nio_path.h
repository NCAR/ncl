/*
 *      $Id: nio_path.h,v 1.1 2007-01-10 00:50:38 dbrown Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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

#endif
