/*
 *	$Id: wks.h,v 1.2 2000-07-12 16:51:04 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/***********************************************************************
* 
*	NCAR Graphics - UNIX Version 3.1.2
*	Copyright (C) 1987, 1988, 1989, 1991, 2000
*	University Corporation for Atmospheric Research
*	All Rights Reserved
*     
***********************************************************************/

#define DEFAULT_GKS_OUTPUT	"gmeta"
#define DEFAULT_TRANSLATOR	"ctrans"

#define MAX_UNITS		256

/* Information below should, in general, not be altered */

#ifndef TRUE
#define TRUE			1
#endif

#ifndef FALSE
#define FALSE			0
#endif

#define RECORDSIZE		1440

#define MF_CLOSED		(FILE *) NULL

#define NO_OUTPUT		0
#define FILE_OUTPUT		1
#define PIPE_OUTPUT		2

#define MF_READ_ERROR		302
#define MF_WRITE_ERROR		303

/* For those systems that don't define the constants for lseek(). */

#ifndef L_SET
#define L_SET 0
#endif

#ifndef L_INCR
#define L_INCR 1
#endif

