/*
 *	$Id: sunraster.h,v 1.9 2000-08-22 03:30:25 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
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

#ifndef _RASTER_SUN_
#define _RASTER_SUN_

#define RAS_SUN_ESC	128

#ifdef	alpha
typedef	unsigned int	UInt32_T;
#else
typedef	unsigned long	UInt32_T;
#endif

/* Sun image encoding types. */

typedef enum {
	RT_OLD		= 0,	/* ras_length will be zero. */
	RT_STANDARD	= 1,
	RT_BYTE_ENCODED	= 2,	/* Run-length compressed. */
	RT_EXPERIMENTAL = 0xffff
} SunEncodingType;

typedef enum {
	RMT_NONE	= 0,
	RMT_EQUAL_RGB	= 1,
	RMT_RAW		= 2
} SunColormapType;

/* Include file for Sun rasterfiles */

#define SUN_HEADER_SIZE	32

typedef struct SunInfoStruct {
	UInt32_T	ras_magic B32;	/* magic number */
	UInt32_T	ras_width B32;	/* width (pixels) of image */
	UInt32_T	ras_height B32;	/* height (pixels) of image */
	UInt32_T	ras_depth B32;	/* depth - 1,8,24 bits */
	UInt32_T	ras_length B32;	/* length (bytes) of image */
	SunEncodingType	ras_type B32;
	SunColormapType	ras_maptype B32;
	UInt32_T	ras_maplength B32;/* length (bytes) of following map */
} SunInfo;

#define	RAS_MAGIC	0x59a66a95

#endif /* _RASTER_SUN_ */
