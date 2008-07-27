/*
 *	$Id: sunraster.h,v 1.11 2008-07-27 03:22:41 haley Exp $
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

#ifndef _RASTER_SUN_
#define _RASTER_SUN_

#include <stdint.h>

#define RAS_SUN_ESC	128

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
	uint32_t	ras_magic B32;	/* magic number */
	uint32_t	ras_width B32;	/* width (pixels) of image */
	uint32_t	ras_height B32;	/* height (pixels) of image */
	uint32_t	ras_depth B32;	/* depth - 1,8,24 bits */
	uint32_t	ras_length B32;	/* length (bytes) of image */
	SunEncodingType	ras_type B32;
	SunColormapType	ras_maptype B32;
	uint32_t	ras_maplength B32;/* length (bytes) of following map */
} SunInfo;

#define	RAS_MAGIC	0x59a66a95

#endif /* _RASTER_SUN_ */
