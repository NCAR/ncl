/*
 *	$Id: sunraster.h,v 1.6 1993-05-11 18:49:30 haley Exp $
 */
#ifndef _RASTER_SUN_
#define _RASTER_SUN_

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
	unsigned long	ras_magic B32;	/* magic number */
	unsigned long	ras_width B32;	/* width (pixels) of image */
	unsigned long	ras_height B32;	/* height (pixels) of image */
	unsigned long	ras_depth B32;	/* depth - 1,8,24 bits */
	unsigned long	ras_length B32;	/* length (bytes) of image */
	SunEncodingType	ras_type B32;
	SunColormapType	ras_maptype B32;
	unsigned long	ras_maplength B32;/* length (bytes) of following map */
} SunInfo;

#define	RAS_MAGIC	0x59a66a95

#endif /* _RASTER_SUN_ */
