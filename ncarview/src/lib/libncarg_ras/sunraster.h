/*
 *	$Id: sunraster.h,v 1.5 1993-02-10 19:19:17 don Exp $
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
#ifdef DEAD
	unsigned long	ras_type B32;	/* type of file; see RT_* */
	unsigned long	ras_maptype B32; /* type of colormap; see RMT_* below */
#endif
	SunEncodingType	ras_type B32;
	SunColormapType	ras_maptype B32;
	unsigned long	ras_maplength B32;/* length (bytes) of following map */
} SunInfo;

#define	RAS_MAGIC	0x59a66a95


#ifdef DEAD
#define RT_OLD		0	/* Raw pixrect image in 68000 byte order */
#define RT_STANDARD	1	/* Raw pixrect image in 68000 byte order */
#define RT_BYTE_ENCODED	2	/* Run-length compression of bytes */
#define RT_EXPERIMENTAL 0xffff	/* Reserved for testing */
#endif

#ifdef DEAD
#define RMT_NONE	0	/* ras_maplength is expected to be 0 */
#define RMT_EQUAL_RGB	1	/* red[ras_maplength/3],green[],blue[] */
#define RMT_RAW		2
#endif

#endif /* _RASTER_SUN_ */
