/*
 *	$Id: sunraster.h,v 1.3 1992-03-20 18:44:02 don Exp $
 */
#ifndef _RASTER_SUN_
#define _RASTER_SUN_

#define RAS_SUN_ESC	128

extern Raster	*SunOpen();
extern Raster	*SunOpenWrite();
extern int	SunRead();
extern int	SunWrite();
extern int	SunClose();
extern int	SunPrintInfo();

/* Include file for Sun rasterfiles */

#define SUN_HEADER_SIZE	32

typedef struct SunInfoStruct {
	unsigned long	ras_magic B32;	/* magic number */
	unsigned long	ras_width B32;	/* width (pixels) of image */
	unsigned long	ras_height B32;	/* height (pixels) of image */
	unsigned long	ras_depth B32;	/* depth - 1,8,24 bits */
	unsigned long	ras_length B32;	/* length (bytes) of image */
	unsigned long	ras_type B32;	/* type of file; see RT_* */
	unsigned long	ras_maptype B32; /* type of colormap; see RMT_* below */
	unsigned long	ras_maplength B32;/* length (bytes) of following map */
} SunInfo;

#define	RAS_MAGIC	0x59a66a95

/* Sun image encoding types. */

#define RT_OLD		0	/* Raw pixrect image in 68000 byte order */
#define RT_STANDARD	1	/* Raw pixrect image in 68000 byte order */
#define RT_BYTE_ENCODED	2	/* Run-length compression of bytes */
#define RT_EXPERIMENTAL 0xffff	/* Reserved for testing */

/* Sun colormap types. */
#define RMT_NONE	0	/* ras_maplength is expected to be 0 */
#define RMT_EQUAL_RGB	1	/* red[ras_maplength/3],green[],blue[] */
#define RMT_RAW		2

#endif /* _RASTER_SUN_ */
