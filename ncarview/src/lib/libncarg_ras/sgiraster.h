#ifndef	__SGIRASTER_H__
#define	__SGIRASTER_H__


#define SGI_FORMAT_NAME		"sgi"
#define SGI_MAGIC 		0732

typedef enum {
	SGI_CM_NORMAL,		/* file contains rows of values which 
				 * are either RGB values (zsize == 3) 
				 * or greyramp values (zsize == 1) */
	SGI_CM_DITHERED,
	SGI_CM_SCREEN,		/* File contains data which is a screen
				 * image; getrow() returns buffer which 
				 * can be displayed writepixels(). */
	SGI_CM_COLORMAP		/* File has colormap. */
} SgiColormapType;

typedef enum {
	SGI_IL_UNKNOWN,
	SGI_IL_PIXEL,
	SGI_IL_SCANPLANE
} SgiInterleaveType;

#define SGI_TYPEMASK		0xff00
#define SGI_BPPMASK		0x00ff
#define SGI_TYPE_VERBATIM	0x0000
#define SGI_TYPE_RLE		0x0100
#define SGI_ISRLE(type)		(((type) & SGI_TYPEMASK) == SGI_TYPE_RLE)
#define SGI_ISVERBATIM(type)	(((type) & SGI_TYPEMASK) == SGI_TYPE_VERBATIM)
#define SGI_BPP(type)		((type) & SGI_BPPMASK)
#define RLE(bpp)		(SGI_TYPE_RLE | (bpp))
#define VERBATIM(bpp)		(SGI_TYPE_VERBATIM | (bpp))
#define	IBUFSIZE(pixels)	((pixels+(pixels>>6))<<2)
#define	RLE_NOP			0x00

/*
SGI reserves 512 bytes for the header, but the structure itself
is not (yet) that large.
*/

#define RAS_SGI_RESERVED	512

typedef struct {
	/* Saved on disk. */
	unsigned short	imagic;
	unsigned short 	type;
	unsigned short 	dim;
	unsigned short 	xsize;
	unsigned short 	ysize;
	unsigned short 	zsize;
	unsigned long 	min;
	unsigned long 	max;
	unsigned long	wastebytes;	
	char 		name[80];
	SgiColormapType	colormap;
	/* Used in memory. */
	long 		file;
	unsigned short 	flags;
	short		dorev;
	short		x;
	short		y;
	short		z;
	short		cnt;
	unsigned short	*ptr;
	unsigned short	*base;
	unsigned short	*tmpbuf;
	unsigned long	offset;
	unsigned long	rleend;		/* for rle images */
	unsigned long	*rowstart;	/* for rle images */
	long		*rowsize;	/* for rle images */
} SGIInfo;

#endif	/* !__SGIRASTER_H__ */
