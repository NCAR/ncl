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

#ifdef	alpha
typedef	unsigned int	UInt32_T;
typedef	unsigned short	UInt16_T;
typedef	int		Int32_T;
typedef	short		Int16_T;
#else
typedef	unsigned long	UInt32_T;
typedef	unsigned short	UInt16_T;
typedef	long		Int32_T;
typedef	short		Int16_T;
#endif

#define RAS_SGI_RESERVED	512

typedef struct {
	/* Saved on disk. */
	UInt16_T	imagic B16;
	UInt16_T 	type B16;
	UInt16_T 	dim B16;
	UInt16_T 	xsize B16;
	UInt16_T 	ysize B16;
	UInt16_T 	zsize B16;
	UInt32_T 	min B32;
	UInt32_T 	max B32;
	UInt32_T	wastebytes B32;	
	char 		name[80];
	SgiColormapType	colormap B32;
	/* Used in memory. */
	Int32_T 	file B32;
	UInt16_T 	flags B16;
	Int16_T		dorev B16;
	Int16_T		x B16;
	Int16_T		y B16;
	Int16_T		z B16;
	Int16_T		cnt B16;
	UInt16_T	*ptr B16;
	UInt16_T	*base B16;
	UInt16_T	*tmpbuf B16;
	UInt32_T	offset B32;
	UInt32_T	rleend B32;		/* for rle images */
	UInt32_T	*rowstart B32;	/* for rle images */
	Int32_T		*rowsize B32;	/* for rle images */
} SGIInfo;

#endif	/* !__SGIRASTER_H__ */
