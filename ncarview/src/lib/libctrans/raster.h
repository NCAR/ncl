/*
 *	$Id: raster.h,v 1.2 1991-06-18 15:03:31 clyne Exp $
 */
#ifndef _RASTER_
#define _RASTER_

#include <stdio.h>

/* Definitions related to machine portability */

#define CARD32	unsigned long

#ifdef CRAY
#define B32	:32
#else
#define B32
#endif

/* Definitions common to all machines */

#ifndef True
#define True		1
#endif

#ifndef False
#define False		0
#endif

#define E_FATAL		1
#define E_NONFATAL	2
#define E_WARNING	3

#define RAS_UNKNOWN	0
#define RAS_INDEXED	1
#define RAS_DIRECT	2
#define RAS_DIRECT_YUV	3

struct RasterStruct {
	char			*name;
	char			*format;
	int			fd;
	FILE			*fp;
	int			written;
	int			nx;
	int			ny;		/* Vertical dimension */
	int			depth;		/* Bits deep */
	int			length;		/* Image length in bytes */
	int			type;		/* Encoding type */
	int			ncolor;		/* Number of colors */
	char			*text;		/* Comments and such */
	char			*dep;		/* Format dependent */
	int			map_loaded;	/* Color map force loaded */
	unsigned char		*red;		/* Red color table */
	unsigned char		*green;		/* Green color table */
	unsigned char		*blue;		/* Blue color table */
	unsigned char		*data;		/* Image pixels */
	struct RasterStruct	*(*Open)();
	struct RasterStruct	*(*OpenWrite)();
	int			(*Read)();
	int			(*Write)();
	int			(*Close)();
	int			(*PrintInfo)();
};

typedef struct RasterStruct Raster;

static char *raster_encodings[] = {
	"RAS_UNKNOWN",
	"RAS_INDEXED",
	"RAS_DIRECT",
	"RAS_DIRECT_YUV"
};

static char *raster_formats[] = {
	"nrif",
	"sun",
	"xwd",
	"hdf"
};

#define INDEXED_PIXEL(ras, x, y) \
	ras->data[((y) * ras->nx) + (x)]

#define INDEXED_RED(ras, pixel) \
	ras->red[(pixel)]

#define INDEXED_GREEN(ras, pixel) \
	ras->green[(pixel)]

#define INDEXED_BLUE(ras, pixel) \
	ras->blue[(pixel)]

#define COPY_COLORMAP(from, to) \
	{ \
	int	i; \
		for(i=0; i<256; i++) { \
			(to)->red[i] = (from)->red[i]; \
			(to)->green[i] = (from)->green[i]; \
			(to)->blue[i] = (from)->blue[i]; \
		} \
	}

#define DIRECT_RED(ras, x, y) \
	(ras)->data[(y) * 3 * (ras)->nx + (x) * 3 + 0]

#define DIRECT_GREEN(ras, x, y) \
	(ras)->data[(y) * 3 * (ras)->nx + (x) * 3 + 1]

#define DIRECT_BLUE(ras, x, y) \
	(ras)->data[(y) * 3 * (ras)->nx + (x) * 3 + 2]

#define DIRECT_Y(ras, x, y) \
	(ras)->data[(y) * 2 * (ras)->nx + (x) * 2 + 0]

#define DIRECT_UV(ras, x, y) \
	(ras)->data[(y) * 3 * (ras)->nx + (x) * 3 + 1]


#endif _RASTER_
