#ifndef _RASTER_
#define _RASTER_

#include <stdio.h>

/* Definitions related to machine portability */

#define CARD32	unsigned long
#define CARD16	unsigned int
#define CARD8	unsigned char

#ifdef CRAY
#define B32	:32
#else
#define B32
#endif

/* Definitions of error codes for the library */

#define RAS_E_SYSTEM				0
#define RAS_E_INTERNAL_PROGRAMMING		1
#define RAS_E_8BIT_PIXELS_ONLY			2
#define RAS_E_8BIT_INTENSITIES_ONLY		3
#define RAS_E_8BIT_RUNLENGTHS_ONLY		4
#define RAS_E_NOT_IN_CORRECT_FORMAT		5
#define RAS_E_UNSUPPORTED_ENCODING		6
#define RAS_E_IMPROPER_COLORMAP_LOAD		7
#define RAS_E_COLORMAP_TOO_BIG			8
#define RAS_E_IMAGE_SIZE_CHANGED		9
#define RAS_E_NO_FORMAT_SPECIFIED		10
#define RAS_E_NO_STDIN_WITH_HDF			11
#define RAS_E_NULL_NAME				12
#define RAS_E_UNKNOWN_FORMAT			13
#define RAS_E_INVALID_COLORMAP			14
#define RAS_E_BAD_OPTION			15
#define RAS_E_UNSUPPORTED_RESOLUTION		16
#define RAS_E_BOGUS_RASTER_STRUCTURE		17
#define RAS_E_UNSUPPORTED_FUNCTIONS		18
#define RAS_E_TOO_MANY_DITHERBITS		19
#define RAS_E_SUN_RLE_UNSUPPORTED		20

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

#define RAS_OK		1
#define RAS_EOF		0
#define RAS_ERROR	-1

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
