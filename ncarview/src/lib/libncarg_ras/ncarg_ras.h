/*
 *	$Id: ncarg_ras.h,v 1.21 2008-07-27 03:22:41 haley Exp $
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

#ifndef _RASTER_
#define _RASTER_

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>	 /* included for 'struct stat' */
#include <ncarg/c.h>

/*************************** TYPE DEFINITIONS *************************/

typedef enum {
	RAS_OK		= 1,
	RAS_EOF		= 0,
	RAS_ERROR	= -1
} RasterErrorType;

#define RAS_DEFAULT_NCOLORS	256

typedef enum {
	RAS_UNKNOWN,
	RAS_INDEXED,
	RAS_DIRECT
} RasterEncoding;

struct RasterStruct {
	/* File Related */
	char			*name;
	char			*format;
	char			*dep;		/* Format dependent */
	int			fd;		/* File descriptor */
	FILE			*fp;		/* FILE pointer */
	int			written;	/* File has been written */
	int			read;		/* File has been read */
	/*
	The next three items are associated with the file
	and tracked to insure that they do not change from
	frame to frame i.e. this interface assumes that
	all images within a single file have the same
	dimensions and encoding type.
	*/
	int			file_nx;
	int			file_ny;
	RasterEncoding		file_type;

	/* Image Related */
	int			nx;
	int			ny;		/* Vertical dimension */
	int			depth;		/* Bits deep */
	int			length;		/* Image length in bytes */
	RasterEncoding		type;		/* Encoding type */
	int			ncolor;		/* Number of colors */
	char			*text;		/* Comments and such */
	int			map_forced;	/* Color map force loaded */
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
	int			(*ImageCount)();
};

typedef struct RasterStruct Raster;

struct	RasStatStruct {
	struct stat	stat;	/* UNIX stat struct	*/
	RasterEncoding	type;	/* Encoding type	*/
	int		nx, 	/* horizontal dimension	*/
			ny;	/* vertical dimension	*/
};

typedef	struct	RasStatStruct	RasStat;

typedef struct _RasterfileStruct {
	char			*name;
	char			*format;
	int			mode;
	int			fd;
	FILE			*fp;
	int			written;
	Raster			*ras;
	char			*dep;
} RasterfileStruct;

/********************* IMAGE PROCESSING MACROS ***********************/

#define INDEXED_PIXEL(ras, x, y) \
	ras->data[((y) * ras->nx) + (x)]

#define INDEXED_PIXEL_PTR(ras, x, y) \
	&ras->data[((y) * ras->nx) + (x)]

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

#define DIRECT_RED_PTR(ras, x, y) \
	&(ras)->data[(y) * 3 * (ras)->nx + (x) * 3 + 0]

#define DIRECT_GREEN_PTR(ras, x, y) \
	&(ras)->data[(y) * 3 * (ras)->nx + (x) * 3 + 1]

#define DIRECT_BLUE_PTR(ras, x, y) \
	&(ras)->data[(y) * 3 * (ras)->nx + (x) * 3 + 2]

#define DIRECT_Y(ras, x, y) \
	(ras)->data[(y) * 2 * (ras)->nx + (x) * 2 + 0]

#define DIRECT_UV(ras, x, y) \
	(ras)->data[(y) * 3 * (ras)->nx + (x) * 3 + 1]


#define NrtNformatName		"NrtNformatName"
#define NrtNformatDesc		"NrtNformatDesc"

/* Definitions related to machine portability */

#define CARD32	unsigned long
#define CARD16	unsigned int
#define CARD8	unsigned char

#ifdef CRAY
#define B32	:32
#define B16	:16
#else
#define B32
#define B16
#endif /* CRAY */

/**************************** ERROR CODES ********************************/

/*
These macros relate to the HDF library. For new versions,
these must be checked for consistency!.
*/

#define HDF_ERROR_START		2001
#define HDF_ERRNO		(HDF_ERROR_START - 1 + abs(DFerror))

/* Raster library error codes */

#define RAS_ERROR_START		1001

typedef enum {
	 RAS_E_SYSTEM = RAS_ERROR_START,
	 RAS_E_INTERNAL,
	 RAS_E_8BIT_PIXELS_ONLY,
	 RAS_E_8BIT_INTENSITIES_ONLY,
	 RAS_E_8BIT_RUNLENGTHS_ONLY,
	 RAS_E_NOT_IN_CORRECT_FORMAT,
	 RAS_E_UNSUPPORTED_ENCODING,
	 RAS_E_IMPROPER_COLORMAP_LOAD,
	 RAS_E_COLORMAP_TOO_BIG,
	 RAS_E_IMAGE_SIZE_CHANGED,
	 RAS_E_IMAGE_TYPE_CHANGED,
	 RAS_E_NO_FORMAT_SPECIFIED,
	 RAS_E_NULL_NAME,
	 RAS_E_UNKNOWN_FORMAT,
	 RAS_E_INVALID_COLORMAP,
	 RAS_E_NO_OPTION_PARM,
	 RAS_E_UNSUPPORTED_RESOLUTION,
	 RAS_E_BOGUS_RASTER_STRUCTURE,
	 RAS_E_UNSUPPORTED_FUNCTION,
	 RAS_E_SUN_RLE_UNSUPPORTED,
	 RAS_E_PARALLAX,
	 RAS_E_PREMATURE_EOF,
	 RAS_E_PROGRAMMING,
	 RAS_E_TOO_MANY_DITHERBITS,
	 RAS_E_UNKNOWN_RESOURCE,
	 RAS_E_BOGUS_COOKIE
} RasterErrorIndex;

/* Definitions common to all machines */

#ifndef True
#define True		1
#endif

#ifndef False
#define False		0
#endif

/************************* Function Prototypes **************************/

/* This macro protects C function names from C++ name-mangling. */
NCARG_PROTO_BEGIN

extern	int	RasterInit(
#ifdef	NeedFuncProto
	int	*argc,
	char	*argv[]
#endif
	);

extern	int	RasterPrintOptions();

extern	Raster *RasterOpen(
#ifdef	NeedFuncProto
        char    *name,
        char    *format
#endif
	);

extern	Raster *RasterOpenWrite(
#ifdef	NeedFuncProto
        char    	*name,
        int     	nx,
        int     	ny,
        char   		*comment,
        RasterEncoding	encoding,
        char    	*format
#endif
	);

extern	int RasterWrite(
#ifdef	NeedFuncProto
        Raster  *ras
#endif
	);

extern	int RasterRead(
#ifdef	NeedFuncProto
        Raster  *ras
#endif
	);

extern	int RasterClose(
#ifdef	NeedFuncProto
        Raster  *ras
#endif
	);

extern	int RasterPrintInfo(
#ifdef	NeedFuncProto
        Raster  *ras
#endif
	);

extern	int RasterLoadPalette(
#ifdef	NeedFuncProto
        Raster  *ras,
        unsigned char	*colors
#endif
	);

extern	int RasterPrintColors(
#ifdef	NeedFuncProto
        Raster  *ras
#endif
	);

extern	int RasterCopyColormap(
#ifdef	NeedFuncProto
        Raster  *src,
        Raster  *dst
#endif
	);

extern	Raster *RasterCreate(
#ifdef	NeedFuncProto
	int		nx,
	int		ny,
	RasterEncoding	encoding
#endif
	);

extern	char   *RasterTypeString(
#ifdef	NeedFuncProto
	RasterEncoding	type
#endif
	);

extern	int RasterDestroy(
#ifdef	NeedFuncProto
        Raster  *ras
#endif
	);

extern	Voidptr	ras_malloc(
#ifdef	NeedFuncProto
	unsigned	len
#endif
	);

extern	Voidptr	ras_calloc(
#ifdef NeedFuncProto
	unsigned	nelem,
	unsigned	elsize
#endif
	);

extern	void	ras_free(
#ifdef NeedFuncProto
	Voidptr		p
#endif
	);

extern	int	RasterStat(
#ifdef	NeedFuncProto
	char	*path,
	char	*format,
	RasStat	*ras_stat,
	int	*icount
#endif
	);

extern const char *RasterGetFormatFromName(
#ifdef NeedFuncProto
	const char	*name
#endif
	);

extern	int RasterResampleBilinear(
#ifdef	NeedFuncProto
        Raster  *src,
        Raster  *dst,
	int	verbose
#endif
	);

extern	int RasterResampleNearestNeighbor(
#ifdef	NeedFuncProto
        Raster  *src,
        Raster  *dst,
	int	verbose
#endif
	);

extern	Raster	*RasterScale(
#ifdef	NeedFuncProto
        Raster  *src,
	int	scale
#endif
	);


extern	void	RasterCopy(
#ifdef	NeedFuncProto
	Raster	*src,
	Raster	*dst,
	int	src_x, 
	int	src_y, 
	int	src_nx, 
	int	src_ny
#endif
);

extern	void	RasterOp(
#ifdef	NeedFuncProto
	Raster	*src,
	Raster	*dst,
	int	src_x,
	int	src_y,
	int	src_nx,
	int	src_ny,
	int	dst_x,
	int	dst_y,
	int	op
#endif
);

extern	int RasterCenterCrop(
#ifdef	NeedFuncProto
	Raster		*src,
	Raster		*dst
#endif
);

extern	int RasterInvert(
#ifdef	NeedFuncProto
	Raster		*src,
	Raster		*dst
#endif
);


extern int	RasterInitError();

extern int	RasterSetError(
#ifdef	NeedFuncProto
	int	error_number
#endif
);

extern const char *RasterGetError();

extern int	RasterEsprintfError();

extern int	RasterPrintError();



/********* Function definitions for Generic driver ********************/

extern int GenericClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

/********* Function definitions for SGI semi-public interface ***********/

extern Raster *SGIOpen(
#ifdef NeedFuncProto
	char	*name
#endif
	);

extern int SGIRead(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern Raster *SGIOpenWrite(
#ifdef NeedFuncProto
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
#endif
	);

extern int SGIWrite(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int SGIPrintInfo(
#ifdef NeedFuncProto
	Raster		*ras
#endif
	);

extern int SGIClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int SGISetFunctions(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

/********* Function definitions for NRIF semi-public interface ***********/

extern Raster *NrifOpen(
#ifdef NeedFuncProto
	char	*name
#endif
	);

extern int NrifRead(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern Raster *NrifOpenWrite(
#ifdef NeedFuncProto
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
#endif
	);

extern int NrifWrite(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int NrifPrintInfo(
#ifdef NeedFuncProto
	Raster		*ras
#endif
	);

extern int NrifClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int NrifSetFunctions(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

/********* Function definitions for Sun semi-public interface ***********/

extern Raster *SunOpen(
#ifdef NeedFuncProto
	char	*name
#endif
	);

extern int SunRead(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern Raster *SunOpenWrite(
#ifdef NeedFuncProto
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
#endif
	);

extern int SunWrite(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int SunPrintInfo(
#ifdef NeedFuncProto
	Raster		*ras
#endif
	);

extern int SunClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int SunSetFunctions(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

/********* Function definitions for XWD semi-public interface ***********/

extern Raster *XWDOpen(
#ifdef NeedFuncProto
	char	*name
#endif
	);

extern int XWDRead(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern Raster *XWDOpenWrite(
#ifdef NeedFuncProto
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
#endif
	);

extern int XWDWrite(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int XWDPrintInfo(
#ifdef NeedFuncProto
	Raster		*ras
#endif
	);

extern int XWDClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int XWDSetFunctions(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

/********* Function definitions for HDF semi-public interface ***********/

extern Raster *HDFOpen(
#ifdef NeedFuncProto
	char	*name
#endif
	);

extern int HDFRead(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern Raster *HDFOpenWrite(
#ifdef NeedFuncProto
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
#endif
	);

extern int HDFWrite(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int HDFPrintInfo(
#ifdef NeedFuncProto
	Raster		*ras
#endif
	);

extern int HDFClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int HDFSetFunctions(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

/********* Function definitions for AVS semi-public interface ***********/

extern Raster *AVSOpen(
#ifdef NeedFuncProto
	char	*name
#endif
	);

extern int AVSRead(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern Raster *AVSOpenWrite(
#ifdef NeedFuncProto
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
#endif
	);

extern int AVSWrite(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int AVSPrintInfo(
#ifdef NeedFuncProto
	Raster		*ras
#endif
	);

extern int AVSClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int AVSSetFunctions(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

/********* Function definitions for Parallax semi-public interface ***********/

extern Raster *ParallaxOpen(
#ifdef NeedFuncProto
	char	*name
#endif
	);

extern int ParallaxRead(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern Raster *ParallaxOpenWrite(
#ifdef NeedFuncProto
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
#endif
	);

extern int ParallaxWrite(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int ParallaxPrintInfo(
#ifdef NeedFuncProto
	Raster		*ras
#endif
	);

extern int ParallaxClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int ParallaxSetFunctions(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

/********* Function definitions for HPPCL semi-public interface ***********/

extern Raster *HPPCLOpen(
#ifdef NeedFuncProto
	char	*name
#endif
	);

extern int HPPCLRead(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern Raster *HPPCLOpenWrite(
#ifdef NeedFuncProto
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
#endif
	);

extern int HPPCLWrite(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int HPPCLPrintInfo(
#ifdef NeedFuncProto
	Raster		*ras
#endif
	);

extern int HPPCLClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int HPPCLSetFunctions(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

/********* Function definitions for Abekas semi-public interface ***********/

extern Raster *AbekasOpen(
#ifdef NeedFuncProto
	char	*name
#endif
	);

extern int AbekasRead(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern Raster *AbekasOpenWrite(
#ifdef NeedFuncProto
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
#endif
	);

extern int AbekasWrite(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int AbekasPrintInfo(
#ifdef NeedFuncProto
	Raster		*ras
#endif
	);

extern int AbekasClose(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

extern int AbekasSetFunctions(
#ifdef NeedFuncProto
	Raster	*ras
#endif
	);

NCARG_PROTO_END

#endif /* _RASTER_ */
