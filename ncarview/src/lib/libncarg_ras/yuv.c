/*
**      $Id: yuv.c,v 1.4 2008-07-27 03:18:47 haley Exp $
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

/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
**	File:		yuv.c
**
**	Author:		John Clyne
**			National Center for Atmospheric Research
**			PO 3000, Boulder, Colorado
**
**	Date:		Mon Jan 23 16:10:14 MST 1995
**
**	Description:	
*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include "options.h"
#include "yuv.h"

#define YUV_FORMAT_NAME	"yuv"


#define	RGB2Y(R,G,B)	(unsigned char) ( \
				(219.0 * ( \
				(0.299/255.0 * (R)) + \
				(0.587/255.0 * (G)) + \
				(0.114/255.0 * (B)) \
				)) + 16.0 + 0.5 \
			)

#define	RGB2U(R,G,B)	(unsigned char) ( \
				(224.0 * ( \
				(-0.1686/255.0 * (R)) + \
				(-0.3311/255.0 * (G)) + \
				(+0.4997/255.0 * (B)) \
				)) + 128.0 + 0.5 \
			)

#define	RGB2V(R,G,B)	(unsigned char) ( \
				(224.0 * ( \
				(+0.4998/255.0 * (R)) + \
				(-0.4185/255.0 * (G)) + \
				(-0.0813/255.0 * (B)) \
				)) + 128.0 + 0.5 \
			)

Raster	*YUVOpen(
	char	*name
) {
	Raster	*ras;

	(void) ESprintf(RAS_E_UNSUPPORTED_FUNCTION, "YUVOpen()");
	return(NULL);


	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(E_UNKNOWN, "ras_calloc(%d,1)", sizeof(Raster));
		return( (Raster *) NULL );
	}

	if (!strcmp(name, "stdin")) {
		ras->fd = fileno(stdin);
		ras->fp = stdin;
	}
	else {
		ras->fd  = open(name, O_RDONLY);
		if (ras->fd == -1) {
			(void) ESprintf(errno, "open(%s, O_RDONLY)", name);
			return( (Raster *) NULL );
		}

		ras->fp = fdopen(ras->fd, "r");
		if (ras->fp == (FILE *) NULL) {
			(void) ESprintf(errno, "fdopen(%d)", ras->fd, name);
			return( (Raster *) NULL );
		}
	}

	/* Record the name of the file. */

	ras->name = (char *) ras_calloc((unsigned) (strlen(name)+1), 1);
	(void) strcpy(ras->name, name);

	/* Record the format. */

	ras->format=(char *)ras_calloc((unsigned)(strlen(YUV_FORMAT_NAME)+1),1);
	(void) strcpy(ras->format, YUV_FORMAT_NAME);

	YUVSetFunctions(ras);

	return(ras);
}

int	YUVRead(
	Raster	*ras
) {
	int		status;
	int		y, length;
	unsigned char	*p;

	(void) ESprintf(RAS_E_UNSUPPORTED_FUNCTION, "YUVRead()");
	return(RAS_ERROR);

	if (OptionInX == 0 || OptionInY == 0) {
		(void) ESprintf(E_UNKNOWN,
			"YUV(\"%s\") - Input resolution not specified",
			ras->name);
		return(RAS_ERROR);
	}

	if (ras->read == False) {
		/* Set file-related variables. */
		ras->read	= True;
		ras->written	= False;
		ras->file_nx	= OptionInX;
		ras->file_ny	= OptionInY;

		ras->nx		= OptionInX;
		ras->ny		= OptionInY;


		if (OptionIndexed) {
			ras->file_type	= RAS_INDEXED;
			ras->type	= RAS_INDEXED;
			ras->length	= ras->nx * ras->ny;
			ras->ncolor	= 0;
		}
		else {
			ras->file_type	= RAS_DIRECT;
			ras->type	= RAS_DIRECT;
			ras->length	= ras->nx * ras->ny * 3;
			ras->ncolor	= 256*256*256;
		}

		/* Allocate image storage. */

		ras->data = (unsigned char *) ras_calloc(ras->length, 1);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, "ras_calloc(%d,1)", ras->length);
			return(RAS_ERROR);
		}

		if (OptionInOffset) {
			if (fseek(ras->fp,(long) OptionInOffset,SEEK_SET) < 0) {
				(void) ESprintf(
					errno, "fseek(,%d,)", OptionInOffset
				);
				return(RAS_ERROR);
			}
		}
	}

	if (OptionInInvert == False) { /* Don't invert the picture. */
		status = fread(ras->data, 1, ras->length, ras->fp);
		if (status == 0) {
			return(RAS_EOF);
		}
		else if (status != ras->length) {
			(void) ESprintf(errno, "fread(,1,%d,)", ras->length);
			return(RAS_ERROR);
		}
	}
	else {
		if (OptionIndexed) {
			(void) ESprintf(E_UNKNOWN,"Can't invert Indexed image");
			return(RAS_ERROR);
		}
		for(y=0; y<ras->ny; y++) {
			p = &DIRECT_RED(ras, 0, ras->ny-y-1);
			length = ras->nx * 3;
			status = fread((char *) p, 1, length, ras->fp);
			if (status == 0) {
				return(RAS_EOF);
			}
			else if (status != length) {
				(void) ESprintf(errno,
					"YUVRead(\"%s\" - Inverted)",
					ras->name);
				return(RAS_ERROR);
			}
		}
	}

	return(RAS_OK);
}

/* ARGSUSED */
Raster	*YUVOpenWrite(
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
) {
	Raster		*ras;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "File name not specified");
		return( (Raster *) NULL );
	}

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "ras_calloc(%d)", sizeof(Raster));
		return( (Raster *) NULL );
	}


	if (!strcmp(name, "stdout")) {
		ras->fp = stdout;
	}
	else {
		ras->fp = fopen(name, "w");

		if (! ras->fp) {
			(void) ESprintf(errno, "open(%s,,0664)", name);
			return( (Raster *) NULL );
		}
	}
	ras->fd = fileno(ras->fp);

	ras->name = (char *) ras_calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc(
			(unsigned) strlen(YUV_FORMAT_NAME) + 1, 1
			);
	(void) strcpy(ras->format, YUV_FORMAT_NAME);

	if (comment != (char *) NULL) {
		ras->text = (char *) ras_calloc(
			(unsigned) (strlen(comment) + 1),1
			);

		(void) strcpy(ras->text, comment);
	}
	else {
		ras->text = (char *) NULL;
	}

	ras->nx	= nx;
	ras->ny	= ny;

	switch (encoding) {
	case RAS_INDEXED:

		(void) ESprintf(
			RAS_E_UNSUPPORTED_ENCODING,
			"Only DIRECT color is supported for YUV rasterfiles"
		);
		return( (Raster *) NULL );

	case RAS_DIRECT:

		ras->type	= RAS_DIRECT;
		ras->length	= ras->nx * ras->ny * 3;
		ras->ncolor	= 256*256*256;
		ras->red	= (unsigned char *) NULL;
		ras->green	= (unsigned char *) NULL;
		ras->blue	= (unsigned char *) NULL;
		ras->data	= (unsigned char *) 
					ras_calloc((unsigned) ras->length, 1);
		break;

		default:
			(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,"");
			return( (Raster *) NULL );
	}

	YUVSetFunctions(ras);

	return(ras);
}

int	YUVWrite(
	Raster	*ras
) {
	int		nb;
	int		i;
	unsigned char	*yuvbuf = NULL;
	unsigned char	*yuvptr;
	int		yuvbuflen = 0;
	int		rc;
	int		x,y;

	yuvbuf = ras_calloc(ras->nx*2, 1);
	if (! yuvbuf) {
		(void) ESprintf(errno, "ras_calloc(%d,1)", ras->nx*2);
		return(RAS_ERROR);
	}
	yuvbuflen = 2 * ras->nx;

	if (ras->type == RAS_INDEXED) {
		unsigned char	ya[256], ua[256], va[256];
		unsigned char	*ciptr;	/* color index ptr	*/

		/*
		**	build a yuv lookup table
		*/
		for (i=0; i<ras->ncolor; i++) {

			ya[i] = RGB2Y(ras->red[i], ras->green[i], ras->blue[i]);
			ua[i] = RGB2U(ras->red[i], ras->green[i], ras->blue[i]);
			va[i] = RGB2V(ras->red[i], ras->green[i], ras->blue[i]);
		}

		for(y=0,ciptr=ras->data; y<ras->ny; y++) {
			for(x=0,yuvptr=yuvbuf; x<ras->nx; x++) {

				if (x % 2) {
					*yuvptr++ = va[*ciptr++];
				}
				else {
					*yuvptr++ = ua[*ciptr++];
				}
				*yuvptr++ = ya[*ciptr++];
			}

			rc = fwrite(yuvbuf, 1, yuvbuflen, ras->fp);
			if (rc != yuvbuflen) {
				(void) ESprintf(
					errno, 
					"fwrite(,1,%d,)", yuvbuflen
				);
			
				return(RAS_ERROR);
			}
		}
	}
	else {
		unsigned char	*rgb;

		for(y=0,rgb=ras->data; y<ras->ny; y++) {
			for(x=0,yuvptr=yuvbuf; x<ras->nx; x++) {

				if (x % 2) {
					*yuvptr++ = RGB2V(rgb[0],rgb[1],rgb[2]);
				}
				else {
					*yuvptr++ = RGB2U(rgb[0],rgb[1],rgb[2]);
				}
				*yuvptr++ = RGB2Y(rgb[0],rgb[1],rgb[2]);
				rgb+=3;
			}

			rc = fwrite(yuvbuf, 1, yuvbuflen, ras->fp);
			if (rc != yuvbuflen) {
				(void) ESprintf(
					errno, 
					"fwrite(,1,%d,)", yuvbuflen
				);
			
				return(RAS_ERROR);
			}
		}
	}

	return(RAS_OK);
}

/* ARGSUSED */
int	YUVPrintInfo(
	Raster		*ras
) {
	return(RAS_OK);
}

int	YUVClose(
	Raster	*ras
) {
	int	status;

	status = GenericClose(ras);
	return(status);
}

int	YUVSetFunctions(
	Raster	*ras
) {
	extern	int	ImageCount_();

	ras->Open      = YUVOpen;
	ras->OpenWrite = YUVOpenWrite;
	ras->Read      = YUVRead;
	ras->Write     = YUVWrite;
	ras->Close     = YUVClose;
	ras->PrintInfo = YUVPrintInfo;
	ras->ImageCount = ImageCount_;
	return(RAS_OK);
}
