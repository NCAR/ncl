/*
 *	$Id: parallax.c,v 1.2 1991-08-16 11:13:35 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	parallax.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	1/31/91
 *
 *	Description:
 *		This file contains a collection of functions
 *		which provides access to a raster sequence
 *		using a general abstraction.
 *
 *		This particular set of routines provides
 *		basic access functions for the Parallax video
 *		frame buffer. The buffer has a fixed size
 *		and can be read or written to. Option settings
 *		determine how the buffer is treated relative
 *		to raster structures that aren't just the
 *		proper size.
 *
 *		Encoding schemes:
 *			The Parallax video frame buffer is
 *			a true color device but it may
 *			be fed a raster structure that is
 *			either indexed or true.
 *		
 */
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <pixrect/pixrect_hs.h>
#include <errno.h>
#include <pixrect/tv1var.h>
#include <string.h>
#include "/sys/sun/tvio.h"
#include "ncarg_ras.h"
#include "parallax.h"

static char	*FormatName = "parallax";
static int	OptionCenter = True;

static int	fd;
static Parallax	*pvp;
static int	vmefd = -1;

int
ParallaxProbe(name)
	char	*name;
{
	return(RAS_OK);
}

/**********************************************************************
 *	Function: ParallaxOpen(name)
 *
 *	Description:
 *		Opens the Parallax graphics buffer. "name" is
 *		ignored.
 *
 *	Returns:
 *		Pointer to Raster structure, to be used
 *		subsequently by other image access routines.
 *		
 *********************************************************************/
Raster *
ParallaxOpen(name)
	char	*name;
{
	Raster	*ras;
	char	*calloc();

	/* Allocate the Raster structure. */

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	/* 
	Argument "name" is ignored. This library assumes one
	Parallax board in the system.
	*/

	ras->name = (char *) calloc((unsigned) (strlen(name)+1), 1);
	(void) strcpy(ras->name, FormatName);

	ras->format = (char *) calloc((unsigned) (strlen(FormatName) + 1), 1);
	(void) strcpy(ras->format, FormatName);

	ParallaxSetFunctions(ras);

	return(ras);
}


/**********************************************************************
 *	Function: ParallaxOpenWrite(name, nx, ny, comment, encoding)
 *
 *	Description:
 *		Opens the Parallax graphics buffer for writing.
 *		"name" is ignored, as are "nx", "ny", "comment",
 *		and "encoding". The Parallax board is a fixed size
 *		and is a true color device.
 *
 *	Returns:
 *		Pointer to Raster structure, to be used
 *		subsequently by other image access routines.
 *		
 *********************************************************************/
Raster *
ParallaxOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	int		encoding;
{
	Raster		*ras;

	/* Allocate the Raster structure. */

	ras = (Raster *) calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	ras->dep = (char *) NULL;

	/*
	Open the video frame buffer device. This file
	descriptor is used for all functions except read
	and write.
	*/

	ras->fd = open(VFB_DEVICE, O_RDWR);
	if (ras->fd == -1) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}
	
	/* Open the VME bus for memory mapped I/O. */

	if (vmefd > 0) return(0);

	if ( (vmefd = open(VFB_VME_DEVICE, O_RDWR)) == -1) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	/* Map the Parallax structure into virtual memory. */

	pvp = (Parallax *) mmap( (caddr_t) 0, 0x400000, PROT_READ|PROT_WRITE,
		MAP_SHARED, vmefd, VFB_VME_ADDRESS);

	if (pvp < (Parallax *) 0) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	ras->name = (char *) calloc((unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, FormatName);

	ras->format = (char *) calloc((unsigned) (strlen(FormatName) + 1), 1);
	(void) strcpy(ras->format, FormatName);

	if (encoding == RAS_INDEXED) {
		ras->type	= RAS_INDEXED;
		ras->nx		= VFB_WIDTH;
		ras->ny		= VFB_HEIGHT;
		ras->length	= ras->nx * ras->ny;
		ras->ncolor	= 256;
		ras->red = (unsigned char *) calloc((unsigned) ras->ncolor, 1);
		ras->green = (unsigned char *) calloc((unsigned)ras->ncolor, 1);
		ras->blue = (unsigned char *) calloc((unsigned) ras->ncolor, 1);
		ras->data = (unsigned char *) calloc((unsigned) ras->length, 1);
	}
	else if (encoding == RAS_DIRECT) {
		ras->type	= RAS_DIRECT;
		ras->nx		= VFB_WIDTH;
		ras->ny		= VFB_HEIGHT;
		ras->length	= ras->nx * ras->ny * 3;
		ras->ncolor	= 0;
		ras->red	= (unsigned char *) NULL;
		ras->green	= (unsigned char *) NULL;
		ras->blue	= (unsigned char *) NULL;
		ras->data = (unsigned char *) calloc((unsigned) ras->length, 1);
	}
	else {
		(void) RasterSetError(RAS_E_UNSUPPORTED_ENCODING);
		return( (Raster *) NULL );
	}

	(void) ParallaxSetFunctions(ras);

	return(ras);
}

#ifdef DEAD
int
ParallaxWrite(ras)
	Raster	*ras;
{
	ParallaxInfo	*dep;
	int		nb;
	unsigned long	swaptest = 1;

	/* Swap bytes if necessary. */

	if (*(char *) &swaptest)
		_swaplong((char *) dep, SUN_HEADER_SIZE);

	nb = write(ras->fd, (char *) ras->dep, SUN_HEADER_SIZE);
	if (nb != SUN_HEADER_SIZE) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->red, ras->ncolor);
	if (nb != ras->ncolor) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->green, ras->ncolor);
	if (nb != ras->ncolor) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->blue, ras->ncolor);
	if (nb != ras->ncolor) return(RAS_EOF);

	nb = write(ras->fd, (char *) ras->data, ras->nx * ras->ny);
	if (nb != ras->nx * ras->ny) return(RAS_EOF);

	return(RAS_OK);
}
#endif DEAD

ParallaxWrite(ras)
	Raster	*ras;
{
	Raster		*temp;
	int		status;
	long		word;
	int		sx, sy, dx, dy;		/* source and dest indices */
	int		src_x, src_y;		/* source upper-left corner */
	int		src_nx, src_ny;		/* source extent */
	int		dst_x, dst_y;		/* dest upper-left corner */
	int		vfb_width, vfb_height;	/* video frame buffer extent */
	int		i, r, g, b, pixel;
	static long	colormap_lwd[256];

	vfb_width = VFB_WIDTH;
	vfb_height = VFB_HEIGHT - VFB_Y_OFFSET;

	if (ras->type  == RAS_INDEXED) {
		for (i=0; i<256; i++)
			colormap_lwd[i] = 
				ras->red[i] | 
				ras->green[i]<<8 | 
				ras->blue[i]<<16;
	}

	/* Set defaults for image positioning. */

	src_x = 0; src_y = VFB_Y_OFFSET;
	src_nx = ras->nx; src_ny = ras->ny;
	dst_x = 0; dst_y = 0;

	/* Calculate X mapping */

	if (ras->nx > vfb_width) {
		dst_x = 0;
		src_nx = vfb_width;
		if (OptionCenter)
			src_x = (ras->nx - vfb_width) / 2;
		else
			src_x = 0;
	}
	else {
		src_x = 0;
		src_nx = ras->nx;
		if (OptionCenter)
			dst_x = (vfb_width - src_nx) / 2;
		else
			dst_x = 0;
	}

	/* Calculate Y mapping */

	if (ras->ny >= vfb_height) {
		dst_y = 0;
		src_ny = vfb_height;
		if (OptionCenter)
			src_y = (ras->ny - vfb_height) / 2;
		else
			src_y = 0;
	}
	else {
		src_y = 0;
		src_ny = ras->ny;
		if (OptionCenter)
			dst_y = (vfb_height - src_ny) / 2;
		else
			dst_y = 0;
	}

	for(sy=src_y, dy=dst_y + VFB_Y_OFFSET; sy<src_y+src_ny-1; sy++, dy++) {
		for(sx=src_x, dx=dst_x; sx<src_x+src_nx-1; sx++, dx++) {
			if (ras->type  == RAS_INDEXED) {
			  pixel  = INDEXED_PIXEL(ras, sx, sy);
			  pvp->fb.line[dy].pixel[dx].lwd = 
			    colormap_lwd[pixel];
			}
			else if (ras->type == RAS_DIRECT) {
			  pvp->fb.line[dy].pixel[dx].lwd =
				DIRECT_RED(ras, sx, sy) | 
				DIRECT_GREEN(ras, sx, sy) <<8 | 
				DIRECT_BLUE(ras, sx, sy) << 16;
			}
		}
	}
	return(RAS_OK);
}

int
ParallaxPrintInfo(ras)
	Raster		*ras;
{
	return(RAS_OK);
}

int
ParallaxRead(ras)
	Raster	*ras;
{
	return(RAS_OK);
}

int
ParallaxClose(ras)
	Raster	*ras;
{
	free( (char *) ras->data);
	if (ras->red != (unsigned char *) NULL) free( (char *) ras->red);
	if (ras->green != (unsigned char *) NULL) free( (char *) ras->green);
	if (ras->blue != (unsigned char *) NULL) free( (char *) ras->blue);
	return(RAS_OK);
}

int
ParallaxSetFunctions(ras)
	Raster	*ras;
{
	ras->Open      = ParallaxOpen;
	ras->OpenWrite = ParallaxOpenWrite;
	ras->Read      = ParallaxRead;
	ras->Write     = ParallaxWrite;
	ras->Close     = ParallaxClose;
	ras->PrintInfo = ParallaxPrintInfo;
	return(RAS_OK);
}
