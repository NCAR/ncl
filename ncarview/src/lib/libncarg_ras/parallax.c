/*
 *	$Id: parallax.c,v 1.8 1992-03-23 21:45:48 clyne Exp $
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
 *		This file is part of a library which provides
 *		access to a raster files and memory raster objects
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
 *			be supplied with a raster structure 
 *			that is either indexed or true.
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

static int	parallax_fd	= -1;
static int	parallax_vmefd	= -1;
static Parallax	*parallax;

/**********************************************************************
 *	Function: ParallaxOpen(name)
 *
 *	Description:
 *		Opens the Parallax graphics buffer for reading
 *		(real-time video digitizing capability).
 *
 *		This library assumes that there is only one
 *		Parallax video frame buffer in the system.
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
	(void) RasterSetError(RAS_E_UNSUPPORTED_FUNCTIONS);
	return( (Raster *) NULL );
}


/**********************************************************************
 *	Function: ParallaxOpenWrite(name, nx, ny, comment, encoding)
 *
 *	Description:
 *		Opens the Parallax graphics buffer for writing.
 *		"name" is recorded but unused, as are "nx", "ny", 
 *		"comment", and "encoding". The Parallax board is 
 *		a fixed size and is a true color device.
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
	int		status;
	Raster		*RasterCreate();
	char		*calloc();

	/* Allocate the Raster structure and load some symbols. */

	ras = RasterCreate(nx, ny, encoding);

	ras->name = (char *) calloc((unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, FormatName);

	ras->format = (char *) calloc((unsigned) (strlen(FormatName) + 1), 1);
	(void) strcpy(ras->format, FormatName);

	ras->text = (char *) calloc((unsigned) (strlen(comment) + 1), 1);
	(void) strcpy(ras->text, comment);

	/* Initialize the Parallax frame buffer board. */
	status = ParallaxInit();
	if (status != RAS_OK) {
		return( (Raster *) NULL );
	}

	/* Load friendly functions. */
	(void) ParallaxSetFunctions(ras);

	return(ras);
}

/**********************************************************************
 *	Function: ParallaxWrite(ras)
 *
 *	Description:
 *		Copies the image contents of "ras" to the
 *		Parallax frame buffer. The "ras" object does
 *		not *have* to be one created by doing a
 *		ParallaxOpenWrite(). Both indexed color
 *		and true color objects are acceptable,
 *		and oversize/undersized imagery is adjusted
 *		to fit the frame buffer by centering or
 *		cropping.
 *
 *	Returns:
 *		RAS_OK		If all is well
 *		RAS_ERROR	Otherwise
 *
 *	Notes:
 *		There's no byte-swapping code 'cause this thing ONLY
 *		runs on Sun's. hahahahahahahaha
 *		
 *		
 *********************************************************************/
int
ParallaxWrite(ras)
	Raster	*ras;
{
	int		sx, sy, dy;		/* source and dest indices */
	int		src_x, src_y;		/* source upper-left corner */
	int		src_nx, src_ny;		/* source extent */
	int		dst_x, dst_y;		/* dest upper-left corner */
	int		vfb_width, vfb_height;	/* video frame buffer extent */
	int		i;
	static long	colormap_lwd[256];
	unsigned char	*fb_ptr;
	unsigned long	*fb_lptr;
	unsigned char	*rgb_ptr;

	/* Take into account the vertical interval in the video buffer. */

	vfb_width = VFB_WIDTH;
	vfb_height = VFB_HEIGHT - VFB_Y_OFFSET;

	/*
	For indexed color raster objects, precompute Parallax-compatible
	frame buffer entries.
	*/

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

	/*
	In order to avoid repetitive and costly address arithmetic,
	this loop works with pointers more than would be ideal.
	*/

	for(sy=src_y, dy=dst_y + VFB_Y_OFFSET; sy<src_y+src_ny; sy++, dy++) {
		if (ras->type == RAS_INDEXED) {
			rgb_ptr = &INDEXED_PIXEL(ras, src_x, sy);
			fb_lptr = &parallax->fb.line[dy].pixel[dst_x].lwd;
		}
		else if (ras->type == RAS_DIRECT) {
			rgb_ptr = &DIRECT_RED(ras, src_x, sy);
			fb_ptr = (unsigned char *) 
				 &parallax->fb.line[dy].pixel[dst_x].lwd;
		}
		for(sx=src_x; sx<src_x+src_nx; sx++) {
			if (ras->type  == RAS_INDEXED) {
				*fb_lptr++ = colormap_lwd[*rgb_ptr++];
			}
			else if (ras->type == RAS_DIRECT) {
				*(fb_ptr+3) = *rgb_ptr++;
				*(fb_ptr+2) = *rgb_ptr++;
				*(fb_ptr+1) = *rgb_ptr++;
				fb_ptr += 4;
			}
		}
	}

	return(RAS_OK);
}

int
ParallaxPrintInfo(ras)
	Raster		*ras;
{

	if (ras->text != (char *) NULL) {
		(void) fprintf(stderr, "Parallax Framebuffer Information\n");
		(void) fprintf(stderr, "--------------------------------\n");
		(void) fprintf(stderr, "text: %s\n", ras->text);
	}
	return(RAS_OK);
}

int
ParallaxRead(ras)
	Raster	*ras;
{
	(void) RasterSetError(RAS_E_UNSUPPORTED_FUNCTIONS);
	return(RAS_ERROR);
}

int
ParallaxClose(ras)
	Raster	*ras;
{
	parallax_fd	= -1;
	parallax_vmefd	= -1;

	if (ras != (Raster *) NULL) {
		if (ras->data != (unsigned char *) NULL) {
			free( (char *) ras->data);
		}
	}

	if (ras->red   != (unsigned char *) NULL) free( (char *) ras->red);
	if (ras->green != (unsigned char *) NULL) free( (char *) ras->green);
	if (ras->blue  != (unsigned char *) NULL) free( (char *) ras->blue);

	return(RAS_OK);
}

int
ParallaxSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open      = ParallaxOpen;
	ras->OpenWrite = ParallaxOpenWrite;
	ras->Read      = ParallaxRead;
	ras->Write     = ParallaxWrite;
	ras->Close     = ParallaxClose;
	ras->PrintInfo = ParallaxPrintInfo;
	ras->ImageCount = ImageCount_;
	return(RAS_OK);
}

/********************************************************************
 *
 * Functions below this line are private to this file.
 *
 *******************************************************************/

/********************************************************************
 *
 *	Function:	ParallaxInit()
 *
 *	Description:	This function initializes the Parallax
 *			VideoView frame buffer. The steps
 *			taken prepare the board for general
 *			ioctl() manipulation, as well as
 *			reading and writing. Special 
 *			synchronization steps must be taken
 *			in order to digitize incoming video
 *			signals.
 *
 *	Returns:	RAS_OK		if all's well
 *			RAS_ERROR	if not
 *
 *	
 ******************************************************************/
static int
ParallaxInit()
{
	static int	arg;
	int		status;
	caddr_t		mmap();

	/*
	Open the video frame buffer device. This file
	descriptor is used in ioctl() call's, but not
	for reading and writing, which is accomplished
	through direct memory-mapped I/O.
	*/

	/* Open the Parallax frame buffer device. */

	parallax_fd = open(VFB_DEVICE, O_RDWR);
	if (parallax_fd == -1) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( RAS_ERROR );
	}
	
	/* Open the VME bus for memory mapped I/O. */

	if (parallax_vmefd != -1) {
		(void) RasterSetError(RAS_E_UNSUPPORTED_FUNCTIONS);
		return(RAS_ERROR);
	}

	if ( (parallax_vmefd = open(VFB_VME_DEVICE, O_RDWR)) == -1) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( RAS_ERROR );
	}

	/* Map the Parallax structure into virtual memory. */

	parallax = (Parallax *) mmap( (caddr_t) 0, 0x400000, 
		PROT_READ | PROT_WRITE, MAP_SHARED, parallax_vmefd, 
		(off_t) VFB_VME_ADDRESS);

	if ( (int) parallax == -1) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( RAS_ERROR );
	}

	/* Sync the frame buffer */
	arg = 0;
	status = ioctl(parallax_fd, TVIOSYNC, &arg);
	if ( status == -1) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return( RAS_ERROR );
	}

	/* Input video will be NTSC composite video. */
	arg = TVIO_NTSC;
	status = ioctl(parallax_fd, TVIOSFORMAT, &arg);
	if ( status == -1) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return( RAS_ERROR );
	}

	/* Set compression ratio to be 1:1 */
	arg = 1;
	status = ioctl(parallax_fd, TVIOSCOMPRESS, &arg);
	if ( status == -1) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return( RAS_ERROR );
	}

	/* Direct video to video monitor, rather than to Sun screen. */
	arg = TVIO_VIDEOOUT;
	status = ioctl(parallax_fd, TVIOSOUT, &arg);
	if ( status == -1) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return( RAS_ERROR );
	}

	/* Output is RS-170A (component RGBS), not YUV (Betacam). */
	arg = TVIO_RGB;
	status = ioctl(parallax_fd, TVIOSCOMPOUT, &arg);
	if ( status == -1) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return( RAS_ERROR );
	}

	/* Set Genlock on i.e. sync to incoming blackburst signal. */
	arg = TRUE;
	status = ioctl(parallax_fd, TVIOSGENLOCK, &arg);
	if ( status == -1) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return( RAS_ERROR );
	}

	/* Set Genlock to sync on composite sync input. */
	arg = TVIO_NTSC;
	status = ioctl(parallax_fd, TVIOSSYNC, &arg);
	if ( status == -1) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return( RAS_ERROR );
	}

	/* Set Chroma Demodulation to automatic. */
	arg = TVIO_AUTO;
	status = ioctl(parallax_fd, TVIOSSYNC, &arg);
	if ( status == -1) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return( RAS_ERROR );
	}

	/* Sync the frame buffer */
	arg = 0;
	status = ioctl(parallax_fd, TVIOSYNC, &arg);
	if ( status == -1) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return( RAS_ERROR );
	}

	status = ParallaxClear();
	if (status != RAS_OK) {
		(void) RasterSetError(RAS_E_PARALLAX);
		return(RAS_ERROR);
	}

	return(RAS_OK);
}

int
ParallaxClear()
{
	int		dx, dy;
	unsigned long	*fb_lptr;

	for(dy=0; dy<VFB_HEIGHT; dy++) {
		fb_lptr = &parallax->fb.line[dy].pixel[0].lwd;
		for(dx=0; dx<VFB_WIDTH; dx++) {
			*fb_lptr++ = 0l;
		}
	}
	return(RAS_OK);
}

ParallaxPrintStatus()
{
	static int	arg;
	static int	format;
	static int	out;
	static int	compout;
	int		status;

	if ( (status = ioctl(parallax_fd, TVIOGFORMAT, &format)) == -1) {
		perror("TVIO Get Format");
	}

	if ( (status = ioctl(parallax_fd, TVIOGOUT, &out)) == -1) {
		perror("TVIO Get Video Destination");
	}

	status = ioctl(parallax_fd, TVIOGCOMPOUT, &compout);
	if ( status == -1) {
		perror("TVIO Get Component Out");
	}

	(void) fprintf(stderr,"Input Format:      %s\n", format_names[format]);
	(void) fprintf(stderr,"Video Destination: %s\n", out_names[out]);
	(void) fprintf(stderr,"Component Out:     %s\n", format_names[compout]);
}
