/*
 *	$Id: hplj.c,v 1.9 1993-01-19 16:35:26 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	hplj.c
 *
 *	Author: John Clyne
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	3/5/91
 *
 *	Description:
 *		This file contains a collection of functions
 *		which provides access to a raster sequence
 *		using a general abstraction.
 *
 *		This particular set of routines provides
 *		basic file access functions for hplj (HP LaserJet) 
 *		files.
 *
 *		Encoding schemes are limited to:
 *			* 8-bit indexed	color with 8-bit color map values.
 *		
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include "ncarg_ras.h"
#include "options.h"
#include "hplj.h"


static char	*FormatName = "hplj";
static char	*Comment = "hplj file from NCAR raster utilities";


/*
 * determine the starting position for the raster image. i.e. the device
 * coordinate system. The LaserJet does internal pixel 
 * replication  whenever dpi is not the max value, 300. We need to remember
 * this when we calculate starting position of image.
 */
static	void	start_position(nx, ny, dpi, orientation, start_x, start_y)
	int	nx, ny;		/* dimension of image	*/
	int	dpi;		/* dots per inch	*/
	int	orientation;
	int	*start_x,	/* starting x coord in dpi	*/
		*start_y;	/* starting y coord in dpi	*/

{
	char	buf[80];
	char	*cptr;
	int	total_width;	/* width of page in dpi		*/
	int	total_height;	/* height of page in dpi	*/
	int	image_width;	/* actual width of image after pixel rep.  */
	int	image_height;	/* actual height of image after pixel rep. */

	total_width = HPLJ_PAPER_WIDTH * HPLJ_MAX_RES;
	total_height = HPLJ_PAPER_HEIGHT * HPLJ_MAX_RES;
	image_width = nx * HPLJ_MAX_RES / dpi;
	image_height = ny * HPLJ_MAX_RES / dpi;

	if (orientation == RAS_LANDSCAPE) {
		*start_y = (total_width - image_width) / 2;
		*start_x = ((total_height - image_height) / 2);
	}
	else {
		*start_y = (total_height - image_height) / 2;
		*start_x = (total_width - image_width) / 2;
	}
}

/*
 *	encode and write a single scanline
 *
 * on entry
 *	*fp		: file pointer to write to
 *	x,y		: starting position of the scanline
 *	*data		: formatted scanline
 *	data_len	: num bytes in data_len
 * on exit
 *	return		: 0 => ok, else failure
 */
scanline(fp, x, y, data, data_len)
	FILE		*fp;
	int		x;
	int		y;
	unsigned char	*data;
	int		data_len;
{
	unsigned char	buf[BUFSIZ];
	int		len;

	/*
	 * position the cursor
	 */
	sprintf((char *) buf, HPLJ_POSITION, x, y);
	len = strlen((char *) buf);
	if (fwrite((char *) buf, 1, len, fp) != len) {
		ESprintf(errno, "fwrite(,1,%d,)", len);
		return(-1);
	}

	/*
	 * Enter graphics mode
	 */
	sprintf((char *) buf, HPLJ_START);
	len = strlen((char *) buf);
	if (fwrite((char *) buf, 1, len, fp) != len) {
		ESprintf(errno, "fwrite(,1,%d,)", len);
		return(-1);
	}

	/*
	 * send the data stream header
	 */
	sprintf((char *) buf, HPLJ_TRANSFER, data_len);
	len = strlen((char *) buf);
	if (fwrite((char *) buf, 1, len, fp) != len) {
		ESprintf(errno, "fwrite(,1,%d,)", len);
		return(-1);
	}

	/*
	 * send the scan line
	 */
	if (fwrite((char *) data, 1, data_len, fp) != data_len) {
		ESprintf(errno, "fwrite(,1,%d,)", len);
		return(-1);
	}

	/*
	 * Exit graphics mode
	 */
	sprintf((char *) buf, HPLJ_END);
	len = strlen((char *) buf);
	if (fwrite((char *) buf, 1, len, fp) != len) {
		ESprintf(errno, "fwrite(,1,%d,)", len);
		return(-1);
	}

	return(0);
}

/*
 *	format and encode in portrait mode
 *
 *	encode the generic raster image into a compressed HP PCL raster
 *	format and write the resultant image out to disk
 */
static	int	write_compressed_port(ras)
	Raster	*ras;
{
	HPLJ_Info	*hplj = (HPLJ_Info *) ras->dep;	

	int		step = HPLJ_MAX_RES / hplj->dpi;
	int		ry, rx;
	int		hx, hy;
	int		run;	/* num contiguous "on" bits in a run	*/
	int		hx_;	/* first x coord set in a scan line	*/

	unsigned char	data[BUFSIZ];
	unsigned char	*ptr;
	int		data_len;
	unsigned char	mask;	/* byte-sized bit mask	*/


	/*
	 * scan convert each line in the generic image into PCL format. 
	 * Perform the necessary 8-bit to 1-bit dithering required by
	 * PCL format.
	 */
	for (ry=0, hy=hplj->start_y; ry<ras->ny; ry++, hy+=step) {

		/*
		 * find first set bit. We need to reset the position
		 * at the beginning of each scan line so we might as
		 * well skip any leading zeros. At the end of this
		 * loop rx and hx will be set appropriately for the first bit.
		 */
		for(rx=0, hx = hplj->start_x; rx<ras->nx; rx++, hx+= step) {
			if (INDEXED_PIXEL(ras, rx, ry)) {
				hx_ = hx;
				break;
			}
		}

		if (rx >= ras->nx) continue;	/* empty scan line	*/

		/*
		 * compress a single scan line from bytes into bits.
		 *
 		 * N.B. we're guaranteed to have at least one set bit
		 * in the scanline by the above conditional.
		 */
		bzero((char *) data, sizeof(data));
		for (mask=0x80,ptr=data,data_len=1; rx<ras->nx; rx++, hx+=step){

                        if (! mask) {
                                mask = 0x80;    /* reset mask   */
                                ptr++;          /* end of byte  */
				data_len++;
                        }
			if (INDEXED_PIXEL(ras, rx, ry)) {
                                *ptr |= mask;
			}
                        mask >>= 1;
                }

		if (scanline(ras->fp, hx_, hy, data, data_len) < 0) {
			return(-1);
		}

	}

	return(0);
}

/*
 *	format and encode in landscape mode
 *
 *	encode the generic raster image into a compressed HP PCL raster
 *	format and write the resultant image out to disk
 */
static	int	write_compressed_land(ras)
	Raster	*ras;
{
	HPLJ_Info	*hplj = (HPLJ_Info *) ras->dep;	

	int		step = HPLJ_MAX_RES / hplj->dpi;
	int		rx, ry;
	int		hy, hx;
	int		run;	/* num contiguous "on" bits in a run	*/
	int		hy_;	/* first y coord set in a scan line	*/

	unsigned char	data[BUFSIZ];
	unsigned char	*ptr;
	int		data_len;
	unsigned char	mask;	/* byte-sized bit mask	*/


	/*
	 * scan convert each line in the generic image into PCL format. 
	 * Perform the necessary 8-bit to 1-bit dithering required by
	 * PCL format.
	 */
	for (rx=0, hx=hplj->start_x; rx<ras->nx; rx++, hx+=step) {

		/*
		 * find first set bit. We need to reset the position
		 * at the beginning of each scan line so we might as
		 * well skip any leading zeros. At the end of this
		 * loop ry and hy will be set appropriately for the first bit.
		 */
		for(ry=0, hy = hplj->start_y; ry<ras->ny; ry++, hy+= step) {
			if (INDEXED_PIXEL(ras, rx, ry)) {
				hy_ = hy;
				break;
			}
		}

		if (ry >= ras->ny) continue;	/* empty scan line	*/

		/*
		 * compress a single scan line from by into bits.
		 *
 		 * N.B. we're guaranteed to have at least one set bit
		 * in the scanline by the above conditional.
		 */
		bzero((char *) data, sizeof(data));
		for (mask=0x80,ptr=data,data_len=1; ry<ras->ny; ry++, hy+=step){

                        if (! mask) {
                                mask = 0x80;    /* reset mask   */
                                ptr++;          /* end of byte  */
				data_len++;
                        }
			if (INDEXED_PIXEL(ras, rx, ry)) {
                                *ptr |= mask;
			}
                        mask >>= 1;
                }

		if (scanline(ras->fp, hx, hy_, data, data_len) < 0) {
			return(-1);
		}

	}

	return(0);
}

/*ARGSUSED*/
Raster *
HPLJOpen(name)
	char	*name;
{
	(void) ESprintf(RAS_E_UNSUPPORTED_FUNCTION, "HPLJOpen()");
	return((Raster *) NULL);
}

int
HPLJPrintInfo(ras)
	Raster	*ras;
{
	HPLJ_Info	*hplj = (HPLJ_Info *) ras->dep;

	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "HPLJ Rasterfile Information\n");
	(void) fprintf(stderr, "--------------------------\n");

	if (hplj->orientation ==  RAS_LANDSCAPE) 
		(void) fprintf(stderr, "orientation:	landscape\n");
	else 
		(void) fprintf(stderr, "orientation:	portrait\n");

	/*
	 * print out encoding info here if we knew it
	 */

	(void) fprintf(stderr, "row length:	%d\n", hplj->row_size);
	(void) fprintf(stderr, "resolution:	%ddpi\n", hplj->dpi);
	
}


/*ARGSUSED*/
Raster *
HPLJOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
{
	Raster		*ras;
	HPLJ_Info	*hplj;
	int		dpi;

	if (name == (char *) NULL) {
		(void) ESprintf(RAS_E_NULL_NAME, "HPLJOpenWrite()");
		return( (Raster *) NULL );
	}

	if (encoding != RAS_INDEXED) {
		(void) ESprintf(RAS_E_UNSUPPORTED_ENCODING,
			"Only INDEXED is supported for HPLJ");
		return( (Raster *) NULL );
	}

	dpi = OptionDotsPerInch;

	if ( !(dpi == 75 || dpi == 100 || dpi == 150 || dpi == 300)) {
		(void) ESprintf(RAS_E_UNSUPPORTED_RESOLUTION,
			"DPI options for HP LaserJet are (75,100,150,300)");
		return( (Raster *) NULL );
	}
		

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);

	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, "HPLJOpenWrite()");
		return( (Raster *) NULL );
	}

	ras->dep = (char *) ras_calloc(sizeof(HPLJ_Info),1);

	if (ras->dep == (char *) NULL) {
		(void) ESprintf(errno, "HPLJOpenWrite()");
		ras_free(ras);
		return( (Raster *) NULL );
	}

	hplj = (HPLJ_Info *) ras->dep;

	hplj->do_compress = OptionCompression;	/* currently ignored	*/
	hplj->orientation = OptionOrientation;
	hplj->dpi = dpi;


	/* 
	 * find starting position
	 */
	start_position(
		nx,ny,dpi,hplj->orientation,&(hplj->start_x), &(hplj->start_y)
	);

	if (!strcmp(name, "stdout")) {
		ras->fp = stdout;
	}
	else {
		ras->fp = fopen(name, "w");

		if (ras->fp == NULL) {
			(void) ESprintf(errno, "HPLJOpenWrite(\"%s\")",name);
			return( (Raster *) NULL );
		}
	}
	ras->fd = fileno(ras->fp);

	ras->name = (char *) ras_calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	ras->text = Comment;

	ras->nx	= nx;
	ras->ny	= ny;
	ras->length	= ras->nx * ras->ny;
	ras->ncolor	= 256;
	ras->type	= RAS_INDEXED;
	ras->red	= (unsigned char *)ras_calloc((unsigned)ras->ncolor, 1);
	ras->green	= (unsigned char *)ras_calloc((unsigned)ras->ncolor, 1);
	ras->blue	= (unsigned char *)ras_calloc((unsigned)ras->ncolor, 1);
	ras->data	= (unsigned char *)ras_calloc((unsigned)ras->length, 1);
	ras->type 	= encoding;


	HPLJSetFunctions(ras);

	return(ras);
}

int
HPLJWrite(ras)
	Raster	*ras;
{
	char		*errmsg = "HPLJWrite(\"%s\")";
	int		nb;
	int		i,j;

	HPLJ_Info	*hplj = (HPLJ_Info *) ras->dep;	

	unsigned char	buf[BUFSIZ];
	int		len;

	if (ras == (Raster *) NULL) {
		(void) ESprintf(RAS_E_BOGUS_RASTER_STRUCTURE,errmsg,ras->name);
		return(RAS_ERROR);
	}

	/*
	 * reset the printer
	 */
	sprintf((char *) buf, HPLJ_RESET);
	len = strlen((char *) buf);
	if (fwrite((char *) buf, 1, len, ras->fp) != len) {
		ESprintf(errno, "fwrite(,1,%d,)", len);
		return(-1);
	}

	/*
	 * set the orientation (portrait or landscape)
	 */
	if (hplj->orientation == RAS_LANDSCAPE) {
		sprintf((char *) buf, HPLJ_LANDSCAPE);
	}
	else {
		sprintf((char *) buf, HPLJ_PORTRAIT);
	}
	len = strlen((char *) buf);
	if (fwrite((char *) buf, 1, len, ras->fp) != len) {
		ESprintf(errno, "fwrite(,1,%d,)", len);
		return(-1);
	}

	/*
	 * set the resolution in dots per inch
	 */
	sprintf((char *) buf, HPLJ_RESOLUTION, hplj->dpi);
	len = strlen((char *) buf);
	if (fwrite((char *) buf, 1, len, ras->fp) != len) {
		ESprintf(errno, "fwrite(,1,%d,)", len);
		return(-1);
	}


	/*
	 * encode and write the image
	 */
	if (hplj->orientation ==  RAS_LANDSCAPE) {
		if (write_compressed_land(ras) < 0) {
			return(RAS_ERROR);
		}
	}
	else {
		if (write_compressed_port(ras) < 0) {
			return(RAS_ERROR);
		}
	}

	/*
 	 * Eject page
	 */
	sprintf((char *) buf, HPLJ_EJECT);
	len = strlen((char *) buf);
	if (fwrite((char *) buf, 1, len, ras->fp) != len) {
		ESprintf(errno, "fwrite(,1,%d,)", len);
		return(-1);
	}

	return(RAS_OK);
}

int
HPLJClose(ras)
	Raster	*ras;
{
	int		status;
	HPLJ_Info	*hplj = (HPLJ_Info *) ras->dep;

	if (hplj) {
		ras_free(hplj);
	}

	status = GenericClose(ras);
	return(status);
}

/*ARGSUSED*/
int	HPLJRead(ras)
	Raster	*ras;
{
	(void) ESprintf(RAS_E_UNSUPPORTED_FUNCTION, "HPLJRead()");
	return(RAS_ERROR);
}

int
HPLJSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open        = HPLJOpen;
	ras->OpenWrite   = HPLJOpenWrite;
	ras->Read        = HPLJRead;
	ras->Write       = HPLJWrite;
	ras->Close       = HPLJClose;
	ras->PrintInfo   = HPLJPrintInfo;
	ras->ImageCount = ImageCount_;
}

