/*
 *	$Id: hplj.c,v 1.2 1991-08-16 11:13:11 clyne Exp $
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
#include <fcntl.h>
#include <sys/types.h>
#include "ncarg_ras.h"
#include "options.h"
#include "hplj.h"


static char	*FormatName = "hplj";
static char	*Comment = "hplj file from NCAR raster utilities";

extern	char	*malloc(), *calloc(), *strcpy();


/*ARGSUSED*/
Raster *
HPLJOpen(name)
	char	*name;
{
	(void) RasterSetError(RAS_E_UNSUPPORTED_FUNCTIONS);
	return((Raster *) NULL);
}

int
HPLJPrintInfo(ras)
	Raster	*ras;
{
	HPLJ_Info	*dep = (HPLJ_Info *) ras->dep;
	HPLJ_Ras	*rdep = (HPLJ_Ras *) &(dep->ras);	

	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "HPLJ Rasterfile Information\n");
	(void) fprintf(stderr, "--------------------------\n");

	if (strcmp(rdep->orientation, HPLJ_LANDSCAPE) == 0)
		(void) fprintf(stderr, "orientation:	landscape\n");
	else 
		(void) fprintf(stderr, "orientation:	portrait\n");

	/*
	 * print out encoding info here if we knew it
	 */
#ifdef	DEAD
#endif

	(void) fprintf(stderr, "row length:	%d\n", dep->row_size);
	(void) fprintf(stderr, "resolution:	%ddpi\n", dep->dpi);
	
}


/*ARGSUSED*/
Raster *
HPLJOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	int		encoding;
{
	Raster		*ras;
	HPLJ_Info	*dep;
	HPLJ_Ras	*rdep;
	int		dpi;
	int	orientation;

	char	*position();

	if (name == (char *) NULL) {
		(void) RasterSetError(RAS_E_NULL_NAME);
		return( (Raster *) NULL );
	}

	dpi = OptionDotsPerInch;
	orientation = OptionOrientation;

	if ( !(dpi == 75 || dpi == 100 || dpi == 150 || dpi == 300)) {
		(void) RasterSetError(RAS_E_UNSUPPORTED_RESOLUTION);
		return( (Raster *) NULL );
	}
		

	ras = (Raster *) calloc(sizeof(Raster), 1);

	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	ras->dep = (char *) calloc(sizeof(HPLJ_Info),1);

	if (ras->dep == (char *) NULL) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	dep = (HPLJ_Info *) ras->dep;
	rdep = (HPLJ_Ras *) &(dep->ras);


	/*
	 * 	set up HPLJ_Info struct
	 */
	dep->dpi = dpi;

	rdep->reset = malloc((unsigned) strlen(HPLJ_RESET) + 1);
	(void) strcpy(rdep->reset, HPLJ_RESET);
	
	if (orientation == RAS_LANDSCAPE) { /* landscape or portrait mode? */
		rdep->orientation = malloc((unsigned) strlen(HPLJ_LANDSCAPE)+1);
		(void) strcpy(rdep->orientation, HPLJ_LANDSCAPE);
	} 
	else {
		rdep->orientation = malloc((unsigned) strlen(HPLJ_PORTRAIT) +1);
		(void) strcpy(rdep->orientation, HPLJ_PORTRAIT);

	}

	rdep->encoding = malloc((unsigned) strlen(HPLJ_ENCODING) + 1);
	(void) strcpy(rdep->encoding, HPLJ_ENCODING);

	/* 
	 * find starting position
	 */
	rdep->position = position(nx, ny, dpi, orientation);

	rdep->start_graph = malloc((unsigned) strlen(HPLJ_START) + 1);
	(void) strcpy(rdep->start_graph, HPLJ_START);

	if (create_data_space(dep, nx, ny) < 0) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return( (Raster *) NULL );
	}

	rdep->end_graph = malloc((unsigned) strlen(HPLJ_END) + 1);
	(void) strcpy(rdep->end_graph, HPLJ_END);
	
	rdep->eject = malloc((unsigned) strlen(HPLJ_EJECT) + 1);
	(void) strcpy(rdep->eject, HPLJ_EJECT);

	if (!strcmp(name, "stdout")) {
		ras->fd = fileno(stdout);
	}
	else {
		ras->fd = open(name, O_WRONLY | O_CREAT, 0644);

		if (ras->fd == -1) {
			(void) RasterSetError(RAS_E_SYSTEM);
			return( (Raster *) NULL );
		}
	}

	ras->name = (char *) calloc((unsigned) strlen(name) + 1, 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) calloc((unsigned) strlen(FormatName) + 1, 1);
	(void) strcpy(ras->format, FormatName);

	ras->nx	= nx;
	ras->ny	= ny;
	ras->length	= ras->nx * ras->ny;
	ras->ncolor	= 256;
	ras->type	= RAS_INDEXED;
	ras->red	= (unsigned char *) calloc((unsigned) ras->ncolor, 1);
	ras->green	= (unsigned char *) calloc((unsigned) ras->ncolor, 1);
	ras->blue	= (unsigned char *) calloc((unsigned) ras->ncolor, 1);
	ras->data	= (unsigned char *) calloc((unsigned) ras->length, 1);

	if (encoding != RAS_INDEXED) {
		(void) RasterSetError(RAS_E_UNSUPPORTED_ENCODING);
		return( (Raster *) NULL );
	}
	else {
		ras->type = RAS_INDEXED;
	}

	HPLJSetFunctions(ras);

	return(ras);
}

int
HPLJWrite(ras)
	Raster	*ras;
{
	int		nb;
	int		i,j;

	HPLJ_Info	*dep = (HPLJ_Info *) ras->dep;	
	HPLJ_Ras	*rdep = (HPLJ_Ras *) &(dep->ras);	
	unsigned char	mask;
	unsigned char	*lras,	/* pointer to data in ras->data	*/
			*hp;	/* pointer to data in ras->dep->ras.data*/
	unsigned char	*ptr;
	int		len;	/* length of dep->trans_data		*/

	if (ras == (Raster *) NULL) {
		(void) RasterSetError(RAS_E_BOGUS_RASTER_STRUCTURE);
		return(RAS_ERROR);
	}

	/*
	 * crunch 8-bit per pixel image into bi-level image. Load HP
	 * "Transfer Raster Data" instruction while we're at it
	 */
	len = strlen(dep->trans_data);
	lras = ras->data;
	hp = rdep->data;
	for (i=0; i < ras->ny; i++) {
		mask = 0x80;
		/* 
		 * insert control string
		 */
		bcopy(dep->trans_data, (char*) hp, len); 
		ptr = hp + len;		/* begining data for this row	*/
		
		/*
		 * load a row of data with bits
		 */
		for (j=0; j < ras->nx; j++) {
			if (! mask) {
				mask = 0x80;	/* reset mask	*/
				ptr++;		/* end of byte	*/
			}
			if (*lras) 
				*ptr |= mask;
			mask >>= 1;
			lras++;
		}
		hp += dep->row_size;	/* skip to next row	*/
	}

	/*
	 * reset the printer
	 */
	nb = write(ras->fd, (char *) rdep->reset, strlen(rdep->reset));
	if (nb != strlen(rdep->reset)) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}

	/*
	 * set orientation (landscape or portrait
	 */
	nb = write(ras->fd,(char *)rdep->orientation,strlen(rdep->orientation));
	if (nb != strlen(rdep->orientation)) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}

	/*
 	 * data encoding format
	 */
	nb = write(ras->fd, (char *) rdep->encoding, strlen(rdep->encoding));
	if (nb != strlen(rdep->encoding)) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}

	/*
 	 * position of upper right corner of image
	 */
	nb = write(ras->fd, (char *) rdep->position, strlen(rdep->position));
	if (nb != strlen(rdep->position)) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}

	/*
 	 * put printer in raster graphics mode
	 */
	nb = write(ras->fd,(char *)rdep->start_graph,strlen(rdep->start_graph));
	if (nb != strlen(rdep->start_graph)) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}

	/*
 	 * Send data 
	 */
	nb = write(ras->fd, (char *) rdep->data, dep->image_size);
	if (nb != dep->image_size) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}

	/*
 	 * Exit raster graphics mode
	 */
	nb = write(ras->fd, (char *) rdep->end_graph, strlen(rdep->end_graph));
	if (nb != strlen(rdep->end_graph)) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}

	/*
 	 * Eject page
	 */
	nb = write(ras->fd, (char *) rdep->eject, strlen(rdep->eject));
	if (nb != strlen(rdep->eject)) {
		(void) RasterSetError(RAS_E_SYSTEM);
		return(RAS_ERROR);
	}

	return(RAS_OK);
}

int
HPLJClose(ras)
	Raster	*ras;
{
	HPLJ_Info	*dep = (HPLJ_Info *) ras->dep;

	free((char *) ras->data);
	free((char *) ras->red);
	free((char *) ras->green);
	free((char *) ras->blue);
	if (ras->fd >= 0) (void) close(ras->fd);
	if (dep) {
		if (dep->ras.reset) free(dep->ras.reset);
		if (dep->ras.orientation) free(dep->ras.orientation);
		if (dep->ras.encoding) free(dep->ras.encoding);
		if (dep->ras.position) free(dep->ras.position);
		if (dep->ras.start_graph) free(dep->ras.start_graph);
		if (dep->ras.data) free((char *) dep->ras.data);
		if (dep->ras.end_graph) free(dep->ras.end_graph);
		if (dep->ras.eject) free(dep->ras.eject);

		if (dep->trans_data) free(dep->trans_data);
		free((char *) dep);
	}
	free((char *) ras);
	return(RAS_OK);
}

static	_swapshort (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;

    while (bp < ep) {
	c = *bp;
	*bp = *(bp + 1);
	bp++;
	*bp++ = c;
    }
}

static	_swaplong (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;
    }
}

int	HPLJRead()
{
	(void) RasterSetError(RAS_E_UNSUPPORTED_FUNCTIONS);
	return(RAS_ERROR);
}

int
HPLJSetFunctions(ras)
	Raster	*ras;
{
	ras->Open        = HPLJOpen;
	ras->OpenWrite   = HPLJOpenWrite;
	ras->Read        = HPLJRead;
	ras->Write       = HPLJWrite;
	ras->Close       = HPLJClose;
	ras->PrintInfo   = HPLJPrintInfo;
}

/*
 * determine the starting position for the raster image and build the 
 * control string  which represents it. The LaserJet does internal pixel 
 * replication  whenever dpi is not the max value, 300. We need to remember
 * this when we calculate starting position of image.
 */
static	char	*position(nx, ny, dpi, orientation)
	int	nx, ny;		/* dimension of image	*/
	int	dpi;		/* dots per inch	*/

	int	orientation;
{
	char	buf[80];
	char	*cptr;
	int	total_width;	/* width of page in dpi		*/
	int	total_height;	/* height of page in dpi	*/
	int	image_width;	/* actual width of image after pixel rep.  */
	int	image_height;	/* actual height of image after pixel rep. */
	int	x_start,	/* starting x coord in dpi	*/
		y_start;	/* starting y coord in dpi	*/

	total_width = HPLJ_PAPER_WIDTH * HPLJ_MAX_RES;
	total_height = HPLJ_PAPER_HEIGHT * HPLJ_MAX_RES;
	image_width = nx * HPLJ_MAX_RES / dpi;
	image_height = ny * HPLJ_MAX_RES / dpi;

	if (orientation == RAS_LANDSCAPE) {
		y_start = (total_width - image_width) / 2;
		x_start = ((total_height - image_height) / 2) + image_height;
	}
	else {
		y_start = (total_height - image_height) / 2;
		x_start = (total_width - image_width) / 2;
	}

	bzero(buf, 80);
	(void) sprintf(buf, "*p%dx%dY", x_start, y_start);

	if ((cptr = malloc((unsigned) strlen(buf) + 1)) == NULL) return(NULL);

	(void) strcpy(cptr, buf);
	return(cptr);
	
}

/*
 * alloc  memory for bi-level encoded image  and set up HP "Transfer
 * Raster Data" command. Also, zero out memory for image.
 */
static	create_data_space(hplj, nx, ny)
	HPLJ_Info	*hplj;
	int	nx, ny;		/* dimension of image			*/
{

	char	*trans_data;	/* transfer raster data command		*/
	int	num_bytes;	/* number of bytes in a scan line	*/
	int	row_size;	/* num_bytes + size of trans_data	*/
	unsigned char	*data;	/* space for bi-level image		*/

	char	buf[80];


	/*
	 * find out how may 8-bit bytes needed to encode image a i pixel
	 * per bit
 	 */
	num_bytes = nx / 8;
	if (nx % 8) num_bytes++; 

	/*
	 * set up transfer raster data command
	 */
	bzero(buf, 80);
	(void) sprintf(buf, "*b%dW", num_bytes);
	if ((trans_data = malloc((unsigned)strlen(buf)+1)) == NULL) return(-1);
	(void) strcpy(trans_data, buf);

	/*
	 * alloc memory for bi-level image
	 */
	row_size = num_bytes + strlen(trans_data);

	data = (unsigned char *) malloc ((unsigned) (row_size * ny));
	if (! data) return(-1);

	bzero((char *) data, row_size * ny);

	hplj->trans_data = trans_data;
	hplj->row_size = row_size;
	hplj->image_size = row_size * ny;
	hplj->ras.data = data;

	return(1);
}
