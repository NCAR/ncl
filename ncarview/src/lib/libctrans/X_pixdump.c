/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*
 * #include <X11/Xos.h>
 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "XWDFile.h"
#include <stdio.h>


extern	char	*malloc();


/*
 *	pixmap_dump
 *	[exported]
 *
 *		dump a pixmap to a file
 * on entry
 *	*dpy		: the display
 *	pixmap		: the pixmap to dump
 *	x,y		: upper left coordinate of pixmap for dump
 *	width, height	: width and height of area for dump
 *	cmap		: the colormap to use for dump
 *	*visual		: the visual to use (cannot be TrueColor or Director
 *			  Color
 *	format		: format for dump (XYPixmap or ZPixmap)
 *	*fp		: file pointer for output file
 * on exit
 *	return		: -1 => failure, else success
 */
pixmap_dump(dpy, pixmap ,x, y, width, height, cmap, visual, format, fp)
	Display	*dpy;
	Pixmap	pixmap;
	int	x,y;
	unsigned	width, height;
	Colormap	cmap;
	Visual		*visual;
	int format;
	FILE		*fp;
{
	register i;

	unsigned long swaptest = 1;
	XColor *colors;		/* list of colors in the color table	*/
	unsigned buffer_size;	/* size of image structure used for pix	*/
	char *win_name = "NCAR View pixdump";
	int win_name_size;	/* len of win_name			*/
	int header_size;	/* size of output header		*/
	int ncolors;		/* number of colors alocated in cmap	*/
	XImage *image;		/* struct to hold the pixmap		*/

	XWDFileHeader header;	/* output file header			*/



	/* sizeof(char) is included for the null string terminator. */
	win_name_size = strlen(win_name) + sizeof(char);

	/*
	 * Snarf the pixmap with XGetImage.
	 */
	image = XGetImage (dpy, (Drawable) pixmap, 
				x, y, width, height, AllPlanes, format);
	if (!image) {
		return (-1);
	}

	/*
	 * Determine the pixmap size.
	 */
	buffer_size = image_size(image, format);

	/*
	 * get the colors used from the color map
	 */
	ncolors = get_Xcolors(dpy, visual, cmap, &colors);

	/*
	 * Calculate header size.
	 */
	header_size = sizeof(header) + win_name_size;

	/*
	 * Write out header information.
	 */
	header.header_size = (xwdval) header_size;
	header.file_version = (xwdval) XWD_FILE_VERSION;
	header.pixmap_format = (xwdval) format;
	header.pixmap_depth = (xwdval) image->depth;
	header.pixmap_width = (xwdval) image->width;
	header.pixmap_height = (xwdval) image->height;
	header.xoffset = (xwdval) image->xoffset;
	header.byte_order = (xwdval) image->byte_order;
	header.bitmap_unit = (xwdval) image->bitmap_unit;
	header.bitmap_bit_order = (xwdval) image->bitmap_bit_order;
	header.bitmap_pad = (xwdval) image->bitmap_pad;
	header.bits_per_pixel = (xwdval) image->bits_per_pixel;
	header.bytes_per_line = (xwdval) image->bytes_per_line;
	header.visual_class = (xwdval) visual->class;
	header.red_mask = (xwdval) visual->red_mask;
	header.green_mask = (xwdval) visual->green_mask;
	header.blue_mask = (xwdval) visual->blue_mask;
	header.bits_per_rgb = (xwdval) visual->bits_per_rgb;
	header.colormap_entries = (xwdval) visual->map_entries;
	header.ncolors = ncolors;
	header.window_width = (xwdval) width;
	header.window_height = (xwdval) height;
	header.window_x = x;
	header.window_y = y;
	header.window_bdrwidth = (xwdval) 0;


	/*
	 * see if byte swaped, take action
	 */
	if (*(char *) &swaptest) {
		_swaplong((char *) &header, sizeof(header));
		for (i = 0; i < ncolors; i++) {
			_swaplong((char *) &colors[i].pixel, sizeof(long));
			_swapshort((char *) &colors[i].red, 3 * sizeof(short));
		}
	}

	(void) fwrite((char *)&header, sizeof(header), 1, fp);
	(void) fwrite(win_name, win_name_size, 1, fp);

	/*
	 * Write out the color maps, if any
	 */
	(void) fwrite((char *) colors, sizeof(XColor), ncolors, fp);

	/*
	 * Write out the buffer.
	 */
	/*
	 *    This copying of the bit stream (data) to a file is to be replaced
	 *  by an Xlib call which hasn't been written yet.  It is not clear
	 *  what other functions of xwd will be taken over by this (as yet)
	 *  non-existant X function.
	 */
	(void) fwrite(image->data, (int) buffer_size, 1, fp);

	/*
	* free the color buffer.
	*/
	if(ncolors > 0) cfree((char *) colors);

	/*
	 * Free image
	 */
	XDestroyImage(image);

	return(1);
}




/*
 *	image_size:
 *	[internal]
 *
 *		Determine the pixmap size.
 * on entry:
 *	*image		: an XImage
 *	format		: format of the image (XYPixmap, ZPixmap)
 * on exit
 *	return		: size of image in bytes
*/

static	int image_size(image, format)
	XImage *image;
	int	format;
{
	if (format != ZPixmap)
		return(image->bytes_per_line * image->height * image->depth);

	/* XYPixmap	*/
	return(image->bytes_per_line * image->height);
}


/*
 *	get_Xcolors
 *	[internal]
 *
 *		Get the XColors of all pixels in image - returns # of colors
 *
 * on entry
 *	*dpy		: the display
 *	*visual		: the visual to use (TrueColor and DirectColor not
 *			  supported yet)
 *	cmap		: the colormap
 * on exit
 *	*colors		: contains a list of XColors.
 *	return		: number of elements in *colors
 */
static	int get_Xcolors(dpy, visual, cmap, colors)
	Display	*dpy;
	Visual	*visual;
	Colormap	cmap;
	XColor **colors;
{
	int i, ncolors;

	if (!cmap)
		return(0);

	if (visual->class == TrueColor ||
		visual->class == DirectColor)
		return(0);    /* XXX punt for now */

	ncolors = visual->map_entries;
	if (!(*colors = (XColor *) malloc 
			(sizeof(XColor) * (unsigned) ncolors))) {

		return(-1);
	}

	for (i=0; i<ncolors; i++)
		(*colors)[i].pixel = i;

	XQueryColors(dpy, cmap, *colors, ncolors);

	return(ncolors);
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
