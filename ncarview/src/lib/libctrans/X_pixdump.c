/*
 *	$Id: X_pixdump.c,v 1.11 1992-04-03 20:40:31 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/XWDFile.h>
#include <ncarv.h>

typedef unsigned long Pixel;

#ifdef	X11R3
typedef	unsigned long	CARD32;
#endif


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
	header.header_size = (CARD32) header_size;
	header.file_version = (CARD32) XWD_FILE_VERSION;
	header.pixmap_format = (CARD32) format;
	header.pixmap_depth = (CARD32) image->depth;
	header.pixmap_width = (CARD32) image->width;
	header.pixmap_height = (CARD32) image->height;
	header.xoffset = (CARD32) image->xoffset;
	header.byte_order = (CARD32) image->byte_order;
	header.bitmap_unit = (CARD32) image->bitmap_unit;
	header.bitmap_bit_order = (CARD32) image->bitmap_bit_order;
	header.bitmap_pad = (CARD32) image->bitmap_pad;
	header.bits_per_pixel = (CARD32) image->bits_per_pixel;
	header.bytes_per_line = (CARD32) image->bytes_per_line;
	header.visual_class = (CARD32) visual->class;
	header.red_mask = (CARD32) visual->red_mask;
	header.green_mask = (CARD32) visual->green_mask;
	header.blue_mask = (CARD32) visual->blue_mask;
	header.bits_per_rgb = (CARD32) visual->bits_per_rgb;
	header.colormap_entries = (CARD32) visual->map_entries;
	header.ncolors = ncolors;
	header.window_width = (CARD32) width;
	header.window_height = (CARD32) height;
	header.window_x = x;
	header.window_y = y;
	header.window_bdrwidth = (CARD32) 0;


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


#define lowbit(x) ((x) & (~(x) + 1))

/*
 * Get the XColors of all pixels in image - returns # of colors
 */
static	int get_Xcolors(dpy, visual, colormap, colors)
	Display	*dpy;
	Visual	*visual;
	Colormap	colormap;
	XColor **colors;
{
    int i, ncolors;

    if (!colormap)
	return(0);

    if (visual->class == TrueColor)
	return(0);    /* colormap is not needed */

    ncolors = visual->map_entries;
    *colors = (XColor *) icMalloc ((unsigned) (sizeof(XColor) * ncolors));

    if (visual->class == DirectColor) {
	Pixel red, green, blue, red1, green1, blue1;

	red = green = blue = 0;
	red1 = lowbit(visual->red_mask);
	green1 = lowbit(visual->green_mask);
	blue1 = lowbit(visual->blue_mask);
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = red|green|blue;
	  (*colors)[i].pad = 0;
	  red += red1;
	  if (red > visual->red_mask)
	    red = 0;
	  green += green1;
	  if (green > visual->green_mask)
	    green = 0;
	  blue += blue1;
	  if (blue > visual->blue_mask)
	    blue = 0;
	}
    } else {
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = i;
	  (*colors)[i].pad = 0;
	}
    }

    XQueryColors(dpy, colormap, *colors, ncolors);
#ifdef	DEBUG
	for (i=0; i < ncolors; i++) {
		fprintf(stderr, "Index [%d] (%d %d %d)\n", i, (*colors)[i].red,
			(*colors)[i].green, (*colors)[i].blue);
	}
#endif
    
    return(ncolors);
}

#ifdef	DEAD
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
	*colors = (XColor *) icMalloc (sizeof(XColor) * (unsigned) ncolors);

	for (i=0; i<ncolors; i++)
		(*colors)[i].pixel = i;

	XQueryColors(dpy, cmap, *colors, ncolors);

	return(ncolors);
}
#endif

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
