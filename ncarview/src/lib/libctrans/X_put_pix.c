/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include 	<stdio.h>
#include 	<X11/Xlib.h>
#include	"Xdefs.h"
#include	<ncarv.h>


/*	Xctrans_putpixels:
 *
 *	Author		John Clyne	(clyne@redcloud.ucar.edu)
 *	
 *	Date		Mon Dec 12 11:18:27 MST 1988
 *
 *
 *		This file contains the ctrans version of XPutPixel.
 *	XPutPixel is extremely slow. This file contains routine used
 *	to buffer up a large XImage without making a call to XPutPixel
 *	for each pixel in the image.
 */
extern	Pixeltype	Colortab[];
static	Pixeltype	table[MAX_COLOR_SIZE];

static	Bool	use_XPutPixel;	/* true if have to user XPutPixel	*/
static	Bool	lsbFirst;	/* true if server is byte swapped	*/
static	int	bytes_per_pixel;
static	int	image_size;

/*
 *	init_putpixel:
 *
 * 		intialize the Xctrans_putpixels module. Note this routine
 *	must be called whenever the XImage structure is changed.
 *
 * On entry
 *	image	: a newly created XImage
 */
init_putpixel(ximage ) 
	XImage *ximage;

{

	register	int i;
	unsigned long	swaptest = 1;	/* used to test if client byte swaped*/


	/* 
	 * build a table of all possible pixel values. 
	 */
	for (i=0; i < MAX_COLOR_SIZE; i++ )
		table[i] = Colortab[i];


	switch (ximage->bits_per_pixel)
	{
	case 8:
	    use_XPutPixel = FALSE;
	    bytes_per_pixel = 1;
	    break;
	case 16:
	    use_XPutPixel = FALSE;
	    bytes_per_pixel = 2;
	    break;
	case 32:
	    use_XPutPixel = FALSE;
	    bytes_per_pixel = 4;
	    break;
	case 64:
	    use_XPutPixel = FALSE;
	    bytes_per_pixel = 8;
	    break;
	default:
	    use_XPutPixel = TRUE;
	    break;
	}

	/*
 	 *	if possible translate values in table to format required
	 *	by an XImage. This way we can reference the table when
 	 *	converting a raster image into a XImage format and avoid
	 *	the need  for translating each pixel in that image.
	 */
	if (!use_XPutPixel)
	{

		/*
		 *	if (client is LSBfirst and server is MSB first)
		 *	or (client is MSBfirst and server is LSB first)
		 *	swap bytes. The client and the server need to have
		 *	image data in the same format.
		 */
		lsbFirst = ximage->byte_order == LSBFirst;

		if (((*(char *) &swaptest) && !lsbFirst)
			|| (!(*(char *) &swaptest) && lsbFirst)) {

			swapBytes((char *) &table[0], 
					sizeof(Pixeltype), MAX_COLOR_SIZE);

		}
	}

	image_size = ximage->width * ximage->height;

}

/*
 *	build_image
 *
 *		build an XImage from a scan line of pixel values
 *
 * On entry
 *	ximage	: a ptr to an XImage created with XCreateImage
 *	pixel	: a ptr to array of pixels to be place in ximage
 *
 * On exit
 *	ximage	: contains pixel data
 */
build_image (ximage, pixel) 
XImage *ximage;
Pixeltype	*pixel;
 
{
	register	i, x, y;
	register	int index = 0;

	register unsigned char *c_ptr = (unsigned char *) ximage->data;

	/*
 	 * See if we're on a bogus machine and require XPutPixel
	 */
	if (use_XPutPixel) {
		for (y = 0; y < ximage->height; y++)
			for (x = 0; x < ximage->width; x++, index++)
				XPutPixel (ximage, x, y, pixel[index]);
	}

	/*
 	 *	Happy Days !!!
	 */
	else {

		/*
		 * transfer data from pixel into ximage using the 
		 * previously created table to map pixel values into
		 * ximage data format
		 */
		if (! lsbFirst) {
			for (i = 0; i < image_size; i++, index++) {
				bcopy (((char *) (&table[pixel[index]])
				 + sizeof(Pixeltype) - bytes_per_pixel),
				(char *) c_ptr, bytes_per_pixel);

				c_ptr += bytes_per_pixel;
			}
		}

		else {	/* MSBFirst (most significant byte first)	*/
			for (i = 0; i < image_size; i++, index++) {

				bcopy ((char *) (&table[pixel[index]]),
				(char *) c_ptr, bytes_per_pixel);

				c_ptr += bytes_per_pixel;
			}
		}
	}
}
