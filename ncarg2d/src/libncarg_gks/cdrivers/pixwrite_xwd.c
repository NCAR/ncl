/*
 *      $Id: pixwrite_xwd.c,v 1.2 2004-03-22 21:22:40 dbrown Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *      File:           pixwrite_xwd.c
 *
 *      Author:         David Brown
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Thu Mar 18 17:55:40 MST 2004
 *
 *      Description:    This file writes an xwd file for the PIX workstation.
 *                      
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xmu/WinUtil.h>
typedef unsigned long Pixel;
#include "X11/XWDFile.h"
#include <ncarg/c.h>
#include "gks_device.h"
#include "common.h"
#include "gksc.h"
#include "gks.h"
#include <ncarg/gksP.h>
#include "x.h"
#include "pix_device.h"
#include "pixddi.h"


static void _swapshort (bp, n)
    register char *bp;
    register unsigned int n;
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

static void _swaplong (bp, n)
    register char *bp;
    register unsigned int n;
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

#define lowbit(x) ((x) & (~(x) + 1))

static int
ReadColors(dpy,vis,cmap,colors)
Display	 *dpy;
Visual *vis ;
Colormap cmap ;
XColor **colors ;
{
    int i,ncolors ;
    

    ncolors = vis->map_entries;

    if (!(*colors = (XColor *) malloc (sizeof(XColor) * ncolors)))
	    return 0;

    if (vis->class == DirectColor ||
        vis->class == TrueColor) {
        Pixel red, green, blue, red1, green1, blue1;

        red = green = blue = 0;
        red1 = lowbit(vis->red_mask);
        green1 = lowbit(vis->green_mask);
        blue1 = lowbit(vis->blue_mask);
        for (i=0; i<ncolors; i++) {
          (*colors)[i].pixel = red|green|blue;
          (*colors)[i].pad = 0;
          red += red1;
          if (red > vis->red_mask)
            red = 0;
          green += green1;
          if (green > vis->green_mask)
            green = 0;
          blue += blue1;
          if (blue > vis->blue_mask)
            blue = 0;
        }
    } else {
        for (i=0; i<ncolors; i++) {
          (*colors)[i].pixel = i;
          (*colors)[i].pad = 0;
        }
    }

    XQueryColors(dpy, cmap, *colors, ncolors);
   
    return(ncolors);
}


#define BUF_MAX 1024

static FILE *OpenFile
(
	PIXddp    *xi
)
{

	char buf[BUF_MAX];
	char *cp;
	FILE *fp;

	strncpy(buf,xi->filename,BUF_MAX - 6);
	cp = &(buf[strlen(xi->filename)]);
	if (xi->frame_count > 0) {
		sprintf(cp,".%d.xwd",xi->frame_count);
	}
	else {
		sprintf(cp,".xwd");
	}
	fp = fopen(buf,"w");

	return(fp);

}
int PIX_Write_XWD
#ifdef  NeedFuncProto
(
        PIXddp    *xi

)
#else
(xi)
        PIXddp    *xi;
#endif

{
	Status s;
	Window root;
	int x,y;
	unsigned int width,height,bw,depth;
	XImage *image;	       
	int image_size;
	unsigned long swaptest = 1;
	XColor *colors;
	int win_name_size;
	int header_size;
	int ncolors, i;
	char *win_name = "xwd";
	Bool got_win_name = False;
	XWDFileHeader header;
	XWDColor xwdcolor;
	Colormap            cmap ;
	Visual              *vis ;
	FILE *fp;
	int format = ZPixmap;

	fp = OpenFile(xi);
	if (! fp) {
		ESprintf(ERR_OPN_PDF,"Error creating XWD output file");
		return(ERR_OPN_PDF);
	}
	s = XGetGeometry(xi->dpy,xi->pix,&root,&x,&y,&width,&height,
			 &bw,&depth);
	image = XGetImage (xi->dpy,xi->pix,x,y,width,height,AllPlanes,format);

	if (!image) {
		ESprintf(ERR_IMAGE_MEMORY,"Error creating XImage");
		return(ERR_IMAGE_MEMORY);
	}
	image_size = image->bytes_per_line * image->height;

	vis = xi->vis;
	cmap = xi->cmap;
	ncolors = ReadColors(xi->dpy,vis,cmap, &colors);
	if (! ncolors) {
		ESprintf(ERR_IMAGE_MEMORY,"Error creating XImage");
		return(ERR_IMAGE_MEMORY);
	}

	
	/*
	 * Calculate header size.
	 */
	win_name_size = strlen(win_name) + sizeof(char);
	header_size = SIZEOF(XWDheader) + win_name_size;

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

	header.visual_class = (CARD32) vis->class;
	header.red_mask = (CARD32) vis->red_mask;
	header.green_mask = (CARD32) vis->green_mask;
	header.blue_mask = (CARD32) vis->blue_mask;
	header.bits_per_rgb = (CARD32) vis->bits_per_rgb;
	header.colormap_entries = (CARD32) vis->map_entries;

	header.ncolors = ncolors;
	header.window_width = (CARD32) width;
	header.window_height = (CARD32) height;
	header.window_x = 0;
	header.window_y = 0;
	header.window_bdrwidth = 0;

	if (*(char *) &swaptest) {
		_swaplong((char *) &header, (unsigned int)sizeof(header));
		for (i = 0; i < ncolors; i++) {
			_swaplong((char *) &colors[i].pixel, 
				  (unsigned int) sizeof(long));
			_swapshort((char *) &colors[i].red, 
				   (unsigned int)(3 * sizeof(short)));
		}
	}
	

	if (fwrite((char *)&header, SIZEOF(XWDheader), 1, fp) != 1 ||
	    fwrite(win_name, win_name_size, 1, fp) != 1) {
		perror("xwd");
		exit(1);
	}

	/*
	 * Write out the color maps, if any
	 */

	for (i = 0; i < ncolors; i++) {
		xwdcolor.pixel = colors[i].pixel;
		xwdcolor.red = colors[i].red;
		xwdcolor.green = colors[i].green;
		xwdcolor.blue = colors[i].blue;
		xwdcolor.flags = colors[i].flags;
		if (fwrite((char *) &xwdcolor, SIZEOF(XWDColor), 1, fp) != 1) {
			perror("xwd");
			exit(1);
		}
	}

	/*
	 * Write out the buffer.
	 */

	if (fwrite(image->data, (int) image_size, 1, fp) != 1) {
		perror("xwd");
		exit(1);
	}

	/*
	 * free the color buffer.
	 */

	if(ncolors > 0) free(colors);

	/*
	 * Free image
	 */
	XDestroyImage(image);

	fclose(fp);


	return 0;
}
