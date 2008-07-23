/*
 *      $Id: pixwrite_xwd.c,v 1.6 2008-07-23 17:28:01 haley Exp $
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
	int ii,jj,red,green,blue,*dc;
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

/*
 * Mary thinks these printfs are old debug statements, so she's
 * commenting  them out for now.
 *
 *        printf(" height = %d\n",image->height);
 *        printf(" width = %d\n",image->width);
 *        printf(" format = %d\n",image->format);
 *        printf(" byte_order = %d\n",image->byte_order);
 *        printf(" bitmap_unit = %d\n",image->bitmap_unit);
 *        printf(" bitmap_bit_order = %d\n",image->bitmap_bit_order);
 *        printf(" bitmap_pad = %d\n",image->bitmap_pad);
 *        printf(" bytes_per_line = %d\n",image->bytes_per_line);
 *        printf(" depth = %d\n",image->depth);
 *        printf(" bits_per_pixel = %d\n",image->bits_per_pixel);
 *        printf(" rred_mask = %d\n", image->red_mask);
 *        printf(" blue_mask = %d\n", image->blue_mask);
 *        printf(" green_mask = %d\n", image->green_mask);
 */
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

/*
 * Mary thinks these printfs are old debug statements, so she's
 * commenting  them out for now.
 *
 *	printf("header.header_size = %d\n",header.header_size);
 *	printf("header.file_version = %d\n",header.file_version);
 *	printf("header.pixmap_format = %d\n",header.pixmap_format);
 *	printf("header.pixmap_depth = %d\n",header.pixmap_depth);
 *	printf("header.pixmap_width = %d\n",header.pixmap_width);
 *	printf("header.pixmap_height = %d\n",header.pixmap_height);
 *	printf("header.xoffset = %d\n",header.xoffset);
 *	printf("header.byte_order = %d\n",header.byte_order);
 *	printf("header.bitmap_unit = %d\n",header.bitmap_unit);
 *	printf("header.bitmap_bit_order = %d\n",header.bitmap_bit_order);
 *	printf("header.bitmap_pad = %d\n",header.bitmap_pad);
 *	printf("header.bits_per_pixel = %d\n",header.bits_per_pixel);
 *	printf("header.bytes_per_line = %d\n",header.bytes_per_line);
 *
 *	printf("header.visual_class = %d\n",header.visual_class);
 *	printf("header.red_mask = %d\n",header.red_mask);
 *	printf("header.green_mask = %d\n",header.green_mask);
 *	printf("header.blue_mask = %d\n",header.blue_mask);
 *	printf("header.bits_per_rgb = %d\n",header.bits_per_rgb);
 *	printf("header.colormap_entries = %d\n",header.colormap_entries);
 *
 *	printf("header.ncolors = %d\n",header.ncolors);
 *	printf("header.window_width = %d\n",header.window_width);
 *	printf("header.window_height = %d\n",header.window_height);
 *	printf("header.window_x = %d\n",header.window_x);
 *	printf("header.window_y = %d\n",header.window_y);
 *	printf("header.window_bdrwidth = %d\n",header.window_bdrwidth);
 *
 *        printf("PseudoColor %d\n",PseudoColor);
 *        printf("StaticColor %d\n",StaticColor);
 *        printf("GrayScale %d\n",GrayScale);
 *        printf("StaticGray %d\n",StaticGray);
 *        printf("DirectColor %d\n",DirectColor);
 *        printf("TrueColor %d\n",TrueColor);
 *
 *        printf("AllPlanes %d\n",AllPlanes);
 */

        for (ii = 0; ii < image->height; ii++) {
          for (jj = 0; jj < image->width; jj++) {
            red = XGetPixel(image,jj,ii) & header.red_mask;
            green = XGetPixel(image,jj,ii) & header.green_mask;
            blue = XGetPixel(image,jj,ii) & header.blue_mask;
/*
 * Mary thinks this printf is an old debug statement, so she's
 * commenting  it out for now.
 *
 *            printf("for pixel (%3d,%3d) RGB = %3d %3d %3d\n",ii,jj,
 *                           red,green >> 8,blue >> 16);
 */
          }
        }

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
		xwdcolor.pad = colors[i].pad;
		if (fwrite((char *) &xwdcolor, SIZEOF(XWDColor), 1, fp) != 1) {
			perror("xwd");
			exit(1);
		}
	}
/*
 * Mary thinks these printfs are old debug statements, so she's
 * commenting  them out for now.
 *
 *        printf ("ncolors = %d, sizeof XWDcolor = %d, total size = %d\n",
 *            ncolors,SIZEOF(XWDColor),ncolors*SIZEOF(XWDColor));
 *        printf ("image_size= %d\n",image_size);
 */
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
