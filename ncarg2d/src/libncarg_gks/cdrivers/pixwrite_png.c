/*
 *      $Id: pixwrite_png.c,v 1.12 2008-07-23 17:28:01 haley Exp $
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
 *      File:           pixwrite_png.c
 *
 *      Author:         Fred Clare 
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Tue Mar 23 12:22:01 MST 2004
 *
 *      Description:    This file writes an png file for the PIX workstation.
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
#include <png.h>

void write_png(unsigned char *, unsigned long, unsigned long, int, FILE *);
int isf(int i);

static FILE *OpenPNGFile (PIXddp *xi)
{
  char buf[256];
  char *cp;
  FILE *fp;

  strncpy(buf,xi->filename,127);
  cp = &(buf[strlen(xi->filename)]);
  if (xi->frame_count > 0) {
    sprintf(cp,".%06d.png",xi->frame_count);
  }
  else {
    sprintf(cp,".png");
  }
  fp = fopen(buf,"wb");
  return(fp);
}

int PIX_Write_PNG (PIXddp *xi)
{
  Status s;
  Window root;
  int x,y,rshift,gshift,bshift,kount;
  long pval,tval;
  unsigned int width,height,bw,depth;
  XImage *image;
  int image_size,pix_mask;
  XColor *colors;
  int win_name_size;
  int header_size;
  int ncolors, i;
  char *win_name = "xwd";
  unsigned char *pixmap;
  Bool got_win_name = False;

  XWDFileHeader header;
  XWDColor xwdcolor;

  Colormap            cmap ;
  Visual              *vis ;
  int format = ZPixmap,ii,jj,indx,redm,greenm,bluem;

  FILE *fp;

  fp = OpenPNGFile(xi);
  if (!fp) {
    ESprintf(ERR_OPN_PNG,"Error creating PNG output file");
    return(ERR_OPN_PNG);
  }

  s = XGetGeometry(xi->dpy,xi->pix,&root,&x,&y,&width,&height,
                   &bw,&depth);
  if (depth == 24) {
    pix_mask = 16777215;
  }
  else {
    ESprintf(ERR_PIX_DPT,"Unsupported pixmap depth for PNG output");
    return(ERR_PIX_DPT);
  }
  
  image = XGetImage (xi->dpy,xi->pix,x,y,width,height,AllPlanes,format);
  if (!image) {
    ESprintf(ERR_IMAGE_MEMORY,"Error creating XImage");
    return(ERR_IMAGE_MEMORY);
  }
  if (format != ZPixmap) {
    ESprintf(ERR_PIX_MAP,"Image must have format ZPixmap");
    return(ERR_PIX_MAP);
  }
  image_size = image->bytes_per_line * image->height;

  pixmap = (unsigned char *) calloc(3*width*height,sizeof(char));

  vis = xi->vis;

  redm = vis->red_mask;
  greenm = vis->green_mask;
  bluem = vis->blue_mask;
  rshift = isf(redm);
  gshift = isf(greenm);
  bshift = isf(bluem);

  indx = 0;
  for (ii = 0; ii < height; ii++) {
    for (jj = 0; jj < width; jj++) {
      pval = XGetPixel(image,jj,ii);
      tval = (pval & redm) >> rshift;
      pixmap[indx] = tval;
      tval = (pval & greenm) >> gshift;
      pixmap[indx+1] = tval;
      tval = (pval & bluem) >> bshift;
      pixmap[indx+2] = tval;
      indx += 3;
    }
  }

  write_png(pixmap,width,height,8,fp);
  fflush(fp);
  fclose(fp);
  return 0;
}

void write_png(unsigned char *pix_map, unsigned long width, 
               unsigned long height, int depth, FILE *fp) {

  int i,pixel_size;
  static int pic_num=0;
  int bit_depth,color_type,filter_method,compression_type,
      interlace_type, png_transforms;
  char *pnum,*output_name;
  unsigned long swaptest = 1;

  png_structp png_ptr;
  png_infop   info_ptr;
  png_bytep   *row_pointers;

  png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, 
              (png_voidp) NULL, (png_error_ptr) NULL, (png_error_ptr) NULL);
  if (!png_ptr) {
    printf("write_png: Failed to create structure pointer\n");
    exit(2);
  }

  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_write_struct(&png_ptr, (png_infopp) NULL);
    printf("write_png: Failed to create info structure.\n");
    exit(3);
  }

  if (setjmp(png_jmpbuf(png_ptr))) {
    png_destroy_write_struct(&png_ptr, &info_ptr);
    fclose(fp);
    printf ("write_png: Error in writing PNG file.\n");
    exit(4);
  }

  png_init_io(png_ptr, fp);

  bit_depth        = depth;
  color_type       = PNG_COLOR_TYPE_RGB;
  interlace_type   = 0;
  compression_type = 0;
  filter_method    = 0;
  pixel_size       = 3;

  row_pointers = png_malloc(png_ptr, height*sizeof(png_bytep));
  for (i = 0; i < height; i++) {
    row_pointers[i] = (png_bytep) (pix_map + i*width*pixel_size);
  }
  png_set_rows(png_ptr, info_ptr, row_pointers);

  png_set_IHDR(png_ptr, info_ptr, width, height, bit_depth, color_type, 
               interlace_type, compression_type, filter_method);

  
/*
 *  Check if byte swapping is necessary - if the image is LSBFirst
 *  and you are running on a big endian machine, or if the image is
 *  not LSBFirst and you are running on a little endian machine.
 */
  if (((*(char *) &swaptest) && (!LSBFirst))
                || (!(*(char *) &swaptest) && (LSBFirst))) {
    png_transforms = PNG_TRANSFORM_PACKING; 
  }
  else {
    png_transforms = 0;
  }

  png_write_png(png_ptr, info_ptr, png_transforms, NULL);
  png_destroy_write_struct(&png_ptr, &info_ptr);
}

/*
 *  isf returns the number of zero bits in the least significant
 *  part of an integer.
 */
int isf(int i) {
 int kount = 0;

 if (i != 0) {
   while (((i >> kount) & 1) != 1) kount++;
 }
 else {
   kount = 0;
 }
 return(kount);

}
