/*
 *      $Id: pdf.c,v 1.28.2.1 2010-03-17 20:53:30 brownrig Exp $
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
 *
 *      File:           pdf.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Thu Aug 19 10:30:32 MDT 1993
 *
 *      Description:    This file contains the definition of the PDF
 *                      device driver.
 */
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <ncarg/gksP.h>
#include <ncarg/c.h>
#include <ncarg/gks.h>
#include "gksc.h"
#include "gks.h"
#include "common.h"
#include "pdf_device.h"
#include "pdfddi.h"
#include "pdf.h"
#include "argb.h"

extern void gerr_hand(Gint, Gint, const char *);

void PDFPutLine(int, int, int, int);
void PDFPutCircle(int, int, int);
void PDFPutFonts(FILE *);
void PDFDefaultColorTable(PDFddp *);
void PDFPlotBackground(PDFddp *, int, int, int, int);
static int PDFoutput_string(PDFddp *, char *);
void rgb2cmyk(float, float, float, float *, float *, float *, float *);
void adjust_lines(void);

/*
 *  *********************** Globals ***********************************
 */

/*
 *  An array to build the individual page streams.
 *
 */
char **page_lines;
/* int maximum_lines=1000000; */
int maximum_lines=100000;
int  starting_page_object_number, stream_size, object_number, 
     byte_count, num_page_lines, restore_level;
float red,green,blue,cyan,magenta,yellow,black;

/*
 *  obj_pointer is an array of byte offsets for the PDF objects.
 *
 */
int *object_pointer;

char *PDFFontNames[] = 
  {
   "/Times-Roman",           "/Times-Bold",       "/Times-Italic",
   "/Times-BoldItalic",      "/Helvetica",
   "/Helvetica-Bold",        "/Helvetica-Oblique",
   "/Helvetica-BoldOblique", "/Courier",
   "/Courier-Bold",          "/Courier-Oblique",
   "/Courier-BoldOblique",   "/Symbol"
  };

extern int      orig_wks_id;
int             co_model,port_land;

/*
 *  *********************** End Globals *******************************
 */

void PDFprint_points(PDFddp *psa, PDFPoint *points, unsigned num, 
                        terminator_type terminator)
{
  int     i,tmpx,tmpy;

  for (i = 0 ; i < num ; i++) {
    tmpx = psa->dspace.llx 
            + (int) (points[i].x * (float) psa->dspace.xspan);
    tmpy = psa->dspace.lly 
            + (int) (points[i].y * (float) psa->dspace.yspan);
/*
 *  Return if a coordinate is out-of-range.
 */
    if (abs(tmpx) > 99999 || abs(tmpy) > 99999) {
      printf("**Warning**: PDF - coordinate out of range.\n");
      return;
    }

    bump_page_lines();
    switch (terminator) {
      case NONE:
        sprintf(page_lines[num_page_lines],"%6d %6d ", tmpx, tmpy);
        stream_size += 14;
        break;
      case MOVETO:
        sprintf(page_lines[num_page_lines],"%6d %6d m\n",tmpx, tmpy);
        stream_size += 16;
        break;
      case LINETO:
        sprintf(page_lines[num_page_lines],"%6d %6d l\n",tmpx, tmpy);
        stream_size += 16;
        break;
      default:
        break;
    }
  }
}

/*
 * Set the current dash pattern depending on the line type.
 */
void PDFset_dashpattern (PDFddp *psa)
{
  int     rscale, dash_size, dot_size, gap_size;
                
  rscale = (int)((1./PDF_SCALE)*
                    (MAX(1.,.5*(psa->attributes.linewidth))));
  dash_size = 10 * rscale;
  dot_size = 3 * rscale;
  gap_size = 5 * rscale;
  switch (psa->attributes.linetype) {
  case SOLID_LINE:
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"[] 0 d\n");
    stream_size += 7;
    break;
  case DASHED_LINE:
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"[%6d] 0 d\n", dash_size);
    stream_size += 13;
    break;
  case DOTTED_LINE:
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"[%6d] 0 d\n", dot_size);
    stream_size += 13;
    break;
  case DASH_DOT_LINE:
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"[%6d %6d %6d %6d] 0 d\n", 
            dash_size, gap_size, dot_size, gap_size);
    stream_size += 34;
    break;
  case DASH_DOT_DOT_LINE:
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"[%6d %6d %6d %6d %6d %6d] 0 d\n", 
            dash_size, gap_size, dot_size, gap_size, dot_size, gap_size);
    stream_size += 48;
    break;
  default:
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"[] 0 d\n");
    stream_size += 7;
    break;
  }
}

/*
 *  Calculate the current clipping rectangle in PDF coordinates.
 *
 */
PDFClipRect *GetPDFClipping (PDFddp *psa, CoordSpace s1, CoordSpace s2)
{
  static  PDFClipRect     rect;
  CoordSpace      rtmp;

  if ((s2.llx < s1.urx) && (s1.llx < s2.urx) &&
      (s2.lly < s1.ury) && (s1.lly < s2.ury)) {
          rtmp.llx = MAX(s1.llx, s2.llx);
          rtmp.lly = MAX(s1.lly, s2.lly);
          rtmp.urx = MIN(s1.urx, s2.urx);
          rtmp.ury = MIN(s1.ury, s2.ury);
                
/*
 *  Convert to workstation viewport coordinates.
 */
    rtmp.llx = rtmp.llx * (psa->transform).x_scale + (psa->transform).x_trans;
    rtmp.lly = rtmp.lly * (psa->transform).y_scale + (psa->transform).y_trans;
    rtmp.urx = rtmp.urx * (psa->transform).x_scale + (psa->transform).x_trans;
    rtmp.ury = rtmp.ury * (psa->transform).y_scale + (psa->transform).y_trans;
          
/*
 *  Calculate the PDF coordinates.
 */
    rect.llx = (psa->dspace.llx) + (int)(((float)(psa->dspace.xspan))*rtmp.llx);
    rect.urx = (psa->dspace.llx) + (int)(((float)(psa->dspace.xspan))*rtmp.urx);
    rect.lly = (psa->dspace.lly) + (int)(((float)(psa->dspace.yspan))*rtmp.lly);
    rect.ury = (psa->dspace.lly) + (int)(((float)(psa->dspace.yspan))*rtmp.ury);
    rect.null = FALSE;
  }
  else
  {
    rect.llx = rect.urx = rect.lly = rect.ury = 0;
    rect.null = TRUE;
  }
  return (&rect);
}

/*
 *  Put out the appropriate clipping rectangle.
 */
void PDFOutputClipping (PDFddp *psa, int type)
{
  int tx,ty,x0,x1,y0,y1,current_color;

  if (psa->pdf_clip.null == FALSE) {
    if (type == PDF_DEFAULT_CLIPPING_RECT) {
      x0 = psa->dspace.llx;
      x1 = psa->dspace.urx;
      y0 = psa->dspace.lly;
      y1 = psa->dspace.ury;
    }
    else {
      x0 = psa->pdf_clip.llx+1;
      x1 = psa->pdf_clip.urx+1;
      y0 = psa->pdf_clip.lly+1;
      y1 = psa->pdf_clip.ury+1;
    }

/*
 *  Go back up to the default graphics state and
 *  save it.  Clipping rectangles are determined by 
 *  intersecting the current clipping rectangle with 
 *  the newly-defined one, so we need to intersect 
 *  the newly-defined rectangle with the default 
 *  clipping rectangle.
 */
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"Q q\n");
    stream_size += 4;

/*
 *  Put out the scale factor.
 */
    bump_page_lines();
    sprintf(page_lines[num_page_lines],
              "%7.5f 0.0 0.0 %7.5f 0.0 0.0 cm\n",psa->scaling,psa->scaling);
    stream_size += 35;

/*
 *  Modify transformation matrix if landscape mode.
 */
    if (port_land == LANDSCAPE) {
      tx = -((psa->dspace.lly) + (psa->dspace.urx));
      ty =   (psa->dspace.llx) - (psa->dspace.lly);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"0. -1. 1. 0. 0. 0. cm\n");
      stream_size += 22;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"1. 0. 0. 1. %6d %6d cm\n", tx, ty);
      stream_size += 29;
    }

/*
 *  Establish the new clipping rectangle.
 */
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"S\n");
    stream_size += 2;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%6d %6d m\n",x0,y0);
    stream_size += 16;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%6d %6d l\n",x1,y0);
    stream_size += 16;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%6d %6d l\n",x1,y1);
    stream_size += 16;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%6d %6d l\n",x0,y1);
    stream_size += 16;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%6d %6d l\n",x0,y0);
    stream_size += 16;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"W n\n");
    stream_size += 4;
  }

/*
 *  Re-establish all of the graphics state attributes.
 */

/*
 *  Put out the line join, line cap, and miter limit.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%2d j %2d J %10.3f M\n",
           psa->line_join, psa->line_cap, psa->miter_limit);
  stream_size += 23;

/*
 *  Linewidth.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%6d w\n", psa->attributes.linewidth_set);
  stream_size += 9;

/*
 *  Dash pattern.
 */
  PDFset_dashpattern(psa);

/*
 *  Current color.
 */
  current_color = psa->attributes.pdf_colr_ind;
  index2rgb(psa->color_map, current_color, &red, &green, &blue);

  if (co_model != 0) {
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",
            red,green,blue);
    stream_size += 21;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",
            red,green,blue);
    stream_size += 21;
  }
  else {
    rgb2cmyk(red,green,blue,&cyan,&magenta,&yellow,&black);
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
            cyan,magenta,yellow,black);
    stream_size += 26;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
            cyan,magenta,yellow,black);
    stream_size += 26;
  }
}

void PDFpreamble (PDFddp *psa, preamble_type type)
{
  int     i,current_color,tx,ty,current_clipping;
  float   scl = psa->scaling;
  FILE    *fp;
  char    *strn,*lines[10];

  fp = psa->file_pointer;


  if (type == FOR_FILE) {

/*
 *  Initialize the page_lines buffer.
 */
    page_lines = (char **) calloc(maximum_lines,sizeof(char *));

    for (i = 0; i < maximum_lines; i++) {
      page_lines[i] = (char *) calloc(LINE_SIZE,sizeof(char));
    }

/*
 *  Initialize the array of object pointers.
 *
 */
    object_pointer = (int *) calloc(MAX_OBJECTS,sizeof(int));

/*
 *  Initialize the restore level to zero.
 *
 */
    restore_level = 0;

/*
 *  The pointer to object number 0 is 0; object number 1 is the
 *  Pages object - its pointer will be filled in at file termination.
 *
 */
    byte_count = 0;
    object_pointer[0] = byte_count;

    strn = "%PDF-1.4\n";
    (void) fprintf(fp, "%s", strn);
    byte_count += 9;           
    strn = "%\307\354\217\242\n";
    (void) fprintf(fp, "%s", strn);
    byte_count += 6;        
  
    object_number = 2;
    object_pointer[object_number] = byte_count;
    lines[0]   = "[/PDF /Text /ImageB /ImageC /ImageI]";
    byte_count += PDFPutObject(fp,object_number,1,lines);

/*
 *  Put out the font objects.
 */
    PDFPutFonts(fp);

/*
 *  Set the object number for the first page.
 */
    starting_page_object_number = object_number;

/*
 *  Set up the default color table.
 *
 */
    PDFDefaultColorTable(psa);
    (void) fflush(fp);
  }

  else if (type == FOR_PICTURE) {

/*
 *  Put out the stream dictionary for the current page.
 *
 */
    object_number = starting_page_object_number + 2*(psa->page_number) - 1;
    object_pointer[object_number] = byte_count;
    byte_count += PDFPutStreamDict(fp, object_number, object_number+1,
                                   psa->paper_width, psa->paper_height);

/*
 *  Begin the stream for this page.
 *
 */
    object_pointer[object_number+1] = byte_count;
    stream_size = 0;

/*
 *  ** First element ** - save the default graphics state.
 */
    num_page_lines = 0;
    sprintf(page_lines[num_page_lines], "q\n");
    stream_size += 2;
    restore_level = 1;

/*
 *  Put out the scale factor.
 */
    bump_page_lines();
    sprintf(page_lines[num_page_lines],
              "%7.5f 0.0 0.0 %7.5f 0.0 0.0 cm\n",scl,scl);
    stream_size += 35;

/*
 *  Modify transformation matrix if landscape mode.
 */
    if (port_land == LANDSCAPE) {
      tx = -((psa->dspace.lly) + (psa->dspace.urx));
      ty =  (psa->dspace.llx) - (psa->dspace.lly);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"0. -1. 1. 0. 0. 0. cm\n");
      stream_size += 22;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"1. 0. 0. 1. %6d %6d cm\n", tx, ty);
      stream_size += 29;
    }

/*
 *  Put out the line join, line cap, and miter limit.
 */
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%2d j %2d J %10.3f M\n",
             psa->line_join, psa->line_cap, psa->miter_limit);
    stream_size += 23;

/*
 *  Linewidth.
 */
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%6d w\n",psa->attributes.linewidth_set);
    stream_size += 9;

/*
 *  Dash pattern.
 */
    PDFset_dashpattern(psa);

/*
 *  Set the current color.
 */
    current_color = psa->attributes.pdf_colr_ind;
    index2rgb(psa->color_map, current_color, &red, &green, &blue);
    if (co_model != 0) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",
              red,green,blue);
      stream_size += 21;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",
              red,green,blue);
      stream_size += 21;
    }
    else {
      rgb2cmyk(red,green,blue,&cyan,&magenta,&yellow,&black);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
    }

/*
 *  Color the background.
 */
    current_clipping = psa->attributes.clip_ind;
    psa->attributes.clip_ind = CLIPPING_OFF;
    if (psa->background && (psa->suppress_flag != 1) &&
                           (psa->suppress_flag != 2)) {
       PDFbackground(psa);
    }
    psa->attributes.clip_ind = current_clipping;

    PDFOutputClipping (psa, PDF_DEFAULT_CLIPPING_RECT);
    if (psa->attributes.clip_ind == CLIPPING_ON) {
       PDFOutputClipping (psa, PDF_CLIPPING_RECT);
    }

/*
 *  Establish the new clipping rectangle.
 *  x0 = psa->pdf_clip.llx;
 *  x1 = psa->pdf_clip.urx;
 *  y0 = psa->pdf_clip.lly;
 *  y1 = psa->pdf_clip.ury;
 *  bump_page_lines();
 *  sprintf(page_lines[num_page_lines],"%6d %6d m\n",x0,y0);
 *  stream_size += 16;
 *  bump_page_lines();
 *  sprintf(page_lines[num_page_lines],"%6d %6d l\n",x1,y0);
 *  stream_size += 16;
 *  bump_page_lines();
 *  sprintf(page_lines[num_page_lines],"%6d %6d l\n",x1,y1);
 *  stream_size += 16;
 *  bump_page_lines();
 *  sprintf(page_lines[num_page_lines],"%6d %6d l\n",x0,y1);
 *  stream_size += 16;
 *  bump_page_lines();
 *  sprintf(page_lines[num_page_lines],"%6d %6d l\n",x0,y0);
 *  stream_size += 16;
 *  bump_page_lines();
 *  sprintf(page_lines[num_page_lines],"W n\n");
 *  stream_size += 4;
 */
  }
}

/*
 * Color the background.
 */
void PDFbackground (PDFddp *psa)
{
/*
 *  Plot the background using color index 0.
 */
  int llx,lly,urx,ury;
  float rscl;

  rscl = 1./psa->scaling;
  if ((psa->type == PDF14) && (psa->full_background != FALSE)) {
    llx = 0;
    lly = 0;
    urx = (int) 72.*8.5*rscl;
    ury = (int) 72.*11.*rscl;
    PDFPlotBackground(psa,llx,lly,urx,ury);
  }
  else if (psa->attributes.clip_ind == CLIPPING_ON) {
    llx = psa->pdf_clip.llx;
    lly = psa->pdf_clip.lly;
    urx = psa->pdf_clip.urx;
    ury = psa->pdf_clip.ury;
    PDFPlotBackground(psa,llx,lly,urx,ury);
  }
  else { 
    llx = psa->dspace.llx;
    lly = psa->dspace.lly;
    urx = psa->dspace.urx;
    ury = psa->dspace.ury;
    PDFPlotBackground(psa,llx,lly,urx,ury);
  }
}

/*
 *  Put out the default color table.
 */
void PDFDefaultColorTable(PDFddp *psa)
{
  int i;
  char tmpstr[9], *strng;
  const char *color_values[MAX_COLORS] = {
     "1. 1. 1." , "0. 0. 0." , "1. 0. 0." , "0. 1. 0." , "0. 0. 1." , 
     "0. 1. 1." , "1. 0. .8" , "1. 1. 0." , "1. .5 0." , ".6 .8 0." , 
     "0. 1. .6" , "0. .5 1." , ".5 0. .8" , "1. 0. .6" , ".3 .3 .3" , 
     ".7 .7 .7" , "1. 1. .3" , ".7 1. .5" , ".5 1. .6" , ".2 1. .7" ,
     ".3 .8 .8" , ".5 .7 .8" , ".7 .5 .8" , "1. .3 .9" , ".7 .9 .5" , 
     ".4 .9 .5" , ".2 .9 .7" , ".2 .7 .9" , ".2 .5 1." , ".5 .3 1." , 
     ".7 .2 1." , ".9 .1 1." , ".9 1. .2" , ".7 1. .3" , ".5 1. .3" , 
     ".2 1. .5" , ".2 .8 .6" , ".2 .7 .7" , ".2 .5 .8" , ".2 .4 .9" ,
     ".4 .3 .9" , ".7 .2 .9" , ".8 .2 .8" , ".9 .3 .7" , ".8 .4 .6" , 
     ".8 .6 .5" , ".9 .7 .4" , ".9 .7 .3" , "1. .9 .9" , ".8 1. .1" , 
     ".6 1. .2" , ".5 1. .2" , ".2 .9 .5" , ".2 .8 .5" , ".2 .7 .7" , 
     ".2 .5 .7" , ".2 .4 .9" , ".4 .3 .9" , ".5 .2 .9" , ".8 .2 .7" ,
     "1. .2 .7" , "1. .3 .6" , "1. .4 .5" , "1. .5 .4" , "1. .8 .1" , 
     ".7 1. .0" , ".6 1. .1" , ".4 1. .2" , ".1 1. .3" , ".1 .8 .4" , 
     ".2 .7 .5" , ".1 .6 .7" , ".1 .5 .7" , ".1 .4 .8" , ".2 .3 .9" , 
     ".2 .2 1." , ".3 .1 1." , ".5 .1 .9" , ".7 .0 .8" , ".9 .0 .7" ,
     ".9 .7 .1" , ".7 .9 .1" , ".5 .9 .1" , ".3 .9 .2" , ".1 .9 .3" , 
     ".2 .7 .4" , ".1 .7 .5" , ".1 .5 .6" , ".1 .4 .7" , ".2 .3 .7" , 
     ".2 .2 .8" , ".3 .2 .8" , ".5 .1 .7" , ".7 .1 .7" , ".9 .2 .5" , 
     ".8 .2 .5" , ".8 .7 .0" , ".6 .8 .1" , ".4 .9 .1" , ".3 .9 .1" ,
     ".1 .9 .2" , ".2 .8 .2" , ".2 .7 .3" , ".2 .6 .5" , ".1 .5 .5" , 
     ".1 .4 .6" , ".2 .3 .7" , ".3 .2 .7" , ".4 .2 .6" , ".7 .2 .5" , 
     ".8 .2 .4" , "1. .2 .3" , ".7 .7 .0" , ".5 .7 .1" , ".4 .8 .1" , 
     ".2 .9 .1" , ".2 .9 .1" , ".2 .8 .2" , ".1 .7 .2" , ".1 .7 .3" ,
     ".1 .6 .4" , ".1 .4 .5" , ".1 .4 .5" , ".2 .3 .5" , ".4 .3 .4" , 
     ".5 .3 .4" , ".7 .3 .3" , ".8 .3 .2" , ".7 .6 .0" , ".5 .7 .1" , 
     ".4 .7 .1" , ".2 .7 .2" , ".1 .7 .2" , ".1 .6 .3" , ".1 .5 .4" , 
     ".1 .4 .4" , ".1 .3 .5" , ".1 .2 .7" , ".1 .1 .7" , ".4 .1 .5" ,
     ".5 .1 .5" , ".6 .1 .4" , ".7 .1 .4" , ".9 .1 .3" , ".5 .5 .1" , 
     ".4 .6 .1" , ".3 .6 .1" , ".2 .6 .2" , ".1 .5 .2" , ".1 .4 .3" , 
     ".1 .4 .4" , ".2 .3 .4" , ".2 .2 .4" , ".2 .2 .5" , ".2 .1 .5" , 
     ".3 .1 .5" , ".4 .1 .4" , ".5 .1 .4" , ".7 .1 .3" , ".8 .1 .2" ,
     ".5 .4 .1" , ".4 .5 .1" , ".3 .6 .0" , ".2 .6 .1" , ".1 .7 .1" , 
     ".1 .6 .1" , ".1 .5 .2" , ".1 .4 .2" , ".1 .4 .2" , ".1 .3 .3" , 
     ".1 .2 .4" , ".1 .2 .5" , ".1 .1 .5" , ".1 .0 .6" , ".3 .0 .5" , 
     ".5 .0 .4" , ".7 .3 .0" , ".7 .4 .0" , ".5 .4 .1" , ".3 .4 .2" ,
     ".2 .3 .2" , ".2 .2 .3" , ".2 .2 .4" , ".3 .1 .4" , ".5 .1 .3" , 
     ".6 .1 .2" , ".7 .1 .2" , ".8 .0 .2" , "1. .0 .2" , "1. .1 .1" , 
     ".9 .2 .1" , ".8 .2 .1" , ".6 .2 .0" , ".4 .4 .0" , ".2 .4 .1" , 
     ".2 .3 .2" , ".2 .2 .2" , ".2 .2 .3" , ".2 .1 .3" , ".3 .1 .3" ,
     ".5 .1 .2" , ".7 .1 .2" , ".8 .0 .2" , ".9 .0 .1" , "1. .0 .1" , 
     "1. .1 .1" , ".8 .2 .0" , ".7 .2 .0" , ".5 .2 .0" , ".4 .3 .0" , 
     ".1 .4 .1" , ".1 .4 .1" , ".1 .3 .2" , ".1 .2 .2" , ".1 .2 .3" , 
     ".2 .1 .4" , ".2 .0 .3" , ".4 .0 .2" , ".6 .0 .2" , ".7 .0 .1" ,
     ".8 .0 .1" , ".7 .1 .1" , ".7 .1 .1" , ".6 .2 .0" , ".4 .2 .0" , 
     ".2 .3 .0" , ".1 .3 .0" , ".1 .2 .1" , ".1 .2 .1" , ".1 .1 .2" , 
     ".1 .1 .3" , ".1 .0 .3" , ".1 .0 .2" , ".2 .0 .2" , ".3 .0 .2" , 
     ".3 .0 .1" , ".4 .0 .1" , ".4 .1 .1" , ".4 .1 .0" , ".2 .1 .0" ,
     ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , 
     ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , 
     ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , 
     ".0 .0 .0"
  };

/* 
 *  Store the default values in a local color table.
 */
  for (i = 0; i < MAX_COLORS; i++) {
    strcpy(tmpstr, color_values[i]);
    strng = strtok(tmpstr, " ");
    psa->color_map[3*i] = atof(strng);
    strng = strtok((char *) NULL, " ");
    psa->color_map[3*i+1] = atof(strng);
    strng = strtok((char *) NULL, " ");
    psa->color_map[3*i+2] = atof(strng);
/*
 *  The last part of color_map has flags to indicate if user-set.
 */
    psa->color_map[i + (3*MAX_COLORS)] = 0;
  }
}

/*
 *  Initialize local data.
 */
static void PDFinit(PDFddp *psa, int *coords)
{
  int     i, cllx, clly, curx, cury;
  float   rscale;

  psa->hatch_spacing       = PDF_HATCH_SPACING;
  psa->stack_size          = MAX_STACK;
  psa->path_size           = MAX_PATH;
  psa->line_join           = ROUND;
  psa->line_cap            = ROUNDED;
  psa->nominal_width_scale = .5;
  psa->full_background     = FALSE;
  psa->suppress_flag       = SUPPRESS_FLAG;
  psa->miter_limit         = MITER_LIMIT_DEFAULT;
  psa->sfill_spacing       = PDF_FILL_SPACING;

  /*
   *  Flag to suppress putting out background color rectangle.
   */
  psa->suppress_flag = *(coords+6);

  /*
   *  Color model flag is in *(coords+5); set the global
   *  variable co_model to its value.
   */
  co_model = *(coords+5);

  /*
   *  Coordinate initializations (the scale factor is in *(coords+4)).
   */
  rscale = (float) *(coords+4);
  if (rscale > 0.) {
    psa->scaling = 1./rscale;       
  }
  else {
    psa->scaling = PDF_SCALE;
    rscale       = 1./PDF_SCALE;
  }
  cllx = *coords;
  clly = *(coords+1);
  curx = *(coords+2);
  cury = *(coords+3);
  if ((cllx != -9999) && (clly != -9999) && 
    (curx != -9999) && (cury != -9999)) {
    psa->dspace.llx = (int) (((float) cllx) * rscale);
    psa->dspace.urx = (int) (((float) curx) * rscale);
    psa->dspace.lly = (int) (((float) clly) * rscale);
    psa->dspace.ury = (int) (((float) cury) * rscale);
  }
  else {
    psa->dspace.llx = (int) (((float) LLX_DEFAULT) * rscale);
    psa->dspace.urx = (int) (((float) URX_DEFAULT) * rscale);
    psa->dspace.lly = (int) (((float) LLY_DEFAULT) * rscale);
    psa->dspace.ury = (int) (((float) URY_DEFAULT) * rscale);
  }
  psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
  psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

  psa->pdf_clip.llx = psa->dspace.llx;
  psa->pdf_clip.lly = psa->dspace.lly;
  psa->pdf_clip.urx = psa->dspace.urx;
  psa->pdf_clip.ury = psa->dspace.ury;
  psa->pdf_clip.null = FALSE;

  psa->background = FALSE;
  psa->pict_empty = TRUE;
  psa->page_number = 1;

  psa->paper_width = *(coords+8);
  psa->paper_height = *(coords+9);

  for (i = 0; i < NUM_PDF_FONTS; i++) {
    psa->fonts_used[i] = 0;
  };

  psa->attributes.linetype         = LINETYPE_DEFAULT;
  psa->attributes.linetype_set     = LINETYPE_DEFAULT;
  psa->attributes.linewidth        = LINEWIDTH_DEFAULT;
  psa->attributes.linewidth_set    = 
    (int) ((psa->nominal_width_scale) *
    (psa->attributes.linewidth)/(psa->scaling));
  psa->attributes.linewidth_set    = LINEWIDTH_DEFAULT;
  psa->attributes.line_colr_ind    = LINE_COLR_DEFAULT;
  psa->attributes.marker_type      = MARKER_TYPE_DEFAULT;
  psa->attributes.marker_size      = MARKER_SIZE_DEFAULT;
  psa->attributes.marker_colr_ind  = MARKER_COLR_IND_DEFAULT;
  psa->attributes.text_font        = TEXT_FONT_DEFAULT;
  psa->attributes.text_prec        = TEXT_PREC_DEFAULT;
  psa->attributes.char_expan       = CHAR_EXPAN_DEFAULT;
  psa->attributes.char_space       = CHAR_SPACE_DEFAULT;
  psa->attributes.text_colr_ind    = TEXT_COLR_IND_DEFAULT;
  psa->attributes.char_ht          = CHAR_HT_DEFAULT;
  psa->attributes.char_up_vec_x    = CHAR_UP_VEC_X_DEFAULT;
  psa->attributes.char_up_vec_y    = CHAR_UP_VEC_Y_DEFAULT;
  psa->attributes.char_base_vec_x  = CHAR_BASE_VEC_X_DEFAULT;
  psa->attributes.char_base_vec_y  = CHAR_BASE_VEC_y_DEFAULT;
  psa->attributes.text_path        = TEXT_PATH_DEFAULT;
  psa->attributes.text_align_horiz = TEXT_ALIGN_HORIZ_DEFAULT;
  psa->attributes.text_align_vert  = TEXT_ALIGN_VERT_DEFAULT;
  psa->attributes.fill_int_style   = FILL_INT_STYLE_DEFAULT;
  psa->attributes.fill_style_ind   = FILL_STYLE_IND_DEFAULT;
  psa->attributes.fill_colr_ind    = FILL_COLR_IND_DEFAULT;
  psa->attributes.pdf_colr_ind     = PDF_COLR_IND_DEFAULT;
  psa->attributes.clip_ind         = CLIP_IND_DEFAULT;

}

/*
 *  Given an NCAR font number and a character, return a structure giving the
 *  equivalent PostScript font number (as per the association given below), 
 *  character number, and normalized character width.
 *
 *  The function returns a zero if the character is available and returns
 *  a -1 if it is not.  If the character is not availavble, the description
 *  for a filled Times-Roman asterisk is returned in psfc.
 */
static int MapFonts (PDFddp *psa, int cnum, PDFCharInfo *psfc)
{

/*
 *  Metrics for the standard thirteen PostScript fonts:
 *
 *    Times-Roman
 *    Times-Bold
 *    Times-Italic
 *    Times-BoldItalic
 *    Helvetica 
 *    Helvetica-Bold
 *    Helvetica-Oblique
 *    Helvetica-BoldOblique
 *    Courier
 *    Courier-Bold
 *    Courier-Oblique
 *    Courier-BoldOblique
 *    Symbol
 *
 *  In the PSFontMetrics array, for each of the thirteen fonts, 
 *  the first 96 elements are character widths for character 
 *  numbers 32-127; the second 96 elements are widths for 
 *  character numbers 160-255; the final element is the nominal 
 *  font height (the height of an "X" for all but the Symbol font).  
 *  All of the numbers are based on PostScript fonts normalized 
 *  by a scale of 1000.
 */
        const int PDFFontMetrics[NUM_PDF_FONTS][193] =
          {
            /* Times-Roman  */
            {
              250, 333, 408, 500, 500, 833, 778, 333, 333, 333, 500, 564, 250,
              333, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              278, 278, 564, 564, 564, 444, 921, 722, 667, 667, 722, 611, 556,
              722, 722, 333, 389, 722, 611, 889, 722, 722, 556, 722, 667, 556,
              611, 722, 722, 944, 722, 722, 611, 333, 278, 333, 469, 500, 333,
              444, 500, 444, 500, 444, 333, 500, 500, 278, 278, 500, 278, 778,
              500, 500, 500, 500, 333, 389, 278, 500, 500, 722, 500, 500, 444,
              480, 200, 480, 541, 250, 250, 333, 500, 500, 167, 500, 500, 500,
              500, 180, 444, 500, 333, 333, 556, 556, 250, 500, 500, 500, 250,
              250, 453, 350, 333, 444, 444, 500,1000,1000, 250, 444, 250, 333,
              333, 333, 333, 333, 333, 333, 333, 250, 333, 333, 250, 333, 333,
              333,1000, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
              250, 250, 250, 250, 250, 889, 250, 276, 250, 250, 250, 250, 611,
              722, 889, 310, 250, 250, 250, 250, 250, 667, 250, 250, 250, 278,
              250, 250, 278, 500, 722, 500, 250, 250, 250, 250, 662
            },

            /* Times-Bold  */
            {
              250, 333, 555, 500, 500,1000, 833, 333, 333, 333, 500, 570, 250,
              333, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              333, 333, 570, 570, 570, 500, 930, 722, 667, 722, 722, 667, 611,
              778, 778, 389, 500, 778, 667, 944, 722, 778, 611, 778, 722, 556,
              667, 722, 722,1000, 722, 722, 667, 333, 278, 333, 581, 500, 333,
              500, 556, 444, 556, 444, 333, 500, 556, 278, 333, 556, 278, 833,
              556, 500, 556, 556, 444, 389, 333, 556, 500, 722, 500, 500, 444,
              394, 220, 394, 520, 250, 250, 333, 500, 500, 167, 500, 500, 500,
              500, 278, 500, 500, 333, 333, 556, 556, 250, 500, 500, 500, 250,
              250, 540, 350, 333, 500, 500, 500,1000,1000, 250, 500, 250, 333,
              333, 333, 333, 333, 333, 333, 333, 250, 333, 333, 250, 333, 333,
              333,1000, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
              250, 250, 250, 250, 250,1000, 250, 300, 250, 250, 250, 250, 667,
              778,1000, 330, 250, 250, 250, 250, 250, 722, 250, 250, 250, 278,
              250, 250, 278, 500, 722, 556, 250, 250, 250, 250, 676
            },

            /* Times-Italic  */
            {
              250, 333, 420, 500, 500, 833, 778, 333, 333, 333, 500, 675, 250,
              333, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              333, 333, 675, 675, 675, 500, 920, 611, 611, 667, 722, 611, 611,
              722, 722, 333, 444, 667, 556, 833, 667, 722, 611, 722, 611, 500,
              556, 722, 611, 833, 611, 556, 556, 389, 278, 389, 422, 500, 333,
              500, 500, 444, 500, 444, 278, 500, 500, 278, 278, 444, 278, 722,
              500, 500, 500, 500, 389, 389, 278, 500, 444, 667, 444, 444, 389,
              400, 275, 400, 541, 250, 250, 389, 500, 500, 167, 500, 500, 500,
              500, 214, 556, 500, 333, 333, 500, 500, 250, 500, 500, 500, 250,
              250, 523, 350, 333, 556, 556, 500, 889,1000, 250, 500, 250, 333,
              333, 333, 333, 333, 333, 333, 333, 250, 333, 333, 250, 333, 333,
              333, 889, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
              250, 250, 250, 250, 250, 889, 250, 276, 250, 250, 250, 250, 556,
              722, 944, 310, 250, 250, 250, 250, 250, 667, 250, 250, 250, 278,
              250, 250, 278, 500, 667, 500, 250, 250, 250, 250, 653
            },

            /* Times-BoldItalic  */
            {
              250, 389, 555, 500, 500, 833, 778, 333, 333, 333, 500, 570, 250,
              333, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              333, 333, 570, 570, 570, 500, 832, 667, 667, 667, 722, 667, 667,
              722, 778, 389, 500, 667, 611, 889, 722, 722, 611, 722, 667, 556,
              611, 722, 667, 889, 667, 611, 611, 333, 278, 333, 570, 500, 333,
              500, 500, 444, 500, 444, 333, 500, 556, 278, 278, 500, 278, 778,
              556, 500, 500, 500, 389, 389, 278, 556, 444, 667, 500, 444, 389,
              348, 220, 348, 570, 250, 250, 389, 500, 500, 167, 500, 500, 500,
              500, 278, 500, 500, 333, 333, 556, 556, 250, 500, 500, 500, 250,
              250, 500, 350, 333, 500, 500, 500,1000,1000, 250, 500, 250, 333,
              333, 333, 333, 333, 333, 333, 333, 250, 333, 333, 250, 333, 333,
              333,1000, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
              250, 250, 250, 250, 250, 944, 250, 266, 250, 250, 250, 250, 611,
              722, 944, 300, 250, 250, 250, 250, 250, 722, 250, 250, 250, 278,
              250, 250, 278, 500, 722, 500, 250, 250, 250, 250, 669
            },

            /* Helvetica */
            {
              278, 278, 355, 556, 556, 889, 667, 222, 333, 333, 389, 584, 278,
              333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556,
              278, 278, 584, 584, 584, 556,1015, 667, 667, 722, 722, 667, 611,
              778, 722, 278, 500, 667, 556, 833, 722, 778, 667, 778, 722, 667,
              611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556, 222,
              556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833,
              556, 556, 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500,
              334, 260, 334, 584, 278, 278, 333, 556, 556, 167, 556, 556, 556,
              556, 191, 333, 556, 333, 333, 500, 500, 278, 556, 556, 556, 278,
              278, 537, 350, 222, 333, 333, 556,1000,1000, 278, 611, 278, 333,
              333, 333, 333, 333, 333, 333, 333, 278, 333, 333, 278, 333, 333,
              333,1000, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
              278, 278, 278, 278, 278,1000, 278, 370, 278, 278, 278, 278, 556,
              778,1000, 365, 278, 278, 278, 278, 278, 889, 278, 278, 278, 278,
              278, 278, 222, 611, 944, 611, 278, 278, 278, 278, 718
            },

            /* Helvetica-Bold  */
            {
              278, 333, 474, 556, 556, 889, 722, 278, 333, 333, 389, 584, 278,
              333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556,
              333, 333, 584, 584, 584, 611, 975, 722, 722, 722, 722, 667, 611,
              778, 722, 278, 556, 722, 611, 833, 722, 778, 667, 778, 722, 667,
              611, 722, 667, 944, 667, 667, 611, 333, 278, 333, 584, 556, 278,
              556, 611, 556, 611, 556, 333, 611, 611, 278, 278, 556, 278, 889,
              611, 611, 611, 611, 389, 556, 333, 611, 556, 778, 556, 556, 500,
              389, 280, 389, 584, 278, 278, 333, 556, 556, 167, 556, 556, 556,
              556, 238, 500, 556, 333, 333, 611, 611, 278, 556, 556, 556, 278,
              278, 556, 350, 278, 500, 500, 556,1000,1000, 278, 611, 278, 333,
              333, 333, 333, 333, 333, 333, 333, 278, 333, 333, 278, 333, 333,
              333,1000, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
              278, 278, 278, 278, 278,1000, 278, 370, 278, 278, 278, 278, 611,
              778,1000, 365, 278, 278, 278, 278, 278, 889, 278, 278, 278, 278,
              278, 278, 278, 611, 944, 611, 278, 278, 278, 278, 718
            },

            /* Helvetica-Oblique */
            {
              278, 278, 355, 556, 556, 889, 667, 222, 333, 333, 389, 584, 278,
              333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556,
              278, 278, 584, 584, 584, 556,1015, 667, 667, 722, 722, 667, 611,
              778, 722, 278, 500, 667, 556, 833, 722, 778, 667, 778, 722, 667,
              611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556, 222,
              556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833,
              556, 556, 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500,
              334, 260, 334, 584, 278, 278, 333, 556, 556, 167, 556, 556, 556,
              556, 191, 333, 556, 333, 333, 500, 500, 278, 556, 556, 556, 278,
              278, 537, 350, 222, 333, 333, 556,1000,1000, 278, 611, 278, 333,
              333, 333, 333, 333, 333, 333, 333, 278, 333, 333, 278, 333, 333,
              333,1000, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
              278, 278, 278, 278, 278,1000, 278, 370, 278, 278, 278, 278, 556,
              778,1000, 365, 278, 278, 278, 278, 278, 889, 278, 278, 278, 278,
              278, 278, 222, 611, 944, 611, 278, 278, 278, 278, 718
            },

            /* Helvetica-BoldOblique */
            {
              278, 333, 474, 556, 556, 889, 722, 278, 333, 333, 389, 584, 278,
              333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556,
              333, 333, 584, 584, 584, 611, 975, 722, 722, 722, 722, 667, 611,
              778, 722, 278, 556, 722, 611, 833, 722, 778, 667, 778, 722, 667,
              611, 722, 667, 944, 667, 667, 611, 333, 278, 333, 584, 556, 278,
              556, 611, 556, 611, 556, 333, 611, 611, 278, 278, 556, 278, 889,
              611, 611, 611, 611, 389, 556, 333, 611, 556, 778, 556, 556, 500,
              389, 280, 389, 584, 278, 278, 333, 556, 556, 167, 556, 556, 556,
              556, 238, 500, 556, 333, 333, 611, 611, 278, 556, 556, 556, 278,
              278, 556, 350, 278, 500, 500, 556,1000,1000, 278, 611, 278, 333,
              333, 333, 333, 333, 333, 333, 333, 278, 333, 333, 278, 333, 333,
              333,1000, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
              278, 278, 278, 278, 278,1000, 278, 370, 278, 278, 278, 278, 611,
              778,1000, 365, 278, 278, 278, 278, 278, 889, 278, 278, 278, 278,
              278, 278, 278, 611, 944, 611, 278, 278, 278, 278, 718
            },

            /* Courier */
            {
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 563
            },

            /* Courier-Bold */
            {
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 583
            },

            /* Courier-Oblique */
            {
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 563
            },

            /* Courier-BoldOblique */
            {
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 583
            },

            /* Symbol */
            {
              250, 333, 713, 500, 549, 833, 778, 439, 333, 333, 500, 549, 250,
              549, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              278, 278, 549, 549, 549, 444, 549, 722, 667, 722, 612, 611, 763,
              603, 722, 333, 631, 722, 686, 889, 722, 722, 768, 741, 556, 592,
              611, 690, 439, 768, 645, 795, 611, 333, 863, 333, 658, 500, 500,
              631, 549, 549, 494, 439, 521, 411, 603, 329, 603, 549, 549, 576,
              521, 549, 549, 521, 549, 603, 439, 576, 713, 686, 493, 686, 494,
              480, 200, 480, 549, 250, 250, 620, 247, 549, 167, 713, 500, 753,
              753, 753, 753,1042, 987, 603, 987, 603, 400, 549, 411, 549, 549,
              713, 494, 460, 549, 549, 549, 549,1000, 603,1000, 658, 823, 686,
              795, 987, 768, 768, 823, 768, 768, 713, 713, 713, 713, 713, 713,
              713, 768, 713, 790, 790, 890, 823, 549, 250, 713, 603, 603,1042,
              987, 603, 987, 603, 494, 329, 790, 790, 786, 713, 384, 384, 384,
              384, 384, 384, 494, 494, 494, 494, 250, 329, 274, 686, 686, 686,
              384, 384, 384, 384, 384, 384, 494, 494, 494, 250, 675
            }
          };

        /*
         *  Arrays for converting Hershey math symbols to PDF fonts and 
         *  characters.
         */
        const int hms2psf[96] = 
             {
                 4,  4,  4,  4, 12,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
                 4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
                 4, 12, 12, 12, 12, 12,  4, 12, 12, 12, 12, 12, 12,  0,  0, 12,
                12, 12, -1, 12, -1, 12, 12, 12, 12, 12, -1,  0,  0,  0,  0, 12,
                 4, 12, 12, 12,  0,  0,  0, -1, -1, 12, 12, 12, 12, 12, 12, 12,
                12, 12, 12, 12, 12, 12, 12, -1, 12, 12,  0,  0,  0,  0, 12,  0
             };
        const int hms2psc[96] = 
             {
                 32,  33,  34,  35,  83,  37,  38,  39,  40,  41,  42,  43,
                 44,  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,
                 56,  57,  58,  59,  60,  61,  62,  63,  64,  94, 208,  92,
                183, 189,  92,  42, 183, 176, 164, 163, 179, 178, 179, 225,
                241, 189,  -1, 177,  -1, 180, 183, 184, 185, 186,  -1,  91,
                 92,  93,  94,  36,  96, 181, 126, 162, 198,  39,  96,  -1,
                 -1, 214, 204, 200, 201, 199, 206, 174, 173, 172, 175, 182,
                209, 214, 242,  -1, 165,  80, 167, 123,  32, 125, 126,  32
             };

        /*
         *  Array for converting NCAR math symbol characters to PDF 
         *  Symbol font characters.
         */
        const int nms2psc[96] = 
             {
                 32,  64,  92, 163,  94, 165, 166,  34,  36,  39, 126, 171, 
                172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 
                184, 185, 186, 187, 188,  -1, 190, 191, 192, 193, 194, 195, 
                196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 
                208, 209,  32,  32,  32, 213, 214, 215, 216, 217, 218, 219,
                220, 221, 222, 223, 224, 225,  32,  32,  32, 229,  32,  32,
                 32,  32,  32,  32,  32,  32,  32,  32,  32, 241, 242,  -1,
                 -1,  -1,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32
             };
        /*
         *  Array for converting NCAR text symbol characters to PDF 
         *  text symbols.
         */
        const int nts2pts[96] = 
             {
                 32,  32, 162,  32,  32,  32,  32, 167,  32, 169, 170, 171, 
                172, 173,  32,  32,  32,  32, 178, 179,  32,  32, 182,  32, 
                184,  32, 186, 187, 188,  32,  32,  32,  32, 193, 194, 195, 
                196, 197, 198, 199, 200,  32, 202, 203,  32, 205, 206, 207, 
                 32,  32, 210, 211, 212,  32,  32,  32,  32,  32,  32,  32, 
                 32,  32,  32,  32,  32,  32,  -1,  -1,  -1,  32,  32,  32, 
                 32,  32,  32,  32,  -1,  -1,  -1,  -1,  32,  32,  32,  32, 
                 32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32, 
             };
        int index=0, fnum, return_value=0;

        psfc->char_num = cnum;
        psfc->outline = FALSE;
        fnum =  psa->attributes.text_font;

        switch (fnum) {
        case    DEFAULT: 
                psfc->font = PDF_HELVETICA;
                psa->fonts_used[PDF_HELVETICA] = 1;
                break;
        case    H_CARTOGRAPHIC_ROMAN:
                psfc->font = PDF_HELVETICA;
                psa->fonts_used[PDF_HELVETICA] = 1;
                break;
        case    H_CARTOGRAPHIC_GREEK:
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PDF_SYMBOL;
                                psa->fonts_used[PDF_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PDF_HELVETICA;
                                psa->fonts_used[PDF_HELVETICA] = 1;
                        }
                }
                break;
        case    H_SIMPLEX_ROMAN:
                psfc->font = PDF_HELVETICA;
                psa->fonts_used[PDF_HELVETICA] = 1;
                break;
        case    H_SIMPLEX_GREEK:
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PDF_SYMBOL;
                                psa->fonts_used[PDF_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PDF_HELVETICA;
                                psa->fonts_used[PDF_HELVETICA] = 1;
                        }
                }
                break;
        case    H_SIMPLEX_SCRIPT: 
                return_value = -1;
        case    H_COMPLEX_ROMAN:
                psfc->font = PDF_TIMES_ROMAN;
                psa->fonts_used[PDF_TIMES_ROMAN] = 1;
                break;
        case    H_COMPLEX_GREEK:
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PDF_SYMBOL;
                                psa->fonts_used[PDF_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PDF_TIMES_ROMAN;
                                psa->fonts_used[PDF_TIMES_ROMAN] = 1;
                        }
                }
                break;
        case    H_COMPLEX_SCRIPT:
                return_value = -1;
                break;
        case    H_COMPLEX_ITALIC:
                psfc->font = PDF_TIMES_ITALIC;
                psa->fonts_used[PDF_TIMES_ITALIC] = 1;
                break;
        case    H_COMPLEX_CYRILLIC:
                return_value = -1;
                break;
        case    H_DUPLEX_ROMAN:
                psfc->font = PDF_HELVETICA_BOLD;
                psa->fonts_used[PDF_HELVETICA_BOLD] = 1;
                break;
        case    H_TRIPLEX_ROMAN: 
                psfc->font = PDF_TIMES_BOLD;
                psa->fonts_used[PDF_TIMES_BOLD] = 1;
                break;
        case    H_TRIPLEX_ITALIC:
                psfc->font = PDF_TIMES_BOLDITALIC;
                psa->fonts_used[PDF_TIMES_BOLDITALIC] = 1;
                break;
        case    H_GOTHIC_GERMAN:
                return_value = -1;
                break;
        case    H_GOTHIC_ENGLISH:
                return_value = -1;
                break;
        case    H_GOTHIC_ITALIAN:
                return_value = -1;
                break;
        case    H_MATH_SYMBOLS:
                if ((cnum >= 32) && (cnum <= 127)) {
                        psfc->font = hms2psf[cnum-32];
                        psfc->char_num = hms2psc[cnum-32];
                        psa->fonts_used[psfc->font] = 1;
                }
                break;
        case    H_SYMBOL_SET1:
                return_value = -1;
                break;
        case    H_SYMBOL_SET2:
                return_value = -1;
                break;
        case    NCAR_HELVETICA: 
                psfc->font = PDF_HELVETICA;
                psa->fonts_used[PDF_HELVETICA] = 1;
                break;
        case    NCAR_HELVETICA_BOLD:
                psfc->font = PDF_HELVETICA_BOLD;
                psa->fonts_used[PDF_HELVETICA_BOLD] = 1;
                break;
        case    NCAR_HELVETICA_OBLIQUE:
                psfc->font = PDF_HELVETICA_OBLIQUE;
                psa->fonts_used[PDF_HELVETICA_OBLIQUE] = 1;
                break;
        case    NCAR_HELVETICA_BOLDOBLIQUE:
                psfc->font = PDF_HELVETICA_BOLDOBLIQUE;
                psa->fonts_used[PDF_HELVETICA_BOLDOBLIQUE] = 1;
                break;
        case    NCAR_TIMES_ROMAN:
                psfc->font = PDF_TIMES_ROMAN;
                psa->fonts_used[PDF_TIMES_ROMAN] = 1;
                break;
        case    NCAR_TIMES_BOLD: 
                psfc->font = PDF_TIMES_BOLD;
                psa->fonts_used[PDF_TIMES_BOLD] = 1;
                break;
        case    NCAR_TIMES_ITALIC:
                psfc->font = PDF_TIMES_ITALIC;
                psa->fonts_used[PDF_TIMES_ITALIC] = 1;
                break;
        case    NCAR_TIMES_BOLDITALIC:
                psfc->font = PDF_TIMES_BOLDITALIC;
                psa->fonts_used[PDF_TIMES_BOLDITALIC] = 1;
                break;
        case    NCAR_COURIER:
                psfc->font = PDF_COURIER;
                psa->fonts_used[PDF_COURIER] = 1;
                break;
        case    NCAR_COURIER_BOLD:
                psfc->font = PDF_COURIER_BOLD;
                psa->fonts_used[PDF_COURIER_BOLD] = 1;
                break;
        case    NCAR_COURIER_OBLIQUE:
                psfc->font = PDF_COURIER_OBLIQUE;
                psa->fonts_used[PDF_COURIER_OBLIQUE] = 1;
                break;
        case    NCAR_COURIER_BOLDOBLIQUE:
                psfc->font = PDF_COURIER_BOLDOBLIQUE;
                psa->fonts_used[PDF_COURIER_BOLDOBLIQUE] = 1;
                break;
        case    NCAR_GREEK: 
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PDF_SYMBOL;
                                psa->fonts_used[PDF_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PDF_TIMES_ROMAN;
                                psa->fonts_used[PDF_TIMES_ROMAN] = 1;
                        }
                        if (cnum == 118) {
                                psfc->char_num = 161;
                        }
                }
                break;
        case    NCAR_MATH_SYMBOLS:
                psfc->font = PDF_SYMBOL;
                psa->fonts_used[PDF_SYMBOL] = 1;
                if ((cnum >= 32) && (cnum <= 127)) {
                        psfc->char_num = nms2psc[cnum-32];
                }
                break;
        case    NCAR_TEXT_SYMBOLS:
                psfc->font = PDF_TIMES_ROMAN;   
                psa->fonts_used[PDF_TIMES_ROMAN] = 1;
                if ((cnum >= 32) && (cnum <= 127)) {
                        if (cnum == 54) {
                                psfc->font = PDF_HELVETICA;
                                psa->fonts_used[PDF_HELVETICA] = 1;
                        }
                        if (cnum >= 82 && cnum <= 84) {
                                psfc->font = PDF_SYMBOL;
                                psa->fonts_used[PDF_HELVETICA] = 1;
                        }
                        psfc->char_num = nts2pts[cnum-32];
                }
                break;
        case    NCAR_WEATHER1:
                return_value = -1;
                break;
        case    NCAR_WEATHER2:
                return_value = -1;
                break;
        case    NCAR_HELVETICA_O: 
                psfc->font = PDF_HELVETICA;
                psa->fonts_used[PDF_HELVETICA] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_HELVETICA_BOLD_O:
                psfc->font = PDF_HELVETICA_BOLD;
                psa->fonts_used[PDF_HELVETICA_BOLD] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_HELVETICA_OBLIQUE_O:
                psfc->font = PDF_HELVETICA_OBLIQUE;
                psa->fonts_used[PDF_HELVETICA_OBLIQUE] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_HELVETICA_BOLDOBLIQUE_O:
                psfc->font = PDF_HELVETICA_BOLDOBLIQUE;
                psa->fonts_used[PDF_HELVETICA_BOLDOBLIQUE] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_TIMES_ROMAN_O:
                psfc->font = PDF_TIMES_ROMAN;
                psa->fonts_used[PDF_TIMES_ROMAN] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_TIMES_BOLD_O: 
                psfc->font = PDF_TIMES_BOLD;
                psa->fonts_used[PDF_TIMES_BOLD] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_TIMES_ITALIC_O:
                psfc->font = PDF_TIMES_ITALIC;
                psa->fonts_used[PDF_TIMES_ITALIC] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_TIMES_BOLDITALIC_O:
                psfc->font = PDF_TIMES_BOLDITALIC;
                psa->fonts_used[PDF_TIMES_BOLDITALIC] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_COURIER_O:
                psfc->font = PDF_COURIER;
                psa->fonts_used[PDF_COURIER] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_COURIER_BOLD_O:
                psfc->font = PDF_COURIER_BOLD;
                psa->fonts_used[PDF_COURIER_BOLD] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_COURIER_OBLIQUE_O:
                psfc->font = PDF_COURIER_OBLIQUE;
                psa->fonts_used[PDF_COURIER_OBLIQUE] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_COURIER_BOLDOBLIQUE_O:
                psfc->font = PDF_COURIER_BOLDOBLIQUE;
                psa->fonts_used[PDF_COURIER_BOLDOBLIQUE] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_GREEK_O: 
                psfc->outline = TRUE;
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PDF_SYMBOL;
                                psa->fonts_used[PDF_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PDF_TIMES_ROMAN;
                                psa->fonts_used[PDF_TIMES_ROMAN] = 1;
                        }
                        if (cnum == 118) {
                                psfc->char_num = 161;
                        }
                }
                break;
        case    NCAR_MATH_SYMBOLS_O:
                psfc->font = PDF_SYMBOL;
                psa->fonts_used[PDF_SYMBOL] = 1;
                psfc->outline = TRUE;
                if ((cnum >= 32) && (cnum <= 127)) {
                        psfc->char_num = nms2psc[cnum-32];
                }
                break;
        case    NCAR_TEXT_SYMBOLS_O:
                psfc->font = PDF_TIMES_ROMAN;   
                psa->fonts_used[PDF_TIMES_ROMAN] = 1;
                psfc->outline = TRUE;
                if ((cnum >= 32) && (cnum <= 127)) {
                        if (cnum == 54) {
                                psfc->font = PDF_HELVETICA;
                                psa->fonts_used[PDF_HELVETICA] = 1;
                        }
                        if (cnum >= 82 && cnum <= 84) {
                                psfc->font = PDF_SYMBOL;
                                psa->fonts_used[PDF_SYMBOL] = 1;
                        }
                        psfc->char_num = nts2pts[cnum-32];
                }
                break;
        case    NCAR_WEATHER1_O:
                return_value = -1;
                break;
        case    NCAR_WEATHER2_O:
                return_value = -1;
                break;
        default:
                return_value = -1;
                break;
        }

        if ((psfc->char_num >= 32) && (psfc->char_num <= 127)) {
                index = (psfc->char_num) - 32;
        }
        else if ((psfc->char_num >= 160) && (psfc->char_num <= 255)) {
                index = (psfc->char_num) - 160 + 96;
        }
        else if (psfc->char_num == -1) {
                return_value = -1;
        }
        else {
                psfc->font_height = 
                        PDFFontMetrics[psfc->font][FONT_HEIGHT_ARRAY_INDEX];
                psfc->char_width = 0;
                return(0);
        }

        /*
         *  Return a filled Times-Roman asterisk if character not available.
         */
        if (return_value == -1) {
                psfc->font = PDF_TIMES_ROMAN;
                psa->fonts_used[PDF_TIMES_ROMAN] = 1;
                psfc->char_num = 42;
                psfc->char_width = PDFFontMetrics[psfc->font][10];
                psfc->font_height = 
                        PDFFontMetrics[psfc->font][FONT_HEIGHT_ARRAY_INDEX];
                psfc->outline = FALSE;
                return(return_value);
        }

        psfc->char_width = PDFFontMetrics[psfc->font][index];
        psfc->font_height = PDFFontMetrics[psfc->font][FONT_HEIGHT_ARRAY_INDEX];
        if (fnum == H_CARTOGRAPHIC_ROMAN || fnum == H_CARTOGRAPHIC_GREEK) {
                psfc->font_height *= 1.5;
        }

        return(0);

}

/*
 *  Function for reversing the characters in a string.
 */
void PDFreverse_chars(char *str)
{
        int     strl = strlen(str), i, index;
        char    ctmp;

        for (i = 0; i < strl/2; i++) {
                index = strl-i-1;
                strncpy(&ctmp, str+i, 1);
                strncpy(str+i, str+index, 1);
                strncpy(str+index, &ctmp, 1);
        }
}

/*
 *  Set up the file name for the output file depending on the workstation
 *  ID, and if it is PDF, EPS, or EPSI, or whether it has been specifically
 *  provided.
 */
char *PDFGetFileName(int wkid, char *file_name)
{
  static char tname[257];
  static char *tch;

/*
 *  A setting of the environment variable NCARG_GKS_PDFOUTPUT
 *  takes precedence over everything.
 */
  tch = getenv("NCARG_GKS_PDFOUTPUT");
  if ( (tch != (char *) NULL) && (strlen(tch) > 0)) {
    return (tch);
  }

  if ( (strncmp(file_name, "DEFAULT", 7) == 0) ||
       (strlen(file_name) == 0) ) {
    (void) sprintf(tname,"gmeta%d.pdf",wkid);
    return (tname);
  }
  else {      /* User has defined a name */
    tch = strtok(file_name, " ");
    return (tch);   
  }
}

/*
 *  Put out a character string for PDF display.  The special
 *  characters "(", ")", "\" are put out as PDF octal
 *  constants as are any characters having an ASCII Decimal Equivalent
 *  larger than 127.
 */
static int PDFoutput_string(PDFddp *psa, char *str)
{
  char    ctmp;
  int     i, itmp, j=0;

  bump_page_lines();
  for (i = 0; i < (size_t) strlen(str); i++) {
    strncpy(&ctmp, str+i, 1);
    itmp = (int) (ctmp & 255);
    if (itmp == 40) {
      sprintf((page_lines[num_page_lines]+j), "\\(");
      stream_size += 2;
      j = j+2;
    } 
    else if (itmp == 41) {
      sprintf((page_lines[num_page_lines]+j), "\\)");
      stream_size += 2;
      j = j+2;
    } 
    else if (itmp == 92) {
      sprintf((page_lines[num_page_lines]+j), "\\\\");
      stream_size += 2;
      j = j+2;
    }
    else {
      sprintf((page_lines[num_page_lines]+j), "%c", ctmp);
      j++;
      stream_size += 1;
    }
  }
  return(j);
}


/*
 *  Put out polyline.  If the second argument is non-zero, draw
 *  a line from the last to the first point if required.
 */
static void PDFOutputPolyline (GKSC *gksc, int closed)
{
  PDFPoint *pptr   = (PDFPoint *) gksc->p.list, tpoint;
  PDFddp   *psa    = (PDFddp *) gksc->ddp;
  int      npoints = gksc->p.num, i;

/*
 *  Put the line out.
 */
  for (i = 0; i < npoints; i++) {
    if (i == 0) {
      PDFprint_points((PDFddp *) gksc->ddp, pptr+i, 1, MOVETO);
    }
    else {
      PDFprint_points((PDFddp *) gksc->ddp, pptr+i, 1, LINETO);
    }
  }
/*
 *  If a closed polygon is requested, draw a line from the last 
 *  point to the first if they are not the same.
 */
  if (closed) {
    if ((pptr[0].x != pptr[npoints-1].x) || (pptr[0].y != pptr[npoints-1].y)) {
      tpoint.x = pptr[npoints-1].x;
      tpoint.y = pptr[npoints-1].y;
      PDFprint_points(psa, &tpoint, 1, MOVETO);
      tpoint.x = pptr[0].x;
      tpoint.y = pptr[0].y;
      PDFprint_points(psa, &tpoint, 1, LINETO);
    }
  }
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"S\n");
  stream_size += 2;
}

/*
 *  Put out the polymarkers
 */
static void PDFOutputPolymarker (GKSC *gksc, int markersize, int markertype)
{
  PDFPoint *pptr = (PDFPoint *) gksc->p.list;
  PDFddp   *psa = (PDFddp *) gksc->ddp;
  int      npoints = gksc->p.num, i, tsize;
  int      hs, qs, px, py;

  /*
   *  Put the markers out.
   */
  tsize = MAX(1, (int) (1.7 * (float) markersize));
  hs    = 0.500*tsize;
  qs    = 0.707*hs;

  
  for (i = 0; i < npoints; i ++) {
    px = psa->dspace.llx + (int) (pptr[i].x * (float) psa->dspace.xspan);
    py = psa->dspace.lly + (int) (pptr[i].y * (float) psa->dspace.yspan);

    switch (markertype) {
    case    DOT_MARKER:      /* Dots cannot be scaled */
            tsize = MAX(1, (int) (.28/(psa->scaling)));
            PDFPutCircle(px, py, tsize);
            break;
    case    PLUS_MARKER:
            PDFPutLine(px+hs, py,    px-hs, py   );
            PDFPutLine(px   , py-hs, px,    py+hs);
            break;
    case    STAR_MARKER:
            PDFPutLine(px   , py+hs, px   , py-hs);
            PDFPutLine(px+qs, py+qs, px-qs, py-qs);
            PDFPutLine(px-qs, py+qs, px+qs, py-qs);
            break;
    case    CIRCLE_MARKER:
            PDFPutCircle(px, py, hs);
            break;
    case    X_MARKER:
            PDFPutLine(px+qs, py+qs, px-qs, py-qs);
            PDFPutLine(px-qs, py+qs, px+qs, py-qs);
            break;
    default:
            PDFPutLine(px+hs, py, px-hs, py);
            PDFPutLine(px+qs, py+qs, px-qs, py+qs);
            PDFPutLine(px-qs, py+qs, px+qs, py-qs);
            break;
    }
  }
}

/*ARGSUSED*/
int PDFOpenWorkstation(GKSC *gksc)
{
  char    *sptr = (char *) gksc->s.list;
  PDFddp  *psa;
  char    *ctmp;
  FILE    *fp;
  int     *pint, wks_type;
  extern  int     orig_wks_id;
  _NGCesc *cesc;

  psa = (PDFddp *) malloc (sizeof (PDFddp));
  if (psa == (PDFddp *) NULL) {
    ESprintf(ERR_PDF_MEMORY, "PDF: malloc(%d)", sizeof(PDFddp));
    return(ERR_PDF_MEMORY);
  }

  gksc->ddp = (GKSC_Ptr) psa;

/*
 * Handle Initial C Escape Elements.
 * (none currently defined for ps - so all of them cause gerhnd.)
 */
  while((cesc = _NGGetCEscInit())){
    gerr_hand(182,11,NULL);
  }

  pint        = (int *)(gksc->i.list);
  wks_type    = *(pint+1);        
  psa->wks_id = orig_wks_id;
  psa->color  = COLOR;
  psa->type   = PDF14;

  if (wks_type == 11) {
    psa->orientation = PORTRAIT;
    port_land = PORTRAIT;
  }
  else if (wks_type == 12) {
    psa->orientation = LANDSCAPE;
    port_land = LANDSCAPE;
  }
  else {
    ESprintf(ERR_OPN_PDF, "PDF: invalid workstation type: %d\n", wks_type);
  }

  psa->output_file = PDFGetFileName(psa->wks_id, sptr);
  if (strncmp(psa->output_file, "stdout", 6) == 0) {
    fp = stdout;
  }
  else {
    fp = fopen(psa->output_file,"w");
  }

  if (fp == (FILE *) NULL) {
    ctmp = (char *) calloc(strlen(psa->output_file)+3, 1);
    strcat(ctmp,"\"");
    strcat(ctmp+1,psa->output_file);
    strcat(ctmp+1+strlen(psa->output_file),"\"");
    ESprintf(ERR_OPN_PDF, "PDF: fopen(%s, \"w\")", ctmp);
    free(ctmp);
    return(ERR_OPN_PDF);
  }
  psa->file_pointer = fp;
        
  PDFinit(psa, pint+2);       /* Initialize local data. */

  /* 
   *  Initialize all transformations as well as the device coordinate
   *  space (store these in the device dependent data).
   */
  TransformSetWindow(&(psa->tsystem), 0.0, 0.0, 1.0, 1.0);
  TransformSetViewport(&(psa->tsystem),0.0, 0.0, 1.0, 1.0);
  TransformSetNDScreenSpace(&(psa->tsystem),0.0, 0.0, 1.0, 1.0);
  TransformSetScreenSpace(&(psa->tsystem),0.0, 0.0, 1.0, 1.0);

  psa->transform = TransformGetTransform(&psa->tsystem);

  PDFpreamble(psa, FOR_FILE);

  return(0);
}

/*ARGSUSED*/
int PDFActivateWorkstation(GKSC *gksc)
{
  return(0);
}

/*ARGSUSED*/
int PDFDeactivateWorkstation(GKSC *gksc)
{
  PDFddp   *psa;

  psa = (PDFddp *) gksc->ddp;

  (void) fflush(psa->file_pointer);
  return(0);
}

/*ARGSUSED*/
int PDFCloseWorkstation(GKSC *gksc)
{
  PDFddp  *psa;
  int     i, j, startxref;
  char    *tstring;
  FILE    *fp;

  psa = (PDFddp *) gksc->ddp;
  fp  = psa->file_pointer;

/*
 *  Put out the pages dictionary.
 */
  object_pointer[1] = byte_count;
  tstring = " 1 0 obj\n<<\n/Type /Pages\n/Kids [\n";
  fprintf(fp,"%s", tstring);
  byte_count += strlen(tstring);

  for (i = 0; i < psa->page_number-1; i++) {
    j = starting_page_object_number + 2*(i+1) - 1; 
    fprintf(fp,"%6d 0 R\n",j);
    byte_count += 11;
  }
  fprintf(fp,"]\n/Count %6d\n",psa->page_number-1);
  byte_count += 16;

  tstring = ">>\nendobj\n";
  fprintf(fp,"%s",tstring);
  byte_count += strlen(tstring);

/*
 *  Put out the catalog dictionary.
 */
  object_number +=2;
  object_pointer[object_number] = byte_count;
  fprintf(fp,"%6d 0 obj\n<<\n/Type /Catalog\n/Pages 1 0 R\n>>\nendobj\n",
          object_number);
  byte_count += 54;

/*
 *  Put out the xref table.
 */
  bump_object_number();
  startxref = byte_count;
  fprintf(fp,"xref\n");
  byte_count += 5;
  fprintf(fp,"0 %6d\n0000000000 65535 f \n",object_number);
  byte_count += 29;
  for (i = 1; i < object_number; i++) {
    fprintf(fp,"%010d 00000 n \n",object_pointer[i]);
  }
  
/*
 *  Put out the trailer.
 */
  fprintf(fp,"trailer\n<<\n/Size %6d\n/Root %6d 0 R\n>>\n",
          object_number,object_number-1);

/*
 *  Put out the startxref pointer.
 */
  fprintf(fp,"startxref\n%10d\n%%%%EOF\n",startxref);

  (void) fflush(fp);
  psa->pict_empty = TRUE;

/*
 *  Free allocated memory.
 */
  free(psa);                             
  for (i = 0; i < maximum_lines; i++) {  
    free(page_lines[i]);                
  }                                    
  free(page_lines);                   
  free(object_pointer);              
  fclose(fp);
  return(0);
}

/*ARGSUSED*/
int PDFClearWorkstation(GKSC *gksc)
{
  PDFddp  *psa;
  psa = (PDFddp *) gksc->ddp;

/*
 *  Put out a picture preamble if the picture is empty.
 */
  if (psa->pict_empty) {
    PDFpreamble(psa, FOR_PICTURE);
    psa->pict_empty = FALSE;
  }

/*
 *  Stroke the current path in case it has not been stroked.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"S\n");
  stream_size += 2;

/*
 *  Restore the graphics state to what it was at the stream start.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"Q\n");
  stream_size += 2;
  restore_level--;
  if (restore_level != 0) {
    fprintf(stderr,"PDF - restore level unmatched at stream end\n");
  }

/*
 *  Write out the current page stream.
 *
 */
  (void) PDFPutStream(psa->file_pointer);

  (void) fflush(psa->file_pointer);
  psa->pict_empty = TRUE;
  psa->page_number++;
  return(0);
}


/*ARGSUSED*/
int PDFPolyline(GKSC *gksc)
{
  PDFddp  *psa;
  int     pdf_linewidth, requested_color, current_color, requested_type;

  psa = (PDFddp *) gksc->ddp;

  if (psa->pict_empty) {
    PDFpreamble(psa, FOR_PICTURE);
    psa->pict_empty = FALSE;
  }

  requested_color = psa->attributes.line_colr_ind;
  current_color = psa->attributes.pdf_colr_ind;
  if (requested_color != current_color) {
    index2rgb(psa->color_map, requested_color, &red, &green, &blue);
    if (co_model != 0) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",
              red,green,blue);
      stream_size += 21;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",
              red,green,blue);
      stream_size += 21;
    }
    else {
      rgb2cmyk(red,green,blue,&cyan,&magenta,&yellow,&black);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
    }
    psa->attributes.pdf_colr_ind = requested_color;
  }

  pdf_linewidth = (int) ((psa->nominal_width_scale) * 
                          (psa->attributes.linewidth)/(psa->scaling));
  pdf_linewidth = MAX(1, pdf_linewidth);
  if (pdf_linewidth != psa->attributes.linewidth_set) {
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%6d w\n",pdf_linewidth);
    stream_size += 9;
    psa->attributes.linewidth_set = pdf_linewidth;

  }

  requested_type = psa->attributes.linetype;
  if (requested_type != psa->attributes.linetype_set) {
    PDFset_dashpattern(psa);
    psa->attributes.linetype_set = requested_type;
  }

  if ((psa->pdf_clip.null) && (psa->attributes.clip_ind == CLIPPING_ON)) 
    return(0);
                
  PDFOutputPolyline (gksc,0);
  return(0);
}

/*ARGSUSED*/
int PDFPolymarker(GKSC *gksc)
{

  PDFddp  *psa;
  int     markersize, markertype, requested_color, current_color;
  int     linewidth;

  psa = (PDFddp *) gksc->ddp;

  if (psa->pict_empty) {
    PDFpreamble(psa, FOR_PICTURE);
    psa->pict_empty = FALSE;
  }

  requested_color = psa->attributes.marker_colr_ind;
  current_color = psa->attributes.pdf_colr_ind;
  if (requested_color != current_color) {
    index2rgb(psa->color_map, requested_color, &red, &green, &blue);

    if (co_model != 0) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",
              red,green,blue);
      stream_size += 21;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",
              red,green,blue);
      stream_size += 21;
    }
    else {
      rgb2cmyk(red,green,blue,&cyan,&magenta,&yellow,&black);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
    }
    psa->attributes.pdf_colr_ind = requested_color;
  }

/*
 * Nominal length of marker element is six units in the default
 * PDF user coordinate space (half width is three units).
 */
  markersize = 3 * (int) ((psa->transform.y_scale) *
                          (psa->attributes.marker_size)/(psa->scaling));
  markertype = psa->attributes.marker_type;

/*
 *  Save the current graphics state and set up the line 
 *  attributes to be used.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"q\n");
  stream_size += 2;
  restore_level++;

  bump_page_lines();
  sprintf(page_lines[num_page_lines],"[] 0 d\n");
  stream_size += 7;

  bump_page_lines();
  linewidth = (int) ((psa->nominal_width_scale) *
                    (LINEWIDTH_DEFAULT/(psa->scaling)));
  sprintf(page_lines[num_page_lines],"%6d w\n",linewidth);
  stream_size += 9;

  if ((psa->pdf_clip.null) && 
          (psa->attributes.clip_ind == CLIPPING_ON)) return(0);

  PDFOutputPolymarker (gksc, markersize, markertype);
 
/*
 *  Restore original linewith and type.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"Q\n");
  stream_size += 2;
  restore_level--;

  return(0);
}

/*ARGSUSED*/
int PDFText(GKSC *gksc)
{
  PDFPoint    *pptr = (PDFPoint *) gksc->p.list;
  char        *sptr = (char *) gksc->s.list;
  char ctmp,  *tptr;
  PDFddp      *psa;
  PDFCharInfo fc;
  PDFTextent  textent;
  int     requested_color, current_color;
  int     PDFFontScale, PDFCharHeight, PDFCharSpace;
  int     i, j, found, x_position, y_position, y_inc, return_value=0;
  int     string_height=0, max_char_width, char_spacing=0;
  int     num_chars, char_num, rchars, old_font=0, current_font, strpos, fcount;
  int     x_orig, y_orig, x_offset_to_orig, y_offset_to_orig, xpos, ypos;
  int     text_linewidth;

  float   xoffset=0.0, yoffset=0.0, vert_offset=0.0;
  float   nominal_vert_adjust, tdiff, bdiff, char_expn;
  float   tm_a, tm_b, tm_c, tm_d, tmp1, aspect_ratio;
  float   fc2wsvp, string_height_wsvp, char_spacing_wsvp;
  float   max_char_width_wsvp;
  float   up_x, up_y, base_x, base_y, mag_up, mag_base, base2up_ratio;

  psa = (PDFddp *) gksc->ddp;

/*
 *  Save original coordinates, return if out-of-range.
 */
  x_orig = psa->dspace.llx + (int) (pptr[0].x * (float) psa->dspace.xspan);
  y_orig = psa->dspace.lly + (int) (pptr[0].y * (float) psa->dspace.yspan);
  if (abs(x_orig) > 99999 || abs(y_orig) > 99999) {
    return(0);
  }

  text_linewidth = 
     (int) (psa->nominal_width_scale * (LINEWIDTH_DEFAULT/(psa->scaling)));
  num_chars = strlen(sptr);
  if (num_chars == 0) {
    return(0);
  }

  if (psa->pict_empty) {
    PDFpreamble(psa, FOR_PICTURE);
    psa->pict_empty = FALSE;
  }

/*
 *  Stroke any open subpaths.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"S\n");
  stream_size += 2;

  requested_color = psa->attributes.text_colr_ind;
  current_color = psa->attributes.pdf_colr_ind;
  if (requested_color != current_color) {
    index2rgb(psa->color_map, requested_color, &red, &green, &blue);
    if (co_model != 0) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",
              red,green,blue);
      stream_size += 21;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",
              red,green,blue);
      stream_size += 21;
    }
    else {
      rgb2cmyk(red,green,blue,&cyan,&magenta,&yellow,&black);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
    }
    psa->attributes.pdf_colr_ind = requested_color;
  }

  up_x          = psa->attributes.char_up_vec_x;
  up_y          = psa->attributes.char_up_vec_y;
  base_x        = psa->attributes.char_base_vec_x;
  base_y        = psa->attributes.char_base_vec_y;
  mag_up        = MAG(up_x, up_y);
  mag_base      = MAG(base_x, base_y);
  base2up_ratio = mag_base/mag_up;
  PDFCharHeight = (int)((psa->attributes.char_ht)*(float) (psa->dspace.yspan));

/*
 *  Find the text extent boundaries in font coordinate units.
 *  This computation is complicated by the fact that the character 
 *  spacing may cause the left extent of the string to move to the 
 *  left instead of right, and similarly for the top extent.
 */
  aspect_ratio = ((float)(psa->dspace.yspan))/((float)(psa->dspace.xspan));
  for (i = 0, textent.left = 50000, textent.right = -50000, 
      textent.top = 50000, textent.bottom = -50000,
      x_position = 0, y_position = 0, max_char_width = 0; i < num_chars; i++) {
    strncpy(&ctmp, sptr+i, 1);      

/*
 *  Find the equivalent PostScript font and character to the
 *  current input character.
 */
    found = MapFonts(psa, (int) (ctmp & 255), &fc);
    if (found < 0) {
      return_value = ERR_PDF_CHAR;
    }

/*
 *  Use height of first character for subsequent computation
 *  of adjustment for vertical alignment.
 */
    if (i == 0) {     
      string_height = fc.font_height;
      char_spacing = (int)((psa->attributes.char_space) *
                       base2up_ratio * (float) string_height);
    }

    textent.left  = MIN(textent.left, x_position);
    textent.right = MAX(textent.right, x_position + fc.char_width);
    x_position += fc.char_width;

/*
 *  The vertical text extent is 1000 (or 0 if the character 
 *  is null) since that is what the font coordinates are 
 *  scaled by.
 */
    y_inc = ((fc.char_width == 0) ? 0 : 1000);
    textent.bottom = MAX(textent.bottom, y_position + y_inc);
    textent.top = MIN(textent.top, y_position);
    y_position += y_inc;
    max_char_width = MAX(max_char_width, fc.char_width);

    if (i == (num_chars - 1)) {
            break;
    }
    x_position += char_spacing;
    y_position += char_spacing;
  }

  textent.left  = (int) ((float) textent.left  * aspect_ratio);
  textent.right = (int) ((float) textent.right * aspect_ratio);

/*
 *  Convert character height to proper scalefont argument.  Scale
 *  the PS font by a factor SC so that  (font_height/1000.)*SC equals
 *  the PS character height.
 */
  PDFFontScale = (int)(1000. * (float) PDFCharHeight) / 
                                          ((float)(fc.font_height));

/*
 *  Scale factor for converting font coordinate units to 
 *  workstation viewport units.
 */
  fc2wsvp = (0.001 * (float) PDFFontScale) / (psa->dspace.yspan);

  textent.left_wsvp   = (float) textent.left * fc2wsvp;
  textent.right_wsvp  = (float) textent.right * fc2wsvp;
  textent.top_wsvp    = (float) textent.top * fc2wsvp;
  textent.bottom_wsvp = (float) textent.bottom * fc2wsvp;
  string_height_wsvp  = (float) string_height * fc2wsvp;
  char_spacing_wsvp   = (float) char_spacing * fc2wsvp;
  max_char_width_wsvp = (float) max_char_width * fc2wsvp;
  tdiff               = .43 * ((1000./string_height) - 1.);
  bdiff               = .57 * ((1000./string_height) - 1.);

/*
 *  Save the current graphics state.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"q\n");
  stream_size += 2;
  restore_level++;

/*
 *  Translate the axes to the current point, scale as per
 *  the character expansion factor, rotate in accordance
 *  with the base vector, and deform as per the angle
 *  between the base and up vector.
 */

/*
 *  Normalize up and base vectors, calculate the transformation
 *  matrix.
 */
  up_x = up_x/mag_up;
  up_y = up_y/mag_up;
  base_x = base_x/mag_base;
  base_y = base_y/mag_base;
  char_expn = psa->attributes.char_expan;
  tm_a = char_expn * base2up_ratio * base_x;
  tm_b = char_expn * base2up_ratio * base_y;
  tmp1 = up_x * base_x + up_y * base_y;
  tm_c = tm_a * tmp1 + base_y * (up_x * base_y - up_y * base_x);
  tm_d = tm_b * tmp1 + base_x * (up_y * base_x - up_x * base_y);


/*
 *  Adjust the input coordinate in accordance with the horizontal
 *  and vertical alignments.
 */
  if (((psa->attributes.text_path) == RIGHT_TEXT_PATH) ||
      ((psa->attributes.text_path) == LEFT_TEXT_PATH)) {
    switch (psa->attributes.text_align_horiz) {
    case NORMAL_ALIGNMENT_HORIZ:
      if ((psa->attributes.text_path) == RIGHT_TEXT_PATH) {
        xoffset = textent.left_wsvp;
      }
      else if ((psa->attributes.text_path) == LEFT_TEXT_PATH) {
        xoffset = textent.right_wsvp;
      }
        break;
    case LEFT_ALIGNMENT_HORIZ:
      xoffset = textent.left_wsvp;
      break;
    case CENTER_ALIGNMENT_HORIZ:
      xoffset = 0.5*(textent.left_wsvp + textent.right_wsvp);
      break;
    case RIGHT_ALIGNMENT_HORIZ:
      xoffset = textent.right_wsvp;
      break;
    }
    pptr[0].x = -xoffset;
  }
  else if (((psa->attributes.text_path) == UP_TEXT_PATH) ||
      ((psa->attributes.text_path) == DOWN_TEXT_PATH)) {
    switch (psa->attributes.text_align_horiz) {
    case NORMAL_ALIGNMENT_HORIZ:
      xoffset = 0.;
      break;
    case LEFT_ALIGNMENT_HORIZ:
      xoffset = -.5 * max_char_width_wsvp;
      break;
    case CENTER_ALIGNMENT_HORIZ:
      xoffset = 0.;
      break;
    case RIGHT_ALIGNMENT_HORIZ:
      xoffset =  .5 * max_char_width_wsvp;
      break;
    }
    pptr[0].x = -xoffset;
  }

  if (((psa->attributes.text_path) == RIGHT_TEXT_PATH) ||
      ((psa->attributes.text_path) == LEFT_TEXT_PATH)) {
    switch (psa->attributes.text_align_vert) {
    case NORMAL_ALIGNMENT_VERT:
      yoffset = 0.;
      break;
    case TOP_ALIGNMENT_VERT:
      yoffset = (1. + tdiff) * string_height_wsvp;
      break;
    case CAP_ALIGNMENT_VERT:
      yoffset = string_height_wsvp;
      break;
    case HALF_ALIGNMENT_VERT:
      yoffset = 0.5 * string_height_wsvp;
      break;
    case BASE_ALIGNMENT_VERT:
      yoffset = 0.;
      break;
    case BOTTOM_ALIGNMENT_VERT:
      yoffset = -bdiff * string_height_wsvp;
      break;
    }
    pptr[0].y = -yoffset;
  }
  else if (((psa->attributes.text_path) == UP_TEXT_PATH) ||
      ((psa->attributes.text_path) == DOWN_TEXT_PATH)) {
    nominal_vert_adjust = bdiff * string_height_wsvp + char_spacing_wsvp;
    switch (psa->attributes.text_align_vert) {
    case NORMAL_ALIGNMENT_VERT:
      if ((psa->attributes.text_path) == UP_TEXT_PATH) {
          vert_offset = -textent.bottom_wsvp - char_spacing_wsvp;
      }
      else if ((psa->attributes.text_path) == DOWN_TEXT_PATH) {
          vert_offset = -nominal_vert_adjust - textent.top_wsvp;
      }
      break;
    case TOP_ALIGNMENT_VERT:
      vert_offset = -nominal_vert_adjust - textent.top_wsvp;
      break;
    case CAP_ALIGNMENT_VERT:
      vert_offset = -nominal_vert_adjust - 
                    tdiff * string_height_wsvp - textent.top_wsvp;
      break;
    case HALF_ALIGNMENT_VERT:
      vert_offset = -nominal_vert_adjust - 
                    0.5 * (textent.bottom_wsvp + textent.top_wsvp);
      break;
    case BASE_ALIGNMENT_VERT:
      vert_offset = -textent.bottom_wsvp - char_spacing_wsvp;
      break;
    case BOTTOM_ALIGNMENT_VERT:
      vert_offset = -nominal_vert_adjust - textent.bottom_wsvp;
      break;
    }
    pptr[0].y = -vert_offset;
  }
  /*
   *  Reverse the characters in the input string if the
   *  text path is left or up.
   */
  if ((psa->attributes.text_path) == LEFT_TEXT_PATH ||
       (psa->attributes.text_path) == UP_TEXT_PATH ) {
          PDFreverse_chars(sptr);
  }

/*
 *  Adjust the original position with the offset.
 */
  x_offset_to_orig = (int) (((float) (psa->dspace.xspan)) * pptr[0].x); 
  y_offset_to_orig = (int) (((float) (psa->dspace.yspan)) * pptr[0].y); 

  PDFCharSpace = (int) ((float) PDFCharHeight * base2up_ratio *
                  (psa->attributes.char_space));
  
/*
 *  Put out the string accounting for the fact that a string
 *  in a single GKS font may result in strings from several
 *  PostScript fonts.
 */
  if ((psa->pdf_clip.null) && 
          (psa->attributes.clip_ind == CLIPPING_ON)) return(0);

  for (i = 0, strpos = 0, fcount = 0; i < num_chars; i++) {
    strncpy(&ctmp, sptr+i, 1);
    found = MapFonts(psa, (int) (ctmp & 255), &fc);
    if (i == 0) {
            old_font = fc.font;
    }
    fcount++;
    current_font = fc.font;
    if ((current_font != old_font) || (i == (num_chars-1))) {
      if (i == (num_chars-1)) {
        fcount++;
        old_font = current_font;
      }
      tptr = (char *) calloc(sizeof(char), fcount);
      for (j = 0; j < fcount-1; j++) {
        strncpy(&ctmp, sptr+strpos+j, 1);
        found = MapFonts(psa, (int) (ctmp & 255), &fc);
        *(tptr+j) = (char) fc.char_num;
      }

      if (((psa->attributes.text_path) == RIGHT_TEXT_PATH) ||
         ((psa->attributes.text_path) == LEFT_TEXT_PATH)) {

        bump_page_lines();
        sprintf(page_lines[num_page_lines],"S\n");
        stream_size += 2;
        bump_page_lines();
        sprintf(page_lines[num_page_lines],"BT\n");
        stream_size += 3;

        bump_page_lines();
        sprintf(page_lines[num_page_lines],"%6d w\n",text_linewidth);
        stream_size += 9;


/*
 *  Set the outline flag (keep this code block contiguous).
 */
        bump_page_lines();
        if (fc.outline == FALSE) {
          sprintf(page_lines[num_page_lines],"2 Tr\n");
        }
        else {
          sprintf(page_lines[num_page_lines],"1 Tr\n");
        }
        stream_size += 5;

        bump_page_lines();
        sprintf(page_lines[num_page_lines],"%s %6d Tf\n",
                PDFFontNames[old_font], PDFFontScale);
        stream_size += strlen(PDFFontNames[old_font]) + 1 + 6 + 4;

        bump_page_lines();
        sprintf(page_lines[num_page_lines],
          "%10.3f %10.3f %10.3f %10.3f %6d %6d Tm\n", 
          tm_a, tm_b, tm_c, tm_d, x_orig, y_orig);
        stream_size += 61;

        bump_page_lines();
        sprintf(page_lines[num_page_lines],"%6d %6d Td\n",
            x_offset_to_orig,y_offset_to_orig);
        stream_size += 17;

        if (char_spacing != 0) {
          bump_page_lines();
          sprintf(page_lines[num_page_lines],"%6d Tc\n",char_spacing);
          stream_size += 10;
        }

        bump_page_lines();
        sprintf(page_lines[num_page_lines],"(");
        stream_size += 1;
        rchars = PDFoutput_string(psa,tptr);
        sprintf((page_lines[num_page_lines]+rchars),") Tj\nET\n");
        stream_size += 8;

        for (j = 0; j < strlen(tptr); j++) {
          strncpy(&ctmp, tptr+j, 1);
          found = MapFonts(psa, (int) (ctmp & 255), &fc);
          if ((psa->attributes.text_path) == RIGHT_TEXT_PATH) {
            x_offset_to_orig += 
              (int) (((float) (psa->dspace.xspan)) * fc2wsvp * fc.char_width);
          }
          else {
            x_offset_to_orig -= 
              (int) (((float) (psa->dspace.xspan)) * fc2wsvp * fc.char_width);
          }
        }
      }
      else if (((psa->attributes.text_path)==UP_TEXT_PATH) ||
          ((psa->attributes.text_path) == DOWN_TEXT_PATH)) {
        for (char_num = 0; char_num < num_chars; char_num++) {
          strncpy(&ctmp, sptr+char_num, 1);
          found = MapFonts(psa, (int) (ctmp & 255), &fc);
          bump_page_lines();
          sprintf(page_lines[num_page_lines],"BT\n");
          stream_size += 3;

          bump_page_lines();
          if (fc.outline == FALSE) {
            sprintf(page_lines[num_page_lines],"2 Tr\n");
          }
          else {
            sprintf(page_lines[num_page_lines],"1 Tr\n");
          }
          stream_size += 5;

          bump_page_lines();
          sprintf(page_lines[num_page_lines],"%s %6d Tf\n",
                  PDFFontNames[old_font], PDFFontScale);
          stream_size += strlen(PDFFontNames[old_font]) + 1 + 6 + 4;

          bump_page_lines();
          sprintf(page_lines[num_page_lines],
            "%10.3f %10.3f %10.3f %10.3f %6d %6d Tm\n", 
            tm_a, tm_b, tm_c, tm_d, x_orig, y_orig);
          stream_size += 61;

          bump_page_lines();
          sprintf(page_lines[num_page_lines],"%6d %6d Td\n",
              x_offset_to_orig,y_offset_to_orig);
/*
 *  I don't understand why there is a 0.25 factor below, rather than
 *  a 0.50, but it seems to work.
 */
          xpos = -0.25 * fc.char_width * aspect_ratio;
          ypos = -(char_num+1)*(PDFFontScale+PDFCharSpace);
          stream_size += 17;

          bump_page_lines();
          sprintf(page_lines[num_page_lines],"%6d %6d Td\n",xpos,ypos);
          stream_size += 17;

          bump_page_lines();
          sprintf(page_lines[num_page_lines],
                      "(%c) Tj\nET\n",*(tptr+char_num));
          stream_size += 10;
        }
      }

      free(tptr);
      old_font = current_font;
      strpos = strpos + fcount - 1;
      fcount = 1;
    }
  }

/*
 *  Restore the graphics state.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"Q\n");
  stream_size += 2;
  restore_level--;

  return(return_value);
}

/*ARGSUSED*/
int PDFFillArea(GKSC *gksc)
{
  PDFddp   *psa  = (PDFddp *) gksc->ddp;
  PDFPoint *pptr = (PDFPoint *) gksc->p.list;
  int     requested_color, current_color;
  int     npoints = gksc->p.num, i, linewidth;

  if (psa->pict_empty) {
    PDFpreamble(psa, FOR_PICTURE);
    psa->pict_empty = FALSE;
  }

  requested_color = psa->attributes.fill_colr_ind;
  current_color = psa->attributes.pdf_colr_ind;
  if (requested_color != current_color) {
    index2rgb(psa->color_map, requested_color, &red, &green, &blue);

    if (co_model != 0) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",
              red,green,blue);
      stream_size += 21;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",
              red,green,blue);
      stream_size += 21;
    }
    else {
      rgb2cmyk(red,green,blue,&cyan,&magenta,&yellow,&black);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
    }

    psa->attributes.pdf_colr_ind = requested_color;
  }

/*
 *  Save the current graphics state and set up the line 
 *  attributes to be used.
 */
  if (psa->attributes.fill_int_style != SOLID_FILL && psa->attributes.fill_int_style != SOLID_TEXT_FILL) {
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"q\n");
    stream_size += 2;
    restore_level++;

    bump_page_lines();
    sprintf(page_lines[num_page_lines],"[] 0 d\n");
    stream_size += 7;

    bump_page_lines();
    linewidth = (int) (1.1*(psa->dspace.yspan)*(psa->sfill_spacing));
    sprintf(page_lines[num_page_lines],"%6d w\n",linewidth);
    stream_size += 9;
  }

  if ((psa->pdf_clip.null) && 
          (psa->attributes.clip_ind == CLIPPING_ON)) return(0);

  switch(psa->attributes.fill_int_style) {
  case HOLLOW_FILL:   /* Put out polyline */
    PDFOutputPolyline (gksc, 1);
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"S\n");
    stream_size += 2;
    break;
  case SOLID_FILL:
  case SOLID_TEXT_FILL:    /* Jira1667 */
    for (i = 0; i < npoints; i++) {
      if (i == 0) {
        PDFprint_points((PDFddp *) gksc->ddp, pptr+i, 1, MOVETO);
      }
      else {
        PDFprint_points((PDFddp *) gksc->ddp, pptr+i, 1, LINETO);
      }
    }
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"f*\n");
    stream_size += 3;
    break;
  case PATTERN_FILL:  /* currently not implemented, issue polyline */
    PDFOutputPolyline (gksc, 1);
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"S\n");
    stream_size += 2;
    break;
  case HATCH_FILL:
    switch (psa->attributes.fill_style_ind) {
    case HORIZONTAL_HATCH:
      pdf_SoftFill (gksc,   0., psa->hatch_spacing);  
      break;
    case VERTICAL_HATCH:
      pdf_SoftFill (gksc,  90., psa->hatch_spacing);  
      break;
    case POSITIVE_HATCH:
      pdf_SoftFill (gksc,  45., psa->hatch_spacing);  
      break;
    case NEGATIVE_HATCH:
      pdf_SoftFill (gksc, 135., psa->hatch_spacing);  
      break;
    case HORIZ_VERT_HATCH:
      pdf_SoftFill (gksc,   0., psa->hatch_spacing);  
      pdf_SoftFill (gksc,  90., psa->hatch_spacing);  
      break;
    case POS_NEG_HATCH:
      pdf_SoftFill (gksc,  45., psa->hatch_spacing);  
      pdf_SoftFill (gksc, 135., psa->hatch_spacing);  
      break;
    default:
      PDFOutputPolyline (gksc, 1);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"S\n");
      stream_size += 2;
      break;
    }
    break;
  default:
    PDFOutputPolyline (gksc, 1);
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"S\n");
    stream_size += 2;
    break;
  }

/*
 *  Restore line attributes.
 */
  if (psa->attributes.fill_int_style != SOLID_FILL && psa->attributes.fill_int_style != SOLID_TEXT_FILL) {
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"Q\n");
    stream_size += 2;
    restore_level--;
  }

  return(0);
}


/*ARGSUSED*/
int PDFCellarray(GKSC *gksc)
{
  PDFPoint        *pptr = (PDFPoint *) gksc->p.list;
  int             *iptr = (int *) gksc->i.list;
  PDFddp  *psa = (PDFddp *) gksc->ddp;

  int     *xptr = (int *) gksc->x.list;  /* color index array */

  int     nx = iptr[0];   /* number of cols       */
  int     ny = iptr[1];   /* number of rows       */

  int     index;
  int     i, j, color_index, xtr, ytr, line_offset;
  float   x_scale, y_scale;
  float   tred, tgreen, tblue;

  PDFPoint *Pptr = &pptr[0];
  PDFPoint *Qptr = &pptr[1];
  PDFPoint *Rptr = &pptr[2];

  if (psa->pict_empty) {
    PDFpreamble(psa, FOR_PICTURE);
    psa->pict_empty = FALSE;
  }

  if ((psa->pdf_clip.null) && 
        (psa->attributes.clip_ind == CLIPPING_ON)) return(0);

  x_scale = ((Rptr->x - Pptr->x) * (psa->dspace.xspan));
  y_scale = ((Qptr->y - Rptr->y) * (psa->dspace.yspan));

/*
 *  Stroke and save the current graphics state.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"S\n");
  stream_size += 2;
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"q\n");
  stream_size += 2;
  restore_level++;

/*
 *  Translate and scale (the matrix is a composition of the
 *  matrix [1./nx 0 0 -1/ny 0 1], that maps the image space to
 *  the unit square in user space, and the matrix 
 *  [x_scale 0 0 y_scale xtr ytr] that maps the unit square
 *  to the proper position in user space.  Note that the
 *  (0,0) coordinate in image space is at the upper left 
 *  for PDF, opposed to the lower left for PostScript.
 */
  xtr = (psa->dspace.llx) + (int)(((float)(psa->dspace.xspan))*Pptr[0].x);
  ytr = (psa->dspace.lly) + (int)(((float)(psa->dspace.xspan))*Qptr[0].y);
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%15.6f 0. 0. %15.6f %6d %6d cm\n",
          x_scale, -y_scale, xtr, ytr);
  stream_size += 55;

/*
 *  Put out the begin image.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"BI\n");
  stream_size += 3;
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"/W %6d\n",nx);
  stream_size += 10;
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"/H %6d\n",ny);
  stream_size += 10;
  bump_page_lines();
  if (co_model != 0) {
    sprintf(page_lines[num_page_lines],"/CS /RGB\n");
    stream_size += 9;
  }
  else {
    sprintf(page_lines[num_page_lines],"/CS /CMYK\n");
    stream_size += 10;
  }
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"/BPC 8\n");
  stream_size += 7;
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"/F [/ASCIIHexDecode]\n");
  stream_size += 21;

/*
 *  Put out the data.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"ID\n");
  stream_size += 3;
  bump_page_lines();
  line_offset = 0;
  for (i = 0, index = 0; i < ny; i++) {
    for (j = 0; j < nx; j++, index++) {
      if ( ((index % 6) == 0) && (index > 0)) {
        sprintf(page_lines[num_page_lines]+line_offset,"\n");
        stream_size += 1;
        bump_page_lines();
        line_offset = 0;
      }
      color_index = xptr[index];
      index2rgb(psa->color_map, color_index, &tred, &tgreen, &tblue);
      if (co_model != 0) {
        sprintf(page_lines[num_page_lines]+line_offset,"%02X%02X%02X",
          (int)(255. * tred),
          (int)(255. * tgreen),
          (int)(255. * tblue));
        stream_size += 6;
        line_offset += 6;
      }
      else {
        rgb2cmyk(tred,tgreen,tblue,&cyan,&magenta,&yellow,&black);
        sprintf(page_lines[num_page_lines]+line_offset,"%02X%02X%02X%02X",
          (int)(255. * cyan),
          (int)(255. * magenta),
          (int)(255. * yellow),
          (int)(255. * black));
        stream_size += 8;
        line_offset += 8;
      }
    }
  }
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"\n>\n");
  stream_size += 3;

/*
 *  End image, restore graphics state.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"EI\nQ\n");
  stream_size += 5;
  restore_level--;
  
  return(0);
}

/*ARGSUSED*/
int PDFSetLinetype(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *iptr = (int *) gksc->i.list;

  psa->attributes.linetype = iptr[0];
  return(0);
}

/*ARGSUSED*/
int PDFSetLineWidthScaleFactor(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  float    *fptr = (float *) gksc->f.list;

  psa->attributes.linewidth = fptr[0];
  return(0);
}


/*ARGSUSED*/
int PDFSetPolylineColorIndex(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *xptr = (int *) gksc->x.list;

  psa->attributes.line_colr_ind = xptr[0];
  return(0);
}

/*ARGSUSED*/
int PDFSetMarkerType(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *iptr = (int *) gksc->i.list;

  psa->attributes.marker_type = iptr[0];
  return(0);
}


/*ARGSUSED*/
int PDFSetMarkerSizeScaleFactor(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  float    *fptr = (float *) gksc->f.list;

  psa->attributes.marker_size = fptr[0];
  return(0);
}

/*ARGSUSED*/
int PDFSetPolymarkerColorIndex(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *xptr = (int *) gksc->x.list;

  psa->attributes.marker_colr_ind = xptr[0];
  return(0);
}


/*ARGSUSED*/
int PDFSetTextFontAndPrecision(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *iptr = (int *) gksc->i.list;

  psa->attributes.text_font = iptr[0];
  psa->attributes.text_prec = iptr[1];

  return(0);
}

/*ARGSUSED*/
int PDFSetCharacterExpansionFactor(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  float    *fptr = (float *) gksc->f.list;

  psa->attributes.char_expan = fptr[0];
  return(0);
}


/*ARGSUSED*/
int PDFSetCharacterSpacing(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  float    *fptr = (float *) gksc->f.list;

  psa->attributes.char_space = fptr[0];
  return(0);
}

/*ARGSUSED*/
int PDFSetTextColorIndex(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *xptr = (int *) gksc->x.list;

  psa->attributes.text_colr_ind = xptr[0];
  return(0);
}


/*ARGSUSED*/
int PDFSetCharacterHeightAndUpVector(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  float           *fptr = (float *) gksc->f.list;

  double          up_x = (double) fptr[0];
  double          up_y = (double) fptr[2];
  double          base_x = (double) fptr[1];
  double          base_y = (double) fptr[3];

/*
 *  Transform to workstation viewport space.
 */
  up_x *= (psa->transform).x_scale;
  up_y *= (psa->transform).y_scale;
  base_x *= (psa->transform).x_scale;
  base_y *= (psa->transform).y_scale;
  psa->attributes.char_up_vec_x = (float) up_x;
  psa->attributes.char_up_vec_y = (float) up_y;
  psa->attributes.char_base_vec_x = (float) base_x;
  psa->attributes.char_base_vec_y = (float) base_y;

  psa->attributes.char_ht = MAG(up_x,up_y);

  return(0);
}

/*ARGSUSED*/
int PDFSetTextPath(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *iptr = (int *) gksc->i.list;

  psa->attributes.text_path = iptr[0];
  return(0);
}


/*ARGSUSED*/
int PDFSetTextAlignment(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *iptr = (int *) gksc->i.list;

  psa->attributes.text_align_horiz = iptr[0];
  psa->attributes.text_align_vert = iptr[1];
  return(0);
}

/*ARGSUSED*/
int PDFSetFillAreaInteriorStyle(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *iptr = (int *) gksc->i.list;

  psa->attributes.fill_int_style = iptr[0];
  return(0);
}


/*ARGSUSED*/
int PDFSetFillAreaStyleIndex(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int      *iptr = (int *) gksc->i.list;
  psa->attributes.fill_style_ind = iptr[0];
  return(0);
}

/*ARGSUSED*/
int PDFSetFillAreaColorIndex(GKSC *gksc)
{
  PDFddp *psa = (PDFddp *) gksc->ddp;

  int    *xptr = (int *) gksc->x.list;

  psa->attributes.fill_colr_ind = xptr[0];
  return(0);
}


/*ARGSUSED*/
int PDFSetColorRepresentation(GKSC *gksc)
{
  PDFddp    *psa = (PDFddp *) gksc->ddp;

  int       *xptr = (int *) gksc->x.list;
  PDFColor  *rgbptr = (PDFColor *) gksc->rgb.list;

  unsigned  index   = (unsigned) xptr[0];

  if (index & ARGB_MASK)  /* ARGB value vs. index?  */
      return 1;

  float     r =  rgbptr[0].r;
  float     g =  rgbptr[0].g;
  float     b =  rgbptr[0].b;

  psa->color_map[3*index  ] = r;
  psa->color_map[3*index+1] = g;
  psa->color_map[3*index+2] = b;
  psa->color_map[index+3*MAX_COLORS] = 1;

  if ((index == 0)  && (psa->suppress_flag != 1) &&
                       (psa->suppress_flag != 2)) {
/*
 *  Do not flag the background setting if the defined
 *  color is white.
 */
     if ((psa->color_map[0] != 1.) || (psa->color_map[1] != 1.) ||
         (psa->color_map[2] != 1.)) {
            psa->background = TRUE;
     }
  }

/*
 *  If the index being defined is the same as the current
 *  color, reset the current color to the newly defined value.
 */
  if (psa->attributes.pdf_colr_ind == (int) index ) {
    if (co_model != 0) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",r,g,b);
      stream_size += 21;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",r,g,b);
      stream_size += 21;
    }
    else {
      rgb2cmyk(red,green,blue,&cyan,&magenta,&yellow,&black);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
    }
  }
  return(0);
}

/*ARGSUSED*/
int PDFSetClipIndicator(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  int     *iptr = (int *) gksc->i.list, np = 2;
  int     orig_clip_ind, rec_chg = 0, default_rect = 0;

  float   tclipx[2], tclipy[2];

  PDFClipRect *Crect;

/*
 *  Check to see if the PDF clipping rectangle before 
 *  this call is the default rectangle.
 */
  if ((psa->pdf_clip.llx == psa->dspace.llx) && 
      (psa->pdf_clip.lly == psa->dspace.lly) &&
      (psa->pdf_clip.urx == psa->dspace.urx) &&
      (psa->pdf_clip.ury == psa->dspace.ury)) {
          default_rect = 1;
  }

/*
 *  Save the current GKS viewport rectangle and current clip
 *  indicator.  The GKS clipping rectangle is stored in ndc.
 */
  PDFConvPoints(gksc->ddp, tclipx, tclipy, &gksc->p, &np, COOKED_TO_RAW);
  psa->gks_clip.llx = tclipx[0];
  psa->gks_clip.lly = tclipy[0];
  psa->gks_clip.urx = tclipx[1];
  psa->gks_clip.ury = tclipy[1];
  orig_clip_ind = psa->attributes.clip_ind;
  psa->attributes.clip_ind = iptr[0];

/*
 *  Calculate the new PDF clip rectangle and check to
 *  to see if it has changed and store it if it has.
 */
  Crect = GetPDFClipping (psa, psa->gks_clip, psa->tsystem.window);
  if ((Crect->llx != psa->pdf_clip.llx) || 
      (Crect->lly != psa->pdf_clip.lly) ||
      (Crect->urx != psa->pdf_clip.urx) ||
      (Crect->ury != psa->pdf_clip.ury)) {
        psa->pdf_clip.llx = Crect->llx;
        psa->pdf_clip.lly = Crect->lly;
        psa->pdf_clip.urx = Crect->urx;
        psa->pdf_clip.ury = Crect->ury;
        psa->pdf_clip.null = Crect->null;
        rec_chg = 1;
  }

  if (psa->pict_empty == FALSE) {
    if (iptr[0] == CLIPPING_ON) {
      if (orig_clip_ind == CLIPPING_OFF) {
        if (default_rect == 1) {
          return(0);
        }
        else {
          PDFOutputClipping (psa, PDF_CLIPPING_RECT);
        }
      }
      else {
        if (rec_chg == 1) { 
          PDFOutputClipping (psa, PDF_CLIPPING_RECT);
        }
        else {
          return(0);
        }
      }
    }
    else {
      if (orig_clip_ind == CLIPPING_OFF) {
              return(0);
      }
      else {
        if (default_rect == 1) { 
          return(0);
        }
        else {
          PDFOutputClipping (psa, PDF_DEFAULT_CLIPPING_RECT);
        }
      }
    }
  }
  return(0);
}


/*ARGSUSED*/
int PDFGetColorRepresentation(gksc)
        GKSC    *gksc;
{
        PDFddp   *psa = (PDFddp *) gksc->ddp;

        int     *xptr = (int *) gksc->x.list;
        PDFColor *rgbptr = (PDFColor *) gksc->rgb.list;

        int     index   = xptr[0];

        index2rgb(psa->color_map, index, &rgbptr[0].r, &rgbptr[0].g, &rgbptr[0].b);

        return(0);
}

void PDFNcarLogo(GKSC *gksc,float x,float y,float size)
{
/*  Currently not implemented.  What is here is header stuff  */
/*  that could be used when this is implemented.              */
  PDFddp  *psa;
  int     llx,lly,urx,ury,xspan,yspan;
  float   scaling,xp,yp,scale_factor;
  char    *translate,*scale;

  FILE    *fp;

  psa = (PDFddp *) gksc->ddp;

  if (psa->pict_empty == TRUE) {
     PDFpreamble(psa, FOR_PICTURE);
  }
                        
  fp = psa->file_pointer;
  scaling = psa->scaling;
  translate = (char *) calloc(33,sizeof(char));
  scale = (char *) calloc(29,sizeof(char));

  llx = psa->dspace.llx;
  lly = psa->dspace.lly;
  urx = psa->dspace.urx;
  ury = psa->dspace.ury;
  xspan = psa->dspace.xspan;
  yspan = psa->dspace.yspan;

  scale_factor = (scaling * size * yspan)/67.;

  xp = scaling*((float) llx + x*(float) xspan);
  yp = scaling*((float) lly + y*(float) yspan);
}

/*ARGSUSED*/
int PDFEsc(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;

  char    *sptr = (char *) gksc->s.list, *strng;
  int     *iptr = (int *) gksc->i.list;

  int     escape_id = iptr[0], plflag;
  float   rscale,logox,logoy,logos;
  static  int     saved_color_index;
  static  float   sred,sgreen,sblue;

  switch (escape_id) {
  case -1450:    /* no C-escapes implemented */
	break;
  case -1510:    /* Save color setting before segment copy */
    if (psa->pict_empty) {
      PDFpreamble(psa, FOR_PICTURE);
      psa->pict_empty = FALSE;
    }
    saved_color_index = psa->attributes.pdf_colr_ind;
    index2rgb(psa->color_map, saved_color_index, &sred, &sgreen, &sblue);
    break;
  case -1511:  /* Restore color setting after segment copy */
    psa->attributes.pdf_colr_ind = saved_color_index;

    if (co_model != 0) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",
              sred,sgreen,sblue);
      stream_size += 21;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",
              sred,sgreen,sblue);
      stream_size += 21;
    }
    else {
      rgb2cmyk(sred,sgreen,sblue,&cyan,&magenta,&yellow,&black);
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
              cyan,magenta,yellow,black);
      stream_size += 26;
    }
    break;
  case -1512:  /* Spacing between fill lines in range 0. to 1. */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->sfill_spacing = (float) atof(strng);
    break;
  case -1513:  /* Spacing between hatch lines in range 0. to 1. */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->hatch_spacing = (float) atof(strng);
    break;
  case -1514:  /* Size of operand stack */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->stack_size = (int) atoi(strng);
    break;
  case -1515:  /* Path size */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->path_size = (int) atoi(strng);
    break;
  case -1516:  /* Linewidth scale */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->nominal_width_scale = 1.0 * (float) atof(strng);
    break;
  case -1517:  /* Full background */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->full_background = (int) atoi(strng);
    break;
  case -1518:  /* Line joins */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->line_join = (linejoin_type) atoi(strng);
    if (psa->pict_empty == FALSE) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%2d j %2d J %10.3f M\n",
               psa->line_join, psa->line_cap, psa->miter_limit);
      stream_size += 23;
    }
    break;
  case -1519:  /* Line caps */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->line_cap = (linecap_type) atoi(strng);
    if (psa->pict_empty == FALSE) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%2d J\n", psa->line_cap);
      stream_size += 5;
    }
    break;
  case -1520:  /* Miter limit */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->miter_limit = (float) atof(strng);
    if (psa->pict_empty == FALSE) {
      bump_page_lines();
      sprintf(page_lines[num_page_lines],"%10.3f M\n", psa->miter_limit);
      stream_size += 13;
    }
    break;
  case -1521:  /* Corner points for positioning plot on the page */
    rscale = 1./psa->scaling;
    strng = strtok(sptr, " ");
    psa->dspace.llx = (int) (rscale * (float) atoi(strng));
    strng = strtok((char *) NULL, " ");
    psa->dspace.lly = (int) (rscale * (float) atoi(strng));
    strng = strtok((char *) NULL, " ");
    psa->dspace.urx = (int) (rscale * (float) atoi(strng));
    strng = strtok((char *) NULL, " ");
    psa->dspace.ury = (int) (rscale * (float) atoi(strng));

    psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
    psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

    psa->pdf_clip.llx = psa->dspace.llx;
    psa->pdf_clip.lly = psa->dspace.lly;
    psa->pdf_clip.urx = psa->dspace.urx;
    psa->pdf_clip.ury = psa->dspace.ury;
    psa->pdf_clip.null = FALSE;
    break;
  case -1524:  /* Suppress background color */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->suppress_flag = (int) atoi(strng);
    break;
  case -1525:  /* Specify portrait/landscape mode */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    plflag = (int) atoi(strng);
    if (plflag == 0) {
      psa->orientation = PORTRAIT;
      port_land = PORTRAIT;
    }
    else {
      psa->orientation = LANDSCAPE;
      port_land = LANDSCAPE;
    }
    break;
  case -1526:  /* Corner points for positioning plot on the page */
    rscale = 1./psa->scaling;
    strng = strtok(sptr, " ");  /* Skip over the workstation ID */
    strng = strtok((char *) NULL, " ");
    psa->dspace.llx = (int) (rscale * (float) atoi(strng));
    strng = strtok((char *) NULL, " ");
    psa->dspace.lly = (int) (rscale * (float) atoi(strng));
    strng = strtok((char *) NULL, " ");
    psa->dspace.urx = (int) (rscale * (float) atoi(strng));
    strng = strtok((char *) NULL, " ");
    psa->dspace.ury = (int) (rscale * (float) atoi(strng));

    psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
    psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

    psa->pdf_clip.llx = psa->dspace.llx;
    psa->pdf_clip.lly = psa->dspace.lly;
    psa->pdf_clip.urx = psa->dspace.urx;
    psa->pdf_clip.ury = psa->dspace.ury;
    psa->pdf_clip.null = FALSE;
    break;
  case -1527:  /* Positioning and size specifications for NCAR logo   */
               /* Currently not implemented - can't get here from awi */
    rscale = 1./psa->scaling;
    strng = strtok(sptr, " ");  /* Skip over the workstation ID */
    strng = strtok((char *) NULL, " ");
    logox = (float) atof(strng);
    strng = strtok((char *) NULL, " ");
    logoy = (float) atof(strng);
    strng = strtok((char *) NULL, " ");
    logos = (float) atof(strng);
    PDFNcarLogo(gksc,logox,logoy,logos); 
    break;
  case -1529:  /* Paper width */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->paper_width = (int) atoi(strng);
    break;
  case -1530:  /* Paper width */
    strng = strtok(sptr, " ");
    strng = strtok((char *) NULL, " ");
    psa->paper_height = (int) atoi(strng);
    break;
  default:
    return ERR_INV_ESCAPE;
  }
  return(0);
}

/*ARGSUSED*/
int PDFUpdateWorkstation(GKSC *gksc)
{
        PDFddp   *psa = (PDFddp *) gksc->ddp;

        (void) fflush(psa->file_pointer);
        return(0);
}

/*ARGSUSED*/
int PDFSetViewport(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;
  float   *fptr = (float *) gksc->f.list;

  PDFClipRect     *Crect;
  int     rec_chg = 0;

/*
 *  If the workstation viewport has changed, update the transformation.
 */
  if ((psa->tsystem.viewport.llx != fptr[0]) ||
      (psa->tsystem.viewport.urx != fptr[1]) ||
      (psa->tsystem.viewport.lly != fptr[2]) ||
      (psa->tsystem.viewport.ury != fptr[3])) {
    TransformSetViewport(&psa->tsystem, fptr[0], fptr[2], fptr[1], fptr[3]);
    psa->transform = TransformGetTransform(&psa->tsystem);
  }
  else {
    return(0);
  }

/*
 *  re-evaluate the clipping rectangle.
 */
  Crect = GetPDFClipping (psa, psa->gks_clip, psa->tsystem.window);
  if ((Crect->llx != psa->pdf_clip.llx) ||
      (Crect->lly != psa->pdf_clip.lly) ||
      (Crect->urx != psa->pdf_clip.urx) ||
      (Crect->ury != psa->pdf_clip.ury)) {
          psa->pdf_clip.llx = Crect->llx;
          psa->pdf_clip.lly = Crect->lly;
          psa->pdf_clip.urx = Crect->urx;
          psa->pdf_clip.ury = Crect->ury;
          psa->pdf_clip.null = Crect->null;
          rec_chg = 1;
  }

  if ((psa->attributes.clip_ind == CLIPPING_ON) && (rec_chg == 1) &&
       (psa->pict_empty == FALSE)) {
    PDFOutputClipping (psa, PDF_CLIPPING_RECT);
  }

  return(0);
}

/*ARGSUSED*/
int PDFSetWindow(GKSC *gksc)
{
  PDFddp   *psa = (PDFddp *) gksc->ddp;
  float   *fptr = (float *) gksc->f.list;

  PDFClipRect     *Crect;
  int     rec_chg = 0;

  if ((fptr[0] >= fptr[1]) || (fptr[2] >= fptr[3])) {
    return (ERR_INV_RECT);
  }

/*
 *  If the workstation window has changed, update the
 *  transformation.
 */
  if ((psa->tsystem.window.llx != fptr[0]) ||
      (psa->tsystem.window.urx != fptr[1]) ||
      (psa->tsystem.window.lly != fptr[2]) ||
      (psa->tsystem.window.ury != fptr[3])) {
          TransformSetWindow(&psa->tsystem, fptr[0], fptr[2], 
                  fptr[1], fptr[3]);
          psa->transform = TransformGetTransform(&psa->tsystem);
  }
  else {
    return(0);
  }

/*
 *  Calculate the new clip rectangle and check to
 *  to see if it has changed and store it if it has.
 */
  Crect = GetPDFClipping (psa, psa->gks_clip, psa->tsystem.window);
  if ((Crect->llx != psa->pdf_clip.llx) ||
      (Crect->lly != psa->pdf_clip.lly) ||
      (Crect->urx != psa->pdf_clip.urx) ||
      (Crect->ury != psa->pdf_clip.ury)) {
          psa->pdf_clip.llx = Crect->llx;
          psa->pdf_clip.lly = Crect->lly;
          psa->pdf_clip.urx = Crect->urx;
          psa->pdf_clip.ury = Crect->ury;
          psa->pdf_clip.null = Crect->null;
          rec_chg = 1;
  }

  if ((psa->attributes.clip_ind == CLIPPING_ON) && (rec_chg == 1) &&
                                   (psa->pict_empty == FALSE)) {
    PDFOutputClipping (psa, PDF_CLIPPING_RECT);
  }
  return(0);
}

/*
 *  Write out an object from specified lines.
 */
int PDFPutObject(FILE *fp, int object_number, int num_lines, char *guts[]) {
  int i,bcount = 0;
  fprintf(fp,"%6d 0 obj\n",object_number);
  bcount = bcount+13;
  for (i = 0; i < num_lines; i++) {
    bcount = bcount+strlen(guts[i])+1;
    fprintf(fp,"%s\n",guts[i]);
  }
  fprintf(fp,"endobj\n");
  bcount = bcount+7;
  return (bcount);
}

/*
 *  Write out a stream dictionary for page streams.  Returns
 *  the byte count.
 */
int PDFPutStreamDict(FILE *fp, int obj_num, int obj_contents_num, int width, 
                     int height) {
  int tbcnt=0;

  fprintf(fp,"%6d 0 obj\n<<\n",obj_num);
  tbcnt += 16;
  fprintf(fp,"/Type /Page\n");
  tbcnt += 12;
  fprintf(fp,"/Parent 1 0 R\n");
  tbcnt += 14;
  fprintf(fp,"/MediaBox [0 0 %05d %05d]\n",width,height);
  tbcnt += 28;
/*
 *  Have commented out the rotation, since this interferes with
 *  user-set changes in orientation at the beginning of pictures.
 */
/*  if (port_land == LANDSCAPE) {    */
/*    fprintf(fp,"/Rotate -90\n");   */
/*    tbcnt += 12;                   */
/*  }                                */
  fprintf(fp,"/Contents %6d 0 R\n",obj_contents_num);
  tbcnt += 21;
  fprintf(fp,"/Resources << /ProcSet 2 0 R\n");
  tbcnt += 29;
  fprintf(fp,"/Font << /Helvetica 3 0 R /Helvetica-Bold 4 0 R\n");
  tbcnt += 48;
  fprintf(fp,"         /Helvetica-Oblique 5 0 R /Helvetica-BoldOblique 6 0 R\n");
  tbcnt += 63;
  fprintf(fp,"         /Times-Roman 7 0 R /Times-Bold 8 0 R\n");
  tbcnt += 46;
  fprintf(fp,"         /Times-Italic 9 0 R /Times-BoldItalic 10 0 R\n");
  tbcnt += 54;
  fprintf(fp,"         /Courier 11 0 R /Courier-Bold 12 0 R\n");
  tbcnt += 46;
  fprintf(fp,"         /Courier-Oblique 13 0 R /Courier-BoldOblique 14 0 R\n");
  tbcnt += 61;
  fprintf(fp,"         /Symbol 15 0 R\n");
  tbcnt += 24;
  fprintf(fp,"      >>\n");
  tbcnt += 9;
  fprintf(fp,"           >>\n");
  tbcnt += 14;
  fprintf(fp,">>\nendobj\n");
  tbcnt += 10;

  return(tbcnt);
}

/*
 *  Write out a content stream.  The current value for object_number
 *  should be the stream dictionary for the content stream.
 */
void PDFPutStream(FILE *fp) {
  int i;
  fprintf(fp, "%6d 0 obj\n<< /Length %10d >>\nstream\n",object_number+1,
               stream_size);
  for (i = 0; i < num_page_lines+1; i++) {
    fprintf(fp,"%s", page_lines[i]);
    fflush(fp);
  }
  fprintf(fp, "endstream\nendobj\n");
  byte_count = byte_count + 45 + stream_size + 17;
}
/*
 *  Write out a single line segment.
 */
void PDFPutLine(int px0, int py0, int px1, int py1) {

/*
 *  Return if any coordinate is out-of-range.
 */
  if (abs(px0) > 99999 || abs(py0) > 99999 ||
      abs(px1) > 99999 || abs(py0) > 99999 ) {
    return;
  }
      
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%6d %6d m\n", px0, py0);
  stream_size += 16;
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%6d %6d l\n", px1, py1);
  stream_size += 16;
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"S\n");
  stream_size += 2;
}
/*
 *  Write out a circle with center at (x,y) and radius r.  The arguments allow 
 *  one to form the Bezier points, starting at the lower point of 
 *  the circle.  For example, xmr is x minus the radius.
 */
void PDFPutCircle(int x, int y, int r) {

/*
 *  Specify various points on the circle, including the Bezier
 *  control points.
 */
  int b,xpr,xmr,ypr,ymr,xpb,xmb,ypb,ymb;
  b   = (int) (0.545 * (float) r);
  xpr = x+r;
  xmr = x-r;
  ypr = y+r;
  ymr = y-r;
  xpb = x+b;
  xmb = x-b;
  ypb = y+b;
  ymb = y-b;

/*
 *  Move to the bottom point on the circle.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%6d %6d m\n", x, ymr);
  stream_size += 16;

/*
 *  Draw a curve to the point on the circle's right.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%6d %6d %6d %6d %6d %6d c\n", 
          xpb, ymr, xpr, ymb, xpr, y);
  stream_size += 44;

/*
 *  Draw a curve to the top point of the circle.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%6d %6d %6d %6d %6d %6d c\n", 
          xpr, ypb, xpb, ypr, x, ypr);
  stream_size += 44;

/*
 *  Draw a curve to the point on the circle's left.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%6d %6d %6d %6d %6d %6d c\n", 
          xmb, ypr, xmr, ypb, xmr, y);
  stream_size += 44;

/*
 *  Draw a curve back to the bottom point.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"%6d %6d %6d %6d %6d %6d c\n", 
          xmr, ymb, xmb, ymr, x, ymr);
  stream_size += 44;

/*
 *  Stroke.
 */
  bump_page_lines();
  sprintf(page_lines[num_page_lines],"S\n");
  stream_size += 2;
}

void PDFPutFonts(FILE *fp) {
  char *lines[10];
  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Helvetica";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Helvetica-Bold";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Helvetica-Oblique";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Helvetica-BoldOblique";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Times-Roman";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Times-Bold";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Times-Italic";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Times-BoldItalic";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Courier";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Courier-Bold";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Courier-Oblique";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Courier-BoldOblique";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);

  bump_object_number();
  object_pointer[object_number] = byte_count;
  lines[0]   = "<<";
  lines[1]   = "/Type /Font";
  lines[2]   = "/Subtype /Type1";
  lines[3]   = "/BaseFont /Symbol";
  lines[4]   = ">>";
  byte_count += PDFPutObject(fp,object_number,5,lines);
}
void PDFPlotBackground(PDFddp *psa, int xll, int yll, int xur, int yur) {

  float r,g,b;
  int current_color;

  current_color = psa->attributes.pdf_colr_ind;
  r = psa->color_map[0];
  g = psa->color_map[1];
  b = psa->color_map[2];

  if (co_model != 0) {
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",r,g,b);
    stream_size += 21;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",r,g,b);
    stream_size += 21;
  }
  else {
    rgb2cmyk(r,g,b,&cyan,&magenta,&yellow,&black);
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
            cyan,magenta,yellow,black);
    stream_size += 26;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
            cyan,magenta,yellow,black);
    stream_size += 26;
  }

  bump_page_lines();
  sprintf(page_lines[num_page_lines],
           "%6d %6d m\n%6d %6d l\n%6d %6d l\n%6d %6d l\n%6d %6d l\n",
           xll, yll, xur, yll, xur, yur, xll, yur, xll, yll); 
  stream_size += 80;

  bump_page_lines();
  sprintf(page_lines[num_page_lines],"f*\n");
  stream_size += 3;

  index2rgb(psa->color_map, current_color, &r, &g, &b);

  if (co_model != 0) {
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f RG\n",r,g,b);
    stream_size += 21;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f rg\n",r,g,b);
    stream_size += 21;
  }
  else {
    rgb2cmyk(r,g,b,&cyan,&magenta,&yellow,&black);
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f K\n",
            cyan,magenta,yellow,black);
    stream_size += 26;
    bump_page_lines();
    sprintf(page_lines[num_page_lines],"%5.2f %5.2f %5.2f %5.2f k\n",
            cyan,magenta,yellow,black);
    stream_size += 26;
  }
}
void rgb2cmyk(float r, float g, float b,
              float *c, float *m, float *y, float *k) {
  *c = 1.-r;
  *m = 1.-g;
  *y = 1.-b;
  *k = MIN(MIN(*c,*m),*y);
  *c = *c-*k;
  *m = *m-*k;
  *y = *y-*k;
}
void bump_object_number() {
  if (object_number > MAX_OBJECTS) {
    object_pointer = (int *) realloc(object_pointer,2*MAX_OBJECTS*sizeof(int));
    if (object_pointer == NULL) {
      fprintf(stderr,"PDF - not enough memory to store all objects.\n");
    }
  }
  object_number++;
}
void bump_page_lines() {
  if (num_page_lines >= maximum_lines-1) {
    adjust_lines();
  }
  num_page_lines++;
}
void adjust_lines() {
  int i;

  page_lines = \
   (char **) realloc(page_lines,(maximum_lines+LINE_INCREMENT)*sizeof(char *));
  if (page_lines == (char **) NULL) {
    printf ("PDF - unable to allocate space for object, object too large.\n");
  }
  for (i = 0; i < LINE_INCREMENT; i++) {
    page_lines[maximum_lines+i] = (char *) calloc(LINE_SIZE,sizeof(char));
  }
  maximum_lines += LINE_INCREMENT;
}
