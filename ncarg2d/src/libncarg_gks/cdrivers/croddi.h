/*
 *      $Id: croddi.h,v 1.6 2010-02-09 23:16:19 brownrig Exp $
 */
/*
 *      File:           croddi.h
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Mar  7 11:55:16 MST 2008
 *
 *      Description:    This file defines the device dependent structure for
 *                      the gksc.ddp field for the cairo drivers.
 */

#ifndef _croddi_
#define _croddi_

#include "cro.h"
#include "crotiff.h"
#include "gks.h"
#include "common.h"
#include "transform.h"

typedef struct  CROddi_ {
  int             wks_id;
  int             wks_type;
  int             is_vector_type;
  Transform2D     transform;
  TransSystem     tsystem;
  CRODeviceSpace  dspace;
  char          *output_file;      /* for file-based formats               */
  char          *window_title;     /* for window-based formats (i.e., X11) */
  char          *icon_title;       /*                "                     */
  cro_color     color;
  cro_orientation  orientation;    /* applicable only to PS/PDF formats */
  float         sfill_spacing;
  float         hatch_spacing;
  int           cairo_fill_hack;   /* see Jira ncl-1913 */
  linejoin_type line_join;
  linecap_type  line_cap;
  float         miter_limit;
  int           stack_size;
  int           path_size;
  float         nominal_width_scale;
  int           full_background;
  float         scaling;
  int           image_width;       /* resolution, for image/window output formats only */
  int           image_height;      /*                     "                            */
  int           paper_width;       /* paper size, for document-based formats only      */
  int           paper_height;      /*                     "                            */
  int           window_pos_x;      /* window-based formats only                        */
  int           window_pos_y;      /*                     "                            */
  int           background;
  float         background_alpha;
  int           pict_empty;
  int           page_number;
  int           suppress_flag;
  int           max_color;
  unsigned int  *ctable;
  int           fonts_used[13];
  int           frame_count;
  CROattribute  attributes;
  CoordSpace    gks_clip;
  CROClipRect   cro_clip;
  int           useTiffCompression;
  TiffHandle*   tiffClosure;
  TiffGeoReference* georefData;
} CROddp;

#endif  /*      _croddi_        */
