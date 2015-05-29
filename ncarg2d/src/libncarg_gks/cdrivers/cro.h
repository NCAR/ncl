/*
 *      $Id: cro.h,v 1.5 2010-04-02 16:36:16 brownrig Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/

/*
 *      File:         cro.h
 *
 *      Author:       Fred Clare
 *                    National Center for Atmospheric Research
 *                    PO 3000, Boulder, Colorado
 *
 *      Date:         Wed Aug 27 13:32:56 MDT 2008
 *
 *      Description:  defines constants and structures relevant
 *                    to the cairo drivers.
 */
#ifndef _cro_driver_
#define _cro_driver_

#define CRO_FILL_SPACING 	.0005   /* default software fill line spacing */
#define CRO_HATCH_SPACING       0.01    /* default spacing of hatch fill lines*/
#define MAX_PATH            10000000    /* maximum number of points in a path,*/

#define CRO_SCALE 		.04     /* coordinate scale factor */

/* postscript/pdf page defaults */
#define LLX_DEFAULT           36    /* default lower left X coordinate */
#define URX_DEFAULT          576    /* default upper right X coordinate */
#define LLY_DEFAULT          126    /* default lower left Y coordinate */
#define URY_DEFAULT          666    /* default upper right Y coordinate */
#define PSPDF_PAGESIZE_X     612    /* page size in points, at 72 points/inch */
#define PSPDF_PAGESIZE_Y     792    /*                  "                     */

/* image-based formats defaults */
#define DEFAULT_IMAGE_WIDTH 1000
#define DEFAULT_IMAGE_HEIGHT 1000

#define DEFAULT_CLIPPING_RECT   0
#define CRO_CLIPPING_RECT       1

#define MITER_LIMIT_DEFAULT    10.
#define SUPPRESS_FLAG           0

#define LINETYPE_DEFAULT        1
#define LINEWIDTH_DEFAULT       1.0
#define LINE_COLR_DEFAULT       1
#define MARKER_TYPE_DEFAULT             3
#define MARKER_SIZE_DEFAULT             1.
#define MARKER_COLR_IND_DEFAULT         1
#define TEXT_FONT_DEFAULT               1
#define TEXT_PREC_DEFAULT               0
#define CHAR_EXPAN_DEFAULT              1.
#define CHAR_SPACE_DEFAULT              0.
#define TEXT_COLR_IND_DEFAULT           1
#define CHAR_HT_DEFAULT                 .01
#define CHAR_UP_VEC_X_DEFAULT           0.
#define CHAR_UP_VEC_Y_DEFAULT           1.
#define CHAR_BASE_VEC_X_DEFAULT         1.
#define CHAR_BASE_VEC_y_DEFAULT         0.
#define TEXT_PATH_DEFAULT               0
#define TEXT_ALIGN_HORIZ_DEFAULT        0
#define TEXT_ALIGN_VERT_DEFAULT         0
#define FILL_INT_STYLE_DEFAULT          0
#define FILL_STYLE_IND_DEFAULT          1
#define FILL_COLR_IND_DEFAULT           1
#define CLIP_IND_DEFAULT                1
#define CRO_COLR_IND_DEFAULT            1

#define NUM_CRO_FONTS                   37
#define DEFAULT                          1
#define H_CARTOGRAPHIC_ROMAN            -2
#define H_CARTOGRAPHIC_GREEK            -3
#define H_SIMPLEX_ROMAN                 -4
#define H_SIMPLEX_GREEK                 -5
#define H_SIMPLEX_SCRIPT                -6
#define H_COMPLEX_ROMAN                 -7
#define H_COMPLEX_GREEK                 -8
#define H_COMPLEX_SCRIPT                -9
#define H_COMPLEX_ITALIC                -10
#define H_COMPLEX_CYRILLIC              -11
#define H_DUPLEX_ROMAN                  -12
#define H_TRIPLEX_ROMAN                 -13
#define H_TRIPLEX_ITALIC                -14
#define H_GOTHIC_GERMAN                 -15
#define H_GOTHIC_ENGLISH                -16
#define H_GOTHIC_ITALIAN                -17
#define H_MATH_SYMBOLS                  -18
#define H_SYMBOL_SET1                   -19
#define H_SYMBOL_SET2                   -20
#define NCAR_HELVETICA                  -21
#define NCAR_HELVETICA_BOLD             -22
#define NCAR_HELVETICA_OBLIQUE          -23
#define NCAR_HELVETICA_BOLDOBLIQUE      -24
#define NCAR_TIMES_ROMAN                -25
#define NCAR_TIMES_BOLD                 -26
#define NCAR_TIMES_ITALIC               -27
#define NCAR_TIMES_BOLDITALIC           -28
#define NCAR_COURIER                    -29
#define NCAR_COURIER_BOLD               -30
#define NCAR_COURIER_OBLIQUE            -31
#define NCAR_COURIER_BOLDOBLIQUE        -32
#define NCAR_GREEK                      -33
#define NCAR_MATH_SYMBOLS               -34
#define NCAR_TEXT_SYMBOLS               -35
#define NCAR_WEATHER1                   -36
#define NCAR_WEATHER2                   -37
#define NCAR_HELVETICA_O                -121
#define NCAR_HELVETICA_BOLD_O           -122
#define NCAR_HELVETICA_OBLIQUE_O        -123
#define NCAR_HELVETICA_BOLDOBLIQUE_O    -124
#define NCAR_TIMES_ROMAN_O              -125
#define NCAR_TIMES_BOLD_O               -126
#define NCAR_TIMES_ITALIC_O             -127
#define NCAR_TIMES_BOLDITALIC_O         -128
#define NCAR_COURIER_O                  -129
#define NCAR_COURIER_BOLD_O             -130
#define NCAR_COURIER_OBLIQUE_O          -131
#define NCAR_COURIER_BOLDOBLIQUE_O      -132
#define NCAR_GREEK_O                    -133
#define NCAR_MATH_SYMBOLS_O             -134
#define NCAR_TEXT_SYMBOLS_O             -135
#define NCAR_WEATHER1_O                 -136
#define NCAR_WEATHER2_O                 -137

/* cairo workstation types */
#define CPS                 40  /* cairo Postscript  */
#define CPNG                41  /* cairo PNG         */
#define CPDF                42  /* cairo PDF         */
#define CTIFF               43  /* cairo (geo)TIFF   */
#define CX11                44  /* cairo x11 surface */
#define CEPS                45  /* cairo EPS */
#define CQT                 46  /* cairo Qt */
#define CSVG                47  /* cairo SVG */

#include <cairo/cairo.h>
#include <cairo/cairo-ps.h>
#include <cairo/cairo-pdf.h>
#include <cairo/cairo-xlib.h>
#include <cairo/cairo-svg.h>
#include <cairo/cairo-ft.h>

typedef enum {MONO, COLOR} cro_color;

typedef struct  CROPoint_       {
        float   x,y;
        } CROPoint;

typedef struct  CROColor_       {
        float   r,g,b;
        } CROColor;

typedef struct  CRODeviceSpace_ {
        int     llx,
                lly,
                urx,
                ury,
                xspan,          /* span of X coordinates */
                yspan;          /* span of Y coordinates */
        } CRODeviceSpace;

typedef struct  CROBoundingBox_ {
        int     llx,
                lly,
                urx,
                ury;
        } CROBoundingBox;

typedef struct  CROCharInfo_ {
        int     font;           /* A font number */
        int     char_num;       /* The character number */
        int     char_width;     /* The character width in a normalized */
                                /*   font scaled by 1000 */
        int     font_height;    /* Height of standard capital in current font */
        int     outline;        /* Flags outlines (TRUE) or filled (FALSE)  */
        } CROCharInfo;

typedef struct  CROTextext_ {   /* Text extent in viewport space */
        int     left;
        int     right;
        int     bottom;
        int     top;
        } CROTextext;

typedef struct  CROClipRect_ {  /* Current clipping rectangle in PS coords. */
        int     null;           /* null = 1 implies empty  */
        double  llx;
        double  lly;
        double  urx;
        double  ury;
        } CROClipRect;

typedef struct  CROattribute_ {
        int     linetype;
        int     linetype_set;       /*  Linetype actually existing in file */
        float   linewidth;
        int     linewidth_set;      /*  Linewidth actually existing in file  */
        unsigned int line_colr_ind;
        float   line_alpha;
        int     marker_type;
        float   marker_size;
        unsigned int marker_colr_ind;
        float   marker_alpha;
        int     text_font;
        int     text_prec;
        float   char_expan;
        float   char_space;
        unsigned int text_colr_ind;
        float   text_alpha;
        float   char_ht;
        float   char_up_vec_x;
        float   char_up_vec_y;
        float   char_base_vec_x;
        float   char_base_vec_y;
        int     text_path;
        int     text_align_horiz;
        int     text_align_vert;
        int     fill_int_style;
        int     fill_style_ind;
        unsigned int fill_colr_ind;
        float   fill_alpha;
        int     clip_ind;
        unsigned int cro_colr_ind;  /*  Index of current CRO color */
        int     norm_tran;          /*  GKS normalization transformation number */
        } CROattribute;

struct color_value {
  double alpha;
  double red;
  double green;
  double blue;
};

typedef enum {PORTRAIT, LANDSCAPE} cro_orientation;
typedef enum {BUTT, ROUNDED, PROJECTING} linecap_type;
typedef enum {MITER,ROUND, BEVEL} linejoin_type;

/*
 * Fortran function macro.  This macro needs to surround any "C" reference
 * to a function that is written in fortran or is written in "C" to be
 * Fortran callable.
 */
#ifndef NGCALLF

#define NGCALLF(reg,caps)   reg##_

#endif  /* NGCALLF */

#ifdef BuildQtEnabled
extern cairo_surface_t *qt_surface;
extern cairo_t         *qt_context;
extern int qt_screen_width;
extern int qt_screen_height;
#endif

static cairo_t* getContext(int wksId);

void reverse_chars(char *);

#ifdef BuildQtEnabled
void setCairoQtSurface(cairo_surface_t *surface);
void setCairoQtWinSize(int width, int height);
#endif

#endif  /* _cro_driver_ */
