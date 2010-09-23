/*
 *	$Id: pdf.h,v 1.7 2010-01-15 05:15:48 fred Exp $
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
 *      File:		pdf.h
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Tue Oct 15 18:04:15 MDT 2002
 *
 *      Description:	PDF device driver header file
 */

#include <stdio.h>

#ifndef	_pdf_driver_
#define	_pdf_driver_

#define POINTS_PER_LINE        6        /* max pts. on an output line (even)*/
#define PDF_FILL_SPACING 	.0005   /* default software fill line spacing */
#define PDF_HATCH_SPACING 	.01     /* default spacing of hatch fill lines*/
#define MAX_STACK               200     /* max number of items on the stack */
#define MAX_PATH                2500    /* maximum number of points in a path,*/
#define MAX_OBJECTS             25000   /* maximum number of PDF objects */
#define LINE_SIZE               100     /* maximum lenght of a line */
#define LINE_INCREMENT          50000   /* no. of lines to increment the */
                                        /* page_line array when it fills */

#define PDF_SCALE 		.04     /* coordinate scale factor */

#define LLX_DEFAULT		36      /* default lower left X coordinate */
#define URX_DEFAULT		576     /* default upper right X coordinate */
#define LLY_DEFAULT		126     /* default lower left Y coordinate */
#define URY_DEFAULT		666     /* default upper right Y coordinate */

#define PDF_DEFAULT_CLIPPING_RECT 0
#define PDF_CLIPPING_RECT	  1

#define MITER_LIMIT_DEFAULT		10.
#define SUPPRESS_FLAG      		0

#define LINETYPE_DEFAULT                1       
#define LINEWIDTH_DEFAULT               1.
#define LINE_COLR_DEFAULT               1
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
#define PDF_COLR_IND_DEFAULT            1

#define NUM_PDF_FONTS			13
#define DEFAULT                         1
#define H_CARTOGRAPHIC_ROMAN 		-2
#define H_CARTOGRAPHIC_GREEK		-3
#define H_SIMPLEX_ROMAN    		-4
#define H_SIMPLEX_GREEK   		-5
#define H_SIMPLEX_SCRIPT 		-6
#define H_COMPLEX_ROMAN 		-7
#define H_COMPLEX_GREEK			-8
#define H_COMPLEX_SCRIPT     		-9
#define H_COMPLEX_ITALIC    		-10
#define H_COMPLEX_CYRILLIC 		-11
#define H_DUPLEX_ROMAN    		-12
#define H_TRIPLEX_ROMAN  		-13
#define H_TRIPLEX_ITALIC		-14
#define H_GOTHIC_GERMAN			-15
#define H_GOTHIC_ENGLISH		-16
#define H_GOTHIC_ITALIAN		-17
#define H_MATH_SYMBOLS 			-18
#define H_SYMBOL_SET1 			-19
#define H_SYMBOL_SET2			-20
#define NCAR_HELVETICA			-21
#define NCAR_HELVETICA_BOLD		-22
#define NCAR_HELVETICA_OBLIQUE		-23
#define NCAR_HELVETICA_BOLDOBLIQUE	-24
#define NCAR_TIMES_ROMAN		-25
#define NCAR_TIMES_BOLD			-26
#define NCAR_TIMES_ITALIC		-27
#define NCAR_TIMES_BOLDITALIC		-28
#define NCAR_COURIER			-29
#define NCAR_COURIER_BOLD		-30
#define NCAR_COURIER_OBLIQUE		-31
#define NCAR_COURIER_BOLDOBLIQUE	-32
#define NCAR_GREEK			-33
#define NCAR_MATH_SYMBOLS		-34
#define NCAR_TEXT_SYMBOLS		-35
#define NCAR_WEATHER1			-36
#define NCAR_WEATHER2			-37
#define NCAR_HELVETICA_O		-121
#define NCAR_HELVETICA_BOLD_O		-122
#define NCAR_HELVETICA_OBLIQUE_O	-123
#define NCAR_HELVETICA_BOLDOBLIQUE_O	-124
#define NCAR_TIMES_ROMAN_O		-125
#define NCAR_TIMES_BOLD_O		-126
#define NCAR_TIMES_ITALIC_O		-127
#define NCAR_TIMES_BOLDITALIC_O		-128
#define NCAR_COURIER_O			-129
#define NCAR_COURIER_BOLD_O		-130
#define NCAR_COURIER_OBLIQUE_O		-131
#define NCAR_COURIER_BOLDOBLIQUE_O	-132
#define NCAR_GREEK_O			-133
#define NCAR_MATH_SYMBOLS_O		-134
#define NCAR_TEXT_SYMBOLS_O		-135
#define NCAR_WEATHER1_O			-136
#define NCAR_WEATHER2_O			-137
#define PDF_TIMES_ROMAN			0
#define PDF_TIMES_BOLD			1
#define PDF_TIMES_ITALIC		2
#define PDF_TIMES_BOLDITALIC		3
#define PDF_HELVETICA			4
#define PDF_HELVETICA_BOLD		5
#define PDF_HELVETICA_OBLIQUE		6
#define PDF_HELVETICA_BOLDOBLIQUE	7
#define PDF_COURIER			8
#define PDF_COURIER_BOLD		9
#define PDF_COURIER_OBLIQUE		10
#define PDF_COURIER_BOLDOBLIQUE		11
#define PDF_SYMBOL			12

/* PDF workstation types */
#define PDP 				11  /* PDF portrait          */
#define PDL 				12  /* PDF landscape         */

/* PDF workstation types */
#define CPP                             20  /* color  ps   portrait  */
#define CEP                             21  /* color  eps  portrait  */
#define CIP                             22  /* color  epsi portrait  */
#define MPP                             23  /* mono   ps   portrait  */
#define MEP                             24  /* mono   eps  portrait  */
#define MIP                             25  /* mono   epsi portrait  */
#define CPL                             26  /* color  ps   landscape */
#define CEL                             27  /* color  eps  landscape */
#define CIL                             28  /* color  epsi landscape */
#define MPL                             29  /* mono   ps   landscape */
#define MEL                             30  /* mono   eps  landscape */
#define MIL                             31  /* mono   epsi landscape */

#define FONT_HEIGHT_ARRAY_INDEX		192

typedef	struct	PDFPoint_	{
	float	x,y;
	} PDFPoint;

typedef	struct	PDFColor_	{
	float	r,g,b;
	} PDFColor;

typedef struct 	PDFDeviceSpace_	{
	int	llx,
		lly,
		urx,
		ury,
		xspan,		/* span of X coordinates */
		yspan;		/* span of Y coordinates */
	} PDFDeviceSpace;

typedef struct 	PDFCharInfo_ {
	int 	font;           /* A PostScript font number */
	int	char_num;       /* The PostScript character number */
	int 	char_width;     /* The character width in a normalized PDF */
				/*   font scaled by 1000 */
	int     font_height;    /* Height of standard capital in current font */
	int	outline;	/* Flags outlines (TRUE) or filled (FALSE)  */
	} PDFCharInfo;

typedef struct 	PDFTextent_ {	/* Text extent in font and viewport space */
	int	left;
	int	right;
	int	bottom;
	int	top;
	float	left_wsvp;
	float	right_wsvp;
	float	bottom_wsvp;
	float	top_wsvp;
	} PDFTextent;

typedef	struct	PDFClipRect_ {	/* Current clipping rectangle in PDF coords. */
	int	null;		/* null = 1 implies empty  */
	int	llx;
	int	lly;
	int	urx;
	int	ury;
	} PDFClipRect;

typedef struct	PDFattribute_ {
	int	linetype;
	int	linetype_set;       /*  Linetype actually existing in file */
	float	linewidth;
	int	linewidth_set;      /*  Linewidth actually esixting in file  */
	int	line_colr_ind;
	int	marker_type;
	float	marker_size;
	int	marker_colr_ind;
	int	text_font;
	int	text_prec;
	float	char_expan;
	float	char_space;
	int	text_colr_ind;
	float	char_ht;
	float	char_up_vec_x;
	float	char_up_vec_y;
	float	char_base_vec_x;
	float	char_base_vec_y;
	int	text_path;
	int	text_align_horiz;
	int	text_align_vert;
	int	fill_int_style;
	int	fill_style_ind;
	int	fill_colr_ind;
	int	clip_ind;
	int	pdf_colr_ind;       /*  Index of current PDF color being used */
	int	null_clip_rect;     /*  Flags a null clipping rectangle */
	} PDFattribute;

typedef enum {MONO, COLOR} pdf_color;
typedef enum {PDF14} pdf_file_type;
typedef enum {PORTRAIT, LANDSCAPE} pdf_orientation;
typedef enum {NONE, MOVETO, RMOVETO, LINETO, NEW_LINE,
                POLY_POINT, POLY_FILL, POLY_MARKER} terminator_type;
typedef enum {FOR_FILE, FOR_PICTURE} preamble_type;
typedef enum {BUTT, ROUNDED, PROJECTING} linecap_type;
typedef enum {MITER,ROUND, BEVEL} linejoin_type;

void reverse_chars(char *);
void bump_page_lines();
void bump_object_number();
int  PDFPutStreamDict(FILE *, int, int, int, int);
void PDFPutStream(FILE *);

#endif	/* _pdf_driver_	*/
