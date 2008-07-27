/*
 *	$Id: readfont.c,v 1.13 2008-07-27 03:18:44 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

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
#include <stdlib.h>
#include <errno.h>
#include <ncarg/c.h>
#define		READFONT
#include "readfont.h"




/*	readfont.c:
 *		author : John Clyne
 *		3/18/88
 *
 *	This file is responsible for reading the binary version of the
 *	fontcap into the data table "Fontable". The font table is used
 *	for translation of CGM commands for text into CGMC's. (see Ctran
 *	maintenance manual for information on CGMC). For proper function of
 *	"readfont", the binary version of the fontcap must be created
 *	in exact format of that produced by the FORTRAN translator
 *	"fontc". For information on fontcaps see "NCAR Computer
 *	Graphics Metafile Translator" documentation. For information
 *	on their storage see fontcap translation documentation
 *
 *
 *	This module makes the following assumptions which are not explicitly
 *	stated in the fontcap manual:
 *
 *	1) The Font coordinate system is constant for all characters in a
 *	given font. 
 *
 *	2) Each character defined in a font is centered left to right 
 *	in the font coordinate system.
 *
 *	3) left and right extents for the character bodies for variable-spaced
 *	fonts may vary.
 * 
 *	4) Top and bottom extent is constant for each character in a given
 *	font.
 */ 

/*	decodefont:
 *	[internal]
 *
 *	This routine decodes the  integer values stored in the 
 *	fontcap that represent stroke sequences for characters into 
 *	meaningful fields
 *	
 * on entry:
 *	readfont has been called
 * on exit
 *	fcap_template	: contain the stroke sequences
 *	fcap_current	: has memory allocated sufficient for template
 *	var_space	: true if font type is VAR_SPACE or VAR_FILL, else 0 
 */
static
int decodefont()
{
	int k,i;
	unsigned 	size;		/*number of strokes in a sequence*/
	unsigned	index = 0;	/*index into strokearray	*/



	F_CHAR_START(fcap_template)         = fontcap_raw->char_start;
	F_CHAR_END(fcap_template)           = fontcap_raw->char_end;
	F_CHAR_WIDTH(fcap_template)         = fontcap_raw->char_width;
	F_CHAR_HEIGHT(fcap_template)        = fontcap_raw->font_cap -
						fontcap_raw->font_base;

	F_FONT_TOP(fcap_template)           = fontcap_raw->font_top;
	F_FONT_CAP(fcap_template)           = fontcap_raw->font_cap;
	F_FONT_HALF(fcap_template)          = fontcap_raw->font_half;
	F_FONT_BASE(fcap_template)          = fontcap_raw->font_base;
	F_FONT_BOTTOM(fcap_template)        = fontcap_raw->font_bottom;
	F_FONT_TYPE(fcap_template)	    = fontcap_raw->font_type;
	F_FONT_RIGHT(fcap_template)         = fontcap_raw->font_right;

	var_space = (fontcap_raw->font_type == VAR_SPACE ||
			fontcap_raw->font_type == VAR_FILL);

	F_NUMCHAR(fcap_template) = 
	F_NUMCHAR(fcap_current) = 
		fontcap_raw->char_end - fontcap_raw->char_start + 1;

	/*
	 *	place terminating index value for fontcap_raw.strokes in
	 *	the stroke array
	 */
	fontcap_raw->pointers[F_NUMCHAR(fcap_template)] = 
				fontcap_raw->last_pointer;


	for (i = 0, index = 0;i < F_NUMCHAR(fcap_template); i++) { 
		size = fontcap_raw->pointers[i+1] - fontcap_raw->pointers[i];

		/*
		 * in variable space fonts "width" information is stored
		 * with the stroke information. We don't want to include
		 * it here
		 */
		size = var_space ? size - 1 : size;

		if (size) {
		/* 
 		 * malloc only enough memory necessary to describe a 
		 * particular character. Free any memory alloced in
		 * a previous call.
		 */
		if (fcap_template.char_des[i].p_c) {
			free((Voidptr) fcap_template.char_des[i].p_c);
			fcap_template.char_des[i].p_c = NULL;
		}

		fcap_template.char_des[i].p_c = (Pen_coord *) malloc (
			size * sizeof(Pen_coord)
		);
		if (! fcap_template.char_des[i].p_c) {
			ESprintf(errno, "malloc(%d)", size * sizeof(Pen_coord));
			return(-1);
		}


		if (fcap_current.char_des[i].p_c) {
			free((Voidptr) fcap_current.char_des[i].p_c);
			fcap_current.char_des[i].p_c = NULL;
		}

		fcap_current.char_des[i].p_c = (Pen_coord *) malloc (
			size * sizeof(Pen_coord)
		);
		if (! fcap_current.char_des[i].p_c) {
			ESprintf(errno, "malloc(%d)", size * sizeof(Pen_coord));
			return(-1);
		}
		}	/* (if size)	*/

		F_NUMSTROKE(fcap_template, i) = 
		F_NUMSTROKE(fcap_current, i) = size;

		/*
		 * variable space fonts store individual character widths
		 * in first stroke position. See "FONTCAP Files" in the
		 * Installers Guide
		 */
		if (var_space) {
			rightExtent1[i] = 
				GETBITS(fontcap_raw->strokes[index],
				fontcap_raw->c_y_start,fontcap_raw->c_y_len);

			leftExtent1[i] = 
				GETBITS(fontcap_raw->strokes[index], 
				fontcap_raw->c_x_start,fontcap_raw->c_x_len);

			index++;
		}


		for (k = 0; k < size; k++) {
			F_X_COORD(fcap_template, i, k) = 
				GETBITS(fontcap_raw->strokes[index], 
				fontcap_raw->c_x_start,fontcap_raw->c_x_len); 

			F_Y_COORD(fcap_template, i, k) = 
				GETBITS(fontcap_raw->strokes[index],
				fontcap_raw->c_y_start,fontcap_raw->c_y_len); 


			F_PEN(fcap_template, i, k) = 
				GETBITS(fontcap_raw->strokes[index],
				fontcap_raw->c_pen_start,
				fontcap_raw->c_pen_len); 

			F_PAINT_ST(fcap_template, i, k) = 
				GETBITS(fontcap_raw->strokes[index],
				fontcap_raw->pt_beg_start,
				fontcap_raw->pt_beg_len); 

			F_PAINT_ED(fcap_template, i, k) = 
				GETBITS(fontcap_raw->strokes[index],
				fontcap_raw->pt_end_start,
				fontcap_raw->pt_end_len); 

			index++;
		}
	}

	return(0);

}



#ifdef	DEBUG_FCAP
/*
 *	print_fontcap
 *	[internal]
 *
 *	print out contents of the fontcap for debuging
 */
static	print_fontcap() 
{

	(void) fprintf(stderr,"char_start	%d\n", fontcap_raw->char_start);  
	(void) fprintf(stderr,"char_end  	%d\n", fontcap_raw->char_end);  
	(void) fprintf(stderr,"c_x_start     	%d\n", fontcap_raw->c_x_start);  

	(void) fprintf(stderr,"c_y_start     	%d\n", fontcap_raw->c_y_start);  

	(void) fprintf(stderr,"c_pen_start   	%d\n", fontcap_raw->c_pen_start);

	(void) fprintf(stderr,"pt_beg_start   	%d\n", fontcap_raw->pt_beg_start);  
	(void) fprintf(stderr,"pt_end_start   	%d\n", fontcap_raw->pt_end_start);  
	(void) fprintf(stderr,"pt_beg_len   	%d\n", fontcap_raw->pt_beg_len);
	(void) fprintf(stderr,"pt_end_len   	%d\n", fontcap_raw->pt_end_len); 
	(void) fprintf(stderr,"c_x_len     	%d\n", fontcap_raw->c_x_len);  
	(void) fprintf(stderr,"c_y_len     	%d\n", fontcap_raw->c_y_len);  

	(void) fprintf(stderr,"c_pen_len     	%d\n", fontcap_raw->c_pen_len);  

	(void) fprintf(stderr,"char_width  	%d\n", fontcap_raw->char_width); 
	(void) fprintf(stderr,"font_top 	%d\n", fontcap_raw->font_top);  
	(void) fprintf(stderr,"font_cap 	%d\n", fontcap_raw->font_cap);  

	(void) fprintf(stderr,"font_half 	%d\n", fontcap_raw->font_half); 

	(void) fprintf(stderr,"font_base 	%d\n", fontcap_raw->font_base);  
	(void) fprintf(stderr,"font_bottom  	%d\n", fontcap_raw->font_bottom);
	(void) fprintf(stderr,"font_right 	%d\n", fontcap_raw->font_right);
}


print_strokes(fcap) 
	Fcap	*fcap;
{
	int	i,k;

	for (i = 0; i < fcap->numchar; i++) {
		(void) fprintf(stderr, "\n\nchar [%d] \n	", i);
		(void) fprintf(stderr, "x,y ");
		for (k = 0; k < fcap->char_des[i].numstroke; k++) {

			(void) fprintf(stderr, "%d %d	", 
				fcap->char_des[i].p_c[k].x_coord,
				fcap->char_des[i].p_c[k].y_coord);
		}
	}
}


#endif	/* DEBUG_FCAP	*/

/*	Init_Readfont:
 *	[exported]
 *		Initialize the readfont_module
 *	on entry
 *		fontcap : is the name of a binary version of a fontcap
 *	on exit
 *		file fontcap has been opened and processing can begin.
 */
int	Init_Readfont(fontcap)
	char 	*fontcap;		/*fontcap name	*/
{
	int	fd;		/* fontcap file descriptor	*/
	int	status;		/* return status		*/

	/*
	 *	malloc memory for fontcap 
	 */
	if (! (fontcap_raw = (Fontcap_raw *) malloc (sizeof (Fontcap_raw)))) {
		ESprintf(errno, "malloc(%d)", sizeof(Fontcap_raw));
		return(-1);
	}

	/*
	 *	open and read the fontcap
	 */
	if ((fd = open(fontcap,0))  == -1) {
		if (fontcap_raw) free ((Voidptr) fontcap_raw);
		ESprintf(errno, "open(%s,0)", fontcap);
		return (-1);
	}

#ifdef	VMS
	if ((VMSfcread(fd,(char *) fontcap_raw, sizeof(Fontcap_raw))) 
				!= sizeof(Fontcap_raw)) {

		if (fontcap_raw) free ((Voidptr) fontcap_raw);
		(void) close (fd);
		ESprintf(errno, "VMSfcread(%d,,%d)", fd, sizeof(Fontcap_raw));
		return (-1);
	}
#else
	if ((read(fd,(char *) fontcap_raw, sizeof(Fontcap_raw))) 
				!= sizeof(Fontcap_raw)) {

		if (fontcap_raw) free ((Voidptr) fontcap_raw);
		(void) close (fd);
		ESprintf(errno, "read(%d,,%d)", fd, sizeof(Fontcap_raw));
		return (-1);
	}

#endif
	/*
	 *	translate the fontcap into a more meaningful form
	 */
	status = decodefont();

#ifdef	DEBUG_FCAP
	print_fontcap();
	print_strokes(&fcap_template);
#endif

	/*
	 *	de alloc raw fontcap
	 */
	if (fontcap_raw != (Fontcap_raw *) NULL) free((Voidptr) fontcap_raw);
	(void) close (fd);

	return (status);
}


