/*
 *	$Id: gcread.c,v 1.11 2008-07-27 03:18:44 haley Exp $
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
/*
 *	gcread.c:
 *	
 *	Author		Gary Horton
 *
 *	Date		Feb, 1988.
 *
 *	
 *	[revised]	John clyne (clyne@bierstadt.ucar.edu)
 * 
 *	Date		Wed Mar 15 09:52:01 MST 1989
 *
 *	
 *		Read in the binary version of the graphcap. Convert
 *	Fortran strings represeted as ints into C strings.
 *		
 */

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#define GCREAD
#include "graphcap.h"
#include "ctrandef.h"

/*
 *	F_2_Cstring:
 *	[internal]
 *
 *	convert an array of integers in to  a C string
 *
 * on entry
 *	Fstring		: an integer array
 *	len		: num elements to be converted in Fstring
 * on exit
 *	Fstring		: converted to a C string
 *	
 */
static
F_2_Cstring(Fstring, len)
	int	*Fstring;	
	int	len;
{
   
	int	i;
	SignedChar	*ptr;

	for (i=0,ptr = (SignedChar *) Fstring; i<len; i++, ptr++,Fstring++){
		*ptr = (SignedChar) *Fstring; 
	}

	/*
	 *	add null C string terminator
	 */
	*ptr = '\0';
}

/*
 *	GP_Init:
 *
 *	Open the binary graphcap file, read it into the "Graphcap" 
 *	convert Fortran character strings into C strings.
 *
 * on entry
 *	gcfile		: path to the binary file	
 */
int	GP_Init(gcfile)
	char gcfile[];
{
	int	fd;


	if ((fd = open(gcfile,O_RDONLY)) < 0) {
		ESprintf(errno, "open(%s,O_RDONLY)", gcfile);
		return (-1);
	}

	/*
	 *	read in the graphcap
	 */
#ifdef	VMS
	if (VMSgcread(fd, (char *) &graphcap, GCAP_SIZE) != GCAP_SIZE) {
		ESprintf(errno, "VMSgcread(%d, ,%d)", fd, GCAP_SIZE);
		return (-1);
	}
#else
	if (read(fd, (char *) &graphcap, GCAP_SIZE) != GCAP_SIZE) {
		ESprintf(errno, "read(%d, ,%d)", fd, GCAP_SIZE);
		return(-1);
	}
#endif

	/*
	 * 	convert fortrans chars into C strings
	 */
		/*
		 *	device class
		 */

	F_2_Cstring(graphcap.dev_class.graphic_init, 
			graphcap.dev_class.graphic_init_size); 

	F_2_Cstring(graphcap.dev_class.erase, 
			graphcap.dev_class.erase_size); 

	F_2_Cstring(graphcap.dev_class.text_init, 
			graphcap.dev_class.text_init_size); 

	F_2_Cstring(graphcap.dev_class.cursor_home, 
			graphcap.dev_class.cursor_home_size); 
		/*
		 *	line stuff
		 */

	F_2_Cstring(graphcap.dev_line.line_draw_start, 
			graphcap.dev_line.line_draw_start_size); 

	F_2_Cstring(graphcap.dev_line.line_draw_term, 
			graphcap.dev_line.line_draw_term_size); 

	F_2_Cstring(graphcap.dev_line.line_move_start, 
			graphcap.dev_line.line_move_start_size); 

	F_2_Cstring(graphcap.dev_line.line_move_term, 
			graphcap.dev_line.line_move_term_size); 

	F_2_Cstring(graphcap.dev_line.line_colour_start, 
			graphcap.dev_line.line_colour_start_size); 

	F_2_Cstring(graphcap.dev_line.line_colour_term, 
			graphcap.dev_line.line_colour_term_size); 

	F_2_Cstring(graphcap.dev_line.line_width_start, 
			graphcap.dev_line.line_width_start_size); 

	F_2_Cstring(graphcap.dev_line.line_width_term, 
			graphcap.dev_line.line_width_term_size); 

	F_2_Cstring(graphcap.dev_line.line_back_col_start, 
			graphcap.dev_line.line_back_col_start_size); 

	F_2_Cstring(graphcap.dev_line.line_back_col_term, 
			graphcap.dev_line.line_back_col_term_size); 

	F_2_Cstring(graphcap.dev_line.line_point_start, 
			graphcap.dev_line.line_point_start_size); 

	F_2_Cstring(graphcap.dev_line.line_point_term, 
			graphcap.dev_line.line_point_term_size); 

		/*
		 *	User prompt
		 */
	F_2_Cstring(graphcap.dev_user_prompt.user_prompt, 
			graphcap.dev_user_prompt.user_prompt_size); 

		/*
		 *	Colour stuff
		 */
	F_2_Cstring(graphcap.dev_colour.map_start, 
			graphcap.dev_colour.map_start_size); 

	F_2_Cstring(graphcap.dev_colour.map_term, 
			graphcap.dev_colour.map_term_size); 

		/*
		 *	Polygon stuff
		 */
	F_2_Cstring(graphcap.dev_poly.polygon_colour_start, 
			graphcap.dev_poly.polygon_colour_start_size); 

	F_2_Cstring(graphcap.dev_poly.polygon_colour_term, 
			graphcap.dev_poly.polygon_colour_term_size); 

	F_2_Cstring(graphcap.dev_poly.polygon_start, 
			graphcap.dev_poly.polygon_start_size); 

	F_2_Cstring(graphcap.dev_poly.polygon_term, 
			graphcap.dev_poly.polygon_term_size); 

	F_2_Cstring(graphcap.dev_poly.poly_back_col_start, 
			graphcap.dev_poly.poly_back_col_start_size); 

	F_2_Cstring(graphcap.dev_poly.poly_back_col_term, 
			graphcap.dev_poly.poly_back_col_term_size); 

	F_2_Cstring(graphcap.dev_poly.poly_point_start, 
			graphcap.dev_poly.poly_point_start_size); 

	F_2_Cstring(graphcap.dev_poly.poly_point_term, 
			graphcap.dev_poly.poly_point_term_size); 
		/*
		 *	Raster stuff
		 */
	F_2_Cstring(graphcap.dev_raster.raster_hor_start, 
			graphcap.dev_raster.raster_hor_start_size); 

	F_2_Cstring(graphcap.dev_raster.raster_hor_term, 
			graphcap.dev_raster.raster_hor_term_size); 

#ifdef	DEBUG_GCAP
	print_graphcap();
#endif

	(void) close (fd);

	/* successful */

	return (0);

} /* end of GP_Init  */





#ifdef	DEBUG_GCAP
/*
 *	print_format
 *	[internal]
 *
 *	print out formated encoding data from graphcap
 *
 * on entry
 *	format		: an array of formating data
 *	size		: num elements in format
 */
static
print_format(format, size)
	int	format[];
	int	size;
{
	int	i;
	(void) fprintf (stderr, "	start	count	type	value\n");
	for (i=0; i<size; i++) {
		(void) fprintf(stderr, "	%d	%d	%d	%d\n", 
			format[i], format[i+size], 
			format[i+(2*size)], format[i + (3*size)]);
	}
}

/*
 *	print_graphcap:
 *	[internal]
 *
 *	print out contents of graphcap
 */
static
print_graphcap()
{

	
	(void) fprintf (stderr, 
		"******* Device class informatioin *******\n\n");

	(void) fprintf (stderr, "GRAPHIC_INIT            %s\n", GRAPHIC_INIT); 
	(void) fprintf (stderr, "TEXT_INIT               %s\n", TEXT_INIT); 
	(void) fprintf (stderr, "BATCH                   %d\n", BATCH); 
	(void) fprintf (stderr, "ERASE                   %s\n", ERASE); 
	(void) fprintf (stderr, "CURSOR_HOME             %s\n", CURSOR_HOME); 
	(void) fprintf (stderr, "USER_PROMPT                    %s\n", 
								USER_PROMPT); 


	(void) fprintf (stderr, 
		"\n******* Workstation Drawing Space *******\n\n");

	(void) fprintf (stderr, "COORD_LOWER_LEFT_X      %d\n", 
						LOWER_LEFT_X); 
	(void) fprintf (stderr, "COORD_LOWER_LEFT_Y      %d\n", LOWER_LEFT_Y); 
	(void) fprintf (stderr, "COORD_UPPER_RIGHT_X     %d\n", UPPER_RIGHT_X); 
	(void) fprintf (stderr, "COORD_UPPER_RIGHT_Y     %d\n", UPPER_RIGHT_Y); 
	(void) fprintf (stderr, "XOFFSET                 %d\n", XOFFSET); 
	(void) fprintf (stderr, "YOFFSET                 %d\n", YOFFSET); 
	(void) fprintf (stderr, "XSCALE                  %f\n", XSCALE); 
	(void) fprintf (stderr, "YSCALE                  %f\n", YSCALE); 
	(void) fprintf (stderr, "COORD_FORMAT \n" ); 
		print_format((int *) COORD_FORMAT ,COORD_FORMAT_SIZE); 
	(void) fprintf (stderr, "COORD_ENCODING %d\n", COORD_ENCODING);
	(void) fprintf (stderr, "COORD_FLOATING_INFO \n" ); 

	(void) fprintf (stderr, 
		"*\n****** Device Vector Counts *******\n\n");

	(void) fprintf (stderr,"VECTOR_COUNT_ENCODING   %d\n",
					VECTOR_COUNT_ENCODING);
	(void) fprintf (stderr, "VECTOR_COUNT_FLOAT_ENCODING_INFO \n" ); 
	(void) fprintf (stderr, "VECTOR_COUNT_FORMAT \n" ); 
		print_format((int *)VECTOR_COUNT_FORMAT, 
					VECTOR_COUNT_FORMAT_SIZE);

	(void) fprintf (stderr, 
		"\n******* Device Color Capabilities *******\n\n");

	(void) fprintf (stderr, "COLOUR_AVAILABLE         %d\n",COLOUR_AVAIL); 
	(void) fprintf (stderr, "COLOUR_INDEX_ENCODING    %d\n",
							COLOUR_INDEX_ENCODING); 
	(void) fprintf (stderr, "COLOUR_INDEX_FORMAT \n"); 
		print_format((int *)COLOUR_INDEX_FORMAT, 
					COLOUR_INDEX_FORMAT_SIZE);

	(void) fprintf (stderr, "COLOUR_INDEX_FLOAT_INFO \n"); 
	(void) fprintf (stderr, "MAP_AVAILABLE           %d\n", MAP_AVAIL); 
	(void) fprintf (stderr, "MAP_INDEX_RANGE_MAX     %d\n", 
							MAP_INDEX_MAX); 
	(void) fprintf (stderr, "MAP_INDEX_RANGE_DEFINED %d\n", 
					MAP_INDEX_DEFINED); 

	(void) fprintf (stderr, "MAP_INTENSITY_ENCODING  %d\n",
						MAP_INTENSITY_ENCODING); 

	(void) fprintf (stderr, "MAP_INTENSITY_FLOAT_ENCODING_INFO \n"); 
	(void) fprintf (stderr, "MAP_INTENSITY_FORMAT \n" ); 
		print_format((int *)MAP_INTENSITY_FORMAT, 
						MAP_INTENSITY_FORMAT_SIZE);

	(void) fprintf (stderr, "MAP_INDIVIDUAL          %d\n", MAP_INDIVIDUAL); 
	(void) fprintf (stderr, "MAP_INSTRUCTION_START   %s\n", 
						MAP_START); 
	(void) fprintf (stderr, "MAP_INSTRUCTION_TERM    %s\n", 
						MAP_TERM); 
	(void) fprintf (stderr, "MAP_INIT		%s\n",MAP_INIT); 
	(void) fprintf (stderr, "MAP_MODEL               %d\n", MAP_MODEL); 

	(void) fprintf (stderr, 
		"\n******* Line Control *******\n\n");

	(void) fprintf (stderr, "LINE_DRAW_POLY_FLAG            %d\n", 
						POLY_FLAG); 
	(void) fprintf (stderr, "LINE_DRAW_INSTRUCTION_START    %s\n", 
						LINE_DRAW_START);
	(void) fprintf (stderr, "LINE_DRAW_INSTRUCTION_TERM     %s\n", 
						LINE_DRAW_TERM); 
	(void) fprintf (stderr, "LINE_MOVE_INSTRUCTION_START    %s\n", 
						LINE_MOVE_START); 
	(void) fprintf (stderr, "LINE_MOVE_INSTRUCTION_TERM     %s\n", 
						LINE_MOVE_TERM); 
	(void) fprintf (stderr, "DASH_BIT_LENGTH                %d\n", 
						DASH_BIT_LENGTH); 
	(void) fprintf (stderr, "LINE_COLOR_INSTRUCTION_START   %s\n", 
						LINE_COLOUR_START); 
	(void) fprintf (stderr, "LINE_WIDTH_INSTRUCTION_TERM    %s\n", 
						LINE_WIDTH_TERM); 
	(void) fprintf (stderr, "LINE_WIDTH_INSTRUCTION_START   %s\n", 
						LINE_WIDTH_START); 
	(void) fprintf (stderr, "LINE_COLOR_INSTRUCTION_TERM    %s\n", 
						LINE_COLOUR_TERM); 
	(void) fprintf (stderr, "LINE_WIDTH_ENCODING            %d\n",
						LINE_WIDTH_ENCODING); 
	(void) fprintf (stderr, "LINE_WIDTH_FORMAT \n" ); 
		print_format((int *)LINE_WIDTH_FORMAT, LINE_WIDTH_FORMAT_SIZE);
	(void) fprintf (stderr, "LINE_WIDTH_RANGE \n" ); 
	(void) fprintf (stderr, "LINE_WIDTH_SCALE               %f\n", 
						LINE_WIDTH_SCALE); 
	
#ifdef	DEAD
	(void) fprintf (stderr, 
		"\n******* Marker Control *******\n\n");

	(void) fprintf (stderr, "MARKER_COLOR_INSTRUCTION_START %s\n", 
						MARKER_COLOUR_START); 
	(void) fprintf (stderr, "MARKER_COLOR_INSTRUCTION_TERM  %s\n", 
						MARKER_COLOUR_TERM); 
#endif

	(void) fprintf (stderr, 
		"\n******* Graphical Text Control *******\n\n");

	(void) fprintf (stderr, 
		"\n******* Bundle Tables *******\n\n");

	(void) fprintf (stderr, 
		"\n******* Polygon Control *******\n\n");

	(void) fprintf (stderr, "POLYGON_INSTRUCTION_START      %s\n", 
						POLYGON_START); 
	(void) fprintf (stderr, "POLYGON_INSTRUCTION_TERM       %s\n", 
						POLYGON_TERM); 
	(void) fprintf (stderr, "POLYGON_COLOR_INSTR_START      %s\n", 
						POLYGON_COLOUR_START); 
	(void) fprintf (stderr, "POLYGON_COLOR_INSTR_TERM       %s\n", 
						POLYGON_COLOUR_TERM); 

	(void) fprintf (stderr, 
		"\n******* Raster Control *******\n\n");

	(void) fprintf (stderr, "RASTER_COORD_LOWER_LEFT_X      %d\n", 
						RASTER_LOWER_LEFT_X); 
	(void) fprintf (stderr, "RASTER_COORD_LOWER_LEFT_Y      %d\n", 
						RASTER_LOWER_LEFT_Y); 
	(void) fprintf (stderr, "RASTER_COORD_UP_RIGHT_X        %d\n", 
						RASTER_UPPER_RIGHT_X); 
	(void) fprintf (stderr, "RASTER_COORD_UP_RIGHT_Y        %d\n", 
						RASTER_UPPER_RIGHT_Y); 
	(void) fprintf (stderr, "RASTER_COORD_XOFFSET           %d\n", 
						RASTER_XOFFSET); 
	(void) fprintf (stderr, "RASTER_COORD_YOFFSET           %d\n", 
						RASTER_YOFFSET); 
	(void) fprintf (stderr, "RASTER_COORD_XSCALE            %f\n", 
						RASTER_XSCALE); 
	(void) fprintf (stderr, "RASTER_COORD_YSCALE            %f\n", 
						RASTER_YSCALE); 
	(void) fprintf (stderr, "RASTER_DATA_ENCODING           %d\n",
						RASTER_DATA_ENCODING); 
		
	(void) fprintf (stderr, "RASTER_VECTOR_COUNT_FORMAT \n" ); 
		print_format((int *)RASTER_VECTOR_COUNT_FORMAT, 
					RASTER_VECTOR_COUNT_FORMAT_SIZE);
		
	(void) fprintf (stderr, "RASTER_VECTOR_COUNT_ENCODING   %d\n",
						RASTER_VECTOR_COUNT_ENCODING); 
	(void) fprintf (stderr, "RASTER_VECTOR_FLOAT_INFO \n" ); 
	(void) fprintf (stderr, "RASTER_HORIZONTAL_INSTR_START  %s\n", 
						RASTER_HOR_START); 
	(void) fprintf (stderr, "RASTER_HORIZONTAL_INSTR_TERM   %s\n", 
						RASTER_HOR_TERM); 
}
#endif	/* DEBUG_GCAP	*/
