/*
 *	$Id: text.c,v 1.18 1993-01-06 21:12:26 clyne Exp $
 */
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
#include <string.h>
#include <math.h>
#include <errno.h>
#include <ncarg/c.h>
#include "ctrans.h"
#include "cgmc.h"
#include "text.h"
#include "readfont.h"
#include "default.h"


/* 	text.c: 
 *		Author : John clyne
 *		3/21/88
 *
 *		This file contains the font processing routines that handle 
 *	CGM commands for text. The file works in conjunction with readfont.c.
 *	text.c uses information stored in the fontcap to produce the necessary
 *	CGMC commands to stroke text via polylines. The data stored in the 
 *	fontcap must first be decoded by readfont.c and stored in the table
 *	"fcap_template". After text.c is initialized and the fontcap is decoded
 *	text processing may begin. The coordinates for the stroke sequences 
 *	stored in fcap_template must be transformed in response to character
 *	control commands received from the CGM. The transformed version of 
 *	fcap_template is stored in fcap_current.
 *
 *	Note to programmer:
 *		In order to understand these routines one must first understand
 *	the meaning of the CGM text commands as described in ISO/DIS 1. In 
 *	particular the commands; Text, Character Expansion Factor, Character 
 *	Spacing, Character Height, Character Orientation, Text Path, and Text
 *	Alignment.
 */	


/* coordinate transformation matrix necessary for scaling and rotating of
 * characters within each characters space. The matrix is global in order
 * to avoid recalculating its value for each "text" command if it is not 
 * explicitly changed.
 */
static	float	matrix[2][2];	



/* globals which retain information necessary for coordinate transformation
 *from one invocation of the font processing routines to the next
 */
static	int	X_spacing, Y_spacing;	/* character spacing		*/
static	int	Width;			/* the absolute width of a 
					 * string  after appropriate spacing 
					 * has  been added.
					 */
static	int	Spacing;		/* spacing added to the character body
					 * in the x or y direction
					 */

static	int	Widtharray2[WDTH_SPACE];/* array containing widths of each
					 * character for variable space fonts
					 */


/* the following vars retain default values that are stored in "default.c".
 * the values are compared against the default.c values to determine if any
 * changes have been made. This will prevent the need to recalculate the 
 * transformation matrix between text commands if the font attributes have not
 * changed.
 */
static	Rtype	Char_space = 0.0;	/* character spacing	*/
static	Rtype 	Char_expan = 1.0;	/* char expansion factor	*/
static	VDCtype	Char_height = 0;	/* char height		*/
static	Etype	Text_path = 0;
static	struct	{
	VDCtype	x_up,
		y_up,
		x_base,
		y_base;
	} Char_orien;		/* character orientation*/


static	CGMC	tempcgmc;		/* since text is stroked with lines
					 * it is necessary to make sure that 
					 * any line attributes that were set
					 * do not effect text to be stoked
					 * So below we save line attributes
					 * and temporarily set them to defaults
					 */					
static	boolean FontIsInit = FALSE;	/* fontcap sucessfully processed? */

static	float	cosBase,
		sinBase,	/* sin and cos of angle of rotation of 
				 * base vector	
				 */
		cosUp,
		sinUp;		/* sin and cos of angle of rotation of 
				 * "up vector" with  respect to the y 
				 * coordinate  of the screen.
			 	 */
static	float	X_scale(height)
	int	height;
{
	float	mag1;
	float	mag2;
	
	mag1 = MAG(CHAR_X_BASE, CHAR_Y_BASE); 
	mag2 = MAG(CHAR_X_UP, CHAR_Y_UP);

	return (((float) CHAR_HEIGHT / (float) height) 
		* CHAR_EXPAN * (mag1 / mag2));
}

/* 	make_matrix:
 *		This routine calculates the transformation matrix necessary
 *		for handling character orientation, character expansion and
 *		character height. 
 *	on entry:
		fcap_template has been filled in
 *	on exit
 *		matrix : contains data necessary to compute transformations
 */
static	make_matrix()
{
	float	sx,sy;		/* scale in x and y direction	*/


	sy = Y_SCALE(F_CHAR_HEIGHT(fcap_template));
	sx = X_scale(F_CHAR_HEIGHT(fcap_template)); 


	/* The origin of the font coordinate system body must be at
	 * x = y = 0
	 */

	cosBase = (float) CHAR_X_BASE / MAG(CHAR_X_BASE,CHAR_Y_BASE);
	sinBase = (float) CHAR_Y_BASE / MAG(CHAR_X_BASE,CHAR_Y_BASE);
	cosUp = (float)   CHAR_Y_UP / MAG(CHAR_X_UP,CHAR_Y_UP);
	sinUp = (float) -(CHAR_X_UP) / MAG(CHAR_X_UP,CHAR_Y_UP);

	/* calculate transformation matrix	*/
	matrix[0][0] = sx * cosBase; 
	matrix[0][1] = sx * sinBase; 
	matrix[1][0] = -sy * sinUp; 
	matrix[1][1] = sy * cosUp; 
}
  
/* 	trans_coord:
 *		perform coordinate transformations on list of character strokes
 *		using the transformation matrix.
 *	on entry
 *		f1 : a pointer to the un altered font table as it is represented
 *			in the fontcap.
 *	on exit
 *		f2 : transformed version of f1 via the transformation matrix
 */
static trans_coord(f1,f2)
	Fcap	*f1, *f2;	/* font lists	*/
{
	int	i,k;
	float	yscale = Y_SCALE(f1->char_height);
	float xscale = X_scale(f1->char_height);

	for (i = 0; i < F_NUMCHAR(*f1); i++) {
		for (k = 0; k < F_NUMSTROKE(*f1, i); k++) {

			F_X_COORD(*f2, i, k) = 
				(F_X_COORD(*f1, i, k) * matrix[0][0]) 
				+ (F_Y_COORD(*f1, i, k) * matrix[1][0]);

			F_Y_COORD(*f2, i, k) = 
				(F_X_COORD(*f1, i, k) * matrix[0][1]) 
				+ (F_Y_COORD(*f1, i, k) * matrix[1][1]); 
		}

	}


	/* if variable spacing is desired	*/
	if (var_space) {
		/* scale widths of individual characters	*/
		for (i=0;i<F_NUMCHAR(*f1) ;i++) 
			Widtharray2[i] = xscale * Widtharray1[i];
	}



	/* scale other portions of font coordinate system 
	 * note: only scaling is done here. Rotation is done in the alignment
	 * routines
	 */

	F_CHAR_HEIGHT(*f2) = F_CHAR_HEIGHT(*f1) * yscale;

	F_CHAR_WIDTH(*f2) = F_CHAR_WIDTH(*f1) * xscale;
	

		/* scale y coordinates parameters*/
	F_FONT_TOP(*f2) = F_FONT_TOP(*f1) * yscale;
	F_FONT_CAP(*f2) = F_FONT_CAP(*f1) * yscale;
	F_FONT_HALF(*f2) = F_FONT_HALF(*f1) * yscale;
	F_FONT_BASE(*f2) = F_FONT_BASE(*f1) * yscale;
	F_FONT_BOTTOM(*f2) = F_FONT_BOTTOM(*f1) * yscale;

		/*scale x coordinates parameters*/
	F_FONT_RIGHT(*f2) = F_FONT_RIGHT(*f1) * xscale;

}

/* 	Init_Font:
 *		This is the initialization routine for the font processor. It
 *		must be invoked with a fontcap file name and path prior to
 *		any font processing
 */
int	Init_Font(fontcap)
	char *fontcap;
{
	static	char	*fontCap = NULL;
	char	*f = fontcap;
	int	status = 0;


	FontIsInit = FALSE;

	if (Init_Readfont(f) != 0) {
		status = -1;

		/*
		 * try and use previous fontcap if it exists
		 */
		if (fontCap) {
			f = fontCap;
			if (Init_Readfont(f) < 0) {
				return (status);
			}
		}
		else return(status);
	}

	/* create space for temp cmgc	*/
	if ( !(tempcgmc.r = (Rtype *) malloc (sizeof(Rtype)))) {
		ESprintf(errno, "malloc(%d)", sizeof(Rtype));
		return(-1);
	}
	if ( !(tempcgmc.ix = (IXtype *) malloc (sizeof(IXtype)))) {
		ESprintf(errno, "malloc(%d)", sizeof(IXtype));
		return(-1);
	}
	if ( !(tempcgmc.ci = (CItype *) malloc (sizeof(CItype)))) {
		ESprintf(errno, "malloc(%d)", sizeof(CItype));
		return(-1);
	}

	/*
	 * this is a hack to make sure translation tables get updated
	 * by modified() when a different font is selected other then
	 * the default. See the modified() function
	 */
	Char_expan = -1;

	FontIsInit = TRUE;

	/*
	 * record name of the fontcap
	 */
	if (fontCap) free((Voidptr) fontCap);
	if (! (fontCap = malloc((unsigned) strlen(f) + 1))) {
		ESprintf(errno, "malloc(%d)", strlen(f) + 1);
		return(-1);
	}
	(void) strcpy(fontCap, f);

	return (status);
}

/* 	path_spacing:
 *		Determine necessary spacing between characters (either 
 *		horizontal or vertical depending on path ) based on "text 
 *		path" and "character spacing". These values are effected and
 *		therefore scaled appropriately by "Character Height", "Character
 *		expansion" and "Character Orientation".				
 *	on exit
 *		Y_spacing,
 *		X_spacing : contain spacing in x and/or y directions. 
 *			    NOTE : This spacing is in the along the Untranslated
 *				    coordinate system, not along base and up 
 *				    vector.
 *		Width :   if mono space font or if path is up or down then 
 *				contain height of and width of character width 
 *			   	appropriate spacing added.
 *			  else contain spacing between characters
 *		Spacing:	the amount of space added to the character
 *				body in either the x or y direction depending
 *				on the path
 *		
 */
static path_spacing ()
{
	int	dir = 1;

	switch (TEXT_PATH) { 
		case PATH_LEFT: 
			dir = -1;	/* negate value	*/
		case PATH_RIGHT:
			Spacing = (int) ((CHAR_SPACE * CHAR_HEIGHT) 
				* (MAG(CHAR_X_BASE,CHAR_Y_BASE) 
				/ MAG(CHAR_X_UP,CHAR_Y_UP)));

			Width = ((var_space) ? 0
				: F_FONT_RIGHT(fcap_current)) + Spacing;

			X_spacing = (int) ( 
				Width * ((float) CHAR_X_BASE / 
					MAG(CHAR_X_BASE,CHAR_Y_BASE)) 
				* dir);

			Y_spacing = (int) ( 
				Width * ((float) CHAR_Y_BASE / 
					MAG(CHAR_X_BASE,CHAR_Y_BASE))
				 * dir);

			break;

		case PATH_DOWN:
			dir = -1;	/* negate value	*/
		case PATH_UP:
			Spacing = CHAR_SPACE * CHAR_HEIGHT;

			/*
			 * this should be "width = top + spacing" if the
			 * font coordinate system truly began at (0,0)
			 */
			Width = (F_FONT_TOP(fcap_current) -
				F_FONT_BOTTOM(fcap_current) + Spacing);


			X_spacing = (int) (
				Width * (CHAR_X_UP / MAG(CHAR_X_UP,CHAR_Y_UP))
				* dir);

			Y_spacing = (int) ( 
				Width * (CHAR_Y_UP / MAG(CHAR_X_UP,CHAR_Y_UP))
				* dir);
			break;
		default:
			break;
	  }
}


/* 	modified:
 *		Test character attribute values against their values at
 *	last invocation. If any changes modify appropriate transformation
 *	tables.
 */
static modified ()
{

	/* these booleans are used to prevent redundant calculations. This can
	 * be a significant time saver if text attributes a changed frequently. 
	 */
	boolean	m_t = FALSE;
	boolean	p = FALSE;

	if (Char_expan != CHAR_EXPAN) {	/* if CHAR_EXPAN has changed	*/
		Char_expan = CHAR_EXPAN;	/* record the change	*/
		m_t = TRUE;			/* set boolean		*/
		p = TRUE;
	}

	if (Char_height != CHAR_HEIGHT) {
		Char_height = CHAR_HEIGHT;
		m_t =  p = TRUE;
	}

	if (Char_space != CHAR_SPACE) {
		Char_space = CHAR_SPACE;
		p = TRUE;
	}

	if (Text_path != TEXT_PATH) {
		Text_path = TEXT_PATH;
		p = TRUE;
	}

	if ((Char_orien.x_up != CHAR_X_UP) ||
		(Char_orien.y_up != CHAR_Y_UP) ||
		(Char_orien.x_base != CHAR_X_BASE) ||
		(Char_orien.y_base != CHAR_Y_BASE)){

		Char_orien.x_up = CHAR_X_UP;
		Char_orien.y_up = CHAR_Y_UP;
		Char_orien.x_base = CHAR_X_BASE;
		Char_orien.y_base = CHAR_Y_BASE;
		m_t = p = TRUE;
	}

	/* if any changes effecting matrix or  Fontable */
	if (m_t) {				
		make_matrix();
		trans_coord(&fcap_template,&fcap_current);
	}

	if (p) {
		path_spacing();
	}
	
}

/*	str_width:
 *		This function calculates the translation along baseline that
 *	in necessary for horizontal  alignment of  the text. The value returned
 *	must be "rotated" to accommodate the rotation of the string due to 
 *	orientation and path.
 *	
 *	on entry:
 *		strlen : length of the text string.
 *		s	: the string
 *		Width  : if mono space font or path is up or down 
 *				absolute Width of string along base vector
 *			 else  contain spacing between characters
 *	on exit:
 *		return = x translation value along  base vector.
 *		 
 */ 

static int	str_width(strlen, s)
	int	strlen;		
	char	*s;
{

	/*switch on horizontal alignment	*/
	switch (TEXT_ALI_H) {

	/*normal alignment see ISO/DIS 8632/1 description on Text Alignment*/
	case A_NORM_H :	
		switch (TEXT_PATH) {
			case PATH_RIGHT :	/*left align*/
					return(0);		

			case PATH_LEFT 	: 	/*right align*/
					return(F_FONT_RIGHT(fcap_current));

			case PATH_UP 	:   	/*centre align*/ 
			case PATH_DOWN 	: 
					return(F_FONT_RIGHT(fcap_current) / 2);
			}

	case A_LEFT :	/* align left	*/
		switch (TEXT_PATH) {
			case PATH_RIGHT : return(0);
			case PATH_LEFT  : 
				if (var_space)
					return (-(var_width( (char *) s+1,
						strlen-1)));

				return(-((strlen-1) * Width));
			case PATH_UP    : 
			case PATH_DOWN  : return(0);
			}

	case A_CENTER :	/*align center	*/
		switch (TEXT_PATH) {
			case PATH_RIGHT : 
				if (var_space)
					return((var_width(s,strlen) - Spacing)/2);
				return(((strlen * Width) - Spacing) / 2);
			case PATH_LEFT 	: 
				if (var_space)
					return(-(((var_width(s,strlen) 
						- Spacing) / 2) 
						- F_FONT_RIGHT(fcap_current)));

				return(-((((strlen * Width) - Spacing) / 2) 
					- F_FONT_RIGHT(fcap_current)));
			case PATH_UP 	: 
			case PATH_DOWN 	: return((F_FONT_RIGHT(fcap_current))/ 2); 
		}

	case A_RIGHT  : 	/*align right	*/
		switch (TEXT_PATH) {
			case PATH_RIGHT : 
				if (var_space) 
					return(var_width(s,strlen) - Width);

				return(((strlen-1) * Width) 
						+ F_FONT_RIGHT(fcap_current));
			case PATH_LEFT 	:  
			case PATH_UP	: 
			case PATH_DOWN 	:  return(F_FONT_RIGHT(fcap_current)); 
		}
	default    :
		return(0);
	}
}

/*	str_heigth:
 *		This function calculates the translation along the up 
 *	vector that is necessary for vertical alignment of the text. 
 *	The value returned must be "rotated" to accommodate the rotation 
 *	of the string due to orientation and path.
 *	
 *	on entry:
 *		strlen 	: the length of the string.
 *		fcap_current has been filled in.
 *		Width  : if mono space font or path is up or down 
 *				absolute Width of string along base vector
 *			 else  contain spacing between characters
 *	on exit:
 *		return = y translation value along up vector.
 *		 
 */ 
static int	str_height(strlen)
	int	strlen;
{
	switch (TEXT_ALI_V) {
	case A_NORM_V :
		switch (TEXT_PATH) {
			case PATH_RIGHT : 
			case PATH_LEFT : 
			case PATH_UP :  return(F_FONT_BASE(fcap_current));
			case PATH_DOWN : return(F_FONT_TOP(fcap_current)); 
		}
	case A_TOP : 
		switch(TEXT_PATH) {
			case PATH_RIGHT :
			case PATH_LEFT :
			case PATH_DOWN :
				return (F_FONT_TOP(fcap_current));
			case PATH_UP :  
				return((Width * (strlen-1)) 
					+ F_FONT_TOP(fcap_current));
		}

	case A_CAP :
		switch(TEXT_PATH) {
			case PATH_RIGHT :
			case PATH_LEFT :
			case PATH_DOWN : 
				return (F_FONT_CAP(fcap_current));
			case PATH_UP :  
				return((Width * (strlen-1)) 
					+ F_FONT_CAP(fcap_current)); 
		}
	case A_HALF : 
		switch(TEXT_PATH) {
			case PATH_RIGHT :
			case PATH_LEFT :
				return (F_FONT_HALF(fcap_current));
			case PATH_DOWN : 
				return (-((((strlen * Width) - Spacing) / 2)
					- F_FONT_TOP(fcap_current)));
			case PATH_UP :  
				return(((Width * strlen) - Spacing) / 2); 
		}
	case A_BASE : 
		switch(TEXT_PATH) {
			case PATH_RIGHT :
			case PATH_LEFT :
			case PATH_UP :  
				return (F_FONT_BASE(fcap_current));
			case PATH_DOWN : 
				return(-1 * ((Width * (strlen-1)) 
					- F_FONT_BASE(fcap_current))); 
		}
	case A_BOTTOM : 
		switch(TEXT_PATH) {
			case PATH_RIGHT :
			case PATH_LEFT :
			case PATH_UP :  
				return (F_FONT_BOTTOM(fcap_current));
			case PATH_DOWN : 
				return(-1 * (Width * (strlen-1))); 
		}
	default :
		return(0);
	}
		
  
}
 
/*	text_align:
 *		This routine calculates the necessary rotation and translation
 *	necessary to support text alignment.
 *
 *	on entry:
 *		strlen : length of string of text
 *		s      : the string
 *	on exit:
 *		transx : calculated adjustment of x coordinate
 *		transy : calculated adjustment of y coordinate
 */
static void	text_align(transx,transy, strlen,s)

	long	*transx,*transy;
	int	strlen;
	char	*s;
{
	float	x,y;	/*temp storage	*/

	x = (float) str_width(strlen,s);	/*translation of x	*/
	y = (float) str_height(strlen);		/*translation of y	*/


	*transx = -((x * cosBase) - (y * sinUp));
	*transy = -((x * sinBase) + (y * cosUp)); 
}


/*	var_width:
 *		This function calculates the width of the text extent
 *	rectangle for a variable spaced font.
 *
 *	on entry:
 *		var_space is true;
 *		strlen : length of string to print
 *		Width  : amount of additional spacing to add due to
 *				CHAR_SPACING	
 *	on exit :
 *		return = Width of text extent rectangle
 */

int	var_width(s,strlen)
	char	*s;
	int	strlen;
{
	int	i;
	int	index;
	int	total = 0;

	for (i=0;i<strlen;i++) {
		index = s[i] - F_CHAR_START(fcap_template);
		total += (Widtharray2[index] + Width);
	}

	return(total);
}

	






/* 	Text:
 *		This function translates the CGM command "text" into the
 *	polylines necessary to stroke the text character codes.
 *	The cgmc's thus contain completely transformed polylines,
 *	no further scaling, rotating or translation is necessary.
 *	Hence, Text is able to invoke Process with cgmc's as arguments
 *
 *	Note:
 *		Characters contained in the text that are note described
 *	in the fontcap are ignored. This INCLUDES any control
 *	characters. eg. newline, tab.
 *
 *	on entry
 *		cgmc : contains a "text" command and its data
 *		X_spacing, Y_spacing : contain spacing in x and y direction
 */

int	Text(cgmc)
	CGMC	*cgmc;
{
	int	index;		/* index into the transformed fontlist	*/
	int	x_space = 0;	/* spacing in x and y direction		*/
	int	y_space = 0;
	int	i,k;
	long	trans_x, trans_y;
	int	numstroke;	/* number of strokes making up the font	*/
	int	char_ind;
	int	str_ind;	/* index to the current string		*/
	int	var_x_adj = 0;
	int	var_y_adj = 0;	/* space adjustment needed for variable space
				 * fonts
				 */
	int	status = 0;

	static	char *string = NULL;
	static	unsigned str_space = 0;

	/* variables that will contain "text" commands input data*/
	static	Ptype	p;  		/* point of origin of text	*/

	/*store old values		*/
	Rtype	line_width = LINE_WIDTH;
	IXtype 	line_type = LINE_TYPE;
	CItype	line_colour = LINE_COLOUR.index;

	/*
	 * make sure font has not changed
	 */
	if (TEXT_F_IND_DAMAGE) {
		if (setFont(TEXT_F_IND) < 0) status = -1;
		TEXT_F_IND_DAMAGE = FALSE;
	}

	if (! FontIsInit)
		return(0);	/* no font, nothing to do	*/

	/*set line width */
	tempcgmc.class = 5;
	tempcgmc.command = 3;
	tempcgmc.r[0] = 1.0;
	tempcgmc.Rnum = 1;
	(void) Process(&tempcgmc);

	/*set line type	*/
	tempcgmc.class = 5;
	tempcgmc.command = 2;
	tempcgmc.ix[0] = 1;
	tempcgmc.IXnum = 1;
	(void) Process(&tempcgmc);

	/*set line colour to text colour	*/
	/*remember text is stroked with lines	*/
	tempcgmc.class = 5;
	tempcgmc.command = 4;
	tempcgmc.ci[0] = TEXT_COLOUR.index;
	tempcgmc.CInum = 1;
	(void) Process(&tempcgmc);
	

	modified();	/* recalc transformation values if attributes changed*/


	/*
	 *	process each string in the cgmc
	 */
	for (str_ind = 0; str_ind < cgmc->Snum; str_ind++) {

	/* make sure their is sufficient space in s.string	*/
	if (( i = cgmc->s->string_space[str_ind]) > str_space) {
		if (string != (char *) NULL) free ((Voidptr) string);
		string = (char *) malloc ((unsigned) i * sizeof(char));
		if (! string) {
			ESprintf(errno, "malloc(%d)", i * sizeof(char));
			return(-1);
		}
		str_space = i;
	}

	(void) strcpy(string,cgmc->s->string[0]);

	/* calculate text alignment	*/
	text_align(&trans_x, &trans_y, strlen(string), string); 

	/*store contents of cgmc	*/
	p.x = cgmc->p[0].x + trans_x; 
	p.y = cgmc->p[0].y + trans_y;


	/* transform text in to cgmc polylines	*/
	for (char_ind=0; char_ind < strlen(string); char_ind++) {
		k = i = 0;		/* stroke index in Fontable and cgmc	*/

		/* index into Fontable*/
		index = string[char_ind] - F_CHAR_START(fcap_template);	

		/* number of strokes making up a character.	*/
		numstroke = F_NUMSTROKE(fcap_current, index);

		/*
		 * if its a non-charcter convert it to a space.
		 */
		if (numstroke <= 1) {
			index = (int) ' ' - F_CHAR_START(fcap_template);
			numstroke = F_NUMSTROKE(fcap_current, index);
		}


#ifdef	DEAD
		if (index >=0 && index < F_NUMCHAR(fcap_template) && numstroke){
#else
		if (index >=0 && index < F_NUMCHAR(fcap_template)){
#endif
			if (numstroke) {


			/* calculate possistion of THIS character for
			 * for variable space font
			 */
			if (var_space && ((TEXT_PATH == PATH_RIGHT) ||
				(TEXT_PATH == PATH_LEFT)))  {

				var_x_adj = 
				var_y_adj = 
					-((F_FONT_RIGHT(fcap_current) 
					- Widtharray2[index])/2);

				var_x_adj *=
					CHAR_X_BASE/MAG(CHAR_X_BASE,CHAR_Y_BASE);
				var_y_adj *=
					CHAR_Y_BASE/MAG(CHAR_X_BASE,CHAR_Y_BASE);
				if (TEXT_PATH == PATH_LEFT) {
					var_x_adj *= -1;
					var_y_adj *= -1;
				}
			}



			/* make sure there is room in cgmc for strokes	*/
			if (cgmc->Pspace <= numstroke) {
				if (cgmc->p != (Ptype *) NULL) 
					free((Voidptr) cgmc->p);

				cgmc->p = (Ptype * ) malloc (
					((unsigned) numstroke + 1)*sizeof(Ptype)
				);
				if (! cgmc->p) {
					ESprintf(errno, "malloc()");
					return(-1);
				}

				cgmc->Pspace = numstroke + 1;
			}

			cgmc->p[k].x = F_X_COORD(fcap_current, index, i)
				+ x_space + var_x_adj + p.x;

			cgmc->p[k].y = F_Y_COORD(fcap_current, index, i)
				+ y_space + var_y_adj + p.y;

			k++; i++; 



			for (;i < numstroke;i++) { 
				if (!(F_PEN(fcap_template, index, i))) {
					cgmc->Pnum = k;

					/*cgm polyline command	*/ 
					cgmc->class = 4;
					cgmc->command = 1;	
#ifdef	DEBUG_TEXT
					draw(cgmc);
#else
					if (cgmc->Pnum > 1) {
						(void) Process(cgmc); 
					}
					k = 0;
#endif
				} 

				cgmc->p[k].x = F_X_COORD(fcap_current, index, i)					+ x_space + var_x_adj + p.x;

				cgmc->p[k].y = F_Y_COORD(fcap_current, index, i)
					+ y_space + var_y_adj + p.y;
				k++; 
			}
			cgmc->Pnum = k;

			cgmc->class = 4;
			cgmc->command = 1; 
#ifdef	DEBUG_TEXT
			draw(cgmc);
#else
			if (cgmc->Pnum > 1) {
				(void) Process(cgmc); 
			}
			k = 0;
#endif
			}	/* if numstroke	*/

			/* calculate possistion of NEXT character for
			 * for variable space font
			 */
			if (var_space && (TEXT_PATH == PATH_RIGHT))  {
				x_space += X_spacing 
					+ (Widtharray2[index] 
				* CHAR_X_BASE/MAG(CHAR_X_BASE,CHAR_Y_BASE));

				y_space += Y_spacing 
					+ (Widtharray2[index] 
				* CHAR_Y_BASE/MAG(CHAR_X_BASE,CHAR_Y_BASE));
			}

			if (var_space && (TEXT_PATH == PATH_LEFT))  {
				x_space += (X_spacing 
					- (Widtharray2[index] 
				* CHAR_X_BASE/MAG(CHAR_X_BASE,CHAR_Y_BASE)));

				y_space += (Y_spacing 
					- (Widtharray2[index] 
				* CHAR_Y_BASE/MAG(CHAR_X_BASE,CHAR_Y_BASE)));
			}
	
			/* calculate possistion of next character for
			 * for mono space font
			 */
			if (!(var_space) || (TEXT_PATH == PATH_UP)
				|| (TEXT_PATH == PATH_DOWN)) {
				x_space += X_spacing; 
				y_space += Y_spacing;
			}

		}	/* if index	*/
	}

	}	/* for loop	*/

	/*now return line modes to normal	*/
	/*set line width */
	tempcgmc.class = 5;
	tempcgmc.command = 3;
	tempcgmc.r[0] = line_width;
	tempcgmc.Rnum = 1;
	(void) Process(&tempcgmc);

	/*set line type	*/
	tempcgmc.class = 5;
	tempcgmc.command = 2;
	tempcgmc.ix[0] = line_type;
	tempcgmc.IXnum = 1;
	(void) Process(&tempcgmc);

	/*set line colour to normal	*/
	tempcgmc.class = 5;
	tempcgmc.command = 4;
	tempcgmc.ci[0] = line_colour;
	tempcgmc.CInum = 1;
	(void) Process(&tempcgmc);

	return (status);
}
 



#ifdef	DEBUG_TEXT
/* 	draw:
 *		debuging routine that prints values of points in computed
 *	polyline
 */
draw (cgmc)
	CGMC	*cgmc;
{
	int i;
	for (i=0;i<cgmc->Pnum;i++) {
		(void) fprintf(stderr,"x coord = %d, y coord = %d\n", cgmc->p[i].x, cgmc->p[i].y);
	}
	(void) fprintf(stderr,"\n");
}
#endif


