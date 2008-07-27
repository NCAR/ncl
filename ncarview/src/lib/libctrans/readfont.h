/*
 *	$Id: readfont.h,v 1.6 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*
 *	readfont.h
 *
 *	author		John Clyne
 *
 *	Date		Wed Mar 15 09:52:01 MST 1989
 *
 *	reafont.h contains the struct representing the raw fontcap
 *	as well as one to represent the processed (cooked) one.
 *	Also contains macros for accessing data in cooked fontcap
 */

#ifndef	_readfont_
#define	_readfont_

#define CHRSM1  	128	/* max number of chars in the font	*/
#define CHRSM2  	5121	/* max size of array describing fonts	*/

#define	C_S_SPACE	11	/* size of character scratch space	*/
#define	F_S_SPACE	9	/* size of font scratch space		*/
#define	CO_S_SPACE	10	/* size of font scratch space		*/
#define	NC_SPACE	300	/* size of font scratch space		*/
#define	WDTH_SPACE	CHRSM1

/*
 * valid font types
 */
#define	MONO_SPACE	0
#define	VAR_SPACE	1
#define	MONO_FILL	2
#define	VAR_FILL	3

/*
 *	The raw Fontcap. This struct coresponds precisely to the layout
 *	of the binary fontcap produced by the font pre-processor. 
 *	The order of items in the struct MUST not be changed unless
 *	the layout of the binary fontcap is modified.
 */
typedef struct {
	int	char_start,		/* character_start		*/
		char_end, 		/* character_end		*/
		char_width,		/* width for MONO spaced font	*/

		char_scratch[C_S_SPACE],/* space for future expansion	*/

		font_right,		/* right side of font coord sys	*/
		font_top,		/* top of font coord system	*/
		font_cap,		/* top of captial letter in sys	*/
		font_half,		/* vertical center of capital	*/
		font_base,		/* vertical base of capital let	*/
		font_bottom,		/* vert bottom of font coord sys*/

		font_type,		/* type of font (0,1,2,3)	*/

		font_scratch[F_S_SPACE],/* space for future expansion	*/


		c_x_start,		/* position of x coord bits	*/
		c_x_len,		/* num bits in x coord		*/
		c_y_start,		/* position of y coord bits	*/
		c_y_len,		/* num bits in y coord		*/
		c_pen_start,		/* position of pen up/down bits	*/
		c_pen_len,		/* num bits in pen up/down	*/
		pt_beg_start,		/* position of paint begin bits	*/	
		pt_beg_len,		/* num bits in paint begin	*/
		pt_end_start,		/* position of paint end bits	*/	
		pt_end_len,		/* num bits in paint end	*/

		coord_scratch[CO_S_SPACE],	/* coord scratch space	*/

		new_class[NC_SPACE],	/* space for new classes	*/


		pointers[CHRSM1],	/* index in to strokes		*/
		last_pointer,		/* index to last stroke		*/
		strokes[CHRSM2];	/* strokes makeing up the font	*/


} Fontcap_raw;


/*
 *	pen position - struct defining a single coordinate in a single
 *	character in the font.
 */	
typedef struct {
	int	x_coord,
		y_coord;
	boolean pen,
		paint_st,
		paint_ed;
		
} Pen_coord, *P_c;

/*
 *	character description -  struct completely defining a single character
 *	stroke sequence
 */
typedef	struct {
	P_c	p_c;
	int	numstroke;
} Char_des;


/*
 *	Fontcap cooked - The processed fontcap. Describes every character
 *	in the font
 */
typedef struct	{
	int	char_start,		/* character_start		*/
		char_end, 		/* character_end		*/
		char_height,		/* height of a character (norm)	*/
		char_width,		/* charcter_width		*/

		font_right,
		font_top,
		font_cap,
		font_half,
		font_base,
		font_bottom,
		font_type;


	int	numchar;		/* number of chars in the font	*/

	Char_des	char_des[CHRSM1];	/* an array that describes
						 * the stroke sequence for
						 * each char in the font
						 */
} Fcap;

#define	F_CHAR_START(F)		((F).char_start)
#define	F_CHAR_END(F)		((F).char_end)
#define	F_CHAR_HEIGHT(F)	((F).char_height)
#define	F_CHAR_WIDTH(F)		((F).char_width)
#define	F_FONT_RIGHT(F)		((F).font_right)
#define	F_FONT_TOP(F)		((F).font_top)
#define	F_FONT_CAP(F)		((F).font_cap)
#define	F_FONT_HALF(F)		((F).font_half)
#define	F_FONT_BASE(F)		((F).font_base)
#define	F_FONT_BOTTOM(F)	((F).font_bottom)
#define	F_FONT_TYPE(F)		((F).font_type)
	
#define	F_NUMCHAR(F)		((F).numchar)

#define	F_X_COORD(F, C, S)	((F).char_des[(C)].p_c[(S)].x_coord)
#define	F_Y_COORD(F, C, S)	((F).char_des[(C)].p_c[(S)].y_coord)
#define	F_PEN(F, C, S)		((F).char_des[(C)].p_c[(S)].pen)
#define	F_PAINT_ST(F, C, S)	((F).char_des[(C)].p_c[(S)].paint_st)
#define	F_PAINT_ED(F, C, S)	((F).char_des[(C)].p_c[(S)].paint_ed)
#define	F_NUMSTROKE(F, C)	((F).char_des[(C)].numstroke)

#ifdef	READFONT
	Fontcap_raw	*fontcap_raw;	/* the raw unprocessed fontcap	*/
	
	Fcap		fcap_template,	/* processed original fontcap	*/
			fcap_current;	/* translated fontcap		*/

	int		leftExtent1[WDTH_SPACE];
	int		rightExtent1[WDTH_SPACE];
	boolean	var_space;		/* true if variable spacing	*/
#else
	extern Fcap	fcap_template,
			fcap_current;
	extern	int	leftExtent1[];
	extern	int	rightExtent1[];
	extern	boolean	var_space;

#endif


extern	int     Init_Readfont(
#ifdef	NeedFuncProto
	char    *fontcap
#endif
);

#endif	/* _readfont_	*/
