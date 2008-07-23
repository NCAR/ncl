/*
 *	$Id: text.h,v 1.4 2008-07-23 17:29:43 haley Exp $
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
 *	File		text.h
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun  5 18:35:24 MDT 1991
 *
 *	Description:	Defines for text.c
 */	

#ifndef	_text_
#define	_text_

#include <ncarg/c.h>
#include "common.h"

/*
 *	text attribute setting masks
 */
#define	TEXT_FONT_SG		(1L << 0)
#define	TEXT_PRECISION_SG	(1L << 1)
#define	CHAR_EXPAN_FACTOR_SG	(1L << 2)
#define	CHAR_SPACING_SG		(1L << 3)
#define	CHAR_HEIGHT_SG		(1L << 4)
#define	CHAR_ORIENTATION_SG	(1L << 5)
#define	TEXT_PATH_SG		(1L << 6)
#define	TEXT_ALIGNMENT_SG	(1L << 7)


/*
 *	data structure for setting/storing text attributes
 */
typedef	struct	TextAttribute_ {
	int	text_font;		/* text font index		*/
	int	text_precision;		/* stroke precision		*/
	float	char_expan_factor;	/* character expansion factor	*/
	float	char_spacing;		/* character spacing		*/
	float	char_height;		/* absolute character height	*/
	struct	{
		float	x_up, y_up,
			x_base, y_base;
		} orientation;		/* orientation of characters	*/
	int	text_path;		/* direction of text path	*/
	struct	{
		int	horizontal,
			vertical;
		}text_alignment;	/* alignment of text 		*/

	} TextAttribute;



/*
 *	data structure for storing current text attributes and for caching
 *	information needed to translate a text string quickly
 */
typedef	struct	TextInfoType_	{
        TextAttribute   ta;
	Boolean	dirty;		/* true => attributes have changed	*/
	Boolean	font_init;	/* true => have a valid font		*/

		/*
		 * coordinate transformation matrix necessary for scaling and 
		 * rotating  of characters within each characters space.
		 */
        float   matrix[2][2];
        Boolean	var_space;
	float	cos_base, sin_base,
		/*
		 * sin and cos of angle of rotation of "up vector" with  
		 * respect to the y coordinate  of the screen.
		 */
		cos_up, sin_up;
	int	spacing;
	int	x_spacing,
		y_spacing;

	} TextInfoType; 


extern	int	SetTextAttribute(
#ifdef	NeedFuncProto
	TextAttribute	*ta,
	unsigned long	mask
#endif
);

int     Text_(
#ifdef	NeedFuncProto
        int     x,
	int	y,
        char    *string,
        void    (*lines)(),
        Voidptr lines_data
#endif
);

#endif	/*	_text_	*/
