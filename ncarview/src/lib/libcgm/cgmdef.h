/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

#ifndef	_cgmdef_
#define	_cgmdef_

#define	MAXCLASS	7	/* maximum number of CGM classes	*/
#define	MAXFUNCPERCLASS	35	/* maximum CGM elements in a CGM class	*/

/* 
 *	Class 1 defines
 */

#define	VDC_IS_INT	0
#define	VDC_IS_REAL	1

#define	REAL_MODE_FLOAT	0
#define	REAL_MODE_FIXED	1

#define	BASIC_7_BIT	0
#define	BASIC_8_BIT	1
#define	EXTENDED_7_BIT	2
#define	EXTENDED_8_BIT	3

/* 
 *	Class 2 defines
 */
#define	MODE_ABSTRACT	0
#define	MODE_METRIC	1

#define	MODE_INDEXED	0
#define	MODE_DIRECT	1


#define MODE_ABSOLUTE	0
#define MODE_SCALED	1

/*
 *	Class 5 defines
 */

/*	line type	*/
#define	L_SOLID		1
#define	L_DASH		2
#define	L_DOT		3
#define	L_DASH_DOT	4
#define	L_DASH_DOT_DOT	5

/*
 *      The supported marker types
 */
#define MARKER_X        5
#define MARKER_CIRCLE	4
#define MARKER_STAR     3
#define MARKER_PLUS     2
#define MARKER_DOT      1




/*
 *	text precision
 */
#define	PREC_STRING	0
#define	PREC_CHAR	1
#define	PREC_STROKE	2

/*  text path   */
#define PATH_RIGHT      0       /* text path is right   */
#define PATH_LEFT       1       /* text path is left    */
#define PATH_UP         2       /* text path is up      */
#define PATH_DOWN       3       /* text path is down    */

/*enumerated type values for the CGM element text alignment     */
#define A_NORM_H        0       /*normal horizontal     */
#define A_LEFT          1
#define A_CENTER        2
#define A_RIGHT         3
#define A_CO_HOR        4       /*continous horizontal  */

#define A_NORM_V        0       /*normal vertical       */
#define A_TOP           1
#define A_CAP           2
#define A_HALF          3
#define A_BASE          4
#define A_BOTTOM        5
#define A_CO_VER        6       /*continous vertical    */

/*	interior style	*/
#define	HOLLOW_S	0	
#define	SOLID_S		1	
#define	PATTERN_S	2
#define	HATCH_S		3
#define	EMPTY_S		4

/*	hatch	index	*/
#define	HORIZONTAL	1
#define	VERTICAL	2
#define	POSITIVE	3
#define	NEGATIVE	4
#define	HORIZ_VERT	5
#define	POS_NEG	6

/*	hatch index help	*/
#define	H_ANGLE		0.0
#define	V_ANGLE		90.0
#define	P_ANGLE		45.0
#define	N_ANGLE		135.0

/*
 *	The following is an incomplete list used to identify
 *	some of the CGM elements
 */


/* 
 *	cgm delimeter elements (class 0)	
 */
#define	DEL_ELEMENT	0	

#define	NOOP_ID		0	/* no operation		*/
#define BEG_MF_ID	1	/* begin metafile	*/
#define END_MF_ID	2	/* end metafile		*/
#define BEG_PIC_ID	3	/* begin picture	*/
#define BEG_PIC_B_ID	4	/* begin picture body	*/
#define END_PIC_ID	5	/* end picture		*/

/*
 *	cgm descriptor elements (class 1)
 */
#define	DES_ELEMENT	1

#define	MF_VERSION_ID	1
#define	MF_ELIST_ID	11
#define	MF_DEFAULTS_ID	12

/*
 *	cgm graphical primitive elements (class 4)
 */
#define	GRP_ELEMENT		4

#define POLYGON_ID		7
#define CELL_ARRAY_ID		9

/*
 *	cgm attribute elements (class 4)
 */
#define	ATT_ELEMENT		5

#define	INTERIOR_STYLE_ID	22
#define	FILL_COLOUR_ID		23
#define COLOR_TABLE_ID		34

/*
 *	cgm escape elements (class 6)
 */
#define	ESC_ELEMENT		6

#define	ESCAPE_ID		1


#endif	/* _cgmdef_	*/
