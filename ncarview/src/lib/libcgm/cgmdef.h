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
 *	Class 2 defines
 */

#define	INDEXED	0
#define	DIRECT	1


#define ABSOLUTE	0
#define SCALED		1

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





/*	interior style	*/
#define	HOLLOW_S	0	
#define	SOLID_S		1	
#define	PATTERN_S	2
#define	HATCH_S		3
#define	EMPTY_S		4

/*	hatch	index	*/
#define	HORIZONTAL	1
#define	VERTICAL	2
#define	POSSITIVE	3
#define	NEGATIVE	4
#define	HORIZ_VERT	5
#define	POSS_NEG	6

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

#define BEG_MF		1	/* begin metafile	*/
#define END_MF		2	/* end metafile		*/
#define BEG_PIC		3	/* begin picture	*/
#define BEG_PIC_B	4	/* begin picture body	*/
#define END_PIC		5	/* end picture		*/

/*
 *	cgm descriptor elements (class 1)
 */
#define	DES_ELEMENT	1

#define	MF_VERSION	1
#define	MF_ELIST	11
#define	MF_DEFAULTS	12

/*
 *	cgm graphical primitive elements (class 4)
 */
#define	GRP_ELEMENT	4

#define CELL_ARRAY	9

/*
 *	cgm attribute elements (class 4)
 */
#define	ATT_ELEMENT	5

#define COLOR_TABLE	34


#endif	_cgmdef_
