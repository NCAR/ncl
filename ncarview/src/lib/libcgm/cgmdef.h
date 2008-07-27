/*
 *	$Id: cgmdef.h,v 1.13 2008-07-27 03:22:38 haley Exp $
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

typedef	enum {
	DEL_ELEMENT,
	DES_ELEMENT,
	PIC_DEC_ELEMENT,
	CON_ELEMENT,
	GRP_ELEMENT,
	ATT_ELEMENT,
	ESC_ELEMENT
} CGM_ElementClass;

/* 
 *	cgm delimeter elements (class 0)	
 */
typedef	enum {
	NOOP_ID,		/* no operation		*/
	BEG_MF_ID,		/* begin metafile	*/
	END_MF_ID,		/* end metafile		*/
	BEG_PIC_ID,		/* begin picture	*/
	BEG_PIC_B_ID,		/* begin picture body	*/
	END_PIC_ID		/* end picture		*/
} CGM_Class0Element;

/*
 *	cgm descriptor elements (class 1)
 */
typedef	enum {
	MF_VERSION_ID = 1,
	MF_DESCRIPTION_ID,
	VDC_TYPE_ID,
	INT_PREC_ID,
	REAL_PREC_ID,
	INDEX_PREC_ID,
	COLOUR_PREC_ID,
	COLOUR_INDEX_PREC_ID,
	MAX_COLOUR_ID,
	COLOUR_VAL_EXTEND_ID,
	MF_ELIST_ID,
	MF_DEFAULTS_ID,
	FONT_LIST_ID,
	CHAR_SET_LIST_ID,
	CHAR_COD_ANN_ID,
	NAME_PREC_ID,
	MAX_VDC_EXTENT
} CGM_Class1Element;
	

/*
 *	Picture Descriptor Elements
 */
typedef	enum {
	SCALE_MODE_ID = 1,
	COLOUR_SELECT_MODE_ID,
	LINE_WIDTH_SPEC_MODE_ID,
	MARKER_SIZE_SPEC_MODE_ID,
	EDGE_WIDTH_SPEC_MODE_ID,
	VDC_EXTENT_ID,
	BACKGROUND_COLOUR_ID
} CGM_Class2Element;


/*
 *	CGM Control Elements
 */
typedef	enum {
	VDC_INT_PREC_ID = 1,
	VDC_REAL_PREC_ID,
	AUXILLARY_COLOUR_ID,
	TRANSPARENCY_ID,
	CLIP_RECTANGLE_ID,
	CLIP_INDICATOR_ID
} CGM_Class3Element;


/*
 *	cgm graphical primitive elements (class 4)
 */

typedef	enum {
	POLYLINE_ID = 1,
	DISJ_POLYLINE_ID,
	POLYMARKER_ID,
	TEXT_ID,
	RESTRICTED_TEXT_ID,
	APPEND_TEXT_ID,
	POLYGON_ID,
	POLYGON_SET_ID,
	CELL_ARRAY_ID,
	G_D_P_ID,
	RECTANGLE_ID,
	CIRCLE_ID,
	CIRULAR_ARC_3_PT_ID,
	CIRULAR_ARC_3_PT_CLOSE_ID,
	CIRULAR_ARC_CENTRE_ID,
	CIRULAR_ARC_CENTRE_CLOSE_ID,
	ELIPSE_ID
} CGM_Class4Element;

	

/*
 *	cgm attribute elements (class 4)
 */

typedef	enum {
	LINE_BUNDLE_INDEX_ID = 1,
	LINE_TYPE_ID,
	LINE_WIDTH_ID,
	LINE_COLOUR_ID,
	MARKER_BUNDLE_INDEX_ID,
	MARKER_TYPE_ID,
	MARKER_WIDTH_ID,
	MARKER_COLOUR_ID,
	TEXT_BUNDLE_INDEX_ID,
	TEXT_FONT_INDEX_ID,
	TEXT_PRECISION_ID,
	CHAR_EXPAN_FACTOR_ID,
	CHAR_SPACING_ID,
	TEXT_COLOUR_ID,
	CHAR_HEIGHT_ID,
	CHAR_ORIENTATION_ID,
	TEXT_PATH_ID,
	TEXT_ALIGN_ID,
	CHAR_SET_INDEX_ID,
	ALT_CHAR_SET_INDEX_ID,
	FILL_BUNDLE_INDEX_ID,
	INTERIOR_STYLE_ID,
	FILL_COLOUR_ID,
	HATCH_INDEX_ID,
	PATTERN_INDEX_ID,
	EDGE_BUNDLE_ID,
	EDGE_TYPE_ID,
	EDGE_WIDTH_ID,
	EDGE_COLOUR_ID,
	EDGE_VISIBILITY_ID,
	FILL_REF_PT_ID,
	PATTERN_TAB_ID,
	PATTERN_SIZE_ID,
	COLOR_TABLE_ID,
	ASPECT_SOURCE_FLAG_ID
} CGM_Class5Element;
	

/*
 *	cgm escape elements (class 6)
 */
typedef	enum {
	ESCAPE_ID = 1
} CGM_Class6Element;

/*
 *	cgm external elements
 */
typedef	enum {
	MESSAGE_ID = 1,
	APPLICATION_ID
} CGM_Class7Element;


#endif	/* _cgmdef_	*/
