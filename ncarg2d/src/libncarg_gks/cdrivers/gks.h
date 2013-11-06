/*
 *	$Id: gks.h,v 1.9 2008-07-23 17:29:43 haley Exp $
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
 *      File:		gks.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Wed May  1 17:49:30 MDT 1991
 *
 *      Description:	This file contains some definitions about GKS.
 */
#ifndef	_gks_
#define	_gks_

/*
 *	opcodes for gks functions as defined by the gks standard
 */
#define	GKS_GET_COLOR_REPRESENTATION		-256
#define	GKS_OPEN_WORKSTATION			-3
#define	GKS_ACTIVATE_WORKSTATION		-2
#define	GKS_DEACTIVATE_WORKSTATION		-1
#define	GKS_CLOSE_WORKSTATION			0
#define	GKS_CLEAR_WORKSTATION			1
#define	GKS_UPDATE_WORKSTATION			3
#define	GKS_ESCAPE				6
#define	GKS_POLYLINE				11
#define	GKS_POLYMARKER				12
#define	GKS_TEXT				13
#define	GKS_FILL_AREA				14
#define	GKS_CELL_ARRAY				15
#define	GKS_SET_LINETYPE			22
#define	GKS_SET_LINEWIDTH_SCALE_FACTOR		23
#define	GKS_SET_POLYLINE_COLOR_INDEX		24
#define	GKS_SET_MARKER_TYPE			26
#define	GKS_SET_MARKER_SIZE_SCALE_FACTOR	27
#define	GKS_SET_POLYMARKER_COLOR_INDEX		28
#define	GKS_SET_TEXT_FONT_AND_PRECISION		30
#define	GKS_SET_CHARACTER_EXPANSION_FACTOR	31
#define	GKS_SET_CHARACTER_SPACING		32
#define	GKS_SET_TEXT_COLOR_INDEX		33
#define	GKS_SET_CHARACTER_HEIGHT_AND_UP_VECTOR	34
#define	GKS_SET_TEXT_PATH			35
#define	GKS_SET_TEXT_ALIGNMENT			36
#define	GKS_SET_FILL_AREA_INTERIOR_STYLE	38
#define	GKS_SET_FILL_AREA_STYLE_INDEX		39
#define	GKS_SET_FILL_AREA_COLOR_INDEX		40
#define	GKS_SET_COLOR_REPRESENTATION		56
#define	GKS_SET_CLIP_INDICATOR			61
#define	GKS_SET_WINDOW				71
#define	GKS_SET_VIEWPORT			72



/*
 *	line types
 */
#define	SOLID_LINE		1
#define	DASHED_LINE		2
#define	DOTTED_LINE		3
#define	DASH_DOT_LINE		4
#define	DASH_DOT_DOT_LINE	5

/*
 *	marker types
 */
#define	DOT_MARKER	1
#define	PLUS_MARKER	2
#define	STAR_MARKER	3
#define	CIRCLE_MARKER	4
#define	X_MARKER	5

/*
 *	text precisions
 */
#define	STRING_PREC	0
#define	CHAR_PREC	1
#define	STROKE_PREC	2


/*
 *	text paths
 */
#define	RIGHT_TEXT_PATH	0
#define	LEFT_TEXT_PATH	1
#define	UP_TEXT_PATH	2
#define	DOWN_TEXT_PATH	3

/*
 *	text alignments, horizontal
 */
#define	NORMAL_ALIGNMENT_HORIZ	0
#define	LEFT_ALIGNMENT_HORIZ	1
#define	CENTER_ALIGNMENT_HORIZ	2
#define	RIGHT_ALIGNMENT_HORIZ	3

/*
 *	text alignments, vertical
 */
#define	NORMAL_ALIGNMENT_VERT	0
#define	TOP_ALIGNMENT_VERT	1
#define	CAP_ALIGNMENT_VERT	2
#define	HALF_ALIGNMENT_VERT	3
#define	BASE_ALIGNMENT_VERT	4
#define	BOTTOM_ALIGNMENT_VERT	5

/*
 *	polygon fill styles
 */
#define	HOLLOW_FILL	0
#define	SOLID_FILL	1
#define	PATTERN_FILL	2
#define	HATCH_FILL	3
#define SOLID_TEXT_FILL 4     /* added for Jira1667 */

/*
 *	hatch styles
 */
#define	HORIZONTAL_HATCH	1
#define	VERTICAL_HATCH		2
#define	POSITIVE_HATCH		3
#define	NEGATIVE_HATCH		4
#define	HORIZ_VERT_HATCH	5
#define	POS_NEG_HATCH		6

/*
 * clipping indicator
 */
#define CLIPPING_OFF		0
#define CLIPPING_ON 		1

/*
 * maximum allowable colors
 */
#define	MAX_COLORS		256
/*
 * If the color allocated is is more than this percentage different from
 * the one requested, then report a ERR_DIFF_COLOR.
 * A value of 0 would mean "I don't care what color I get" - so no error.
 */
#define	DEF_COLOR_ERR		(10)

/*
 *	local escape codes
 */
#define	ESCAPE_PAUSE		-1396
#define	ESCAPE_COLOR_ERROR	-1400
#define	ESCAPE_PRIVATE_CMAP	-1401
#define	ESCAPE_COLOR_MODEL	-1402

/*
 *  Escapes to be ignored if they come this way.
 */
#define	ESCAPE_COORDS_0   	-1521
#define	ESCAPE_COORDS_1         -1526
#define	ESCAPE_ORIENT           -1525

#endif	/*	_gks_	*/
