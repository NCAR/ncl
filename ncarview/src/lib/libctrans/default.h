/*
 *	$Id: default.h,v 1.18 2008-07-27 03:22:39 haley Exp $
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
 *	Author(s):	Tinsley Galyean (tag@boulder.colorado.edu)
 *			John Clyne
 *
 *	Date:	Thu Mar 10 15:15:44 MST 1988
 *
 */


#include 	<ncarg/c.h> 
#include	<ncarg/cgmdef.h>
#include 	"cgmc.h"
#include 	"text.h"

typedef	struct	RGBTuple_	{	
	unsigned short	red,
			green,
			blue;
	} RGBTuple;

/*
 *	an element in the color lookup table
 */
typedef	struct	ColorElement_ {
	RGBTuple	rgb;
	boolean		damage;		/* element should be updated?	*/
	boolean		defined;	/* element is defined?		*/
	} ColorElement;

/*
 *	The color lookup table
 */
typedef	struct	ColorLUTable_ {
	ColorElement	*ce;		/* color lookup table entries	*/
	int		size;		/* num of elements in ce	*/
	int		total_damage;	/* total ce's touched/modified	*/
	boolean		damage;		/* has any ce been modified	*/
	} ColorLUTable;

typedef	boolean	ColorElementAccess;	/* element has been changed	*/
			

typedef struct {
	Itype	mfversion;	/* METAFILE VERSION 		*/
	Itype 	dcp;		/*COLOUR PRECISION		*/ 
	Itype 	cip;		/*COLOUR INDEX PRECISION	*/
	Itype	max_c_i;	/*MAXIMUM COLOUR INDEX	*/ 

	CDtype	c_v_e_min;	/*COLOUR VALUE EXTENT.minimum	*/
	CDtype	c_v_e_max;	/*COLOUR VALUE EXTENT.maximum	*/

	Etype	char_c_a;	/*CHARACTER CODING ANNOUNCER	*/

	Etype	scalemodee;	/*SCALING MODE enum parm	*/
	Rtype	scalemoder;	/*SCALING MODE real parm	*/

	Etype	mar_siz_mod;	/*MARKER SIZE MODE		*/
	Etype	edg_wid_mod;	/*EDGE WIDTH MODE		*/

	CDtype	backcolr;	/* BACKGROUND	COLOUR		*/
	boolean	backcolr_damage;/* BACKGROUND COLOUR damage	*/

	Etype	trans;		/*TRANSPARENCY			*/

	IXtype	line_ind;	/*LINE BUNDLE INDEX		*/

	IXtype	marker_ind;	/*MARKER BUNDLE INDEX		*/

	IXtype	text_ind;	/*TEXT BUNDLE INDEX	*/
	IXtype	text_f_ind;	/*TEXT FONT INDEX	*/
	boolean	text_f_ind_damage;	/*TEXT FONT INDEX	*/
	Etype	text_prec;	/*TEXT PRECISION		*/


	Etype 	csm;		/*COLOUR SELECTION MODE			*/ 
	Itype 	ixp;		/*INDEX PRECISION		*/ 
	Itype 	ip;		/*INTEGER PRECISION		*/ 
	Etype 	vdc_type;	/*VDC TYPE			*/ 
	Itype 	vdc_int;	/*VDC INTEGER PRECISION		*/
	Itype 	vdc_real_mode;  /*VDC REAL PRECISION.mode		*/	
	Itype 	vdc_real_exp;	/*VDC REAL PRECISION.exponent		*/	
	Itype 	vdc_real_man; 	/*VDC REAL PRECISION.mantisa		*/	

	Itype 	real_mode;	/*REAL PRECISION.mode		*/	
	Itype 	real_exp;   	/*REAL PRECISION.exponent		*/	
	Itype 	real_man;   	/*REAL PRECISION.mantisa		*/	

	long	xmin,ymin,xmax,ymax;	/* VDC EXTENT		*/
	boolean	vdc_extent_damage;	
	long	clipxmin,clipymin,clipxmax,clipymax;
	boolean clipflag;
	boolean	clip_damage;	/* CLIP damage	*/

	int	intprec;	/* Integer Precission */

	IXtype	markertype;
	boolean marker_type_damage;
	int	markerbundleindex;
	Etype	markersizemode;
	float	markersize;
	boolean marker_size_damage;

	Rtype	char_space;
	VDCtype	char_height;
	Rtype	char_expan;

	/*character orientation, vectors for x and y direction	*/
	VDCtype	char_x_up;
	VDCtype	char_y_up;
	VDCtype	char_x_base;
	VDCtype	char_y_base;

	/*text alignment*/
	Etype	text_ali_h;	/*horizontal alignment	*/
	Etype	text_ali_v;	/*vertical alignment	*/
	Rtype	text_ali_c_h;	/*continous horizontal alignment	*/
	Rtype	text_ali_c_v;	/*continous vertical alignment	*/

	Etype	text_path;
	boolean	text_att_damage;

	IXtype	char_ind;	/*CHARACTER SET INDEX		*/
	IXtype	alt_char_ind;	/*ALTERNATE CHARACTER SET INDEX	*/

	IXtype	fill_ind;	/*FILL BUNDLE INDEX		*/
	Etype	int_style;	/*INTERIOR STYLE		*/
	boolean	int_style_damage;	/*interior style damage	*/

	IXtype	hatch_ind;	/*HATCH INDEX			*/
	IXtype	pattern_ind;	/*PATTERN INDEX			*/
	IXtype	edge_ind;	/*EDGE BUNDLE INDEX		*/
	IXtype	edge_type;	/*EDGE TYPE			*/

	Etype	edge_vis;	/*EDGE VISIBILITY		*/
	Ptype	fill_r_pt;	/*FILL REFERENCE POINT		*/

	COtype	line_colour;
	boolean	line_colour_damage; /* true if line_colour needs to be sent */
	COtype	text_colour;
	COtype	fill_colour;
	boolean	fill_colour_damage; /* true if fill_colour needs to be sent */
	COtype	marker_colour;
	boolean	marker_colour_damage;

	IXtype	line_type;
	boolean	line_type_damage;

	Etype	line_width_mode;/* LINE WIDTH SPECIFICATION MODE	*/
	Rtype	line_width;
	boolean	line_width_damage;

} DEFAULTTABLE;


#ifdef DEFAULT
static DEFAULTTABLE	defaulttable = {

	1,				/* METAFILE VERSION 		*/

	8,				/*COLOUR PRECISION		*/
	8,				/*COLOUR INDEX PRECISION	*/
	255,				/*MAXIMUM COLOUR INDEX	*/ 

					/*COLOUR VALUE EXTENT		*/
	0,				/*minimum red	*/
	0,				/*minimum green	*/
	0,				/*minimum blue	*/
	255,				/*maximum red	*/
	255,				/*maximum green	*/
	255,				/*maximum blue	*/

	0,				/*CHARACTER CODING ANNOUNCER	*/

	0,				/*SCALING MODE enum parm	*/
	0.0,				/*SCALING MODE real parm	*/

	1,				/*MARKER SIZE MODE		*/
	1,				/*EDGE WIDTH MODE		*/

	0,0,0,				/* BACKGROUND COLOUR		*/
	TRUE,				/* BACKGROUND COLOUR damage	*/

	1,				/*TRANSPARENCY			*/

	1,				/*LINE BUNDLE INDEX		*/

	1,				/*MARKER BUNDLE INDEX		*/

	1,				/*TEXT BUNDLE INDEX		*/
	1,				/*TEXT FONT INDEX		*/
	TRUE,				/*TEXT FONT INDEX damage	*/
	0,				/*TEXT PRECISION		*/

	0,				/*COLOUR SELECTION MODE 	*/

	16,				/*INDEX PRECISION		*/
	16,				/*INTEGER PRECISION		*/
	0,				/*VDC TYPE			*/
	16,				/*VDC INTEGER PRECISION		*/

	1,16,16,			/*VDC REAL PRECISION		*/	

	1,16,16,			/*REAL PRECISION		*/	

	0,0,32767,32767,		/* VDC Extent defaults 		*/
	TRUE,				/* VDC extent damage		*/
	0,0,32767,32767,		/* Clipping defaults 		*/
	TRUE,				/* Clip flag default False 	*/
	TRUE,				/* clip damage 			*/

	16,

	3,				/* markertype default */
	TRUE,				/* marker type damage	*/
	0,				/* markerbundleindex default */
	MODE_SCALED,			/* markersizemode default */
	1.0,				/* markersize default */
	TRUE,				/* marker size damage	*/

	0.0,				/* char spacing */
	328,				/* char height */
	1.0,				/* char expansion factor */

	/*character orientation, vectors for x and y direction	*/
	0,				/* char x up */
	1,				/* char y up */
	1,				/* char x base */
	0,				/* char y base */

	/*text alignment*/
	A_NORM_H,			/*horizontal alignment	*/
	A_NORM_V,			/*vertical alignment	*/
	0.0,				/*continous horizontal alignment*/
	0.0,				/*continous vertical alignment	*/

	0,				/* text path */
	TRUE,

	1,				/*CHARACTER SET INDEX		*/
	1,				/*ALTERNATE CHARACTER SET INDEX	*/

	1,				/*FILL BUNDLE INDEX		*/
	0,				/*INTERIOR STYLE		*/
	TRUE,				/* interior style damage	*/

	1,				/*HATCH INDEX			*/
	1,				/*PATTERN INDEX			*/
	1,				/*EDGE BUNDLE INDEX		*/
	1,				/*EDGE TYPE			*/

	0,				/*EDGE VISIBILITY		*/
	0,0,				/*FILL REFERENCE POINT		*/

	1,				/* line colour (indexed)	*/
	1,1,1,				/*			direct	*/
	TRUE,				/* line colour damage */

	1,				/* text colour (indexed)	*/
	1,1,1,				/*			direct	*/

	1,				/* fill colour (indexed)	*/
	1,1,1,				/*			direct	*/
	TRUE,				/* fill colour damage */

	1,				/* marker colour (indexed)	*/
	1,1,1,				/*			direct	*/
	TRUE,				/* marker colour damage		*/

	1,				/* line type */
	TRUE,				/* line type damage	*/

	1,				/* line_width_mode	*/
	1.0,				/* line_width */
	TRUE,				/* line width damage */
	
};


#endif

extern 	DEFAULTTABLE		*dt;
extern	ColorLUTable		*clut;

#define	MFVERSION	dt->mfversion	/* METAFILE VERSION		*/

#define E_PREC		16	/*precission for enumerated data types	*/
#define DCP		dt->dcp		/*COLOUR PRECISION		*/
#define CIP	 	dt->cip		/*COLOUR INDEX PRECISION	*/
#define	MAX_C_I		dt->max_c_i	/*MAXIMUM COLOUR INDEX	*/


#define	C_V_E_MIN	dt->c_v_e_min	/*COLOUR VALUE EXTENT.minimum	*/
#define	C_V_E_MAX	dt->c_v_e_max	/*COLOUR VALUE EXTENT.maximum	*/

#define CHAR_C_A	dt->char_c_a	/*CHARACTER CODING ANNOUNCER	*/

#define	SCAlEMODEE	dt->scalemodee	/*SCALE MODE			*/
#define	SCAlEMODER	dt->scalemoder

#define MAR_SIZ_MOD	dt->mar_siz_mod	/*MARKER SIZE MODE		*/

#define EDG_WID_MOD	dt->edg_wid_mod	/*EDGE WIDTH MODE		*/

#define	BACKCOLR	dt->backcolr	/* BACKGROUND COLOUR	*/
#define	BACKCOLR_DAMAGE	dt->backcolr_damage	/* BACKGROUND COLOUR	*/

#define	TRANS		dt->trans	/*TRANSPARENCY			*/

#define	LINE_IND	dt->line_ind	/*LINE BUNDLE INDEX		*/

#define	MARKER_IND	dt->marker_ind	/*MARKER BUNDLE INDEX		*/

#define	TEXT_IND	dt->text_ind	/*TEXT BUNDLE INDEX		*/
#define	TEXT_F_IND	dt->text_f_ind	/*TEXT FONT INDEX		*/
#define	TEXT_F_IND_DAMAGE	dt->text_f_ind_damage	/**/
#define TEXT_PREC	dt->text_prec	/*TEXT PRECISION		*/

#define	CHAR_IND	dt->char_ind	/*CHARACTER SET INDEX		*/
#define	ALT_CHAR_IND	dt->alt_char_ind/*ALTERNATE CHARACTER SET INDEX	*/

#define FILL_IND	dt->fill_ind	/*FILL BUNDLE INDEX		*/
#define INT_STYLE	dt->int_style	/*INTERIOR STYLE		*/
#define INT_STYLE_DAMAGE dt->int_style_damage 	/*INTERIOR STYLE	*/

#define	HATCH_IND	dt->hatch_ind	/*HATCH INDEX			*/
#define	PATTERN_IND	dt->pattern_ind	/*PATTERN INDEX			*/
#define	EDGE_IND	dt->edge_ind	/*EDGE BUNDLE INDEX		*/
#define	EDGE_TYPE	dt->edge_type	/*EDGE TYPE			*/

#define	EDGE_VIS	dt->edge_vis	/*EDGE VISIBILITY		*/
#define FILL_R_PT	dt->fill_r_pt	/*FILL REFERENCE POINT		*/


#define CSM		dt->csm		/*COLOUR SELECTION MODE		*/
#define IXP		dt->ixp		/*INDEX PRECISION		*/
#define IP		dt->ip		/*INTEGER PRECISION		*/
#define VDC_TYPE	dt->vdc_type	/*VDC TYPE			*/
#define VDC_INT		dt->vdc_int	/*VDC INTEGER PRECISION		*/

#define VDC_REAL_MODE   dt->vdc_real_mode	/*VDC REAL PRECISION.mode	*/
#define VDC_REAL_EXP   	dt->vdc_real_exp	/*VDC REAL PRECISION.exponent*/
#define VDC_REAL_MAN   	dt->vdc_real_man	/*VDC REAL PRECISION.mantisa	*/

#define REAL_MODE   	dt->real_mode	/*REAL PRECISION.mode		*/
#define REAL_EXP   	dt->real_exp	/*REAL PRECISION.exponent	*/
#define REAL_MAN   	dt->real_man	/*REAL PRECISION.mantisa	*/	


#define XMIN	dt->xmin
#define YMIN	 dt->ymin
#define XMAX	 dt->xmax
#define YMAX	 dt->ymax
#define	VDC_EXTENT_DAMAGE	dt->vdc_extent_damage

#define CLIPXMIN   dt->clipxmin
#define CLIPYMIN   dt->clipymin
#define CLIPXMAX   dt->clipxmax
#define CLIPYMAX   dt->clipymax

#define CLIPFLAG   dt->clipflag

#define	CLIP_DAMAGE	dt->clip_damage

#define INTPREC	   dt->intprec

#define	MARKER_TYPE		dt->markertype
#define	MARKER_TYPE_DAMAGE	dt->marker_type_damage
#define	MARKER_BUNDLE_INDEX	dt->markerbundleindex

#define	MARKER_SCALE	0.01 	/* multiplied by vdc extent when marker size
				 * specification mode is scaled
				 */
#define	MARKER_SIZE	(dt->markersizemode == MODE_ABSOLUTE ? (long)dt->markersize : (long) (dt->xmax - dt->xmin)  * MARKER_SCALE * dt->markersize)
#define	MARKER_SIZE_DAMAGE	dt->marker_size_damage

/*
 * 	Text default stuff
 */
#define CHAR_SPACE	dt->char_space
#define CHAR_HEIGHT	dt->char_height
#define	CHAR_EXPAN	dt->char_expan

/*character orientation, vectors for x and y direction	*/
#define	CHAR_X_UP	dt->char_x_up	
#define	CHAR_Y_UP	dt->char_y_up	
#define	CHAR_X_BASE	dt->char_x_base	
#define	CHAR_Y_BASE	dt->char_y_base	

#define TEXT_PATH	dt->text_path	

#define	TEXT_ATT_DAMAGE	dt->text_att_damage
 
/*text alignment*/
#define TEXT_ALI_H	dt->text_ali_h
#define TEXT_ALI_V	dt->text_ali_v
#define TEXT_ALI_C_H	dt->text_ali_c_h
#define TEXT_ALI_C_V	dt->text_ali_c_v

#define LINE_COLOUR	 	dt->line_colour

#define LINE_COLOUR_DAMAGE	dt->line_colour_damage

#define TEXT_COLOUR	 	dt->text_colour

#define FILL_COLOUR		dt->fill_colour

#define FILL_COLOUR_DAMAGE	dt->fill_colour_damage
#define MARKER_COLOUR	 	dt->marker_colour
#define MARKER_COLOUR_DAMAGE 	dt->marker_colour_damage

#define LINE_TYPE		dt->line_type
#define LINE_TYPE_DAMAGE	dt->line_type_damage

#define LINE_WIDTH		dt->line_width
#define LINE_WIDTH_MODE		dt->line_width_mode
#define LINE_WIDTH_DAMAGE	dt->line_width_damage

#define	COLOUR_TABLE_DAMAGE	clut->damage
#define	COLOUR_TOTAL_DAMAGE	clut->total_damage

#define	COLOUR_INDEX_DAMAGE(I)	(clut->ce[(I)].damage)
#define	COLOUR_INDEX_RED(I)	(clut->ce[(I)].rgb.red)
#define	COLOUR_INDEX_GREEN(I)	(clut->ce[(I)].rgb.green)
#define	COLOUR_INDEX_BLUE(I)	(clut->ce[(I)].rgb.blue)
