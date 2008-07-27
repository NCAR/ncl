/*
 *	$Id: default.c,v 1.30 2008-07-27 03:18:43 haley Exp $
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
 *	Author(s):	Tinsley Galyean (tag@boulder.colorado.edu)
 *			John Clyne
 *
 *	Date:	Thu Mar 10 15:15:44 MST 1988
 *
 */
#define	INDEXED	0
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ncarg/c.h>
#include "cgmc.h"
#define	 DEFAULT
#include "default.h"
#include "ctrandef.h"

static	struct	{
	float	min_line_width;
	boolean	min_line_width_set;
	float	max_line_width;
	boolean	max_line_width_set;
	float	line_width_scale;
	boolean	line_width_scale_set;
	float	rgb_scale;
	} oPtion = {1.0, FALSE, 1.0, FALSE, 1.0, FALSE, 1.0};

static	ColorElement defaultCmap[] = {
	{{  0,   0,   0}, TRUE, TRUE},	/* background color	*/
	{{255, 255, 255}, TRUE, TRUE},	/* foreground color	*/
	{{255,   0,   0}, TRUE, TRUE},	/* red			*/
	{{  0, 255,   0}, TRUE, TRUE},	/* green 		*/
	{{  0,   0, 255}, TRUE, TRUE},	/* blue 		*/
	{{255, 255,   0}, TRUE, TRUE},	/* 			*/
	{{  0, 255, 255}, TRUE, TRUE},	/* secondaries		*/
	{{255,   0, 255}, TRUE, TRUE},	/* 			*/
	}; 

static	int	defaultCmapSize = sizeof (defaultCmap) / sizeof (ColorElement);



static DEFAULTTABLE	picdefaulttable;
DEFAULTTABLE		*dt = &defaulttable;

/*
 *	colorLUTable contains the color lookup table which is defined
 *	for the duration of the metafile
 */
static	ColorLUTable		colorLUTable;

/*
 *	piccolorLUTable only lives for the duration of a single 
 *	metafile frame.	it is analogous to picdefaulttable.
 */
static	ColorLUTable		picColorLUTable;

/*
 * An array indicating wheather a particular color index was changed
 * via ColrTable()
 */
static	ColorElementAccess		*CEA;

/*
 * hook to the outside world
 */
ColorLUTable			*clut = &colorLUTable;



InitDefault()
{
	static	boolean	isInit = FALSE;
	int	max_intensity = (1 << DCP) - 1;
	int	tmp;
	int	i;

	/*
	 * do a one time memory allocation
	 */
	if (! isInit) {;

		colorLUTable.ce = (ColorElement *) malloc (
			(unsigned) (sizeof(ColorElement) * (MAX_C_I + 1))
		);
		picColorLUTable.ce = (ColorElement *) malloc (
			(unsigned) (sizeof(ColorElement) * (MAX_C_I + 1))
		);
		CEA = (ColorElementAccess *) malloc (
			(unsigned) (sizeof(ColorElementAccess) * (MAX_C_I + 1))
		);

		colorLUTable.size = MAX_C_I + 1;
		picColorLUTable.size = MAX_C_I + 1;
	}
	
	/*
	 * copy in the initial color map which has a few entries defined
	 */
	for (i=0; i<defaultCmapSize; i++) {
		colorLUTable.ce[i].damage = TRUE;
		colorLUTable.ce[i].defined = TRUE;

		tmp = defaultCmap[i].rgb.red * oPtion.rgb_scale;
		tmp = tmp > max_intensity ? max_intensity : tmp;
		colorLUTable.ce[i].rgb.red = (unsigned char) tmp;

		tmp = defaultCmap[i].rgb.green * oPtion.rgb_scale;
		tmp = tmp > max_intensity ? max_intensity : tmp;
		colorLUTable.ce[i].rgb.green = (unsigned char) tmp;

		tmp = defaultCmap[i].rgb.blue * oPtion.rgb_scale;
		tmp = tmp > max_intensity ? max_intensity : tmp;
		colorLUTable.ce[i].rgb.blue = (unsigned char) tmp;

		CEA[i] = TRUE;
	}
	for (; i<=MAX_C_I; i++) {
		colorLUTable.ce[i].damage = FALSE;
		colorLUTable.ce[i].defined = FALSE;
		CEA[i] = FALSE;
	}
	colorLUTable.total_damage = defaultCmapSize;
	colorLUTable.damage = TRUE;

	isInit = TRUE;
}

/*
 *	Call this routine if after InitDefault has been called if no
 *	default ctrans-supplied color map is desired
 */
void	_CtDefNoColorDefault()
{
	int	i;

	for (i=0; i<defaultCmapSize; i++) {
		colorLUTable.ce[i].damage = FALSE;
		colorLUTable.ce[i].defined = FALSE;
		CEA[i] = FALSE;
	}
	colorLUTable.damage = FALSE;
}

SetInPic(value)
boolean		value;
{
	int	i;

	if (value) {

#ifdef	RS6000
		/*
		 * AIX - hosed as usual
		 */
		memmove((void *) &picdefaulttable,(const void *) &defaulttable, 
			sizeof (DEFAULTTABLE));
#else
		picdefaulttable = defaulttable;
#endif

		dt = &picdefaulttable;

		/*
		 * load globally set color table into current color
		 * table.
		 */
		picColorLUTable.total_damage = 0;
		for(i=0; i < colorLUTable.size; i++) {
			picColorLUTable.ce[i].rgb = colorLUTable.ce[i].rgb;

			picColorLUTable.ce[i].defined = 
						colorLUTable.ce[i].defined;

			/*
			 * set damage based on access
			 */
			picColorLUTable.ce[i].damage = CEA[i];
			CEA[i] = FALSE;

			if (picColorLUTable.ce[i].damage) {
				picColorLUTable.total_damage++;
			}

		}
		if(picColorLUTable.total_damage)
			picColorLUTable.damage = TRUE;
		clut = &picColorLUTable;


	} else {
		dt = &defaulttable;
		clut = &colorLUTable;
	}
}

/* Functions called from the jumptable.  That change defaults */
/* Class 1, 2, 3, and 5  functions from CGM */

/* Class 1 */
/*ARGSUSED*/
int MFVersion(c)
CGMC *c;
{

	dt->mfversion = c->i[0];

	return(0);
}

/*ARGSUSED*/
int MFDesc(c)
CGMC *c;
{
	return (0);
}

/*ARGSUSED*/
int VDCType(c)
CGMC *c;
{
	dt->vdc_type = c->e[0];

	if (dt->vdc_type == VDC_IS_INT) {
		return (0);
	}

	if (dt->vdc_type == VDC_IS_REAL) {
		ESprintf(EINVAL, "Unsupported vdc type (%d)",dt->vdc_type);
		return(-1);
	}

	ESprintf(EINVAL, "Illegal vdc type (%d)",dt->vdc_type);
	return(-1);
}

/*ARGSUSED*/
int IntergerPrec(c)
	CGMC *c;
{
	dt->ip = c->i[0];

	if (dt->ip == 8 || dt->ip == 16 || dt->ip == 24 || dt->ip == 32) {
		return (0);
	}

	ESprintf(EINVAL, "Illegal integer precision(%d)",dt->ip);
	return(-1);
}

/*ARGSUSED*/
int RealPrec(c)
	CGMC *c;
{
	dt->real_mode = c->i[0];
	dt->real_exp = c->i[1];
	dt->real_man = c->i[2];

	if (dt->real_mode == REAL_MODE_FIXED) {
		if ((dt->real_exp == 16 && dt->real_man == 16) || 
			(dt->real_exp == 32 && dt->real_man == 32)) {

			return(0);
		}
	}

	/*
	 * floating point reals not supported yet
	 */
	if (dt->real_mode == REAL_MODE_FLOAT) {
		if ((dt->real_exp == 9 && dt->real_man == 23) || 
			(dt->real_exp == 12 && dt->real_man == 52)) {

			ESprintf(EINVAL, 
				"Unsupported real precision(mode=%d,exp=%d,man=%d)",
				dt->real_mode, dt->real_exp, dt->real_man
			);
			return(-1);
		}
	}

	ESprintf(EINVAL, 
		"Illegal real precision(mode=%d,exp=%d,man=%d)",
		dt->real_mode, dt->real_exp, dt->real_man
	);
	return(-1);
}

/*ARGSUSED*/
int IndexPrec(c)
	CGMC *c;
{
	dt->ixp = c->i[0];
	if (dt->ixp == 8 || dt->ixp == 16 || dt->ixp == 24 || dt->ixp == 32) {
		return (0);
	}

	ESprintf(EINVAL,"Illegal index precision(%d)",dt->ixp);
	return(-1);
}

/*ARGSUSED*/
int ColrPrec(c)
CGMC *c;
{
	dt->dcp = c->i[0];
	if (dt->dcp == 8) {
		return (0);
	}
	if (dt->dcp == 16 || dt->dcp == 24 || dt->dcp == 32) {
		ESprintf(EINVAL, "Unsupported color precision(%d)",dt->dcp);
		return(-1);
	}

	ESprintf(EINVAL, "Illegal color precision(%d)",dt->dcp);
	return (-1);
}

/*ARGSUSED*/
int ColrIndexPrec(c)
	CGMC *c;
{
	dt->cip = c->i[0];

	if (dt->cip == 8) {
		return (0);
	}
	if (dt->cip == 16 || dt->cip == 24 || dt->cip == 32) {
		ESprintf(EINVAL, "Unsupported color index precision(%d)",dt->cip);
		return(-1);
	}

	ESprintf(EINVAL, "Illegal color index precision(%d)",dt->cip);
	return (-1);
}

/*ARGSUSED*/
int MaxColrIndex(c)
	CGMC *c;
{
	dt->max_c_i = c->ci[0];
	return (0);
}

/*ARGSUSED*/
int ColrValueExt(c)
CGMC *c;
{
	dt->c_v_e_min.red = c->cd[0].red;
	dt->c_v_e_min.green = c->cd[0].green;
	dt->c_v_e_min.blue = c->cd[0].blue;

	dt->c_v_e_max.red = c->cd[1].red;
	dt->c_v_e_max.green = c->cd[1].green;
	dt->c_v_e_max.blue = c->cd[1].blue;

	if (dt->c_v_e_min.red != 0 
		|| dt->c_v_e_min.green !=0 
		|| dt->c_v_e_min.blue != 0 
		|| dt->c_v_e_max.red != 255 
		|| dt->c_v_e_max.green != 255 
		|| dt->c_v_e_max.blue != 255) {

		ESprintf(ENOSYS, "Unsupported CGM element");
		return (-1);
	}
	return(0);
}

/*ARGSUSED*/
int MFElemList(c)
CGMC *c;
{
	return (0);
}

/*ARGSUSED*/
int MFDefaults(c)
CGMC *c;
{
	return (0);
}


/*ARGSUSED*/
int CharSetList(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int CharCoding(c)
CGMC *c;
{
	dt->char_c_a = c->e[0];

	if (dt->char_c_a != BASIC_7_BIT) {
		ESprintf(
			EINVAL, 
			"Unsupported character encoding(%d)",dt->char_c_a
		);
		return(-1);
	}
	return (0);
}

/* Class 2 */
/*ARGSUSED*/
int ScaleMode(c)
CGMC *c;
{
	dt->scalemodee = c->e[0];
	dt->scalemoder = c->r[0];

	if (dt->scalemodee == MODE_ABSTRACT) {
		return (0);
	}

	if (dt->scalemodee == MODE_METRIC) {
		ESprintf(EINVAL, "Unsupported scale mode(%d)",dt->scalemodee);
		return(-1);
	}

	ESprintf(EINVAL, "Illegal scale mode(%d)",dt->scalemodee);
	return (-1);
}

/*ARGSUSED*/
int ColrMode(c)
CGMC *c;
{
	dt->csm = c->e[0];

	if (dt->csm == MODE_INDEXED) {
		return (0);
	}

	if (dt->csm == MODE_DIRECT) {
		ESprintf(EINVAL, "Unsupported color mode(%d)",dt->csm);
		return(-1);
	}

	ESprintf(EINVAL, "Illegal color mode(%d)",dt->csm);
	return (-1);
}

/*ARGSUSED*/
int LineWidthMode(c)
CGMC *c;
{

	dt->line_width_mode = c->e[0];

	if (dt->line_width_mode == MODE_SCALED) {
		return (0);
	}

	if (dt->line_width_mode == MODE_ABSOLUTE) {
		ESprintf(
			EINVAL, "Unsupported line width mode(%d)",
			dt->line_width_mode
		);
		return(-1);
	}

	ESprintf(EINVAL, "Illegal line width mode(%d)",dt->line_width_mode);
	return (-1);
}

/*ARGSUSED*/
int MarkerSizeMode(c)
CGMC *c;
{
	dt->mar_siz_mod = c->e[0];
	if (dt->mar_siz_mod == MODE_SCALED) {
		return (0);
	}

	if (dt->mar_siz_mod == MODE_ABSOLUTE) {
		ESprintf(
			EINVAL, "Unsupported marker size mode(%d)",
			dt->mar_siz_mod
		);
		return(-1);
	}

	ESprintf(EINVAL, "Illegal marker size mode(%d)",dt->mar_siz_mod);
	return (-1);
}

/*ARGSUSED*/
int EdgeWidthMode(c)
CGMC *c;
{
	dt->edg_wid_mod = c->e[0];

	if (dt->edg_wid_mod == MODE_SCALED) {
		return (0);
	}

	if (dt->edg_wid_mod == MODE_ABSOLUTE) {
		ESprintf(
			EINVAL, "Unsupported edge width mode(%d)",
			dt->edg_wid_mod
		);
		return(-1);
	}

	ESprintf(EINVAL, "Illegal edge width mode(%d)",dt->edg_wid_mod);
	return (-1);
}

int VDCExt(c)
CGMC *c;
{
	XMIN = c->p[0].x;
	YMIN = c->p[0].y;
	XMAX = c->p[1].x;
	YMAX = c->p[1].y;

	dt->char_height = (VDCtype) 0.01*(YMAX - YMIN);

	dt->vdc_extent_damage = TRUE;

	return (0);
}

/*ARGSUSED*/
int BackColr(c)
CGMC *c;
{

	dt->backcolr.red = c->cd[0].red;
	dt->backcolr.green = c->cd[0].green;
	dt->backcolr.blue = c->cd[0].blue;

	dt->backcolr_damage = TRUE;

	clut->ce[0].rgb.red = (unsigned char) c->cd[0].red;
	clut->ce[0].rgb.green = (unsigned char) c->cd[0].green;
	clut->ce[0].rgb.blue = (unsigned char) c->cd[0].blue;
	clut->ce[0].defined = TRUE;

	if (! clut->ce[0].damage) {
		clut->total_damage++;
		clut->ce[0].damage = TRUE;
	}

	clut->damage = TRUE;

	return (0);
}



			/* Class 3 */



/*ARGSUSED*/
int VDCIntergerPrec(c)
CGMC *c;
{
	dt->vdc_int = c->i[0];
	if (dt->vdc_int == 8 || dt->vdc_int == 16 || 
		dt->vdc_int == 24 || dt->vdc_int == 32) {

		return (0);
	}
	ESprintf(EINVAL, "Illegal integer precision(%d)",dt->vdc_int);
	return(-1);
}

/*ARGSUSED*/
int VDCRealPrec(c)
CGMC *c;
{
	dt->vdc_real_mode = c->i[0];
	dt->vdc_real_exp = c->i[0];
	dt->vdc_real_man = c->i[0];
	if (dt->vdc_real_mode == 1) {
		if ((dt->vdc_real_exp == 16 && dt->vdc_real_man == 16) || 
			(dt->vdc_real_exp == 32 && dt->vdc_real_man == 32)) {

			return(0);
		}
	}

	/*
	 * floating point reals not supported yet
	 */
	else if (dt->vdc_real_mode == 0) {
		if ((dt->vdc_real_exp == 9 && dt->vdc_real_man == 23) || 
			(dt->vdc_real_exp == 12 && dt->vdc_real_man == 52)) {

			ESprintf(EINVAL, 
				"Unsupported real precision(mode=%d,exp=%d,man=%d)",
				dt->vdc_real_mode, dt->vdc_real_exp, 
				dt->vdc_real_man
			);
			return(-1);
		}
	}

	ESprintf(EINVAL, 
		"Illegal real precision(mode=%d,exp=%d,man=%d)",
		dt->vdc_real_mode, dt->vdc_real_exp, dt->vdc_real_man
	);
	return(-1);
}

/*ARGSUSED*/
int AuxColr(c)
	CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int Transparency(c)
	CGMC *c;
{
	dt->trans = c->e[0];

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}
int ClipRect(c)
	CGMC *c;
{
	CLIPXMIN = c->p[0].x;
	CLIPYMIN = c->p[0].y;
	CLIPXMAX = c->p[1].x;
	CLIPYMAX = c->p[1].y;

	dt->clip_damage = TRUE;
	return (0);
}
int Clip(c)
	CGMC *c;
{

	CLIPFLAG = c->e[0];
	dt->clip_damage = TRUE;

	return (0);
}

/* Class 5 */

/*ARGSUSED*/
int LineIndex(c)
	CGMC *c;
{
	dt->line_ind = c->ix[0];
	return (0);
}

/*ARGSUSED*/
int LineType(c)
CGMC *c;
{
	int	status = 0;

	dt->line_type = c->ix[0];

	dt->line_type_damage = TRUE;

	switch (dt->line_type) {
	case L_SOLID :
	case L_DASH :
	case L_DOT :
	case L_DASH_DOT :
	case L_DASH_DOT_DOT :
		status = 0;
		break;

	default:
		ESprintf(EINVAL, "Illegal line type(%d)",dt->line_type);
		status = -1;
		break;
	}
	return (status);
}

/*ARGSUSED*/
int LineWidth(c)
CGMC *c;
{
	float	line_width = c->r[0];

	if (oPtion.line_width_scale_set) line_width *= oPtion.line_width_scale;

	/*
	 * make sure line width doesn't exceed bounds
	 */
	if (oPtion.min_line_width_set) 
		line_width = MAX(line_width, oPtion.min_line_width);

	if (oPtion.max_line_width_set) 
		line_width = MIN(line_width, oPtion.max_line_width);

	/*
	 * record the line width
	 */
	dt->line_width = line_width;

	dt->line_width_damage = TRUE;
	return (0);
}

/*ARGSUSED*/
int LineColr(c)
CGMC *c;
{

	if (CSM == INDEXED)  {
		if (clut->ce[c->ci[0]].defined) {
			dt->line_colour.index = c->ci[0];
		}
	}
	else {
		dt->line_colour.direct.red = c->cd[0].red;
		dt->line_colour.direct.green = c->cd[0].green;
		dt->line_colour.direct.blue = c->cd[0].blue;
	}

	dt->line_colour_damage = TRUE;
	return (0);
}

/*ARGSUSED*/
int MarkerIndex(c)
CGMC *c;
{ 
	dt->marker_ind = c->ix[0];
	return (0);
}

int MarkerType(c)
CGMC *c;
{
	int	status = 0;

	dt->markertype = c->ix[0];
	dt->marker_type_damage = TRUE;

	switch (dt->markertype) {
	case MARKER_X :
	case MARKER_CIRCLE :
	case MARKER_STAR :
	case MARKER_DOT :
	case MARKER_PLUS :
		status = 0;
		break;

	default:
		ESprintf(EINVAL, "Illegal marker type(%d)",dt->markertype);
		status = -1;
		break;
	}

	return (status);
}

/*ARGSUSED*/
int MarkerSize(c)
CGMC *c;
{

	if (MAR_SIZ_MOD == MODE_SCALED) 
		dt->markersize = c->r[0];
	else
		dt->markersize = c->vdc[0];

	dt->marker_size_damage = TRUE;
	return (0);
}

/*ARGSUSED*/
int MarkerColr(c)
CGMC *c;
{
	if (CSM == INDEXED)  {
		if (clut->ce[c->ci[0]].defined) {
			dt->marker_colour.index = c->ci[0];
		}
	}
	else {
		dt->marker_colour.direct.red = c->cd[0].red;
		dt->marker_colour.direct.green = c->cd[0].green;
		dt->marker_colour.direct.blue = c->cd[0].blue;
	}

	dt->marker_colour_damage = TRUE; 
	return (0);
} 

/*ARGSUSED*/
int TextIndex(c)
CGMC *c;
{
	dt->text_ind = c->ix[0];
	return (0);
}


/*ARGSUSED*/
int TextFontIndex(c)
CGMC *c;
{
	dt->text_f_ind = c->ix[0];
	dt->text_f_ind_damage = TRUE;
	dt->text_att_damage = TRUE;

	return(0);
}

/*ARGSUSED*/
int TextPrec(c)
CGMC *c;
{
	int	status = 0;

	dt->text_prec = c->e[0];

	switch (dt->text_prec) {
	case PREC_STRING :
	case PREC_CHAR :
	case PREC_STROKE :
		status = 0;
		break;

	default:
		ESprintf(EINVAL, "Illegal text precision(%d)",dt->text_prec);
		status = -1;
		break;
	}
	return (status);
}

int CharExpan(c)
CGMC *c;
{

	dt->text_att_damage = TRUE;
	dt->char_expan = c->r[0];
	return (0);
}

int CharSpace(c)
CGMC *c;
{

	dt->text_att_damage = TRUE;
	dt->char_space = c->r[0];
	return (0);
}

/*ARGSUSED*/
int TextColr(c)
CGMC *c;
{
	if (CSM == INDEXED)  {
		if (clut->ce[c->ci[0]].defined) {
			dt->text_colour.index = c->ci[0];
		}
	}
	else {
		dt->text_colour.direct.red = c->cd[0].red;
		dt->text_colour.direct.green = c->cd[0].green;
		dt->text_colour.direct.blue = c->cd[0].blue;
	}
	return (0);
}
int CharHeight(c)
CGMC *c;
{

	dt->text_att_damage = TRUE;
	dt->char_height = c->vdc[0];
	return (0);
}
int CharOri(c)
CGMC *c;
{

	dt->text_att_damage = TRUE;
	dt->char_x_up = c->vdc[0];
	dt->char_y_up = c->vdc[1];
	dt->char_x_base = c->vdc[2];
	dt->char_y_base = c->vdc[3];
	return (0);
}
int TextPath(c)
CGMC *c;
{
	int	status = 0;

	dt->text_att_damage = TRUE;
	dt->text_path = c->e[0];

	switch (dt->text_path) {
	case PATH_RIGHT :
	case PATH_LEFT :
	case PATH_UP :
	case PATH_DOWN:
		status = 0;
		break;

	default:
		ESprintf(EINVAL, "Illegal text path(%d)",dt->text_path);
		status = -1;
		break;
	}
	return (status);
}

/*ARGSUSED*/
int TextAlign(c)
CGMC *c;
{
	int	status = 0;

	dt->text_att_damage = TRUE;
	dt->text_ali_h = c->e[0];
	dt->text_ali_v = c->e[1];
	dt->text_ali_c_h = c->r[0];
	dt->text_ali_c_h = c->r[1];

	switch (dt->text_ali_h) {
	case A_NORM_H :
	case A_LEFT :
	case A_CENTER :
	case A_RIGHT:
	case A_CO_HOR:
		status = 0;
		break;

	default:
		ESprintf(
			EINVAL, "Illegal horizontal text alignment(%d)",
			dt->text_ali_h
		);
		status = -1;
		break;
	}

	switch (dt->text_ali_v) {
	case A_NORM_V :
	case A_TOP :
	case A_CAP :
	case A_HALF:
	case A_BASE:
	case A_BOTTOM:
	case A_CO_VER:
		status = 0;
		break;

	default:
		ESprintf(
			EINVAL, "Illegal vertical text alignment(%d)",
			dt->text_ali_v
		);
		status = -1;
		break;
	}
	return (status);
}

/*ARGSUSED*/
int CharSetIndex(c)
CGMC *c;
{
	dt->char_ind = c->ix[0];
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int AltCharSetIndex(c)
CGMC *c;
{
	dt->alt_char_ind = c->ix[0];
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int FillIndex(c)
CGMC *c;
{
	dt->fill_ind = c->ix[0];

/*
 *	ifdef out this warning message because this element appears
 *	in so many of pre-ncarg3.2 NCGMs even though we have never
 *	supported it.
 */
#ifdef	DEAD
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
#else
	return(0);
#endif
}

/*ARGSUSED*/
int IntStyle(c)
CGMC *c;
{
	int	status = 0;

	dt->int_style = c->e[0];
	dt->int_style_damage = TRUE;

	switch (dt->int_style) {
	case HOLLOW_S :
	case SOLID_S :
	case HATCH_S :
	case EMPTY_S :
		status = 0;
		break;

	case PATTERN_S :
		ESprintf(EINVAL, "Unsupported fill style(%d)",dt->int_style);
		status = -1;
		break;
	default:
		ESprintf(EINVAL, "Illegal fill style(%d)",dt->int_style);
		status = -1;
		break;
	}

	return (status);
}

/*ARGSUSED*/
int FillColr(c)
CGMC *c;
{
	if (CSM == INDEXED)  {
		if (clut->ce[c->ci[0]].defined) {
			dt->fill_colour.index = c->ci[0];
		}
	}
	else {
		dt->fill_colour.direct.red = c->cd[0].red;
		dt->fill_colour.direct.green = c->cd[0].green;
		dt->fill_colour.direct.blue = c->cd[0].blue;
	}
	dt->fill_colour_damage = TRUE;
	return (0);
}

/*ARGSUSED*/
int HatchIndex(c)
CGMC *c;
{
	int	status = 0;

	dt->hatch_ind = c->ix[0];

	switch (dt->hatch_ind) {
	case HORIZONTAL :
	case VERTICAL :
	case POSITIVE :
	case NEGATIVE :
	case HORIZ_VERT :
	case POS_NEG :
		status = 0;
		break;

	default:
		ESprintf(EINVAL, "Illegal hatch index(%d)",dt->hatch_ind);
		status = -1;
		break;
	}

	return (status);
}

/*ARGSUSED*/
int PatIndex(c)
CGMC *c;
{
	dt->pattern_ind = c->ix[0];

/*
 *	ifdef out this warning message because this element appears
 *	in so many of pre-ncarg3.2 NCGMs even though we have never
 *	supported it.
 */
#ifdef	DEAD
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
#else
	return(0);
#endif
}

/*ARGSUSED*/
int EdgeIndex(c)
CGMC *c;
{
	dt->edge_ind = c->ix[0];

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int EdgeType(c)
CGMC *c;
{
	dt->edge_type = c->ix[0];
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int EdgeWidth(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int EdgeColr(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int EdgeVis(c)
CGMC *c;
{
	dt->edge_vis = c->e[0];
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int FillRefPt(c)
CGMC *c;
{
	dt->fill_r_pt.x = c->p[0].x;
	dt->fill_r_pt.y = c->p[0].y;

/*
 *	ifdef out this warning message because this element appears
 *	in so many of pre-ncarg3.2 NCGMs even though we have never
 *	supported it.
 */
#ifdef	DEAD
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
#else
	return(0);
#endif
}

/*ARGSUSED*/
int PatTable(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
int PatSize(c)
CGMC *c;
{

/*
 *	ifdef out this warning message because this element appears
 *	in so many of pre-ncarg3.2 NCGMs even though we have never
 *	supported it.
 */
#ifdef	DEAD
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
#else
	return(0);
#endif
}

/*ARGSUSED*/
int ColrTable(c)
CGMC *c;
{
	int	color_index;
	int	tmp;
	int	i;
	int	max_intensity = (1 << DCP) - 1;

	color_index = c->ci[0];
	if (color_index == 0) {
		dt->backcolr_damage = TRUE;
	}

	for (i=0; i <c->CDnum && i<=MAX_C_I; i++,color_index++){
		tmp = c->cd[i].red * oPtion.rgb_scale;
		tmp = tmp > max_intensity ? max_intensity : tmp;
		clut->ce[color_index].rgb.red = (unsigned char) tmp;

		tmp = c->cd[i].green * oPtion.rgb_scale;
		tmp = tmp > max_intensity ? max_intensity : tmp;
		clut->ce[color_index].rgb.green = (unsigned char) tmp;

		tmp = c->cd[i].blue * oPtion.rgb_scale;
		tmp = tmp > max_intensity ? max_intensity : tmp;
		clut->ce[color_index].rgb.blue = (unsigned char) tmp;

		clut->ce[color_index].defined = TRUE;
		if (! clut->ce[color_index].damage) {
			clut->total_damage++;
			clut->ce[color_index].damage = TRUE;
		}

		/*
		 * this element has been changed
		 */
		CEA[color_index] = TRUE;
	}

	clut->damage = TRUE;

	return (0);
}

/*ARGSUSED*/
int ASF(c)
CGMC *c;
{
	int	i;

	for (i=0; i<(c->Enum-1); i+=2) {
		if (c->e[i+1] == 1) {
			ESprintf(ENOSYS, "Unsupported CGM element");
			return (-1);
		}
	}

	return(0);
}

/*
 *	some utility functions
 */

/*
 *	set mininum line width
 */
SetMinLineWidthDefault(line_width)
	float	line_width;
{

	/*
	 * Make sure the metafile global default is in range
	 */
	defaulttable.line_width = MAX(line_width, defaulttable.line_width);

	/*
	 * signal the change made to the default
	 */
	LINE_WIDTH_DAMAGE = TRUE;

	/*
	 * record the new minimum for future use
	 */
	oPtion.min_line_width = line_width;
	oPtion.min_line_width_set = TRUE;;
	
}

/*
 *	Set Maximum line width.
 */
SetMaxLineWidthDefault(line_width)
	float	line_width;
{
	defaulttable.line_width = MIN(line_width, defaulttable.line_width);

	LINE_WIDTH_DAMAGE = TRUE;

	oPtion.max_line_width = line_width;
	oPtion.max_line_width_set = TRUE;;
	
}

/*
 *	set additional line scaling
 */
SetAdditionalLineScale(line_scale)
	float	line_scale;
{
	LINE_WIDTH_DAMAGE = TRUE;

	defaulttable.line_width = line_scale;

	oPtion.line_width_scale = line_scale;
	oPtion.line_width_scale_set = TRUE;;
}

/*
 *	set rgb intensity scaling
 */
SetRGBIntensityScale(rgb_scale)
	float	rgb_scale;
{
	COLOUR_TABLE_DAMAGE = TRUE;

	oPtion.rgb_scale = rgb_scale;
}
