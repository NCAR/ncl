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
#include "cgmc.h"
#define	 DEFAULT
#include "default.h"
#include	<cterror.h>

static	struct	{
	float	min_line_width;
	boolean	min_line_width_set;
	float	max_line_width;
	boolean	max_line_width_set;
	float	line_width_scale;
	boolean	line_width_scale_set;
	} Option = {1.0, FALSE, 1.0, FALSE, 1.0, FALSE};

SetInPic(value)
boolean		value;
{
	if (value) {

#ifdef	RS6000
		bcopy((char *) &defaulttable, (char *) &picdefaulttable, 
			sizeof (DEFAULTTABLE));
#else
		picdefaulttable = defaulttable;
#endif

		dt = &picdefaulttable;

		/*	
		 *	mark accessed attributes as damaged by last frame
		 *	so they will be updated for the current frame
		 */
		LINE_TYPE_DAMAGE = LINE_TYPE_ACCESS;
		LINE_WIDTH_DAMAGE = LINE_WIDTH_ACCESS;
		LINE_COLOUR_DAMAGE = LINE_COLOUR_ACCESS;
		MARKER_COLOUR_DAMAGE = MARKER_COLOUR_ACCESS;
		FILL_COLOUR_DAMAGE = FILL_COLOUR_ACCESS;
		INT_STYLE_DAMAGE = INT_STYLE_ACCESS;
		CLIP_DAMAGE = CLIP_ACCESS;
		MARKER_TYPE_DAMAGE = MARKER_TYPE_ACCESS;
		MARKER_SIZE_DAMAGE = MARKER_SIZE_ACCESS;
		BACKCOLR_DAMAGE = BACKCOLR_ACCESS;
		TEXT_F_IND_DAMAGE = TEXT_F_IND_ACCESS;

		/*	clear access list for new frame	*/
		LINE_TYPE_ACCESS	= 
		LINE_WIDTH_ACCESS	=  
		LINE_COLOUR_ACCESS	=
		MARKER_COLOUR_ACCESS	= 
		FILL_COLOUR_ACCESS	= 
		INT_STYLE_DAMAGE	=
		CLIP_ACCESS		= 
		MARKER_TYPE_ACCESS	=
		MARKER_SIZE_ACCESS	= 
		BACKCOLR_ACCESS		= 
		TEXT_F_IND_ACCESS	= FALSE;

	} else 
		dt = &defaulttable;
}

/* Functions called from the jumptable.  That change defaults */
/* Class 1, 2, 3, and 5  functions from CGM */

/* Class 1 */
/*ARGSUSED*/
Ct_err MFVersion(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"MFVersion\n");
#endif

	dt->mfversion = c->i[0];

	return(OK);
}

/*ARGSUSED*/
Ct_err MFDesc(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"MFDesc\n");
#endif
	return (OK);
}

/*ARGSUSED*/
Ct_err VDCType(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"VDCType\n");
#endif
	dt->vdc_type = c->e[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err IntergerPrec(c)
CGMC *c;
{
#ifdef DEBUG 
	(void)fprintf(stderr,"IntegerPrec\n");
#endif
	dt->ip = c->i[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err RealPrec(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"RealPrec\n");
#endif
	dt->real_mode = c->i[0];
	dt->real_exp = c->i[0];
	dt->real_man = c->i[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err IndexPrec(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"IndexPrec\n");
#endif
	dt->ixp = c->i[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err ColrPrec(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"ColrPrec\n");
#endif
	dt->dcp = c->i[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err ColrIndexPrec(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"ColrIndexPrec\n");
#endif
	dt->cip = c->i[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err MaxColrIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"MaxColrIndex\n");
#endif
	dt->max_c_i = c->ci[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err ColrValueExt(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"ColrValueExt\n");
#endif
	dt->c_v_e_min.red = c->cd[0].red;
	dt->c_v_e_min.green = c->cd[0].green;
	dt->c_v_e_min.blue = c->cd[0].blue;

	dt->c_v_e_max.red = c->cd[1].red;
	dt->c_v_e_max.green = c->cd[1].green;
	dt->c_v_e_max.blue = c->cd[1].blue;
	return (OK);
}

/*ARGSUSED*/
Ct_err MFElemList(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"MFElemList\n");
#endif
	return (OK);
}

/*ARGSUSED*/
Ct_err MFDefaults(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"MFDefaults\n");
#endif
	return (OK);
}


/*ARGSUSED*/
Ct_err CharSetList(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"CharSetList\n");
#endif
	return (OK);
}

/*ARGSUSED*/
Ct_err CharCoding(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"CharCoding\n");
#endif
	dt->char_c_a = c->e[0];
	return (OK);
}

/* Class 2 */
/*ARGSUSED*/
Ct_err ScaleMode(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"ScaleMode\n");
#endif
	dt->scalemodee = c->e[0];
	dt->scalemoder = c->r[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err ColrMode(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"ColrMode\n");
#endif
	dt->csm = c->e[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err LineWidthMode(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"LineWidthMode\n");
#endif

	dt->line_width_mode = c->e[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err MarkerSizeMode(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"MarkerSizeMode\n");
#endif
	dt->mar_siz_mod = c->e[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err EdgeWidthMode(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"EdgeWidthMode\n");
#endif
	dt->edg_wid_mod = c->e[0];
	return (OK);
}

Ct_err VDCExt(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"VDCExt\n");
#endif
	XMIN = c->p[0].x;
	YMIN = c->p[0].y;
	XMAX = c->p[1].x;
	YMAX = c->p[1].y;

	dt->char_height = (VDCtype) 0.01*(YMAX - YMIN);
	return (OK);
}

/*ARGSUSED*/
Ct_err BackColr(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"BackColr\n");
#endif

	dt->backcolr.red = c->cd[0].red;
	dt->backcolr.green = c->cd[0].green;
	dt->backcolr.blue = c->cd[0].blue;

	dt->backcolr_damage = at->backcolr_access = TRUE;

	return (OK);
}



			/* Class 3 */



/*ARGSUSED*/
Ct_err VDCIntergerPrec(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"VDCIntergerPrec\n");
#endif
	dt->vdc_int = c->i[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err VDCRealPrec(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"VDCRealPrec\n");
#endif
	dt->vdc_real_mode = c->i[0];
	dt->vdc_real_exp = c->i[0];
	dt->vdc_real_man = c->i[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err AuxColr(c)
CGMC *c;


{
#ifdef DEBUG
	(void)fprintf(stderr,"AuxColr\n");
#endif
	return (OK);
}

/*ARGSUSED*/
Ct_err Transparency(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"Transparency\n");
#endif
	dt->trans = c->e[0];
	return (OK);
}
Ct_err ClipRect(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"ClipRect\n");
#endif
	CLIPXMIN = c->p[0].x;
	CLIPYMIN = c->p[0].y;
	CLIPXMAX = c->p[1].x;
	CLIPYMAX = c->p[1].y;

	dt->clip_damage = at->clip_access = TRUE;
	return (OK);
}
Ct_err Clip(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"Clip\n");
#endif

	CLIPFLAG = c->e[0];
	dt->clip_damage = at->clip_access = TRUE;

	return (OK);
}

/* Class 5 */

/*ARGSUSED*/
Ct_err LineIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"LineIndex\n");
#endif
	dt->line_ind = c->ix[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err LineType(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"LineType\n");
#endif

	dt->line_type = c->ix[0];

	dt->line_type_damage = at->line_type_access = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err LineWidth(c)
CGMC *c;
{
	float	line_width = c->r[0];

	if (Option.line_width_scale_set) line_width *= Option.line_width_scale;

	/*
	 * make sure line width doesn't exceed bounds
	 */
	if (Option.min_line_width_set) 
		line_width = MAX(line_width, Option.min_line_width);

	if (Option.max_line_width_set) 
		line_width = MIN(line_width, Option.max_line_width);

	/*
	 * record the line width
	 */
	dt->line_width = line_width;

	dt->line_width_damage = at->line_width_access = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err LineColr(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"LineColr\n");
#endif

	if (CSM == INDEXED) 
		dt->line_colour.index = c->ci[0];
	else {
		dt->line_colour.direct.red = c->cd[0].red;
		dt->line_colour.direct.green = c->cd[0].green;
		dt->line_colour.direct.blue = c->cd[0].blue;
	}

	dt->line_colour_damage = at->line_colour_access = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err MarkerIndex(c)
CGMC *c;
{ 
#ifdef DEBUG
	(void)fprintf(stderr,"MarkerIndex\n");
#endif
	dt->marker_ind = c->ix[0];
	return (OK);
}

Ct_err MarkerType(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"MarkerType\n");
#endif

	dt->markertype = c->ix[0];
	dt->marker_type_damage = at->marker_type_access = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err MarkerSize(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"MarkerSize\n");
#endif

	if (MAR_SIZ_MOD == SCALED) 
		dt->markersize = c->r[0];
	else
		dt->markersize = c->vdc[0];

	dt->marker_size_damage = at->marker_size_access = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err MarkerColr(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"MarkerColr\n");
#endif
	if (CSM == INDEXED) 
		dt->marker_colour.index = c->ci[0];
	else {
		dt->marker_colour.direct.red = c->cd[0].red;
		dt->marker_colour.direct.green = c->cd[0].green;
		dt->marker_colour.direct.blue = c->cd[0].blue;
	}

	dt->marker_colour_damage = at->marker_colour_access = TRUE; 
	return (OK);
} 

/*ARGSUSED*/
Ct_err TextIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"TextIndex\n");
#endif
	dt->text_ind = c->ix[0];
	return (OK);
}


/*ARGSUSED*/
Ct_err TextFontIndex(c)
CGMC *c;
{
	dt->text_f_ind = c->ix[0];
	dt->text_f_ind_damage = at->text_f_ind_access = TRUE;

	return(OK);
}

/*ARGSUSED*/
Ct_err TextPrec(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"TextPrec\n");
#endif
	dt->text_prec = c->e[0];
	return (OK);
}

Ct_err CharExpan(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"CharExpan\n");
#endif

	dt->char_expan = c->r[0];
	return (OK);
}

Ct_err CharSpace(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"CharSpace\n");
#endif

	dt->char_space = c->r[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err TextColr(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"TextColr\n");
#endif
	if (CSM == INDEXED) 
		dt->text_colour.index = c->ci[0];
	else {
		dt->text_colour.direct.red = c->cd[0].red;
		dt->text_colour.direct.green = c->cd[0].green;
		dt->text_colour.direct.blue = c->cd[0].blue;
	}
	return (OK);
}
Ct_err CharHeight(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"CharHeight\n");
#endif

	dt->char_height = c->vdc[0];
	return (OK);
}
Ct_err CharOri(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"CharOri\n");
#endif

	dt->char_x_up = c->vdc[0];
	dt->char_y_up = c->vdc[1];
	dt->char_x_base = c->vdc[2];
	dt->char_y_base = c->vdc[3];
	return (OK);
}
Ct_err TextPath(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"TextPath\n");
#endif

	dt->text_path = c->e[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err TextAlign(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"TextAlign\n");
#endif
	dt->text_ali_h = c->e[0];
	dt->text_ali_v = c->e[1];
	dt->text_ali_c_h = c->r[0];
	dt->text_ali_c_h = c->r[1];
	return (OK);
}

/*ARGSUSED*/
Ct_err CharSetIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"CharSetIndex\n");
#endif
	dt->char_ind = c->ix[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err AltCharSetIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"AltCharSetIndex\n");
#endif
	dt->alt_char_ind = c->ix[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err FillIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"FillIndex\n");
#endif
	dt->fill_ind = c->ix[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err IntStyle(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"IntStyle\n");
#endif
	dt->int_style = c->e[0];
	dt->int_style_damage = at->int_style_access = TRUE;
	
	return (OK);
}

/*ARGSUSED*/
Ct_err FillColr(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"FillColr\n");
#endif
	if (CSM == INDEXED) 
		dt->fill_colour.index = c->ci[0];
	else {
		dt->fill_colour.direct.red = c->cd[0].red;
		dt->fill_colour.direct.green = c->cd[0].green;
		dt->fill_colour.direct.blue = c->cd[0].blue;
	}
	dt->fill_colour_damage = at->fill_colour_access = TRUE;
	return (OK);
}

/*ARGSUSED*/
Ct_err HatchIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"HatchIndex\n");
#endif
	dt->hatch_ind = c->ix[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err PatIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"PatIndex\n");
#endif
	dt->pattern_ind = c->ix[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err EdgeIndex(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"EdgeIndex\n");
#endif
	dt->edge_ind = c->ix[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err EdgeType(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"EdgeType\n");
#endif
	dt->edge_type = c->ix[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err EdgeWidth(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"EdgeWidth\n");
#endif
	return (OK);
}

/*ARGSUSED*/
Ct_err EdgeColr(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"EdgeColr\n");
#endif
	return (OK);
}

/*ARGSUSED*/
Ct_err EdgeVis(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"EdgeVis\n");
#endif
	dt->edge_vis = c->e[0];
	return (OK);
}

/*ARGSUSED*/
Ct_err FillRefPt(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"FillRefPt\n");
#endif
	dt->fill_r_pt.x = c->p[0].x;
	dt->fill_r_pt.y = c->p[0].y;
	return (OK);
}

/*ARGSUSED*/
Ct_err PatTable(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"PatTable\n");
#endif
	return (OK);
}

/*ARGSUSED*/
Ct_err PatSize(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"PatSize\n");
#endif
	return (OK);
}

/*
 *  The ColrTable function has been moved to gcap.c
 */

/*ARGSUSED*/
Ct_err ASF(c)
CGMC *c;
{
#ifdef DEBUG
	(void)fprintf(stderr,"ASF\n");
#endif
	return (OK);
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
	LINE_WIDTH_ACCESS = TRUE;

	/*
	 * record the new minimum for future use
	 */
	Option.min_line_width = line_width;
	Option.min_line_width_set = TRUE;;
	
}

/*
 *	Set Maximum line width.
 */
SetMaxLineWidthDefault(line_width)
	float	line_width;
{
	defaulttable.line_width = MIN(line_width, defaulttable.line_width);

	LINE_WIDTH_ACCESS = TRUE;

	Option.max_line_width = line_width;
	Option.max_line_width_set = TRUE;;
	
}

/*
 *	set additional line scaling
 */
SetAdditionalLineScale(line_scale)
	float	line_scale;
{
	LINE_WIDTH_ACCESS = TRUE;

	defaulttable.line_width = line_scale;

	Option.line_width_scale = line_scale;
	Option.line_width_scale_set = TRUE;;
}
