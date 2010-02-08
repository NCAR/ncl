/*
 *	$Id: psddi.h,v 1.7 2010-02-08 06:03:13 fred Exp $
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
 *      File:		psddi.h
 *
 *      Author:		Fred Clare
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Wed August 11 14:23:39 MDT 1993
 *
 *      Description:	This file defines the device dependent structure for
 *			gksc.ddp field for a PostScript device
 */

#ifndef	_psddi_
#define	_psddi_

#include "ps.h"
#include "gks.h"
#include "common.h"
#include "transform.h"

typedef	struct	PSddi_	{
	int   		wks_id;
	Transform2D	transform;
	TransSystem	tsystem;
	PSDeviceSpace	dspace;
        PSBoundingBox   bspace;
	ps_color	color;
	ps_file_type	type;
	ps_orientation 	orientation;
	char       	*file_name;
	float		sfill_spacing;
	float		hatch_spacing;
	int		stack_size;
	int		path_size;
	linejoin_type	line_join;
	linecap_type	line_cap;
	float		nominal_width_scale;
	int		full_background;
	float		miter_limit;
	float		scaling;
	char  		*output_file;
	FILE		*file_pointer;
	int		background;
	int		pict_empty;
	int		page_number;
        int             suppress_flag;
        int             paper_height;
        int             paper_width;
	float		color_map[1024];
	int		fonts_used[13];
	PSattribute	attributes;
	CoordSpace	gks_clip;        /* GKS clip rectangle stored in ndc */
	PSClipRect	ps_clip;
	} PSddp;

void DefaultColorTable(PSddp *);
void PSpreamble (PSddp *, preamble_type);
void OutputClipping (PSddp *, int);
void PSbackground (PSddp *);

#endif	/*	_psddi_	*/
