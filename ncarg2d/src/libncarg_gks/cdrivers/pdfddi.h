/*
 *	$Id: pdfddi.h,v 1.1 2003-01-13 23:47:23 fred Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *      File:		pdfddi.h
 *
 *      Author:		Fred Clare
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Wed Oct 16 11:20:18 MDT 2002
 *
 *      Description:	This file defines the device dependent structure for
 *			gksc.ddp field for a PDF device
 */

#ifndef	_pdfddi_
#define	_pdfddi_

#include "pdf.h"
#include "gks.h"
#include "common.h"
#include "transform.h"

typedef	struct	PDFddi_	{
	int   		wks_id;
	Transform2D	transform;
	TransSystem	tsystem;
	PDFDeviceSpace	dspace;
	pdf_color	color;
	pdf_file_type	type;
	pdf_orientation orientation;
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
	float		color_map[1024];
	int		fonts_used[13];
	PDFattribute	attributes;
	CoordSpace	gks_clip;        /* GKS clip rectangle stored in ndc */
	PDFClipRect	pdf_clip;
	} PDFddp;

void DefaultColorTable(PDFddp *);
void PDFpreamble (PDFddp *, preamble_type);
void OutputClipping (PDFddp *, int);
void PDFbackground (PDFddp *);

#endif	/*	_pdfddi_	*/
