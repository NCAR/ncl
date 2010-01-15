/*
 *	$Id: pdfddi.h,v 1.3 2010-01-15 05:15:48 fred Exp $
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
        int             paper_height;
        int             paper_width;
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
