/*
 *	$Id: clear_text.c,v 1.4 1991-09-26 16:29:07 clyne Exp $
 */
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
 *	Clear_text.c:
 *
 *	Author:	John clyne	(clyne@bierstadt.ucar.edu)
 *
 *	Date:	Sat Jan  7 15:14:03 MST 1989
 *
 *	
 *		This file completely contains the clear text capability
 *	of ctrans. Clear_text will print out the contents of a metafile
 *	to stdout. The depth of information output may be selected 
 *	interactively. Clear_text will print out all elments of the
 *	CGM defined by the standard. But, will only print out the parameters
 *	of those elements supported by NCAR as listed in the "NCAR Graphics 
 *	Installers Guide."
 */
/*LINTLIBRARY*/

#include <stdio.h>
#include	<sys/types.h>
#include <ncarv.h>
#include <cgm_tools.h>
#include <cterror.h>
#include "cgmc.h"
#include "text.h"

#ifndef lint
static char *RCSid = "$Header: /home/brownrig/SVN/CVS/ncarg/ncarview/src/lib/libctrans/clear_text.c,v 1.4 1991-09-26 16:29:07 clyne Exp $";
#endif


static	struct	{
	BoolType_	data,	/* true if user wants element's parameters   */	
			para;	/* true if user also wants all data points */
	} commLineOpt;

static	boolean	data,	/* true if user wants element's parameters   */	
		para;	/* true if user also wants all data points */

static  Option  options[] =  {
        {"Data", BoolType, (unsigned long) &commLineOpt.data, 
						sizeof (BoolType_ )},
        {"Para", BoolType, (unsigned long) &commLineOpt.para, 
						sizeof (BoolType_ )},
	{NULL}
        };


/*
 *	some things ctrans needs to know to interpret the metafile
 */
static	int	color_sel_mode = INDEXED; 	/* colour selection mode     */
static	int	line_wid_mode = SCALED;		/* line width selection mode */
static	int	marker_siz_mode = SCALED;	/* marker size selection mode*/


/*
 *	Below are the functions called from the jumptable
 *
 * 	Class 0 Function
/*ARGSUSED*/
Ct_err	CTXT_NoOp(c)
CGMC *c;
{
	(void) printf("\nNo-Op\n");
}
/*ARGSUSED*/
Ct_err	CTXT_BegMF(c)
CGMC *c;
{
	/*
	 *	parse clear_text specific command line args
	 */

	getOptions((caddr_t) 0, options);

	data = commLineOpt.data;
	para = commLineOpt.para;

	(void) printf("\nBEGIN METAFILE\n");
	if (para) 
		(void) printf("	metafile name	-> \"%s\"\n", c->s->string[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EndMF(c)
CGMC *c;
{

	(void) printf("\nEND METAFILE\n");
	return (OK);
}


/*ARGSUSED*/
Ct_err	CTXT_BegPic(c)
CGMC *c;
{

	(void) printf("\nBEGIN PICTURE\n");

	if (para) 
		(void) printf("	picture name	-> \"%s\"\n", c->s->string[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_BegPicBody(c)
CGMC *c;
{

	(void) printf("\nBEGIN PICTURE BODY\n");

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EndPic(c)
CGMC *c;
{

	(void) printf("\nEND PICTURE\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_ClearDevice(c)
CGMC *c;
{
	return(OK);
}

/*
 *	Class 1 elements
 */

/*ARGSUSED*/
Ct_err	CTXT_MFVersion(c)
CGMC *c;
{

	(void) printf("\nMETAFILE VERSION\n");

	if (para) 
		(void) printf("	version number	-> %i\n", c->i[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_MFDesc(c)
CGMC *c;
{

	(void) printf("\nMETAFILE DESCRIPTION\n");

	if (para) 
		(void) printf("	description	-> \"%s\"\n", c->s->string[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_VDCType(c)
CGMC *c;
{

	(void) printf("\nVDC TYPE\n");

	if (para) {
		switch (c->e[0]) {
		
		case 0:
			(void) printf("	type	 	-> integer\n");
			break;
		case 1:
			(void) printf("	type	 	->   real\n");
			break;
		default:
			(void) printf("	undefined type	-> %6d\n",c->e[0]);
		}
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_IntergerPrec(c)
CGMC *c;
{

	(void) printf("\nINTEGER PRECISION\n");

	if (para) 
		(void) printf("	precision	-> %6d\n", c->i[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_RealPrec(c)
CGMC *c;
{

	(void) printf("\nREAL PRECISION (*unsupported*)\n");

	if (para) { 
		switch (c->i[0]) {
		
		case 0:
			(void) printf("	type	 	->  float\n");
			break;
		case 1:
			(void) printf("	type	 	->  fixed\n");
			break;
		default:
			(void) printf("	undefined rep	-> %6d\n",c->i[0]);
		}

		(void) printf("	exponent width	-> %6d\n", c->i[1]);
		(void) printf("	mantisa width	-> %6d\n", c->i[2]);

	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_IndexPrec(c)
CGMC *c;
{

	(void) printf("\nINDEX PRECISION\n");
	
	if (para) 
		(void) printf("	precision	-> %6d\n", c->i[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_ColrPrec(c)
CGMC *c;
{

	(void) printf("\nCOLOUR PRECISION\n");
	
	if (para) 
		(void) printf("	precision	-> %6d\n", c->i[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_ColrIndexPrec(c)
CGMC *c;
{

	(void) printf("\nCOLOUR INDEX PRECISION\n");
	
	if (para) 
		(void) printf("	precision	-> %6d\n", c->i[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_MaxColrIndex(c)
CGMC *c;
{

	(void) printf("\nMAXIMUM COLOUR INDEX (*unsupported*)\n");
	
	if (para) 
		(void) printf("	index		-> %6d\n", c->ci[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_ColrValueExt(c)
CGMC *c;
{

	(void) printf("\nCOLOUR VALUE EXTENT (*unsupported*)\n");
	
	if (para) {
		(void) printf("	minimum rgb	-> %6d, %6d, %6d\n", 
			c->cd[0].red, c->cd[0].green, c->cd[0].blue);

		(void) printf("	maximum rgb	-> %6d, %6d, %6d\n", 
			c->cd[1].red, c->cd[1].green, c->cd[1].blue);
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_MFElemList(c)
CGMC *c;
{

	int	i,j;

	(void) printf("\nMETAFILE ELEMENT LIST \n");

	if (para && c->i[0]) {

		if (c->ix[0] == -1) {

			if (c->ix[1] == 0)
				(void) printf("	elements	: The Drawing Set\n");
			else if (c->ix[1] == 1)
				(void) printf("	elements	: Drawing-Plus-Control Set\n");

			else
				(void) printf("	invalid set	:\n");
		}

		else {
			(void) printf("			_class_   _id_\n");

			for (i = 0,j = 0; i < c->i[0]; i++,j+=2) {

				(void) printf("			%6d, %6d\n",
					c->ix[j], c->ix[j+1]);
			}
		}
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_MFDefaults(c)
CGMC *c;
{

	(void) printf("\nMetafile Defaults Replacement\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_FontList(c)
CGMC *c;
{
	int	i;

	(void) printf("\nFONT LIST \n");

	if (para) {
		for (i=0; i<c->Snum; i++)
			(void) printf("\n font		-> \"%s\"\n", 
				c->s->string[i]);
	}
	return (OK);
}


/*ARGSUSED*/
Ct_err	CTXT_CharSetList(c)
CGMC *c;
{

	(void) printf("\nCHARACTER SET LIST (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_CharCoding(c)
CGMC *c;
{

	(void) printf("\nCHARACTER CODING ANNOUNCER (*unsupported*)\n");
	return (OK);
}


/* 
 *	Class 2 elements
 */
/*ARGSUSED*/
Ct_err	CTXT_ScaleMode(c)
CGMC *c;
{

	(void) printf("\nSCALING MODE (*unsupported*)\n");

	if (para) {

		switch (c->e[0]) {
		
		case 0:
			(void) printf("	mode	 	-> abstract\n");
			break;
		case 1:
			(void) printf("	mode	 	-> metric\n");
			(void) printf("\n scale factor	-> %8.2f\n", c->r[0]);
			break;

		default:
			(void) printf("\n invalid mode	-> %6d\n", c->e[0]);
		}
	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_ColrMode(c)
CGMC *c;
{

	(void) printf("\nCOLOUR SELECTION MODE\n");

	if (para) {

		switch (c->e[0]) {
		
		case 0:
			(void) printf("	mode	 	-> indexed\n");
			color_sel_mode = INDEXED;
			break;
		case 1:
			(void) printf("	mode	 	-> direct\n");
			color_sel_mode = DIRECT;
			break;
		default:
			(void) printf("\n invalid mode	-> %6d\n", c->e[0]);
		}
	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_LineWidthMode(c)
CGMC *c;
{

	(void) printf("\nLINE WIDTH SPECIFICATION MODE (*unsupported*)\n");

	if (para) {

		switch (c->e[0]) {
		
		case ABSOLUTE:
			(void) printf("	mode	 	-> absolute\n");
			line_wid_mode = ABSOLUTE;
			break;
		case SCALED:
			(void) printf("	mode	 	-> scaled\n");
			line_wid_mode = SCALED;
			break;
		default:
			(void) printf("\n invalid mode	-> %6d\n", c->e[0]);
		}
	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_MarkerSizeMode(c)
CGMC *c;
{

	(void) printf("\nMARKER SIZE SPECIFICATION MODE (*unsupported*)\n");

	if (para) {

		switch (c->e[0]) {
		
		case ABSOLUTE:
			(void) printf("	mode	 	-> absolute\n");
			marker_siz_mode = ABSOLUTE;
			break;
		case SCALED:
			(void) printf("	mode	 	-> scaled\n");
			marker_siz_mode = SCALED;
			break;
		default:
			(void) printf("\n invalid mode	-> %6d\n", c->e[0]);
		}
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EdgeWidthMode(c)
CGMC *c;
{

	(void) printf("\nEDGE WIDTH SPECIFICATION MODE (*unsupported*)\n");

	if (para) {

		switch (c->e[0]) {
		
		case ABSOLUTE:
			(void) printf("	mode	 	-> absolute\n");
			break;
		case SCALED:
			(void) printf("	mode	 	-> scaled\n");
			break;
		default:
			(void) printf("\n invalid mode	-> %6d\n", c->e[0]);
		}
	}
	return (OK);
}

Ct_err	CTXT_VDCExt(c)
CGMC *c;
{

	(void) printf("\nVDC EXTENT\n");

	if (para) {
		(void) printf("	first point	-> %6d, %6d\n", 
			c->p[0].x, c->p[0].y);

		(void) printf("	second point	-> %6d, %6d\n", 
			c->p[1].x, c->p[1].y);
	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_BackColr(c)
CGMC *c;
{

	(void) printf("\nBACKGROUND COLOUR\n");

	if (para) {
		(void) printf("	background rgb	-> (%6d, %6d, %6d)\n",
			c->cd[0].red, c->cd[0].green, c->cd[0].blue);

	}

	return (OK);
}

/* 
 *	Class 3 elements
 */

/*ARGSUSED*/
Ct_err	CTXT_VDCIntergerPrec(c)
CGMC *c;
{

	(void) printf("\nVDC INTEGER PRECISION\n");

	if (para) 
		(void) printf("	precision	-> %6d\n", c->i[0]);
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_VDCRealPrec(c)
CGMC *c;
{

	(void) printf("\nVDC REAL PRECISION (*unsupported*)\n");

	if (para) { 
		switch (c->i[0]) {
		
		case 0:
			(void) printf("	type	 	->  float\n");
			break;
		case 1:
			(void) printf("	type	 	->  fixed\n");
			break;
		default:
			(void) printf("	undefined rep	-> %6d\n",c->i[0]);
		}

		(void) printf("	exponent width	-> %6d\n", c->i[1]);
		(void) printf("	mantisa width	-> %6d\n", c->i[2]);
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_AuxColr(c)
CGMC *c;
{

	(void) printf("\nAUXILIARY COLOUR (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_Transparency(c)
CGMC *c;
{

	(void) printf("\nTRANSPARENCY (*unsupported*)\n");
	return (OK);
}
Ct_err	CTXT_ClipRect(c)
CGMC *c;
{

	(void) printf("\nCLIP RECTANGLE\n");

	if (para) {
		(void) printf("	first corner	-> %6d, %6d\n",
			c->p[0].x, c->p[0].y);

		(void) printf("	second corner	-> %6d, %6d\n",
			c->p[1].x, c->p[1].y);
	}

	return (OK);
}
Ct_err	CTXT_Clip(c)
CGMC *c;
{

	(void) printf("\nCLIP INDICATOR\n");

	if (para) {
		if (c->e[0] == 0)
			(void) printf("	flag		->    off\n");
		else
			(void) printf("	flag		->     on\n");
	}
	return (OK);
}


/*
 *	Class 4 elementes
 */


/*ARGSUSED*/
Ct_err	CTXT_PolyLine(c)
CGMC *c;
{
	int	i;


	(void) printf("\nPOLYLINE\n");

	if (data) {

		(void) printf("	%d points follow:\n", c->Pnum);
		for (i = 0; i < c->Pnum; i++) {
			(void) printf("		P(%d)	-> %6d, %6d\n",
				i, c->p[i].x, c->p[i].y);
		}
	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_DisjtLine(c)
CGMC *c;
{
	int	i;


	(void) printf("\nDISJOINT POLYLINE (*unsupported*)\n");

	if (data) {

		(void) printf("	%d points follow:\n", c->Pnum);
		for (i = 0; i < c->Pnum; i++) {
			(void) printf("		P(%d)	-> %6d, %6d\n",
				i, c->p[i].x, c->p[i].y);
		}
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_PolyMarker(c)
CGMC *c;
{
	int	i;


	(void) printf("\nPOLYMARKER\n");

	if (data) {

		(void) printf("	%d points follow:\n", c->Pnum);
		for (i = 0; i < c->Pnum; i++) {
			(void) printf("		P(%d)	-> %6d, %6d\n",
				i, c->p[i].x, c->p[i].y);
		}

	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_Text(c)
CGMC *c;
{

	(void) printf("\nTEXT\n");

	if (para) {
		(void) printf("	position	-> %6d, %6d\n", 
			c->p[0].x, c->p[0].y);

		if (c->e[0] == 0)
			(void) printf("	final/not final	->    not final\n");
		else
			(void) printf("	final/not final	->  final\n");

		(void) printf("	string		-> \"%s\"\n", c->s->string[0]);
	}
	return (OK);
}


/*ARGSUSED*/
Ct_err	CTXT_RestrText(c)
CGMC *c;
{

	(void) printf("\nRESTRICTED TEXT (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_ApndText(c)
CGMC *c;
{

	(void) printf("\nAPPEND TEXT (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_Polygon(c)
CGMC *c;
{
	int	i;


	(void) printf("\nPOLYGON\n");

	if (data) {

		(void) printf("	%d points follow:\n", c->Pnum);
		for (i = 0; i < c->Pnum; i++) {
			(void) printf("		P(%d)	-> %6d, %6d\n",
				i, c->p[i].x, c->p[i].y);
		}
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_PolygonSet(c)
CGMC *c;
{

	(void) printf("\nPOLYGON SET (*unsupported*)\n");
	return (OK);
}


/*ARGSUSED*/
#define	RUN	0	/* run length encoding of cell arrays	*/
#define	PACKED	1	/* packed encoding			*/
Ct_err	CTXT_CellArray(c)
CGMC *c;
{

	int	nx,ny;
	int	countindex = 3;	/* index into run lenght cell count	*/
	int	colorindex = 0;	/* index into cell colour	 	*/
	int	counter = 0;	/* number of cells represented by a run	*/


	(void) printf("\nCELL ARRAY \n");

	if (para) {
		(void) printf("	corner point P	-> %6d, %6d\n",
			c->p[0].x, c->p[0].y);

		(void) printf("	corner point Q	-> %6d, %6d\n",
			c->p[1].x, c->p[1].y);

		(void) printf("	corner point R	-> %6d, %6d\n",
			c->p[2].x, c->p[2].y);

		(void) printf("	nx		-> %6d\n", c->i[0]);
		(void) printf("	ny		-> %6d\n", c->i[1]);
		(void) printf("	color precision	-> %6d\n", c->i[2]);

		if (c->e[0] == RUN)
			(void) printf("	cell rep. mode	-> run length list\n");
		else if (c->e[0] == PACKED)
			(void) printf("	cell rep. mode	-> packed list\n");
		else
			(void) printf("	invalid cell rep-> %6d\n", c->e[0]);

	}

	/*
	 *	print out data if requested	
	 */

	if (data && c->e[0] == RUN) {

		(void) printf("	run length encoded data follows:\n"); 

		for(ny = 0; ny < c->i[1]; ny++) { 

			(void) printf("\n		row %d:\n\n", ny);
			(void) printf("\n		_count_		_color_\n\n");
			
			counter = 0;

			/* while not end of row	*/
			while (counter < c->i[0]) {
				if (color_sel_mode == INDEXED) {
					(void) printf ("			%d	%d\n", c->i[countindex], c->ci[colorindex]);

				}
				else {
					(void) printf ("			%6d	%d %d %d\n", 
						c->i[countindex], 
						c->cd[colorindex].red, 
						c->cd[colorindex].green, 
						c->cd[colorindex].blue);
				}

				counter += c->i[countindex];
				countindex++;
				colorindex++;
			}
		}
	} else if (data && c->e[0] == PACKED) {

		(void) printf("	packed encoded data follows:\n"); 

		for (ny = 0; ny < c->i[1]; ny++) {

			(void) printf("\n\n		row %d:", ny);
			for (nx = 0; nx < c->i[0]; nx++, colorindex++) {


				/*
				 * make sure data available in cgmc
				 * This statement will only evaluate to
				 * true when rendering super big cell arrays
				 */
				if (colorindex == c->Cnum && c->more) {
					if (Instr_Dec(c) < 1) {
						ct_error(T_FRE, "metafile");
						return(DIE);
					}

					colorindex = 0;
				}

				/* line too long. Add line feeds	*/
				if (!(nx % 10))
					(void)printf("\n			");

				if (color_sel_mode == INDEXED) {
					(void) printf("%4d",
						c->c[colorindex]);


				}
				else {
					(void) printf("(%4d,%4d,%d)", 
						c->cd[colorindex].red, 
						c->cd[colorindex].green, 
						c->cd[colorindex].blue);
				}
			}
		}
	(void) printf("\n");
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_GDP(c)
CGMC *c;
{

	int	i;


	(void) printf("\nGENERALIZED DRAWING PRIMITIVE \n");

	if (para) {
		(void) printf("	GDP identifier	-> %6d\n", c->i[0]);
		(void) printf("	point list size	-> %6d\n", c->i[1]);
	}

	if (data) {
		(void) printf("	%d points follow:\n", c->Pnum);
		for (i = 0; i < c->i[1]; i++) {
			(void) printf("		P(%d)	-> %6d, %6d\n",
				i, c->p[i].x, c->p[i].y);
		}

		(void)printf("\n	data record	-> \"%s\"\n", c->s->string[0]);
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_Rect(c)
CGMC *c;
{

	(void) printf("\nRECTANGLE (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_Circle(c)
CGMC *c;
{

	(void) printf("\nCIRCLE (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_Arc3Pt(c)
CGMC *c;
{

	(void) printf("\nCIRCULAR ARC 3 POINT (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_Arc3PtClose(c)
CGMC *c;
{

	(void) printf("\nCIRCULAR ARC 3 POINT CLOSE (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_ArcCtr(c)
CGMC *c;
{
	(void) printf("\nCIRCULAR ARC CENTRE (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_ArcCtrClose(c)
CGMC *c;
{

	(void) printf("\nCIRCULAR ARC CENTRE CLOSE (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_Ellipse(c)
CGMC *c;
{

	(void) printf("\nELLIPSE (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EllipArc(c)
CGMC *c;
{

	(void) printf("\nELLIPTICAL ARC (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EllipArcClose(c)
CGMC *c;
{

	(void) printf("\nELLIPTICAL ARC CLOSE (*unsupported*)\n");
	return (OK);
}


/* 
 *	Class 5 elements
 */

/*ARGSUSED*/
Ct_err	CTXT_LineIndex(c)
CGMC *c;
{

	(void) printf("\nLINE BUNDLE INDEX (*unsupported*)\n");

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_LineType(c)
CGMC *c;
{

	(void) printf("\nLINE TYPE\n");

	if (para) {
		switch (c->ix[0]) {
		
		case L_SOLID:
			(void) printf("	line type	->  solid\n");
			break;
		case L_DASH:
			(void) printf("	line type	->   dash\n");
			break;
		case L_DOT:
			(void) printf("	line type	->    dot\n");
			break;
		case L_DASH_DOT:
			(void) printf("	line type	->  dash-dot\n");
			break;
		case L_DASH_DOT_DOT:
			(void) printf("	line type	->  dash-dot-dot\n");
			break;
		default:
			(void) printf("	invalid  type	-> %6d\n", c->ix[0]);
			break;
		}
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_LineWidth(c)
CGMC *c;
{

	(void) printf ("\nLINE WIDTH\n");

	if (para) {
		if (line_wid_mode == ABSOLUTE)
			(void) printf("	absolute width	-> %6d\n", c->vdc[0]);
		else
			(void) printf("	scaled width	-> %8.2f\n", c->r[0]);
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_LineColr(c)
CGMC *c;
{

	(void) printf("\nLINE COLOUR\n");

	if (para) {
		if (color_sel_mode == INDEXED)
			(void) printf("	color index	-> %6d\n", c->ci[0]);
		else
			(void) printf("	direct color -> (%6d, %6d, %6d)\n", 
				c->cd[0].red, c->cd[0].green, c->cd[0].blue);
	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_MarkerIndex(c)
CGMC *c;
{ 

	(void) printf("\nMARKER BUNDLE INDEX (*unsupported*)\n");
	return (OK);
}

/*
 *	marker #defines
 */
#define	DOT_M		1
#define	PLUS_M		2
#define	ASTERISK_M	3
#define	CIRCLE_M	4
#define	CROSS_M		5

Ct_err	CTXT_MarkerType(c)
CGMC *c;
{
	(void) printf("\nMARKER TYPE\n");

	if (para) {
		switch (c->ix[0]) {
		
		case DOT_M:
			(void) printf("	marker type	->    dot\n");
			break;
		case PLUS_M:
			(void) printf("	marker type	->   plus\n");
			break;
		case ASTERISK_M:
			(void) printf("	marker type	-> asterisk\n");
			break;
		case CIRCLE_M:
			(void) printf("	marker type	-> circle\n");
			break;
		case CROSS_M:
			(void) printf("	marker type	->  cross\n");
			break;
		default:
			(void) printf("	invalid  type	-> %6d\n", c->ix[0]);
			break;
		}
	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_MarkerSize(c)
CGMC *c;
{

	(void) printf("\nMARKER SIZE\n");

	if (para) {
		if (marker_siz_mode == ABSOLUTE)
			(void) printf("	absolute size	-> %6d\n", c->vdc[0]);
		else
			(void) printf("	scaled size	-> %8.2f\n", c->r[0]);
	}

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_MarkerColr(c)
CGMC *c;
{

	(void) printf ("\nMARKER COLOUR\n");

	if (para) {
		if (color_sel_mode == INDEXED)
			(void) printf("	color index	-> %6d\n", c->ci[0]);
		else
			(void) printf("	direct color -> (%6d, %6d, %6d)\n", 
				c->cd[0].red, c->cd[0].green, c->cd[0].blue);
	}
	return (OK);
} 

/*ARGSUSED*/
Ct_err	CTXT_TextIndex(c)
CGMC *c;
{
	(void) printf ("\nTEXT BUNDLE INDEX (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_TextFontIndex(c)
CGMC *c;
{
	(void) printf ("\nTEXT FONT INDEX \n");

	if (para) {
		(void) printf("	font index	-> %6d\n", c->ix[0]);
	}
	return (OK);
}


/*ARGSUSED*/
Ct_err	CTXT_TextPrec(c)
CGMC *c;
{
	(void) printf ("\nTEXT PRECISION \n");

	if (para) {
		switch (c->e[0]) {
		
		case STRING_P:
			(void) printf("	text prec.	-> string\n");
			break;
		case CHAR_P:
			(void) printf("	text prec.	-> character\n");
			break;
		case STROKE_P:
			(void) printf("	text prec	-> stroke\n");
			break;
		default:
			(void) printf("	invalid prec	-> %6d\n", c->e[0]);
			break;
		}
	}
	return (OK);
}


/*ARGSUSED*/
Ct_err	CTXT_CharExpan(c)
CGMC *c;
{
	(void) printf("\nCHARACTER EXPANSION FACTOR\n");

	if (para) {
		(void) printf("	expansion	-> %8.2f\n", c->r[0]);
	}
	return (OK);
}

Ct_err	CTXT_CharSpace(c)
CGMC *c;
{
	(void) printf("\nCHARACTER SPACING\n");

	if (para) {
		(void) printf("	spacing		-> %8.2f\n", c->r[0]);
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_TextColr(c)
CGMC *c;
{
	(void) printf("\nTEXT COLOUR\n");

	if (para) {
		if (color_sel_mode == INDEXED)
			(void) printf("	color index	-> %6d\n", c->ci[0]);
		else
			(void) printf("	direct color	-> (%6d, %6d, %6d)\n", 
				c->cd[0].red, c->cd[0].green, c->cd[0].blue);
	}
	return (OK);
}
Ct_err	CTXT_CharHeight(c)
CGMC *c;
{
	(void) printf("\nCHARACTER HEIGHT\n");

	if (para) {
		(void) printf("	height		-> %6d\n", c->vdc[0]);
	}
	return (OK);
}
Ct_err	CTXT_CharOri(c)
CGMC *c;
{
	(void) printf("\nCHARACTER ORIENTATION\n");

	if (para) {
		(void) printf("	x character up	-> %6d\n", c->vdc[0]);
		(void) printf("	y character up	-> %6d\n", c->vdc[1]);
		(void) printf("	x character down-> %6d\n", c->vdc[2]);
		(void) printf("	y character down-> %6d\n", c->vdc[3]);
	}
	return (OK);
}

Ct_err	CTXT_TextPath(c)
CGMC *c;
{
	(void) printf ("\nTEXT PATH\n");

	if (para) {
		switch (c->e[0]) {
		
		case PATH_RIGHT:
			(void) printf("	text path.	->  right\n");
			break;
		case PATH_LEFT:
			(void) printf("	text path.	->   left\n");
			break;
		case PATH_UP:
			(void) printf("	text path	->     up\n");
			break;
		case PATH_DOWN:
			(void) printf("	text path	->   down\n");
			break;
		default:
			(void) printf("	invalid path	-> %6d\n", c->e[0]);
			break;
		}
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_TextAlign(c)
CGMC *c;
{
	(void)printf ("\nTEXT ALIGNMENT\n");

	if (para) {
		/*
		 * horizontal alignment
		 */
		switch (c->e[0]) {
		
		case A_NORM_H:
			(void) printf("	horizontal ali.	-> normal\n");
			break;
		case A_LEFT:
			(void) printf("	horizontal ali.	->   left\n");
			break;
		case A_CENTER:
			(void) printf("	horizontal ali.	-> center\n");
			break;
		case A_RIGHT:
			(void) printf("	horizontal ali.	->  right\n");
			break;
		case A_CO_HOR:
			(void) printf("	cont. hori. ali.-> %6d\n", c->r[0]);
			break;
		default:
			(void) printf("	invalid hor. ali-> %6d\n",c->e[0]);
			break;
		}

		/*
		 * vertical alignment
		 */
		switch (c->e[1]) {
		
		case A_NORM_V:
			(void) printf("	vertical ali.	-> normal\n");
			break;
		case A_TOP:
			(void) printf("	vertical ali.	->    top\n");
			break;
		case A_CAP:
			(void) printf("	vertical ali.	->    cap\n");
			break;
		case A_HALF:
			(void) printf("	vertical ali.	->   half\n");
			break;
		case A_BASE:
			(void) printf("	vertical ali.	->   base\n");
			break;
		case A_BOTTOM:
			(void) printf("	vertical ali.	-> bottom\n");
			break;
		case A_CO_VER:
			(void) printf("	cont. vert ali.	-> %6d\n", c->r[1]);
			break;
		default:
			(void) printf("	invalid ver. ali-> %6d\n",c->e[1]);
			break;
		}

	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_CharSetIndex(c)
CGMC *c;
{
	(void) printf("\nCHARACTER SET INDEX (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_AltCharSetIndex(c)
CGMC *c;
{
	(void) printf("\nALTERNATE CHARACTER SET INDEX (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_FillIndex(c)
CGMC *c;
{
	(void) printf("\nFILL BUNDLE INDEX (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_IntStyle(c)
CGMC *c;
{
	(void) printf("\nINTERIOR STYLE\n");

	if (para) {
		switch (c->e[0]) {
		
		case HOLLOW_S:
			(void) printf("	interior style	-> hollow\n");
			break;
		case SOLID_S:
			(void) printf("	interior style	->  solid\n");
			break;
		case PATTERN_S:
			(void) printf("	interior style	-> pattern\n");
			break;
		case HATCH_S:
			(void) printf("	interior style	->  hatch\n");
			break;
		case EMPTY_S:
			(void) printf("	interior style	->  empty\n");
			break;
		default:
			(void) printf("	invalid style	-> %6d\n",c->e[0]);
			break;
		}
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_FillColr(c)
CGMC *c;
{
	(void) printf("\nFILL COLOUR\n");

	if (para) {
		if (color_sel_mode == INDEXED)
			(void) printf("	fill index	-> %6d\n", c->ci[0]);	
		else
			(void) printf("	direct colour	-> (%6d, %6d, %6d)\n", 
				c->cd[0].red, c->cd[0].green, c->cd[0].blue);	
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_HatchIndex(c)
CGMC *c;
{
	(void) printf("\nHATCH INDEX\n");

	if (para) {
		switch (c->ix[0]) {
		
		case HORIZONTAL:
			(void) printf("	hatch pattern	-> horizontal\n");
			break;
		case VERTICAL:
			(void) printf("	hatch pattern	-> vertical\n");
			break;
		case POSSITIVE:
			(void) printf("	hatch pattern	-> possitive\n");
			break;
		case NEGATIVE:
			(void) printf("	hatch pattern	-> negative\n");
			break;
		case HORIZ_VERT:
			(void) printf("	hatch pattern	-> horizontal & vertical\n");
			break;
		case POSS_NEG:
			(void) printf("	hatch pattern	-> possitive & negative\n");
			break;
		default:
			(void) printf("	invalid hatch	-> %6d\n",c->ix[0]);
			break;
		}
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_PatIndex(c)
CGMC *c;
{
	(void) printf("\nPATTERN INDEX\n");

	if (para) {
		(void) printf("	index		-> %6d\n", c->ix[0]);
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EdgeIndex(c)
CGMC *c;
{
	(void) printf("\nEDGE BUNDLE INDEX (*unsupported*)\n");

	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EdgeType(c)
CGMC *c;
{
	(void) printf("\nEDGE TYPE (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EdgeWidth(c)
CGMC *c;
{
	(void) printf("\nEDGE WIDTH (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EdgeColr(c)
CGMC *c;
{
	(void) printf("\nEDGE COLOUR (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_EdgeVis(c)
CGMC *c;
{
	(void) printf("\nEDGE VISIBILITY (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_FillRefPt(c)
CGMC *c;
{
	(void) printf("\nFILL REFERENCE POINT\n");

	if (para) {
		(void) printf("	fill point	-> %6d, %6d\n", 
			c->p[0].x, c->p[0].y);
	}
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_PatTable(c)
CGMC *c;
{
	(void) printf("\nPATTERN TABLE (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_PatSize(c)
CGMC *c;
{
	(void) printf("\nPATTERN SIZE (*unsupported*)\n");
	return (OK);
}

/*ARGSUSED*/
Ct_err	CTXT_ColrTable(c)
CGMC	*c;
{
	int	i;

	(void) printf("\nCOLOUR TABLE\n");

	if (para) {
		for (i = 0; i < c->CDnum; i++) {
		(void) printf("	index & colour	-> %6d	(%6d, %6d, %6d)\n",
			c->ci[0] + i, 
			c->cd[i].red, c->cd[i].green, c->cd[i].blue);
		}
	}
	return (OK);
}
/*ARGSUSED*/
Ct_err	CTXT_ASF(c)
CGMC *c;
{
	(void) printf("\nASPECT SOURCE FLAGS (*unsupported*)\n");
	return (OK);
}
/*
 * Class 6 Functions
 */

/*ARGSUSED*/
Ct_err	CTXT_Escape(c)
CGMC *c;
{

	int	i;


	(void) printf("\nESCAPE\n");

	if (para) {
		(void) printf("	escape id	-> %6d\n", c->i[0]);
	}

	if (data) {
		(void) printf("	data record follows:\n\n");

		for (i = 0; i < c->Snum; i++) 
			(void) printf("			 \"%s\"\n", 
				c->s->string[i]);
	}
	return (OK);
}

/*
 * Class 7 Functions
 */
/*ARGSUSED*/
Ct_err	CTXT_Message(c)
CGMC *c;
{

	(void) printf("\nMESSAGE (*unsupported*)\n");

	return (OK);
}
/*ARGSUSED*/
Ct_err	CTXT_ApplData(c)
CGMC *c;
{

	(void) printf("\nAPPLICATION DATA (*unsupported*)\n");
	return (OK);
}
