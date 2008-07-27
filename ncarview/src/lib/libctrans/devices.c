/*
 *	$Id: devices.c,v 1.17 2008-07-27 03:18:43 haley Exp $
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
 *	devices.c:
 *
 *	Author		John Clyne	(clyne@redcloud.ucar.edu)
 *			
 *	Date		Mon Nov 28 14:45:24 MST 1988
 *
 *		This file contains the devices ctrans supports. To
 *	add a new device begin here by adding the device and its functions
 *	to the jump table
 */

#include	<stdio.h>
#include	<ncarg/cgm_tools.h>
#include	"cgmc.h"
#define		DEVICES
#include	"devices.h"

int	devicenum = (sizeof(devices)/sizeof(struct device));

/*
 *	Declarations for all the function in the Jump Table
 */

/*
 *	The functions for the GraphCap and FontCap processing
 *	also the funtions for setting the default-table values.
 */

/*	Class 0	*/

int BegMF(), EndMF(), BegPic(), BegPicBody(), EndPic(), ClearDevice();

/*	Class 1	*/

int MFVersion(), MFDesc(), VDCType(), IntergerPrec(), RealPrec(), 
	IndexPrec(),
    ColrPrec(), ColrIndexPrec(), MaxColrIndex(), ColrValueExt(), MFElemList(),
    MFDefaults(), FontList(), CharSetList(), CharCoding();

/*	Class 2	*/

int ScaleMode(), ColrMode(), LineWidthMode(), MarkerSizeMode(), EdgeWidthMode(),
    VDCExt(), BackColr();


/*	Class 3	*/

int VDCIntergerPrec(), VDCRealPrec(), AuxColr(), Transparency(), ClipRect(),
    Clip();

/*	Class 4	*/

int PolyLine(), DisjtLine(), PolyMarker(), Text(), RestrText(), ApndText(),
    Polygon(), PolygonSet(), CellArray(), GDP(), Rect(), Circle(), Arc3Pt(),
    Arc3PtClose(), ArcCtr(), ArcCtrClose(), Ellipse(), EllipArc(),
    EllipArcClose();

/*	Class 5	*/

int LineIndex(), LineType(), LineWidth(), LineColr(), MarkerIndex(),
    MarkerType(), MarkerSize(), MarkerColr(), TextIndex(), TextFontIndex(),
    TextPrec(), CharExpan(), CharSpace(), TextColr(), CharHeight(), CharOri(),
    TextPath(), TextAlign(), CharSetIndex(), AltCharSetIndex(), FillIndex(),
    IntStyle(), FillColr(), HatchIndex(), PatIndex(), EdgeIndex(), EdgeType(),
    EdgeWidth(), EdgeColr(), EdgeVis(), FillRefPt(), PatTable(), PatSize(),
    ColrTable(), ASF();

/*	Class 6	*/

int Escape();

/*	Class 7	*/

int Message(), ApplData();


/*
 *				raster
 *
 */

/*	Class 0	*/

int	Ras_BegMF(), Ras_EndMF(), Ras_BegPic(), Ras_BegPicBody(), 
	Ras_EndPic(), Ras_ClearDevice();

/*	Class 4	*/

int	Ras_PolyMarker(), Ras_CellArray();

/*	Class 5	*/




#ifdef X11

/*
 *				X11
 *
 *	The functions that inturn call the X11 library
 *	note: some functions do not exist because the interface
 *	uses the gcap version. This is true most often when  a CGM
 *	command sets defaults	
 */

/* 	Class 0	*/	

int	X11_BegMF(), X11_EndMF(), X11_BegPic(), X11_BegPicBody(), X11_EndPic(),
	X11_ClearDevice();

/*	Class 1	*/

int X11_MFDesc(), X11_MFElemList(), X11_MFDefaults(), 
    X11_CharSetList(); 

/*	Class 2	*/


/*	Class 3	*/

int X11_AuxColr();

/*	Class 4	*/	

int X11_PolyLine(), X11_DisjtLine(), X11_PolyMarker(), X11_RestrText(), 
    X11_ApndText(), X11_Polygon(), X11_PolygonSet(), X11_CellArray(), X11_GDP(),
    X11_Rect(), X11_Circle(), X11_Arc3Pt(), X11_Arc3PtClose(), X11_ArcCtr(), 
    X11_ArcCtrClose(), X11_Ellipse(), X11_EllipArc(), X11_EllipArcClose();

/*	Class 5	*/

/*	Class 6	*/

int X11_Escape();

/*	Class 7	*/

int X11_Message(), X11_ApplData();


#endif /* X11	*/





#ifdef	CTXT
/*
 *	The functions for the Clear Text driver
 */

/*	Class 0	*/

int	CTXT_NoOp(), CTXT_BegMF(), CTXT_EndMF(), CTXT_BegPic(), 
	CTXT_BegPicBody(), CTXT_EndPic(), CTXT_ClearDevice();

/*	Class 1	*/

int	CTXT_MFVersion(), CTXT_MFDesc(), CTXT_VDCType(),CTXT_IntergerPrec(),
	CTXT_RealPrec(), CTXT_IndexPrec(), CTXT_ColrPrec(), 
	CTXT_ColrIndexPrec(), CTXT_MaxColrIndex(), CTXT_ColrValueExt(), 
	CTXT_MFElemList(), CTXT_MFDefaults(), CTXT_FontList(), 
	CTXT_CharSetList(), CTXT_CharCoding();

/*	Class 2	*/

int	CTXT_ScaleMode(), CTXT_ColrMode(), CTXT_LineWidthMode(), 
	CTXT_MarkerSizeMode(), CTXT_EdgeWidthMode(), CTXT_VDCExt(), 
	CTXT_BackColr();


/*	Class 3	*/

int	CTXT_VDCIntergerPrec(), CTXT_VDCRealPrec(), CTXT_AuxColr(), 
	CTXT_Transparency(), CTXT_ClipRect(), CTXT_Clip();

/*	Class 4	*/

int	CTXT_PolyLine(), CTXT_DisjtLine(), CTXT_PolyMarker(), CTXT_Text(), 
	CTXT_RestrText(), CTXT_ApndText(), CTXT_Polygon(), 
	CTXT_PolygonSet(), CTXT_CellArray(), CTXT_GDP(), CTXT_Rect(), 
	CTXT_Circle(), CTXT_Arc3Pt(), CTXT_Arc3PtClose(), CTXT_ArcCtr(), 
	CTXT_ArcCtrClose(), CTXT_Ellipse(), CTXT_EllipArc(),
	CTXT_EllipArcClose();

/*	Class 5	*/

int	CTXT_LineIndex(), CTXT_LineType(), CTXT_LineWidth(),CTXT_LineColr(),
	CTXT_MarkerIndex(), CTXT_MarkerType(), CTXT_MarkerSize(), 
	CTXT_MarkerColr(), CTXT_TextIndex(), CTXT_TextFontIndex(),
	CTXT_TextPrec(), CTXT_CharExpan(), CTXT_CharSpace(),CTXT_TextColr(),
	CTXT_CharHeight(), CTXT_CharOri(), CTXT_TextPath(),CTXT_TextAlign(),
	CTXT_CharSetIndex(), CTXT_AltCharSetIndex(), CTXT_FillIndex(),
	CTXT_IntStyle(), CTXT_FillColr(), CTXT_HatchIndex(),CTXT_PatIndex(),
	CTXT_EdgeIndex(), CTXT_EdgeType(), CTXT_EdgeWidth(),CTXT_EdgeColr(),
	CTXT_EdgeVis(), CTXT_FillRefPt(), CTXT_PatTable(), CTXT_PatSize(),
	CTXT_ColrTable(), CTXT_ASF();

/*	Class 6	*/

int	CTXT_Escape();

/*	Class 7	*/

int	CTXT_Message(), CTXT_ApplData();

#endif	/* CTXT	*/

#ifdef	SunV
/*
 *
 *		The Sun SunView driver
 * 
 */
/*	class 1	*/
int	SunV_BegMF(), SunV_EndMF(), SunV_BegPic(), SunV_BegPicBody(), 
	SunV_EndPic(), SunV_ClearDevice();

/*	Class 4	*/


int	SunV_PolyLine(), SunV_PolyMarker(), SunV_Polygon(), SunV_CellArray();
    
   

/*	Class 5	*/

int	SunV_ColrTable(); 

#endif
#ifdef	SunR
/*
 *
 *		The Sun raster filter
 * 
 */
/*	class 1	*/
int	SunR_BegMF(), SunR_EndMF(), SunR_BegPic(), SunR_BegPicBody(), 
	SunR_EndPic(), SunR_ClearDevice();

/*	Class 4	*/


int	SunR_PolyLine(), SunR_PolyMarker(), SunR_Polygon(), SunR_CellArray();
    
   

/*	Class 5	*/

int	SunR_ColrTable(); 

#endif
/*ARGSUSED*/
int	noop(c)
CGMC *c;
{
	return(0);
}



/*
 * The Jumptable 
 */
int  (*cmdtab[][MAXCLASS+1][MAXFUNCPERCLASS+1])() = {
	{
	/* Class 0 */
	{
	noop, BegMF, EndMF, BegPic, BegPicBody, 
	EndPic, ClearDevice, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL,
	},
	/* Class 1 */
	{
	NULL, MFVersion, MFDesc, VDCType, IntergerPrec, 
	RealPrec, IndexPrec, ColrPrec, ColrIndexPrec, MaxColrIndex, 
	ColrValueExt, MFElemList, MFDefaults, FontList, CharSetList, 
	CharCoding, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 2 */
	{
	NULL, ScaleMode, ColrMode, LineWidthMode, MarkerSizeMode, 
	EdgeWidthMode, VDCExt, BackColr, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 3 */
	{
	NULL, VDCIntergerPrec, VDCRealPrec, AuxColr, Transparency, 
	ClipRect, Clip, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 4 */
	{
	NULL, PolyLine, DisjtLine, PolyMarker, Text, 
	RestrText, ApndText, Polygon, PolygonSet, CellArray, 
	GDP, Rect, Circle, Arc3Pt, Arc3PtClose, 
	ArcCtr, ArcCtrClose, Ellipse, EllipArc, EllipArcClose,
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 5 */
	{
	NULL, LineIndex, LineType, LineWidth, LineColr, 
	MarkerIndex, MarkerType, MarkerSize, MarkerColr, TextIndex, 
	TextFontIndex, TextPrec, CharExpan, CharSpace, TextColr, 
	CharHeight, CharOri, TextPath, TextAlign, CharSetIndex, 
	AltCharSetIndex, FillIndex, IntStyle, FillColr, HatchIndex,
	PatIndex, EdgeIndex, EdgeType, EdgeWidth, EdgeColr, 
	EdgeVis, FillRefPt, PatTable, PatSize, ColrTable, 
	ASF,
	},
	/* Class 6 */
	{
	NULL, Escape, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 7 */
	{
	NULL, Message, ApplData, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	}

	},


	/* The  functions */

	{
	/* Class 0 */
	{
	noop, Ras_BegMF, Ras_EndMF, Ras_BegPic, Ras_BegPicBody,
	Ras_EndPic, Ras_ClearDevice, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 1 */
	{
	NULL, MFVersion, MFDesc, VDCType, IntergerPrec,
	RealPrec, IndexPrec,ColrPrec,ColrIndexPrec,MaxColrIndex,
	ColrValueExt,MFElemList,MFDefaults,FontList,
	CharSetList, CharCoding, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 2 */
	{
	NULL, ScaleMode, ColrMode, LineWidthMode, MarkerSizeMode,
	EdgeWidthMode, VDCExt, BackColr, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 3 */
	{
	NULL, VDCIntergerPrec, VDCRealPrec, AuxColr, Transparency,
	ClipRect, Clip, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 4 */
	{
	NULL, PolyLine, DisjtLine, Ras_PolyMarker, Text,
	RestrText, ApndText, Polygon, PolygonSet, Ras_CellArray,
	GDP, Rect, Circle, Arc3Pt, Arc3PtClose,
	ArcCtr, ArcCtrClose, Ellipse, EllipArc, EllipArcClose,
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 5 */
	{
	NULL, LineIndex, LineType, LineWidth, LineColr,
	MarkerIndex,MarkerType,MarkerSize,MarkerColr,
	TextIndex, TextFontIndex, TextPrec, CharExpan, 
	CharSpace, TextColr, CharHeight, CharOri, 
	TextPath, TextAlign, CharSetIndex, AltCharSetIndex,
	FillIndex,IntStyle,FillColr,HatchIndex,
	PatIndex, EdgeIndex, EdgeType, EdgeWidth, EdgeColr,
	EdgeVis, FillRefPt, PatTable, PatSize, ColrTable,
	ASF,
	},
	/* Class 6 */
	{
	NULL, Escape, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 7 */
	{
	NULL, Message, ApplData, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	}

	},


#ifdef X11 

	/* The X11_ functions 
	 *
	 *	note the following functions do not exist because they 
	 * are handled by the coresponding graph cap version of the 
	 * function:
	 *
	 *	X11_VDCType(), X11_IntegerPrec(), X11_RealPrec(),
	 *	X11_IndexPrec(), X11_ColrPrec(), X11_ColrIndexPrec(),
	 *	X11_MaxColrIndex(), X11_ColrValueExt(), X11_CharCoding()
	 *
	 *		Class 2
	 *	X11_ScaleMode(), X11_ColrMode(), X11_LineWidthMode(),
	 *	X11_MarkerSizeMode(), X11_EdgeWidthMode(), X11_VDCExt()
	 *
	 *		Class 3
	 *	X11_VDCIntegerPrec(), X11_VDCRealPrec(), X11_Transpanrecy()
	 *	X11_ClipRect(), X11_Clip()
	 *
	 *		Class 4
	 *	X11_text()
	 *
	 *		Class 5
	 *	X11_LineIndex(), X11_LineType()	X11_LineWidth(), X11_LineColr()
	 *	X11_MarkerIndex(), X11_MarkerType() X11_MarkerSize()
	 *	X11_MarkerColour(), X11_TextIndex(), X11_TextFontIndex()
	 *	X11_TextPrec() X11_CharExpan(),	X11_CharSpace(), X11_TextColr() 
	 *	X11_CharHeight(), X11_CharOri() X11_TextPath() X11_TextAlign() 
	 *	X11_CharSetIndex(), X11_AltCharSetIndex(), X11_FillIndex()
	 *	X11_IntStyle() X11_Fill_colr() X11_HatchIndex() X11_PatIndex()
	 *	X11_EdgeIndex(), X11_EdgeType(), X11_EdgeWidth() X11_EdgeColr()
	 *	X11_EdgeVis() X11_FillRefPt(), X11_PatSize()
	 */

	{
	/* Class 0 */
	{
	noop, X11_BegMF, X11_EndMF, X11_BegPic, X11_BegPicBody,
	X11_EndPic, X11_ClearDevice, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 1 */
	{
	NULL, MFVersion, X11_MFDesc, VDCType, IntergerPrec,
	RealPrec, IndexPrec, ColrPrec, ColrIndexPrec, MaxColrIndex,
	ColrValueExt, X11_MFElemList, X11_MFDefaults, FontList,
	X11_CharSetList, CharCoding, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 2 */
	{
	NULL, ScaleMode, ColrMode, LineWidthMode, MarkerSizeMode,
	EdgeWidthMode, VDCExt, BackColr, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 3 */
	{
	NULL, VDCIntergerPrec, VDCRealPrec, X11_AuxColr, Transparency,
	ClipRect, Clip, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 4 */
	{
	NULL, X11_PolyLine, X11_DisjtLine, X11_PolyMarker, Text,
	X11_RestrText, X11_ApndText, X11_Polygon, X11_PolygonSet, X11_CellArray,
	X11_GDP, X11_Rect, X11_Circle, X11_Arc3Pt, X11_Arc3PtClose,
	X11_ArcCtr, X11_ArcCtrClose, X11_Ellipse, X11_EllipArc, X11_EllipArcClose,
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 5 */
	{
	NULL, LineIndex, LineType, LineWidth, LineColr,
	MarkerIndex,MarkerType,MarkerSize,MarkerColr,
	TextIndex, TextFontIndex, TextPrec, CharExpan, 
	CharSpace, TextColr, CharHeight, CharOri, 
	TextPath, TextAlign, CharSetIndex, AltCharSetIndex,
	FillIndex,IntStyle,FillColr,HatchIndex,
	PatIndex, EdgeIndex, EdgeType, EdgeWidth, EdgeColr,
	EdgeVis, FillRefPt, PatTable, PatSize, ColrTable,
	ASF,
	},
	/* Class 6 */
	{
	NULL, X11_Escape, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 7 */
	{
	NULL, X11_Message, X11_ApplData, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	}

	},

#endif /* X11	*/


#ifdef CTXT 

	/*
	 *	The Clear Text portion of the jump table
	 */


	{
	/* Class 0 */
	{
	CTXT_NoOp, CTXT_BegMF, CTXT_EndMF, CTXT_BegPic, CTXT_BegPicBody,
	CTXT_EndPic, CTXT_ClearDevice, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 1 */
	{
	NULL, CTXT_MFVersion, CTXT_MFDesc, CTXT_VDCType, CTXT_IntergerPrec,
	CTXT_RealPrec, CTXT_IndexPrec, CTXT_ColrPrec, CTXT_ColrIndexPrec, 
	CTXT_MaxColrIndex, CTXT_ColrValueExt, CTXT_MFElemList, 
	CTXT_MFDefaults, CTXT_FontList, CTXT_CharSetList, CTXT_CharCoding, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	},
	/* Class 2 */
	{
	NULL, CTXT_ScaleMode, CTXT_ColrMode, CTXT_LineWidthMode, 
	CTXT_MarkerSizeMode, CTXT_EdgeWidthMode, CTXT_VDCExt, 
	CTXT_BackColr, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	},
	/* Class 3 */
	{
	NULL, CTXT_VDCIntergerPrec, CTXT_VDCRealPrec, CTXT_AuxColr, 
	CTXT_Transparency, CTXT_ClipRect, CTXT_Clip, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL,
	},
	/* Class 4 */
	{
	NULL, CTXT_PolyLine, CTXT_DisjtLine, CTXT_PolyMarker, CTXT_Text,
	CTXT_RestrText, CTXT_ApndText, CTXT_Polygon, CTXT_PolygonSet, 
	CTXT_CellArray, CTXT_GDP, CTXT_Rect, CTXT_Circle, CTXT_Arc3Pt, 
	CTXT_Arc3PtClose, CTXT_ArcCtr, CTXT_ArcCtrClose, CTXT_Ellipse, 
	CTXT_EllipArc, CTXT_EllipArcClose, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	},
	/* Class 5 */
	{
	NULL, CTXT_LineIndex, CTXT_LineType, CTXT_LineWidth, CTXT_LineColr,
	CTXT_MarkerIndex,CTXT_MarkerType,CTXT_MarkerSize,CTXT_MarkerColr,
	CTXT_TextIndex, CTXT_TextFontIndex, CTXT_TextPrec, CTXT_CharExpan, 
	CTXT_CharSpace, CTXT_TextColr, CTXT_CharHeight, CTXT_CharOri, 
	CTXT_TextPath, CTXT_TextAlign, CTXT_CharSetIndex, 
	CTXT_AltCharSetIndex, CTXT_FillIndex,CTXT_IntStyle,CTXT_FillColr,
	CTXT_HatchIndex, CTXT_PatIndex, CTXT_EdgeIndex, CTXT_EdgeType, 
	CTXT_EdgeWidth, CTXT_EdgeColr, CTXT_EdgeVis, CTXT_FillRefPt, 
	CTXT_PatTable, CTXT_PatSize, CTXT_ColrTable, CTXT_ASF,
	},
	/* Class 6 */
	{
	NULL, CTXT_Escape, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL,
	},
	/* Class 7 */
	{
	NULL, CTXT_Message, CTXT_ApplData, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL,
	}

	},
#endif /* CTXT	*/

#ifdef	SunV

	{
	/* Class 0 */
	{
	noop, SunV_BegMF, SunV_EndMF, SunV_BegPic, SunV_BegPicBody,
	SunV_EndPic, SunV_ClearDevice, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 1 */
	{
	NULL, MFVersion, MFDesc, VDCType, IntergerPrec,
	RealPrec, IndexPrec, ColrPrec, ColrIndexPrec, MaxColrIndex,
	ColrValueExt, MFElemList, MFDefaults, FontList,
	CharSetList, CharCoding, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 2 */
	{
	NULL, ScaleMode, ColrMode, LineWidthMode, MarkerSizeMode,
	EdgeWidthMode, VDCExt, BackColr, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 3 */
	{
	NULL, VDCIntergerPrec, VDCRealPrec, AuxColr, Transparency,
	ClipRect, Clip, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 4 */
	{
	NULL, SunV_PolyLine, DisjtLine, SunV_PolyMarker, Text,
	RestrText, ApndText, SunV_Polygon, PolygonSet, SunV_CellArray,
	GDP, Rect, Circle, Arc3Pt, Arc3PtClose,
	ArcCtr, ArcCtrClose, Ellipse, EllipArc, EllipArcClose,
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 5 */
	{
	NULL, LineIndex, LineType, LineWidth, LineColr,
	MarkerIndex,MarkerType,MarkerSize,MarkerColr,
	TextIndex, TextFontIndex, TextPrec, CharExpan, 
	CharSpace, TextColr, CharHeight, CharOri, 
	TextPath, TextAlign, CharSetIndex, AltCharSetIndex,
	FillIndex,IntStyle,FillColr,HatchIndex,
	PatIndex, EdgeIndex, EdgeType, EdgeWidth, EdgeColr,
	EdgeVis, FillRefPt, PatTable, PatSize, SunV_ColrTable,
	ASF,
	},
	/* Class 6 */
	{
	NULL, Escape, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 7 */
	{
	NULL, Message, ApplData, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	}

	},
#endif
#ifdef	SunR

	{
	/* Class 0 */
	{
	noop, SunR_BegMF, SunR_EndMF, SunR_BegPic, SunR_BegPicBody,
	SunR_EndPic, SunR_ClearDevice, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 1 */
	{
	NULL, MFVersion, MFDesc, VDCType, IntergerPrec,
	RealPrec, IndexPrec, ColrPrec, ColrIndexPrec, MaxColrIndex,
	ColrValueExt, MFElemList, MFDefaults, FontList,
	CharSetList, CharCoding, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 2 */
	{
	NULL, ScaleMode, ColrMode, LineWidthMode, MarkerSizeMode,
	EdgeWidthMode, VDCExt, BackColr, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 3 */
	{
	NULL, VDCIntergerPrec, VDCRealPrec, AuxColr, Transparency,
	ClipRect, Clip, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 4 */
	{
	NULL, SunR_PolyLine, DisjtLine, SunR_PolyMarker, Text,
	RestrText, ApndText, SunR_Polygon, PolygonSet, SunR_CellArray,
	GDP, Rect, Circle, Arc3Pt, Arc3PtClose,
	ArcCtr, ArcCtrClose, Ellipse, EllipArc, EllipArcClose,
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 5 */
	{
	NULL, LineIndex, LineType, LineWidth, LineColr,
	MarkerIndex,MarkerType,MarkerSize,MarkerColr,
	TextIndex, TextFontIndex, TextPrec, CharExpan, 
	CharSpace, TextColr, CharHeight, CharOri, 
	TextPath, TextAlign, CharSetIndex, AltCharSetIndex,
	FillIndex,IntStyle,FillColr,HatchIndex,
	PatIndex, EdgeIndex, EdgeType, EdgeWidth, EdgeColr,
	EdgeVis, FillRefPt, PatTable, PatSize, SunR_ColrTable,
	ASF,
	},
	/* Class 6 */
	{
	NULL, Escape, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	},
	/* Class 7 */
	{
	NULL, Message, ApplData, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, 
	NULL,
	}

	},
#endif

};
