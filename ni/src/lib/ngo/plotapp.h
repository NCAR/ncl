/*
 *      $Id: plotapp.h,v 1.1 1999-08-11 23:41:56 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotapp.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 15 17:03:14 MST 1999
 *
 *	Description:	
 */
#ifndef	_NG_PLOTAPP_H
#define	_NG_PLOTAPP_H

#include <ncarg/ngo/ngo.h>
#include <ncarg/ngo/dataprofile.h>
#include <ncarg/ngo/graphic.h>

#define NhlNndvFuncDirs		"ndvFuncDirs"
#define NhlNndvFuncFiles	"ndvFuncFiles"
#define NhlNndvFunctions	"ndvFunctions"
#define NhlNndvExecFunction	"ndvExecFunction"
#define NhlNndvObjects		"ndvObjects"
#define NhlNndvData		"ndvData"
#define NhlNpmOverlays		"pmOverlays"
#define NhlNmpDataLimitObject	"mpDataLimitObject"
#define NhlNndvUpdateFunc	"ndvUpdateFunc"



extern int NgNewPlotAppRef
(
	int		go_id,
	NhlString	plot_style,
	NhlString	plot_style_dir,
	NhlString	plot_style_name,
	NhlString	plot_style_class_name,
	NhlBoolean	preview
);

extern void NgDeletePlotAppRef
(
	NrmQuark	qplotstyle
);

extern NgDataProfile NgNewPlotAppDataProfile
( 
	int		go_id,
	NrmQuark	qplotstyle
);

extern NhlErrorTypes NgSetPlotAppDataVars
(
	int		go_id,
	NrmQuark	qplotstyle,
	NhlString	plotname,
	NgDataProfile	dprof,
	NrmQuark	*qfiles,	/* NULL-terminated list of files */
	NrmQuark	*qvars,		/* NULL-terminated list of vars */
	int		vcount,
	NgVarData	*vdata,
	NrmQuark	qgraphics	/* graphic obj array */
);

extern NhlErrorTypes NgUpdatePlotAppDataProfile
(
	int		go_id,
	NrmQuark	qplotstyle,
	NhlString	plotname,
	NgDataProfile	dprof
);


extern NhlErrorTypes NgHandleSyntheticResource
(
	int		go_id,
	NrmQuark	qplotstyle,
	NrmQuark	qplot,
	NgDataProfile	dprof,
	int		obj_ix,
	int		item_ix,
	NgResData	resdata,
	int		*res_count,
	NhlBoolean	preview
);

NhlBoolean NgPlotAppDataUsable
(
	int		go_id,
	NrmQuark	qplotstyle,
	NgVarData	vdata
);

#endif	/* _NG_PLOTAPP_H */
