/*
 *      $Id: graphic.h,v 1.6 1999-07-30 03:20:53 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		graphic.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 10 18:38:01 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_GRAPHIC_H
#define	_NG_GRAPHIC_H

#include <ncarg/ngo/app.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ngo/dataprofile.h>



/*
 * Attributes for multiple element plots
 */
#define ndvOBJECTLIST "ndvObjectList"
#define ndvCLASS      "ndvClass"
#define ndvWKS	      "ndvWks"

#define NgRESDATA_ALLOC_UNIT 16

typedef struct _NgResDataRec {
	int  res_count;
	int  res_alloced;
	char **res;
	NhlPointer *values;
	NrmQuark *types;
	NgVarData *vdata;
} NgResDataRec, *NgResData;

extern NgResData NgReallocResData
(
	NgResData resdata,
	int	  rescount
);

extern void NgFreeResData
(
	NgResData resdata
);

typedef struct _NgHluDataRec {
	int		page_id;
	NrmQuark	q_sym;
	NhlPointer	ddata;        /* data profile */
	NhlPointer	gdata;	      /* graphic info */
	NrmQuark	qplotstyle;
	NhlBoolean	preview;
	int		go_id;
	NgCBWP		destroy_cb;
	NhlBoolean	draw_req;
} NgHluDataRec, *NgHluData;

extern NgHluData NgGetHluData
(
	void
);

void NgFreeHluData
(
	NgHluData hlu_data
);
	

typedef int (*NgSetResProc)
(
        int nclstate,
        NhlPointer res_proc_data,
        int block_id
        );

typedef void (*NgPreviewResProc)
(
        int srlist_id,
        NhlPointer res_proc_data
        );

extern
NhlErrorTypes NgCreatePreviewGraphic
(
	int		goid,
        int		*hlu_id,
	NhlString	hlu_name,
	NhlString	ncl_graphic,
	NhlString	ncl_parent,
	NhlString	classname,
        int		pres_proc_count,
        NgPreviewResProc *pres_procs,
        NhlPointer	*pres_proc_data
        );

extern
NhlErrorTypes NgDestroyPreviewGraphic
(
	int		goid,
        int		hlu_id
        );

extern
NhlErrorTypes NgCreateGraphic
(
	int		goid,
        int		*hlu_id,
	NhlString	hlu_name,
	NhlString	ncl_graphic,
	NhlString	ncl_parent,
	NhlString	classname,
        int		res_proc_count,
        NgSetResProc	*res_procs,
        NhlPointer	*res_proc_data
        );

extern
NhlErrorTypes NgUpdateGraphic
(
	int		goid,
	NhlString	ncl_graphic,
        int		res_proc_count,
        NgSetResProc	*res_procs,
        NhlPointer	*res_proc_data
        );

extern
NhlErrorTypes NgDestroyGraphic
(
	int		goid,
	NhlString	ncl_graphic
        );

extern
NhlErrorTypes NgDrawGraphic
(
	int		goid,
	NhlString	ncl_graphic,
        NhlBoolean	clear
        );

/*
 * a wrapper for NgDrawGraphic that takes a view id instead of a string ref
 */

extern
NhlErrorTypes NgDrawView
(
	int		goid,
	int		view_id,
        NhlBoolean	clear
        );

NhlBoolean NgViewOn(
	int view_id
);

extern int NgAddResList
(
        int		nclstate,
        NhlPointer	data,
        int		block_id
        );

extern void NgPreviewResList
(
        int		setrl_id,
        NhlPointer	data
        );

#endif	/* _NG_GRAPHIC_H */








