/*
 *      $Id: appP.h,v 1.7 1999-05-27 02:28:32 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		appP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Aug 28 17:41:23 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_APPP_H_
#define	_NG_APPP_H_

#include <ncarg/hlu/BaseP.h>

#include <ncarg/ngo/ngoP.h>
#include <ncarg/ngo/app.h>

#define _NgDEFAULT_COLORMAP_PATH ".:$NCARG_ROOT/lib/ncarg/colormaps"

typedef struct _NgAppMgrClassRec *NgAppMgrClass;
typedef struct _NgAppMgrRec *NgAppMgr;

typedef struct _NgWorkProcRec _NgWorkProcRec, *_NgWorkProc;
struct _NgWorkProcRec{
	NgWorkProc	proc;
	NhlPointer	cdata;
	_NgWorkProc	next;
};

#define	_NgGOLISTSIZE	(10)
typedef struct _NgAppGOListRec _NgAppGOListRec, *_NgAppGOList;
struct _NgAppGOListRec{
	int		go[_NgGOLISTSIZE];
	unsigned int	kind[_NgGOLISTSIZE];
	int		num;
	_NgAppGOList	next;
};

typedef struct _NgAppFStackRec _NgAppFStackRec, *_NgAppFStack;
struct _NgAppFStackRec{
	int		active;
	_NgAppFStack	next;
};

typedef struct _NgAppMgrPart {
/* required fields */
	NhlString		app_name;
	NhlString		app_class;
/* export fields */
	int			nclstate;
	NgWksState		wks_state;

/* private fields */
	_NhlCBList		gochangecb;
	_NgWorkProc		wp;
	_NgAppGOList		go;
	_NgAppFStack		active;
        _NhlCB          	delete_wks_cb;
        int			selected_work_id;
} NgAppMgrPart;

typedef struct _NgAppMgrRec {
	NhlObjLayerPart	base;
	NgAppMgrPart	app;
} NgAppMgrRec;

typedef void (*NgRunProc)(
	NgAppMgr	app
);

typedef NhlBoolean (*NgWorkProcHandler)(
	NgAppMgr	app
);

typedef void (*NgDevWorkProc)(
	NgAppMgr		app,
	NgWorkProcHandler	wp
);

typedef struct _NgAppMgrClassPart {
	int		num_mgrs;
	NgRunProc	run_proc;
	NgDevWorkProc	dev_wproc;
} NgAppMgrClassPart;

typedef struct _NgAppMgrClassRec {
	NhlObjClassPart		base_class;
	NgAppMgrClassPart	app_class;
} NgAppMgrClassRec;

extern NgAppMgrClassRec	NgappMgrClassRec;

#endif	/* _NG_APPP_H_ */
