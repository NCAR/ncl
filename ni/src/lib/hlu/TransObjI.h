/*
 *      $Id: TransObjI.h,v 1.9 2004-10-05 22:50:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TransObjI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jan 26 18:35:48 MST 1994
 *
 *	Description:	
 */
#ifndef	_NTransObjI_h
#define	_NTransObjI_h

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/TransObj.h>

/* 
 * private get-only resource that allows an object to determine
 * if the trans obj has changed 
 */

#define NhlNtrChangeCount	".trChangeCount"
#define NhlCtrChangeCount	".TrChangeCount"

/*
 * private resources for communicating plot data limits
 */

#define NhlNtrDataXStartF	".trDataXStartF"
#define NhlNtrDataYStartF	".trDataYStartF"
#define NhlNtrDataXEndF		".trDataXEndF"
#define NhlNtrDataYEndF		".trDataYEndF"
#define NhlCtrDataXStartF	".TrDataXStartF"
#define NhlCtrDataYStartF	".TrDataYStartF"
#define NhlCtrDataXEndF		".TrDataXEndF"
#define NhlCtrDataYEndF		".TrDataYEndF"

/*
 * Cell Bounds Information
 * trXCIsBounds and trYCIsBounds specify whether the provided coordinates
 * are cell boundaries (if not they are cell centers).
 * trDoBounds is set True if the drawing method used is capable of
 * handling cell boundaries. If not the TransObj class is expected to
 * substitute a coordinate set interpolated to cell centers.
 */

#define NhlNtrXCIsBounds         ".trXCIsBounds"
#define NhlNtrYCIsBounds         ".trYCIsBounds"
#define NhlNtrDoBounds           ".trDoBounds"

#define NhlCtrXCIsBounds         ".TrXCIsBounds"
#define NhlCtrYCIsBounds         ".TrYCIsBounds"
#define NhlCtrDoBounds           ".TrDoBounds"


/*
 * Private Global functions defined by the Transform Class
 */

extern NhlErrorTypes _NhlDataLineTo(
#if	NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

extern NhlErrorTypes _NhlCompcLineTo(
#if	NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

extern NhlErrorTypes _NhlWinLineTo(
#if	NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

extern NhlErrorTypes _NhlNDCLineTo(
#if	NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

extern NhlErrorTypes _NhlDataPolygon(
#if	NhlNeedProto
NhlLayer	instance,
float		*x,
float		*y,
int		n
#endif
);

extern NhlErrorTypes _NhlDataToWin(
#if	NhlNeedProto
	NhlLayer /* instance */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans */,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);

extern NhlErrorTypes _NhlWinToData(
#if	NhlNeedProto
	NhlLayer /* instance */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);

extern NhlErrorTypes _NhlWinToNDC(
#if	NhlNeedProto
	NhlLayer /* instance */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
extern NhlErrorTypes _NhlNDCToWin(
#if	NhlNeedProto
	NhlLayer /* instance */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
extern NhlErrorTypes _NhlDataToCompc(
#if	NhlNeedProto
	NhlLayer /* instance */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
extern NhlErrorTypes _NhlCompcToData(
#if	NhlNeedProto
	NhlLayer /* instance */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
extern NhlErrorTypes _NhlCompcToWin(
#if	NhlNeedProto
	NhlLayer /* instance */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);

extern NhlErrorTypes _NhlWinToCompc(
#if	NhlNeedProto
	NhlLayer /* instance */,
	float* /* x */,
	float* /* y */,
	int 	/* n */,
	float* /* xout */,
	float* /* yout */,
	int * /*istrans*/,
	float * /*xmissing */,
	float * /*ymissing */
#endif
);
extern NhlErrorTypes   _NhlSetTrans(
#if	NhlNeedProto
NhlLayer /* instance*/,
NhlLayer  parent
#endif
);

#endif	/* _NTransObjI_h */
