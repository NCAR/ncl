/*
 *      $Id: TransObjI.h,v 1.3 1995-04-07 09:36:09 boote Exp $
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

/*
 * Private Global functions defined by the Transform Class
 */

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
