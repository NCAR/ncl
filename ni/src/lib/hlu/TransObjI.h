/*
 *      $Id: TransObjI.h,v 1.1 1994-01-27 21:26:51 boote Exp $
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
#ifdef NhlNeedProto
NhlLayer   /* instance */,
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

extern NhlErrorTypes _NhlCompcLineTo(
#ifdef NhlNeedProto
NhlLayer   /* instance */,
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

extern NhlErrorTypes _NhlWinLineTo(
#ifdef NhlNeedProto
NhlLayer   /* instance */,
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

extern NhlErrorTypes _NhlNDCLineTo(
#ifdef NhlNeedProto
NhlLayer   /* instance */,
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

/*
 * Private Global functions defined by the Transform Class
 */

extern NhlErrorTypes _NhlDataToWin(
#ifdef NhlNeedProto
	NhlLayer /* instance */,
	NhlLayer /* parent */,
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
#ifdef NhlNeedProto
	NhlLayer /* instance */,
	NhlLayer /* parent */,
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
#ifdef NhlNeedProto
	NhlLayer /* instance */,
	NhlLayer /* parent */,
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
#ifdef NhlNeedProto
	NhlLayer /* instance */,
	NhlLayer /* parent */,
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
#ifdef NhlNeedProto
	NhlLayer /* instance */,
	NhlLayer /* parent */,
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
#ifdef NhlNeedProto
	NhlLayer /* instance */,
	NhlLayer /* parent */,
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
#ifdef NhlNeedProto
	NhlLayer /* instance */,
	NhlLayer /* parent */,
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
#ifdef NhlNeedProto
	NhlLayer /* instance */,
	NhlLayer /* parent */,
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
#ifdef NhlNeedProto
NhlLayer /* instance*/,
NhlLayer  parent
#endif
);

#endif	/* _NTransObjI_h */
