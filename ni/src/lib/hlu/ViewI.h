/*
 *      $Id: ViewI.h,v 1.1 1994-01-27 21:27:13 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ViewI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jan 26 17:52:02 MST 1994
 *
 *	Description:	Private global function declarations for View.
 */
#ifndef	_NVIEWI_H
#define	_NVIEWI_H

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/View.h>

/*
* Globally callable functions from Segments.c
*/
extern void _NhlDestroySegTransDat(
#ifdef NhlNeedProto
NhlTransDat*    /* transdat */
#endif
);

extern NhlTransDat      *_NhlInitSegTransDat(
#ifdef NhlNeedProto
float*, /* x */
float*  /* y */
#endif
);

extern void _NhlResetSegTransDat(
#ifdef NhlNeedProto
NhlTransDat*,   /* transdat */
float*,         /* x */
float*          /* y */
#endif
);
extern void _NhlComputeSegTrans(
#ifdef NhlNeedProto
NhlTransDat*,   /* transdat */
float   *,      /* transform */
float   *,      /* xprime */
float   *       /* yprime */
#endif
);

extern NhlErrorTypes _NhlDrawSegment(
#ifdef NhlNeedProto
NhlTransDat*,   /* transdat */
int             /* wksid */
#endif
);
extern void _NhlEvalTrans(
#ifdef NhlNeedProto
float *,        /*transform */
float,          /* x */
float,          /* y */
float *,        /* xprime */
float *         /* yprime */
#endif
);

extern void _NhlStartSegment(
#ifdef NhlNeedProto
NhlTransDat*    /* transdat */
#endif
);

extern void _NhlSetSegTrans(
#ifdef NhlNeedProto
NhlTransDat*,   /* transdat */
float*          /* transform */
#endif
);

extern void _NhlEndSegment();

typedef struct _NhlViewLayerClassRec *NhlViewLayerClass;
typedef struct _NhlViewLayerRec *NhlViewLayer;

#endif	/* _NVIEWI_H */
