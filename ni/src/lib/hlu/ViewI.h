/*
 *      $Id: ViewI.h,v 1.8 2003-04-04 18:34:08 dbrown Exp $
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
#include <ncarg/hlu/Segments.h>
#include <ncarg/hlu/View.h>

/*
* Globally callable functions from Segments.c
*/
extern void _NhlDestroySegTransDat(
#if	NhlNeedProto
NhlTransDat*    /* transdat */
#endif
);

extern NhlTransDat      *_NhlInitSegTransDat(
#if	NhlNeedProto
float*, /* x */
float*  /* y */
#endif
);

extern void _NhlResetSegTransDat(
#if	NhlNeedProto
NhlTransDat*,   /* transdat */
float*,         /* x */
float*          /* y */
#endif
);
extern void _NhlComputeSegTrans(
#if	NhlNeedProto
NhlTransDat*,   /* transdat */
float   *,      /* transform */
float   *,      /* xprime */
float   *       /* yprime */
#endif
);

extern NhlBoolean _NhlSegmentSpansArea(
#if	NhlNeedProto
NhlTransDat* transdat,
float xmin,
float xmax,
float ymin,
float ymax
#endif
);

extern NhlErrorTypes _NhlDrawSegment(
#if	NhlNeedProto
NhlTransDat*,   /* transdat */
int             /* wksid */
#endif
);

extern void _NhlEvalTrans(
#if     NhlNeedProto
float *,        /*transform */
float,          /* x */
float,          /* y */
float *,        /* xprime */
float *         /* yprime */
#endif
);

extern NhlErrorTypes _NhlStartSegment(
#if	NhlNeedProto
NhlTransDat*    /* transdat */
#endif
);

extern void _NhlSetSegTrans(
#if	NhlNeedProto
NhlTransDat*,   /* transdat */
float*          /* transform */
#endif
);

extern void _NhlEndSegment(
#if	NhlNeedProto
NhlTransDat*    /* transdat */
#endif
);


typedef struct _NhlViewClassRec *NhlViewClass;
typedef struct _NhlViewLayerRec *NhlViewLayer;

typedef struct _NhlAnnoStatusCBDataRec 
	_NhlAnnoStatusCBDataRec, *_NhlAnnoStatusCBData;

struct _NhlAnnoStatusCBDataRec {
	int		id;	/* layer id */
        int		base_id; /* id of base plot */
        int		anno_manager_id; /* NhlNULLOBJID on remove */
	NhlBoolean	isanno;	/* True on add; False on remove */
};

#define	_NhlCBvpAnnoStatus	"CBvpAnnoStatus"	/* cbdata.ptrval is
                                                        _NhlAnnoStatusCBData */
#endif	/* _NVIEWI_H */
