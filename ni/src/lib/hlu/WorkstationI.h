/*
 *      $Id: WorkstationI.h,v 1.3 1994-12-16 20:05:07 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		WorkstationI.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jan 26 17:39:56 MST 1994
 *
 *	Description:	Used to declare "Private" access functions to the
 *			Workstation Class.
 */
#ifndef	_NWORKSTATIONI_H
#define	_NWORKSTATIONI_H

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/Workstation.h>

/*
 * Private Functions to support Workstation Class Objects
 */
extern void _NhlSetLineInfo(
#if	NhlNeedProto
        NhlLayer   /* instance */,
        NhlLayer   /* plot */
#endif
);

extern NhlErrorTypes _NhlWorkstationLineTo(
#if	NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

extern void _NhlSetFillInfo(
#if	NhlNeedProto
NhlLayer instance,
NhlLayer plot
#endif
);

extern NhlErrorTypes _NhlWorkstationFill(
#if	NhlNeedProto
NhlLayer   /* instance */,
float*   /* x */,
float*   /* y */,
int     /* num_points */
#endif
);

extern void _NhlSetMarkerInfo(
#if	NhlNeedProto
NhlLayer instance,
NhlLayer plot
#endif
);

extern NhlErrorTypes _NhlWorkstationMarker(
#if	NhlNeedProto
NhlLayer   /* instance */,
float*   /* x */,
float*   /* y */,
int     /* num_points */
#endif
);

extern NhlErrorTypes _NhlActivateWorkstation(
#if	NhlNeedProto
        NhlLayer   /* layer*/
#endif
);

extern NhlErrorTypes _NhlDeactivateWorkstation(
#if	NhlNeedProto
        NhlLayer   /* layer*/
#endif
);

extern NhlErrorTypes _NhlCloseWorkstation(
#if	NhlNeedProto
        NhlLayer   /*layer*/
#endif
);

extern NhlErrorTypes _NhlOpenWorkstation(
#if	NhlNeedProto
        NhlLayer   /*layer*/
#endif
);

extern  int  _NhlWorkstationId(
#if	NhlNeedProto
        NhlLayer   /*instance */
#endif
);

extern int _NhlGetGksCi(
#if	NhlNeedProto
        NhlLayer   /* workstation*/,
        int /* ci*/
#endif
);

extern int _NhlIsAllocatedColor(
#if	NhlNeedProto
        NhlLayer   /* workstation*/,
        int /* ci*/
#endif
);

extern NhlErrorTypes _NhlSetColor(
#if	NhlNeedProto
NhlLayer   /* inst */,
int     /* ci */,
float   /* red */,
float   /* green */,
float   /* blue */
#endif
);

extern NhlErrorTypes _NhlFreeColor(
#if	NhlNeedProto
        NhlLayer   /* inst */,
        int     /* ci */
#endif
);

extern int _NhlNewColor(
#if	NhlNeedProto
        NhlLayer   /* inst */,
        float   /* red */,
        float   /* green */,
        float   /* blue */
#endif
);

#endif	/* _NWORKSTATIONI_H */
