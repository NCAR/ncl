/*
 *      $Id: Transform.h,v 1.5 1994-12-16 20:04:55 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Transform.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 16:36:47 MDT 1992
 *
 *	Description:	Public header for Transform class.
 */

#ifndef _NTransform_h
#define _NTransform_h

#include <ncarg/hlu/View.h>

/*
 * Public Transform instance resources
 */

#define NhlNtfOverlayOn	"tfOverlayOn"

/*
 * Public Transform class resources
 */

#define NhlCtfOverlayOn	"TfOverlayOn"

/*
 * Public Functions defined by the Transform Class
 */
extern NhlErrorTypes NhlNDCToData(
#if	NhlNeedProto
	int	/*pid*/,
	float* /*x*/,
	float* /*y*/,
	int	/*n*/,
	float* /*xout*/,
	float* /*yout*/,
	float * /* xmissing */,
	float * /* ymissing */,
	int*	/* status */,
	float*  /* out_of_range */
#endif
);

extern NhlErrorTypes NhlDataToNDC(
#if	NhlNeedProto
	int	/*pid*/,
	float* /*x*/,
	float* /*y*/,
	int	/*n*/,
	float* /*xout*/,
	float* /*yout*/,
	float * /* xmissing */,
	float * /* ymissing */,
	int*	/* status */,
	float*  /* out_of_range */
#endif
);

extern NhlErrorTypes NhlDataPolyline(
#if	NhlNeedProto
	int		/* pid */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */
#endif
);

extern NhlErrorTypes NhlNDCPolyline(
#if	NhlNeedProto
	int		/* pid */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */
#endif
);

extern NhlLayerClass NhltransformLayerClass;

#endif /*_NTransform_h */
