/*
 *      $Id: CoordApprox.h,v 1.5 2002-03-18 21:20:06 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Oct 8 15:07:57 MDT 1992
 *
 *	Description:	
 */

#ifndef _NCoordApprox_h
#define  _NCoordApprox_h

#include <ncarg/hlu/hluP.h>
typedef enum _NhlOrdering{
	NhlNONMONOTONIC,
	NhlINCREASING,
	NhlDECREASING
} NhlOrdering;

typedef enum _NhlStatus{
	NhlNONE,
	NhlFORWARD,
	NhlINVERSE,
	NhlBOTHTRANS
} NhlStatus;

#define NhlBOGUS 0

typedef struct _NhlCoordDat {
	NhlStatus	xstatus;
	int	x_use_log;
	float	*x_orig_forward;
	float	*fx_orig_forward;
	float   *x_coefs_forward;
	float	*x_orig_inverse;
	float	*fx_orig_inverse;
	float   *x_coefs_inverse;
	int	nx_forward;
	int	nx_inverse;
	float	xsigma;
	float	x_min;
	float	x_max;
	float	x_int_min;
	float	x_int_max;
	NhlStatus	ystatus;
	int	y_use_log;
	float	*y_orig_forward;
	float	*fy_orig_forward;
	float   *y_coefs_forward;
	float	*y_orig_inverse;
	float	*fy_orig_inverse;
	float   *y_coefs_inverse;
	int	ny_forward;
	int	ny_inverse;
	float	ysigma;
	float	y_min;
	float	y_max;
	float	y_int_min;
	float	y_int_max;
} NhlCoordDat;

extern NhlErrorTypes _NhlCreateSplineCoordApprox(
#if	NhlNeedProto
NhlCoordDat * /*thedat */,
int	/* x_use_log */,
float	* /*x */,
float	* /*x_int*/,
int	/* nx */,
int	/* y_use_log */,
float	* /* y */,
float	* /* y_int */,
int	/* ny */,
float	/* xsigma */,
float	/* ysigma */,
int	/* xsample */,
int	/* ysample */,
NhlStatus	*/* xstatus */,
NhlStatus	*/* ystatus */
#endif
);

extern NhlErrorTypes _NhlDestroySplineCoordApprox(
#if	NhlNeedProto
NhlCoordDat */* thedat */
#endif
);

extern NhlErrorTypes _NhlEvalSplineCoordForward(
#if	NhlNeedProto
NhlCoordDat * 	/*thedat */,
float		/* x */,
float		/* y */,
float*		/* xout */,
float*		/* yout */,
float*		/* xmissing */,
float*		/* ymissing */
#endif
);

extern NhlErrorTypes _NhlEvalSplineCoordInverse(
#if	NhlNeedProto
NhlCoordDat * 	/*thedat */,
float		/* x */,
float		/* y */,
float*		/* xout */,
float*		/* yout */,
float*		/* xmissing */,
float*		/* ymissing */
#endif
);

extern NhlErrorTypes _NhlMultiEvalSplineCoordForward(
#if	NhlNeedProto
        NhlCoordDat     * /*thedat*/, 
        float           * /*x*/, 
        float           * /*y*/, 
        float           * /*xout*/, 
        float           * /*yout*/, 
        int             /*xnpts*/ ,
        int             /*ynpts*/ ,
float*		/* xmissing */,
float*		/* ymissing */
#endif
);

extern NhlErrorTypes _NhlMultiEvalSplineCoordInverse(
#if	NhlNeedProto
        NhlCoordDat     * /*thedat*/, 
        float           * /*x*/, 
        float           * /*y*/, 
        float           * /*xout*/, 
        float           * /*yout*/, 
        int             /*xnpts*/ ,
        int             /*ynpts*/ ,
float*		/* xmissing */,
float*		/* ymissing */
#endif
);

#endif  /*_NCoordApprox_h*/ 
