/*
 *      $Id: cmpf.h,v 1.1 2009-05-15 00:49:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		cmpf.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 15 16:44:03 MDT 1992
 *
 *	Description:	Header file for util library.
 */

#ifndef _cmpf_h
#define _cmpf_h

#include "niohlu.h"

#define	_NhlMIN_NONZERO 1e-32

typedef struct compar_dat {
	int is_zero;
	int exp;
	float b_final;
	int sig_dig;
	float orig_val;
	float lg_abs;
}NhlCompareDat;


extern NhlCompareDat *_NhlCmpFSetup(
#if	NhlNeedProto
float /*val*/,
int /*sig_dig*/
#endif
);

extern float _NhlCmpF(
#if	NhlNeedProto
float /* a */,
NhlCompareDat * /*b*/
#endif
);

extern double _NhlCmpDAny(
#if	NhlNeedProto
double /* a */,
double /*b*/,
int   sig_dig	
#endif
);

extern float _NhlCmpFAny(
#if	NhlNeedProto
float /* a */,
float /*b*/,
int   sig_dig	
#endif
);

extern float _NhlRndIt(
#if  NhlNeedProto
float /* a */,
int /*sig_digit*/
#endif
);

extern float	_NhlCmpFAny2(
#if	NhlNeedProto
	float a, 
	float b, 
	int sig_dig,
	float min_nonzero
#endif
);

extern double	_NhlCmpDAny2(
#if	NhlNeedProto
	double a, 
	double b, 
	int sig_dig,
	double min_nonzero
#endif
);

#endif /* _cmpf_h */
