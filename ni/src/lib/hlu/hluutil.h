/*
 *      $Id: hluutil.h,v 1.4 1995-01-28 01:56:19 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hluutil.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 15 16:44:03 MDT 1992
 *
 *	Description:	Header file for util library.
 */

#include <ncarg/c.h>
#include <ncarg/gks.h>

#ifndef	NhlNeedProto
#ifdef	__STDC__
#define Const const
#define NhlNeedProto 1
#else
#define Const
#define NhlNeedProto 0
#endif
#endif	/* NhlNeedProto */

#ifndef	_NHLCALLF
#define	_NHLCALLF(reg,cap)	NGCALLF(reg,cap)
#endif	/* _NHLCALLF	*/

typedef struct compar_dat {
	int is_zero;
	int exp;
	float b_final;
	int sig_dig;
	float orig_val;
	float lg_abs;
}NhlCompareDat;


extern int wksisact(
#if NhlNeedProto
int	/*n*/
#endif
);
extern int wksisopn(
#if NhlNeedProto 
int     /*n*/ 
#endif 
); 

extern int searchb(
#if NhlNeedProto
float	* /*a*/,
int	  /*n */,
float	  /*value*/
#endif
);

extern void linaprox(
#if NhlNeedProto
int 	/* n */,
float * /* x */,
float * /* y */,
float * /* c */
#endif
);

extern void evallinaprox(
#if NhlNeedProto
int	/* n */,
float * /* x */,
float * /* y */,
float * /* c */,
float   /* xval */,
float * /* yval */
#endif
);

extern void strans(
#if	NhlNeedProto
	float	froml,
	float	fromr,
	float	fromb,
	float	fromt,
	float	tol,
	float	tor,
	float	tob,
	float	tot,
	float	x,
	float	y,
	float	*xout,
	float	*yout
#endif
);

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

