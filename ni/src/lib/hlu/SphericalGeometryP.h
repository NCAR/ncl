/*
 *      $Id: SphericalGeometryP.h,v 1.2 2003-02-27 18:26:51 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  2002			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		SphericalGeometryP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 12 12:35:56 MDT 2002
 *
 *	Description:	Private header file for spherical geometry 
 *			routines.
 */
#ifndef _NSphericalGeometryP_h
#define _NSphericalGeometryP_h

float  abgcsp(float*,float*,float*);
float  adgcsp(float*,float*);
float  acegsp(float*,float*,int,int,int,int,int,int);
float  dpgcsp(float*,float*,float*);
void   fpiqsp(float*,float*,float*,float*,float*,float*,float*);
void   ipgcsp(float*,float*,float*,float*);
void   ipiqsp(float*,float*,float*,float*,float*,float*,float*);
int    icegsp(float*,float*,int,int,int,int,int,int);

double abgcdp(double*,double*,double*);
double adgcdp(double*,double*);
double acegdp(double*,double*,int,int,int,int,int,int);
double dpgcdp(double*,double*,double*);
void   fpiqdp(double*,double*,double*,double*,double*,double*,double*);
void   ipgcdp(double*,double*,double*,double*);
void   ipiqdp(double*,double*,double*,double*,double*,double*,double*);
int    icegdp(double*,double*,int,int,int,int,int,int);

#endif /* _NSphericalGeometryP_h */
