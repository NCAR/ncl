/*
 *      $Id: SphericalGeometry.c,v 1.2 2002-11-07 00:33:19 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  2002			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		SphericalGeometry.c
 *
 *	Author:		David Kennison
 *                      (adapted for HLU library by David I. Brown)
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 12 12:35:56 MDT 2002
 *
 *	Description:	
 *
 *
 */

#include <ncarg/c.h>
#include <sys/file.h>
#include <fcntl.h>
#include <math.h>

#if defined (cray)
#include <fcntl.h>
#include <sys/types.h>
#include <fortran.h>
#endif

#include <float.h>
#include <ncarg/hlu/SphericalGeometryP.h>

double abgcdp
/*
 * (ABGCDP = Angle Between Great Circles, Double Precision)
 *
 * Use from FORTRAN in a statement like this:
 *
 *   RSLT=ABGCDP(AQDP,BQDP,CQDP)
 *
 * This function, given information about the points A, B, and C on the
 * sphere, returns the angle, in degrees, from the great circle through
 * A and B to the great circle through A and C, positive if the point C
 * is in the hemisphere to the "left" of the great circle through A and
 * B, negative otherwise.
 *
 * All variables with names of the form XQDP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  double *aqdp,
  double *bqdp,
  double *cqdp
  )
#else
  (aqdp,bqdp)
  double *aqdp;
  double *bqdp;
  double *cqdp;
#endif
{
  double rddp=57.2957795130823;  /* radians to degrees */

  double xcb2,ycb2     ;
  double           zcb3;
  double xcc2,ycc2     ;
  double           zcc3;
  double      ycc4,zcc4;
  double dnom,cang,sang;

  xcb2=bqdp[0]*bqdp[2]*aqdp[2]+bqdp[0]*bqdp[3]*aqdp[3];
  ycb2=bqdp[0]*bqdp[3]*aqdp[2]-bqdp[0]*bqdp[2]*aqdp[3];

  zcb3=bqdp[1]*aqdp[0]-xcb2*aqdp[1];

  xcc2=cqdp[0]*cqdp[2]*aqdp[2]+cqdp[0]*cqdp[3]*aqdp[3];
  ycc2=cqdp[0]*cqdp[3]*aqdp[2]-cqdp[0]*cqdp[2]*aqdp[3];

  zcc3=cqdp[1]*aqdp[0]-xcc2*aqdp[1];

  if (ycb2!=0.||zcb3!=0.) {
    dnom=sqrt(ycb2*ycb2+zcb3*zcb3);
    cang=ycb2/dnom;
    sang=zcb3/dnom;
  } else {
    cang=1.;
    sang=0.;
  }

  ycc4=ycc2*cang+zcc3*sang;
  zcc4=zcc3*cang-ycc2*sang;

  return (ycc4!=0.||zcc4!=0.)?rddp*atan2(zcc4,ycc4):0.;
}


double acegdp
/*
 * (ACEGDP = Angular Change along Edge of Grid, Double Precision)
 *
 * Use from FORTRAN in code like this:
 *
 *   DOUBLE PRECISION PQDP(4),QQDP(4,IDIM,JDIM)
 *   .
 *   .
 *   .
 *
 *   RSLT=ACEGDP(PQDP,QQDP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
 *
 * The value of this function is the total angle swept out by a vector
 * tangent to the sphere at the point P and pointing in the direction of
 * the shortest great circle route to a point tracing Q, the outer edge
 * of a "grid" defined by the points (QQDP(I,J)), for I from IBEG to IEND
 * and J from JBEG to JEND.  (The edge of the grid is formed of shortest
 * great circle routes from point to point.)
 *
 * The outer edge of the grid divides the surface of the sphere into two
 * areas - one to the left, and one to the right, of Q.  Let P' denote
 * the point opposite P on the sphere.  Note that P' = (-PLAT,PLON+180).
 * In theory (ignoring computational inaccuracies), the function can only
 * have three possible values: +360, if P is to the left of Q and P' is
 * to the right of Q; -360, if P is to the right of Q and P' is to the
 * left of Q; and zero, if both P and P' are on the same side of Q (left
 * or right, but we don't know which).
 *
 * This function is used to tell us whether the point P is "inside" Q
 * (function value greater than 180) or "outside" Q (function value less
 * than 180).
 *
 * All variables with names of the form XQDP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  double *pqdp,
  double *qqdp,
  int    idim,
  int    jdim,
  int    ibeg,
  int    iend,
  int    jbeg,
  int    jend
  )
#else
  (pqdp,qqdp,idim,jdim,ibeg,iend,jbeg,jend)
  double *pqdp;
  double *qqdp;
  int    idim;
  int    jdim;
  int    ibeg;
  int    iend;
  int    jbeg;
  int    jend;
#endif
{
  double anch;
  double *qqd1;
  double *qqd2;
  int i,j;

  anch=0.;

  qqd2=qqdp+4*(ibeg+idim*jbeg);

  for (i=ibeg;i<iend;i++) {
    qqd1=qqd2 ; qqd2=qqd2+4;
    anch+=abgcdp(pqdp,qqd1,qqd2);
  }

  for (j=jbeg;j<jend;j++) {
    qqd1=qqd2 ; qqd2=qqd2+4*idim ;
    anch+=abgcdp(pqdp,qqd1,qqd2);
  }

  for (i=ibeg;i<iend;i++) {
    qqd1=qqd2 ; qqd2=qqd2-4;
    anch+=abgcdp(pqdp,qqd1,qqd2);
  }

  for (j=jbeg;j<jend;j++) {
    qqd1=qqd2 ; qqd2=qqd2-4*idim ;
    anch+=abgcdp(pqdp,qqd1,qqd2);
  }

  return anch;
}


double adgcdp
/*
 * (ADGCDP = Angle in Degrees along Great Circle, Double Precision)
 *
 * Use from FORTRAN in a statement like this:
 *
 *   RSLT=ADGCDP(AQDP,BQDP)
 *
 * This function returns the shortest great circle distance, in degrees,
 * between two points, A and B, on the surface of the globe.
 *
 * All variables with names of the form XQDP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 *
 */
#ifdef NeedFuncProto
  (
  double *aqdp,
  double *bqdp
  )
#else
  (aqdp,bqdp)
  double *aqdp;
  double *bqdp;
#endif
{
  double rdtt=114.5915590261646;  /* radians to degrees, times two */

  return rdtt*asin(sqrt((aqdp[0]*aqdp[2]-bqdp[0]*bqdp[2])*
                        (aqdp[0]*aqdp[2]-bqdp[0]*bqdp[2])+
                        (aqdp[0]*aqdp[3]-bqdp[0]*bqdp[3])*
                        (aqdp[0]*aqdp[3]-bqdp[0]*bqdp[3])+
                        (aqdp[1]        -bqdp[1]        )*
                        (aqdp[1]        -bqdp[1]        ))/2.);
}


double dpgcdp
/*
 * (DPGCDP = Distance of Point from Great Circle, Double Precision)
 *
 * Use from FORTRAN in a statement like this:
 *
 *   RSLT=DPGCDP(AQDP,BQDP,CQDP)
 *
 * This function, given points A, B, and C on the globe, returns the
 * directed distance, in degrees of arc, from the great circle through
 * A and B to the point C, positive if the point C is in the hemisphere
 * to the "left" of the great circle and negative otherwise.
 *
 * All variables with names of the form XQDP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  double *aqdp,
  double *bqdp,
  double *cqdp
  )
#else
  (aqdp,bqdp)
  double *aqdp;
  double *bqdp;
  double *cqdp;
#endif
{
  double rddp=57.2957795130823;  /* radians to degrees */

  double xcb2,ycb2     ;
  double           zcb3;
  double xcc2,ycc2     ;
  double           zcc3;
  double dnom;

  xcb2=bqdp[0]*bqdp[2]*aqdp[2]+bqdp[0]*bqdp[3]*aqdp[3];
  ycb2=bqdp[0]*bqdp[3]*aqdp[2]-bqdp[0]*bqdp[2]*aqdp[3];

  zcb3=bqdp[1]*aqdp[0]-xcb2*aqdp[1];

  xcc2=cqdp[0]*cqdp[2]*aqdp[2]+cqdp[0]*cqdp[3]*aqdp[3];
  ycc2=cqdp[0]*cqdp[3]*aqdp[2]-cqdp[0]*cqdp[2]*aqdp[3];

  zcc3=cqdp[1]*aqdp[0]-xcc2*aqdp[1];

  if (ycb2!=0.||zcb3!=0.) {
    dnom=sqrt(ycb2*ycb2+zcb3*zcb3);
    return rddp*asin((zcc3*ycb2-ycc2*zcb3)/dnom);
  } else {
    return adgcdp(aqdp,cqdp);
  }
}


void fpiqdp
/*
 * (FPIQDP = Find Point In Quadrilateral, Double Precision)
 *
 * Call from FORTRAN using a statement like this:
 *
 *   CALL FPIQDP (AQDP,BQDP,CQDP,DQDP,EQDP,XFRA,YFRA)
 *       DOUBLE PRECISION AQDP(4),BQDP(4),CQDP(4),DQDP(4),EQDP(4)
 *       DOUBLE PRECISION XFRA,YFRA
 *
 * This routine, given four points (A, B, C, and D) that form a
 * "quadrilateral" on the surface of the globe, and a surface point
 * (E) in its interior, returns the two interpolation fractions that
 * one would use in a call to IPIQSP to get the point E.  See comments
 * in IPIQSP; here's a duplicate of the diagram from that routine:
 *
 *                              C------Q----D
 *                              |      |    |
 *                              |      E    |
 *                              |      |    |
 *                              |      |    |
 *                              A------P----B
 *
 * P and Q are positioned such that AP/AB = CQ/CD = XFRA and then E is
 * positioned such that PE/PQ = YFRA (where "XY" is interpreted to mean
 * "the shortest great circle distance from X to Y").
 *
 * All variables with names of the form XQDP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  double *aqdp,
  double *bqdp,
  double *cqdp,
  double *dqdp,
  double *eqdp,
  double *xfra,
  double *yfra
  )
#else
  (aqdp,bqdp,cqdp,dqdp,eqdp,xfra,yfra)
  double *aqdp;
  double *bqdp;
  double *cqdp;
  double *dqdp;
  double *eqdp;
  double *xfra;
  double *yfra;
#endif
{
  double *pqdp,*qqdp,xfr1,xfr2,dst1,dst2,dsta,tmp1,tmp2,pppp[4],qqqq[4];

  pqdp=pppp;
  qqdp=qqqq;

  xfr1=0.;
  dst1=dpgcdp(aqdp,cqdp,eqdp);

  if (dst1>=0.) {

    *xfra=0.;
    pqdp=aqdp;
    qqdp=cqdp;

  } else {

    xfr2=1.;
    dst2=dpgcdp(bqdp,dqdp,eqdp);

    if (dst2<=0.) {

      *xfra=1.;
      pqdp=bqdp;
      qqdp=dqdp;

    } else {

      do {

        *xfra=(dst2*xfr1-dst1*xfr2)/(dst2-dst1);
        ipgcdp(aqdp,bqdp,xfra,pqdp);
        ipgcdp(cqdp,dqdp,xfra,qqdp);
	if (*xfra - xfr1 < DBL_EPSILON ||
	    *xfra - xfr2 > -DBL_EPSILON) 
		break;
        dsta=dpgcdp(pqdp,qqdp,eqdp);
        if (dsta==0.) break;
        if (dst1*dsta>0.) {
          xfr1=*xfra;
          dst1=dsta;
        } else {
          xfr2=*xfra;
          dst2=dsta;
        }

      } while (xfr2-xfr1>1.E-12);

    }

  }

  tmp1=adgcdp(pqdp,eqdp);
  tmp2=adgcdp(pqdp,qqdp);
  *yfra=(tmp2!=0.)?tmp1/tmp2:0.;

}


void ipgcdp
/*
 * (IPGCDP = Interpolate Point on Great Circle, Double Precision)
 *
 * Call from FORTRAN using a statement like this:
 *
 *   CALL IPGCDP (AQDP,BQDP,FRAC,CQDP)
 *
 * Given two points A and B on the surface of the globe and a fraction
 * FRAC, between 0. and 1., this routine interpolates a point C along the
 * shortest great circle route joining A to B such that the distance from
 * A to C, divided by the distance from A to B, is equal to FRAC.
 *
 * All variables with names of the form XQDP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves were used.
 *
 * Code that is commented out either produces unneeded results or is
 * superseded by code that executes faster.
 */
#ifdef NeedFuncProto
  (
  double *aqdp,
  double *bqdp,
  double *frac,
  double *cqdp
  )
#else
  (aqdp,bqdp,frac,cqdp)
  double *aqdp;
  double *bqdp;
  double *frac;
  double *cqdp;
#endif
{
  double xcb2,ycb2     ;
  double xcb3     ,zcb3;
  double      ycb4;
  double xcc2,ycc2,zcc2;
  double xcc3,     zcc3;
  double xcc4,ycc4     ;
  double dnom,calp,salp,beta;

  xcb2=bqdp[0]*bqdp[2]*aqdp[2]+bqdp[0]*bqdp[3]*aqdp[3];
  ycb2=bqdp[0]*bqdp[3]*aqdp[2]-bqdp[0]*bqdp[2]*aqdp[3];

  xcb3=xcb2*aqdp[0]+bqdp[1]*aqdp[1];
  zcb3=bqdp[1]*aqdp[0]-xcb2*aqdp[1];

  if (ycb2!=0.||zcb3!=0.) {
    dnom=sqrt(ycb2*ycb2+zcb3*zcb3);
    calp=ycb2/dnom;
    salp=zcb3/dnom;
  } else {
    calp=1.;
    salp=0.;
  }

  ycb4=ycb2*calp+zcb3*salp;

  beta=(xcb3!=0.||ycb4!=0.)?atan2(ycb4,xcb3):0.;

  xcc2=cos(*frac*beta);
  ycc2=sin(*frac*beta)*calp;
  zcc2=sin(*frac*beta)*salp;

  xcc3=xcc2*aqdp[0]-zcc2*aqdp[1];
  zcc3=zcc2*aqdp[0]+xcc2*aqdp[1];

  xcc4=xcc3*aqdp[2]-ycc2*aqdp[3];
  ycc4=ycc2*aqdp[2]+xcc3*aqdp[3];

  cqdp[0]=sqrt(xcc4*xcc4+ycc4*ycc4);
  cqdp[1]=zcc3;
  cqdp[2]=xcc4/cqdp[0];
  cqdp[3]=ycc4/cqdp[0];
}


void ipiqdp
/*
 * (IPIQDP = Interpolate Point In Quadrilateral, Double Precision)
 *
 * Call from FORTRAN using a statement like this:
 *
 *   CALL IPIQDP (AQDP,BQDP,CQDP,DQDP,XFRA,YFRA,EQDP)
 *
 * This routine, given four points on the sphere (A, B, C, and D) forming
 * a "quadrilateral" and two interpolation fractions (XFRA and YFRA, each
 * between 0 and 1, inclusive), finds a point E defined by the following
 * diagram and returns it.
 *
 *                              C------Q----D
 *                              |      |    |
 *                              |      E    |
 *                              |      |    |
 *                              |      |    |
 *                              A------P----B
 *
 * P and Q are positioned such that AP/AB = CQ/CD = XFRA and then E is
 * positioned such that PE/PQ = YFRA (where "XY" is interpreted to mean
 * "the shortest great circle distance from X to Y").
 *
 * It is assumed that the "quadrilateral" ABDC is "convex" (a working
 * definition of which might be that none of the four great circles
 * defined by its edges - the ones through A and B, B and D, D and C,
 * and C and A - cross it anywhere.  However, this is not verified.
 *
 * All variables with names of the form XQDP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  double *aqdp,
  double *bqdp,
  double *cqdp,
  double *dqdp,
  double *xfra,
  double *yfra,
  double *eqdp
  )
#else
  (aqdp,bqdp,frac,cqdp)
  double *aqdp;
  double *bqdp;
  double *cqdp;
  double *dqdp;
  double *xfra;
  double *yfra;
  double *eqdp;
#endif
{
  double pqdp[4],qqdp[4];

  ipgcdp(aqdp,bqdp,xfra,pqdp);
  ipgcdp(cqdp,dqdp,xfra,qqdp);
  ipgcdp(pqdp,qqdp,yfra,eqdp);
}


float abgcsp
/*
 * (ABGCSP = Angle Between Great Circles, Single Precision)
 *
 * Use from FORTRAN in a statement like this:
 *
 *   RSLT=ABGCSP(AQSP,BQSP,CQSP)
 *
 * This function, given information about the points A, B, and C on the
 * sphere, returns the angle, in degrees, from the great circle through
 * A and B to the great circle through A and C, positive if the point C
 * is in the hemisphere to the "left" of the great circle through A and
 * B, negative otherwise.
 *
 * All variables with names of the form XQSP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  float *aqsp,
  float *bqsp,
  float *cqsp
  )
#else
  (aqsp,bqsp)
  float *aqsp;
  float *bqsp;
  float *cqsp;
#endif
{
  float rdsp=57.2957795130823f;  /* radians to degrees */

  float xcb2,ycb2     ;
  float           zcb3;
  float xcc2,ycc2     ;
  float           zcc3;
  float      ycc4,zcc4;
  float dnom,cang,sang;

  xcb2=bqsp[0]*bqsp[2]*aqsp[2]+bqsp[0]*bqsp[3]*aqsp[3];
  ycb2=bqsp[0]*bqsp[3]*aqsp[2]-bqsp[0]*bqsp[2]*aqsp[3];

  zcb3=bqsp[1]*aqsp[0]-xcb2*aqsp[1];

  xcc2=cqsp[0]*cqsp[2]*aqsp[2]+cqsp[0]*cqsp[3]*aqsp[3];
  ycc2=cqsp[0]*cqsp[3]*aqsp[2]-cqsp[0]*cqsp[2]*aqsp[3];

  zcc3=cqsp[1]*aqsp[0]-xcc2*aqsp[1];

  if (ycb2!=0.f||zcb3!=0.f) {
    dnom=(float)sqrt((double)(ycb2*ycb2+zcb3*zcb3));
    cang=ycb2/dnom;
    sang=zcb3/dnom;
  } else {
    cang=1.f;
    sang=0.f;
  }

  ycc4=ycc2*cang+zcc3*sang;
  zcc4=zcc3*cang-ycc2*sang;

  return (ycc4!=0.f||zcc4!=0.f)?rdsp*(float)atan2((double)zcc4,(double)ycc4):0.f;
}


float acegsp
/*
 * (ACEGSP = Angular Change along Edge of Grid, Double Precision)
 *
 * Use from FORTRAN in code like this:
 *
 *   DOUBLE PRECISION PQSP(4),QQSP(4,IDIM,JDIM)
 *   .
 *   .
 *   .
 *
 *   RSLT=ACEGSP(PQSP,QQSP,IDIM,JDIM,IBEG,IEND,JBEG,JEND)
 *
 * The value of this function is the total angle swept out by a vector
 * tangent to the sphere at the point P and pointing in the direction of
 * the shortest great circle route to a point tracing Q, the outer edge
 * of a "grid" defined by the points (QQSP(I,J)), for I from IBEG to IEND
 * and J from JBEG to JEND.  (The edge of the grid is formed of shortest
 * great circle routes from point to point.)
 *
 * The outer edge of the grid divides the surface of the sphere into two
 * areas - one to the left, and one to the right, of Q.  Let P' denote
 * the point opposite P on the sphere.  Note that P' = (-PLAT,PLON+180).
 * In theory (ignoring computational inaccuracies), the function can only
 * have three possible values: +360, if P is to the left of Q and P' is
 * to the right of Q; -360, if P is to the right of Q and P' is to the
 * left of Q; and zero, if both P and P' are on the same side of Q (left
 * or right, but we don't know which).
 *
 * This function is used to tell us whether the point P is "inside" Q
 * (function value greater than 180) or "outside" Q (function value less
 * than 180).
 *
 * All variables with names of the form XQSP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  float *pqsp,
  float *qqsp,
  int   idim,
  int   jdim,
  int   ibeg,
  int   iend,
  int   jbeg,
  int   jend
  )
#else
  (pqsp,qqsp,idim,jdim,ibeg,iend,jbeg,jend)
  float *pqsp;
  float *qqsp;
  int   idim;
  int   jdim;
  int   ibeg;
  int   iend;
  int   jbeg;
  int   jend;
#endif
{
  float anch;
  float *qqd1;
  float *qqd2;
  int i,j;

  anch=0.f;

  qqd2=qqsp+4*(ibeg+idim*jbeg);

  for (i=ibeg;i<iend;i++) {
    qqd1=qqd2 ; qqd2=qqd2+4;
    anch+=abgcsp(pqsp,qqd1,qqd2);
  }

  for (j=jbeg;j<jend;j++) {
    qqd1=qqd2 ; qqd2=qqd2+4*idim ;
    anch+=abgcsp(pqsp,qqd1,qqd2);
  }

  for (i=ibeg;i<iend;i++) {
    qqd1=qqd2 ; qqd2=qqd2-4;
    anch+=abgcsp(pqsp,qqd1,qqd2);
  }

  for (j=jbeg;j<jend;j++) {
    qqd1=qqd2 ; qqd2=qqd2-4*idim ;
    anch+=abgcsp(pqsp,qqd1,qqd2);
  }

  return anch;
}


float adgcsp
/*
 * (ADGCSP = Angle in Degrees along Great Circle, Double Precision)
 *
 * Use from FORTRAN in a statement like this:
 *
 *   RSLT=ADGCSP(AQSP,BQSP)
 *
 * This function returns the shortest great circle distance, in degrees,
 * between two points, A and B, on the surface of the globe.
 *
 * All variables with names of the form XQSP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 *
 */
#ifdef NeedFuncProto
  (
  float *aqsp,
  float *bqsp
  )
#else
  (aqsp,bqsp)
  float *aqsp;
  float *bqsp;
#endif
{
  float rdtt=114.5915590261646f;  /* radians to degrees, times two */

  return rdtt*(float)asin(sqrt((double)
                               ((aqsp[0]*aqsp[2]-bqsp[0]*bqsp[2])*
                                (aqsp[0]*aqsp[2]-bqsp[0]*bqsp[2])+
                                (aqsp[0]*aqsp[3]-bqsp[0]*bqsp[3])*
                                (aqsp[0]*aqsp[3]-bqsp[0]*bqsp[3])+
                                (aqsp[1]        -bqsp[1]        )*
                                (aqsp[1]        -bqsp[1]        )))/2.f);
}


float dpgcsp
/*
 * (DPGCSP = Distance of Point from Great Circle, Double Precision)
 *
 * Use from FORTRAN in a statement like this:
 *
 *   RSLT=DPGCSP(AQSP,BQSP,CQSP)
 *
 * This function, given points A, B, and C on the globe, returns the
 * directed distance, in degrees of arc, from the great circle through
 * A and B to the point C, positive if the point C is in the hemisphere
 * to the "left" of the great circle and negative otherwise.
 *
 * All variables with names of the form XQSP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  float *aqsp,
  float *bqsp,
  float *cqsp
  )
#else
  (aqsp,bqsp)
  float *aqsp;
  float *bqsp;
  float *cqsp;
#endif
{
  float rdsp=57.2957795130823f;  /* radians to degrees */

  float xcb2,ycb2     ;
  float           zcb3;
  float xcc2,ycc2     ;
  float           zcc3;
  float dnom;

  xcb2=bqsp[0]*bqsp[2]*aqsp[2]+bqsp[0]*bqsp[3]*aqsp[3];
  ycb2=bqsp[0]*bqsp[3]*aqsp[2]-bqsp[0]*bqsp[2]*aqsp[3];

  zcb3=bqsp[1]*aqsp[0]-xcb2*aqsp[1];

  xcc2=cqsp[0]*cqsp[2]*aqsp[2]+cqsp[0]*cqsp[3]*aqsp[3];
  ycc2=cqsp[0]*cqsp[3]*aqsp[2]-cqsp[0]*cqsp[2]*aqsp[3];

  zcc3=cqsp[1]*aqsp[0]-xcc2*aqsp[1];

  if (ycb2!=0.f||zcb3!=0.f) {
    dnom=(float)sqrt((double)(ycb2*ycb2+zcb3*zcb3));
    return rdsp*(float)asin((double)((zcc3*ycb2-ycc2*zcb3)/dnom));
  } else {
    return adgcsp(aqsp,cqsp);
  }
}


void fpiqsp
/*
 * (FPIQSP = Find Point In Quadrilateral, Double Precision)
 *
 * Call from FORTRAN using a statement like this:
 *
 *   CALL FPIQSP (AQSP,BQSP,CQSP,DQSP,EQSP,XFRA,YFRA)
 *       DOUBLE PRECISION AQSP(4),BQSP(4),CQSP(4),DQSP(4),EQSP(4)
 *       DOUBLE PRECISION XFRA,YFRA
 *
 * This routine, given four points (A, B, C, and D) that form a
 * "quadrilateral" on the surface of the globe, and a surface point
 * (E) in its interior, returns the two interpolation fractions that
 * one would use in a call to IPIQSP to get the point E.  See comments
 * in IPIQSP; here's a duplicate of the diagram from that routine:
 *
 *                              C------Q----D
 *                              |      |    |
 *                              |      E    |
 *                              |      |    |
 *                              |      |    |
 *                              A------P----B
 *
 * P and Q are positioned such that AP/AB = CQ/CD = XFRA and then E is
 * positioned such that PE/PQ = YFRA (where "XY" is interpreted to mean
 * "the shortest great circle distance from X to Y").
 *
 * All variables with names of the form XQSP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  float *aqsp,
  float *bqsp,
  float *cqsp,
  float *dqsp,
  float *eqsp,
  float *xfra,
  float *yfra
  )
#else
  (aqsp,bqsp,cqsp,dqsp,eqsp,xfra,yfra)
  float *aqsp;
  float *bqsp;
  float *cqsp;
  float *dqsp;
  float *eqsp;
  float *xfra;
  float *yfra;
#endif
{
  float *pqsp,*qqsp,xfr1,xfr2,dst1,dst2,dsta,tmp1,tmp2,pppp[4],qqqq[4];

  pqsp=pppp;
  qqsp=qqqq;

  xfr1=0.f;
  dst1=dpgcsp(aqsp,cqsp,eqsp);

  if (dst1>=0.f) {

    *xfra=0.f;
    pqsp=aqsp;
    qqsp=cqsp;

  } else {

    xfr2=1.f;
    dst2=dpgcsp(bqsp,dqsp,eqsp);

    if (dst2<=0.f) {

      *xfra=1.f;
      pqsp=bqsp;
      qqsp=dqsp;

    } else {

      do {

        *xfra=(dst2*xfr1-dst1*xfr2)/(dst2-dst1);
        ipgcsp(aqsp,bqsp,xfra,pqsp);
        ipgcsp(cqsp,dqsp,xfra,qqsp);
	if (*xfra - xfr1 < FLT_EPSILON ||
	    *xfra - xfr2 > -FLT_EPSILON) 
		break;
        dsta=dpgcsp(pqsp,qqsp,eqsp);
        if (dsta==0.f) break;
        if (dst1*dsta>0.f) {
          xfr1=*xfra;
          dst1=dsta;
        } else {
          xfr2=*xfra;
          dst2=dsta;
        }

      } while (xfr2-xfr1>1.E-6f);

    }

  }

  tmp1=adgcsp(pqsp,eqsp);
  tmp2=adgcsp(pqsp,qqsp);
  *yfra=(tmp2!=0.f)?tmp1/tmp2:0.f;

}


void ipgcsp
/*
 * (IPGCSP = Interpolate Point on Great Circle, Double Precision)
 *
 * Call from FORTRAN using a statement like this:
 *
 *   CALL IPGCSP (AQSP,BQSP,FRAC,CQSP)
 *
 * Given two points A and B on the surface of the globe and a fraction
 * FRAC, between 0. and 1., this routine interpolates a point C along the
 * shortest great circle route joining A to B such that the distance from
 * A to C, divided by the distance from A to B, is equal to FRAC.
 *
 * All variables with names of the form XQSP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves were used.
 *
 * Code that is commented out either produces unneeded results or is
 * superseded by code that executes faster.
 */
#ifdef NeedFuncProto
  (
  float *aqsp,
  float *bqsp,
  float *frac,
  float *cqsp
  )
#else
  (aqsp,bqsp,frac,cqsp)
  float *aqsp;
  float *bqsp;
  float *frac;
  float *cqsp;
#endif
{
  float xcb2,ycb2     ;
  float xcb3     ,zcb3;
  float      ycb4;
  float xcc2,ycc2,zcc2;
  float xcc3,     zcc3;
  float xcc4,ycc4     ;
  float dnom,calp,salp,beta;

  xcb2=bqsp[0]*bqsp[2]*aqsp[2]+bqsp[0]*bqsp[3]*aqsp[3];
  ycb2=bqsp[0]*bqsp[3]*aqsp[2]-bqsp[0]*bqsp[2]*aqsp[3];

  xcb3=xcb2*aqsp[0]+bqsp[1]*aqsp[1];
  zcb3=bqsp[1]*aqsp[0]-xcb2*aqsp[1];

  if (ycb2!=0.f||zcb3!=0.f) {
    dnom=(float)sqrt((double)(ycb2*ycb2+zcb3*zcb3));
    calp=ycb2/dnom;
    salp=zcb3/dnom;
  } else {
    calp=1.f;
    salp=0.f;
  }

  ycb4=ycb2*calp+zcb3*salp;

  beta=(xcb3!=0.f||ycb4!=0.f)?(float)atan2((double)ycb4,(double)xcb3):0.f;

  xcc2=(float)cos((double)(*frac*beta));
  ycc2=(float)sin((double)(*frac*beta))*calp;
  zcc2=(float)sin((double)(*frac*beta))*salp;

  xcc3=xcc2*aqsp[0]-zcc2*aqsp[1];
  zcc3=zcc2*aqsp[0]+xcc2*aqsp[1];

  xcc4=xcc3*aqsp[2]-ycc2*aqsp[3];
  ycc4=ycc2*aqsp[2]+xcc3*aqsp[3];

  cqsp[0]=(float)sqrt((double)(xcc4*xcc4+ycc4*ycc4));
  cqsp[1]=zcc3;
  cqsp[2]=xcc4/cqsp[0];
  cqsp[3]=ycc4/cqsp[0];
}


void ipiqsp
/*
 * (IPIQSP = Interpolate Point In Quadrilateral, Double Precision)
 *
 * Call from FORTRAN using a statement like this:
 *
 *   CALL IPIQSP (AQSP,BQSP,CQSP,DQSP,XFRA,YFRA,EQSP)
 *
 * This routine, given four points on the sphere (A, B, C, and D) forming
 * a "quadrilateral" and two interpolation fractions (XFRA and YFRA, each
 * between 0 and 1, inclusive), finds a point E defined by the following
 * diagram and returns it.
 *
 *                              C------Q----D
 *                              |      |    |
 *                              |      E    |
 *                              |      |    |
 *                              |      |    |
 *                              A------P----B
 *
 * P and Q are positioned such that AP/AB = CQ/CD = XFRA and then E is
 * positioned such that PE/PQ = YFRA (where "XY" is interpreted to mean
 * "the shortest great circle distance from X to Y").
 *
 * It is assumed that the "quadrilateral" ABDC is "convex" (a working
 * definition of which might be that none of the four great circles
 * defined by its edges - the ones through A and B, B and D, D and C,
 * and C and A - cross it anywhere.  However, this is not verified.
 *
 * All variables with names of the form XQSP are four-element arrays
 * containing the cosine and sine of the latitude and the cosine and
 * sine of the longitude, in that order, of the point X.  Describing
 * the point positions in this way makes this routine execute faster
 * than if the latitudes and longitudes themselves are used.
 */
#ifdef NeedFuncProto
  (
  float *aqsp,
  float *bqsp,
  float *cqsp,
  float *dqsp,
  float *xfra,
  float *yfra,
  float *eqsp
  )
#else
  (aqsp,bqsp,frac,cqsp)
  float *aqsp;
  float *bqsp;
  float *cqsp;
  float *dqsp;
  float *xfra;
  float *yfra;
  float *eqsp;
#endif
{
  float pqsp[4],qqsp[4];

  ipgcsp(aqsp,bqsp,xfra,pqsp);
  ipgcsp(cqsp,dqsp,xfra,qqsp);
  ipgcsp(pqsp,qqsp,yfra,eqsp);
}
