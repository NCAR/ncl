C
C       $Id: curvs2dp.f,v 1.2 2003-08-11 22:44:00 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
C NOTE: If you make any changes to this software, please remember to
C make the same changes to the corresponding single precision routine.
C
      SUBROUTINE CURVS2DP(T,N,PARAM,X,Y,XP,YP,SIGMA,XO,YO)
      DOUBLE PRECISION XO
      DOUBLE PRECISION YO
      DOUBLE PRECISION CURV2DP
C
C  This subroutine is a companion to CURV1SDP which must be
C  called before calling this subroutine.  CURV2SDP calculates
C  an interpolated value for a smoothing spline under tension
C  at a given point.  CURVS1DP must be called before CURVS2DP to
C  determine certain necessary parameters.
C
C  On input:
C
C    T
C      A real value to be mapped onto the interpolating curve.
C      Values of T between zero and one interpolate the original
C      data; any values of T outside this range result in
C      extrapolation.
C
C    N
C      The number of points which were specified to
C      determine the curve.
C
C    PARAM
C      The arc lengths computed by CURVS1DP.
C
C    X
C*PL*ERROR* Comment line too long
C      An array containing the X-coordinate values of the specified points.
C
C    Y
C*PL*ERROR* Comment line too long
C      An array containing the Y-coordinate values of the specified points.
C
C    XP
C      An array of second derivative values as calculated by CURVS1DP.
C
C    YP
C      An array of second derivative values as calculated by CURVS1DP.
C
C    SIGMA
C      The tension factor (its sign is ignored).
C
C  On output:
C
C    XO
C      The interpolated X value.
C
C    YO
C      The interpolated Y value.
C
C  The parameters N, X, Y, XP, YP, and SIGMA should be input
C  unaltered from the output of CURVS1DP.
C
      DOUBLE PRECISION T,PARAM(N),X(N),Y(N),XP(N),YP(N),SIGMA
C
      XO = CURV2DP(T,N,PARAM,X,XP,SIGMA)
      YO = CURV2DP(T,N,PARAM,Y,YP,SIGMA)
C
      RETURN
      END
