C
C       $Id: curvs1dp.f,v 1.3 2008-07-27 03:10:11 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C NOTE: If you make any changes to this software, please remember to
C make the same changes to the corresponding single precision routine.
C
      SUBROUTINE CURVS1DP(N,X,Y,D,ISW,S,EPS,PARAM,XS,YS,XSP,YSP,SIGMA,
     +                    TEMP,IERR)
      DOUBLE PRECISION TSM
      DOUBLE PRECISION SM
C
C  This subroutine determines the parameters necessary to
C  compute a smoothing spline under tension from data values
C  in the plane.  In general the curve will not pass through
C  the original data points.  For actual computation of points
C  on the C  curve it is necessary to call the subroutine CURVS2DP.
C
C  On input:
C
C    N
C      The number of points to be interpolated (N .GE. 2).
C
C    X
C      An array containing N X-coordinates of the input data.
C
C    Y
C      An array containing N Y-coordinates of the
C      input data. (adjacent X-Y pairs must be distinct, i.e.
C      either X(I) .NE. X(I+1) or Y(I) .NE. Y(I+1), for
C      I = 1,...,N-1.)
C
C    D
C      A parameter containing the observation weights.
C      this may either be an array of length N or a scalar
C      (interpreted as a constant).  The value of D
C      corresponding to the observation (X(K),Y(K)) should
C      be an approximation to the standard deviation of error.
C      Effectively the value of D controls how close the smoothed
C      curve comes to the input data points. If D is small
C      then the interpolated curve will pass close to the input data.
C      The larger the value of D, the more freedom the smooth curve
C      has in how close it comes to the input data values.
C
C    ISW
C      A switch indicating whether the parameter D is to be
C      considered a vector or a scalar,
C
C          = 0 if D is an array of length N,
C          = 1 if D is a scalar.
C
C    S
C      The value controlling the smoothing. this
C      must be non-negative.  For S equal to zero, the
C      subroutine does interpolation, larger values lead to
C      smoother funtions.  If parameter D contains standard
C      deviation estimates, a reasonable value for S is
C      FLOAT(N).
C
C    EPS
C      Contains a tolerance on the relative precision to
C      which S is to be interpreted. this must be greater than
C      or equal to zero and less than or equal to one.  A
C      reasonable value for EPS is SQRT(2./FLOAT(N)).
C
C    PARAM
C      An array of length at least N.
C
C    XS
C      An array of length at least N.
C
C    XSP
C      An array of length at least N.
C
C    YS
C      An array of length at least N.
C
C    YSP
C      An array of length at least N.
C
C    SIGMA
C      The tension factor.  This value indicates
C      the degree to which the first derivative part of the
C      smoothing functional is emphasized.  If SIGMA is nearly
C      zero (e.g. 0.001), the resulting curve is approximately a
C      cubic spline.  If SIGMA is large (e.g. 50.), the
C      resulting curve is nearly a polygonal line.  If sigma
C      equals zero a cubic spline results.  A standard value for
C      SIGMA is approximately 1.
C
C    TEMP
C      An array of length at least 19*N which is used
C      for scratch storage.
C
C
C  On output:
C
C    PARAM
C      PARAM(I) is the arc length of the curve up through
C      point (X(I),Y(I)), divided by the total arc length.
C
C    XS
C      The smoothed X-coordinate values.
C
C    XSP
C      The values of the second derivative of the
C      smoothed X-coordinate values at the given nodes.
C
C    YS
C      The smoothed Y-coordinate values.
C
C    YSP
C      The values of the second derivative of the
C      smoothed Y-coordinate values at the given nodes.
C
C    IERR
C      An error flag,
C         = 0 for normal return,
C         = 1 if N is less than 2,
C         = 2 if S is negative,
C         = 3 if EPS is negative or greater than one,
C         = 5 if a D-value is non-positive.
C
C  and
C
C    N, X, Y, D, ISW, S, EPS, and SIGMA are unaltered.
C
C this subroutine references package module CURVSDP.
C
      DOUBLE PRECISION X(N),Y(N),D(N),S,EPS,PARAM(N),XS(N),YS(N),XSP(N),
     +                 YSP(N),SIGMA,TEMP(N,20)
C
C  Calculate the arc length of each polygonal segment.
C
      TEMP(1,1) = 0.D0
      DO 10 I = 2,N
          IM1 = I - 1
          TEMP(I,1) = SQRT((X(I)-X(IM1))**2+ (Y(I)-Y(IM1))**2)
   10 CONTINUE
C
C  Calculate the total arc length.
C
      TSM = 0
      DO 5 I = 1,N
          TSM = TSM + TEMP(I,1)
    5 CONTINUE
C
C  Calculate the arc length parameter values at the input points.
C
      PARAM(1) = 0.D0
      DO 20 I = 2,N
          SM = 0
          DO 30 J = 1,I
              SM = SM + TEMP(J,1)
   30     CONTINUE
          PARAM(I) = SM/TSM
   20 CONTINUE
C
C  Get the smoothing coordinates for both X and Y.
C
      CALL CURVSDP(N,PARAM,X,D,ISW,S,EPS,XS,XSP,SIGMA,TEMP(1,2),IERR)
      CALL CURVSDP(N,PARAM,Y,D,ISW,S,EPS,YS,YSP,SIGMA,TEMP(1,11),IERR)
C
      RETURN
      END
