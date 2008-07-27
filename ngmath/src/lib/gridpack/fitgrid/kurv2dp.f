C
C $Id: kurv2dp.f,v 1.3 2008-07-27 03:10:11 haley Exp $
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
      SUBROUTINE KURV2DP(T,XS,YS,N,X,Y,XP,YP,S,SIGMA)
      DOUBLE PRECISION TN
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION DEL1
      DOUBLE PRECISION DEL2
      DOUBLE PRECISION DELS
      DOUBLE PRECISION SUMX
      DOUBLE PRECISION SUMY
      DOUBLE PRECISION D
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION SIGDEL
      DOUBLE PRECISION SS
      DOUBLE PRECISION DUMMY
      DOUBLE PRECISION S1
      DOUBLE PRECISION S2
c
      INTEGER N
      DOUBLE PRECISION T,XS,YS,X(N),Y(N),XP(N),YP(N),S(N),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine performs the mapping of points in the
c interval (0.,1.) onto a curve in the plane. the subroutine
c kurv1 should be called earlier to determine certain
c necessary parameters. the resulting curve has a parametric
c representation both of whose components are splines under
c tension and functions of the polygonal arclength
c parameter.
c
c on input--
c
c   t contains a real value to be mapped to a point on the
c   curve. the interval (0.,1.) is mapped onto the entire
c   curve, with 0. mapping to (x(1),y(1)) and 1. mapping
c   to (x(n),y(n)). values outside this interval result in
c   extrapolation.
c
c   n contains the number of points which were specified
c   to determine the curve.
c
c   x and y are arrays containing the x- and y-coordinates
c   of the specified points.
c
c   xp and yp are the arrays output from kurv1 containing
c   curvature information.
c
c   s is an array containing the polygonal arclengths of
c   the curve.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, xp, yp, s, and sigma should be
c input unaltered from the output of kurv1.
c
c on output--
c
c   xs and ys contain the x- and y-coordinates of the image
c   point on the curve.
c
c none of the input parameters are altered.
c
c this subroutine references package modules intrvldp and
c snhcshdp.
c
c-----------------------------------------------------------
c
c determine interval
c
      TN = S(N)*T
      IM1 = INTRVLDP(TN,S,N)
      I = IM1 + 1
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N-1)/S(N)
c
c set up and perform interpolation
c
      DEL1 = TN - S(IM1)
      DEL2 = S(I) - TN
      DELS = S(I) - S(IM1)
      SUMX = (X(I)*DEL1+X(IM1)*DEL2)/DELS
      SUMY = (Y(I)*DEL1+Y(IM1)*DEL2)/DELS
      IF (SIGMAP.NE.0.D0) GO TO 1
      D = DEL1*DEL2/ (6.D0*DELS)
      C1 = (DEL1+DELS)*D
      C2 = (DEL2+DELS)*D
      XS = SUMX - XP(I)*C1 - XP(IM1)*C2
      YS = SUMY - YP(I)*C1 - YP(IM1)*C2
      RETURN
    1 SIGDEL = SIGMAP*DELS
      CALL SNHCSHDP(SS,DUMMY,SIGDEL,-1)
      CALL SNHCSHDP(S1,DUMMY,SIGMAP*DEL1,-1)
      CALL SNHCSHDP(S2,DUMMY,SIGMAP*DEL2,-1)
      D = SIGDEL*SIGMAP* (1.D0+SS)
      C1 = DEL1* (S1-SS)/D
      C2 = DEL2* (S2-SS)/D
      XS = SUMX + XP(I)*C1 + XP(IM1)*C2
      YS = SUMY + YP(I)*C1 + YP(IM1)*C2
      RETURN
      END
