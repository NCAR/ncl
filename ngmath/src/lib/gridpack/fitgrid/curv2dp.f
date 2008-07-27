C
C $Id: curv2dp.f,v 1.3 2008-07-27 03:10:10 haley Exp $
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
      FUNCTION CURV2DP(T,N,X,Y,YP,SIGMA)
      DOUBLE PRECISION CURV2DP
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION DEL1
      DOUBLE PRECISION DEL2
      DOUBLE PRECISION DELS
      DOUBLE PRECISION SUM
      DOUBLE PRECISION SIGDEL
      DOUBLE PRECISION SS
      DOUBLE PRECISION DUMMY
      DOUBLE PRECISION S1
      DOUBLE PRECISION S2
c
      INTEGER N
      DOUBLE PRECISION T,X(N),Y(N),YP(N),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function interpolates a curve at a given point
c using a spline under tension. the subroutine curv1 should
c be called earlier to determine certain necessary
c parameters.
c
c on input--
c
c   t contains a real value to be mapped onto the interpo-
c   lating curve.
c
c   n contains the number of points which were specified to
c   determine the curve.
c
c   x and y are arrays containing the abscissae and
c   ordinates, respectively, of the specified points.
c
c   yp is an array of second derivative values of the curve
c   at the nodes.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, yp, and sigma should be input
c unaltered from the output of curv1.
c
c on output--
c
c   curv2dp contains the interpolated value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvldp and
c snhcshdp.
c
c-----------------------------------------------------------
c
c determine interval
c
      IM1 = INTRVLDP(T,X,N)
      I = IM1 + 1
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N-1)/ (X(N)-X(1))
c
c set up and perform interpolation
c
      DEL1 = T - X(IM1)
      DEL2 = X(I) - T
      DELS = X(I) - X(IM1)
      SUM = (Y(I)*DEL1+Y(IM1)*DEL2)/DELS
      IF (SIGMAP.NE.0.D0) GO TO 1
      CURV2DP = SUM - DEL1*DEL2* (YP(I)* (DEL1+DELS)+
     +        YP(IM1)* (DEL2+DELS))/ (6.D0*DELS)
      RETURN
    1 SIGDEL = SIGMAP*DELS
      CALL SNHCSHDP(SS,DUMMY,SIGDEL,-1)
      CALL SNHCSHDP(S1,DUMMY,SIGMAP*DEL1,-1)
      CALL SNHCSHDP(S2,DUMMY,SIGMAP*DEL2,-1)
      CURV2DP = SUM + (YP(I)*DEL1* (S1-SS)+YP(IM1)*DEL2* (S2-SS))/
     +        (SIGDEL*SIGMAP* (1.D0+SS))
      RETURN
      END
