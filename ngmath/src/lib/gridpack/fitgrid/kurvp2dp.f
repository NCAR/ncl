C
C $Id: kurvp2dp.f,v 1.3 2008-07-27 03:10:12 haley Exp $
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
      SUBROUTINE KURVP2DP(T,XS,YS,N,X,Y,XP,YP,S,SIGMA)
      DOUBLE PRECISION TN
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION SI
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
      DOUBLE PRECISION CI
      DOUBLE PRECISION CIM1
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
c interval (0.,1.) onto a closed curve in the plane. the
c subroutine kurvp1 should be called earlier to determine
c certain necessary parameters. the resulting curve has a
c parametric representation both of whose components are
c periodic splines under tension and functions of the poly-
c gonal arclength parameter.
c
c on input--
c
c   t contains a value to be mapped onto the curve. the
c   interval (0.,1.) is mapped onto the entire closed curve
c   with both 0. and 1. mapping to (x(1),y(1)). the mapping
c   is periodic with period one thus any interval of the
c   form (tt,tt+1.) maps onto the entire curve.
c
c   n contains the number of points which were specified
c   to determine the curve.
c
c   x and y are arrays containing the x- and y-coordinates
c   of the specified points.
c
c   xp and yp are the arrays output from kurvp1 containing
c   curvature information.
c
c   s is an array containing the polygonal arclengths of
c   the curve.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, xp, yp, s and sigma should
c be input unaltered from the output of kurvp1.
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
      TN = T - DBLE(INT(T))
      IF (TN.LT.0.D0) TN = TN + 1.D0
      TN = S(N)*TN + S(1)
      IM1 = N
      IF (TN.LT.S(N)) IM1 = INTRVLDP(TN,S,N)
      I = IM1 + 1
      IF (I.GT.N) I = 1
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N)/S(N)
c
c set up and perform interpolation
c
      SI = S(I)
      IF (IM1.EQ.N) SI = S(N) + S(1)
      DEL1 = TN - S(IM1)
      DEL2 = SI - TN
      DELS = SI - S(IM1)
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
      CI = DEL1* (S1-SS)/D
      CIM1 = DEL2* (S2-SS)/D
      XS = SUMX + XP(I)*CI + XP(IM1)*CIM1
      YS = SUMY + YP(I)*CI + YP(IM1)*CIM1
      RETURN
      END
