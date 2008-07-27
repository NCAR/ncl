C
C $Id: kurvpddp.f,v 1.3 2008-07-27 03:10:12 haley Exp $
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
      SUBROUTINE KURVPDDP(T,XS,YS,XST,YST,XSTT,YSTT,N,X,Y,XP,YP,S,SIGMA)
      DOUBLE PRECISION TN
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION SI
      DOUBLE PRECISION DEL1
      DOUBLE PRECISION DEL2
      DOUBLE PRECISION DELS
      DOUBLE PRECISION SUMX
      DOUBLE PRECISION SUMY
      DOUBLE PRECISION SUMXT
      DOUBLE PRECISION SUMYT
      DOUBLE PRECISION DELS6
      DOUBLE PRECISION D
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION CT1
      DOUBLE PRECISION CT2
      DOUBLE PRECISION CTT1
      DOUBLE PRECISION CTT2
      DOUBLE PRECISION SIGDEL
      DOUBLE PRECISION SS
      DOUBLE PRECISION DUMMY
      DOUBLE PRECISION S1
      DOUBLE PRECISION CO1
      DOUBLE PRECISION S2
      DOUBLE PRECISION CO2
c
      INTEGER N
      DOUBLE PRECISION T,XS,YS,XST,YST,XSTT,YSTT,X(N),Y(N),XP(N),YP(N),
     +                 S(N),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine performs the mapping of points in the
c interval (0.,1.) onto a closed curve in the plane. it also
c returns the first and second derivatives of the component
c functions. the subroutine kurvp1 should be called earlier
c to determine certain necessary parameters. the resulting
c curve has a parametric representation both of whose
c components are periodic splines under tension and
c functions of the polygonal arclength parameter.
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
c   point on the curve. xst and yst contain the first
c   derivatives of the x- and y-components of the mapping
c   with respect to t. xstt and ystt contain the second
c   derivatives of the x- and y-components of the mapping
c   with respect to t.
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
      SUMXT = S(N)* (X(I)-X(IM1))/DELS
      SUMYT = S(N)* (Y(I)-Y(IM1))/DELS
      IF (SIGMAP.NE.0.D0) GO TO 1
      DELS6 = 6.D0*DELS
      D = DEL1*DEL2/DELS6
      C1 = - (DEL1+DELS)*D
      C2 = - (DEL2+DELS)*D
      DELS6 = DELS6/S(N)
      CT1 = (2.D0*DEL1*DEL1-DEL2* (DEL1+DELS))/DELS6
      CT2 = - (2.D0*DEL2*DEL2-DEL1* (DEL2+DELS))/DELS6
      DELS = DELS/ (S(N)*S(N))
      CTT1 = DEL1/DELS
      CTT2 = DEL2/DELS
      GO TO 2
    1 SIGDEL = SIGMAP*DELS
      CALL SNHCSHDP(SS,DUMMY,SIGDEL,-1)
      CALL SNHCSHDP(S1,CO1,SIGMAP*DEL1,0)
      CALL SNHCSHDP(S2,CO2,SIGMAP*DEL2,0)
      D = SIGDEL*SIGMAP* (1.D0+SS)
      C1 = DEL1* (S1-SS)/D
      C2 = DEL2* (S2-SS)/D
      CT1 = (CO1-SS)*S(N)/D
      CT2 = - (CO2-SS)*S(N)/D
      CTT1 = DEL1* (1.D0+S1)*S(N)*S(N)/ (DELS* (1.D0+SS))
      CTT2 = DEL2* (1.D0+S2)*S(N)*S(N)/ (DELS* (1.D0+SS))
    2 XS = SUMX + C1*XP(I) + C2*XP(IM1)
      YS = SUMY + C1*YP(I) + C2*YP(IM1)
      XST = SUMXT + CT1*XP(I) + CT2*XP(IM1)
      YST = SUMYT + CT1*YP(I) + CT2*YP(IM1)
      XSTT = CTT1*XP(I) + CTT2*XP(IM1)
      YSTT = CTT1*YP(I) + CTT2*YP(IM1)
      RETURN
      END
