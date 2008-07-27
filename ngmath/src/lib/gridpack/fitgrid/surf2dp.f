C
C $Id: surf2dp.f,v 1.3 2008-07-27 03:10:12 haley Exp $
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
      FUNCTION SURF2DP(XX,YY,M,N,X,Y,Z,IZ,ZP,SIGMA)
      DOUBLE PRECISION SURF2DP
      DOUBLE PRECISION HERMZ
      DOUBLE PRECISION F1
      DOUBLE PRECISION F2
      DOUBLE PRECISION FP1
      DOUBLE PRECISION FP2
      DOUBLE PRECISION DEL1
      DOUBLE PRECISION DEL2
      DOUBLE PRECISION DELS
      DOUBLE PRECISION HERMNZ
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION SINHM1
      DOUBLE PRECISION SINHMS
      DOUBLE PRECISION SINHM2
      DOUBLE PRECISION SIGMAX
      DOUBLE PRECISION SIGMAY
      DOUBLE PRECISION ZIM1
      DOUBLE PRECISION ZI
      DOUBLE PRECISION ZXXIM1
      DOUBLE PRECISION ZXXI
      DOUBLE PRECISION DUMMY
c
      INTEGER M,N,IZ
      DOUBLE PRECISION XX,YY,X(M),Y(N),Z(IZ,N),ZP(M,N,3),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function interpolates a surface at a given coordinate
c pair using a bi-spline under tension. the subroutine surf1
c should be called earlier to determine certain necessary
c parameters.
c
c on input--
c
c   xx and yy contain the x- and y-coordinates of the point
c   to be mapped onto the interpolating surface.
c
c   m and n contain the number of grid lines in the x- and
c   y-directions, respectively, of the rectangular grid
c   which specified the surface.
c
c   x and y are arrays containing the x- and y-grid values,
c   respectively, each in increasing order.
c
c   z is a matrix containing the m * n functional values
c   corresponding to the grid values (i. e. z(i,j) is the
c   surface value at the point (x(i),y(j)) for i = 1,...,m
c   and j = 1,...,n).
c
c   iz contains the row dimension of the array z as declared
c   in the calling program.
c
c   zp is an array of 3*m*n locations stored with the
c   various surface derivative information determined by
c   surf1.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters m, n, x, y, z, iz, zp, and sigma should be
c input unaltered from the output of surf1.
c
c on output--
c
c   surf2dp contains the interpolated surface value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvldp and
c snhcshdp.
c
c-----------------------------------------------------------
c
c inline one dimensional cubic spline interpolation
c
      HERMZ(F1,F2,FP1,FP2) = (F2*DEL1+F1*DEL2)/DELS -
     +                       DEL1*DEL2* (FP2* (DEL1+DELS)+
     +                       FP1* (DEL2+DELS))/ (6.D0*DELS)
c
c inline one dimensional spline under tension interpolation
c
      HERMNZ(F1,F2,FP1,FP2,SIGMAP) = (F2*DEL1+F1*DEL2)/DELS +
     +                               (FP2*DEL1* (SINHM1-SINHMS)+
     +                               FP1*DEL2* (SINHM2-SINHMS))/
     +                               (SIGMAP*SIGMAP*DELS* (1.D0+SINHMS))
c
c denormalize tension factor in x and y direction
c
      SIGMAX = ABS(SIGMA)*DBLE(M-1)/ (X(M)-X(1))
      SIGMAY = ABS(SIGMA)*DBLE(N-1)/ (Y(N)-Y(1))
c
c determine y interval
c
      JM1 = INTRVLDP(YY,Y,N)
      J = JM1 + 1
c
c determine x interval
c
      IM1 = INTRVLDP(XX,X,M)
      I = IM1 + 1
      DEL1 = YY - Y(JM1)
      DEL2 = Y(J) - YY
      DELS = Y(J) - Y(JM1)
      IF (SIGMAY.NE.0.D0) GO TO 1
c
c perform four interpolations in y-direction
c
      ZIM1 = HERMZ(Z(I-1,J-1),Z(I-1,J),ZP(I-1,J-1,1),ZP(I-1,J,1))
      ZI = HERMZ(Z(I,J-1),Z(I,J),ZP(I,J-1,1),ZP(I,J,1))
      ZXXIM1 = HERMZ(ZP(I-1,J-1,2),ZP(I-1,J,2),ZP(I-1,J-1,3),
     +         ZP(I-1,J,3))
      ZXXI = HERMZ(ZP(I,J-1,2),ZP(I,J,2),ZP(I,J-1,3),ZP(I,J,3))
      GO TO 2
    1 CALL SNHCSHDP(SINHM1,DUMMY,SIGMAY*DEL1,-1)
      CALL SNHCSHDP(SINHM2,DUMMY,SIGMAY*DEL2,-1)
      CALL SNHCSHDP(SINHMS,DUMMY,SIGMAY*DELS,-1)
      ZIM1 = HERMNZ(Z(I-1,J-1),Z(I-1,J),ZP(I-1,J-1,1),ZP(I-1,J,1),
     +       SIGMAY)
      ZI = HERMNZ(Z(I,J-1),Z(I,J),ZP(I,J-1,1),ZP(I,J,1),SIGMAY)
      ZXXIM1 = HERMNZ(ZP(I-1,J-1,2),ZP(I-1,J,2),ZP(I-1,J-1,3),
     +         ZP(I-1,J,3),SIGMAY)
      ZXXI = HERMNZ(ZP(I,J-1,2),ZP(I,J,2),ZP(I,J-1,3),ZP(I,J,3),SIGMAY)
c
c perform final interpolation in x-direction
c
    2 DEL1 = XX - X(IM1)
      DEL2 = X(I) - XX
      DELS = X(I) - X(IM1)
      IF (SIGMAX.NE.0.D0) GO TO 3
      SURF2DP = HERMZ(ZIM1,ZI,ZXXIM1,ZXXI)
      RETURN
    3 CALL SNHCSHDP(SINHM1,DUMMY,SIGMAX*DEL1,-1)
      CALL SNHCSHDP(SINHM2,DUMMY,SIGMAX*DEL2,-1)
      CALL SNHCSHDP(SINHMS,DUMMY,SIGMAX*DELS,-1)
      SURF2DP = HERMNZ(ZIM1,ZI,ZXXIM1,ZXXI,SIGMAX)
      RETURN
      END
