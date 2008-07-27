C
C $Id: curvp1dp.f,v 1.3 2008-07-27 03:10:10 haley Exp $
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
      SUBROUTINE CURVP1DP(N,X,Y,P,YP,TEMP,SIGMA,IERR)
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION DELX1
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DIAG1
      DOUBLE PRECISION SDIAG1
      DOUBLE PRECISION DELX2
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DIAG2
      DOUBLE PRECISION SDIAG2
      DOUBLE PRECISION DIAG
      DOUBLE PRECISION YPN
c
      INTEGER N,IERR
      DOUBLE PRECISION X(N),Y(N),P,YP(N),TEMP(1),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a periodic interpolatory spline under tension
c through a sequence of functional values. for actual ends
c of the curve may be specified or omitted.  for actual
c computation of points on the curve it is necessary to call
c the function curvp2.
c
c on input--
c
c   n is the number of values to be interpolated (n.ge.2).
c
c   x is an array of the n increasing abscissae of the
c   functional values.
c
c   y is an array of the n ordinates of the values, (i. e.
c   y(k) is the functional value corresponding to x(k) ).
c
c   p is the period (p .gt. x(n)-x(1)).
c
c   yp is an array of length at least n.
c
c   temp is an array of length at least 2*n which is used
c   for scratch storage.
c
c and
c
c   sigma contains the tension factor.  this value indicates
c   the curviness desired. if abs(sigma) is nearly zero
c   (e.g. .001) the resulting curve is approximately a
c   cubic spline. if abs(sigma) is large (e.g. 50.) the
c   resulting curve is nearly a polygonal line. if sigma
c   equals zero a cubic spline results.  a standard value
c   for sigma is approximately 1. in absolute value.
c
c on output--
c
c   yp contains the values of the second derivative of the
c   curve at the given nodes.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2,
c        = 2 if p is less than or equal to x(n)-x(1),
c        = 3 if x-values are not strictly increasing.
c
c and
c
c  n, x, y, and sigma are unaltered.
c
c this subroutine references package modules terms and
c snhcsh.
c
c-----------------------------------------------------------
c
      NM1 = N - 1
      NP1 = N + 1
      IERR = 0
      IF (N.LE.1) GO TO 6
      IF (P.LE.X(N)-X(1) .OR. P.LE.0.D0) GO TO 7
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N)/P
c
c set up right hand side and tridiagonal system for yp and
c perform forward elimination
c
      DELX1 = P - (X(N)-X(1))
      DX1 = (Y(1)-Y(N))/DELX1
      CALL TERMSDP(DIAG1,SDIAG1,SIGMAP,DELX1)
      DELX2 = X(2) - X(1)
      IF (DELX2.LE.0.D0) GO TO 8
      DX2 = (Y(2)-Y(1))/DELX2
      CALL TERMSDP(DIAG2,SDIAG2,SIGMAP,DELX2)
      DIAG = DIAG1 + DIAG2
      YP(1) = (DX2-DX1)/DIAG
      TEMP(NP1) = -SDIAG1/DIAG
      TEMP(1) = SDIAG2/DIAG
      DX1 = DX2
      DIAG1 = DIAG2
      SDIAG1 = SDIAG2
      IF (N.EQ.2) GO TO 2
      DO 1 I = 2,NM1
          NPI = N + I
          DELX2 = X(I+1) - X(I)
          IF (DELX2.LE.0.D0) GO TO 8
          DX2 = (Y(I+1)-Y(I))/DELX2
          CALL TERMSDP(DIAG2,SDIAG2,SIGMAP,DELX2)
          DIAG = DIAG1 + DIAG2 - SDIAG1*TEMP(I-1)
          YP(I) = (DX2-DX1-SDIAG1*YP(I-1))/DIAG
          TEMP(NPI) = -TEMP(NPI-1)*SDIAG1/DIAG
          TEMP(I) = SDIAG2/DIAG
          DX1 = DX2
          DIAG1 = DIAG2
    1 SDIAG1 = SDIAG2
    2 DELX2 = P - (X(N)-X(1))
      DX2 = (Y(1)-Y(N))/DELX2
      CALL TERMSDP(DIAG2,SDIAG2,SIGMAP,DELX2)
      YP(N) = DX2 - DX1
      TEMP(NM1) = TEMP(2*N-1) - TEMP(NM1)
      IF (N.EQ.2) GO TO 4
c
c perform first step of back substitution
c
      DO 3 I = 3,N
          IBAK = NP1 - I
          NPIBAK = N + IBAK
          YP(IBAK) = YP(IBAK) - TEMP(IBAK)*YP(IBAK+1)
    3 TEMP(IBAK) = TEMP(NPIBAK) - TEMP(IBAK)*TEMP(IBAK+1)
    4 YP(N) = (YP(N)-SDIAG2*YP(1)-SDIAG1*YP(NM1))/
     +        (DIAG1+DIAG2+SDIAG2*TEMP(1)+SDIAG1*TEMP(NM1))
c
c perform second step of back substitution
c
      YPN = YP(N)
      DO 5 I = 1,NM1
    5 YP(I) = YP(I) + TEMP(I)*YPN
      RETURN
c
c too few points
c
    6 IERR = 1
      RETURN
c
c period too small
c
    7 IERR = 2
      RETURN
c
c x-values not strictly increasing
c
    8 IERR = 3
      RETURN
      END
