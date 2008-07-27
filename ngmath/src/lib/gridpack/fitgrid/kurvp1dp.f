C
C $Id: kurvp1dp.f,v 1.3 2008-07-27 03:10:11 haley Exp $
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
      SUBROUTINE KURVP1DP(N,X,Y,XP,YP,TEMP,S,SIGMA,IERR)
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION DELS1
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DY1
      DOUBLE PRECISION DIAG1
      DOUBLE PRECISION SDIAG1
      DOUBLE PRECISION DELS2
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DY2
      DOUBLE PRECISION DIAG2
      DOUBLE PRECISION SDIAG2
      DOUBLE PRECISION DIAG
      DOUBLE PRECISION DIAGIN
      DOUBLE PRECISION XPN
      DOUBLE PRECISION YPN
c
      INTEGER N,IERR
      DOUBLE PRECISION X(N),Y(N),XP(N),YP(N),TEMP(1),S(N),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a spline under tension forming a closed curve in
c the plane and passing through a sequence of pairs
c (x(1),y(1)),...,(x(n),y(n)). for actual computation of
c points on the curve it is necessary to call the subroutine
c kurvp2.
c
c on input--
c
c   n is the number of points to be interpolated (n.ge.2).
c
c   x is an array containing the n x-coordinates of the
c   points.
c
c   y is an array containing the n y-coordinates of the
c   points. (adjacent x-y pairs must be distinct, i. e.
c   either x(i) .ne. x(i+1) or y(i) .ne. y(i+1), for
c   i = 1,...,n-1 and either x(1) .ne. x(n) or y(1) .ne. y(n).)
c
c   xp and yp are arrays of length at least n.
c
c   temp is an array of length at least 2*n which is used
c   for scratch storage.
c
c   s is an array of length at least n.
c
c and
c
c   sigma contains the tension factor. this value indicates
c   the curviness desired. if abs(sigma) is nearly zero
c   (e.g. .001) the resulting curve is approximately a cubic
c   spline. if abs(sigma) is large (e. g. 50.) the resulting
c   curve is nearly a polygonal line. if sigma equals zero a
c   cubic spline results. a standard value for sigma is
c   approximately 1. in absolute value.
c
c on output--
c
c   xp and yp contain information about the curvature of the
c   curve at the given nodes.
c
c   s contains the polygonal arclengths of the curve.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2,
c        = 2 if adjacent coordinate pairs coincide.
c
c and
c
c   n, x, y, and sigma are unaltered,
c
c this subroutine references package modules terms and
c snhcsh.
c
c-----------------------------------------------------------
c
      NM1 = N - 1
      NP1 = N + 1
      IERR = 0
      IF (N.LE.1) GO TO 7
c
c determine polygonal arclengths
c
      S(1) = SQRT((X(N)-X(1))**2+ (Y(N)-Y(1))**2)
      DO 1 I = 2,N
          IM1 = I - 1
    1 S(I) = S(IM1) + SQRT((X(I)-X(IM1))**2+ (Y(I)-Y(IM1))**2)
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N)/S(N)
c
c set up right hand sides of tridiagonal (with corner
c elements) linear system for xp and yp
c
      DELS1 = S(1)
      IF (DELS1.EQ.0.D0) GO TO 8
      DX1 = (X(1)-X(N))/DELS1
      DY1 = (Y(1)-Y(N))/DELS1
      CALL TERMSDP(DIAG1,SDIAG1,SIGMAP,DELS1)
      DELS2 = S(2) - S(1)
      IF (DELS2.EQ.0.D0) GO TO 8
      DX2 = (X(2)-X(1))/DELS2
      DY2 = (Y(2)-Y(1))/DELS2
      CALL TERMSDP(DIAG2,SDIAG2,SIGMAP,DELS2)
      DIAG = DIAG1 + DIAG2
      DIAGIN = 1.D0/DIAG
      XP(1) = (DX2-DX1)*DIAGIN
      YP(1) = (DY2-DY1)*DIAGIN
      TEMP(NP1) = -SDIAG1*DIAGIN
      TEMP(1) = SDIAG2*DIAGIN
      DX1 = DX2
      DY1 = DY2
      DIAG1 = DIAG2
      SDIAG1 = SDIAG2
      IF (N.EQ.2) GO TO 3
      DO 2 I = 2,NM1
          NPI = N + I
          DELS2 = S(I+1) - S(I)
          IF (DELS2.EQ.0.D0) GO TO 8
          DX2 = (X(I+1)-X(I))/DELS2
          DY2 = (Y(I+1)-Y(I))/DELS2
          CALL TERMSDP(DIAG2,SDIAG2,SIGMAP,DELS2)
          DIAG = DIAG1 + DIAG2 - SDIAG1*TEMP(I-1)
          DIAGIN = 1.D0/DIAG
          XP(I) = (DX2-DX1-SDIAG1*XP(I-1))*DIAGIN
          YP(I) = (DY2-DY1-SDIAG1*YP(I-1))*DIAGIN
          TEMP(NPI) = -TEMP(NPI-1)*SDIAG1*DIAGIN
          TEMP(I) = SDIAG2*DIAGIN
          DX1 = DX2
          DY1 = DY2
          DIAG1 = DIAG2
    2 SDIAG1 = SDIAG2
    3 DELS2 = S(1)
      DX2 = (X(1)-X(N))/DELS2
      DY2 = (Y(1)-Y(N))/DELS2
      CALL TERMSDP(DIAG2,SDIAG2,SIGMAP,DELS2)
      XP(N) = DX2 - DX1
      YP(N) = DY2 - DY1
      TEMP(NM1) = TEMP(2*N-1) - TEMP(NM1)
      IF (N.EQ.2) GO TO 5
c
c perform first step of back substitution
c
      DO 4 I = 3,N
          IBAK = NP1 - I
          NPIBAK = N + IBAK
          XP(IBAK) = XP(IBAK) - TEMP(IBAK)*XP(IBAK+1)
          YP(IBAK) = YP(IBAK) - TEMP(IBAK)*YP(IBAK+1)
    4 TEMP(IBAK) = TEMP(NPIBAK) - TEMP(IBAK)*TEMP(IBAK+1)
    5 XP(N) = (XP(N)-SDIAG2*XP(1)-SDIAG1*XP(NM1))/
     +        (DIAG1+DIAG2+SDIAG2*TEMP(1)+SDIAG1*TEMP(NM1))
      YP(N) = (YP(N)-SDIAG2*YP(1)-SDIAG1*YP(NM1))/
     +        (DIAG1+DIAG2+SDIAG2*TEMP(1)+SDIAG1*TEMP(NM1))
c
c perform second step of back substitution
c
      XPN = XP(N)
      YPN = YP(N)
      DO 6 I = 1,NM1
          XP(I) = XP(I) + TEMP(I)*XPN
    6 YP(I) = YP(I) + TEMP(I)*YPN
      RETURN
c
c too few points
c
    7 IERR = 1
      RETURN
c
c coincident adjacent points
c
    8 IERR = 2
      RETURN
      END
