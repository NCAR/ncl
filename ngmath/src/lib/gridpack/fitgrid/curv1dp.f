C
C $Id: curv1dp.f,v 1.3 2008-07-27 03:10:10 haley Exp $
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
      SUBROUTINE CURV1DP(N,X,Y,SLP1,SLPN,ISLPSW,YP,TEMP,SIGMA,IERR)
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION SLPP1
      DOUBLE PRECISION DELX1
      DOUBLE PRECISION DELX2
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION C3
      DOUBLE PRECISION SLPPN
      DOUBLE PRECISION DELXN
      DOUBLE PRECISION DELXNM
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DIAG1
      DOUBLE PRECISION SDIAG1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DIAG2
      DOUBLE PRECISION SDIAG2
      DOUBLE PRECISION DIAG
c
      INTEGER N,ISLPSW,IERR
      DOUBLE PRECISION X(N),Y(N),SLP1,SLPN,YP(N),TEMP(N),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute an interpolatory spline under tension through
c a sequence of functional values. the slopes at the two
c ends of the curve may be specified or omitted.  for actual
c computation of points on the curve it is necessary to call
c the function curv2dp.
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
c   slp1 and slpn contain the desired values for the first
c   derivative of the curve at x(1) and x(n), respectively.
c   the user may omit values for either or both of these
c   parameters and signal this with islpsw.
c
c   islpsw contains a switch indicating which slope data
c   should be used and which should be estimated by this
c   subroutine,
c          = 0 if slp1 and slpn are to be used,
c          = 1 if slp1 is to be used but not slpn,
c          = 2 if slpn is to be used but not slp1,
c          = 3 if both slp1 and slpn are to be estimated
c              internally.
c
c   yp is an array of length at least n.
c
c   temp is an array of length at least n which is used for
c   scratch storage.
c
c and
c
c   sigma contains the tension factor. this value indicates
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
c        = 2 if x-values are not strictly increasing.
c
c and
c
c   n, x, y, slp1, slpn, islpsw and sigma are unaltered.
c
c this subroutine references package modules ceez, terms,
c and snhcsh.
c
c-----------------------------------------------------------
c
      NM1 = N - 1
      NP1 = N + 1
      IERR = 0
      IF (N.LE.1) GO TO 8
      IF (X(N).LE.X(1)) GO TO 9
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N-1)/ (X(N)-X(1))
c
c approximate end slopes
c
      IF (ISLPSW.GE.2) GO TO 1
      SLPP1 = SLP1
      GO TO 2
    1 DELX1 = X(2) - X(1)
      DELX2 = DELX1 + DELX1
      IF (N.GT.2) DELX2 = X(3) - X(1)
      IF (DELX1.LE.0.D0 .OR. DELX2.LE.DELX1) GO TO 9
      CALL CEEZDP(DELX1,DELX2,SIGMAP,C1,C2,C3,N)
      SLPP1 = C1*Y(1) + C2*Y(2)
      IF (N.GT.2) SLPP1 = SLPP1 + C3*Y(3)
    2 IF (ISLPSW.EQ.1 .OR. ISLPSW.EQ.3) GO TO 3
      SLPPN = SLPN
      GO TO 4
    3 DELXN = X(N) - X(NM1)
      DELXNM = DELXN + DELXN
      IF (N.GT.2) DELXNM = X(N) - X(N-2)
      IF (DELXN.LE.0.D0 .OR. DELXNM.LE.DELXN) GO TO 9
      CALL CEEZDP(-DELXN,-DELXNM,SIGMAP,C1,C2,C3,N)
      SLPPN = C1*Y(N) + C2*Y(NM1)
      IF (N.GT.2) SLPPN = SLPPN + C3*Y(N-2)
c
c set up right hand side and tridiagonal system for yp and
c perform forward elimination
c
    4 DELX1 = X(2) - X(1)
      IF (DELX1.LE.0.D0) GO TO 9
      DX1 = (Y(2)-Y(1))/DELX1
      CALL TERMSDP(DIAG1,SDIAG1,SIGMAP,DELX1)
      YP(1) = (DX1-SLPP1)/DIAG1
      TEMP(1) = SDIAG1/DIAG1
      IF (N.EQ.2) GO TO 6
      DO 5 I = 2,NM1
          DELX2 = X(I+1) - X(I)
          IF (DELX2.LE.0.D0) GO TO 9
          DX2 = (Y(I+1)-Y(I))/DELX2
          CALL TERMSDP(DIAG2,SDIAG2,SIGMAP,DELX2)
          DIAG = DIAG1 + DIAG2 - SDIAG1*TEMP(I-1)
          YP(I) = (DX2-DX1-SDIAG1*YP(I-1))/DIAG
          TEMP(I) = SDIAG2/DIAG
          DX1 = DX2
          DIAG1 = DIAG2
    5 SDIAG1 = SDIAG2
    6 DIAG = DIAG1 - SDIAG1*TEMP(NM1)
      YP(N) = (SLPPN-DX1-SDIAG1*YP(NM1))/DIAG
c
c perform back substitution
c
      DO 7 I = 2,N
          IBAK = NP1 - I
    7 YP(IBAK) = YP(IBAK) - TEMP(IBAK)*YP(IBAK+1)
      RETURN
c
c too few points
c
    8 IERR = 1
      RETURN
c
c x-values not strictly increasing
c
    9 IERR = 2
      RETURN
      END
