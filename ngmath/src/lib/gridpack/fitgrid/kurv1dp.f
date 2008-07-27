C
C $Id: kurv1dp.f,v 1.3 2008-07-27 03:10:11 haley Exp $
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
      SUBROUTINE KURV1DP(N,X,Y,SLP1,SLPN,ISLPSW,XP,YP,TEMP,S,SIGMA,IERR)
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION SLPP1X
      DOUBLE PRECISION SLPP1Y
      DOUBLE PRECISION DELS1
      DOUBLE PRECISION DELS2
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION C3
      DOUBLE PRECISION SX
      DOUBLE PRECISION SY
      DOUBLE PRECISION DELT
      DOUBLE PRECISION SLPPNX
      DOUBLE PRECISION SLPPNY
      DOUBLE PRECISION DELSN
      DOUBLE PRECISION DELSNM
      DOUBLE PRECISION DX1
      DOUBLE PRECISION DY1
      DOUBLE PRECISION DIAG1
      DOUBLE PRECISION SDIAG1
      DOUBLE PRECISION DX2
      DOUBLE PRECISION DY2
      DOUBLE PRECISION DIAG2
      DOUBLE PRECISION SDIAG2
      DOUBLE PRECISION DIAG
      DOUBLE PRECISION DIAGIN
c
      INTEGER N,ISLPSW,IERR
      DOUBLE PRECISION X(N),Y(N),SLP1,SLPN,XP(N),YP(N),TEMP(N),S(N),
     +                 SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a spline under tension forming a curve in the
c plane and passing through a sequence of pairs (x(1),y(1)),
c ...,(x(n),y(n)). for actual computation of points on the
c curve it is necessary to call the subroutine kurv2.
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
c   i = 1,...,n-1.)
c
c   slp1 and slpn contain the desired values for the angles
c   (in radians) of the slope at (x(1),y(1)) and (x(n),y(n))
c   respectively. the angles are measured counter-clock-
c   wise from the x-axis and the positive sense of the curve
c   is assumed to be that moving from point 1 to point n.
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
c   xp and yp are arrays of length at least n.
c
c   temp is an array of length at least n which is used
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
c   n, x, y, slp1, slpn, islpsw, and sigma are unaltered.
c
c this subroutine references package modules ceez, terms,
c and snhcsh.
c
c-----------------------------------------------------------
c
      NM1 = N - 1
      NP1 = N + 1
      IERR = 0
      IF (N.LE.1) GO TO 11
c
c determine polygonal arclengths
c
      S(1) = 0.D0
      DO 1 I = 2,N
          IM1 = I - 1
    1 S(I) = S(IM1) + SQRT((X(I)-X(IM1))**2+ (Y(I)-Y(IM1))**2)
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N-1)/S(N)
c
c approximate end slopes
c
      IF (ISLPSW.GE.2) GO TO 2
      SLPP1X = COS(SLP1)
      SLPP1Y = SIN(SLP1)
      GO TO 4
    2 DELS1 = S(2) - S(1)
      DELS2 = DELS1 + DELS1
      IF (N.GT.2) DELS2 = S(3) - S(1)
      IF (DELS1.EQ.0.D0 .OR. DELS2.EQ.0.D0) GO TO 12
      CALL CEEZDP(DELS1,DELS2,SIGMAP,C1,C2,C3,N)
      SX = C1*X(1) + C2*X(2)
      SY = C1*Y(1) + C2*Y(2)
      IF (N.EQ.2) GO TO 3
      SX = SX + C3*X(3)
      SY = SY + C3*Y(3)
    3 DELT = SQRT(SX*SX+SY*SY)
      SLPP1X = SX/DELT
      SLPP1Y = SY/DELT
    4 IF (ISLPSW.EQ.1 .OR. ISLPSW.EQ.3) GO TO 5
      SLPPNX = COS(SLPN)
      SLPPNY = SIN(SLPN)
      GO TO 7
    5 DELSN = S(N) - S(NM1)
      DELSNM = DELSN + DELSN
      IF (N.GT.2) DELSNM = S(N) - S(N-2)
      IF (DELSN.EQ.0.D0 .OR. DELSNM.EQ.0.D0) GO TO 12
      CALL CEEZDP(-DELSN,-DELSNM,SIGMAP,C1,C2,C3,N)
      SX = C1*X(N) + C2*X(NM1)
      SY = C1*Y(N) + C2*Y(NM1)
      IF (N.EQ.2) GO TO 6
      SX = SX + C3*X(N-2)
      SY = SY + C3*Y(N-2)
    6 DELT = SQRT(SX*SX+SY*SY)
      SLPPNX = SX/DELT
      SLPPNY = SY/DELT
c
c set up right hand sides and tridiagonal system for xp and
c yp and perform forward elimination
c
    7 DX1 = (X(2)-X(1))/S(2)
      DY1 = (Y(2)-Y(1))/S(2)
      CALL TERMSDP(DIAG1,SDIAG1,SIGMAP,S(2))
      XP(1) = (DX1-SLPP1X)/DIAG1
      YP(1) = (DY1-SLPP1Y)/DIAG1
      TEMP(1) = SDIAG1/DIAG1
      IF (N.EQ.2) GO TO 9
      DO 8 I = 2,NM1
          DELS2 = S(I+1) - S(I)
          IF (DELS2.EQ.0.D0) GO TO 12
          DX2 = (X(I+1)-X(I))/DELS2
          DY2 = (Y(I+1)-Y(I))/DELS2
          CALL TERMSDP(DIAG2,SDIAG2,SIGMAP,DELS2)
          DIAG = DIAG1 + DIAG2 - SDIAG1*TEMP(I-1)
          DIAGIN = 1.D0/DIAG
          XP(I) = (DX2-DX1-SDIAG1*XP(I-1))*DIAGIN
          YP(I) = (DY2-DY1-SDIAG1*YP(I-1))*DIAGIN
          TEMP(I) = SDIAG2*DIAGIN
          DX1 = DX2
          DY1 = DY2
          DIAG1 = DIAG2
    8 SDIAG1 = SDIAG2
    9 DIAG = DIAG1 - SDIAG1*TEMP(NM1)
      XP(N) = (SLPPNX-DX1-SDIAG1*XP(NM1))/DIAG
      YP(N) = (SLPPNY-DY1-SDIAG1*YP(NM1))/DIAG
c
c perform back substitution
c
      DO 10 I = 2,N
          IBAK = NP1 - I
          XP(IBAK) = XP(IBAK) - TEMP(IBAK)*XP(IBAK+1)
   10 YP(IBAK) = YP(IBAK) - TEMP(IBAK)*YP(IBAK+1)
      RETURN
c
c too few points
c
   11 IERR = 1
      RETURN
c
c coincident adjacent points
c
   12 IERR = 2
      RETURN
      END
