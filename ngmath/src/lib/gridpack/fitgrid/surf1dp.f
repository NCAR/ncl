C
C $Id: surf1dp.f,v 1.3 2008-07-27 03:10:12 haley Exp $
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
      SUBROUTINE SURF1DP(M,N,X,Y,Z,IZ,ZX1,ZXM,ZY1,ZYN,ZXY11,ZXYM1,ZXY1N,
     +                 ZXYMN,ISLPSW,ZP,TEMP,SIGMA,IERR)
      DOUBLE PRECISION SIGMAY
      DOUBLE PRECISION DELY1
      DOUBLE PRECISION DELY2
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION C3
      DOUBLE PRECISION DELYN
      DOUBLE PRECISION DELYNM
      DOUBLE PRECISION SIGMAX
      DOUBLE PRECISION DELX1
      DOUBLE PRECISION DELX2
      DOUBLE PRECISION ZXY1NS
      DOUBLE PRECISION DELXM
      DOUBLE PRECISION DELXMM
      DOUBLE PRECISION ZXYMNS
      DOUBLE PRECISION DEL1
      DOUBLE PRECISION DELI
      DOUBLE PRECISION DIAG1
      DOUBLE PRECISION SDIAG1
      DOUBLE PRECISION DIAGI
      DOUBLE PRECISION DEL2
      DOUBLE PRECISION DIAG2
      DOUBLE PRECISION SDIAG2
      DOUBLE PRECISION DIAGIN
      DOUBLE PRECISION T
c
      INTEGER M,N,IZ,ISLPSW,IERR
      DOUBLE PRECISION X(M),Y(N),Z(IZ,N),ZX1(N),ZXM(N),ZY1(M),ZYN(M),
     +                 ZXY11,ZXYM1,ZXY1N,ZXYMN,ZP(M,N,3),TEMP(1),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute an interpolatory surface passing through a rect-
c angular grid of functional values. the surface determined
c can be represented as the tensor product of splines under
c tension. the x- and y-partial derivatives around the
c boundary and the x-y-partial derivatives at the four
c corners may be specified or omitted. for actual mapping
c of points onto the surface it is necessary to call the
c function surf2.
c
c on input--
c
c   m is the number of grid lines in the x-direction, i. e.
c   lines parallel to the y-axis (m .ge. 2).
c
c   n is the number of grid lines in the y-direction, i. e.
c   lines parallel to the x-axis (n .ge. 2).
c
c   x is an array of the m x-coordinates of the grid lines
c   in the x-direction. these should be strictly increasing.
c
c   y is an array of the n y-coordinates of the grid lines
c   in the y-direction. these should be strictly increasing.
c
c   z is an array of the m * n functional values at the grid
c   points, i. e. z(i,j) contains the functional value at
c   (x(i),y(j)) for i = 1,...,m and j = 1,...,n.
c
c   iz is the row dimension of the matrix z used in the
c   calling program (iz .ge. m).
c
c   zx1 and zxm are arrays of the m x-partial derivatives
c   of the function along the x(1) and x(m) grid lines,
c   respectively. thus zx1(j) and zxm(j) contain the x-part-
c   ial derivatives at the points (x(1),y(j)) and
c   (x(m),y(j)), respectively, for j = 1,...,n. either of
c   these parameters will be ignored (and approximations
c   supplied internally) if islpsw so indicates.
c
c   zy1 and zyn are arrays of the n y-partial derivatives
c   of the function along the y(1) and y(n) grid lines,
c   respectively. thus zy1(i) and zyn(i) contain the y-part-
c   ial derivatives at the points (x(i),y(1)) and
c   (x(i),y(n)), respectively, for i = 1,...,m. either of
c   these parameters will be ignored (and estimations
c   supplied internally) if islpsw so indicates.
c
c   zxy11, zxym1, zxy1n, and zxymn are the x-y-partial
c   derivatives of the function at the four corners,
c   (x(1),y(1)), (x(m),y(1)), (x(1),y(n)), and (x(m),y(n)),
c   respectively. any of the parameters will be ignored (and
c   estimations supplied internally) if islpsw so indicates.
c
c   islpsw contains a switch indicating which boundary
c   derivative information is user-supplied and which
c   should be estimated by this subroutine. to determine
c   islpsw, let
c        i1 = 0 if zx1 is user-supplied (and = 1 otherwise),
c        i2 = 0 if zxm is user-supplied (and = 1 otherwise),
c        i3 = 0 if zy1 is user-supplied (and = 1 otherwise),
c        i4 = 0 if zyn is user-supplied (and = 1 otherwise),
c        i5 = 0 if zxy11 is user-supplied
c                                       (and = 1 otherwise),
c        i6 = 0 if zxym1 is user-supplied
c                                       (and = 1 otherwise),
c        i7 = 0 if zxy1n is user-supplied
c                                       (and = 1 otherwise),
c        i8 = 0 if zxymn is user-supplied
c                                       (and = 1 otherwise),
c   then islpsw = i1 + 2*i2 + 4*i3 + 8*i4 + 16*i5 + 32*i6
c                   + 64*i7 + 128*i8
c   thus islpsw = 0 indicates all derivative information is
c   user-supplied and islpsw = 255 indicates no derivative
c   information is user-supplied. any value between these
c   limits is valid.
c
c   zp is an array of at least 3*m*n locations.
c
c   temp is an array of at least n+n+m locations which is
c   used for scratch storage.
c
c and
c
c   sigma contains the tension factor. this value indicates
c   the curviness desired. if abs(sigma) is nearly zero
c   (e. g. .001) the resulting surface is approximately the
c   tensor product of cubic splines. if abs(sigma) is large
c   (e. g. 50.) the resulting surface is approximately
c   bi-linear. if sigma equals zero tensor products of
c   cubic splines result. a standard value for sigma is
c   approximately 1. in absolute value.
c
c on output--
c
c   zp contains the values of the xx-, yy-, and xxyy-partial
c   derivatives of the surface at the given nodes.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2 or m is less than 2,
c        = 2 if the x-values or y-values are not strictly
c            increasing.
c
c and
c
c   m, n, x, y, z, iz, zx1, zxm, zy1, zyn, zxy11, zxym1,
c   zxy1n, zxymn, islpsw, and sigma are unaltered.
c
c this subroutine references package modules ceez, terms,
c and snhcsh.
c
c-----------------------------------------------------------
c
      MM1 = M - 1
      MP1 = M + 1
      NM1 = N - 1
      NP1 = N + 1
      NPM = N + M
      IERR = 0
      IF (N.LE.1 .OR. M.LE.1) GO TO 46
      IF (Y(N).LE.Y(1)) GO TO 47
c
c denormalize tension factor in y-direction
c
      SIGMAY = ABS(SIGMA)*DBLE(N-1)/ (Y(N)-Y(1))
c
c obtain y-partial derivatives along y = y(1)
c
      IF ((ISLPSW/8)*2.NE. (ISLPSW/4)) GO TO 2
      DO 1 I = 1,M
    1 ZP(I,1,1) = ZY1(I)
      GO TO 5
    2 DELY1 = Y(2) - Y(1)
      DELY2 = DELY1 + DELY1
      IF (N.GT.2) DELY2 = Y(3) - Y(1)
      IF (DELY1.LE.0.D0 .OR. DELY2.LE.DELY1) GO TO 47
      CALL CEEZDP(DELY1,DELY2,SIGMAY,C1,C2,C3,N)
      DO 3 I = 1,M
    3 ZP(I,1,1) = C1*Z(I,1) + C2*Z(I,2)
      IF (N.EQ.2) GO TO 5
      DO 4 I = 1,M
    4 ZP(I,1,1) = ZP(I,1,1) + C3*Z(I,3)
c
c obtain y-partial derivatives along y = y(n)
c
    5 IF ((ISLPSW/16)*2.NE. (ISLPSW/8)) GO TO 7
      DO 6 I = 1,M
          NPI = N + I
    6 TEMP(NPI) = ZYN(I)
      GO TO 10
    7 DELYN = Y(N) - Y(NM1)
      DELYNM = DELYN + DELYN
      IF (N.GT.2) DELYNM = Y(N) - Y(N-2)
      IF (DELYN.LE.0.D0 .OR. DELYNM.LE.DELYN) GO TO 47
      CALL CEEZDP(-DELYN,-DELYNM,SIGMAY,C1,C2,C3,N)
      DO 8 I = 1,M
          NPI = N + I
    8 TEMP(NPI) = C1*Z(I,N) + C2*Z(I,NM1)
      IF (N.EQ.2) GO TO 10
      DO 9 I = 1,M
          NPI = N + I
    9 TEMP(NPI) = TEMP(NPI) + C3*Z(I,N-2)
   10 IF (X(M).LE.X(1)) GO TO 47
c
c denormalize tension factor in x-direction
c
      SIGMAX = ABS(SIGMA)*DBLE(M-1)/ (X(M)-X(1))
c
c obtain x-partial derivatives along x = x(1)
c
      IF ((ISLPSW/2)*2.NE.ISLPSW) GO TO 12
      DO 11 J = 1,N
   11 ZP(1,J,2) = ZX1(J)
      IF ((ISLPSW/32)*2.EQ. (ISLPSW/16) .AND.
     +    (ISLPSW/128)*2.EQ. (ISLPSW/64)) GO TO 15
   12 DELX1 = X(2) - X(1)
      DELX2 = DELX1 + DELX1
      IF (M.GT.2) DELX2 = X(3) - X(1)
      IF (DELX1.LE.0.D0 .OR. DELX2.LE.DELX1) GO TO 47
      CALL CEEZDP(DELX1,DELX2,SIGMAX,C1,C2,C3,M)
      IF ((ISLPSW/2)*2.EQ.ISLPSW) GO TO 15
      DO 13 J = 1,N
   13 ZP(1,J,2) = C1*Z(1,J) + C2*Z(2,J)
      IF (M.EQ.2) GO TO 15
      DO 14 J = 1,N
   14 ZP(1,J,2) = ZP(1,J,2) + C3*Z(3,J)
c
c obtain x-y-partial derivative at (x(1),y(1))
c
   15 IF ((ISLPSW/32)*2.NE. (ISLPSW/16)) GO TO 16
      ZP(1,1,3) = ZXY11
      GO TO 17
   16 ZP(1,1,3) = C1*ZP(1,1,1) + C2*ZP(2,1,1)
      IF (M.GT.2) ZP(1,1,3) = ZP(1,1,3) + C3*ZP(3,1,1)
c
c obtain x-y-partial derivative at (x(1),y(n))
c
   17 IF ((ISLPSW/128)*2.NE. (ISLPSW/64)) GO TO 18
      ZXY1NS = ZXY1N
      GO TO 19
   18 ZXY1NS = C1*TEMP(N+1) + C2*TEMP(N+2)
      IF (M.GT.2) ZXY1NS = ZXY1NS + C3*TEMP(N+3)
c
c obtain x-partial derivative along x = x(m)
c
   19 IF ((ISLPSW/4)*2.NE. (ISLPSW/2)) GO TO 21
      DO 20 J = 1,N
          NPMPJ = NPM + J
   20 TEMP(NPMPJ) = ZXM(J)
      IF ((ISLPSW/64)*2.EQ. (ISLPSW/32) .AND.
     +    (ISLPSW/256)*2.EQ. (ISLPSW/128)) GO TO 24
   21 DELXM = X(M) - X(MM1)
      DELXMM = DELXM + DELXM
      IF (M.GT.2) DELXMM = X(M) - X(M-2)
      IF (DELXM.LE.0.D0 .OR. DELXMM.LE.DELXM) GO TO 47
      CALL CEEZDP(-DELXM,-DELXMM,SIGMAX,C1,C2,C3,M)
      IF ((ISLPSW/4)*2.EQ. (ISLPSW/2)) GO TO 24
      DO 22 J = 1,N
          NPMPJ = NPM + J
   22 TEMP(NPMPJ) = C1*Z(M,J) + C2*Z(MM1,J)
      IF (M.EQ.2) GO TO 24
      DO 23 J = 1,N
          NPMPJ = NPM + J
   23 TEMP(NPMPJ) = TEMP(NPMPJ) + C3*Z(M-2,J)
c
c obtain x-y-partial derivative at (x(m),y(1))
c
   24 IF ((ISLPSW/64)*2.NE. (ISLPSW/32)) GO TO 25
      ZP(M,1,3) = ZXYM1
      GO TO 26
   25 ZP(M,1,3) = C1*ZP(M,1,1) + C2*ZP(MM1,1,1)
      IF (M.GT.2) ZP(M,1,3) = ZP(M,1,3) + C3*ZP(M-2,1,1)
c
c obtain x-y-partial derivative at (x(m),y(n))
c
   26 IF ((ISLPSW/256)*2.NE. (ISLPSW/128)) GO TO 27
      ZXYMNS = ZXYMN
      GO TO 28
   27 ZXYMNS = C1*TEMP(NPM) + C2*TEMP(NPM-1)
      IF (M.GT.2) ZXYMNS = ZXYMNS + C3*TEMP(NPM-2)
c
c set up right hand sides and tridiagonal system for y-grid
c perform forward elimination
c
   28 DEL1 = Y(2) - Y(1)
      IF (DEL1.LE.0.D0) GO TO 47
      DELI = 1.D0/DEL1
      DO 29 I = 1,M
   29 ZP(I,2,1) = DELI* (Z(I,2)-Z(I,1))
      ZP(1,2,3) = DELI* (ZP(1,2,2)-ZP(1,1,2))
      ZP(M,2,3) = DELI* (TEMP(NPM+2)-TEMP(NPM+1))
      CALL TERMSDP(DIAG1,SDIAG1,SIGMAY,DEL1)
      DIAGI = 1.D0/DIAG1
      DO 30 I = 1,M
   30 ZP(I,1,1) = DIAGI* (ZP(I,2,1)-ZP(I,1,1))
      ZP(1,1,3) = DIAGI* (ZP(1,2,3)-ZP(1,1,3))
      ZP(M,1,3) = DIAGI* (ZP(M,2,3)-ZP(M,1,3))
      TEMP(1) = DIAGI*SDIAG1
      IF (N.EQ.2) GO TO 34
      DO 33 J = 2,NM1
          JM1 = J - 1
          JP1 = J + 1
          NPMPJ = NPM + J
          DEL2 = Y(JP1) - Y(J)
          IF (DEL2.LE.0.D0) GO TO 47
          DELI = 1.D0/DEL2
          DO 31 I = 1,M
   31     ZP(I,JP1,1) = DELI* (Z(I,JP1)-Z(I,J))
          ZP(1,JP1,3) = DELI* (ZP(1,JP1,2)-ZP(1,J,2))
          ZP(M,JP1,3) = DELI* (TEMP(NPMPJ+1)-TEMP(NPMPJ))
          CALL TERMSDP(DIAG2,SDIAG2,SIGMAY,DEL2)
          DIAGIN = 1.D0/ (DIAG1+DIAG2-SDIAG1*TEMP(JM1))
          DO 32 I = 1,M
   32     ZP(I,J,1) = DIAGIN* (ZP(I,JP1,1)-ZP(I,J,1)-SDIAG1*ZP(I,JM1,1))
          ZP(1,J,3) = DIAGIN* (ZP(1,JP1,3)-ZP(1,J,3)-SDIAG1*ZP(1,JM1,3))
          ZP(M,J,3) = DIAGIN* (ZP(M,JP1,3)-ZP(M,J,3)-SDIAG1*ZP(M,JM1,3))
          TEMP(J) = DIAGIN*SDIAG2
          DIAG1 = DIAG2
   33 SDIAG1 = SDIAG2
   34 DIAGIN = 1.D0/ (DIAG1-SDIAG1*TEMP(NM1))
      DO 35 I = 1,M
          NPI = N + I
   35 ZP(I,N,1) = DIAGIN* (TEMP(NPI)-ZP(I,N,1)-SDIAG1*ZP(I,NM1,1))
      ZP(1,N,3) = DIAGIN* (ZXY1NS-ZP(1,N,3)-SDIAG1*ZP(1,NM1,3))
      TEMP(N) = DIAGIN* (ZXYMNS-ZP(M,N,3)-SDIAG1*ZP(M,NM1,3))
c
c perform back substitution
c
      DO 37 J = 2,N
          JBAK = NP1 - J
          JBAKP1 = JBAK + 1
          T = TEMP(JBAK)
          DO 36 I = 1,M
   36     ZP(I,JBAK,1) = ZP(I,JBAK,1) - T*ZP(I,JBAKP1,1)
          ZP(1,JBAK,3) = ZP(1,JBAK,3) - T*ZP(1,JBAKP1,3)
   37 TEMP(JBAK) = ZP(M,JBAK,3) - T*TEMP(JBAKP1)
c
c set up right hand sides and tridiagonal system for x-grid
c perform forward elimination
c
      DEL1 = X(2) - X(1)
      IF (DEL1.LE.0.D0) GO TO 47
      DELI = 1.D0/DEL1
      DO 38 J = 1,N
          ZP(2,J,2) = DELI* (Z(2,J)-Z(1,J))
   38 ZP(2,J,3) = DELI* (ZP(2,J,1)-ZP(1,J,1))
      CALL TERMSDP(DIAG1,SDIAG1,SIGMAX,DEL1)
      DIAGI = 1.D0/DIAG1
      DO 39 J = 1,N
          ZP(1,J,2) = DIAGI* (ZP(2,J,2)-ZP(1,J,2))
   39 ZP(1,J,3) = DIAGI* (ZP(2,J,3)-ZP(1,J,3))
      TEMP(N+1) = DIAGI*SDIAG1
      IF (M.EQ.2) GO TO 43
      DO 42 I = 2,MM1
          IM1 = I - 1
          IP1 = I + 1
          NPI = N + I
          DEL2 = X(IP1) - X(I)
          IF (DEL2.LE.0.D0) GO TO 47
          DELI = 1.D0/DEL2
          DO 40 J = 1,N
              ZP(IP1,J,2) = DELI* (Z(IP1,J)-Z(I,J))
   40     ZP(IP1,J,3) = DELI* (ZP(IP1,J,1)-ZP(I,J,1))
          CALL TERMSDP(DIAG2,SDIAG2,SIGMAX,DEL2)
          DIAGIN = 1.D0/ (DIAG1+DIAG2-SDIAG1*TEMP(NPI-1))
          DO 41 J = 1,N
              ZP(I,J,2) = DIAGIN* (ZP(IP1,J,2)-ZP(I,J,2)-
     +                    SDIAG1*ZP(IM1,J,2))
   41     ZP(I,J,3) = DIAGIN* (ZP(IP1,J,3)-ZP(I,J,3)-SDIAG1*ZP(IM1,J,3))
          TEMP(NPI) = DIAGIN*SDIAG2
          DIAG1 = DIAG2
   42 SDIAG1 = SDIAG2
   43 DIAGIN = 1.D0/ (DIAG1-SDIAG1*TEMP(NPM-1))
      DO 44 J = 1,N
          NPMPJ = NPM + J
          ZP(M,J,2) = DIAGIN* (TEMP(NPMPJ)-ZP(M,J,2)-SDIAG1*ZP(MM1,J,2))
   44 ZP(M,J,3) = DIAGIN* (TEMP(J)-ZP(M,J,3)-SDIAG1*ZP(MM1,J,3))
c
c perform back substitution
c
      DO 45 I = 2,M
          IBAK = MP1 - I
          IBAKP1 = IBAK + 1
          NPIBAK = N + IBAK
          T = TEMP(NPIBAK)
          DO 45 J = 1,N
              ZP(IBAK,J,2) = ZP(IBAK,J,2) - T*ZP(IBAKP1,J,2)
   45 ZP(IBAK,J,3) = ZP(IBAK,J,3) - T*ZP(IBAKP1,J,3)
      RETURN
c
c too few points
c
   46 IERR = 1
      RETURN
c
c points not strictly increasing
c
   47 IERR = 2
      RETURN
      END
