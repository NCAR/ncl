C
C $Id: curvssdp.f,v 1.3 2008-07-27 03:10:11 haley Exp $
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
      SUBROUTINE CURVSSDP(N,X,Y,D,ISW,S,EPS,YS,YSP,SIGMA,TD,TSD1,HD,HSD1,
     +                  HSD2,RD,RSD1,RSD2,V,IERR)
      DOUBLE PRECISION P
      DOUBLE PRECISION RDIM1
      DOUBLE PRECISION YSPIM2
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION DELXI1
      DOUBLE PRECISION DELYI1
      DOUBLE PRECISION DIM1
      DOUBLE PRECISION DELXI
      DOUBLE PRECISION DELYI
      DOUBLE PRECISION DI
      DOUBLE PRECISION SL
      DOUBLE PRECISION SU
      DOUBLE PRECISION BETAPP
      DOUBLE PRECISION BETAP
      DOUBLE PRECISION ALPHAP
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION BETA
      DOUBLE PRECISION HSD1P
      DOUBLE PRECISION HDIM1
      DOUBLE PRECISION HDI
      DOUBLE PRECISION RSD2I
      DOUBLE PRECISION RSD1I
      DOUBLE PRECISION SUM
      DOUBLE PRECISION F
      DOUBLE PRECISION G
      DOUBLE PRECISION WIM2
      DOUBLE PRECISION WIM1
      DOUBLE PRECISION TUI
      DOUBLE PRECISION WI
      DOUBLE PRECISION H
      DOUBLE PRECISION STEP
c
      INTEGER N,ISW,IERR
      DOUBLE PRECISION X(N),Y(N),D(N),S,EPS,YS(N),YSP(N),SIGMA,TD(N),
     +                 TSD1(N),HD(N),HSD1(N),HSD2(N),RD(N),RSD1(N),
     +                 RSD2(N),V(N)
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a smoothing spline under tension. for a given
c increasing sequence of abscissae (x(i)), i = 1,..., n and
c associated ordinates (y(i)), i = 1,..., n, the function
c determined minimizes the summation from i = 1 to n-1 of
c the square of the second derivative of f plus sigma
c squared times the difference of the first derivative of f
c and (f(x(i+1))-f(x(i)))/(x(i+1)-x(i)) squared, over all
c functions f with two continuous derivatives such that the
c summation of the square of (f(x(i))-y(i))/d(i) is less
c than or equal to a given constant s, where (d(i)), i = 1,
c ..., n are a given set of observation weights. the
c function determined is a spline under tension with third
c derivative discontinuities at (x(i)), i = 2,..., n-1. for
c actual computation of points on the curve it is necessary
c to call the function curv2dp.
c
c on input--
c
c   n is the number of values to be smoothed (n.ge.2).
c
c   x is an array of the n increasing abscissae of the
c   values to be smoothed.
c
c   y is an array of the n ordinates of the values to be
c   smoothed, (i. e. y(k) is the functional value
c   corresponding to x(k) ).
c
c   d is a parameter containing the observation weights.
c   this may either be an array of length n or a scalar
c   (interpreted as a constant). the value of d
c   corresponding to the observation (x(k),y(k)) should
c   be an approximation to the standard deviation of error.
c
c   isw contains a switch indicating whether the parameter
c   d is to be considered a vector or a scalar,
c          = 0 if d is an array of length n,
c          = 1 if d is a scalar.
c
c   s contains the value controlling the smoothing. this
c   must be non-negative. for s equal to zero, the
c   subroutine does interpolation, larger values lead to
c   smoother funtions. if parameter d contains standard
c   deviation estimates, a reasonable value for s is
c   float(n).
c
c   eps contains a tolerance on the relative precision to
c   which s is to be interpreted. this must be greater than
c   or equal to zero and less than equal or equal to one. a
c   reasonable value for eps is sqrt(2./float(n)).
c
c   ys is an array of length at least n.
c
c   ysp is an array of length at least n.
c
c   sigma contains the tension factor. this value indicates
c   the degree to which the first derivative part of the
c   smoothing functional is emphasized. if sigma is nearly
c   zero (e. g. .001) the resulting curve is approximately a
c   cubic spline. if sigma is large (e. g. 50.) the
c   resulting curve is nearly a polygonal line. if sigma
c   equals zero a cubic spline results. a standard value for
c   sigma is approximately 1.
c
c and
c
c   td, tsd1, hd, hsd1, hsd2, rd, rsd1, rsd2, and v are
c   arrays of length at least n which are used for scratch
c   storage.
c
c on output--
c
c   ys contains the smoothed ordinate values.
c
c   ysp contains the values of the second derivative of the
c   smoothed curve at the given nodes.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2,
c        = 2 if s is negative,
c        = 3 if eps is negative or greater than one,
c        = 4 if x-values are not strictly increasing,
c        = 5 if a d-value is non-positive.
c
c and
c
c   n, x, y, d, isw, s, eps, and sigma are unaltered.
c
c this subroutine references package modules terms and
c snhcsh.
c
c-----------------------------------------------------------
c
      IF (N.LT.2) GO TO 16
      IF (S.LT.0.D0) GO TO 17
      IF (EPS.LT.0.D0 .OR. EPS.GT.1.D0) GO TO 18
      IERR = 0
      P = 0.D0
      V(1) = 0.D0
      V(N) = 0.D0
      YSP(1) = 0.D0
      YSP(N) = 0.D0
      IF (N.EQ.2) GO TO 14
      RSD1(1) = 0.D0
      RD(1) = 0.D0
      RSD2(N) = 0.D0
      RDIM1 = 0.D0
      YSPIM2 = 0.D0
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N-1)/ (X(N)-X(1))
c
c form t matrix and second differences of y into ys
c
      NM1 = N - 1
      NM3 = N - 3
      DELXI1 = 1.D0
      DELYI1 = 0.D0
      DIM1 = 0.D0
      DO 1 I = 1,NM1
          DELXI = X(I+1) - X(I)
          IF (DELXI.LE.0.D0) GO TO 19
          DELYI = (Y(I+1)-Y(I))/DELXI
          YS(I) = DELYI - DELYI1
          CALL TERMSDP(DI,TSD1(I+1),SIGMAP,DELXI)
          TD(I) = DI + DIM1
          HD(I) = - (1.D0/DELXI+1.D0/DELXI1)
          HSD1(I+1) = 1.D0/DELXI
          DELXI1 = DELXI
          DELYI1 = DELYI
    1 DIM1 = DI
c
c calculate lower and upper tolerances
c
      SL = S* (1.D0-EPS)
      SU = S* (1.D0+EPS)
      IF (ISW.EQ.1) GO TO 3
c
c form h matrix - d array
c
      IF (D(1).LE.0.D0 .OR. D(2).LE.0.D0) GO TO 20
      BETAPP = 0.D0
      BETAP = 0.D0
      ALPHAP = 0.D0
      DO 2 I = 2,NM1
          ALPHA = HD(I)*D(I)*D(I)
          IF (D(I+1).LE.0.D0) GO TO 20
          BETA = HSD1(I+1)*D(I+1)*D(I+1)
          HD(I) = (HSD1(I)*D(I-1))**2 + ALPHA*HD(I) + BETA*HSD1(I+1)
          HSD2(I) = HSD1(I)*BETAPP
          HSD1(I) = HSD1(I)* (ALPHA+ALPHAP)
          ALPHAP = ALPHA
          BETAPP = BETAP
    2 BETAP = BETA
      GO TO 5
c
c form h matrix - d constant
c
    3 IF (D(1).LE.0.D0) GO TO 20
      SL = D(1)*D(1)*SL
      SU = D(1)*D(1)*SU
      HSD1P = 0.D0
      HDIM1 = 0.D0
      DO 4 I = 2,NM1
          HDI = HD(I)
          HD(I) = HSD1(I)*HSD1(I) + HDI*HDI + HSD1(I+1)*HSD1(I+1)
          HSD2(I) = HSD1(I)*HSD1P
          HSD1P = HSD1(I)
          HSD1(I) = HSD1P* (HDI+HDIM1)
    4 HDIM1 = HDI
c
c top of iteration
c cholesky factorization of p*t+h into r
c
    5 DO 6 I = 2,NM1
          RSD2I = HSD2(I)
          RSD1I = P*TSD1(I) + HSD1(I) - RSD2I*RSD1(I-1)
          RSD2(I) = RSD2I*RDIM1
          RDIM1 = RD(I-1)
          RSD1(I) = RSD1I*RDIM1
          RD(I) = 1.D0/ (P*TD(I)+HD(I)-RSD1I*RSD1(I)-RSD2I*RSD2(I))
          YSP(I) = YS(I) - RSD1(I)*YSP(I-1) - RSD2(I)*YSPIM2
    6 YSPIM2 = YSP(I-1)
c
c back solve of r(transpose)* r * ysp = ys
c
      YSP(NM1) = RD(NM1)*YSP(NM1)
      IF (N.EQ.3) GO TO 8
      DO 7 IBAK = 1,NM3
          I = NM1 - IBAK
    7 YSP(I) = RD(I)*YSP(I) - RSD1(I+1)*YSP(I+1) - RSD2(I+2)*YSP(I+2)
    8 SUM = 0.D0
      DELYI1 = 0.D0
      IF (ISW.EQ.1) GO TO 10
c
c calculation of residual norm
c  - d array
c
      DO 9 I = 1,NM1
          DELYI = (YSP(I+1)-YSP(I))/ (X(I+1)-X(I))
          V(I) = (DELYI-DELYI1)*D(I)*D(I)
          SUM = SUM + V(I)* (DELYI-DELYI1)
    9 DELYI1 = DELYI
      V(N) = -DELYI1*D(N)*D(N)
      GO TO 12
c
c calculation of residual norm
c  - d constant
c
   10 DO 11 I = 1,NM1
          DELYI = (YSP(I+1)-YSP(I))/ (X(I+1)-X(I))
          V(I) = DELYI - DELYI1
          SUM = SUM + V(I)* (DELYI-DELYI1)
   11 DELYI1 = DELYI
      V(N) = -DELYI1
   12 SUM = SUM - V(N)*DELYI1
c
c test for convergence
c
      IF (SUM.LE.SU) GO TO 14
c
c calculation of newton correction
c
      F = 0.D0
      G = 0.D0
      WIM2 = 0.D0
      WIM1 = 0.D0
      DO 13 I = 2,NM1
          TUI = TSD1(I)*YSP(I-1) + TD(I)*YSP(I) + TSD1(I+1)*YSP(I+1)
          WI = TUI - RSD1(I)*WIM1 - RSD2(I)*WIM2
          F = F + TUI*YSP(I)
          G = G + WI*WI*RD(I)
          WIM2 = WIM1
   13 WIM1 = WI
      H = F - P*G
      IF (H.LE.0.D0) GO TO 14
c
c update p - newton step
c
      STEP = (SUM-SQRT(SUM*SL))/H
      IF (SL.NE.0.D0) STEP = STEP*SQRT(SUM/SL)
      P = P + STEP
      GO TO 5
c
c store smoothed y-values and second derivatives
c
   14 DO 15 I = 1,N
          YS(I) = Y(I) - V(I)
   15 YSP(I) = P*YSP(I)
      RETURN
c
c n less than 2
c
   16 IERR = 1
      RETURN
c
c s negative
c
   17 IERR = 2
      RETURN
c
c eps negative or greater than 1
c
   18 IERR = 3
      RETURN
c
c x-values not strictly increasing
c
   19 IERR = 4
      RETURN
c
c weight non-positive
c
   20 IERR = 5
      RETURN
      END
