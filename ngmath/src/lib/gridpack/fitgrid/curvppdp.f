C
C $Id: curvppdp.f,v 1.3 2008-07-27 03:10:10 haley Exp $
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
      SUBROUTINE CURVPPDP(N,X,Y,P,D,ISW,S,EPS,YS,YSP,SIGMA,TD,TSD1,HD,
     +                  HSD1,HSD2,RD,RSD1,RSD2,RNM1,RN,V,IERR)
      DOUBLE PRECISION Q
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION DELXI1
      DOUBLE PRECISION DELYI1
      DOUBLE PRECISION DIM1
      DOUBLE PRECISION DELXI
      DOUBLE PRECISION DELYI
      DOUBLE PRECISION DI
      DOUBLE PRECISION HSD11
      DOUBLE PRECISION SL
      DOUBLE PRECISION SU
      DOUBLE PRECISION BETAPP
      DOUBLE PRECISION BETAP
      DOUBLE PRECISION ALPHAP
      DOUBLE PRECISION SUMD
      DOUBLE PRECISION SUMY
      DOUBLE PRECISION DISQ
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION HSD1IP
      DOUBLE PRECISION BETA
      DOUBLE PRECISION CON
      DOUBLE PRECISION SUM
      DOUBLE PRECISION HSD1P
      DOUBLE PRECISION HDIM1
      DOUBLE PRECISION HDI
      DOUBLE PRECISION YSPNM1
      DOUBLE PRECISION YSPN
      DOUBLE PRECISION RSD1I
      DOUBLE PRECISION SUMNM1
      DOUBLE PRECISION SUM2
      DOUBLE PRECISION SUMN
      DOUBLE PRECISION RSD2I
      DOUBLE PRECISION RNM1T
      DOUBLE PRECISION RNT
      DOUBLE PRECISION RDN
      DOUBLE PRECISION F
      DOUBLE PRECISION G
      DOUBLE PRECISION RNM1SM
      DOUBLE PRECISION RNSM
      DOUBLE PRECISION WIM2
      DOUBLE PRECISION WIM1
      DOUBLE PRECISION TUI
      DOUBLE PRECISION WI
      DOUBLE PRECISION H
      DOUBLE PRECISION STEP
c
      INTEGER N,ISW,IERR
      DOUBLE PRECISION X(N),Y(N),P,D(N),S,EPS,YS(N),YSP(N),SIGMA,TD(N),
     +                 TSD1(N),HD(N),HSD1(N),HSD2(N),RD(N),RSD1(N),
     +                 RSD2(N),RNM1(N),RN(N),V(N)
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the parameters necessary to
c compute a periodic smoothing spline under tension. for a
c given increasing sequence of abscissae (x(i)), i = 1,...,n
c and associated ordinates (y(i)), i = 1,...,n, letting p be
c the period, x(n+1) = x(1)+p, and y(n+1) = y(1), the
c function determined minimizes the summation from i = 1 to
c n of the square of the second derivative of f plus sigma
c squared times the difference of the first derivative of f
c and (f(x(i+1))-f(x(i)))/(x(i+1)-x(i)) squared, over all
c functions f with period p and two continuous derivatives
c such that the summation of the square of
c (f(x(i))-y(i))/d(i) is less than or equal to a given
c constant s, where (d(i)), i = 1,...,n are a given set of
c observation weights. the function determined is a periodic
c spline under tension with third derivative discontinuities
c at (x(i)) i = 1,...,n (and all periodic translations of
c these values). for actual computation of points on the
c curve it is necessary to call the function curvp2dp.
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
c   p is the period (p .gt. x(n)-x(1)).
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
c   td, tsd1, hd, hsd1, hsd2, rd, rsd1, rsd2, rnm1, rn, and
c   v are arrays of length at least n which are used for
c   scratch storage.
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
c        = 5 if a d-value is non-positive,
c        = 6 if p is less than or equal to x(n)-x(1).
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
      IF (N.LT.2) GO TO 25
      IF (S.LT.0.D0) GO TO 26
      IF (EPS.LT.0.D0 .OR. EPS.GT.1.D0) GO TO 27
      IF (P.LE.X(N)-X(1)) GO TO 30
      IERR = 0
      Q = 0.D0
      RSD1(1) = 0.D0
      RSD2(1) = 0.D0
      RSD2(2) = 0.D0
      RSD1(N-1) = 0.D0
      RSD2(N-1) = 0.D0
      RSD2(N) = 0.D0
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N)/P
c
c form t matrix and second differences of y into ys
c
      NM1 = N - 1
      NM2 = N - 2
      NM3 = N - 3
      DELXI1 = X(1) + P - X(N)
      DELYI1 = (Y(1)-Y(N))/DELXI1
      CALL TERMSDP(DIM1,TSD1(1),SIGMAP,DELXI1)
      HSD1(1) = 1.D0/DELXI1
      DO 1 I = 1,N
          IP1 = I + 1
          IF (I.EQ.N) IP1 = 1
          DELXI = X(IP1) - X(I)
          IF (I.EQ.N) DELXI = X(1) + P - X(N)
          IF (DELXI.LE.0.D0) GO TO 28
          DELYI = (Y(IP1)-Y(I))/DELXI
          YS(I) = DELYI - DELYI1
          CALL TERMSDP(DI,TSD1(IP1),SIGMAP,DELXI)
          TD(I) = DI + DIM1
          HD(I) = - (1.D0/DELXI+1.D0/DELXI1)
          HSD1(IP1) = 1.D0/DELXI
          DELXI1 = DELXI
          DELYI1 = DELYI
    1 DIM1 = DI
      HSD11 = HSD1(1)
      IF (N.GE.3) GO TO 2
      TSD1(2) = TSD1(1) + TSD1(2)
      TSD1(1) = 0.D0
      HSD1(2) = HSD1(1) + HSD1(2)
      HSD1(1) = 0.D0
c
c calculate lower and upper tolerances
c
    2 SL = S* (1.D0-EPS)
      SU = S* (1.D0+EPS)
      IF (D(1).LE.0.D0) GO TO 29
      IF (ISW.EQ.1) GO TO 5
c
c form h matrix - d array
c
      BETAPP = HSD1(N)*D(N)*D(N)
      BETAP = HSD1(1)*D(1)*D(1)
      ALPHAP = HD(N)*D(N)*D(N)
      IM1 = N
      SUMD = 0.D0
      SUMY = 0.D0
      DO 3 I = 1,N
          DISQ = D(I)*D(I)
          SUMD = SUMD + 1.D0/DISQ
          SUMY = SUMY + Y(I)/DISQ
          IP1 = I + 1
          IF (I.EQ.N) IP1 = 1
          ALPHA = HD(I)*DISQ
          IF (D(IP1).LE.0.D0) GO TO 29
          HSD1IP = HSD1(IP1)
          IF (I.EQ.N) HSD1IP = HSD11
          BETA = HSD1IP*D(IP1)*D(IP1)
          HD(I) = (HSD1(I)*D(IM1))**2 + ALPHA*HD(I) + BETA*HSD1IP
          HSD2(I) = HSD1(I)*BETAPP
          HSD1(I) = HSD1(I)* (ALPHA+ALPHAP)
          IM1 = I
          ALPHAP = ALPHA
          BETAPP = BETAP
    3 BETAP = BETA
      IF (N.EQ.3) HSD1(3) = HSD1(3) + HSD2(2)
c
c test for straight line fit
c
      CON = SUMY/SUMD
      SUM = 0.D0
      DO 4 I = 1,N
    4 SUM = SUM + ((Y(I)-CON)/D(I))**2
      IF (SUM.LE.SU) GO TO 23
      GO TO 8
c
c form h matrix - d constant
c
    5 SL = D(1)*D(1)*SL
      SU = D(1)*D(1)*SU
      HSD1P = HSD1(N)
      HDIM1 = HD(N)
      SUMY = 0.D0
      DO 6 I = 1,N
          SUMY = SUMY + Y(I)
          HSD1IP = HSD11
          IF (I.LT.N) HSD1IP = HSD1(I+1)
          HDI = HD(I)
          HD(I) = HSD1(I)*HSD1(I) + HDI*HDI + HSD1IP*HSD1IP
          HSD2(I) = HSD1(I)*HSD1P
          HSD1P = HSD1(I)
          HSD1(I) = HSD1P* (HDI+HDIM1)
    6 HDIM1 = HDI
      IF (N.EQ.3) HSD1(3) = HSD1(3) + HSD2(2)
c
c test for straight line fit
c
      CON = SUMY/DBLE(N)
      SUM = 0.D0
      DO 7 I = 1,N
    7 SUM = SUM + (Y(I)-CON)**2
      IF (SUM.LE.SU) GO TO 23
c
c top of iteration
c cholesky factorization of q*t+h into r
c
c
c i = 1
c
    8 RD(1) = 1.D0/ (Q*TD(1)+HD(1))
      RNM1(1) = HSD2(1)
      YSPNM1 = YS(NM1)
      RN(1) = Q*TSD1(1) + HSD1(1)
      YSPN = YS(N)
      YSP(1) = YS(1)
      RSD1I = Q*TSD1(2) + HSD1(2)
      RSD1(2) = RSD1I*RD(1)
      SUMNM1 = 0.D0
      SUM2 = 0.D0
      SUMN = 0.D0
      IF (N.EQ.3) GO TO 11
      IF (N.EQ.2) GO TO 12
c
c i = 2
c
      RD(2) = 1.D0/ (Q*TD(2)+HD(2)-RSD1I*RSD1(2))
      RNM1(2) = -RNM1(1)*RSD1(2)
      RN(2) = HSD2(2) - RN(1)*RSD1(2)
      YSP(2) = YS(2) - RSD1(2)*YSP(1)
      IF (N.EQ.4) GO TO 10
      DO 9 I = 3,NM2
          RSD2I = HSD2(I)
          RSD1I = Q*TSD1(I) + HSD1(I) - RSD2I*RSD1(I-1)
          RSD2(I) = RSD2I*RD(I-2)
          RSD1(I) = RSD1I*RD(I-1)
          RD(I) = 1.D0/ (Q*TD(I)+HD(I)-RSD1I*RSD1(I)-RSD2I*RSD2(I))
          RNM1(I) = -RNM1(I-2)*RSD2(I) - RNM1(I-1)*RSD1(I)
          RNM1T = RNM1(I-2)*RD(I-2)
          SUMNM1 = SUMNM1 + RNM1T*RNM1(I-2)
          RNM1(I-2) = RNM1T
          SUM2 = SUM2 + RNM1T*RN(I-2)
          YSPNM1 = YSPNM1 - RNM1T*YSP(I-2)
          RN(I) = -RN(I-2)*RSD2(I) - RN(I-1)*RSD1(I)
          RNT = RN(I-2)*RD(I-2)
          SUMN = SUMN + RNT*RN(I-2)
          RN(I-2) = RNT
          YSPN = YSPN - RNT*YSP(I-2)
    9 YSP(I) = YS(I) - RSD1(I)*YSP(I-1) - RSD2(I)*YSP(I-2)
c
c i = n-3
c
   10 RNM1(NM3) = HSD2(NM1) + RNM1(NM3)
      RNM1(NM2) = RNM1(NM2) - HSD2(NM1)*RSD1(NM2)
      RNM1T = RNM1(NM3)*RD(NM3)
      SUMNM1 = SUMNM1 + RNM1T*RNM1(NM3)
      RNM1(NM3) = RNM1T
      SUM2 = SUM2 + RNM1T*RN(NM3)
      YSPNM1 = YSPNM1 - RNM1T*YSP(NM3)
      RNT = RN(NM3)*RD(NM3)
      SUMN = SUMN + RNT*RN(NM3)
      RN(NM3) = RNT
      YSPN = YSPN - RNT*YSP(NM3)
c
c i = n-2
c
   11 RNM1(NM2) = Q*TSD1(NM1) + HSD1(NM1) + RNM1(NM2)
      RNM1T = RNM1(NM2)*RD(NM2)
      SUMNM1 = SUMNM1 + RNM1T*RNM1(NM2)
      RNM1(NM2) = RNM1T
      RN(NM2) = HSD2(N) + RN(NM2)
      SUM2 = SUM2 + RNM1T*RN(NM2)
      YSPNM1 = YSPNM1 - RNM1T*YSP(NM2)
      RNT = RN(NM2)*RD(NM2)
      SUMN = SUMN + RNT*RN(NM2)
      RN(NM2) = RNT
      YSPN = YSPN - RNT*YSP(NM2)
c
c i = n-1
c
   12 RD(NM1) = 1.D0/ (Q*TD(NM1)+HD(NM1)-SUMNM1)
      YSP(NM1) = YSPNM1
      RN(NM1) = Q*TSD1(N) + HSD1(N) - SUM2
      RNT = RN(NM1)*RD(NM1)
      SUMN = SUMN + RNT*RN(NM1)
      RN(NM1) = RNT
      YSPN = YSPN - RNT*YSP(NM1)
c
c i = n
c
      RDN = Q*TD(N) + HD(N) - SUMN
      RD(N) = 0.D0
c
c modified test based on communication with Alan Cline, 21-May-02.
c problems with the original test (rdn .gt. 0.) surfaced in a demo
c example run on a Linux system.
c
      IF (RDN.GT.0.D0 .AND. Q.NE.0.D0) RD(N) = 1.D0/RDN
      YSP(N) = YSPN
c
c back solve of r(transpose)* r * ysp = ys
c
      YSP(N) = RD(N)*YSP(N)
      YSP(NM1) = RD(NM1)*YSP(NM1) - RN(NM1)*YSP(N)
      IF (N.EQ.2) GO TO 14
      YSPN = YSP(N)
      YSPNM1 = YSP(NM1)
      DO 13 IBAK = 1,NM2
          I = NM1 - IBAK
   13 YSP(I) = RD(I)*YSP(I) - RSD1(I+1)*YSP(I+1) - RSD2(I+2)*YSP(I+2) -
     +         RNM1(I)*YSPNM1 - RN(I)*YSPN
   14 SUM = 0.D0
      DELYI1 = (YSP(1)-YSP(N))/ (X(1)+P-X(N))
      IF (ISW.EQ.1) GO TO 16
c
c calculation of residual norm
c  - d array
c
      DO 15 I = 1,NM1
          DELYI = (YSP(I+1)-YSP(I))/ (X(I+1)-X(I))
          V(I) = (DELYI-DELYI1)*D(I)*D(I)
          SUM = SUM + V(I)* (DELYI-DELYI1)
   15 DELYI1 = DELYI
      DELYI = (YSP(1)-YSP(N))/ (X(1)+P-X(N))
      V(N) = (DELYI-DELYI1)*D(N)*D(N)
      GO TO 18
c
c calculation of residual norm
c  - d constant
c
   16 DO 17 I = 1,NM1
          DELYI = (YSP(I+1)-YSP(I))/ (X(I+1)-X(I))
          V(I) = DELYI - DELYI1
          SUM = SUM + V(I)* (DELYI-DELYI1)
   17 DELYI1 = DELYI
      DELYI = (YSP(1)-YSP(N))/ (X(1)+P-X(N))
      V(N) = DELYI - DELYI1
   18 SUM = SUM + V(N)* (DELYI-DELYI1)
c
c test for convergence
c
      IF (SUM.LE.SU .AND. SUM.GE.SL .AND. Q.GT.0.D0) GO TO 21
c
c calculation of newton correction
c
      F = 0.D0
      G = 0.D0
      RNM1SM = 0.D0
      RNSM = 0.D0
      IM1 = N
      IF (N.EQ.2) GO TO 20
      WIM2 = 0.D0
      WIM1 = 0.D0
      DO 19 I = 1,NM2
          TUI = TSD1(I)*YSP(IM1) + TD(I)*YSP(I) + TSD1(I+1)*YSP(I+1)
          WI = TUI - RSD1(I)*WIM1 - RSD2(I)*WIM2
          RNM1SM = RNM1SM - RNM1(I)*WI
          RNSM = RNSM - RN(I)*WI
          F = F + TUI*YSP(I)
          G = G + WI*WI*RD(I)
          IM1 = I
          WIM2 = WIM1
   19 WIM1 = WI
   20 TUI = TSD1(NM1)*YSP(IM1) + TD(NM1)*YSP(NM1) + TSD1(N)*YSP(N)
      WI = TUI + RNM1SM
      F = F + TUI*YSP(NM1)
      G = G + WI*WI*RD(NM1)
      TUI = TSD1(N)*YSP(NM1) + TD(N)*YSP(N) + TSD1(1)*YSP(1)
      WI = TUI + RNSM - RN(NM1)*WI
      F = F + TUI*YSP(N)
      G = G + WI*WI*RD(N)
      H = F - Q*G
      IF (H.LE.0.D0 .AND. Q.GT.0.D0) GO TO 21
c
c update q - newton step
c
      STEP = (SUM-SQRT(SUM*SL))/H
      IF (SL.NE.0.D0) STEP = STEP*SQRT(SUM/SL)
      Q = Q + STEP
      GO TO 8
c
c store smoothed y-values and second derivatives
c
   21 DO 22 I = 1,N
          YS(I) = Y(I) - V(I)
   22 YSP(I) = Q*YSP(I)
      RETURN
c
c store constant ys and zero ysp
c
   23 DO 24 I = 1,N
          YS(I) = CON
   24 YSP(I) = 0.D0
      RETURN
c
c n less than 2
c
   25 IERR = 1
      RETURN
c
c s negative
c
   26 IERR = 2
      RETURN
c
c eps negative or greater than 1
c
   27 IERR = 3
      RETURN
c
c x-values not strictly increasing
c
   28 IERR = 4
      RETURN
c
c weight non-positive
c
   29 IERR = 5
      RETURN
c
c incorrect period
c
   30 IERR = 6
      RETURN
      END
