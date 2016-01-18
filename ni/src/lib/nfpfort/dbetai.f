C NCLFORTSTART
      subroutine betainc (x,a,b,result)
      implicit none
      double precision x,a,b,result

c NCL: alpha = betainc (x,a,b)
C NCLEND
      double precision dbetaislatec

      result = dbetaislatec(x,a,b)
      return
      end

c -----------------------------------------------
c All funtion names were changed by appending "SLATEC" to them
c I did this to avoid naming conflicts
c ----------------------------------------

      DOUBLE PRECISION FUNCTION DBETAISLATEC (X, PIN, QIN)
C***BEGIN PROLOGUE  DBETAISLATEC
C***PURPOSE  Calculate the incomplete Beta function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7F
C***TYPE      DOUBLE PRECISION (BETAI-S, DBETAI-D)
C***KEYWORDS  FNLIB, INCOMPLETE BETA FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C   DBETAI calculates the DOUBLE PRECISION incomplete beta function.
C
C   The incomplete beta function ratio is the probability that a
C   random variable from a beta distribution having parameters PIN and
C   QIN will be less than or equal to X.
C
C     -- Input Arguments -- All arguments are DOUBLE PRECISION.
C   X      upper limit of integration.  X must be in (0,1) inclusive.
C   PIN    first beta distribution parameter.  PIN must be .GT. 0.0.
C   QIN    second beta distribution parameter.  QIN must be .GT. 0.0.
C
C***REFERENCES  Nancy E. Bosten and E. L. Battiste, Remark on Algorithm
C                 179, Communications of the ACM 17, 3 (March 1974),
C                 pp. 156.
C***ROUTINES CALLED  D1MACH, DLBETA, XERDJS
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERDJS.  (THJ)
C   920528  DESCRIPTION and REFERENCES sections revised.  (WRB)
C***END PROLOGUE  DBETAI
      DOUBLE PRECISION X, PIN, QIN, ALNEPS, ALNSML, C, EPS, FINSUM, P,
     1  PS, Q, SML, TERM, XB, XI, Y, D1MACH, DLBETASLATEC, P1
      LOGICAL FIRST
      SAVE EPS, ALNEPS, SML, ALNSML, FIRST
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBETAI
      IF (FIRST) THEN
         EPS = D1MACH(3)
         ALNEPS = LOG (EPS)
         SML = D1MACH(1)
         ALNSML = LOG (SML)
      ENDIF
      FIRST = .FALSE.
C
cdjs  18 JAN 2016
cdjs  IF (X .LT. 0.D0 .OR. X .GT. 1.D0) CALL XERDJS ('SLATEC/NCL', 
cdjs +   'DBETAI','X IS NOT IN THE RANGE (0,1)', 1, 2)
cdjs  IF (PIN .LE. 0.D0 .OR. QIN .LE. 0.D0) CALL XERDJS ('SLATEC/NCL',
cdjs +   'DBETAI', 'P AND/OR Q IS LE ZERO', 2, 2)
C
      IF (X .LT. 0.D0 .OR. X .GT. 1.D0 .OR.
     +    PIN .LE. 0.D0 .OR. QIN .LE. 0.D0) THEN
          DBETAISLATEC = 1.0D30
          RETURN
      END IF
C
      Y = X
      P = PIN
      Q = QIN
      IF (Q.LE.P .AND. X.LT.0.8D0) GO TO 20
      IF (X.LT.0.2D0) GO TO 20
      Y = 1.0D0 - Y
      P = QIN
      Q = PIN
C
 20   IF ((P+Q)*Y/(P+1.D0).LT.EPS) GO TO 80
C
C EVALUATE THE INFINITE SUM FIRST.  TERM WILL EQUAL
C Y**P/BETA(PS,P) * (1.-PS)-SUB-I * Y**I / FAC(I) .
C
      PS = Q - AINT(Q)
      IF (PS.EQ.0.D0) PS = 1.0D0
      XB = P*LOG(Y) - DLBETASLATEC(PS,P) - LOG(P)
      DBETAISLATEC = 0.0D0
      IF (XB.LT.ALNSML) GO TO 40
C
      DBETAISLATEC = EXP (XB)
      TERM = DBETAISLATEC*P
      IF (PS.EQ.1.0D0) GO TO 40
      N = MAX (ALNEPS/LOG(Y), 4.0D0)
      DO 30 I=1,N
        XI = I
        TERM = TERM * (XI-PS)*Y/XI
        DBETAISLATEC = DBETAISLATEC + TERM/(P+XI)
 30   CONTINUE
C
C NOW EVALUATE THE FINITE SUM, MAYBE.
C
 40   IF (Q.LE.1.0D0) GO TO 70
C
      XB = P*LOG(Y) + Q*LOG(1.0D0-Y) - DLBETASLATEC(P,Q) - LOG(Q)
      IB = MAX (XB/ALNSML, 0.0D0)
      TERM = EXP(XB - IB*ALNSML)
      C = 1.0D0/(1.D0-Y)
      P1 = Q*C/(P+Q-1.D0)
C
      FINSUM = 0.0D0
      N = Q
      IF (Q.EQ.DBLE(N)) N = N - 1
      DO 50 I=1,N
        IF (P1.LE.1.0D0 .AND. TERM/EPS.LE.FINSUM) GO TO 60
        XI = I
        TERM = (Q-XI+1.0D0)*C*TERM/(P+Q-XI)
C
        IF (TERM.GT.1.0D0) IB = IB - 1
        IF (TERM.GT.1.0D0) TERM = TERM*SML
C
        IF (IB.EQ.0) FINSUM = FINSUM + TERM
 50   CONTINUE
C
 60   DBETAISLATEC = DBETAISLATEC + FINSUM
 70   IF (Y.NE.X .OR. P.NE.PIN) DBETAISLATEC = 1.0D0 - DBETAISLATEC
      DBETAISLATEC = MAX (MIN (DBETAISLATEC, 1.0D0), 0.0D0)
      RETURN
C
 80   DBETAISLATEC = 0.0D0
      XB = P*LOG(MAX(Y,SML)) - LOG(P) - DLBETASLATEC(P,Q)
      IF (XB.GT.ALNSML .AND. Y.NE.0.0D0) DBETAISLATEC = EXP(XB)
      IF (Y.NE.X .OR. P.NE.PIN) DBETAISLATEC = 1.0D0 - DBETAISLATEC
C
      RETURN
      END
C===> DECK D9LGMC
      DOUBLE PRECISION FUNCTION D9LGMCSLATEC (X)
C***BEGIN PROLOGUE  D9LGMC
C***SUBSIDIARY
C***PURPOSE  Compute the log Gamma correction factor so that
C            LOG(DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-5.)*LOG(X) - X
C            + D9LGMC(X).
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (R9LGMC-S, D9LGMC-D, C9LGMC-C)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, CORRECTION TERM, FNLIB,
C             LOG GAMMA, LOGARITHM, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the log gamma correction factor for X .GE. 10. so that
C LOG (DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-.5)*LOG(X) - X + D9lGMC(X)
C
C Series for ALGM       on the interval  0.          to  1.00000E-02
C                                        with weighted error   1.28E-31
C                                         log weighted error  30.89
C                               significant figures required  29.81
C                                    decimal places required  31.48
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERDJS
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERDJS.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9LGMC
      DOUBLE PRECISION X, ALGMCS(15), XBIG, XMAX, DCSEVLSLATEC, D1MACH
      LOGICAL FIRST
      SAVE ALGMCS, NALGM, XBIG, XMAX, FIRST
      DATA ALGMCS(  1) / +.1666389480 4518632472 0572965082 2 D+0      /
      DATA ALGMCS(  2) / -.1384948176 0675638407 3298605913 5 D-4      /
      DATA ALGMCS(  3) / +.9810825646 9247294261 5717154748 7 D-8      /
      DATA ALGMCS(  4) / -.1809129475 5724941942 6330626671 9 D-10     /
      DATA ALGMCS(  5) / +.6221098041 8926052271 2601554341 6 D-13     /
      DATA ALGMCS(  6) / -.3399615005 4177219443 0333059966 6 D-15     /
      DATA ALGMCS(  7) / +.2683181998 4826987489 5753884666 6 D-17     /
      DATA ALGMCS(  8) / -.2868042435 3346432841 4462239999 9 D-19     /
      DATA ALGMCS(  9) / +.3962837061 0464348036 7930666666 6 D-21     /
      DATA ALGMCS( 10) / -.6831888753 9857668701 1199999999 9 D-23     /
      DATA ALGMCS( 11) / +.1429227355 9424981475 7333333333 3 D-24     /
      DATA ALGMCS( 12) / -.3547598158 1010705471 9999999999 9 D-26     /
      DATA ALGMCS( 13) / +.1025680058 0104709120 0000000000 0 D-27     /
      DATA ALGMCS( 14) / -.3401102254 3167487999 9999999999 9 D-29     /
      DATA ALGMCS( 15) / +.1276642195 6300629333 3333333333 3 D-30     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  D9LGMC
      IF (FIRST) THEN
         NALGM = INITDSSLATEC (ALGMCS, 15, REAL(D1MACH(3)) )
         XBIG = 1.0D0/SQRT(D1MACH(3))
         XMAX = EXP (MIN(LOG(D1MACH(2)/12.D0), -LOG(12.D0*D1MACH(1))))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 10.D0) CALL XERDJS ('SLATEC/NCL', 'D9LGMC',
     +   'X MUST BE GE 10', 1, 2)
      IF (X.GE.XMAX) GO TO 20
C
      D9LGMCSLATEC = 1.D0/(12.D0*X)
      IF (X.LT.XBIG) D9LGMCSLATEC =DCSEVLSLATEC(2.0D0*(10.D0/X)**2-1.D0,
     1                                          ALGMCS, NALGM) / X
      RETURN
C
 20   D9LGMCSLATEC = 0.D0
      CALL XERDJS ('SLATEC/NCL','D9LGMC','X SO BIG D9LGMC UNDERFLOWS',2,
     +   1)
      RETURN
C
      END
C===> DECK DCSEVL
      DOUBLE PRECISION FUNCTION DCSEVLSLATEC (X, CS, N)
C***BEGIN PROLOGUE  DCSEVL
C***PURPOSE  Evaluate a Chebyshev series.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      DOUBLE PRECISION (CSEVL-S, DCSEVL-D)
C***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Evaluate the N-term Chebyshev series CS at X.  Adapted from
C  a method presented in the paper by Broucke referenced below.
C
C       Input Arguments --
C  X    value at which the series is to be evaluated.
C  CS   array of N terms of a Chebyshev series.  In evaluating
C       CS, only half the first coefficient is summed.
C  N    number of terms in array CS.
C
C***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
C                 Chebyshev series, Algorithm 446, Communications of
C                 the A.C.M. 16, (1973) pp. 254-256.
C               L. Fox and I. B. Parker, Chebyshev Polynomials in
C                 Numerical Analysis, Oxford University Press, 1968,
C                 page 56.
C***ROUTINES CALLED  D1MACH, XERDJS
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERDJS.  (THJ)
C   900329  Prologued revised extensively and code rewritten to allow
C           X to be slightly outside interval (-1,+1).  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DCSEVL
      DOUBLE PRECISION B0, B1, B2, CS(*), ONEPL, TWOX, X, D1MACH
      LOGICAL FIRST
      SAVE FIRST, ONEPL
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DCSEVL
      IF (FIRST) ONEPL = 1.0D0 + D1MACH(4)
      FIRST = .FALSE.
      IF (N .LT. 1) CALL XERDJS ('SLATEC/NCL', 'DCSEVL',
     +   'NUMBER OF TERMS .LE. 0', 2, 2)
      IF (N .GT. 1000) CALL XERDJS ('SLATEC/NCL', 'DCSEVL',
     +   'NUMBER OF TERMS .GT. 1000', 3, 2)
      IF (ABS(X) .GT. ONEPL) CALL XERDJS ('SLATEC/NCL', 'DCSEVL',
     +   'X OUTSIDE THE INTERVAL (-1,+1)', 1, 1)
C
      B1 = 0.0D0
      B0 = 0.0D0
      TWOX = 2.0D0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE
C
      DCSEVLSLATEC = 0.5D0*(B0-B2)
C
      RETURN
      END
C===> DECK DGAMLM
      SUBROUTINE DGAMLM (XMIN, XMAX)
C***BEGIN PROLOGUE  DGAMLM
C***PURPOSE  Compute the minimum and maximum bounds for the argument in
C            the Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A, R2
C***TYPE      DOUBLE PRECISION (GAMLIM-S, DGAMLM-D)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, LIMITS, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Calculate the minimum and maximum legal bounds for X in gamma(X).
C XMIN and XMAX are not the only bounds, but they are the only non-
C trivial ones to calculate.
C
C             Output Arguments --
C XMIN   double precision minimum legal value of X in gamma(X).  Any
C        smaller value of X might result in underflow.
C XMAX   double precision maximum legal value of X in gamma(X).  Any
C        larger value of X might cause overflow.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, XERDJS
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERDJS.  (THJ)
C***END PROLOGUE  DGAMLM
      DOUBLE PRECISION XMIN, XMAX, ALNBIG, ALNSML, XLN, XOLD, D1MACH
C***FIRST EXECUTABLE STATEMENT  DGAMLM
      ALNSML = LOG(D1MACH(1))
      XMIN = -ALNSML
      DO 10 I=1,10
        XOLD = XMIN
        XLN = LOG(XMIN)
        XMIN = XMIN - XMIN*((XMIN+0.5D0)*XLN - XMIN - 0.2258D0 + ALNSML)
     1    / (XMIN*XLN+0.5D0)
        IF (ABS(XMIN-XOLD).LT.0.005D0) GO TO 20
 10   CONTINUE
      CALL XERDJS ('SLATEC/NCL', 'DGAMLM', 'UNABLE TO FIND XMIN', 1, 2)
C
 20   XMIN = -XMIN + 0.01D0
C
      ALNBIG = LOG (D1MACH(2))
      XMAX = ALNBIG
      DO 30 I=1,10
        XOLD = XMAX
        XLN = LOG(XMAX)
        XMAX = XMAX - XMAX*((XMAX-0.5D0)*XLN - XMAX + 0.9189D0 - ALNBIG)
     1    / (XMAX*XLN-0.5D0)
        IF (ABS(XMAX-XOLD).LT.0.005D0) GO TO 40
 30   CONTINUE
      CALL XERDJS ('SLATEC/NCL', 'DGAMLM', 'UNABLE TO FIND XMAX', 2, 2)
C
 40   XMAX = XMAX - 0.01D0
      XMIN = MAX (XMIN, -XMAX+1.D0)
C
      RETURN
      END
C===> DECK DGAMMA
      DOUBLE PRECISION FUNCTION DGAMMASLATEC (X)
C***BEGIN PROLOGUE  DGAMMA
C***PURPOSE  Compute the complete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (GAMMA-S, DGAMMA-D, CGAMMA-C)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DGAMMA(X) calculates the double precision complete Gamma function
C for double precision argument X.
C
C Series for GAM        on the interval  0.          to  1.00000E+00
C                                        with weighted error   5.79E-32
C                                         log weighted error  31.24
C                               significant figures required  30.00
C                                    decimal places required  32.05
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9LGMC, DCSEVL, DGAMLM, INITDS, XERDJS
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERDJS.  (THJ)
C   920618  Removed space from variable name.  (RWC, WRB)
C***END PROLOGUE  DGAMMA
      DOUBLE PRECISION X, GAMCS(42), DXREL, PI, SINPIY, SQ2PIL, XMAX,
     1  XMIN, Y, D9LGMCSLATEC, DCSEVLSLATEC, D1MACH
      LOGICAL FIRST
C
      SAVE GAMCS, PI, SQ2PIL, NGAM, XMIN, XMAX, DXREL, FIRST
      DATA GAMCS(  1) / +.8571195590 9893314219 2006239994 2 D-2      /
      DATA GAMCS(  2) / +.4415381324 8410067571 9131577165 2 D-2      /
      DATA GAMCS(  3) / +.5685043681 5993633786 3266458878 9 D-1      /
      DATA GAMCS(  4) / -.4219835396 4185605010 1250018662 4 D-2      /
      DATA GAMCS(  5) / +.1326808181 2124602205 8400679635 2 D-2      /
      DATA GAMCS(  6) / -.1893024529 7988804325 2394702388 6 D-3      /
      DATA GAMCS(  7) / +.3606925327 4412452565 7808221722 5 D-4      /
      DATA GAMCS(  8) / -.6056761904 4608642184 8554829036 5 D-5      /
      DATA GAMCS(  9) / +.1055829546 3022833447 3182350909 3 D-5      /
      DATA GAMCS( 10) / -.1811967365 5423840482 9185589116 6 D-6      /
      DATA GAMCS( 11) / +.3117724964 7153222777 9025459316 9 D-7      /
      DATA GAMCS( 12) / -.5354219639 0196871408 7408102434 7 D-8      /
      DATA GAMCS( 13) / +.9193275519 8595889468 8778682594 0 D-9      /
      DATA GAMCS( 14) / -.1577941280 2883397617 6742327395 3 D-9      /
      DATA GAMCS( 15) / +.2707980622 9349545432 6654043308 9 D-10     /
      DATA GAMCS( 16) / -.4646818653 8257301440 8166105893 3 D-11     /
      DATA GAMCS( 17) / +.7973350192 0074196564 6076717535 9 D-12     /
      DATA GAMCS( 18) / -.1368078209 8309160257 9949917230 9 D-12     /
      DATA GAMCS( 19) / +.2347319486 5638006572 3347177168 8 D-13     /
      DATA GAMCS( 20) / -.4027432614 9490669327 6657053469 9 D-14     /
      DATA GAMCS( 21) / +.6910051747 3721009121 3833697525 7 D-15     /
      DATA GAMCS( 22) / -.1185584500 2219929070 5238712619 2 D-15     /
      DATA GAMCS( 23) / +.2034148542 4963739552 0102605193 2 D-16     /
      DATA GAMCS( 24) / -.3490054341 7174058492 7401294910 8 D-17     /
      DATA GAMCS( 25) / +.5987993856 4853055671 3505106602 6 D-18     /
      DATA GAMCS( 26) / -.1027378057 8722280744 9006977843 1 D-18     /
      DATA GAMCS( 27) / +.1762702816 0605298249 4275966074 8 D-19     /
      DATA GAMCS( 28) / -.3024320653 7353062609 5877211204 2 D-20     /
      DATA GAMCS( 29) / +.5188914660 2183978397 1783355050 6 D-21     /
      DATA GAMCS( 30) / -.8902770842 4565766924 4925160106 6 D-22     /
      DATA GAMCS( 31) / +.1527474068 4933426022 7459689130 6 D-22     /
      DATA GAMCS( 32) / -.2620731256 1873629002 5732833279 9 D-23     /
      DATA GAMCS( 33) / +.4496464047 8305386703 3104657066 6 D-24     /
      DATA GAMCS( 34) / -.7714712731 3368779117 0390152533 3 D-25     /
      DATA GAMCS( 35) / +.1323635453 1260440364 8657271466 6 D-25     /
      DATA GAMCS( 36) / -.2270999412 9429288167 0231381333 3 D-26     /
      DATA GAMCS( 37) / +.3896418998 0039914493 2081663999 9 D-27     /
      DATA GAMCS( 38) / -.6685198115 1259533277 9212799999 9 D-28     /
      DATA GAMCS( 39) / +.1146998663 1400243843 4761386666 6 D-28     /
      DATA GAMCS( 40) / -.1967938586 3451346772 9510399999 9 D-29     /
      DATA GAMCS( 41) / +.3376448816 5853380903 3489066666 6 D-30     /
      DATA GAMCS( 42) / -.5793070335 7821357846 2549333333 3 D-31     /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DGAMMA
      IF (FIRST) THEN
         NGAM = INITDSSLATEC (GAMCS, 42, 0.1*REAL(D1MACH(3)) )
C
         CALL DGAMLM (XMIN, XMAX)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.10.D0) GO TO 50
C
C COMPUTE GAMMA(X) FOR -XBND .LE. X .LE. XBND.  REDUCE INTERVAL AND FIND
C GAMMA(1+Y) FOR 0.0 .LE. Y .LT. 1.0 FIRST OF ALL.
C
      N = X
      IF (X.LT.0.D0) N = N - 1
      Y = X - N
      N = N - 1
      DGAMMASLATEC = 0.9375D0 + DCSEVLSLATEC (2.D0*Y-1.D0, GAMCS, NGAM)
      IF (N.EQ.0) RETURN
C
      IF (N.GT.0) GO TO 30
C
C COMPUTE GAMMA(X) FOR X .LT. 1.0
C
      N = -N
      IF (X .EQ. 0.D0) CALL XERDJS ('SLATEC/NCL', 'DGAMMA','X IS 0',4,2)
      IF (X .LT. 0.0 .AND. X+N-2 .EQ. 0.D0) CALL XERDJS ('SLATEC/NCL',
     +   'DGAMMA', 'X IS A NEGATIVE INTEGER', 4, 2)
      IF (X .LT. (-0.5D0) .AND. ABS((X-AINT(X-0.5D0))/X) .LT. DXREL)
     +   CALL XERDJS ('SLATEC/NCL', 'DGAMMA',
     +   'ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INTEGER',
     +   1, 1)
C
      DO 20 I=1,N
        DGAMMASLATEC = DGAMMASLATEC/(X+I-1 )
 20   CONTINUE
      RETURN
C
C GAMMA(X) FOR X .GE. 2.0 AND X .LE. 10.0
C
 30   DO 40 I=1,N
        DGAMMASLATEC = (Y+I) * DGAMMASLATEC
 40   CONTINUE
      RETURN
C
C GAMMA(X) FOR ABS(X) .GT. 10.0.  RECALL Y = ABS(X).
C
 50   IF (X .GT. XMAX) CALL XERDJS ('SLATEC/NCL', 'DGAMMA',
     +   'X SO BIG GAMMA OVERFLOWS', 3, 2)
C
      DGAMMASLATEC = 0.D0
      IF (X .LT. XMIN) CALL XERDJS ('SLATEC/NCL', 'DGAMMA',
     +   'X SO SMALL GAMMA UNDERFLOWS', 2, 1)
      IF (X.LT.XMIN) RETURN
C
      DGAMMASLATEC = EXP((Y-0.5D0)*LOG(Y)- Y +SQ2PIL +D9LGMCSLATEC(Y))
      IF (X.GT.0.D0) RETURN
C
      IF (ABS((X-AINT(X-0.5D0))/X) .LT. DXREL) CALL XERDJS('SLATEC/NCL',
     +   'DGAMMA',
     +   'ANSWER LT HALF PRECISION, X TOO NEAR NEGATIVE INTEGER', 1, 1)
C
      SINPIY = SIN (PI*Y)
      IF (SINPIY .EQ. 0.D0) CALL XERDJS ('SLATEC/NCL', 'DGAMMA',
     +   'X IS A NEGATIVE INTEGER', 4, 2)
C
      DGAMMASLATEC = -PI/(Y*SINPIY*DGAMMASLATEC)
C
      RETURN
      END
C===> DECK DLBETA
      DOUBLE PRECISION FUNCTION DLBETASLATEC (A, B)
C***BEGIN PROLOGUE  DLBETA
C***PURPOSE  Compute the natural logarithm of the complete Beta
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7B
C***TYPE      DOUBLE PRECISION (ALBETA-S, DLBETA-D, CLBETA-C)
C***KEYWORDS  FNLIB, LOGARITHM OF THE COMPLETE BETA FUNCTION,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLBETA(A,B) calculates the double precision natural logarithm of
C the complete beta function for double precision arguments
C A and B.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D9LGMC, DGAMMA, DLNGAM, DLNREL, XERDJS
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERDJS.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DLBETA
      DOUBLE PRECISION A, B, P, Q, CORR, SQ2PIL, D9LGMCSLATEC, 
     1                 DGAMMASLATEC, DLNGAMSLATEC, DLNRELSLATEC
      EXTERNAL DGAMMASLATEC
      SAVE SQ2PIL
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
C***FIRST EXECUTABLE STATEMENT  DLBETA
      P = MIN (A, B)
      Q = MAX (A, B)
C
      IF (P .LE. 0.D0) CALL XERDJS ('SLATEC/NCL', 'DLBETA',
     +   'BOTH ARGUMENTS MUST BE GT ZERO', 1, 2)
C
      IF (P.GE.10.D0) GO TO 30
      IF (Q.GE.10.D0) GO TO 20
C
C P AND Q ARE SMALL.
C
      DLBETASLATEC = 
     1      LOG(DGAMMASLATEC(P)*(DGAMMASLATEC(Q)/DGAMMASLATEC(P+Q)) )
      RETURN
C
C P IS SMALL, BUT Q IS BIG.
C
 20   CORR = D9LGMCSLATEC(Q) - D9LGMCSLATEC(P+Q)
      DLBETASLATEC = DLNGAMSLATEC(P) + CORR + P - P*LOG(P+Q)
     1  + (Q-0.5D0)*DLNRELSLATEC(-P/(P+Q))
      RETURN
C
C P AND Q ARE BIG.
C
 30   CORR = D9LGMCSLATEC(P) + D9LGMCSLATEC(Q) - D9LGMCSLATEC(P+Q)
      DLBETASLATEC = -0.5D0*LOG(Q)+ SQ2PIL+ CORR+ (P-0.5D0)*LOG(P/(P+Q))
     1  + Q*DLNRELSLATEC(-P/(P+Q))
      RETURN
C
      END
C===> DECK DLNGAM
      DOUBLE PRECISION FUNCTION DLNGAMSLATEC (X)
C***BEGIN PROLOGUE  DLNGAM
C***PURPOSE  Compute the logarithm of the absolute value of the Gamma
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (ALNGAM-S, DLNGAM-D, CLNGAM-C)
C***KEYWORDS  ABSOLUTE VALUE, COMPLETE GAMMA FUNCTION, FNLIB, LOGARITHM,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLNGAM(X) calculates the double precision logarithm of the
C absolute value of the Gamma function for double precision
C argument X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9LGMCSLATEC, DGAMMA, XERDJS
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERDJS.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DLNGAM
      DOUBLE PRECISION X, DXREL, PI, SINPIY, SQPI2L, SQ2PIL, XMAX,
     1  Y, DGAMMASLATEC, D9LGMCSLATEC, D1MACH, TEMP
      LOGICAL FIRST
      EXTERNAL DGAMMASLATEC
      SAVE SQ2PIL, SQPI2L, PI, XMAX, DXREL, FIRST
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA SQPI2L / +.2257913526 4472743236 3097614947 441 D+0    /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DLNGAM
      IF (FIRST) THEN
         TEMP = 1.D0/LOG(D1MACH(2))
         XMAX = TEMP*D1MACH(2)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS (X)
      IF (Y.GT.10.D0) GO TO 20
C
C LOG (ABS (DGAMMASLATEC(X)) ) FOR ABS(X) .LE. 10.0
C
      DLNGAMSLATEC = LOG (ABS (DGAMMASLATEC(X)) )
      RETURN
C
C LOG ( ABS (DGAMMASLATEC(X)) ) FOR ABS(X) .GT. 10.0
C
 20   IF (Y .GT. XMAX) CALL XERDJS ('SLATEC/NCL', 'DLNGAM',
     +   'ABS(X) SO BIG DLNGAM OVERFLOWS', 2, 2)
C
      IF (X.GT.0.D0) DLNGAMSLATEC = SQ2PIL+(X-0.5D0)*LOG(X)
     +                                     -X+D9LGMCSLATEC(Y)
      IF (X.GT.0.D0) RETURN
C
      SINPIY = ABS (SIN(PI*Y))
      IF (SINPIY .EQ. 0.D0) CALL XERDJS ('SLATEC/NCL', 'DLNGAM',
     +   'X IS A NEGATIVE INTEGER', 3, 2)
C
      IF (ABS((X-AINT(X-0.5D0))/X) .LT. DXREL) CALL XERDJS('SLATEC/NCL',
     +   'DLNGAM',
     +   'ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INTEGER',
     +   1, 1)
C
      DLNGAMSLATEC = SQPI2L+(X-0.5D0)*LOG(Y)-X
     1                      -LOG(SINPIY)-D9LGMCSLATEC(Y)
      RETURN
C
      END
C===> DECK DLNREL
      DOUBLE PRECISION FUNCTION DLNRELSLATEC (X)
C***BEGIN PROLOGUE  DLNREL
C***PURPOSE  Evaluate ln(1+X) accurate in the sense of relative error.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C4B
C***TYPE      DOUBLE PRECISION (ALNREL-S, DLNREL-D, CLNREL-C)
C***KEYWORDS  ELEMENTARY FUNCTIONS, FNLIB, LOGARITHM
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLNREL(X) calculates the double precision natural logarithm of
C (1.0+X) for double precision argument X.  This routine should
C be used when X is small and accurate to calculate the logarithm
C accurately (in the relative error sense) in the neighborhood
C of 1.0.
C
C Series for ALNR       on the interval -3.75000E-01 to  3.75000E-01
C                                        with weighted error   6.35E-32
C                                         log weighted error  31.20
C                               significant figures required  30.93
C                                    decimal places required  32.01
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERDJS
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERDJS.  (THJ)
C***END PROLOGUE  DLNREL
      DOUBLE PRECISION ALNRCS(43), X, XMIN,  DCSEVLSLATEC, D1MACH
      LOGICAL FIRST
      SAVE ALNRCS, NLNREL, XMIN, FIRST
      DATA ALNRCS(  1) / +.1037869356 2743769800 6862677190 98 D+1     /
      DATA ALNRCS(  2) / -.1336430150 4908918098 7660415531 33 D+0     /
      DATA ALNRCS(  3) / +.1940824913 5520563357 9261993747 50 D-1     /
      DATA ALNRCS(  4) / -.3010755112 7535777690 3765377765 92 D-2     /
      DATA ALNRCS(  5) / +.4869461479 7154850090 4563665091 37 D-3     /
      DATA ALNRCS(  6) / -.8105488189 3175356066 8099430086 22 D-4     /
      DATA ALNRCS(  7) / +.1377884779 9559524782 9382514960 59 D-4     /
      DATA ALNRCS(  8) / -.2380221089 4358970251 3699929149 35 D-5     /
      DATA ALNRCS(  9) / +.4164041621 3865183476 3918599019 89 D-6     /
      DATA ALNRCS( 10) / -.7359582837 8075994984 2668370319 98 D-7     /
      DATA ALNRCS( 11) / +.1311761187 6241674949 1522943450 11 D-7     /
      DATA ALNRCS( 12) / -.2354670931 7742425136 6960923301 75 D-8     /
      DATA ALNRCS( 13) / +.4252277327 6034997775 6380529625 67 D-9     /
      DATA ALNRCS( 14) / -.7719089413 4840796826 1081074933 00 D-10    /
      DATA ALNRCS( 15) / +.1407574648 1359069909 2153564721 91 D-10    /
      DATA ALNRCS( 16) / -.2576907205 8024680627 5370786275 84 D-11    /
      DATA ALNRCS( 17) / +.4734240666 6294421849 1543950059 38 D-12    /
      DATA ALNRCS( 18) / -.8724901267 4742641745 3012632926 75 D-13    /
      DATA ALNRCS( 19) / +.1612461490 2740551465 7398331191 15 D-13    /
      DATA ALNRCS( 20) / -.2987565201 5665773006 7107924168 15 D-14    /
      DATA ALNRCS( 21) / +.5548070120 9082887983 0413216972 79 D-15    /
      DATA ALNRCS( 22) / -.1032461915 8271569595 1413339619 32 D-15    /
      DATA ALNRCS( 23) / +.1925023920 3049851177 8785032448 68 D-16    /
      DATA ALNRCS( 24) / -.3595507346 5265150011 1897078442 66 D-17    /
      DATA ALNRCS( 25) / +.6726454253 7876857892 1945742267 73 D-18    /
      DATA ALNRCS( 26) / -.1260262416 8735219252 0824256375 46 D-18    /
      DATA ALNRCS( 27) / +.2364488440 8606210044 9161589555 19 D-19    /
      DATA ALNRCS( 28) / -.4441937705 0807936898 8783891797 33 D-20    /
      DATA ALNRCS( 29) / +.8354659446 4034259016 2412939946 66 D-21    /
      DATA ALNRCS( 30) / -.1573155941 6479562574 8992535210 66 D-21    /
      DATA ALNRCS( 31) / +.2965312874 0247422686 1543697066 66 D-22    /
      DATA ALNRCS( 32) / -.5594958348 1815947292 1560132266 66 D-23    /
      DATA ALNRCS( 33) / +.1056635426 8835681048 1872841386 66 D-23    /
      DATA ALNRCS( 34) / -.1997248368 0670204548 3149994666 66 D-24    /
      DATA ALNRCS( 35) / +.3778297781 8839361421 0498559999 99 D-25    /
      DATA ALNRCS( 36) / -.7153158688 9081740345 0381653333 33 D-26    /
      DATA ALNRCS( 37) / +.1355248846 3674213646 5020245333 33 D-26    /
      DATA ALNRCS( 38) / -.2569467304 8487567430 0798293333 33 D-27    /
      DATA ALNRCS( 39) / +.4874775606 6216949076 4595199999 99 D-28    /
      DATA ALNRCS( 40) / -.9254211253 0849715321 1323733333 33 D-29    /
      DATA ALNRCS( 41) / +.1757859784 1760239233 2697600000 00 D-29    /
      DATA ALNRCS( 42) / -.3341002667 7731010351 3770666666 66 D-30    /
      DATA ALNRCS( 43) / +.6353393618 0236187354 1802666666 66 D-31    /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DLNREL
      IF (FIRST) THEN
         NLNREL = INITDSSLATEC (ALNRCS, 43, 0.1*REAL(D1MACH(3)))
         XMIN = -1.0D0 + SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LE. (-1.D0)) CALL XERDJS ('SLATEC/NCL', 'DLNREL', 
     +   'X IS LE -1' , 2, 2)
      IF (X .LT. XMIN) CALL XERDJS ('SLATEC/NCL', 'DLNREL',
     +   'ANSWER LT HALF PRECISION BECAUSE X TOO NEAR -1', 1, 1)
C
      IF (ABS(X).LE.0.375D0) DLNRELSLATEC = X*(1.D0 -
     1  X*DCSEVLSLATEC (X/.375D0, ALNRCS, NLNREL))
C
      IF (ABS(X).GT.0.375D0) DLNRELSLATEC = LOG (1.0D0+X)
C
      RETURN
      END

C===> DECK INITDS
      FUNCTION INITDSSLATEC (OS, NOS, ETA)
C***BEGIN PROLOGUE  INITDS
C***PURPOSE  Determine the number of terms needed in an orthogonal
C            polynomial series so that it meets a specified accuracy.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      DOUBLE PRECISION (INITS-S, INITDS-D)
C***KEYWORDS  CHEBYSHEV, FNLIB, INITIALIZE, ORTHOGONAL POLYNOMIAL,
C             ORTHOGONAL SERIES, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Initialize the orthogonal series, represented by the array OS, so
C  that INITDS is the number of terms needed to insure the error is no
C  larger than ETA.  Ordinarily, ETA will be chosen to be one-tenth
C  machine precision.
C
C             Input Arguments --
C   OS     double precision array of NOS coefficients in an orthogonal
C          series.
C   NOS    number of coefficients in OS.
C   ETA    single precision scalar containing requested accuracy of
C          series.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERDJS
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891115  Modified error message.  (WRB)
C   891115  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERDJS.  (THJ)
C***END PROLOGUE  INITDS
      DOUBLE PRECISION OS(*)
C***FIRST EXECUTABLE STATEMENT  INITDS
      IF (NOS .LT. 1) CALL XERDJS ('SLATEC/NCL', 'INITDS',
     +   'Number of coefficients is less than 1', 2, 1)
C
      ERR = 0.
      DO 10 II = 1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(REAL(OS(I)))
        IF (ERR.GT.ETA) GO TO 20
   10 CONTINUE
C
   20 IF (I .EQ. NOS) CALL XERDJS ('SLATEC/NCL', 'INITDS',
     +   'Chebyshev series too short for specified accuracy', 1, 1)
      INITDSSLATEC = I
C
      RETURN
      END
C ++++++++++ DJS CREATED THIS SHORT ROUTINE***********
C ++++++++++ IT REPLACES "XERMSG"          ***********
      SUBROUTINE XERDJS (LIBRAR, SUBROU, MESSG, NERR, LEVEL)
      CHARACTER*(*) LIBRAR, SUBROU, MESSG
      INTEGER       NERR, LEVEL

      LENL = LEN(LIBRAR)
      LENS = LEN(SUBROU)
      LENM = LEN(MESSG)
      PRINT *,"==>", LIBRAR(1:LENL),": ",SUBROU(1:LENS),": "
     +             , MESSG(1:LENM), ": NERR=",NERR
     +             , ": LEVEL=",LEVEL

      RETURN
      END 

