C NCLFORTSTART
      SUBROUTINE CHISUB(P,DF,CHI)
C input
      DOUBLE PRECISION P,DF
C output
      DOUBLE PRECISION CHI
C NCLEND
C     SharedObject:   CHIIN::chisub(p,df,chi)  declare "chi" before call
C**********************************************************************
C      THIS IS AN INTERFACE THAT EMULATES IMSL's "CHIIN" FUNCTION
C
C     P <--> The integral from 0 to X of the chi-square
C            distribution.
C            Input range: [0, 1].
C                    REAL
C
C     DF <--> Degrees of freedom of the
C             chi-square distribution.
C             Input range: (0, +infinity).
C             Search range: [ 1E-300, 1E300]
C                    REAL
C
C
C     CHI<--> Upper limit of integration of the non-central
C            chi-square distribution.
C            Input range: [0, +infinity).
C            Search range: [0,1E300]
C                   REAL
C
C********************END INPUT ARGS***************************
C
C                   INTERNAL STUFF
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C               10 indicates error returned from cumgam.  See
C                  references in cdfgam
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C

c local [temporary]

      INTEGER WHICH,STATUS
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION Q,X,BOUND

C to mimic IMSL "chiin"
      WHICH = 2
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Already double-precision (DBLE)
      Q = 1.0d+0 - DBLE(P)
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
      CALL CDFCHI(WHICH,DBLE(P),Q,X,DBLE(DF),STATUS,BOUND)
C*PT*WARNING* Non-reversible tranformation (SNGL)

      CHI = X

      RETURN
      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION ALGDIV(A,B)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C
C     COMPUTATION OF LN(GAMMA(B)/GAMMA(A+B)) WHEN B .GE. 8
C
C                         --------
C
C     IN THIS ALGORITHM, DEL(X) IS THE FUNCTION DEFINED BY
C     LN(GAMMA(X)) = (X - 0.5)*LN(X) - X + 0.5*LN(2*PI) + DEL(X).
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION C,C0,C1,C2,C3,C4,C5,D,H,S11,S3,S5,S7,S9,T,U,V,W,
     +                 X,X2
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALNREL
      EXTERNAL ALNREL
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DLOG
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA C0/.833333333333333D-01/,C1/-.277777777760991D-02/,
     +     C2/.793650666825390D-03/,C3/-.595202931351870D-03/,
     +     C4/.837308034031215D-03/,C5/-.165322962780713D-02/
C     ..
C     .. Executable Statements ..
C------------------------
      IF (A.LE.B) GO TO 10
      H = B/A
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      C = 1.0D0/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
      X = H/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
      D = A + (B-0.5D0)
      GO TO 20

   10 H = A/B
C*PT*WARNING* Constant already double-precision
      C = H/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      X = 1.0D0/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
      D = B + (A-0.5D0)
C
C                SET SN = (1 - X**N)/(1 - X)
C
   20 X2 = X*X
C*PT*WARNING* Constant already double-precision
      S3 = 1.0D0 + (X+X2)
C*PT*WARNING* Constant already double-precision
      S5 = 1.0D0 + (X+X2*S3)
C*PT*WARNING* Constant already double-precision
      S7 = 1.0D0 + (X+X2*S5)
C*PT*WARNING* Constant already double-precision
      S9 = 1.0D0 + (X+X2*S7)
C*PT*WARNING* Constant already double-precision
      S11 = 1.0D0 + (X+X2*S9)
C*PT*WARNING* Constant already double-precision
C
C                SET W = DEL(B) - DEL(A + B)
C
      T = (1.0D0/B)**2
      W = ((((C5*S11*T+C4*S9)*T+C3*S7)*T+C2*S5)*T+C1*S3)*T + C0
      W = W* (C/B)
C
C                    COMBINE THE RESULTS
C
      U = D*ALNREL(A/B)
C*PT*WARNING* Constant already double-precision
      V = A* (DLOG(B)-1.0D0)
      IF (U.LE.V) GO TO 30
      ALGDIV = (W-V) - U
      RETURN

   30 ALGDIV = (W-U) - V
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION ALNGAM(X)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION ALNGAM(X)
C                 double precision LN of the GAMma function
C
C
C                              Function
C
C
C     Returns the natural logarithm of GAMMA(X).
C
C
C                              Arguments
C
C
C     X --> value at which scaled log gamma is to be returned
C                    X is DOUBLE PRECISION
C
C
C                              Method
C
C
C     If X .le. 6.0, then use recursion to get X below 3
C     then apply rational approximation number 5236 of
C     Hart et al, Computer Approximations, John Wiley and
C     Sons, NY, 1968.
C
C     If X .gt. 6.0, then use recursion to get X to at least 12 and
C     then use formula 5423 of the same source.
C
C**********************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION HLN2PI
C*PT*WARNING* Constant already double-precision
      PARAMETER (HLN2PI=0.91893853320467274178D0)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION OFFSET,PROD,XX
      INTEGER I,N
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION COEF(5),SCOEFD(4),SCOEFN(9)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DEVLPL
      EXTERNAL DEVLPL
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LOG,DBLE,INT
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA SCOEFN(1)/0.62003838007127258804D2/,
     +     SCOEFN(2)/0.36036772530024836321D2/,
     +     SCOEFN(3)/0.20782472531792126786D2/,
     +     SCOEFN(4)/0.6338067999387272343D1/,
     +     SCOEFN(5)/0.215994312846059073D1/,
     +     SCOEFN(6)/0.3980671310203570498D0/,
     +     SCOEFN(7)/0.1093115956710439502D0/,
     +     SCOEFN(8)/0.92381945590275995D-2/,
     +     SCOEFN(9)/0.29737866448101651D-2/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA SCOEFD(1)/0.62003838007126989331D2/,
     +     SCOEFD(2)/0.9822521104713994894D1/,
     +     SCOEFD(3)/-0.8906016659497461257D1/,
     +     SCOEFD(4)/0.1000000000000000000D1/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA COEF(1)/0.83333333333333023564D-1/,
     +     COEF(2)/-0.27777777768818808D-2/,
     +     COEF(3)/0.79365006754279D-3/,COEF(4)/-0.594997310889D-3/,
     +     COEF(5)/0.8065880899D-3/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
      IF (.NOT. (X.LE.6.0D0)) GO TO 70
C*PT*WARNING* Constant already double-precision
      PROD = 1.0D0
      XX = X
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (X.GT.3.0D0)) GO TO 30
C*PT*WARNING* Constant already double-precision
   10 IF (.NOT. (XX.GT.3.0D0)) GO TO 20
C*PT*WARNING* Constant already double-precision
      XX = XX - 1.0D0
      PROD = PROD*XX
      GO TO 10

   20 CONTINUE
C*PT*WARNING* Constant already double-precision
   30 IF (.NOT. (X.LT.2.0D0)) GO TO 60
C*PT*WARNING* Constant already double-precision
   40 IF (.NOT. (XX.LT.2.0D0)) GO TO 50
      PROD = PROD/XX
C*PT*WARNING* Constant already double-precision
      XX = XX + 1.0D0
      GO TO 40

   50 CONTINUE
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
   60 ALNGAM = DEVLPL(SCOEFN,9,XX-2.0D0)/DEVLPL(SCOEFD,4,XX-2.0D0)
C
C
C     COMPUTE RATIONAL APPROXIMATION TO GAMMA(X)
C
C
      ALNGAM = ALNGAM*PROD
      ALNGAM = LOG(ALNGAM)
      GO TO 110

   70 OFFSET = HLN2PI
C*PT*WARNING* Constant already double-precision
C
C
C     IF NECESSARY MAKE X AT LEAST 12 AND CARRY CORRECTION IN OFFSET
C
C
      N = INT(12.0D0-X)
      IF (.NOT. (N.GT.0)) GO TO 90
C*PT*WARNING* Constant already double-precision
      PROD = 1.0D0
      DO 80 I = 1,N
C*PT*WARNING* Already double-precision (DBLE)
          PROD = PROD* (X+DBLE(I-1))
   80 CONTINUE
      OFFSET = OFFSET - LOG(PROD)
C*PT*WARNING* Already double-precision (DBLE)
      XX = X + DBLE(N)
      GO TO 100

   90 XX = X
C*PT*WARNING* Constant already double-precision
C
C
C     COMPUTE POWER SERIES
C
C
  100 ALNGAM = DEVLPL(COEF,5,1.0D0/ (XX**2))/XX
C*PT*WARNING* Constant already double-precision
      ALNGAM = ALNGAM + OFFSET + (XX-0.5D0)*LOG(XX) - XX
  110 RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION ALNREL(A)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C            EVALUATION OF THE FUNCTION LN(1 + A)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION P1,P2,P3,Q1,Q2,Q3,T,T2,W,X
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DLOG
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA P1/-.129418923021993D+01/,P2/.405303492862024D+00/,
     +     P3/-.178874546012214D-01/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q1/-.162752256355323D+01/,Q2/.747811014037616D+00/,
     +     Q3/-.845104217945565D-01/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C--------------------------
      IF (ABS(A).GT.0.375D0) GO TO 10
C*PT*WARNING* Constant already double-precision
      T = A/ (A+2.0D0)
      T2 = T*T
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W = (((P3*T2+P2)*T2+P1)*T2+1.0D0)/ (((Q3*T2+Q2)*T2+Q1)*T2+1.0D0)
C*PT*WARNING* Constant already double-precision
      ALNREL = 2.0D0*T*W
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Already double-precision (DBLE)
C
   10 X = 1.D0 + DBLE(A)
      ALNREL = DLOG(X)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION APSER(A,B,X,EPS)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C     APSER YIELDS THE INCOMPLETE BETA RATIO I(SUB(1-X))(B,A) FOR
C     A .LE. MIN(EPS,EPS*B), B*X .LE. 1, AND X .LE. 0.5. USED WHEN
C     A IS VERY SMALL. USE ONLY IF ABOVE INEQUALITIES ARE SATISFIED.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,EPS,X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AJ,BX,C,G,J,S,T,TOL
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION PSI
      EXTERNAL PSI
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DLOG
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C--------------------
      DATA G/.577215664901533D0/
C     ..
C     .. Executable Statements ..
C--------------------
      BX = B*X
      T = X - BX
C*PT*WARNING* Constant already double-precision
      IF (B*EPS.GT.2.D-2) GO TO 10
      C = DLOG(X) + PSI(B) + G + T
      GO TO 20

   10 C = DLOG(BX) + G + T
C*PT*WARNING* Constant already double-precision
C
   20 TOL = 5.0D0*EPS*ABS(C)
C*PT*WARNING* Constant already double-precision
      J = 1.0D0
C*PT*WARNING* Constant already double-precision
      S = 0.0D0
C*PT*WARNING* Constant already double-precision
   30 J = J + 1.0D0
      T = T* (X-BX/J)
      AJ = T/J
      S = S + AJ
      IF (ABS(AJ).GT.TOL) GO TO 30
C
      APSER = -A* (C+S)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION BASYM(A,B,LAMBDA,EPS)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR IX(A,B) FOR LARGE A AND B.
C     LAMBDA = (A + B)*Y - B  AND EPS IS THE TOLERANCE USED.
C     IT IS ASSUMED THAT LAMBDA IS NONNEGATIVE AND THAT
C     A AND B ARE GREATER THAN OR EQUAL TO 15.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,EPS,LAMBDA
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BSUM,DSUM,E0,E1,F,H,H2,HN,J0,J1,R,R0,R1,S,SUM,T,
     +                 T0,T1,U,W,W0,Z,Z0,Z2,ZN,ZNM1
      INTEGER I,IM1,IMJ,J,M,MM1,MMJ,N,NP1,NUM
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION A0(21),B0(21),C(21),D(21)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION BCORR,ERFC1,RLOG1
      EXTERNAL BCORR,ERFC1,RLOG1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,EXP,SQRT
C     ..
C     .. Data statements ..
C------------------------
C     ****** NUM IS THE MAXIMUM VALUE THAT N CAN TAKE IN THE DO LOOP
C            ENDING AT STATEMENT 50. IT IS REQUIRED THAT NUM BE EVEN.
C            THE ARRAYS A0, B0, C, D HAVE DIMENSION NUM + 1.
C
C------------------------
C     E0 = 2/SQRT(PI)
C     E1 = 2**(-3/2)
C------------------------
      DATA NUM/20/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA E0/1.12837916709551D0/,E1/.353553390593274D0/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C------------------------
      BASYM = 0.0D0
      IF (A.GE.B) GO TO 10
      H = A/B
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      R0 = 1.0D0/ (1.0D0+H)
      R1 = (B-A)/B
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W0 = 1.0D0/SQRT(A* (1.0D0+H))
      GO TO 20

   10 H = B/A
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      R0 = 1.0D0/ (1.0D0+H)
      R1 = (B-A)/A
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W0 = 1.0D0/SQRT(B* (1.0D0+H))
C
   20 F = A*RLOG1(-LAMBDA/A) + B*RLOG1(LAMBDA/B)
      T = EXP(-F)
C*PT*WARNING* Constant already double-precision
      IF (T.EQ.0.0D0) RETURN
      Z0 = SQRT(F)
C*PT*WARNING* Constant already double-precision
      Z = 0.5D0* (Z0/E1)
      Z2 = F + F
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
      A0(1) = (2.0D0/3.0D0)*R1
C*PT*WARNING* Constant already double-precision
      C(1) = -0.5D0*A0(1)
      D(1) = -C(1)
C*PT*WARNING* Constant already double-precision
      J0 = (0.5D0/E0)*ERFC1(1,Z0)
      J1 = E1
      SUM = J0 + D(1)*W0*J1
C*PT*WARNING* Constant already double-precision
C
      S = 1.0D0
      H2 = H*H
C*PT*WARNING* Constant already double-precision
      HN = 1.0D0
      W = W0
      ZNM1 = Z
      ZN = Z2
      DO 70 N = 2,NUM,2
          HN = H2*HN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          A0(N) = 2.0D0*R0* (1.0D0+H*HN)/ (N+2.0D0)
          NP1 = N + 1
          S = S + HN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          A0(NP1) = 2.0D0*R1*S/ (N+3.0D0)
C
          DO 60 I = N,NP1
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
              R = -0.5D0* (I+1.0D0)
              B0(1) = R*A0(1)
              DO 40 M = 2,I
C*PT*WARNING* Constant already double-precision
                  BSUM = 0.0D0
                  MM1 = M - 1
                  DO 30 J = 1,MM1
                      MMJ = M - J
                      BSUM = BSUM + (J*R-MMJ)*A0(J)*B0(MMJ)
   30             CONTINUE
                  B0(M) = R*A0(M) + BSUM/M
   40         CONTINUE
C*PT*WARNING* Constant already double-precision
              C(I) = B0(I)/ (I+1.0D0)
C*PT*WARNING* Constant already double-precision
C
              DSUM = 0.0D0
              IM1 = I - 1
              DO 50 J = 1,IM1
                  IMJ = I - J
                  DSUM = DSUM + D(IMJ)*C(J)
   50         CONTINUE
              D(I) = - (DSUM+C(I))
   60     CONTINUE
C*PT*WARNING* Constant already double-precision
C
          J0 = E1*ZNM1 + (N-1.0D0)*J0
          J1 = E1*ZN + N*J1
          ZNM1 = Z2*ZNM1
          ZN = Z2*ZN
          W = W0*W
          T0 = D(N)*W*J0
          W = W0*W
          T1 = D(NP1)*W*J1
          SUM = SUM + (T0+T1)
          IF ((ABS(T0)+ABS(T1)).LE.EPS*SUM) GO TO 80
   70 CONTINUE
C
   80 U = EXP(-BCORR(A,B))
      BASYM = E0*T*U*SUM
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION BCORR(A0,B0)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C
C     EVALUATION OF  DEL(A0) + DEL(B0) - DEL(A0 + B0)  WHERE
C     LN(GAMMA(A)) = (A - 0.5)*LN(A) - A + 0.5*LN(2*PI) + DEL(A).
C     IT IS ASSUMED THAT A0 .GE. 8 AND B0 .GE. 8.
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A0,B0
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,C,C0,C1,C2,C3,C4,C5,H,S11,S3,S5,S7,S9,T,W,X,
     +                 X2
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DMAX1,DMIN1
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA C0/.833333333333333D-01/,C1/-.277777777760991D-02/,
     +     C2/.793650666825390D-03/,C3/-.595202931351870D-03/,
     +     C4/.837308034031215D-03/,C5/-.165322962780713D-02/
C     ..
C     .. Executable Statements ..
C------------------------
      A = DMIN1(A0,B0)
      B = DMAX1(A0,B0)
C
      H = A/B
C*PT*WARNING* Constant already double-precision
      C = H/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      X = 1.0D0/ (1.0D0+H)
      X2 = X*X
C*PT*WARNING* Constant already double-precision
C
C                SET SN = (1 - X**N)/(1 - X)
C
      S3 = 1.0D0 + (X+X2)
C*PT*WARNING* Constant already double-precision
      S5 = 1.0D0 + (X+X2*S3)
C*PT*WARNING* Constant already double-precision
      S7 = 1.0D0 + (X+X2*S5)
C*PT*WARNING* Constant already double-precision
      S9 = 1.0D0 + (X+X2*S7)
C*PT*WARNING* Constant already double-precision
      S11 = 1.0D0 + (X+X2*S9)
C*PT*WARNING* Constant already double-precision
C
C                SET W = DEL(B) - DEL(A + B)
C
      T = (1.0D0/B)**2
      W = ((((C5*S11*T+C4*S9)*T+C3*S7)*T+C2*S5)*T+C1*S3)*T + C0
      W = W* (C/B)
C*PT*WARNING* Constant already double-precision
C
C                   COMPUTE  DEL(A) + W
C
      T = (1.0D0/A)**2
      BCORR = (((((C5*T+C4)*T+C3)*T+C2)*T+C1)*T+C0)/A + W
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION BETALN(A0,B0)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C     EVALUATION OF THE LOGARITHM OF THE BETA FUNCTION
C-----------------------------------------------------------------------
C     E = 0.5*LN(2*PI)
C--------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A0,B0
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,C,E,H,U,V,W,Z
      INTEGER I,N
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALGDIV,ALNREL,BCORR,GAMLN,GSUMLN
      EXTERNAL ALGDIV,ALNREL,BCORR,GAMLN,GSUMLN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DLOG,DMAX1,DMIN1
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA E/.918938533204673D0/
C     ..
C     .. Executable Statements ..
C--------------------------
      A = DMIN1(A0,B0)
      B = DMAX1(A0,B0)
C*PT*WARNING* Constant already double-precision
      IF (A.GE.8.0D0) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (A.GE.1.0D0) GO TO 20
C*PT*WARNING* Constant already double-precision
C-----------------------------------------------------------------------
C                   PROCEDURE WHEN A .LT. 1
C-----------------------------------------------------------------------
      IF (B.GE.8.0D0) GO TO 10
      BETALN = GAMLN(A) + (GAMLN(B)-GAMLN(A+B))
      RETURN

   10 BETALN = GAMLN(A) + ALGDIV(A,B)
      RETURN
C*PT*WARNING* Constant already double-precision
C-----------------------------------------------------------------------
C                PROCEDURE WHEN 1 .LE. A .LT. 8
C-----------------------------------------------------------------------
   20 IF (A.GT.2.0D0) GO TO 40
C*PT*WARNING* Constant already double-precision
      IF (B.GT.2.0D0) GO TO 30
      BETALN = GAMLN(A) + GAMLN(B) - GSUMLN(A,B)
      RETURN
C*PT*WARNING* Constant already double-precision

   30 W = 0.0D0
C*PT*WARNING* Constant already double-precision
      IF (B.LT.8.0D0) GO TO 60
      BETALN = GAMLN(A) + ALGDIV(A,B)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C                REDUCTION OF A WHEN B .LE. 1000
C
   40 IF (B.GT.1000.0D0) GO TO 80
C*PT*WARNING* Constant already double-precision
      N = A - 1.0D0
C*PT*WARNING* Constant already double-precision
      W = 1.0D0
      DO 50 I = 1,N
C*PT*WARNING* Constant already double-precision
          A = A - 1.0D0
          H = A/B
C*PT*WARNING* Constant already double-precision
          W = W* (H/ (1.0D0+H))
   50 CONTINUE
      W = DLOG(W)
C*PT*WARNING* Constant already double-precision
      IF (B.LT.8.0D0) GO TO 60
      BETALN = W + GAMLN(A) + ALGDIV(A,B)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C                 REDUCTION OF B WHEN B .LT. 8
C
   60 N = B - 1.0D0
C*PT*WARNING* Constant already double-precision
      Z = 1.0D0
      DO 70 I = 1,N
C*PT*WARNING* Constant already double-precision
          B = B - 1.0D0
          Z = Z* (B/ (A+B))
   70 CONTINUE
      BETALN = W + DLOG(Z) + (GAMLN(A)+ (GAMLN(B)-GSUMLN(A,B)))
      RETURN
C*PT*WARNING* Constant already double-precision
C
C                REDUCTION OF A WHEN B .GT. 1000
C
   80 N = A - 1.0D0
C*PT*WARNING* Constant already double-precision
      W = 1.0D0
      DO 90 I = 1,N
C*PT*WARNING* Constant already double-precision
          A = A - 1.0D0
C*PT*WARNING* Constant already double-precision
          W = W* (A/ (1.0D0+A/B))
   90 CONTINUE
      BETALN = (DLOG(W)-N*DLOG(B)) + (GAMLN(A)+ALGDIV(A,B))
      RETURN
C-----------------------------------------------------------------------
C                   PROCEDURE WHEN A .GE. 8
C-----------------------------------------------------------------------
  100 W = BCORR(A,B)
      H = A/B
C*PT*WARNING* Constant already double-precision
      C = H/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
      U = - (A-0.5D0)*DLOG(C)
      V = B*ALNREL(H)
      IF (U.LE.V) GO TO 110
C*PT*WARNING* Constant already double-precision
      BETALN = (((-0.5D0*DLOG(B)+E)+W)-V) - U
      RETURN
C*PT*WARNING* Constant already double-precision

  110 BETALN = (((-0.5D0*DLOG(B)+E)+W)-U) - V
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION BFRAC(A,B,X,Y,LAMBDA,EPS)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C     CONTINUED FRACTION EXPANSION FOR IX(A,B) WHEN A,B .GT. 1.
C     IT IS ASSUMED THAT  LAMBDA = (A + B)*Y - B.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,EPS,LAMBDA,X,Y
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ALPHA,AN,ANP1,BETA,BN,BNP1,C,C0,C1,E,N,P,R,R0,S,
     +                 T,W,YP1
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION BRCOMP
      EXTERNAL BRCOMP
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     ..
C     .. Executable Statements ..
C--------------------
      BFRAC = BRCOMP(A,B,X,Y)
C*PT*WARNING* Constant already double-precision
      IF (BFRAC.EQ.0.0D0) RETURN
C*PT*WARNING* Constant already double-precision
C
      C = 1.0D0 + LAMBDA
      C0 = B/A
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      C1 = 1.0D0 + 1.0D0/A
C*PT*WARNING* Constant already double-precision
      YP1 = Y + 1.0D0
C*PT*WARNING* Constant already double-precision
C
      N = 0.0D0
C*PT*WARNING* Constant already double-precision
      P = 1.0D0
C*PT*WARNING* Constant already double-precision
      S = A + 1.0D0
C*PT*WARNING* Constant already double-precision
      AN = 0.0D0
C*PT*WARNING* Constant already double-precision
      BN = 1.0D0
C*PT*WARNING* Constant already double-precision
      ANP1 = 1.0D0
      BNP1 = C/C1
      R = C1/C
C*PT*WARNING* Constant already double-precision
C
C        CONTINUED FRACTION CALCULATION
C
   10 N = N + 1.0D0
      T = N/A
      W = N* (B-N)*X
      E = A/S
      ALPHA = (P* (P+C0)*E*E)* (W*X)
C*PT*WARNING* Constant already double-precision
      E = (1.0D0+T)/ (C1+T+T)
      BETA = N + W/S + E* (C+N*YP1)
C*PT*WARNING* Constant already double-precision
      P = 1.0D0 + T
C*PT*WARNING* Constant already double-precision
      S = S + 2.0D0
C
C        UPDATE AN, BN, ANP1, AND BNP1
C
      T = ALPHA*AN + BETA*ANP1
      AN = ANP1
      ANP1 = T
      T = ALPHA*BN + BETA*BNP1
      BN = BNP1
      BNP1 = T
C
      R0 = R
      R = ANP1/BNP1
      IF (ABS(R-R0).LE.EPS*R) GO TO 20
C
C        RESCALE AN, BN, ANP1, AND BNP1
C
      AN = AN/BNP1
      BN = BN/BNP1
      ANP1 = R
C*PT*WARNING* Constant already double-precision
      BNP1 = 1.0D0
      GO TO 10
C
C                 TERMINATION
C
   20 BFRAC = BFRAC*R
      RETURN

      END
      SUBROUTINE BGRAT(A,B,X,Y,W,EPS,IERR)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR IX(A,B) WHEN A IS LARGER THAN B.
C     THE RESULT OF THE EXPANSION IS ADDED TO W. IT IS ASSUMED
C     THAT A .GE. 15 AND B .LE. 1.  EPS IS THE TOLERANCE USED.
C     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,EPS,W,X,Y
      INTEGER IERR
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BM1,BP2N,CN,COEF,DJ,J,L,LNX,N2,NU,P,Q,R,S,SUM,T,
     +                 T2,U,V,Z
      INTEGER I,N,NM1
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION C(30),D(30)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALGDIV,ALNREL,GAM1
      EXTERNAL ALGDIV,ALNREL,GAM1
C     ..
C     .. External Subroutines ..
      EXTERNAL GRAT1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DLOG,EXP
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C
      BM1 = (B-0.5D0) - 0.5D0
C*PT*WARNING* Constant already double-precision
      NU = A + 0.5D0*BM1
C*PT*WARNING* Constant already double-precision
      IF (Y.GT.0.375D0) GO TO 10
      LNX = ALNREL(-Y)
      GO TO 20

   10 LNX = DLOG(X)
   20 Z = -NU*LNX
C*PT*WARNING* Constant already double-precision
      IF (B*Z.EQ.0.0D0) GO TO 70
C*PT*WARNING* Constant already double-precision
C
C                 COMPUTATION OF THE EXPANSION
C                 SET R = EXP(-Z)*Z**B/GAMMA(B)
C
      R = B* (1.0D0+GAM1(B))*EXP(B*DLOG(Z))
C*PT*WARNING* Constant already double-precision
      R = R*EXP(A*LNX)*EXP(0.5D0*BM1*LNX)
      U = ALGDIV(B,A) + B*DLOG(NU)
      U = R*EXP(-U)
C*PT*WARNING* Constant already double-precision
      IF (U.EQ.0.0D0) GO TO 70
      CALL GRAT1(B,Z,R,P,Q,EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
      V = 0.25D0* (1.0D0/NU)**2
C*PT*WARNING* Constant already double-precision
      T2 = 0.25D0*LNX*LNX
      L = W/U
      J = Q/R
      SUM = J
C*PT*WARNING* Constant already double-precision
      T = 1.0D0
C*PT*WARNING* Constant already double-precision
      CN = 1.0D0
C*PT*WARNING* Constant already double-precision
      N2 = 0.0D0
      DO 50 N = 1,30
          BP2N = B + N2
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          J = (BP2N* (BP2N+1.0D0)*J+ (Z+BP2N+1.0D0)*T)*V
C*PT*WARNING* Constant already double-precision
          N2 = N2 + 2.0D0
          T = T*T2
C*PT*WARNING* Constant already double-precision
          CN = CN/ (N2* (N2+1.0D0))
          C(N) = CN
C*PT*WARNING* Constant already double-precision
          S = 0.0D0
          IF (N.EQ.1) GO TO 40
          NM1 = N - 1
          COEF = B - N
          DO 30 I = 1,NM1
              S = S + COEF*C(I)*D(N-I)
              COEF = COEF + B
   30     CONTINUE
   40     D(N) = BM1*CN + S/N
          DJ = D(N)*J
          SUM = SUM + DJ
C*PT*WARNING* Constant already double-precision
          IF (SUM.LE.0.0D0) GO TO 70
          IF (ABS(DJ).LE.EPS* (SUM+L)) GO TO 60
   50 CONTINUE
C
C                    ADD THE RESULTS TO W
C
   60 IERR = 0
      W = W + U*SUM
      RETURN
C
C               THE EXPANSION CANNOT BE COMPUTED
C
   70 IERR = 1
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION BPSER(A,B,X,EPS)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C     POWER SERIES EXPANSION FOR EVALUATING IX(A,B) WHEN B .LE. 1
C     OR B*X .LE. 0.7.  EPS IS THE TOLERANCE USED.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,EPS,X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A0,APB,B0,C,N,SUM,T,TOL,U,W,Z
      INTEGER I,M
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALGDIV,BETALN,GAM1,GAMLN1
      EXTERNAL ALGDIV,BETALN,GAM1,GAMLN1
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DLOG,DMAX1,DMIN1,EXP
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C
      BPSER = 0.0D0
C*PT*WARNING* Constant already double-precision
      IF (X.EQ.0.0D0) RETURN
C-----------------------------------------------------------------------
C            COMPUTE THE FACTOR X**A/(A*BETA(A,B))
C-----------------------------------------------------------------------
      A0 = DMIN1(A,B)
C*PT*WARNING* Constant already double-precision
      IF (A0.LT.1.0D0) GO TO 10
      Z = A*DLOG(X) - BETALN(A,B)
      BPSER = EXP(Z)/A
      GO TO 100

   10 B0 = DMAX1(A,B)
C*PT*WARNING* Constant already double-precision
      IF (B0.GE.8.0D0) GO TO 90
C*PT*WARNING* Constant already double-precision
      IF (B0.GT.1.0D0) GO TO 40
C
C            PROCEDURE FOR A0 .LT. 1 AND B0 .LE. 1
C
      BPSER = X**A
C*PT*WARNING* Constant already double-precision
      IF (BPSER.EQ.0.0D0) RETURN
C
      APB = A + B
C*PT*WARNING* Constant already double-precision
      IF (APB.GT.1.0D0) GO TO 20
C*PT*WARNING* Constant already double-precision
      Z = 1.0D0 + GAM1(APB)
      GO TO 30
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision

   20 U = DBLE(A) + DBLE(B) - 1.D0
C*PT*WARNING* Constant already double-precision
      Z = (1.0D0+GAM1(U))/APB
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
   30 C = (1.0D0+GAM1(A))* (1.0D0+GAM1(B))/Z
      BPSER = BPSER*C* (B/APB)
      GO TO 100
C
C         PROCEDURE FOR A0 .LT. 1 AND 1 .LT. B0 .LT. 8
C
   40 U = GAMLN1(A0)
C*PT*WARNING* Constant already double-precision
      M = B0 - 1.0D0
      IF (M.LT.1) GO TO 60
C*PT*WARNING* Constant already double-precision
      C = 1.0D0
      DO 50 I = 1,M
C*PT*WARNING* Constant already double-precision
          B0 = B0 - 1.0D0
          C = C* (B0/ (A0+B0))
   50 CONTINUE
      U = DLOG(C) + U
C
   60 Z = A*DLOG(X) - U
C*PT*WARNING* Constant already double-precision
      B0 = B0 - 1.0D0
      APB = A0 + B0
C*PT*WARNING* Constant already double-precision
      IF (APB.GT.1.0D0) GO TO 70
C*PT*WARNING* Constant already double-precision
      T = 1.0D0 + GAM1(APB)
      GO TO 80
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision

   70 U = DBLE(A0) + DBLE(B0) - 1.D0
C*PT*WARNING* Constant already double-precision
      T = (1.0D0+GAM1(U))/APB
C*PT*WARNING* Constant already double-precision
   80 BPSER = EXP(Z)* (A0/A)* (1.0D0+GAM1(B0))/T
      GO TO 100
C
C            PROCEDURE FOR A0 .LT. 1 AND B0 .GE. 8
C
   90 U = GAMLN1(A0) + ALGDIV(A0,B0)
      Z = A*DLOG(X) - U
      BPSER = (A0/A)*EXP(Z)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
  100 IF (BPSER.EQ.0.0D0 .OR. A.LE.0.1D0*EPS) RETURN
C*PT*WARNING* Constant already double-precision
C-----------------------------------------------------------------------
C                     COMPUTE THE SERIES
C-----------------------------------------------------------------------
      SUM = 0.0D0
C*PT*WARNING* Constant already double-precision
      N = 0.0D0
C*PT*WARNING* Constant already double-precision
      C = 1.0D0
      TOL = EPS/A
C*PT*WARNING* Constant already double-precision
  110 N = N + 1.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      C = C* (0.5D0+ (0.5D0-B/N))*X
      W = C/ (A+N)
      SUM = SUM + W
      IF (ABS(W).GT.TOL) GO TO 110
C*PT*WARNING* Constant already double-precision
      BPSER = BPSER* (1.0D0+A*SUM)
      RETURN

      END
      SUBROUTINE BRATIO(A,B,X,Y,W,W1,IERR)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C
C            EVALUATION OF THE INCOMPLETE BETA FUNCTION IX(A,B)
C
C                     --------------------
C
C     IT IS ASSUMED THAT A AND B ARE NONNEGATIVE, AND THAT X .LE. 1
C     AND Y = 1 - X.  BRATIO ASSIGNS W AND W1 THE VALUES
C
C                      W  = IX(A,B)
C                      W1 = 1 - IX(A,B)
C
C     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS.
C     IF NO INPUT ERRORS ARE DETECTED THEN IERR IS SET TO 0 AND
C     W AND W1 ARE COMPUTED. OTHERWISE, IF AN ERROR IS DETECTED,
C     THEN W AND W1 ARE ASSIGNED THE VALUE 0 AND IERR IS SET TO
C     ONE OF THE FOLLOWING VALUES ...
C
C        IERR = 1  IF A OR B IS NEGATIVE
C        IERR = 2  IF A = B = 0
C        IERR = 3  IF X .LT. 0 OR X .GT. 1
C        IERR = 4  IF Y .LT. 0 OR Y .GT. 1
C        IERR = 5  IF X + Y .NE. 1
C        IERR = 6  IF X = A = 0
C        IERR = 7  IF Y = B = 0
C
C--------------------
C     WRITTEN BY ALFRED H. MORRIS, JR.
C        NAVAL SURFACE WARFARE CENTER
C        DAHLGREN, VIRGINIA
C     REVISED ... NOV 1991
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,W,W1,X,Y
      INTEGER IERR
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A0,B0,EPS,LAMBDA,T,X0,Y0,Z
      INTEGER IERR1,IND,N
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION APSER,BASYM,BFRAC,BPSER,BUP,FPSER,SPMPAR
      EXTERNAL APSER,BASYM,BFRAC,BPSER,BUP,FPSER,SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL BGRAT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DMAX1,DMIN1
C     ..
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     ****** EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE SMALLEST
C            FLOATING POINT NUMBER FOR WHICH 1.0 + EPS .GT. 1.0
C
      EPS = SPMPAR(1)
C*PT*WARNING* Constant already double-precision
C
C-----------------------------------------------------------------------
      W = 0.0D0
C*PT*WARNING* Constant already double-precision
      W1 = 0.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (A.LT.0.0D0 .OR. B.LT.0.0D0) GO TO 270
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (A.EQ.0.0D0 .AND. B.EQ.0.0D0) GO TO 280
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (X.LT.0.0D0 .OR. X.GT.1.0D0) GO TO 290
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (Y.LT.0.0D0 .OR. Y.GT.1.0D0) GO TO 300
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      Z = ((X+Y)-0.5D0) - 0.5D0
C*PT*WARNING* Constant already double-precision
      IF (ABS(Z).GT.3.0D0*EPS) GO TO 310
C
      IERR = 0
C*PT*WARNING* Constant already double-precision
      IF (X.EQ.0.0D0) GO TO 210
C*PT*WARNING* Constant already double-precision
      IF (Y.EQ.0.0D0) GO TO 230
C*PT*WARNING* Constant already double-precision
      IF (A.EQ.0.0D0) GO TO 240
C*PT*WARNING* Constant already double-precision
      IF (B.EQ.0.0D0) GO TO 220
C*PT*WARNING* Constant already double-precision
C
      EPS = DMAX1(EPS,1.D-15)
C*PT*WARNING* Constant already double-precision
      IF (DMAX1(A,B).LT.1.D-3*EPS) GO TO 260
C
      IND = 0
      A0 = A
      B0 = B
      X0 = X
      Y0 = Y
C*PT*WARNING* Constant already double-precision
      IF (DMIN1(A0,B0).GT.1.0D0) GO TO 40
C*PT*WARNING* Constant already double-precision
C
C             PROCEDURE FOR A0 .LE. 1 OR B0 .LE. 1
C
      IF (X.LE.0.5D0) GO TO 10
      IND = 1
      A0 = B
      B0 = A
      X0 = Y
      Y0 = X
C
   10 IF (B0.LT.DMIN1(EPS,EPS*A0)) GO TO 90
C*PT*WARNING* Constant already double-precision
      IF (A0.LT.DMIN1(EPS,EPS*B0) .AND. B0*X0.LE.1.0D0) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (DMAX1(A0,B0).GT.1.0D0) GO TO 20
C*PT*WARNING* Constant already double-precision
      IF (A0.GE.DMIN1(0.2D0,B0)) GO TO 110
C*PT*WARNING* Constant already double-precision
      IF (X0**A0.LE.0.9D0) GO TO 110
C*PT*WARNING* Constant already double-precision
      IF (X0.GE.0.3D0) GO TO 120
      N = 20
      GO TO 140
C*PT*WARNING* Constant already double-precision
C
   20 IF (B0.LE.1.0D0) GO TO 110
C*PT*WARNING* Constant already double-precision
      IF (X0.GE.0.3D0) GO TO 120
C*PT*WARNING* Constant already double-precision
      IF (X0.GE.0.1D0) GO TO 30
C*PT*WARNING* Constant already double-precision
      IF ((X0*B0)**A0.LE.0.7D0) GO TO 110
C*PT*WARNING* Constant already double-precision
   30 IF (B0.GT.15.0D0) GO TO 150
      N = 20
      GO TO 140
C
C             PROCEDURE FOR A0 .GT. 1 AND B0 .GT. 1
C
   40 IF (A.GT.B) GO TO 50
      LAMBDA = A - (A+B)*X
      GO TO 60

   50 LAMBDA = (A+B)*Y - B
C*PT*WARNING* Constant already double-precision
   60 IF (LAMBDA.GE.0.0D0) GO TO 70
      IND = 1
      A0 = B
      B0 = A
      X0 = Y
      Y0 = X
      LAMBDA = ABS(LAMBDA)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
   70 IF (B0.LT.40.0D0 .AND. B0*X0.LE.0.7D0) GO TO 110
C*PT*WARNING* Constant already double-precision
      IF (B0.LT.40.0D0) GO TO 160
      IF (A0.GT.B0) GO TO 80
C*PT*WARNING* Constant already double-precision
      IF (A0.LE.100.0D0) GO TO 130
C*PT*WARNING* Constant already double-precision
      IF (LAMBDA.GT.0.03D0*A0) GO TO 130
      GO TO 200
C*PT*WARNING* Constant already double-precision

   80 IF (B0.LE.100.0D0) GO TO 130
C*PT*WARNING* Constant already double-precision
      IF (LAMBDA.GT.0.03D0*B0) GO TO 130
      GO TO 200
C
C            EVALUATION OF THE APPROPRIATE ALGORITHM
C
   90 W = FPSER(A0,B0,X0,EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W1 = 0.5D0 + (0.5D0-W)
      GO TO 250
C
  100 W1 = APSER(A0,B0,X0,EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W = 0.5D0 + (0.5D0-W1)
      GO TO 250
C
  110 W = BPSER(A0,B0,X0,EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W1 = 0.5D0 + (0.5D0-W)
      GO TO 250
C
  120 W1 = BPSER(B0,A0,Y0,EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W = 0.5D0 + (0.5D0-W1)
      GO TO 250
C*PT*WARNING* Constant already double-precision
C
  130 W = BFRAC(A0,B0,X0,Y0,LAMBDA,15.0D0*EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W1 = 0.5D0 + (0.5D0-W)
      GO TO 250
C
  140 W1 = BUP(B0,A0,Y0,X0,N,EPS)
      B0 = B0 + N
C*PT*WARNING* Constant already double-precision
  150 CALL BGRAT(B0,A0,Y0,X0,W1,15.0D0*EPS,IERR1)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W = 0.5D0 + (0.5D0-W1)
      GO TO 250
C
  160 N = B0
      B0 = B0 - N
C*PT*WARNING* Constant already double-precision
      IF (B0.NE.0.0D0) GO TO 170
      N = N - 1
C*PT*WARNING* Constant already double-precision
      B0 = 1.0D0
  170 W = BUP(B0,A0,Y0,X0,N,EPS)
C*PT*WARNING* Constant already double-precision
      IF (X0.GT.0.7D0) GO TO 180
      W = W + BPSER(A0,B0,X0,EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W1 = 0.5D0 + (0.5D0-W)
      GO TO 250
C*PT*WARNING* Constant already double-precision
C
  180 IF (A0.GT.15.0D0) GO TO 190
      N = 20
      W = W + BUP(A0,B0,X0,Y0,N,EPS)
      A0 = A0 + N
C*PT*WARNING* Constant already double-precision
  190 CALL BGRAT(A0,B0,X0,Y0,W,15.0D0*EPS,IERR1)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W1 = 0.5D0 + (0.5D0-W)
      GO TO 250
C*PT*WARNING* Constant already double-precision
C
  200 W = BASYM(A0,B0,LAMBDA,100.0D0*EPS)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W1 = 0.5D0 + (0.5D0-W)
      GO TO 250
C*PT*WARNING* Constant already double-precision
C
C               TERMINATION OF THE PROCEDURE
C
  210 IF (A.EQ.0.0D0) GO TO 320
C*PT*WARNING* Constant already double-precision
  220 W = 0.0D0
C*PT*WARNING* Constant already double-precision
      W1 = 1.0D0
      RETURN
C*PT*WARNING* Constant already double-precision
C
  230 IF (B.EQ.0.0D0) GO TO 330
C*PT*WARNING* Constant already double-precision
  240 W = 1.0D0
C*PT*WARNING* Constant already double-precision
      W1 = 0.0D0
      RETURN
C
  250 IF (IND.EQ.0) RETURN
      T = W
      W = W1
      W1 = T
      RETURN
C
C           PROCEDURE FOR A AND B .LT. 1.E-3*EPS
C
  260 W = B/ (A+B)
      W1 = A/ (A+B)
      RETURN
C
C                       ERROR RETURN
C
  270 IERR = 1
      RETURN

  280 IERR = 2
      RETURN

  290 IERR = 3
      RETURN

  300 IERR = 4
      RETURN

  310 IERR = 5
      RETURN

  320 IERR = 6
      RETURN

  330 IERR = 7
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION BRCMP1(MU,A,B,X,Y)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C          EVALUATION OF  EXP(MU) * (X**A*Y**B/BETA(A,B))
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,X,Y
      INTEGER MU
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A0,APB,B0,C,CONST,E,H,LAMBDA,LNX,LNY,T,U,V,X0,Y0,
     +                 Z
      INTEGER I,N
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALGDIV,ALNREL,BCORR,BETALN,ESUM,GAM1,GAMLN1,RLOG1
      EXTERNAL ALGDIV,ALNREL,BCORR,BETALN,ESUM,GAM1,GAMLN1,RLOG1
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DLOG,DMAX1,DMIN1,EXP,SQRT
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C-----------------
C     CONST = 1/SQRT(2*PI)
C-----------------
      DATA CONST/.398942280401433D0/
C     ..
C     .. Executable Statements ..
C
      A0 = DMIN1(A,B)
C*PT*WARNING* Constant already double-precision
      IF (A0.GE.8.0D0) GO TO 130
C*PT*WARNING* Constant already double-precision
C
      IF (X.GT.0.375D0) GO TO 10
      LNX = DLOG(X)
      LNY = ALNREL(-X)
      GO TO 30
C*PT*WARNING* Constant already double-precision

   10 IF (Y.GT.0.375D0) GO TO 20
      LNX = ALNREL(-Y)
      LNY = DLOG(Y)
      GO TO 30

   20 LNX = DLOG(X)
      LNY = DLOG(Y)
C
   30 Z = A*LNX + B*LNY
C*PT*WARNING* Constant already double-precision
      IF (A0.LT.1.0D0) GO TO 40
      Z = Z - BETALN(A,B)
      BRCMP1 = ESUM(MU,Z)
      RETURN
C-----------------------------------------------------------------------
C              PROCEDURE FOR A .LT. 1 OR B .LT. 1
C-----------------------------------------------------------------------
   40 B0 = DMAX1(A,B)
C*PT*WARNING* Constant already double-precision
      IF (B0.GE.8.0D0) GO TO 120
C*PT*WARNING* Constant already double-precision
      IF (B0.GT.1.0D0) GO TO 70
C
C                   ALGORITHM FOR B0 .LE. 1
C
      BRCMP1 = ESUM(MU,Z)
C*PT*WARNING* Constant already double-precision
      IF (BRCMP1.EQ.0.0D0) RETURN
C
      APB = A + B
C*PT*WARNING* Constant already double-precision
      IF (APB.GT.1.0D0) GO TO 50
C*PT*WARNING* Constant already double-precision
      Z = 1.0D0 + GAM1(APB)
      GO TO 60
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision

   50 U = DBLE(A) + DBLE(B) - 1.D0
C*PT*WARNING* Constant already double-precision
      Z = (1.0D0+GAM1(U))/APB
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
   60 C = (1.0D0+GAM1(A))* (1.0D0+GAM1(B))/Z
C*PT*WARNING* Constant already double-precision
      BRCMP1 = BRCMP1* (A0*C)/ (1.0D0+A0/B0)
      RETURN
C
C                ALGORITHM FOR 1 .LT. B0 .LT. 8
C
   70 U = GAMLN1(A0)
C*PT*WARNING* Constant already double-precision
      N = B0 - 1.0D0
      IF (N.LT.1) GO TO 90
C*PT*WARNING* Constant already double-precision
      C = 1.0D0
      DO 80 I = 1,N
C*PT*WARNING* Constant already double-precision
          B0 = B0 - 1.0D0
          C = C* (B0/ (A0+B0))
   80 CONTINUE
      U = DLOG(C) + U
C
   90 Z = Z - U
C*PT*WARNING* Constant already double-precision
      B0 = B0 - 1.0D0
      APB = A0 + B0
C*PT*WARNING* Constant already double-precision
      IF (APB.GT.1.0D0) GO TO 100
C*PT*WARNING* Constant already double-precision
      T = 1.0D0 + GAM1(APB)
      GO TO 110
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision

  100 U = DBLE(A0) + DBLE(B0) - 1.D0
C*PT*WARNING* Constant already double-precision
      T = (1.0D0+GAM1(U))/APB
C*PT*WARNING* Constant already double-precision
  110 BRCMP1 = A0*ESUM(MU,Z)* (1.0D0+GAM1(B0))/T
      RETURN
C
C                   ALGORITHM FOR B0 .GE. 8
C
  120 U = GAMLN1(A0) + ALGDIV(A0,B0)
      BRCMP1 = A0*ESUM(MU,Z-U)
      RETURN
C-----------------------------------------------------------------------
C              PROCEDURE FOR A .GE. 8 AND B .GE. 8
C-----------------------------------------------------------------------
  130 IF (A.GT.B) GO TO 140
      H = A/B
C*PT*WARNING* Constant already double-precision
      X0 = H/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      Y0 = 1.0D0/ (1.0D0+H)
      LAMBDA = A - (A+B)*X
      GO TO 150

  140 H = B/A
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      X0 = 1.0D0/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
      Y0 = H/ (1.0D0+H)
      LAMBDA = (A+B)*Y - B
C
  150 E = -LAMBDA/A
C*PT*WARNING* Constant already double-precision
      IF (ABS(E).GT.0.6D0) GO TO 160
      U = RLOG1(E)
      GO TO 170

  160 U = E - DLOG(X/X0)
C
  170 E = LAMBDA/B
C*PT*WARNING* Constant already double-precision
      IF (ABS(E).GT.0.6D0) GO TO 180
      V = RLOG1(E)
      GO TO 190

  180 V = E - DLOG(Y/Y0)
C
  190 Z = ESUM(MU,- (A*U+B*V))
      BRCMP1 = CONST*SQRT(B*X0)*Z*EXP(-BCORR(A,B))
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION BRCOMP(A,B,X,Y)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C               EVALUATION OF X**A*Y**B/BETA(A,B)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,X,Y
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A0,APB,B0,C,CONST,E,H,LAMBDA,LNX,LNY,T,U,V,X0,Y0,
     +                 Z
      INTEGER I,N
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALGDIV,ALNREL,BCORR,BETALN,GAM1,GAMLN1,RLOG1
      EXTERNAL ALGDIV,ALNREL,BCORR,BETALN,GAM1,GAMLN1,RLOG1
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DLOG,DMAX1,DMIN1,EXP,SQRT
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C-----------------
C     CONST = 1/SQRT(2*PI)
C-----------------
      DATA CONST/.398942280401433D0/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C
      BRCOMP = 0.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (X.EQ.0.0D0 .OR. Y.EQ.0.0D0) RETURN
      A0 = DMIN1(A,B)
C*PT*WARNING* Constant already double-precision
      IF (A0.GE.8.0D0) GO TO 130
C*PT*WARNING* Constant already double-precision
C
      IF (X.GT.0.375D0) GO TO 10
      LNX = DLOG(X)
      LNY = ALNREL(-X)
      GO TO 30
C*PT*WARNING* Constant already double-precision

   10 IF (Y.GT.0.375D0) GO TO 20
      LNX = ALNREL(-Y)
      LNY = DLOG(Y)
      GO TO 30

   20 LNX = DLOG(X)
      LNY = DLOG(Y)
C
   30 Z = A*LNX + B*LNY
C*PT*WARNING* Constant already double-precision
      IF (A0.LT.1.0D0) GO TO 40
      Z = Z - BETALN(A,B)
      BRCOMP = EXP(Z)
      RETURN
C-----------------------------------------------------------------------
C              PROCEDURE FOR A .LT. 1 OR B .LT. 1
C-----------------------------------------------------------------------
   40 B0 = DMAX1(A,B)
C*PT*WARNING* Constant already double-precision
      IF (B0.GE.8.0D0) GO TO 120
C*PT*WARNING* Constant already double-precision
      IF (B0.GT.1.0D0) GO TO 70
C
C                   ALGORITHM FOR B0 .LE. 1
C
      BRCOMP = EXP(Z)
C*PT*WARNING* Constant already double-precision
      IF (BRCOMP.EQ.0.0D0) RETURN
C
      APB = A + B
C*PT*WARNING* Constant already double-precision
      IF (APB.GT.1.0D0) GO TO 50
C*PT*WARNING* Constant already double-precision
      Z = 1.0D0 + GAM1(APB)
      GO TO 60
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision

   50 U = DBLE(A) + DBLE(B) - 1.D0
C*PT*WARNING* Constant already double-precision
      Z = (1.0D0+GAM1(U))/APB
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
   60 C = (1.0D0+GAM1(A))* (1.0D0+GAM1(B))/Z
C*PT*WARNING* Constant already double-precision
      BRCOMP = BRCOMP* (A0*C)/ (1.0D0+A0/B0)
      RETURN
C
C                ALGORITHM FOR 1 .LT. B0 .LT. 8
C
   70 U = GAMLN1(A0)
C*PT*WARNING* Constant already double-precision
      N = B0 - 1.0D0
      IF (N.LT.1) GO TO 90
C*PT*WARNING* Constant already double-precision
      C = 1.0D0
      DO 80 I = 1,N
C*PT*WARNING* Constant already double-precision
          B0 = B0 - 1.0D0
          C = C* (B0/ (A0+B0))
   80 CONTINUE
      U = DLOG(C) + U
C
   90 Z = Z - U
C*PT*WARNING* Constant already double-precision
      B0 = B0 - 1.0D0
      APB = A0 + B0
C*PT*WARNING* Constant already double-precision
      IF (APB.GT.1.0D0) GO TO 100
C*PT*WARNING* Constant already double-precision
      T = 1.0D0 + GAM1(APB)
      GO TO 110
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision

  100 U = DBLE(A0) + DBLE(B0) - 1.D0
C*PT*WARNING* Constant already double-precision
      T = (1.0D0+GAM1(U))/APB
C*PT*WARNING* Constant already double-precision
  110 BRCOMP = A0*EXP(Z)* (1.0D0+GAM1(B0))/T
      RETURN
C
C                   ALGORITHM FOR B0 .GE. 8
C
  120 U = GAMLN1(A0) + ALGDIV(A0,B0)
      BRCOMP = A0*EXP(Z-U)
      RETURN
C-----------------------------------------------------------------------
C              PROCEDURE FOR A .GE. 8 AND B .GE. 8
C-----------------------------------------------------------------------
  130 IF (A.GT.B) GO TO 140
      H = A/B
C*PT*WARNING* Constant already double-precision
      X0 = H/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      Y0 = 1.0D0/ (1.0D0+H)
      LAMBDA = A - (A+B)*X
      GO TO 150

  140 H = B/A
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      X0 = 1.0D0/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
      Y0 = H/ (1.0D0+H)
      LAMBDA = (A+B)*Y - B
C
  150 E = -LAMBDA/A
C*PT*WARNING* Constant already double-precision
      IF (ABS(E).GT.0.6D0) GO TO 160
      U = RLOG1(E)
      GO TO 170

  160 U = E - DLOG(X/X0)
C
  170 E = LAMBDA/B
C*PT*WARNING* Constant already double-precision
      IF (ABS(E).GT.0.6D0) GO TO 180
      V = RLOG1(E)
      GO TO 190

  180 V = E - DLOG(Y/Y0)
C
  190 Z = EXP(- (A*U+B*V))
      BRCOMP = CONST*SQRT(B*X0)*Z*EXP(-BCORR(A,B))
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION BUP(A,B,X,Y,N,EPS)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C     EVALUATION OF IX(A,B) - IX(A+N,B) WHERE N IS A POSITIVE INTEGER.
C     EPS IS THE TOLERANCE USED.
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,EPS,X,Y
      INTEGER N
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AP1,APB,D,L,R,T,W
      INTEGER I,K,KP1,MU,NM1
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION BRCMP1,EXPARG
      EXTERNAL BRCMP1,EXPARG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,EXP
C     ..
C     .. Executable Statements ..
C
C          OBTAIN THE SCALING FACTOR EXP(-MU) AND
C             EXP(MU)*(X**A*Y**B/BETA(A,B))/A
C
      APB = A + B
C*PT*WARNING* Constant already double-precision
      AP1 = A + 1.0D0
      MU = 0
C*PT*WARNING* Constant already double-precision
      D = 1.0D0
C*PT*WARNING* Constant already double-precision
      IF (N.EQ.1 .OR. A.LT.1.0D0) GO TO 10
C*PT*WARNING* Constant already double-precision
      IF (APB.LT.1.1D0*AP1) GO TO 10
      MU = ABS(EXPARG(1))
      K = EXPARG(0)
      IF (K.LT.MU) MU = K
      T = MU
      D = EXP(-T)
C
   10 BUP = BRCMP1(MU,A,B,X,Y)/A
C*PT*WARNING* Constant already double-precision
      IF (N.EQ.1 .OR. BUP.EQ.0.0D0) RETURN
      NM1 = N - 1
      W = D
C
C          LET K BE THE INDEX OF THE MAXIMUM TERM
C
      K = 0
C*PT*WARNING* Constant already double-precision
      IF (B.LE.1.0D0) GO TO 50
C*PT*WARNING* Constant already double-precision
      IF (Y.GT.1.D-4) GO TO 20
      K = NM1
      GO TO 30
C*PT*WARNING* Constant already double-precision

   20 R = (B-1.0D0)*X/Y - A
C*PT*WARNING* Constant already double-precision
      IF (R.LT.1.0D0) GO TO 50
      K = NM1
      T = NM1
      IF (R.LT.T) K = R
C
C          ADD THE INCREASING TERMS OF THE SERIES
C
   30 DO 40 I = 1,K
          L = I - 1
          D = ((APB+L)/ (AP1+L))*X*D
          W = W + D
   40 CONTINUE
      IF (K.EQ.NM1) GO TO 70
C
C          ADD THE REMAINING TERMS OF THE SERIES
C
   50 KP1 = K + 1
      DO 60 I = KP1,NM1
          L = I - 1
          D = ((APB+L)/ (AP1+L))*X*D
          W = W + D
          IF (D.LE.EPS*W) GO TO 70
   60 CONTINUE
C
C               TERMINATE THE PROCEDURE
C
   70 BUP = BUP*W
      RETURN

      END
      SUBROUTINE CDFBET(WHICH,P,Q,X,Y,A,B,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFBET( WHICH, P, Q, X, Y, A, B, STATUS, BOUND )
C               Cumulative Distribution Function
C                         BETa Distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the beta distribution given
C     values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from X,Y,A and B
C               iwhich = 2 : Calculate X and Y from P,Q,A and B
C               iwhich = 3 : Calculate A from P,Q,X,Y and B
C               iwhich = 4 : Calculate B from P,Q,X,Y and A
C
C                    INTEGER WHICH
C
C     P <--> The integral from 0 to X of the chi-square
C            distribution.
C            Input range: [0, 1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: [0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     X <--> Upper limit of integration of beta density.
C            Input range: [0,1].
C            Search range: [0,1]
C                    DOUBLE PRECISION X
C
C     Y <--> 1-X.
C            Input range: [0,1].
C            Search range: [0,1]
C            X + Y = 1.0.
C                    DOUBLE PRECISION Y
C
C     A <--> The first parameter of the beta density.
C            Input range: (0, +infinity).
C            Search range: [1D-300,1D300]
C                    DOUBLE PRECISION A
C
C     B <--> The second parameter of the beta density.
C            Input range: (0, +infinity).
C            Search range: [1D-300,1D300]
C                    DOUBLE PRECISION B
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                4 if X + Y .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Cumulative distribution function  (P)  is calculated directly by
C     code associated with the following reference.
C
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C                              Note
C
C
C     The beta density is proportional to
C               t^(A-1) * (1-t)^(B-1)
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ZERO,INF
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PARAMETER (ZERO=1.0D-300,INF=1.0D300)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ONE
C*PT*WARNING* Constant already double-precision
      PARAMETER (ONE=1.0D0)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,BOUND,P,Q,X,Y
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FX,XHI,XLO,CUM,CCUM,XY,PQ
      LOGICAL QHI,QLEFT,QPORQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPMPAR
      EXTERNAL SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL CUMBET,DINVR,DSTINV,DSTZR,DZROR
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.4))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 4.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LT.0.0D0).OR. (P.GT.1.0D0))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50
C*PT*WARNING* Constant already double-precision

   40 BOUND = 1.0D0
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.1) GO TO 110
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Q
C
      IF (.NOT. ((Q.LT.0.0D0).OR. (Q.GT.1.0D0))) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Q.LT.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 90
C*PT*WARNING* Constant already double-precision

   80 BOUND = 1.0D0
   90 STATUS = -3
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.2) GO TO 150
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     X
C
      IF (.NOT. ((X.LT.0.0D0).OR. (X.GT.1.0D0))) GO TO 140
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (X.LT.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 130
C*PT*WARNING* Constant already double-precision

  120 BOUND = 1.0D0
  130 STATUS = -4
      RETURN

  140 CONTINUE
  150 IF (WHICH.EQ.2) GO TO 190
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Y
C
      IF (.NOT. ((Y.LT.0.0D0).OR. (Y.GT.1.0D0))) GO TO 180
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Y.LT.0.0D0)) GO TO 160
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 170
C*PT*WARNING* Constant already double-precision

  160 BOUND = 1.0D0
  170 STATUS = -5
      RETURN

  180 CONTINUE
  190 IF (WHICH.EQ.3) GO TO 210
C*PT*WARNING* Constant already double-precision
C
C     A
C
      IF (.NOT. (A.LE.0.0D0)) GO TO 200
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -6
      RETURN

  200 CONTINUE
  210 IF (WHICH.EQ.4) GO TO 230
C*PT*WARNING* Constant already double-precision
C
C     B
C
      IF (.NOT. (B.LE.0.0D0)) GO TO 220
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -7
      RETURN

  220 CONTINUE
  230 IF (WHICH.EQ.1) GO TO 270
C
C     P + Q
C
      PQ = P + Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PQ)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 260
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PQ.LT.0.0D0)) GO TO 240
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 250
C*PT*WARNING* Constant already double-precision

  240 BOUND = 1.0D0
  250 STATUS = 3
      RETURN

  260 CONTINUE
  270 IF (WHICH.EQ.2) GO TO 310
C
C     X + Y
C
      XY = X + Y
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((XY)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 300
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (XY.LT.0.0D0)) GO TO 280
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 290
C*PT*WARNING* Constant already double-precision

  280 BOUND = 1.0D0
  290 STATUS = 4
      RETURN

  300 CONTINUE
  310 IF (.NOT. (WHICH.EQ.1)) QPORQ = P .LE. Q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (WHICH)) THEN
C
C     Calculating P and Q
C
          CALL CUMBET(X,Y,A,B,P,Q)
          STATUS = 0

      ELSE IF ((2).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Calculating X and Y
C
          CALL DSTZR(0.0D0,1.0D0,ATOL,TOL)
          IF (.NOT. (QPORQ)) GO TO 340
          STATUS = 0
          CALL DZROR(STATUS,X,FX,XLO,XHI,QLEFT,QHI)
          Y = ONE - X
  320     IF (.NOT. (STATUS.EQ.1)) GO TO 330
          CALL CUMBET(X,Y,A,B,CUM,CCUM)
          FX = CUM - P
          CALL DZROR(STATUS,X,FX,XLO,XHI,QLEFT,QHI)
          Y = ONE - X
          GO TO 320

  330     GO TO 370

  340     STATUS = 0
          CALL DZROR(STATUS,Y,FX,XLO,XHI,QLEFT,QHI)
          X = ONE - Y
  350     IF (.NOT. (STATUS.EQ.1)) GO TO 360
          CALL CUMBET(X,Y,A,B,CUM,CCUM)
          FX = CCUM - Q
          CALL DZROR(STATUS,Y,FX,XLO,XHI,QLEFT,QHI)
          X = ONE - Y
          GO TO 350

  360     CONTINUE
  370     IF (.NOT. (STATUS.EQ.-1)) GO TO 400
          IF (.NOT. (QLEFT)) GO TO 380
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 390

  380     STATUS = 2
C*PT*WARNING* Constant already double-precision
          BOUND = 1.0D0
  390     CONTINUE
  400     CONTINUE

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Computing A
C
          A = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,A,FX,QLEFT,QHI)
  410     IF (.NOT. (STATUS.EQ.1)) GO TO 440
          CALL CUMBET(X,Y,A,B,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 420
          FX = CUM - P
          GO TO 430

  420     FX = CCUM - Q
  430     CALL DINVR(STATUS,A,FX,QLEFT,QHI)
          GO TO 410

  440     IF (.NOT. (STATUS.EQ.-1)) GO TO 470
          IF (.NOT. (QLEFT)) GO TO 450
          STATUS = 1
          BOUND = ZERO
          GO TO 460

  450     STATUS = 2
          BOUND = INF
  460     CONTINUE
  470     CONTINUE

      ELSE IF ((4).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Computing B
C
          B = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,B,FX,QLEFT,QHI)
  480     IF (.NOT. (STATUS.EQ.1)) GO TO 510
          CALL CUMBET(X,Y,A,B,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 490
          FX = CUM - P
          GO TO 500

  490     FX = CCUM - Q
  500     CALL DINVR(STATUS,B,FX,QLEFT,QHI)
          GO TO 480

  510     IF (.NOT. (STATUS.EQ.-1)) GO TO 540
          IF (.NOT. (QLEFT)) GO TO 520
          STATUS = 1
          BOUND = ZERO
          GO TO 530

  520     STATUS = 2
          BOUND = INF
  530     CONTINUE
  540 END IF

      RETURN
C
      END
      SUBROUTINE CDFBIN(WHICH,P,Q,S,XN,PR,OMPR,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFBIN ( WHICH, P, Q, S, XN, PR, OMPR, STATUS, BOUND )
C               Cumulative Distribution Function
C                         BINomial distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the binomial
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from S,XN,PR and OMPR
C               iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR
C               iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR
C               iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN
C                    INTEGER WHICH
C
C     P <--> The cumulation from 0 to S of the binomial distribution.
C            (Probablility of S or fewer successes in XN trials each
C            with probability of success PR.)
C            Input range: [0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: [0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     S <--> The number of successes observed.
C            Input range: [0, XN]
C            Search range: [0, XN]
C                    DOUBLE PRECISION S
C
C     XN  <--> The number of binomial trials.
C              Input range: (0, +infinity).
C              Search range: [1E-300, 1E300]
C                    DOUBLE PRECISION XN
C
C     PR  <--> The probability of success in each binomial trial.
C              Input range: [0,1].
C              Search range: [0,1]
C                    DOUBLE PRECISION PR
C
C     OMPR  <--> 1-PR
C              Input range: [0,1].
C              Search range: [0,1]
C              PR + OMPR = 1.0
C                    DOUBLE PRECISION OMPR
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                4 if PR + OMPR .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula  26.5.24    of   Abramowitz  and    Stegun,  Handbook   of
C     Mathematical   Functions (1966) is   used  to reduce the  binomial
C     distribution  to  the  cumulative incomplete    beta distribution.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C**********************************************************************

C     .. Parameters ..
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ZERO,INF
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PARAMETER (ZERO=1.0D-300,INF=1.0D300)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ONE
C*PT*WARNING* Constant already double-precision
      PARAMETER (ONE=1.0D0)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,P,Q,PR,OMPR,S,XN
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FX,XHI,XLO,CUM,CCUM,PQ,PROMPR
      LOGICAL QHI,QLEFT,QPORQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPMPAR
      EXTERNAL SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL DINVR,DSTINV,DSTZR,DZROR,CUMBIN
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).AND. (WHICH.GT.4))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 4.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LT.0.0D0).OR. (P.GT.1.0D0))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50
C*PT*WARNING* Constant already double-precision

   40 BOUND = 1.0D0
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.1) GO TO 110
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Q
C
      IF (.NOT. ((Q.LT.0.0D0).OR. (Q.GT.1.0D0))) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Q.LT.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 90
C*PT*WARNING* Constant already double-precision

   80 BOUND = 1.0D0
   90 STATUS = -3
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.3) GO TO 130
C*PT*WARNING* Constant already double-precision
C
C     XN
C
      IF (.NOT. (XN.LE.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -5
      RETURN

  120 CONTINUE
  130 IF (WHICH.EQ.2) GO TO 170
C*PT*WARNING* Constant already double-precision
C
C     S
C
      IF (.NOT. ((S.LT.0.0D0).OR. ((WHICH.NE.3).AND. (S.GT.XN))))
     +    GO TO 160
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (S.LT.0.0D0)) GO TO 140
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 150

  140 BOUND = XN
  150 STATUS = -4
      RETURN

  160 CONTINUE
  170 IF (WHICH.EQ.4) GO TO 210
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     PR
C
      IF (.NOT. ((PR.LT.0.0D0).OR. (PR.GT.1.0D0))) GO TO 200
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PR.LT.0.0D0)) GO TO 180
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 190
C*PT*WARNING* Constant already double-precision

  180 BOUND = 1.0D0
  190 STATUS = -6
      RETURN

  200 CONTINUE
  210 IF (WHICH.EQ.4) GO TO 250
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     OMPR
C
      IF (.NOT. ((OMPR.LT.0.0D0).OR. (OMPR.GT.1.0D0))) GO TO 240
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (OMPR.LT.0.0D0)) GO TO 220
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 230
C*PT*WARNING* Constant already double-precision

  220 BOUND = 1.0D0
  230 STATUS = -7
      RETURN

  240 CONTINUE
  250 IF (WHICH.EQ.1) GO TO 290
C
C     P + Q
C
      PQ = P + Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PQ)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 280
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PQ.LT.0.0D0)) GO TO 260
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 270
C*PT*WARNING* Constant already double-precision

  260 BOUND = 1.0D0
  270 STATUS = 3
      RETURN

  280 CONTINUE
  290 IF (WHICH.EQ.4) GO TO 330
C
C     PR + OMPR
C
      PROMPR = PR + OMPR
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PROMPR)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 320
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PROMPR.LT.0.0D0)) GO TO 300
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 310
C*PT*WARNING* Constant already double-precision

  300 BOUND = 1.0D0
  310 STATUS = 4
      RETURN

  320 CONTINUE
  330 IF (.NOT. (WHICH.EQ.1)) QPORQ = P .LE. Q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (WHICH)) THEN
C
C     Calculating P
C
          CALL CUMBIN(S,XN,PR,OMPR,P,Q)
          STATUS = 0

      ELSE IF ((2).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating S
C
          S = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,XN,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,S,FX,QLEFT,QHI)
  340     IF (.NOT. (STATUS.EQ.1)) GO TO 370
          CALL CUMBIN(S,XN,PR,OMPR,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 350
          FX = CUM - P
          GO TO 360

  350     FX = CCUM - Q
  360     CALL DINVR(STATUS,S,FX,QLEFT,QHI)
          GO TO 340

  370     IF (.NOT. (STATUS.EQ.-1)) GO TO 400
          IF (.NOT. (QLEFT)) GO TO 380
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 390

  380     STATUS = 2
          BOUND = XN
  390     CONTINUE
  400     CONTINUE

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating XN
C
          XN = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,XN,FX,QLEFT,QHI)
  410     IF (.NOT. (STATUS.EQ.1)) GO TO 440
          CALL CUMBIN(S,XN,PR,OMPR,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 420
          FX = CUM - P
          GO TO 430

  420     FX = CCUM - Q
  430     CALL DINVR(STATUS,XN,FX,QLEFT,QHI)
          GO TO 410

  440     IF (.NOT. (STATUS.EQ.-1)) GO TO 470
          IF (.NOT. (QLEFT)) GO TO 450
          STATUS = 1
          BOUND = ZERO
          GO TO 460

  450     STATUS = 2
          BOUND = INF
  460     CONTINUE
  470     CONTINUE

      ELSE IF ((4).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Calculating PR and OMPR
C
          CALL DSTZR(0.0D0,1.0D0,ATOL,TOL)
          IF (.NOT. (QPORQ)) GO TO 500
          STATUS = 0
          CALL DZROR(STATUS,PR,FX,XLO,XHI,QLEFT,QHI)
          OMPR = ONE - PR
  480     IF (.NOT. (STATUS.EQ.1)) GO TO 490
          CALL CUMBIN(S,XN,PR,OMPR,CUM,CCUM)
          FX = CUM - P
          CALL DZROR(STATUS,PR,FX,XLO,XHI,QLEFT,QHI)
          OMPR = ONE - PR
          GO TO 480

  490     GO TO 530

  500     STATUS = 0
          CALL DZROR(STATUS,OMPR,FX,XLO,XHI,QLEFT,QHI)
          PR = ONE - OMPR
  510     IF (.NOT. (STATUS.EQ.1)) GO TO 520
          CALL CUMBIN(S,XN,PR,OMPR,CUM,CCUM)
          FX = CCUM - Q
          CALL DZROR(STATUS,OMPR,FX,XLO,XHI,QLEFT,QHI)
          PR = ONE - OMPR
          GO TO 510

  520     CONTINUE
  530     IF (.NOT. (STATUS.EQ.-1)) GO TO 560
          IF (.NOT. (QLEFT)) GO TO 540
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 550

  540     STATUS = 2
C*PT*WARNING* Constant already double-precision
          BOUND = 1.0D0
  550     CONTINUE
  560 END IF

      RETURN

      END
      SUBROUTINE CDFCHI(WHICH,P,Q,X,DF,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFCHI( WHICH, P, Q, X, DF, STATUS, BOUND )
C               Cumulative Distribution Function
C               CHI-Square distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the chi-square
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next three argument
C               values is to be calculated from the others.
C               Legal range: 1..3
C               iwhich = 1 : Calculate P and Q from X and DF
C               iwhich = 2 : Calculate X from P,Q and DF
C               iwhich = 3 : Calculate DF from P,Q and X
C                    INTEGER WHICH
C
C     P <--> The integral from 0 to X of the chi-square
C            distribution.
C            Input range: [0, 1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     X <--> Upper limit of integration of the non-central
C            chi-square distribution.
C            Input range: [0, +infinity).
C            Search range: [0,1E300]
C                    DOUBLE PRECISION X
C
C     DF <--> Degrees of freedom of the
C             chi-square distribution.
C             Input range: (0, +infinity).
C             Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DF
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C               10 indicates error returned from cumgam.  See
C                  references in cdfgam
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula    26.4.19   of Abramowitz  and     Stegun, Handbook  of
C     Mathematical Functions   (1966) is used   to reduce the chisqure
C     distribution to the incomplete distribution.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ZERO,INF
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PARAMETER (ZERO=1.0D-300,INF=1.0D300)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,DF,P,Q,X
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FX,CUM,CCUM,PQ,PORQ
      LOGICAL QHI,QLEFT,QPORQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPMPAR
      EXTERNAL SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL DINVR,DSTINV,CUMCHI
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.3))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 3.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LT.0.0D0).OR. (P.GT.1.0D0))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50
C*PT*WARNING* Constant already double-precision

   40 BOUND = 1.0D0
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.1) GO TO 110
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Q
C
      IF (.NOT. ((Q.LE.0.0D0).OR. (Q.GT.1.0D0))) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Q.LE.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 90
C*PT*WARNING* Constant already double-precision

   80 BOUND = 1.0D0
   90 STATUS = -3
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.2) GO TO 130
C*PT*WARNING* Constant already double-precision
C
C     X
C
      IF (.NOT. (X.LT.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -4
      RETURN

  120 CONTINUE
  130 IF (WHICH.EQ.3) GO TO 150
C*PT*WARNING* Constant already double-precision
C
C     DF
C
      IF (.NOT. (DF.LE.0.0D0)) GO TO 140
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -5
      RETURN

  140 CONTINUE
  150 IF (WHICH.EQ.1) GO TO 190
C
C     P + Q
C
      PQ = P + Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PQ)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 180
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PQ.LT.0.0D0)) GO TO 160
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 170
C*PT*WARNING* Constant already double-precision

  160 BOUND = 1.0D0
  170 STATUS = 3
      RETURN

  180 CONTINUE
  190 IF (WHICH.EQ.1) GO TO 220
C
C     Select the minimum of P or Q
C
      QPORQ = P .LE. Q
      IF (.NOT. (QPORQ)) GO TO 200
      PORQ = P
      GO TO 210

  200 PORQ = Q
  210 CONTINUE
C
C     Calculate ANSWERS
C
  220 IF ((1).EQ. (WHICH)) THEN
C
C     Calculating P and Q
C
          STATUS = 0
          CALL CUMCHI(X,DF,P,Q)
C*PT*WARNING* Constant already double-precision
          IF (PORQ.GT.1.5D0) THEN
              STATUS = 10
              RETURN

          END IF

      ELSE IF ((2).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating X
C
          X = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,X,FX,QLEFT,QHI)
  230     IF (.NOT. (STATUS.EQ.1)) GO TO 270
          CALL CUMCHI(X,DF,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 240
          FX = CUM - P
          GO TO 250

  240     FX = CCUM - Q
C*PT*WARNING* Constant already double-precision
  250     IF (.NOT. ((FX+PORQ).GT.1.5D0)) GO TO 260
          STATUS = 10
          RETURN

  260     CALL DINVR(STATUS,X,FX,QLEFT,QHI)
          GO TO 230

  270     IF (.NOT. (STATUS.EQ.-1)) GO TO 300
          IF (.NOT. (QLEFT)) GO TO 280
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 290

  280     STATUS = 2
          BOUND = INF
  290     CONTINUE
  300     CONTINUE

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating DF
C
          DF = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,DF,FX,QLEFT,QHI)
  310     IF (.NOT. (STATUS.EQ.1)) GO TO 350
          CALL CUMCHI(X,DF,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 320
          FX = CUM - P
          GO TO 330

  320     FX = CCUM - Q
C*PT*WARNING* Constant already double-precision
  330     IF (.NOT. ((FX+PORQ).GT.1.5D0)) GO TO 340
          STATUS = 10
          RETURN

  340     CALL DINVR(STATUS,DF,FX,QLEFT,QHI)
          GO TO 310

  350     IF (.NOT. (STATUS.EQ.-1)) GO TO 380
          IF (.NOT. (QLEFT)) GO TO 360
          STATUS = 1
          BOUND = ZERO
          GO TO 370

  360     STATUS = 2
          BOUND = INF
  370     CONTINUE
  380 END IF

      RETURN

      END
      SUBROUTINE CDFCHN(WHICH,P,Q,X,DF,PNONC,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFCHN( WHICH, P, Q, X, DF, PNONC, STATUS, BOUND )
C               Cumulative Distribution Function
C               Non-central Chi-Square
C
C
C                              Function
C
C
C     Calculates any one parameter of the non-central chi-square
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next three argument
C               values is to be calculated from the others.
C               Input range: 1..4
C               iwhich = 1 : Calculate P and Q from X and DF
C               iwhich = 2 : Calculate X from P,DF and PNONC
C               iwhich = 3 : Calculate DF from P,X and PNONC
C               iwhich = 3 : Calculate PNONC from P,X and DF
C                    INTEGER WHICH
C
C     P <--> The integral from 0 to X of the non-central chi-square
C            distribution.
C            Input range: [0, 1-1E-16).
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Q is not used by this subroutine and is only included
C            for similarity with other cdf* routines.
C                    DOUBLE PRECISION Q
C
C     X <--> Upper limit of integration of the non-central
C            chi-square distribution.
C            Input range: [0, +infinity).
C            Search range: [0,1E300]
C                    DOUBLE PRECISION X
C
C     DF <--> Degrees of freedom of the non-central
C             chi-square distribution.
C             Input range: (0, +infinity).
C             Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DF
C
C     PNONC <--> Non-centrality parameter of the non-central
C                chi-square distribution.
C                Input range: [0, +infinity).
C                Search range: [0,1E4]
C                    DOUBLE PRECISION PNONC
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula  26.4.25   of   Abramowitz   and   Stegun,  Handbook  of
C     Mathematical  Functions (1966) is used to compute the cumulative
C     distribution function.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C                            WARNING
C
C     The computation time  required for this  routine is proportional
C     to the noncentrality  parameter  (PNONC).  Very large  values of
C     this parameter can consume immense  computer resources.  This is
C     why the search range is bounded by 10,000.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION TENT4
C*PT*WARNING* Constant already double-precision
      PARAMETER (TENT4=1.0D4)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ZERO,ONE,INF
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PARAMETER (ZERO=1.0D-300,ONE=1.0D0-1.0D-16,INF=1.0D300)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,DF,P,Q,PNONC,X
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FX,CUM,CCUM
      LOGICAL QHI,QLEFT
C     ..
C     .. External Subroutines ..
      EXTERNAL DINVR,DSTINV,CUMCHN
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.4))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 4.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C
C     P
C

      IF (.NOT. ((P.LT.0.0D0).OR. (P.GT.ONE))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50

   40 BOUND = ONE
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.2) GO TO 90
C*PT*WARNING* Constant already double-precision
C
C     X
C
      IF (.NOT. (X.LT.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -4
      RETURN

   80 CONTINUE
   90 IF (WHICH.EQ.3) GO TO 110
C*PT*WARNING* Constant already double-precision
C
C     DF
C
      IF (.NOT. (DF.LE.0.0D0)) GO TO 100
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -5
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.4) GO TO 130
C*PT*WARNING* Constant already double-precision
C
C     PNONC
C
      IF (.NOT. (PNONC.LT.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -6
      RETURN

  120 CONTINUE
C
C     Calculate ANSWERS
C
  130 IF ((1).EQ. (WHICH)) THEN
C
C     Calculating P and Q
C
          CALL CUMCHN(X,DF,PNONC,P,Q)
          STATUS = 0

      ELSE IF ((2).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating X
C
          X = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,X,FX,QLEFT,QHI)
  140     IF (.NOT. (STATUS.EQ.1)) GO TO 150
          CALL CUMCHN(X,DF,PNONC,CUM,CCUM)
          FX = CUM - P
          CALL DINVR(STATUS,X,FX,QLEFT,QHI)
          GO TO 140

  150     IF (.NOT. (STATUS.EQ.-1)) GO TO 180
          IF (.NOT. (QLEFT)) GO TO 160
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 170

  160     STATUS = 2
          BOUND = INF
  170     CONTINUE
  180     CONTINUE

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating DF
C
          DF = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,DF,FX,QLEFT,QHI)
  190     IF (.NOT. (STATUS.EQ.1)) GO TO 200
          CALL CUMCHN(X,DF,PNONC,CUM,CCUM)
          FX = CUM - P
          CALL DINVR(STATUS,DF,FX,QLEFT,QHI)
          GO TO 190

  200     IF (.NOT. (STATUS.EQ.-1)) GO TO 230
          IF (.NOT. (QLEFT)) GO TO 210
          STATUS = 1
          BOUND = ZERO
          GO TO 220

  210     STATUS = 2
          BOUND = INF
  220     CONTINUE
  230     CONTINUE

      ELSE IF ((4).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating PNONC
C
          PNONC = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,TENT4,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,PNONC,FX,QLEFT,QHI)
  240     IF (.NOT. (STATUS.EQ.1)) GO TO 250
          CALL CUMCHN(X,DF,PNONC,CUM,CCUM)
          FX = CUM - P
          CALL DINVR(STATUS,PNONC,FX,QLEFT,QHI)
          GO TO 240

  250     IF (.NOT. (STATUS.EQ.-1)) GO TO 280
          IF (.NOT. (QLEFT)) GO TO 260
          STATUS = 1
          BOUND = ZERO
          GO TO 270

  260     STATUS = 2
          BOUND = TENT4
  270     CONTINUE
  280 END IF

      RETURN

      END
      SUBROUTINE CDFF(WHICH,P,Q,F,DFN,DFD,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFF( WHICH, P, Q, F, DFN, DFD, STATUS, BOUND )
C               Cumulative Distribution Function
C               F distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the F distribution
C     given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from F,DFN and DFD
C               iwhich = 2 : Calculate F from P,Q,DFN and DFD
C               iwhich = 3 : Calculate DFN from P,Q,F and DFD
C               iwhich = 4 : Calculate DFD from P,Q,F and DFN
C                    INTEGER WHICH
C
C       P <--> The integral from 0 to F of the f-density.
C              Input range: [0,1].
C                    DOUBLE PRECISION P
C
C       Q <--> 1-P.
C              Input range: (0, 1].
C              P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C       F <--> Upper limit of integration of the f-density.
C              Input range: [0, +infinity).
C              Search range: [0,1E300]
C                    DOUBLE PRECISION F
C
C     DFN < --> Degrees of freedom of the numerator sum of squares.
C               Input range: (0, +infinity).
C               Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DFN
C
C     DFD < --> Degrees of freedom of the denominator sum of squares.
C               Input range: (0, +infinity).
C               Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DFD
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula   26.6.2   of   Abramowitz   and   Stegun,  Handbook  of
C     Mathematical  Functions (1966) is used to reduce the computation
C     of the  cumulative  distribution function for the  F  variate to
C     that of an incomplete beta.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C                              WARNING
C
C     The value of the  cumulative  F distribution is  not necessarily
C     monotone in  either degrees of freedom.  There  thus may  be two
C     values  that  provide a given CDF  value.   This routine assumes
C     monotonicity and will find an arbitrary one of the two values.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ZERO,INF
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PARAMETER (ZERO=1.0D-300,INF=1.0D300)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,DFD,DFN,F,P,Q
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION PQ,FX,CUM,CCUM
      LOGICAL QHI,QLEFT,QPORQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPMPAR
      EXTERNAL SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL DINVR,DSTINV,CUMF
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.4))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 4.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LT.0.0D0).OR. (P.GT.1.0D0))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50
C*PT*WARNING* Constant already double-precision

   40 BOUND = 1.0D0
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.1) GO TO 110
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Q
C
      IF (.NOT. ((Q.LE.0.0D0).OR. (Q.GT.1.0D0))) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Q.LE.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 90
C*PT*WARNING* Constant already double-precision

   80 BOUND = 1.0D0
   90 STATUS = -3
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.2) GO TO 130
C*PT*WARNING* Constant already double-precision
C
C     F
C
      IF (.NOT. (F.LT.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -4
      RETURN

  120 CONTINUE
  130 IF (WHICH.EQ.3) GO TO 150
C*PT*WARNING* Constant already double-precision
C
C     DFN
C
      IF (.NOT. (DFN.LE.0.0D0)) GO TO 140
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -5
      RETURN

  140 CONTINUE
  150 IF (WHICH.EQ.4) GO TO 170
C*PT*WARNING* Constant already double-precision
C
C     DFD
C
      IF (.NOT. (DFD.LE.0.0D0)) GO TO 160
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -6
      RETURN

  160 CONTINUE
  170 IF (WHICH.EQ.1) GO TO 210
C
C     P + Q
C
      PQ = P + Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PQ)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 200
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PQ.LT.0.0D0)) GO TO 180
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 190
C*PT*WARNING* Constant already double-precision

  180 BOUND = 1.0D0
  190 STATUS = 3
      RETURN

  200 CONTINUE
  210 IF (.NOT. (WHICH.EQ.1)) QPORQ = P .LE. Q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (WHICH)) THEN
C
C     Calculating P
C
          CALL CUMF(F,DFN,DFD,P,Q)
          STATUS = 0

      ELSE IF ((2).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating F
C
          F = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,F,FX,QLEFT,QHI)
  220     IF (.NOT. (STATUS.EQ.1)) GO TO 250
          CALL CUMF(F,DFN,DFD,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 230
          FX = CUM - P
          GO TO 240

  230     FX = CCUM - Q
  240     CALL DINVR(STATUS,F,FX,QLEFT,QHI)
          GO TO 220

  250     IF (.NOT. (STATUS.EQ.-1)) GO TO 280
          IF (.NOT. (QLEFT)) GO TO 260
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 270

  260     STATUS = 2
          BOUND = INF
  270     CONTINUE
  280     CONTINUE

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating DFN
C
          DFN = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,DFN,FX,QLEFT,QHI)
  290     IF (.NOT. (STATUS.EQ.1)) GO TO 320
          CALL CUMF(F,DFN,DFD,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 300
          FX = CUM - P
          GO TO 310

  300     FX = CCUM - Q
  310     CALL DINVR(STATUS,DFN,FX,QLEFT,QHI)
          GO TO 290

  320     IF (.NOT. (STATUS.EQ.-1)) GO TO 350
          IF (.NOT. (QLEFT)) GO TO 330
          STATUS = 1
          BOUND = ZERO
          GO TO 340

  330     STATUS = 2
          BOUND = INF
  340     CONTINUE
  350     CONTINUE

      ELSE IF ((4).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating DFD
C
          DFD = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,DFD,FX,QLEFT,QHI)
  360     IF (.NOT. (STATUS.EQ.1)) GO TO 390
          CALL CUMF(F,DFN,DFD,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 370
          FX = CUM - P
          GO TO 380

  370     FX = CCUM - Q
  380     CALL DINVR(STATUS,DFD,FX,QLEFT,QHI)
          GO TO 360

  390     IF (.NOT. (STATUS.EQ.-1)) GO TO 420
          IF (.NOT. (QLEFT)) GO TO 400
          STATUS = 1
          BOUND = ZERO
          GO TO 410

  400     STATUS = 2
          BOUND = INF
  410     CONTINUE
  420 END IF

      RETURN

      END
      SUBROUTINE CDFFNC(WHICH,P,Q,F,DFN,DFD,PHONC,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFFNC( WHICH, P, Q, F, DFN, DFD, PNONC, STATUS, BOUND
C               Cumulative Distribution Function
C               Non-central F distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the Non-central F
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next five argument
C               values is to be calculated from the others.
C               Legal range: 1..5
C               iwhich = 1 : Calculate P and Q from F,DFN,DFD and PNONC
C               iwhich = 2 : Calculate F from P,Q,DFN,DFD and PNONC
C               iwhich = 3 : Calculate DFN from P,Q,F,DFD and PNONC
C               iwhich = 4 : Calculate DFD from P,Q,F,DFN and PNONC
C               iwhich = 5 : Calculate PNONC from P,Q,F,DFN and DFD
C                    INTEGER WHICH
C
C       P <--> The integral from 0 to F of the non-central f-density.
C              Input range: [0,1-1E-16).
C                    DOUBLE PRECISION P
C
C       Q <--> 1-P.
C            Q is not used by this subroutine and is only included
C            for similarity with other cdf* routines.
C                    DOUBLE PRECISION Q
C
C       F <--> Upper limit of integration of the non-central f-density.
C              Input range: [0, +infinity).
C              Search range: [0,1E300]
C                    DOUBLE PRECISION F
C
C     DFN < --> Degrees of freedom of the numerator sum of squares.
C               Input range: (0, +infinity).
C               Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DFN
C
C     DFD < --> Degrees of freedom of the denominator sum of squares.
C               Must be in range: (0, +infinity).
C               Input range: (0, +infinity).
C               Search range: [ 1E-300, 1E300]
C                    DOUBLE PRECISION DFD
C
C     PNONC <-> The non-centrality parameter
C               Input range: [0,infinity)
C               Search range: [0,1E4]
C                    DOUBLE PRECISION PHONC
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula  26.6.20   of   Abramowitz   and   Stegun,  Handbook  of
C     Mathematical  Functions (1966) is used to compute the cumulative
C     distribution function.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C                            WARNING
C
C     The computation time  required for this  routine is proportional
C     to the noncentrality  parameter  (PNONC).  Very large  values of
C     this parameter can consume immense  computer resources.  This is
C     why the search range is bounded by 10,000.
C
C                              WARNING
C
C     The  value  of the  cumulative  noncentral F distribution is not
C     necessarily monotone in either degrees  of freedom.  There  thus
C     may be two values that provide a given  CDF value.  This routine
C     assumes monotonicity  and will find  an arbitrary one of the two
C     values.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION TENT4
C*PT*WARNING* Constant already double-precision
      PARAMETER (TENT4=1.0D4)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ZERO,ONE,INF
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PARAMETER (ZERO=1.0D-300,ONE=1.0D0-1.0D-16,INF=1.0D300)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,DFD,DFN,F,P,Q,PHONC
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FX,CUM,CCUM
      LOGICAL QHI,QLEFT
C     ..
C     .. External Subroutines ..
      EXTERNAL DINVR,DSTINV,CUMFNC
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.5))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 5.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LT.0.0D0).OR. (P.GT.ONE))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50

   40 BOUND = ONE
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.2) GO TO 90
C*PT*WARNING* Constant already double-precision
C
C     F
C
      IF (.NOT. (F.LT.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -4
      RETURN

   80 CONTINUE
   90 IF (WHICH.EQ.3) GO TO 110
C*PT*WARNING* Constant already double-precision
C
C     DFN
C
      IF (.NOT. (DFN.LE.0.0D0)) GO TO 100
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -5
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.4) GO TO 130
C*PT*WARNING* Constant already double-precision
C
C     DFD
C
      IF (.NOT. (DFD.LE.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -6
      RETURN

  120 CONTINUE
  130 IF (WHICH.EQ.5) GO TO 150
C*PT*WARNING* Constant already double-precision
C
C     PHONC
C
      IF (.NOT. (PHONC.LT.0.0D0)) GO TO 140
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -7
      RETURN

  140 CONTINUE
C
C     Calculate ANSWERS
C
  150 IF ((1).EQ. (WHICH)) THEN
C
C     Calculating P
C
          CALL CUMFNC(F,DFN,DFD,PHONC,P,Q)
          STATUS = 0

      ELSE IF ((2).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating F
C
          F = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,F,FX,QLEFT,QHI)
  160     IF (.NOT. (STATUS.EQ.1)) GO TO 170
          CALL CUMFNC(F,DFN,DFD,PHONC,CUM,CCUM)
          FX = CUM - P
          CALL DINVR(STATUS,F,FX,QLEFT,QHI)
          GO TO 160

  170     IF (.NOT. (STATUS.EQ.-1)) GO TO 200
          IF (.NOT. (QLEFT)) GO TO 180
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 190

  180     STATUS = 2
          BOUND = INF
  190     CONTINUE
  200     CONTINUE

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating DFN
C
          DFN = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,DFN,FX,QLEFT,QHI)
  210     IF (.NOT. (STATUS.EQ.1)) GO TO 220
          CALL CUMFNC(F,DFN,DFD,PHONC,CUM,CCUM)
          FX = CUM - P
          CALL DINVR(STATUS,DFN,FX,QLEFT,QHI)
          GO TO 210

  220     IF (.NOT. (STATUS.EQ.-1)) GO TO 250
          IF (.NOT. (QLEFT)) GO TO 230
          STATUS = 1
          BOUND = ZERO
          GO TO 240

  230     STATUS = 2
          BOUND = INF
  240     CONTINUE
  250     CONTINUE

      ELSE IF ((4).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating DFD
C
          DFD = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,DFD,FX,QLEFT,QHI)
  260     IF (.NOT. (STATUS.EQ.1)) GO TO 270
          CALL CUMFNC(F,DFN,DFD,PHONC,CUM,CCUM)
          FX = CUM - P
          CALL DINVR(STATUS,DFD,FX,QLEFT,QHI)
          GO TO 260

  270     IF (.NOT. (STATUS.EQ.-1)) GO TO 300
          IF (.NOT. (QLEFT)) GO TO 280
          STATUS = 1
          BOUND = ZERO
          GO TO 290

  280     STATUS = 2
          BOUND = INF
  290     CONTINUE
  300     CONTINUE

      ELSE IF ((5).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating PHONC
C
          PHONC = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,TENT4,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,PHONC,FX,QLEFT,QHI)
  310     IF (.NOT. (STATUS.EQ.1)) GO TO 320
          CALL CUMFNC(F,DFN,DFD,PHONC,CUM,CCUM)
          FX = CUM - P
          CALL DINVR(STATUS,PHONC,FX,QLEFT,QHI)
          GO TO 310

  320     IF (.NOT. (STATUS.EQ.-1)) GO TO 350
          IF (.NOT. (QLEFT)) GO TO 330
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 340

  330     STATUS = 2
          BOUND = TENT4
  340     CONTINUE
  350 END IF

      RETURN

      END
      SUBROUTINE CDFGAM(WHICH,P,Q,X,SHAPE,SCALE,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFGAM( WHICH, P, Q, X, SHAPE, SCALE, STATUS, BOUND )
C               Cumulative Distribution Function
C                         GAMma Distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the gamma
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from X,SHAPE and SCALE
C               iwhich = 2 : Calculate X from P,Q,SHAPE and SCALE
C               iwhich = 3 : Calculate SHAPE from P,Q,X and SCALE
C               iwhich = 4 : Calculate SCALE from P,Q,X and SHAPE
C                    INTEGER WHICH
C
C     P <--> The integral from 0 to X of the gamma density.
C            Input range: [0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C
C     X <--> The upper limit of integration of the gamma density.
C            Input range: [0, +infinity).
C            Search range: [0,1E300]
C                    DOUBLE PRECISION X
C
C     SHAPE <--> The shape parameter of the gamma density.
C                Input range: (0, +infinity).
C                Search range: [1E-300,1E300]
C                  DOUBLE PRECISION SHAPE
C
C
C     SCALE <--> The scale parameter of the gamma density.
C                Input range: (0, +infinity).
C                Search range: (1E-300,1E300]
C                   DOUBLE PRECISION SCALE
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                10 if the gamma or inverse gamma routine cannot
C                   compute the answer.  Usually happens only for
C                   X and SHAPE very large (gt 1E10 or more)
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Cumulative distribution function (P) is calculated directly by
C     the code associated with:
C
C     DiDinato, A. R. and Morris, A. H. Computation of the  incomplete
C     gamma function  ratios  and their  inverse.   ACM  Trans.  Math.
C     Softw. 12 (1986), 377-393.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C                              Note
C
C
C
C     The gamma density is proportional to
C       T**(SHAPE - 1) * EXP(- SCALE * T)
C
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ZERO,INF
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PARAMETER (ZERO=1.0D-300,INF=1.0D300)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,P,Q,SCALE,SHAPE,X
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION XX
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FX,XSCALE,CUM,CCUM,PQ,PORQ
      INTEGER IERR
      LOGICAL QHI,QLEFT,QPORQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPMPAR
      EXTERNAL SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL GAMINV,DINVR,DSTINV,CUMGAM
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.4))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 4.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LT.0.0D0).OR. (P.GT.1.0D0))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50
C*PT*WARNING* Constant already double-precision

   40 BOUND = 1.0d0
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.1) GO TO 110
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Q
C
      IF (.NOT. ((Q.LE.0.0D0).OR. (Q.GT.1.0D0))) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Q.LE.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 90
C*PT*WARNING* Constant already double-precision

   80 BOUND = 1.0D0
   90 STATUS = -3
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.2) GO TO 130
C*PT*WARNING* Constant already double-precision
C
C     X
C
      IF (.NOT. (X.LT.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -4
      RETURN

  120 CONTINUE
  130 IF (WHICH.EQ.3) GO TO 150
C*PT*WARNING* Constant already double-precision
C
C     SHAPE
C
      IF (.NOT. (SHAPE.LE.0.0D0)) GO TO 140
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -5
      RETURN

  140 CONTINUE
  150 IF (WHICH.EQ.4) GO TO 170
C*PT*WARNING* Constant already double-precision
C
C     SCALE
C
      IF (.NOT. (SCALE.LE.0.0D0)) GO TO 160
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -6
      RETURN

  160 CONTINUE
  170 IF (WHICH.EQ.1) GO TO 210
C
C     P + Q
C
      PQ = P + Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PQ)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 200
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PQ.LT.0.0D0)) GO TO 180
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 190
C*PT*WARNING* Constant already double-precision

  180 BOUND = 1.0D0
  190 STATUS = 3
      RETURN

  200 CONTINUE
  210 IF (WHICH.EQ.1) GO TO 240
C
C     Select the minimum of P or Q
C
      QPORQ = P .LE. Q
      IF (.NOT. (QPORQ)) GO TO 220
      PORQ = P
      GO TO 230

  220 PORQ = Q
  230 CONTINUE
C
C     Calculate ANSWERS
C
  240 IF ((1).EQ. (WHICH)) THEN
C
C     Calculating P
C
          STATUS = 0
          XSCALE = X*SCALE
          CALL CUMGAM(XSCALE,SHAPE,P,Q)
C*PT*WARNING* Constant already double-precision
          IF (PORQ.GT.1.5D0) STATUS = 10

      ELSE IF ((2).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Computing X
C
          CALL GAMINV(SHAPE,XX,-1.0D0,P,Q,IERR)
C*PT*WARNING* Constant already double-precision
          IF (IERR.LT.0.0D0) THEN
              STATUS = 10
              RETURN

          ELSE
              X = XX/SCALE
              STATUS = 0
          END IF

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Computing SHAPE
C
          SHAPE = 5.0D0
          XSCALE = X*SCALE
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,SHAPE,FX,QLEFT,QHI)
  250     IF (.NOT. (STATUS.EQ.1)) GO TO 290
          CALL CUMGAM(XSCALE,SHAPE,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 260
          FX = CUM - P
          GO TO 270

  260     FX = CCUM - Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
  270     IF (.NOT. ((QPORQ.AND. (CUM.GT.1.5D0)).OR.
     +        ((.NOT.QPORQ).AND. (CCUM.GT.1.5D0)))) GO TO 280
          STATUS = 10
          RETURN

  280     CALL DINVR(STATUS,SHAPE,FX,QLEFT,QHI)
          GO TO 250

  290     IF (.NOT. (STATUS.EQ.-1)) GO TO 320
          IF (.NOT. (QLEFT)) GO TO 300
          STATUS = 1
          BOUND = ZERO
          GO TO 310

  300     STATUS = 2
          BOUND = INF
  310     CONTINUE
  320     CONTINUE

      ELSE IF ((4).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Computing SCALE
C
          CALL GAMINV(SHAPE,XX,-1.0D0,P,Q,IERR)
C*PT*WARNING* Constant already double-precision
          IF (IERR.LT.0.0D0) THEN
              STATUS = 10
              RETURN

          ELSE
              SCALE = XX/X
              STATUS = 0
          END IF

      END IF

      RETURN

      END
      SUBROUTINE CDFNBN(WHICH,P,Q,S,XN,PR,OMPR,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFNBN ( WHICH, P, S, XN, PR, STATUS, BOUND )
C               Cumulative Distribution Function
C               Negative BiNomial distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the negative binomial
C     distribution given values for the others.
C
C     The  cumulative  negative   binomial  distribution  returns  the
C     probability that there  will be  F or fewer failures before  the
C     XNth success in binomial trials each of which has probability of
C     success PR.
C
C     The individual term of the negative binomial is the probability of
C     S failures before XN successes and is
C          Choose( S, XN+S-1 ) * PR^(XN) * (1-PR)^S
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which of the next four argument
C               values is to be calculated from the others.
C               Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from S,XN,PR and OMPR
C               iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR
C               iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR
C               iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN
C                    INTEGER WHICH
C
C     P <--> The cumulation from 0 to S of the  negative
C            binomial distribution.
C            Input range: [0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     S <--> The upper limit of cumulation of the binomial distribution.
C            There are F or fewer failures before the XNth success.
C            Input range: [0, +infinity).
C            Search range: [0, 1E300]
C                    DOUBLE PRECISION S
C
C     XN  <--> The number of successes.
C              Input range: [0, +infinity).
C              Search range: [0, 1E300]
C                    DOUBLE PRECISION XN
C
C     PR  <--> The probability of success in each binomial trial.
C              Input range: [0,1].
C              Search range: [0,1].
C                    DOUBLE PRECISION PR
C
C     OMPR  <--> 1-PR
C              Input range: [0,1].
C              Search range: [0,1]
C              PR + OMPR = 1.0
C                    DOUBLE PRECISION OMPR
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                4 if PR + OMPR .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula   26.5.26   of   Abramowitz  and  Stegun,  Handbook   of
C     Mathematical Functions (1966) is used  to  reduce calculation of
C     the cumulative distribution  function to that of  an  incomplete
C     beta.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION INF
C*PT*WARNING* Constant already double-precision
      PARAMETER (INF=1.0D300)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ONE
C*PT*WARNING* Constant already double-precision
      PARAMETER (ONE=1.0D0)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,P,Q,PR,OMPR,S,XN
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FX,XHI,XLO,PQ,PROMPR,CUM,CCUM
      LOGICAL QHI,QLEFT,QPORQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPMPAR
      EXTERNAL SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL DINVR,DSTINV,DSTZR,DZROR,CUMNBN
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.4))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 4.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LT.0.0D0).OR. (P.GT.1.0D0))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50
C*PT*WARNING* Constant already double-precision

   40 BOUND = 1.0D0
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.1) GO TO 110
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Q
C
      IF (.NOT. ((Q.LE.0.0D0).OR. (Q.GT.1.0D0))) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Q.LE.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 90
C*PT*WARNING* Constant already double-precision

   80 BOUND = 1.0D0
   90 STATUS = -3
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.2) GO TO 130
C*PT*WARNING* Constant already double-precision
C
C     S
C
      IF (.NOT. (S.LT.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -4
      RETURN

  120 CONTINUE
  130 IF (WHICH.EQ.3) GO TO 150
C*PT*WARNING* Constant already double-precision
C
C     XN
C
      IF (.NOT. (XN.LT.0.0D0)) GO TO 140
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -5
      RETURN

  140 CONTINUE
  150 IF (WHICH.EQ.4) GO TO 190
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     PR
C
      IF (.NOT. ((PR.LT.0.0D0).OR. (PR.GT.1.0D0))) GO TO 180
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PR.LT.0.0D0)) GO TO 160
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 170
C*PT*WARNING* Constant already double-precision

  160 BOUND = 1.0D0
  170 STATUS = -6
      RETURN

  180 CONTINUE
  190 IF (WHICH.EQ.4) GO TO 230
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     OMPR
C
      IF (.NOT. ((OMPR.LT.0.0D0).OR. (OMPR.GT.1.0D0))) GO TO 220
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (OMPR.LT.0.0D0)) GO TO 200
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 210
C*PT*WARNING* Constant already double-precision

  200 BOUND = 1.0D0
  210 STATUS = -7
      RETURN

  220 CONTINUE
  230 IF (WHICH.EQ.1) GO TO 270
C
C     P + Q
C
      PQ = P + Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PQ)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 260
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PQ.LT.0.0D0)) GO TO 240
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 250
C*PT*WARNING* Constant already double-precision

  240 BOUND = 1.0D0
  250 STATUS = 3
      RETURN

  260 CONTINUE
  270 IF (WHICH.EQ.4) GO TO 310
C
C     PR + OMPR
C
      PROMPR = PR + OMPR
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PROMPR)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 300
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PROMPR.LT.0.0D0)) GO TO 280
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 290
C*PT*WARNING* Constant already double-precision

  280 BOUND = 1.0D0
  290 STATUS = 4
      RETURN

  300 CONTINUE
  310 IF (.NOT. (WHICH.EQ.1)) QPORQ = P .LE. Q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (WHICH)) THEN
C
C     Calculating P
C
          CALL CUMNBN(S,XN,PR,OMPR,P,Q)
          STATUS = 0

      ELSE IF ((2).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating S
C
          S = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,S,FX,QLEFT,QHI)
  320     IF (.NOT. (STATUS.EQ.1)) GO TO 350
          CALL CUMNBN(S,XN,PR,OMPR,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 330
          FX = CUM - P
          GO TO 340

  330     FX = CCUM - Q
  340     CALL DINVR(STATUS,S,FX,QLEFT,QHI)
          GO TO 320

  350     IF (.NOT. (STATUS.EQ.-1)) GO TO 380
          IF (.NOT. (QLEFT)) GO TO 360
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 370

  360     STATUS = 2
          BOUND = INF
  370     CONTINUE
  380     CONTINUE

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating XN
C
          XN = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,XN,FX,QLEFT,QHI)
  390     IF (.NOT. (STATUS.EQ.1)) GO TO 420
          CALL CUMNBN(S,XN,PR,OMPR,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 400
          FX = CUM - P
          GO TO 410

  400     FX = CCUM - Q
  410     CALL DINVR(STATUS,XN,FX,QLEFT,QHI)
          GO TO 390

  420     IF (.NOT. (STATUS.EQ.-1)) GO TO 450
          IF (.NOT. (QLEFT)) GO TO 430
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 440

  430     STATUS = 2
          BOUND = INF
  440     CONTINUE
  450     CONTINUE

      ELSE IF ((4).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Calculating PR and OMPR
C
          CALL DSTZR(0.0D0,1.0D0,ATOL,TOL)
          IF (.NOT. (QPORQ)) GO TO 480
          STATUS = 0
          CALL DZROR(STATUS,PR,FX,XLO,XHI,QLEFT,QHI)
          OMPR = ONE - PR
  460     IF (.NOT. (STATUS.EQ.1)) GO TO 470
          CALL CUMNBN(S,XN,PR,OMPR,CUM,CCUM)
          FX = CUM - P
          CALL DZROR(STATUS,PR,FX,XLO,XHI,QLEFT,QHI)
          OMPR = ONE - PR
          GO TO 460

  470     GO TO 510

  480     STATUS = 0
          CALL DZROR(STATUS,OMPR,FX,XLO,XHI,QLEFT,QHI)
          PR = ONE - OMPR
  490     IF (.NOT. (STATUS.EQ.1)) GO TO 500
          CALL CUMNBN(S,XN,PR,OMPR,CUM,CCUM)
          FX = CCUM - Q
          CALL DZROR(STATUS,OMPR,FX,XLO,XHI,QLEFT,QHI)
          PR = ONE - OMPR
          GO TO 490

  500     CONTINUE
  510     IF (.NOT. (STATUS.EQ.-1)) GO TO 540
          IF (.NOT. (QLEFT)) GO TO 520
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 530

  520     STATUS = 2
C*PT*WARNING* Constant already double-precision
          BOUND = 1.0D0
  530     CONTINUE
  540 END IF

      RETURN

      END
      SUBROUTINE CDFNOR(WHICH,P,Q,X,MEAN,SD,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFNOR( WHICH, P, Q, X, MEAN, SD, STATUS, BOUND )
C               Cumulative Distribution Function
C               NORmal distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the normal
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH  --> Integer indicating  which of the  next  parameter
C     values is to be calculated using values  of the others.
C     Legal range: 1..4
C               iwhich = 1 : Calculate P and Q from X,MEAN and SD
C               iwhich = 2 : Calculate X from P,Q,MEAN and SD
C               iwhich = 3 : Calculate MEAN from P,Q,X and SD
C               iwhich = 4 : Calculate SD from P,Q,X and MEAN
C                    INTEGER WHICH
C
C     P <--> The integral from -infinity to X of the normal density.
C            Input range: (0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C     X < --> Upper limit of integration of the normal-density.
C             Input range: ( -infinity, +infinity)
C                    DOUBLE PRECISION X
C
C     MEAN <--> The mean of the normal density.
C               Input range: (-infinity, +infinity)
C                    DOUBLE PRECISION MEAN
C
C     SD <--> Standard Deviation of the normal density.
C             Input range: (0, +infinity).
C                    DOUBLE PRECISION SD
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C
C
C     A slightly modified version of ANORM from
C
C     Cody, W.D. (1993). "ALGORITHM 715: SPECFUN - A Portabel FORTRAN
C     Package of Special Function Routines and Test Drivers"
C     acm Transactions on Mathematical Software. 19, 22-32.
C
C     is used to calulate the  cumulative standard normal distribution.
C
C     The rational functions from pages  90-95  of Kennedy and Gentle,
C     Statistical  Computing,  Marcel  Dekker, NY,  1980 are  used  as
C     starting values to Newton's Iterations which compute the inverse
C     standard normal.  Therefore no  searches  are necessary for  any
C     parameter.
C
C     For X < -15, the asymptotic expansion for the normal is used  as
C     the starting value in finding the inverse standard normal.
C     This is formula 26.2.12 of Abramowitz and Stegun.
C
C
C                              Note
C
C
C      The normal density is proportional to
C      exp( - 0.5 * (( X - MEAN)/SD)**2)
C
C
C**********************************************************************
C     .. Parameters ..
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,MEAN,P,Q,SD,X
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION Z,PQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..

      DOUBLE PRECISION DINVNR,SPMPAR
      EXTERNAL DINVNR,SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL CUMNOR
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      STATUS = 0
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.4))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 4.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LE.0.0D0).OR. (P.GT.1.0D0))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LE.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50
C*PT*WARNING* Constant already double-precision

   40 BOUND = 1.0D0
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.1) GO TO 110
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Q
C
      IF (.NOT. ((Q.LE.0.0D0).OR. (Q.GT.1.0D0))) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Q.LE.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 90
C*PT*WARNING* Constant already double-precision

   80 BOUND = 1.0D0
   90 STATUS = -3
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.1) GO TO 150
C
C     P + Q
C
      PQ = P + Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PQ)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 140
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PQ.LT.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 130
C*PT*WARNING* Constant already double-precision

  120 BOUND = 1.0D0
  130 STATUS = 3
      RETURN

  140 CONTINUE
  150 IF (WHICH.EQ.4) GO TO 170
C*PT*WARNING* Constant already double-precision
C
C     SD
C
      IF (.NOT. (SD.LE.0.0D0)) GO TO 160
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -6
      RETURN

  160 CONTINUE
C
C     Calculate ANSWERS
C
  170 IF ((1).EQ. (WHICH)) THEN
C
C     Computing P
C
          Z = (X-MEAN)/SD
          CALL CUMNOR(Z,P,Q)

      ELSE IF ((2).EQ. (WHICH)) THEN
C
C     Computing X
C
          Z = DINVNR(P,Q)
          X = SD*Z + MEAN

      ELSE IF ((3).EQ. (WHICH)) THEN
C
C     Computing the MEAN
C
          Z = DINVNR(P,Q)
          MEAN = X - SD*Z

      ELSE IF ((4).EQ. (WHICH)) THEN
C
C     Computing SD
C
          Z = DINVNR(P,Q)
          SD = (X-MEAN)/Z
      END IF

      RETURN

      END
      SUBROUTINE CDFPOI(WHICH,P,Q,S,XLAM,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFPOI( WHICH, P, Q, S, XLAM, STATUS, BOUND )
C               Cumulative Distribution Function
C               POIsson distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the Poisson
C     distribution given values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which  argument
C               value is to be calculated from the others.
C               Legal range: 1..3
C               iwhich = 1 : Calculate P and Q from S and XLAM
C               iwhich = 2 : Calculate A from P,Q and XLAM
C               iwhich = 3 : Calculate XLAM from P,Q and S
C                    INTEGER WHICH
C
C        P <--> The cumulation from 0 to S of the poisson density.
C               Input range: [0,1].
C                    DOUBLE PRECISION P
C
C        Q <--> 1-P.
C               Input range: (0, 1].
C               P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C        S <--> Upper limit of cumulation of the Poisson.
C               Input range: [0, +infinity).
C               Search range: [0,1E300]
C                    DOUBLE PRECISION S
C
C     XLAM <--> Mean of the Poisson distribution.
C               Input range: [0, +infinity).
C               Search range: [0,1E300]
C                    DOUBLE PRECISION XLAM
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula   26.4.21  of   Abramowitz  and   Stegun,   Handbook  of
C     Mathematical Functions (1966) is used  to reduce the computation
C     of  the cumulative distribution function to that  of computing a
C     chi-square, hence an incomplete gamma function.
C
C     Cumulative  distribution function  (P) is  calculated  directly.
C     Computation of other parameters involve a seach for a value that
C     produces  the desired value of  P.   The  search relies  on  the
C     monotinicity of P with the other parameter.
C
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION INF
C*PT*WARNING* Constant already double-precision
      PARAMETER (INF=1.0D300)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,P,Q,S,XLAM
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FX,CUM,CCUM,PQ
      LOGICAL QHI,QLEFT,QPORQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPMPAR
      EXTERNAL SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL DINVR,DSTINV,CUMPOI
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.3))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 3.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LT.0.0D0).OR. (P.GT.1.0D0))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50
C*PT*WARNING* Constant already double-precision

   40 BOUND = 1.0D0
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.1) GO TO 110
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Q
C
      IF (.NOT. ((Q.LE.0.0D0).OR. (Q.GT.1.0D0))) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Q.LE.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 90
C*PT*WARNING* Constant already double-precision

   80 BOUND = 1.0D0
   90 STATUS = -3
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.2) GO TO 130
C*PT*WARNING* Constant already double-precision
C
C     S
C
      IF (.NOT. (S.LT.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -4
      RETURN

  120 CONTINUE
  130 IF (WHICH.EQ.3) GO TO 150
C*PT*WARNING* Constant already double-precision
C
C     XLAM
C
      IF (.NOT. (XLAM.LT.0.0D0)) GO TO 140
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -5
      RETURN

  140 CONTINUE
  150 IF (WHICH.EQ.1) GO TO 190
C
C     P + Q
C
      PQ = P + Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PQ)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 180
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PQ.LT.0.0D0)) GO TO 160
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 170
C*PT*WARNING* Constant already double-precision

  160 BOUND = 1.0D0
  170 STATUS = 3
      RETURN

  180 CONTINUE
  190 IF (.NOT. (WHICH.EQ.1)) QPORQ = P .LE. Q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (WHICH)) THEN
C
C     Calculating P
C
          CALL CUMPOI(S,XLAM,P,Q)
          STATUS = 0

      ELSE IF ((2).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating S
C
          S = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,S,FX,QLEFT,QHI)
  200     IF (.NOT. (STATUS.EQ.1)) GO TO 230
          CALL CUMPOI(S,XLAM,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 210
          FX = CUM - P
          GO TO 220

  210     FX = CCUM - Q
  220     CALL DINVR(STATUS,S,FX,QLEFT,QHI)
          GO TO 200

  230     IF (.NOT. (STATUS.EQ.-1)) GO TO 260
          IF (.NOT. (QLEFT)) GO TO 240
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 250

  240     STATUS = 2
          BOUND = INF
  250     CONTINUE
  260     CONTINUE

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Calculating XLAM
C
          XLAM = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(0.0D0,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,XLAM,FX,QLEFT,QHI)
  270     IF (.NOT. (STATUS.EQ.1)) GO TO 300
          CALL CUMPOI(S,XLAM,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 280
          FX = CUM - P
          GO TO 290

  280     FX = CCUM - Q
  290     CALL DINVR(STATUS,XLAM,FX,QLEFT,QHI)
          GO TO 270

  300     IF (.NOT. (STATUS.EQ.-1)) GO TO 330
          IF (.NOT. (QLEFT)) GO TO 310
          STATUS = 1
C*PT*WARNING* Constant already double-precision
          BOUND = 0.0D0
          GO TO 320

  310     STATUS = 2
          BOUND = INF
  320     CONTINUE
  330 END IF

      RETURN

      END
      SUBROUTINE CDFT(WHICH,P,Q,T,DF,STATUS,BOUND)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      SUBROUTINE CDFT( WHICH, P, Q, T, DF, STATUS, BOUND )
C               Cumulative Distribution Function
C                         T distribution
C
C
C                              Function
C
C
C     Calculates any one parameter of the t distribution given
C     values for the others.
C
C
C                              Arguments
C
C
C     WHICH --> Integer indicating which  argument
C               values is to be calculated from the others.
C               Legal range: 1..3
C               iwhich = 1 : Calculate P and Q from T and DF
C               iwhich = 2 : Calculate T from P,Q and DF
C               iwhich = 3 : Calculate DF from P,Q and T
C                    INTEGER WHICH
C
C        P <--> The integral from -infinity to t of the t-density.
C              Input range: (0,1].
C                    DOUBLE PRECISION P
C
C     Q <--> 1-P.
C            Input range: (0, 1].
C            P + Q = 1.0.
C                    DOUBLE PRECISION Q
C
C        T <--> Upper limit of integration of the t-density.
C               Input range: ( -infinity, +infinity).
C               Search range: [ -1E300, 1E300 ]
C                    DOUBLE PRECISION T
C
C        DF <--> Degrees of freedom of the t-distribution.
C                Input range: (0 , +infinity).
C                Search range: [1e-300, 1E10]
C                    DOUBLE PRECISION DF
C
C     STATUS <-- 0 if calculation completed correctly
C               -I if input parameter number I is out of range
C                1 if answer appears to be lower than lowest
C                  search bound
C                2 if answer appears to be higher than greatest
C                  search bound
C                3 if P + Q .ne. 1
C                    INTEGER STATUS
C
C     BOUND <-- Undefined if STATUS is 0
C
C               Bound exceeded by parameter number I if STATUS
C               is negative.
C
C               Lower search bound if STATUS is 1.
C
C               Upper search bound if STATUS is 2.
C
C
C                              Method
C
C
C     Formula  26.5.27  of   Abramowitz   and  Stegun,   Handbook   of
C     Mathematical Functions  (1966) is used to reduce the computation
C     of the cumulative distribution function to that of an incomplete
C     beta.
C
C     Computation of other parameters involve a seach for a value that
C     produces  the desired  value  of P.   The search relies  on  the
C     monotinicity of P with the other parameter.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION TOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (TOL=1.0D-8)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ATOL
C*PT*WARNING* Constant already double-precision
      PARAMETER (ATOL=1.0D-50)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ZERO,INF
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      PARAMETER (ZERO=1.0D-300,INF=1.0D300)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION MAXDF
C*PT*WARNING* Constant already double-precision
      PARAMETER (MAXDF=1.0d10)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION BOUND,DF,P,Q,T
      INTEGER STATUS,WHICH
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION FX,CUM,CCUM,PQ
      LOGICAL QHI,QLEFT,QPORQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPMPAR,DT1
      EXTERNAL SPMPAR,DT1
C     ..
C     .. External Subroutines ..
      EXTERNAL DINVR,DSTINV,CUMT
C     ..
C     .. Executable Statements ..
C
C     Check arguments
C
      IF (.NOT. ((WHICH.LT.1).OR. (WHICH.GT.3))) GO TO 30
      IF (.NOT. (WHICH.LT.1)) GO TO 10
C*PT*WARNING* Constant already double-precision
      BOUND = 1.0D0
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 BOUND = 3.0D0
   20 STATUS = -1
      RETURN

   30 IF (WHICH.EQ.1) GO TO 70
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     P
C
      IF (.NOT. ((P.LE.0.0D0).OR. (P.GT.1.0D0))) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.LE.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 50
C*PT*WARNING* Constant already double-precision

   40 BOUND = 1.0D0
   50 STATUS = -2
      RETURN

   60 CONTINUE
   70 IF (WHICH.EQ.1) GO TO 110
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     Q
C
      IF (.NOT. ((Q.LE.0.0D0).OR. (Q.GT.1.0D0))) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Q.LE.0.0D0)) GO TO 80
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 90
C*PT*WARNING* Constant already double-precision

   80 BOUND = 1.0D0
   90 STATUS = -3
      RETURN

  100 CONTINUE
  110 IF (WHICH.EQ.3) GO TO 130
C*PT*WARNING* Constant already double-precision
C
C     DF
C
      IF (.NOT. (DF.LE.0.0D0)) GO TO 120
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      STATUS = -5
      RETURN

  120 CONTINUE
  130 IF (WHICH.EQ.1) GO TO 170
C
C     P + Q
C
      PQ = P + Q
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (ABS(((PQ)-0.5D0)-0.5D0).GT. (3.0D0*SPMPAR(1))))
     +    GO TO 160
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (PQ.LT.0.0D0)) GO TO 140
C*PT*WARNING* Constant already double-precision
      BOUND = 0.0D0
      GO TO 150
C*PT*WARNING* Constant already double-precision

  140 BOUND = 1.0D0
  150 STATUS = 3
      RETURN

  160 CONTINUE
  170 IF (.NOT. (WHICH.EQ.1)) QPORQ = P .LE. Q
C
C     Select the minimum of P or Q
C
C
C     Calculate ANSWERS
C
      IF ((1).EQ. (WHICH)) THEN
C
C     Computing P and Q
C
          CALL CUMT(T,DF,P,Q)
          STATUS = 0

      ELSE IF ((2).EQ. (WHICH)) THEN
C
C     Computing T
C
C     .. Get initial approximation for T
C
          T = DT1(P,Q,DF)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(-INF,INF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,T,FX,QLEFT,QHI)
  180     IF (.NOT. (STATUS.EQ.1)) GO TO 210
          CALL CUMT(T,DF,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 190
          FX = CUM - P
          GO TO 200

  190     FX = CCUM - Q
  200     CALL DINVR(STATUS,T,FX,QLEFT,QHI)
          GO TO 180

  210     IF (.NOT. (STATUS.EQ.-1)) GO TO 240
          IF (.NOT. (QLEFT)) GO TO 220
          STATUS = 1
          BOUND = -INF
          GO TO 230

  220     STATUS = 2
          BOUND = INF
  230     CONTINUE
  240     CONTINUE

      ELSE IF ((3).EQ. (WHICH)) THEN
C*PT*WARNING* Constant already double-precision
C
C     Computing DF
C
          DF = 5.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
          CALL DSTINV(ZERO,MAXDF,0.5D0,0.5D0,5.0D0,ATOL,TOL)
          STATUS = 0
          CALL DINVR(STATUS,DF,FX,QLEFT,QHI)
  250     IF (.NOT. (STATUS.EQ.1)) GO TO 280
          CALL CUMT(T,DF,CUM,CCUM)
          IF (.NOT. (QPORQ)) GO TO 260
          FX = CUM - P
          GO TO 270

  260     FX = CCUM - Q
  270     CALL DINVR(STATUS,DF,FX,QLEFT,QHI)
          GO TO 250

  280     IF (.NOT. (STATUS.EQ.-1)) GO TO 310
          IF (.NOT. (QLEFT)) GO TO 290
          STATUS = 1
          BOUND = ZERO
          GO TO 300

  290     STATUS = 2
          BOUND = MAXDF
  300     CONTINUE
  310 END IF

      RETURN

      END
      SUBROUTINE CUMBET(X,Y,A,B,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE CUMBET(X,Y,A,B,CUM,CCUM)
C          Double precision cUMulative incomplete BETa distribution
C
C
C                              Function
C
C
C     Calculates the cdf to X of the incomplete beta distribution
C     with parameters a and b.  This is the integral from 0 to x
C     of (1/B(a,b))*f(t)) where f(t) = t**(a-1) * (1-t)**(b-1)
C
C
C                              Arguments
C
C
C     X --> Upper limit of integration.
C                                        X is DOUBLE PRECISION
C
C     Y --> 1 - X.
C                                        Y is DOUBLE PRECISION
C
C     A --> First parameter of the beta distribution.
C                                        A is DOUBLE PRECISION
C
C     B --> Second parameter of the beta distribution.
C                                        B is DOUBLE PRECISION
C
C     CUM <-- Cumulative incomplete beta distribution.
C                                        CUM is DOUBLE PRECISION
C
C     CCUM <-- Compliment of Cumulative incomplete beta distribution.
C                                        CCUM is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Calls the routine BRATIO.
C
C                                   References
C
C     Didonato, Armido R. and Morris, Alfred H. Jr. (1992) Algorithim
C     708 Significant Digit Computation of the Incomplete Beta Function
C     Ratios. ACM ToMS, Vol.18, No. 3, Sept. 1992, 360-373.
C
C**********************************************************************

C     .. Scalar Arguments ..
      DOUBLE PRECISION X,Y,A,B,CUM,CCUM
C     ..
C     .. Local Scalars ..
      INTEGER IERR
C     ..
C     .. External Routines ..
      EXTERNAL BRATIO
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
      IF (.NOT. (X.LE.0.0D0)) GO TO 10
C*PT*WARNING* Constant already double-precision
      CUM = 0.0D0
C*PT*WARNING* Constant already double-precision
      CCUM = 1.0D0
      RETURN
C*PT*WARNING* Constant already double-precision

   10 IF (.NOT. (Y.LE.0.0D0)) GO TO 20
C*PT*WARNING* Constant already double-precision
      CUM = 1.0D0
C*PT*WARNING* Constant already double-precision
      CCUM = 0.0D0
      RETURN

   20 CALL BRATIO(A,B,X,Y,CUM,CCUM,IERR)

C     Call bratio routine


      RETURN

      END
      SUBROUTINE CUMBIN(S,XN,PR,OMPR,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE CUMBIN(S,XN,PBIN,OMPR,CUM,CCUM)
C                    CUmulative BINomial distribution
C
C
C                              Function
C
C
C     Returns the probability   of 0  to  S  successes in  XN   binomial
C     trials, each of which has a probability of success, PBIN.
C
C
C                              Arguments
C
C
C     S --> The upper limit of cumulation of the binomial distribution.
C                                                  S is DOUBLE PRECISION
C
C     XN --> The number of binomial trials.
C                                                  XN is DOUBLE PRECISIO
C
C     PBIN --> The probability of success in each binomial trial.
C                                                  PBIN is DOUBLE PRECIS
C
C     OMPR --> 1 - PBIN
C                                                  OMPR is DOUBLE PRECIS
C
C     CUM <-- Cumulative binomial distribution.
C                                                  CUM is DOUBLE PRECISI
C
C     CCUM <-- Compliment of Cumulative binomial distribution.
C                                                  CCUM is DOUBLE PRECIS

C
C
C                              Method
C
C
C     Formula  26.5.24    of   Abramowitz  and    Stegun,  Handbook   of
C     Mathematical   Functions (1966) is   used  to reduce the  binomial
C     distribution  to  the  cumulative    beta distribution.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION PR,OMPR,S,XN,CUM,CCUM
C     ..
C     .. External Subroutines ..
      EXTERNAL CUMBET
C     ..
C     .. Executable Statements ..
      IF (.NOT. (S.LT.XN)) GO TO 10
C*PT*WARNING* Constant already double-precision
      CALL CUMBET(PR,OMPR,S+1.0D0,XN-S,CCUM,CUM)
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 CUM = 1.0D0
C*PT*WARNING* Constant already double-precision
      CCUM = 0.0D0
   20 RETURN

      END
      SUBROUTINE CUMCHI(X,DF,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE FUNCTION CUMCHI(X,DF,CUM,CCUM)
C             CUMulative of the CHi-square distribution
C
C
C                              Function
C
C
C     Calculates the cumulative chi-square distribution.
C
C
C                              Arguments
C
C
C     X       --> Upper limit of integration of the
C                 chi-square distribution.
C                                                 X is DOUBLE PRECISION
C
C     DF      --> Degrees of freedom of the
C                 chi-square distribution.
C                                                 DF is DOUBLE PRECISION
C
C     CUM <-- Cumulative chi-square distribution.
C                                                 CUM is DOUBLE PRECISIO
C
C     CCUM <-- Compliment of Cumulative chi-square distribution.
C                                                 CCUM is DOUBLE PRECISI
C
C
C                              Method
C
C
C     Calls incomplete gamma function (CUMGAM)
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION DF,X,CUM,CCUM
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,XX
C     ..
C     .. External Subroutines ..
      EXTERNAL CUMGAM
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
      A = DF*0.5D0
C*PT*WARNING* Constant already double-precision
      XX = X*0.5D0
      CALL CUMGAM(XX,A,CUM,CCUM)
      RETURN

      END
      SUBROUTINE CUMCHN(X,DF,PNONC,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE CUMCHN(X,DF,PNONC,CUM,CCUM)
C             CUMulative of the Non-central CHi-square distribution
C
C
C                              Function
C
C
C     Calculates     the       cumulative      non-central    chi-square
C     distribution, i.e.,  the probability   that  a   random   variable
C     which    follows  the  non-central chi-square  distribution,  with
C     non-centrality  parameter    PNONC  and   continuous  degrees   of
C     freedom DF, is less than or equal to X.
C
C
C                              Arguments
C
C
C     X       --> Upper limit of integration of the non-central
C                 chi-square distribution.
C                                                 X is DOUBLE PRECISION
C
C     DF      --> Degrees of freedom of the non-central
C                 chi-square distribution.
C                                                 DF is DOUBLE PRECISION
C
C     PNONC   --> Non-centrality parameter of the non-central
C                 chi-square distribution.
C                                                 PNONC is DOUBLE PRECIS
C
C     CUM <-- Cumulative non-central chi-square distribution.
C                                                 CUM is DOUBLE PRECISIO
C
C     CCUM <-- Compliment of Cumulative non-central chi-square distribut
C                                                 CCUM is DOUBLE PRECISI

C
C
C                              Method
C
C
C     Uses  formula  26.4.25   of  Abramowitz  and  Stegun, Handbook  of
C     Mathematical    Functions,  US   NBS   (1966)    to calculate  the
C     non-central chi-square.
C
C
C                              Variables
C
C
C     EPS     --- Convergence criterion.  The sum stops when a
C                 term is less than EPS*SUM.
C                                                 EPS is DOUBLE PRECISIO
C
C     NTIRED  --- Maximum number of terms to be evaluated
C                 in each sum.
C                                                 NTIRED is INTEGER
C
C     QCONV   --- .TRUE. if convergence achieved -
C                 i.e., program did not stop on NTIRED criterion.
C                                                 QCONV is LOGICAL
C
C     CCUM <-- Compliment of Cumulative non-central
C              chi-square distribution.
C                                                 CCUM is DOUBLE PRECISI
C
C**********************************************************************
C
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DF,PNONC,X,CUM,CCUM
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ADJ,CENTAJ,CENTWT,CHID2,DFD2,EPS,LCNTAJ,LCNTWT,
     +                 LFACT,PCENT,PTERM,SUM,SUMADJ,TERM,WT,XNONC,XX
      INTEGER I,ICENT,ITERB,ITERF,NTIRED
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALNGAM
      EXTERNAL ALNGAM
C     ..
C     .. External Subroutines ..
      EXTERNAL CUMCHI
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LOG,EXP,DBLE,INT
C*PT*WARNING* Already double-precision
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION DG
      LOGICAL QSMALL,QTIRED
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA NTIRED,EPS/1000,1.0D-5/
C     ..
C     .. Statement Function definitions ..
      QTIRED(I) = I .GT. NTIRED
C*PT*WARNING* Constant already double-precision
      QSMALL(XX) = SUM .LT. 1.0D-20 .OR. XX .LT. EPS*SUM
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Already double-precision (DBLE)
      DG(I) = DF + 2.0D0*DBLE(I)
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C
      IF (.NOT. (X.LE.0.0D0)) GO TO 10
C*PT*WARNING* Constant already double-precision
      CUM = 0.0D0
C*PT*WARNING* Constant already double-precision
      CCUM = 1.0D0
      RETURN
C*PT*WARNING* Constant already double-precision

   10 IF (.NOT. (PNONC.LE.1.0D-10)) GO TO 20
C
C
C     When non-centrality parameter is (essentially) zero,
C     use cumulative chi-square distribution
C
C
      CALL CUMCHI(X,DF,CUM,CCUM)
      RETURN
C*PT*WARNING* Constant already double-precision

   20 XNONC = PNONC/2.0D0
C**********************************************************************
C
C     The following code calcualtes the weight, chi-square, and
C     adjustment term for the central term in the infinite series.
C     The central term is the one in which the poisson weight is
C     greatest.  The adjustment term is the amount that must
C     be subtracted from the chi-square to move up two degrees
C     of freedom.
C
C**********************************************************************
      ICENT = INT(XNONC)
      IF (ICENT.EQ.0) ICENT = 1
C*PT*WARNING* Constant already double-precision
      CHID2 = X/2.0D0
C*PT*WARNING* Already double-precision (DBLE)
C
C
C     Calculate central weight term
C
C
      LFACT = ALNGAM(DBLE(ICENT+1))
      LCNTWT = -XNONC + ICENT*LOG(XNONC) - LFACT
      CENTWT = EXP(LCNTWT)
C
C
C     Calculate central chi-square
C
C
      CALL CUMCHI(X,DG(ICENT),PCENT,CCUM)
C*PT*WARNING* Constant already double-precision
C
C
C     Calculate central adjustment term
C
C
      DFD2 = DG(ICENT)/2.0D0
C*PT*WARNING* Constant already double-precision
      LFACT = ALNGAM(1.0D0+DFD2)
      LCNTAJ = DFD2*LOG(CHID2) - CHID2 - LFACT
      CENTAJ = EXP(LCNTAJ)
      SUM = CENTWT*PCENT
C**********************************************************************
C
C     Sum backwards from the central term towards zero.
C     Quit whenever either
C     (1) the zero term is reached, or
C     (2) the term gets small relative to the sum, or
C     (3) More than NTIRED terms are totaled.
C
C**********************************************************************
      ITERB = 0
C*PT*WARNING* Constant already double-precision
      SUMADJ = 0.0D0
      ADJ = CENTAJ
      WT = CENTWT
      I = ICENT
C
      GO TO 40

   30 IF (QTIRED(ITERB) .OR. QSMALL(TERM) .OR. I.EQ.0) GO TO 50
C*PT*WARNING* Constant already double-precision
   40 DFD2 = DG(I)/2.0D0
C
C
C     Adjust chi-square for two fewer degrees of freedom.
C     The adjusted value ends up in PTERM.
C
C
      ADJ = ADJ*DFD2/CHID2
      SUMADJ = SUMADJ + ADJ
      PTERM = PCENT + SUMADJ
C
C
C     Adjust poisson weight for J decreased by one
C
C
      WT = WT* (I/XNONC)
      TERM = WT*PTERM
      SUM = SUM + TERM
      I = I - 1
      ITERB = ITERB + 1
      GO TO 30

   50 ITERF = 0
C**********************************************************************
C
C     Now sum forward from the central term towards infinity.
C     Quit when either
C     (1) the term gets small relative to the sum, or
C     (2) More than NTIRED terms are totaled.
C
C**********************************************************************
      SUMADJ = CENTAJ
      ADJ = CENTAJ
      WT = CENTWT
      I = ICENT
C
      GO TO 70

   60 IF (QTIRED(ITERF) .OR. QSMALL(TERM)) GO TO 80
C
C
C     Update weights for next higher J
C
C
   70 WT = WT* (XNONC/ (I+1))
C
C
C     Calculate PTERM and add term to sum
C
C
      PTERM = PCENT - SUMADJ
      TERM = WT*PTERM
      SUM = SUM + TERM
C
C
C     Update adjustment term for DF for next iteration
C
C
      I = I + 1
C*PT*WARNING* Constant already double-precision
      DFD2 = DG(I)/2.0D0
      ADJ = ADJ*CHID2/DFD2
      SUMADJ = SUMADJ + ADJ
      ITERF = ITERF + 1
      GO TO 60

   80 CUM = SUM
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      CCUM = 0.5D0 + (0.5D0-CUM)
C
      RETURN

      END
      SUBROUTINE CUMF(F,DFN,DFD,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE CUMF(F,DFN,DFD,CUM,CCUM)
C                    CUMulative F distribution
C
C
C                              Function
C
C
C     Computes  the  integral from  0  to  F of  the f-density  with DFN
C     and DFD degrees of freedom.
C
C
C                              Arguments
C
C
C     F --> Upper limit of integration of the f-density.
C                                                  F is DOUBLE PRECISION
C
C     DFN --> Degrees of freedom of the numerator sum of squares.
C                                                  DFN is DOUBLE PRECISI
C
C     DFD --> Degrees of freedom of the denominator sum of squares.
C                                                  DFD is DOUBLE PRECISI
C
C     CUM <-- Cumulative f distribution.
C                                                  CUM is DOUBLE PRECISI
C
C     CCUM <-- Compliment of Cumulative f distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C
C                              Method
C
C
C     Formula  26.5.28 of  Abramowitz and   Stegun   is  used to  reduce
C     the cumulative F to a cumulative beta distribution.
C
C
C                              Note
C
C
C     If F is less than or equal to 0, 0 is returned.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION DFD,DFN,F,CUM,CCUM
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..

      DOUBLE PRECISION DSUM,PROD,XX,YY
      INTEGER IERR
C*PT*WARNING* Already double-precision
C     ..
C     .. Parameters ..
      DOUBLE PRECISION HALF
C*PT*WARNING* Constant already double-precision
      PARAMETER (HALF=0.5D0)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DONE
C*PT*WARNING* Constant already double-precision
      PARAMETER (DONE=1.0D0)
C     ..
C     .. External Subroutines ..
      EXTERNAL BRATIO
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..

      IF (.NOT. (F.LE.0.0D0)) GO TO 10
C*PT*WARNING* Constant already double-precision
      CUM = 0.0D0
C*PT*WARNING* Constant already double-precision
      CCUM = 1.0D0
      RETURN

   10 PROD = DFN*F
C
C     XX is such that the incomplete beta with parameters
C     DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM
C
C     YY is 1 - XX
C
C     Calculate the smaller of XX and YY accurately
C
      DSUM = DFD + PROD
      XX = DFD/DSUM
      IF (XX.GT.HALF) THEN
          YY = PROD/DSUM
          XX = DONE - YY

      ELSE
          YY = DONE - XX
      END IF

      CALL BRATIO(DFD*HALF,DFN*HALF,XX,YY,CCUM,CUM,IERR)
      RETURN

      END
      SUBROUTINE CUMFNC(F,DFN,DFD,PNONC,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C               F -NON- -C-ENTRAL F DISTRIBUTION
C
C
C
C                              Function
C
C
C     COMPUTES NONCENTRAL F DISTRIBUTION WITH DFN AND DFD
C     DEGREES OF FREEDOM AND NONCENTRALITY PARAMETER PNONC
C
C
C                              Arguments
C
C
C     X --> UPPER LIMIT OF INTEGRATION OF NONCENTRAL F IN EQUATION
C
C     DFN --> DEGREES OF FREEDOM OF NUMERATOR
C
C     DFD -->  DEGREES OF FREEDOM OF DENOMINATOR
C
C     PNONC --> NONCENTRALITY PARAMETER.
C
C     CUM <-- CUMULATIVE NONCENTRAL F DISTRIBUTION
C
C     CCUM <-- COMPLIMENT OF CUMMULATIVE
C
C
C                              Method
C
C
C     USES FORMULA 26.6.20 OF REFERENCE FOR INFINITE SERIES.
C     SERIES IS CALCULATED BACKWARD AND FORWARD FROM J = LAMBDA/2
C     (THIS IS THE TERM WITH THE LARGEST POISSON WEIGHT) UNTIL
C     THE CONVERGENCE CRITERION IS MET.
C
C     FOR SPEED, THE INCOMPLETE BETA FUNCTIONS ARE EVALUATED
C     BY FORMULA 26.5.16.
C
C
C               REFERENCE
C
C
C     HANDBOOD OF MATHEMATICAL FUNCTIONS
C     EDITED BY MILTON ABRAMOWITZ AND IRENE A. STEGUN
C     NATIONAL BUREAU OF STANDARDS APPLIED MATEMATICS SERIES - 55
C     MARCH 1965
C     P 947, EQUATIONS 26.6.17, 26.6.18
C
C
C                              Note
C
C
C     THE SUM CONTINUES UNTIL A SUCCEEDING TERM IS LESS THAN EPS
C     TIMES THE SUM (OR THE SUM IS LESS THAN 1.0E-20).  EPS IS
C     SET TO 1.0E-4 IN A DATA STATEMENT WHICH CAN BE CHANGED.
C
C**********************************************************************

C     .. Scalar Arguments ..
      DOUBLE PRECISION DFD,DFN,PNONC,F,CUM,CCUM
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DSUM,DUMMY,PROD,XX,YY
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION ADN,AUP,B,BETDN,BETUP,CENTWT,DNTERM,EPS,SUM,
     +                 UPTERM,XMULT,XNONC,X
      INTEGER I,ICENT,IERR
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALNGAM
      EXTERNAL ALNGAM
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LOG,DBLE,EXP
C     ..
C     .. Statement Functions ..
      LOGICAL QSMALL
C     ..
C     .. External Subroutines ..
      EXTERNAL BRATIO,CUMF
C*PT*WARNING* Already double-precision
C     ..
C     .. Parameters ..
      DOUBLE PRECISION HALF
C*PT*WARNING* Constant already double-precision
      PARAMETER (HALF=0.5D0)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DONE
C*PT*WARNING* Constant already double-precision
      PARAMETER (DONE=1.0D0)
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA EPS/1.0D-4/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Statement Function definitions ..
      QSMALL(X) = SUM .LT. 1.0D-20 .OR. X .LT. EPS*SUM
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C
      IF (.NOT. (F.LE.0.0D0)) GO TO 10
C*PT*WARNING* Constant already double-precision
      CUM = 0.0D0
C*PT*WARNING* Constant already double-precision
      CCUM = 1.0D0
      RETURN
C*PT*WARNING* Constant already double-precision

   10 IF (.NOT. (PNONC.LT.1.0D-10)) GO TO 20
C
C     Handle case in which the non-centrality parameter is
C     (essentially) zero.

      CALL CUMF(F,DFN,DFD,CUM,CCUM)
      RETURN
C*PT*WARNING* Constant already double-precision

   20 XNONC = PNONC/2.0D0

C     Calculate the central term of the poisson weighting factor.

      ICENT = XNONC
      IF (ICENT.EQ.0) ICENT = 1
C*PT*WARNING* Already double-precision (DBLE)

C     Compute central weight term

      CENTWT = EXP(-XNONC+ICENT*LOG(XNONC)-ALNGAM(DBLE(ICENT+1)))

C     Compute central incomplete beta term
C     Assure that minimum of arg to beta and 1 - arg is computed
C          accurately.

      PROD = DFN*F
      DSUM = DFD + PROD
      YY = DFD/DSUM
      IF (YY.GT.HALF) THEN
          XX = PROD/DSUM
          YY = DONE - XX

      ELSE
          XX = DONE - YY
      END IF
C*PT*WARNING* Already double-precision (DBLE)

      CALL BRATIO(DFN*HALF+DBLE(ICENT),DFD*HALF,XX,YY,BETDN,DUMMY,IERR)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Already double-precision (DBLE)
      ADN = DFN/2.0D0 + DBLE(ICENT)
      AUP = ADN
C*PT*WARNING* Constant already double-precision
      B = DFD/2.0D0
      BETUP = BETDN
      SUM = CENTWT*BETDN

C     Now sum terms backward from icent until convergence or all done

      XMULT = CENTWT
      I = ICENT
C*PT*WARNING* Constant already double-precision
      DNTERM = EXP(ALNGAM(ADN+B)-ALNGAM(ADN+1.0D0)-ALNGAM(B)+
     +         ADN*LOG(XX)+B*LOG(YY))
   30 IF (QSMALL(XMULT*BETDN) .OR. I.LE.0) GO TO 40
      XMULT = XMULT* (I/XNONC)
      I = I - 1
      ADN = ADN - 1
      DNTERM = (ADN+1)/ ((ADN+B)*XX)*DNTERM
      BETDN = BETDN + DNTERM
      SUM = SUM + XMULT*BETDN
      GO TO 30

   40 I = ICENT + 1

C     Now sum forwards until convergence

      XMULT = CENTWT
      IF ((AUP-1+B).EQ.0) THEN
          UPTERM = EXP(-ALNGAM(AUP)-ALNGAM(B)+ (AUP-1)*LOG(XX)+
     +             B*LOG(YY))

      ELSE
          UPTERM = EXP(ALNGAM(AUP-1+B)-ALNGAM(AUP)-ALNGAM(B)+
     +             (AUP-1)*LOG(XX)+B*LOG(YY))
      END IF

      GO TO 60

   50 IF (QSMALL(XMULT*BETUP)) GO TO 70
   60 XMULT = XMULT* (XNONC/I)
      I = I + 1
      AUP = AUP + 1
C*PT*WARNING* Constant already double-precision
      UPTERM = (AUP+B-2.0D0)*XX/ (AUP-1)*UPTERM
      BETUP = BETUP - UPTERM
      SUM = SUM + XMULT*BETUP
      GO TO 50

   70 CUM = SUM
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision

      CCUM = 0.5D0 + (0.5D0-CUM)
      RETURN

      END
      SUBROUTINE CUMGAM(X,A,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE CUMGAM(X,A,CUM,CCUM)
C           Double precision cUMulative incomplete GAMma distribution
C
C
C                              Function
C
C
C     Computes   the  cumulative        of    the     incomplete   gamma
C     distribution, i.e., the integral from 0 to X of
C          (1/GAM(A))*EXP(-T)*T**(A-1) DT
C     where GAM(A) is the complete gamma function of A, i.e.,
C          GAM(A) = integral from 0 to infinity of
C                    EXP(-T)*T**(A-1) DT
C
C
C                              Arguments
C
C
C     X --> The upper limit of integration of the incomplete gamma.
C                                                X is DOUBLE PRECISION
C
C     A --> The shape parameter of the incomplete gamma.
C                                                A is DOUBLE PRECISION
C
C     CUM <-- Cumulative incomplete gamma distribution.
C                                        CUM is DOUBLE PRECISION
C
C     CCUM <-- Compliment of Cumulative incomplete gamma distribution.
C                                                CCUM is DOUBLE PRECISIO
C
C
C                              Method
C
C
C     Calls the routine GRATIO.
C
C**********************************************************************
C
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,X,CUM,CCUM
C     ..
C     .. External Routines ..
      EXTERNAL GRATIO
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
      IF (.NOT. (X.LE.0.0D0)) GO TO 10
C*PT*WARNING* Constant already double-precision
      CUM = 0.0D0
C*PT*WARNING* Constant already double-precision
      CCUM = 1.0D0
      RETURN

   10 CALL GRATIO(A,X,CUM,CCUM,0)

C     Call gratio routine

      RETURN

      END
      SUBROUTINE CUMNBN(S,XN,PR,OMPR,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE CUMNNBN(S,XN,PR,OMPR,CUM,CCUM)
C                    CUmulative Negative BINomial distribution
C
C
C                              Function
C
C
C     Returns the probability that it there will be S or fewer failures
C     before there are XN successes, with each binomial trial having
C     a probability of success PR.
C
C     Prob(# failures = S | XN successes, PR)  =
C                        ( XN + S - 1 )
C                        (            ) * PR^XN * (1-PR)^S
C                        (      S     )
C
C
C                              Arguments
C
C
C     S --> The number of failures
C                                                  S is DOUBLE PRECISION
C
C     XN --> The number of successes
C                                                  XN is DOUBLE PRECISIO
C
C     PR --> The probability of success in each binomial trial.
C                                                  PR is DOUBLE PRECISIO
C
C     OMPR --> 1 - PR
C                                                  OMPR is DOUBLE PRECIS
C
C     CUM <-- Cumulative negative binomial distribution.
C                                                  CUM is DOUBLE PRECISI
C
C     CCUM <-- Compliment of Cumulative negative binomial distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C
C                              Method
C
C
C     Formula  26.5.26    of   Abramowitz  and    Stegun,  Handbook   of
C     Mathematical   Functions (1966) is   used  to reduce the  negative
C     binomial distribution to the cumulative beta distribution.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION PR,OMPR,S,XN,CUM,CCUM
C     ..
C     .. External Subroutines ..
      EXTERNAL CUMBET
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
      CALL CUMBET(PR,OMPR,XN,S+1.D0,CUM,CCUM)
      RETURN

      END
      SUBROUTINE CUMNOR(ARG,RESULT,CCUM)
C**********************************************************************
C
C     SUBROUINE CUMNOR(X,RESULT,CCUM)
C
C
C                              Function
C
C
C     Computes the cumulative  of    the  normal   distribution,   i.e.,
C     the integral from -infinity to x of
C          (1/sqrt(2*pi)) exp(-u*u/2) du
C
C     X --> Upper limit of integration.
C                                        X is DOUBLE PRECISION
C
C     RESULT <-- Cumulative normal distribution.
C                                        RESULT is DOUBLE PRECISION
C
C     CCUM <-- Compliment of Cumulative normal distribution.
C                                        CCUM is DOUBLE PRECISION
C
C
C     Renaming of function ANORM from:
C
C     Cody, W.D. (1993). "ALGORITHM 715: SPECFUN - A Portabel FORTRAN
C     Package of Special Function Routines and Test Drivers"
C     acm Transactions on Mathematical Software. 19, 22-32.
C
C     with slight modifications to return ccum and to deal with
C     machine constants.
C
C**********************************************************************
C
C
C Original Comments:
C------------------------------------------------------------------
C
C This function evaluates the normal distribution function:
C
C                              / x
C                     1       |       -t*t/2
C          P(x) = ----------- |      e       dt
C                 sqrt(2 pi)  |
C                             /-oo
C
C   The main computation evaluates near-minimax approximations
C   derived from those in "Rational Chebyshev approximations for
C   the error function" by W. J. Cody, Math. Comp., 1969, 631-637.
C   This transportable program uses rational functions that
C   theoretically approximate the normal distribution function to
C   at least 18 significant decimal digits.  The accuracy achieved
C   depends on the arithmetic system, the compiler, the intrinsic
C   functions, and proper selection of the machine-dependent
C   constants.
C
C*******************************************************************
C*******************************************************************
C
C Explanation of machine-dependent constants.
C
C   MIN   = smallest machine representable number.
C
C   EPS   = argument below which anorm(x) may be represented by
C           0.5  and above which  x*x  will not underflow.
C           A conservative value is the largest machine number X
C           such that   1.0 + X = 1.0   to machine precision.
C*******************************************************************
C*******************************************************************
C
C Error returns
C
C  The program returns  ANORM = 0     for  ARG .LE. XLOW.
C
C
C Intrinsic functions required are:
C
C     ABS, AINT, EXP
C
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C  Latest modification: March 15, 1992
C
C------------------------------------------------------------------
      INTEGER I
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION A,ARG,B,C,D,DEL,EPS,HALF,P,ONE,Q,RESULT,SIXTEN,
     +                 TEMP,SQRPI,THRSH,ROOT32,X,XDEN,XNUM,Y,XSQ,ZERO,
     +                 MIN,CCUM
      DIMENSION A(5),B(4),C(9),D(8),P(6),Q(5)
C*PT*WARNING* Already double-precision
C------------------------------------------------------------------
C  External Function
C------------------------------------------------------------------
      DOUBLE PRECISION SPMPAR
      EXTERNAL SPMPAR
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C------------------------------------------------------------------
C  Mathematical constants
C
C  SQRPI = 1 / sqrt(2*pi), ROOT32 = sqrt(32), and
C  THRSH is the argument for which anorm = 0.75.
C------------------------------------------------------------------
      DATA ONE,HALF,ZERO,SIXTEN/1.0D0,0.5D0,0.0D0,1.60D0/,
     +     SQRPI/3.9894228040143267794D-1/,THRSH/0.66291D0/,
     +     ROOT32/5.656854248D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C------------------------------------------------------------------
C  Coefficients for approximation in first interval
C------------------------------------------------------------------
      DATA A/2.2352520354606839287D00,1.6102823106855587881D02,
     +     1.0676894854603709582D03,1.8154981253343561249D04,
     +     6.5682337918207449113D-2/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA B/4.7202581904688241870D01,9.7609855173777669322D02,
     +     1.0260932208618978205D04,4.5507789335026729956D04/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C------------------------------------------------------------------
C  Coefficients for approximation in second interval
C------------------------------------------------------------------
      DATA C/3.9894151208813466764D-1,8.8831497943883759412D00,
     +     9.3506656132177855979D01,5.9727027639480026226D02,
     +     2.4945375852903726711D03,6.8481904505362823326D03,
     +     1.1602651437647350124D04,9.8427148383839780218D03,
     +     1.0765576773720192317D-8/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA D/2.2266688044328115691D01,2.3538790178262499861D02,
     +     1.5193775994075548050D03,6.4855582982667607550D03,
     +     1.8615571640885098091D04,3.4900952721145977266D04,
     +     3.8912003286093271411D04,1.9685429676859990727D04/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C------------------------------------------------------------------
C  Coefficients for approximation in third interval
C------------------------------------------------------------------
      DATA P/2.1589853405795699D-1,1.274011611602473639D-1,
     +     2.2235277870649807D-2,1.421619193227893466D-3,
     +     2.9112874951168792D-5,2.307344176494017303D-2/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q/1.28426009614491121D00,4.68238212480865118D-1,
     +     6.59881378689285515D-2,3.78239633202758244D-3,
     +     7.29751555083966205D-5/
C*PT*WARNING* Constant already double-precision
C------------------------------------------------------------------
C  Machine dependent constants
C------------------------------------------------------------------
      EPS = SPMPAR(1)*0.5D0
      MIN = SPMPAR(2)
C------------------------------------------------------------------
      X = ARG
      Y = ABS(X)
      IF (Y.LE.THRSH) THEN
C------------------------------------------------------------------
C  Evaluate  anorm  for  |X| <= 0.66291
C------------------------------------------------------------------
          XSQ = ZERO
          IF (Y.GT.EPS) XSQ = X*X
          XNUM = A(5)*XSQ
          XDEN = XSQ
          DO 10 I = 1,3
              XNUM = (XNUM+A(I))*XSQ
              XDEN = (XDEN+B(I))*XSQ
   10     CONTINUE
          RESULT = X* (XNUM+A(4))/ (XDEN+B(4))
          TEMP = RESULT
          RESULT = HALF + TEMP
          CCUM = HALF - TEMP
C------------------------------------------------------------------
C  Evaluate  anorm  for 0.66291 <= |X| <= sqrt(32)
C------------------------------------------------------------------
      ELSE IF (Y.LE.ROOT32) THEN
          XNUM = C(9)*Y
          XDEN = Y
          DO 20 I = 1,7
              XNUM = (XNUM+C(I))*Y
              XDEN = (XDEN+D(I))*Y
   20     CONTINUE
          RESULT = (XNUM+C(8))/ (XDEN+D(8))
          XSQ = AINT(Y*SIXTEN)/SIXTEN
          DEL = (Y-XSQ)* (Y+XSQ)
          RESULT = EXP(-XSQ*XSQ*HALF)*EXP(-DEL*HALF)*RESULT
          CCUM = ONE - RESULT
          IF (X.GT.ZERO) THEN
              TEMP = RESULT
              RESULT = CCUM
              CCUM = TEMP
          END IF
C------------------------------------------------------------------
C  Evaluate  anorm  for |X| > sqrt(32)
C------------------------------------------------------------------
      ELSE
          RESULT = ZERO
          XSQ = ONE/ (X*X)
          XNUM = P(6)*XSQ
          XDEN = XSQ
          DO 30 I = 1,4
              XNUM = (XNUM+P(I))*XSQ
              XDEN = (XDEN+Q(I))*XSQ
   30     CONTINUE
          RESULT = XSQ* (XNUM+P(5))/ (XDEN+Q(5))
          RESULT = (SQRPI-RESULT)/Y
          XSQ = AINT(X*SIXTEN)/SIXTEN
          DEL = (X-XSQ)* (X+XSQ)
          RESULT = EXP(-XSQ*XSQ*HALF)*EXP(-DEL*HALF)*RESULT
          CCUM = ONE - RESULT
          IF (X.GT.ZERO) THEN
              TEMP = RESULT
              RESULT = CCUM
              CCUM = TEMP
          END IF

      END IF
C*PT*WARNING* Constant already double-precision

      IF (RESULT.LT.MIN) RESULT = 0.0D0
C*PT*WARNING* Constant already double-precision
      IF (CCUM.LT.MIN) CCUM = 0.0D0
C------------------------------------------------------------------
C  Fix up for negative argument, erf, etc.
C------------------------------------------------------------------
C----------Last card of ANORM ----------
      END
      SUBROUTINE CUMPOI(S,XLAM,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE CUMPOI(S,XLAM,CUM,CCUM)
C                    CUMulative POIsson distribution
C
C
C                              Function
C
C
C     Returns the  probability  of  S   or  fewer events in  a   Poisson
C     distribution with mean XLAM.
C
C
C                              Arguments
C
C
C     S --> Upper limit of cumulation of the Poisson.
C                                                  S is DOUBLE PRECISION
C
C     XLAM --> Mean of the Poisson distribution.
C                                                  XLAM is DOUBLE PRECIS
C
C     CUM <-- Cumulative poisson distribution.
C                                        CUM is DOUBLE PRECISION
C
C     CCUM <-- Compliment of Cumulative poisson distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C
C                              Method
C
C
C     Uses formula  26.4.21   of   Abramowitz and  Stegun,  Handbook  of
C     Mathematical   Functions  to reduce   the   cumulative Poisson  to
C     the cumulative chi-square distribution.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION S,XLAM,CUM,CCUM
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION CHI,DF
C     ..
C     .. External Subroutines ..
      EXTERNAL CUMCHI
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
      DF = 2.0D0* (S+1.0D0)
C*PT*WARNING* Constant already double-precision
      CHI = 2.0D0*XLAM
      CALL CUMCHI(CHI,DF,CCUM,CUM)
      RETURN

      END
      SUBROUTINE CUMT(T,DF,CUM,CCUM)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE CUMT(T,DF,CUM,CCUM)
C                    CUMulative T-distribution
C
C
C                              Function
C
C
C     Computes the integral from -infinity to T of the t-density.
C
C
C                              Arguments
C
C
C     T --> Upper limit of integration of the t-density.
C                                                  T is DOUBLE PRECISION
C
C     DF --> Degrees of freedom of the t-distribution.
C                                                  DF is DOUBLE PRECISIO
C
C     CUM <-- Cumulative t-distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C     CCUM <-- Compliment of Cumulative t-distribution.
C                                                  CCUM is DOUBLE PRECIS
C
C
C                              Method
C
C
C     Formula 26.5.27   of     Abramowitz  and   Stegun,    Handbook  of
C     Mathematical Functions  is   used   to  reduce the  t-distribution
C     to an incomplete beta.
C
C**********************************************************************

C     .. Scalar Arguments ..
      DOUBLE PRECISION DF,T,CUM,CCUM
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION XX,A,OMA,TT,YY,DFPTT
C     ..
C     .. External Subroutines ..
      EXTERNAL CUMBET
C     ..
C     .. Executable Statements ..
      TT = T*T
      DFPTT = DF + TT
      XX = DF/DFPTT
      YY = TT/DFPTT
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      CALL CUMBET(XX,YY,0.5D0*DF,0.5D0,A,OMA)
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (T.LE.0.0D0)) GO TO 10
C*PT*WARNING* Constant already double-precision
      CUM = 0.5D0*A
      CCUM = OMA + CUM
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 CCUM = 0.5D0*A
      CUM = OMA + CCUM
   20 RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DBETRM(A,B)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DBETRM( A, B )
C          Double Precision Sterling Remainder for Complete
C                    Beta Function
C
C
C                              Function
C
C
C     Log(Beta(A,B)) = Lgamma(A) + Lgamma(B) - Lgamma(A+B)
C     where Lgamma is the log of the (complete) gamma function
C
C     Let ZZ be approximation obtained if each log gamma is approximated
C     by Sterling's formula, i.e.,
C     Sterling(Z) = LOG( SQRT( 2*PI ) ) + ( Z-0.5 ) * LOG( Z ) - Z
C
C     Returns Log(Beta(A,B)) - ZZ
C
C
C                              Arguments
C
C
C     A --> One argument of the Beta
C                    DOUBLE PRECISION A
C
C     B --> The other argument of the Beta
C                    DOUBLE PRECISION B
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DSTREM
      EXTERNAL DSTREM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MAX,MIN
C     ..
C     .. Executable Statements ..

C     Try to sum from smallest to largest
      DBETRM = -DSTREM(A+B)
      DBETRM = DBETRM + DSTREM(MAX(A,B))
      DBETRM = DBETRM + DSTREM(MIN(A,B))
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DEVLPL(A,N,X)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DEVLPL(A,N,X)
C              Double precision EVALuate a PoLynomial at X
C
C
C                              Function
C
C
C     returns
C          A(1) + A(2)*X + ... + A(N)*X**(N-1)
C
C
C                              Arguments
C
C
C     A --> Array of coefficients of the polynomial.
C                                        A is DOUBLE PRECISION(N)
C
C     N --> Length of A, also degree of polynomial - 1.
C                                        N is INTEGER
C
C     X --> Point at which the polynomial is to be evaluated.
C                                        X is DOUBLE PRECISION
C
C**********************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER N
C*PT*WARNING* Already double-precision
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION A(N)
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION TERM
      INTEGER I
C     ..
C     .. Executable Statements ..
      TERM = A(N)
      DO 10 I = N - 1,1,-1
          TERM = A(I) + TERM*X
   10 CONTINUE
      DEVLPL = TERM
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DEXPM1(X)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      DOUBLE PRECISION FUNCTION dexpm1(x)
C            Evaluation of the function EXP(X) - 1
C
C
C                              Arguments
C
C
C     X --> Argument at which exp(x)-1 desired
C                    DOUBLE PRECISION X
C
C
C                              Method
C
C
C     Renaming of function rexp from code of:
C
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,W
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,EXP
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA P1/.914041914819518D-09/,P2/.238082361044469D-01/,
     +     Q1/-.499999999085958D+00/,Q2/.107141568980644D+00/,
     +     Q3/-.119041179760821D-01/,Q4/.595130811860248D-03/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C
      IF (ABS(X).GT.0.15D0) GO TO 10
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DEXPM1 = X* (((P2*X+P1)*X+1.0D0)/
     +         ((((Q4*X+Q3)*X+Q2)*X+Q1)*X+1.0D0))
      RETURN
C
   10 W = EXP(X)
C*PT*WARNING* Constant already double-precision
      IF (X.GT.0.0D0) GO TO 20
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DEXPM1 = (W-0.5D0) - 0.5D0
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision

   20 DEXPM1 = W* (0.5D0+ (0.5D0-1.0D0/W))
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DINVNR(P,Q)
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DINVNR(P,Q)
C     Double precision NoRmal distribution INVerse
C
C
C                              Function
C
C
C     Returns X  such that CUMNOR(X)  =   P,  i.e., the  integral from -
C     infinity to X of (1/SQRT(2*PI)) EXP(-U*U/2) dU is P
C
C
C                              Arguments
C
C
C     P --> The probability whose normal deviate is sought.
C                    P is DOUBLE PRECISION
C
C     Q --> 1-P
C                    P is DOUBLE PRECISION
C
C
C                              Method
C
C
C     The  rational   function   on  page 95    of Kennedy  and  Gentle,
C     Statistical Computing, Marcel Dekker, NY , 1980 is used as a start
C     value for the Newton method of finding roots.
C
C
C                              Note
C
C
C     If P or Q .lt. machine EPS returns +/- DINVNR(EPS)
C
C**********************************************************************
C     .. Parameters ..
      INTEGER MAXIT
      PARAMETER (MAXIT=100)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION EPS
C*PT*WARNING* Constant already double-precision
      PARAMETER (EPS=1.0D-13)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION R2PI
C*PT*WARNING* Constant already double-precision
      PARAMETER (R2PI=0.3989422804014326D0)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION NHALF
C*PT*WARNING* Constant already double-precision
      PARAMETER (NHALF=-0.5D0)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION P,Q
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION STRTX,XCUR,CUM,CCUM,PP,DX
      INTEGER I
      LOGICAL QPORQ
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION STVALN
      EXTERNAL STVALN
C     ..
C     .. External Subroutines ..
      EXTERNAL CUMNOR
C*PT*WARNING* Already double-precision
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION DENNOR,X

      DENNOR(X) = R2PI*EXP(NHALF*X*X)
C     ..
C     .. Executable Statements ..
C
C     FIND MINIMUM OF P AND Q
C
      QPORQ = P .LE. Q
      IF (.NOT. (QPORQ)) GO TO 10
      PP = P
      GO TO 20

   10 PP = Q
C
C     INITIALIZATION STEP
C
   20 STRTX = STVALN(PP)
      XCUR = STRTX
C
C     NEWTON INTERATIONS
C
      DO 30 I = 1,MAXIT
          CALL CUMNOR(XCUR,CUM,CCUM)
          DX = (CUM-PP)/DENNOR(XCUR)
          XCUR = XCUR - DX
          IF (ABS(DX/XCUR).LT.EPS) GO TO 40
   30 CONTINUE
      DINVNR = STRTX
C
C     IF WE GET HERE, NEWTON HAS FAILED
C
      IF (.NOT.QPORQ) DINVNR = -DINVNR
      RETURN
C
C     IF WE GET HERE, NEWTON HAS SUCCEDED
C
   40 DINVNR = XCUR
      IF (.NOT.QPORQ) DINVNR = -DINVNR
      RETURN

      END
      SUBROUTINE DINVR(STATUS,X,FX,QLEFT,QHI)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE DINVR(STATUS, X, FX, QLEFT, QHI)
C          Double precision
C          bounds the zero of the function and invokes zror
C                    Reverse Communication
C
C
C                              Function
C
C
C     Bounds the    function  and  invokes  ZROR   to perform the   zero
C     finding.  STINVR  must  have   been  called  before this   routine
C     in order to set its parameters.
C
C
C                              Arguments
C
C
C     STATUS <--> At the beginning of a zero finding problem, STATUS
C                 should be set to 0 and INVR invoked.  (The value
C                 of parameters other than X will be ignored on this cal
C
C                 When INVR needs the function evaluated, it will set
C                 STATUS to 1 and return.  The value of the function
C                 should be set in FX and INVR again called without
C                 changing any of its other parameters.
C
C                 When INVR has finished without error, it will return
C                 with STATUS 0.  In that case X is approximately a root
C                 of F(X).
C
C                 If INVR cannot bound the function, it returns status
C                 -1 and sets QLEFT and QHI.
C                         INTEGER STATUS
C
C     X <-- The value of X at which F(X) is to be evaluated.
C                         DOUBLE PRECISION X
C
C     FX --> The value of F(X) calculated when INVR returns with
C            STATUS = 1.
C                         DOUBLE PRECISION FX
C
C     QLEFT <-- Defined only if QMFINV returns .FALSE.  In that
C          case it is .TRUE. If the stepping search terminated
C          unsucessfully at SMALL.  If it is .FALSE. the search
C          terminated unsucessfully at BIG.
C                    QLEFT is LOGICAL
C
C     QHI <-- Defined only if QMFINV returns .FALSE.  In that
C          case it is .TRUE. if F(X) .GT. Y at the termination
C          of the search and .FALSE. if F(X) .LT. Y at the
C          termination of the search.
C                    QHI is LOGICAL

C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION FX,X,ZABSST,ZABSTO,ZBIG,ZRELST,ZRELTO,ZSMALL,
     +                 ZSTPMU
      INTEGER STATUS
      LOGICAL QHI,QLEFT
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION ABSSTP,ABSTOL,BIG,FBIG,FSMALL,RELSTP,RELTOL,
     +                 SMALL,STEP,STPMUL,XHI,XLB,XLO,XSAVE,XUB,YY,ZX,ZY,
     +                 ZZ
      INTEGER I99999
      LOGICAL QBDD,QCOND,QDUM1,QDUM2,QINCR,QLIM,QOK,QUP
C     ..
C     .. External Subroutines ..
      EXTERNAL DSTZR,DZROR
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX,MIN
C     ..
C     .. Statement Functions ..
      LOGICAL QXMON
C     ..
C     .. Save statement ..
      SAVE
C     ..
C     .. Statement Function definitions ..
      QXMON(ZX,ZY,ZZ) = ZX .LE. ZY .AND. ZY .LE. ZZ
C     ..
C     .. Executable Statements ..

      IF (STATUS.GT.0) GO TO 310

      QCOND = .NOT. QXMON(SMALL,X,BIG)
      IF (QCOND) STOP ' SMALL, X, BIG not monotone in INVR'
      XSAVE = X
C
C     See that SMALL and BIG bound the zero and set QINCR
C
      X = SMALL
C     GET-FUNCTION-VALUE
C DJS ASSIGN 10 TO I99999
      I99999 = 10
      GO TO 300

   10 FSMALL = FX
      X = BIG
C     GET-FUNCTION-VALUE
C DJS ASSIGN 20 TO I99999
      I99999 = 20
      GO TO 300

   20 FBIG = FX
      QINCR = FBIG .GT. FSMALL
      IF (.NOT. (QINCR)) GO TO 50
C*PT*WARNING* Constant already double-precision
      IF (FSMALL.LE.0.0D0) GO TO 30
      STATUS = -1
      QLEFT = .TRUE.
      QHI = .TRUE.
      RETURN
C*PT*WARNING* Constant already double-precision

   30 IF (FBIG.GE.0.0D0) GO TO 40
      STATUS = -1
      QLEFT = .FALSE.
      QHI = .FALSE.
      RETURN

   40 GO TO 80
C*PT*WARNING* Constant already double-precision

   50 IF (FSMALL.GE.0.0D0) GO TO 60
      STATUS = -1
      QLEFT = .TRUE.
      QHI = .FALSE.
      RETURN
C*PT*WARNING* Constant already double-precision

   60 IF (FBIG.LE.0.0D0) GO TO 70
      STATUS = -1
      QLEFT = .FALSE.
      QHI = .TRUE.
      RETURN

   70 CONTINUE
   80 X = XSAVE
      STEP = MAX(ABSSTP,RELSTP*ABS(X))
C      YY = F(X) - Y
C     GET-FUNCTION-VALUE
C DJS ASSIGN 90 TO I99999
      I99999 = 90
      GO TO 300

   90 YY = FX
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (YY.EQ.0.0D0)) GO TO 100
      STATUS = 0
      QOK = .TRUE.
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision

  100 QUP = (QINCR .AND. (YY.LT.0.0D0)) .OR.
     +      (.NOT.QINCR .AND. (YY.GT.0.0D0))
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     HANDLE CASE IN WHICH WE MUST STEP HIGHER
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF (.NOT. (QUP)) GO TO 170
      XLB = XSAVE
      XUB = MIN(XLB+STEP,BIG)
      GO TO 120

  110 IF (QCOND) GO TO 150
C      YY = F(XUB) - Y
  120 X = XUB
C     GET-FUNCTION-VALUE
C DJS ASSIGN 130 TO I99999
      I99999 = 130
      GO TO 300

  130 YY = FX
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      QBDD = (QINCR .AND. (YY.GE.0.0D0)) .OR.
     +       (.NOT.QINCR .AND. (YY.LE.0.0D0))
      QLIM = XUB .GE. BIG
      QCOND = QBDD .OR. QLIM
      IF (QCOND) GO TO 140
      STEP = STPMUL*STEP
      XLB = XUB
      XUB = MIN(XLB+STEP,BIG)
  140 GO TO 110

  150 IF (.NOT. (QLIM.AND..NOT.QBDD)) GO TO 160
      STATUS = -1
      QLEFT = .FALSE.
      QHI = .NOT. QINCR
      X = BIG
      RETURN

  160 GO TO 240
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     HANDLE CASE IN WHICH WE MUST STEP LOWER
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  170 XUB = XSAVE
      XLB = MAX(XUB-STEP,SMALL)
      GO TO 190

  180 IF (QCOND) GO TO 220
C      YY = F(XLB) - Y
  190 X = XLB
C     GET-FUNCTION-VALUE
C DJS ASSIGN 200 TO I99999
      I99999 = 200
      GO TO 300

  200 YY = FX
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      QBDD = (QINCR .AND. (YY.LE.0.0D0)) .OR.
     +       (.NOT.QINCR .AND. (YY.GE.0.0D0))
      QLIM = XLB .LE. SMALL
      QCOND = QBDD .OR. QLIM
      IF (QCOND) GO TO 210
      STEP = STPMUL*STEP
      XUB = XLB
      XLB = MAX(XUB-STEP,SMALL)
  210 GO TO 180

  220 IF (.NOT. (QLIM.AND..NOT.QBDD)) GO TO 230
      STATUS = -1
      QLEFT = .TRUE.
      QHI = QINCR
      X = SMALL
      RETURN

  230 CONTINUE
  240 CALL DSTZR(XLB,XUB,ABSTOL,RELTOL)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     IF WE REACH HERE, XLB AND XUB BOUND THE ZERO OF F.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      STATUS = 0
      GO TO 260

  250 IF (.NOT. (STATUS.EQ.1)) GO TO 290
  260 CALL DZROR(STATUS,X,FX,XLO,XHI,QDUM1,QDUM2)
      IF (.NOT. (STATUS.EQ.1)) GO TO 280
C     GET-FUNCTION-VALUE
C DJS ASSIGN 270 TO I99999
      I99999 = 270
      GO TO 300

  270 CONTINUE
  280 GO TO 250

  290 X = XLO
      STATUS = 0
      RETURN

      ENTRY DSTINV(ZSMALL,ZBIG,ZABSST,ZRELST,ZSTPMU,ZABSTO,ZRELTO)
C**********************************************************************
C
C      SUBROUTINE DSTINV( SMALL, BIG, ABSSTP, RELSTP, STPMUL,
C     +                   ABSTOL, RELTOL )
C      Double Precision - SeT INverse finder - Reverse Communication
C
C
C                              Function
C
C
C     Concise Description - Given a monotone function F finds X
C     such that F(X) = Y.  Uses Reverse communication -- see invr.
C     This routine sets quantities needed by INVR.
C
C          More Precise Description of INVR -
C
C     F must be a monotone function, the results of QMFINV are
C     otherwise undefined.  QINCR must be .TRUE. if F is non-
C     decreasing and .FALSE. if F is non-increasing.
C
C     QMFINV will return .TRUE. if and only if F(SMALL) and
C     F(BIG) bracket Y, i. e.,
C          QINCR is .TRUE. and F(SMALL).LE.Y.LE.F(BIG) or
C          QINCR is .FALSE. and F(BIG).LE.Y.LE.F(SMALL)
C
C     if QMFINV returns .TRUE., then the X returned satisfies
C     the following condition.  let
C               TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
C     then if QINCR is .TRUE.,
C          F(X-TOL(X)) .LE. Y .LE. F(X+TOL(X))
C     and if QINCR is .FALSE.
C          F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X))
C
C
C                              Arguments
C
C
C     SMALL --> The left endpoint of the interval to be
C          searched for a solution.
C                    SMALL is DOUBLE PRECISION
C
C     BIG --> The right endpoint of the interval to be
C          searched for a solution.
C                    BIG is DOUBLE PRECISION
C
C     ABSSTP, RELSTP --> The initial step size in the search
C          is MAX(ABSSTP,RELSTP*ABS(X)). See algorithm.
C                    ABSSTP is DOUBLE PRECISION
C                    RELSTP is DOUBLE PRECISION
C
C     STPMUL --> When a step doesn't bound the zero, the step
C                size is multiplied by STPMUL and another step
C                taken.  A popular value is 2.0
C                    DOUBLE PRECISION STPMUL
C
C     ABSTOL, RELTOL --> Two numbers that determine the accuracy
C          of the solution.  See function for a precise definition.
C                    ABSTOL is DOUBLE PRECISION
C                    RELTOL is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Compares F(X) with Y for the input value of X then uses QINCR
C     to determine whether to step left or right to bound the
C     desired x.  the initial step size is
C          MAX(ABSSTP,RELSTP*ABS(S)) for the input value of X.
C     Iteratively steps right or left until it bounds X.
C     At each step which doesn't bound X, the step size is doubled.
C     The routine is careful never to step beyond SMALL or BIG.  If
C     it hasn't bounded X at SMALL or BIG, QMFINV returns .FALSE.
C     after setting QLEFT and QHI.
C
C     If X is successfully bounded then Algorithm R of the paper
C     'Two Efficient Algorithms with Guaranteed Convergence for
C     Finding a Zero of a Function' by J. C. P. Bus and
C     T. J. Dekker in ACM Transactions on Mathematical
C     Software, Volume 1, No. 4 page 330 (DEC. '75) is employed
C     to find the zero of the function F(X)-Y. This is routine
C     QRZERO.
C
C**********************************************************************
      SMALL = ZSMALL
      BIG = ZBIG
      ABSSTP = ZABSST
      RELSTP = ZRELST
      STPMUL = ZSTPMU
      ABSTOL = ZABSTO
      RELTOL = ZRELTO
      RETURN

      STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***'
C     TO GET-FUNCTION-VALUE
  300 STATUS = 1
      RETURN

  310 CONTINUE
C DJS GO TO I99999
      IF (I99999 .EQ.  10) GO TO 10 
      IF (I99999 .EQ.  20) GO TO 20 
      IF (I99999 .EQ.  90) GO TO 90 
      IF (I99999 .EQ. 130) GO TO 130 
      IF (I99999 .EQ. 200) GO TO 200 
      IF (I99999 .EQ. 270) GO TO 270 

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DLANOR(X)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      DOUBLE PRECISION FUNCTION DLANOR( X )
C           Double precision Logarith of the Asymptotic Normal
C
C
C                              Function
C
C
C      Computes the logarithm of the cumulative normal distribution
C      from abs( x ) to infinity for abs( x ) >= 5.
C
C
C                              Arguments
C
C
C      X --> Value at which cumulative normal to be evaluated
C                     DOUBLE PRECISION X
C
C
C                              Method
C
C
C      23 term expansion of formula 26.2.12 of Abramowitz and Stegun.
C      The relative error at X = 5 is about 0.5E-5.
C
C
C                              Note
C
C
C      ABS(X) must be >= 5 else there is an error stop.
C
C**********************************************************************
C     .. Parameters ..
      DOUBLE PRECISION DLSQPI
C*PT*WARNING* Constant already double-precision
      PARAMETER (DLSQPI=0.91893853320467274177D0)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION APPROX,CORREC,XX,XX2
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION COEF(12)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DEVLPL,DLN1PX
      EXTERNAL DEVLPL,DLN1PX
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,LOG
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA COEF/-1.0D0,3.0D0,-15.0D0,105.0D0,-945.0D0,10395.0D0,
     +     -135135.0D0,2027025.0D0,-34459425.0D0,654729075.0D0,
     +     -13749310575D0,316234143225.0D0/
C     ..
C     .. Executable Statements ..

      XX = ABS(X)
C*PT*WARNING* Constant already double-precision
      IF (XX.LT.5.0D0) STOP ' Argument too small in DLANOR'
C*PT*WARNING* Constant already double-precision

      APPROX = -DLSQPI - 0.5D0*XX*XX - LOG(XX)

      XX2 = XX*XX
C*PT*WARNING* Constant already double-precision
      CORREC = DEVLPL(COEF,12,1.0D0/XX2)/XX2
      CORREC = DLN1PX(CORREC)

      DLANOR = APPROX + CORREC

      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DLN1MX(X)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DLN1MX(X)
C               Double precision LN(1-X)
C
C
C                              Function
C
C
C     Returns ln(1-x) for small x (good accuracy if x .le. 0.1).
C     Note that the obvious code of
C               LOG(1.0-X)
C     won't work for small X because 1.0-X loses accuracy
C
C
C                              Arguments
C
C
C     X --> Value for which ln(1-x) is desired.
C                                        X is DOUBLE PRECISION
C
C
C                              Method
C
C
C     If X > 0.1, the obvious code above is used ELSE
C     The Taylor series for 1-x is expanded to 20 terms.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DLN1PX
      EXTERNAL DLN1PX
C     ..
C     .. Executable Statements ..

      DLN1MX = DLN1PX(-X)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DLN1PX(A)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DLN1PX(X)
C               Double precision LN(1+X)
C
C
C                              Function
C
C
C     Returns ln(1+x)
C     Note that the obvious code of
C               LOG(1.0+X)
C     won't work for small X because 1.0+X loses accuracy
C
C
C                              Arguments
C
C
C     X --> Value for which ln(1-x) is desired.
C                                        X is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Renames ALNREL from:
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C**********************************************************************
C-----------------------------------------------------------------------
C            EVALUATION OF THE FUNCTION LN(1 + A)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION P1,P2,P3,Q1,Q2,Q3,T,T2,W,X
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DLOG
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA P1/-.129418923021993D+01/,P2/.405303492862024D+00/,
     +     P3/-.178874546012214D-01/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q1/-.162752256355323D+01/,Q2/.747811014037616D+00/,
     +     Q3/-.845104217945565D-01/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C--------------------------
      IF (ABS(A).GT.0.375D0) GO TO 10
C*PT*WARNING* Constant already double-precision
      T = A/ (A+2.0D0)
      T2 = T*T
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W = (((P3*T2+P2)*T2+P1)*T2+1.0D0)/ (((Q3*T2+Q2)*T2+Q1)*T2+1.0D0)
C*PT*WARNING* Constant already double-precision
      DLN1PX = 2.0D0*T*W
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Already double-precision (DBLE)
C
   10 X = 1.D0 + DBLE(A)
      DLN1PX = DLOG(X)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DLNBET(A0,B0)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C      DOUBLE PRECISION FUNCTION DLNBET( A, B )
C          Double precision LN of the complete BETa
C
C
C                              Function
C
C
C     Returns the natural log of the complete beta function,
C     i.e.,
C
C                  ln( Gamma(a)*Gamma(b) / Gamma(a+b)
C
C
C                              Arguments
C
C
C   A,B --> The (symmetric) arguments to the complete beta
C                  DOUBLE PRECISION A, B
C
C
C                              Method
C
C
C     Renames BETALN from:
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C**********************************************************************
C-----------------------------------------------------------------------
C     EVALUATION OF THE LOGARITHM OF THE BETA FUNCTION
C-----------------------------------------------------------------------
C     E = 0.5*LN(2*PI)
C--------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A0,B0
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,C,E,H,U,V,W,Z
      INTEGER I,N
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALGDIV,ALNREL,BCORR,GAMLN,GSUMLN
      EXTERNAL ALGDIV,ALNREL,BCORR,GAMLN,GSUMLN
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DLOG,DMAX1,DMIN1
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA E/.918938533204673D0/
C     ..
C     .. Executable Statements ..
C--------------------------
      A = DMIN1(A0,B0)
      B = DMAX1(A0,B0)
C*PT*WARNING* Constant already double-precision
      IF (A.GE.8.0D0) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (A.GE.1.0D0) GO TO 20
C*PT*WARNING* Constant already double-precision
C-----------------------------------------------------------------------
C                   PROCEDURE WHEN A .LT. 1
C-----------------------------------------------------------------------
      IF (B.GE.8.0D0) GO TO 10
      DLNBET = GAMLN(A) + (GAMLN(B)-GAMLN(A+B))
      RETURN

   10 DLNBET = GAMLN(A) + ALGDIV(A,B)
      RETURN
C*PT*WARNING* Constant already double-precision
C-----------------------------------------------------------------------
C                PROCEDURE WHEN 1 .LE. A .LT. 8
C-----------------------------------------------------------------------
   20 IF (A.GT.2.0D0) GO TO 40
C*PT*WARNING* Constant already double-precision
      IF (B.GT.2.0D0) GO TO 30
      DLNBET = GAMLN(A) + GAMLN(B) - GSUMLN(A,B)
      RETURN
C*PT*WARNING* Constant already double-precision

   30 W = 0.0D0
C*PT*WARNING* Constant already double-precision
      IF (B.LT.8.0D0) GO TO 60
      DLNBET = GAMLN(A) + ALGDIV(A,B)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C                REDUCTION OF A WHEN B .LE. 1000
C
   40 IF (B.GT.1000.0D0) GO TO 80
C*PT*WARNING* Constant already double-precision
      N = A - 1.0D0
C*PT*WARNING* Constant already double-precision
      W = 1.0D0
      DO 50 I = 1,N
C*PT*WARNING* Constant already double-precision
          A = A - 1.0D0
          H = A/B
C*PT*WARNING* Constant already double-precision
          W = W* (H/ (1.0D0+H))
   50 CONTINUE
      W = DLOG(W)
C*PT*WARNING* Constant already double-precision
      IF (B.LT.8.0D0) GO TO 60
      DLNBET = W + GAMLN(A) + ALGDIV(A,B)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C                 REDUCTION OF B WHEN B .LT. 8
C
   60 N = B - 1.0D0
C*PT*WARNING* Constant already double-precision
      Z = 1.0D0
      DO 70 I = 1,N
C*PT*WARNING* Constant already double-precision
          B = B - 1.0D0
          Z = Z* (B/ (A+B))
   70 CONTINUE
      DLNBET = W + DLOG(Z) + (GAMLN(A)+ (GAMLN(B)-GSUMLN(A,B)))
      RETURN
C*PT*WARNING* Constant already double-precision
C
C                REDUCTION OF A WHEN B .GT. 1000
C
   80 N = A - 1.0D0
C*PT*WARNING* Constant already double-precision
      W = 1.0D0
      DO 90 I = 1,N
C*PT*WARNING* Constant already double-precision
          A = A - 1.0D0
C*PT*WARNING* Constant already double-precision
          W = W* (A/ (1.0D0+A/B))
   90 CONTINUE
      DLNBET = (DLOG(W)-N*DLOG(B)) + (GAMLN(A)+ALGDIV(A,B))
      RETURN
C-----------------------------------------------------------------------
C                   PROCEDURE WHEN A .GE. 8
C-----------------------------------------------------------------------
  100 W = BCORR(A,B)
      H = A/B
C*PT*WARNING* Constant already double-precision
      C = H/ (1.0D0+H)
C*PT*WARNING* Constant already double-precision
      U = - (A-0.5D0)*DLOG(C)
      V = B*ALNREL(H)
      IF (U.LE.V) GO TO 110
C*PT*WARNING* Constant already double-precision
      DLNBET = (((-0.5D0*DLOG(B)+E)+W)-V) - U
      RETURN
C*PT*WARNING* Constant already double-precision

  110 DLNBET = (((-0.5D0*DLOG(B)+E)+W)-U) - V
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DLNGAM(A)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DLNGAM(X)
C                 Double precision LN of the GAMma function
C
C
C                              Function
C
C
C     Returns the natural logarithm of GAMMA(X).
C
C
C                              Arguments
C
C
C     X --> value at which scaled log gamma is to be returned
C                    X is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Renames GAMLN from:
C     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant
C     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM
C     Trans. Math.  Softw. 18 (1993), 360-373.
C
C**********************************************************************
C-----------------------------------------------------------------------
C            EVALUATION OF LN(GAMMA(A)) FOR POSITIVE A
C-----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS
C          NAVAL SURFACE WARFARE CENTER
C          DAHLGREN, VIRGINIA
C--------------------------
C     D = 0.5*(LN(2*PI) - 1)
C--------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION C0,C1,C2,C3,C4,C5,D,T,W
      INTEGER I,N
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION GAMLN1
      EXTERNAL GAMLN1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DLOG
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C--------------------------
      DATA D/.418938533204673D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA C0/.833333333333333D-01/,C1/-.277777777760991D-02/,
     +     C2/.793650666825390D-03/,C3/-.595202931351870D-03/,
     +     C4/.837308034031215D-03/,C5/-.165322962780713D-02/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C-----------------------------------------------------------------------
      IF (A.GT.0.8D0) GO TO 10
      DLNGAM = GAMLN1(A) - DLOG(A)
      RETURN
C*PT*WARNING* Constant already double-precision

   10 IF (A.GT.2.25D0) GO TO 20
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      T = (A-0.5D0) - 0.5D0
      DLNGAM = GAMLN1(T)
      RETURN
C*PT*WARNING* Constant already double-precision
C
   20 IF (A.GE.10.0D0) GO TO 40
C*PT*WARNING* Constant already double-precision
      N = A - 1.25D0
      T = A
C*PT*WARNING* Constant already double-precision
      W = 1.0D0
      DO 30 I = 1,N
C*PT*WARNING* Constant already double-precision
          T = T - 1.0D0
          W = T*W
   30 CONTINUE
C*PT*WARNING* Constant already double-precision
      DLNGAM = GAMLN1(T-1.0D0) + DLOG(W)
      RETURN
C*PT*WARNING* Constant already double-precision
C
   40 T = (1.0D0/A)**2
      W = (((((C5*T+C4)*T+C3)*T+C2)*T+C1)*T+C0)/A
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DLNGAM = (D+W) + (A-0.5D0)* (DLOG(A)-1.0D0)
      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DSTREM(Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-P,R-Z),INTEGER (I-N),LOGICAL (Q)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DSTREM( Z )
C             Double precision Sterling Remainder
C
C
C                              Function
C
C
C     Returns   Log(Gamma(Z))  -  Sterling(Z)  where   Sterling(Z)  is
C     Sterling's Approximation to Log(Gamma(Z))
C
C     Sterling(Z) = LOG( SQRT( 2*PI ) ) + ( Z-0.5 ) * LOG( Z ) - Z
C
C
C                              Arguments
C
C
C     Z --> Value at which Sterling remainder calculated
C           Must be positive.
C                  DOUBLE PRECISION Z
C
C
C                              Method
C
C
C
C     If Z >= 6 uses 9 terms of series in Bernoulli numbers
C     (Values calculated using Maple)
C     Otherwise computes difference explicitly
C
C**********************************************************************

C     .. Parameters ..
      DOUBLE PRECISION HLN2PI
C*PT*WARNING* Constant already double-precision
      PARAMETER (HLN2PI=0.91893853320467274178D0)
      INTEGER NCOEF
      PARAMETER (NCOEF=10)
C*PT*WARNING* Already double-precision
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION Z
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION STERL
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION COEF(NCOEF)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DEVLPL,DLNGAM
      EXTERNAL DEVLPL,DLNGAM
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC LOG
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA COEF/0.0D0,0.0833333333333333333333333333333D0,
     +     -0.00277777777777777777777777777778D0,
     +     0.000793650793650793650793650793651D0,
     +     -0.000595238095238095238095238095238D0,
     +     0.000841750841750841750841750841751D0,
     +     -0.00191752691752691752691752691753D0,
     +     0.00641025641025641025641025641026D0,
     +     -0.0295506535947712418300653594771D0,
     +     0.179644372368830573164938490016D0/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..

C    For information, here are the next 11 coefficients of the
C    remainder term in Sterling's formula
C            -1.39243221690590111642743221691
C            13.4028640441683919944789510007
C            -156.848284626002017306365132452
C            2193.10333333333333333333333333
C            -36108.7712537249893571732652192
C            691472.268851313067108395250776
C            -0.152382215394074161922833649589D8
C            0.382900751391414141414141414141D9
C            -0.108822660357843910890151491655D11
C            0.347320283765002252252252252252D12
C            -0.123696021422692744542517103493D14
C

      IF (Z.LE.0.0D0) STOP 'Zero or negative argument in DSTREM'
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (Z.GT.6.0D0)) GO TO 10
C*PT*WARNING* Constant already double-precision
      DSTREM = DEVLPL(COEF,10,1.0D0/Z**2)*Z
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 STERL = HLN2PI + (Z-0.5D0)*LOG(Z) - Z
      DSTREM = DLNGAM(Z) - STERL
   20 RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION DT1(P,Q,DF)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DT1(P,Q,DF)
C     Double precision Initalize Approximation to
C           INVerse of the cumulative T distribution
C
C
C                              Function
C
C
C     Returns  the  inverse   of  the T   distribution   function, i.e.,
C     the integral from 0 to INVT of the T density is P. This is an
C     initial approximation
C
C
C                              Arguments
C
C
C     P --> The p-value whose inverse from the T distribution is
C          desired.
C                    P is DOUBLE PRECISION
C
C     Q --> 1-P.
C                    Q is DOUBLE PRECISION
C
C     DF --> Degrees of freedom of the T distribution.
C                    DF is DOUBLE PRECISION
C
C**********************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION DF,P,Q
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION DENPOW,SUM,TERM,X,XP,XX
      INTEGER I
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION COEF(5,4),DENOM(4)
      INTEGER IDEG(4)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DINVNR,DEVLPL
      EXTERNAL DINVNR,DEVLPL
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA (COEF(I,1),I=1,5)/1.0D0,1.0D0,3*0.0D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA (COEF(I,2),I=1,5)/3.0D0,16.0D0,5.0D0,2*0.0D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA (COEF(I,3),I=1,5)/-15.0D0,17.0D0,19.0D0,3.0D0,0.0D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA (COEF(I,4),I=1,5)/-945.0D0,-1920.0D0,1482.0D0,776.0D0,79.0D0/
      DATA IDEG/2,3,4,5/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA DENOM/4.0D0,96.0D0,384.0D0,92160.0D0/
C     ..
C     .. Executable Statements ..
      X = ABS(DINVNR(P,Q))
      XX = X*X
      SUM = X
C*PT*WARNING* Constant already double-precision
      DENPOW = 1.0D0
      DO 10 I = 1,4
          TERM = DEVLPL(COEF(1,I),IDEG(I),XX)*X
          DENPOW = DENPOW*DF
          SUM = SUM + TERM/ (DENPOW*DENOM(I))
   10 CONTINUE
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (P.GE.0.5D0)) GO TO 20
      XP = SUM
      GO TO 30

   20 XP = -SUM
   30 DT1 = XP
      RETURN

      END
      SUBROUTINE DZROR(STATUS,X,FX,XLO,XHI,QLEFT,QHI)
C*PT*WARNING* Already double-precision
C**********************************************************************
C
C     SUBROUTINE DZROR(STATUS, X, FX, XLO, XHI, QLEFT, QHI)
C     Double precision ZeRo of a function -- Reverse Communication
C
C
C                              Function
C
C
C     Performs the zero finding.  STZROR must have been called before
C     this routine in order to set its parameters.
C
C
C                              Arguments
C
C
C     STATUS <--> At the beginning of a zero finding problem, STATUS
C                 should be set to 0 and ZROR invoked.  (The value
C                 of other parameters will be ignored on this call.)
C
C                 When ZROR needs the function evaluated, it will set
C                 STATUS to 1 and return.  The value of the function
C                 should be set in FX and ZROR again called without
C                 changing any of its other parameters.
C
C                 When ZROR has finished without error, it will return
C                 with STATUS 0.  In that case (XLO,XHI) bound the answe
C
C                 If ZROR finds an error (which implies that F(XLO)-Y an
C                 F(XHI)-Y have the same sign, it returns STATUS -1.  In
C                 this case, XLO and XHI are undefined.
C                         INTEGER STATUS
C
C     X <-- The value of X at which F(X) is to be evaluated.
C                         DOUBLE PRECISION X
C
C     FX --> The value of F(X) calculated when ZROR returns with
C            STATUS = 1.
C                         DOUBLE PRECISION FX
C
C     XLO <-- When ZROR returns with STATUS = 0, XLO bounds the
C             inverval in X containing the solution below.
C                         DOUBLE PRECISION XLO
C
C     XHI <-- When ZROR returns with STATUS = 0, XHI bounds the
C             inverval in X containing the solution above.
C                         DOUBLE PRECISION XHI
C
C     QLEFT <-- .TRUE. if the stepping search terminated unsucessfully
C                at XLO.  If it is .FALSE. the search terminated
C                unsucessfully at XHI.
C                    QLEFT is LOGICAL
C
C     QHI <-- .TRUE. if F(X) .GT. Y at the termination of the
C              search and .FALSE. if F(X) .LT. Y at the
C              termination of the search.
C                    QHI is LOGICAL
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION FX,X,XHI,XLO,ZABSTL,ZRELTL,ZXHI,ZXLO
      INTEGER STATUS
      LOGICAL QHI,QLEFT
C     ..
C     .. Save statement ..
      SAVE
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,ABSTOL,B,C,D,FA,FB,FC,FD,FDA,FDB,M,MB,P,Q,
     +                 RELTOL,TOL,W,XXHI,XXLO,ZX
      INTEGER EXT,I99999
      LOGICAL FIRST,QRZERO
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,MAX,SIGN
C*PT*WARNING* Already double-precision
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION FTOL
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Statement Function definitions ..
      FTOL(ZX) = 0.5D0*MAX(ABSTOL,RELTOL*ABS(ZX))
C     ..
C     .. Executable Statements ..

      IF (STATUS.GT.0) GO TO 280
      XLO = XXLO
      XHI = XXHI
      B = XLO
      X = XLO
C     GET-FUNCTION-VALUE
C DJS ASSIGN 10 TO I99999
      I99999 = 10
      GO TO 270

   10 FB = FX
      XLO = XHI
      A = XLO
      X = XLO
C     GET-FUNCTION-VALUE
C DJS ASSIGN 20 TO I99999
      I99999 = 20
      GO TO 270
C*PT*WARNING* Constant already double-precision
C
C     Check that F(ZXLO) < 0 < F(ZXHI)  or
C                F(ZXLO) > 0 > F(ZXHI)
C
   20 IF (.NOT. (FB.LT.0.0D0)) GO TO 40
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (FX.LT.0.0D0)) GO TO 30
      STATUS = -1
      QLEFT = FX .LT. FB
      QHI = .FALSE.
      RETURN

   30 CONTINUE
C*PT*WARNING* Constant already double-precision
   40 IF (.NOT. (FB.GT.0.0D0)) GO TO 60
C*PT*WARNING* Constant already double-precision
      IF (.NOT. (FX.GT.0.0D0)) GO TO 50
      STATUS = -1
      QLEFT = FX .GT. FB
      QHI = .TRUE.
      RETURN

   50 CONTINUE
   60 FA = FX
C
      FIRST = .TRUE.
   70 C = A
      FC = FA
      EXT = 0
   80 IF (.NOT. (ABS(FC).LT.ABS(FB))) GO TO 100
      IF (.NOT. (C.NE.A)) GO TO 90
      D = A
      FD = FA
   90 A = B
      FA = FB
      XLO = C
      B = XLO
      FB = FC
      C = A
      FC = FA
  100 TOL = FTOL(XLO)
C*PT*WARNING* Constant already double-precision
      M = (C+B)*.5D0
      MB = M - B
      IF (.NOT. (ABS(MB).GT.TOL)) GO TO 240
      IF (.NOT. (EXT.GT.3)) GO TO 110
      W = MB
      GO TO 190

  110 TOL = SIGN(TOL,MB)
      P = (B-A)*FB
      IF (.NOT. (FIRST)) GO TO 120
      Q = FA - FB
      FIRST = .FALSE.
      GO TO 130

  120 FDB = (FD-FB)/ (D-B)
      FDA = (FD-FA)/ (D-A)
      P = FDA*P
      Q = FDB*FA - FDA*FB
C*PT*WARNING* Constant already double-precision
  130 IF (.NOT. (P.LT.0.0D0)) GO TO 140
      P = -P
      Q = -Q
C*PT*WARNING* Constant already double-precision
  140 IF (EXT.EQ.3) P = P*2.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (.NOT. ((P*1.0D0).EQ.0.0D0.OR.P.LE. (Q*TOL))) GO TO 150
      W = TOL
      GO TO 180

  150 IF (.NOT. (P.LT. (MB*Q))) GO TO 160
      W = P/Q
      GO TO 170

  160 W = MB
  170 CONTINUE
  180 CONTINUE
  190 D = A
      FD = FA
      A = B
      FA = FB
      B = B + W
      XLO = B
      X = XLO
C     GET-FUNCTION-VALUE
C DJS ASSIGN 200 TO I99999
      I99999 = 200
      GO TO 270

  200 FB = FX
C*PT*WARNING* Constant already double-precision
      IF (.NOT. ((FC*FB).GE.0.0D0)) GO TO 210
      GO TO 70

  210 IF (.NOT. (W.EQ.MB)) GO TO 220
      EXT = 0
      GO TO 230

  220 EXT = EXT + 1
  230 GO TO 80

  240 XHI = C
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      QRZERO = (FC.GE.0.0D0 .AND. FB.LE.0.0D0) .OR.
     +         (FC.LT.0.0D0 .AND. FB.GE.0.0D0)
      IF (.NOT. (QRZERO)) GO TO 250
      STATUS = 0
      GO TO 260

  250 STATUS = -1
  260 RETURN

      ENTRY DSTZR(ZXLO,ZXHI,ZABSTL,ZRELTL)
C**********************************************************************
C
C     SUBROUTINE DSTZR( XLO, XHI, ABSTOL, RELTOL )
C     Double precision SeT ZeRo finder - Reverse communication version
C
C
C                              Function
C
C
C
C     Sets quantities needed by ZROR.  The function of ZROR
C     and the quantities set is given here.
C
C     Concise Description - Given a function F
C     find XLO such that F(XLO) = 0.
C
C          More Precise Description -
C
C     Input condition. F is a double precision function of a single
C     double precision argument and XLO and XHI are such that
C          F(XLO)*F(XHI)  .LE.  0.0
C
C     If the input condition is met, QRZERO returns .TRUE.
C     and output values of XLO and XHI satisfy the following
C          F(XLO)*F(XHI)  .LE. 0.
C          ABS(F(XLO)  .LE. ABS(F(XHI)
C          ABS(XLO-XHI)  .LE. TOL(X)
C     where
C          TOL(X) = MAX(ABSTOL,RELTOL*ABS(X))
C
C     If this algorithm does not find XLO and XHI satisfying
C     these conditions then QRZERO returns .FALSE.  This
C     implies that the input condition was not met.
C
C
C                              Arguments
C
C
C     XLO --> The left endpoint of the interval to be
C           searched for a solution.
C                    XLO is DOUBLE PRECISION
C
C     XHI --> The right endpoint of the interval to be
C           for a solution.
C                    XHI is DOUBLE PRECISION
C
C     ABSTOL, RELTOL --> Two numbers that determine the accuracy
C                      of the solution.  See function for a
C                      precise definition.
C                    ABSTOL is DOUBLE PRECISION
C                    RELTOL is DOUBLE PRECISION
C
C
C                              Method
C
C
C     Algorithm R of the paper 'Two Efficient Algorithms with
C     Guaranteed Convergence for Finding a Zero of a Function'
C     by J. C. P. Bus and T. J. Dekker in ACM Transactions on
C     Mathematical Software, Volume 1, no. 4 page 330
C     (Dec. '75) is employed to find the zero of F(X)-Y.
C
C**********************************************************************
      XXLO = ZXLO
      XXHI = ZXHI
      ABSTOL = ZABSTL
      RELTOL = ZRELTL
      RETURN

      STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***'
C     TO GET-FUNCTION-VALUE
  270 STATUS = 1
      RETURN

  280 CONTINUE
C DJS GO TO I99999
      IF (I99999 .EQ.  10) GO TO 10 
      IF (I99999 .EQ.  20) GO TO 20 
      IF (I99999 .EQ. 200) GO TO 200 

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION ERF(X)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C             EVALUATION OF THE REAL ERROR FUNCTION
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AX,BOT,C,T,TOP,X2
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION A(5),B(3),P(8),Q(8),R(5),S(4)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,EXP,SIGN
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C-------------------------
C-------------------------
C-------------------------
C-------------------------
      DATA C/.564189583547756D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA A(1)/.771058495001320D-04/,A(2)/-.133733772997339D-02/,
     +     A(3)/.323076579225834D-01/,A(4)/.479137145607681D-01/,
     +     A(5)/.128379167095513D+00/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA B(1)/.301048631703895D-02/,B(2)/.538971687740286D-01/,
     +     B(3)/.375795757275549D+00/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA P(1)/-1.36864857382717D-07/,P(2)/5.64195517478974D-01/,
     +     P(3)/7.21175825088309D+00/,P(4)/4.31622272220567D+01/,
     +     P(5)/1.52989285046940D+02/,P(6)/3.39320816734344D+02/,
     +     P(7)/4.51918953711873D+02/,P(8)/3.00459261020162D+02/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q(1)/1.00000000000000D+00/,Q(2)/1.27827273196294D+01/,
     +     Q(3)/7.70001529352295D+01/,Q(4)/2.77585444743988D+02/,
     +     Q(5)/6.38980264465631D+02/,Q(6)/9.31354094850610D+02/,
     +     Q(7)/7.90950925327898D+02/,Q(8)/3.00459260956983D+02/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA R(1)/2.10144126479064D+00/,R(2)/2.62370141675169D+01/,
     +     R(3)/2.13688200555087D+01/,R(4)/4.65807828718470D+00/,
     +     R(5)/2.82094791773523D-01/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA S(1)/9.41537750555460D+01/,S(2)/1.87114811799590D+02/,
     +     S(3)/9.90191814623914D+01/,S(4)/1.80124575948747D+01/
C     ..
C     .. Executable Statements ..
C-------------------------
      AX = ABS(X)
C*PT*WARNING* Constant already double-precision
      IF (AX.GT.0.5D0) GO TO 10
      T = X*X
C*PT*WARNING* Constant already double-precision
      TOP = ((((A(1)*T+A(2))*T+A(3))*T+A(4))*T+A(5)) + 1.0D0
C*PT*WARNING* Constant already double-precision
      BOT = ((B(1)*T+B(2))*T+B(3))*T + 1.0D0
      ERF = X* (TOP/BOT)
      RETURN
C*PT*WARNING* Constant already double-precision
C
   10 IF (AX.GT.4.0D0) GO TO 20
      TOP = ((((((P(1)*AX+P(2))*AX+P(3))*AX+P(4))*AX+P(5))*AX+P(6))*AX+
     +      P(7))*AX + P(8)
      BOT = ((((((Q(1)*AX+Q(2))*AX+Q(3))*AX+Q(4))*AX+Q(5))*AX+Q(6))*AX+
     +      Q(7))*AX + Q(8)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ERF = 0.5D0 + (0.5D0-EXP(-X*X)*TOP/BOT)
C*PT*WARNING* Constant already double-precision
      IF (X.LT.0.0D0) ERF = -ERF
      RETURN
C*PT*WARNING* Constant already double-precision
C
   20 IF (AX.GE.5.8D0) GO TO 30
      X2 = X*X
C*PT*WARNING* Constant already double-precision
      T = 1.0D0/X2
      TOP = (((R(1)*T+R(2))*T+R(3))*T+R(4))*T + R(5)
C*PT*WARNING* Constant already double-precision
      BOT = (((S(1)*T+S(2))*T+S(3))*T+S(4))*T + 1.0D0
      ERF = (C-TOP/ (X2*BOT))/AX
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ERF = 0.5D0 + (0.5D0-EXP(-X2)*ERF)
C*PT*WARNING* Constant already double-precision
      IF (X.LT.0.0D0) ERF = -ERF
      RETURN
C*PT*WARNING* Constant already double-precision
C
   30 ERF = SIGN(1.0D0,X)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION ERFC1(IND,X)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C         EVALUATION OF THE COMPLEMENTARY ERROR FUNCTION
C
C          ERFC1(IND,X) = ERFC(X)            IF IND = 0
C          ERFC1(IND,X) = EXP(X*X)*ERFC(X)   OTHERWISE
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER IND
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AX,BOT,C,E,T,TOP,W
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION A(5),B(3),P(8),Q(8),R(5),S(4)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION EXPARG
      EXTERNAL EXPARG
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,EXP
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C-------------------------
C-------------------------
C-------------------------
C-------------------------
      DATA C/.564189583547756D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA A(1)/.771058495001320D-04/,A(2)/-.133733772997339D-02/,
     +     A(3)/.323076579225834D-01/,A(4)/.479137145607681D-01/,
     +     A(5)/.128379167095513D+00/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA B(1)/.301048631703895D-02/,B(2)/.538971687740286D-01/,
     +     B(3)/.375795757275549D+00/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA P(1)/-1.36864857382717D-07/,P(2)/5.64195517478974D-01/,
     +     P(3)/7.21175825088309D+00/,P(4)/4.31622272220567D+01/,
     +     P(5)/1.52989285046940D+02/,P(6)/3.39320816734344D+02/,
     +     P(7)/4.51918953711873D+02/,P(8)/3.00459261020162D+02/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q(1)/1.00000000000000D+00/,Q(2)/1.27827273196294D+01/,
     +     Q(3)/7.70001529352295D+01/,Q(4)/2.77585444743988D+02/,
     +     Q(5)/6.38980264465631D+02/,Q(6)/9.31354094850610D+02/,
     +     Q(7)/7.90950925327898D+02/,Q(8)/3.00459260956983D+02/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA R(1)/2.10144126479064D+00/,R(2)/2.62370141675169D+01/,
     +     R(3)/2.13688200555087D+01/,R(4)/4.65807828718470D+00/,
     +     R(5)/2.82094791773523D-01/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA S(1)/9.41537750555460D+01/,S(2)/1.87114811799590D+02/,
     +     S(3)/9.90191814623914D+01/,S(4)/1.80124575948747D+01/
C     ..
C     .. Executable Statements ..
C-------------------------
C
C                     ABS(X) .LE. 0.5
C
      AX = ABS(X)
C*PT*WARNING* Constant already double-precision
      IF (AX.GT.0.5D0) GO TO 10
      T = X*X
C*PT*WARNING* Constant already double-precision
      TOP = ((((A(1)*T+A(2))*T+A(3))*T+A(4))*T+A(5)) + 1.0D0
C*PT*WARNING* Constant already double-precision
      BOT = ((B(1)*T+B(2))*T+B(3))*T + 1.0D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ERFC1 = 0.5D0 + (0.5D0-X* (TOP/BOT))
      IF (IND.NE.0) ERFC1 = EXP(T)*ERFC1
      RETURN
C*PT*WARNING* Constant already double-precision
C
C                  0.5 .LT. ABS(X) .LE. 4
C
   10 IF (AX.GT.4.0D0) GO TO 20
      TOP = ((((((P(1)*AX+P(2))*AX+P(3))*AX+P(4))*AX+P(5))*AX+P(6))*AX+
     +      P(7))*AX + P(8)
      BOT = ((((((Q(1)*AX+Q(2))*AX+Q(3))*AX+Q(4))*AX+Q(5))*AX+Q(6))*AX+
     +      Q(7))*AX + Q(8)
      ERFC1 = TOP/BOT
      GO TO 40
C*PT*WARNING* Constant already double-precision
C
C                      ABS(X) .GT. 4
C
   20 IF (X.LE.-5.6D0) GO TO 60
      IF (IND.NE.0) GO TO 30
C*PT*WARNING* Constant already double-precision
      IF (X.GT.100.0D0) GO TO 70
      IF (X*X.GT.-EXPARG(1)) GO TO 70
C*PT*WARNING* Constant already double-precision
C
   30 T = (1.0D0/X)**2
      TOP = (((R(1)*T+R(2))*T+R(3))*T+R(4))*T + R(5)
C*PT*WARNING* Constant already double-precision
      BOT = (((S(1)*T+S(2))*T+S(3))*T+S(4))*T + 1.0D0
      ERFC1 = (C-T*TOP/BOT)/AX
C
C                      FINAL ASSEMBLY
C
   40 IF (IND.EQ.0) GO TO 50
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (X.LT.0.0D0) ERFC1 = 2.0D0*EXP(X*X) - ERFC1
      RETURN
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)

   50 W = DBLE(X)*DBLE(X)
      T = W
C*PT*WARNING* Already double-precision (DBLE)
      E = W - DBLE(T)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ERFC1 = ((0.5D0+ (0.5D0-E))*EXP(-T))*ERFC1
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (X.LT.0.0D0) ERFC1 = 2.0D0 - ERFC1
      RETURN
C*PT*WARNING* Constant already double-precision
C
C             LIMIT VALUE FOR LARGE NEGATIVE X
C
   60 ERFC1 = 2.0D0
C*PT*WARNING* Constant already double-precision
      IF (IND.NE.0) ERFC1 = 2.0D0*EXP(X*X)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C             LIMIT VALUE FOR LARGE POSITIVE X
C                       WHEN IND = 0
C
   70 ERFC1 = 0.0D0
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION ESUM(MU,X)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C                    EVALUATION OF EXP(MU + X)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER MU
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION W
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC EXP
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..

      IF (X.GT.0.0D0) GO TO 10
C
      IF (MU.LT.0) GO TO 20
      W = MU + X
C*PT*WARNING* Constant already double-precision
      IF (W.GT.0.0D0) GO TO 20
      ESUM = EXP(W)
      RETURN
C
   10 IF (MU.GT.0) GO TO 20
      W = MU + X
C*PT*WARNING* Constant already double-precision
      IF (W.LT.0.0D0) GO TO 20
      ESUM = EXP(W)
      RETURN
C
   20 W = MU
      ESUM = EXP(W)*EXP(X)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION EXPARG(L)
C--------------------------------------------------------------------
C     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
C     EXP(W) CAN BE COMPUTED.
C
C     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
C     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
C
C     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED.
C--------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER L
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION LNB
      INTEGER B,M
C     ..
C     .. External Functions ..
      INTEGER IPMPAR
      EXTERNAL IPMPAR
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE,DLOG
C     ..
C     .. Executable Statements ..
C
      B = IPMPAR(4)
      IF (B.NE.2) GO TO 10
C*PT*WARNING* Constant already double-precision
      LNB = .69314718055995D0
      GO TO 40

   10 IF (B.NE.8) GO TO 20
C*PT*WARNING* Constant already double-precision
      LNB = 2.0794415416798D0
      GO TO 40

   20 IF (B.NE.16) GO TO 30
C*PT*WARNING* Constant already double-precision
      LNB = 2.7725887222398D0
      GO TO 40
C*PT*WARNING* Already double-precision (DBLE)

   30 LNB = DLOG(DBLE(B))
C
   40 IF (L.EQ.0) GO TO 50
      M = IPMPAR(9) - 1
C*PT*WARNING* Constant already double-precision
      EXPARG = 0.99999D0* (M*LNB)
      RETURN

   50 M = IPMPAR(10)
C*PT*WARNING* Constant already double-precision
      EXPARG = 0.99999D0* (M*LNB)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION FPSER(A,B,X,EPS)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C
C                 EVALUATION OF I (A,B)
C                                X
C
C          FOR B .LT. MIN(EPS,EPS*A) AND X .LE. 0.5.
C
C-----------------------------------------------------------------------
C
C                  SET  FPSER = X**A
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B,EPS,X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AN,C,S,T,TOL
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION EXPARG
      EXTERNAL EXPARG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DLOG,EXP
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..

      FPSER = 1.0D0
C*PT*WARNING* Constant already double-precision
      IF (A.LE.1.D-3*EPS) GO TO 10
C*PT*WARNING* Constant already double-precision
      FPSER = 0.0D0
      T = A*DLOG(X)
      IF (T.LT.EXPARG(1)) RETURN
      FPSER = EXP(T)
C
C                NOTE THAT 1/B(A,B) = B
C
   10 FPSER = (B/A)*FPSER
      TOL = EPS/A
C*PT*WARNING* Constant already double-precision
      AN = A + 1.0D0
      T = X
      S = T/AN
C*PT*WARNING* Constant already double-precision
   20 AN = AN + 1.0D0
      T = X*T
      C = T/AN
      S = S + C
      IF (ABS(C).GT.TOL) GO TO 20
C*PT*WARNING* Constant already double-precision
C
      FPSER = FPSER* (1.0D0+A*S)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION GAM1(A)
C*PT*WARNING* Already double-precision
C     ------------------------------------------------------------------
C     COMPUTATION OF 1/GAMMA(A+1) - 1  FOR -0.5 .LE. A .LE. 1.5
C     ------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BOT,D,S1,S2,T,TOP,W
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION P(7),Q(5),R(9)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C     -------------------
C     -------------------
C     -------------------
C     -------------------
      DATA P(1)/.577215664901533D+00/,P(2)/-.409078193005776D+00/,
     +     P(3)/-.230975380857675D+00/,P(4)/.597275330452234D-01/,
     +     P(5)/.766968181649490D-02/,P(6)/-.514889771323592D-02/,
     +     P(7)/.589597428611429D-03/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q(1)/.100000000000000D+01/,Q(2)/.427569613095214D+00/,
     +     Q(3)/.158451672430138D+00/,Q(4)/.261132021441447D-01/,
     +     Q(5)/.423244297896961D-02/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA R(1)/-.422784335098468D+00/,R(2)/-.771330383816272D+00/,
     +     R(3)/-.244757765222226D+00/,R(4)/.118378989872749D+00/,
     +     R(5)/.930357293360349D-03/,R(6)/-.118290993445146D-01/,
     +     R(7)/.223047661158249D-02/,R(8)/.266505979058923D-03/,
     +     R(9)/-.132674909766242D-03/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA S1/.273076135303957D+00/,S2/.559398236957378D-01/
C     ..
C     .. Executable Statements ..
C     -------------------
      T = A
C*PT*WARNING* Constant already double-precision
      D = A - 0.5D0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (D.GT.0.0D0) T = D - 0.5D0
      IF (T) 40,10,20
C*PT*WARNING* Constant already double-precision
C
   10 GAM1 = 0.0D0
      RETURN
C
   20 TOP = (((((P(7)*T+P(6))*T+P(5))*T+P(4))*T+P(3))*T+P(2))*T + P(1)
C*PT*WARNING* Constant already double-precision
      BOT = (((Q(5)*T+Q(4))*T+Q(3))*T+Q(2))*T + 1.0D0
      W = TOP/BOT
C*PT*WARNING* Constant already double-precision
      IF (D.GT.0.0D0) GO TO 30
      GAM1 = A*W
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision

   30 GAM1 = (T/A)* ((W-0.5D0)-0.5D0)
      RETURN
C
   40 TOP = (((((((R(9)*T+R(8))*T+R(7))*T+R(6))*T+R(5))*T+R(4))*T+R(3))*
     +      T+R(2))*T + R(1)
C*PT*WARNING* Constant already double-precision
      BOT = (S2*T+S1)*T + 1.0D0
      W = TOP/BOT
C*PT*WARNING* Constant already double-precision
      IF (D.GT.0.0D0) GO TO 50
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      GAM1 = A* ((W+0.5D0)+0.5D0)
      RETURN

   50 GAM1 = T*W/A
      RETURN

      END
      SUBROUTINE GAMINV(A,X,X0,P,Q,IERR)
C*PT*WARNING* Already double-precision
C ----------------------------------------------------------------------
C            INVERSE INCOMPLETE GAMMA RATIO FUNCTION
C
C     GIVEN POSITIVE A, AND NONEGATIVE P AND Q WHERE P + Q = 1.
C     THEN X IS COMPUTED WHERE P(A,X) = P AND Q(A,X) = Q. SCHRODER
C     ITERATION IS EMPLOYED. THE ROUTINE ATTEMPTS TO COMPUTE X
C     TO 10 SIGNIFICANT DIGITS IF THIS IS POSSIBLE FOR THE
C     PARTICULAR COMPUTER ARITHMETIC BEING USED.
C
C                      ------------
C
C     X IS A VARIABLE. IF P = 0 THEN X IS ASSIGNED THE VALUE 0,
C     AND IF Q = 0 THEN X IS SET TO THE LARGEST FLOATING POINT
C     NUMBER AVAILABLE. OTHERWISE, GAMINV ATTEMPTS TO OBTAIN
C     A SOLUTION FOR P(A,X) = P AND Q(A,X) = Q. IF THE ROUTINE
C     IS SUCCESSFUL THEN THE SOLUTION IS STORED IN X.
C
C     X0 IS AN OPTIONAL INITIAL APPROXIMATION FOR X. IF THE USER
C     DOES NOT WISH TO SUPPLY AN INITIAL APPROXIMATION, THEN SET
C     X0 .LE. 0.
C
C     IERR IS A VARIABLE THAT REPORTS THE STATUS OF THE RESULTS.
C     WHEN THE ROUTINE TERMINATES, IERR HAS ONE OF THE FOLLOWING
C     VALUES ...
C
C       IERR =  0    THE SOLUTION WAS OBTAINED. ITERATION WAS
C                    NOT USED.
C       IERR.GT.0    THE SOLUTION WAS OBTAINED. IERR ITERATIONS
C                    WERE PERFORMED.
C       IERR = -2    (INPUT ERROR) A .LE. 0
C       IERR = -3    NO SOLUTION WAS OBTAINED. THE RATIO Q/A
C                    IS TOO LARGE.
C       IERR = -4    (INPUT ERROR) P + Q .NE. 1
C       IERR = -6    20 ITERATIONS WERE PERFORMED. THE MOST
C                    RECENT VALUE OBTAINED FOR X IS GIVEN.
C                    THIS CANNOT OCCUR IF X0 .LE. 0.
C       IERR = -7    ITERATION FAILED. NO VALUE IS GIVEN FOR X.
C                    THIS MAY OCCUR WHEN X IS APPROXIMATELY 0.
C       IERR = -8    A VALUE FOR X HAS BEEN OBTAINED, BUT THE
C                    ROUTINE IS NOT CERTAIN OF ITS ACCURACY.
C                    ITERATION CANNOT BE PERFORMED IN THIS
C                    CASE. IF X0 .LE. 0, THIS CAN OCCUR ONLY
C                    WHEN P OR Q IS APPROXIMATELY 0. IF X0 IS
C                    POSITIVE THEN THIS CAN OCCUR WHEN A IS
C                    EXCEEDINGLY CLOSE TO X AND A IS EXTREMELY
C                    LARGE (SAY A .GE. 1.E20).
C ----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS, JR.
C        NAVAL SURFACE WEAPONS CENTER
C        DAHLGREN, VIRGINIA
C     -------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,P,Q,X,X0
      INTEGER IERR
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A0,A1,A2,A3,AM1,AMAX,AP1,AP2,AP3,APN,B,B1,B2,B3,
     +                 B4,C,C1,C2,C3,C4,C5,D,E,E2,EPS,G,H,LN10,PN,QG,QN,
     +                 R,RTA,S,S2,SUM,T,TOL,U,W,XMAX,XMIN,XN,Y,Z
      INTEGER IOP
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION AMIN(2),BMIN(2),DMIN(2),EMIN(2),EPS0(2)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALNREL,GAMLN,GAMLN1,GAMMA,RCOMP,SPMPAR
      EXTERNAL ALNREL,GAMLN,GAMLN1,GAMMA,RCOMP,SPMPAR
C     ..
C     .. External Subroutines ..
      EXTERNAL GRATIO
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DLOG,DMAX1,EXP,SQRT
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C     -------------------
C     LN10 = LN(10)
C     C = EULER CONSTANT
C     -------------------
C     -------------------
C     -------------------
C     -------------------
      DATA LN10/2.302585D0/
C*PT*WARNING* Constant already double-precision
      DATA C/.577215664901533D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA A0/3.31125922108741D0/,A1/11.6616720288968D0/,
     +     A2/4.28342155967104D0/,A3/.213623493715853D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA B1/6.61053765625462D0/,B2/6.40691597760039D0/,
     +     B3/1.27364489782223D0/,B4/.036117081018842D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA EPS0(1)/1.D-10/,EPS0(2)/1.D-08/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA AMIN(1)/500.0D0/,AMIN(2)/100.0D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA BMIN(1)/1.D-28/,BMIN(2)/1.D-13/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA DMIN(1)/1.D-06/,DMIN(2)/1.D-04/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA EMIN(1)/2.D-03/,EMIN(2)/6.D-03/
C*PT*WARNING* Constant already double-precision
      DATA TOL/1.D-5/
C     ..
C     .. Executable Statements ..
C     -------------------
C     ****** E, XMIN, AND XMAX ARE MACHINE DEPENDENT CONSTANTS.
C            E IS THE SMALLEST NUMBER FOR WHICH 1.0 + E .GT. 1.0.
C            XMIN IS THE SMALLEST POSITIVE NUMBER AND XMAX IS THE
C            LARGEST POSITIVE NUMBER.
C
      E = SPMPAR(1)
      XMIN = SPMPAR(2)
      XMAX = SPMPAR(3)
C*PT*WARNING* Constant already double-precision
C     -------------------
      X = 0.0D0
C*PT*WARNING* Constant already double-precision
      IF (A.LE.0.0D0) GO TO 300
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision
      T = DBLE(P) + DBLE(Q) - 1.D0
      IF (ABS(T).GT.E) GO TO 320
C
      IERR = 0
C*PT*WARNING* Constant already double-precision
      IF (P.EQ.0.0D0) RETURN
C*PT*WARNING* Constant already double-precision
      IF (Q.EQ.0.0D0) GO TO 270
C*PT*WARNING* Constant already double-precision
      IF (A.EQ.1.0D0) GO TO 280
C*PT*WARNING* Constant already double-precision
C
      E2 = 2.0D0*E
C*PT*WARNING* Constant already double-precision
      AMAX = 0.4D-10/ (E*E)
      IOP = 1
C*PT*WARNING* Constant already double-precision
      IF (E.GT.1.D-10) IOP = 2
      EPS = EPS0(IOP)
      XN = X0
C*PT*WARNING* Constant already double-precision
      IF (X0.GT.0.0D0) GO TO 160
C*PT*WARNING* Constant already double-precision
C
C        SELECTION OF THE INITIAL APPROXIMATION XN OF X
C                       WHEN A .LT. 1
C
      IF (A.GT.1.0D0) GO TO 80
C*PT*WARNING* Constant already double-precision
      G = GAMMA(A+1.0D0)
      QG = Q*G
C*PT*WARNING* Constant already double-precision
      IF (QG.EQ.0.0D0) GO TO 360
      B = QG/A
C*PT*WARNING* Constant already double-precision
      IF (QG.GT.0.6D0*A) GO TO 40
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (A.GE.0.30D0 .OR. B.LT.0.35D0) GO TO 10
      T = EXP(- (B+C))
      U = T*EXP(T)
      XN = T*EXP(U)
      GO TO 160
C*PT*WARNING* Constant already double-precision
C
   10 IF (B.GE.0.45D0) GO TO 40
C*PT*WARNING* Constant already double-precision
      IF (B.EQ.0.0D0) GO TO 360
      Y = -DLOG(B)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      S = 0.5D0 + (0.5D0-A)
      Z = DLOG(Y)
      T = Y - S*Z
C*PT*WARNING* Constant already double-precision
      IF (B.LT.0.15D0) GO TO 20
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      XN = Y - S*DLOG(T) - DLOG(1.0D0+S/ (T+1.0D0))
      GO TO 220
C*PT*WARNING* Constant already double-precision

   20 IF (B.LE.0.01D0) GO TO 30
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      U = ((T+2.0D0* (3.0D0-A))*T+ (2.0D0-A)* (3.0D0-A))/
     +    ((T+ (5.0D0-A))*T+2.0D0)
      XN = Y - S*DLOG(T) - DLOG(U)
      GO TO 220

   30 C1 = -S*Z
C*PT*WARNING* Constant already double-precision
      C2 = -S* (1.0D0+C1)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      C3 = S* ((0.5D0*C1+ (2.0D0-A))*C1+ (2.5D0-1.5D0*A))
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      C4 = -S* (((C1/3.0D0+ (2.5D0-1.5D0*A))*C1+ ((A-6.0D0)*A+7.0D0))*
     +     C1+ ((11.0D0*A-46)*A+47.0D0)/6.0D0)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      C5 = -S* ((((-C1/4.0D0+ (11.0D0*A-17.0D0)/6.0D0)*C1+ ((-3.0D0*A+
     +     13.0D0)*A-13.0D0))*C1+0.5D0* (((2.0D0*A-25.0D0)*A+72.0D0)*A-
     +     61.0D0))*C1+ (((25.0D0*A-195.0D0)*A+477.0D0)*A-379.0D0)/
     +     12.0D0)
      XN = ((((C5/Y+C4)/Y+C3)/Y+C2)/Y+C1) + Y
C*PT*WARNING* Constant already double-precision
      IF (A.GT.1.0D0) GO TO 220
      IF (B.GT.BMIN(IOP)) GO TO 220
      X = XN
      RETURN
C*PT*WARNING* Constant already double-precision
C
   40 IF (B*Q.GT.1.D-8) GO TO 50
      XN = EXP(- (Q/A+C))
      GO TO 70
C*PT*WARNING* Constant already double-precision

   50 IF (P.LE.0.9D0) GO TO 60
      XN = EXP((ALNREL(-Q)+GAMLN1(A))/A)
      GO TO 70

   60 XN = EXP(DLOG(P*G)/A)
C*PT*WARNING* Constant already double-precision
   70 IF (XN.EQ.0.0D0) GO TO 310
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      T = 0.5D0 + (0.5D0-XN/ (A+1.0D0))
      XN = XN/T
      GO TO 160
C*PT*WARNING* Constant already double-precision
C
C        SELECTION OF THE INITIAL APPROXIMATION XN OF X
C                       WHEN A .GT. 1
C
   80 IF (Q.LE.0.5D0) GO TO 90
      W = DLOG(P)
      GO TO 100

   90 W = DLOG(Q)
C*PT*WARNING* Constant already double-precision
  100 T = SQRT(-2.0D0*W)
C*PT*WARNING* Constant already double-precision
      S = T - (((A3*T+A2)*T+A1)*T+A0)/ ((((B4*T+B3)*T+B2)*T+B1)*T+1.0D0)
C*PT*WARNING* Constant already double-precision
      IF (Q.GT.0.5D0) S = -S
C
      RTA = SQRT(A)
      S2 = S*S
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      XN = A + S*RTA + (S2-1.0D0)/3.0D0 + S* (S2-7.0D0)/ (36.0D0*RTA) -
     +     ((3.0D0*S2+7.0D0)*S2-16.0D0)/ (810.0D0*A) +
     +     S* ((9.0D0*S2+256.0D0)*S2-433.0D0)/ (38880.0D0*A*RTA)
C*PT*WARNING* Constant already double-precision
      XN = DMAX1(XN,0.0D0)
      IF (A.LT.AMIN(IOP)) GO TO 110
      X = XN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      D = 0.5D0 + (0.5D0-X/A)
      IF (ABS(D).LE.DMIN(IOP)) RETURN
C*PT*WARNING* Constant already double-precision
C
  110 IF (P.LE.0.5D0) GO TO 130
C*PT*WARNING* Constant already double-precision
      IF (XN.LT.3.0D0*A) GO TO 220
      Y = - (W+GAMLN(A))
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      D = DMAX1(2.0D0,A* (A-1.0D0))
      IF (Y.LT.LN10*D) GO TO 120
C*PT*WARNING* Constant already double-precision
      S = 1.0D0 - A
      Z = DLOG(Y)
      GO TO 30
C*PT*WARNING* Constant already double-precision

  120 T = A - 1.0D0
C*PT*WARNING* Constant already double-precision
      XN = Y + T*DLOG(XN) - ALNREL(-T/ (XN+1.0D0))
C*PT*WARNING* Constant already double-precision
      XN = Y + T*DLOG(XN) - ALNREL(-T/ (XN+1.0D0))
      GO TO 220
C*PT*WARNING* Constant already double-precision
C
  130 AP1 = A + 1.0D0
C*PT*WARNING* Constant already double-precision
      IF (XN.GT.0.70D0*AP1) GO TO 170
      W = W + GAMLN(AP1)
C*PT*WARNING* Constant already double-precision
      IF (XN.GT.0.15D0*AP1) GO TO 140
C*PT*WARNING* Constant already double-precision
      AP2 = A + 2.0D0
C*PT*WARNING* Constant already double-precision
      AP3 = A + 3.0D0
      X = EXP((W+X)/A)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      X = EXP((W+X-DLOG(1.0D0+ (X/AP1)* (1.0D0+X/AP2)))/A)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      X = EXP((W+X-DLOG(1.0D0+ (X/AP1)* (1.0D0+X/AP2)))/A)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      X = EXP((W+X-DLOG(1.0D0+ (X/AP1)* (1.0D0+ (X/AP2)* (1.0D0+
     +    X/AP3))))/A)
      XN = X
C*PT*WARNING* Constant already double-precision
      IF (XN.GT.1.D-2*AP1) GO TO 140
      IF (XN.LE.EMIN(IOP)*AP1) RETURN
      GO TO 170
C
  140 APN = AP1
      T = XN/APN
C*PT*WARNING* Constant already double-precision
      SUM = 1.0D0 + T
C*PT*WARNING* Constant already double-precision
  150 APN = APN + 1.0D0
      T = T* (XN/APN)
      SUM = SUM + T
C*PT*WARNING* Constant already double-precision
      IF (T.GT.1.D-4) GO TO 150
      T = W - DLOG(SUM)
      XN = EXP((XN+T)/A)
C*PT*WARNING* Constant already double-precision
      XN = XN* (1.0D0- (A*DLOG(XN)-XN-T)/ (A-XN))
      GO TO 170
C*PT*WARNING* Constant already double-precision
C
C                 SCHRODER ITERATION USING P
C
  160 IF (P.GT.0.5D0) GO TO 220
C*PT*WARNING* Constant already double-precision
  170 IF (P.LE.1.D10*XMIN) GO TO 350
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      AM1 = (A-0.5D0) - 0.5D0
  180 IF (A.LE.AMAX) GO TO 190
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      D = 0.5D0 + (0.5D0-XN/A)
      IF (ABS(D).LE.E2) GO TO 350
C
  190 IF (IERR.GE.20) GO TO 330
      IERR = IERR + 1
      CALL GRATIO(A,XN,PN,QN,0)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (PN.EQ.0.0D0 .OR. QN.EQ.0.0D0) GO TO 350
      R = RCOMP(A,XN)
C*PT*WARNING* Constant already double-precision
      IF (R.EQ.0.0D0) GO TO 350
      T = (PN-P)/R
C*PT*WARNING* Constant already double-precision
      W = 0.5D0* (AM1-XN)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (ABS(T).LE.0.1D0 .AND. ABS(W*T).LE.0.1D0) GO TO 200
C*PT*WARNING* Constant already double-precision
      X = XN* (1.0D0-T)
C*PT*WARNING* Constant already double-precision
      IF (X.LE.0.0D0) GO TO 340
      D = ABS(T)
      GO TO 210
C*PT*WARNING* Constant already double-precision
C
  200 H = T* (1.0D0+W*T)
C*PT*WARNING* Constant already double-precision
      X = XN* (1.0D0-H)
C*PT*WARNING* Constant already double-precision
      IF (X.LE.0.0D0) GO TO 340
C*PT*WARNING* Constant already double-precision
      IF (ABS(W).GE.1.0D0 .AND. ABS(W)*T*T.LE.EPS) RETURN
      D = ABS(H)
  210 XN = X
      IF (D.GT.TOL) GO TO 180
      IF (D.LE.EPS) RETURN
      IF (ABS(P-PN).LE.TOL*P) RETURN
      GO TO 180
C*PT*WARNING* Constant already double-precision
C
C                 SCHRODER ITERATION USING Q
C
  220 IF (Q.LE.1.D10*XMIN) GO TO 350
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      AM1 = (A-0.5D0) - 0.5D0
  230 IF (A.LE.AMAX) GO TO 240
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      D = 0.5D0 + (0.5D0-XN/A)
      IF (ABS(D).LE.E2) GO TO 350
C
  240 IF (IERR.GE.20) GO TO 330
      IERR = IERR + 1
      CALL GRATIO(A,XN,PN,QN,0)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (PN.EQ.0.0D0 .OR. QN.EQ.0.0D0) GO TO 350
      R = RCOMP(A,XN)
C*PT*WARNING* Constant already double-precision
      IF (R.EQ.0.0D0) GO TO 350
      T = (Q-QN)/R
C*PT*WARNING* Constant already double-precision
      W = 0.5D0* (AM1-XN)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (ABS(T).LE.0.1D0 .AND. ABS(W*T).LE.0.1D0) GO TO 250
C*PT*WARNING* Constant already double-precision
      X = XN* (1.0D0-T)
C*PT*WARNING* Constant already double-precision
      IF (X.LE.0.0D0) GO TO 340
      D = ABS(T)
      GO TO 260
C*PT*WARNING* Constant already double-precision
C
  250 H = T* (1.0D0+W*T)
C*PT*WARNING* Constant already double-precision
      X = XN* (1.0D0-H)
C*PT*WARNING* Constant already double-precision
      IF (X.LE.0.0D0) GO TO 340
C*PT*WARNING* Constant already double-precision
      IF (ABS(W).GE.1.0D0 .AND. ABS(W)*T*T.LE.EPS) RETURN
      D = ABS(H)
  260 XN = X
      IF (D.GT.TOL) GO TO 230
      IF (D.LE.EPS) RETURN
      IF (ABS(Q-QN).LE.TOL*Q) RETURN
      GO TO 230
C
C                       SPECIAL CASES
C
  270 X = XMAX
      RETURN
C*PT*WARNING* Constant already double-precision
C
  280 IF (Q.LT.0.9D0) GO TO 290
      X = -ALNREL(-P)
      RETURN

  290 X = -DLOG(Q)
      RETURN
C
C                       ERROR RETURN
C
  300 IERR = -2
      RETURN
C
  310 IERR = -3
      RETURN
C
  320 IERR = -4
      RETURN
C
  330 IERR = -6
      RETURN
C
  340 IERR = -7
      RETURN
C
  350 X = XN
      IERR = -8
      RETURN
C
  360 X = XMAX
      IERR = -8
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION GAMLN(A)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C            EVALUATION OF LN(GAMMA(A)) FOR POSITIVE A
C-----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS
C          NAVAL SURFACE WARFARE CENTER
C          DAHLGREN, VIRGINIA
C--------------------------
C     D = 0.5*(LN(2*PI) - 1)
C--------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION C0,C1,C2,C3,C4,C5,D,T,W
      INTEGER I,N
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION GAMLN1
      EXTERNAL GAMLN1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DLOG
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C--------------------------
      DATA D/.418938533204673D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA C0/.833333333333333D-01/,C1/-.277777777760991D-02/,
     +     C2/.793650666825390D-03/,C3/-.595202931351870D-03/,
     +     C4/.837308034031215D-03/,C5/-.165322962780713D-02/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C-----------------------------------------------------------------------
      IF (A.GT.0.8D0) GO TO 10
      GAMLN = GAMLN1(A) - DLOG(A)
      RETURN
C*PT*WARNING* Constant already double-precision

   10 IF (A.GT.2.25D0) GO TO 20
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      T = (A-0.5D0) - 0.5D0
      GAMLN = GAMLN1(T)
      RETURN
C*PT*WARNING* Constant already double-precision
C
   20 IF (A.GE.10.0D0) GO TO 40
C*PT*WARNING* Constant already double-precision
      N = A - 1.25D0
      T = A
C*PT*WARNING* Constant already double-precision
      W = 1.0D0
      DO 30 I = 1,N
C*PT*WARNING* Constant already double-precision
          T = T - 1.0D0
          W = T*W
   30 CONTINUE
C*PT*WARNING* Constant already double-precision
      GAMLN = GAMLN1(T-1.0D0) + DLOG(W)
      RETURN
C*PT*WARNING* Constant already double-precision
C
   40 T = (1.0D0/A)**2
      W = (((((C5*T+C4)*T+C3)*T+C2)*T+C1)*T+C0)/A
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      GAMLN = (D+W) + (A-0.5D0)* (DLOG(A)-1.0D0)
      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION GAMLN1(A)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C     EVALUATION OF LN(GAMMA(1 + A)) FOR -0.2 .LE. A .LE. 1.25
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION P0,P1,P2,P3,P4,P5,P6,Q1,Q2,Q3,Q4,Q5,Q6,R0,R1,R2,
     +                 R3,R4,R5,S1,S2,S3,S4,S5,W,X
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C----------------------
      DATA P0/.577215664901533D+00/,P1/.844203922187225D+00/,
     +     P2/-.168860593646662D+00/,P3/-.780427615533591D+00/,
     +     P4/-.402055799310489D+00/,P5/-.673562214325671D-01/,
     +     P6/-.271935708322958D-02/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q1/.288743195473681D+01/,Q2/.312755088914843D+01/,
     +     Q3/.156875193295039D+01/,Q4/.361951990101499D+00/,
     +     Q5/.325038868253937D-01/,Q6/.667465618796164D-03/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA R0/.422784335098467D+00/,R1/.848044614534529D+00/,
     +     R2/.565221050691933D+00/,R3/.156513060486551D+00/,
     +     R4/.170502484022650D-01/,R5/.497958207639485D-03/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA S1/.124313399877507D+01/,S2/.548042109832463D+00/,
     +     S3/.101552187439830D+00/,S4/.713309612391000D-02/,
     +     S5/.116165475989616D-03/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C----------------------
      IF (A.GE.0.6D0) GO TO 10
C*PT*WARNING* Constant already double-precision
      W = ((((((P6*A+P5)*A+P4)*A+P3)*A+P2)*A+P1)*A+P0)/
     +    ((((((Q6*A+Q5)*A+Q4)*A+Q3)*A+Q2)*A+Q1)*A+1.0D0)
      GAMLN1 = -A*W
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
   10 X = (A-0.5D0) - 0.5D0
C*PT*WARNING* Constant already double-precision
      W = (((((R5*X+R4)*X+R3)*X+R2)*X+R1)*X+R0)/
     +    (((((S5*X+S4)*X+S3)*X+S2)*X+S1)*X+1.0D0)
      GAMLN1 = X*W
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION GAMMA(A)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C
C         EVALUATION OF THE GAMMA FUNCTION FOR REAL ARGUMENTS
C
C                           -----------
C
C     GAMMA(A) IS ASSIGNED THE VALUE 0 WHEN THE GAMMA FUNCTION CANNOT
C     BE COMPUTED.
C
C-----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS, JR.
C          NAVAL SURFACE WEAPONS CENTER
C          DAHLGREN, VIRGINIA
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION BOT,D,G,LNX,PI,R1,R2,R3,R4,R5,S,T,TOP,W,X,Z
      INTEGER I,J,M,N
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION P(7),Q(7)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION EXPARG,SPMPAR
      EXTERNAL EXPARG,SPMPAR
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DLOG,EXP,INT,MOD,SIN
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C--------------------------
C     D = 0.5*(LN(2*PI) - 1)
C--------------------------
C--------------------------
C--------------------------
      DATA PI/3.1415926535898D0/
C*PT*WARNING* Constant already double-precision
      DATA D/.41893853320467274178D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA P(1)/.539637273585445D-03/,P(2)/.261939260042690D-02/,
     +     P(3)/.204493667594920D-01/,P(4)/.730981088720487D-01/,
     +     P(5)/.279648642639792D+00/,P(6)/.553413866010467D+00/,
     +     P(7)/1.0D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q(1)/-.832979206704073D-03/,Q(2)/.470059485860584D-02/,
     +     Q(3)/.225211131035340D-01/,Q(4)/-.170458969313360D+00/,
     +     Q(5)/-.567902761974940D-01/,Q(6)/.113062953091122D+01/,
     +     Q(7)/1.0D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA R1/.820756370353826D-03/,R2/-.595156336428591D-03/,
     +     R3/.793650663183693D-03/,R4/-.277777777770481D-02/,
     +     R5/.833333333333333D-01/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C--------------------------
      GAMMA = 0.0D0
      X = A
C*PT*WARNING* Constant already double-precision
      IF (ABS(A).GE.15.0D0) GO TO 110
C*PT*WARNING* Constant already double-precision
C-----------------------------------------------------------------------
C            EVALUATION OF GAMMA(A) FOR ABS(A) .LT. 15
C-----------------------------------------------------------------------
      T = 1.0D0
      M = INT(A) - 1
C
C     LET T BE THE PRODUCT OF A-J WHEN A .GE. 2
C
      IF (M) 40,30,10
   10 DO 20 J = 1,M
C*PT*WARNING* Constant already double-precision
          X = X - 1.0D0
          T = X*T
   20 CONTINUE
C*PT*WARNING* Constant already double-precision
   30 X = X - 1.0D0
      GO TO 80
C
C     LET T BE THE PRODUCT OF A+J WHEN A .LT. 1
C
   40 T = A
C*PT*WARNING* Constant already double-precision
      IF (A.GT.0.0D0) GO TO 70
      M = -M - 1
      IF (M.EQ.0) GO TO 60
      DO 50 J = 1,M
C*PT*WARNING* Constant already double-precision
          X = X + 1.0D0
          T = X*T
   50 CONTINUE
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
   60 X = (X+0.5D0) + 0.5D0
      T = X*T
C*PT*WARNING* Constant already double-precision
      IF (T.EQ.0.0D0) RETURN
C
   70 CONTINUE
C*PT*WARNING* Constant already double-precision
C
C     THE FOLLOWING CODE CHECKS IF 1/T CAN OVERFLOW. THIS
C     CODE MAY BE OMITTED IF DESIRED.
C
      IF (ABS(T).GE.1.D-30) GO TO 80
C*PT*WARNING* Constant already double-precision
      IF (ABS(T)*SPMPAR(3).LE.1.0001D0) RETURN
C*PT*WARNING* Constant already double-precision
      GAMMA = 1.0D0/T
      RETURN
C
C     COMPUTE GAMMA(1 + X) FOR  0 .LE. X .LT. 1
C
   80 TOP = P(1)
      BOT = Q(1)
      DO 90 I = 2,7
          TOP = P(I) + X*TOP
          BOT = Q(I) + X*BOT
   90 CONTINUE
      GAMMA = TOP/BOT
C*PT*WARNING* Constant already double-precision
C
C     TERMINATION
C
      IF (A.LT.1.0D0) GO TO 100
      GAMMA = GAMMA*T
      RETURN

  100 GAMMA = GAMMA/T
      RETURN
C*PT*WARNING* Constant already double-precision
C-----------------------------------------------------------------------
C            EVALUATION OF GAMMA(A) FOR ABS(A) .GE. 15
C-----------------------------------------------------------------------
  110 IF (ABS(A).GE.1.D3) RETURN
C*PT*WARNING* Constant already double-precision
      IF (A.GT.0.0D0) GO TO 120
      X = -A
      N = X
      T = X - N
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (T.GT.0.9D0) T = 1.0D0 - T
      S = SIN(PI*T)/PI
      IF (MOD(N,2).EQ.0) S = -S
C*PT*WARNING* Constant already double-precision
      IF (S.EQ.0.0D0) RETURN
C*PT*WARNING* Constant already double-precision
C
C     COMPUTE THE MODIFIED ASYMPTOTIC SUM
C
  120 T = 1.0D0/ (X*X)
      G = ((((R1*T+R2)*T+R3)*T+R4)*T+R5)/X
C
C     ONE MAY REPLACE THE NEXT STATEMENT WITH  LNX = ALOG(X)
C     BUT LESS ACCURACY WILL NORMALLY BE OBTAINED.
C
      LNX = DLOG(X)
C
C     FINAL ASSEMBLY
C
      Z = X
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      G = (D+G) + (Z-0.5D0)* (LNX-1.D0)
      W = G
C*PT*WARNING* Already double-precision (DBLE)
      T = G - DBLE(W)
C*PT*WARNING* Constant already double-precision
      IF (W.GT.0.99999D0*EXPARG(0)) RETURN
C*PT*WARNING* Constant already double-precision
      GAMMA = EXP(W)* (1.0D0+T)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (A.LT.0.0D0) GAMMA = (1.0D0/ (GAMMA*S))/X
      RETURN

      END
      SUBROUTINE GRAT1(A,X,R,P,Q,EPS)
C*PT*WARNING* Already double-precision
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,EPS,P,Q,R,X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A2N,A2NM1,AM0,AN,AN0,B2N,B2NM1,C,CMA,G,H,J,L,SUM,
     +                 T,TOL,W,Z
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ERF,ERFC1,GAM1,REXP
      EXTERNAL ERF,ERFC1,GAM1,REXP
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DLOG,EXP,SQRT
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C        EVALUATION OF THE INCOMPLETE GAMMA RATIO FUNCTIONS
C                      P(A,X) AND Q(A,X)
C
C     IT IS ASSUMED THAT A .LE. 1.  EPS IS THE TOLERANCE TO BE USED.
C     THE INPUT ARGUMENT R HAS THE VALUE E**(-X)*X**A/GAMMA(A).
C-----------------------------------------------------------------------
      IF (A*X.EQ.0.0D0) GO TO 120
C*PT*WARNING* Constant already double-precision
      IF (A.EQ.0.5D0) GO TO 100
C*PT*WARNING* Constant already double-precision
      IF (X.LT.1.1D0) GO TO 10
      GO TO 60
C*PT*WARNING* Constant already double-precision
C
C             TAYLOR SERIES FOR P(A,X)/X**A
C
   10 AN = 3.0D0
      C = X
C*PT*WARNING* Constant already double-precision
      SUM = X/ (A+3.0D0)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      TOL = 0.1D0*EPS/ (A+1.0D0)
C*PT*WARNING* Constant already double-precision
   20 AN = AN + 1.0D0
      C = -C* (X/AN)
      T = C/ (A+AN)
      SUM = SUM + T
      IF (ABS(T).GT.TOL) GO TO 20
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      J = A*X* ((SUM/6.0D0-0.5D0/ (A+2.0D0))*X+1.0D0/ (A+1.0D0))
C
      Z = A*DLOG(X)
      H = GAM1(A)
C*PT*WARNING* Constant already double-precision
      G = 1.0D0 + H
C*PT*WARNING* Constant already double-precision
      IF (X.LT.0.25D0) GO TO 30
C*PT*WARNING* Constant already double-precision
      IF (A.LT.X/2.59D0) GO TO 50
      GO TO 40
C*PT*WARNING* Constant already double-precision

   30 IF (Z.GT.-.13394D0) GO TO 50
C
   40 W = EXP(Z)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      P = W*G* (0.5D0+ (0.5D0-J))
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      Q = 0.5D0 + (0.5D0-P)
      RETURN
C
   50 L = REXP(Z)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W = 0.5D0 + (0.5D0+L)
      Q = (W*J-L)*G - H
C*PT*WARNING* Constant already double-precision
      IF (Q.LT.0.0D0) GO TO 90
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      P = 0.5D0 + (0.5D0-Q)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C              CONTINUED FRACTION EXPANSION
C
   60 A2NM1 = 1.0D0
C*PT*WARNING* Constant already double-precision
      A2N = 1.0D0
      B2NM1 = X
C*PT*WARNING* Constant already double-precision
      B2N = X + (1.0D0-A)
C*PT*WARNING* Constant already double-precision
      C = 1.0D0
   70 A2NM1 = X*A2N + C*A2NM1
      B2NM1 = X*B2N + C*B2NM1
      AM0 = A2NM1/B2NM1
C*PT*WARNING* Constant already double-precision
      C = C + 1.0D0
      CMA = C - A
      A2N = A2NM1 + CMA*A2N
      B2N = B2NM1 + CMA*B2N
      AN0 = A2N/B2N
      IF (ABS(AN0-AM0).GE.EPS*AN0) GO TO 70
      Q = R*AN0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      P = 0.5D0 + (0.5D0-Q)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C                SPECIAL CASES
C
   80 P = 0.0D0
C*PT*WARNING* Constant already double-precision
      Q = 1.0D0
      RETURN
C*PT*WARNING* Constant already double-precision
C
   90 P = 1.0D0
C*PT*WARNING* Constant already double-precision
      Q = 0.0D0
      RETURN
C*PT*WARNING* Constant already double-precision
C
  100 IF (X.GE.0.25D0) GO TO 110
      P = ERF(SQRT(X))
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      Q = 0.5D0 + (0.5D0-P)
      RETURN

  110 Q = ERFC1(0,SQRT(X))
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      P = 0.5D0 + (0.5D0-Q)
      RETURN
C
  120 IF (X.LE.A) GO TO 80
      GO TO 90

      END
      SUBROUTINE GRATIO(A,X,ANS,QANS,IND)
C*PT*WARNING* Already double-precision
C ----------------------------------------------------------------------
C        EVALUATION OF THE INCOMPLETE GAMMA RATIO FUNCTIONS
C                      P(A,X) AND Q(A,X)
C
C                        ----------
C
C     IT IS ASSUMED THAT A AND X ARE NONNEGATIVE, WHERE A AND X
C     ARE NOT BOTH 0.
C
C     ANS AND QANS ARE VARIABLES. GRATIO ASSIGNS ANS THE VALUE
C     P(A,X) AND QANS THE VALUE Q(A,X). IND MAY BE ANY INTEGER.
C     IF IND = 0 THEN THE USER IS REQUESTING AS MUCH ACCURACY AS
C     POSSIBLE (UP TO 14 SIGNIFICANT DIGITS). OTHERWISE, IF
C     IND = 1 THEN ACCURACY IS REQUESTED TO WITHIN 1 UNIT OF THE
C     6-TH SIGNIFICANT DIGIT, AND IF IND .NE. 0,1 THEN ACCURACY
C     IS REQUESTED TO WITHIN 1 UNIT OF THE 3RD SIGNIFICANT DIGIT.
C
C     ERROR RETURN ...
C        ANS IS ASSIGNED THE VALUE 2 WHEN A OR X IS NEGATIVE,
C     WHEN A*X = 0, OR WHEN P(A,X) AND Q(A,X) ARE INDETERMINANT.
C     P(A,X) AND Q(A,X) ARE COMPUTATIONALLY INDETERMINANT WHEN
C     X IS EXCEEDINGLY CLOSE TO A AND A IS EXTREMELY LARGE.
C ----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS, JR.
C        NAVAL SURFACE WEAPONS CENTER
C        DAHLGREN, VIRGINIA
C     --------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,ANS,QANS,X
      INTEGER IND
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A2N,A2NM1,ACC,ALOG10,AM0,AMN,AN,AN0,APN,B2N,
     +                 B2NM1,C,C0,C1,C2,C3,C4,C5,C6,CMA,D10,D20,D30,D40,
     +                 D50,D60,D70,E,E0,G,H,J,L,R,RT2PIN,RTA,RTPI,RTX,S,
     +                 SUM,T,T1,THIRD,TOL,TWOA,U,W,X0,Y,Z
      INTEGER I,IOP,M,MAX,N
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION ACC0(3),BIG(3),D0(13),D1(12),D2(10),D3(8),D4(6),
     +                 D5(4),D6(2),E00(3),WK(20),X00(3)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ERF,ERFC1,GAM1,GAMMA,REXP,RLOG,SPMPAR
      EXTERNAL ERF,ERFC1,GAM1,GAMMA,REXP,RLOG,SPMPAR
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DLOG,DMAX1,EXP,INT,SQRT
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C     --------------------
C     --------------------
C     ALOG10 = LN(10)
C     RT2PIN = 1/SQRT(2*PI)
C     RTPI   = SQRT(PI)
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
C     --------------------
      DATA ACC0(1)/5.D-15/,ACC0(2)/5.D-7/,ACC0(3)/5.D-4/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA BIG(1)/20.0D0/,BIG(2)/14.0D0/,BIG(3)/10.0D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA E00(1)/.25D-3/,E00(2)/.25D-1/,E00(3)/.14D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA X00(1)/31.0D0/,X00(2)/17.0D0/,X00(3)/9.7D0/
C*PT*WARNING* Constant already double-precision
      DATA ALOG10/2.30258509299405D0/
C*PT*WARNING* Constant already double-precision
      DATA RT2PIN/.398942280401433D0/
C*PT*WARNING* Constant already double-precision
      DATA RTPI/1.77245385090552D0/
C*PT*WARNING* Constant already double-precision
      DATA THIRD/.333333333333333D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA D0(1)/.833333333333333D-01/,D0(2)/-.148148148148148D-01/,
     +     D0(3)/.115740740740741D-02/,D0(4)/.352733686067019D-03/,
     +     D0(5)/-.178755144032922D-03/,D0(6)/.391926317852244D-04/,
     +     D0(7)/-.218544851067999D-05/,D0(8)/-.185406221071516D-05/,
     +     D0(9)/.829671134095309D-06/,D0(10)/-.176659527368261D-06/,
     +     D0(11)/.670785354340150D-08/,D0(12)/.102618097842403D-07/,
     +     D0(13)/-.438203601845335D-08/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA D10/-.185185185185185D-02/,D1(1)/-.347222222222222D-02/,
     +     D1(2)/.264550264550265D-02/,D1(3)/-.990226337448560D-03/,
     +     D1(4)/.205761316872428D-03/,D1(5)/-.401877572016461D-06/,
     +     D1(6)/-.180985503344900D-04/,D1(7)/.764916091608111D-05/,
     +     D1(8)/-.161209008945634D-05/,D1(9)/.464712780280743D-08/,
     +     D1(10)/.137863344691572D-06/,D1(11)/-.575254560351770D-07/,
     +     D1(12)/.119516285997781D-07/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA D20/.413359788359788D-02/,D2(1)/-.268132716049383D-02/,
     +     D2(2)/.771604938271605D-03/,D2(3)/.200938786008230D-05/,
     +     D2(4)/-.107366532263652D-03/,D2(5)/.529234488291201D-04/,
     +     D2(6)/-.127606351886187D-04/,D2(7)/.342357873409614D-07/,
     +     D2(8)/.137219573090629D-05/,D2(9)/-.629899213838006D-06/,
     +     D2(10)/.142806142060642D-06/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA D30/.649434156378601D-03/,D3(1)/.229472093621399D-03/,
     +     D3(2)/-.469189494395256D-03/,D3(3)/.267720632062839D-03/,
     +     D3(4)/-.756180167188398D-04/,D3(5)/-.239650511386730D-06/,
     +     D3(6)/.110826541153473D-04/,D3(7)/-.567495282699160D-05/,
     +     D3(8)/.142309007324359D-05/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA D40/-.861888290916712D-03/,D4(1)/.784039221720067D-03/,
     +     D4(2)/-.299072480303190D-03/,D4(3)/-.146384525788434D-05/,
     +     D4(4)/.664149821546512D-04/,D4(5)/-.396836504717943D-04/,
     +     D4(6)/.113757269706784D-04/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA D50/-.336798553366358D-03/,D5(1)/-.697281375836586D-04/,
     +     D5(2)/.277275324495939D-03/,D5(3)/-.199325705161888D-03/,
     +     D5(4)/.679778047793721D-04/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA D60/.531307936463992D-03/,D6(1)/-.592166437353694D-03/,
     +     D6(2)/.270878209671804D-03/
C*PT*WARNING* Constant already double-precision
      DATA D70/.344367606892378D-03/
C     ..
C     .. Executable Statements ..
C     --------------------
C     ****** E IS A MACHINE DEPENDENT CONSTANT. E IS THE SMALLEST
C            FLOATING POINT NUMBER FOR WHICH 1.0 + E .GT. 1.0 .
C
      E = SPMPAR(1)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C     --------------------
      IF (A.LT.0.0D0 .OR. X.LT.0.0D0) GO TO 430
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      IF (A.EQ.0.0D0 .AND. X.EQ.0.0D0) GO TO 430
C*PT*WARNING* Constant already double-precision
      IF (A*X.EQ.0.0D0) GO TO 420
C
      IOP = IND + 1
      IF (IOP.NE.1 .AND. IOP.NE.2) IOP = 3
      ACC = DMAX1(ACC0(IOP),E)
      E0 = E00(IOP)
      X0 = X00(IOP)
C*PT*WARNING* Constant already double-precision
C
C            SELECT THE APPROPRIATE ALGORITHM
C
      IF (A.GE.1.0D0) GO TO 10
C*PT*WARNING* Constant already double-precision
      IF (A.EQ.0.5D0) GO TO 390
C*PT*WARNING* Constant already double-precision
      IF (X.LT.1.1D0) GO TO 160
      T1 = A*DLOG(X) - X
      U = A*EXP(T1)
C*PT*WARNING* Constant already double-precision
      IF (U.EQ.0.0D0) GO TO 380
C*PT*WARNING* Constant already double-precision
      R = U* (1.0D0+GAM1(A))
      GO TO 250
C
   10 IF (A.GE.BIG(IOP)) GO TO 30
      IF (A.GT.X .OR. X.GE.X0) GO TO 20
      TWOA = A + A
      M = INT(TWOA)
C*PT*WARNING* Already double-precision (DBLE)
      IF (TWOA.NE.DBLE(M)) GO TO 20
      I = M/2
C*PT*WARNING* Already double-precision (DBLE)
      IF (A.EQ.DBLE(I)) GO TO 210
      GO TO 220

   20 T1 = A*DLOG(X) - X
      R = EXP(T1)/GAMMA(A)
      GO TO 40
C
   30 L = X/A
C*PT*WARNING* Constant already double-precision
      IF (L.EQ.0.0D0) GO TO 370
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      S = 0.5D0 + (0.5D0-L)
      Z = RLOG(L)
C*PT*WARNING* Constant already double-precision
      IF (Z.GE.700.0D0/A) GO TO 410
      Y = A*Z
      RTA = SQRT(A)
      IF (ABS(S).LE.E0/RTA) GO TO 330
C*PT*WARNING* Constant already double-precision
      IF (ABS(S).LE.0.4D0) GO TO 270
C*PT*WARNING* Constant already double-precision
C
      T = (1.0D0/A)**2
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      T1 = (((0.75D0*T-1.0D0)*T+3.5D0)*T-105.0D0)/ (A*1260.0D0)
      T1 = T1 - Y
      R = RT2PIN*RTA*EXP(T1)
C*PT*WARNING* Constant already double-precision
C
   40 IF (R.EQ.0.0D0) GO TO 420
      IF (X.LE.DMAX1(A,ALOG10)) GO TO 50
      IF (X.LT.X0) GO TO 250
      GO TO 100
C*PT*WARNING* Constant already double-precision
C
C                 TAYLOR SERIES FOR P/R
C
   50 APN = A + 1.0D0
      T = X/APN
      WK(1) = T
      DO 60 N = 2,20
C*PT*WARNING* Constant already double-precision
          APN = APN + 1.0D0
          T = T* (X/APN)
C*PT*WARNING* Constant already double-precision
          IF (T.LE.1.D-3) GO TO 70
          WK(N) = T
   60 CONTINUE
      N = 20
C
   70 SUM = T
C*PT*WARNING* Constant already double-precision
      TOL = 0.5D0*ACC
C*PT*WARNING* Constant already double-precision
   80 APN = APN + 1.0D0
      T = T* (X/APN)
      SUM = SUM + T
      IF (T.GT.TOL) GO TO 80
C
      MAX = N - 1
      DO 90 M = 1,MAX
          N = N - 1
          SUM = SUM + WK(N)
   90 CONTINUE
C*PT*WARNING* Constant already double-precision
      ANS = (R/A)* (1.0D0+SUM)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      QANS = 0.5D0 + (0.5D0-ANS)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C                 ASYMPTOTIC EXPANSION
C
  100 AMN = A - 1.0D0
      T = AMN/X
      WK(1) = T
      DO 110 N = 2,20
C*PT*WARNING* Constant already double-precision
          AMN = AMN - 1.0D0
          T = T* (AMN/X)
C*PT*WARNING* Constant already double-precision
          IF (ABS(T).LE.1.D-3) GO TO 120
          WK(N) = T
  110 CONTINUE
      N = 20
C
  120 SUM = T
  130 IF (ABS(T).LE.ACC) GO TO 140
C*PT*WARNING* Constant already double-precision
      AMN = AMN - 1.0D0
      T = T* (AMN/X)
      SUM = SUM + T
      GO TO 130
C
  140 MAX = N - 1
      DO 150 M = 1,MAX
          N = N - 1
          SUM = SUM + WK(N)
  150 CONTINUE
C*PT*WARNING* Constant already double-precision
      QANS = (R/X)* (1.0D0+SUM)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ANS = 0.5D0 + (0.5D0-QANS)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C             TAYLOR SERIES FOR P(A,X)/X**A
C
  160 AN = 3.0D0
      C = X
C*PT*WARNING* Constant already double-precision
      SUM = X/ (A+3.0D0)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      TOL = 3.0D0*ACC/ (A+1.0D0)
C*PT*WARNING* Constant already double-precision
  170 AN = AN + 1.0D0
      C = -C* (X/AN)
      T = C/ (A+AN)
      SUM = SUM + T
      IF (ABS(T).GT.TOL) GO TO 170
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      J = A*X* ((SUM/6.0D0-0.5D0/ (A+2.0D0))*X+1.0D0/ (A+1.0D0))
C
      Z = A*DLOG(X)
      H = GAM1(A)
C*PT*WARNING* Constant already double-precision
      G = 1.0D0 + H
C*PT*WARNING* Constant already double-precision
      IF (X.LT.0.25D0) GO TO 180
C*PT*WARNING* Constant already double-precision
      IF (A.LT.X/2.59D0) GO TO 200
      GO TO 190
C*PT*WARNING* Constant already double-precision

  180 IF (Z.GT.-.13394D0) GO TO 200
C
  190 W = EXP(Z)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ANS = W*G* (0.5D0+ (0.5D0-J))
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      QANS = 0.5D0 + (0.5D0-ANS)
      RETURN
C
  200 L = REXP(Z)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W = 0.5D0 + (0.5D0+L)
      QANS = (W*J-L)*G - H
C*PT*WARNING* Constant already double-precision
      IF (QANS.LT.0.0D0) GO TO 380
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ANS = 0.5D0 + (0.5D0-QANS)
      RETURN
C
C             FINITE SUMS FOR Q WHEN A .GE. 1
C                 AND 2*A IS AN INTEGER
C
  210 SUM = EXP(-X)
      T = SUM
      N = 1
C*PT*WARNING* Constant already double-precision
      C = 0.0D0
      GO TO 230
C
  220 RTX = SQRT(X)
      SUM = ERFC1(0,RTX)
      T = EXP(-X)/ (RTPI*RTX)
      N = 0
C*PT*WARNING* Constant already double-precision
      C = -0.5D0
C
  230 IF (N.EQ.I) GO TO 240
      N = N + 1
C*PT*WARNING* Constant already double-precision
      C = C + 1.0D0
      T = (X*T)/C
      SUM = SUM + T
      GO TO 230

  240 QANS = SUM
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ANS = 0.5D0 + (0.5D0-QANS)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C              CONTINUED FRACTION EXPANSION
C
  250 TOL = DMAX1(5.0D0*E,ACC)
C*PT*WARNING* Constant already double-precision
      A2NM1 = 1.0D0
C*PT*WARNING* Constant already double-precision
      A2N = 1.0D0
      B2NM1 = X
C*PT*WARNING* Constant already double-precision
      B2N = X + (1.0D0-A)
C*PT*WARNING* Constant already double-precision
      C = 1.0D0
  260 A2NM1 = X*A2N + C*A2NM1
      B2NM1 = X*B2N + C*B2NM1
      AM0 = A2NM1/B2NM1
C*PT*WARNING* Constant already double-precision
      C = C + 1.0D0
      CMA = C - A
      A2N = A2NM1 + CMA*A2N
      B2N = B2NM1 + CMA*B2N
      AN0 = A2N/B2N
      IF (ABS(AN0-AM0).GE.TOL*AN0) GO TO 260
C
      QANS = R*AN0
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ANS = 0.5D0 + (0.5D0-QANS)
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C                GENERAL TEMME EXPANSION
C
  270 IF (ABS(S).LE.2.0D0*E .AND. A*E*E.GT.3.28D-3) GO TO 430
      C = EXP(-Y)
C*PT*WARNING* Constant already double-precision
      W = 0.5D0*ERFC1(1,SQRT(Y))
C*PT*WARNING* Constant already double-precision
      U = 1.0D0/A
      Z = SQRT(Z+Z)
C*PT*WARNING* Constant already double-precision
      IF (L.LT.1.0D0) Z = -Z
      IF (IOP-2) 280,290,300
C*PT*WARNING* Constant already double-precision
C
  280 IF (ABS(S).LE.1.D-3) GO TO 340
      C0 = ((((((((((((D0(13)*Z+D0(12))*Z+D0(11))*Z+D0(10))*Z+D0(9))*Z+
     +     D0(8))*Z+D0(7))*Z+D0(6))*Z+D0(5))*Z+D0(4))*Z+D0(3))*Z+D0(2))*
     +     Z+D0(1))*Z - THIRD
      C1 = (((((((((((D1(12)*Z+D1(11))*Z+D1(10))*Z+D1(9))*Z+D1(8))*Z+
     +     D1(7))*Z+D1(6))*Z+D1(5))*Z+D1(4))*Z+D1(3))*Z+D1(2))*Z+D1(1))*
     +     Z + D10
      C2 = (((((((((D2(10)*Z+D2(9))*Z+D2(8))*Z+D2(7))*Z+D2(6))*Z+
     +     D2(5))*Z+D2(4))*Z+D2(3))*Z+D2(2))*Z+D2(1))*Z + D20
      C3 = (((((((D3(8)*Z+D3(7))*Z+D3(6))*Z+D3(5))*Z+D3(4))*Z+D3(3))*Z+
     +     D3(2))*Z+D3(1))*Z + D30
      C4 = (((((D4(6)*Z+D4(5))*Z+D4(4))*Z+D4(3))*Z+D4(2))*Z+D4(1))*Z +
     +     D40
      C5 = (((D5(4)*Z+D5(3))*Z+D5(2))*Z+D5(1))*Z + D50
      C6 = (D6(2)*Z+D6(1))*Z + D60
      T = ((((((D70*U+C6)*U+C5)*U+C4)*U+C3)*U+C2)*U+C1)*U + C0
      GO TO 310
C
  290 C0 = (((((D0(6)*Z+D0(5))*Z+D0(4))*Z+D0(3))*Z+D0(2))*Z+D0(1))*Z -
     +     THIRD
      C1 = (((D1(4)*Z+D1(3))*Z+D1(2))*Z+D1(1))*Z + D10
      C2 = D2(1)*Z + D20
      T = (C2*U+C1)*U + C0
      GO TO 310
C
  300 T = ((D0(3)*Z+D0(2))*Z+D0(1))*Z - THIRD
C*PT*WARNING* Constant already double-precision
C
  310 IF (L.LT.1.0D0) GO TO 320
      QANS = C* (W+RT2PIN*T/RTA)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ANS = 0.5D0 + (0.5D0-QANS)
      RETURN

  320 ANS = C* (W-RT2PIN*T/RTA)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      QANS = 0.5D0 + (0.5D0-ANS)
      RETURN
C*PT*WARNING* Constant already double-precision
C
C               TEMME EXPANSION FOR L = 1
C
  330 IF (A*E*E.GT.3.28D-3) GO TO 430
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      C = 0.5D0 + (0.5D0-Y)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      W = (0.5D0-SQRT(Y)* (0.5D0+ (0.5D0-Y/3.0D0))/RTPI)/C
C*PT*WARNING* Constant already double-precision
      U = 1.0D0/A
      Z = SQRT(Z+Z)
C*PT*WARNING* Constant already double-precision
      IF (L.LT.1.0D0) Z = -Z
      IF (IOP-2) 340,350,360
C
  340 C0 = ((((((D0(7)*Z+D0(6))*Z+D0(5))*Z+D0(4))*Z+D0(3))*Z+D0(2))*Z+
     +     D0(1))*Z - THIRD
      C1 = (((((D1(6)*Z+D1(5))*Z+D1(4))*Z+D1(3))*Z+D1(2))*Z+D1(1))*Z +
     +     D10
      C2 = ((((D2(5)*Z+D2(4))*Z+D2(3))*Z+D2(2))*Z+D2(1))*Z + D20
      C3 = (((D3(4)*Z+D3(3))*Z+D3(2))*Z+D3(1))*Z + D30
      C4 = (D4(2)*Z+D4(1))*Z + D40
      C5 = (D5(2)*Z+D5(1))*Z + D50
      C6 = D6(1)*Z + D60
      T = ((((((D70*U+C6)*U+C5)*U+C4)*U+C3)*U+C2)*U+C1)*U + C0
      GO TO 310
C
  350 C0 = (D0(2)*Z+D0(1))*Z - THIRD
      C1 = D1(1)*Z + D10
      T = (D20*U+C1)*U + C0
      GO TO 310
C
  360 T = D0(1)*Z - THIRD
      GO TO 310
C*PT*WARNING* Constant already double-precision
C
C                     SPECIAL CASES
C
  370 ANS = 0.0D0
C*PT*WARNING* Constant already double-precision
      QANS = 1.0D0
      RETURN
C*PT*WARNING* Constant already double-precision
C
  380 ANS = 1.0D0
C*PT*WARNING* Constant already double-precision
      QANS = 0.0D0
      RETURN
C*PT*WARNING* Constant already double-precision
C
  390 IF (X.GE.0.25D0) GO TO 400
      ANS = ERF(SQRT(X))
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      QANS = 0.5D0 + (0.5D0-ANS)
      RETURN

  400 QANS = ERFC1(0,SQRT(X))
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      ANS = 0.5D0 + (0.5D0-QANS)
      RETURN
C*PT*WARNING* Constant already double-precision
C
  410 IF (ABS(S).LE.2.0D0*E) GO TO 430
  420 IF (X.LE.A) GO TO 370
      GO TO 380
C*PT*WARNING* Constant already double-precision
C
C                     ERROR RETURN
C
  430 ANS = 2.0D0
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION GSUMLN(A,B)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C          EVALUATION OF THE FUNCTION LN(GAMMA(A + B))
C          FOR 1 .LE. A .LE. 2  AND  1 .LE. B .LE. 2
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,B
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION ALNREL,GAMLN1
      EXTERNAL ALNREL,GAMLN1
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE,DLOG
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
      X = DBLE(A) + DBLE(B) - 2.D0
C*PT*WARNING* Constant already double-precision
      IF (X.GT.0.25D0) GO TO 10
C*PT*WARNING* Constant already double-precision
      GSUMLN = GAMLN1(1.0D0+X)
      RETURN
C*PT*WARNING* Constant already double-precision

   10 IF (X.GT.1.25D0) GO TO 20
      GSUMLN = GAMLN1(X) + ALNREL(X)
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision

   20 GSUMLN = GAMLN1(X-1.0D0) + DLOG(X* (1.0D0+X))
      RETURN

      END
      INTEGER FUNCTION IPMPAR(I)
C-----------------------------------------------------------------------
C
C     IPMPAR PROVIDES THE INTEGER MACHINE CONSTANTS FOR THE COMPUTER
C     THAT IS USED. IT IS ASSUMED THAT THE ARGUMENT I IS AN INTEGER
C     HAVING ONE OF THE VALUES 1-10. IPMPAR(I) HAS THE VALUE ...
C
C  INTEGERS.
C
C     ASSUME INTEGERS ARE REPRESENTED IN THE N-DIGIT, BASE-A FORM
C
C               SIGN ( X(N-1)*A**(N-1) + ... + X(1)*A + X(0) )
C
C               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,N-1.
C
C     IPMPAR(1) = A, THE BASE.
C
C     IPMPAR(2) = N, THE NUMBER OF BASE-A DIGITS.
C
C     IPMPAR(3) = A**N - 1, THE LARGEST MAGNITUDE.
C
C  FLOATING-POINT NUMBERS.
C
C     IT IS ASSUMED THAT THE SINGLE AND DOUBLE PRECISION FLOATING
C     POINT ARITHMETICS HAVE THE SAME BASE, SAY B, AND THAT THE
C     NONZERO NUMBERS ARE REPRESENTED IN THE FORM
C
C               SIGN (B**E) * (X(1)/B + ... + X(M)/B**M)
C
C               WHERE X(I) = 0,1,...,B-1 FOR I=1,...,M,
C               X(1) .GE. 1, AND EMIN .LE. E .LE. EMAX.
C
C     IPMPAR(4) = B, THE BASE.
C
C  SINGLE-PRECISION
C
C     IPMPAR(5) = M, THE NUMBER OF BASE-B DIGITS.
C
C     IPMPAR(6) = EMIN, THE SMALLEST EXPONENT E.
C
C     IPMPAR(7) = EMAX, THE LARGEST EXPONENT E.
C
C  DOUBLE-PRECISION
C
C     IPMPAR(8) = M, THE NUMBER OF BASE-B DIGITS.
C
C     IPMPAR(9) = EMIN, THE SMALLEST EXPONENT E.
C
C     IPMPAR(10) = EMAX, THE LARGEST EXPONENT E.
C
C-----------------------------------------------------------------------
C
C     TO DEFINE THIS FUNCTION FOR THE COMPUTER BEING USED, ACTIVATE
C     THE DATA STATMENTS FOR THE COMPUTER BY REMOVING THE C FROM
C     COLUMN 1. (ALL THE OTHER DATA STATEMENTS SHOULD HAVE C IN
C     COLUMN 1.)
C
C-----------------------------------------------------------------------
C
C     IPMPAR IS AN ADAPTATION OF THE FUNCTION I1MACH, WRITTEN BY
C     P.A. FOX, A.D. HALL, AND N.L. SCHRYER (BELL LABORATORIES).
C     IPMPAR WAS FORMED BY A.H. MORRIS (NSWC). THE CONSTANTS ARE
C     FROM BELL LABORATORIES, NSWC, AND OTHER SOURCES.
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER I
C     ..
C     .. Local Arrays ..
      INTEGER IMACH(10)
C     ..
C     .. Data statements ..
C
C     MACHINE CONSTANTS FOR AMDAHL MACHINES.
C
C     DATA IMACH( 1) /   2 /
C     DATA IMACH( 2) /  31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /  16 /
C     DATA IMACH( 5) /   6 /
C     DATA IMACH( 6) / -64 /
C     DATA IMACH( 7) /  63 /
C     DATA IMACH( 8) /  14 /
C     DATA IMACH( 9) / -64 /
C     DATA IMACH(10) /  63 /
C
C     MACHINE CONSTANTS FOR THE AT&T 3B SERIES, AT&T
C     PC 7300, AND AT&T 6300.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -125 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   33 /
C     DATA IMACH( 3) / 8589934591 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   24 /
C     DATA IMACH( 6) / -256 /
C     DATA IMACH( 7) /  255 /
C     DATA IMACH( 8) /   60 /
C     DATA IMACH( 9) / -256 /
C     DATA IMACH(10) /  255 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   39 /
C     DATA IMACH( 3) / 549755813887 /
C     DATA IMACH( 4) /    8 /
C     DATA IMACH( 5) /   13 /
C     DATA IMACH( 6) /  -50 /
C     DATA IMACH( 7) /   76 /
C     DATA IMACH( 8) /   26 /
C     DATA IMACH( 9) /  -50 /
C     DATA IMACH(10) /   76 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
C
C     DATA IMACH( 1) /      2 /
C     DATA IMACH( 2) /     39 /
C     DATA IMACH( 3) / 549755813887 /
C     DATA IMACH( 4) /      8 /
C     DATA IMACH( 5) /     13 /
C     DATA IMACH( 6) /    -50 /
C     DATA IMACH( 7) /     76 /
C     DATA IMACH( 8) /     26 /
C     DATA IMACH( 9) / -32754 /
C     DATA IMACH(10) /  32780 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
C     60 BIT ARITHMETIC, AND THE CDC CYBER 995 64 BIT
C     ARITHMETIC (NOS OPERATING SYSTEM).
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   48 /
C     DATA IMACH( 3) / 281474976710655 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   48 /
C     DATA IMACH( 6) / -974 /
C     DATA IMACH( 7) / 1070 /
C     DATA IMACH( 8) /   95 /
C     DATA IMACH( 9) / -926 /
C     DATA IMACH(10) / 1070 /
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 995 64 BIT
C     ARITHMETIC (NOS/VE OPERATING SYSTEM).
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    63 /
C     DATA IMACH( 3) / 9223372036854775807 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    48 /
C     DATA IMACH( 6) / -4096 /
C     DATA IMACH( 7) /  4095 /
C     DATA IMACH( 8) /    96 /
C     DATA IMACH( 9) / -4096 /
C     DATA IMACH(10) /  4095 /
C
C     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    63 /
C     DATA IMACH( 3) / 9223372036854775807 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    47 /
C     DATA IMACH( 6) / -8189 /
C     DATA IMACH( 7) /  8190 /
C     DATA IMACH( 8) /    94 /
C     DATA IMACH( 9) / -8099 /
C     DATA IMACH(10) /  8190 /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   15 /
C     DATA IMACH( 3) / 32767 /
C     DATA IMACH( 4) /   16 /
C     DATA IMACH( 5) /    6 /
C     DATA IMACH( 6) /  -64 /
C     DATA IMACH( 7) /   63 /
C     DATA IMACH( 8) /   14 /
C     DATA IMACH( 9) /  -64 /
C     DATA IMACH(10) /   63 /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   23 /
C     DATA IMACH( 3) / 8388607 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   23 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   38 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000
C     AND DPS 8/70 SERIES.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   35 /
C     DATA IMACH( 3) / 34359738367 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   27 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   63 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   15 /
C     DATA IMACH( 3) / 32767 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   23 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   39 /
C     DATA IMACH( 9) / -128 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   15 /
C     DATA IMACH( 3) / 32767 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   23 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   55 /
C     DATA IMACH( 9) / -128 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 9000.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -126 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE ICL 2900, THE ITEL AS/6, THE XEROX SIGMA
C     5/7/9 AND THE SEL SYSTEMS 85/86.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /   16 /
C     DATA IMACH( 5) /    6 /
C     DATA IMACH( 6) /  -64 /
C     DATA IMACH( 7) /   63 /
C     DATA IMACH( 8) /   14 /
C     DATA IMACH( 9) /  -64 /
C     DATA IMACH(10) /   63 /
C
C     MACHINE CONSTANTS FOR THE IBM PC.
C
C      DATA imach(1)/2/
C      DATA imach(2)/31/
C      DATA imach(3)/2147483647/
C      DATA imach(4)/2/
C      DATA imach(5)/24/
C      DATA imach(6)/-125/
C      DATA imach(7)/128/
C      DATA imach(8)/53/
C      DATA imach(9)/-1021/
C      DATA imach(10)/1024/
C
C     MACHINE CONSTANTS FOR THE MACINTOSH II - ABSOFT
C     MACFORTRAN II.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -125 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR THE MICROVAX - VMS FORTRAN.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   24 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   56 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   35 /
C     DATA IMACH( 3) / 34359738367 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   27 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   54 /
C     DATA IMACH( 9) / -101 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   35 /
C     DATA IMACH( 3) / 34359738367 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   27 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   62 /
C     DATA IMACH( 9) / -128 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   24 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   56 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -125 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR THE SILICON GRAPHICS IRIS-4D
C     SERIES (MIPS R3000 PROCESSOR).
C
C     DATA IMACH( 1) /     2 /
C     DATA IMACH( 2) /    31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /     2 /
C     DATA IMACH( 5) /    24 /
C     DATA IMACH( 6) /  -125 /
C     DATA IMACH( 7) /   128 /
C     DATA IMACH( 8) /    53 /
C     DATA IMACH( 9) / -1021 /
C     DATA IMACH(10) /  1024 /
C
C     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
C     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
C     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
C
      DATA IMACH(1)/2/
      DATA IMACH(2)/31/
      DATA IMACH(3)/2147483647/
      DATA IMACH(4)/2/
      DATA IMACH(5)/24/
      DATA IMACH(6)/-125/
      DATA IMACH(7)/128/
      DATA IMACH(8)/53/
      DATA IMACH(9)/-1021/
      DATA IMACH(10)/1024/
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   35 /
C     DATA IMACH( 3) / 34359738367 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   27 /
C     DATA IMACH( 6) / -128 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   60 /
C     DATA IMACH( 9) /-1024 /
C     DATA IMACH(10) / 1023 /
C
C     MACHINE CONSTANTS FOR THE VAX 11/780.
C
C     DATA IMACH( 1) /    2 /
C     DATA IMACH( 2) /   31 /
C     DATA IMACH( 3) / 2147483647 /
C     DATA IMACH( 4) /    2 /
C     DATA IMACH( 5) /   24 /
C     DATA IMACH( 6) / -127 /
C     DATA IMACH( 7) /  127 /
C     DATA IMACH( 8) /   56 /
C     DATA IMACH( 9) / -127 /
C     DATA IMACH(10) /  127 /
C
      IPMPAR = IMACH(I)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION PSI(XX)
C*PT*WARNING* Already double-precision
C---------------------------------------------------------------------
C
C                 EVALUATION OF THE DIGAMMA FUNCTION
C
C                           -----------
C
C     PSI(XX) IS ASSIGNED THE VALUE 0 WHEN THE DIGAMMA FUNCTION CANNOT
C     BE COMPUTED.
C
C     THE MAIN COMPUTATION INVOLVES EVALUATION OF RATIONAL CHEBYSHEV
C     APPROXIMATIONS PUBLISHED IN MATH. COMP. 27, 123-127(1973) BY
C     CODY, STRECOK AND THACHER.
C
C---------------------------------------------------------------------
C     PSI WAS WRITTEN AT ARGONNE NATIONAL LABORATORY FOR THE FUNPACK
C     PACKAGE OF SPECIAL FUNCTION SUBROUTINES. PSI WAS MODIFIED BY
C     A.H. MORRIS (NSWC).
C---------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION XX
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION AUG,DEN,DX0,PIOV4,SGN,UPPER,W,X,XMAX1,XMX0,
     +                 XSMALL,Z
      INTEGER I,M,N,NQ
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION P1(7),P2(4),Q1(6),Q2(4)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION SPMPAR
      INTEGER IPMPAR
      EXTERNAL SPMPAR,IPMPAR
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,COS,DBLE,DLOG,DMIN1,INT,SIN
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C---------------------------------------------------------------------
C
C     PIOV4 = PI/4
C     DX0 = ZERO OF PSI TO EXTENDED PRECISION
C
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C
C     COEFFICIENTS FOR RATIONAL APPROXIMATION OF
C     PSI(X) / (X - X0),  0.5 .LE. X .LE. 3.0
C
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C
C     COEFFICIENTS FOR RATIONAL APPROXIMATION OF
C     PSI(X) - LN(X) + 1 / (2*X),  X .GT. 3.0
C
C---------------------------------------------------------------------
      DATA PIOV4/.785398163397448D0/
C*PT*WARNING* Constant already double-precision
      DATA DX0/1.461632144968362341262659542325721325D0/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA P1(1)/.895385022981970D-02/,P1(2)/.477762828042627D+01/,
     +     P1(3)/.142441585084029D+03/,P1(4)/.118645200713425D+04/,
     +     P1(5)/.363351846806499D+04/,P1(6)/.413810161269013D+04/,
     +     P1(7)/.130560269827897D+04/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q1(1)/.448452573429826D+02/,Q1(2)/.520752771467162D+03/,
     +     Q1(3)/.221000799247830D+04/,Q1(4)/.364127349079381D+04/,
     +     Q1(5)/.190831076596300D+04/,Q1(6)/.691091682714533D-05/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA P2(1)/-.212940445131011D+01/,P2(2)/-.701677227766759D+01/,
     +     P2(3)/-.448616543918019D+01/,P2(4)/-.648157123766197D+00/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q2(1)/.322703493791143D+02/,Q2(2)/.892920700481861D+02/,
     +     Q2(3)/.546117738103215D+02/,Q2(4)/.777788548522962D+01/
C     ..
C     .. Executable Statements ..
C---------------------------------------------------------------------
C
C     MACHINE DEPENDENT CONSTANTS ...
C
C        XMAX1  = THE SMALLEST POSITIVE FLOATING POINT CONSTANT
C                 WITH ENTIRELY INTEGER REPRESENTATION.  ALSO USED
C                 AS NEGATIVE OF LOWER BOUND ON ACCEPTABLE NEGATIVE
C                 ARGUMENTS AND AS THE POSITIVE ARGUMENT BEYOND WHICH
C                 PSI MAY BE REPRESENTED AS ALOG(X).
C
C        XSMALL = ABSOLUTE ARGUMENT BELOW WHICH PI*COTAN(PI*X)
C                 MAY BE REPRESENTED BY 1/X.
C
C---------------------------------------------------------------------
      XMAX1 = IPMPAR(3)
C*PT*WARNING* Constant already double-precision
      XMAX1 = DMIN1(XMAX1,1.0D0/SPMPAR(1))
C*PT*WARNING* Constant already double-precision
      XSMALL = 1.D-9
C---------------------------------------------------------------------
      X = XX
C*PT*WARNING* Constant already double-precision
      AUG = 0.0D0
C*PT*WARNING* Constant already double-precision
      IF (X.GE.0.5D0) GO TO 50
C---------------------------------------------------------------------
C     X .LT. 0.5,  USE REFLECTION FORMULA
C     PSI(1-X) = PSI(X) + PI * COTAN(PI*X)
C---------------------------------------------------------------------
      IF (ABS(X).GT.XSMALL) GO TO 10
C*PT*WARNING* Constant already double-precision
      IF (X.EQ.0.0D0) GO TO 100
C*PT*WARNING* Constant already double-precision
C---------------------------------------------------------------------
C     0 .LT. ABS(X) .LE. XSMALL.  USE 1/X AS A SUBSTITUTE
C     FOR  PI*COTAN(PI*X)
C---------------------------------------------------------------------
      AUG = -1.0D0/X
      GO TO 40
C---------------------------------------------------------------------
C     REDUCTION OF ARGUMENT FOR COTAN
C---------------------------------------------------------------------
   10 W = -X
      SGN = PIOV4
C*PT*WARNING* Constant already double-precision
      IF (W.GT.0.0D0) GO TO 20
      W = -W
      SGN = -SGN
C---------------------------------------------------------------------
C     MAKE AN ERROR EXIT IF X .LE. -XMAX1
C---------------------------------------------------------------------
   20 IF (W.GE.XMAX1) GO TO 100
      NQ = INT(W)
C*PT*WARNING* Already double-precision (DBLE)
      W = W - DBLE(NQ)
C*PT*WARNING* Constant already double-precision
      NQ = INT(W*4.0D0)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision
      W = 4.0D0* (W-DBLE(NQ)*.25D0)
C---------------------------------------------------------------------
C     W IS NOW RELATED TO THE FRACTIONAL PART OF  4.0 * X.
C     ADJUST ARGUMENT TO CORRESPOND TO VALUES IN FIRST
C     QUADRANT AND DETERMINE SIGN
C---------------------------------------------------------------------
      N = NQ/2
C*PT*WARNING* Constant already double-precision
      IF ((N+N).NE.NQ) W = 1.0D0 - W
      Z = PIOV4*W
      M = N/2
      IF ((M+M).NE.N) SGN = -SGN
C---------------------------------------------------------------------
C     DETERMINE FINAL VALUE FOR  -PI*COTAN(PI*X)
C---------------------------------------------------------------------
      N = (NQ+1)/2
      M = N/2
      M = M + M
      IF (M.NE.N) GO TO 30
C*PT*WARNING* Constant already double-precision
C---------------------------------------------------------------------
C     CHECK FOR SINGULARITY
C---------------------------------------------------------------------
      IF (Z.EQ.0.0D0) GO TO 100
C*PT*WARNING* Constant already double-precision
C---------------------------------------------------------------------
C     USE COS/SIN AS A SUBSTITUTE FOR COTAN, AND
C     SIN/COS AS A SUBSTITUTE FOR TAN
C---------------------------------------------------------------------
      AUG = SGN* ((COS(Z)/SIN(Z))*4.0D0)
      GO TO 40
C*PT*WARNING* Constant already double-precision

   30 AUG = SGN* ((SIN(Z)/COS(Z))*4.0D0)
C*PT*WARNING* Constant already double-precision
   40 X = 1.0D0 - X
C*PT*WARNING* Constant already double-precision
   50 IF (X.GT.3.0D0) GO TO 70
C---------------------------------------------------------------------
C     0.5 .LE. X .LE. 3.0
C---------------------------------------------------------------------
      DEN = X
      UPPER = P1(1)*X
C
      DO 60 I = 1,5
          DEN = (DEN+Q1(I))*X
          UPPER = (UPPER+P1(I+1))*X
   60 CONTINUE
C
      DEN = (UPPER+P1(7))/ (DEN+Q1(6))
C*PT*WARNING* Already double-precision (DBLE)
      XMX0 = DBLE(X) - DX0
      PSI = DEN*XMX0 + AUG
      RETURN
C---------------------------------------------------------------------
C     IF X .GE. XMAX1, PSI = LN(X)
C---------------------------------------------------------------------
   70 IF (X.GE.XMAX1) GO TO 90
C*PT*WARNING* Constant already double-precision
C---------------------------------------------------------------------
C     3.0 .LT. X .LT. XMAX1
C---------------------------------------------------------------------
      W = 1.0D0/ (X*X)
      DEN = W
      UPPER = P2(1)*W
C
      DO 80 I = 1,3
          DEN = (DEN+Q2(I))*W
          UPPER = (UPPER+P2(I+1))*W
   80 CONTINUE
C*PT*WARNING* Constant already double-precision
C
      AUG = UPPER/ (DEN+Q2(4)) - 0.5D0/X + AUG
   90 PSI = AUG + DLOG(X)
      RETURN
C*PT*WARNING* Constant already double-precision
C---------------------------------------------------------------------
C     ERROR RETURN
C---------------------------------------------------------------------
  100 PSI = 0.0D0
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION RCOMP(A,X)
C*PT*WARNING* Already double-precision
C     -------------------
C     EVALUATION OF EXP(-X)*X**A/GAMMA(A)
C     -------------------
C     RT2PIN = 1/SQRT(2*PI)
C     -------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION RT2PIN,T,T1,U
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION GAM1,GAMMA,RLOG
      EXTERNAL GAM1,GAMMA,RLOG
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DLOG,EXP,SQRT
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA RT2PIN/.398942280401433D0/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C     -------------------
      RCOMP = 0.0D0
C*PT*WARNING* Constant already double-precision
      IF (A.GE.20.0D0) GO TO 20
      T = A*DLOG(X) - X
C*PT*WARNING* Constant already double-precision
      IF (A.GE.1.0D0) GO TO 10
C*PT*WARNING* Constant already double-precision
      RCOMP = (A*EXP(T))* (1.0D0+GAM1(A))
      RETURN

   10 RCOMP = EXP(T)/GAMMA(A)
      RETURN
C
   20 U = X/A
C*PT*WARNING* Constant already double-precision
      IF (U.EQ.0.0D0) RETURN
C*PT*WARNING* Constant already double-precision
      T = (1.0D0/A)**2
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      T1 = (((0.75D0*T-1.0D0)*T+3.5D0)*T-105.0D0)/ (A*1260.0D0)
      T1 = T1 - A*RLOG(U)
      RCOMP = RT2PIN*SQRT(A)*EXP(T1)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION REXP(X)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C            EVALUATION OF THE FUNCTION EXP(X) - 1
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,W
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,EXP
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA P1/.914041914819518D-09/,P2/.238082361044469D-01/,
     +     Q1/-.499999999085958D+00/,Q2/.107141568980644D+00/,
     +     Q3/-.119041179760821D-01/,Q4/.595130811860248D-03/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C-----------------------
      IF (ABS(X).GT.0.15D0) GO TO 10
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      REXP = X* (((P2*X+P1)*X+1.0D0)/ ((((Q4*X+Q3)*X+Q2)*X+Q1)*X+1.0D0))
      RETURN
C
   10 W = EXP(X)
C*PT*WARNING* Constant already double-precision
      IF (X.GT.0.0D0) GO TO 20
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      REXP = (W-0.5D0) - 0.5D0
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision

   20 REXP = W* (0.5D0+ (0.5D0-1.0D0/W))
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION RLOG(X)
C*PT*WARNING* Already double-precision
C     -------------------
C     COMPUTATION OF  X - 1 - LN(X)
C     -------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,P0,P1,P2,Q1,Q2,R,T,U,W,W1
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE,DLOG
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C     -------------------
      DATA A/.566749439387324D-01/
C*PT*WARNING* Constant already double-precision
      DATA B/.456512608815524D-01/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA P0/.333333333333333D+00/,P1/-.224696413112536D+00/,
     +     P2/.620886815375787D-02/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q1/-.127408923933623D+01/,Q2/.354508718369557D+00/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C     -------------------
      IF (X.LT.0.61D0 .OR. X.GT.1.57D0) GO TO 40
C*PT*WARNING* Constant already double-precision
      IF (X.LT.0.82D0) GO TO 10
C*PT*WARNING* Constant already double-precision
      IF (X.GT.1.18D0) GO TO 20
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C              ARGUMENT REDUCTION
C
      U = (X-0.5D0) - 0.5D0
C*PT*WARNING* Constant already double-precision
      W1 = 0.0D0
      GO TO 30
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision
C
   10 U = DBLE(X) - 0.7D0
C*PT*WARNING* Constant already double-precision
      U = U/0.7D0
C*PT*WARNING* Constant already double-precision
      W1 = A - U*0.3D0
      GO TO 30
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision
C
   20 U = 0.75D0*DBLE(X) - 1.D0
C*PT*WARNING* Constant already double-precision
      W1 = B + U/3.0D0
C*PT*WARNING* Constant already double-precision
C
C               SERIES EXPANSION
C
   30 R = U/ (U+2.0D0)
      T = R*R
C*PT*WARNING* Constant already double-precision
      W = ((P2*T+P1)*T+P0)/ ((Q2*T+Q1)*T+1.0D0)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      RLOG = 2.0D0*T* (1.0D0/ (1.0D0-R)-R*W) + W1
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C
   40 R = (X-0.5D0) - 0.5D0
      RLOG = R - DLOG(X)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION RLOG1(X)
C*PT*WARNING* Already double-precision
C-----------------------------------------------------------------------
C             EVALUATION OF THE FUNCTION X - LN(1 + X)
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,B,H,P0,P1,P2,Q1,Q2,R,T,W,W1
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE,DLOG
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
C------------------------
      DATA A/.566749439387324D-01/
C*PT*WARNING* Constant already double-precision
      DATA B/.456512608815524D-01/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA P0/.333333333333333D+00/,P1/-.224696413112536D+00/,
     +     P2/.620886815375787D-02/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA Q1/-.127408923933623D+01/,Q2/.354508718369557D+00/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
C------------------------
      IF (X.LT.-0.39D0 .OR. X.GT.0.57D0) GO TO 40
C*PT*WARNING* Constant already double-precision
      IF (X.LT.-0.18D0) GO TO 10
C*PT*WARNING* Constant already double-precision
      IF (X.GT.0.18D0) GO TO 20
C
C              ARGUMENT REDUCTION
C
      H = X
C*PT*WARNING* Constant already double-precision
      W1 = 0.0D0
      GO TO 30
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision
C
   10 H = DBLE(X) + 0.3D0
C*PT*WARNING* Constant already double-precision
      H = H/0.7D0
C*PT*WARNING* Constant already double-precision
      W1 = A - H*0.3D0
      GO TO 30
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Already double-precision (DBLE)
C*PT*WARNING* Constant already double-precision
C
   20 H = 0.75D0*DBLE(X) - 0.25D0
C*PT*WARNING* Constant already double-precision
      W1 = B + H/3.0D0
C*PT*WARNING* Constant already double-precision
C
C               SERIES EXPANSION
C
   30 R = H/ (H+2.0D0)
      T = R*R
C*PT*WARNING* Constant already double-precision
      W = ((P2*T+P1)*T+P0)/ ((Q2*T+Q1)*T+1.0D0)
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      RLOG1 = 2.0D0*T* (1.0D0/ (1.0D0-R)-R*W) + W1
      RETURN
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C
C
   40 W = (X+0.5D0) + 0.5D0
      RLOG1 = X - DLOG(W)
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION SPMPAR(I)
C-----------------------------------------------------------------------
C
C     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
C     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
C     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
C     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
C     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN
C
C        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,
C
C        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,
C
C        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
C
C-----------------------------------------------------------------------
C     WRITTEN BY
C        ALFRED H. MORRIS, JR.
C        NAVAL SURFACE WARFARE CENTER
C        DAHLGREN VIRGINIA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     MODIFIED BY BARRY W. BROWN TO RETURN DOUBLE PRECISION MACHINE
C     CONSTANTS FOR THE COMPUTER BEING USED.  THIS MODIFICATION WAS
C     MADE AS PART OF CONVERTING BRATIO TO DOUBLE PRECISION
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER I
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION B,BINV,BM1,ONE,W,Z
      INTEGER EMAX,EMIN,IBETA,M
C     ..
C     .. External Functions ..
      INTEGER IPMPAR
      EXTERNAL IPMPAR
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE
C     ..
C     .. Executable Statements ..
C
      IF (I.GT.1) GO TO 10
      B = IPMPAR(4)
      M = IPMPAR(8)
      SPMPAR = B** (1-M)
      RETURN
C
   10 IF (I.GT.2) GO TO 20
      B = IPMPAR(4)
      EMIN = IPMPAR(9)
C*PT*WARNING* Already double-precision (DBLE)
      ONE = DBLE(1)
      BINV = ONE/B
      W = B** (EMIN+2)
      SPMPAR = ((W*BINV)*BINV)*BINV
      RETURN
C
   20 IBETA = IPMPAR(4)
      M = IPMPAR(8)
      EMAX = IPMPAR(10)
C
      B = IBETA
      BM1 = IBETA - 1
C*PT*WARNING* Already double-precision (DBLE)
      ONE = DBLE(1)
      Z = B** (M-1)
      W = ((Z-ONE)*B+BM1)/ (B*Z)
C
      Z = B** (EMAX-2)
      SPMPAR = ((W*Z)*B)*B
      RETURN

      END
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION FUNCTION STVALN(P)
C*PT*WARNING* Already double-precision
C
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION STVALN(P)
C                    STarting VALue for Neton-Raphon
C                calculation of Normal distribution Inverse
C
C
C                              Function
C
C
C     Returns X  such that CUMNOR(X)  =   P,  i.e., the  integral from -
C     infinity to X of (1/SQRT(2*PI)) EXP(-U*U/2) dU is P
C
C
C                              Arguments
C
C
C     P --> The probability whose normal deviate is sought.
C                    P is DOUBLE PRECISION
C
C
C                              Method
C
C
C     The  rational   function   on  page 95    of Kennedy  and  Gentle,
C     Statistical Computing, Marcel Dekker, NY , 1980.
C
C**********************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION P
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION SIGN,Y,Z
C*PT*WARNING* Already double-precision
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION XDEN(5),XNUM(5)
C*PT*WARNING* Already double-precision
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DEVLPL
      EXTERNAL DEVLPL
C*PT*WARNING* Already double-precision (DBLE)
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC DBLE,LOG,SQRT
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Data statements ..
      DATA XNUM/-0.322232431088D0,-1.000000000000D0,-0.342242088547D0,
     +     -0.204231210245D-1,-0.453642210148D-4/
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
C*PT*WARNING* Constant already double-precision
      DATA XDEN/0.993484626060D-1,0.588581570495D0,0.531103462366D0,
     +     0.103537752850D0,0.38560700634D-2/
C*PT*WARNING* Constant already double-precision
C     ..
C     .. Executable Statements ..
      IF (.NOT. (P.LE.0.5D0)) GO TO 10
C*PT*WARNING* Constant already double-precision
      SIGN = -1.0D0
      Z = P
      GO TO 20
C*PT*WARNING* Constant already double-precision

   10 SIGN = 1.0D0
C*PT*WARNING* Constant already double-precision
      Z = 1.0D0 - P
C*PT*WARNING* Constant already double-precision
   20 Y = SQRT(-2.0D0*LOG(Z))
      STVALN = Y + DEVLPL(XNUM,5,Y)/DEVLPL(XDEN,5,Y)
      STVALN = SIGN*STVALN
      RETURN

      END
