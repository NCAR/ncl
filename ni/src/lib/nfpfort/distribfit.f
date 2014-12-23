C NCLFORTSTART
      subroutine weibfit(maxn,xin,xmsg,nmin,level,weib,ier)
      implicit none
C                                                 ! INPUT
      integer maxn, nmin, ier
      double precision xin(maxn), xmsg, level
C                                                 ! OUTPUT
      double precision  weib(6)
C NCLEND
C
C
C Modified from:  http://ftp.uni-bayreuth.de/math/statlib/jqt/29-1-2
C Shea directory name: weibull.2_param_mle.ncl.f
c
c  Weibull Maximum Likelihood Parameter Estimates with Censored Data
c  J. Bert Keats, Frederick P. Lawrence, and F. K. Wang
c  Journal of Quality Technology, Volume 29, Number 1, pp. 105--110, January 1997

C                                                 ! LOCAL
      double precision x(maxn)
c     double precision y(maxn), cycle(maxn)
C      integer censor(maxn)
C****************************************************
C* FORTRAN PROGRAM USES MAXIMUM LIKELIHOOD TO       *
C* ESTIMATE THE PARAMETERS OF TWO-PARAMETER WEIBULL *
C* DISTRIBUTION. THE PROGRAM CAN BE USED FOR ALL    *
C* CENSORING CASES:                                 *
C*     CASE 1: MULTIPLY CENSORED DATA               *
C*     CASE 2: SINGLY CENSORED DATA                 *
C*     CASE 3: COMPLETE DATA                        *
C* CENSORED TYPE (0: FAILURE; 1: NON-FAILURE)*
C****************************************************
      INTEGER I,R,S,J,L,CC,ERROR,CASE
C     INTEGER INDEX
      INTEGER N,K,K1
      DOUBLE PRECISION BETA, THETA
      DOUBLE PRECISION PI,T2,T3,T4,ST1,ST2,NUM,NUM1
      DOUBLE PRECISION VARB,VART,COVBT,DEM1,DEM2,DEM3
      DOUBLE PRECISION S1R,S2R,S3R,S4R,S2S,S3S,S4S
      DOUBLE PRECISION DENOM,DENOM1,DENOM2,DENOM3,DENOM4
      DOUBLE PRECISION CON,IB,IT,COV,LOW1,LOW2,UP1,UP2
      DOUBLE PRECISION ALPHA,NORM,DELTA,L1,L2

      ier = 0
      do i=1,6
         weib(i) = xmsg
      end do

      case  = 3

      n = 0
      do 10 i=1,maxn
         if (xin(i).ne.xmsg .and. xin(i).gt.0.0d0) then
             n = n+1
         end if
10    continue

      if (n.lt.nmin) then
c c c     print *,"weibfit: not enough values: n=",n
          ier = 1
          return
      end if
    
      ALPHA = (1.0 - LEVEL) / 2.0D0
      R = 0
      S = 0
      J = 1
      L = 1
      DO 20 I=1,N
C C C   IF (CENSOR(I) .EQ. 0) THEN
        IF (xin(i).ne.xmsg .and. xin(i).gt.0.0d0) THEN
C C C     X(J) = CYCLE(I)
          X(J) = xin(i)
          J = J + 1
          R = R + 1
C C C   ELSE
C C C     Y(L) = CYCLE(I)
C C C     L = L + 1
C C C     S = S + 1
        END IF
20      CONTINUE
C
C MENON'S ESTIMATE OF BETA AS INITIAL
C APPROXIMATION OF BETA
C
      PI  = 3.141592653589793
      ST1 = 0
      ST2 = 0
      DO I = 1, R
        ST1 = ST1 +  DLOG(X(I))
        ST2 = ST2 + ( DLOG(X(I)))**2
      END DO

      S1R = ST1
      ST1 = (ST1**2)/(FLOAT(R))
      BETA = (6.0 * (ST2 - ST1))/((PI**2)*(FLOAT(R - 1)))
      IF (BETA .EQ. 0.0) BETA = .0001
      BETA = 1. / SQRT(BETA)
      CC = 0
      DELTA = 0
C
C NEWTON-RAPHSON ITERATIVE ESTIMATE OF BETA
C
      DO 100 K = 1,25
        S2R = 0
        S3R = 0
        S4R = 0
        S2S = 0
        S3S = 0
        S4S = 0
        DO 80 I = 1,R
          S2R = S2R + X(I)**BETA
          S3R = S3R + (X(I)**BETA) *  DLOG(X(I))
          S4R = S4R + (X(I)**BETA) * ( DLOG(X(I))**2)
80        CONTINUE
C C C     DO 85 I = 1,S
C C C      S2S = S2S + Y(I)**BETA
C C C      S3S = S3S + (Y(I)**BETA) *  DLOG(Y(I))
C C C      S4S = S4S + (Y(I)**BETA) * ( DLOG(Y(I))**2)
C C C 85   CONTINUE
        NUM1 = (S3R + S3S) / (S2R + S2S)
        NUM = (1. / BETA) + (S1R / R) - NUM1
        DENOM1 = (S3R + S3S)**2
        DENOM2 = (S2R + S2S) * (S4R + S4S)
        DENOM3 = (S2R + S2S)**2
        DENOM4 = 1.0 / BETA**2
        DENOM = DENOM4 - ((DENOM1 - DENOM2) / DENOM3)
        DELTA = NUM / DENOM
        BETA = BETA + DELTA
        K1 = K
C
C TEST FOR CONVERGENCE
C
        IF (ABS(DELTA) .LT. 0.0001) THEN
          CC = 1
          GOTO 105
        END IF
100     CONTINUE
C
C INDICATE NON-CONVERGENCE
C
105   IF (CC .EQ. 0) THEN
c c c   WRITE(*,*) ' THE NEWTON-RAPHSON IS NOT ', *  'CONVERGED'
          ier   = 2
          return
      ELSE
C
C IF CONVERGENCE HAS OCCURRED CALCULATE THETA & BETA
C
        S2R = 0
        S3R = 0
        S4R = 0
        S2S = 0
        S3S = 0
        S4S = 0
        DO 90 I = 1,R
          S2R = S2R + X(I)**BETA
          S3R = S3R + (X(I)**BETA) *  DLOG(X(I))
          S4R = S4R + (X(I)**BETA) * ( DLOG(X(I))**2)
90        CONTINUE
C C C   DO 95 I = 1,S
C C C     S2S = S2S + Y(I)**BETA
C C C     S3S = S3S + (Y(I)**BETA) *  DLOG(Y(I))
C C C     S4S = S4S + (Y(I)**BETA) * ( DLOG(Y(I))**2)
C C C   95        CONTINUE
        T2 = S2R + S2S
        T3 = S3R + S3S
        T4 = S4R + S4S
        THETA = (T2 / FLOAT(R))**(1.0 / BETA)
      END IF
C
C COMPUTE THE CONFIDENCE INTERVAL OF THE PARAMETERS
C THETA & BETA
C
      DEM1 = THETA**BETA
      DEM2 = THETA**(1 + BETA)
      DEM3 = THETA**(2 + BETA)
      L1   =  DLOG(THETA)
      L2   = ( DLOG(THETA))**2
      IB   = R / BETA**2 + (L2 * T2 - 2 * L1 * T3 + T4)/ DEM1
      IT   = (BETA * (BETA + 1) / DEM3 * T2) - (R * BETA / THETA**2)
      COV  = R / THETA - (T2 - BETA * L1 * T2 + BETA * T3) / DEM2

C Determinant of 2x2 matrix
C    [A  B               [Var(B)    Cov(B,T)
C     C  D]               Cov(B,T)  VAR(T)  ]
C          = A*D-B*C     = Var(B)*VAR(T)-Cov(B,T)**2
C                           IB     iT     COV**2

      CON  = (IB *IT) - COV**2
      VARB = IT / CON            
      VART = IB / CON
      COVBT= COV / CON
C                                    AVOID ANY NAMING CONFLICT
C C C CALL NORMAL(ALPHA,NORM,ERROR)
      CALL QNORMALQ(ALPHA,NORM,ERROR)

      LOW1 = BETA  - NORM * SQRT(VARB)
      UP1  = BETA  + NORM * SQRT(VARB)
      LOW2 = THETA - NORM * SQRT(VART)
      UP2  = THETA + NORM * SQRT(VART)

      weib(1) = BETA
      weib(2) = THETA
      weib(3) = LOW1
      weib(4) = UP1
      weib(5) = LOW2
      weib(6) = UP2

      return

C C C IF (CASE .EQ. 1) THEN
C C C   WRITE(*,300) N,R,S
C C C   WRITE(1,300) N,R,S
C300C C CFORMAT(10X,'MAXIMUM LIKELIHOOD ESTIMATION',//,
C C C*C C C10X,'FOR MULTIPLY CENSORED DATA',//,
C C C*C C C10X,'SAMPLE SIZE IS ',I3,//,
C C C*C C C10X,'NUMBER OF FAILURES IS ',I3,//,
C C C*C C C10X,'NUMBER OF NON-FAILURES IS ',I3,//)
C C C ELSE IF (CASE .EQ. 2) THEN
C C C   WRITE(*,301) N,R,S
C C C   WRITE(1,301) N,R,S
C301C C CFORMAT(10X,'MAXIMUM LIKELIHOOD ESTIMATION',//,
C C C*   10X,'FOR SINGLY CENSORED DATA',//,
C C C*   10X,'SAMPLE SIZE IS ',I3,//,
C C C*   10X,'NUMBER OF FAILURES IS ',I3,//,
C C C*   10X,'NUMBER OF NON-FAILURES IS ',I3,//)

C C C ELSE IF (CASE .EQ. 3) THEN
C C C   WRITE(*,302) N,R,S
C C C   WRITE(1,302) N,R,S
C302C C CFORMAT(10X,'MAXIMUM LIKELIHOOD ESTIMATION',//,
C C C*    10X,'FOR COMPLETE DATA',//,
C C C*    10X,'SAMPLE SIZE IS ',I3,//,
C C C*    10X,'NUMBER OF FAILURES IS ',I3,//,
C C C*    10X,'NUMBER OF NON-FAILURES IS ',I3,//)
C C C END IF

C C C LEVEL = LEVEL*100.
C C C WRITE(*,600) LEVEL
C C C WRITE(1,600) LEVEL
C600  FORMAT(25X,F5.2,' % CONFIDENCE INTERVAL',/,
C C C*25X,'--------------------------',/,
C C C*1X,'PARAMETER ESTIMATE ',6X,' LOWER LIMIT ',
C C C*'UPPER LIMIT',/,
C C C*1X,'------------------------------------------',
C C C*'--------',/)
C C C WRITE(*,700) BETA,LOW1,UP1
C C C WRITE(1,700) BETA,LOW1,UP1
C C C700   FORMAT(3X,'BETA',5X,F9.4,5X,F9.4,4X,F9.4,/)
C C C WRITE(*,800) THETA,LOW2,UP2
C C C WRITE(1,800) THETA,LOW2,UP2
C800  FORMAT(3X,'THETA',4X,F9.4,5X,F9.4,4X,F9.4,/)
      RETURN
      END
C****************************************************
C* SUBROUTINE TO GENERATE THE INVERSE NORMAL VALUE, *
C* TAKEN FROM: ``STATISTICAL COMPUTING'' BY KENNEDY,*
C* W. J. & GENTLE, J. E. (1980), pp. 95.            *
C****************************************************
      SUBROUTINE QNORMALQ(ALPHA,NORM,ERROR)
C C C SUBROUTINE NORMAL(ALPHA,NORM,ERROR)
      implicit none 
      INTEGER ERROR
      DOUBLE PRECISION ALPHA,NORM,LIM,P0,P1,P2,P3,P4
      DOUBLE PRECISION Q0,Q1,Q2,Q3,Q4,Y
      LIM = 1.0D-20
      P0 = -0.322232431088
      P1 = -1.0
      P2 = -0.342242088547
      P3 = -0.0204231210245
      P4 = -0.453642210148D-4
      Q0 = 0.0993484626060
      Q1 = 0.588581570495
      Q2 = 0.531103462366
      Q3 = 0.103537752850
      Q4 = 0.0038560700634
      ERROR = 1
      NORM = 0
      IF (ALPHA .GT. 0.5) ALPHA = 1. - ALPHA
      IF (ALPHA .LT. LIM) THEN
        NORM = 0
        GO TO 10
      END IF
      ERROR = 0
      IF (ALPHA .EQ. 0.5) THEN
        NORM = 0
        GO TO 10
      END IF
      Y = DSQRT(DLOG(1. / ALPHA**2))
      NORM = Y + ((((Y*P4+P3)*Y+P2)*Y+P1)*Y+P0)/
     *           ((((Y*Q4+Q3)*Y+Q2)*Y+Q1)*Y+Q0)
      IF (ALPHA .GT. 0.5) NORM = -NORM
10    RETURN
      END

