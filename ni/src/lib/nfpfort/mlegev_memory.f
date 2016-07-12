C NCLFORTSTART
      subroutine dmlegevi(x, nx, xmsg, vals, xx, ierr)
      implicit none
C                                               !  INPUT
      integer nx
      double precision x(nx),  xx(nx), xmsg
C                                               !  OUTPUT
      integer ierr
      double precision vals(6)
C NCLEND
c
c Interface to AS-215 algorithm
c     (1) remove msg values
c     (2) create 1st guess for parameters
c     (2) return a selected subset of values
c                                               ! LOCAL
      integer monit, nxn, n
      double precision para(3), vcov(6)         ! see subroutine
      double precision euler, pi, six, zero     ! local variables  
      double precision x1, x2, x3, xvar, xstd, xavg

c c c double precision xx(nx)                   ! f90 auto array
c c c double precision,allocatable::xx(:)       ! allocate in calling routine
      
      pi    = 3.141592653589793d0
      euler = 0.577215664901532d0        ! Euler–Mascheroni constant
      six   = 6.0d0
      zero  = 0.0d0
c                                               ! strip msg
c     allocate(xx(nx))
      nxn = 0
      do n=1,nx
         if (x(n).ne.xmsg) then
             nxn = nxn+1
             xx(nxn) = x(n)
         end if
      end do

c c c if (nxn.eq.0) then
      if (nxn.lt.10) then                       ! arbitrary
          do n=1,6
             vals(n) = xmsg
          end do
          ierr  = -999                          ! all msg
      else
          x1  = zero 
          x2  = zero 
          do n=1,nxn
             x1 = x1 + xx(n)
             x2 = x2 + xx(n)*xx(n) 
          end do
          x3    = x1*x1/nxn
          xvar  = (x2-x3)/(nxn-1)
          xstd  = sqrt(max(xvar,zero))          ! prevent possible roundoff
          xavg  = x1/nxn
                                                ! 1st guess at parameters
          para(1) = zero                        ! location, center, shift
          para(2) = xstd*sqrt(six/pi)           ! scale
          para(3) = xavg-euler*para(1)          ! shape

          monit = 0                             ! no monitoring
          call dmlegev(xx, nxn, para, vcov, monit, ierr)

          vals(1) = para(1)                     ! location (center, shift)
          vals(2) = para(2)                     ! scale parameter
          vals(3) = para(3)                     ! shape parameter

c                                               ! std errors
          vals(4) = sqrt(vcov(1))               ! location (center, shift)
          vals(5) = sqrt(vcov(3))               ! scale
          vals(6) = sqrt(vcov(6))               ! shape
      end if
      return
      end
C-----------------------------------------------------------------
c Algorithm AS 215
c Maximum-Likelihood Estimation of the Parameters of the Generalized Extreme-Value Distribution
c J. R. M. Hosking
c Journal of the Royal Statistical Society. Series C (Applied Statistics) 
c Vol. 34, No. 3 (1985), pp. 301-310
c Stable URL: http://www.jstor.org/stable/2347483
C-----------------------------------------------------------------
      SUBROUTINE DMLEGEV(X, N, PARA, VCOV, MONIT, IFAULT)
      IMPLICIT NONE
      INTEGER N, MONIT, IFAULT
      DOUBLE PRECISION PARA(3), VCOV(6), X(N)

C CODE:    http://ftp.uni-bayreuth.de/math/statlib/apstat/215
C ARTICLE: http://www.jstor.org/stable/pdf/2347483

c INPUT
c  X  Double precision array (N) input: data array
c  N Integer input: sample size
c  PARA Double precision array (3) input: initial estimates of the parameters, in the order #, ae, k
c
C OUTPUT
C VCOV(6) double precision array (6): on successful exit (IFAULT= 0)
C         the lower triangle of the estimated variance-covariance matrix, H-1, 
C         of the maximum-likelihood estimates, stored row by row.
C MONIT  Integer input: flag for monitoring of iterations (see below)
C IFAULT Integer output: failure indicator (see below)
C      0 on successful exit
C      1 if N < 2. PARA and VCO V are set to zero
C      2 if maximum number of iterations reached
C      3 if maximum number of evaluations of log-likelihood reached
C      4 if maximum number of steplength reductions reache
C
      INTEGER I, ITYPE, KK, MAXEV, MAXIT, MAXSR, NEVAL, NITER, NSR

C        ALGORITHM AS215   APPL. STATIST. (1985) VOL. 34, NO. 3
C        Modifications in AS R76 (1989) have been incorporated.
C
C        Additional modifications by J. R. M. Hosking, August 1994:
C        Modify steepest-ascent step so that it is invariant to
C        rescaling the data.  Improves chance of convergence from
C        poor initial values.
C
C        MAXIMUM-LIKELIHOOD ESTIMATION OF GENERALIZED EXTREME-VALUE
C        DISTRIBUTION
C
      DOUBLE PRECISION A, ACCA, ACCG, ACCU, AI, AIGI, AN, D, DA, DAA,
     *  DAG, DELA, DELG, DELU, DG, DGG, DU, DUA, DUG, DUU, E, F, FOLD,
     *  G, GAI, GG, GI, GIPQ, GNORM, H, HALF, HE, HH, ONE, P, PA, PQ,
     *  PQG, PU, Q, QA, QU, R, RA, RATIO, RG, RU, SE, SH, SHE, SHH,
     *  SHHE, SMALL, SRF, STEPA, STEPG, STEPU, SY, SYE, SYHE, SYYE,
     *  TEMP1, TEMP2, U, VLNEG, XMAX, XMIN, Y, YE, Z, ZERO
      CHARACTER*8 ACTI1, ACTI2, ACTI3, ACTI4, ACTI5, ACTI6, ACTI7,
     *  ACTI8, ACTI9
      DATA ACTI1/'  NEWTON'/, ACTI2/'  ST.ASC'/, ACTI3/'  RESETK'/,
     *  ACTI4/'  SR.INF'/, ACTI5/'  SR.LIK'/, ACTI6/'  MAX.SR'/,
     *  ACTI7/'  MAX.EV'/, ACTI8/'  MAX.IT'/, ACTI9/'  CONVGD'/
      DATA ZERO /0.0D0/, HALF /0.5D0/, ONE/1.0D0/
C
C        ADDU,ACCA,ACCG ARE ACCURACY CRITERIA FOR TESTING CONVERGENCE
C        STEPU,STEPA,STEPG ARE MAXIMUM STEPLENGTHS FOR ITERATIONS
C        ACCU,ACCA,STEPU,STEPA ARE SCALED BY CURRENT VALUE OF A WHEN
C        USED IN PROGRAM
C
      DATA ACCU, ACCA, ACCG /3 * 1D-5/, STEPU, STEPA, STEPG /
     *  0.5D0, 0.25D0, 0.2D0/
C
C        MAXIT IS MAX. NO. OF ITERATIONS
C        MAXEV IS MAX. NO. OF EVALUATIONS OF LIKELIHOOD FUNCTION
C        SRF IS STEPLENGTH REDUCTION FACTOR
C        MAXSR IS MAX. NO. OF STEPLENGTH REDUCTIONS PERMITTED PER
C        ITERATION
C
      DATA MAXIT /30/, MAXEV /50/, SRF /0.25D0/, MAXSR /30/
C
C        SMALL IS A SMALL NUMBER, USED TO ADJUST THE SHAPE PARAMETER TO
C        AVOID AN EXACT ZERO VALUE OR BORDERLINE INFEASIBILITY
C        ALNEG IS A LARGE NEGATIVE NUMBER, USED TO INITIALIZE
C        LOG-LIKELIHOOD
C
      DATA SMALL /1.D-3/, VLNEG /-1.D37/
C
C        FIND MIN AND MAX DATA VALUE
C
      DO 10 I = 1, 6
  10  VCOV(I) = ZERO
      IFAULT = 1
      IF (N .LE. 2) GOTO 170
      XMIN = X(1)
      XMAX = X(1)
      DO 20 I = 2, N
        IF (X(I) .LT. XMIN) XMIN = X(I)
        IF (X(I) .GT. XMAX) XMAX = X(I)
   20 CONTINUE
C
C       INITIALIZATION
C       U IS LOCATION PARAMETER
C       A IS SCALE PARAMETER
C       G IS SHAPE PARAMETER
C
      IF (MONIT .GT. 0) WRITE (6, 6000)
      IFAULT = 0
      NITER = 0
      NEVAL =0
      FOLD = VLNEG
      U = PARA(1)
      A = PARA(2)
      G = PARA(3)
      IF (ABS(G) .LT. SMALL) G = SMALL
      IF (A .LE. ZERO) A = ONE
      AN = DBLE(FLOAT(N))
C
C        CHECK WHETHER ALL DATA POINTS LIE WITHIN THE RANGE OF THE GEV
C        DISTRIBUTION WITH THE INITIAL PARAMETERS - IF NOT, ADJUST THE
C        SHAPE PARAMETER SO AS TO BRING ALL POINTS WITHIN RANGE
C
      IF (G .GT. ZERO) GOTO 30
      IF (XMIN .GE. U) GOTO 40
      Z = A / (XMIN - U)
      IF (G .GT. Z) GOTO 40
      IF (MONIT .GT. 0) WRITE (6, 6010) NITER, NEVAL, U, A, G, ACTI3
      G = Z + SMALL
      IF (G .GE. ZERO) G = HALF * Z
      GOTO 40
   30 IF (XMAX .LE. U) GOTO 40
      Z = A / (XMAX - U)
      IF (G .LT. Z) GOTO 40
      IF(MONIT .GT. 0) WRITE (6, 6010) NITER, NEVAL, U, A, G, ACTI3
      G = Z - SMALL
      IF (G .LE. ZERO) G = HALF * Z
C
C        START OF MAIN LOOP
C
   40 DO 140 NITER = 1, MAXIT
C
      NSR = 0
   50 IF (NEVAL .GE. MAXEV) GOTO 150
      NEVAL = NEVAL + 1
      AI = ONE / A
      GI = ONE /G
      GAI = G * AI
      AIGI = AI * GI
      GG = ONE - G
C
C       ACCUMULATE SUMS OF QUANTITIES OCCURRING IN LIKELIHOOD
C       DERIVATIVES
C
C       IN PRESCOTT AND WALDEN'S NOTATION:
C       Z IS 1 - K * (X(I)-U) / A
C       Y IS THE REDUCED VARIATE - (1/K) * LOG(Z)
C       E IS EXP(-Y)
C       H IS EXP(K*Y)
C
      SY = ZERO
      SE = ZERO
      SYE = ZERO
      SYYE = ZERO
      SH = ZERO
      SHE = ZERO
      SYHE = ZERO
      SHHE = ZERO
      SHH = ZERO
      DO 60 I = 1, N
        Z = ONE -GAI * (X(I) - U)
        Y = -GI * LOG(Z)
        E = EXP(-Y)
        H = ONE / Z
        YE = Y * E
        HE = H * E
        HH = H * H
        SY = SY + Y
        SE = SE + E
        SYE = SYE + YE
        SYYE = SYYE + Y * YE
        SH = SH + H
        SHE = SHE + HE
        SYHE = SYHE + Y * HE
        SHHE = SHHE + HH * E
        SHH = SHH + HH
   60 CONTINUE
C
C       F IS CURRENT VALUE OF LIKELIHOOD FUNCTIONN
C
      F = -AN * LOG(A) - GG * SY - SE
      IF (F .GT. FOLD) GOTO 90
C
C       LIKELIHOOD HAS NOT INCREASED - REDUCE STEPLENGTH AND TRY AGAIN
C
      IF (MONIT .GT. 0) WRITE(6, 6010) NITER, NEVAL, U, A, G, ACTI5, F
      IF (NSR .EQ. MAXSR) GOTO 80
   70 NSR = NSR + 1
      U = U - DELU
      A = A - DELA
      G = G - DELG
      DELU = SRF * DELU
      DELA = SRF * DELA
      DELG = SRF * DELG
      U = U + DELU
      A = A + DELA
      G = G + DELG
      IF (A .GT. G * (XMIN - U) .AND. A .GT. G * (XMAX - U) .AND. G .NE.
     *  ZERO) GO TO 50
      IF (MONIT .GT. 0) WRITE (6, 6010) NITER, NEVAL, U, A, G, ACTI4
      IF (NSR .LT. MAXSR) GOTO 70
C
C        MAX. NO. OF STEPLENGTH REDUCTIONS REACHED
C        IF CURRENT ITERATION IS NEWTON-RAPHSON, TRY STEEPEST ASCENT
C        INSTEAD.  IF CURRENT ITERATION IS STEEPEST ASCENT, GIVE UP.
C
   80 U = U - DELU
      A = A - DELA
      G = G - DELG
      IF (MONIT .GT. 0) WRITE(6, 6010) NITER, NEVAL, U, A, G, ACTI6
      IF (ITYPE .EQ. 1) GOTO 100
      IFAULT = 4
      GOTO 160
C
C        P,Q,R, ARE AS DEFINED IN FLOOD STUDIES REPORT
C
   90 FOLD = F
      P = AN - SE
      Q = SHE - GG * SH
      R = AN - SY + SYE
      PQ = P + Q
      GIPQ = GI * PQ
C
C        FIRST DERIVATIVES OF LOG-LIKELIHOOD
C
      DU = -AI * Q
      DA = -AIGI * PQ
      DG = -GI * (R - GIPQ)
      IF (MONIT .GT. 0) GNORM = SQRT(DU * DU + DA * DA + DG * DG)
C
C        DERIVATIVES OF P,Q,R
C
      PU = -AI * SHE
      PA = GI * PU + AIGI * SE
      QU = GG * AI * (SHHE + G * SHH)
      RU = AI * (SH - SHE + SYHE)
      RA = GI * RU - AIGI * (AN - SE + SYE)
      RG = GI * (SY - SYE + SYYE - A * RA)
      QA = AI * Q + GI * (PU + QU)
      PQG = GIPQ + A * (RA - GI * (PA + QA))
C
C         MINUS SECOND DERIVATIVE OF LOG-LIKELIHOOD (HESSIAN MATRIX)
C
      DUU = AI * QU
      DUA = AIGI * (PU + QU)
      DAA = -AIGI * (AI * PQ - PA - QA)
      DUG = GI * (RU - GI * (PU + QU))
      DAG = -AIGI * (GIPQ - PQG)
      DGG = GI * (RG - GI * (PQG + R - GIPQ - GIPQ))
C
C         INVERT HESSIAN MATRIX
C
      DO 95 KK = 1, 3
        IF (DUU .LE. ZERO) GO TO 100
        D = ONE / DUU
        TEMP1 = -DUA * D
        IF (KK .GT. 2) TEMP1 = -TEMP1
        TEMP2 = -DUG * D
        IF (KK .GT. 1) TEMP2 = -TEMP2
        DUU = DAA + TEMP1 * DUA
        DUA = DAG + TEMP1 * DUG
        DAA = DGG + TEMP2 * DUG
        DUG = TEMP1
        DAG = TEMP2
        DGG = D
   95 CONTINUE
C
C        CALCULATE STEPLENGTHS
C
      ITYPE = 1
      IF (MONIT .GT. 0) WRITE(6, 6010) NITER, NEVAL, U, A, G, ACTI1, F,
     *  GNORM
      DELU = DUU * DU + DUA * DA + DUG * DG
      DELA = DUA * DU + DAA * DA + DAG * DG
      DELG = DUG * DU + DAG * DA + DGG * DG
      RATIO = MAX(ABS(DELU) / (STEPU * A), ABS(DELA) / (STEPA * A),
     *  ABS(DELG) / STEPG)
      IF (RATIO .LT. ONE) GOTO 110
      RATIO = ONE / RATIO
      DELU = DELU * RATIO
      DELA = DELA * RATIO
      DELG = DELG * RATIO
      GOTO 110
C
C        HESSIAN IS NOT POSITIVE DEFINITE - MAKE A LARGE STEP IN THE
C        DIRECTION OF STEEPEST ASCENT
C
  100 ITYPE = 2
      IF (MONIT .GT. 0) WRITE(6, 6010) NITER, NEVAL, U, A, G, ACTI2, F,
     *  GNORM
C---------------------------------------  NEXT 11 LINES ADDED, AUG.94
      D = ABS(VLNEG)
      TEMP1 = D
      IF (DU .NE. ZERO) TEMP1 = STEPU / (ABS(DU) * A)
      TEMP2 = D
      IF (DA .NE. ZERO) TEMP2 = STEPA / (ABS(DA) * A)
      Z = D
      IF (DG .NE. ZERO) Z = STEPG / ABS(DG)
      RATIO = MIN(TEMP1, TEMP2, Z)
      DELU = RATIO * DU * A * A
      DELA = RATIO * DA * A * A
      DELG = RATIO * DG
C---------------------------------------  NEXT 11 LINES REMOVED, AUG.94
C      D = ABS(VLNEG)
C      TEMP1 = D
C      IF (DU .NE. ZERO) TEMP1 = STEPU * A / ABS(DU)
C      TEMP2 = D
C      IF (DA .NE. ZERO) TEMP2 = STEPA * A / ABS(DA)
C      Z = D
C      IF (DG .NE. ZERO) Z = STEPG / ABS(DG)
C      RATIO = MIN(TEMP1, TEMP2, Z)
C      DELU = RATIO * DU
C      DELA = RATIO * DA
C      DELG = RATIO * DG
C---------------------------------------  END OF DELETED CODE
C
C        ADJUST PARAMETERS
C
  110 U = U + DELU
      A = A + DELA
      G = G + DELG
C
C        TEST FOR FEASIBILITY
C
      IF (A .GT. G * (XMIN - U) .AND. A .GT. G * (XMAX - U) .AND. G .NE.
     *  ZERO) GOTO 130
      DO 120 NSR = 1, MAXSR
        IF (MONIT .GT. 0) WRITE(6, 6010) NITER, NEVAL, U, A, G, ACTI4
        U = U - DELU
        A = A - DELA
        G = G - DELG
        DELU = SRF * DELU
        DELA = SRF * DELA
        DELG = SRF * DELG
        U = U + DELU
        A = A + DELA
        G = G + DELG
        IF (A .GT. G * (XMIN - U) .AND. A .GT. G * (XMAX - Y) .AND.
     *     G .NE. ZERO) GOTO 140
  120 CONTINUE
      GOTO 80
C
C        TEST FOR CONVERGENCE
C
  130 IF (ABS(DELU) .GT. ACCU * A) GOTO 140
      IF (ABS(DELA) .GT. ACCA * A) GOTO 140
      IF (ABS(DELG) .GT. ACCG) GOTO 140
      IF (MONIT .GT. 0) WRITE(6, 6010) NITER, NEVAL, U, A, G, ACTI9
      VCOV(1) = DUU
      VCOV(2) = DUA
      VCOV(3) = DAA
      VCOV(4) = DUG
      VCOV(5) = DAG
      VCOV(6) = DGG
      GOTO 160
C
C        END OF MAIN LOOP
C
  140 CONTINUE
C
C        ITERATIONS NOT CONVERGED - SET FAULT FLAG
C
      IFAULT = 2
      IF (MONIT .GT. 0) WRITE(6, 6010) MAXIT, NEVAL, U, A, G, ACTI8
      GOTO 160
  150 IFAULT = 3
      IF (MONIT .GT. 0) WRITE(6, 6010) NITER, MAXEV, U, A, G, ACTI7
C
C        ITERATION FINISHED -COPY RESULTS INTO ARRAY PARA
C
  160 IF (MONIT .GT. 0) WRITE(6, 6020)
      PARA(1) = U
      PARA(2) = A
      PARA(3) = G
      RETURN
C
  170 DO 180 I = 1, 3
  180 PARA(I) = ZERO
      RETURN
C
 6000 FORMAT(/' MAXIMUM-LIKELIHOOD ESTIMATION OF GENERALIZED EXTREME',
     *  1X, 'VALUE DISTRIBUTION'// ' ITER EVAL', 8X, 'XI', 5X, 'ALPHA',
     *  9X, 'K  ACTION', 6X, 'LOG-L', 7X, 'GNORM')
 6010 FORMAT(1X, I4, I5, 3F10.4, 1X, A, F11.3, 1PD12.2)
 6020 FORMAT(1H0)
      END


