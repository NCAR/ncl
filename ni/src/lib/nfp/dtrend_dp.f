C*PL*ERROR* Comment line too long
C -----------------------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DDTRNDX(X,NPTS,IOPT,XMEAN,XVARI,XVARO,C,IER)
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION XVARI
      DOUBLE PRECISION XVARO
      DOUBLE PRECISION XCNTR
      DOUBLE PRECISION FN
      DOUBLE PRECISION SUMX
      DOUBLE PRECISION SUMY
      DOUBLE PRECISION SUMX2
      DOUBLE PRECISION SUMX3
      DOUBLE PRECISION SUMX4
      DOUBLE PRECISION SUMXY
      DOUBLE PRECISION SUMX2Y
      DOUBLE PRECISION XS
      DOUBLE PRECISION D
      DOUBLE PRECISION XBAR

C DETREND THE SERIES X

C NOMENCLATURE :

C .   INPUT

C .   X         - SERIES TO BE DETRENDED
C .   NPTS      - LENGTH OF X
C .   IOPT      - DETRENDING OPTION
C*PL*ERROR* Comment line too long
C .               IOPT < 0 : CALCULATE THE MEAN AND VARIANCE OF X THEN RETURN
C .               IOPT = 0 : REMOVE THE MEAN ONLY
C*PL*ERROR* Comment line too long
C .               IOPT = 1 : REMOVE THE MEAN AND USE LINEAR LST SQRS TO DETREND
C*PL*ERROR* Comment line too long
C .               IOPT = 2 : REMOVE THE MEAN AND USE QUADRATIC LST SQRS TO DETRD
C .
C .   OUTPUT
C .
C .   XMEAN     - MEAN OF THE ORIGINAL INPUT  SERIES (OUTPUT)
C .   XVARI     - VARIANCE OF THE ORIGINAL INPUT SERIES (OUTPUT)
C .   XVARO     - VARIANCE OF THE DETRENDED SERIES (OUTPUT)
C .               NOTE : XVARI = XVARO WHEN IOPT @ 0
C .   C         - COEFFICIENTS OF THE LEAST SQUARES POLYNOMIAL (OUTPUT)
C .               MUST BE DIMENSIONED (3) IN THE CALLING PROGRAM
C .               C(1)  CONSTANT
C .               C(2)  COEF OF FIRST POWER [SLOPE IF IOPT=1]
C .               C(3)  COEF OF SECOND POWER
C .   IER       - ERROR CODE

      DOUBLE PRECISION X(NPTS),C(3)
C NCLEND

      C(1) = 0.D0
      C(2) = 0.D0
      C(3) = 0.D0

      IER = 0
      IF (NPTS.LT.2) IER = -21
      IF (IER.NE.0) RETURN

      N = NPTS

C CALCULATE THE MEAN AND VARIANCE OF THE INPUT SERIES

      XMEAN = 0.0D0
      XVARI = 0.0D0
      DO 10 I = 1,N
          XMEAN = XMEAN + X(I)
   10 XVARI = XVARI + X(I)*X(I)
      XVARI = (XVARI-XMEAN*XMEAN/DBLE(N))/DBLE(N)
      XMEAN = XMEAN/DBLE(N)
      XVARO = XVARI

      IF (IOPT.LT.0) RETURN

C MUST AT LEAST WANT THE SERIES MEAN REMOVED (IOPT=0)

      DO 20 I = 1,N
   20 X(I) = X(I) - XMEAN

      IF (IOPT.EQ.1) THEN

C MUST ALSO WANT LEAST SQUARES TREND LINE REMOVED

          C(2) = 0.D0
          XCNTR = DBLE(N+1)*0.5D0
          DO 30 I = 1,N
   30     C(2) = C(2) + X(I)* (DBLE(I)-XCNTR)

          C(2) = C(2)*12.D0/DBLE(N* (N*N-1))
C Y-INTERCEPT
          C(1) = XMEAN - C(2)*XCNTR

          DO 40 I = 1,N
   40     X(I) = X(I) - C(2)* (DBLE(I)-XCNTR)

      ELSE IF (IOPT.EQ.2) THEN

C MUST WANT QUADRATIC DETRENDING

          FN = DBLE(N)
          SUMX = 0.D0
          SUMY = 0.D0
          SUMX2 = 0.D0
          SUMX3 = 0.D0
          SUMX4 = 0.D0
          SUMXY = 0.D0
          SUMX2Y = 0.D0
          DO 50 I = 1,N
              XS = DBLE(I)
              SUMX = SUMX + XS
              SUMY = SUMY + X(I)
              SUMX2 = SUMX2 + XS**2
              SUMX3 = SUMX3 + XS**3
              SUMX4 = SUMX4 + XS**4
              SUMXY = SUMXY + XS*X(I)
              SUMX2Y = SUMX2Y + XS**2*X(I)
   50     CONTINUE
          D = FN* (SUMX2*SUMX4-SUMX3**2) +
     +        SUMX* (SUMX3*SUMX2-SUMX*SUMX4) +
     +        SUMX2* (SUMX*SUMX3-SUMX2**2)
          C(1) = (SUMX2*SUMX4-SUMX3**2)*SUMY +
     +           (SUMX3*SUMX2-SUMX*SUMX4)*SUMXY +
     +           (SUMX*SUMX3-SUMX2**2)*SUMX2Y
          C(2) = (SUMX3*SUMX2-SUMX*SUMX4)*SUMY +
     +           (FN*SUMX4-SUMX2**2)*SUMXY +
     +           (SUMX*SUMX2-FN*SUMX3)*SUMX2Y
          C(3) = (SUMX*SUMX3-SUMX2**2)*SUMY +
     +           (SUMX*SUMX2-FN*SUMX3)*SUMXY + (FN*SUMX2-SUMX**2)*SUMX2Y
          C(1) = C(1)/D
          C(2) = C(2)/D
          C(3) = C(3)/D
          DO 60 I = 1,N
   60     X(I) = X(I) - (C(1)+C(2)*DBLE(I)+C(3)*DBLE(I)**2)

      END IF

C CALCULATE THE VARIANCE OF THE DETRENDED SERIES
C .   XBAR SHOULD BE ZERO TO MACHINE ACCURACY SUBTRACT IT OUT ANYWAY

      XBAR = 0.0D0
      XVARO = 0.0D0
      DO 90 I = 1,N
          XBAR = XBAR + X(I)
   90 XVARO = XVARO + X(I)*X(I)
      XVARO = (XVARO-XBAR*XBAR/DBLE(N))/DBLE(N)

      XBAR = XBAR/DBLE(N)
      DO 95 I = 1,N
   95 X(I) = X(I) - XBAR

      RETURN
      END
