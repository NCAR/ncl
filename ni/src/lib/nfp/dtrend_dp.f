C NCLFORTSTART
      SUBROUTINE DDTRNDX(X,NPTS,IOPT,XMEAN,XVARI,XVARO,C,IER)
      IMPLICIT NONE
      INTEGER NPTS, IOPT, IER
      DOUBLE PRECISION X(NPTS),C(3)
      DOUBLE PRECISION XMEAN
      DOUBLE PRECISION XVARI
      DOUBLE PRECISION XVARO
C NCLEND

      INTEGER I, N
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
C .               IOPT < 0 : CALCULATE THE MEAN AND VARIANCE OF X THEN
C .                          RETURN
C .               IOPT = 0 : REMOVE THE MEAN ONLY
C .               IOPT = 1 : REMOVE THE MEAN AND USE LINEAR LST SQRS TO
C .                          DETREND
C .               IOPT = 2 : REMOVE THE MEAN AND USE QUADRATIC 
C .                          LST SQRS TO DETRD
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


      C(1) = 0.D0
      C(2) = 0.D0
      C(3) = 0.D0

      IER = 0
      IF (NPTS.LT.2) IER = -21
      IF (IER.NE.0) RETURN

      N  = NPTS
      FN = DBLE(N)

C CALCULATE THE MEAN AND VARIANCE OF THE INPUT SERIES

      XMEAN = 0.0D0
      XVARI = 0.0D0
      DO I = 1,N
         XMEAN = XMEAN + X(I)
         XVARI = XVARI + X(I)*X(I)
      END DO
      XVARI = (XVARI-XMEAN*XMEAN/FN)/FN
      XMEAN = XMEAN/FN
      XVARO = XVARI

      IF (IOPT.LT.0) RETURN

C MUST AT LEAST WANT THE SERIES MEAN REMOVED (IOPT=0)

      DO I = 1,N
          X(I) = X(I) - XMEAN
      END DO

      IF (IOPT.EQ.1) THEN

C MUST ALSO WANT LEAST SQUARES TREND LINE REMOVED

          C(2) = 0.D0
          XCNTR = DBLE(N+1)*0.5D0
          DO I = 1,N
             C(2) = C(2) + X(I)* (DBLE(I)-XCNTR)
          END DO

C orig    C(2) = C(2)*12.D0/DBLE(N* (N*N-1))
          C(2) = (C(2)/FN)*(12.D0/(FN*FN-1.D0))

C Y-INTERCEPT
          C(1) = XMEAN - C(2)*XCNTR

          DO I = 1,N
             X(I) = X(I) - C(2)* (DBLE(I)-XCNTR)
          END DO

      ELSE IF (IOPT.EQ.2) THEN

C MUST WANT QUADRATIC DETRENDING

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

      XBAR  = 0.0D0
      XVARO = 0.0D0
      DO I = 1,N
         XBAR = XBAR + X(I)
         XVARO = XVARO + X(I)*X(I)
      END DO
      XVARO = (XVARO-XBAR*XBAR/FN)/FN

      XBAR = XBAR/FN
      DO I = 1,N
         X(I) = X(I) - XBAR
      END DO

      RETURN
      END
c -------------------------------------------------------------
      SUBROUTINE DDTRNDMSG(X,Y,NPTS,XMSG,YMSG,IOPT,YDT,SLOPE,YINT,IER)
      IMPLICIT NONE

c NCL:   yNew = dtrend_msg (x[*]:numeric, y:numeric
c                          ,remove_mean:logical, return_info:logical)

c this routine will calculate the least squares "slope" and y-intercept
c .   remove the trend.
c .   (a) missing data are allowed.
c .   (b) data need not be equally spaced in "x"

c .   yLine = slope*y + yint
c .   ydt   = y - yLine

c arguments :
c .   x,y      - input vectors
c .   npts     - length of vectors x and y
c .   x/ymsg   - missing code: if no msg values set to some number
c .                            which will not be encountered.
c .              ymsg will be used to fill missing values
c .   iopt     - remove the mean of y prior to detrending
c .              this does not affect the slope but does affect 
c .              the y-intercept
c .   ydt      - detrended series
c .              this could be "y" if original series not needed
c .   slope    - slope (trend ... regression coef)
c .   yint     - y-intercept
c .   ier      - if (ier.ne.0) an error has occurred

      INTEGER NPTS,IOPT,IER
      DOUBLE PRECISION X(1:NPTS),Y(1:NPTS),SLOPE,YINT,YDT(1:NPTS)
      DOUBLE PRECISION XMSG,YMSG

      INTEGER N
      DOUBLE PRECISION XYN,XSUM,YSUM,X2SUM,Y2SUM,XYSUM
      DOUBLE PRECISION XBAR,YBAR,XVAR,YVAR,XYVAR


      IER = 0
      SLOPE = YMSG
      YINT = YMSG

      IF (NPTS.LT.2) IER = 1
      IF (IER.NE.0) RETURN

      DO N = 1,NPTS
          YDT(N) = Y(N)
      END DO
c                         this was added after the original code
      IF (IOPT.NE.0) THEN
          XYN = 0.0D0
          YSUM = 0.0D0
          DO N = 1,NPTS
              IF (X(N).NE.XMSG .AND. YDT(N).NE.YMSG) THEN
                  YSUM = YSUM + YDT(N)
                  XYN = XYN + 1.D0
              END IF
          END DO
c                               all msg values
          IF (XYN.LT.1.D0) THEN
              IER = 5
              RETURN
          END IF

          YBAR = YSUM/XYN
          DO N = 1,NPTS
              IF (YDT(N).NE.YMSG) THEN
                  YDT(N) = YDT(N) - YBAR
              END IF
          END DO
      END IF


      XYN = 0.0D0
      XSUM = 0.0D0
      YSUM = 0.0D0
      X2SUM = 0.0D0
      Y2SUM = 0.0D0
      XYSUM = 0.0D0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG .AND. YDT(N).NE.YMSG) THEN
              XSUM = XSUM + X(N)
              YSUM = YSUM + YDT(N)
              X2SUM = X2SUM + X(N)*X(N)
              Y2SUM = Y2SUM + Y(N)*YDT(N)
              XYSUM = XYSUM + X(N)*YDT(N)
              XYN = XYN + 1.D0
          END IF
      END DO

      IF (XYN.LT.1.D0) THEN
C all msg values
          IER = 5
          RETURN
      ELSE IF (XYN.LT.3.D0) THEN
C not enough data
          IER = 6
          RETURN
      END IF

      XBAR = XSUM/XYN
      YBAR = YSUM/XYN
      XVAR = X2SUM - XSUM*XSUM/XYN
      YVAR = Y2SUM - YSUM*YSUM/XYN
      XYVAR = XYSUM - XSUM*YSUM/XYN

      SLOPE = XYVAR/XVAR
      YINT = YBAR - SLOPE*XBAR

      DO N = 1,NPTS
          IF (X(N).NE.XMSG .AND. YDT(N).NE.YMSG) THEN
              YDT(N) = YDT(N) - (SLOPE*X(N)+YINT)
          ELSE
              YDT(N) = YMSG
          END IF
      END DO

      RETURN
      END
