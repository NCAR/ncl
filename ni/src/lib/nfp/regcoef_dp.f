c -------------------------------------------------------------
      SUBROUTINE DREGCOEF(X,Y,NPTS,DMSG,RCOEF,TVAL,NPTXY,XAVE,YAVE,IER)
      IMPLICIT NONE

c NCL:  rcoef = regcoef (x,y,tval,nptxy)
c NCL:  rcoef = regline (x,y,tval,nptxy,xave,yave)

c this routine will calculate the regression coef (trend, slope,..),
c .   x and y averages, the t-statistic and the number of pts used
c .   missing data are allowed.

c .   see: brownlee
c .        statistical theory and methodology
c .        j wiley 1965   pgs: 281-284     QA276  .B77

c arguments :
c .   x,y      - input vectors
c .   npts     - length of vectors x and y
c .   dmsg     - missing code: if no msg values set to some number
c .                            which will not be encountered.
c .              dmsg will be used to fill missing values
c .   rcoef    - slope (trend ... regression coef)
c .   tval     - t-statistic (null hypothesis: H0: r=0.0)
c .   nptxy    - number of points used
c .   xave     - average of x
c .   yave     - average of y
c .   ier      - if (ier.ne.0) an error has occurred

C input
      INTEGER NPTS,NPTXY
C input
      DOUBLE PRECISION X(1:NPTS),Y(1:NPTS),DMSG
C output
      INTEGER IER
C output
      DOUBLE PRECISION RCOEF,TVAL,YAVE,XAVE

C local
      LOGICAL REGDBG
      INTEGER N
      DOUBLE PRECISION RNULL,XYN,DF
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION XSUM,YSUM,X2SUM,Y2SUM,XYSUM,XVAR,YVAR,XYVAR,
     +                 SSQREG,SSQ,VB,SQRTVB

      IER = 0
      TVAL = DMSG
      RCOEF = DMSG
      IF (NPTS.LT.2) IER = 1
      IF (IER.NE.0) RETURN

C null hypothesis
      RNULL = 0.0D0

      XYN = 0.0D0
C*PT*WARNING* Constant already double-precision
      XSUM = 0.d0
C*PT*WARNING* Constant already double-precision
      YSUM = 0.d0
C*PT*WARNING* Constant already double-precision
      X2SUM = 0.d0
C*PT*WARNING* Constant already double-precision
      Y2SUM = 0.d0
C*PT*WARNING* Constant already double-precision
      XYSUM = 0.d0
      DO N = 1,NPTS
          IF (X(N).NE.DMSG .AND. Y(N).NE.DMSG) THEN
C*PT*WARNING* Already double-precision (DBLE)
              XSUM = XSUM + DBLE(X(N))
C*PT*WARNING* Already double-precision (DBLE)
              YSUM = YSUM + DBLE(Y(N))
C*PT*WARNING* DPROD found - result may be incorrect
              X2SUM = X2SUM + X(N)*X(N)
C*PT*WARNING* DPROD found - result may be incorrect
              Y2SUM = Y2SUM + Y(N)*Y(N)
C*PT*WARNING* DPROD found - result may be incorrect
              XYSUM = XYSUM + X(N)*Y(N)
              XYN = XYN + 1.D0
          END IF
      END DO

      NPTXY = XYN

      IF (XYN.LT.1.D0) THEN
C all msg values
          IER = 5
          RETURN
      ELSE IF (XYN.LT.3.D0) THEN
C not enough data
          IER = 6
          RETURN
      END IF
C*PT*WARNING* Non-reversible tranformation (SNGL)

      XAVE = XSUM/XYN
C*PT*WARNING* Non-reversible tranformation (SNGL)
      YAVE = YSUM/XYN
C*PT*WARNING* Already double-precision (DBLE)

      XVAR = X2SUM - XSUM*XSUM/DBLE(XYN)
C*PT*WARNING* Already double-precision (DBLE)
      YVAR = Y2SUM - YSUM*YSUM/DBLE(XYN)
C*PT*WARNING* Already double-precision (DBLE)
      XYVAR = XYSUM - XSUM*YSUM/DBLE(XYN)
C*PT*WARNING* Non-reversible tranformation (SNGL)

C regression coef (b in book)
      RCOEF = XYVAR/XVAR
C sum of squares due to regression
      SSQREG = XYVAR* (XYVAR/XVAR)
C*PT*WARNING* Already double-precision (DBLE)
      SSQ = (YVAR-SSQREG)/DBLE(XYN-2.D0)
C v[b] in book {variance of B}
      VB = SSQ/XVAR
      SQRTVB = DSQRT(VB)
C*PT*WARNING* Non-reversible tranformation (SNGL)
C t-statistic
      TVAL = (RCOEF-RNULL)/DSQRT(VB)
C degrees of freedom
      DF = XYN - 2.D0

      REGDBG = .false.
      IF (REGDBG) THEN
          WRITE (*,FMT='(///,'' trtest: debug'')')
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' xsum  ='',f12.5)') XSUM
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' ysum  ='',f12.5)') YSUM
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' x2sum ='',f12.5)') X2SUM
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' y2sum ='',f12.5)') Y2SUM
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' xysum ='',f12.5)') XYSUM
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' xvar  ='',f12.5)') XVAR
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' yvar  ='',f12.9)') YVAR
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' xyvar ='',f12.9)') XYVAR
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' ssqreg='',f12.9)') SSQREG
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' ssq   ='',f12.9)') SSQ
          WRITE (*,FMT='('' rcoef ='',f12.9)') RCOEF
          WRITE (*,FMT='('' rnull ='',f12.9)') RNULL
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' vb    ='',f15.9)') VB
C*PT*WARNING* Non-reversible tranformation (SNGL)
          WRITE (*,FMT='('' sqrtvb='',f15.9)') SQRTVB
          WRITE (*,FMT='('' tval  ='',f15.9)') TVAL
      END IF

      RETURN
      END
