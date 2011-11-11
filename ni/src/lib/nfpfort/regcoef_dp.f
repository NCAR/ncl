c -------------------------------------------------------------
      SUBROUTINE DREGCOEF(X,Y,NPTS,XMSG,YMSG,RCOEF,TVAL,NPTXY,XAVE,
     +     YAVE,RSTD,YINT,IER)
      IMPLICIT NONE

c NCL:  rcoef = regCoef (x,y)
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
c .   xmsg,ymsg- missing code: if no msg values set to some number
c .                            which will not be encountered.
c .              dmsg will be used to fill missing values
c .   rcoef    - slope (trend ... regression coef)
c .   tval     - t-statistic (null hypothesis: H0: r=0.0)
c .   nptxy    - number of points used
c .   rstd     - standard deviation of the regression coefficient
c .   xave     - average of x
c .   yave     - average of y
c .   yint     - y-intercept
c .   ier      - if (ier.ne.0) an error has occurred

C input
      INTEGER NPTS,NPTXY
C input
      DOUBLE PRECISION X(1:NPTS),Y(1:NPTS),XMSG,YMSG
C output
      INTEGER IER
C output
      DOUBLE PRECISION RCOEF,TVAL,YAVE,XAVE,RSTD,YINT

C local
      LOGICAL REGDBG
      INTEGER N
      DOUBLE PRECISION RNULL,XYN,DF
      DOUBLE PRECISION XSUM,YSUM,X2SUM,Y2SUM,XYSUM,XVAR,YVAR,XYVAR,
     +                 SSQREG,SSQ,VB,SQRTVB

      IER = 0
      TVAL  = YMSG
      RCOEF = YMSG
      RSTD  = YMSG
      IF (NPTS.LT.2) IER = 1
      IF (IER.NE.0) RETURN

C null hypothesis
      RNULL = 0.0D0

      XYN = 0.0D0
      XSUM = 0.d0
      YSUM = 0.d0
      X2SUM = 0.d0
      Y2SUM = 0.d0
      XYSUM = 0.d0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG .AND. Y(N).NE.YMSG) THEN
              XSUM = XSUM + DBLE(X(N))
              YSUM = YSUM + DBLE(Y(N))
              X2SUM = X2SUM + X(N)*X(N)
              Y2SUM = Y2SUM + Y(N)*Y(N)
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

      XAVE = XSUM/XYN
      YAVE = YSUM/XYN

      XVAR = X2SUM - XSUM*XSUM/DBLE(XYN)
      YVAR = Y2SUM - YSUM*YSUM/DBLE(XYN)
      XYVAR = XYSUM - XSUM*YSUM/DBLE(XYN)

      IF (XVAR.GT.0.D0) then
C regression coef (b in book)
          RCOEF = XYVAR/XVAR
C sum of squares due to regression
          SSQREG = XYVAR* (XYVAR/XVAR)
          SSQ = (YVAR-SSQREG)/DBLE(XYN-2.D0)
C v[b] in book {variance of B}
          VB = SSQ/XVAR
          SQRTVB = DSQRT(VB)
C t-statistic and standard deviation of reg coef
          IF (SQRTVB.GT.0.D0) then
              TVAL = (RCOEF-RNULL)/SQRTVB
              RSTD = SQRTVB
          ELSE
              TVAL = YMSG
          END IF
C degrees of freedom
          DF = XYN - 2.D0
C y-intercept
          YINT = YAVE - RCOEF*XAVE
      ELSE
c XVAR = 0.0
          IER    = 7
          SSQREG = YMSG
          SSQ    = YMSG
          VB     = YMSG
          SQRTVB = YMSG
          TVAL   = YMSG
          RCOEF  = YMSG
          YINT   = YMSG
      END IF

      REGDBG = .false.
      IF (REGDBG) THEN
          WRITE (*,FMT='(///,'' trtest: debug'')')
          WRITE (*,FMT='('' xsum  ='',f12.5)') XSUM
          WRITE (*,FMT='('' ysum  ='',f12.5)') YSUM
          WRITE (*,FMT='('' x2sum ='',f12.5)') X2SUM
          WRITE (*,FMT='('' y2sum ='',f12.5)') Y2SUM
          WRITE (*,FMT='('' xysum ='',f12.5)') XYSUM
          WRITE (*,FMT='('' xvar  ='',f12.5)') XVAR
          WRITE (*,FMT='('' yvar  ='',f12.9)') YVAR
          WRITE (*,FMT='('' xyvar ='',f12.9)') XYVAR
          WRITE (*,FMT='('' ssqreg='',f12.9)') SSQREG
          WRITE (*,FMT='('' ssq   ='',f12.9)') SSQ
          WRITE (*,FMT='('' rcoef ='',f12.9)') RCOEF
          WRITE (*,FMT='('' rnull ='',f12.9)') RNULL
          WRITE (*,FMT='('' vb    ='',f15.9)') VB
          WRITE (*,FMT='('' rstd(=sqrtvb)='',f15.9)') RSTD   
          WRITE (*,FMT='('' tval  ='',f15.9)') TVAL
          WRITE (*,FMT='('' yint  ='',f15.9)') YINT
      END IF

      RETURN
      END


