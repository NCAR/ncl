c --------------------------------------------------------------
      SUBROUTINE DEOFTS7(X,NROW,NCOL,NOBS,MSTA,XMSG,NEVAL,EVEC,JOPT,XX,
     +                  WRK,EVECTS,IER)
      DOUBLE PRECISION XBAR
      DOUBLE PRECISION XVAR
      DOUBLE PRECISION XSTD
      DOUBLE PRECISION CON

c f77

c takes output of prneof.f and creates a time series of the
c .   eof amplitudes

c nomenclature :
C*PL*ERROR* Comment line too long
c .   x         - matrix containing the data.  it contains n observations
c .               for each of m stations or grid pts.
C*PL*ERROR* Comment line too long
c .   nrow,ncol - exact row (observation) and column (station) dimensions
c .               of x in the calling routine.
c .   nobs      - actual number of observations (nobs <= nrow)
c .   msta      - actual number of stations     (msta <= ncol)
c .   xmsg      - missing code (if no obs are missing set to some
c .               number which will not be encountered)
C*PL*ERROR* Comment line too long
c .   neval     - no. of eigenvalues and eigen vectors computed by prneof.
c .   evec      - array created by prneof
c .               this must be dimensioned at least (ncol,neval) in the
c .               calling routine.
c .   jopt      - =0 means covariance  was used to generate evec
c .               =1 means correlation was used to generate evec
c .   xx        - work vector of the same dimensions as x
c .   wrk       - work vector of length nobs
c .   evects    - time series of eof amplitudes for each eigenvalue
c .   ier       - error code


      INTEGER NROW,NCOL,NOBS,MSTA

      DOUBLE PRECISION X(NROW,NCOL),EVEC(NCOL,NEVAL),XX(NROW,NCOL),
     +                 WRK(NOBS),XMSG
      INTEGER NEVAL
      DOUBLE PRECISION EVECTS(NROW,NEVAL)
      INTEGER IER

      IER = 0
      IF (NROW.LE.0 .OR. NCOL.LE.0) IER = IER + 1
      IF (NOBS.LE.0 .OR. MSTA.LE.0) IER = IER + 10
      IF (IER.NE.0) THEN
          WRITE (*,FMT='(/,'' sub eofts7: ier='',5i5)') IER,NROW,NCOL,
     +      NOBS,MSTA
          STOP
          RETURN
      END IF

c set to msg as default

      DO M = 1,NCOL
          DO N = 1,NROW
              XX(N,M) = XMSG
          END DO
      END DO

      DO K = 1,NEVAL
          DO M = 1,NROW
              EVECTS(M,K) = XMSG
          END DO
      END DO

c calculate each station/grid-pt  long-term mean and standard deviation

      DO M = 1,MSTA
          DO N = 1,NOBS
              WRK(N) = X(N,M)
          END DO

          CALL DSTAT2(WRK,NOBS,XMSG,XBAR,XVAR,XSTD,KNTX,IER)
          CON = 1.0D0
          IF (JOPT.EQ.1 .AND. XSTD.GT.0.D0) THEN
              CON = 1.D0/XSTD
          END IF

c c c    write (*,"(' eofts7: m,nobs,kntx,xbar,xstd=',3i5,2f8.2)")
c c c*                        m,nobs,kntx,xbar,xstd

          DO N = 1,NOBS
              IF (X(N,M).NE.XMSG .AND. XBAR.NE.XMSG) THEN
                  XX(N,M) = (X(N,M)-XBAR)*CON
              ELSE
                  XX(N,M) = XMSG
              END IF
          END DO

      END DO

c c c write (*,"(//,'ANOMAILIES',/)")
c c c do n=1,nobs
c c c     write (*,"(i5 , 10(1x,f9.3) )") n, (xx(n,m),m=1,msta)
c c c enddo

c calculate the amplitude time series

      DO K = 1,NEVAL
          DO N = 1,NOBS
              EVECTS(N,K) = 0.0D0
              DO M = 1,MSTA
                  IF (XX(N,M).NE.XMSG.AND.EVEC(M,K).NE.XMSG) THEN
                      EVECTS(N,K) = EVECTS(N,K) + EVEC(M,K)*XX(N,M)
                  END IF
              END DO
          END DO
      END DO

      RETURN
      END
