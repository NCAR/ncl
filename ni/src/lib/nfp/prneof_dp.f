c ---------------------------------------------------------
      SUBROUTINE DDRVEOF(X,NROW,NCOL,NOBS,MSTA,XMSG,NEVAL,EVAL,EVEC,
     +                  PCVAR,TRACE,IOPT,JOPT,CSSM,LCSSM,WORK,LWORK,
     +                  WEVAL,IWORK,LIWORK,IFAIL,LIFAIL,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION TRACE
      DOUBLE PRECISION TOL
      DOUBLE PRECISION DEPSMACH
      DOUBLE PRECISION VLOW
      DOUBLE PRECISION VUP

c driver to calculate :
C*PL*ERROR* Comment line too long
c .   the principal components (eignvalues and corresponding eigenvectors)
c .       of the data array x. x may contain missing observations.
c .       if it has msg data, this will calculate a var-cov matrix
c .       but it may not be positive definite.

c .       USES LAPACK/BLAS ROUTINES

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
c .   neval     - no. of eigenvalues and eigen vectors to be computed.
C*PL*ERROR* Comment line too long
c .               neval <= msta , if not msta eigenvalues and eigenvectors
c .               will be computed and ier = -2.
c .   eval      - vector containing the eigenvalues in DESCENDING order.
c .               eval must be at least neval in length.
c .   evec      - an array which will contains the eigenvector info.
c .               this must be dimensioned at least (ncol,neval) in the
c .               calling routine. There is some normalization done
c .               but I am not sure waht it is.
C*PL*ERROR* Comment line too long
c .   pcvar     - contains the % variance associated with each eigenvalue.
c .               this must be dimensioned at least neval in length.
c .   trace     - trace of the variance - covariance matrix.in the
c .               case of a var-covar matrix , the trace should
c .               equal the sum of all possible eigenvalues.
c .   iopt      - not used; kept for compatibility with old routine
c .   jopt      - =  0 : use var-covar matrix in prncmp
c .                  1 : use correlation matrix in prncmp
c .   cssm      - real vector to hold the covariance/correlation matrix
c .   lcssm     - length of cssm =msta*(msta+1)/2
c .   work      - real vector
c .   weval     - = msta
c .   lwork     - = 8*msta
c .   iwork     - integer vector
c .   liwork    - = 5*msta
c .   ifail     - integer vector
c .   lifail    - = msta
c .   ier       - error code

c NOTE: upon return if one want to get loadings,
c       something like this should be tried.
c
c           to calculate the coef or amplitude vector
c           associated with the n th eigenvector
c           mult the anomaly matrix (datam) times the
c           transpose of the eigenvector
c               meval = neval
c               do n=1,meval
c                  evwrk(n) = 0.
c                 do nyr=1,nyrs
c                    evyran(nyr,n) = 0.0
c                    evyrpv(nyr,n) = 0.0
c                   do m=1,msta
c                      if (datam(nyr,m).ne.xmsg) then
c                          temp = evec(m,n)*datam(nyr,m)
c                          evyran(nyr,n) = evyran(nyr,n) + temp
c                          evyrpv(nyr,n) = evyrpv(nyr,n) + temp*temp
c                          evwrk(n) = evwrk(n) + temp*temp
c                      endif
c                   enddo
c                 enddo
c               .
c               enddo


      DOUBLE PRECISION X(1:NROW,1:NCOL),EVAL(*),EVEC(1:NCOL,*)
      REAL PCVAR(*)
      INTEGER*8 LCSSM
      DOUBLE PRECISION CSSM(LCSSM),WORK(LWORK),WEVAL(LIFAIL)
      INTEGER IWORK(LIWORK),IFAIL(LIFAIL)

      CHARACTER*16 LABEL
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION TEMP

      DATA IPR/6/
      DATA IPRFLG/1/
c calculate covariance or correlation matrix in symmetric storage mode

      IER = 0
      IF (JOPT.EQ.0) THEN
          CALL DVCMSSM(X,NROW,NCOL,NOBS,MSTA,XMSG,CSSM,LSSM,IER)
      ELSE
          CALL DCRMSSM(X,NROW,NCOL,NOBS,MSTA,XMSG,CSSM,LSSM,IER)
      END IF
      IF (IER.NE.0) THEN
          WRITE (IPR,FMT=
     +'(//'' sub drveof: ier,jopt= '',2i3
     +   ,'' returned from vcmssm/crmssm'')') IER,JOPT
          IF (IER.GT.0) RETURN
      END IF

c activate if print of cov/cor matrix [work] is desired

      IF (IPRFLG.EQ.2) THEN
          IF (JOPT.EQ.0) THEN
              LABEL(1:15) = 'covar matrix:  '
          ELSE
              LABEL(1:15) = 'correl matrix: '
          END IF
          WRITE (IPR,FMT='(//,a15,''sym storage mode'')') LABEL(1:15)
          CALL DSSMIOX(CSSM,MSTA)
      END IF

c calculate the trace  before it is destroyed by sspevx

      NA = 0
C*PT*WARNING* Already double-precision (DBLE)
      TEMP = DBLE(0.D0)
      DO NN = 1,MSTA
          NA = NA + NN
C*PT*WARNING* Already double-precision (DBLE)
          TEMP = TEMP + DBLE(CSSM(NA))
      END DO
C*PT*WARNING* Already double-precision (DBLE)
      IF (TEMP.EQ.DBLE(0.D0)) THEN
          IER = -88
          WRITE (IPR,FMT=
     +'(//'' sub drveof: ier,jopt= '',2i3
     +   ,'' trace=0.0'')') IER,JOPT
          RETURN
      END IF
      TRACE = TEMP

c calculate the specified number of eigenvalues and the corresponding
c .   eigenvectors.

      MEVAL = MIN(NEVAL,MSTA)

      TOL = 10.D0*DEPSMACH(IPR)
      VLOW = 0.0D0
      VUP = 0.0D0
      ILOW = MAX(MSTA-MEVAL+1,1)
      IUP = MSTA
      CALL DSPEVX('V','I','U',MSTA,CSSM,VLOW,VUP,ILOW,IUP,TOL,MEVOUT,
     +            WEVAL,EVEC,NCOL,WORK,IWORK,IFAIL,INFO)

      IF (INFO.NE.0) THEN
          IER = IER + INFO
          WRITE (IPR,FMT=
     +'(//,'' sub drveof: sspevx error: info=''
     +      ,   i9)') INFO
          WRITE (IPR,FMT=
     +'(   '' sub drveof: sspevx error: ifail=''
     +      ,   20i5)') (IFAIL(I),I=1,MIN(20,MSTA))
      END IF

c reverse the order of things from "sspevx" so
c .   largest eigenvalues/vectors are first.

      DO N = 1,MEVAL
          WORK(N) = WEVAL(N)
      END DO
      DO N = 1,MEVAL
          EVAL(N) = WORK(NEVAL+1-N)
      END DO

      DO N = 1,MSTA
          DO NN = 1,NEVAL
              WORK(NN) = EVEC(N,NN)
          END DO
          DO NN = 1,NEVAL
              EVEC(N,NN) = WORK(NEVAL+1-NN)
          END DO
      END DO
C
C Convert EVEC and EVAL to single precision real
C
      DO N = 1,MEVAL
          PCVAR(N) = REAL(EVAL(N)/TRACE)*100.
      END DO

      RETURN
      END

      SUBROUTINE DNCLDRV(XX,X,NROW,NCOL,NOBS,NSTA,XMSG,NEVAL,EVAL,EVEC,
     +                 PCVAR,TRACE,IOPT,JOPT,PCRIT,EVECX,CSSM,LCSSM,
     +                 WORK,LWORK,WEVAL,IWORK,LIWORK,IFAIL,LIFAIL,IER)
      DOUBLE PRECISION PCRITX

      INTEGER NROW,NCOL,NOBS,NSTA,NEVAL,IOPT,JOPT,IER
      INTEGER*8 LCSSM
      INTEGER LWORK,LIWORK,LIFAIL
      DOUBLE PRECISION XX(NROW,NCOL),EVAL(NEVAL),EVEC(NCOL,NEVAL),
     +                 PCRIT,XMSG,TRACE,X(NROW,NCOL),
     +                 EVECX(NCOL,NEVAL)
      DOUBLE PRECISION CSSM(LCSSM),WORK(LWORK),WEVAL(LIFAIL)
      REAL PCVAR(NEVAL)
      INTEGER IWORK(LIWORK),IFAIL(LIFAIL)

      DATA IPR/6/
      DATA IPRFLG/1/

      PCRITX = PCRIT*0.01D0
c                                   ! counts the total number of
c                                   ! locations with > pcrit non=msg
      MSTA = 0
      DO NC = 1,NSTA
c                                   ! counter for this location
          KNT = 0
          DO NR = 1,NOBS
              IF (XX(NR,NC).NE.XMSG) THEN
                  KNT = KNT + 1
              END IF
          END DO
          IF (DBLE(KNT)/DBLE(NOBS).GE.PCRITX) THEN
              MSTA = MSTA + 1
              DO IROW = 1,NROW
                  X(IROW,MSTA) = XX(IROW,NC)
              END DO
          END IF
      END DO

c      write (*,'(//'' sub dncldrv: nrow,ncol,nobs,msta= ''
c     1              ,4i3)') nrow,ncol,nobs,msta
      CALL DDRVEOF(X,NROW,NCOL,NOBS,MSTA,XMSG,NEVAL,EVAL,EVECX,PCVAR,
     +             TRACE,IOPT,JOPT,CSSM,LCSSM,WORK,LWORK,WEVAL,IWORK,
     +             LIWORK,IFAIL,LIFAIL,IER)

c before returning put the evecs in the correct location

C preset the return array to msg
      DO NE = 1,NEVAL
          DO NC = 1,NCOL
              EVEC(NC,NE) = XMSG
          END DO
      END DO
c                                   ! counts the total number of
c                                   ! locations with > pcrit non=msg
      MSTA = 0
      DO NC = 1,NSTA
c counter for this location
          KNT = 0
          DO NR = 1,NOBS
              IF (XX(NR,NC).NE.XMSG) THEN
                  KNT = KNT + 1
              END IF
          END DO
          IF (DBLE(KNT)/DBLE(NOBS).GE.PCRITX) THEN
              MSTA = MSTA + 1
              DO IEVAL = 1,NEVAL
                  EVEC(NC,IEVAL) = EVECX(MSTA,IEVAL)
              END DO
          END IF
      END DO

      RETURN
      END
