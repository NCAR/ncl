      SUBROUTINE DRVEOFT(X,NROW,NCOL,NROBS,NCSTA,XMSG,NEVAL,EVAL,EVEC,
     +                   PCVAR,TRACE,IOPT,JOPT,CSSM,LCSSM,WORK,LWORK,
     +                   IWORK,LIWORK,IFAIL,LIFAIL,TEOF,WEVAL,IER)

      IMPLICIT NONE
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION TRACE
      DOUBLE PRECISION TOL
      DOUBLE PRECISION EPSMACH
      DOUBLE PRECISION VLOW
      DOUBLE PRECISION VUP
      DOUBLE PRECISION XBAR
      DOUBLE PRECISION XVAR
      DOUBLE PRECISION XSTD
      DOUBLE PRECISION CON
      DOUBLE PRECISION EVNORM
      DOUBLE PRECISION EVSQRT

c operate on the *TRANSPOSE* OF X: then use matrix stuff t
c .   get the desired eof information.

c driver to calculate :
c .   the principal components (eignvalues and eigenvectors)
c .       of the data array x. x may contain
c .       missing observations. If it has msg data, this will
c .       calculate a var-cov matrix but it may not be
c .       positive definite.

c . The eigenvectors and eigenvalues of the *TRANSPOSED* matrix
c .      are used to derive the corresponding values
c .      associated with the original matrix.

c .       USES LAPACK/BLAS ROUTINES

c nomenclature :
c .   x         - matrix containing the data.  it contains n observations
c .               for each of m stations or grid pts.
c .   nrow,ncol - exact row (observation) and column (station) dimensions
c .               of x in the calling routine.
c .   nrobs     - actual number of observations (nrobs <= nrow)
c .   ncsta     - actual number of stations     (ncsta <= ncol)
c .   xmsg      - missing code (if no obs are missing set to some
c .               number which will not be encountered)
c .   neval     - no. of eigenvalues and eigen vectors to be computed.
c .               neval <= ncsta , if not ncsta eigenvalues and eigenvectors
c .               will be computed and ier = -2.
c .   eval      - vector containing the eigenvalues in DESCENDING order.
c .               eval must be at least neval in length.
c .   evec      - an array which will contains the eigenvector info.
c .               this must be dimensioned at least (ncol,neval) in the
c .               calling routine. There is some normalization done
c .               but I am not sure what it is.
c .   pcvar     - contains the % variance associated with each eigenvalue.
c .               this must be dimensioned at least neval in length.
c .   trace     - trace of the variance - covariance matrix.in the
c .               case of a var-covar matrix , the trace should
c .               equal the sum of all possible eigenvalues.
c .   iopt      - not used; kept for compatibility with old routine
c .   jopt      - =  0 : use var-covar matrix in prncmp
c .                  1 : use correlation matrix in prncmp
c .   ier       - error code

      INTEGER NROW,NCOL,NROBS,NCSTA,NEVAL,IOPT,JOPT,IER
      DOUBLE PRECISION X(NROW,NCOL),EVAL(NEVAL),EVEC(NCOL,NEVAL)
      REAL             PCVAR(NEVAL)

c temporary arrays (automatic or passed in via interface)

      INTEGER LWORK, LIWORK, LIFAIL
      INTEGER*8 LCSSM
      DOUBLE PRECISION CSSM(LCSSM),WORK(LWORK),TEOF(NROBS,NEVAL)
      DOUBLE PRECISION WEVAL(LIFAIL)
      INTEGER IWORK(LIWORK),IFAIL(NROBS)
      CHARACTER*16 LABEL
      DOUBLE PRECISION TEMP
      INTEGER NA, N, NN, NR, NC, MEVAL, ILOW, IUP
      INTEGER MEVOUT, INFO, IPR, IPRFLG, KNTX

      DATA IPR/6/
      DATA IPRFLG/1/

c calculate covariance or correlation matrix in symmetric storage mode
c .   of the *TRANSPOSE* of the input data matrix

      IER = 0
      IF (JOPT.EQ.0) THEN
          CALL TVCMSSM(X,NROW,NCOL,NROBS,NCSTA,XMSG,CSSM,LCSSM,IER)
      ELSE
          CALL TCRMSSM(X,NROW,NCOL,NROBS,NCSTA,XMSG,CSSM,LCSSM,IER)
      END IF
      IF (IER.NE.0) THEN
C          WRITE (IPR,FMT=
C     +'(//'' sub drveoft: ier,jopt= '',2i3
C     +   ,'' returned from vcmssm/crmssm'')') IER,JOPT
c fatal: input dimension error
          IF (IER.GT.0) RETURN
      END IF

c activate if print of cov/cor matrix [work] is desired

      IF (IPRFLG.EQ.2) THEN
          IF (JOPT.EQ.0) THEN
              LABEL(1:15) = 'DRVEOFT: covar matrix:  '
          ELSE
              LABEL(1:15) = 'DRVEOFT: correl matrix: '
          END IF
C          WRITE (IPR,FMT='(//,a15,''sym storage mode'')') LABEL(1:15)
          CALL SSMIOX(CSSM,NROBS)
      END IF

c calculate the trace  before it is destroyed by dspevx

      NA = 0
C*PT*WARNING* Already double-precision (DBLE)
      TEMP = DBLE(0.D0)
      DO NN = 1,NROBS
          NA = NA + NN
          TEMP = TEMP + CSSM(NA)
      END DO
C*PT*WARNING* Already double-precision (DBLE)
      IF (TEMP.EQ.DBLE(0.D0)) THEN
          IER = -88
C          WRITE (IPR,FMT=
C     +'(//'' SUB DRVEOFT: ier,jopt= '',2i3
C     +     ,'' trace=0.0'')') IER,JOPT
          RETURN
      END IF
      TRACE = DBLE(TEMP)

c calculate the specified number of eigenvalues and the corresponding
c .   eigenvectors.

c neval <= nrobs
      MEVAL = MIN(NEVAL,NROBS-1)

      TOL = 10.D0*EPSMACH(IPR)
c not used
      VLOW = 0.0D0
c not used
      VUP = 0.0D0
      ILOW = MAX(NROBS-MEVAL+1,1)
      IUP = NROBS
      MEVOUT = 0

      CALL DSPEVX('V','I','U',NROBS,CSSM,VLOW,VUP,ILOW,IUP,TOL,MEVOUT,
     +            WEVAL,TEOF,NROW,WORK,IWORK,IFAIL,INFO)

      IF (INFO.NE.0) THEN
          IER = IER + INFO
C          WRITE (IPR,FMT=
C     +'(//,'' SUB DRVEOFT: dspevx error: info=''
C     +      ,   i9)') INFO
C          WRITE (IPR,FMT=
C     +'(   '' SUB DRVEOFT: dspevx error: ifail=''
C     +      ,   20i5)') (IFAIL(I),I=1,MIN(20,NROBS))
      END IF

c reverse the order of things from "dspevx" so
c .   largest eigenvalues/vectors are first.

c eigenvalues
      DO N = 1,MEVAL
          WORK(N) = WEVAL(N)
      END DO

      DO N = 1,MEVAL
          EVAL(N) = WORK(NEVAL+1-N)
      END DO

c eigenvectors
      DO NR = 1,NROBS
          DO NN = 1,MEVAL
              WORK(NN) = TEOF(NR,NN)
          END DO
          DO NN = 1,MEVAL
              TEOF(NR,NN) = WORK(MEVAL+1-NN)
          END DO
      END DO

c % variance explained
      DO N = 1,MEVAL
          PCVAR(N) = REAL(EVAL(N)/TRACE)*100.0
      END DO

c ++++++++++++++++
c section to invert from transposed grid to 'original' grid.
c ++++++++++++++++

c calculate the eofs from the TRANSPOSED

      DO N = 1,MEVAL
          DO NC = 1,NCSTA
              EVEC(NC,N) = 0.0D0

              CALL DSTAT2(X(1,NC),NROBS,XMSG,XBAR,XVAR,XSTD,KNTX,IER)
              CON = 1.0D0
              IF (JOPT.EQ.1 .AND. XSTD.GT.0.D0) THEN
                  CON = 1.D0/XSTD
              END IF

              DO NR = 1,NROBS
                  IF (X(NR,NC).NE.XMSG .AND. TEOF(NR,N).NE.XMSG) THEN
                      EVEC(NC,N) = EVEC(NC,N) +
     +                             TEOF(NR,N)* (X(NR,NC)-XBAR)*CON
                  END IF
              END DO
          END DO
      END DO

      DO N = 1,MEVAL
          EVNORM = 0.0D0
          EVSQRT = SQRT(EVAL(N))

          DO NC = 1,NCSTA
              EVEC(NC,N) = EVEC(NC,N)/EVSQRT
          END DO

          DO NC = 1,NCSTA
              EVNORM = EVNORM + EVEC(NC,N)*EVEC(NC,N)
          END DO
          EVNORM = SQRT(EVNORM)

          DO NC = 1,NCSTA
              EVEC(NC,N) = EVEC(NC,N)/EVNORM
          END DO
      END DO

      RETURN
      END
c ----------------------------------------------------------------------

C NCLFORTSTART
      SUBROUTINE TVCMSSM(X,NROW,NCOL,NRT,NCS,XMSG,VCM,LVCM,IER)
      IMPLICIT NONE

      INTEGER*8 LVCM
      INTEGER NROW,NCOL,NRT,NCS,IER
      DOUBLE PRECISION X(NROW,NCOL),VCM(LVCM),XMSG
C NCLEND

c this routine will calculate the variance-couariance matrix (vcm)
c .   of the *TRANSPOSE* of the array x containing missing data.
c .   Obviously if x does contain missing data then vcm is
c .   only an approximation.

c note : symmetric storage mode is utilized for vcm to save space.

c input:
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt <= nrow : ncs <= ncol)
c     xmsg     - missing data code (if none set to some no. not encountered)
c output:
c     vcm      - var-cov matrix
c     lvcm     - length of vcm (lvcm=nrt*(nrt+1)/2)
c     ier      - error code (if ier=-1 then vcm contains missing entry)

      INTEGER NRA,NRB,I,NN
      DOUBLE PRECISION XN,XRA,XRB,XRARB

      IER = 0
      IF (NROW.LT.1 .OR. NCOL.LT.1) IER = IER + 1
      IF (NRT.LT.1 .OR. NCS.LT.1) IER = IER + 10
      IF (IER.NE.0) RETURN

c calculate the var-cov between rows (obs)

      NN = 0
      DO NRA = 1,NRT
          DO NRB = 1,NRA
              XN = 0.D0
              XRA = 0.D0
              XRB = 0.D0
              XRARB = 0.D0
              NN = NN + 1
              DO I = 1,NCS
                  IF (X(NRA,I).NE.XMSG .AND. X(NRB,I).NE.XMSG) THEN
                      XN = XN + 1.D0
                      XRA = XRA + X(NRA,I)
                      XRB = XRB + X(NRB,I)
                      XRARB = XRARB + X(NRA,I)*X(NRB,I)
                  END IF
              END DO
              IF (XN.GE.2.D0) THEN
                  VCM(NN) = (XRARB- (XRA*XRB)/XN)/ (XN-1.D0)
              ELSE
                  IER = -1
                  VCM(NN) = XMSG
              END IF
          END DO
      END DO

      RETURN
      END
c ----------------------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE TCRMSSM(X,NROW,NCOL,NRT,NCS,XMSG,CRM,LCRM,IER)
      IMPLICIT NONE

      INTEGER NROW,NCOL,NRT,NCS,IER
      INTEGER*8 LCRM
      DOUBLE PRECISION X(NROW,NCOL),CRM(LCRM),XMSG
C NCLEND

c this routine will calculate the correlation matrix   (crm)
c .   of the *TRANSPOSE* of array x containing missing data.
c .   Obviously if x does contain missing data then crm is
c .   only an approximation.

c note : symmetric storage mode is utilized for crm to save space.

c input:
c     x        - input data array  ( unchanged on output)
c     nrow,ncol- exact dimensions of x in calling routine
c     nrt,ncs  - dimension of sub-matrix which contains the data
c                (nrt <= nrow : ncs <= ncol)
c     xmsg     - missing data code (if none set to some no. not encountered)
c output:
c     crm      - correlation matrix
c     lcrm     - length of crm  (lcrm >= nrt*(nrt+1)/2)
c     ier      - error code (if ier=-1 then crm contains missing entry)

      INTEGER NRA,NRB,I,NN
      DOUBLE PRECISION XN,XRA,XRB,XRARB,XRA2,XRB2,XVARA,XVARB

      IER = 0
      IF (NROW.LT.1 .OR. NCOL.LT.1) IER = IER + 1
      IF (NRT.LT.1 .OR. NCS.LT.1) IER = IER + 10
      IF (IER.NE.0) RETURN

c calculate the var-cov between columns (stations)

      NN = 0
      DO NRA = 1,NRT
          DO NRB = 1,NRA
              XN = 0.D0
              XRA = 0.D0
              XRB = 0.D0
              XRA2 = 0.D0
              XRB2 = 0.D0
              XRARB = 0.D0
              NN = NN + 1
              DO I = 1,NCS
                  IF (X(NRA,I).NE.XMSG .AND. X(NRB,I).NE.XMSG) THEN
                      XN = XN + 1.D0
                      XRA = XRA + X(NRA,I)
                      XRB = XRB + X(NRB,I)
                      XRA2 = XRA2 + X(NRA,I)*X(NRA,I)
                      XRB2 = XRB2 + X(NRB,I)*X(NRB,I)
                      XRARB = XRARB + X(NRA,I)*X(NRB,I)
                  END IF
              END DO
              IF (XN.GE.2.D0) THEN
                  XVARA = (XRA2- ((XRA*XRA)/ (XN)))/ (XN-1.D0)
                  XVARB = (XRB2- ((XRB*XRB)/ (XN)))/ (XN-1.D0)
                  IF (XVARA.GT.0.D0 .AND. XVARB.GT.0.D0) THEN
                      CRM(NN) = (XRARB- ((XRA*XRB)/ (XN)))/ (XN-1.D0)
                      CRM(NN) = CRM(NN)/ (SQRT(XVARA)*SQRT(XVARB))
                  ELSE
                      IER = -1
                      CRM(NN) = XMSG
                  END IF
              ELSE
                  IER = -1
                  CRM(NN) = XMSG
              END IF
          END DO
      END DO

      RETURN
      END

      SUBROUTINE TNCLEOF(XX,NROW,NCOL,NOBS,NSTA,XMSG,NEVAL,EVAL,EVEC,
     +                   PCVAR,TRACE,IOPT,JOPT,PCRIT,X,EVECX,TEOF,
     +                   WEVAL,CSSM,LCSSM,WORK,LWORK,IWORK,LIWORK,
     +                   IFAIL,LIFAIL,IER)

      IMPLICIT NONE

      INTEGER NROW,NCOL,NOBS,NSTA,NEVAL,IOPT,JOPT,IER
      DOUBLE PRECISION XX(NROW,NCOL),EVAL(NEVAL),EVEC(NCOL,NEVAL),
     +                 PCRIT,XMSG,TRACE
      REAL             PCVAR(NEVAL)
      INTEGER LWORK,LIWORK,LIFAIL
      INTEGER*8 LCSSM

      DOUBLE PRECISION X(NROW,NCOL),EVECX(NCOL,NEVAL),TEOF(NOBS,NEVAL)
      DOUBLE PRECISION CSSM(LCSSM),WORK(LWORK),WEVAL(LIFAIL)
      INTEGER IWORK(LIWORK),IFAIL(LIFAIL)
      INTEGER IPR,IPRFLG,MSTA,KNT,NR,NC,NE
      DOUBLE PRECISION PCRITX

      DATA IPR/6/
      DATA IPRFLG/1/

      IF (PCRIT.GT.1.0D0) THEN
          PCRITX = PCRIT*0.01D0
      ELSE
          PCRITX = PCRIT
      END IF
c counts the total number of
c locations with > pcrit non=msg
c
      MSTA = 0
      DO NC = 1,NSTA
c counter for this location
c
          KNT = 0
          DO NR = 1,NOBS
              IF (XX(NR,NC).NE.XMSG) THEN
                  KNT = KNT + 1
              END IF
          END DO
          IF (DBLE(KNT)/DBLE(NOBS).GE.PCRITX) THEN
              MSTA = MSTA + 1
C             x(:,msta) = xx(:,nc)
              DO NR = 1,NROW
                  X(NR,MSTA) = XX(NR,NC)
              END DO
          END IF
      END DO

C      write (*,'(//'' sub tncldrv: nrow,ncol,nobs,msta= ''
C     1              ,4i3)') nrow,ncol,nobs,msta


      CALL DRVEOFT(X,NROW,NCOL,NOBS,MSTA,XMSG,NEVAL,EVAL,EVECX,
     +             PCVAR,TRACE,IOPT,JOPT,CSSM,LCSSM,WORK,LWORK,
     +             IWORK,LIWORK,IFAIL,LIFAIL,TEOF,WEVAL,IER)

c before returning put the evecs in the correct location

c preset the return array to msg
c
      DO NE = 1,NEVAL
          DO NC = 1,NCOL
              EVEC(NC,NE) = XMSG
          END DO
      END DO
c counts the total number of
c locations with > pcrit non=msg
c
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
C             evec(nc,:) = evecx(msta,:)
              DO NE = 1,NEVAL
                  EVEC(NC,NE) = EVECX(MSTA,NE)
              END DO
          END IF
      END DO

      RETURN
      END
