C NCLFORTSTART
      SUBROUTINE TDRVPRC(X,NROW,NCOL,NROBS,NCSTA,XMSG,NEVAL,EVAL,EVEC,
     +                   PCVAR,TRACE,IOPT,JOPT,PCXMIN,IREVERT,IER)
      IMPLICIT NONE

c this operates on the TRANSPOSE of the array "x"
c .   It results in [sometimes, MUCH] faster execution

      INTEGER NROW,NCOL,NROBS,NCSTA,NEVAL,IOPT,JOPT,IREVERT,IER
      DOUBLE PRECISION X(NROW,NCOL),EVAL(NEVAL),EVEC(NCOL,NEVAL),
     +                 PCVAR(NEVAL),TRACE,XMSG,PCXMIN
C NCLEND

c======== .so info=====================
c SGI/dataproc: WRAPIT -L /usr/lib64 -l complib.sgimath_mp prneofTranspose.f
c Sun/CGD: WRAPIT -L /opt/SUNWspro/lib -l sunmath -l fsu -l fui -lsunperf prneofTranspose.f
c======================================

c temporary arrays (automatic ... ?add to interface?) and variables

      INTEGER LSSM,LWORK,LIWORK
      DOUBLE PRECISION CSSM(NROBS* (NROBS+1)/2),WORK(8*NROBS),
     +                 TEOF(NROBS,NEVAL),WEVAL(NROW),W2D(NROBS,NEVAL),
     +                 WEVEC(NCOL,NEVAL),CON,PCX,XDATA(NROW,NCOL),
     +                 XDVAR(NCOL),XAVE(NCOL),XVAR(NCOL),XSD

      INTEGER IWORK(5*NROBS),IFAIL(NROBS)
      INTEGER K,KNTX,NE,NR,NC,MCSTA

c initialize

      LSSM   = NROBS* (NROBS+1)/2
      LWORK  = 8*NROBS
      LIWORK = 5*NROBS

      DO NR = 1,NROW
          WEVAL(NR) = XMSG
      END DO

      DO NE = 1,NEVAL
          EVAL(NE) = XMSG

          DO NC = 1,NCOL
              EVEC(NC,NE) = XMSG
              WEVEC(NC,NE) = XMSG
          END DO

          DO NR = 1,NROBS
              W2D(NR,NE) = XMSG
              TEOF(NR,NE) = XMSG
          END DO
      END DO

      MCSTA = 0
      DO NC = 1,NCSTA

c statistics for this station/grid-point

          CALL DSTAT2(X(1,NC),NROBS,XMSG,XAVE(NC),XVAR(NC),XSD,KNTX,IER)

c quick way of eliminating stations/grid-points

          PCX = (DBLE(KNTX)/DBLE(NROBS))*100.D0
          IF (PCX.LT.PCXMIN .OR. XSD.LE.0.0D0) THEN
              XAVE(NC) = XMSG
          END IF

c possible normalizing for jopt=1

          CON = 1.0D0
          IF (JOPT.EQ.1 .AND. XAVE(NC).NE.XMSG) CON = 1.D0/XSD

c work with anomalies: XDAVE=0.0 [or standardized anomalies]

          IF (XAVE(NC).NE.XMSG) THEN
              MCSTA = MCSTA + 1

              DO NR = 1,NROBS
                  IF (X(NR,NC).NE.XMSG) THEN
                      XDATA(NR,MCSTA) = (X(NR,NC)-XAVE(NC))*CON
                  ELSE
                      XDATA(NR,MCSTA) = XMSG
                  END IF
              END DO

              IF (JOPT.EQ.0) THEN
                  XDVAR(MCSTA) = XVAR(NC)
              ELSE
                  XDVAR(MCSTA) = 1.0D0
              END IF
          END IF

      END DO

      CALL XRVEOFT(XDATA,NROW,NCOL,NROBS,MCSTA,XMSG,NEVAL,WEVAL,WEVEC,
     +             PCVAR,TRACE,IOPT,JOPT,CSSM,LSSM,WORK,LWORK,IWORK,
     +             LIWORK,IFAIL,TEOF,W2D,XDVAR,IREVERT,IER)

      DO K = 1,NEVAL
          EVAL(K) = WEVAL(K)
          DO NC = 1,NCSTA
              EVEC(NC,K) = XMSG
          END DO
      END DO

      MCSTA = 0
      DO NC = 1,NCSTA
          IF (XAVE(NC).NE.XMSG) THEN
              MCSTA = MCSTA + 1
              DO K = 1,NEVAL
                  EVEC(NC,K) = WEVEC(MCSTA,K)
              END DO
          END IF
      END DO

      RETURN
      END
c ---------------------------------------------------------
      SUBROUTINE XRVEOFT(X,NROW,NCOL,NROBS,NCSTA,XMSG,NEVAL,EVAL,EVEC,
     +                   PCVAR,TRACE,IOPT,JOPT,CSSM,LSSM,WORK,LWORK,
     +                   IWORK,LIWORK,IFAIL,TEOF,W2D,XVAR,IREVERT,IER)
      IMPLICIT NONE

c operate on the *TRANSPOSE* OF X: then use matrix stuff to
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
c .       USES AUTOMATIC ARRAYS SO USE WITH F90 (or Cray f77)

c nomenclature :
c .   x         - matrix containing the data. it contains n observations
c .               for each of m stations or grid pts.
c .   nrow,ncol - exact row (observation) and column (station) 
c .               dimensions of x in the calling routine.
c .   nrobs     - actual number of observations (nrobs <= nrow)
c .   ncsta     - actual number of stations     (ncsta <= ncol)
c .   xmsg      - missing code (if no obs are missing set to some
c .               number which will not be encountered)
c .   neval     - no. of eigenvalues and eigenvectors to be computed.
c .               neval <= min(nrobs,ncsta). 
c .               If not, ncsta eigenvalues and eigenvectors
c .               will be computed and ier = -2.
c .   eval      - vector containing the eigenvalues in DESCENDING order.
c .               eval must be at least neval in length.
c .   evec      - an array which will contains the eigenvector info.
c .               this must be dimensioned at least (ncol,neval) in the
c .               calling routine. There is some normalization done
c .               but I am not sure waht it is.
c .   pcvar     - contains % variance associated with each eigenvalue.
c .               This must be dimensioned at least neval in length.
c .   trace     - trace of the variance - covariance matrix.in the
c .               case of a var-covar matrix , the trace should
c .               equal the sum of all possible eigenvalues.
c .   iopt      - not used; kept for compatibility with old routine
c .   jopt      - =  0 : use var-covar matrix in prncmp
c .                  1 : use correlation matrix in prncmp
c .   irevert   - switch for returned eigenvalues
c .               0=return_transposed_info
c .               1=return_original_matrix_info [default]
c .   ier       - error code

      INTEGER NROW,NCOL,NROBS,NCSTA,NEVAL,IOPT,JOPT,LSSM,LWORK,LIWORK
      INTEGER IWORK(LIWORK),IFAIL(NROBS),IREVERT,IER

      DOUBLE PRECISION X(NROW,NCOL),EVAL(NEVAL),EVEC(NCOL,NEVAL),
     +                 PCVAR(NEVAL),XMSG,TRACE

c temporary arrays (automatic or passed in via interface)

      DOUBLE PRECISION CSSM(LSSM),WORK(LWORK),TEOF(NROW,NEVAL),
     +                 W2D(NROW,NEVAL), XVAR(NCOL),TOTVAR

      CHARACTER*16     LABEL
      DOUBLE PRECISION TEMP,TOL,EPSMACH
      INTEGER N,NR,NC,MEVAL,KNT,IPR,IPRFLG,MEVOUT,VLOW,VUP,ILOW,IUP,INFO
      DOUBLE PRECISION TBAR,TVAR,TSTD,TOL,EPSMACH

      DATA IPR/6/
      DATA IPRFLG/0/

c calculate covariance or correlation matrix in symmetric storage mode
c .   of the *TRANSPOSE* of the *anomalies* of the input data matrix
c .   JOPT=1 ... already handled in driver ... use covariance routine

      IER = 0
c c c IF (JOPT.EQ.0) THEN
          CALL DVCMSSMT(X,NROW,NCOL,NROBS,NCSTA,XMSG,CSSM,LSSM,IER)
c c c ELSE
c c c     CALL DCRMSSMT(X,NROW,NCOL,NROBS,NCSTA,XMSG,CSSM,LSSM,IER)
c c c END IF

      IF (IER.NE.0) THEN
          WRITE (IPR,FMT='(//'' sub drveoft: ier,jopt= '',2i3
     +   ,'' returned from vcmssm/crmssm'')') IER,JOPT
          IF (IER.GT.0) RETURN
      END IF

c activate if print of cov/cor matrix [work] is desired

      IF (IPRFLG.EQ.2) THEN
          IF (JOPT.EQ.0) THEN
              LABEL(1:15) = 'DRVEOFT: covar matrix:  '
          ELSE
              LABEL(1:15) = 'DRVEOFT: correl matrix: '
          END IF
          WRITE (IPR,FMT='(//,a15,''sym storage mode'')') LABEL(1:15)
          CALL DSSMIOX(CSSM,NROBS)
      END IF

c calculate the trace of transposed matrix
c before matrix is destroyed by sspevx

      N = 0
      TEMP = 0.d0
      DO NR = 1,NROBS
          N = N + NR
          TEMP = TEMP + CSSM(N)
      END DO
      IF (TEMP.LE.0.d0) THEN
          IER = -88
          WRITE (IPR,FMT='(//'' SUB DRVEOFT: ier,jopt= '',2i3
     +     ,'' trace='',fe15.5)') IER,JOPT,TRACE
          RETURN
      END IF
      TRACE = TEMP

c calculate specified number of eigenvalues and eigenvectors.
c .   make sure that  neval <= nrobs

      MEVAL = MIN(NEVAL,NROBS)

      TOL   = 10.D0*EPSMACH(IPR)
      VLOW  = 0.0D0
      VUP   = 0.0D0
      ILOW  = MAX(NROBS-MEVAL+1,1)
      IUP   = NROBS
      MEVOUT = 0

      CALL SSPEVX('V','I','U',NROBS,CSSM,VLOW,VUP,ILOW,IUP,TOL,MEVOUT,
     +            EVAL,TEOF,NROW,WORK,IWORK,IFAIL,INFO)

      IF (INFO.NE.0) THEN
          IER = IER + INFO
          WRITE (IPR,FMT='(//,'' SUB DRVEOFT: sspevx error: info=''
     +                       ,   i9)') INFO
          WRITE (IPR,FMT='(   '' SUB DRVEOFT: sspevx error: ifail=''
     +      ,   20i5)') (IFAIL(N),N=1,MIN(20,NROBS))
      END IF

c reverse the order of things from "sspevx" so
c .   largest eigenvalues/vectors are first.

      DO N = 1,MEVAL
          WORK(N) = EVAL(N)
      END DO

      DO N = 1,MEVAL
          EVAL(N) = WORK(MEVAL+1-N)
      END DO

      DO N = 1,MEVAL
          DO NR = 1,NROBS
              W2D(NR,N) = TEOF(NR,N)
          END DO
      END DO

      DO N = 1,MEVAL
          DO NR = 1,NROBS
              TEOF(NR,N) = W2D(NR,MEVAL+1-N)
          END DO
      END DO

c percent variance explained of *transposed* matrix

      DO N = 1,MEVAL
          PCVAR(N) = (EVAL(N)/TRACE)*100.D0
      END DO

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c section to invert from transposed grid to 'original' grid.
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c calculate the DESIRED eofs from the TRANSPOSED
c remember: x(nr,nc) are anomalies

      DO N = 1,MEVAL
          DO NC = 1,NCSTA
              TEMP = 0.0d0

              DO NR = 1,NROBS
                  IF (X(NR,NC).NE.XMSG .AND. TEOF(NR,N).NE.XMSG) THEN
                      TEMP = TEMP + TEOF(NR,N)*X(NR,NC)
                  END IF
              END DO

              EVEC(NC,N) = TEMP
          END DO
      END DO

c remove the mean from evec

      DO N = 1,MEVAL
          CALL DSTAT2(EVEC(1,N),NCSTA,XMSG,TBAR,TVAR,TSTD,KNT,IER)
          DO NC = 1,NCSTA
              EVEC(NC,N) = EVEC(NC,N) - TBAR
          END DO
      END DO

c normalize by euclidean norm

      DO N = 1,MEVAL
          TEMP  = 0.0d0
          DO NC = 1,NCSTA
              TEMP = TEMP + EVEC(NC,N)*EVEC(NC,N)
          END DO
          TEMP = DSQRT(TEMP)

          DO NC = 1,NCSTA
              EVEC(NC,N) = EVEC(NC,N)/TEMP
          END DO
      END DO

      IF (IREVERT.EQ.1) THEN

c Basically, here we try to create the eigenvalues that would have
c .   been created using the untransposed data matrix
c .   totvar is the same as the trace of the *original* matrix
c .          same as the sum of the diagonal elements

          IF (JOPT.EQ.0) THEN
              TOTVAR = 0.D0
              DO NC = 1,NCSTA
                  TOTVAR = TOTVAR + XVAR(NC)
              END DO
          ELSE
              TOTVAR = NCSTA
          END IF

          TRACE = TOTVAR

c calculate the variance of the 'original' amplitude time series
c These would be the eigenvalues of the original matrix
c To avoid confusion, these will be set to the eigenvalues for return

          DO N = 1,MEVAL
              DO NR = 1,NROBS
                  TEMP  = 0.0d0
                  DO NC = 1,NCSTA
                      IF (X(NR,NC).NE.XMSG) THEN
                          TEMP = TEMP + DBLE(EVEC(NC,N)*X(NR,NC))
                      END IF
                  END DO
                  WORK(NR) = TEMP
              END DO

              CALL DSTAT2(WORK,NROBS,XMSG,TBAR,TVAR,TSTD,KNT,IER)
              EVAL(N)  = TVAR
              PCVAR(N) = (TVAR/TRACE)*100.D0

          END DO
      END IF

      RETURN
      END
c ----------------------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DVCMSSMT(X,NROW,NCOL,NRT,NCS,XMSG,VCM,LVCM,IER)
      IMPLICIT NONE

      INTEGER NROW,NCOL,NRT,NCS,LVCM,IER
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
c     xmsg     - missing data code 
c                (if none set to some no. not encountered)
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
              XN    = 0.d0
              XRA   = 0.d0
              XRB   = 0.d0
              XRARB = 0.d0
              NN = NN + 1

              IF (NN.GT.LVCM) THEN
                  PRINT *,'VCMSSMT: NN > LVCM: STOP'
                  STOP
              END IF

              DO I = 1,NCS
                  IF (X(NRA,I).NE.XMSG .AND. X(NRB,I).NE.XMSG) THEN
                      XN = XN + 1.d0
                      XRA = XRA + X(NRA,I)
                      XRB = XRB + X(NRB,I)
                      XRARB = XRARB + X(NRA,I)*X(NRB,I)
                  END IF
              END DO
              IF (XN.GE.2.D0) THEN
                  VCM(NN) = (XRARB- (XRA*XRB)/XN)/ (XN-1.d0)
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
      SUBROUTINE DCRMSSMT(X,NROW,NCOL,NRT,NCS,XMSG,CRM,LCRM,IER)
      IMPLICIT NONE

      INTEGER NROW,NCOL,NRT,NCS,LCRM,IER
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
c     xmsg     - missing data code 
c .              (if none set to some no. not encountered)
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
              XN    = 0.D0
              XRA   = 0.D0
              XRB   = 0.D0
              XRA2  = 0.D0
              XRB2  = 0.D0
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
                      CRM(NN) = CRM(NN)/ (DSQRT(XVARA)*DSQRT(XVARB))
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
