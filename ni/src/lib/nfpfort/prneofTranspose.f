C NCLFORTSTART
      SUBROUTINE TDRVPRC(X,NROW,NCOL,NROBS,NCSTA,XMSG,NEVAL,EVAL,EVEC,
     +                   PCVAR,TRACE,IOPT,JOPT,PCRIT,IER,XDATA,XDATAT)
      IMPLICIT NONE

c this operates on the TRANSPOSE of the array "x"
c .   It results in [sometimes, MUCH] faster execution
c Note for NCL: in *this* routine NROBS=NROW, NCSTA=NCOL

      INTEGER          NROW,NCOL,NROBS,NCSTA,NEVAL,IOPT,JOPT,IER
      DOUBLE PRECISION X(NROW,NCOL),EVAL(NEVAL),EVEC(NCOL,NEVAL),
     +                 PCVAR(NEVAL),TRACE,XMSG,PCRIT,XDATA(NROW,NCOL),
     +                 XDATAT(NCSTA,NROW)
C NCLEND

c======== .so info=====================
c SGI/dataproc: WRAPIT -L /usr/lib64 -l complib.sgimath_mp prneofTranspose.f
c Sun/CGD: WRAPIT -L /opt/SUNWspro/lib -l sunmath -l fsu -l fui -lsunperf prneofTranspose.f
c======================================
     

c AUTOMATIC FORTRAN ARRAYS TO HOLD DATA AND DATA STATS
      DOUBLE PRECISION XAVE(NCOL), XVAR(NCOL), XDVAR(NCOL)
               

C LOCAL VARIABLES
      DOUBLE PRECISION PCX, CON, XSD
      INTEGER          KNTX,KER,NR,NC,MCSTA


c strip all grid points that have less that "PCRIT" valid values
c .   create the "XDATA". This may have fewer columns/grid-pts
c .   than the original "X" array if not all columns 
c .   had the minimun number of valid values.

      MCSTA = 0
      DO NC = 1,NCOL

c statistics for this station/grid-point

          CALL DSTAT2(X(1,NC),NROW,XMSG,XAVE(NC),XVAR(NC),XSD,KNTX,KER)

c quick way of eliminating stations/grid-points

          PCX = (DBLE(KNTX)/DBLE(NROW))*100.D0
          IF (PCX.LT.PCRIT .OR. XSD.LE.0.0D0) THEN
              XAVE(NC) = XMSG
          END IF

c possible normalizing for jopt=1

          CON = 1.0D0
          IF (JOPT.EQ.1 .AND. XAVE(NC).NE.XMSG 
     +                  .AND. XSD.GT.0.D0) CON = 1.D0/XSD

c WORK WITH ANOMALIES: XDAVE=0.0 [or standardized anomalies]

          IF (XAVE(NC).NE.XMSG) THEN
              MCSTA = MCSTA + 1

              DO NR = 1,NROW
                  IF (X(NR,NC).NE.XMSG) THEN
                      XDATA(NR,MCSTA) = (X(NR,NC)-XAVE(NC))*CON
c c c                 XDATA(NR,MCSTA) =  X(NR,NC)
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

c pass the selected data (XDATA) to the EOF driver 

      CALL XRVEOFT(XDATA,XDATAT,NROW,NCOL,NROBS,MCSTA,XMSG,
     +             NEVAL,EVAL,EVEC,PCVAR,TRACE,
     +             XDVAR,XAVE,JOPT,IER)

      RETURN
      END

c ---------------------------------------------------------
      SUBROUTINE XRVEOFT(XDATA,XDATAT,NROW,NCOL,NROBS,NCSTA,
     +                   XMSG,NEVAL,EVAL,EVEC,PCVAR,TRACE,
     +                   XDVAR,XAVE,JOPT,IER)
      IMPLICIT NONE

C **** USES AUTOMATIC ARRAYS SO USE WITH g77 or f90 

c operate on the *TRANSPOSE* OF XDATA: then use matrix stuff to
c .   get the desired eof information.

c Note: NROW=NROBS but, it is possible for  MCSTA<=NCOL
c .                     when some stations/grid_pts do not have enough data

c driver to calculate :
c .   the principal components (eignvalues and eigenvectors)
c .       of the data array XDATA. XDATA may contain
c .       missing observations. If it has msg data, this will
c .       calculate a var-cov matrix but it may not be
c .       positive definite.

c . The eigenvectors and eigenvalues of the *TRANSPOSED* matrix
c .      are used to derive the corresponding eigenvectors
c .      associated with the original matrix.

c .       USES LAPACK/BLAS ROUTINES

c nomenclature :
c .   xdata     - matrix containing the data. it contains n observations
c .               for each of m stations or grid pts.
c .   nrow,ncol - exact row (observation) and column (station) 
c .               dimensions of xdata in the calling routine.
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
c .   ier       - error code

      INTEGER          NROW,NCOL,NROBS,NCSTA,NEVAL,
     +                 LSSM,LWORK,LIWORK,JOPT,IER

      DOUBLE PRECISION XDATA(NROW,NCOL),PCVAR(NEVAL),XMSG,TRACE,
     +                 EVAL(NEVAL), EVEC(NCOL,NEVAL),
     +                 XDATAT(NCSTA,NROW)

c temporary arrays (automatic or passed in via interface)

      DOUBLE PRECISION CSSM(NROW*(NROW+1)/2),
     *                 WORK(8*NROW)
      INTEGER          IWORK(5*NROW)

      DOUBLE PRECISION TEOFPC(NROW,NEVAL),
     +                 WEVAL(NROW),WEVEC(NCSTA,NEVAL),
     +                 W2D(NROW,NEVAL), XAVE(NCOL),XDVAR(NCOL)
      INTEGER          IFAIL(NROBS)

c local
      CHARACTER*16     LABEL
      DOUBLE PRECISION TEMP,TOL,EPSMACH,VLOW,VUP
      INTEGER          N,NE,NR,NC,MEVAL,IPR,IPRFLG,
     +                 MEVOUT,ILOW,IUP,INFO,MCSTA

      DATA IPR/6/
      DATA IPRFLG/0/

c length of various temporary arrays

      LSSM   = NROW* (NROW+1)/2
      LWORK  = 8*NROW
      LIWORK = 5*NROW

c EXPLICITLY INITALIZE

      DO NR = 1,NROW
          WEVAL(NR) = XMSG
      END DO

      DO NE = 1,NEVAL
          EVAL(NE) = XMSG

          DO NC = 1,NCOL
              EVEC(NC,NE)  = XMSG
          END DO

          DO NC = 1,NCSTA 
              WEVEC(NC,NE) = XMSG
          END DO 

          DO NR = 1,NROW 
              W2D(NR,NE)  = XMSG
              TEOFPC(NR,NE) = XMSG
          END DO
      END DO

c create transposed data array

      DO NC=1,NCSTA
          DO NR=1,NROW
              XDATAT(NC,NR) = XDATA(NR,NC)
          END DO
      END DO

c compute the covariance matrix using the transposed data 

      CALL DVCMSSM(XDATAT,NCSTA,NROW,NCSTA,NROW,XMSG,CSSM,LSSM,IER)

      IF (IER.NE.0) THEN
          WRITE (IPR,FMT='(//'' sub drveoft: ier= '',i3
     +   ,'' returned from vcmssm/crmssm'')') IER
          IF (IER.GT.0) RETURN
      END IF

c activate if print of cov/cor matrix [work] is desired

      IF (IPRFLG.EQ.2) THEN
          IF (JOPT.EQ.0) THEN
              LABEL(1:15) = 'DRVEOFT: covar matrix:  '
          ELSE
              LABEL(1:15) = 'DRVEOFT: correl matrix: '
          END IF
          WRITE (IPR,"(//,a15,'sym storage mode')") LABEL(1:15)
          CALL DSSMIOX(CSSM,NROW)
      END IF

c calculate the trace of transposed matrix (TRACE)
c before matrix is destroyed by sspevx

      N = 0
      TRACE = 0.d0
      DO NR = 1,NROW
          N = N + NR
          TRACE = TRACE + CSSM(N)
      END DO
      IF (TRACE.LE.0.d0) THEN
          IER = -88
          WRITE (IPR,FMT='(//'' SUB DRVEOFT: ier,jopt= '',2i3
     +           ,'' trace='',f15.5)') IER,JOPT,TRACE
          RETURN
      END IF


c calculate specified number of eigenvalues and eigenvectors.
c .   make sure that  neval <= nrow (=nrobs)
c
c .   Remember the TEOFPC are the eofs of the *transposed* matrix.
c .   This means they are the principal components of the original data. 

      MEVAL = MIN(NEVAL,NROW)

      TOL   = 10.D0*EPSMACH(IPR)
      VLOW  = 0.0D0
      VUP   = 0.0D0
      ILOW  = MAX(NROBS-MEVAL+1,1)
      IUP   = NROBS
      MEVOUT = 0

      CALL DSPEVX('V','I','U',NROW,CSSM,VLOW,VUP,ILOW,IUP,TOL,MEVOUT,
     +            WEVAL,TEOFPC,NROW,WORK,IWORK,IFAIL,INFO)

      IF (INFO.NE.0) THEN
          IER = IER + INFO
          WRITE (IPR,FMT='(//,'' SUB DRVEOFT: sspevx error: info=''
     +                       ,   i9)') INFO
          WRITE (IPR,FMT='(   '' SUB DRVEOFT: sspevx error: ifail=''
     +      ,   20i5)') (IFAIL(N),N=1,MIN(20,NROBS))
      END IF

c reorder so eigenvalues are in descending order
c .   make sure the asocciated eigenvectors are also reordered

      DO N = 1,MEVAL
          WORK(N) = WEVAL(N)
      END DO

      DO N = 1,MEVAL
          WEVAL(N) = WORK(MEVAL+1-N)
      END DO

      DO N = 1,MEVAL
          DO NR = 1,NROW
              W2D(NR,N) = TEOFPC(NR,N)
          END DO
      END DO

      DO N = 1,MEVAL
          DO NR = 1,NROW
              TEOFPC(NR,N) = W2D(NR,MEVAL+1-N)
          END DO
      END DO

c percent variance explained (PCVAR) of transposed matrix.
c .   Both the eigenvalues (hence, PCVAR) are equivalent
c .   due to "duality" od space/time.

      DO N = 1,MEVAL
          PCVAR(N) = (WEVAL(N)/TRACE)*100.D0
      END DO

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c section to invert from transposed data space to original data space.
c ie: compute the eigenvectors of the original data
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c calculate the SPATIAL eofs from the TEOFPC and XDATAT

      DO N = 1,MEVAL
          DO NC = 1,NCSTA
              WEVEC(NC,N) = 0.0d0

              DO NR = 1,NROW
                  IF (XDATAT(NC,NR).NE.XMSG) THEN
                      WEVEC(NC,N) = WEVEC(NC,N) 
     +                            + TEOFPC(NR,N)*XDATAT(NC,NR)
                  END IF
              END DO

          END DO
      END DO

c normalize spatial eofs to variance=1.0
c normalize the time series [principal components] 

      DO N = 1,MEVAL

          TEMP  = 0.0d0
          DO NC = 1,NCSTA
              TEMP = TEMP + WEVEC(NC,N)*WEVEC(NC,N)
          END DO
          TEMP = DSQRT(TEMP)

          DO NC = 1,NCSTA
              WEVEC(NC,N) = WEVEC(NC,N)/TEMP
          END DO

      END DO

c return only the requested number of eigenvalues

      DO N = 1,NEVAL
          EVAL(N) = WEVAL(N)
          DO NC = 1,NCOL
              EVEC(NC,N) = XMSG
          END DO
      END DO

c return the requested eigenvector
c .   the purpose of the "if" is to reassign to correct locations

      MCSTA = 0
      DO NC = 1,NCOL
          IF (XAVE(NC).NE.XMSG) THEN
              MCSTA = MCSTA + 1
              DO N = 1,NEVAL
                  EVEC(NC,N) = WEVEC(MCSTA,N)
              END DO
          END IF
      END DO

      RETURN
      END
