      SUBROUTINE DRMVMEAN(X,NPTS,XMSG,IER)
      IMPLICIT NONE

c remove the series mean from a series
c NCL:  xNew = rmv_mean (x)

C input
      INTEGER NPTS,IER
      DOUBLE PRECISION XMSG
C input/output
      DOUBLE PRECISION X(NPTS)

C local
      INTEGER NPTUSE,N
      DOUBLE PRECISION XMEAN,XVAR,XSD

      CALL DSTAT2(X,NPTS,XMSG,XMEAN,XVAR,XSD,NPTUSE,IER)

      IF (IER.EQ.0) THEN
          DO N = 1,NPTS
              IF (X(N).NE.XMSG) X(N) = X(N) - XMEAN
          END DO
      END IF

      RETURN
      END
c -------------------------------------------------------------
      SUBROUTINE DRMVMED(X,WORK,NPTS,XMSG,IER)
      IMPLICIT NONE

c remove the series median from a series
c NCL:  xNew = rmv_median (x)

C input
      INTEGER NPTS,IER
      DOUBLE PRECISION XMSG
C input/output
      DOUBLE PRECISION X(NPTS)
      DOUBLE PRECISION WORK(NPTS)

C local
      INTEGER NPTUSE,N
      DOUBLE PRECISION XMEDIAN,XMRANGE,XRANGE

      CALL DMEDMRNG(X,WORK,NPTS,XMSG,XMEDIAN,XMRANGE,XRANGE,NPTUSE,IER)

      IF (IER.EQ.0) THEN
          DO N = 1,NPTS
              IF (X(N).NE.XMSG) X(N) = X(N) - XMEDIAN
          END DO
      END IF

      RETURN
      END
c -------------------------------------------------------------
      SUBROUTINE DXSTND(X,NPTS,XMSG,IOPT,IER)
      IMPLICIT NONE

c remove the mean from a series and standardize by the
c .   standardize by the standard deviation (sample or pop)

c NCL:  xNew = standardize(x,iopt)

C input
      INTEGER NPTS,IOPT,IER
      DOUBLE PRECISION XMSG
C input/output
      DOUBLE PRECISION X(NPTS)

C local
      INTEGER NPTUSE,N
      DOUBLE PRECISION XMEAN,XVAR,XSD,RN

      CALL DSTAT2(X,NPTS,XMSG,XMEAN,XVAR,XSD,NPTUSE,IER)

      IF (IOPT.EQ.1 .AND. NPTUSE.GT.1) THEN
          RN = NPTUSE
C population std deviation
          XSD = SQRT(((RN-1.D0)*XVAR)/RN)
      END IF

      IF (IER.EQ.0 .AND. XSD.GT.0.) THEN
          DO N = 1,NPTS
              IF (X(N).NE.XMSG) X(N) = (X(N)-XMEAN)/XSD
          END DO
      END IF

      RETURN
      END
