C NCLFORTSTART
      SUBROUTINE DRMSD(X,Y,NPTS,XMSG,YMSG,XYRMSD,NPTUSE,IER)
      IMPLICIT NONE
c
c NCL:    rmsd = dim_rmsd(x,y)
c         interface must chk that the length of x and y are the same
c                                         input
      INTEGER NPTS
      DOUBLE PRECISION X(NPTS),Y(NPTS),XMSG,YMSG
c                                         output
      INTEGER NPTUSE,IER
      DOUBLE PRECISION XYRMSD
C NCLEND

c this routine will calculate estimates of the first two  moments
c .   of the vector x containing missing data.

c input arguments:
c .   x,y      - input vectors
c .   npts     - length of x and y
c .   xmsg,ymsg- missing code: if there are no msg values
c .                            set xmsg to some value which will
c .                            not be encountered.

c output arguments:
c .   xyrmsd   - root-mean-square-difference
c .   nptuse  - no. of points used to calculate the estimates
c .   ier      - if (ier.ne.0) an error has occurred

c note :
c .   uncalculated quantities are set to xmsg (nptuse set to zero)

      INTEGER N
      DOUBLE PRECISION RN

      NPTUSE = 0
      XYRMSD = XMSG

      IF (NPTS.LT.1) THEN
          IER = 1
          RETURN
      END IF

      IER = 0
      XYRMSD = 0.D0
      RN = 0.D0
      DO N = 1,NPTS
          IF (X(N).NE.XMSG .AND. Y(N).NE.YMSG) THEN
              RN = RN + 1.0D0
              XYRMSD = XYRMSD + (X(N)-Y(N))**2
          END IF
      END DO

      NPTUSE = RN

      IF (RN.GT.0.D0) THEN
          XYRMSD = SQRT(XYRMSD/RN)
      ELSE
          XYRMSD = XMSG
C error code for all msg values
          IER = 2
      END IF

      RETURN
      END
