      SUBROUTINE DCALCORC(X,XAVE,XSTD,XMSG,NXY,Y,YMSG,CORC,IER)
      IMPLICIT NONE

c this routine will calculate the cross correlation coef between
c .   series x and series y
c this routine was originally meant to be in the inner portion
c .   of a loop

c     x         - input data 1D array  ( unchanged on output)
c     xave,xstd - ave and st. dev of x (input)
c     xmsg      - missing data code (if none set to some no. not
c                 encountered)
c     nxy       - length of x and y 1D vectors
c     y         - input data 1D array  ( unchanged on output)
c     ymsg      - missing data code (if none set to some no. not
c                 encountered)
c     corc      - correlation coef
c     ier       - error code (if ier=-1 then corc contains missing
c                 entry)
c --------------- input arguments
      INTEGER NXY
      DOUBLE PRECISION X(NXY),Y(NXY),XAVE,XSTD,XMSG,YMSG
c --------------- output arguments
      INTEGER IER
      DOUBLE PRECISION CORC
c --------------- local stuff
      INTEGER N,NPTUSX,NPTUSY
      DOUBLE PRECISION XVAR,YAVE,YVAR,YSTD,RN,XY,COVAR

      CORC = XMSG
      IF (XAVE.EQ.XMSG .OR. XSTD.EQ.XMSG) THEN
          CALL DSTAT2(X,NXY,XMSG,XAVE,XVAR,XSTD,NPTUSX,IER)
          IF (IER.NE.0 .OR. XSTD.EQ.0.0D0) THEN
              IER = 201
              RETURN
          END IF
      END IF

c calculate the mean and var of y

      CALL DSTAT2(Y,NXY,YMSG,YAVE,YVAR,YSTD,NPTUSY,IER)
      IF (IER.NE.0) RETURN
      IF (YSTD.EQ.0.D0) THEN
          IER = 203
          RETURN
      END IF

c calculate the cross correlation coef
c .   calculate the mean and variance of column nc in the data array

      RN = 0.D0
      XY = 0.D0
      DO N = 1,NXY
          IF (X(N).NE.XMSG .AND. Y(N).NE.YMSG) THEN
              RN = RN + 1.D0
              XY = XY + (Y(N)-YAVE)* (X(N)-XAVE)
          END IF
      END DO
      IF (RN.GT.2.D0) THEN
          COVAR = XY/ (RN-1.D0)
          CORC = COVAR/ (XSTD*YSTD)
      END IF

      RETURN
      END

      SUBROUTINE DCALCOVC(X,XAVE,XSTD,XMSG,NXY,Y,YMSG,COVC,IER)
      IMPLICIT NONE

c this routine will calculate the cross covariance between
c .   series x and series y 
c this routine was originally meant to be in the inner portion
c .   of a loop

c THIS ROUTINE DOES NOT NEED XSTD BUT IT WAS DUPLICTED
c FROM DCALCORC AND I WANTED TO MINIMIZE ANY WORK MaryH
c WOULD HAVE TO DO. SHE CAN BASICALLY DUPLICATE THE INTERFACE
c FOR DCALCORC.

c     x         - input data 1D array  ( unchanged on output)
c     xave,xstd - ave and st. dev of x (input)
c     xmsg      - missing data code (if none set to some no. not
c                 encountered)
c     nxy       - length of x and y 1D vectors
c     y         - input data 1D array  ( unchanged on output)
c     ymsg      - missing data code (if none set to some no. not
c                 encountered)
c     corc      - correlation coef
c     ier       - error code (if ier=-1 then corc contains missing
c                 entry)
c --------------- input arguments
      INTEGER NXY
      DOUBLE PRECISION X(NXY),Y(NXY),XAVE,XSTD,XMSG,YMSG
c --------------- output arguments
      INTEGER IER
      DOUBLE PRECISION COVC
c --------------- local stuff
      INTEGER N,NPTUSX,NPTUSY
      DOUBLE PRECISION XVAR,YAVE,YVAR,YSTD,RN,XY,COVAR

      COVC = XMSG
C C C IF (XAVE.EQ.XMSG .OR. XSTD.EQ.XMSG) THEN
      IF (XAVE.EQ.XMSG) THEN
          CALL DSTAT2(X,NXY,XMSG,XAVE,XVAR,XSTD,NPTUSX,IER)
          IF (IER.NE.0 .OR. XSTD.EQ.0.0D0) THEN
              IER = 201
              RETURN
          END IF
      END IF

c calculate the mean and var of y

      CALL DSTAT2(Y,NXY,YMSG,YAVE,YVAR,YSTD,NPTUSY,IER)
      IF (IER.NE.0) RETURN
      IF (YSTD.EQ.0.D0) THEN
          IER = 203
          RETURN
      END IF

c calculate the cross covariance 
c .   calculate the mean and variance of column nc in the data array

      RN = 0.D0
      XY = 0.D0
      DO N = 1,NXY
          IF (X(N).NE.XMSG .AND. Y(N).NE.YMSG) THEN
              RN = RN + 1.D0
              XY = XY + (Y(N)-YAVE)* (X(N)-XAVE)
          END IF
      END DO
      IF (RN.GT.2.D0) THEN
          COVC = XY/ (RN-1.D0)
      END IF

      RETURN
      END
