C
C $Id: pj17z0.f,v 1.1 1999-04-02 23:06:01 kennison Exp $
C
      SUBROUTINE PJ17Z0 (COORD,CRDIO,INDIC)
C
C -- E Q U I R E C T A N G U L A R
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,X0,Y0,LAT1 *******************************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ17/ A,LON0,X0,Y0,LAT1
      DATA HALFPI /1.5707963267948966D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         LON = ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + A * LON * COS(LAT1)
         PROJ(2) = Y0 + A * GEOG(2)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C -- I N V E R S E   . . .
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         GEOG(2) = Y / A
         IF (ABS(GEOG(2)) .LE. HALFPI) GO TO 240
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT (/' ERROR PJ17Z0'/
     .            ' INPUT DATA ERROR')
         IERROR = 174
         RETURN
  240    GEOG(1) = ADJLZ0 (LON0 + X / (A * COS(LAT1) ))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
