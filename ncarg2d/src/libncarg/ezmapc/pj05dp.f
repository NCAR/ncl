C
C $Id: pj05dp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJ05DP (COORD,CRDIO,INDIC)
C
C -- M E R C A T O R
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,X0,Y0,NS,F,RH0,LAT1,M1 **************
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC05DP/ A,LON0,X0,Y0,E,M1
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         IF (ABS(ABS(GEOG(2)) - HALFPI) .GT. EPSLN) GO TO 240
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT (/' ERROR PJ05DP'/
     .            ' TRANSFORMATION CANNOT BE COMPUTED AT THE POLES')
         IERR = 053
         RETURN
  240    SINPHI = SIN (GEOG(2))
         TS = TSFNDP (E,GEOG(2),SINPHI)
         PROJ(1) = X0 + A * M1 * ADJLDP (GEOG(1) - LON0)
         PROJ(2) = Y0 - A * M1 * LOG (TS)
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
         IERR = 0
         X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         TS = EXP (- Y / (A * M1))
         GEOG(2) = PHI2DP (E,TS)
         IF (IERR .EQ. 0) GO TO 280
         IERR = 055
         RETURN
  280    GEOG(1) = ADJLDP (LON0 + X / (A * M1))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
