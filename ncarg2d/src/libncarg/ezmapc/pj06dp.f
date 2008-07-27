C
C $Id: pj06dp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJ06DP (COORD,CRDIO,INDIC)
C
C -- P O L A R   S T E R E O G R A P H I C
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IND
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,LATC,X0,Y0,E4,MCS,TCS,FAC,IND *******
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC06DP/ A,LON0,X0,Y0,E,E4,FAC,MCS,TCS,IND
      DATA ZERO,ONE,TWO /0.0D0,1.0D0,2.0D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         CON1 = FAC * ADJLDP (GEOG(1) - LON0)
         CON2 = FAC * GEOG(2)
         SINPHI = SIN (CON2)
         TS = TSFNDP (E,CON2,SINPHI)
         IF (IND .EQ. 0) GO TO 240
         RH = A * MCS * TS / TCS
         GO TO 260
  240    RH = TWO * A * TS / E4
  260    PROJ(1) = X0 + FAC * RH * SIN (CON1)
         PROJ(2) = Y0 - FAC * RH * COS (CON1)
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
         X = FAC * (PROJ(1) - X0)
         Y = FAC * (PROJ(2) - Y0)
         RH = SQRT (X * X + Y * Y)
         IF (IND .EQ. 0) GO TO 340
         TS = RH * TCS / (A * MCS)
         GO TO 360
  340    TS = RH * E4 / (TWO * A)
  360    GEOG(2) = FAC * PHI2DP (E,TS)
         IF (IERR .EQ. 0) GO TO 380
         IERR = 064
         RETURN
  380    IF (RH .NE. ZERO) GO TO 400
         GEOG(1) = FAC * LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  400    GEOG(1) = ADJLDP (FAC * ATAN2 (X , -Y) + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
