C
C $Id: pj06z0.f,v 1.1 1999-04-02 23:05:57 kennison Exp $
C
      SUBROUTINE PJ06Z0 (COORD,CRDIO,INDIC)
C
C -- P O L A R   S T E R E O G R A P H I C
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER IND
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,LATC,X0,Y0,E4,MCS,TCS,FAC,IND *******
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ06/ A,LON0,X0,Y0,E,E4,FAC,MCS,TCS,IND
      DATA ZERO,ONE,TWO /0.0D0,1.0D0,2.0D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         CON1 = FAC * ADJLZ0 (GEOG(1) - LON0)
         CON2 = FAC * GEOG(2)
         SINPHI = SIN (CON2)
         TS = TSFNZ0 (E,CON2,SINPHI)
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
         IERROR = 0
         X = FAC * (PROJ(1) - X0)
         Y = FAC * (PROJ(2) - Y0)
         RH = SQRT (X * X + Y * Y)
         IF (IND .EQ. 0) GO TO 340
         TS = RH * TCS / (A * MCS)
         GO TO 360
  340    TS = RH * E4 / (TWO * A)
  360    GEOG(2) = FAC * PHI2Z0 (E,TS)
         IF (IERROR .EQ. 0) GO TO 380
         IERROR = 064
         RETURN
  380    IF (RH .NE. ZERO) GO TO 400
         GEOG(1) = FAC * LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  400    GEOG(1) = ADJLZ0 (FAC * ATAN2 (X , -Y) + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
