C
C $Id: pj07z0.f,v 1.1 1999-04-02 23:05:57 kennison Exp $
C
      SUBROUTINE PJ07Z0 (COORD,CRDIO,INDIC)
C
C -- P O L Y C O N I C
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,LAT0,X0,Y0,E0,E1,E2,ML0 *************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ07/ A,LON0,X0,Y0,E,E0,E1,E2,E3,ES,ML0
      DATA TOL /1.0D-7/
      DATA ZERO,ONE /0.0D0,1.0D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         CON = ADJLZ0 (GEOG(1) - LON0)
         IF (ABS(GEOG(2)) .GT. TOL) GO TO 240
         PROJ(1) = X0 + A * CON
         PROJ(2) = Y0 - A * ML0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
  240    SINPHI = SIN (GEOG(2))
         COSPHI = COS (GEOG(2))
         ML = MLFNZ0 (E0,E1,E2,E3,GEOG(2))
         MS = MSFNZ0 (E,SINPHI,COSPHI)
         CON = CON * SINPHI
         PROJ(1) = X0 + A * MS * SIN (CON) / SINPHI
         PROJ(2) = Y0 + A * (ML - ML0 + MS * (ONE - COS(CON)) / SINPHI)
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
         AL = ML0 + Y / A
         IF (ABS (AL) .GT. TOL) GO TO 340
         GEOG(1) = X / A + LON0
         GEOG(2) = ZERO
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  340    B = AL * AL + (X / A) ** 2
         CALL PHI4Z0 (ES,E0,E1,E2,E3,AL,B,C,GEOG(2))
         IF (IERROR .EQ. 0) GO TO 360
         IERROR = 074
         RETURN
  360    GEOG(1) = ADJLZ0 (ASINZ0 (X * C / A) / SIN (GEOG(2)) + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
