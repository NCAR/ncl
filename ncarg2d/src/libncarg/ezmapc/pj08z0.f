C
C $Id: pj08z0.f,v 1.1 1999-04-02 23:05:58 kennison Exp $
C
      SUBROUTINE PJ08Z0 (COORD,CRDIO,INDIC)
C
C -- E Q U I D I S T A N T   C O N I C
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C ** PARAMETERS * A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,E0,E1,E2,E3,NS,GL,RH0
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ08/ A,LON0,X0,Y0,E0,E1,E2,E3,GL,NS,RH0
      DATA ZERO,ONE /0.0D0,1.0D0/
      DATA EPSLN /1.0D-10/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         ML = MLFNZ0 (E0,E1,E2,E3,GEOG(2))
         RH = A * (GL - ML)
         THETA = NS * ADJLZ0 (GEOG(1) - LON0)
         PROJ(1) = X0 + RH * SIN (THETA)
         PROJ(2) = Y0 + RH0 - RH * COS (THETA)
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
         Y = RH0 - PROJ(2) + Y0
         RH = SIGN (SQRT (X * X + Y * Y) , NS)
         THETA = ZERO
         CON = SIGN (ONE , NS)
         IF (RH .NE. ZERO) THETA = ATAN2 (CON * X , CON * Y)
         ML = GL - RH / A
         GEOG(2) = PHI3Z0 (ML,E0,E1,E2,E3)
         IF (IERROR .EQ. 0) GO TO 340
         IERROR = 085
         RETURN
  340    GEOG(1) = ADJLZ0 (LON0 + THETA / NS)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
