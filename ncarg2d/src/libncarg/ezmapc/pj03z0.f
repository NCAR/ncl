C
C $Id: pj03z0.f,v 1.1 1999-04-02 23:05:56 kennison Exp $
C
      SUBROUTINE PJ03Z0 (COORD,CRDIO,INDIC)
C
C -- A L B E R S   C O N I C A L   E Q U A L   A R E A
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,NS,C,RH0 *******
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ03/ A,LON0,X0,Y0,C,E,ES,NS,RH0
      DATA TOL /1.0D-7/
      DATA HALFPI /1.5707963267948966D0/
      DATA ZERO,HALF,ONE /0.0D0,0.5D0,1.0D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         SINPHI = SIN (GEOG(2))
         COSPHI = COS (GEOG(2))
         QS = QSFNZ0 (E,SINPHI,COSPHI)
         RH = A * SQRT (C - NS * QS) / NS
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
         CON = RH * NS / A
         QS = (C - CON * CON) / NS
         IF (E .LT. TOL) GO TO 260
         CON = ONE - HALF * (ONE - ES) * LOG ((ONE - E) /
     .         (ONE + E)) / E
         IF ((ABS(CON) - ABS(QS)) .GT. TOL) GO TO 260
         GEOG(2) = SIGN (HALFPI , QS)
         GO TO 280
  260    GEOG(2) = PHI1Z0 (E,QS)
         IF (IERROR .EQ. 0) GO TO 280
         IERROR = 035
         RETURN
  280    GEOG(1) = ADJLZ0 (THETA / NS + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
