C
C $Id: pj03sp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJ03SP (COORD,CRDIO,INDIC)
C
C -- A L B E R S   C O N I C A L   E Q U A L   A R E A
C
      IMPLICIT REAL (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,NS,C,RH0 *******
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC03SP/ A,LON0,X0,Y0,C,E,ES,NS,RH0
      DATA TOL /1.0E-7/
      DATA HALFPI /1.5707963267948966E0/
      DATA ZERO,HALF,ONE /0.0E0,0.5E0,1.0E0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         SINPHI = SIN (GEOG(2))
         COSPHI = COS (GEOG(2))
         QS = QSFNSP (E,SINPHI,COSPHI)
         RH = A * SQRT (C - NS * QS) / NS
         THETA = NS * ADJLSP (GEOG(1) - LON0)
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
         IERR = 0
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
  260    GEOG(2) = PHI1SP (E,QS)
         IF (IERR .EQ. 0) GO TO 280
         IERR = 035
         RETURN
  280    GEOG(1) = ADJLSP (THETA / NS + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
