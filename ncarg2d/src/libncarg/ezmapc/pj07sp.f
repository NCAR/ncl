C
C $Id: pj07sp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJ07SP (COORD,CRDIO,INDIC)
C
C -- P O L Y C O N I C
C
      IMPLICIT REAL (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,LAT0,X0,Y0,E0,E1,E2,ML0 *************
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC07SP/ A,LON0,X0,Y0,E,E0,E1,E2,E3,ES,ML0
      DATA TOL /1.0E-7/
      DATA ZERO,ONE /0.0E0,1.0E0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         CON = ADJLSP (GEOG(1) - LON0)
         IF (ABS(GEOG(2)) .GT. TOL) GO TO 240
         PROJ(1) = X0 + A * CON
         PROJ(2) = Y0 - A * ML0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
  240    SINPHI = SIN (GEOG(2))
         COSPHI = COS (GEOG(2))
         ML = MLFNSP (E0,E1,E2,E3,GEOG(2))
         MS = MSFNSP (E,SINPHI,COSPHI)
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
         IERR = 0
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
         CALL PHI4SP (ES,E0,E1,E2,E3,AL,B,C,GEOG(2))
         IF (IERR .EQ. 0) GO TO 360
         IERR = 074
         RETURN
  360    GEOG(1) = ADJLSP (ASINSP (X * C / A) / SIN (GEOG(2)) + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
