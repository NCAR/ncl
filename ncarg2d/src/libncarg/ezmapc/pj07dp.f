C
C $Id: pj07dp.f,v 1.2 2000-07-12 16:23:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE PJ07DP (COORD,CRDIO,INDIC)
C
C -- P O L Y C O N I C
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LON0,LAT0,X0,Y0,E0,E1,E2,ML0 *************
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC07DP/ A,LON0,X0,Y0,E,E0,E1,E2,E3,ES,ML0
      DATA TOL /1.0D-7/
      DATA ZERO,ONE /0.0D0,1.0D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         CON = ADJLDP (GEOG(1) - LON0)
         IF (ABS(GEOG(2)) .GT. TOL) GO TO 240
         PROJ(1) = X0 + A * CON
         PROJ(2) = Y0 - A * ML0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
  240    SINPHI = SIN (GEOG(2))
         COSPHI = COS (GEOG(2))
         ML = MLFNDP (E0,E1,E2,E3,GEOG(2))
         MS = MSFNDP (E,SINPHI,COSPHI)
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
         CALL PHI4DP (ES,E0,E1,E2,E3,AL,B,C,GEOG(2))
         IF (IERR .EQ. 0) GO TO 360
         IERR = 074
         RETURN
  360    GEOG(1) = ADJLDP (ASINDP (X * C / A) / SIN (GEOG(2)) + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
