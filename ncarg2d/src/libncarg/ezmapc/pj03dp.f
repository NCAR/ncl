C
C $Id: pj03dp.f,v 1.3 2000-08-22 15:04:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE PJ03DP (COORD,CRDIO,INDIC)
C
C -- A L B E R S   C O N I C A L   E Q U A L   A R E A
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,NS,C,RH0 *******
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC03DP/ A,LON0,X0,Y0,C,E,ES,NS,RH0
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
         IERR = 0
         SINPHI = SIN (GEOG(2))
         COSPHI = COS (GEOG(2))
         QS = QSFNDP (E,SINPHI,COSPHI)
         RH = A * SQRT (C - NS * QS) / NS
         THETA = NS * ADJLDP (GEOG(1) - LON0)
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
  260    GEOG(2) = PHI1DP (E,QS)
         IF (IERR .EQ. 0) GO TO 280
         IERR = 035
         RETURN
  280    GEOG(1) = ADJLDP (THETA / NS + LON0)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
