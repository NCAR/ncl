C
C $Id: pj06dp.f,v 1.4 2001-08-16 23:12:54 kennison Exp $
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
