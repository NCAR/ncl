C
C $Id: pj08sp.f,v 1.2 2000-07-12 16:23:57 haley Exp $
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
      SUBROUTINE PJ08SP (COORD,CRDIO,INDIC)
C
C -- E Q U I D I S T A N T   C O N I C
C
      IMPLICIT REAL (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C ** PARAMETERS * A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,E0,E1,E2,E3,NS,GL,RH0
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC08SP/ A,LON0,X0,Y0,E0,E1,E2,E3,GL,NS,RH0
      DATA ZERO,ONE /0.0E0,1.0E0/
      DATA EPSLN /1.0E-10/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         ML = MLFNSP (E0,E1,E2,E3,GEOG(2))
         RH = A * (GL - ML)
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
         ML = GL - RH / A
         GEOG(2) = PHI3SP (ML,E0,E1,E2,E3)
         IF (IERR .EQ. 0) GO TO 340
         IERR = 085
         RETURN
  340    GEOG(1) = ADJLSP (LON0 + THETA / NS)
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
