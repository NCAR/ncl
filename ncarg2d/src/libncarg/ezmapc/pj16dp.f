C
C $Id: pj16dp.f,v 1.2 2000-07-12 16:23:59 haley Exp $
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
      SUBROUTINE PJ16DP (COORD,CRDIO,INDIC)
C
C -- S I N U S O I D A L
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,X0,Y0 ************************************
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC16DP/ A,LON0,X0,Y0
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN /1.0D-10/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         LON = ADJLDP (GEOG(1) - LON0)
         PROJ(1) = X0 + A * LON * COS (GEOG(2))
         PROJ(2) = Y0 + A * GEOG(2)
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
         GEOG(2) = Y / A
         IF (ABS(GEOG(2)) .LE. HALFPI) GO TO 230
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,2020)
 2020    FORMAT (/' ERROR PJ16DP'/
     .            ' INPUT DATA ERROR')
         IERR = 164
         RETURN
  230    CON = ABS (GEOG(2)) - HALFPI
         IF (ABS (CON) .GT. EPSLN) GO TO 240
         GEOG(1) = LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    GEOG(1) = ADJLDP (LON0 + X / (A * COS (GEOG(2))))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
