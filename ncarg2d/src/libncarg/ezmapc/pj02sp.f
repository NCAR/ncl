C
C $Id: pj02sp.f,v 1.3 2000-08-22 15:04:17 haley Exp $
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
      SUBROUTINE PJ02SP (COORD,CRDIO,INDIC)
C
C -- S T A T E   P L A N E
C
      IMPLICIT REAL (A-Z)
      INTEGER ITYPE
      INTEGER INDIC, FWD, INV
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC02SP/ ITYPE
C
      PARAMETER (FWD=0, INV=1)
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
C
C     TRANSVERSE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 1) THEN
            CALL PJ09SP (GEOG,PROJ,FWD)
         END IF
C
C     LAMBERT CONFORMAL PROJECTION
C
         IF (ITYPE .EQ. 2) THEN
            CALL PJ04SP (GEOG,PROJ,FWD)
         END IF
C
C     POLYCONIC PROJECTION
C
         IF (ITYPE .EQ. 3) THEN
            CALL PJ07SP (GEOG,PROJ,FWD)
         END IF
C
C     OBLIQUE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 4) THEN
            CALL PJ20SP (GEOG,PROJ,FWD)
         END IF
C
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
C
C     TRANSVERSE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 1) THEN
            CALL PJ09SP (PROJ,GEOG,INV)
         END IF
C
C     LAMBERT CONFORMAL PROJECTION
C
         IF (ITYPE .EQ. 2) THEN
            CALL PJ04SP (PROJ,GEOG,INV)
         END IF
C
C     POLYCONIC PROJECTION
C
         IF (ITYPE .EQ. 3) THEN
            CALL PJ07SP (PROJ,GEOG,INV)
         END IF
C
C     OBLIQUE MERCATOR PROJECTION
C
         IF (ITYPE .EQ. 4) THEN
            CALL PJ20SP (PROJ,GEOG,INV)
         END IF
C
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
