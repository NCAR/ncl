C
C $Id: mpiola.f,v 1.4 2000-08-22 15:04:03 haley Exp $
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
      INTEGER FUNCTION MPIOLA (IAID,ILVL)
C
        PARAMETER (MNAI=6000)
C
C The value of MPIOLA(IAID,ILVL) is the area identifier of the largest
C area, at level ILVL, that contains the area with area identifier IAID.
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        ITMP=IAID
        NSTP=0
C
  101   IF (IPAR(ITMP).GE.1.AND.IPAR(ITMP).LE.MNAI) THEN
          IF (IATY(IPAR(ITMP)).GE.ILVL) THEN
            ITMP=IPAR(ITMP)
            NSTP=NSTP+1
            IF (NSTP.LT.10) GO TO 101
          END IF
        END IF
C
        MPIOLA=ITMP
C
        RETURN
C
      END
