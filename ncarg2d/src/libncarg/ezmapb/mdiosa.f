C
C $Id: mdiosa.f,v 1.1 2001-08-16 23:10:46 kennison Exp $
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
      INTEGER FUNCTION MDIOSA (IAID,ILVL)
C
        INTEGER IAID,ILVL
C
        PARAMETER (MNAI=6000)
C
C The value of MDIOSA(IAID,ILVL) is the area identifier of the smallest
C area, at level ILVL, that contains the area with area identifier IAID.
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
C Declare local variables.
C
        INTEGER          ITMP,NSTP
C
        ITMP=IAID
        NSTP=0
C
  101   IF (IATY(ITMP).GT.ILVL) THEN
          IF (IPAR(ITMP).GE.1.AND.IPAR(ITMP).LE.MNAI) THEN
            IF (IATY(IPAR(ITMP)).NE.0) THEN
              ITMP=IPAR(ITMP)
              NSTP=NSTP+1
              IF (NSTP.LT.10) GO TO 101
            END IF
          END IF
        END IF
C
        MDIOSA=ITMP
C
        RETURN
C
      END
