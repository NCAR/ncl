C
C $Id: mdipan.f,v 1.1 2001-08-16 23:10:46 kennison Exp $
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
      INTEGER FUNCTION MDIPAN (IAIN,ANME)
C
        INTEGER       IAIN
        CHARACTER*(*) ANME
C
        PARAMETER (MNAI=6000)
C
C The value of MDIPAN (IAIN,ANME) is non-zero if and only if the area
C with area identifier IAIN is a part of some area having the name ANME.
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
        COMMON /MAPCMY/  NAME(MNAI),FLNS
        CHARACTER*64     NAME,FLNS
        SAVE   /MAPCMY/
C
C Declare local variables.
C
        INTEGER          ITMP,NSTP
C
        MDIPAN=0
C
        IF (ANME.EQ.' ') RETURN
C
        ITMP=IAIN
        NSTP=0
C
  101   IF (ITMP.GE.1.AND.ITMP.LE.MNAI) THEN
          IF (NAME(ITMP).NE.ANME) THEN
            IF (IPAR(ITMP).NE.0.AND.NSTP.LT.10) THEN
              ITMP=IPAR(ITMP)
              NSTP=NSTP+1
              GO TO 101
            END IF
          ELSE
            MDIPAN=1
          END IF
        END IF
C
        RETURN
C
      END
