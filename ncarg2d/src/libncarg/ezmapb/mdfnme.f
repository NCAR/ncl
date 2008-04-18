C
C $Id: mdfnme.f,v 1.3 2008-04-18 04:09:21 kennison Exp $
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
      CHARACTER*128 FUNCTION MDFNME (IAIN,ILVL)
C
        INTEGER       IAIN,ILVL
C
        PARAMETER (MNAI=8000)
C
C The value of MDFNME(IAIN) is the full name of the area with the area
C identifier IAIN, including the names of all its parents up to the
C the level specified by ILVL.
C
C Assume that IHON is the area identifier for the area named "Honshu",
C which has area type 3, meaning that it exists at the third level in
C the dataset; then it is that case that
C
C   MDFNME (IHON,3) = 'Japan - Honshu'
C   MDFNME (IHON,2) = 'Eurasia - Japan - Honshu'
C   MDFNME (IHON,1) = 'Land - Eurasia - Japan - Honshu'
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
        COMMON /MAPCMY/  NAME(MNAI),FLNS
        CHARACTER*64     NAME
        CHARACTER*512    FLNS
        SAVE   /MAPCMY/
C
C Declare local variables.
C
        INTEGER          ITMP,NSTP
        CHARACTER*128    CTMP
C
        MDFNME=' '
C
        ITMP=IAIN
        NSTP=0
C
  101   IF (ITMP.GE.1.AND.ITMP.LE.MNAI) THEN
          IF (IATY(ITMP).NE.0) THEN
            IF (NAME(ITMP).NE.' ') THEN
              IF (MDFNME.EQ.' ') THEN
                MDFNME=NAME(ITMP)
              ELSE
                CTMP=MDFNME
                MDFNME=NAME(ITMP)(1:MDILNB(NAME(ITMP)))//' - '//CTMP
              END IF
            END IF
            ITMP=IPAR(ITMP)
            IF (ITMP.GE.1.AND.ITMP.LE.MNAI) THEN
              IF (IATY(ITMP).GE.ILVL) THEN
                NSTP=NSTP+1
                IF (NSTP.LT.10) GO TO 101
              END IF
            END IF
          END IF
        END IF
C
        RETURN
C
      END
