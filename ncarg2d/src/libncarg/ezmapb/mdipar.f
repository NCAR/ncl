C
C $Id: mdipar.f,v 1.1 2001-08-16 23:10:47 kennison Exp $
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
      INTEGER FUNCTION MDIPAR (IAIN)
C
        INTEGER IAIN
C
        PARAMETER (MNAI=6000)
C
C The value of MDIPAR(IAIN) is the area identifier of the parent of the
C area whose area identifier is IAIN.  The parent of an an area is the
C area of which it is a part.  For example, the area named "Honshu" is
C a part of the area named "Japan", which is its parent.
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
        MDIPAR=0
C
        IF (IAIN.GE.1.AND.IAIN.LE.MNAI) THEN
          IF (IATY(IAIN).NE.0) THEN
            MDIPAR=IPAR(IAIN)
          END IF
        END IF
C
        RETURN
C
      END
