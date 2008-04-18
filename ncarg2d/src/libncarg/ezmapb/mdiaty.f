C
C $Id: mdiaty.f,v 1.2 2008-04-18 04:09:21 kennison Exp $
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
      INTEGER FUNCTION MDIATY (IAIN)
C
        INTEGER IAIN
C
        PARAMETER (MNAI=8000)
C
C The value of MDIATY(IAIN) is the area type for the area whose area
C identifier is IAIN.
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
        MDIATY=0
        IF (IAIN.GE.1.AND.IAIN.LE.MNAI) THEN
          MDIATY=IATY(IAIN)
        END IF
        RETURN
      END
