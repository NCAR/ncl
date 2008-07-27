C
C $Id: mdiaty.f,v 1.3 2008-07-27 00:17:06 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
