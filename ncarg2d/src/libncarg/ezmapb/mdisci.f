C
C $Id: mdisci.f,v 1.3 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION MDISCI (IAIN)
C
        INTEGER IAIN
C
        PARAMETER (MNAI=8000)
C
C The value of MDISCI(IAIN) is a "suggested color index" for the area
C whose area identifier is IAIN.  The suggested color indices for
C adjacent areas at a given level are guaranteed to be different.
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
        MDISCI=0
C
        IF (IAIN.GE.1.AND.IAIN.LE.MNAI) THEN
          IF (IATY(IAIN).NE.0) THEN
            MDISCI=ISCI(IAIN)
          END IF
        END IF
C
        RETURN
C
      END
