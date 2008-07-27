C
C $Id: mdipar.f,v 1.3 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION MDIPAR (IAIN)
C
        INTEGER IAIN
C
        PARAMETER (MNAI=8000)
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
