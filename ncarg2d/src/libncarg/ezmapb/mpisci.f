C
C $Id: mpisci.f,v 1.3 1998-05-24 00:40:52 kennison Exp $
C
      INTEGER FUNCTION MPISCI (IAIN)
C
        PARAMETER (MNAI=2000)
C
C The value of MPISCI(IAIN) is a "suggested color index" for the area
C whose area identifier is IAIN.  The suggested color indices for
C adjacent areas at a given level are guaranteed to be different.
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        MPISCI=0
C
        IF (IAIN.GE.1.AND.IAIN.LE.MNAI) THEN
          IF (IATY(IAIN).NE.0) THEN
            MPISCI=ISCI(IAIN)
          END IF
        END IF
C
        RETURN
C
      END
