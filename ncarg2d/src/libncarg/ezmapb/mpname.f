C
C $Id: mpname.f,v 1.1 1998-04-16 20:45:51 kennison Exp $
C
      CHARACTER*64 FUNCTION MPNAME (IAIN)
C
        PARAMETER (MNAI=2000)
C
C The value of "MPNAME(IAIN)" is the name of the area with the area
C identifier IAIN.
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        COMMON /MAPCMY/ NAME(MNAI),FLNS
        CHARACTER*64    NAME
        CHARACTER*128   FLNS
        SAVE   /MAPCMY/
C
        MPNAME=' '
C
        IF (IAIN.GE.1.AND.IAIN.LE.MNAI) THEN
          IF (IATY(IAIN).NE.0) THEN
            MPNAME=NAME(IAIN)
          END IF
        END IF
C
        RETURN
C
      END
