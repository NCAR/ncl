C
C $Id: mpipar.f,v 1.2 1998-04-30 22:43:55 kennison Exp $
C
      INTEGER FUNCTION MPIPAR (IAIN)
C
        PARAMETER (MNAI=2000)
C
C The value of "MPIPAR(IAIN)" is the area identifier of the parent of
C the area whose area identifier is IAIN.  The parent of an an area is
C the area of which it is a part.  For example, the area named "Honshu"
C is a part of the area named "Japan", which is its parent.
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        MPIPAR=0
C
        IF (IAIN.GE.1.AND.IAIN.LE.MNAI) THEN
          IF (IATY(IAIN).NE.0) THEN
            MPIPAR=IPAR(IAIN)
          END IF
        END IF
C
        RETURN
C
      END
