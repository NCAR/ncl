C
C $Id: mpiosa.f,v 1.1 1998-05-24 00:40:50 kennison Exp $
C
      INTEGER FUNCTION MPIOSA (IAID,ILVL)
C
        PARAMETER (MNAI=2000)
C
C The value of MPIOSA(IAID,ILVL) is the area identifier of the smallest
C area, at level ILVL, that contains the area with area identifier IAID.
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        ITMP=IAID
        NSTP=0
C
  101   IF (IATY(ITMP).GT.ILVL) THEN
          IF (IPAR(ITMP).GE.1.AND.IPAR(ITMP).LE.MNAI) THEN
            IF (IATY(IPAR(ITMP)).NE.0) THEN
              ITMP=IPAR(ITMP)
              NSTP=NSTP+1
              IF (NSTP.LT.10) GO TO 101
            END IF
          END IF
        END IF
C
        MPIOSA=ITMP
C
        RETURN
C
      END
