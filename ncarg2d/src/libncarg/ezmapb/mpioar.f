C
C $Id: mpioar.f,v 1.1 1998-04-16 20:45:42 kennison Exp $
C
      INTEGER FUNCTION MPIOAR (IAID,ILVL)
C
        PARAMETER (MNAI=2000)
C
C The value of "MPIOAR(IAID,ILVL)" is the area identifier, at level
C ILVL, of the smallest area containing the area with area identifier
C IAID.
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
        MPIOAR=ITMP
C
        RETURN
C
      END
