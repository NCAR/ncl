C
C $Id: mpiola.f,v 1.2 1999-07-29 22:55:46 kennison Exp $
C
      INTEGER FUNCTION MPIOLA (IAID,ILVL)
C
        PARAMETER (MNAI=6000)
C
C The value of MPIOLA(IAID,ILVL) is the area identifier of the largest
C area, at level ILVL, that contains the area with area identifier IAID.
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        ITMP=IAID
        NSTP=0
C
  101   IF (IPAR(ITMP).GE.1.AND.IPAR(ITMP).LE.MNAI) THEN
          IF (IATY(IPAR(ITMP)).GE.ILVL) THEN
            ITMP=IPAR(ITMP)
            NSTP=NSTP+1
            IF (NSTP.LT.10) GO TO 101
          END IF
        END IF
C
        MPIOLA=ITMP
C
        RETURN
C
      END
