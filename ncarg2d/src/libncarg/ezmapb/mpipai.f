C
C $Id: mpipai.f,v 1.1 1998-04-16 20:45:43 kennison Exp $
C
      INTEGER FUNCTION MPIPAI (IAIN,IAIP)
C
        PARAMETER (MNAI=2000)
C
C The value of "MPIPAI (IAIN,IAIP)" is non-zero if and only if the area
C with area identifier IAIN is a part of the area with area identifier
C IAIP.
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        MPIPAI=0
C
        IF (IAIP.LE.0) RETURN
C
        ITMP=IAIN
        NSTP=0
C
  101   IF (ITMP.GE.1.AND.ITMP.LE.MNAI) THEN
          IF (ITMP.NE.IAIP) THEN
            IF (IPAR(ITMP).NE.0.AND.NSTP.LT.10) THEN
              ITMP=IPAR(ITMP)
              NSTP=NSTP+1
              GO TO 101
            END IF
          ELSE
            MPIPAI=1
          END IF
        END IF
C
        RETURN
C
      END
