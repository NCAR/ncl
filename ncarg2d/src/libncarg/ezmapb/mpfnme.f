C
C $Id: mpfnme.f,v 1.1 1998-04-16 20:45:37 kennison Exp $
C
      CHARACTER*128 FUNCTION MPFNME (IAIN,ILVL)
C
        PARAMETER (MNAI=2000)
C
C The value of "MPFNME(IAIN)" is the full name of the area with the
C area identifier IAIN, including the names of all its parents up to
C the level specified by ILVL.
C
C Assume that IHON is the area identifier for the area named "Honshu",
C which has area type 3, meaning that it exists at the third level in
C the dataset; then it is that case that
C
C   MPFNME (IHON,3) = 'Japan - Honshu'
C   MPFNME (IHON,2) = 'Eurasia - Japan - Honshu'
C   MPFNME (IHON,1) = 'Land - Eurasia - Japan - Honshu'
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        COMMON /MAPCMY/ NAME(MNAI),FLNS
        CHARACTER*64    NAME
        CHARACTER*128   FLNS
        SAVE   /MAPCMY/
C
        CHARACTER*128 CTMP
C
        MPFNME=' '
C
        ITMP=IAIN
        NSTP=0
C
  101   IF (ITMP.GE.1.AND.ITMP.LE.MNAI) THEN
          IF (IATY(ITMP).NE.0) THEN
            IF (NAME(ITMP).NE.' ') THEN
              IF (MPFNME.EQ.' ') THEN
                MPFNME=NAME(ITMP)
              ELSE
                CTMP=MPFNME
                MPFNME=NAME(ITMP)(1:MPILNB(NAME(ITMP)))//' - '//CTMP
              END IF
            END IF
            ITMP=IPAR(ITMP)
            IF (ITMP.GE.1.AND.ITMP.LE.MNAI) THEN
              IF (IATY(ITMP).GE.ILVL) THEN
                NSTP=NSTP+1
                IF (NSTP.LT.10) GO TO 101
              END IF
            END IF
          END IF
        END IF
C
        RETURN
C
      END
