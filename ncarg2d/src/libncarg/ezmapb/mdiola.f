C
C $Id: mdiola.f,v 1.3 2008-07-27 00:17:06 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION MDIOLA (IAID,ILVL)
C
        INTEGER IAID,ILVL
C
        PARAMETER (MNAI=8000)
C
C The value of MDIOLA(IAID,ILVL) is the area identifier of the largest
C area, at level ILVL, that contains the area with area identifier IAID.
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
C Declare local variables.
C
        INTEGER          ITMP,NSTP
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
        MDIOLA=ITMP
C
        RETURN
C
      END
