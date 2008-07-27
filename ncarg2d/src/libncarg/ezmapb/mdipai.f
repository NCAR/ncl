C
C $Id: mdipai.f,v 1.3 2008-07-27 00:17:06 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION MDIPAI (IAIN,IAIP)
C
        INTEGER IAIN,IAIP
C
        PARAMETER (MNAI=8000)
C
C The value of MDIPAI (IAIN,IAIP) is non-zero if and only if the area
C with area identifier IAIN is a part of the area with area identifier
C IAIP.
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
C Declare local variables.
C
        INTEGER          ITMP,NSTP
C
        MDIPAI=0
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
            MDIPAI=1
          END IF
        END IF
C
        RETURN
C
      END
