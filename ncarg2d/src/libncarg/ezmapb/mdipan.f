C
C $Id: mdipan.f,v 1.4 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION MDIPAN (IAIN,ANME)
C
        INTEGER       IAIN
        CHARACTER*(*) ANME
C
        PARAMETER (MNAI=8000)
C
C The value of MDIPAN (IAIN,ANME) is non-zero if and only if the area
C with area identifier IAIN is a part of some area having the name ANME.
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
        COMMON /MAPCMY/  NAME(MNAI),FLNS
        CHARACTER*64     NAME
        CHARACTER*512    FLNS
        SAVE   /MAPCMY/
C
C Declare local variables.
C
        INTEGER          ITMP,NSTP
C
        MDIPAN=0
C
        IF (ANME.EQ.' ') RETURN
C
        ITMP=IAIN
        NSTP=0
C
  101   IF (ITMP.GE.1.AND.ITMP.LE.MNAI) THEN
          IF (NAME(ITMP).NE.ANME) THEN
            IF (IPAR(ITMP).NE.0.AND.NSTP.LT.10) THEN
              ITMP=IPAR(ITMP)
              NSTP=NSTP+1
              GO TO 101
            END IF
          ELSE
            MDIPAN=1
          END IF
        END IF
C
        RETURN
C
      END
