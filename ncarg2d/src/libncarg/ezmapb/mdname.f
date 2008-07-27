C
C $Id: mdname.f,v 1.4 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      CHARACTER*64 FUNCTION MDNAME (IAIN)
C
        INTEGER      IAIN
C
        PARAMETER (MNAI=8000)
C
C The value of MDNAME(IAIN) is the name of the area with the area
C identifier IAIN.
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
        MDNAME=' '
C
        IF (IAIN.GE.1.AND.IAIN.LE.MNAI) THEN
          IF (IATY(IAIN).NE.0) THEN
            MDNAME=NAME(IAIN)
          END IF
        END IF
C
        RETURN
C
      END
