C
C $Id: bcseti.f,v 1.4 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE BCSETI(PA,IVAL)
C
C Set integer-valued parameters for the Bezier curve package.
C
C Arguments
C     Input   PA       Character string indicating parameter
C                      to be set.
C             IVAL     An integer value for setting PA.
C

      CHARACTER*(*) PA
      CHARACTER*3   CTMP
C
      include 'bccom.h'
C
      CTMP = PA(1:3)
C
      IF (CTMP.EQ.'NPC' .OR. CTMP.EQ.'npc') THEN
C
C  Set flag to indicate number of points on a curve.
C
        NPPC = IVAL
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
      RETURN
C
  500 FORMAT(' BCSETI -- Invalid keyword = ',A3,', no action taken')
C
      END
