C
C $Id: bcgeti.f,v 1.4 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE BCGETI(PA,IVAL)
C
C Retrieve integer-valued parameters for the Bezier curve package.
C
C Arguments
C     Input
C             PA       Character string indicating parameter.
C
C     Output
C             IVAL     An integer value giving the current setting
C                      of the specified parameter.
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
C  Get flag indicating the number of points per curve.
C
        IVAL = NPPC
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
      RETURN
C
  500 FORMAT(' NTGETI -- Invalid keyword = ',A3,', no action taken')
C
      END
