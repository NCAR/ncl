C
C $Id: bcsetr.f,v 1.4 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE BCSETR(PA,RVAL)
C
C Set real-valued parameters for the Bezier curve package.
C
C Arguments
C     Input   PA       Character string indicating parameter to be set.
C             RVAL     A real number for setting PA.
C

      CHARACTER*(*) PA
      CHARACTER*3   CTMP
C
      include 'bccom.h'
C
      CTMP = PA(1:3)
C
      IF (CTMP.EQ.'FTL' .OR. CTMP.EQ.'ftl') THEN
C
C  Set the flatness tolerance limit (fraction of maximum screen height).
C
        FRATIO = RVAL 
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
      RETURN
C
  500 FORMAT(' BCSETR -- Invalid keyword = ',A3,', no action taken')
C
      END
