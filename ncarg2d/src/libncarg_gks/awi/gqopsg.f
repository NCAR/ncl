C
C	$Id: gqopsg.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQOPSG(ERRIND,SGNA)
C
C  INQUIRE NAME OF OPEN SEGMENT
C
      include 'gkscom.h'
C
      INTEGER ERRIND,SGNA
      ERRIND = 0
C
      IF (OPS .EQ. GSGOP) THEN
        SGNA = CURSEG
      ELSE
        ERRIND = 4
      ENDIF
C
      RETURN
      END
