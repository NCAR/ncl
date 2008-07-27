C
C	$Id: gqclip.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQCLIP(ERRIND,CLSW,CLRECT)
C
C  INQUIRE CLIPPING INDICATOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND,CLSW
      REAL CLRECT(4)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CLSW = CCLIP
        INT = CNT+1
        CLRECT(1) = NTVP(INT,1)
        CLRECT(2) = NTVP(INT,2)
        CLRECT(3) = NTVP(INT,3)
        CLRECT(4) = NTVP(INT,4)
      ELSE
        CLSW = -1
        CLRECT(1) = -1.
        CLRECT(2) = -1.
        CLRECT(3) = -1.
        CLRECT(4) = -1.
      ENDIF
C
      RETURN
      END
