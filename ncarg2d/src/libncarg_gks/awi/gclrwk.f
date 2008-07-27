C
C	$Id: gclrwk.f,v 1.10 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GCLRWK(WKID,COFL)
C
      INTEGER WKID,COFL
C
      CALL GZCLRWK(WKID,COFL)
C
      RETURN
      END
