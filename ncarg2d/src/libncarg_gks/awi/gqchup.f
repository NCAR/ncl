C
C	$Id: gqchup.f,v 1.5 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQCHUP(ERRIND,CHUX,CHUY)
C
C  INQUIRE CHARACTER UP VECTOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHUX,CHUY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CHUX = CCHUP(1)
        CHUY = CCHUP(2)
      ELSE
        CHUX = 0.
        CHUY = 0.
      ENDIF
C
      RETURN
      END
