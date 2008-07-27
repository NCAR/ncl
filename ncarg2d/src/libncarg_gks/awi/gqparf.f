C
C	$Id: gqparf.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPARF(ERRIND,RFX,RFY)
C
C  INQUIRE PATTERN REFERENCE POINT
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    RFX,RFY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        RFX = CPARF(1)
        RFY = CPARF(2)
      ELSE
        RFX = -1.E20
        RFY = -1.E20
      ENDIF
C
      RETURN
      END
