C
C	$Id: gqtxp.f,v 1.5 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQTXP(ERRIND,TXP)
C
C  INQUIRE TEXT PATH
C
      include 'gkscom.h'
C
      INTEGER ERRIND,TXP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        TXP = CTXP
      ELSE
        TXP = -1
      ENDIF
C
      RETURN
      END
