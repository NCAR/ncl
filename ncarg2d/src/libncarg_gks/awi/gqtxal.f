C
C	$Id: gqtxal.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQTXAL(ERRIND,TXALH,TXALV)
C
C  INQUIRE TEXT ALIGNMENT
C
      include 'gkscom.h'
C
      INTEGER ERRIND,TXALH,TXALV
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND.EQ.0) THEN
        TXALH = CTXAL(1)
        TXALV = CTXAL(2)
      ELSE
        TXALH = -1
        TXALV = -1
      ENDIF
C
      RETURN
      END
