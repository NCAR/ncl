C
C	$Id: gqtxfp.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQTXFP(ERRIND,FONT,PREC)
C
C  INQUIRE TEXT FONT AND PRECISION
C
      include 'gkscom.h'
C
      INTEGER ERRIND,FONT,PREC
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        FONT = CTXFP(1)
        PREC = CTXFP(2)
      ELSE
        FONT = -1
        PREC = -1
      ENDIF
C
      RETURN
      END
