C
C	$Id: gqchw.f,v 1.5 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQCHW (ERRIND,CHW)
C
C  INQUIRE CHARACTER WIDTH
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHW
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CHW = CCHH*CCHXP
      ELSE
        CHW = -1.
      ENDIF
C
      RETURN
      END
