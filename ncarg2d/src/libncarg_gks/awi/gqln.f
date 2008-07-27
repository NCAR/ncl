C
C	$Id: gqln.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQLN(ERRIND,LTYPE)
C
C  INQUIRE LINETYPE
C
      include 'gkscom.h'
C
      INTEGER ERRIND,LTYPE
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        LTYPE = CLN
      ELSE
        LTYPE = -1
      ENDIF
C
      RETURN
      END
