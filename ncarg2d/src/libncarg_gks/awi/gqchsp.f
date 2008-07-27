C
C	$Id: gqchsp.f,v 1.5 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQCHSP(ERRIND,CHSP)
C
C  INQUIRE CHARACTER SPACING
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHSP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        CHSP = CCHSP
      ELSE
        CHSP = -1.E20
      ENDIF
C
      RETURN
      END
