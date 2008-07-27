C
C	$Id: gqchb.f,v 1.5 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQCHB (ERRIND,CHBX,CHBY)
C
C  INQUIRE CHARACTER BASE VECTOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    CHBX,CHBY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
C
      IF (ERRIND .EQ. 0) THEN
        CHBX =  CCHUP(2)
        CHBY = -CCHUP(1)
      ELSE
        CHBX = 0.
        CHBY = 0.
      ENDIF
C
      RETURN
      END
