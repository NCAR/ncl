C
C	$Id: gqmksc.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQMKSC(ERRIND,MSZSF)
C
C  INQUIRE MARKER SIZE SCALE FACTOR
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL MSZSF
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        MSZSF = CMKS
      ELSE
        MSZSF = 0
      ENDIF
C
      RETURN
      END
