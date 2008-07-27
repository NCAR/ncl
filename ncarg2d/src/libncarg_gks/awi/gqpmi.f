C
C	$Id: gqpmi.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPMI(ERRIND,INDEX)
C
C  INQUIRE POLYMARKER INDEX
C
      include 'gkscom.h'
C
      INTEGER ERRIND,INDEX
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND.EQ.0) THEN
        INDEX = CPMI
      ELSE
        INDEX = -1
      ENDIF
C
      RETURN
      END
