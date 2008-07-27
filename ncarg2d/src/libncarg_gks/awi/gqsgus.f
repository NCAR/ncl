C
C	$Id: gqsgus.f,v 1.6 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQSGUS(N,ERRIND,OL,SGNA)
C
C  INQUIRE SET member OF SEGMENT NAMES IN USE
C
      include 'gkscom.h'
C
      INTEGER N,ERRIND,OL,SGNA
C
C  Check that GKS is in the proper state.
C
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check if N is in bounds.
C
      IF (N.LT.0 .OR. N.GT.NUMSEG) THEN
        ERRIND = 2002
        GOTO 100
      ENDIF
      IF (NUMSEG .EQ. 0) GO TO 100
      OL   = NUMSEG
      IF (N .EQ. 0) RETURN
      SGNA = SEGS(N)
      RETURN
C
  100 CONTINUE
      OL   = NUMSEG
      SGNA = -1
      RETURN
      END
