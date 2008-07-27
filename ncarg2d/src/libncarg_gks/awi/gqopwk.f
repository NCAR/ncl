C
C	$Id: gqopwk.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQOPWK(N,ERRIND,OL,WKID)
C
C  INQUIRE SET member OF OPEN WORKSTATIONS
C
      include 'gkscom.h'
C
      INTEGER N,ERRIND,OL,WKID
C
C  Check that GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check if N is in bounds.
C
      IF (N.LT.0 .OR. N.GT.NOPWK) THEN
        ERRIND = 2002
        GOTO 100
      ENDIF
C
      OL   = NOPWK
      IF (N .EQ. 0) RETURN
      WKID = SOPWK(N)
      RETURN
C
  100 CONTINUE
      OL   = NOPWK
      WKID = -1
      RETURN
      END
