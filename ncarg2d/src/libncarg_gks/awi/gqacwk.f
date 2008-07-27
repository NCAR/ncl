C
C	$Id: gqacwk.f,v 1.5 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQACWK(N,ERRIND,OL,WKID)
C
C  INQUIRE SET member OF ACTIVE WORKSTATIONS
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
      IF (N .EQ. 0) THEN
        OL = NACWK
        RETURN
      ENDIF
      IF (N.LT.0 .OR. N.GT.NACWK) THEN
        ERRIND = 2002
        GOTO 100
      ENDIF
      OL   = NACWK
      WKID = SACWK(N)
      RETURN
C
  100 CONTINUE
      OL   = NACWK
      WKID = -1
      RETURN
      END
