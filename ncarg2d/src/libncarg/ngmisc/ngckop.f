C
C	$Id: ngckop.f,v 1.4 2008-07-27 00:17:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION NGCKOP(IWKID)
C
C  Function to check if the workstation, whose workstation ID is
C  given in IWKID, is currently open.  If it is open, then the
C  function returns a 1, otherwise the function returns a 0.
C
      CALL GQOPWK(0,IER,NOPN,LID)
      IF (NOPN .EQ. 0) THEN
        NGCKOP = 0
        RETURN
      ELSE
        DO 10 I=1,NOPN
          CALL GQOPWK(I,IER,NOPN,LID)
          IF (IWKID .EQ. LID) THEN
            NGCKOP = 1
            RETURN
          ENDIF
   10   CONTINUE
      ENDIF
C
      NGCKOP = 0
      RETURN
      END
