C
C	$Id: ngckop.f,v 1.1 1994-04-26 18:22:33 fred Exp $
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
