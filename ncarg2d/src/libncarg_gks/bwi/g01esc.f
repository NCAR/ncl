C
C	$Id: g01esc.f,v 1.2 1993-01-09 02:06:00 fred Exp $
C
      SUBROUTINE G01ESC
C
C  Process ESCAPE elements.
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01ins.h'
C
      INTEGER  KALL, NBYTES, G01PBL
      SAVE KALL
      DATA KALL/0/
      KALL = KALL+1
C
C  Treat the first call -- put out the opcode, FCTID, and first data
C  record if there is one.
C
      IF (KALL .EQ. 1) THEN
        IF (STRL1 .GT. 0) THEN
          NBYTES = G01PBL(STRL1,1+(MINTFW-1)/8)
        ELSE
          NBYTES = 2
        ENDIF
C
C  Put out class, id, length
C
        CALL GPUTNI (6, 1, NBYTES, RERR)
        IF (RERR .NE. 0)  RETURN
C
C  Put out function identifier (2-byte integer).
C
        CALL GPUTPR (ID(1),MINTFW,1,RERR)
        IF (RERR .NE. 0)  RETURN
C
C  Put out first data record if there is one.
C
        IF (STRL2 .GT. 0) THEN
          CALL GPUTPS (STR, STRL1, 80, 0, RERR)
          IF (RERR .NE. 0)  RETURN
        ENDIF
C
C  If there is to be no continuation, check for consistency, and
C  reset the parameter "KALL".
C
        IF (CONT .EQ. 0) THEN
          IF (MOD(STRL1,80) .EQ. 0) THEN
            KALL = 0
            RETURN
          ELSE
            RERR = 325
            RETURN
          ENDIF
        ENDIF
      ENDIF
C
C  Treat the continuation calls.
C
      IF (KALL .GT. 1) THEN
        IF (CONT .EQ. 0) THEN
          CALL GPUTPS (STR, 80, 80, 1, RERR)
          IF (RERR .NE. 0)  RETURN
          KALL = 0
        ELSE
          CALL GPUTPS (STR, 80, 80, 1, RERR)
          IF (RERR .NE. 0)  RETURN
        ENDIF
      ENDIF
C
      RETURN
      END
