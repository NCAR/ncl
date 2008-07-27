C
C	$Id: g01esc.f,v 1.6 2008-07-27 00:21:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C
C  Two bytes for the function identifier and one for the string
C  count of the empty string.
C
          NBYTES = 3
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
        IF (STRL2 .EQ. 0) THEN
          CALL GPUTPS (STR, STRL1,  0, 0, RERR)
        ELSE IF (STRL2 .GT. 0) THEN
          CALL GPUTPS (STR, STRL1, 80, 0, RERR)
        ENDIF
        IF (RERR .NE. 0)  RETURN
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
