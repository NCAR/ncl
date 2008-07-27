C
C	$Id: g01msg.f,v 1.4 2008-07-27 00:21:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE G01MSG(IACT)
C
C  Put out MESSAGE elements.
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01ins.h'
C
C
C  IACT = 0  for no action, or
C       = 1  for action
C       
      INTEGER IACT,G01PBL
C
C  Find the first non-blank, non-null and set the length.
C
      ILEN = 1
      DO 10 I=80,1,-1
        IF (STR(I:I).NE.' ' .AND. STR(I:I).NE.CHAR(0)) THEN
          ILEN = I
          GO TO 20
        ENDIF
   10 CONTINUE
   20 CONTINUE
C
C  Put out opcode (class and id) and length.
C
      NBYTES = G01PBL (ILEN, 1+(MEFW-1)/8 )
      CALL GPUTNI (7, 1, NBYTES, IERR)
C
C  Put out action/no-action flag.
C
      CALL GPUTPR (IACT, MEFW, 1, RERR)
C
C  Put out the message string.
C
      CALL GPUTPS (STR(1:ILEN), ILEN, ILEN, 0, IERR)
C
      RETURN
      END
