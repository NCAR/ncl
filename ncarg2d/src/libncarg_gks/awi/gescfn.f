C
C	$Id: gescfn.f,v 1.4 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GESCFN (IOS, STATUS)
C
C  ESCAPE elements from segments.
C       
      include 'trinst.h'
      include 'trpars.h'
C
      INTEGER IOS, STATUS
      INTEGER BUFS
      PARAMETER (BUFS=100)
      INTEGER BUFF(BUFS), COUNT, CURR
C
      STATUS = 0
C
C  Get the argument count.
C
    5 CONTINUE
      CALL GOPDEC(COUNT,MWHCPR,1,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Loop until the instruction is processed.
C
   10 CONTINUE
      IF (COUNT .GT. BUFS) THEN
        COUNT = COUNT - BUFS
        CURR = BUFS
      ELSE
        CURR = COUNT
        COUNT = 0
      END IF
C
      CALL GOPDEC(BUFF, BYTSIZ, CURR, IOS, STATUS)
      IF (STATUS .NE. 0) RETURN
C
      IF (COUNT.NE.0) GO TO 10
C
C  Get the next part of the instruction if a continue.
C
      IF (CNTINU) THEN
        CALL GNPART(IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
        GO TO 5
      END IF
C
      RETURN
      END
