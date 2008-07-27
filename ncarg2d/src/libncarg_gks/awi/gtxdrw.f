C
C	$Id: gtxdrw.f,v 1.4 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GTXDRW(IOS,STATUS)
C
C  Process TEXT elements.
C
      include 'trpars.h'
      include 'trinst.h'
      include 'gkscom.h'
C
C  Number of characters used in the text instruction
C
      INTEGER MAXCHR
      PARAMETER (MAXCHR=256)
      INTEGER IOS, STATUS
      INTEGER X, Y, SCHAR(MAXCHR), LLEN, FFLAG, CCONT, CMORE, CLEFT
      CHARACTER*256 ACHAR
C
      CMORE = 0
      STATUS = 0
C
C  Get the text coordinate position.
C
   10 CONTINUE
      CALL GOPDEC(X,MOPLEN,1,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
      CALL GOPDEC(Y,MOPLEN,1,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Final flag.
C
      CALL GOPDEC(FFLAG,MENCPR,1,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
      LLEN = LEN - (MOPLEN*2 + MENCPR) / 8
   20 CONTINUE
C
C  20 Loop for string packets
C
C  Read in the character count.
C
      CALL GOPDEC(CCONT,8,1,IOS,STATUS)
      LLEN = LLEN - 1
      IF (CCONT .EQ. 255) THEN
C
C  Long format, read in string continue and string count.
C
        IF (STATUS .NE. 0) RETURN
        CALL GOPDEC(CMORE,1,1,IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
        CALL GOPDEC(CCONT, 15, 1, IOS, STATUS)
        LLEN = LLEN - 2
      END IF
      IF (STATUS .NE. 0) RETURN
C
C  Round characters used, to the size of the internal buffer, SCHAR.
C
      CLEFT = 0
      IF (CCONT .GT. MAXCHR) THEN
        CLEFT = CCONT - MAXCHR
        CCONT = MAXCHR
      END IF
C
C  Read in the character string.
C
      CALL GOPDEC(SCHAR,8,CCONT,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
      LEN = LLEN - CCONT
C
C  Send out the characters.
C
      IF (CCONT .GT. 0) THEN
C
C  Convert to ASCII characters.
C
        ACHAR = ' '
        DO 30 J=1,CCONT
          ACHAR(J:J) = CHAR(SCHAR(J))
   30   CONTINUE
        XTMP = REAL(X)/32767.
        YTMP = REAL(Y)/32767.
C
C  Transform the text coordinate using the current segment transform.
C
        XN = CURTM(1,1)*XTMP+CURTM(1,2)*YTMP+CURTM(1,3)
        YN = CURTM(2,1)*XTMP+CURTM(2,2)*YTMP+CURTM(2,3)
        CALL GTX(XN,YN,ACHAR(1:CCONT))
      ENDIF
      IF (STATUS.NE.0) RETURN
C
C  Dump any characters left over.
C
      CALL GSKPOP(8, CLEFT, IOS, STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  If string continued get the next packet.
C
      IF (CMORE .EQ. 1) GO TO 20
C
C  If continued, get the next part.
C
      IF (CNTINU) THEN
        CALL GNPART(IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
        GO TO 10
      END IF
C
      RETURN
      END
