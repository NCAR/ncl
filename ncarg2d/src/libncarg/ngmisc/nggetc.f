C
C	$Id: nggetc.f,v 1.6 2008-07-27 00:17:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGGETC (CNP,CVP)
C
      SAVE
C
      CHARACTER*(*) CNP,CVP
C
C  This subroutine is called to retrieve the character value of a
C  specified parameter.
C
C  CNP is the name of the parameter whose value is to be retrieved.
C
C  CVP is a character variable in which the desired value is to be
C  returned by NGGETC.
C
C Declare a local character variable in which to form an error message.
C
      CHARACTER*38 CTM
C
      include 'ngcomn.h'
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL NGBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('NGGETC - Uncleared prior error',1) .NE. 0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36)='NGGETC - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        RETURN
      ENDIF
C
C Get the appropriate parameter value.
C
      IF (CNP(1:2).EQ.'ME' .OR. CNP(1:2).EQ.'me' .OR.
     +    CNP(1:2).EQ.'Me') THEN
C
C  Check to see if the length of the supplied string is long enough
C  to hold the result.
C
        ILEN = 0
        DO 10 I=1,256
          IF (CFILNM(I:I).EQ.' ' .OR. CFILNM(I:I).EQ.CHAR(0)) THEN
            ILEN = I-1
            GO TO 20
          ENDIF
   10   CONTINUE
        CALL SETER('NGGETC - file name too long for ME', 2, 1)
        GO TO 110
   20   CONTINUE
        IF (ILEN .GT. LEN(CVP)) THEN
          CALL SETER('NGGETC - supplied string not long enough to receiv       
     +e value for ME', 3, 1)
          GO TO 110
        ENDIF
        CVP = CFILNM(1:ILEN)
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'PI' .OR. CNP(1:2).EQ.'pi' .OR.
     +    CNP(1:2).EQ.'Pi') THEN
C
C  Check to see if the length of the supplied string is long enough
C  to hold the result.
C
        ILEN = 0
        DO 30 I=1,80
          IF (CPICNM(I:I).EQ.' ' .OR. CPICNM(I:I).EQ.CHAR(0)) THEN
            ILEN = I-1
            GO TO 40
          ENDIF
   30   CONTINUE
   40   CONTINUE
        IF (ILEN .GT. LEN(CVP)) THEN
          CALL SETER('NGGETC - supplied string not long enough to receiv       
     +e value for PI', 4, 1)
          GO TO 110
        ENDIF
        CVP = CPICNM(1:ILEN)
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'SE' .OR. CNP(1:2).EQ.'se' .OR.
     +    CNP(1:2).EQ.'Se') THEN
C
C  Check to see if the length of the supplied string is long enough
C  to hold the result.
C
        ILEN = 0
        DO 50 I=1,15
          IF (CSEGNM(I:I).EQ.' ' .OR. CSEGNM(I:I).EQ.CHAR(0)) THEN
            ILEN = I-1
            GO TO 60
          ENDIF
   50   CONTINUE
        ILEN = 15
   60   CONTINUE
        IF (ILEN .GT. LEN(CVP)) THEN
          CALL SETER('NGGETC - supplied string not long enough to receiv       
     +e value for SE', 4, 1)
          GO TO 110
        ENDIF
        CVP = CSEGNM(1:ILEN)
        GO TO 110
      ELSE
        CTM(1:36) = 'NGGETC - Parameter name not known - '
        CTM(37:38) = CNP(1:2)
        CALL SETER (CTM(1:38), 5, 1)
        GO TO 110
      ENDIF
C
  110 CONTINUE
      RETURN
      END
