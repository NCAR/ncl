C
C	$Id: csgetr.f,v 1.6 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSGETR (CNP,RVP)
C
      SAVE
C
      CHARACTER*(*) CNP
C
C  This subroutine is called to retrieve the real value of a specified
C  parameter.
C
C  CNP is the name of the parameter whose value is to be retrieved.
C
C  RVP is a real variable in which the desired value is to be returned
C  by CSGETR.
C
C  Declare a local character variable in which to form an error message.
C
      CHARACTER*80 CTM
C
      include 'cscomn.h'
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL CSBLDA
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('CSGETR - Uncleared prior error',1) .NE. 0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36)='CSGETR - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 110
      ENDIF
C
C Get the appropriate parameter value.
C
      IF (CNP(1:3).EQ.'TOL' .OR. CNP(1:3).EQ.'tol' .OR. 
     +    CNP(1:3).EQ.'Tol') THEN
        RVP = REAL(TOLIC)
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'TTF' .OR. CNP(1:3).EQ.'ttf' .OR. 
     +    CNP(1:3).EQ.'Ttf') THEN
        RVP = REAL(TOLSG)
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'SIG' .OR. CNP(1:3).EQ.'sig' .OR. 
     +    CNP(1:3).EQ.'Sig') THEN
        RVP = REAL(USSIG)
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'MVL' .OR. CNP(1:3).EQ.'mvl' .OR. 
     +    CNP(1:3).EQ.'Mvl') THEN
        RVP = REAL(RMVAL)
        GO TO 110
      ELSE
        CTM(1:36) = 'CSGETR - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 2, 1)
        GO TO 110
      ENDIF
C
  110 CONTINUE
      RETURN
      END
