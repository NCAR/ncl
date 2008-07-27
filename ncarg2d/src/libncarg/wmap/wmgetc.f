C
C	$Id: wmgetc.f,v 1.5 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMGETC (CNP,CVP)
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
C  returned by WMGETC.
C
C Declare a local character variable in which to form an error message.
C
      CHARACTER*80 CTM
C
      include 'wmcomn.h'
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL WMBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('WMGETC - Uncleared prior error',1) .NE. 0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 3) THEN
        CTM(1:36)='WMGETC - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        RETURN
      ENDIF
C
C Get the appropriate parameter value.
C
      IF (CNP(1:3).EQ.'FRO' .OR. CNP(1:3).EQ.'fro' .OR.
     +    CNP(1:3).EQ.'Fro') THEN
        IF (IFRONT .EQ. 1) THEN
          CVP = 'COLD'
        ELSE IF (IFRONT .EQ. 2) THEN
          CVP = 'WARM'
        ELSE IF (IFRONT .EQ. 3) THEN
          CVP = 'STATIONARY'
        ELSE IF (IFRONT .EQ. 4) THEN
          CVP = 'OCCLUDED'
        ELSE
          CTM(1:42) = 'WMGETC - Parameter value out of range for '
          CTM(43:45) = CNP(1:3)
          CALL SETER (CTM(1:45), 2, 1)
          GO TO 110
        ENDIF
        GO TO 110
      ELSE
        CTM(1:36) = 'WMGETC - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 5, 1)
        GO TO 110
      ENDIF
C
  110 CONTINUE
      RETURN
      END
