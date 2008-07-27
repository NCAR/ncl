C
C	$Id: nggetr.f,v 1.6 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGGETR (CNP,RVP)
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
C  by NGGETR.
C
C  Declare a local character variable in which to form an error message.
C
      CHARACTER*80 CTM
C
      include 'ngcomn.h'
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL NGBLDA
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('NGGETR - Uncleared prior error',1) .NE. 0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36)='NGGETR - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 110
      ENDIF
C
C Get the appropriate parameter value.
C
      IF (CNP(1:2).EQ.'FI' .OR. CNP(1:2).EQ.'fi' .OR. 
     +    CNP(1:2).EQ.'Fi') THEN
        RVP = FILSPC
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'HA' .OR. CNP(1:2).EQ.'ha' .OR. 
     +    CNP(1:2).EQ.'Ha') THEN
        RVP = HATSPC
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'MI' .OR. CNP(1:2).EQ.'mi' .OR. 
     +    CNP(1:2).EQ.'Mi') THEN
        RVP = RMITER
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'NO' .OR. CNP(1:2).EQ.'no' .OR. 
     +    CNP(1:2).EQ.'No') THEN
        RVP = RNLSCL
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'OX' .OR. CNP(1:2).EQ.'ox' .OR. 
     +    CNP(1:2).EQ.'Ox') THEN
        RVP = OXLOGO
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'OY' .OR. CNP(1:2).EQ.'oy' .OR. 
     +    CNP(1:2).EQ.'Oy') THEN
        RVP = OYLOGO
        GO TO 110
      ELSE IF (CNP(1:2).EQ.'OS' .OR. CNP(1:2).EQ.'os' .OR. 
     +    CNP(1:2).EQ.'Os') THEN
        RVP = OLSIZE
        GO TO 110
      ELSE
        CTM(1:36) = 'NGGETR - Parameter name not known - '
        CTM(37:38) = CNP(1:2)
        CALL SETER (CTM(1:38), 2, 1)
        GO TO 110
      ENDIF
C
  110 CONTINUE
      RETURN
      END
