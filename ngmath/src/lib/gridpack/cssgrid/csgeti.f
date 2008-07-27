C
C	$Id: csgeti.f,v 1.5 2008-07-27 03:10:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSGETI (CNP,IVP)
C
      SAVE
C
      CHARACTER*(*) CNP
C
C  This subroutine is called to retrieve the integer value of a specified
C  parameter.
C
C  CNP is the name of the parameter whose value is to be retrieved.
C
C  IVP is an integer variable in which the desired value is to be
C  returned by CSGETI.
C
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
      IF (ICFELL('CSGETI - Uncleared prior error',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 3) THEN
        CTM(1:36)='CSGETI - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 110
      ENDIF
C
C Get the appropriate parameter value.
C
      IF (CNP(1:3).EQ.'NLS' .OR. CNP(1:3).EQ.'nls' .OR. 
     +    CNP(1:3).EQ.'Nls') THEN
        IVP = NUMLS
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'NSG' .OR. CNP(1:3).EQ.'nsg' .OR. 
     +    CNP(1:3).EQ.'Nsg') THEN
        IVP = NUMIT
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'ISG' .OR. CNP(1:3).EQ.'isg' .OR. 
     +    CNP(1:3).EQ.'Isg') THEN
        IVP = ICSIG
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'IGR' .OR. CNP(1:3).EQ.'igr' .OR. 
     +    CNP(1:3).EQ.'Igr') THEN
        IVP = ICSIG
        GO TO 110
      ELSE
        CTM(1:36) = 'CSGETI - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 2, 1)
        GO TO 110
      ENDIF
C
  110 CONTINUE
      RETURN
      END
