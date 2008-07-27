C
C	$Id: csseti.f,v 1.7 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSSETI (CNP,IVP)
C
      SAVE
C
      CHARACTER*(*) CNP
C
C  This subroutine is called to give an integer value to a specified
C  parameter.
C
C  CNP is the name of the parameter whose value is to be set.
C
C  IVP is an integer variable containing the desired value.
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
      IF (ICFELL('CSSETI - Uncleared prior error',1) .NE. 0) RETURN
C
C  Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 3) THEN
        CTM(1:36) = 'CSSETI - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 120
      ENDIF
C
C  Do a crude check to see if IVP is within range (primarily to 
C  make sure that a floating point number has not been supplied).
C
      IF (IVP .GT. 10000000) THEN
        CTM(1:42) = 'CSSETI - Parameter value out of range for '
        CTM(43:45) = CNP(1:3)
        CALL SETER (CTM(1:45), 2, 1)
        GO TO 120
      ENDIF
C
C  NLS - Number of nodes to use in least squares fit.
C
      IF (CNP(1:3).EQ.'NLS' .OR. CNP(1:3).EQ.'nls' .OR.
     +    CNP(1:3).EQ.'Nls') THEN
        NUMLS = IVP
        GO TO 120
C
C  NSG - Maximum number of iterations to use in computing SIGMA array.
C
      ELSE IF (CNP(1:3).EQ.'NSG' .OR. CNP(1:3).EQ.'nsg' .OR.
     +    CNP(1:3).EQ.'Nsg') THEN
        NUMIT = IVP
        GO TO 120
C
C  ISG - Flag to revert to calculating a SIGMA array rather than
C        use a constant SIGMA.  Using a constant SIGMA is effected
C        by setting a value for SIG using CSSETR.  If ICSIG is 0,
C        then a SIGMA array is used, otherwise a constant SIGMA is used.
C
      ELSE IF (CNP(1:3).EQ.'ISG' .OR. CNP(1:3).EQ.'isg' .OR.
     +    CNP(1:3).EQ.'Isg') THEN
         ICSIG = IVP
        GO TO 120
C
C  IGR - Flag to indicate whether local or global gradients should
C        be used.  IGR=1 means use global gradients; IGR=0 means
C        use local gradients.
C
      ELSE IF (CNP(1:3).EQ.'IGR' .OR. CNP(1:3).EQ.'igr' .OR.
     +    CNP(1:3).EQ.'Igr') THEN
         IGFLG = IVP
        GO TO 120
C
C  MVL - Missing value to be used with NCL functions that return arrays.
C        if MVL is set, it will be used as the missing value to be
C        returned.
C
      ELSE IF (CNP(1:3).EQ.'MVL' .OR. CNP(1:3).EQ.'mvl' .OR.
     +         CNP(1:3).EQ.'Mvl') THEN
        RMVAL = DBLE(IVP)
        GO TO 120
      ELSE
        CTM(1:36) = 'CSSETI - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 23, 1)
        GO TO 120
      ENDIF
C
  120 CONTINUE
      RETURN
      END
