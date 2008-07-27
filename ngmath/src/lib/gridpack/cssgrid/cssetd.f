C
C	$Id: cssetd.f,v 1.7 2008-07-27 03:10:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSSETD (CNP,DVP)
C
      SAVE
C
      CHARACTER*(*) CNP
      DOUBLE PRECISION DVP
C
C  This subroutine is called to give a double precision value to a 
C  specified parameter.
C
C  CNP is the name of the parameter whose value is to be set.
C
C  DVP is a double precision variable containing the desired value.
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
      IF (ICFELL('CSSETD - Uncleared prior error',1) .NE. 0) RETURN
C
C  Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36) = 'CSSETD - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 120
      ENDIF
C
C  SIG - Value for constant SIGMA.
C
      IF (CNP(1:3).EQ.'SIG' .OR. CNP(1:3).EQ.'sig' .OR.
     +    CNP(1:3).EQ.'Sig' .OR. CNP(1:3).EQ.'DSG' .OR.
     +    CNP(1:3).EQ.'dsg' .OR. CNP(1:3).EQ.'Dsg') THEN
        USSIG = DVP
        ICSIG = 1 
        GO TO 120
C
C  TOL - Tolerance to use in calculating gradient differences to terminate
C        iteration sequence to compute the SIGMA array.
C
      ELSE IF (CNP(1:3).EQ.'TOL' .OR. CNP(1:3).EQ.'tol' .OR.
     +         CNP(1:3).EQ.'Tol' .OR. CNP(1:3).EQ.'DTL' .OR.
     +         CNP(1:3).EQ.'dtl' .OR. CNP(1:3).EQ.'Dtl') THEN
        TOLIC = DVP
        GO TO 120
C
C  TTF - Tolerance to use in determining how close each SIGMA element 
C        should be to its optimum value in GETSIG.
C
      ELSE IF (CNP(1:3).EQ.'TTF' .OR. CNP(1:3).EQ.'ttf' .OR.
     +         CNP(1:3).EQ.'Ttf' .OR. CNP(1:3).EQ.'TTD' .OR.
     +         CNP(1:3).EQ.'ttd' .OR. CNP(1:3).EQ.'Ttd') THEN
        TOLSG = DVP
        GO TO 120
C
C  MVL - Missing value to be used with NCL functions that return arrays.
C        if MVL is set, it will be used as the missing value to be
C        returned.
C
      ELSE IF (CNP(1:3).EQ.'MVL' .OR. CNP(1:3).EQ.'mvl' .OR.
     +         CNP(1:3).EQ.'Mvl' .OR. CNP(1:3).EQ.'DMV' .OR.
     +         CNP(1:3).EQ.'dmv' .OR. CNP(1:3).EQ.'Dmv') THEN
        RMVAL = DVP
        GO TO 120
      ELSE
        CTM(1:36) = 'CSSETD - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 8, 1)
        GO TO 120
      ENDIF
C
  120 CONTINUE
      RETURN
      END
