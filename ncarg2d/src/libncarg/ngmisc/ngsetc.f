C
C	$Id: ngsetc.f,v 1.6 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGSETC (CNP,CVP)
C
      SAVE
C
      CHARACTER*(*) CNP,CVP
C
C  This subroutine is called to give a character value to a specified
C  parameter.
C
C  CNP is the name of the parameter whose value is to be set.
C
C  CVP is a character variable containing the desired value.
C
C  Declare a local character variable in which to form an error message.
C
      CHARACTER*38 CTM
C
      include 'ngcomn.h'
C
C  Input and output data records for ESCAPE function calls.
C
      PARAMETER (IDRLEN=13)
      CHARACTER*80 IDR(IDRLEN),CDUM
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL NGBLDA
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('NGSETC - Uncleared prior error',1) .NE. 0) RETURN
C
C  Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36)='NGSETC - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 120
      ENDIF
C
C  Set the appropriate parameter value.
C
      DO 10 I=1,IDRLEN
        IDR(I) = ' '
   10 CONTINUE
C
C  ME - Metafile name.
C
      IF (CNP(1:2).EQ.'ME' .OR. CNP(1:2).EQ.'me' .OR.
     +    CNP(1:2).EQ.'Me') THEN
        LENCVP = LEN(CVP)
        IF (LENCVP .EQ. 0) THEN 
          CALL SETER ('NGSETC - value for ME must not be empty', 2, 1)       
          GO TO 120
        ENDIF
        CFILNM = CVP
        NLEN = ((LENCVP-1)/80) + 1
        DO 20 I=1,NLEN
          IB = 80*(I-1)+1
          IF (I .LT. NLEN) THEN
            IE = IB+79
          ELSE
            IE = IB+MOD(LENCVP-1,80)
          ENDIF
          IDR(I)(1:IE-IB+1) = CVP(IB:IE)
   20   CONTINUE
        CALL GESC(-1391,NLEN,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  PI - Picture name.
C
      ELSE IF (CNP(1:2).EQ.'PI' .OR. CNP(1:2).EQ.'pi' .OR.
     +         CNP(1:2).EQ.'Pi') THEN
        IDR(1) = CVP
        CPICNM = CVP
        CALL GESC(-1393,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  SE - Root name for segment file names.
C
      ELSE IF (CNP(1:2).EQ.'SE' .OR. CNP(1:2).EQ.'se' .OR.
     +         CNP(1:2).EQ.'Se') THEN
        IDR(1) = CVP
        CSEGNM = CVP
        CALL GESC(-1394,1,IDR,1,IDUM,CDUM)
        GO TO 120
      ELSE
        CTM(1:36) = 'NGSETC - Parameter name not known - '
        CTM(37:38) = CNP(1:2)
        CALL SETER (CTM(1:38), 3, 1)
        GO TO 120
      ENDIF
C
  120 CONTINUE
      RETURN
      END
