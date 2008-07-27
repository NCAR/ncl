C
C	$Id: wmsetc.f,v 1.7 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMSETC (CNP,CVP)
C
      SAVE
C
      CHARACTER*(*) CNP,CVP
      CHARACTER*3   CC
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
      CHARACTER*80 CTM
C
      include 'wmcomn.h'
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL WMBLDA
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('WMSETC - Uncleared prior error',1) .NE. 0) RETURN
C
C  Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 3) THEN
        CTM(1:36)='WMSETC - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 120
      ENDIF
C
C  FRO - Front type (one of 'WARM', 'COLD', 'OCCLUDED', 'STATIONARY',
C        'SQUALL', 'TROPICAL', 'CONVERGENCE').       
C
      IF (CNP(1:3).EQ.'FRO' .OR. CNP(1:3).EQ.'fro' .OR.
     +    CNP(1:3).EQ.'Fro') THEN
        CC = CVP(1:3)
        IF (CC.EQ.'COL' .OR. CC.EQ.'col' .OR. CC.EQ.'Col') THEN
          IFRONT = 1
          DO 10 I=1,MAXSYM
            ISTYPE(I) = IFRONT
   10     CONTINUE
        ELSE IF (CC.EQ.'WAR' .OR. CC.EQ.'war' .OR. CC.EQ.'War') THEN
          IFRONT = 2
          DO 40 I=1,MAXSYM
            ISTYPE(I) = IFRONT
   40     CONTINUE
        ELSE IF (CC.EQ.'STA' .OR. CC.EQ.'sta' .OR. CC.EQ.'Sta') THEN
          IFRONT = 3
          DO 50 I=1,MAXSYM
            IF (MOD(I,2) .EQ. 1) THEN
              ISTYPE(I) = 2
            ELSE
              ISTYPE(I) = -1
            ENDIF
   50     CONTINUE
        ELSE IF (CC.EQ.'OCC' .OR. CC.EQ.'occ' .OR. CC.EQ.'Occ') THEN
          IFRONT = 4
          DO 60 I=1,MAXSYM
            IF (MOD(I,2) .EQ. 1) THEN
              ISTYPE(I) = 2
            ELSE
              ISTYPE(I) = 1
            ENDIF
   60     CONTINUE
        ELSE IF (CC.EQ.'SQU' .OR. CC.EQ.'squ' .OR. CC.EQ.'Squ') THEN
          IFRONT = 5
        ELSE IF (CC.EQ.'TRO' .OR. CC.EQ.'tro' .OR. CC.EQ.'Tro') THEN
          IFRONT = 6
        ELSE IF (CC.EQ.'CON' .OR. CC.EQ.'con' .OR. CC.EQ.'Con') THEN
          IFRONT = 7
          DO 65 I=1,MAXSYM
            IF (MOD(I,2) .EQ. 1) THEN
              ISTYPE(I) = 3
            ELSE
              ISTYPE(I) = -3
            ENDIF
   65     CONTINUE
        ELSE
          CTM(1:42) = 'WMSETC - Parameter value out of range for '
          CTM(43:45) = CNP(1:3)
          CALL SETER (CTM(1:45), 2, 1)
          GO TO 120
        ENDIF
        GO TO 120
      ELSE
        CTM(1:36) = 'WMSETC - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 3, 1)
        GO TO 120
      ENDIF
C
  120 CONTINUE
      RETURN
      END
