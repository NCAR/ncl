C
C	$Id: ngsetr.f,v 1.3 2000-07-12 16:24:47 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE NGSETR (CNP,RVP)
C
      SAVE
C
      CHARACTER*(*) CNP
C
C  This subroutine is called to give a real value to a specified
C  parameter.
C
C  CNP is the name of the parameter whose value is to be set.
C
C  RVP is a real variable containing the desired value.
C
C  Declare a local character variable in which to form an error message.       
C
      CHARACTER*80 CTM
C
      include 'ngcomn.h'
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL NGBLDA
C
C  Input and output data records for ESCAPE function calls.
C
      CHARACTER*80 IDR,CDUM
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('NGSETR - Uncleared prior error',1) .NE. 0) RETURN
C
C  Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 2) THEN
        CTM(1:36) = 'NGSETR - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 120
      ENDIF
C
C  Set the appropriate parameter.
C
      IDR = ' '
C
C  FI - Spacing of fill area lines (in NDC).
C
      IF (CNP(1:2).EQ.'FI' .OR. CNP(1:2).EQ.'fi' .OR.
     +         CNP(1:2).EQ.'Fi') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETR - set a workstation ID using WO before setting FI'
          CALL SETER (CTM(1:60), 2, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
  500   FORMAT(I5)
        IF (RVP.LE.0. .OR. RVP.GT.1.) THEN
          CALL SETER('NGSETR - invalid value for FI', 3, 1)
          GO TO 120
        ELSE
          FILSPC = RVP
        ENDIF
        WRITE(IDR(7:15), 540) FILSPC
  540   FORMAT(F9.7)
        CALL GESC(-1512,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  HA - Spacing of hatch lines (in NDC).
C
      ELSE IF (CNP(1:2).EQ.'HA' .OR. CNP(1:2).EQ.'ha' .OR.
     +         CNP(1:2).EQ.'Ha') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETR - set a workstation ID using WO before setting HA'
          CALL SETER (CTM(1:60), 4, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        IF (RVP.LE.0. .OR. RVP.GT.1.) THEN
          CALL SETER('NGSETR - invalid value for HA', 5, 1)
          GO TO 120
        ELSE
          HATSPC = RVP
        ENDIF
        WRITE(IDR(7:15), 540) HATSPC
        CALL GESC(-1513,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  MI - Miter limit.
C
      ELSE IF (CNP(1:2).EQ.'MI' .OR. CNP(1:2).EQ.'mi' .OR.
     +         CNP(1:2).EQ.'Mi') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETR - set a workstation ID using WO before setting MI'
          CALL SETER (CTM(1:60), 6, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        RMITER = MAX(1., RVP)
        WRITE(IDR(7:15), 550) RMITER
  550   FORMAT(F9.3)
        CALL GESC(-1520,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
C  NO - Nominal linewidth.
C
      ELSE IF (CNP(1:2).EQ.'NO' .OR. CNP(1:2).EQ.'no' .OR.
     +         CNP(1:2).EQ.'No') THEN
        IF (IWKID .LT. 0) THEN
          CTM =
     +      'NGSETR - set a workstation ID using WO before setting NO'
          CALL SETER (CTM(1:60), 7, 1)
          GO TO 120
        ENDIF
        WRITE(IDR(1: 5), 500) IWKID
        RNLSCL = MAX(0., RVP)
        WRITE(IDR(7:15), 550) RNLSCL
        CALL GESC(-1516,1,IDR,1,IDUM,CDUM)
        GO TO 120
C
      ELSE
        CTM(1:36) = 'NGSETR - Parameter name not known - '
        CTM(37:38) = CNP(1:2)
        CALL SETER (CTM(1:38), 8, 1)
        GO TO 120
      ENDIF
C
  120 CONTINUE
      RETURN
      END
