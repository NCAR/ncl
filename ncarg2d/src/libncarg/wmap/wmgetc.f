C
C	$Id: wmgetc.f,v 1.2 2000-07-12 16:27:03 haley Exp $
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
C Declare the block data routine external to force its loading.
C
      EXTERNAL WMBLDA
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
