C
C     $Id: shgeti.f,v 1.2 2000-07-13 02:49:29 haley Exp $
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
      SUBROUTINE SHGETI (CNP,IVP)
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
C  returned by SHGETI.
C
C
C  Declare a local character variable in which to form an error message.
C
      CHARACTER*80 CTM
C
      include 'shcomn.h'
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL SHBLDA
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('SHGETI - Uncleared prior error',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(CNP) .LT. 3) THEN
        CTM(1:36)='SHGETI - Parameter name too short - '
        CTM(37:36+LEN(CNP)) = CNP
        CALL SETER (CTM(1:36+LEN(CNP)), 1, 1)
        GO TO 110
      ENDIF
C
C Get the appropriate parameter value.
C
      IF (CNP(1:3).EQ.'NFL' .OR. CNP(1:3).EQ.'nfl' .OR. 
     +    CNP(1:3).EQ.'Nfl') THEN
        IVP = NMINFL
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'NLS' .OR. CNP(1:3).EQ.'nls' .OR. 
     +    CNP(1:3).EQ.'Nls') THEN
        IVP = NMLSTQ
        GO TO 110
      ELSE IF (CNP(1:3).EQ.'NCL' .OR. CNP(1:3).EQ.'ncl' .OR. 
     +    CNP(1:3).EQ.'Ncl') THEN
        IVP = NMCELS
        GO TO 110
      ELSE
        CTM(1:36) = 'SHGETI - Parameter name not known - '
        CTM(37:39) = CNP(1:3)
        CALL SETER (CTM(1:39), 2, 1)
        GO TO 110
      ENDIF
C
  110 CONTINUE
      RETURN
      END
