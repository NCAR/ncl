C
C	$Id: gtsign.f,v 1.2 2000-07-12 16:25:14 haley Exp $
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
C
C============================================================
C                       GTSIGN
C============================================================
      SUBROUTINE GTSIGN(STRING,POS,SIGN)
C
C Checks for a '+' or '-' character at specified position in string
C
C ON Entry:
C    STRING is a character string
C    POS    points to the position to look for the sign character
C
C ON Exit :
C
C    SIGN   =  1 if a '+' or a digit was found at POS
C           = -1 if a '-' was found at POS
C           =  0 if no sign and no digit were found at POS
C    POS    =  POS + 1 if a '+' or '-' was found
C              UNCHANGED if no sign was found
C ======================================================================
C
C INPUT argument
C
      CHARACTER*(*) STRING
C
C INPUT/OUTPUT argument
C
      INTEGER       POS
C
C OUTPUT argument
C
      INTEGER       SIGN
C
C INTERNAL variable
C
      CHARACTER*1 CHR
C
C ======================================================================
C
C Begin
C
      CHR = STRING(POS:POS)
      IF (CHR .EQ. '-') THEN
           SIGN = -1
           POS  = POS + 1
      ELSE IF (CHR .EQ. '+') THEN
           SIGN = 1
           POS  = POS + 1
      ELSE IF ((CHR .LT. '0') .OR. (CHR .GT. '9')) THEN
           SIGN = 0
      ENDIF
C
      RETURN
      END
