C
C	$Id: gtsign.f,v 1.4 2008-07-27 00:17:21 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
