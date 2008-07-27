C
C	$Id: gtdgts.f,v 1.4 2008-07-27 00:17:21 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C============================================================
C                       GTDGTS
C============================================================
      SUBROUTINE GTDGTS(STRING,POS,MAXLEN,BASE,NUMBER)
C
C Routine to extract a number from a character string
C
C ON Entry:
C
C    STRING is a character string
C    POS    is the position to look for the first digit (POS <= MAXLEN)
C    MAXLEN is the length of STRING
C    BASE   is a string indicating the base of the number
C
C ON Exit :
C
C    NUMBER contains the integer (base 10) obtained from interpreting
C           the longest string of digits as a base BASE number, or 0
C           if no number was found.
C    POS    points to the character following the last digit (NUMBER <>
C           or UNCHANGED if NUMBER = 0
C ======================================================================
C
C INPUT arguments
C
      CHARACTER*(*) STRING
      CHARACTER*(*) BASE
      INTEGER       MAXLEN
C
C INPUT/OUTPUT arguments
C
      INTEGER       POS
C
C OUTPUT arguments
C
      INTEGER       NUMBER
C
C INTERNAL variables
C
      CHARACTER*1   MAXDIG, DIGIT
      INTEGER       IBASE,  IZERO, INDEX, MAX
C
C ======================================================================
C
C  Begin
C                Initialize the number and store the value of a
C                repeatedly needed function call.
      NUMBER = 0
      IZERO  = ICHAR ('0')
C
C                Find the integer representation of the base and the
C                character which is the greatest digit allowed in a
C                number of that base.
C
      IBASE  = 0
      DO 50 INDEX = 1, LEN(BASE)
          IBASE = IBASE*10 + ICHAR(BASE(INDEX:INDEX)) - IZERO
 50   CONTINUE
      MAX = IBASE - 1
      WRITE (MAXDIG, '(I1)') MAX
C
C                Examine each character, updating the value of the
C                number and the position of the first possible
C                non-digit,  until a non-digit is found.
C
      DO 100  INDEX = POS, MAXLEN
           DIGIT = STRING(INDEX:INDEX)
           IF ((DIGIT .GE. '0') .AND. (DIGIT .LE. MAXDIG)) THEN
                 NUMBER = NUMBER*IBASE + ICHAR(DIGIT) - IZERO
                 POS = POS + 1
           ELSE
                 GOTO 110
           ENDIF
 100  CONTINUE
 110  CONTINUE
C
      RETURN
      END
