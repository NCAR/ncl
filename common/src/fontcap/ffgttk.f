C
C	$Id: ffgttk.f,v 1.4 2008-07-27 12:23:41 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE FFGTTK(INPUT,NUM,TOKENS,TLENS)
C
C  Tokenize the input line INPUT and store the NUM tokens 
C  in the character array TOKENS.  The array TLENS contains
C  the lengths of the tokens in the TOKENS array.  The tokens
C  are separated by one or more blanks, or by one or more
C  parentheses.
C
      CHARACTER*80 INPUT,TOKENS(*)
      INTEGER      TLENS(*)
C
      IP = 1
      NT = 0
C
   10 CONTINUE
      IF (IP .GT. 80) GO TO 30
      IF (INPUT(IP:IP).NE.' ') THEN
        NT = NT+1
        TOKENS(NT) = ' '
        J = 0
        DO 20 K=1,80
          J = J+1 
          TOKENS(NT)(J:J) = INPUT(IP+K-1:IP+K-1)
          L = IP+K
          IF (L .GT. 80) THEN
            TLENS(NT) = J
            GO TO 30
          ELSE IF (INPUT(L:L).EQ.' ' .OR. INPUT(L:L).EQ.')' .OR.
     +             INPUT(L:L).EQ.')' ) THEN
            TLENS(NT) = J
            IP = L
            GO TO 10
          ENDIF 
   20   CONTINUE
      ELSE
        IP = IP+1
        GO TO 10
      ENDIF
   30 CONTINUE
C
      NUM = NT
C
      RETURN
      END
