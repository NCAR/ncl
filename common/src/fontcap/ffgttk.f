C
C	$Id: ffgttk.f,v 1.2 2000-07-11 21:29:47 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
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
C License along with this library; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
