C
C	$Id: fftkin.f,v 1.3 2000-08-22 03:53:13 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C

      SUBROUTINE FFTKIN(INPUT,NUM,IVALS,IER)
C
C  Tokenize the input line INPUT and convert the tokens to
C  integers.  Store the NUM integers in the integer array IVALS.  
C  The tokens are separated by one or more blanks, or by one or more
C  parentheses.
C
      CHARACTER*80 INPUT,CTMP
      INTEGER      IVALS(*)
C
      IP = 1
      NT = 0
C
   10 CONTINUE
      CTMP = ' '
      IF (IP .GT. 80) GO TO 30
      IF (INPUT(IP:IP).NE.' ' .AND. INPUT(IP:IP).NE.'(' .AND.
     +    INPUT(IP:IP).NE.')' ) THEN
        NT = NT+1
        J = 0
        DO 20 K=1,80
          J = J+1 
          CTMP(J:J) = INPUT(IP+K-1:IP+K-1)
          L = IP+K
          IF (L .GT. 80) THEN
            READ(CTMP,500) IVALS(NT)
  500       FORMAT(I6)
            GO TO 30
          ELSE IF (INPUT(L:L).EQ.' ' .OR. INPUT(L:L).EQ.')' .OR.
     +             INPUT(L:L).EQ.')' ) THEN
            READ(CTMP,500) IVALS(NT)
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

