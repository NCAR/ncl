C
C	$Id: fftkin.f,v 1.1 1997-01-24 21:59:53 haley Exp $
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

