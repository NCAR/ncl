C
C
C       $Id: search.f,v 1.2 1997-05-05 21:45:43 boote Exp $
C
C This subprogram is used by subroutines TERP1 and TERP2 in 
C library ncarm.  It is lower-level, i.e. not called by users.
C **********************************************************
      SUBROUTINE SEARCH (XBAR,X,N,I)
      DIMENSION       X(N)
      SAVE
      DATA B/.69314718/
C
C IF XBAR IS OUTSIDE RANGE OF X TABLE EXTRAPOLATE
C
      IF (XBAR .GT. X(2)) GO TO 101
      I = 1
      RETURN
  101 CONTINUE
      IF (XBAR .LT. X(N-1)) GO TO 102
      I = N-1
      RETURN
  102 CONTINUE
C
C FIND MAXIMUM POWER OF TWO LESS THAN N
C
      M = INT((ALOG(FLOAT(N)))/B)
      I = 2**M
      IF (I .GE. N) I = I/2
      K = I
      NM1 = N-1
C
C CONDUCT BINARY SEARCH.
C
  103 CONTINUE
      K = K/2
      IF (XBAR .GE. X(I)) GO TO 104
      I = I-K
      GO TO 103
  104 CONTINUE
      IF (XBAR .LE. X(I+1)) RETURN
      I = MIN0(I+K,NM1)
      GO TO 103
      END


