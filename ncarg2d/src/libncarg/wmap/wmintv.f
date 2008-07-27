C
C	$Id: wmintv.f,v 1.4 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMINTV(X0,X,N,NDX)
C
C  Given a value X0 and an array X (of length N) of monotone
C  increasing values, this subroutine returns the index NDX
C  such that:
C
C    NDX = 1    if  X0 .LT. X(2)
C    NDX = N-1  if  X(N-1) .LT. X0
C    NDX = I    if  X(I) .LE. X0  and  X0 .LE. X(I+1)
C
      DIMENSION X(N)
C
      XX = X0
C
C  Handle extremes.
C
      IF (XX .LE. X(2)) THEN
        NDX = 1
        RETURN
      ELSE IF (XX .GE. X(N-1)) THEN
        NDX = N-1
        RETURN
      END IF
C
C  Binary search.
C
      IL = 2
      IH = N-1
   10 CONTINUE
      I = (IL+IH)/2
      IF (XX .LT. X(I)) THEN
         IH = I
      ELSE IF (XX .GT. X(I+1)) THEN
         IL = I+1
      ELSE
         NDX = I
         RETURN
      END IF
      GO TO 10
      END
