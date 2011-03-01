C
C
C       $Id: interp.f,v 1.1 1994-08-11 23:17:27 boote Exp $
C
C This subprogram is used by subroutines TERP1 and TERP2 in 
C library ncarm.  It is lower-level, i.e. not called by users.
C *************************************************************
      SUBROUTINE INTERP (N,X,F,W,Y,I,INT,TAB,ITAB)
      DIMENSION       X(1)       ,F(1)       ,W(1)       ,TAB(3)
     -                ,ITAB(3)
      SAVE
C
C ARITHMETIC STATEMENT FUNCTION USED TO LOCATE ENTRIES IN F AND W ARRAYS
C
      II(INDEX)=(INDEX-1)*INT+1
C
C PERFORM INTERPOLATION OR EXTRAPOLATION
C
      FLK = X(I+1)-X(I)
      FLP = X(I+1)-Y
      FL0 = Y-X(I)
      I0 = II(I)
      IP = I0+INT
      IF (ITAB(1) .NE. 0) GO TO 101
      GO TO 102
  101 CONTINUE
C
C CALCULATE F(Y)
C
      A = (W(I0)*FLP**3+W(IP)*FL0**3)/(6.*FLK)
      B = (F(IP)/FLK-W(IP)*FLK/6.)*FL0
      C = (F(I0)/FLK-W(I0)*FLK/6.)*FLP
      TAB(1) = A+B+C
  102 CONTINUE
      IF (ITAB(2) .NE. 0) GO TO 103
      GO TO 104
  103 CONTINUE
C
C CALCULATE FIRST DERIVATIVE AT Y
C
      A = (W(IP)*FL0**2-W(I0)*FLP**2)/(2.*FLK)
      B = (F(IP)-F(I0))/FLK
      C = (W(I0)-W(IP))*FLK/6.
      TAB(2) = A+B+C
  104 CONTINUE
      IF (ITAB(3) .NE. 0) GO TO 105
      GO TO 106
  105 CONTINUE
C
C CALCULATE SECOND DERIVATIVE AT Y
C
      TAB(3) = (W(I0)*FLP+W(IP)*FL0)/FLK
  106 CONTINUE
      RETURN
      END


