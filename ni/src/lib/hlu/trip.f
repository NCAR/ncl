C
C
C  $Id: trip.f,v 1.1 1994-08-11 23:17:37 boote Exp $
C
C This subprogram is used by subroutine COEFF1 in library 
C ncarm.  It is lower-level, i.e. not called by users.
C *******************************************************
      SUBROUTINE TRIP (N,A,B,C,Y,Z,INT)
      DIMENSION       A(1)       ,B(1)       ,C(1)       ,Y(1)       ,
     1                Z(1)
      SAVE
C
C ARITHMETIC STATEMENT FUNCTION USED TO LOCATE ENTRIES IN ARRAY Z.
C
      II(INDEX)=(INDEX-1)*INT+1
C
C GAUSSIAN ELIMINATION
C
      BN = B(N)
      YN = Y(N)
      V = C(N)
      Y(1) = Y(1)/B(1)
      A(1) = A(1)/B(1)
      B(1) = C(1)/B(1)
      NM2 = N-2
      DO 101 J=2,NM2
         DEN = B(J)-A(J)*B(J-1)
         B(J) = C(J)/DEN
         Y(J) = (Y(J)-A(J)*Y(J-1))/DEN
         A(J) = -A(J)*A(J-1)/DEN
         BN = BN-V*A(J-1)
         YN = YN-V*Y(J-1)
         V = -V*B(J-1)
  101 CONTINUE
      DEN = B(N-1)-A(N-1)*B(N-2)
      B(N-1) = (C(N-1)-A(N-1)*A(N-2))/DEN
      Y(N-1) = (Y(N-1)-A(N-1)*Y(N-2))/DEN
      BN = BN-V*A(N-2)
      YN = YN-V*Y(N-2)
      V = A(N)-V*B(N-2)
C BACK SUBSTITUTION
      IIN = II(N)
      Z(IIN) = (YN-V*Y(N-1))/(BN-V*B(N-1))
      IIN2 = II(N-1)
      Z(IIN2) = Y(N-1)-B(N-1)*Z(IIN)
      NM1 = N-1
      IN = II(N)
      DO 102 J=2,NM1
         K = N-J
         IK = II(K)
      IKT = IK+INT
  102 Z(IK) = Y(K)-B(K)*Z(IKT)-A(K)*Z(IN)
      RETURN
      END


