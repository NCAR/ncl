C NCLFORTSTART
      SUBROUTINE DSIMPN(N,DX,FI,S)
      IMPLICIT NONE
C
C NCL:   fx = simpson(f,dx)
C
C Subroutine for integration over f(x) with the Simpson rule.
C N: # pts; FI: integrand f(x); DX: interval; S: integral.
C
C     -----------------> INPUT
      INTEGER N
      DOUBLE PRECISION FI(N),DX
C     -----------------> OUTPUT
      DOUBLE PRECISION S
C NCLEND
C     -----------------> LOCAL
      INTEGER I
      DOUBLE PRECISION S0,S1,S2

      S  = 0.D0
      S0 = 0.D0
      S1 = 0.D0
      S2 = 0.D0
      DO I=2,N-1,2
         S1 = S1 + FI(I-1)
         S0 = S0 + FI(I)
         S2 = S2 + FI(I+1)
      END DO
      S = DX*(S1+4.D0*S0+S2)/3.D0
C
C If N is even, add the last slice separately
C
      IF (MOD(N,2).EQ.0) S= S+DX*(5.D0*FI(N)+8.D0*FI(N-1)-FI(N-2))/12.D0

      RETURN
      END
