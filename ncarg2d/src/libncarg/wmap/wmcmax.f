      REAL FUNCTION WMCMAX(N,X)
      DIMENSION X(N)
C
C  Calculates the maximum absolute value of an array.
C
      AMS = ABS(X(1))
      DO 10 I=2,N
        AMS = MAX(AMS,ABS(X(I)))
   10 CONTINUE
C
      WMCMAX = AMS
      RETURN
      END
