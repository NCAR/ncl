      SUBROUTINE VORS(NV,NF,V,A,B,C,ND,XMSG)
      IMPLICIT NONE
      INTEGER NV,NF,ND
      DOUBLE PRECISION V(ND,NF),A(NV),B(NV),C(NV), XMSG

      INTEGER I,J,KR,M,N
      DOUBLE PRECISION T,PI,TWOPI,REPS,AA,BB,CC,DD,XN,XD,Y,CY,SY,TEMP

      DOUBLE PRECISION SUMF,SCPF

c +++ very old fortran 66 ++++ used arithmetic "if"
c         I have made some minor changes made
c
c ***** This gives the same answers as IMSL's FROTA *****
c *****      with w=1.0, norm=1, maxit=30           *****
c
c nv    - # of variables [rows]
c nf    - # of factors   [columns]
c v     - matrix to be rotated
c a     - % variation of rotated factors
c b     - communalities of variables [%]
c c     - work vector
c nd    - lead dimension [# rows] of v in the main program
c
c Orthogonal Rotation according to varimax  criteria.
c Kaiser row normalization is used prior to rotation.
c ie: On input, each row is normalized by dividing each row
c     by the square root of the sum of its squared elements.
c     Before returning each row is denormalized.

c Each possible pair of factor vectors [principal components]
c    is rotated to maximize the column-variance criterion
c    in turn until a complete pass through all combinations
c    does not result in any rotations of more than "reps" radians.
c The percentages of trace are recomputed for each factor vector
c    and returned in vector "a".
c The percentages of trace are computed for each row of the
c    rotated matrix and returned in vector "b". When  computed
c    as proportions (row sums of squares of the loading matrix) 
c    they are called "communalities" and are not affected by the 
c    rotation process. This will be exactly 100% only when all
c    the variance of the particular variable is completely
c    accounted for by the extracted factors.
c
c D.J. Veldman
c Fortran Programming for the Behavioral Sciences
c Holt, Rinehart, Winston, 1967
c pp 214-215

      T = NV
      PI = 4.D0*ATAN(1.D0)
      TWOPI = 2.D0*PI
C epsilon (radians)=1 deg
c c c reps = pi/180.
C this matches IMSL
      REPS = 0.0001D0

      DO N=1,NV
         A(N) = XMSG
         B(N) = XMSG
         C(N) = XMSG
      END DO

C normalize the rows of v
      DO I = 1,NV
          B(I) = SQRT(SUMF(V, (-I), (-NF),ND,XMSG))
          IF (B(I).NE.0.0D0 .AND. B(I).NE.XMSG) THEN
              DO J = 1,NF
                 IF (V(I,J).NE.XMSG) THEN
                     V(I,J) = V(I,J)/B(I)
                 END IF
             END DO
          END IF
      END DO

   10 CONTINUE
      KR = 0
      DO M = 1,NF
          DO N = M,NF
              IF (M.NE.N) THEN
C compute angle of rotation
                  DO I = 1,NV
                      IF (V(I,M).NE.XMSG .AND. V(I,N).NE.XMSG) THEN
                          A(I) = V(I,M)*V(I,M) - V(I,N)*V(I,N)
                          C(I) = 2.0D0*V(I,M)*V(I,N)
                      END IF
                  END DO

                  AA = SUMF(A,1,NV,ND,XMSG)
                  BB = SUMF(C,1,NV,ND,XMSG)
                  CC = SUMF(A,1,(-NV),ND,XMSG) - SUMF(C,1,(-NV),ND,XMSG)
                  DD = SCPF(A,C,1,1,NV,ND,XMSG)*2.0D0
                  XN = DD - 2.0D0*AA*BB/T
c compute angle of rotation
                  XD = CC - (AA*AA-BB*BB)/T

                  Y = ATAN(XN/XD)
                  IF (XD.LT.0.D0) THEN
                      IF (XN.GE.0.D0) Y = Y + TWOPI
                      Y = Y - PI
                  END IF
                  Y = Y/4.0D0
                  IF (ABS(Y).LT.REPS) GO TO 1002

c count rotations
                  KR = KR + 1
                  CY = COS(Y)
                  SY = SIN(Y)
c rotate the axis
                  DO I = 1,NV
                     IF (V(I,M).NE.XMSG .AND. V(I,N).NE.XMSG) THEN      
                         TEMP = V(I,M)*CY + V(I,N)*SY
                         V(I,N) = V(I,N)*CY - V(I,M)*SY
                         V(I,M) = TEMP
                     END IF
                  END DO
              END IF
 1002         CONTINUE
          END DO
      END DO
      IF (KR.GT.0) GO TO 10
c denormailize rows of v
      DO J = 1,NF
          DO I = 1,NV
              IF (V(I,J).NE.XMSG .AND. B(I).NE.XMSG) THEN
                  V(I,J) = V(I,J)*B(I)
              END IF
          END DO
c % variation
          A(J) = (SUMF(V,J, (-NV),ND,XMSG)/T)*100.D0
      END DO

      DO I = 1,NV
C % communitalities
          IF (B(I).NE.XMSG) THEN
              B(I) = B(I)*B(I)*100.D0
          END IF
      END DO

      RETURN
      END
c ------------------------------------
      DOUBLE PRECISION FUNCTION SUMF(X,KK,NN,ND,XMSG)
      IMPLICIT NONE
      INTEGER KK,NN,ND
      DOUBLE PRECISION X(ND,1), XMSG

c         awkward due to arithmetic if in orig code
c computes sum of x or x**2 from a vector
c x   = input array
c nn  = number of values to be summed.
c       if (nn.lt.0) sum of x**2 is computed
c kk  = row or column number if x is a matrix.
c       kk=1 means x is a vector
c       kk>1 means it is a column vector
c       kk<0 means it is a   row  vector
c nd  = actual first dimension of x in the
c       aclling routine

C local
      INTEGER N,K,I

      SUMF = 0.0D0
      N = IABS(NN)
      K = IABS(KK)
      IF (NN.LE.0) THEN
          IF (NN.EQ.0) GO TO 55
          IF (KK.LT.0) GO TO 15
          IF (KK.EQ.0) GO TO 55
          GO TO 25
      END IF
      IF (KK.LT.0) GO TO 35
      IF (KK.EQ.0) GO TO 55
      GO TO 45
   15 CONTINUE
      DO I = 1,N
          IF (X(K,I).NE.XMSG) THEN
              SUMF = SUMF + X(K,I)*X(K,I)
          END IF
      END DO
      RETURN

   25 CONTINUE
      DO I = 1,N
          IF (X(I,K).NE.XMSG) THEN
              SUMF = SUMF + X(I,K)*X(I,K)
          END IF
      END DO
      RETURN

   35 CONTINUE
      DO I = 1,N
          IF (X(K,I).NE.XMSG) THEN
              SUMF = SUMF + X(K,I)
          END IF
      END DO
      RETURN

   45 CONTINUE
      DO I = 1,N
          IF (X(I,K).NE.XMSG) THEN
              SUMF = SUMF + X(I,K)
          END IF
      END DO
   55 CONTINUE
      RETURN

      END
c ------------------------------------------------
      DOUBLE PRECISION FUNCTION SCPF(X,Y,KX,KY,N,ND,XMSG)
      IMPLICIT NONE
      INTEGER KX,KY,N,ND
      DOUBLE PRECISION X(ND,1),Y(ND,1),XMSG

C local
      INTEGER J,K,I

c         awkward due to arithmetic if in orig code
c computes the sum of cross products (scalar product) of two vectors
c x,y - input arrays [may be the same]
c kx,ky - row  or column numbers for x and y (if matrices)
c         set=1 if vector
c         if kx or ky is positive and not 1, it is a column vector
c         if kx or ky is negative and not 1, it is a   row  vector
c n     - the number of products to be summed, elements of each vector
c nd    - actual first dimension of x in the calling routine

      J = IABS(KX)
      K = IABS(KY)
      SCPF = 0.0D0

      IF (KX.LE.0) THEN
          IF (KX.EQ.0) GO TO 55
          IF (KY.LT.0) GO TO 15
          IF (KY.EQ.0) GO TO 55
          GO TO 25
      END IF
      IF (KY.LT.0) GO TO 35
      IF (KY.EQ.0) GO TO 55
      GO TO 45
   15 CONTINUE
      DO I = 1,N
          IF (X(J,I).NE.XMSG .AND. Y(K,I).NE.XMSG) THEN
              SCPF = SCPF + X(J,I)*Y(K,I)
          END IF
      END DO
      RETURN

   25 CONTINUE
      DO I = 1,N
          IF (X(J,I).NE.XMSG .AND. Y(I,K).NE.XMSG) THEN
              SCPF = SCPF + X(J,I)*Y(I,K)
          END IF
      END DO
      RETURN

   35 CONTINUE
      DO I = 1,N
          IF (X(I,J).NE.XMSG .AND. Y(K,I).NE.XMSG) THEN
              SCPF = SCPF + X(I,J)*Y(K,I)
          END IF
      END DO
      RETURN

   45 CONTINUE
      DO I = 1,N
          IF (X(I,J).NE.XMSG .AND. Y(I,K).NE.XMSG) THEN
              SCPF = SCPF + X(I,J)*Y(I,K)
          END IF
      END DO
      RETURN

   55 CONTINUE
      RETURN
      END

