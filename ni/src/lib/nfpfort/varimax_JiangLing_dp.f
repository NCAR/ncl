C NCLFORTSTART
      SUBROUTINE ZROTEOF (NV,NF,V,ND, EVAL, ROTPCV, ROTVAR, VMSG, IOPT,
     +                    KFLAG)
      IMPLICIT NONE
      INTEGER  NV,NF,ND, IOPT, KFLAG
      DOUBLE PRECISION V(ND,NF),EVAL(NF),ROTPCV(NF),ROTVAR(NF), VMSG
C NCLEND
      INTEGER N,M
      DOUBLE PRECISION A(NV),B(NV),C(NV), TVAR

      TVAR = EVAL(1)/(ROTPCV(1)*0.01d0)
c     PRINT *, "TVAR=",TVAR

C check for missing values: Use appropriate subroutine
      KFLAG = 0
      DO N=1,NF
        DO M=1,ND
           IF (V(M,N).EQ.VMSG) THEN
               KFLAG = 1
               GO TO 100
           END IF
        END DO
      END DO
  100 CONTINUE

      IF (IOPT.NE.0) THEN
C Scale the input eigenvector matrix
          DO N=1,NF
            DO M=1,ND
               IF (V(M,N).NE.VMSG) THEN
                   V(M,N) = V(M,N)*DSQRT( EVAL(N) )
               END IF
            END DO
          END DO
      END IF

      DO M=1,NV
         A(M) = VMSG
         B(M) = VMSG
         C(M) = VMSG
      END DO

      IF (KFLAG.EQ.0) THEN
          CALL ZVORS(NV,NF,V,A,B,C,ND) 
      ELSE
          CALL VORSMSG(NV,NF,V,A,B,C,ND,VMSG) 
      END IF  

      DO N=1,NF
        ROTVAR(N) = 0.0D0
        DO M=1,ND
           IF (V(M,N).NE.VMSG) THEN
              ROTVAR(N) = ROTVAR(N) + V(M,N)**2
           END IF
        END DO
      END DO
      
C % variance explained

      IF (IOPT.NE.0) THEN
          DO N=1,NF
            ROTPCV(N) = (ROTVAR(N)/TVAR)*100D0
          END DO
      ELSE 
          DO N=1,NF
            ROTPCV(N) = 100.D0/ND
          END DO
      END IF
      
      IF (IOPT.LE.0) RETURN

C unscale [denormalize]

      DO N=1,NF
        DO M=1,ND
           IF (V(M,N).NE.VMSG) THEN
               V(M,N) = V(M,N)/DSQRT( ROTVAR(N) )
           END IF
        END DO
      END DO


      RETURN
      END
c ------------------------------------
      SUBROUTINE ZVORS(NV,NF,V,A,B,C,ND)
      IMPLICIT NONE
      INTEGER NV,NF,ND
      DOUBLE PRECISION V(ND,NF),A(NV),B(NV),C(NV)

      INTEGER I,J,KR,M,N
      DOUBLE PRECISION T,PI,TWOPI,REPS,AA,BB,CC,DD,XN,XD,Y,CY,SY,TEMP

      DOUBLE PRECISION DSUMFR,DSCPFR
      DOUBLE PRECISION VC, VCL, X1, X2, X3

c +++ very old fortran 66 ++++ used arithmetic "if"
c         I have made some minor changes made

c note: changed names SCPF=>DSCPFR and   SUMF=>DSUMFR
c       to minimize possible name conflicts with other funtions/subroutines
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
c    and returned in vector "a" (actually, the first "nfac" 
c    elements of "a").
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


C      write(*,*) NF,ND,NV


      T = NV
      PI = 4.D0*ATAN(1.D0)
      TWOPI = 2.D0*PI
C epsilon (radians)=1 deg
c c c reps = pi/180.
C this matches IMSL
      REPS = 0.0001D0

C normalize the rows of v
      DO I = 1,NV
          B(I) = SQRT(DSUMFR(V, (-I), (-NF),ND))
          DO J = 1,NF
              V(I,J) = V(I,J)/B(I)
          END DO
      END DO
      
      
      
      X1 = 0
      X3 = 0
      
      DO J = 1, NF
         X2 = 0
         DO I = 1, ND
            X1 = X1 + V(I,J)**4
            X2 = X2 + V(I,J)**2
         END DO         
         X3 = X3 + X2**2
      END DO
      
      VC = X1 - X3 / ND
      

   10 CONTINUE
      
      DO WHILE(.true.)
      
      VCL = VC
      
      DO M = 1,NF
          DO N = M,NF
              IF (M.NE.N) THEN
C compute angle of rotation
                  DO I = 1,NV
                      A(I) = V(I,M)*V(I,M) - V(I,N)*V(I,N)
                      C(I) = 2.0D0*V(I,M)*V(I,N)
                  END DO

                  AA = DSUMFR(A,1,NV,ND)
                  BB = DSUMFR(C,1,NV,ND)
                  CC = DSUMFR(A,1, (-NV),ND) - DSUMFR(C,1, (-NV),ND)
                  DD = DSCPFR(A,C,1,1,NV,ND)*2.0D0
                  XN = DD - 2.0D0*AA*BB/T
c compute angle of rotation
                  XD = CC - (AA*AA-BB*BB)/T

                  Y = ATAN(XN/XD)
                  IF (XD.LT.0.D0) THEN
                      IF (XN.GE.0.D0) Y = Y + TWOPI
                      Y = Y - PI
                  END IF
                  Y = Y/4.0D0

c count rotations
                  CY = COS(Y)
                  SY = SIN(Y)
c rotate the axis
                  DO I = 1,NV
                      TEMP = V(I,M)*CY + V(I,N)*SY
                      V(I,N) = V(I,N)*CY - V(I,M)*SY
                      V(I,M) = TEMP
                  END DO
              END IF
          END DO
      END DO
     
      X1 = 0
      X3 = 0
      
      DO J = 1, NF
         X2 = 0
         DO I = 1, ND
            X1 = X1 + V(I,J)**4
            X2 = X2 + V(I,J)**2
         END DO         
         X3 = X3 + X2**2
      END DO
      
      VC = X1 - X3 / ND      
      
      
      IF(ABS(VC-VCL) .lt. 1E-5) EXIT      
      
      END DO
      
c denormailize rows of v
      DO J = 1,NF
          DO I = 1,NV
              V(I,J) = V(I,J)*B(I)
          END DO
c % variation: note the A is dimensioned A(NV)
c              but the % variation is in A(NF) [a subset of A]
          A(J) = (DSUMFR(V,J, (-NV),ND)/T)*100.D0
      END DO

      DO I = 1,NV
C % communitalities
          B(I) = B(I)*B(I)*100.D0
      END DO

      RETURN
      END
