C NCLFORTSTART
      SUBROUTINE ROTEOF (NV,NF,V,ND, EVAL, ROTPCV, ROTVAR, VMSG, IOPT) 
      IMPLICIT NONE
      INTEGER  NV,NF,ND, IOPT
      DOUBLE PRECISION V(ND,NF),EVAL(NF),ROTPCV(NF),ROTVAR(NF), VMSG
C NCLEND
      INTEGER N,M, KFLAG
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

      IF (IOPT.LE.0) THEN
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
          CALL VORS(NV,NF,V,A,B,C,ND) 
      ELSE
          CALL VORSMSG(NV,NF,V,A,B,C,ND,VMSG) 
      END IF  

      DO N=1,NF
         ROTVAR(N)   = 0.0D0
        DO M=1,ND
           ROTVAR(N) = ROTVAR(N) + V(M,N)**2
        END DO
      END DO
      
C % variance explained

      IF (IOPT.LE.0) THEN
          DO N=1,NF
            ROTPCV(N) = (ROTVAR(N)/TVAR)*100D0
          END DO
      ELSE 
          DO N=1,NF
            ROTPCV(N) = 100.D0/ND
          END DO
      END IF
      
      IF (IABS(IOPT).EQ.1) RETURN

C unscale [denormalize]

      IF (IOPT.EQ.0) THEN
          DO N=1,NF
            DO M=1,ND
               IF (V(M,N).NE.VMSG) THEN
                   V(M,N) = V(M,N)/DSQRT( ROTVAR(N) )
               END IF
            END DO
          END DO
      END IF


      RETURN
      END
C ------------
      SUBROUTINE VORSMSG(NV,NF,V,A,B,C,ND,VMSG)
      IMPLICIT NONE
      INTEGER NV,NF,ND
      DOUBLE PRECISION V(ND,NF),A(NV),B(NV),C(NV), VMSG

c local
      INTEGER N,M, NVTMP
      DOUBLE PRECISION VTMP(ND,NF)

      DO N=1,NF
        DO M=1,ND
           VTMP(M,N) = VMSG
        END DO
      END DO

      DO N=1,NF
         NVTMP = 0
        DO M=1,ND
           IF (V(M,N).NE.VMSG) THEN
               NVTMP = NVTMP+1 
               VTMP(NVTMP,N) = V(M,N)
           END IF
        END DO
      END DO

      CALL VORS(NVTMP,NF,VTMP,A,B,C,ND) 

      DO N=1,NF
         NVTMP = 0
        DO M=1,ND
           IF (V(M,N).NE.VMSG) THEN
               NVTMP = NVTMP+1 
               V(M,N) = VTMP(NVTMP,N)
           END IF
        END DO
      END DO

      RETURN
      END
