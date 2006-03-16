
      PROGRAM CCPHAND
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)

      PARAMETER (K=40,N=40,LRWK=1000,LIWK=1000,INCL=19)
      REAL Z(K,N), RWRK(LRWK), RLEVEL(INCL)
      INTEGER M, IWRK(LIWK)
      
      DATA RLEVEL /-20., -10., -5., -4., -3., -2., -1., 0., 
     1     1., 2., 3., 4., 5., 10., 20., 30., 40., 50., 100. /
      
      CALL GETDAT (Z, K, M, N) 
C 
C Open GKS, open and activate a workstation.
C 
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C     
C Read in hand picked levels
C     
      CALL CPSETI('CLS - CONTOUR LEVEL SELECTION FLAG',0)
      CALL CPSETI('NCL - NUMBER OF CONTOUR LEVELS',INCL)
      
      DO 10, I=1,INCL
         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETR('CLV - CONTOUR LEVEL VALUE',RLEVEL(I))
         IF (MOD(RLEVEL(I),5.).EQ.0)
     1        CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
 10   CONTINUE
C     
C Call conpack normally
C     
      CALL CPRECT(Z,K,M,N,RWRK,LRWK,IWRK,LIWK)
      CALL CPBACK(Z, RWRK, IWRK)
      CALL CPCLDR(Z,RWRK,IWRK)
C     
C Close frame and close GKS
C     
      CALL FRAME
C 
C Deactivate and close workstation, close GKS.
C 
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
      
      SUBROUTINE GETDAT (Z, K, M, N)
      INTEGER I,J,K,M,N
      REAL Z(K,N)
      
      M=K
      DO 10, I=1,M
         DO 20, J=1,N
            Z(I,J)= 10.E-5*(-16.*REAL(I**2*J) +
     1           34*REAL(I*J**2) - REAL(6*I) + 93.)
 20      CONTINUE
 10   CONTINUE
      
      RETURN
      END
