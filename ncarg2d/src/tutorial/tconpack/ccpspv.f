
      PROGRAM CCPSPV
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

      PARAMETER (K=40,N=40,LRWK=1000,LIWK=1000)
      REAL Z(K,N), RWRK(LRWK)
      INTEGER M, IWRK(LIWK)

      CALL GETDAT (Z, K, M, N) 
C
C Mark values near the center of the array with a value of -1.0
C
      DO 5, I=2*K/5,3*K/5
         DO 15, J=2*N/7,3*N/7
            Z(I,J)=-1.0
 15      CONTINUE
 5    CONTINUE
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Tell Conpack that data points with a value of -1.0 shouldn't be drawn
C
      CALL CPSETR('SPV - SPECIAL VALUE',-1.0)
C
C Force Conpack to outline the special value region
C
      CALL CPSETI('PAI - PARAMETER ARRAY INDEX',-2)
      CALL CPSETI('CLU - CONTOUR LEVEL USE FLAGS',1)
C
C Initialize Conpack
C
      CALL CPRECT(Z,K,M,N,RWRK,LRWK,IWRK,LIWK)
C
C Draw perimeter
C
      CALL CPBACK(Z, RWRK, IWRK)
C
C Draw Contours
C
      CALL CPCLDR(Z,RWRK,IWRK)
C
C Close frame and close GKS
C
      CALL FRAME
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
