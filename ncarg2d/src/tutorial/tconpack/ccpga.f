
      PROGRAM CCPGA
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

      PARAMETER (K=30,N=30,LRWK=1000,LIWK=1000)
      REAL Z(K,N), RWRK(LRWK)
      INTEGER IWRK(LIWK)
      
      CALL GETDAT (Z, K, M, N) 
C 
C Open GKS, open and activate a workstation.
C 
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Turn clipping off
C
      CALL GSCLIP (0)
C
C Set X and Y min and max values
C
      CALL CPSETR ('XC1 - X COORDINATE AT INDEX 1',2.0)
      CALL CPSETR ('XCM - X COORDINATE AT INDEX M',20.0)
      CALL CPSETR ('YC1 - Y COORDINATE AT INDEX 1',0.0)
      CALL CPSETR ('YCN - Y COORDINATE AT INDEX N',.01)
C     
C Make viewport slightly smaller so that labels will fit
C
      CALL CPSETR ('VPL - VIEWPORT LEFT',0.10)
      CALL CPSETR ('VPB - VIEWPORT BOTTOM',0.10)
C     
C Initialize Conpack
C
      CALL CPRECT(Z,K,M,N,RWRK,LRWK,IWRK,LIWK)
C     
C Draw and label perimeter
C
      CALL LABMOD('(E7.2)','(E7.2)',0,0,10,10,0,0,1)
      CALL GRIDAL(K-1,0,N-1,0,1,1,5,0.,0.)
      
C Draw Contours
      CALL CPCLDR(Z,RWRK,IWRK)
      
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
