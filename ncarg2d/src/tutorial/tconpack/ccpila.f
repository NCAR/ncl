
      PROGRAM CCPILA
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

      PARAMETER (M=40,N=40,LRWK=3500,LIWK=4000)
      REAL Z(M,N), RWRK(LRWK)
      INTEGER IWRK(LIWK)
      
      CALL GETDAT (Z, M, M, N)
C 
C Open GKS, open and activate a workstation.
C 
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP (0)
C 
C Initialize Conpack
C 
      CALL CPSETR('HLA - HIGH/LOW LABEL ANGLE',30.)
      CALL CPSETR('ILA - INFORMATION LABEL ANGLE',90.)
      CALL CPSETR('ILX - INFORMATION LABEL X COORD',-0.02)
      CALL CPSETR('ILY - INFORMATION LABEL Y COORD',0.25)
      CALL CPSETI('ILP - INFORMATION LABEL POSITION', 0)
      CALL CPRECT(Z, M, M, N, RWRK, LRWK, IWRK, LIWK)
C 
C Draw Perimeter
C 
      CALL CPBACK(Z, RWRK, IWRK)
C 
C Draw Contours
C 
      CALL CPLBDR(Z,RWRK,IWRK)
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
      
      L=K
      DO 10, I=1,L
         READ (*,*) (Z(I,J),J=1,N)
 10   CONTINUE
      
      RETURN
      END
