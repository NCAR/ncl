
      PROGRAM CCPILS
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
C Turn off the flag that causes CONPACK to look for overlap problems
C as high/low labels are generated.  This will cause the output to
C look as it did when the tutorial was written.  (A bug fix affected
C the output of this example by causing CONPACK to look for overlap;
C prior to that, it had been erroneously failing to do so.)  - DJK
C (03/08/96).
C
      CALL CPSETI('HLO - HIGH/LOW LABEL OVERLAP FLAG',0)
C
C Set label sizes 
C
      CALL CPSETR('HLS - HIGH/LOW LABEL SIZE',.030)
      CALL CPSETR('ILS - INFORMATION LABEL SIZE',.005)
C
C Initialize Conpack
C
      CALL CPRECT(Z, M, M, N, RWRK, LRWK, IWRK, LIWK)
C
C Draw Perimeter
C
      CALL CPBACK(Z, RWRK, IWRK)
C
C Draw Labels
C
      CALL CPLBDR(Z,RWRK,IWRK)
C
C Close frame
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
