      PROGRAM CCPILT
C 
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C 
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
      
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
C Set up label options
C
      CALL CPSETC('ILT - INFORMATION LABEL TEXT',
    1      'Modified Sine Function Contoured from $CMN$ to $CMX$ by $CIU$')
      CALL CPSETR('ILX - INFORMATION LABEL X COORDINATE',.5)
      CALL CPSETR('ILP - INFORMATION LABEL POSITION',0)
C
C Initialize Conpack
C
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
      
      OPEN (10,FILE='ccpex.dat',STATUS='OLD')
      L=K
      DO 10, I=1,L
         READ (10,*) (Z(I,J),J=1,N)
 10   CONTINUE
      
      RETURN
      END
