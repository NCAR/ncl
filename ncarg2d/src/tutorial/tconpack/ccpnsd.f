
      PROGRAM CCPNSD
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
      
      DIMENSION CIT(10),LIT(10)
      
      DATA CIT /1.,2.,3.,4.,5.,6.,7.,8.,9.,0./
      DATA LIT /5, 5, 5, 5, 5, 5, 5, 5, 5, 5 /
      
      CALL GETDAT (Z, K, M, N) 
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP(0)
C
C Change nice values to match old CONREC nice values
C Draw labels at every 5th contour level no matter which contour
C level interval is chosen.
C
      DO 101 I=1,10
         CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETR ('CIT - CONTOUR INTERVAL TABLE',CIT(I))
         CALL CPSETI ('LIT - LABEL INTERVAL TABLE',LIT(I))
 101  CONTINUE
      
      CALL CPSETI('NSD - NUMBER OF SIGNIFICANT DIGITS',-5)
      CALL CPSETI('NLS - NUMERIC LEFTMOST SIGNIFICANT DIGIT',0)
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
C Draw Labels
C
      CALL CPLBDR(Z,RWRK,IWRK)
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
            Z(I,J)= 10.E-7*(-16.*REAL(I**2*J) +
     +            34*REAL(I*J**2) - REAL(6*I) + 93.)
 20      CONTINUE
 10   CONTINUE
      
      RETURN
      END
