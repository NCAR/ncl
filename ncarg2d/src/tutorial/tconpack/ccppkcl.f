
      PROGRAM CCPPKCL
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
      REAL ZREG(K,N), RWRK(LRWK)
      INTEGER M, NOCL, IWRK(LIWK)
      
      CALL GETDAT (ZREG, K, M, N) 
C     
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      
      CALL CPSETI('CLS - CONTOUR LEVEL SELECTION FLAG',-20)
      CALL CPRECT(ZREG,K,M,N,RWRK,LRWK,IWRK,LIWK)
C     
C Force contour lines to be chosen
C
      CALL CPPKCL (ZREG, RWRK, IWRK)
C
C Get the number of contour levels chosen
C
      CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NOCL)
      CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',NOCL+1)
      CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NOCL+1)
      CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',0.0)
      
      CALL CPBACK(ZREG, RWRK, IWRK)
      CALL CPCLDR(ZREG,RWRK,IWRK)
C     
C Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
      
      SUBROUTINE GETDAT (ZREG, K, M, N)
      INTEGER I,J,K,M,N
      REAL ZREG(K,N)
      
      M=K
      DO 10, I=1,M
         DO 20, J=1,N
            ZREG(I,J)= 10.E-5*(-16.*REAL(I**2*J) +
     +            34*REAL(I*J**2) - REAL(6*I) + 93.)
 20      CONTINUE
 10   CONTINUE
      
      RETURN
      END
      
