
      PROGRAM CCPCLD
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
      INTEGER I, M, IWRK(LIWK),IBTS (16,7)
      
      DATA IBTS / 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 
     1     1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
     2     1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
     3     1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0,
     4     1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
     5     1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1,
     6     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /
      
      CALL GETDAT (Z, K, M, N) 
C 
C Open GKS, open and activate a workstation.
C 
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Choose the number of contour levels
C
      CALL CPSETI('CLS - CONTOUR LEVEL SELECTION FLAG',-7)
      
C Call conpack normally
      CALL CPRECT(Z,K,M,N,RWRK,LRWK,IWRK,LIWK)
      CALL CPPKCL(Z, RWRK, IWRK)
C Set a different dash pattern for each contour line
      CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NOCL)
      DO 11, I=1,NOCL 
         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETI('CLD - CONTOUR LINE DASH PATTERN',
     +        IPAT(IBTS(1,I)))
 11   CONTINUE
      
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
     +           34*REAL(I*J**2) - REAL(6*I) + 93.)
 20      CONTINUE
 10   CONTINUE
      
      RETURN
      END
      
      INTEGER FUNCTION IPAT (IBTS)
      DIMENSION IBTS(16)
      
      IPAT=0
      DO 101 I=1,16
         IPAT=IOR(ISHIFT(IPAT,1),IBTS(I))
 101  CONTINUE
      
      RETURN
      END
