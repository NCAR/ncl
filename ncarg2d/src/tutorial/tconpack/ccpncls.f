      PROGRAM CCPREC
C 
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C 
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
      
      PARAMETER (M=40,N=40,LRWK=1000,LIWK=1000)
      REAL Z(M,N), RWRK(LRWK)
      INTEGER IWRK(LIWK)
      
      CALL GETDAT (Z, M, N) 
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Make Conpack draw exactly 10 equally spaced contour levels
C
      CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION FLAG',-10)
C
C Initialize Conpack
C
      CALL CPRECT (Z, M, M, N, RWRK, LRWK, IWRK, LIWK)
C
C Draw perimeter
C
      CALL CPBACK (Z, RWRK, IWRK)
C
C Draw Contours
C
      CALL CPCLDR (Z, RWRK, IWRK)
C
C Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
      
      SUBROUTINE GETDAT (Z, M, N)
      INTEGER I,J,M,N
      REAL Z(M,N)
      
      DO 10, I=1,M
         DO 20, J=1,N
            Z(I,J)= 10E-5*(-16.*REAL(I**2*J) +
     +           34*REAL(I*J**2) - REAL(6*I) + 93.)
 20      CONTINUE
 10   CONTINUE
      
      RETURN
      END
      
      
