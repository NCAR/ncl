
      PROGRAM CCPRWC
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

      PARAMETER (K=400,N=400,LRWK=2000,LIWK=2000)
      REAL Z(K,N), RWRK(LRWK)
      INTEGER M, IWRK(LIWK)

      CALL GETDAT (Z, K, M, N) 
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Initialize Conpack
C
      CALL CPRECT(Z,K,M,N,RWRK,LRWK,IWRK,LIWK)
C
C Draw perimeter
C
      CALL CPBACK(Z, RWRK, IWRK)
C
C Turn on line labels for every line
C
      CALL CPPKCL(Z, RWRK, IWRK)
      CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCL)
      DO 10, I=1,NCL
         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
 10   CONTINUE
C
C Set RWC so that labels come out on some lines
C
      CALL CPSETI('RWC - REAL WORKSPACE FOR CONTOURS',125)
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
            Z(I,J)= 10.E-8*(-16.*REAL(I**2*J) +
     +            34*REAL(I*J**2) - REAL(6*I) + 93.)
 20      CONTINUE
 10   CONTINUE

      RETURN
      END

