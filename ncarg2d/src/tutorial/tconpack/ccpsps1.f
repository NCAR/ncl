
      PROGRAM CCPSPS1
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

      PARAMETER (K=9,N=7,LRWK=1000,LIWK=1000,LZDT=2000)
      REAL Z(K,N), ZDAT(LZDT), RWRK(LRWK)
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
      CALL CPSPS1(Z,K,M,N,RWRK,LRWK,IWRK,LIWK,ZDAT,LZDT)
C
C Draw perimeter
C
      CALL CPBACK(ZDAT, RWRK, IWRK)
C
C Use a different line attribute on every line drawn
C Draw Contours
C
      CALL CPCLDR(ZDAT,RWRK,IWRK)
C
C Mark data points
C
      CALL MARK (X, Y, M, N)
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
     +           34*REAL(I*J**2) - REAL(6*I) + 93.)
 20      CONTINUE
 10   CONTINUE
      
      RETURN
      END
      
      SUBROUTINE MARK (X, Y, M, N)
      
      REAL X, Y
      INTEGER I, J
      
      CALL GETSET(DUM1,DUM2,DUM3,DUM4,XMIN,XMAX,YMIN,YMAX,IDUM5)
      CALL GSMKSC(.5)
      
      DO 11 I=1,M
         DO 21 J=1,N
            X = REAL(I-1)*(XMAX-XMIN)/REAL(M-1)+XMIN
            Y = REAL(J-1)*(YMAX-YMIN)/REAL(N-1)+YMIN
            CALL POINTS (X, Y, 1, -4, 0)
 21      CONTINUE
 11   CONTINUE
      
      
      RETURN
      END

