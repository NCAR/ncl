
      PROGRAM CCPSET
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

      CALL GETDAT (Z, K, M, N) 
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Draw a perimeter around the whole frame to show off SET call
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PERIM(0,0,0,0)
C
C Force Data to be drawn in a rectangle twice as long as it is wide
C and only draw in the lower right hand corner of the screen
C
      CALL SET (0.5,0.98,0.125,0.375,0.,2.,0.,1.,1)
      CALL CPSETI('SET - DO-SET-CALL FLAG',0)
      CALL CPSETR('XC1 - X COORDINATE AT INDEX 1',0.)
      CALL CPSETR('XCM - X COORDINATE AT INDEX M',2.)
      CALL CPSETR('YC1 - Y COORDINATE AT INDEX 1',0.)
      CALL CPSETR('YCN - Y COORDINATE AT INDEX N',1.)
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
