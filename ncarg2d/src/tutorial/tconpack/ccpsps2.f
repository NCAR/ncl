
      PROGRAM CCPSPS2
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

      PARAMETER (M=20,N=30,LRWK=3500,LIWK=3500,LZRG=2000)
      REAL X(M), Y(N), Z(M,N), ZREG(LZRG), RWRK(LRWK)
      INTEGER IWRK(LIWK)
      DATA X /  1.,  2.,  3.,  5.,  7., 11., 13., 17., 19., 23.,
     +     29., 31., 37., 41., 43., 47., 53., 59., 61., 67./

      CALL GETDAT (X, Y, Z, M, N) 
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Turn clipping off
C
      CALL GSCLIP(0)
C
C Limit viewport so there's room to mark the data points
C
      CALL CPSETR('VPR',.8)
C
C Initialize Conpack
C
      CALL CPSPS2(X,Y,Z,M,M,N,RWRK,LRWK,IWRK,LIWK,ZREG,LZRG)
C
C Draw perimiter
C
      CALL CPBACK(ZREG, RWRK, IWRK)
C
C Draw Contours
C
      CALL CPCLDR(ZREG,RWRK,IWRK)
C
C Mark data points
C
      CALL MARK (X,Y,M,N)
C
C Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE GETDAT (X, Y, Z, M, N)
      INTEGER I,J,M,N
      REAL X(M), Y(N), Z(M,N)
C
C X and Y data locations must be in increasing order.
C
      Y(1)=1.
      DO 10, I=2,N
         Y(I) = 1.1*Y(I-1) + 1./REAL(I)
 10   CONTINUE

      DO 30, I=1,M
         DO 40, J=1,N
            Z(I,J)= 10.E-5*(-16.*REAL(X(I)**2*Y(J)) +
     +            34*REAL(X(I)*Y(J)**2) - REAL(6*X(I)) + 93.)
 40      CONTINUE
 30   CONTINUE

      RETURN
      END

      SUBROUTINE MARK (X, Y, M, N)

      REAL X(M), Y(N)
      INTEGER I, J

      CALL GSMKSC(.5)

      DO 11 I=1,M
         DO 21 J=1,N
            CALL POINTS (X(I), Y(J), 1, -4, 0)
 21      CONTINUE
 11   CONTINUE

      RETURN
      END
