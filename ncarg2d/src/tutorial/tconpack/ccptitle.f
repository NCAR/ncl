      PROGRAM CCPTIT
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)

      PARAMETER (M=40,N=40,LRWK=3500,LIWK=4000)
      REAL Z(M,N), RWRK(LRWK), SIZE, Y
      INTEGER IWRK(LIWK)

      CALL GETDAT (Z, M, M, N)
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP (0)
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
      CALL CPCLDR(Z,RWRK,IWRK)
C
C Draw a Title
C
      CALL GETSET(VPL,VPR,VPB,VPT,WL,WR,WB,WT,LL)
      SIZE = AMIN1(.5 * (1.0 - VPT),.017)
      Y = AMIN1((1.0+VPT)/2.0+.017,.017+VPT)
      CALL SET (0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)
      CALL PLCHHQ (.5, Y, 'An Interesting Data Field',SIZE, 0., 0.)
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

      L=K
      DO 10, I=1,L
         READ (*,*) (Z(I,J),J=1,N)
 10   CONTINUE

      RETURN
      END
