
      PROGRAM FTEX06
C
C  Example of SURF1/SURF2.
C
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
C
      PARAMETER (NXI=11,NYI=17,NXO=31,NYO=21,IDTEMP=2*NYI+NXI)
C
      DIMENSION X(NXI),Y(NYI),Z(NXI,NYI)
      DIMENSION ZX1(NYI),ZXM(NYI),ZY1(NXI),ZYN(NXI)
      DIMENSION ZP(NXI,NYI,3),TEMP(IDTEMP)
      DIMENSION XO(NXO),YO(NYO),ZO(NXO,NYO)
C
C Declare a function ZF(U,V) that defines a surface.
C
      ZF(U,V)=.5+.25*SIN(-7.*U)+.25*COS(5.*V)
C
C Define the surface to be drawn.
C
      DO 104 I=1,NXI
         X(I) = REAL(I-1)/REAL(NXI-1)
         DO 103 J=1,NYI
            Y(J) = REAL(J-1)/REAL(NYI-1)
            Z(I,J)=ZF(X(I),Y(J))
 103     CONTINUE
 104  CONTINUE
C
C  Do SURF1 set up.
C
      SIGMA = 1.
      ISF   = 255
      CALL SURF1(NXI,NYI,X,Y,Z,NXI,ZX1,ZXM,ZY1,ZYN,
     +           ZXY11,ZXYM1,ZXY1N,ZXYMN,ISF,ZP,TEMP,SIGMA,IERR)
      IF (IERR .NE. 0) THEN
        PRINT *, 'Error return from SURF =',IERR
        STOP
      ENDIF
C
C  Get interpolated points using SURF2.
C
      TINCX = 1.0/(NXO-1)
      TINCY = 1.0/(NYO-1)
      DO 20 I=1,NXO
        XO(I) = (I-1)*TINCX
        DO 10 J=1,NYO
          YO(J) = (J-1)*TINCY
          ZO(I,J) = SURF2(XO(I),YO(J),NXI,NYI,X,Y,Z,NXI,ZP,SIGMA)
   10   CONTINUE
   20 CONTINUE
C
C  Plot a surface.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL TDEZ2D(NXO, NYO, XO, YO, ZO, 3., 36., 67., -6)
      CALL FRAME()
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
