
      PROGRAM FTEX06D
C
C  Example of SURF1DP/SURF2DP.
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
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION Z
      DOUBLE PRECISION ZX1
      DOUBLE PRECISION ZXM
      DOUBLE PRECISION ZY1
      DOUBLE PRECISION ZYN
      DOUBLE PRECISION ZP
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION XO
      DOUBLE PRECISION YO
      DOUBLE PRECISION ZO
      DOUBLE PRECISION ZF
      DOUBLE PRECISION U
      DOUBLE PRECISION V
      DOUBLE PRECISION SIGMA
      DOUBLE PRECISION ZXY11
      DOUBLE PRECISION ZXYM1
      DOUBLE PRECISION ZXY1N
      DOUBLE PRECISION ZXYMN
      DOUBLE PRECISION TINCX
      DOUBLE PRECISION TINCY
      DOUBLE PRECISION SURF2DP
C
      DIMENSION X(NXI),Y(NYI),Z(NXI,NYI)
      DIMENSION ZX1(NYI),ZXM(NYI),ZY1(NXI),ZYN(NXI)
      DIMENSION ZP(NXI,NYI,3),TEMP(IDTEMP)
      DIMENSION XO(NXO),YO(NYO),ZO(NXO,NYO)
C
C Declare a function ZF(U,V) that defines a surface.
C
      ZF(U,V) = .5D0 + .25D0*SIN(-7.D0*U) + .25D0*COS(5.D0*V)
C
C Define the surface to be drawn.
C
      DO 104 I = 1,NXI
          X(I) = DBLE(I-1)/DBLE(NXI-1)
          DO 103 J = 1,NYI
              Y(J) = DBLE(J-1)/DBLE(NYI-1)
              Z(I,J) = ZF(X(I),Y(J))
  103     CONTINUE
  104 CONTINUE
C
C  Do SURF1DP set up.
C
      SIGMA = 1.D0
      ISF = 255
      CALL SURF1DP(NXI,NYI,X,Y,Z,NXI,ZX1,ZXM,ZY1,ZYN,ZXY11,ZXYM1,ZXY1N,
     +     ZXYMN,ISF,ZP,TEMP,SIGMA,IERR)
      IF (IERR.NE.0) THEN
          PRINT *,'Error return from SURFDP =',IERR
          STOP
      END IF
C
C  Get interpolated points using SURF2DP.
C
      TINCX = 1.0D0/ (NXO-1)
      TINCY = 1.0D0/ (NYO-1)
      DO 20 I = 1,NXO
          XO(I) = (I-1)*TINCX
          DO 10 J = 1,NYO
              YO(J) = (J-1)*TINCY
              ZO(I,J) = SURF2DP(XO(I),YO(J),NXI,NYI,X,Y,Z,NXI,ZP,SIGMA)
   10     CONTINUE
   20 CONTINUE
C
C This next section plots the data. You must have a double precision
C version of NCAR Graphics in order for this section to compile and
C run correctly.  For now, it is commented out.
C
C
C  Plot a surface.
C
C      CALL GOPKS(IERRF,ISZDM)
C      CALL GOPWK(IWKID,LUNIT,IWTYPE)
C      CALL GACWK(IWKID)
C      CALL TDEZ2D(NXO,NYO,XO,YO,ZO,3.D0,36.D0,67.D0,-6)
C      CALL FRAME
C      CALL GDAWK(IWKID)
C      CALL GCLWK(IWKID)
C      CALL GCLKS
C
      STOP
      END
