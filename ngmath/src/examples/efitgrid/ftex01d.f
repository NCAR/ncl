
      PROGRAM FTEX01D
C
C  Example of CURV1DP, CURV2DP, CURVDDP, CURVIDP.
C
C  Define dimensions, declare arrays.
C
      PARAMETER (IDIM=11,IOUT=201)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION YP
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION XO
      DOUBLE PRECISION YO
      DOUBLE PRECISION YD
      DOUBLE PRECISION YI
      DOUBLE PRECISION SLP1
      DOUBLE PRECISION SLPN
      DOUBLE PRECISION SIGMA
      DOUBLE PRECISION XINC
      DOUBLE PRECISION CURV2DP
      DOUBLE PRECISION CURVDDP
      DOUBLE PRECISION CURVIDP
      DIMENSION X(IDIM),Y(IDIM),YP(IDIM),TEMP(IDIM,2)
      DIMENSION XO(IOUT),YO(IOUT),YD(IOUT),YI(IOUT)
C
C Specify the input data.
C
C
      DATA X/0.00D0,2.00D0,5.00D0,8.00D0,10.00D0,13.00D0,15.00D0,
     +     18.00D0,21.00D0,23.00D0,30.00D0/
      DATA Y/1.00D0,0.81D0,0.00D0,-0.81D0,-1.00D0,-0.84D0,-0.56D0,
     +     0.04D0,0.73D0,1.18D0,2.0D0/
C
C  Call CURV1DP setup, specifying that the derivatives should be
C  zero at the end points.
C
      SLP1 = 0.D0
      SLPN = 0.D0
      ISLPSW = 0
      SIGMA = 1.D0
      CALL CURV1DP(IDIM,X,Y,SLP1,SLPN,ISLPSW,YP,TEMP,SIGMA,IERR)
C
C  Call CURV2DP and calculate the interpolated values, the derivatives,
C  and the integrals.
C
      XINC = 30.D0/ (IOUT-1)
      DO 10 I = 1,IOUT
          XO(I) = (I-1)*XINC
          YO(I) = CURV2DP(XO(I),IDIM,X,Y,YP,SIGMA)
          YD(I) = CURVDDP(XO(I),IDIM,X,Y,YP,SIGMA)
          YI(I) = CURVIDP(XO(1),XO(I),IDIM,X,Y,YP,SIGMA)
   10 CONTINUE
C
C This next section plots the data. You must have a double precision
C version of NCAR Graphics in order for this section to compile and
C run correctly.  For now, it is commented out.
C
C  Draw a plot of the interpolated functions and mark the original points.
C
C      CALL DRWFT1(IDIM,X,Y,IOUT,XO,YO,YD,YI)
C
      STOP
      END
C      SUBROUTINE DRWFT1(II,X,Y,IO,XO,YO,YD,YI)
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C      DOUBLE PRECISION X
C      DOUBLE PRECISION Y
C      DOUBLE PRECISION XO
C      DOUBLE PRECISION YO
C      DOUBLE PRECISION YD
C      DOUBLE PRECISION YI
C      DOUBLE PRECISION YPOS_TOP
C      DOUBLE PRECISION YB
C      DOUBLE PRECISION YT
C
C      DATA YPOS_TOP/0.88D0/
C
C  Open GKS, open and activate a workstation.
C
C      CALL GOPKS(IERRF,ISZDM)
C      CALL GOPWK(IWKID,LUNIT,IWTYPE)
C      CALL GACWK(IWKID)
C
C  Define a color table.
C
C      CALL GSCR(IWKID,0,1.0D0,1.0D0,1.0D0)
C      CALL GSCR(IWKID,1,0.0D0,0.0D0,0.0D0)
C      CALL GSCR(IWKID,2,1.0D0,0.0D0,0.0D0)
C      CALL GSCR(IWKID,3,0.0D0,1.0D0,0.0D0)
C      CALL GSCR(IWKID,4,0.0D0,0.0D0,1.0D0)
C      CALL GSCLIP(0)
C
C  Plot main title.
C
C      CALL PLCHHQ(0.50D0,0.95D0,
C     +     ':F25:Demo for CURVDP, CURVDDP, CURVIDP',
C     +     0.030D0,0.D0,0.D0)
C
C  Graph the interpolated function values and mark the original
C  input data points.
C
C      YB = -1.0D0
C      YT = 2.0D0
C      CALL BKGFT1(YPOS_TOP,'Function',YB,YT)
C      CALL GRIDAL(6,5,3,1,1,1,10,0.0D0,YB)
C
C  Mark the original data points.
C
C      CALL GSMKSC(2.D0)
C      CALL GSPMCI(4)
C      CALL GSLWSC(1.D0)
C      CALL GPM(II,X,Y)
C
C  Graph the interpolated function values.
C
C      CALL GPL(IO,XO,YO)
C
C  Graph the derivative.
C
C      YB = -0.3D0
C      YT = 0.3D0
C      CALL BKGFT1(YPOS_TOP-0.3D0,'Derivative',YB,YT)
C      CALL GRIDAL(6,5,3,1,1,1,10,0.0D0,YB)
C      CALL GPL(IO,XO,YD)
C      CALL GSPLCI(1)
C
C  Graph the integral.
C
C      YB = -6.0D0
C      YT = 10.0D0
C      CALL BKGFT1(YPOS_TOP-0.6D0,'Integral',YB,YT)
C      CALL GRIDAL(6,5,4,1,1,1,10,0.0D0,YB)
C      CALL GPL(IO,XO,YI)
C      CALL GSPLCI(1)
C      CALL FRAME
C
C      CALL GDAWK(IWKID)
C      CALL GCLWK(IWKID)
C      CALL GCLKS
C
C      RETURN
C      END
C      SUBROUTINE BKGFT1(YPOS,LABEL,YB,YT)
C      DOUBLE PRECISION YPOS
C      DOUBLE PRECISION YB
C      DOUBLE PRECISION YT
C      DOUBLE PRECISION XX
C      DOUBLE PRECISION YY
C      DIMENSION XX(2),YY(2)
C      CHARACTER*(*) LABEL
C
C      CALL SET(0.D0,1.D0,0.D0,1.D0,0.D0,1.D0,0.D0,1.D0,1)
C      CALL PCSETI('FN',21)
C      CALL PLCHHQ(0.20D0,YPOS-0.03D0,LABEL,0.025D0,0.D0,-1.D0)
C      CALL SET(0.13D0,0.93D0,YPOS-0.2D0,YPOS,0.0D0,30.0D0,YB,YT,1)
C      XX(1) = 0.D0
C      XX(2) = 30.D0
C      YY(1) = 0.D0
C      YY(2) = 0.D0
C      CALL GSPLCI(2)
C      CALL GPL(2,XX,YY)
C      CALL GSPLCI(1)
C
C      CALL GASETI('LTY',1)
C      CALL PCSETI('FN',21)
C      CALL GASETR('XLS',0.02D0)
C      CALL GASETC('XLF','(I3)')
C      CALL GASETR('YLS',0.02D0)
C      CALL GASETC('YLF','(F5.1)')
C      CALL GASETR('XMJ',0.02D0)
C      CALL GASETR('YMJ',0.02D0)
C
C      RETURN
C      END
