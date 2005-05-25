
      PROGRAM FTEX02D
C
C  Example of CURVP1DP, CURVPIDP.
C
C  Define dimensions, declare arrays.
C
      PARAMETER (IDIM=10,IOUT=201)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION YP
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION XO
      DOUBLE PRECISION YO
      DOUBLE PRECISION YI
      DOUBLE PRECISION PERIOD
      DOUBLE PRECISION SIGMA
      DOUBLE PRECISION XR
      DOUBLE PRECISION XL
      DOUBLE PRECISION XINC
      DOUBLE PRECISION CURVP2DP
      DOUBLE PRECISION CURVPIDP
      DIMENSION X(IDIM),Y(IDIM),YP(IDIM),TEMP(IDIM,2)
      DIMENSION XO(IOUT),YO(IOUT),YI(IOUT)
C
C Specify the input data.
C
      DATA X/0.000D0,0.210D0,0.360D0,0.540D0,1.000D0,1.500D0,1.970D0,
     +     2.300D0,2.500D0,2.700D0/
      DATA Y/0.000D0,2.600D0,3.000D0,2.500D0,0.000D0,-1.000D0,0.000D0,
     +     0.800D0,0.920D0,0.700D0/
C
C  Call CURVP1DP setup.
C
      PERIOD = 3.D0
      SIGMA = 1.D0
      CALL CURVP1DP(IDIM,X,Y,PERIOD,YP,TEMP,SIGMA,IERR)
C
C  Call CURVP2DP and calculate the interpolated values and the integrals.
C
      XR = 5.D0
      XL = -1.D0
      XINC = (XR-XL)/ (IOUT-1)
      DO 10 I = 1,IOUT
          XO(I) = XL + (I-1)*XINC
          YO(I) = CURVP2DP(XO(I),IDIM,X,Y,PERIOD,YP,SIGMA)
          YI(I) = CURVPIDP(0.D0,XO(I),IDIM,X,Y,PERIOD,YP,SIGMA)
   10 CONTINUE
C
C This next section plots the data. You must have a double precision
C version of NCAR Graphics in order for this section to compile and
C run correctly.  For now, it is commented out.
C
C  Draw a plot of the interpolated functions and mark the original
C  points.
C
C      CALL DRWFT2(XL,XR,IDIM,X,Y,IOUT,XO,YO,YI)
C
      STOP
      END
C      SUBROUTINE DRWFT2(XL,XR,II,X,Y,IO,XO,YO,YI)
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
C      DOUBLE PRECISION XL
C      DOUBLE PRECISION XR
C      DOUBLE PRECISION X
C      DOUBLE PRECISION Y
C      DOUBLE PRECISION XO
C      DOUBLE PRECISION YO
C      DOUBLE PRECISION YI
C      DOUBLE PRECISION YPOS_TOP
C      DOUBLE PRECISION YB
C      DOUBLE PRECISION YT
C
C      DATA YPOS_TOP/0.85D0/
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
C      CALL PLCHHQ(0.50D0,0.95D0,':F25:Demo for CURVP1DP, CURVPIDP',
C     +            0.032D0,0.D0,0.D0)
C
C  Graph the interpolated function values and mark the original
C  input data points.
C
C      YB = -2.0D0
C      YT = 3.0D0
C      CALL BKGFT2(XL,XR,YPOS_TOP,'Function',0.42D0,YB,YT)
C      CALL GRIDAL(6,5,5,1,1,1,10,XL,YB)
C
C  Mark the original data points.
C
C      CALL GSMKSC(2.D0)
C      CALL GSPMCI(4)
C      CALL GPM(II,X,Y)
C
C  Graph the interpolated function values.
C
C      CALL GPL(IO,XO,YO)
C
C  Graph the integral.
C
C      YB = -1.0D0
C      YT = 4.0D0
C      CALL BKGFT2(XL,XR,YPOS_TOP-0.47D0,'Integral (from X = 0.)',0.2D0,
C     +            YB,YT)
C      CALL GRIDAL(6,5,5,1,1,1,10,XL,YB)
C      CALL GPL(IO,XO,YI)
C      CALL GSPLCI(1)
C
C  Indicate the period.
C
C      CALL DRWPRD(0.D0,3.D0,6.5D0)
C
C      CALL FRAME
C
C      CALL GDAWK(IWKID)
C      CALL GCLWK(IWKID)
C      CALL GCLKS
C
C      RETURN
C      END
C      SUBROUTINE BKGFT2(XL,XR,YPOS,LABEL,XLP,YB,YT)
C      DOUBLE PRECISION XL
C      DOUBLE PRECISION XR
C      DOUBLE PRECISION YPOS
C      DOUBLE PRECISION XLP
C      DOUBLE PRECISION YB
C      DOUBLE PRECISION YT
C      DOUBLE PRECISION XX
C      DOUBLE PRECISION YY
C      DIMENSION XX(2),YY(2)
C      CHARACTER*(*) LABEL
C
C      CALL SET(0.D0,1.D0,0.D0,1.D0,0.D0,1.D0,0.D0,1.D0,1)
C      CALL PCSETI('FN',21)
C      CALL PLCHHQ(XLP,YPOS-0.03D0,LABEL,0.025D0,0.D0,-1.D0)
C      CALL SET(0.13D0,0.93D0,YPOS-0.25D0,YPOS,XL,XR,YB,YT,1)
C      XX(1) = XL
C      XX(2) = XR
C      YY(1) = 0.D0
C      YY(2) = 0.D0
C      CALL GSPLCI(2)
C      CALL GPL(2,XX,YY)
C      CALL GSPLCI(1)
C
C      CALL GASETI('LTY',1)
C      CALL PCSETI('FN',21)
C      CALL GASETR('XLS',0.02D0)
C      CALL GASETC('XLF','(F4.1)')
C      CALL GASETR('YLS',0.02D0)
C      CALL GASETC('YLF','(F5.2)')
C      CALL GASETR('XMJ',0.02D0)
C      CALL GASETR('YMJ',0.02D0)
C
C      RETURN
C      END
C      SUBROUTINE DRWPRD(XL,XR,Y)
C      DOUBLE PRECISION XL
C      DOUBLE PRECISION XR
C      DOUBLE PRECISION Y
C      DOUBLE PRECISION XX
C      DOUBLE PRECISION YY
C      DOUBLE PRECISION YOFF
C      DOUBLE PRECISION XMID
C      DOUBLE PRECISION XB
C      DOUBLE PRECISION XE
C      DOUBLE PRECISION CFUX
C      DOUBLE PRECISION YI
C
C  Draws a bounding indicator for the period of the function.
C
C      DIMENSION XX(2),YY(2)
C
C      YOFF = 0.4D0
C      CALL GSPLCI(2)
C      XMID = 0.5D0* (XR-XL)
C      CALL PLCHHQ(XMID,Y,':F25:Period',0.02D0,0.D0,0.D0)
C
C  Vertical lines at the period limits.
C
C      XX(1) = XL
C      XX(2) = XL
C      YY(1) = Y + YOFF
C      YY(2) = Y - YOFF
C      CALL GPL(2,XX,YY)
C      XX(1) = XR
C      XX(2) = XR
C      CALL GPL(2,XX,YY)
C
C  Horizontal lines between label and vertical lines.
C
C      CALL PCSETI('TE',1)
C      CALL PCGETR('XB',XB)
C      CALL PCGETR('XE',XE)
C      XX(1) = XL
C      XX(2) = CFUX(XB) - 0.09D0
C      YY(1) = Y
C      YY(2) = Y
C      CALL GPL(2,XX,YY)
C
C  Left arrow.
C
C      XX(1) = XL
C      YY(1) = Y
C      YI = 0.5D0*YOFF
C      YY(2) = Y + YI
C      XX(2) = XL + YI
C      CALL GPL(2,XX,YY)
C      YY(2) = Y - YI
C      CALL GPL(2,XX,YY)
C
C      XX(1) = XR
C      XX(2) = CFUX(XE) + 0.09D0
C      YY(1) = Y
C      YY(2) = Y
C      CALL GPL(2,XX,YY)
C
C  Right arrow.
C
C      XX(1) = XR
C      YY(1) = Y
C      YY(2) = Y + YI
C      XX(2) = XR - YI
C      CALL GPL(2,XX,YY)
C      YY(2) = Y - YI
C      CALL GPL(2,XX,YY)
C
C      RETURN
C      END
