
      PROGRAM FTEX02
C
C  Example of CURVP1, CURVPI.
C
C  Define dimensions, declare arrays.
C
      PARAMETER (IDIM=10,IOUT=201)
      DIMENSION X(IDIM),Y(IDIM),YP(IDIM),TEMP(IDIM,2)
      DIMENSION XO(IOUT),YO(IOUT),YI(IOUT)
C
C Specify the input data.
C
      DATA X/ 0.000, 0.210, 0.360, 0.540, 1.000, 
     +        1.500, 1.970, 2.300, 2.500, 2.700/
      DATA Y/ 0.000, 2.600, 3.000, 2.500, 0.000,
     +       -1.000, 0.000, 0.800, 0.920, 0.700/
C
C  Call CURVP1 setup.
C
      PERIOD = 3.
      SIGMA  = 1.
      CALL CURVP1(IDIM, X, Y, PERIOD, YP, TEMP, SIGMA, IERR)
C
C  Call CURVP2 and calculate the interpolated values and the integrals.
C
      XR =  5.
      XL = -1.
      XINC = (XR-XL)/(IOUT-1)
      DO 10 I=1,IOUT
        XO(I) = XL+(I-1)*XINC
        YO(I) = CURVP2(XO(I),IDIM,X,Y,PERIOD,YP,SIGMA)
        YI(I) = CURVPI(0.,XO(I),IDIM,X,Y,PERIOD,YP,SIGMA)
   10 CONTINUE
C
C  Draw a plot of the interpolated functions and mark the original points.
C
      CALL DRWFT2(XL,XR,IDIM,X,Y,IOUT,XO,YO,YI)
C
      STOP 
      END
      SUBROUTINE DRWFT2(XL,XR,II,X,Y,IO,XO,YO,YI)
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
      DATA YPOS_TOP/0.85/
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a color table.
C
      CALL GSCR(IWKID, 0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 1, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID, 2, 1.0, 0.0, 0.0)
      CALL GSCR(IWKID, 3, 0.0, 1.0, 0.0)
      CALL GSCR(IWKID, 4, 0.0, 0.0, 1.0)
      CALL GSCLIP(0)
C
C  Plot main title.
C
      CALL PLCHHQ(0.50,0.95,':F25:Demo for curvp1, curvpi',
     +            0.032,0.,0.)
C
C  Graph the interpolated function values and mark the original
C  input data points.
C
      YB = -2.0
      YT =  3.0
      CALL BKGFT2(XL,XR,YPOS_TOP,'Function',0.42,YB,YT)
      CALL GRIDAL(6,5,5,1,1,1,10,XL,YB)
C
C  Mark the original data points.
C
      CALL GSMKSC(2.)
      CALL GSPMCI(4)
      CALL GPM(II,X,Y)
C
C  Graph the interpolated function values.
C
      CALL GPL(IO,XO,YO)
C
C  Graph the integral.
C
      YB = -1.0
      YT =  4.0
      CALL BKGFT2(XL,XR,YPOS_TOP-0.47,'Integral (from X = 0.)',
     +            0.2,YB,YT)
      CALL GRIDAL(6,5,5,1,1,1,10,XL,YB)
      CALL GPL(IO,XO,YI)
      CALL GSPLCI(1)
C
C  Indicate the period.
C
      CALL DRWPRD(0.,3.,6.5)
C
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
C
      RETURN
      END
      SUBROUTINE BKGFT2(XL,XR,YPOS,LABEL,XLP,YB,YT)
      DIMENSION XX(2),YY(2)
      CHARACTER*(*) LABEL
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PCSETI('FN',21)
      CALL PLCHHQ(XLP,YPOS - 0.03,LABEL,0.025,0.,-1.)
      CALL SET(0.13,0.93,YPOS-0.25,YPOS,XL,XR,YB,YT,1)
      XX(1) = XL
      XX(2) = XR
      YY(1) = 0.
      YY(2) = 0.
      CALL GSPLCI(2)
      CALL GPL(2,XX,YY)
      CALL GSPLCI(1)

      CALL GASETI('LTY',1)
      CALL PCSETI('FN',21)
      CALL GASETR('XLS',0.02)
      CALL GASETC('XLF','(F4.1)')
      CALL GASETR('YLS',0.02)
      CALL GASETC('YLF','(F5.2)')
      CALL GASETR('XMJ',0.02)
      CALL GASETR('YMJ',0.02)
C
      RETURN
      END
      SUBROUTINE DRWPRD(XL,XR,Y)
C
C  Draws a bounding indicator for the period of the function.
C
      DIMENSION XX(2),YY(2)
C
      YOFF = 0.4
      CALL GSPLCI(2)
      XMID = 0.5*(XR-XL)
      CALL PLCHHQ(XMID,Y,':F25:Period',0.02,0.,0.)
C
C  Vertical lines at the period limits.
C
      XX(1) = XL
      XX(2) = XL
      YY(1) = Y+YOFF
      YY(2) = Y-YOFF
      CALL GPL(2,XX,YY)
      XX(1) = XR
      XX(2) = XR
      CALL GPL(2,XX,YY)
C
C  Horizontal lines between label and vertical lines.
C
      CALL PCSETI('TE',1)
      CALL PCGETR('XB',XB)
      CALL PCGETR('XE',XE)
      XX(1) = XL
      XX(2) = CFUX(XB)-0.09
      YY(1) = Y
      YY(2) = Y
      CALL GPL(2,XX,YY)
C
C  Left arrow.
C
      XX(1) = XL
      YY(1) = Y
      YI = 0.5*YOFF
      YY(2) = Y+YI
      XX(2) = XL+YI
      CALL GPL(2,XX,YY)
      YY(2) = Y-YI
      CALL GPL(2,XX,YY)
C
      XX(1) = XR
      XX(2) = CFUX(XE)+0.09
      YY(1) = Y
      YY(2) = Y
      CALL GPL(2,XX,YY)
C
C  Right arrow.
C
      XX(1) = XR
      YY(1) = Y
      YY(2) = Y+YI
      XX(2) = XR-YI
      CALL GPL(2,XX,YY)
      YY(2) = Y-YI
      CALL GPL(2,XX,YY)
C
      RETURN
      END
