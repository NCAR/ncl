
      PROGRAM FTEX01
C
C  Example of CURV1, CURV2, CURVD, CURVI.
C
C  Define dimensions, declare arrays.
C
      PARAMETER (IDIM=11,IOUT=201)
      DIMENSION X(IDIM),Y(IDIM),YP(IDIM),TEMP(IDIM,2)
      DIMENSION XO(IOUT),YO(IOUT),YD(IOUT),YI(IOUT)
C
C Specify the input data.
C
C
      DATA X/  0.00,   2.00,   5.00,   8.00,  10.00,  13.00,
     +        15.00,  18.00,  21.00,  23.00,  30.00         /
      DATA Y/  1.00,   0.81,   0.00,  -0.81,  -1.00,  -0.84,
     +        -0.56,   0.04,   0.73,   1.18,   2.0          /
C
C  Call CURV1 setup, specifying that the derivatives should be
C  zero at the end points.
C
      SLP1   = 0.
      SLPN   = 0.
      ISLPSW = 0
      SIGMA  = 1.
      CALL CURV1(IDIM, X, Y, SLP1, SLPN, ISLPSW, YP, TEMP, SIGMA, IERR)
C
C  Call CURV2 and calculate the interpolated values, the derivatives,
C  and the integrals.
C
      XINC = 30./(IOUT-1)
      DO 10 I=1,IOUT
        XO(I) = (I-1)*XINC
        YO(I) = CURV2(XO(I),IDIM,X,Y,YP,SIGMA)
        YD(I) = CURVD(XO(I),IDIM,X,Y,YP,SIGMA)
        YI(I) = CURVI(XO(1),XO(I),IDIM,X,Y,YP,SIGMA)
   10 CONTINUE
C
C  Draw a plot of the interpolated functions and mark the original points.
C
      CALL DRWFT1(IDIM,X,Y,IOUT,XO,YO,YD,YI)
C
      STOP
      END
      SUBROUTINE DRWFT1(II,X,Y,IO,XO,YO,YD,YI)
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
      DATA YPOS_TOP/0.88/
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
      CALL PLCHHQ(0.50,0.95,':F25:Demo for CURV, CURVD, CURVI',
     +            0.030,0.,0.)
C
C  Graph the interpolated function values and mark the original
C  input data points.
C
      YB = -1.0
      YT =  2.0
      CALL BKGFT1(YPOS_TOP,'Function',YB,YT)
      CALL GRIDAL(6,5,3,1,1,1,10,0.0,YB)
C
C  Mark the original data points.
C
      CALL GSMKSC(2.)
      CALL GSPMCI(4)
      CALL GSLWSC(1.)
      CALL GPM(II,X,Y)
C
C  Graph the interpolated function values.
C
      CALL GPL(IO,XO,YO)
C
C  Graph the derivative.
C
      YB = -0.3
      YT =  0.3
      CALL BKGFT1(YPOS_TOP-0.3,'Derivative',YB,YT)
      CALL GRIDAL(6,5,3,1,1,1,10,0.0,YB)
      CALL GPL(IO,XO,YD)
      CALL GSPLCI(1)
C
C  Graph the integral.
C
      YB = -6.0
      YT = 10.0
      CALL BKGFT1(YPOS_TOP-0.6,'Integral',YB,YT)
      CALL GRIDAL(6,5,4,1,1,1,10,0.0,YB)
      CALL GPL(IO,XO,YI)
      CALL GSPLCI(1)
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
C
      RETURN
      END
      SUBROUTINE BKGFT1(YPOS,LABEL,YB,YT)
      DIMENSION XX(2),YY(2)
      CHARACTER*(*) LABEL
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PCSETI('FN',21)
      CALL PLCHHQ(0.20,YPOS - 0.03,LABEL,0.025,0.,-1.)
      CALL SET(0.13,0.93,YPOS-0.2,YPOS,0.0,30.0, YB, YT, 1)
      XX(1) = 0.
      XX(2) = 30.
      YY(1) = 0.
      YY(2) = 0.
      CALL GSPLCI(2)
      CALL GPL(2,XX,YY)
      CALL GSPLCI(1)

      CALL GASETI('LTY',1)
      CALL PCSETI('FN',21)
      CALL GASETR('XLS',0.02)
      CALL GASETC('XLF','(I3)')
      CALL GASETR('YLS',0.02)
      CALL GASETC('YLF','(F5.1)')
      CALL GASETR('XMJ',0.02)
      CALL GASETR('YMJ',0.02)
C
      RETURN
      END
