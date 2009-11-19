
      PROGRAM FTEX03
C
C  Example of CURVS, CURVPS.
C
C  Define dimensions, declare arrays.
C
      PARAMETER (IDIM=10,IOUT=201)
      DIMENSION X(IDIM),Y(IDIM),YS(IDIM),YSP(IDIM),TEMP(IDIM,11)
      DIMENSION XO(IOUT),YOS(IOUT),YOSP(IOUT)
C
C Specify the input data.
C
C
      DATA X/ 0.000, 0.210, 0.360, 0.540, 1.000, 
     +        1.500, 1.970, 2.300, 2.500, 2.700/
      DATA Y/ 0.000, 2.600, 3.000, 2.500, 0.000,
     +       -1.000, 0.000, 0.800, 0.920, 0.700/
C
C  Call CURVS setup.
C
      SIGMA = 1.0
      D     = 0.3
      ISW   = 1
      S     = REAL(IDIM)
      EPS   = SQRT(2./S)
      CALL CURVS(IDIM,X,Y,D,ISW,S,EPS,YS,YSP,SIGMA,TEMP,IERR)       
C
C  Call CURV2 and calculate the interpolated values and the integrals.
C
      XR =  5.
      XL = -1.
      XINC = (XR-XL)/(IOUT-1)
      DO 10 I=1,IOUT
        XO(I) = XL+(I-1)*XINC
        YOS(I) = CURV2(XO(I),IDIM,X,YS,YSP,SIGMA)
   10 CONTINUE
C
C  Call CURVPS setup.
C
      P = 3.
      CALL CURVPS(IDIM,X,Y,P,D,ISW,S,EPS,YS,YSP,SIGMA,TEMP,IERR)       
C
C  Call CURVP2 and calculate the interpolated values.
C
      DO 15 I=1,IOUT
        YOSP(I) = CURVP2(XO(I),IDIM,X,YS,P,YSP,SIGMA)
   15 CONTINUE
C
C  Plot the results.
C
      CALL DRWFT3(XL,XR,IDIM,X,Y,IOUT,XO,YO,YOS,YOSP)
C
      STOP
      END
      SUBROUTINE DRWFT3(XL,XR,II,X,Y,IO,XO,YO,YOS,YOSP)
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
      DATA YPOS_TOP/0.95/
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
      CALL GSCLIP(1)
C
C  Graph the interpolated function values and mark the original
C  input data points.
C
      YB = -2.0
      YT =  4.0
      CALL BKGFT3(XL,XR,YPOS_TOP,'CURVS',0.42,YB,YT)
      CALL GRIDAL(6,5,3,1,1,1,10,XL,YB)
C
C  Mark the original data points.
C
      CALL GSMKSC(2.)
      CALL GSPMCI(4)
      CALL GPM(II,X,Y)
C
C  Graph the interpolated function values.
C
      CALL GPL(IO,XO,YOS)
C
C  Graph the periodic function.
C
      CALL BKGFT3(XL,XR,YPOS_TOP-0.5,'CURVPS',0.42,YB,YT)
      CALL GRIDAL(6,5,3,1,1,1,10,XL,YB)
      CALL GPL(IO,XO,YOSP)
      CALL GSPLCI(1)
C
C  Mark the original data points.
C
      CALL GSMKSC(2.)
      CALL GSPMCI(4)
      CALL GPM(II,X,Y)
C
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
C
      RETURN
      END
      SUBROUTINE BKGFT3(XL,XR,YPOS,LABEL,XLP,YB,YT)
      DIMENSION XX(2),YY(2)
      CHARACTER*(*) LABEL
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PCSETI('FN',21)
      CALL PLCHHQ(XLP,YPOS - 0.03,LABEL,0.025,0.,-1.)
      CALL SET(0.13,0.93,YPOS-0.35,YPOS,XL,XR,YB,YT,1)
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
