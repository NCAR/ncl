
      PROGRAM FTEX07
C
C  This program illustrates the use of curvs1 and curvs2 to
C  interpolate a smoothing tension spline for data in the plane.
C
C  Numeber of input data points and interpolation points.
C
      PARAMETER(NUM_IN=4,NUM_OUT=101)
C
C  GKS parameters.
C
      PARAMETER(IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
C
C  Declare arrays.
C
      DIMENSION X(NUM_IN),Y(NUM_IN)
      DIMENSION XS(NUM_IN),YS(NUM_IN),XSP(NUM_IN),YSP(NUM_IN)
      DIMENSION TEMP(NUM_IN,19),PARAM(NUM_IN),XL(2),YL(2)
      DIMENSION XO(NUM_OUT),YO(NUM_OUT),XOO(NUM_OUT),YOO(NUM_OUT)
C
C  Specify the original data points in the plane.
C
      DATA X/0.5, -1.5,  0.5,  1.5/
      DATA Y/1.5,  0.0, -2.5, -1.0/
C
C  Tension factor.
C
      SIGMA = 1.0
C
C  Specify a uniform observational weight (the interpolated
C  curve can be this far away from the original data points)..
C
      ISW   = 1
      D     = 0.2
C
C  Smoothing factor (larger values result in smoother curves).
C
      S     = REAL(NUM_IN)
C
C  Computational tolerance value.
C
      EPS   = SQRT(2./S)
C
C  Call CURVS1 and CURVS2 to calculate the smoothing spline.
C
      CALL CURVS1(NUM_IN,X,Y,D,ISW,S,EPS,PARAM,XS,YS,XSP,YSP,
     +            SIGMA,TEMP,IERR)
C
      XINC = 1./REAL(NUM_OUT-1)
      DO 10 I=1,NUM_OUT
        T = REAL(I-1)*XINC
        CALL CURVS2(T,NUM_IN,PARAM,XS,YS,XSP,YSP,SIGMA,XO(I),YO(I))
   10 CONTINUE
C
C  Not use CURVS1 and CURVS2 to compute an interpolating tension 
C  spline by setting the smoothing parameter to zero.
C
      S   = 0.
      EPS = 0.
      CALL CURVS1(NUM_IN,X,Y,D,ISW,S,EPS,PARAM,XS,YS,XSP,YSP,
     +            SIGMA,TEMP,IERR)
C
      DO 20 I=1,NUM_OUT
        T = REAL(I-1)*XINC
        CALL CURVS2(T,NUM_IN,PARAM,XS,YS,XSP,YSP,SIGMA,XOO(I),YOO(I))
   20 CONTINUE
C
C  Plot the two curves.
C
      CALL GOPKS (6,IBUF)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define colors.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,1.,0.,0.)
      CALL GSCR(IWKID,3,0.,0.,1.)
C
C  Main title.
C
      CALL PCSETI('FN',25)
      CALL PLCHHQ(0.5,0.95,'Demo for CURVS1/CURVS2',25.,0.,0.)
      CALL SET(0.1, 0.95, 0.2, 0.90, -2., 3., -3., 2., 1)
C
C  Background grid.
C
      CALL GASETI('LTY',1)
      CALL PCSETI('FN',21)
      CALL GSPLCI(1)
      CALL LABMOD('(F4.1)','(F4.1)',4,4,20,20,0,0,0)
      CALL HALFAX(5, 4, 5, 4, -2., -3., 1,1)
C
C  Plot the smoothing spline in red, and its informational label.
C
      CALL GSLWSC(3.)
      CALL GSPLCI(2)
      CALL GPL(NUM_OUT,XO,YO)
      XL(1) = 0.250
      XL(2) = 0.750
      YL(1) = 0.625
      YL(2) = 0.625
      CALL GPL(2,XL,YL)
      CALL PLCHHQ(0.875,0.625,'Smoothing spline',24.,0.,-1.)
C
C  Plot the tension spline in blue, and its informational label.
C
      CALL GSPLCI(3)
      CALL GPL(NUM_OUT,XOO,YOO)
      XL(1) = 0.250
      XL(2) = 0.750
      YL(1) = 0.125
      YL(2) = 0.125
      CALL GPL(2,XL,YL)
      CALL PLCHHQ(0.875,0.125,'Tension spline',24.,0.,-1.)
C
C  Mark the original data with black dots, and label.
C
      CALL NGDOTS(X,Y,NUM_IN,0.13,1)
      CALL NGDOTS(0.500,-0.375,1,0.13,1)
      CALL PLCHHQ(0.875,-0.375,'Original data points',24.,0.,-1.)
C
      CALL FRAME()
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS ()
C
      STOP
      END
