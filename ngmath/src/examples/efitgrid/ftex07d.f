
      PROGRAM FTEX07D
C
C  This program illustrates the use of CURVS1DP and CURVS2DP to
C  interpolate a smoothing tension spline for data in the plane.
C
C  Numeber of input data points and interpolation points.
C
      PARAMETER (NUM_IN=4,NUM_OUT=101)
C
C  GKS parameters.
C
      PARAMETER (IERRF=6,LUNIT=2,IWTYPE=1,IWKID=1)
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION XS
      DOUBLE PRECISION YS
      DOUBLE PRECISION XSP
      DOUBLE PRECISION YSP
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION PARAM
      DOUBLE PRECISION XL
      DOUBLE PRECISION YL
      DOUBLE PRECISION XO
      DOUBLE PRECISION YO
      DOUBLE PRECISION XOO
      DOUBLE PRECISION YOO
      DOUBLE PRECISION SIGMA
      DOUBLE PRECISION D
      DOUBLE PRECISION S
      DOUBLE PRECISION EPS
      DOUBLE PRECISION XINC
      DOUBLE PRECISION T
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
      DATA X/0.5D0,-1.5D0,0.5D0,1.5D0/
      DATA Y/1.5D0,0.0D0,-2.5D0,-1.0D0/
C
C  Tension factor.
C
      SIGMA = 1.0D0
C
C  Specify a uniform observational weight (the interpolated
C  curve can be this far away from the original data points)..
C
      ISW = 1
      D = 0.2D0
C
C  Smoothing factor (larger values result in smoother curves).
C
      S = DBLE(NUM_IN)
C
C  Computational tolerance value.
C
      EPS = SQRT(2.D0/S)
C
C  Call CURVS1DP and CURVS2DP to calculate the smoothing spline.
C
      CALL CURVS1DP(NUM_IN,X,Y,D,ISW,S,EPS,PARAM,XS,YS,XSP,YSP,SIGMA,
     +     TEMP,IERR)
C
      XINC = 1.D0/DBLE(NUM_OUT-1)
      DO 10 I = 1,NUM_OUT
          T = DBLE(I-1)*XINC
          CALL CURVS2DP(T,NUM_IN,PARAM,XS,YS,XSP,YSP,SIGMA,XO(I),YO(I))
   10 CONTINUE
C
C  Not use CURVS1DP and CURVS2DP to compute an interpolating tension
C  spline by setting the smoothing parameter to zero.
C
      S = 0.D0
      EPS = 0.D0
      CALL CURVS1DP(NUM_IN,X,Y,D,ISW,S,EPS,PARAM,XS,YS,XSP,YSP,SIGMA,
     +     TEMP,IERR)
C
      DO 20 I = 1,NUM_OUT
          T = DBLE(I-1)*XINC
          CALL CURVS2DP(T,NUM_IN,PARAM,XS,YS,XSP,YSP,SIGMA,XOO(I),
     +         YOO(I))
   20 CONTINUE
C
C This next section plots the data. You must have a double precision
C version of NCAR Graphics in order for this section to compile and
C run correctly.  For now, it is commented out.
C
C  Plot the two curves.
C
C      CALL GOPKS(6,IBUF)
C      CALL GOPWK(IWKID,LUNIT,IWTYPE)
C      CALL GACWK(IWKID)
C
C  Define colors.
C
C      CALL GSCR(IWKID,0,1.D0,1.D0,1.D0)
C      CALL GSCR(IWKID,1,0.D0,0.D0,0.D0)
C      CALL GSCR(IWKID,2,1.D0,0.D0,0.D0)
C      CALL GSCR(IWKID,3,0.D0,0.D0,1.D0)
C
C  Main title.
C
C      CALL PCSETI('FN',25)
C      CALL PLCHHQ(0.5D0,0.95D0,'Demo for CURVS1DP/CURVS2DP',25.D0,0.D0,
C     +     0.D0)
C      CALL SET(0.1D0,0.95D0,0.2D0,0.90D0,-2.D0,3.D0,-3.D0,2.D0,1)
C
C  Background grid.
C
C      CALL GASETI('LTY',1)
C      CALL PCSETI('FN',21)
C      CALL GSPLCI(1)
C      CALL LABMOD('(F4.1)','(F4.1)',4,4,20,20,0,0,0)
C      CALL HALFAX(5,4,5,4,-2.D0,-3.D0,1,1)
C
C  Plot the smoothing spline in red, and its informational label.
C
C      CALL GSLWSC(3.D0)
C      CALL GSPLCI(2)
C      CALL GPL(NUM_OUT,XO,YO)
C      XL(1) = 0.250D0
C      XL(2) = 0.750D0
C      YL(1) = 0.625D0
C      YL(2) = 0.625D0
C      CALL GPL(2,XL,YL)
C      CALL PLCHHQ(0.875D0,0.625D0,'Smoothing spline',24.D0,0.D0,-1.D0)
C
C  Plot the tension spline in blue, and its informational label.
C
C      CALL GSPLCI(3)
C      CALL GPL(NUM_OUT,XOO,YOO)
C      XL(1) = 0.250D0
C      XL(2) = 0.750D0
C      YL(1) = 0.125D0
C      YL(2) = 0.125D0
C      CALL GPL(2,XL,YL)
C      CALL PLCHHQ(0.875D0,0.125D0,'Tension spline',24.D0,0.D0,-1.D0)
C
C  Mark the original data with black dots, and label.
C
C      CALL NGDOTS(X,Y,NUM_IN,0.13D0,1)
C      CALL NGDOTS(0.500D0,-0.375D0,1,0.13D0,1)
C      CALL PLCHHQ(0.875D0,-0.375D0,'Original data points',24.D0,0.D0,
C     +            -1.D0)
C
C      CALL FRAME
C
C      CALL GDAWK(IWKID)
C      CALL GCLWK(IWKID)
C      CALL GCLKS
C
      STOP
      END
