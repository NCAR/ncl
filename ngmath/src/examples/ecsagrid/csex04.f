
      PROGRAM CSEX04
C
C  Do a 2D approximation and find mixed second order partial derivatives.
C
C
C  The dimensionality of the problem.
C
      PARAMETER (NDIM=2)
C
C  The number of input data points.
C
      PARAMETER (NDATA=1000)
C
C  The number of output data points in the X coordinate direction.
C
      PARAMETER (NX=29)
C
C  The number of output data points in the Y coordinate direction.
C
      PARAMETER (NY=25)
C
C  Specifty the number of knots in the X direction.
C
      PARAMETER (N1=4)
C
C  Specifty the number of knots in the Y direction.
C
      PARAMETER (N2=4)
C
C  The size of the workspace.
C
      PARAMETER (NCF=N1*N2, NWRK=NCF*(NCF+3))
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
C  Dimension the arrays.
C
      DIMENSION XDATA(NDIM,NDATA),YDATA(NDATA),KNOTS(NDIM),WORK(NWRK),
     +          NDERIV(2)
      DATA XMIN,YMIN,XMAX,YMAX / -1.0, -1.0, 1.0, 1.0/
      DATA NDERIV(1),NDERIV(2) / 0, 0/
      REAL XO(NX), YO(NY), FUNC(NX,NY),FUNCD(NX,NY)
C
C  Generate input data using the functiuon f(x,y) = y**2 - 0.5*y*x**2
C
      INDX = NDATA
      DO 10 I=1,INDX
        XDATA(1,I) = XMIN+(XMAX-XMIN)*DSRND1()
        XDATA(2,I) = YMIN+(YMAX-YMIN)*DSRND1()
        XX = XDATA(1,I)
        YY = XDATA(2,I)
        YDATA(I) = YY*YY - 0.5*YY*XX*XX
   10 CONTINUE
C
C  Create the output grid.
C
      DO 102 I=1,NX
        XO(I) = XMIN+(REAL(I-1)/REAL(NX-1))*(XMAX-XMIN)
  102 CONTINUE
      DO 103 J =1,NY
        YO(J)= YMIN+(REAL(J-1)/REAL(NY-1))*(YMAX-YMIN)
  103 CONTINUE
C
C  Specify the numbers of knots in each coordinate direction.
C
      KNOTS(1) = N1
      KNOTS(2) = N2
C
C  Calculate the approximated functuion values.
C
      CALL CSA2S (INDX,XDATA,YDATA,KNOTS,NX,NY,XO,YO,FUNC,NWRK,WORK,IER)
C
C  Calculate the second order mixed partial derivative.
C
      NDERIV(1) = 1
      NDERIV(2) = 1
      CALL CSA2XS (INDX,XDATA,YDATA,-1.,KNOTS,0.,NDERIV,NX,NY,
     +                  XO,YO,FUNCD,NWRK,WORK,IER)
C
C  Plot a surface.
C
C  Open GKS and define the foreground and background color.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Approximated function.
C
      CALL TDEZ2D(NX, NY, XO, YO, FUNC, 2.7, 45., 78., 6)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ(0.5,0.85,
     +   ':F25:z = f(x,y) = y:S:2:E:  - -:H-10::S:1:E::B::V-6:2:E:  y:V-
     +6:*:V+6:x:S:2:E:',0.04,0.,0.)
      CALL FRAME()
C
C  Mixed partial.
C
      CALL TDEZ2D(NX, NY, XO, YO, FUNCD, 2.7, 45., 78., 6)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ(0.5,0.88,
     + ':F25:z =  :F34::S::H8:6:F25::S:2:E::E::F34:>:B::F34::H-35::V-6:6
     +:F25:x:F34:6:F25:y:E:  f(x,y) = - x',0.04,0.,0.)
      CALL FRAME()
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      REAL FUNCTION DSRND1()
C
C  Portable random number generator.
C
      PARAMETER (MPLIER=16807,MODLUS=2147483647,MOBYMP=127773,
     +           MOMDMP=2836)
C
      INTEGER HVLUE, LVLUE, TESTV, NEXTN
      SAVE    NEXTN
      DATA JSEED,IFRST/123456789,0/
C
      IF (IFRST .EQ. 0) THEN
        NEXTN = JSEED
        IFRST = 1
      ENDIF
C
      HVLUE = NEXTN / MOBYMP
      LVLUE = MOD(NEXTN, MOBYMP)
      TESTV = MPLIER*LVLUE - MOMDMP*HVLUE
      IF (TESTV .GT. 0) THEN
        NEXTN = TESTV
      ELSE
        NEXTN = TESTV + MODLUS
      ENDIF
      DSRND1 = REAL(NEXTN)/REAL(MODLUS)
C
      RETURN
      END
