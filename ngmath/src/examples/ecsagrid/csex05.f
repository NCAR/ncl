
      PROGRAM CSEX05
C
C  Do a 3D approximation of a sphere.
C
C
C  The dimensionality of the problem.
C
      PARAMETER (NDIM=3)
C
C  The number of input data points.
C
      PARAMETER (NDATA=1000)
C
C  The number of output data points in the X coordinate direction.
C
      PARAMETER (NX=21)
C
C  The number of output data points in the Y coordinate direction.
C
      PARAMETER (NY=21)
C
C  The number of output data points in the Z coordinate direction.
C
      PARAMETER (NZ=21)
C
C  Specifty the number of knots in the X direction.
C
      PARAMETER (N1=4)
C
C  Specifty the number of knots in the Y direction.
C
      PARAMETER (N2=4)
C
C  Specifty the number of knots in the Z direction.
C
      PARAMETER (N3=4)
C
C  The size of the workspace.
C
      PARAMETER (NCF=N1*N2*N3, NWRK=NCF*(NCF+3))
C
C  Dimension the arrays.
C
      DIMENSION XDATA(NDIM,NDATA),YDATA(NDATA),KNOTS(NDIM),WORK(NWRK)
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
C  Define arrays for use in plotting the isosurface.
C
      REAL XO(NX), YO(NY), ZO(NZ), OUTPUT(NX,NY,NZ)
      INTEGER I, J, K, IER
      DATA XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX / -2., -2., -2., 2.,  2., 2./
C
C  Define the input data on random coordinates bounded by the
C  above values.
C
      DO 10 I=1,NDATA
        XDATA(1,I) = XMIN+(XMAX-XMIN)*DSRND1()
        XDATA(2,I) = YMIN+(YMAX-YMIN)*DSRND1()
        XDATA(3,I) = ZMIN+(ZMAX-ZMIN)*DSRND1()
        YDATA(I)  = XDATA(1,I)**2 + XDATA(2,I)**2 + XDATA(3,I)**2      
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
      DO 104 K=1,NZ
        ZO(K) = ZMIN+(REAL(K-1)/REAL(NZ-1))*(ZMAX-ZMIN)
  104 CONTINUE
C
C  Specify the numbers of knots in each coordinate.
C
      KNOTS(1) = N1
      KNOTS(2) = N2
      KNOTS(3) = N3
C
C  Find the approximating function values on the output grid.
C
      CALL CSA3S (NDATA,XDATA,YDATA,KNOTS,NX,NY,NZ,
     +            XO,YO,ZO,OUTPUT,NWRK,WORK,IER)
C
C  Plot an isosurface.
C
C  Open GKS and define the foreground and background color.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL TDEZ3D(NX, NY, NZ, XO, YO, ZO, OUTPUT, 3., 2., -35., 65., 6)       
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
