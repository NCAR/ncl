C
C       $Id: csex06.f,v 1.2 1999-01-28 23:55:29 fred Exp $
C
      PROGRAM CSEX06
C
C  Do a 3D approximation and plot an isosurface.
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
      PARAMETER (NY=31)
C
C  The number of output data points in the Z coordinate direction.
C
      PARAMETER (NZ=41)
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
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID (these values are used by GKS).
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
C
C  Define arrays for use in plotting the isosurface.
C
      REAL XO(NX),  YO(NY),  ZO(NZ),  OUTPUT(NX,NY,NZ)
      DATA XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX / -2., -2., -2., 2.,  2., 2./
C
C  Define the input data on random coordinates bounded by the
C  above values.
C
      DO 10 I=1,NDATA
        XDATA(1,I) = XMIN+(XMAX-XMIN)*DSRND1()
        XDATA(2,I) = YMIN+(YMAX-YMIN)*DSRND1()
        XDATA(3,I) = ZMIN+(ZMAX-ZMIN)*DSRND1()
        YDATA(I)  =  0.75*XDATA(1,I)**2 - 1.6*XDATA(2,I)**2
     +                    + 2.0*XDATA(3,I)**2.      
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
C  Specify the numbers of nodes in each coordinate.
C
      KNOTS(1) = N1
      KNOTS(2) = N2
      KNOTS(3) = N3
C
C  Calculate the approximating function.
C
      CALL CSA3S (NDATA,XDATA,YDATA,KNOTS,NX,NY,NZ,
     +            XO,YO,ZO,OUTPUT,NWRK,WORK,IER)
C
C  Plot an isosurface.
C
C Open GKS and define the foreground and background color.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL TDEZ3D(NX, NY, NZ, XO, YO, ZO, OUTPUT, .7,
     +            2.3, -13., 75., 6)
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
C  Random number generator.
C
      DATA ISEED/1/
      SAVE ISEED
C
      ISEED = ISEED*1103515245 + 12345
      IT = IAND(ISHIFT(ISEED,-16),32767)
C
      DSRND1 = REAL(IT)/32767.
C
      RETURN
      END
