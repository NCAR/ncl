C
C       $Id: csex05.f,v 1.1 1998-12-10 00:09:10 fred Exp $
C
      PROGRAM CSEX05
C
C  Do a 3D approximation of a sphere.
C
      PARAMETER (NDIM=3, NDATA=1000, NX=21, NY=21, NZ=21, 
     +           N1=4, N2=4, N3=4, NCF=N1*N2*N3, 
     +           NWRK=NCF*(NCF+3))
      DIMENSION XDATA(NDIM,NDATA),YDATA(NDATA),KNOTS(NDIM),WORK(NWRK)
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID (these values are used by GKS).
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
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
