      PROGRAM DSEX01
C
      PARAMETER (NUM=1000, NX=21, NY=21, NZ=21)
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
C
      DOUBLE PRECISION XI(NUM), YI(NUM), ZI(NUM), U(NUM)
      DOUBLE PRECISION XO(NX),  YO(NY),  ZO(NZ),  OUTPUT(NX,NY,NZ)
      DOUBLE PRECISION XMIN,    YMIN,    ZMIN    
      DOUBLE PRECISION XMAX,    YMAX,    ZMAX
      REAL             XP(NX),  YP(NY),  ZP(NZ),  OUTP(NX,NY,NZ)
      INTEGER I, J, K, IER
C
      DATA XMIN,YMIN,ZMIN/ -2.D0, -2.D0, -2.D0/
      DATA XMAX,YMAX,ZMAX/  2.D0,  2.D0,  2.D0/
C
C  Create random data in three space and define a function.
C  To get this to work on your system, you may have to insert
C  the correct random number generator for your compiler.
C
      DO 10 I=1,NUM
        XI(I) = XMIN+(XMAX-XMIN)*DBLE(DSRND1D())
        YI(I) = YMIN+(YMAX-YMIN)*DBLE(DSRND1D())
        ZI(I) = ZMIN+(ZMAX-ZMIN)*DBLE(DSRND1D())
         U(I) = XI(I)**2 + YI(I)**2 + ZI(I)**2
   10 CONTINUE
C
C  Create the output grid.
C
      DO 102 I=1,NX
        XO(I) = XMIN+(DBLE(I-1)/DBLE(NX-1))*(XMAX-XMIN)
  102 CONTINUE
C
      DO 103 J =1,NY
        YO(J)= YMIN+(DBLE(J-1)/DBLE(NY-1))*(YMAX-YMIN)
  103 CONTINUE
C
      DO 104 K=1,NZ
        ZO(K) = ZMIN+(DBLE(K-1)/DBLE(NZ-1))*(ZMAX-ZMIN)
  104 CONTINUE
C
C  Interpolate.
C
      CALL DSGRID3D(NUM,XI,YI,ZI,U,NX,NY,NZ,XO,YO,ZO,OUTPUT,IER)
      IF (IER .NE. 0) THEN
        WRITE(6,520) IER
  520   FORMAT(' Error ',I3,' returned from DSGRID3D') 
        STOP
      ENDIF
C
C  Pass single precision arrays for plotting an isosurface.
C
      DO 105 I=1,NX
        XP(I) = XO(I)
        DO 106 J=1,NY
          YP(J) = YO(J)
          DO 107 K=1,NZ
            ZP(K) = ZO(K)
            OUTP(I,J,K) = OUTPUT(I,J,K)
  107     CONTINUE
  106   CONTINUE
  105 CONTINUE
C
C  Draw plot.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL TDEZ3D(NX, NY, NZ, XP, YP, ZP, OUTP, 3.0, 0., 0., 0., 6)
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      REAL FUNCTION DSRND1D()
C
      DATA ISEED/1/
      SAVE ISEED
C
      ISEED = ISEED*1103515245 + 12345
      IT = IAND(ISHIFT(ISEED,-16),32767)
C
      DSRND1D = REAL(IT)/32767.
C
      RETURN
      END
