
      PROGRAM DSEX06
C
      PARAMETER (NUM=1000, NX=21, NY=21, NZ=21)
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
      REAL XI(NUM), YI(NUM), ZI(NUM), U(NUM)
      REAL XO(NX),  YO(NY),  ZO(NZ),  OUTPUT(NX,NY,NZ)
      INTEGER I, J, K, IER
      DATA XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX / -2., -2., -2., 2., 2., 2./
C
C  3D example.
C
      DO 10 I=1,NUM
        XI(I) = XMIN+(XMAX-XMIN)*DSRND6()
        YI(I) = YMIN+(YMAX-YMIN)*DSRND6()
        ZI(I) = ZMIN+(ZMAX-ZMIN)*DSRND6()
         U(I) = XI(I)**2 + YI(I)**2 + ZI(I)**2
   10 CONTINUE
C
C  Create the output grid.
C
      DO 102 I=1,NX
        XO(I) = XMIN+(REAL(I-1)/REAL(NX-1))*(XMAX-XMIN)
  102 CONTINUE
C
      DO 103 J =1,NY
        YO(J)= YMIN+(REAL(J-1)/REAL(NY-1))*(YMAX-YMIN)
  103 CONTINUE
C
      DO 104 K=1,NZ
        ZO(K) = ZMIN+(REAL(K-1)/REAL(NZ-1))*(ZMAX-ZMIN)
  104 CONTINUE
C
C  Interpolate.
C
      DO 110 I=1,NX
        DO 111 J=1,NY
          DO 112 K=1,NZ
            CALL DSPNT3S (NUM,XI,YI,ZI,U,1,XO(I),YO(J),ZO(K),
     +                    OUTPUT(I,J,K),IER)
            IF (IER .NE. 0) THEN
              WRITE(6,520) IER
  520         FORMAT(' Error ',I3,' returned from DSPNT3S') 
              STOP
            ENDIF
  112     CONTINUE
  111   CONTINUE
  110 CONTINUE
C
C  Plot an isosurface.
C
C Open GKS and define the foreground and background color.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL TDEZ3D(NX, NY, NZ, XO, YO, ZO, OUTPUT, 3.0, 
     +            2., -35., 65., 6)       
      CALL FRAME()
C
C  2D Example.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      REAL FUNCTION DSRND6()
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
      DSRND6 = REAL(NEXTN)/REAL(MODLUS)
C
      RETURN
      END
