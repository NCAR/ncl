
      PROGRAM DSEX05
C
      PARAMETER (NUM=5000, NX=21, NY=21)
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
      REAL XI(NUM), YI(NUM), ZI(NUM)
      REAL XO(NX),  YO(NY),  OUTPUT(NX,NY)
      DATA XMININ,YMININ,XMAXIN,YMAXIN/  0.00,  0.00, 1.00, 1.00/
      DATA XMINOT,YMINOT,XMAXOT,YMAXOT/ -0.21, -0.21, 1.21, 1.21/
      DATA RHO, THETA, PHI/3., -70., 40./
C
C  Create random data in two-space and define a function.
C  To get this to work on your system, you may have to insert
C  the correct random number generator for your compiler.
C
      DO 10 I=1,NUM
        XI(I) = XMININ+(XMAXIN-XMININ)*DSRND5()
        YI(I) = YMININ+(YMAXIN-YMININ)*DSRND5()
        ZI(I) = (XI(I)-0.25)**2 + (YI(I)-0.50)**2
   10 CONTINUE
C
C  Create the output grid.
C
      DO 102 I=1,NX
        XO(I) = XMINOT+(REAL(I-1)/REAL(NX-1))*(XMAXOT-XMINOT)
  102 CONTINUE
      DO 103 J =1,NY
        YO(J)= YMINOT+(REAL(J-1)/REAL(NY-1))*(YMAXOT-YMINOT)
  103 CONTINUE
C
C  Interpolate.
C
      CALL DSGRID2S(NUM, XI, YI, ZI, NX, NY, XO, YO, OUTPUT, IER)
      IF (IER .NE. 0) THEN
        WRITE(6,520) IER
  520   FORMAT(' Error ',I3,' returned from DSGRID2S') 
        STOP
      ENDIF
C
C  Plot a surface.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL TDEZ2D(NX, NY, XO, YO, OUTPUT, RHO, THETA, PHI, 6)
      CALL FRAME()
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      REAL FUNCTION DSRND5()
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
      DSRND5 = REAL(NEXTN)/REAL(MODLUS)
C
      RETURN
      END
