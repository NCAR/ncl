
      PROGRAM DSEX02
C
      PARAMETER (NUM=6, NX=61, NY=61, IWDIM=2*NX*NY)       
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
      REAL XO(NX), YO(NY), OUTPUT(NX,NY)
      REAL XINC, YINC
C
C  Input data points and values.
C
      DATA XI/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/
      DATA YI/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/
      DATA ZI/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/
      DATA RHO, THETA, PHI/3.0, -45., 55./
C
C  Specify the output grid.
C
      XINC = 1./ REAL(NX-1)
      YINC = 1./ REAL(NY-1)
      DO 30 I=1,NX
        XO(I) = REAL((I-1)*XINC)
        DO 40 J=1,NY
          YO(J) = REAL((J-1)*YINC)
   40   CONTINUE
   30 CONTINUE
C
C  Exponent equals 0.5
C
      CALL DSSETR('EXP',0.5)
      CALL DSGRID2S(NUM, XI, YI, ZI, NX, NY, XO, YO, OUTPUT, IER)
      IF (IER .NE. 0) THEN
        WRITE(6,520) IER
  520   FORMAT(' Error ',I3,' returned from DSGRID2S')
        STOP
      ENDIF
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL TDEZ2D(NX, NY, XO, YO, OUTPUT, RHO, THETA, PHI, 6)
      CALL FRAME()
C
C  Exponent equals 1.0
C
      CALL DSSETR('EXP',1.0)
      CALL DSGRID2S(NUM, XI, YI, ZI, NX, NY, XO, YO, OUTPUT, IER)
      IF (IER .NE. 0) THEN
        WRITE(6,520) IER
        STOP
      ENDIF
      CALL TDEZ2D(NX, NY, XO, YO, OUTPUT, RHO, THETA, PHI, 6)
      CALL FRAME()
C
C  Exponent equals 10.0
C
      CALL DSSETR('EXP',5.0)
      CALL DSGRID2S(NUM, XI, YI, ZI, NX, NY, XO, YO, OUTPUT, IER)
      IF (IER .NE. 0) THEN
        WRITE(6,520) IER
        STOP
      ENDIF
C
      CALL TDEZ2D(NX, NY, XO, YO, OUTPUT, RHO, THETA, PHI, 6)
      CALL FRAME()
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
