      PROGRAM DSEX04
C
      PARAMETER (NUM=16, NX=21, NY=21)       
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
C
      REAL XI(NUM), YI(NUM), ZI(NUM)
      REAL XO(NX), YO(NY), OUTPUT(NX,NY)
      REAL XINC, YINC
C
C  Input data points and values.
C
      DATA XI/0.00, 1.00, 0.00, 1.00, 0.30, 0.30, 0.30, 0.69, 
     +        0.71, 0.71, 0.69, 0.70, 0.70, 0.70, 0.69, 0.71/
      DATA YI/0.00, 0.00, 1.00, 1.00, 0.70, 0.30, 0.70, 0.69, 
     +        0.69, 0.71, 0.71, 0.70, 0.69, 0.71, 0.70, 0.70/
      DATA ZI/0.00, 0.00, 0.00, 0.50, 0.50, 0.50, 0.50, 1.00, 
     +        1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00/
      DATA XEYE, YEYE, ZEYE/3.3, -3.3, 3.3/
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
C  Set shadowing flag.
C
C     CALL DSSETI('SHD',1)
      CALL DSGRID2S(NUM, XI, YI, ZI, NX, NY, XO, YO, OUTPUT, IER)
      IF (IER .NE. 0) THEN
        WRITE(6,520) IER
  520   FORMAT(' Error ',I3,' returned from DSGRID2S')
        STOP
      ENDIF
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL DRWTD2(NX, NY, XO, YO, OUTPUT, XEYE, YEYE, ZEYE, -6)
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
