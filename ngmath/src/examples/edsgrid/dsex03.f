      PROGRAM DSEX03
C
      PARAMETER (NUM=171, NX=21, NY=21)
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
C
      REAL XI(NUM), YI(NUM), ZI(NUM)
      REAL XO(NX),  YO(NY),  OUTPUT(NX,NY)
      DATA XMININ,YMININ,XMAXIN,YMAXIN/ -0.2, -0.2, 1.2, 1.2/
      DATA XMINOT,YMINOT,XMAXOT,YMAXOT/  0.0,  0.0, 1.0, 1.0/
      DATA XEYE, YEYE, ZEYE/1.3, -1.8, 3.6/
C
C  Create random data in two-space and define a function.
C  To get this to work on your system, you may have to insert
C  the correct random number generator for your compiler.
C
      DO 10 I=1,NUM
        XI(I) = XMININ+(XMAXIN-XMININ)*DSRAND()
        YI(I) = YMININ+(YMAXIN-YMININ)*DSRAND()
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
      CALL DRWTD2(NX, NY, XO, YO, OUTPUT, XEYE, YEYE, ZEYE, -6)
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
