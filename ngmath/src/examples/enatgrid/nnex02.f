
      PROGRAM NNEX02
C
C  Simple example of natural neighbor linear regridding.
C
      PARAMETER(ISLIM = 6, NUMXOUT = 21, NUMYOUT = 21)
C
C  Dimension for the work space for the NCAR Graphics call to
C  SRFACE to plot the interpolated grid.
C
      PARAMETER(IDIM=2*NUMXOUT*NUMYOUT)
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
C  Input data arrays.
C
      REAL X(ISLIM), Y(ISLIM), Z(ISLIM)
C
C  Output grid arrays.
C
      REAL XI(NUMXOUT), YI(NUMYOUT), ZI(NUMXOUT,NUMYOUT)
C
C  Define the input data arrays.
C
      DATA X/0.00, 1.00, 0.00, 1.00, 0.40, 0.75 /
      DATA Y/0.00, 0.00, 1.00, 1.00, 0.20, 0.65 /
      DATA Z/0.00, 0.00, 0.00, 0.00, 1.25, 0.80 /
C
      DIMENSION IWORK(IDIM)
C
C  Define the output grid.
C
      XMIN = 0.
      XMAX = 1.
      XINC = (XMAX-XMIN)/(NUMXOUT-1.) 
      DO 20 I=1,NUMXOUT
        XI(I) = XMIN+REAL(I-1) * XINC
   20 CONTINUE
C
      YMAX =  1.
      YMIN =  0.
      YINC = (YMAX-YMIN)/(NUMYOUT-1.)
      DO 30 J=1,NUMYOUT
        YI(J) = YMIN+REAL(J-1) * YINC
   30 CONTINUE 
C
C  Set the flag for using estimated gradients.
C
      CALL NNSETI('IGR',1)
C
C  Do the regridding.
C
      CALL NATGRIDS(ISLIM,X,Y,Z,NUMXOUT,NUMYOUT,XI,YI,ZI,IER)
      IF (IER .NE. 0) THEN
        WRITE (6,510) IER
  510   FORMAT('Error return from NATGRIDS = ',I3)
      ENDIF
C
C  Draw a plot of the interpolated surface.
C
C
C Open GKS and define the foreground and background color.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL GSCR(IWKID, 0, 1.00, 1.00, 1.00)
      CALL GSCR(IWKID, 1, 0.00, 0.00, 0.00)
C
      CALL DRWSRF(NUMXOUT,NUMYOUT,XI,YI,ZI,15.,-25.,90.,IWORK)
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C 
      STOP
      END
