
      PROGRAM SPLOGY
C
C  This test program is an example of logarithmic axis scaling
C  in the NCAR Graphics User Coordinate System.
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
C       Dimension a line of 100 points
C
      DIMENSION X(100),Y(100)

C
C       Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C       Turn clipping off
C
      CALL GSCLIP(0)
C
C       Generate a straight line of 100 points.
C
      DO 1 I=1,100
      X(I) = I
      Y(I) = 10.*I
  1   CONTINUE
C
C       Select axes of linear in X, logarithmic in Y,
C        in NCAR Graphics user coordinates.
C
      CALL SET(.05,.95,.20,.95,1.,100.,10.,1000.,2)
C
C       Set attributes for output
C        Assign yellow to color index 2
C
      CALL GSCR(IWKID,0,0.,0.,0.)
      CALL GSCR(IWKID,1,1.,1.,1.)
      CALL GSCR(IWKID,2,1.,1.,0.)
C
C       Generate output (GKS, SPPS, or NCAR utilities)
C
C         Set polyline color index to yellow
C
      CALL GSPLCI(2)
C
C         Initialize the AUTOGRAPH entry EZXY so that
C         the frame is not advanced and the Y axis is logarithmic.
C
      CALL DISPLA(2,0,2)
C
C         Output the polyline (X,Y) using EZXY.
C
      CALL EZXY(X,Y,100,' ')
C
C       Add a yellow title.
C
C        PLOTCHAR uses stroked characters; thus, the yellow polyline
C        color index previously set will still apply.
C
C       Return the window to fractional coordinates for the title.
C       Also, return scaling to linear, linear.
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ(0.5,.05,'Example 5.1.  Log Scaling with SPPS',
     1 .019,0.,0.)
C
C       Advance the frame to ensure all output is plotted
C
      CALL FRAME
C
C       Close GKS
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
