
      PROGRAM SPREVX
C
C  This test program is an example of X axis scale reversal
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
C       Select the normalization transformation
C        window and viewport.  Reverse X in the window.
C
      CALL SET(.10,.95,.20,.95,100.,1.,10.,1000.,1)
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
C         the frame is not advanced.
C
      CALL DISPLA(2,0,1)
C
C         Tell EZXY that the SET ordering of the window
C         is to be used (LSET 3 or 4).
C
      CALL ANOTAT(' ',' ',1,4,0,' ')
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
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ(0.5,.05,'Example 5.2.  X Axis Reversal with SPPS',
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
