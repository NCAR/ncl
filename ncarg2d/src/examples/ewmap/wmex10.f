
      PROGRAM WMEX10
C
C  Example of wind barbs at various angles.
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

      PARAMETER (X1=0.18, X2=0.46, X3=0.74)       
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,.4,0.,.4)
C
C  Wind barbs at various angles using differing attributes.
C
      CALL PLCHHQ(0.5,0.94,':F26:Wind barb examples',.04,0.,0.)
C
C  Set barb color and size.
C
      CALL WMSETI('COL',2)
      CALL WMSETR('WBS',0.25)
C
C  Draw first barb (all defaults with a wind speed of 71 knots).
C
      CALL WMBARB(X1,.5,26.,71.)
C
C  Second barb - change the angle of the tick marks and the spacing 
C  between ticks and draw another barb.
C
      CALL WMGETR('WBA',WBAO)
      CALL WMGETR('WBD',WBDO)
      CALL WMSETR('WBA',42.)
      CALL WMSETR('WBD',.17)
      CALL WMBARB(X2,.5,0.,75.)
      CALL WMSETR('WBA',WBAO)
      CALL WMSETR('WBD',WBDO)
C
C  Third barb - draw a sky cover symbol at base of the barb (these 
C  are drawn automatically when using WMSTNM to plot station model data).
C
      CALL WMGETR('WBS',WBS)
      CALL WMGETR('WBC',WBC)
      CALL WMSETI('WBF',1)
      CALL WMBARB(X3,.5,-26.,71.)
      CALL NGWSYM('N',0,X3,.5,WBS*WBC,2,0)
C
C  Fourth barb - change the size of the barb and the size of the sky 
C  cover symbol.
C 
      CALL WMSETR('WBS',0.20)
      CALL WMSETR('WBC',0.15)
      CALL WMBARB(X1+0.1,0.1,-26.,71.)
      CALL NGWSYM('N',0,X1+0.1,.1,0.20*0.15,2,0)
C
C  Fifth barb - reset original values for parameters, change wind speed
C               to 45 knots.
C
      CALL WMSETR('WBS',WBS)
      CALL WMSETR('WBC',WBC)
      CALL WMSETI('WBF',0)
      CALL WMBARB(X2,.1,0.,45.)
C
C  Sixth barb - change the tick lengths.
C
      CALL WMSETR('WBT',.6)
      CALL WMBARB(X3-0.1,.1,15.4,42.2)
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
