
      PROGRAM THSTGR
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
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL HSTGR(IERR,IWKID)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE HSTGR (IERROR,IWKID)
C
C PURPOSE                To provide a demonstration of the HISTGR
C                        utility and to test each of the four IFLAG
C                        options.
C
C USAGE                  CALL HSTGR (IERROR,IWKID)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An error flag
C                          = 0, If the test is successful,
C                          = 1, otherwise.
C
C I/O                    If the test is successful, the message
C
C                          HISTGR TEST SUCCESSFUL  . . .  SEE PLOT
C                          TO VERIFY PERFORMANCE
C
C                        is written on unit 6.
C
C                        In addition, three (3) labeled frames are
C                        produced.  To determine if the test was
C                        successful examine these plots.
C
C PRECISION              Single
C
C REQUIRED LIBRARY       HISTGR
C FILES
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN 77
C
C HISTORY                Originally written May, 1985; revised
C                        August, 1987
C
C ALGORITHM              THSTGR computes data and calls HISTGR
C                        5 times, exercising different options.
C
C
C  Array DAT1 is filled with values to be used as input for HISTGR
C
      PARAMETER ( NDIM=320, NCLS=17, NWRK=374 )
C
C  NWRK = NDIM + 3*(NCLS+1)
C
      CHARACTER*1 IFC
      CHARACTER*55 MON
      REAL  DAT1(NDIM,2), X, ARR7(7), WORK(NWRK)
      REAL  RGB(3,15), CLASS(NCLS+1)
      REAL  SPAC(2)
      INTEGER  COLORS(15)
C
C  Define the RGB triples needed below.
C
      DATA RGB / 0.70, 0.70, 0.70,
     -           0.75, 0.00, 1.00,
     -           0.30, 0.10, 1.00,
     -           0.10, 0.50, 1.00,
     -           0.00, 0.70, 1.00,
     -           0.00, 1.00, 1.00,
     -           0.00, 1.00, 0.70,
     -           0.00, 1.00, 0.00,
     -           0.70, 0.00, 0.70,
     -           1.00, 1.00, 0.00,
     -           1.00, 0.75, 0.00,
     -           1.00, 0.48, 0.00,
     -           1.00, 0.00, 0.48,
     -           1.00, 0.00, 0.00,
     -           1.00, 1.00, 1.00 /
C
      DATA TX /.5/, TY/.9765/
C
C  Define 15 color indices, 14 spaced throughout the color
C  spectrum, and the last one being white.
C
      CALL GSCR(IWKID,0,0.,0.,0.)
      DO 100 I = 1,15
      CALL GSCR(IWKID,I,RGB(1,I),RGB(2,I),RGB(3,I))
  100 CONTINUE
C
C  Call ENTSR (from PORT library) to recover from warnings
C
      CALL ENTSR(IDUM, 1)
C
C Change the Plotchar special character code from a : to a @
C
      IFC(1:1) = '@'
      CALL PCSETC('FC',IFC)
C
C
C  Frame 1:  Demonstrate the default version of HISTGR, IFLAG = 0.
C
      IFLAG = 0
      NCLASS = 11
      NPTS = 320
C
      DO 105 I=1,NPTS
        DO 105 J=1,2
          DAT1(I,J)=0.
  105 CONTINUE
      DO  110 I=1,NPTS
        X = REAL(I)
        DAT1(I,1) = 10. * ALOG10(0.1*X+1.)
  110 CONTINUE
C
C  (First call HSTOPL('DEF=ON') to activate all default options.)
C
      CALL HSTOPL('DE=ON')
C
C  Flush PLOTIT's buffers.
C
      CALL PLOTIT(0,0,0)
C
C  Set text and polyline color indices to 15 (white).
C
      CALL GSTXCI(15)
      CALL GSPLCI(15)
C
      CALL PLCHLQ (TX,TY,
     +'DEMONSTRATION PLOT FOR DEFAULT VERSION OF HISTGR',16.,0.,0.)
      CALL HISTGR(DAT1, NDIM, NPTS, IFLAG, CLASS, NCLASS, WORK, NWRK)
C
C  Frame 2:  Demonstrate the comparison of two previously determined
C            histograms, IFLAG = 3.
C
      IFLAG = 3
      NCLASS = 6
      NPTS2 = 6
       DO 200 I=1,NPTS2
        DAT1(I,1)=2*SIN(REAL(I))
        DAT1(I,2)=2.5*COS(REAL(I)/.5)
        CLASS(I)=REAL(I)
  200  CONTINUE
C
C  (First call HSTOPL('DEF=ON') to activate all default options.)
C
      CALL HSTOPL('DE=ON')
C
C  Turn on color, title, perimeter, frequency, format, character,
C  label, and spacing options.
C
         COLORS(1) = 8
         COLORS(2) = 3
         COLORS(3) = 14
         COLORS(4) = 11
         COLORS(5) = 6
         COLORS(6) = 13
         COLORS(7) = 14
         COLORS(8) = 5
      CALL HSTOPI('COL=ON',3,0,COLORS,8)
C
C  Choose large, horizontal alphanumeric labels for class labels.
C
      CALL HSTOPC('TI=ON',
     +'OPTIONS CHANGED: PRM,TIT,CHA,LAB,FOR,COL,FQN and SPA',7,3)
      CALL HSTOPL('PR=ON')
      CALL HSTOPC('FQ=ON','MONTHLY PRECIPITATION',7,3)
      CALL HSTOPC('FO=ON','(F3.0)',9,3)
      MON='JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'
      CALL HSTOPC('CH=ON',MON,12,3)
      CALL HSTOPC('LA=ON','COMPARE TWO DATASETS',7,3)
      SPAC(1) = 2.0
      SPAC(2) = -1.5
      CALL HSTOPR('SP=ON',SPAC,2)
C
C  The second argument must be the actual dimension size of DAT1.
C
      CALL HISTGR(DAT1, NDIM, NPTS2, IFLAG, CLASS, NCLASS, WORK, NWRK)
C
C  Frame 3:  Put four plots on 1 frame by setting FRAME = OFF for the
C            first 3 plots and FRAME = ON for the last plot.
C
C     Plot 1:   IFLAG = 0, automatic sorting of the input data into
C               NCLASS = 17 bins.
C
      IFLAG = 0
      NCLASS = 17
      NPTS = 320
      DO  210 I=1,NPTS
        X = REAL(I)
        DAT1(I,1) = 10. * ALOG10(0.1*X+1.)
  210 CONTINUE
C
C  (First call HSTOPL('DEF=ON') to activate all default options.)
C
      CALL HSTOPL('DE=ON')
C
C  Turn on horizontal bars, title, median, window, color,
C  and spacing options.
C
C  Choose large horizontal class labels.
C
      CALL HSTOPC('TI=ON',
     +'OPTIONS: HOR, WIN, FRA, MED, COL, SPA, & TIT',9,3)
      CALL HSTOPL('HO=ON')
      CALL HSTOPL('ME=ON')
C
C  Plot 1 goes into the top left quadrant of the frame.
C
C       ARR7 coordinates are XMIN, XMAX, YMIN, YMAX.
C
      ARR7(1) = 0.
      ARR7(2) = .5
      ARR7(3) = .5
      ARR7(4) = 1.
      CALL HSTOPR('WI=ON',ARR7,4)
C
         COLORS(1) = 8
         COLORS(2) = 2
         COLORS(3) = 10
         COLORS(4) = 4
         COLORS(5) = 5
         COLORS(6) = 6
         COLORS(7) = 7
         COLORS(8) = 8
      CALL HSTOPI('COL=ON',2,0,COLORS,8)
      SPAC(1) = 3.0
      SPAC(2) = 0.0
      CALL HSTOPR('SP=ON',SPAC,2)
C
C  Turn off the frame advance.
C
      CALL HSTOPL('FR=OF')
      CALL HISTGR(DAT1, NDIM, NPTS, IFLAG, CLASS, NCLASS, WORK, NWRK)
C
C     Plot 2:   IFLAG = 2, one set of 11 histogram classes and their
C               associated values are plotted.
C
      IFLAG = 2
      NCLASS = 11
      NPTS2 = 11
      DO 650 I = 1, NPTS2
        CLASS(I)=REAL(2*I)
        DAT1(I,1)=(REAL(2*I))**0.5
  650 CONTINUE
C
C  (First call HSTOPL('DEF=ON') to activate all default options.)
C
      CALL HSTOPL('DE=ON')
C
C  Turn on title, label, frequency, format, and window options.
C
C  Choose medium sized, horizontal labels for class labels
C
      CALL HSTOPC('TI=ON',
     +'OPTIONS CHANGED: LAB,FQN,TIT,FOR,FRA AND WIN',11,3)
      CALL HSTOPC('LA=ON','Y VALUES ASSIGNED TO X VALUES',11,3)
      CALL HSTOPC('FQ=ON','SQUARE ROOT OF CLASS MID-VALUES',
     &12,3)
      CALL HSTOPC('FO=ON','(I3)',11,3)
C
C  Plot 2 goes into the top right quadrant of the frame.
C
      ARR7(1) = .5
      ARR7(2) = 1.
      ARR7(3) = .5
      ARR7(4) = 1.
      CALL HSTOPR('WIN=ON',ARR7,4)
C
C  Turn off color and frame advance options.
C
      CALL HSTOPI('COL=OF',2,0,COLORS,8)
      CALL HSTOPL('FR=OFF')
      CALL HISTGR(DAT1, NDIM, NPTS2, IFLAG, CLASS, NCLASS, WORK, NWRK)
C
C     Plot 3:   IFLAG = 1, input values are sorted into a defined set of
C               8 classes.
C
      IFLAG = 1
      NCLASS = 8
      NPTS = 320
      X = 0.
      DO 500 I = 1,NPTS
        DAT1(I,1) = SIN(X)
        X = X + .02
  500 CONTINUE
C
      CLASS(1) = -0.6
      DO 550 I = 1,NCLASS
        CLASS(I+1) = CLASS(I) + 0.20
550   CONTINUE
C
C  (First call HSTOPL('DEF=ON') to activate all default options.)
C
      CALL HSTOPL('DE=ON')
C
C  Turn on class, draw line, format, title, and label options.
C
      CALL HSTOPI('CL=ON',2,30,COLORS,8)
      CALL HSTOPL('DR=ON')
      CALL HSTOPC('FOR=ON','(F6.2)',9,3)
      CALL HSTOPC('TI=ON',
     +'OPTIONS CHANGED: CLA,DRL,FOR,TIT,LAB,MID,WIN,SHA and FRA',9,3)
      CALL HSTOPC('LA=ON','CLASS VALUES CHOSEN FROM -0.6 to 1.0',
     &9,3)
C
C  Plot 3 goes into the lower left quadrant of the frame.
C
      ARR7(1) = 0.
      ARR7(2) = .5
      ARR7(3) = 0.
      ARR7(4) = .5
      CALL HSTOPR('WI=ON',ARR7,4)
C
C  Turn off color, midvalues, frame advance and shading options.
C
C  Write the class labels at a 30 deg angle.
C
      CALL HSTOPI('CO=OF',2,30,COLORS,8)
      CALL HSTOPL('MI=OFF')
      CALL HSTOPL('FR=OFF')
      CALL HSTOPL('SH=OFF')
      CALL HISTGR(DAT1, NDIM, NPTS, IFLAG, CLASS, NCLASS, WORK, NWRK)
C
C     Plot 4:   IFLAG = 0, input values are sorted into 11 equally sized
C               bins over the range of the input values.
C
      IFLAG = 0
      NCLASS = 11
      NPTS = 320
      X = 0.
      DO 310 I = 1,NPTS
        DAT1(I,1) = SIN(X)
        X = X + .02
  310 CONTINUE
C
C  (First call HSTOPL('DEF=ON') to activate all default options.)
C
      CALL HSTOPL('DE=ON')
C
C  Turn on class, frequency, format, title, window, and label options.
C
C  Choose medium sized vertical class value labels.
C
      CALL HSTOPI('CL=ON',2,90,COLORS,8)
      CALL HSTOPC('FQN=ON','NUMBER OF OCCURENCES IN EACH CLASS',9,3)
      CALL HSTOPC('FOR=ON','(F6.2)',9,3)
      CALL HSTOPC('TI=ON',
     +'OPTIONS CHANGED: CLA,LAB,FQN,TIT,SPA,PER,FOR AND WIN',9,3)
C
C  Plot 4 goes into the lower right quadrant of the frame.
C
      ARR7(1) = .5
      ARR7(2) = 1.
      ARR7(3) = 0.
      ARR7(4) = .5
      CALL HSTOPR('WIN=ON',ARR7,4)
      CALL HSTOPC('LAB=ON','CLASS VALUES COMPUTED WITHIN HISTGR',
     &9,3)
C
C  Turn off color, spacing and percent axis options.
C
      CALL HSTOPI('CO=OF',2,90,COLORS,8)
      CALL HSTOPR('SP=OF',SPAC,2)
      CALL HSTOPL('PER=OFF')
C
      CALL HISTGR(DAT1, NDIM, NPTS, IFLAG, CLASS, NCLASS, WORK, NWRK)
C
C
      IERROR = 0
      WRITE (6,1001)
C
      RETURN
C
 1001 FORMAT ('  HISTGR TEST SUCCESSFUL',24X,
     1        'SEE PLOT TO VERIFY PERFORMANCE')
      END
