
      PROGRAM STEX01
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
C  Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Invoke STITLE example.
C
      CALL EXSTL0 (IERROR)
C
C  Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      END
      SUBROUTINE EXSTL0 (IERROR)
C
C  This subroutine provides a simple example of STITLE usage.
C  It is assumed that GKS has been opened prior to calling
C  this suboutine.
C
      CHARACTER*80    CARDS(12)
C
C  First, set up the CARDS array to specify the text strings, the
C  text sizes, horizontal centering, and vertical coordinate
C  information.  This information is supplied for each line of text.
C  In this example, all the X-coordinates are set to 512, and the
C  horizontal centering is always set to 1, so each line of text will
C  be centered horizontally.  All character sizes will be 1.5 times
C  the default PLOTCHAR character size except the equation supplied
C  in CARD(8) that will be 3.5 times the default PLOTCHAR character
C  size.  Notice that each line has a Y-coordinate specified for it;
C  these coordinates range from 1500 at the top to 200 at the bottom
C  of the scroll.
C
      NCARDS = 12
      CARDS( 1) = '  512 1500    1  1.5The PLOTCHAR'
      CARDS( 2) = '  512 1400    1  1.5utility'
      CARDS( 3) = '  512 1300    1  1.5can be'
      CARDS( 4) = '  512 1200    1  1.5used to'
      CARDS( 5) = '  512 1100    1  1.5write'
      CARDS( 6) = '  512 1000    1  1.5an equation'
      CARDS( 7) = '  512  900    1  1.5like'
      CARDS( 8) = '  512  700    1  3.5C:S:2:N:=A:S:2:N:+B:S:2:N:'
      CARDS( 9) = '  512  500    1  1.5in a'
      CARDS(10) = '  512  400    1  1.5movie title'
      CARDS(11) = '  512  300    1  1.5created by'
      CARDS(12) = '  512  200    1  1.5STITLE'
C
C  Define the remaining inputs for STITLE.
C
C  NYST specifies the Y-coordinate that will be at the vertical center
C  of the frame when scrolling begins.  The value specified (1300)
C  means that the line "can be" will be centered vetically on the
C  first frame and scrolling will proceed from there.
C
      NYST  = 1300
C
C  NYFIN specifies the Y-coordinate that will be at the vertical center
C  of the frame when scrolling is terminated.  The value specified (400)
C  means that the line "movie title" will be centered vertically on
C  the frame when scrolling terminates.
C
      NYFIN = 400
C
C  TST specifies how many seconds  the first frame will remain
C  stationary before scrolling begins.
C
      TST   = 1.0
C
C  Specify the scroll time.  This is the time in seconds that the
C  text will be scrolled from position NYST to NYFIN.
C
      TMV   = 10.0
C
C  Specify how many seconds  the final frame will remain stationary
C  after scrolling stops.
C
      TFIN  =  0.5
C
C  Indicate that this will be a practice run, and not a production
C  run.
C
      MOVIE = 1
C
C  Call SLSETR to indicate that the first frame should be faded
C  in for 2.5 seconds.
C
      CALL SLSETR('FIN',2.5)
C
C  Call SLSETR to indicate that the last frame should be faded
C  out for 2.0 seconds.
C
      CALL SLSETR('FOU',2.0)
C
C  Call STITLE.
C
      CALL STITLE (CARDS,NCARDS,NYST,NYFIN,TST,TMV,TFIN,MOVIE)
C
      RETURN
      END
