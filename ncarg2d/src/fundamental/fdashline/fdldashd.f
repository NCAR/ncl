
      PROGRAM FDLDASHD
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
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL LINEEX (IWKID)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
      SUBROUTINE LINEEX (IWKID)
C
C PURPOSE                To provide a simple demonstration of 
C                        how to set line dash patterns. 
C
C USAGE                  CALL LINEEX (IWKID)
C
C ARGUMENTS
C
C ON INPUT               IWKID
C                          Workstation id
C
C LANGUAGE               FORTRAN
C
C PORTABILITY            FORTRAN 77
C
C NOTE                   The call to GOPWK will have to be modified
C                        when using a non-NCAR GKS package.  The third
C                        argument must be the workstation type for WISS.
C
C
      CHARACTER STR*60
      REAL XCOORD(360), YCOORD(360)
C
C  Data for the graphical objects.
C
      DATA    TWOPI/6.283185/
C
C Establish the viewport and window.
C
      CALL SET(.01,.99,0.01,.99,0.,1.,0.,1.,1)
C
C Turn buffering off
C
      CALL SETUSV('PB',2)
C
C Set up a color table
C
C White background
C
      CALL GSCR (IWKID,0,1.,1.,1.)
C
C Black foreground
C
      CALL GSCR (IWKID,1,0.,0.,0.)
C
C Red
C
      CALL GSCR (IWKID,2,1.,0.,0.)
C
C Green
C
      CALL GSCR (IWKID,3,0.,1.,0.)
C
C Blue
C
      CALL GSCR (IWKID,4,0.,0.,1.)
C
C Create and Plot 2 Sine Curves
C
C Set the dash pattern
C
      STR = '$''$''Line''drawn''with''VECTD$''$''$''$'''
      CALL DASHDC(STR,15,20)
C
C Move plotter pen
C
      CALL FRSTD(0.,.60)
      DO 22 I=1,360
         Y = (SIN(REAL(I)*(TWOPI/360.)) * .25) + .60
         CALL VECTD(REAL(I)/360.,Y)
 22   CONTINUE
C
C Set the Dash pattern
C
      STR = '$$$$$$Line''drawn''with''CURVED$$$$$$$$$$$$'
      CALL DASHDC(STR,15,20)
      CALL GSLWSC(3.)
      DO 23 I=1,360
         XCOORD(I) = REAL(I)/360.
         YCOORD(I) = (SIN(REAL(I)*(TWOPI/360.)) * .25) + .45
 23   CONTINUE
C
C Draw the second curve
C
      CALL CURVED(XCOORD, YCOORD, 360)
C
C Draw a straight line
C
      CALL GSLWSC(4.)
C
C 1111100110011111 binary  = 63903 decimal
C
      CALL DASHDB(63903)
      CALL LINED(0.1,.15, .9,.15)
C
C Label the line
C
      CALL PLCHLQ(0.5,0.10,'Line drawn with LINED',20.,0.,0.)
C
C  Create a background perimeter 
C
      CALL GSPLCI(1)
      CALL FRSTPT( 0.0, 0.0)
      CALL VECTOR( 1.0, 0.0)
      CALL VECTOR( 1.0, 1.0)
      CALL VECTOR( 0.0, 1.0)
      CALL VECTOR( 0.0, 0.0)
C
C  Label the plot
C
      CALL PLCHLQ(0.7,0.90,'Setting Dash Patterns',25.,0.,0.)
      CALL PLCHLQ(0.7,0.81,'with DASHDB and DASHDC',25.,0.,0.)
      CALL FRAME
C
      END
