
      PROGRAM FPCLOQU
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
      CALL EXPCLQ (IWKID)
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
C
      SUBROUTINE EXPCLQ (IWKID)
C
C PURPOSE                To provide a simple demonstration of the
C                        PLCHLQ text drawing techniques.
C
C USAGE                  CALL EXPCLQ (IWKID)
C
C ARGUMENTS
C
C ON INPUT               IWKID
C                          A workstation id
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
      REAL XCOOR2(500), YCOOR2(500)
C
C Turn buffering off
C
      CALL SETUSV('PB',2)
C
C Set up a color table
C
C  White background
C
      CALL GSCR (IWKID,0,1.,1.,1.)
C
C  Black foreground
C
      CALL GSCR (IWKID,1,0.,0.,0.)
C
C  Red
C
      CALL GSCR (IWKID,2,1.,0.,0.)
C
C  Green
C
      CALL GSCR (IWKID,3,0.,1.,0.)
C
C  Blue
C
      CALL GSCR (IWKID,4,0.,0.,1.)
C
C Establish the viewport and window.
C
      CALL SET(.0,1.,0.,1.,0.,1.,0.,1.,1)
C
C Get the coordinates for a Spiral in the center of the frame
C
      CALL GNSPRL(.5,.5,.45,500,4,XCOOR2, YCOOR2)
C
C  Set line width
C
      CALL GSLWSC(3.)
C
C  Set line color
C
      CALL GSPLCI(2)
C
C  Draw the spiral
C
      CALL CURVED(XCOOR2,YCOOR2,500)
C
C Draw labels
C  Set text color
C
      CALL GSTXCI(4)
C
C Set the font
C
      CALL GSTXFP(-13,2)
C
C Plot the strings
C
      CALL PLCHLQ(.41,.58,'Use',.04,55.,0.)
      CALL PLCHLQ(.58,.62,'PLCHLQ',.03,-25.,0.)
C
C Set the font
C
      CALL GSTXFP(-6,2)
C
C Plot the strings
C
      CALL PLCHLQ(.66,.47,'to',.04,-90.,0.)
      CALL PLCHLQ(.57,.33,'access',.05,-145.,0.)
C
C Set the font
C
      CALL GSTXFP(-12,2)
C
C Plot the strings
C
      CALL PLCHLQ(.39,.32,'the',.05,150.,0.)
      CALL PLCHLQ(.28,.47,'GKS',.05,110.,0.)
C
C Set the font
C
      CALL GSTXFP(-16,2)
C
C Plot the strings
C
      CALL PLCHLQ(.34,.67,'fonts',.05,45.,0.)
      CALL PLCHLQ(.525,.75,'and',.05,0.,0.)
C
C Set the font
C
      CALL GSTXFP(-7,2)
C
C  Plot the strings
C
      CALL PLCHLQ(.74,.59,'position',.05,-65.,0.)
      CALL PLCHLQ(.73,.31,'text',.05,-130.,0.)
C
C Set the font
C
      CALL GSTXFP(1,2)
C
C  Plot the strings
C
      CALL PLCHLQ(.57,.20,'at',.05,-165.,0.)
      CALL PLCHLQ(.40,.20,'any',.05,165.,0.)
C
C Set the font
C
      CALL GSTXFP(-9,2)
C
C Plot the string
C
      CALL PLCHLQ(.23,.32,'angle.',.05,130.,0.)
C
C Advance the Frame
C
      CALL FRAME
C
      END

      SUBROUTINE GNSPRL(XCNTR,YCNTR,IRADUS,NPTS,LOOPS,XCOORD,YCOORD)
      INTEGER NPTS, LOOPS
      REAL XCNTR, YCNTR, IRADUS, XCOORD(NPTS), YCOORD(NPTS)
C
C This function generates the coordinates for a spiral with
C center at XCNTR, YCNTR, and an initial radius of IRADUS. The spiral
C will turn on itself LOOPS times.  There are
C NPTS in the Spiral and the coordinates are returned in the
C arrays XCOORD and YCOORD.
C
      RADIUS = IRADUS
C
C Compute number of radians per degree
C
      RADPDG = 2.*3.14159/360.
C
C Initialize the angle
C
       ANGLE = 0.
C
C Calculate the change in angle (360./number of points in circle)
C
       DELTA = REAL(LOOPS) * 360./(REAL(NPTS-1))
C
C Convert to radians
C
       DELTA = DELTA * RADPDG
C
C Calculate the change in radius
C
       DRAD = RADIUS/(REAL(NPTS - 1))
C
C Calculate each coordinate
C
       DO 12 I=1,NPTS
          XCOORD(I) = RADIUS * (COS(ANGLE)) + XCNTR
          YCOORD(I) = RADIUS * (SIN(ANGLE)) + YCNTR
C
C Increase the angle
C
          ANGLE = ANGLE + DELTA
C
C Reduce the radius
C
          RADIUS = RADIUS - DRAD
  12   CONTINUE

      RETURN
      END
