C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL EXPCLQ
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
C
C
      SUBROUTINE EXPCLQ 
C
C PURPOSE                To provide a simple demonstration of the
C                        PLCHLQ text drawing techniques.
C
C USAGE                  CALL EXPCLQ
C
C ARGUMENTS
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
C     White background
      CALL GSCR (1,0,1.,1.,1.)
C     Black foreground
      CALL GSCR (1,1,0.,0.,0.)
C     Red
      CALL GSCR (1,2,1.,0.,0.)
C     Green
      CALL GSCR (1,3,0.,1.,0.)
C     Blue
      CALL GSCR (1,4,0.,0.,1.)

C
C Establish the viewport and window.
C
      CALL SET(.0,1.,0.,1.,0.,1.,0.,1.,1)

C Get the coordinates for a Spiral in the center of the frame
      CALL GNSPRL(.5,.5,.45,500,4,XCOOR2, YCOOR2)
C     Set line width
      CALL GSLWSC(3.)
C     Set line color
      CALL GSPLCI(2)
C     Draw the spiral
      CALL CURVED(XCOOR2,YCOOR2,500)
         
C Draw labels
C     Set text color
      CALL GSTXCI(4)
C     Set the font
      CALL GSTXFP(-13,2)
C     Plot the strings
      CALL PLCHLQ(.41,.58,'Use',.04,55.,0.)
      CALL PLCHLQ(.58,.62,'PLCHLQ',.03,-25.,0.)
C     Set the font
      CALL GSTXFP(-6,2)
C     Plot the strings
      CALL PLCHLQ(.66,.47,'to',.04,-90.,0.)
      CALL PLCHLQ(.57,.33,'access',.05,-145.,0.)
C     Set the font
      CALL GSTXFP(-12,2)
C     Plot the strings
      CALL PLCHLQ(.39,.32,'the',.05,150.,0.)
      CALL PLCHLQ(.28,.47,'GKS',.05,110.,0.)
C     Set the font
      CALL GSTXFP(-16,2)
C     Plot the strings
      CALL PLCHLQ(.34,.67,'fonts',.05,45.,0.)
      CALL PLCHLQ(.525,.75,'and',.05,0.,0.)
C     Set the font
      CALL GSTXFP(-7,2)
C     Plot the strings
      CALL PLCHLQ(.74,.59,'position',.05,-65.,0.)
      CALL PLCHLQ(.73,.31,'text',.05,-130.,0.)
C     Set the font
      CALL GSTXFP(1,2)
C     Plot the strings
      CALL PLCHLQ(.57,.20,'at',.05,-165.,0.)
      CALL PLCHLQ(.40,.20,'any',.05,165.,0.)
C     Set the font
      CALL GSTXFP(-9,2)
C     Plot the string
      CALL PLCHLQ(.23,.32,'angle.',.05,130.,0.)

C Advance the Frame
      CALL FRAME
C
      END

      SUBROUTINE GNSPRL(XCNTR,YCNTR,IRADUS,NPTS,LOOPS,XCOORD,YCOORD)
      REAL XCNTR, YCNTR, IRADUS, XCOORD(NPTS), YCOORD(NPTS)
      INTEGER NPTS, LOOPS

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

C Initialize the angle
       ANGLE = 0.
C Calculate the change in angle (360./number of points in circle)
       DELTA = REAL(LOOPS) * 360./(REAL(NPTS-1))
C Convert to radians
       DELTA = DELTA * RADPDG
C Calculate the change in radius
       DRAD = RADIUS/(REAL(NPTS - 1))

C Calculate each coordinate
       DO 12 I=1,NPTS
 	  XCOORD(I) = RADIUS * (COS(ANGLE)) + XCNTR
 	  YCOORD(I) = RADIUS * (SIN(ANGLE)) + YCNTR
C         Increase the angle
          ANGLE = ANGLE + DELTA
C         Reduce the radius
          RADIUS = RADIUS - DRAD
  12   CONTINUE

      RETURN
      END
