
      PROGRAM CMPDD
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
C Open GKS, and turn off clipping.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP (0)
C
C Call the mapping routine CMPDD
C
      CALL TCMPDD(IWKID)
C
C Close GKS and quit.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE TCMPDD(IWKID)
C
C CMPDD demonstrates setting the dash pattern for grid lines in Maps.
C
C Set up color table.
C
      CALL COLOR(IWKID)
C
C Draw Continental, political outlines in magenta
C
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','PO')
      CALL MAPSTI ('C5 - CONTINENTAL OUTLINE COLOR',5)
      CALL MAPSTI ('C7 - COUNTRY OUTLINE COLOR',5)
C
C Draw grid lines and limb line in green
C
      CALL MAPSTI ('C2 - GRID COLOR',2)
      CALL MAPSTI ('C4 - LIMB COLOR',2)
C
C Draw labels and perimeter in white
C
      CALL MAPSTI ('C1 - PERIMETER COLOR',1)
      CALL MAPSTI ('C3 - LABEL COLOR',1)
C
C Set up satellite projection
C
      CALL MAPROJ ('SV',40.,-50.,0.)
      CALL MAPSTR ('SA - SATELLITE DISTANCE',5.)
      CALL MAPSET ('MA',0.,0.,0.,0.)
C
C Set grid spacing to 10 degrees, and anchor grid curve at 10 degree 
C intervals.
C
      CALL MAPSTR ('GR - GRID SPACING',10.)
      CALL MAPSTR ('GD - GRID DRAWING RESOLUTION',10.)
C
C Change the dash pattern of the grid lines to be long dashes with short
C spaces
C
      CALL MAPSTI ('DA - DASH PATTERN',64764)
C
C Initialize Maps.
C
      CALL MAPINT
C
C Draw the latitiude and longitude lines and the limb line
C
      CALL MAPGRD
C
C Advance the frame.
C
      CALL FRAME
C
C Done.
C
      RETURN
      END
      SUBROUTINE COLOR(IWKID)
C
C Background color
C White
C
      CALL GSCR(IWKID,0,1.,1.,1.)
C
C Foreground colors
C
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,0.,.7,0.)
      CALL GSCR(IWKID,3,1.,1.,0.)
      CALL GSCR(IWKID,4,.3,.3,1.)
      CALL GSCR(IWKID,5,1.,0.,1.)
      CALL GSCR(IWKID,6,0.,1.,1.)
      CALL GSCR(IWKID,7,1.,0.,0.)

      RETURN
      END
