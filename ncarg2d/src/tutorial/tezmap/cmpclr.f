
      PROGRAM CMPCLR
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
C Open GKS, Turn Clipping off
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Invoke demo driver
C
      CALL TCMPCLR(IWKID)
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE TCMPCLR(IWKID) 
C
C CMPCLR demonstrates using the color parameters in Maps
C
C Set up Maps.
C
      REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)

      DATA PLIM1 /0.,0./
      DATA PLIM2 /0.,0./
      DATA PLIM3 /0.,0./
      DATA PLIM4 /0.,0./

      CALL COLOR(IWKID)
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','PS')
      CALL MAPSTI ('C1 - PERIMETER COLOR',1)
      CALL MAPSTI ('C2 - GRID COLOR',2)
      CALL MAPSTI ('C3 - LABEL COLOR',3)
      CALL MAPSTI ('C4 - LIMB COLOR',4)
      CALL MAPSTI ('C5 - CONTINENTAL OUTLINE COLOR',5)
      CALL MAPSTI ('C6 - US STATE OUTLINE COLOR',6)
      CALL MAPSTI ('C7 - COUNTRY OUTLINE COLOR',7)
      CALL MAPROJ ('SV',40.,-50.,0.)
      CALL MAPSTR ('SA - SATELLITE DISTANCE',5.)
      CALL MAPSET ('MA',PLIM1,PLIM2,PLIM3,PLIM4)
C
C Initialize Maps.
C
      CALL MAPINT
C
C Draw a perimeter and outline all the countries.
C
      CALL MAPGRD
      CALL MAPLBL
      CALL MAPLOT
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
C The background is white here for better visibility on paper
C
      CALL GSCR(IWKID,0,1.,1.,1.)
C
C Foreground colors
C
      CALL GSCR(IWKID,1,.7,0.,0.)
      CALL GSCR(IWKID,2,0.,.7,0.)
      CALL GSCR(IWKID,3,.7,.4,0.)
      CALL GSCR(IWKID,4,.3,.3,.7)
      CALL GSCR(IWKID,5,.7,0.,.7)
      CALL GSCR(IWKID,6,0.,.7,.7)
      CALL GSCR(IWKID,7,0.,0.,0.)

      RETURN
      END
