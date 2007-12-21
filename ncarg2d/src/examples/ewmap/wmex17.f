      PROGRAM WMEX16
C
C  Example of drawing vectors over a map.
C
C
C  Define the error file, the Fortran unit number, the workstation type,
C  and the workstation ID to be used in calls to GKS routines.
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
      CALL GOPKS (IERRF, 0)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define some colors.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,1.,0.,0.)
      CALL GSCR(IWKID,3,0.,1.,0.)
      CALL GSCR(IWKID,4,0.,0.,1.)
      CALL GSCR(IWKID,5,0.,0.,0.)
      CALL GSCR(IWKID,6,0.,1.,1.)
      CALL GSCR(IWKID,7,1.,0.,1.)
C
C  Tell EZMAP what part of the frame to use.
C
      CALL MAPPOS (.0,1.,0.,1.)
C
C  Draw an orthographic projection with continental outlines plotted.
C
      CALL MAPROJ ('OR',70.,-10.,0.)
      CALL MAPSET ('CO',55.,-50.,85.,-20.)
      CALL MAPSTC ('OU','CO')
C
C  Draw the EZMAP background.
C
      CALL MAPDRW()
C
C  Draw vectors based at the same point on the map.
C  
      CALL WMSETR('VCW - linewidth scale',2.)
      CALL WMSETI('VCC - vector color',2)
      CALL WMVECTMAP(70.,-40.,  0., 80.)
      CALL WMSETI('VCC - vector color',3)
      CALL WMVECTMAP(70.,-40., 80., 80.)
      CALL WMSETI('VCC - vector color',4)
      CALL WMVECTMAP(70.,-40., 80.,  0.)
      CALL WMSETI('VCC - vector color',5)
      CALL WMVECTMAP(70.,-40.,-80.,  0.)
      CALL WMSETI('VCC - vector color',6)
      CALL WMVECTMAP(70.,-40.,  0.,-80.)
      CALL WMSETI('VCC - vector color',7)
      CALL WMVECTMAP(70.,-40.,-80.,-80.)
C
C  Draw label box.
C
      CALL WMVLBL(0.95,0.0015)
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      END
