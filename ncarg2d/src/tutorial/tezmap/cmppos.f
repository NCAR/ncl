
      PROGRAM CMPPOS
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
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
      CALL GSCLIP (0)
C
C Draw map in lower lefthand corner of the window
C
      CALL MAPPOS (0.5, 1.0, 0.0, 0.5)
      CALL MAPDRW
C
C Draw a perimeter around the viewport
C
      CALL GSELNT(0)
      CALL PLOTIF (0.0,0.0,0)
      CALL PLOTIF (1.0,0.0,1)
      CALL PLOTIF (1.0,1.0,1)
      CALL PLOTIF (0.0,1.0,1)
      CALL PLOTIF (0.0,0.0,1)

      CALL FRAME
C
C Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
