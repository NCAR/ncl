
      PROGRAM FGKE01
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1) 
      PARAMETER (IWKID2=2)
C
C  Simple example illustrating opening GKS, opening and activating
C  a CGM workstation and an X11 workstation.
C
      CALL GOPKS (IERRF, ISZDM)
C
C  Specify text alignment of (center, half) and set character height.
C
      CALL GSTXAL(2,3)
      CALL GSCHH(.05)
C
C  Open a CGM workstation with workstation ID of IWKID.
C
      CALL GOPWK (IWKID, LUNIT, 1)
C
C  Open an X11 workstation with workstation ID of IWKID2.
C
      CALL GOPWK(IWKID2,0,8)
C
C  Activate the workstations.
C
      CALL GACWK(IWKID)
      CALL GACWK(IWKID2)
C
C  Draw a text string.
C
      CALL GTX(.5,.5,'Text')
      CALL FRAME
C
C  Close things down.
C
      CALL GDAWK(IWKID)
      CALL GDAWK(IWKID2)
      CALL GCLWK(IWKID)
      CALL GCLWK(IWKID2)
      CALL GCLKS
C
      STOP
      END
