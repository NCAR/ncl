      PROGRAM GKE01
C
C  Simple example illustrating opening GKS, opening and activating
C  a CGM workstation and an X11 workstation.
C
      CALL GOPKS(6,0)
C
C  Specify text alignment of (center, half) and set character height.
C
      CALL GSTXAL(2,3)
      CALL GSCHH(.05)
C
C  Open a CGM workstation with workstation ID of 1.
C
      CALL GOPWK(1,2,1)
C
C  Open an X11 workstation with workstation ID of 2.
C
      CALL GOPWK(2,0,8)
C
C  Activate the workstations.
C
      CALL GACWK(1)
      CALL GACWK(2)
C
C  Draw a text string.
C
      CALL GTX(.5,.5,'Text')
      CALL FRAME
C
C  Close things down.
C
      CALL GDAWK(1)
      CALL GDAWK(2)
      CALL GCLWK(1)
      CALL GCLWK(2)
      CALL GCLKS
C
      STOP
      END
