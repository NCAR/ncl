
      PROGRAM PGKEX24
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

      DIMENSION X(100),Y(100)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF,IDUM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a small color table for the CGM workstation.
C
      CALL GSCR(IWKID, 0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 1, 0.4, 0.0, 0.4)
      CALL GSCR(IWKID, 2, 0.0, 0.0, 1.0)
C
C  Turn clipping off
C
      CALL GSCLIP(0)
C
C  Generate a straight line of 100 points.
C
      DO 10 I=1,100
        X(I) = I
        Y(I) = 10.*I
   10 CONTINUE
C
C  Use SET to define normalization transformation 1 with linear
C  scaling in the X direction and log scaling in the Y direction.
C
      CALL SET(.15,.85,.15,.85,1.,100.,10.,1000.,2)
C
C  Set line color to blue.
C
      CALL GSPLCI(2)
C
C  Initialize the AUTOGRAPH entry EZXY so that the frame is not 
C  advanced and the Y axis is logarithmic.
C
      CALL DISPLA(2,0,2)
C
C  Output the polyline (X,Y) using EZXY.
C
      CALL EZXY(X,Y,100,' ')
C
C  Establish the identity transformation for character plotting.
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C  Title the plot using Plotchar.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.5,.05,'Log Scaling with SPPS',.025,0.,0.)
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
