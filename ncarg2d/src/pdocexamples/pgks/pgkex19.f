      PROGRAM LOGSCL
C
      DIMENSION X(100),Y(100)
C
C  Open GKS, open and activate the metafile workstation.
C
      CALL GOPKS (6,IDUM)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C  Define a small color table for the CGM workstation.
C
      CALL GSCR(1,0,0.,0.,0.)
      CALL GSCR(1,1,0.,1.,1.)
      CALL GSCR(1,2,1.,1.,0.)
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
C  Set line color to yellow.
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
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
C
      STOP
      END
