      PROGRAM TGFLAS
C
C PURPOSE                To provide a simple demonstration of the
C                        GFLASH package.
C
C I/O                    If there is a normal exit from GFLASH,
C                        the message
C
C                          GFLASH TEST SUCCESSFUL . . . SEE PLOTS TO
C                          VERIFY PERFORMANCE
C
C                        is written on unit 6
C
C NOTE                   The call to GOPWK will have to be modified
C                        when using a non-NCAR GKS package.  The third
C                        argument must be the workstation type for WISS.
C
C
C  Data for the border and two lines.
C
      DIMENSION PERIMX(5),PERIMY(5)
      DATA      PERIMX/0., 1., 1., 0., 0./
      DATA      PERIMY/0., 0., 1., 1., 0./
C
      DIMENSION RLIN1X(2),RLIN1Y(2)
      DATA      RLIN1X/.25,.75/
      DATA      RLIN1Y/.25,.75/
C
      DIMENSION RLIN2X(2),RLIN2Y(2)
      DATA      RLIN2X/.75,.25/
      DATA      RLIN2Y/.25,.75/
C
C Open GKS, open a workstation of type 1, activate the workstation.
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C  Initialize the GFLASH package.  If using a non-NCAR GKS package
C  the final argument in the following call should be replaced with
C  the workstation type for WISS.
C
      CALL GOPWK(9,1,3)
C
C  Establish character height and text alignment.
C
C
C  Put a line with positive slope into flash buffer 1.
C
      CALL GFLAS1(1)
      CALL GPL(2,RLIN1X,RLIN1Y)
      CALL GFLAS2
C
C  Put a line with negative slope into flash buffer 2.
C
      CALL GFLAS1(2)
      CALL GPL(2,RLIN2X,RLIN2Y)
      CALL GFLAS2
C
C  Draw the border.
C
      CALL GPL(5,PERIMX,PERIMY)
C
C  Put the two segments into the picture.
C
      CALL GFLAS3(1)
      CALL GFLAS3(2)
      CALL FRAME
C
C  Close the GFLASH package.
C
      CALL GCLWK(9)
C
C  Deactivate and close the metafile workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
C
      END
