
      PROGRAM FGKE02
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
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
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
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
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
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      END
