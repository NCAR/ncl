
      PROGRAM PGKEX12
C
C  Illustrate the various fonts.
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

      CHARACTER*27 LABEL
      DATA LABEL /'NCAR Graphics, Release 4.0'/
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF,IDUM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C  Define colors.
C
      CALL GSCR(IWKID,0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID,1, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID,2, 0.4, 0.0, 0.4)
C
C  Set up character attributes.
C
      CALL GSCHH(.022)
      CALL GSTXAL(2,3)
      CALL GSTXCI(1)
C
C  Loop through the Hershey fonts.
C
      X = .5
      DO 10 I=1,20
        Y = .02+.043*REAL(20-I)
        CALL PCSETI('FN',I)
        IF (I .EQ. 18) CALL PCSETR('AS',.25)
        IF (I .EQ. 19) CALL PCSETR('AS',.5)
        IF (I .EQ. 20) CALL PCSETR('AS',.25)
        CALL PLCHHQ(X,Y,LABEL,.02,0.,0.)
   10 CONTINUE
C
C  Label the plot using Plotchar.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',2)
      CALL PCSETR('AS',0.)
      CALL PLCHHQ(.5,.98,'Same string',.032,0.,0.)
      CALL PLCHHQ(.5,.92,'using various fonts',.032,0.,0.)
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
