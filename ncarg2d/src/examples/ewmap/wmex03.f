
      PROGRAM WMEX03
C
C  Examples of controlling slopes at endpoints of fronts.
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
      CHARACTER*28 LABEL
C
C  Data for picture two illustrating slope control at end points.
C
      PARAMETER (NV=5)
      DIMENSION XV(NV),YV(NV)
      DATA XV/ 0.10, 0.30, 0.50, 0.70, 0.90 /
      DATA YV/ 1.00, 1.08, 1.00, 0.95, 0.94 /
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a color table.
C
      CALL GSCR(IWKID, 0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 1, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID, 2, 1.0, 0.0, 0.0)
      CALL GSCR(IWKID, 3, 0.0, 0.0, 1.0)
C
      CALL PCSETI('CC',1)
      CALL PLCHHQ(0.50,0.96,':F26:Slope control at endpoints',
     +            0.03,0.,0.)
C
C  Set some parameter values.
C
      CALL WMSETI('NMS - number of symbols on front line',6)
      CALL WMSETR('SL1 - slope at start of front line if SLF=0 or 1',0.)       
      CALL WMSETR('SL2 - slope at end of front line if SLF=0 or 1',-15.)       
      CALL WMSETI('WFC - color for warm fronts',1)
      CALL WMSETR('LIN - line widths of front lines',3.)
      CALL WMSETI('REV - line widths of front lines',1)
      CALL WMSETI('WFC - color for warm fronts',2)
      CALL PCSETI('CC',3)
      DO 40 I=3,0,-1
        CALL WMSETI('SLF - flags whether slopes are from SL1 and SL2',I)      
        DO 50 J=1,NV
          YV(J) = YV(J)-0.22
   50   CONTINUE
        CALL WMDRFT(NV,XV,YV)
        WRITE(LABEL,500) I
  500   FORMAT(':F22:SLF=',I1,', SL1=0., SL2=-15.')  
        CALL PLCHHQ(.7,YV(1)+.08,LABEL,.024,0.,0.)
   40 CONTINUE
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
      STOP
C
      END
