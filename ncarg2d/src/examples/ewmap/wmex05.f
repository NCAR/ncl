
      PROGRAM WMEX05
C
C  Examples of regional weather and temperatures.
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
C  Number of points defining region; number of regions; number of points
C  in clip region.
C
      PARAMETER (NU=8, NO=8, NOP1=NO+1, NC=5)
C
C  Data for a region.
C
      DIMENSION XU(NU),YU(NU),XP(NU),YP(NU)
      DIMENSION XOFF(NO),YOFF(NO)
      DIMENSION XCLIP(NC),YCLIP(NC)
      CHARACTER*13 LABELS(NOP1)
C
      DATA XU/ 0.05, 0.20, 0.40, 0.70, 0.80, 0.65, 0.40, 0.05/
      DATA YU/ 0.70, 0.45, 0.60, 0.70, 0.80, 0.95, 0.84, 0.70/
      DATA XOFF/0.10, 0.52, 0.10, 0.52, 0.10, 0.52,  0.10,  0.52/
      DATA YOFF/0.47, 0.47, 0.27, 0.27, 0.07, 0.07, -0.13, -0.13/
      DATA XCLIP/0.10, 0.75, 0.75, 0.10, 0.10/
      DATA YCLIP/0.50, 0.50, 0.85, 0.85, 0.50/
      DATA LABELS/'Ice','Snow','Flurries','Rain','Showers',
     +            'Thunderstorms','Temperature',
     +            'Temperature','(clipped)'/
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
      CALL GSCR(IWKID, 4, 0.0, 1.0, 0.0)
C
      CALL PLCHHQ(0.50,0.93,':F26:Weather and temperature regions',
     +            0.033,0.,0.)       
C
      CALL WMSETI('COL',3)
      SCALE = .4
      CALL WMSETR('RHT - size scale',0.015)
      CALL PCSETI('FN',26)
      DO 10 I=1,NO
        DO 20 J=1,NU
          XP(J) = SCALE*XU(J)+XOFF(I)
          YP(J) = SCALE*YU(J)+YOFF(I)
   20   CONTINUE
        IF (I .LE. NO-2) THEN
          CALL WMDRRG(NU,XP,YP,LABELS(I),1,XP,YP)
          CALL PLCHHQ(XP(3)+.01,YP(3)-.025,LABELS(I),0.02,0.,-1.)       
        ELSE IF (I .EQ. NO-1) THEN
          CALL WMDRRG(NU,XP,YP,'INDEX2',1,XP,YP)
          CALL PLCHHQ(XP(3)+.01,YP(3)-.025,LABELS(I),0.02,0.,-1.)       
        ELSE IF (I .EQ. NO) THEN
          DO 30 J=1,NC
            XCLIP(J) = SCALE*XCLIP(J)+XOFF(I)
            YCLIP(J) = SCALE*YCLIP(J)+YOFF(I)
   30     CONTINUE
          CALL WMDRRG(NU,XP,YP,'INDEX4',NC,XCLIP,YCLIP)
          CALL PLCHHQ(XP(3)+.01,YP(3)-.025,LABELS(I),0.02,0.,-1.)       
          CALL PLCHHQ(XP(3)+.01,YP(3)-.060,LABELS(I+1),0.02,0.,-1.)       
        ENDIF
   10 CONTINUE
C
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
      STOP
C
      END
