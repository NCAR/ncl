
      PROGRAM CLBBAR
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

      INTEGER INDEX(18)
      CHARACTER*12 LABELS(18)
      
      DATA INDEX /1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 
     +      15, 16, 17, 18 /

      DATA LABELS /'White', 'Orchid', 'Red', 'OrangeRed', 'Orange',
     +      'Gold', 'Yellow', 'GreenYellow', 'Chartreuse',       
     +      'Green', 'Celeste', 'Aqua','DeepSkyBlue','RoyalBlue',
     +      'SlateBlue', 'DarkViolet', 'Lavender', 'Grey' /
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL COLOR (IWKID)
      CALL GSFAIS(1)
      CALL LBLBAR(1,0.,1.,0.,1.,18,.5,1.,INDEX,1,LABELS,18,1)
C
C Advance frame
C
      CALL FRAME
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE COLOR (IWKID)
C
C   *** BACKGROUND COLOR ***
C Black
C
      CALL GSCR(IWKID,0,0.,0.,0.)
C
C   *** FORGROUND COLORS ***
C White
C
      CALL GSCR(IWKID,  1, 1.0, 1.0, 1.0)
C
C Orchid
C
      CALL GSCR(IWKID,  2, 0.85, 0.45, 0.8)
C
C Red
C
      CALL GSCR(IWKID,  3, 0.9, 0.25, 0.0)
C
C OrangeRed
C
      CALL GSCR(IWKID,  4, 1.0, 0.0, 0.2)
C
C Orange
C
      CALL GSCR(IWKID,  5, 1.0, 0.65, 0.0)
C
C Gold
C
      CALL GSCR(IWKID,  6, 1.0, 0.85, 0.0)
C
C Yellow
C
      CALL GSCR(IWKID,  7, 1.0, 1.0, 0.0)
C
C GreenYellow
C
      CALL GSCR(IWKID,  8, 0.7, 1.0, 0.2)
C
C Chartreuse
C
      CALL GSCR(IWKID,  9, 0.5, 1.0, 0.0)
C
C Green
C
      CALL GSCR(IWKID, 10, 0.2, 0.8, 0.2)
C
C Celeste
C
      CALL GSCR(IWKID, 11, 0.2, 1.0, 0.5)
C
C Aqua
C
      CALL GSCR(IWKID, 12, 0.0, 0.9, 1.0)
C
C DeepSkyBlue
C
      CALL GSCR(IWKID, 13, 0.0, 0.75, 1.0)
C
C RoyalBlue
C
      CALL GSCR(IWKID, 14, 0.25, 0.45, 0.95)
C
C SlateBlue
C
      CALL GSCR(IWKID, 15, 0.4, 0.35, 0.8)
C
C DarkViolet
C
      CALL GSCR(IWKID, 16, 0.6, 0.0, 0.8)
C
C Lavender
C
      CALL GSCR(IWKID, 17, 0.8, 0.8, 1.0)
C
C Gray
C
      CALL GSCR(IWKID, 18, 0.5, 0.5, 0.5)
C
C Done.
C
      RETURN
C
      END
