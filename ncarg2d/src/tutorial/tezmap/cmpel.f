
      PROGRAM CMPEL
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
C Open GKS and turn off clipping.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP (0)
C
C Call the routine which does all the work
C
      CALL TCMPEL
C
C Close GKS, and end program
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE TCMPEL
C
C Satellite-view.
C
C Do this plot in white with Softfill over the water and no lat/lon
C lines
C
      CALL MAPROJ ('SV',40.,10.,0.)
      CALL MAPSTR ('SA - SATELLITE DISTANCE',5.)
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','PO')
      CALL MAPSTI ('PE - PERIMETER FLAG', 0)
      CALL MAPSTI ('EL - ELLIPTICAL-PERIMETER SELECTOR', 1)
      CALL MAPINT
      CALL MAPLBL
      CALL MAPLOT
      CALL FRAME

      RETURN
      END
