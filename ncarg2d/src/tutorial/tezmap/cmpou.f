
      PROGRAM CMPOU
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
C Open GKS, Turn Clipping off
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Invoke demo driver
C
      CALL TCMPOU
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE TCMPOU 

      REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)

      DATA PLIM1 /30.,0./
      DATA PLIM2 /-15.,0./
      DATA PLIM3 /60.,0./
      DATA PLIM4 /30.,0./
C
C CMPOU demonstrates political boundaries in the Maps utility.
C
C Set up Maps.
C
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','PO')
      CALL MAPROJ ('ME',0.,0.,0.)
      CALL MAPSET ('CO',PLIM1,PLIM2,PLIM3,PLIM4)
C
C Initialize Maps.
C
      CALL MAPINT
C
C Draw a perimeter and outline all the countries.
C
      CALL MAPSTI('LA - LABEL FLAG',0)
      CALL MAPLBL
      CALL MAPLOT
C
C Advance the frame.
C
      CALL FRAME
C
C Done.
C
      RETURN
      END
