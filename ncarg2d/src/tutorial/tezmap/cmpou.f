C
C   $Id: cmpou.f,v 1.3 1994-07-11 14:10:49 haley Exp $
C
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
C Open GKS, Turn Clipping off
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Invoke demo driver
C
      CALL CMPOU
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE CMPOU 

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
