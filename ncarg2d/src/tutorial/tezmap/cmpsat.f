C
C   $Id: cmpsat.f,v 1.4 1995-06-14 14:07:15 haley Exp $
C
      PROGRAM CMPSAT
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
      CALL GSCLIP (0)
C
C International outlines
C
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','PO')
C
C Satellite-view.
C
      CALL MAPROJ ('SV',40.,10.,0.)
      CALL MAPSTR ('SA - SATELLITE DISTANCE',2.)
      CALL MAPSTR ('S1 - SATELLITE ANGLE 1',10.)
      CALL MAPSTR ('S2 - SATELLITE ANGLE 2',15.)
      CALL MAPDRW

      CALL FRAME
C
C Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
      
