C
C   $Id: cmpusr.f,v 1.4 1995-06-14 14:07:19 haley Exp $
C
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
C
C Open GKS, and turn off clipping.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP (0)
C
C Call the mapping routine CMPUSR
C
      CALL CMPUSR
C
C Close GKS and quit.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE CMPUSR
C
C Set up Satellite-view.
C
      CALL MAPROJ ('SV',40.,10.,0.)
      CALL MAPSTR ('SA - SATELLITE DISTANCE',5.)
C
C Use country outline set
C
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','PO')
C
C Set Dot Distance to be 1
C
      CALL MAPSTR ('DD - DISTANCE BETWEEN DOTS',1.)
C
      CALL MAPDRW
      
      CALL FRAME
      
      RETURN
      END
      
      SUBROUTINE MAPUSR (IPRT)
C
C MAPUSR is used to change the appearance of various parts of a map.
C It is called just before and just after each portion of a map is
C drawn.  The default version does nothing.  This version chooses a
C different dash line pattern for each portion of the map in a different
C dash pattern.
C
C IPRT addresses each portion of the map as follows:
C   1 - Perimeter
C   2 - Grid
C   3 - Labels
C   4 - Limb lines
C   5 - Continental outlines
C   6 - US state outlines
C   7 - International outlines
C
C 1110000011100000
C
      IF (IPRT .EQ. 1) CALL DASHDB(57568)
C
C 1111111100000000 
C
      IF (IPRT .EQ. 2) CALL DASHDB(65280)
C
C 1111111111111111
C
      IF (IPRT .EQ. 3) CALL DASHDB(65535)
C
C 0100110001110000
C
      IF (IPRT .EQ. 4) CALL DASHDB(19568)
C
C 1111000011110000 
C
      IF (IPRT .EQ. 5) CALL DASHDB(61680)
C
C 1110010011100100
C
      IF (IPRT .EQ. 6) CALL DASHDB(58596)
C
C 010101010101010101
C
      IF (IPRT .EQ. 7) CALL DASHDB(21845)

      RETURN
      END

