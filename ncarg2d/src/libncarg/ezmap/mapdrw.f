C
C $Id: mapdrw.f,v 1.6 1994-03-18 23:49:52 kennison Exp $
C
      SUBROUTINE MAPDRW
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
C
C Initialize the package, draw and label the grid, and draw outlines.
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPDRW - UNCLEARED PRIOR ERROR',1).NE.0) THEN
        IIER=-1
        RETURN
      END IF
C
C Do it to it.
C
      IF (INTF) THEN
        CALL MAPINT
        IF (ICFELL('MAPDRW',2).NE.0) RETURN
      END IF
      CALL MAPGRD
      IF (ICFELL('MAPDRW',3).NE.0) RETURN
      CALL MAPLBL
      IF (ICFELL('MAPDRW',4).NE.0) RETURN
      CALL MAPLOT
      IF (ICFELL('MAPDRW',5).NE.0) RETURN
C
      RETURN
      END
