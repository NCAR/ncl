C
C $Id: mapdrw.f,v 1.11 1999-04-02 22:59:27 kennison Exp $
C
      SUBROUTINE MAPDRW
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
C Initialize the package, draw and label the grid, and draw outlines.
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPDRW - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Do the lower-level calls necessary to draw a complete map.
C
      IF (INTF) THEN
        CALL MAPINT
        IF (ICFELL('MAPDRW',2).NE.0) RETURN
      END IF
C
      CALL MAPGRD
      IF (ICFELL('MAPDRW',3).NE.0) RETURN
C
      CALL MAPLBL
      IF (ICFELL('MAPDRW',4).NE.0) RETURN
C
      CALL MAPLOT
      IF (ICFELL('MAPDRW',5).NE.0) RETURN
C
      RETURN
      END
