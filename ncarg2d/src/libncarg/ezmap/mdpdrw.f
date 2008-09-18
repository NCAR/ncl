C
C $Id: mdpdrw.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPDRW
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
C Initialize the package, draw and label the grid, and draw outlines.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPDRW - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Do the lower-level calls necessary to draw a complete map.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDPDRW',2).NE.0) RETURN
        END IF
C
        CALL MDPGRD
        IF (ICFELL('MDPDRW',3).NE.0) RETURN
C
        CALL MDPLBL
        IF (ICFELL('MDPDRW',4).NE.0) RETURN
C
        CALL MDPLOT
        IF (ICFELL('MDPDRW',5).NE.0) RETURN
C
        RETURN
C
      END
