C
C $Id: mapdrw.f,v 1.12 2000-07-12 16:23:15 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
