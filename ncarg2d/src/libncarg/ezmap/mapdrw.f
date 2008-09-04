C
C $Id: mapdrw.f,v 1.19 2008-09-04 19:56:57 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPDRW
        IF (ICFELL('MAPDRW - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPDRW
        IF (ICFELL('MAPDRW',2).NE.0) RETURN
        RETURN
      END
