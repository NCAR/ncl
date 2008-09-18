C
C $Id: mapint.f,v 1.25 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPINT
        IF (ICFELL('MAPINT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPINT
        IF (ICFELL('MAPINT',2).NE.0) RETURN
        RETURN
      END
