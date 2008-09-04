C
C $Id: mapfst.f,v 1.14 2008-09-04 19:56:57 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPFST (XLAT,XLON)
        REAL XLAT,XLON
        IF (ICFELL('MAPFST - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPIT (DBLE(XLAT),DBLE(XLON),0)
        IF (ICFELL('MAPFST',2).NE.0) RETURN
        RETURN
      END
