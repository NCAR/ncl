C
C $Id: mapitd.f,v 1.15 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPITD (XLAT,XLON,IFST)
        REAL    XLAT,XLON
        INTEGER IFST
        IF (ICFELL('MAPITD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPITD (DBLE(XLAT),DBLE(XLON),IFST)
        IF (ICFELL('MAPITD',2).NE.0) RETURN
        RETURN
      END
