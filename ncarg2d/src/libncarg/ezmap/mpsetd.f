C
C $Id: mpsetd.f,v 1.8 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPSETD (WHCH,DVAL)
        CHARACTER*(*)    WHCH
        DOUBLE PRECISION DVAL
        IF (ICFELL('MPSETD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDSETD (WHCH,DVAL)
        IF (ICFELL('MPSETD',2).NE.0) RETURN
        RETURN
      END
