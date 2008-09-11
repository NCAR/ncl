C
C $Id: mpgetd.f,v 1.7 2008-09-11 22:53:34 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPGETD (WHCH,DVAL)
        CHARACTER*(*)    WHCH
        DOUBLE PRECISION DVAL
        IF (ICFELL('MPGETD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDGETD (WHCH,DVAL)
        IF (ICFELL('MPGETD',2).NE.0) RETURN
        RETURN
      END
