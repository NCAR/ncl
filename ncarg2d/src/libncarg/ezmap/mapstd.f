C
C $Id: mapstd.f,v 1.6 2008-09-11 04:11:36 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPSTD (WHCH,DVAL)
        CHARACTER*(*)    WHCH
        DOUBLE PRECISION DVAL
        IF (ICFELL('MAPSTD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDSETD (WHCH,DVAL)
        IF (ICFELL('MAPSTD',2).NE.0) RETURN
        RETURN
      END
