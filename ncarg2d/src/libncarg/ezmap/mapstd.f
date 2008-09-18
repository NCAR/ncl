C
C $Id: mapstd.f,v 1.8 2008-09-18 00:42:16 kennison Exp $
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
