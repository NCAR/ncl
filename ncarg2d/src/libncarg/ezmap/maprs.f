C
C $Id: maprs.f,v 1.18 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPRS
        IF (ICFELL('MAPRS - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPRS
        IF (ICFELL('MAPRS',2).NE.0) RETURN
        RETURN
      END
