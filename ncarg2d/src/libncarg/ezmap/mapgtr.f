C
C $Id: mapgtr.f,v 1.17 2008-09-04 19:56:58 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPGTR (WHCH,RVAL)
        CHARACTER*(*) WHCH
        REAL          RVAL
        IF (ICFELL('MAPGTR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDGETR (WHCH,RVAL)
        IF (ICFELL('MAPGTR',2).NE.0) RETURN
        RETURN
      END
