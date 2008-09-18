C
C $Id: mpgetr.f,v 1.18 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPGETR (WHCH,RVAL)
        CHARACTER*(*) WHCH
        REAL          RVAL
        IF (ICFELL('MPGETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDGETR (WHCH,RVAL)
        IF (ICFELL('MPGETR',2).NE.0) RETURN
        RETURN
      END
