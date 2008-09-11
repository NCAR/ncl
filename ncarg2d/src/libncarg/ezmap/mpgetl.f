C
C $Id: mpgetl.f,v 1.16 2008-09-11 04:11:38 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPGETL (WHCH,LVAL)
        CHARACTER*(*) WHCH
        LOGICAL       LVAL
        IF (ICFELL('MPGETL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDGETL (WHCH,LVAL)
        IF (ICFELL('MPGETL',2).NE.0) RETURN
        RETURN
      END
