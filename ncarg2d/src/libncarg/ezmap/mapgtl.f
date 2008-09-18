C
C $Id: mapgtl.f,v 1.20 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPGTL (WHCH,LVAL)
        CHARACTER*(*) WHCH
        LOGICAL       LVAL
        IF (ICFELL('MAPGTL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDGETL (WHCH,LVAL)
        IF (ICFELL('MAPGTL',2).NE.0) RETURN
        RETURN
      END
