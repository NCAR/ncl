C
C $Id: mpsetc.f,v 1.17 2008-09-11 22:53:34 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPSETC (WHCH,CVAL)
        CHARACTER*(*) WHCH
        CHARACTER*(*) CVAL
        IF (ICFELL('MPSETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDSETC (WHCH,CVAL)
        IF (ICFELL('MPSETC',2).NE.0) RETURN
        RETURN
      END
