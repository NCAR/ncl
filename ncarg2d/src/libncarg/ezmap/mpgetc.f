C
C $Id: mpgetc.f,v 1.18 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPGETC (WHCH,CVAL)
        CHARACTER*(*) WHCH
        CHARACTER*(*) CVAL
        IF (ICFELL('MPGETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDGETC (WHCH,CVAL)
        IF (ICFELL('MPGETC',2).NE.0) RETURN
        RETURN
      END
