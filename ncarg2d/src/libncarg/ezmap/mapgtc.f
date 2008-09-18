C
C $Id: mapgtc.f,v 1.21 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPGTC (WHCH,CVAL)
        CHARACTER*(*) WHCH
        CHARACTER*(*) CVAL
        IF (ICFELL('MAPGTC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDGETC (WHCH,CVAL)
        IF (ICFELL('MAPGTC',2).NE.0) RETURN
        RETURN
      END
