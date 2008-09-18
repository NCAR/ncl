C
C $Id: mapstc.f,v 1.21 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPSTC (WHCH,CVAL)
        CHARACTER*(*) WHCH
        CHARACTER*(*) CVAL
        IF (ICFELL('MAPSTC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDSETC (WHCH,CVAL)
        IF (ICFELL('MAPSTC',2).NE.0) RETURN
        RETURN
      END
