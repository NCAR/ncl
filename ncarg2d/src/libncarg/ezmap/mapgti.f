C
C $Id: mapgti.f,v 1.22 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPGTI (WHCH,IVAL)
        CHARACTER*(*) WHCH
        INTEGER       IVAL
        IF (ICFELL('MAPGTI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDGETI (WHCH,IVAL)
        IF (ICFELL('MAPGTI',2).NE.0) RETURN
        RETURN
      END
