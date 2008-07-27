C
C $Id: mapsti.f,v 1.18 2008-07-27 00:17:01 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPSTI (WHCH,IVAL)
        CHARACTER*(*) WHCH
        INTEGER       IVAL
        IF (ICFELL('MAPSTI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDSETI (WHCH,IVAL)
        IF (ICFELL('MAPSTI',2).NE.0) RETURN
        RETURN
      END
