C
C $Id: mpseti.f,v 1.18 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPSETI (WHCH,IVAL)
        CHARACTER*(*) WHCH
        INTEGER       IVAL
        IF (ICFELL('MPSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDSETI (WHCH,IVAL)
        IF (ICFELL('MPSETI',2).NE.0) RETURN
        RETURN
      END
