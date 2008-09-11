C
C $Id: mpgeti.f,v 1.16 2008-09-11 04:11:37 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPGETI (WHCH,IVAL)
        CHARACTER*(*) WHCH
        INTEGER       IVAL
        IF (ICFELL('MPGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDGETI (WHCH,IVAL)
        IF (ICFELL('MPGETI',2).NE.0) RETURN
        RETURN
      END
