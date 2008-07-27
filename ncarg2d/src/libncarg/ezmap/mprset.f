C
C $Id: mprset.f,v 1.13 2008-07-27 00:17:05 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPRSET
        IF (ICFELL('MPRSET - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDRSET
        IF (ICFELL('MPRSET',2).NE.0) RETURN
        RETURN
      END
