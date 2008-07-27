C
C $Id: mapiq.f,v 1.13 2008-07-27 00:17:00 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPIQ
        IF (ICFELL('MAPIQ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPIQ
        IF (ICFELL('MAPIQ',2).NE.0) RETURN
        RETURN
      END
