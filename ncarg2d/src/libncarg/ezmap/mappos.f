C
C $Id: mappos.f,v 1.15 2008-09-11 04:11:36 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPPOS (ARG1,ARG2,ARG3,ARG4)
        REAL ARG1,ARG2,ARG3,ARG4
        IF (ICFELL('MAPPOS - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPPOS (DBLE(ARG1),DBLE(ARG2),DBLE(ARG3),DBLE(ARG4))
        IF (ICFELL('MAPPOS',2).NE.0) RETURN
        RETURN
      END
