C
C $Id: maproj.f,v 1.17 2008-07-27 00:17:01 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPROJ (ARG1,ARG2,ARG3,ARG4)
        CHARACTER*(*) ARG1
        REAL          ARG2,ARG3,ARG4
        IF (ICFELL('MAPROJ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPROJ (ARG1,DBLE(ARG2),DBLE(ARG3),DBLE(ARG4))
        IF (ICFELL('MAPROJ',2).NE.0) RETURN
        RETURN
      END
