C
C $Id: mapsav.f,v 1.19 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPSAV (IFNO)
        INTEGER IFNO
        IF (ICFELL('MAPSAV - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPSAV (IFNO)
        IF (ICFELL('MAPSAV',2).NE.0) RETURN
        RETURN
      END
