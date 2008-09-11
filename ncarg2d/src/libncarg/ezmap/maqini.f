C
C $Id: maqini.f,v 1.5 2008-09-11 22:53:32 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAQINI
        IF (ICFELL('MAQINI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDQINI
        RETURN
      END
