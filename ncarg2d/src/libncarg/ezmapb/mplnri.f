C
C $Id: mplnri.f,v 1.9 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPLNRI (FLNM)
        CHARACTER*(*) FLNM
        IF (ICFELL('MPLNRI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDLNRI (FLNM)
        IF (ICFELL('MPLNRI',2).NE.0) RETURN
        RETURN
      END
