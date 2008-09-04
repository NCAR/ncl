C
C $Id: mapit.f,v 1.16 2008-09-04 19:56:58 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPIT (XLAT,XLON,IFST)
        REAL    XLAT,XLON
        INTEGER IFST
        IF (ICFELL('MAPIT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPIT (DBLE(XLAT),DBLE(XLON),IFST)
        IF (ICFELL('MAPIT',2).NE.0) RETURN
        RETURN
      END
