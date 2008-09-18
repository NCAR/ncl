C
C $Id: mdpfst.f,v 1.8 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPFST (XLAT,XLON)
        DOUBLE PRECISION XLAT,XLON
        IF (ICFELL('MDPFST - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPIT (XLAT,XLON,0)
        IF (ICFELL('MDPFST',2).NE.0) RETURN
        RETURN
      END
