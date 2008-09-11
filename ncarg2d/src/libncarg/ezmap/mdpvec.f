C
C $Id: mdpvec.f,v 1.6 2008-09-11 04:11:37 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPVEC (XLAT,XLON)
        DOUBLE PRECISION XLAT,XLON
        IF (ICFELL('MDPVEC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPIT (XLAT,XLON,1)
        IF (ICFELL('MDPVEC',2).NE.0) RETURN
        RETURN
      END
