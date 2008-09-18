C
C $Id: maqtri.f,v 1.6 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAQTRI (UVAL,VVAL,RLAT,RLON)
        REAL   UVAL,VVAL,RLAT,RLON
        DOUBLE PRECISION DLAT,DLON
        IF (ICFELL('MAQTRI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDQTRI (DBLE(UVAL),DBLE(VVAL),DLAT,DLON)
        IF (ICFELL('MAQTRI',2).NE.0) RETURN
        IF (DLAT.NE.1.D12) THEN
          RLAT=REAL(DLAT)
          RLON=REAL(DLON)
        ELSE
          RLAT=1.E12
          RLON=1.E12
        END IF
        RETURN
      END
