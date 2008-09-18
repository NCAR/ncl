C
C $Id: maptra.f,v 1.17 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPTRA (RLAT,RLON,UVAL,VVAL)
        REAL   RLAT,RLON,UVAL,VVAL
        DOUBLE PRECISION DUVL,DVVL
        IF (ICFELL('MAPTRA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDPTRA (DBLE(RLAT),DBLE(RLON),DUVL,DVVL)
        IF (ICFELL('MAPTRA',2).NE.0) RETURN
        IF (DUVL.NE.1.D12) THEN
          UVAL=REAL(DUVL)
          VVAL=REAL(DVVL)
        ELSE
          UVAL=1.E12
          VVAL=1.E12
        END IF
        RETURN
      END
