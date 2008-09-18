C
C $Id: mapset.f,v 1.21 2008-09-18 00:42:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MAPSET (ARG1,ARG2,ARG3,ARG4,ARG5)
        CHARACTER*(*) ARG1
        REAL          ARG2(2),ARG3(2),ARG4(2),ARG5(2)
        DOUBLE PRECISION DRG2(2),DRG3(2),DRG4(2),DRG5(2)
        DRG2(1)=DBLE(ARG2(1))
        DRG2(2)=DBLE(ARG2(2))
        DRG3(1)=DBLE(ARG3(1))
        DRG3(2)=DBLE(ARG3(2))
        DRG4(1)=DBLE(ARG4(1))
        DRG4(2)=DBLE(ARG4(2))
        DRG5(1)=DBLE(ARG5(1))
        DRG5(2)=DBLE(ARG5(2))
        CALL MDPSET (ARG1,DRG2,DRG3,DRG4,DRG5)
        IF (ICFELL('MAPSET',2).NE.0) RETURN
        RETURN
      END
