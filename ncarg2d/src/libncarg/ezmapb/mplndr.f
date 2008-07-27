C
C $Id: mplndr.f,v 1.14 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPLNDR (FLNM,ILVL)
        CHARACTER*(*) FLNM
        INTEGER       ILVL
        IF (ICFELL('MPLNDR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDLNDR (FLNM,ILVL)
        IF (ICFELL('MPLNDR',2).NE.0) RETURN
        RETURN
      END
