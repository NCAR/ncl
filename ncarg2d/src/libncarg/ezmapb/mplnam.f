C
C $Id: mplnam.f,v 1.13 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPLNAM (FLNM,ILVL,IAMA)
        CHARACTER*(*) FLNM
        INTEGER       ILVL
        INTEGER       IAMA(*)
        IF (ICFELL('MPLNAM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDLNAM (FLNM,ILVL,IAMA)
        IF (ICFELL('MPLNAM',2).NE.0) RETURN
        RETURN
      END
