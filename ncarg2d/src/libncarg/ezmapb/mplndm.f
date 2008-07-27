C
C $Id: mplndm.f,v 1.14 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPLNDM (FLNM,ILVL,IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,
     +                                                           ULPR)
        CHARACTER*(*) FLNM
        INTEGER       ILVL,IAMA(*)
        REAL          XCRA(MCRA),YCRA(MCRA)
        INTEGER       MCRA,IAAI(MNOG),IAGI(MNOG),MNOG
        IF (ICFELL('MPLNDM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
        CALL MDLNDM (FLNM,ILVL,IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
        IF (ICFELL('MPLNDM',2).NE.0) RETURN
        RETURN
      END
