C
C $Id: mapiqa.f,v 1.5 1998-04-16 20:26:45 kennison Exp $
C
      SUBROUTINE MAPIQA (IAMP,IGRP,IDLT,IDRT)
C
      DIMENSION IAMP(*)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPIQA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Terminate the line, if any.
C
      IF (.NOT.(NCRA.GT.1)) GO TO 10000
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPIQA',2).NE.0) RETURN
      NCRA=0
10000 CONTINUE
C
C Done.
C
      RETURN
C
      END
