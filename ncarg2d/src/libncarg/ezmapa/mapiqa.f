C
C $Id: mapiqa.f,v 1.4 1994-03-18 23:18:51 kennison Exp $
C
      SUBROUTINE MAPIQA (IAMP,IGRP,IDLT,IDRT)
C
      DIMENSION IAMP(*)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
C
C Check for an uncleared prior error.
C
      IF (.NOT.(ICFELL('MAPIQA - UNCLEARED PRIOR ERROR',1).NE.0))
     +GO TO 10000
      IIER=-1
      RETURN
10000 CONTINUE
C
C Terminate the line, if any.
C
      IF (.NOT.(NCRA.GT.1)) GO TO 10001
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (.NOT.(ICFELL('MAPIQA',2).NE.0)) GO TO 10002
      IIER=-1
      RETURN
10002 CONTINUE
      NCRA=0
10001 CONTINUE
C
C Done.
C
      RETURN
C
      END
