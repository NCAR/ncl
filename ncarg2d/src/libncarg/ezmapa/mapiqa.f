C
C	$Id: mapiqa.f,v 1.1.1.1 1992-04-17 22:32:16 ncargd Exp $
C
C
C The subroutine MAPIQA.
C --- ---------- ------
C
      SUBROUTINE MAPIQA (IAMP,IGRP,IDLT,IDRT)
      DIMENSION IAMP(*)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
C
C Terminate the line, if any.
C
      IF (.NOT.(NCRA.GT.1)) GO TO 10000
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      NCRA=0
10000 CONTINUE
C
C Done.
C
      RETURN
C
      END
