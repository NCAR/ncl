C
C $Id: mapfst.f,v 1.2 1993-12-21 00:32:43 kennison Exp $
C
      SUBROUTINE MAPFST (XLAT,XLON)
      CALL MAPIT (XLAT,XLON,0)
      IF (ICFELL('MAPFST',1).NE.0) RETURN
      RETURN
      END
