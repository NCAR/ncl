C
C $Id: mapvec.f,v 1.2 1993-12-21 00:33:56 kennison Exp $
C
      SUBROUTINE MAPVEC (XLAT,XLON)
      CALL MAPIT (XLAT,XLON,1)
      IF (ICFELL('MAPVEC',1).NE.0) RETURN
      RETURN
      END
