C
C	$Id: mpgetc.f,v 1.2 1992-09-04 20:38:34 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MPGETC (WHCH,CVAL)
      CHARACTER*(*) WHCH,CVAL
      CALL MAPGTC (WHCH,CVAL)
      RETURN
      END
