C
C	$Id: mpgetl.f,v 1.2 1992-09-04 20:38:37 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MPGETL (WHCH,LVAL)
      CHARACTER*(*) WHCH
      LOGICAL LVAL
      CALL MAPGTL (WHCH,LVAL)
      RETURN
      END
