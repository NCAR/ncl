C
C	$Id: mpsetl.f,v 1.2 1992-09-04 20:38:44 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MPSETL (WHCH,LVAL)
      CHARACTER*(*) WHCH
      LOGICAL LVAL
      CALL MAPSTL (WHCH,LVAL)
      RETURN
      END
