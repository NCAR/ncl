C
C	$Id: mpgetr.f,v 1.2 1992-09-04 20:38:39 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MPGETR (WHCH,RVAL)
      CHARACTER*(*) WHCH
      CALL MAPGTR (WHCH,RVAL)
      RETURN
      END
