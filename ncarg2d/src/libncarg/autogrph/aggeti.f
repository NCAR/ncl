C
C	$Id: aggeti.f,v 1.1.1.1 1992-04-17 22:31:00 ncargd Exp $
C
C
C ---------------------------------------------------------------------
C
      SUBROUTINE AGGETI (TPID,IUSR)
C
      CHARACTER*(*) TPID
      DIMENSION FURA(1)
C
C The routine AGGETI may be used to get the integer-equivalent value of
C any single AUTOGRAPH control parameter.
C
      CALL AGGETP (TPID,FURA,1)
      IUSR=IFIX(FURA(1))
      RETURN
C
      END
