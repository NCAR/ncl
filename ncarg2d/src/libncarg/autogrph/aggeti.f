C
C $Id: aggeti.f,v 1.2 1996-04-18 17:46:09 kennison Exp $
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
