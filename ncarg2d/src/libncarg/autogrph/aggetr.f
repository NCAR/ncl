C
C $Id: aggetr.f,v 1.2 1996-04-18 17:46:10 kennison Exp $
C
      SUBROUTINE AGGETR (TPID,FUSR)
C
      CHARACTER*(*) TPID
      DIMENSION FURA(1)
C
C The routine AGGETR may be used to get the real (floating-point) value
C of any single AUTOGRAPH control parameter.
C
      CALL AGGETP (TPID,FURA,1)
      FUSR=FURA(1)
      RETURN
C
      END
