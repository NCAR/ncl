C
C $Id: aggetf.f,v 1.2 1996-04-18 17:46:08 kennison Exp $
C
      SUBROUTINE AGGETF (TPID,FUSR)
C
      CHARACTER*(*) TPID
      DIMENSION FURA(1)
C
C The routine AGGETF may be used to get the real (floating-point) value
C of any single AUTOGRAPH control parameter.
C
      CALL AGGETP (TPID,FURA,1)
      FUSR=FURA(1)
      RETURN
C
      END
