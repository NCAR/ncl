C
C $Id: agsetr.f,v 1.2 1996-04-18 17:46:19 kennison Exp $
C
      SUBROUTINE AGSETR (TPID,FUSR)
C
      CHARACTER*(*) TPID
      DIMENSION FURA(1)
C
C The routine AGSETR may be used to set the real (floating-point) value
C of any single AUTOGRAPH control parameter.
C
      FURA(1)=FUSR
      CALL AGSETP (TPID,FURA,1)
      RETURN
C
      END
