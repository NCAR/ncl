C
C $Id: agseti.f,v 1.2 1996-04-18 17:46:18 kennison Exp $
C
      SUBROUTINE AGSETI (TPID,IUSR)
C
      CHARACTER*(*) TPID
      DIMENSION FURA(1)
C
C The routine AGSETI may be used to set the integer-equivalent value of
C any single AUTOGRAPH control parameter.
C
      FURA(1)=FLOAT(IUSR)
      CALL AGSETP (TPID,FURA,1)
      RETURN
C
      END
