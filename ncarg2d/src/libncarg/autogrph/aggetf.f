C
C	$Id: aggetf.f,v 1.1.1.1 1992-04-17 22:31:00 ncargd Exp $
C
C
C ---------------------------------------------------------------------
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
