C
C	$Id: agsetr.f,v 1.1.1.1 1992-04-17 22:31:03 ncargd Exp $
C
C
C ---------------------------------------------------------------------
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
