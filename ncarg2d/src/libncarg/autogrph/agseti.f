C
C	$Id: agseti.f,v 1.1.1.1 1992-04-17 22:31:03 ncargd Exp $
C
C
C ---------------------------------------------------------------------
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
