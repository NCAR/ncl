C
C $Id: pcseti.f,v 1.4 1993-01-12 02:41:31 kennison Exp $
C
      SUBROUTINE PCSETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C The subroutine PCSETI may be used to set PLCHHQ parameters which have
C values of type INTEGER.
C
C Just convert it into a call to the routine PCSETR.
C
      CALL PCSETR (WHCH,REAL(IVAL))
C
C Done.
C
      RETURN
C
      END
