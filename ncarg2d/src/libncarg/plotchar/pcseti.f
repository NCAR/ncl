C
C $Id: pcseti.f,v 1.5 1994-03-09 23:23:57 kennison Exp $
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
      IF (ICFELL('PCSETI',1).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
