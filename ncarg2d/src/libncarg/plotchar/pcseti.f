C
C $Id: pcseti.f,v 1.7 1994-03-17 18:44:43 kennison Exp $
C
      SUBROUTINE PCSETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C The subroutine PCSETI may be used to set PLCHHQ parameters which have
C values of type INTEGER.
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PCSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Just convert it into a call to the routine PCSETR.
C
      CALL PCSETR (WHCH,REAL(IVAL))
      IF (ICFELL('PCSETI',2).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
