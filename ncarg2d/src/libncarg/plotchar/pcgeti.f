C
C $Id: pcgeti.f,v 1.4 1993-01-12 02:41:05 kennison Exp $
C
      SUBROUTINE PCGETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C The subroutine PCGETI may be used to get PLCHHQ parameters which have
C values of type INTEGER.
C
C Just convert it into a call to the routine PCGETR.
C
      CALL PCGETR (WHCH,RVAL)
      IVAL=INT(RVAL)
C
C Done.
C
      RETURN
C
      END
