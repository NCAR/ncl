C
C $Id: pcgeti.f,v 1.3 1992-11-18 02:13:53 kennison Exp $
C
C
C ---------------------------------------------------------------------
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
