C
C $Id: pcgeti.f,v 1.8 1994-03-19 00:19:42 kennison Exp $
C
      SUBROUTINE PCGETI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C The subroutine PCGETI may be used to get PLCHHQ parameters which have
C values of type INTEGER.
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PCGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Just convert it into a call to the routine PCGETR.
C
      CALL PCGETR (WHCH,RVAL)
      IF (ICFELL('PCGETI',2).NE.0) RETURN
      IVAL=INT(RVAL)
C
C Done.
C
      RETURN
C
      END
