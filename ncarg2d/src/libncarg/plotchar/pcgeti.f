C
C $Id: pcgeti.f,v 1.5 1994-03-09 23:23:33 kennison Exp $
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
      IF (ICFELL('PCGETI',1).NE.0) THEN
        IVAL=0
        RETURN
      END IF
      IVAL=INT(RVAL)
C
C Done.
C
      RETURN
C
      END
