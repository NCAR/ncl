C
C $Id: slgeti.f,v 1.2 1993-01-14 00:29:45 kennison Exp $
C
      SUBROUTINE SLGETI(PNAM,IVAL)
C
C Get the integer value of an STITLE parameter.
C
C Arguments
C     Input
C             PNAM     The three-character name of some parameter.
C
C     Output
C             IVAL     The integer value of the specified parameter.
C
      CHARACTER*(*) PNAM
      CHARACTER*3   CTMP
C
C Call SLGETR to obtain the real value of the parameter and then
C return the integer portion of that.
C
      CALL SLGETR(PNAM,RVAL)
      IVAL = INT(RVAL)
C
C Done.
C
      RETURN
C
      END
