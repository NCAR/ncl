C
C $Id: slseti.f,v 1.2 1993-01-14 00:29:59 kennison Exp $
C
      SUBROUTINE SLSETI(PNAM,IVAL)
C
C Set a parameter of STITLE, using an integer value.
C
C Arguments
C             PNAM     The three-character name of some parameter.
C
C             IVAL     An integer expression which is to become the
C                      value of the specified parameter.
C
      CHARACTER*(*) PNAM
C
C Pass SLSETR the real equivalent of the integral value and let it do
C the work.
C
      CALL SLSETR(PNAM,REAL(IVAL))
C
C Done.
C
      RETURN
C
      END
