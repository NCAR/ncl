C
C $Id: slseti.f,v 1.3 1995-07-28 18:38:10 kennison Exp $
C
      SUBROUTINE SLSETI (PNAM,IVAL)
C
C Set the integer value of the STITLE parameter named PNAM from IVAL.
C
        CHARACTER*(*) PNAM
C
C Check for an uncleared prior error.
C
        IF (ICFELL('SLSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Pass SLSETR the real equivalent of the integral value and let it do
C the work.
C
        CALL SLSETR (PNAM,REAL(IVAL))
        IF (ICFELL('SLSETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
