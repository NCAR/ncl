C
C $Id: slgeti.f,v 1.3 1995-07-28 18:38:05 kennison Exp $
C
      SUBROUTINE SLGETI (PNAM,IVAL)
C
C Get in IVAL the integer value of the STITLE parameter named PNAM.
C
        CHARACTER*(*) PNAM
C
C Check for an uncleared prior error.
C
        IF (ICFELL('SLGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Call SLGETR to obtain the real value of the parameter and then
C return the integer portion of that.
C
        CALL SLGETR (PNAM,RVAL)
        IF (ICFELL('SLGETI',2).NE.0) RETURN
        IVAL=INT(RVAL)
C
C Done.
C
        RETURN
C
      END
