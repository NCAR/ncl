C
C $Id: idgeti.f,v 1.1 1995-11-03 23:45:20 kennison Exp $
C
      SUBROUTINE IDGETI (PNAM,IVAL)
C
C Get in IVAL the integer value of the BIVAR parameter named PNAM.
C
        CHARACTER*(*) PNAM
C
C Check for an uncleared prior error.
C
        IF (ICFELL('IDGETI (BIVAR) - UNCLEARED PRIOR ERROR',1).NE.0)
     +                                                        RETURN
C
C Call IDGETR to obtain the real value of the parameter and then
C return the integer portion of that.
C
        CALL IDGETR (PNAM,RVAL)
        IF (ICFELL('IDGETI',2).NE.0) RETURN
        IVAL=INT(RVAL)
C
C Done.
C
        RETURN
C
      END
