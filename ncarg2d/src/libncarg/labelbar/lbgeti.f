C
C $Id: lbgeti.f,v 1.2 1994-03-17 20:19:46 kennison Exp $
C
      SUBROUTINE LBGETI (WHCH,IVAL)
C
        CHARACTER*(*) WHCH
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C IVAL is an integer variable in which the desired value is to be
C returned by LBGETI.
C
C
C Check for an uncleared prior error.
C
        IF (ICFELL('LBGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Use LBGETR to retrieve the real value, fix it, and return it to the
C user.
C
        CALL LBGETR (WHCH,RVAL)
        IF (ICFELL('LBGETI',2).NE.0) RETURN
        IVAL=INT(RVAL)
C
C Done.
C
        RETURN
C
      END
