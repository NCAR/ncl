C
C $Id: sfgeti.f,v 1.2 1994-03-17 20:58:36 kennison Exp $
C
      SUBROUTINE SFGETI (CNP,IVP)
C
      CHARACTER*(*) CNP
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C CNP is the name of the parameter whose value is to be retrieved.
C
C IVP is an integer variable in which the desired value is to be
C returned by SFGETI.
C
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Use SFGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL SFGETR (CNP,RVP)
      IF (ICFELL('SFGETI',2).NE.0) RETURN
      IVP=INT(RVP)
C
C Done.
C
      RETURN
C
      END
