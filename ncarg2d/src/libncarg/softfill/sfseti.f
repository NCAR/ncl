C
C $Id: sfseti.f,v 1.2 1994-03-17 20:58:46 kennison Exp $
C
      SUBROUTINE SFSETI (CNP,IVP)
C
      CHARACTER*(*) CNP
C
C This subroutine is called to give an integer value to a specified
C parameter.
C
C CNP is the name of the parameter whose value is to be set.
C
C IVP is an integer variable containing the desired value.
C
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Convert the given value to a real and then use SFSETR to set the
C parameter.
C
      CALL SFSETR (CNP,REAL(IVP))
      IF (ICFELL('SFSETI',2).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
