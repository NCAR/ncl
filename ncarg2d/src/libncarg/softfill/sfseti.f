C
C	$Id: sfseti.f,v 1.1.1.1 1992-04-17 22:32:56 ncargd Exp $
C
C
C-----------------------------------------------------------------------
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
C Convert the given value to a real and then use SFSETR to set the
C parameter.
C
      CALL SFSETR (CNP,REAL(IVP))
C
C Done.
C
      RETURN
C
      END
