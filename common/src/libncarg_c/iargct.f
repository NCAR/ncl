C
C	$Id: iargct.f,v 1.1 1994-08-11 16:29:54 haley Exp $
C
      INTEGER FUNCTION IARGCT(DUMMY)
C***********************************************************************
C	
C	Function: IARGCT()
C
C	Purpose: To determine the number of arguments passed on
C		the command line.
C
C	Input Variables:
C		DUMMY:	Not actually used.
C
C	Function Return:
C		The number of arguments.
C
C
C***********************************************************************
      INTEGER DUMMY, IARGC
C
      IARGCT = IARGC()
C
      RETURN
      END
