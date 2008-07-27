C
C	$Id: iargct.f,v 1.4 2008-07-27 12:23:45 haley Exp $
C                                                                      
C			     Copyright (C)  2000
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
