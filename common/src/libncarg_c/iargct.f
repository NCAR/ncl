C
C	$Id: iargct.f,v 1.2 2000-07-11 21:58:06 haley Exp $
C                                                                      
C			     Copyright (C)  2000
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this library; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
